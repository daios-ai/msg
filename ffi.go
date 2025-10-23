//go:build linux
// +build linux

package mindscript

/*
#define _GNU_SOURCE
#cgo LDFLAGS: -ldl
#cgo pkg-config: libffi
#include <ffi.h>
#include <dlfcn.h>
#include <stdarg.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <stdint.h> // uintptr_t

// Thin wrapper so cgo/gopls reliably sees the symbol.
static int ms_ffi_prep_cif_var(ffi_cif* cif, ffi_abi abi,
    unsigned int nfixedargs, unsigned int ntotalargs,
    ffi_type* rtype, ffi_type** atypes) {
  return ffi_prep_cif_var(cif, abi, nfixedargs, ntotalargs, rtype, atypes);
}

// ffi_call wrapper: accept a generic void* fn and a void** argv vector.
// This avoids cgo’s function-pointer type constraints at the call site.
static void ms_ffi_call(ffi_cif* cif, void* fn, void* rvalue, void** avalue) {
    ffi_call(cif, (void (*)(void))fn, rvalue, avalue);
}

static int ms_default_abi(void) {
  return FFI_DEFAULT_ABI;
}

// Allocate a cif on the C heap (so it outlives the Go stack frame).
static ffi_cif* ms_alloc_cif(void) {
    return (ffi_cif*)malloc(sizeof(ffi_cif));
}

static void* ms_dlopen(const char* path) {
	return dlopen(path, RTLD_LAZY | RTLD_LOCAL);
}
static const char* ms_dlerror(void) {
	return dlerror();
}
static void* ms_dlsym(void* h, const char* name) {
	return dlsym(h, name);
}
static int ms_dlclose(void* h) {
	return dlclose(h);
}

// Clear dlerror, call dlsym, and return the error (if any) alongside the symbol.
static void* ms_dlsym_clear(void* h, const char* name, char** err) {
 	dlerror(); // clear
 	void* p = dlsym(h, name);
	char* e = dlerror();
 	if (e) { if (err) *err = e; return NULL; }
 	if (err) *err = NULL;
 	return p;
}

// errno helpers
static int* ms_errno_loc(void) {
#if defined(__GLIBC__)
  extern int* __errno_location(void);
  return __errno_location();
#else
  return &errno;
#endif
}

// Call a destructor with signature: void (*)(void*)
typedef void (*ms_destructor_fn)(void*);
static inline void ms_call_destructor(void* fn, void* p) {
  ((ms_destructor_fn)fn)(p);
}

// -------- libffi closure helpers (callbacks) ----------
static void* ms_closure_alloc(void** executable) {
  return ffi_closure_alloc(sizeof(ffi_closure), executable);
}
static int ms_prep_closure(void* closure, ffi_cif* cif,
                    void (*thunk)(ffi_cif*, void*, void**, void*),
                    void* userdata, void* executable) {
  return ffi_prep_closure_loc((ffi_closure*)closure, cif, thunk, userdata, executable);
}
// Wrapper that binds the thunk on the C side to avoid cgo func-ptr typing pitfalls.
// Forward declare the static thunk we define below.
static void ms_callback_thunk(ffi_cif*, void*, void**, void*);
static int ms_prep_closure_with_thunk(void* closure, ffi_cif* cif,
                               void* userdata, void* executable) {
  return ffi_prep_closure_loc((ffi_closure*)closure, cif,
                              ms_callback_thunk, userdata, executable);
}
static void ms_closure_free(void* closure) {
  ffi_closure_free((ffi_closure*)closure);
}

// Forward decl to Go; C thunk forwards into it with integer handle.
extern void msCallbackInvoke(ffi_cif*, void*, void**, uintptr_t);
static void ms_callback_thunk(ffi_cif* cif, void* ret, void** args, void* user) {
  msCallbackInvoke(cif, ret, args, (uintptr_t)user);
}
*/
import "C"

import (
	"errors"
	"fmt"
	"runtime/cgo"
	"sort"
	"sync"
	"unsafe"
)

// -------------------------
// Centralized cgo helpers
// -------------------------

// dlerr returns the last dlerror as a Go string, or a fallback label.
func dlerr() string {
	errC := C.ms_dlerror()
	if errC != nil {
		return C.GoString(errC)
	}
	return "unknown dlerror"
}

// openLib returns the handle for a library path/soname and records it for later dlclose.
// If lib is already the default and hDefault != nil, reuse the default handle.
func (m *ffiModule) openLib(lib, defaultName string, hDefault unsafe.Pointer) (unsafe.Pointer, error) {
	if lib == "" || lib == defaultName {
		return hDefault, nil
	}
	cs := C.CString(lib)
	defer C.free(unsafe.Pointer(cs))
	h := C.ms_dlopen(cs)
	if h == nil {
		return nil, fmt.Errorf("dlopen(%q) failed: %s", lib, dlerr())
	}
	m.openLibs = append(m.openLibs, unsafe.Pointer(h))
	return unsafe.Pointer(h), nil
}

// symChecked resolves a symbol name in lib or returns an error with dlerror detail.
func symChecked(lib unsafe.Pointer, name string) (unsafe.Pointer, error) {
	cs := C.CString(name)
	defer C.free(unsafe.Pointer(cs))
	var cerr *C.char
	p := C.ms_dlsym_clear(lib, cs, &cerr)
	if cerr != nil {
		return nil, fmt.Errorf("dlsym(%q) failed: %s", name, C.GoString(cerr))
	}
	return p, nil
}

////////////////////////////////////////////////////////////////////////////////
// Step (2): spec parsing, type system, layout engine, symbol binding (ELF/SysV)
////////////////////////////////////////////////////////////////////////////////

// -------------------------
// C shim helpers (single TU)
// -------------------------

// memory
func cMalloc(n uintptr) unsafe.Pointer                    { return C.malloc(C.size_t(n)) }
func cCalloc(count, size uintptr) unsafe.Pointer          { return C.calloc(C.size_t(count), C.size_t(size)) }
func cRealloc(p unsafe.Pointer, n uintptr) unsafe.Pointer { return C.realloc(p, C.size_t(n)) }
func cFree(p unsafe.Pointer)                              { C.free(p) }
func cMemcpy(dst, src unsafe.Pointer, n uintptr)          { C.memcpy(dst, src, C.size_t(n)) }
func cMemset(dst unsafe.Pointer, b byte, n uintptr)       { C.memset(dst, C.int(int(b)), C.size_t(n)) }

// strings
func cGoString(p unsafe.Pointer) string { return C.GoString((*C.char)(p)) }
func cGoStringN(p unsafe.Pointer, n int) string {
	return C.GoStringN((*C.char)(p), C.int(n))
}
func cCString(s string) unsafe.Pointer { return unsafe.Pointer(C.CString(s)) }
func cCStringFree(p unsafe.Pointer)    { C.free(p) }

// dlopen/dlsym/dlclose wrappers
func cDlopen(path string) (unsafe.Pointer, error) {
	cs := (*C.char)(cCString(path))
	defer cCStringFree(unsafe.Pointer(cs))
	h := C.ms_dlopen(cs)
	if h == nil {
		return nil, fmt.Errorf("dlopen(%q) failed: %s", path, dlerr())
	}
	return unsafe.Pointer(h), nil
}
func cDlclose(h unsafe.Pointer) error {
	if int(C.ms_dlclose(h)) != 0 {
		return fmt.Errorf("dlclose failed: %s", dlerr())
	}
	return nil
}
func cDlsymClear(h unsafe.Pointer, name string) (unsafe.Pointer, error) {
	cs := (*C.char)(cCString(name))
	defer cCStringFree(unsafe.Pointer(cs))
	var cerr *C.char
	p := C.ms_dlsym_clear(h, cs, &cerr)
	if cerr != nil {
		return nil, fmt.Errorf("dlsym(%q) failed: %s", name, C.GoString(cerr))
	}
	return p, nil
}

// libffi call helpers
type cif = *C.ffi_cif

func cAllocCIF() *C.ffi_cif { return C.ms_alloc_cif() }
func cFFIPrepCIFVar(cif *C.ffi_cif, nfixed, ntotal int, rtype *C.ffi_type, atypes unsafe.Pointer) error {
	st := C.ms_ffi_prep_cif_var(cif, C.FFI_DEFAULT_ABI, C.uint(nfixed), C.uint(ntotal), rtype, (**C.ffi_type)(atypes))
	if st != C.FFI_OK {
		return fmt.Errorf("ffi_prep_cif_var failed: %d", int(st))
	}
	return nil
}
func cFFICall(cif *C.ffi_cif, fn unsafe.Pointer, rvalue unsafe.Pointer, argv unsafe.Pointer) {
	C.ms_ffi_call(cif, fn, rvalue, (*unsafe.Pointer)(argv))
}

// errno get/set
func cErrnoGet() int  { return int(*C.ms_errno_loc()) }
func cErrnoSet(v int) { *C.ms_errno_loc() = C.int(v) }

// void**/ffi_type** array helpers
func cAllocVoidPtrArray(n int) unsafe.Pointer {
	return C.malloc(C.size_t(n) * C.size_t(unsafe.Sizeof(uintptr(0))))
}
func cVoidPtrSlice(mem unsafe.Pointer, n int) []unsafe.Pointer {
	return (*[1<<30 - 1]unsafe.Pointer)(mem)[:n:n]
}
func cAllocFFITypeArray(n int) unsafe.Pointer {
	return C.malloc(C.size_t(n) * C.size_t(unsafe.Sizeof(uintptr(0))))
}
func cFFITypeSlice(mem unsafe.Pointer, n int) []*C.ffi_type {
	return (*[1<<30 - 1]*C.ffi_type)(mem)[:n:n]
}

// --- Opaque libffi type helpers (keep all C references in this file) ---

// Return opaque pointers to common builtin ffi_type objects.
func ffiTypeSint32Ptr() unsafe.Pointer  { return unsafe.Pointer(&C.ffi_type_sint32) }
func ffiTypeDoublePtr() unsafe.Pointer  { return unsafe.Pointer(&C.ffi_type_double) }
func ffiTypePointerPtr() unsafe.Pointer { return unsafe.Pointer(&C.ffi_type_pointer) }

// Fill a ffi_type** array (allocated via cAllocFFITypeArray) from registry keys.
func fillFFITypesFromKeys(reg *ffiRegistry, mem unsafe.Pointer, keys []string) error {
	vec := cFFITypeSlice(mem, len(keys)) // uses *C.ffi_type internally (C access kept here)
	for i, k := range keys {
		t, err := ffiTypeFor(reg, k)
		if err != nil {
			return err
		}
		vec[i] = t
	}
	return nil
}

// Set one entry in a ffi_type** array at index idx using an opaque pointer.
func setFFITypeAt(mem unsafe.Pointer, idx int, ty unsafe.Pointer) {
	vec := cFFITypeSlice(mem, idx+1)
	vec[idx] = (*C.ffi_type)(ty)
}

func ffiReturnTypePtr(reg *ffiRegistry, key string) (unsafe.Pointer, error) {
	t, err := ffiTypeFor(reg, key)
	if err != nil {
		return nil, err
	}
	return unsafe.Pointer(t), nil
}

func cFFIPrepCIFVarOpaque(cif *C.ffi_cif, nfixed, ntotal int, rtype unsafe.Pointer, atypes unsafe.Pointer) error {
	return cFFIPrepCIFVar(cif, nfixed, ntotal, (*C.ffi_type)(rtype), atypes)
}

// --- Internal FFI model ------------------------------------------------------

type ffiKind int

const (
	ffiVoid ffiKind = iota
	ffiInt
	ffiFloat
	ffiPointer
	ffiArray
	ffiStruct
	ffiUnion
	ffiEnum
	ffiFuncPtr
	ffiAlias
	ffiHandle // semantic handle (tagged) backed by rep TypeRef
)

// ffiType is the canonical structural description plus computed layout.
// Layout (Size/Align/Offsets) is filled by the layout engine once.
type ffiType struct {
	Kind     ffiKind
	Name     string           // display/debug name (optional in spec)
	Doc      string           // docs
	Key      string           // registry key (reverse map; set by addType)
	Bits     int              // int/float width
	Signed   bool             // ints
	Elem     string           // array element type (registry key)
	Len      int              // array fixed length, or -1 for VLA/flexible
	To       string           // pointer/alias target type (registry key)
	Fields   []ffiField       // struct/union fields
	EnumBase string           // enum underlying int type
	EnumVals map[string]int64 // enum named constants
	Params   []string         // funcptr param types
	Ret      string           // funcptr return type
	Variadic bool             // funcptr variadic

	// Optional pointer/handle tag (semantic label, used to build VTHandle.Kind)
	Tag string

	// Computed layout (SysV ABI)
	Size    uintptr
	Align   uintptr
	Offsets []uintptr // struct: per-field; union: nil; array: nil

	// --- callbacks (funcptr): cached callback CIF + argv type vector (C-heap) ---
	cbCIF      *C.ffi_cif
	cbTypesVec unsafe.Pointer
}

type ffiField struct {
	Name string
	Type string // registry key
	Bits int    // bitfield width; 0 = force new storage unit per C rule
}

type ffiFunction struct {
	Name      string
	Lib       string // if empty, use spec.lib
	Ret       string // type key
	Params    []string
	Variadic  bool
	Doc       string
	RetAsStr  bool // char*/uchar* -> Str convenience
	SymbolPtr unsafe.Pointer
	cif       *C.ffi_cif
	typesVec  unsafe.Pointer // malloc'd array of ffi_type* (argv type vector)
}

type ffiVariable struct {
	Name      string
	Lib       string // if empty, use spec.lib
	Type      string
	SymbolPtr unsafe.Pointer // address of the object (void*)
}

type ffiRegistry struct {
	types map[string]*ffiType
}

func newFFIRegistry() *ffiRegistry { return &ffiRegistry{types: make(map[string]*ffiType)} }

// addType inserts or errors if duplicate.
func (r *ffiRegistry) addType(key string, t *ffiType) error {
	if _, exists := r.types[key]; exists {
		return fmt.Errorf("duplicate type name: %s", key)
	}
	t.Key = key
	r.types[key] = t
	return nil
}

func (r *ffiRegistry) mustGet(key string) *ffiType {
	t, ok := r.types[key]
	if !ok {
		panic("ffi: internal: missing type: " + key)
	}
	return t
}

// ffiModule is a per-module state bag we hide behind a VTHandle.
type ffiModule struct {
	libName string
	libH    unsafe.Pointer // dlopen handle
	reg     *ffiRegistry
	funcs   map[string]*ffiFunction
	vars    map[string]*ffiVariable

	// track all opened handles (default + per-symbol overrides) to close later
	openLibs []unsafe.Pointer

	// live libffi closures for callbacks (freed on close)
	cbs []cbRecord
}

// One record per created callback closure.
type cbRecord struct {
	closure unsafe.Pointer // ffi_closure*
	handle  cgo.Handle     // cgo handle for callback context
}

// Callback context carried through cgo.Handle.
type cbContext struct {
	ip  *Interpreter
	fn  Value
	typ *ffiType
	reg *ffiRegistry
}

// ---------------- One-shot GC destructor registry (shared) --------------------
type gcDtorKind uint8

const (
	gcNone  gcDtorKind = iota
	gcFree             // C.free
	gcCFunc            // void(*)(void*)
)

type gcEntry struct {
	once sync.Once
	kind gcDtorKind
	fn   unsafe.Pointer // for gcCFunc
	lib  unsafe.Pointer // retained dlopen until run; may be nil
}

var gcReg sync.Map // raw pointer -> *gcEntry

func gcInstall(p unsafe.Pointer, e *gcEntry) {
	if p != nil {
		gcReg.Store(p, e)
	}
}
func gcDetach(p unsafe.Pointer) {
	if p != nil {
		gcReg.Delete(p)
	}
}
func gcRunOnce(p unsafe.Pointer) bool {
	if p == nil {
		return false
	}
	v, ok := gcReg.Load(p)
	if !ok {
		return false
	}
	e := v.(*gcEntry)
	e.once.Do(func() {
		switch e.kind {
		case gcFree:
			C.free(p)
		case gcCFunc:
			if e.fn != nil {
				C.ms_call_destructor(e.fn, p)
			}
		}
		if e.lib != nil {
			C.ms_dlclose(e.lib)
			e.lib = nil
		}
	})
	return true
}

// move (used by realloc): re-attach entry to q (fresh Once).
func gcMove(p, q unsafe.Pointer) {
	if p == nil || q == nil || p == q {
		return
	}
	if v, ok := gcReg.Load(p); ok {
		e := v.(*gcEntry)
		e.once = sync.Once{}
		gcReg.Delete(p)
		gcReg.Store(q, e)
	}
}

// -------- libffi thin helpers (minimal; structs-by-value/callbacks deferred) -------
type _cif = *C.ffi_cif // kept to avoid breaking references in this file

func ffiTypeFor(reg *ffiRegistry, key string) (*C.ffi_type, error) {
	t := reg.mustGet(key)
	switch t.Kind {
	case ffiVoid:
		return &C.ffi_type_void, nil
	case ffiInt:
		switch t.Bits {
		case 8:
			if t.Signed {
				return &C.ffi_type_sint8, nil
			}
			return &C.ffi_type_uint8, nil
		case 16:
			if t.Signed {
				return &C.ffi_type_sint16, nil
			}
			return &C.ffi_type_uint16, nil
		case 32:
			if t.Signed {
				return &C.ffi_type_sint32, nil
			}
			return &C.ffi_type_uint32, nil
		case 64:
			if t.Signed {
				return &C.ffi_type_sint64, nil
			}
			return &C.ffi_type_uint64, nil

		default:
			return nil, fmt.Errorf("int bits=%d not supported", t.Bits)
		}
	case ffiFloat:
		switch t.Bits {
		case 32:
			return &C.ffi_type_float, nil
		case 64:
			return &C.ffi_type_double, nil
		default:
			return nil, fmt.Errorf("float bits=%d not supported (only 32/64)", t.Bits)
		}
	case ffiPointer, ffiFuncPtr, ffiHandle:
		return &C.ffi_type_pointer, nil
	case ffiEnum:
		return ffiTypeFor(reg, t.EnumBase)
	case ffiArray, ffiStruct, ffiUnion:
		// Minimal v1: require passing by pointer.
		return nil, fmt.Errorf("aggregate by-value not supported; pass a pointer")
	case ffiAlias:
		return ffiTypeFor(reg, t.To)
	default:
		return nil, fmt.Errorf("unhandled kind")
	}
}

// ensureFuncptrCIF lazily prepares a callback-side CIF for a funcptr type.
func ensureFuncptrCIF(reg *ffiRegistry, ft *ffiType) error {
	if ft.Kind != ffiFuncPtr {
		return fmt.Errorf("internal: ensureFuncptrCIF on non-funcptr")
	}
	if ft.cbCIF != nil {
		return nil
	}
	cf, tp, err := prepCIF(reg, ft.Ret, ft.Params)
	if err != nil {
		return err
	}
	ft.cbCIF = cf
	ft.cbTypesVec = tp
	return nil
}

// prepCIF allocates a C-heap cif and a C-heap ffi_type** argv vector.
// The caller is responsible for freeing both.
func prepCIF(reg *ffiRegistry, ret string, params []string) (cif, unsafe.Pointer, error) {
	rty, err := ffiTypeFor(reg, ret)
	if err != nil {
		return nil, nil, err
	}
	n := len(params)
	var typesPtr **C.ffi_type
	if n > 0 {
		bytes := C.malloc(C.size_t(n) * C.size_t(unsafe.Sizeof(uintptr(0))))
		if bytes == nil {
			return nil, nil, fmt.Errorf("ffi_prep_cif: OOM")
		}
		types := (*[1<<30 - 1]*C.ffi_type)(bytes)[:n:n]
		for i, p := range params {
			pt, err := ffiTypeFor(reg, p)
			if err != nil {
				return nil, nil, fmt.Errorf("param[%d]: %w", i, err)
			}
			types[i] = pt
		}
		typesPtr = (**C.ffi_type)(bytes) // keep allocated; libffi reads it on call
	} else {
		typesPtr = nil
	}
	c := C.ms_alloc_cif()
	if c == nil {
		return nil, nil, fmt.Errorf("ffi_prep_cif: OOM")
	}
	st := C.ffi_prep_cif(c, C.FFI_DEFAULT_ABI, C.uint(n), rty, typesPtr)
	if st != C.FFI_OK {
		return nil, unsafe.Pointer(typesPtr), fmt.Errorf("ffi_prep_cif failed: %d", int(st))
	}
	return c, unsafe.Pointer(typesPtr), nil
}

////////////////////////////////////////////////////////////////////////////////
// Spec parsing & normalization (JSON-compatible Value -> ffiModule)
////////////////////////////////////////////////////////////////////////////////

func parseFFISpec(spec *MapObject) (string, *ffiRegistry, []*ffiFunction, []*ffiVariable, error) {
	// version
	v, ok := spec.Entries["version"]
	if !ok {
		return "", nil, nil, nil, errors.New("ffiOpen: missing 'version'")
	}
	if v.Tag != VTStr || v.Data.(string) != "1" {
		return "", nil, nil, nil, errors.New("ffiOpen: unsupported version (expected \"1\")")
	}

	// lib
	libV, ok := spec.Entries["lib"]
	if !ok || libV.Tag != VTStr {
		return "", nil, nil, nil, errors.New("ffiOpen: missing or invalid 'lib' (expected Str)")
	}
	lib := libV.Data.(string)

	reg := newFFIRegistry()

	// seed some builtin atoms for convenience
	_ = reg.addType("__void__", &ffiType{Kind: ffiVoid, Name: "void", Size: 0, Align: 1})
	_ = reg.addType("__char__", &ffiType{Kind: ffiInt, Name: "char", Bits: 8, Signed: true})
	_ = reg.addType("__uchar__", &ffiType{Kind: ffiInt, Name: "unsigned char", Bits: 8, Signed: false})

	// types (optional)
	if tv, ok := spec.Entries["types"]; ok {
		if tv.Tag != VTMap {
			return "", nil, nil, nil, errors.New("ffiOpen: 'types' must be a map")
		}
		for _, k := range tv.Data.(*MapObject).Keys {
			tvV := tv.Data.(*MapObject).Entries[k]
			t, err := parseTypeObject(reg, k, tvV)
			if err != nil {
				return "", nil, nil, nil, fmt.Errorf("ffiOpen.types[%s]: %v", k, err)
			}
			if err := reg.addType(k, t); err != nil {
				return "", nil, nil, nil, err
			}
		}
	}

	// functions (optional)
	var funs []*ffiFunction
	if fv, ok := spec.Entries["functions"]; ok {
		if fv.Tag != VTArray {
			return "", nil, nil, nil, errors.New("ffiOpen: 'functions' must be an array")
		}
		seen := map[string]bool{}
		for i, raw := range fv.Data.(*ArrayObject).Elems {
			fn, err := parseFunctionObject(reg, raw)
			if err != nil {
				return "", nil, nil, nil, fmt.Errorf("ffiOpen.functions[%d]: %v", i, err)
			}
			if seen[fn.Name] {
				return "", nil, nil, nil, fmt.Errorf("ffiOpen: duplicate function name: %s", fn.Name)
			}
			seen[fn.Name] = true
			funs = append(funs, fn)
		}
	}

	// variables (optional)
	var vars []*ffiVariable
	if vv, ok := spec.Entries["variables"]; ok {
		if vv.Tag != VTArray {
			return "", nil, nil, nil, errors.New("ffiOpen: 'variables' must be an array")
		}
		seen := map[string]bool{}
		for i, raw := range vv.Data.(*ArrayObject).Elems {
			vd, err := parseVariableObject(reg, raw)
			if err != nil {
				return "", nil, nil, nil, fmt.Errorf("ffiOpen.variables[%d]: %v", i, err)
			}
			if seen[vd.Name] {
				return "", nil, nil, nil, fmt.Errorf("ffiOpen: duplicate variable name: %s", vd.Name)
			}
			seen[vd.Name] = true
			vars = append(vars, vd)
		}
	}

	// Resolve/normalize types: replace inline refs with names
	if err := normalizeTypes(reg); err != nil {
		return "", nil, nil, nil, err
	}
	// Compute layout
	if err := computeAllLayouts(reg); err != nil {
		return "", nil, nil, nil, err
	}

	// Enforce: ret_as_str is only valid for char*/unsigned char*
	for _, f := range funs {
		if f.RetAsStr {
			rt := reg.mustGet(f.Ret)
			if !((rt.Kind == ffiPointer || rt.Kind == ffiHandle) && isCharPtr(reg, rt)) {
				return "", nil, nil, nil, fmt.Errorf("function %s: ret_as_str on non-char*", f.Name)
			}
		}
	}

	// Validate function & variable type refs exist
	for _, f := range funs {
		if _, ok := reg.types[f.Ret]; !ok {
			return "", nil, nil, nil, fmt.Errorf("function %s: unknown return type %q", f.Name, f.Ret)
		}
		for i, p := range f.Params {
			if _, ok := reg.types[p]; !ok {
				return "", nil, nil, nil, fmt.Errorf("function %s: unknown param[%d] type %q", f.Name, i, p)
			}
		}
	}
	for _, v := range vars {
		if _, ok := reg.types[v.Type]; !ok {
			return "", nil, nil, nil, fmt.Errorf("variable %s: unknown type %q", v.Name, v.Type)
		}
	}

	return lib, reg, funs, vars, nil
}

// resolveTypeRef normalizes a TypeRef (string key or inline type object) into a registry key.
// If inline, it parses the object, assigns it an anonymous key based on ctx, and inserts it.
func resolveTypeRef(reg *ffiRegistry, ctx string, v Value) (string, error) {
	if v.Tag == VTStr {
		return v.Data.(string), nil
	}
	if v.Tag != VTMap {
		return "", fmt.Errorf("%s: type reference must be a name (Str) or map", ctx)
	}
	anonKey := "__anon__" + ctx
	t, err := parseTypeObject(reg, anonKey, v)
	if err != nil {
		return "", err
	}
	if err := reg.addType(anonKey, t); err != nil {
		return "", err
	}
	return anonKey, nil
}

// parseTypeObject accepts a spec value (map or alias string) describing a Type.
func parseTypeObject(reg *ffiRegistry, key string, v Value) (*ffiType, error) {
	if v.Tag == VTStr {
		// alias to another declared type key
		return &ffiType{Kind: ffiAlias, To: v.Data.(string), Name: key}, nil
	}
	if v.Tag != VTMap {
		return nil, errors.New("type must be a map or string alias")
	}
	m := v.Data.(*MapObject)
	kindV, ok := m.Entries["kind"]
	if !ok || kindV.Tag != VTStr {
		return nil, errors.New(`type: missing "kind": string`)
	}
	kind := kindV.Data.(string)
	t := &ffiType{Name: ffiGetStr(m, "name", ""), Doc: ffiGetStr(m, "doc", "")}

	switch kind {
	case "void":
		t.Kind = ffiVoid
	case "int":
		t.Kind = ffiInt
		t.Bits = int(ffiGetInt(m, "bits", -1))
		sv, ok := m.Entries["signed"]
		if !ok || (sv.Tag != VTBool) {
			return nil, errors.New(`int: missing "signed": bool`)
		}
		t.Signed = sv.Data.(bool)
		if t.Bits != 8 && t.Bits != 16 && t.Bits != 32 && t.Bits != 64 {
			return nil, errors.New("int: bits must be 8/16/32/64")
		}
	case "float":
		t.Kind = ffiFloat
		t.Bits = int(ffiGetInt(m, "bits", -1))
		if t.Bits != 32 && t.Bits != 64 {
			return nil, errors.New("float: bits must be 32/64")
		}
	case "pointer":
		t.Kind = ffiPointer
		to, ok := m.Entries["to"]
		if !ok {
			return nil, errors.New(`pointer: missing "to"`)
		}
		ref, err := resolveTypeRef(reg, "ptr_to__"+key, to)
		if err != nil {
			return nil, err
		}
		t.To = ref
		t.Tag = ffiGetStr(m, "tag", "")
	case "array":
		t.Kind = ffiArray
		of, ok := m.Entries["of"]
		if !ok {
			return nil, errors.New(`array: missing "of"`)
		}
		ref, err := resolveTypeRef(reg, "array_of__"+key, of)
		if err != nil {
			return nil, err
		}
		t.Elem = ref
		if lnV, ok := m.Entries["len"]; ok {
			if lnV.Tag != VTInt || lnV.Data.(int64) < 0 {
				return nil, errors.New("array.len must be non-negative int")
			}
			t.Len = int(lnV.Data.(int64))
		} else {
			t.Len = -1 // VLA/flexible, placement rules apply
		}
	case "struct":
		t.Kind = ffiStruct
		fl, ok := m.Entries["fields"]
		if !ok || fl.Tag != VTArray {
			return nil, errors.New(`struct: missing "fields": [ ... ]`)
		}
		for i, e := range fl.Data.(*ArrayObject).Elems {
			fm, ok := ffiAsMap(e)
			if !ok {
				return nil, fmt.Errorf("struct.fields[%d]: must be map", i)
			}
			f := ffiField{
				Name: ffiGetReqStr(fm, "name"),
			}
			ttv, ok := fm.Entries["type"]
			if !ok {
				return nil, fmt.Errorf("struct.fields[%d]: missing 'type'", i)
			}
			ref, err := resolveTypeRef(reg, fmt.Sprintf("struct_%s_field_%d", key, i), ttv)
			if err != nil {
				return nil, err
			}
			f.Type = ref
			if bf, ok := fm.Entries["bits"]; ok {
				if bf.Tag != VTInt || bf.Data.(int64) < 0 {
					return nil, fmt.Errorf("struct.fields[%d]: bits must be non-negative int", i)
				}
				f.Bits = int(bf.Data.(int64))
			}
			t.Fields = append(t.Fields, f)
		}
	case "union":
		t.Kind = ffiUnion
		fl, ok := m.Entries["fields"]
		if !ok || fl.Tag != VTArray {
			return nil, errors.New(`union: missing "fields": [ ... ]`)
		}
		for i, e := range fl.Data.(*ArrayObject).Elems {
			fm, ok := ffiAsMap(e)
			if !ok {
				return nil, fmt.Errorf("union.fields[%d]: must be map", i)
			}
			f := ffiField{
				Name: ffiGetReqStr(fm, "name"),
			}
			ttv, ok := fm.Entries["type"]
			if !ok {
				return nil, fmt.Errorf("union.fields[%d]: missing 'type'", i)
			}
			ref, err := resolveTypeRef(reg, fmt.Sprintf("union_%s_field_%d", key, i), ttv)
			if err != nil {
				return nil, err
			}
			f.Type = ref
			t.Fields = append(t.Fields, f)
		}
	case "enum":
		t.Kind = ffiEnum
		if bv, ok := m.Entries["base"]; ok && bv.Tag == VTStr {
			t.EnumBase = bv.Data.(string)
		} else {
			return nil, errors.New(`enum: missing "base": string`)
		}
		t.EnumVals = map[string]int64{}
		if vv, ok := m.Entries["values"]; ok {
			vm, ok := ffiAsMap(vv)
			if !ok {
				return nil, errors.New("enum.values must be a map")
			}
			for _, k := range vm.Keys {
				val := vm.Entries[k]
				if val.Tag != VTInt {
					return nil, fmt.Errorf("enum.values[%s] must be int", k)
				}
				t.EnumVals[k] = val.Data.(int64)
			}
		}
	case "funcptr":
		t.Kind = ffiFuncPtr
		// ret
		rv, ok := m.Entries["ret"]
		if !ok {
			return nil, errors.New("funcptr: missing 'ret'")
		}
		ref, err := resolveTypeRef(reg, "funcptr_ret__"+key, rv)
		if err != nil {
			return nil, err
		}
		t.Ret = ref
		// params
		pv, ok := m.Entries["params"]
		if !ok || pv.Tag != VTArray {
			return nil, errors.New("funcptr: missing 'params' array")
		}
		for i, e := range pv.Data.(*ArrayObject).Elems {
			ref, err := resolveTypeRef(reg, fmt.Sprintf("funcptr_param_%s_%d", key, i), e)
			if err != nil {
				return nil, err
			}
			t.Params = append(t.Params, ref)
		}
		if vv, ok := m.Entries["variadic"]; ok && vv.Tag == VTBool {
			t.Variadic = vv.Data.(bool)
		}
	case "alias":
		t.Kind = ffiAlias
		tv, ok := m.Entries["to"]
		if !ok || tv.Tag != VTStr {
			return nil, errors.New(`alias: missing "to": string`)
		}
		t.To = tv.Data.(string)
	case "handle":
		t.Kind = ffiHandle
		t.Tag = ffiGetReqStr(m, "tag")
		tv, ok := m.Entries["rep"]
		if !ok {
			return nil, errors.New(`handle: missing "rep"`)
		}
		ref, err := resolveTypeRef(reg, "handle_rep__"+key, tv)
		if err != nil {
			return nil, err
		}
		t.To = ref
	default:
		return nil, fmt.Errorf("unknown kind %q", kind)
	}
	return t, nil
}

func parseFunctionObject(reg *ffiRegistry, v Value) (*ffiFunction, error) {
	m, ok := ffiAsMap(v)
	if !ok {
		return nil, errors.New("function must be a map")
	}
	name := ffiGetReqStr(m, "name")
	retV, ok := m.Entries["ret"]
	if !ok {
		return nil, errors.New("function: missing 'ret'")
	}
	var ret string
	rk, err := resolveTypeRef(reg, "fn_ret__"+name, retV)
	if err != nil {
		return nil, err
	}
	ret = rk
	params := []string{}
	pv, ok := m.Entries["params"]
	if !ok || pv.Tag != VTArray {
		return nil, errors.New("function: missing 'params' array")
	}
	for i, e := range pv.Data.(*ArrayObject).Elems {
		rk, err := resolveTypeRef(reg, fmt.Sprintf("fn_%s_param_%d", name, i), e)
		if err != nil {
			return nil, err
		}
		params = append(params, rk)
	}
	fn := &ffiFunction{
		Name:     name,
		Lib:      ffiGetStr(m, "lib", ""),
		Ret:      ret,
		Params:   params,
		Variadic: ffiGetBool(m, "variadic", false),
		Doc:      ffiGetStr(m, "doc", ""),
		RetAsStr: ffiGetBool(m, "ret_as_str", false),
	}
	return fn, nil
}

func parseVariableObject(reg *ffiRegistry, v Value) (*ffiVariable, error) {
	m, ok := ffiAsMap(v)
	if !ok {
		return nil, errors.New("variable must be a map")
	}
	name := ffiGetReqStr(m, "name")
	tv, ok := m.Entries["type"]
	if !ok {
		return nil, errors.New("variable: missing 'type'")
	}
	rk, err := resolveTypeRef(reg, "var_type__"+name, tv)
	if err != nil {
		return nil, err
	}
	return &ffiVariable{
		Name: name,
		Lib:  ffiGetStr(m, "lib", ""),
		Type: rk,
	}, nil
}

// normalizeTypes resolves aliases transitively and detects cycles.
// After normalization, all ffiAlias.To point to a non-alias concrete type key.
func normalizeTypes(reg *ffiRegistry) error {
	for name, t := range reg.types {
		if t.Kind != ffiAlias {
			continue
		}
		target := t.To
		seen := map[string]bool{name: true}
		for {
			tt, ok := reg.types[target]
			if !ok {
				return fmt.Errorf("alias %s: unknown target %q", name, target)
			}
			if tt.Kind == ffiAlias {
				if seen[target] {
					return fmt.Errorf("alias cycle at %s", target)
				}
				seen[target] = true
				target = tt.To
				continue
			}
			break
		}
		t.To = target
	}
	return nil
}

////////////////////////////////////////////////////////////////////////////////
// Layout engine (SysV ABI, Linux x86-64 assumptions for step 2)
// NOTE: Conservative, correct-by-construction for common cases.
// Bitfields left for later; emit clear error when present.
////////////////////////////////////////////////////////////////////////////////

func computeAllLayouts(reg *ffiRegistry) error {
	names := make([]string, 0, len(reg.types))
	for k := range reg.types {
		names = append(names, k)
	}
	sort.Strings(names)

	state := make(map[string]uint8, len(reg.types)) // 0=unseen,1=visiting,2=done
	var visit func(string) error
	visit = func(k string) error {
		switch state[k] {
		case 2:
			return nil
		case 1:
			return fmt.Errorf("layout(%s): cyclic aggregate reference", k)
		}
		state[k] = 1
		t := reg.mustGet(k)
		switch t.Kind {
		case ffiAlias:
			if err := visit(t.To); err != nil {
				return err
			}
		case ffiArray:
			if err := visit(t.Elem); err != nil {
				return err
			}
		case ffiStruct, ffiUnion:
			for _, f := range t.Fields {
				ft := reg.mustGet(f.Type)
				if ft.Kind != ffiPointer && ft.Kind != ffiFuncPtr && ft.Kind != ffiHandle {
					if err := visit(f.Type); err != nil {
						return err
					}
				}
			}
		case ffiEnum:
			if err := visit(t.EnumBase); err != nil {
				return err
			}
		}
		if err := computeLayoutOne(reg, t); err != nil {
			return fmt.Errorf("layout(%s): %w", k, err)
		}
		state[k] = 2
		return nil
	}
	for _, k := range names {
		if err := visit(k); err != nil {
			return err
		}
	}
	for _, k := range names {
		t := reg.types[k]
		if t.Align == 0 && t.Kind != ffiVoid {
			return fmt.Errorf("layout(%s): unresolved alignment", k)
		}
	}
	return nil
}

func computeLayoutOne(reg *ffiRegistry, t *ffiType) error {
	switch t.Kind {
	case ffiVoid:
		t.Size, t.Align = 0, 1
	case ffiInt:
		t.Align = alignOfIntBits(t.Bits)
		t.Size = sizeOfIntBits(t.Bits)
	case ffiFloat:
		switch t.Bits {
		case 32:
			t.Size, t.Align = 4, 4
		case 64:
			t.Size, t.Align = 8, 8
		default:
			return fmt.Errorf("float bits=%d not supported (only 32/64)", t.Bits)
		}
	case ffiPointer, ffiFuncPtr, ffiHandle:
		// Pointers/funcptrs/handles pass as pointers: 8-byte size/align on x86-64
		t.Size, t.Align = 8, 8
	case ffiAlias:
		tt := reg.mustGet(t.To)
		t.Size, t.Align = tt.Size, tt.Align
	case ffiArray:
		elem := reg.mustGet(t.Elem)
		if elem.Align == 0 {
			if err := computeLayoutOne(reg, elem); err != nil {
				return fmt.Errorf("array element %s: %w", t.Elem, err)
			}
		}
		if elem.Align == 0 {
			return fmt.Errorf("array element %s unresolved align", t.Elem)
		}
		if t.Len < 0 {
			// VLA/flexible: size unknown until allocated; we expose size=0, align=elem.Align
			t.Size, t.Align = 0, elem.Align
		} else {
			t.Align = elem.Align
			t.Size = uintptr(t.Len) * elem.Size
		}
	case ffiStruct:
		var off uintptr
		maxAlign := uintptr(1)
		offsets := make([]uintptr, len(t.Fields))
		for i, f := range t.Fields {
			if f.Bits != 0 {
				return fmt.Errorf("bitfields not supported yet")
			}
			ft := reg.mustGet(f.Type)
			if ft.Align == 0 {
				if err := computeLayoutOne(reg, ft); err != nil {
					return fmt.Errorf("field %s: %w", f.Name, err)
				}
			}
			if ft.Align == 0 {
				return fmt.Errorf("field %s unresolved align", f.Name)
			}
			a := ft.Align
			off = alignUp(off, a)
			offsets[i] = off
			off += ft.Size
			if a > maxAlign {
				maxAlign = a
			}
		}
		t.Align = maxAlign
		t.Size = alignUp(off, maxAlign)
		t.Offsets = offsets
	case ffiUnion:
		var maxS, maxA uintptr
		for _, f := range t.Fields {
			ft := reg.mustGet(f.Type)
			if ft.Align == 0 {
				if err := computeLayoutOne(reg, ft); err != nil {
					return fmt.Errorf("union field %s: %w", f.Name, err)
				}
			}

			if ft.Size > maxS {
				maxS = ft.Size
			}
			if ft.Align > maxA {
				maxA = ft.Align
			}
		}
		if maxA == 0 {
			maxA = 1
		}
		t.Align = maxA
		t.Size = alignUp(maxS, maxA)
	default:
		// enum uses base integer layout
		if t.Kind == ffiEnum {
			bt := reg.mustGet(t.EnumBase)
			t.Size, t.Align = bt.Size, bt.Align
			return nil
		}
		return fmt.Errorf("layout for kind %v not implemented", t.Kind)
	}
	return nil
}

func sizeOfIntBits(bits int) uintptr {
	switch bits {
	case 8:
		return 1
	case 16:
		return 2
	case 32:
		return 4
	case 64:
		return 8
	default:
		return 0
	}
}
func alignOfIntBits(bits int) uintptr {
	switch bits {
	case 8:
		return 1
	case 16:
		return 2
	case 32:
		return 4
	case 64:
		return 8
	default:
		return 1
	}
}

func alignUp(x, a uintptr) uintptr {
	m := a - 1
	return (x + m) &^ m
}

////////////////////////////////////////////////////////////////////////////////
// Binding helpers used by runtime glue (builtin_ffi.go)
////////////////////////////////////////////////////////////////////////////////

// NOTE: The actual registration of the builtin (`registerFFIBuiltins`) lives in
// builtin_ffi.go. The helpers below are kept here so both engine and glue share
// the same small utilities.

func ffiAsMap(v Value) (*MapObject, bool) {
	if v.Tag != VTMap {
		return nil, false
	}
	return v.Data.(*MapObject), true
}
func ffiGetStr(m *MapObject, key, def string) string {
	if v, ok := m.Entries[key]; ok && v.Tag == VTStr {
		return v.Data.(string)
	}
	return def
}
func ffiGetReqStr(m *MapObject, key string) string {
	if v, ok := m.Entries[key]; ok && v.Tag == VTStr {
		return v.Data.(string)
	}
	fail("ffi: missing field " + key + " (string)")
	return ""
}
func ffiGetInt(m *MapObject, key string, def int64) int64 {
	if v, ok := m.Entries[key]; ok && v.Tag == VTInt {
		return v.Data.(int64)
	}
	return def
}
func ffiGetBool(m *MapObject, key string, def bool) bool {
	if v, ok := m.Entries[key]; ok && v.Tag == VTBool {
		return v.Data.(bool)
	}
	return def
}

func canonicalPtrTagKey(reg *ffiRegistry, t *ffiType) string {
	// Handles/explicitly-tagged pointers: honor verbatim.
	if t.Kind == ffiHandle && t.Tag != "" {
		return t.Tag
	}
	if t.Kind == ffiPointer && t.Tag != "" {
		return t.Tag
	}
	// Prefer stable registry key if present.
	if t.Key != "" {
		if t.Kind == ffiPointer {
			return t.To
		}
		return t.Key
	}
	if t.Kind == ffiPointer {
		return t.To
	}
	if t.Name != "" {
		return t.Name
	}
	return "(anon)"
}

// typeKey returns the registry key for t if known, otherwise t.Name (best-effort).
func typeKey(reg *ffiRegistry, t *ffiType) string {
	if t.Key != "" {
		return t.Key
	}
	if t.Name != "" {
		return t.Name
	}
	return "(anon)"
}

func buildParamSpecs(_ *ffiModule, f *ffiFunction) []ParamSpec {
	ps := make([]ParamSpec, 0, len(f.Params))
	// We can’t express C types in MindScript Type S-exprs; for now declare Any.
	for i := range f.Params {
		ps = append(ps, ParamSpec{Name: fmt.Sprintf("p%d", i), Type: S{"id", "Any"}})
	}
	if f.Variadic {
		// One extra MindScript parameter carrying the varargs bundle (must be an array).
		ps = append(ps, ParamSpec{Name: fmt.Sprintf("p%d", len(f.Params)), Type: S{"id", "Any"}})
	}
	return ps
}

////////////////////////////////////////////////////////////////////////////////
// Marshalling helpers
////////////////////////////////////////////////////////////////////////////////

type cleanupFn func()

func expectPtr(v Value) unsafe.Pointer {
	if v.Tag != VTHandle {
		fail("expected pointer handle")
	}
	h := v.Data.(*Handle)
	if h == nil {
		fail("bad handle")
	}
	p, ok := h.Data.(unsafe.Pointer)
	if !ok {
		fail("bad pointer payload")
	}
	return p
}

func isCharPtr(reg *ffiRegistry, t *ffiType) bool {
	if t.Kind != ffiPointer && t.Kind != ffiHandle {
		return false
	}
	pt := reg.mustGet(t.To)
	return pt.Kind == ffiInt && pt.Bits == 8
}

// Pointer/handle to aggregate?
func isPtrToAggregate(reg *ffiRegistry, t *ffiType) (*ffiType, bool) {
	if t.Kind != ffiPointer && t.Kind != ffiHandle {
		return nil, false
	}
	to := reg.mustGet(t.To)
	switch to.Kind {
	case ffiStruct, ffiUnion, ffiArray:
		return to, true
	}
	return nil, false
}

// ---------- Auto-boxing (map/array literal → temp aggregate buffer) ----------
const autoBoxMaxAlloc = uintptr(16 << 20) // 16 MiB per arg
func mulOvf(a, b uintptr) bool {
	const M = ^uint64(0)
	return b != 0 && uint64(a) > M/uint64(b)
}

// Builds a zeroed C buffer for an aggregate and initializes it from a literal.
// Returns (ptr, cleanup), freeing nested temporaries before the main buffer.
func autoBoxAggregateLiteral(reg *ffiRegistry, agg *ffiType, lit Value) (unsafe.Pointer, func(), error) {
	// size
	var sz uintptr
	switch agg.Kind {
	case ffiStruct, ffiUnion:
		sz = agg.Size
	case ffiArray:
		el := reg.mustGet(agg.Elem)
		if agg.Len >= 0 {
			if mulOvf(uintptr(agg.Len), el.Size) {
				return nil, nil, fmt.Errorf("size overflow")
			}
			sz = uintptr(agg.Len) * el.Size
		} else {
			if lit.Tag != VTArray {
				return nil, nil, fmt.Errorf("flexible array requires array literal")
			}
			n := len(lit.Data.(*ArrayObject).Elems)
			if mulOvf(uintptr(n), el.Size) {
				return nil, nil, fmt.Errorf("size overflow")
			}
			sz = uintptr(n) * el.Size
		}
	default:
		return nil, nil, fmt.Errorf("internal: not an aggregate")
	}
	if sz == 0 {
		sz = 1
	}
	if sz > autoBoxMaxAlloc {
		return nil, nil, fmt.Errorf("allocation exceeds cap")
	}
	base := cMalloc(sz)
	if base == nil {
		return nil, nil, fmt.Errorf("out of memory")
	}
	cMemset(base, 0, sz)
	var nested []func()
	cleanup := func() {
		for i := len(nested) - 1; i >= 0; i-- {
			nested[i]()
		}
		cFree(base)
	}

	// Write one slot (scalar/pointer or nested aggregate-by-value)
	var writeAgg func(t *ffiType, dst unsafe.Pointer, v Value) error
	writeSlot := func(t *ffiType, dst unsafe.Pointer, v Value) error {
		switch t.Kind {
		case ffiPointer, ffiHandle:
			if v.Tag == VTNull {
				*(*unsafe.Pointer)(dst) = nil
				return nil
			}
			if isCharPtr(reg, t) && v.Tag == VTStr {
				cs := C.CString(v.Data.(string))
				*(*unsafe.Pointer)(dst) = unsafe.Pointer(cs)
				nested = append(nested, func() { C.free(unsafe.Pointer(cs)) })
				return nil
			}
			if v.Tag != VTHandle {
				return fmt.Errorf("expected pointer handle")
			}
			*(*unsafe.Pointer)(dst) = expectPtr(v)
			return nil
		case ffiInt, ffiFloat, ffiEnum:
			writeValue(reg, t, dst, v)
			return nil
		default:
			return writeAgg(t, dst, v)
		}
	}

	writeAgg = func(t *ffiType, dst unsafe.Pointer, v Value) error {
		switch t.Kind {
		case ffiStruct:
			if v.Tag != VTMap {
				return fmt.Errorf("struct literal must be a map")
			}
			m := v.Data.(*MapObject)
			seen := make(map[string]struct{}, len(t.Fields))
			for _, f := range t.Fields {
				seen[f.Name] = struct{}{}
			}
			for _, k := range m.Keys {
				if _, ok := seen[k]; !ok {
					return fmt.Errorf("unknown field %q", k)
				}
			}
			for i, f := range t.Fields {
				if f.Bits != 0 {
					return fmt.Errorf("bitfields not supported")
				}
				val, ok := m.Entries[f.Name]
				if !ok {
					continue
				}
				ft := reg.mustGet(f.Type)
				if err := writeSlot(ft, unsafe.Pointer(uintptr(dst)+t.Offsets[i]), val); err != nil {
					return fmt.Errorf("field %s: %w", f.Name, err)
				}
			}
			return nil
		case ffiUnion:
			if v.Tag != VTMap {
				return fmt.Errorf("union literal must be a map with exactly one member")
			}
			m := v.Data.(*MapObject)
			if len(m.Keys) != 1 {
				return fmt.Errorf("union literal must set exactly one member")
			}
			k := m.Keys[0]
			var ft *ffiType
			ok := false
			for _, f := range t.Fields {
				if f.Name == k {
					ft, ok = reg.mustGet(f.Type), true
					break
				}
			}
			if !ok {
				return fmt.Errorf("unknown union member %q", k)
			}
			return writeSlot(ft, dst, m.Entries[k])
		case ffiArray:
			el := reg.mustGet(t.Elem)
			switch v.Tag {
			case VTArray:
				arr := v.Data.(*ArrayObject).Elems
				if t.Len >= 0 && len(arr) > t.Len {
					return fmt.Errorf("array literal length %d exceeds %d", len(arr), t.Len)
				}
				limit := len(arr)
				if t.Len >= 0 && t.Len < limit {
					limit = t.Len
				}
				for i := 0; i < limit; i++ {
					if err := writeSlot(el, unsafe.Pointer(uintptr(dst)+uintptr(i)*el.Size), arr[i]); err != nil {
						return fmt.Errorf("index %d: %w", i, err)
					}
				}
				return nil
			case VTMap:
				m := v.Data.(*MapObject)
				for _, k := range m.Keys {
					var idx int
					if _, err := fmt.Sscanf(k, "%d", &idx); err != nil || idx < 0 {
						return fmt.Errorf("array index %q must be non-negative integer", k)
					}
					if t.Len >= 0 && idx >= t.Len {
						return fmt.Errorf("index %d out of range (len=%d)", idx, t.Len)
					}
					if err := writeSlot(el, unsafe.Pointer(uintptr(dst)+uintptr(idx)*el.Size), m.Entries[k]); err != nil {
						return fmt.Errorf("index %d: %w", idx, err)
					}
				}
				return nil
			}
			return fmt.Errorf("array literal must be an array or index map")
		}
		return fmt.Errorf("internal: not an aggregate")
	}

	if err := writeAgg(agg, base, lit); err != nil {
		cleanup()
		return nil, nil, err
	}
	return base, cleanup, nil
}

// Range & numeric conversions

func mustIntRange(v Value, t *ffiType) int64 {
	switch v.Tag {
	case VTBool:
		if v.Data.(bool) {
			return 1
		}
		return 0
	case VTInt:
		x := v.Data.(int64)
		// early signedness check
		if !t.Signed && x < 0 {
			fail("negative to unsigned")
		}
		min, max := intRange(t)
		if x < min || x > max {
			fail("integer out of range")
		}
		return x
	case VTNum:
		f := v.Data.(float64)
		if f != float64(int64(f)) {
			fail("non-integral float to int")
		}
		return mustIntRange(Int(int64(f)), t)
	default:
		fail("expected integer-compatible")
	}
	return 0
}

func intRange(t *ffiType) (int64, int64) {
	switch t.Bits {
	case 8:
		if t.Signed {
			return -128, 127
		} else {
			return 0, 255
		}
	case 16:
		if t.Signed {
			return -32768, 32767
		} else {
			return 0, 65535
		}
	case 32:
		if t.Signed {
			return -2147483648, 2147483647
		} else {
			return 0, 4294967295
		}
	case 64:
		if t.Signed {
			return -9223372036854775808, 9223372036854775807
		} else {
			return 0, 9223372036854775807
		}
	}
	return 0, 0
}
func mustFloat(v Value) float64 {
	switch v.Tag {
	case VTNum:
		return v.Data.(float64)
	case VTInt:
		return float64(v.Data.(int64))
	case VTBool:
		if v.Data.(bool) {
			return 1
		}
		return 0
	default:
		fail("expected number")
	}
	return 0
}

// Single-point write/read used by calls and variables
func writeValue(reg *ffiRegistry, t *ffiType, dst unsafe.Pointer, v Value) {
	switch t.Kind {
	case ffiInt:
		x := mustIntRange(v, t)
		switch t.Bits {
		case 8:
			*(*C.schar)(dst) = C.schar(x)
		case 16:
			*(*C.short)(dst) = C.short(x)
		case 32:
			*(*C.int)(dst) = C.int(x)
		case 64:
			*(*C.longlong)(dst) = C.longlong(x)
		}
	case ffiFloat:
		switch t.Bits {
		case 32:
			*(*C.float)(dst) = C.float(mustFloat(v))
		case 64:
			*(*C.double)(dst) = C.double(mustFloat(v))
		}
	case ffiEnum:
		base := reg.mustGet(t.EnumBase)
		writeValue(reg, base, dst, v)
	case ffiPointer, ffiHandle:
		if v.Tag == VTNull {
			*(*unsafe.Pointer)(dst) = nil
			return
		}
		*(*unsafe.Pointer)(dst) = expectPtr(v)
	default:
		fail("unsupported type (by-value aggregate)")
	}
}

func readValue(reg *ffiRegistry, t *ffiType, src unsafe.Pointer) Value {
	switch t.Kind {
	case ffiVoid:
		return Null
	case ffiInt:
		switch t.Bits {
		case 8:
			return Int(int64(*(*C.schar)(src)))
		case 16:
			return Int(int64(*(*C.short)(src)))
		case 32:
			return Int(int64(*(*C.int)(src)))
		case 64:
			return Int(int64(*(*C.longlong)(src)))
		}
	case ffiFloat:
		switch t.Bits {
		case 32:
			return Num(float64(*(*C.float)(src)))
		case 64:
			return Num(float64(*(*C.double)(src)))
		}
	case ffiEnum:
		base := reg.mustGet(t.EnumBase)
		return readValue(reg, base, src)
	case ffiPointer, ffiHandle:
		return HandleVal(canonicalPtrTagKey(reg, t), *(*unsafe.Pointer)(src))
	}
	fail("unsupported type (by-value aggregate)")
	return Null
}

func marshalArgs(ip *Interpreter, mod *ffiModule, fn *ffiFunction, ctx CallCtx) ([]unsafe.Pointer, cleanupFn) {
	n := len(fn.Params)
	argBufs := make([]unsafe.Pointer, n)
	cleanups := []func(){}
	for i, key := range fn.Params {
		pt := mod.reg.mustGet(key)
		val := ctx.Arg(fmt.Sprintf("p%d", i))
		// Auto-box: pass literal as temp buffer for pointer/handle-to-aggregate (in-only).
		if agg, ok := isPtrToAggregate(mod.reg, pt); ok && (val.Tag == VTMap || val.Tag == VTArray) {
			tmp, rel, err := autoBoxAggregateLiteral(mod.reg, agg, val)
			if err != nil {
				fail(fmt.Sprintf("ffi: %s: p%d: %s", fn.Name, i, err.Error()))
			}
			buf := C.malloc(C.size_t(unsafe.Sizeof(uintptr(0))))
			if buf == nil {
				rel()
				fail("ffi: OOM")
			}
			*(*unsafe.Pointer)(buf) = tmp
			argBufs[i] = buf
			cleanups = append(cleanups, func() { C.free(buf); rel() })
			continue
		}
		// char*/uchar* bridge for strings
		if (pt.Kind == ffiPointer || pt.Kind == ffiHandle) && isCharPtr(mod.reg, pt) && val.Tag == VTStr {
			cs := C.CString(val.Data.(string))
			buf := C.malloc(C.size_t(unsafe.Sizeof(uintptr(0))))
			if buf == nil {
				fail("ffi: OOM")
			}
			*(*unsafe.Pointer)(buf) = unsafe.Pointer(cs)
			argBufs[i] = buf
			cleanups = append(cleanups, func() { C.free(buf); C.free(unsafe.Pointer(cs)) })
			continue
		}
		// funcptr: accept MindScript callable → build libffi closure; or accept pointer handle/null.
		if pt.Kind == ffiFuncPtr {
			switch val.Tag {
			case VTNull:
				// write NULL
				buf := C.malloc(C.size_t(unsafe.Sizeof(uintptr(0))))
				if buf == nil {
					fail("ffi: OOM")
				}
				*(*unsafe.Pointer)(buf) = nil
				argBufs[i] = buf
				cleanups = append(cleanups, func() { C.free(buf) })
				continue
			case VTHandle:
				// user supplied raw function pointer
				buf := C.malloc(C.size_t(unsafe.Sizeof(uintptr(0))))
				if buf == nil {
					fail("ffi: OOM")
				}
				*(*unsafe.Pointer)(buf) = expectPtr(val)
				argBufs[i] = buf
				cleanups = append(cleanups, func() { C.free(buf) })
				continue
			default:
				// Treat as callable (MindScript function). Build a closure.
				if err := ensureFuncptrCIF(mod.reg, pt); err != nil {
					fail("ffi: callback prep: " + err.Error())
				}
				// Allocate libffi closure (+ executable entry)
				var exec unsafe.Pointer
				cl := C.ms_closure_alloc((*unsafe.Pointer)(unsafe.Pointer(&exec)))
				if cl == nil {
					fail("ffi: closure_alloc OOM")
				}
				// Package user context via cgo.Handle; pass as uintptr_t through the thunk.
				ctxObj := &cbContext{ip: ip, fn: val, typ: pt, reg: mod.reg}
				h := cgo.NewHandle(ctxObj)
				// ffi_prep_closure_loc takes void* userdata; we pass the handle value as a pointer,
				// then cast it to uintptr_t inside the C thunk.
				if st := C.ms_prep_closure_with_thunk(cl, pt.cbCIF, unsafe.Pointer(uintptr(h)), exec); st != C.FFI_OK {
					C.ms_closure_free(cl)
					h.Delete()
					fail(fmt.Sprintf("ffi: callback prep failed: %d", int(st)))
				}
				// Keep closure alive in module; free on module.close()
				mod.cbs = append(mod.cbs, cbRecord{closure: cl, handle: h})
				// Pass executable entry as the function pointer argument
				buf := C.malloc(C.size_t(unsafe.Sizeof(uintptr(0))))
				if buf == nil {
					fail("ffi: OOM")
				}
				*(*unsafe.Pointer)(buf) = exec
				argBufs[i] = buf
				cleanups = append(cleanups, func() { C.free(buf) })
				continue
			}
		}
		// allocate at least 8 bytes for scalar/pointer slot
		sz := pt.Size
		if sz < 8 {
			sz = 8
		}
		buf := C.malloc(C.size_t(sz))
		if buf == nil {
			fail("ffi: OOM")
		}
		writeValue(mod.reg, pt, buf, val)
		argBufs[i] = buf
		cleanups = append(cleanups, func() { C.free(buf) })
	}
	return argBufs, func() {
		for _, f := range cleanups {
			f()
		}
	}
}

//export msCallbackInvoke
func msCallbackInvoke(_cif *C.ffi_cif, ret unsafe.Pointer, args *unsafe.Pointer, user C.uintptr_t) {
	// Rebuild handle → context
	h := cgo.Handle(user)
	v := h.Value()
	ctx, ok := v.(*cbContext)
	if !ok || ctx == nil || ctx.typ == nil {
		return
	}
	ft := ctx.typ
	n := len(ft.Params)

	// args is void** → slice of unsafe.Pointer (each entry is a pointer to the arg storage)
	argv := (*[1<<30 - 1]unsafe.Pointer)(unsafe.Pointer(args))[:n:n]

	in := make([]Value, n)
	for i := 0; i < n; i++ {
		pt := ctx.reg.mustGet(ft.Params[i])
		in[i] = readValue(ctx.reg, pt, argv[i]) // note: no extra deref now
	}
	res := ctx.ip.Apply(ctx.fn, in)

	// Handle void return
	rt := ctx.reg.mustGet(ft.Ret)
	if rt.Kind != ffiVoid {
		writeValue(ctx.reg, rt, ret, res)
	}
}
