//go:build linux
// +build linux

package mindscript

/*
#cgo LDFLAGS: -ldl
#cgo pkg-config: libffi
#include <ffi.h>
#include <dlfcn.h>
#include <stdarg.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>

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
void* ms_dlsym_clear(void* h, const char* name, char** err) {
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
*/
import "C"

import (
	"errors"
	"fmt"
	"sort"
	"strings"
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
}

// -------- libffi thin helpers (minimal; structs-by-value/callbacks deferred) -------
type cif = *C.ffi_cif

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
	// We compute in a topo-like order; retry until nothing changes.
	// For simplicity we iterate a few times bounded by #types.
	names := make([]string, 0, len(reg.types))
	for k := range reg.types {
		names = append(names, k)
	}
	// Keep builtins stable order
	sort.Strings(names)
	changed := true
	iters := 0
	for changed && iters < 8*len(names) {
		changed = false
		iters++
		for _, k := range names {
			t := reg.types[k]
			oldS, oldA := t.Size, t.Align
			if err := computeLayoutOne(reg, t); err != nil {
				return fmt.Errorf("layout(%s): %w", k, err)
			}
			if t.Size != oldS || t.Align != oldA {
				changed = true
			}
		}
	}
	// final validation
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
// Binding symbols (dlsym) and building the MindScript module
////////////////////////////////////////////////////////////////////////////////

func registerFFIBuiltins(ip *Interpreter, target *Env) {
	ip.RegisterRuntimeBuiltin(
		target,
		"ffiOpen",
		[]ParamSpec{{Name: "spec", Type: S{"id", "Any"}}},
		S{"id", "Any"}, // Module
		func(_ *Interpreter, ctx CallCtx) Value {
			specV := ctx.Arg("spec")
			if specV.Tag != VTMap {
				fail("ffiOpen expects a map specification")
			}
			spec := specV.Data.(*MapObject)

			// ------------- Parse, normalize, layout -----------------------------
			lib, reg, funDecls, varDecls, err := parseFFISpec(spec)
			if err != nil {
				fail(err.Error())
			}

			// ------------- dlopen ----------------------------------------------
			cpath := C.CString(lib)
			defer C.free(unsafe.Pointer(cpath))
			h := C.ms_dlopen(cpath)
			if h == nil {
				fail(fmt.Sprintf("ffiOpen: dlopen(%q) failed: %s", lib, dlerr()))
			}

			mod := &ffiModule{
				libName: lib,
				libH:    unsafe.Pointer(h),
				reg:     reg,
				funcs:   map[string]*ffiFunction{},
				vars:    map[string]*ffiVariable{},
				openLibs: []unsafe.Pointer{
					unsafe.Pointer(h),
				},
			}

			// ------------- dlsym functions/variables ---------------------------
			for _, f := range funDecls {
				libH, err := mod.openLib(f.Lib, lib, unsafe.Pointer(h))
				if err != nil {
					fail(fmt.Sprintf("ffiOpen: %s", err.Error()))
				}
				ptr, err := symChecked(libH, f.Name)
				if err != nil {
					fail("ffiOpen: " + err.Error())
				}
				f.SymbolPtr = ptr
				mod.funcs[f.Name] = f
			}
			for _, v := range varDecls {
				libH, err := mod.openLib(v.Lib, lib, unsafe.Pointer(h))
				if err != nil {
					fail(fmt.Sprintf("ffiOpen: %s", err.Error()))
				}
				ptr, err := symChecked(libH, v.Name)
				if err != nil {
					fail("ffiOpen: " + err.Error())
				}
				v.SymbolPtr = ptr
				mod.vars[v.Name] = v
			}

			// ------------- Build the MindScript Module --------------------------
			modEnv := NewEnv(ip.Core)
			modMap := &MapObject{Entries: map[string]Value{}, Keys: []string{}}

			// __lib, __handle, __types
			modMap.Entries["__lib"] = Str(lib)
			modMap.Keys = append(modMap.Keys, "__lib")
			modMap.Entries["__handle"] = HandleVal("c.dl", mod.libH)
			modMap.Keys = append(modMap.Keys, "__handle")

			// Export __types as a map of name -> Handle("c.type", *ffiType)
			typeMap := &MapObject{Entries: map[string]Value{}, Keys: []string{}}
			typeNames := make([]string, 0, len(reg.types))
			for k := range reg.types {
				typeNames = append(typeNames, k)
			}
			sort.Strings(typeNames)
			for _, k := range typeNames {
				// Skip internal anon types from leaking unless referenced by name
				if strings.HasPrefix(k, "__anon_") || strings.HasPrefix(k, "__anon") || strings.HasPrefix(k, "__") {
					continue
				}
				typeMap.Entries[k] = HandleVal("c.type", reg.types[k])
				typeMap.Keys = append(typeMap.Keys, k)
			}
			modMap.Entries["__types"] = Value{Tag: VTMap, Data: typeMap}
			modMap.Keys = append(modMap.Keys, "__types")

			// close()
			ip.RegisterRuntimeBuiltin(
				modEnv,
				"close",
				[]ParamSpec{{Name: "_", Type: S{"id", "Null"}}},
				S{"id", "Null"},
				func(_ *Interpreter, _ CallCtx) Value {
					// close all opened libs once
					if len(mod.openLibs) == 0 {
						fail("ffi.close: handle already closed")
					}
					var firstErr string
					// free libffi allocations (cif + typesVec) first
					for _, f := range mod.funcs {
						if f.typesVec != nil {
							C.free(f.typesVec)
							f.typesVec = nil
						}
						if f.cif != nil {
							C.free(unsafe.Pointer(f.cif))
							f.cif = nil
						}
					}
					for i := len(mod.openLibs) - 1; i >= 0; i-- {
						ptr := mod.openLibs[i]
						if ptr == nil {
							continue
						}
						if int(C.ms_dlclose(ptr)) != 0 && firstErr == "" {
							firstErr = dlerr()
						}
					}
					mod.openLibs = nil
					mod.libH = nil
					if firstErr != "" {
						fail("ffi.close: dlclose failed: " + firstErr)
					}
					return Null
				},
			)
			if fun, err := modEnv.Get("close"); err == nil {
				modMap.Entries["close"] = fun
				modMap.Keys = append(modMap.Keys, "close")
			}

			// __mem submodule with working sizeof/alignof/offsetof/typeof
			memEnv := NewEnv(ip.Core)
			memMap := &MapObject{Entries: map[string]Value{}, Keys: []string{}}

			// Helper: resolve a TypeRef argument of either Str (name) or Handle("c.type")
			resolveTypeArg := func(v Value) *ffiType {
				switch v.Tag {
				case VTStr:
					name := v.Data.(string)
					t, ok := reg.types[name]
					if !ok {
						fail("ffi.__mem: unknown type name: " + name)
					}
					return t
				case VTHandle:
					h := v.Data.(*Handle)
					if h == nil || h.Kind != "c.type" {
						fail("ffi.__mem: expected type (name or c.type handle)")
					}
					ft, _ := h.Data.(*ffiType)
					if ft == nil {
						fail("ffi.__mem: corrupt type handle")
					}
					return ft
				case VTMap:
					// accept inline type object
					key := "__anon__typeof_inline"
					t, err := parseTypeObject(reg, key, v)
					if err != nil {
						fail("ffi.__mem.typeof: " + err.Error())
					}
					_ = reg.addType(key, t) // allow overwrite silently
					// ensure layout is computed for this ad-hoc type (and its children)
					if err := computeLayoutOne(reg, t); err != nil {
						fail("ffi.__mem.typeof: " + err.Error())
					}
					return t
				default:
					fail("ffi.__mem: expected type (name or c.type handle)")
				}
				return nil
			}

			// sizeof/alignof/offsetof/typeof
			registerMem := func(name string, params []ParamSpec, ret S, body func(CallCtx) Value, doc string) {
				ip.RegisterRuntimeBuiltin(memEnv, name, params, ret, func(_ *Interpreter, ctx CallCtx) Value { return body(ctx) })
				if f, err := memEnv.Get(name); err == nil {
					memMap.Entries[name] = f
					memMap.Keys = append(memMap.Keys, name)
				}
				setBuiltinDoc(memEnv, name, doc)
			}
			registerMem("sizeof",
				[]ParamSpec{{Name: "T", Type: S{"id", "Any"}}}, S{"id", "Int"},
				func(ctx CallCtx) Value {
					t := resolveTypeArg(ctx.Arg("T"))
					return Int(int64(t.Size))
				},
				"Return the ABI size (bytes) of a declared FFI type.",
			)
			registerMem("alignof",
				[]ParamSpec{{Name: "T", Type: S{"id", "Any"}}}, S{"id", "Int"},
				func(ctx CallCtx) Value {
					t := resolveTypeArg(ctx.Arg("T"))
					return Int(int64(t.Align))
				},
				"Return the ABI alignment (bytes) of a declared FFI type.",
			)
			registerMem("offsetof",
				[]ParamSpec{{Name: "T", Type: S{"id", "Any"}}, {Name: "field", Type: S{"id", "Str"}}},
				S{"id", "Int"},
				func(ctx CallCtx) Value {
					t := resolveTypeArg(ctx.Arg("T"))
					if t.Kind != ffiStruct {
						fail("offsetof: first arg must be a struct type")
					}
					field := ctx.Arg("field").Data.(string)
					for i, f := range t.Fields {
						if f.Name == field {
							return Int(int64(t.Offsets[i]))
						}
					}
					fail("offsetof: unknown field: " + field)
					return Null
				},
				"Return byte offset of a named field within a struct type.",
			)
			registerMem("typeof",
				[]ParamSpec{{Name: "T", Type: S{"id", "Any"}}}, S{"id", "Any"},
				func(ctx CallCtx) Value {
					t := resolveTypeArg(ctx.Arg("T"))
					return HandleVal("c.type", t)
				},
				"Normalize a type-ref (name or inline) to a c.type handle.",
			)

			// ---- Implemented __mem ops (minimal, spec-conformant) ----
			registerMem("malloc",
				[]ParamSpec{{"n", S{"id", "Int"}}}, S{"id", "Any"},
				func(ctx CallCtx) Value {
					n := ctx.Arg("n").Data.(int64)
					if n < 0 {
						fail("malloc: size < 0")
					}
					p := C.malloc(C.size_t(n))
					if p == nil {
						fail("malloc: out of memory")
					}
					return HandleVal("void*", unsafe.Pointer(p))
				}, "Allocate n bytes; returns a tagged pointer.")
			registerMem("free",
				[]ParamSpec{{"p", S{"id", "Any"}}}, S{"id", "Null"},
				func(ctx CallCtx) Value {
					p := expectPtr(ctx.Arg("p"))
					C.free(p)
					return Null
				}, "Free a pointer previously allocated.")
			registerMem("calloc",
				[]ParamSpec{{"count", S{"id", "Int"}}, {"size", S{"id", "Int"}}}, S{"id", "Any"},
				func(ctx CallCtx) Value {
					c := ctx.Arg("count").Data.(int64)
					s := ctx.Arg("size").Data.(int64)
					if c < 0 || s < 0 {
						fail("calloc: negative")
					}
					p := C.calloc(C.size_t(c), C.size_t(s))
					if p == nil {
						fail("calloc: out of memory")
					}
					return HandleVal("void*", unsafe.Pointer(p))
				}, "Allocate zero-initialized memory.")
			registerMem("realloc",
				[]ParamSpec{{"p", S{"id", "Any"}}, {"n", S{"id", "Int"}}}, S{"id", "Any"},
				func(ctx CallCtx) Value {
					p := expectPtr(ctx.Arg("p"))
					n := ctx.Arg("n").Data.(int64)
					if n < 0 {
						fail("realloc: negative")
					}
					q := C.realloc(p, C.size_t(n))
					if q == nil && n != 0 {
						fail("realloc: out of memory")
					}
					return HandleVal("void*", unsafe.Pointer(q))
				}, "Resize memory.")
			registerMem("new",
				[]ParamSpec{{"T", S{"id", "Any"}}, {"count", S{"id", "Int"}}}, S{"id", "Any"},
				func(ctx CallCtx) Value {
					t := resolveTypeArg(ctx.Arg("T"))
					cnt := ctx.Arg("count").Data.(int64)
					if cnt <= 0 {
						cnt = 1
					}
					// Tag result as "pointer to T"
					ptrTag := typeKey(reg, t)
					if t.Kind == ffiArray && t.Len < 0 {
						// Flexible array: allocate elem.Size * count (header handling left to caller)
						elem := reg.mustGet(t.Elem)
						total := uintptr(cnt) * elem.Size
						p := C.malloc(C.size_t(total))
						if p == nil {
							fail("new: OOM")
						}
						return HandleVal(ptrTag, unsafe.Pointer(p))
					}
					total := uintptr(cnt) * t.Size
					p := C.malloc(C.size_t(total))
					if p == nil {
						fail("new: OOM")
					}
					return HandleVal(ptrTag, unsafe.Pointer(p))
				}, "Allocate storage for T (optionally count).")
			registerMem("cast",
				[]ParamSpec{{"T", S{"id", "Any"}}, {"v", S{"id", "Any"}}}, S{"id", "Any"},
				func(ctx CallCtx) Value {
					t := resolveTypeArg(ctx.Arg("T"))
					p := expectPtr(ctx.Arg("v"))
					return HandleVal(canonicalPtrTagKey(reg, t), p)
				}, "Retag a pointer to T (checked cast).")
			registerMem("string",
				[]ParamSpec{{"p", S{"id", "Any"}}, {"len", S{"id", "Any"}}}, S{"id", "Str"},
				func(ctx CallCtx) Value {
					p := expectPtr(ctx.Arg("p"))
					lv := ctx.Arg("len")
					if lv.Tag == VTNull {
						return Str(C.GoString((*C.char)(p)))
					}
					n := lv.Data.(int64)
					if n < 0 {
						fail("string: len < 0")
					}
					return Str(C.GoStringN((*C.char)(p), C.int(n)))
				}, "Read UTF-8 from char* (NUL-terminated or fixed len).")
			registerMem("copy",
				[]ParamSpec{{"dst", S{"id", "Any"}}, {"src", S{"id", "Any"}}, {"n", S{"id", "Int"}}}, S{"id", "Null"},
				func(ctx CallCtx) Value {
					dst := expectPtr(ctx.Arg("dst"))
					n := ctx.Arg("n").Data.(int64)
					if n < 0 {
						fail("copy: n < 0")
					}
					sv := ctx.Arg("src")
					switch sv.Tag {
					case VTStr:
						cs := C.CString(sv.Data.(string))
						defer C.free(unsafe.Pointer(cs))
						C.memcpy(dst, unsafe.Pointer(cs), C.size_t(n))
					default:
						src := expectPtr(sv)
						C.memcpy(dst, src, C.size_t(n))
					}
					return Null
				}, "memcpy with tag checks.")
			registerMem("fill",
				[]ParamSpec{{"dst", S{"id", "Any"}}, {"byte", S{"id", "Int"}}, {"n", S{"id", "Int"}}}, S{"id", "Null"},
				func(ctx CallCtx) Value {
					dst := expectPtr(ctx.Arg("dst"))
					b := ctx.Arg("byte").Data.(int64)
					n := ctx.Arg("n").Data.(int64)
					if n < 0 || b < 0 || b > 255 {
						fail("fill: invalid arguments")
					}
					C.memset(dst, C.int(b), C.size_t(n))
					return Null
				}, "memset with tag checks.")
			registerMem("errno",
				[]ParamSpec{{"v", S{"id", "Any"}}}, S{"id", "Int"},
				func(ctx CallCtx) Value {
					loc := C.ms_errno_loc()
					v := ctx.Arg("v")
					if v.Tag != VTNull {
						*loc = C.int(v.Data.(int64))
					}
					return Int(int64(*loc))
				}, "Get/set thread-local errno.")

			// Export __mem
			modMap.Entries["__mem"] = Value{Tag: VTMap, Data: memMap}
			modMap.Keys = append(modMap.Keys, "__mem")

			// Export Functions: real callables using libffi
			for name, f := range mod.funcs {
				fn := f // capture
				// Precompute CIF (non-variadic)
				cf, tp, err := prepCIF(reg, fn.Ret, fn.Params)
				if err != nil {
					fail("ffi: " + fn.Name + ": " + err.Error())
				}
				fn.cif = cf
				fn.typesVec = tp
				cif := fn.cif
				// Validate ret_as_str early: must be char*/uchar*
				if fn.RetAsStr {
					rt := mod.reg.mustGet(fn.Ret)
					if !((rt.Kind == ffiPointer || rt.Kind == ffiHandle) && isCharPtr(mod.reg, rt)) {
						fail("ffi: " + fn.Name + ": ret_as_str valid only for char*/unsigned char*")
					}
				}

				ip.RegisterRuntimeBuiltin(
					modEnv,
					name,
					buildParamSpecs(mod, fn),
					S{"id", "Any"},
					func(_ *Interpreter, ctx CallCtx) Value {
						// ----- fixed prefix -----
						argBufs, cleanup := marshalArgs(mod, fn, ctx)
						defer cleanup()
						// ----- non-variadic fast path -----
						if !fn.Variadic {
							// Build C argv (void**)
							var argvPtr *unsafe.Pointer
							if n := len(argBufs); n > 0 {
								bytes := C.malloc(C.size_t(n) * C.size_t(unsafe.Sizeof(uintptr(0))))
								if bytes == nil {
									fail("ffi: OOM")
								}
								defer C.free(bytes)
								argv := (*[1<<30 - 1]unsafe.Pointer)(bytes)[:n:n]
								for i := 0; i < n; i++ {
									argv[i] = argBufs[i]
								}
								argvPtr = (*unsafe.Pointer)(bytes)
							}
							// Ret buffer on C heap (min 8 bytes)
							rt := mod.reg.mustGet(fn.Ret)
							sz := rt.Size
							if sz < 8 {
								sz = 8
							}
							rbuf := C.malloc(C.size_t(sz))
							if rbuf == nil {
								fail("ffi: OOM")
							}
							defer C.free(rbuf)
							C.ms_ffi_call(cif, fn.SymbolPtr, rbuf, argvPtr)
							return unmarshalRet(mod, fn, rbuf)
						}

						// ----- variadic path (MindScript: final argument must be an array) -----
						vaVal := ctx.Arg(fmt.Sprintf("p%d", len(fn.Params)))
						if vaVal.Tag != VTArray {
							fail("variadic function expects final argument to be an array")
						}
						vaElems := vaVal.Data.(*ArrayObject).Elems

						// Prepare fixed ffi_types vector
						fixedTypes := make([]*C.ffi_type, len(fn.Params))
						for i, pkey := range fn.Params {
							pt, err := ffiTypeFor(mod.reg, pkey)
							if err != nil {
								fail("ffi: " + err.Error())
							}
							fixedTypes[i] = pt
						}

						// Promote varargs + buffers and ffi_types
						var vaTypes []*C.ffi_type
						var vaBufs []unsafe.Pointer
						release := []func(){}
						promInt := &ffiType{Kind: ffiInt, Bits: 32, Signed: true}
						promDbl := &ffiType{Kind: ffiFloat, Bits: 64}
						for i, a := range vaElems {
							switch a.Tag {
							case VTInt, VTBool:
								buf := C.malloc(8)
								if buf == nil {
									fail("ffi: OOM")
								}
								writeValue(mod.reg, promInt, buf, a)
								vaBufs = append(vaBufs, buf)
								vaTypes = append(vaTypes, &C.ffi_type_sint32)
								release = append(release, func() { C.free(buf) })
							case VTNum:
								buf := C.malloc(8)
								if buf == nil {
									fail("ffi: OOM")
								}
								writeValue(mod.reg, promDbl, buf, a)
								vaBufs = append(vaBufs, buf)
								vaTypes = append(vaTypes, &C.ffi_type_double)
								release = append(release, func() { C.free(buf) })
							case VTHandle:
								buf := C.malloc(C.size_t(unsafe.Sizeof(uintptr(0))))
								if buf == nil {
									fail("ffi: OOM")
								}
								*(*unsafe.Pointer)(buf) = expectPtr(a)
								vaBufs = append(vaBufs, buf)
								vaTypes = append(vaTypes, &C.ffi_type_pointer)
								release = append(release, func() { C.free(buf) })
							default:
								fail(fmt.Sprintf("unsupported variadic arg[%d]", i))
							}
						}
						defer func() {
							for _, f := range release {
								f()
							}
						}()

						// Build argv = fixed + varargs
						total := len(argBufs) + len(vaBufs)
						argvMem := C.malloc(C.size_t(total) * C.size_t(unsafe.Sizeof(uintptr(0))))
						if argvMem == nil {
							fail("ffi: OOM")
						}
						defer C.free(argvMem)
						argv := (*[1<<30 - 1]unsafe.Pointer)(argvMem)[:total:total]
						copy(argv, argBufs)
						copy(argv[len(argBufs):], vaBufs)

						// Build combined ffi_type* array for ffi_prep_cif_var
						typeMem := C.malloc(C.size_t(total) * C.size_t(unsafe.Sizeof(uintptr(0))))
						if typeMem == nil {
							fail("ffi: OOM")
						}
						defer C.free(typeMem)
						typeVec := (*[1<<30 - 1]*C.ffi_type)(typeMem)[:total:total]
						copy(typeVec, fixedTypes)
						copy(typeVec[len(fixedTypes):], vaTypes)

						// Prepare CIF (variadic) on the stack
						rty, err := ffiTypeFor(mod.reg, fn.Ret)
						if err != nil {
							fail("ffi: " + err.Error())
						}
						var vc C.ffi_cif
						if st := C.ms_ffi_prep_cif_var(&vc, C.FFI_DEFAULT_ABI,
							C.uint(len(fixedTypes)), C.uint(total), rty, (**C.ffi_type)(typeMem)); st != C.FFI_OK {
							fail(fmt.Sprintf("ffi_prep_cif_var failed: %d", int(st)))
						}

						// Return buffer
						rt := mod.reg.mustGet(fn.Ret)
						sz := rt.Size
						if sz < 8 {
							sz = 8
						}
						rbuf := C.malloc(C.size_t(sz))
						if rbuf == nil {
							fail("ffi: OOM")
						}
						defer C.free(rbuf)

						C.ms_ffi_call(&vc, fn.SymbolPtr, rbuf, (*unsafe.Pointer)(argvMem))
						return unmarshalRet(mod, fn, rbuf)
					},
				)
				if v, err := modEnv.Get(name); err == nil {
					modMap.Entries[name] = v
					modMap.Keys = append(modMap.Keys, name)
				}
				if f.Doc != "" {
					setBuiltinDoc(modEnv, name, f.Doc)
				}
			}

			// Export Variables: object with get()/set()/addr()
			for name, v := range mod.vars {
				// build env for var object
				vEnv := NewEnv(modEnv)
				// addr()
				ip.RegisterRuntimeBuiltin(
					vEnv,
					"addr",
					[]ParamSpec{{Name: "_", Type: S{"id", "Null"}}},
					S{"id", "Any"},
					func(_ *Interpreter, _ CallCtx) Value {
						t := mod.reg.mustGet(v.Type)
						return HandleVal(canonicalPtrTagKey(mod.reg, t), v.SymbolPtr)
					},
				)
				// get()
				ip.RegisterRuntimeBuiltin(
					vEnv, "get",
					[]ParamSpec{{Name: "_", Type: S{"id", "Null"}}},
					S{"id", "Any"},
					func(_ *Interpreter, _ CallCtx) Value {
						t := mod.reg.mustGet(v.Type)
						return readValue(mod.reg, t, v.SymbolPtr)
					},
				)
				// set()
				ip.RegisterRuntimeBuiltin(
					vEnv, "set",
					[]ParamSpec{{Name: "value", Type: S{"id", "Any"}}},
					S{"id", "Null"},
					func(_ *Interpreter, ctx CallCtx) Value {
						t := mod.reg.mustGet(v.Type)
						writeValue(mod.reg, t, v.SymbolPtr, ctx.Arg("value"))
						return Null
					},
				)

				// export object map
				vm := &MapObject{Entries: map[string]Value{}, Keys: []string{}}
				for _, n := range []string{"get", "set", "addr"} {
					if f, err := vEnv.Get(n); err == nil {
						vm.Entries[n] = f
						vm.Keys = append(vm.Keys, n)
					}
				}
				modMap.Entries[name] = Value{Tag: VTMap, Data: vm}
				modMap.Keys = append(modMap.Keys, name)
			}

			// Final module
			m := &Module{
				Name: lib,
				Map:  modMap,
				Env:  modEnv,
			}
			return Value{Tag: VTModule, Data: m}
		},
	)

	setBuiltinDoc(target, "ffiOpen", `Open a shared library (ELF/SysV) and return an FFI module.

Implements:
  • Full spec parsing/validation (version/lib/types/functions/variables)
  • Type registry and ABI layout (size/align/offsetof) for common kinds
  • Symbol binding via dlsym for functions and variables
  • __mem with: sizeof, alignof, offsetof, typeof, malloc/calloc/realloc/free,
    new, cast, string, copy, fill, errno
  • Functions are real callables (scalars/pointers); vars expose get/set/addr

Limitations:
  • Aggregates by value and callbacks are not supported yet.
  • Variadics are supported (MindScript last arg = array; C default promotions applied).
`)
}

////////////////////////////////////////////////////////////////////////////////
// FFI-local helpers (prefixed to avoid collisions with project-wide helpers)
////////////////////////////////////////////////////////////////////////////////

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

// Use registry key when available; otherwise fall back to display name.
func canonicalPtrTagKey(reg *ffiRegistry, t *ffiType) string {
	// Handles/explicitly-tagged pointers: honor verbatim.
	if t.Kind == ffiHandle && t.Tag != "" {
		return t.Tag
	}
	if t.Kind == ffiPointer && t.Tag != "" {
		return t.Tag
	}
	// Prefer the stable registry key.
	if reg != nil {
		for k, v := range reg.types {
			if v == t {
				return k
			}
		}
	}
	// Pointer: use pointee key/name; Non-pointer: “pointer to t”.
	if t.Kind == ffiPointer {
		return t.To
	}
	name := t.Name
	if name == "" {
		name = "(anon)"
	}
	return name
}

// typeKey returns the registry key for t if known, otherwise t.Name (best-effort).
func typeKey(reg *ffiRegistry, t *ffiType) string {
	if reg != nil {
		for k, v := range reg.types {
			if v == t {
				return k
			}
		}
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

// -------------------- Marshalling helpers --------------------
// -------------------- Marshalling helpers (condensed) --------------------
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

// Range & numeric conversions (tiny)
// builtin_ffi.go

func mustIntRange(v Value, t *ffiType) int64 {
	switch v.Tag {
	case VTBool:
		if v.Data.(bool) {
			return 1
		}
		return 0
	case VTInt:
		x := v.Data.(int64)
		// >>> add this early branch <<<
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

func marshalArgs(mod *ffiModule, fn *ffiFunction, ctx CallCtx) ([]unsafe.Pointer, cleanupFn) {
	n := len(fn.Params)
	argBufs := make([]unsafe.Pointer, n)
	cleanups := []func(){}
	for i, key := range fn.Params {
		pt := mod.reg.mustGet(key)
		val := ctx.Arg(fmt.Sprintf("p%d", i))
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

func unmarshalRet(mod *ffiModule, fn *ffiFunction, p unsafe.Pointer) Value {
	rt := mod.reg.mustGet(fn.Ret)
	if fn.RetAsStr && (rt.Kind == ffiPointer || rt.Kind == ffiHandle) && isCharPtr(mod.reg, rt) {
		ptr := *(*unsafe.Pointer)(p)
		if ptr == nil {
			return Null // MindScript null for NULL char*
		}
		return Str(C.GoString((*C.char)(ptr)))
	}
	return readValue(mod.reg, rt, p)
}
