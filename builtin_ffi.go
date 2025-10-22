//go:build linux
// +build linux

package mindscript

/*
#cgo LDFLAGS: -ldl
#cgo pkg-config: libffi
#include <ffi.h>
#include <dlfcn.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>

// Forward declarations for helpers defined in ffi.go's C block
extern void* ms_dlopen(const char* path);
extern const char* ms_dlerror(void);
extern void* ms_dlsym(void* h, const char* name);
extern int ms_dlclose(void* h);
extern void* ms_dlsym_clear(void* h, const char* name, char** err);
extern void ms_ffi_call(ffi_cif* cif, void* fn, void* rvalue, void** avalue);
extern int ms_ffi_prep_cif_var(ffi_cif* cif, ffi_abi abi,
    unsigned int nfixedargs, unsigned int ntotalargs,
    ffi_type* rtype, ffi_type** atypes);
extern int* ms_errno_loc(void);
*/
import "C"

import (
	"fmt"
	"sort"
	"strings"
	"unsafe"
)

// registerFFIBuiltins wires the user-facing ffiOpen builtin and builds the
// returned Module with callables, variables, and the __mem toolbox.
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
				// Skip internal anonymous helpers from leaking unless referenced by name
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

			// -------------------- __mem submodule -------------------------------
			memEnv := NewEnv(ip.Core)
			memMap := &MapObject{Entries: map[string]Value{}, Keys: []string{}}

			// Helper: resolve a TypeRef argument of either Str (name) or Handle("c.type") or inline map.
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

			registerMem := func(name string, params []ParamSpec, ret S, body func(CallCtx) Value, doc string) {
				ip.RegisterRuntimeBuiltin(memEnv, name, params, ret, func(_ *Interpreter, ctx CallCtx) Value { return body(ctx) })
				if f, err := memEnv.Get(name); err == nil {
					memMap.Entries[name] = f
					memMap.Keys = append(memMap.Keys, name)
				}
				setBuiltinDoc(memEnv, name, doc)
			}

			// sizeof/alignof/offsetof/typeof
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

			// Memory helpers (raw)
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

			// Spec-driven helpers (new/cast/string/copy/fill/errno)
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

			// -------------------- Export functions ------------------------------
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
						// ----- marshal fixed prefix -----
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
					modMap.entriesPut(name, v)
				}
				if f.Doc != "" {
					setBuiltinDoc(modEnv, name, f.Doc)
				}
			}

			// -------------------- Export variables ------------------------------
			for name, v := range mod.vars {
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

// tiny helper to keep map insertion consistent
func (m *MapObject) entriesPut(k string, v Value) {
	if m.Entries == nil {
		m.Entries = map[string]Value{}
	}
	found := false
	for _, x := range m.Keys {
		if x == k {
			found = true
			break
		}
	}
	if !found {
		m.Keys = append(m.Keys, k)
	}
	m.Entries[k] = v
}
