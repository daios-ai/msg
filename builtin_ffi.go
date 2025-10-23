//go:build linux
// +build linux

package mindscript

import (
	"fmt"
	"runtime"
	"sort"
	"strings"
	"unsafe"
)

// Convert the return buffer into a Value, honoring ret_as_str for char*/uchar*.
func unmarshalRet(mod *ffiModule, fn *ffiFunction, rbuf unsafe.Pointer) Value {
	rt := mod.reg.mustGet(fn.Ret)
	if fn.RetAsStr {
		p := *(*unsafe.Pointer)(rbuf)
		if p == nil {
			return Null
		}
		return Str(cGoString(p))
	}
	return readValue(mod.reg, rt, rbuf)
}

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
			h, e := cDlopen(lib)
			if e != nil {
				fail("ffiOpen: " + e.Error())
			}

			mod := &ffiModule{
				libName: lib,
				libH:    h,
				reg:     reg,
				funcs:   map[string]*ffiFunction{},
				vars:    map[string]*ffiVariable{},
				openLibs: []unsafe.Pointer{
					h,
				},
			}

			// ------------- dlsym functions/variables ---------------------------
			for _, f := range funDecls {
				libH, err := mod.openLib(f.Lib, lib, h)
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
				libH, err := mod.openLib(v.Lib, lib, h)
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
							cFree(f.typesVec)
							f.typesVec = nil
						}
						if f.cif != nil {
							cFree(unsafe.Pointer(f.cif))
							f.cif = nil
						}
					}
					for i := len(mod.openLibs) - 1; i >= 0; i-- {
						ptr := mod.openLibs[i]
						if ptr == nil {
							continue
						}
						if err := cDlclose(ptr); err != nil && firstErr == "" {
							firstErr = err.Error()
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
					p := cMalloc(uintptr(n))
					if p == nil {
						fail("malloc: out of memory")
					}
					return HandleVal("void*", p)
				}, "Allocate n bytes; returns a tagged pointer.")
			registerMem("free",
				[]ParamSpec{{"p", S{"id", "Any"}}}, S{"id", "Null"},
				func(ctx CallCtx) Value {
					p := expectPtr(ctx.Arg("p"))
					if !gcRunOnce(p) {
						cFree(p)
					}
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
					p := cCalloc(uintptr(c), uintptr(s))
					if p == nil {
						fail("calloc: out of memory")
					}
					return HandleVal("void*", p)
				}, "Allocate zero-initialized memory.")
			registerMem("realloc",
				[]ParamSpec{{"p", S{"id", "Any"}}, {"n", S{"id", "Int"}}}, S{"id", "Any"},
				func(ctx CallCtx) Value {
					p := expectPtr(ctx.Arg("p"))
					n := ctx.Arg("n").Data.(int64)
					if n < 0 {
						fail("realloc: negative")
					}
					q := cRealloc(p, uintptr(n))
					if q == nil && n != 0 {
						fail("realloc: out of memory")
					}
					// move any registered destructor from old to new
					if q != nil {
						gcMove(p, q)
					} else {
						gcDetach(p)
					}
					return HandleVal("void*", q)
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
						p := cMalloc(total)
						if p == nil {
							fail("new: OOM")
						}
						return HandleVal(ptrTag, p)
					}
					total := uintptr(cnt) * t.Size
					p := cMalloc(total)
					if p == nil {
						fail("new: OOM")
					}
					return HandleVal(ptrTag, p)
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
						return Str(cGoString(p))
					}
					n := lv.Data.(int64)
					if n < 0 {
						fail("string: len < 0")
					}
					return Str(cGoStringN(p, int(n)))
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
						cs := cCString(sv.Data.(string))
						defer cCStringFree(cs)
						cMemcpy(dst, cs, uintptr(n))
					default:
						src := expectPtr(sv)
						cMemcpy(dst, src, uintptr(n))
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
					cMemset(dst, byte(b), uintptr(n))
					return Null
				}, "memset with tag checks.")
			registerMem("errno",
				[]ParamSpec{{"v", S{"id", "Any"}}}, S{"id", "Int"},
				func(ctx CallCtx) Value {
					v := ctx.Arg("v")
					if v.Tag != VTNull {
						cErrnoSet(int(v.Data.(int64)))
					}
					return Int(int64(cErrnoGet()))
				}, "Get/set thread-local errno.")

			// gc(ptr, finalizer) -> ptr
			// finalizer: null | "free" | {sym, lib?}
			registerMem("gc",
				[]ParamSpec{{"p", S{"id", "Any"}}, {"finalizer", S{"id", "Any"}}}, S{"id", "Any"},
				func(ctx CallCtx) Value {
					vp := ctx.Arg("p")
					if vp.Tag != VTHandle {
						fail("gc: first argument must be a pointer handle")
					}
					h := vp.Data.(*Handle)
					p := expectPtr(vp)

					fv := ctx.Arg("finalizer")
					if fv.Tag == VTNull {
						gcDetach(p)
						runtime.SetFinalizer(h, nil)
						return vp
					}

					e := &gcEntry{}
					switch fv.Tag {
					case VTStr:
						if fv.Data.(string) != "free" {
							fail(`gc: string finalizer must be "free"`)
						}
						e.kind = gcFree
					case VTMap:
						m, ok := ffiAsMap(fv)
						if !ok {
							fail("gc: invalid finalizer map")
						}
						sym := ffiGetReqStr(m, "sym")
						lib := ffiGetStr(m, "lib", "")
						if lib == "" {
							lib = mod.libName
						}
						hlib, err := cDlopen(lib)
						if err != nil {
							fail(fmt.Sprintf("gc: %s", err.Error()))
						}
						fn, err := cDlsymClear(hlib, sym)
						if err != nil {
							_ = cDlclose(hlib)
							fail(fmt.Sprintf("gc: %s", err.Error()))
						}
						e.kind, e.fn, e.lib = gcCFunc, fn, hlib
					default:
						fail("gc: finalizer must be null, \"free\", or {sym, lib?}")
					}

					gcInstall(p, e)
					// Clear any existing finalizer before setting a new one.
					runtime.SetFinalizer(h, nil)
					runtime.SetFinalizer(h, func(_ *Handle) {
						_ = gcRunOnce(p)
						gcDetach(p)
					})
					return vp
				},
				"Attach/detach a destructor for a pointer; one-shot per allocation.",
			)

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
					func(ip2 *Interpreter, ctx CallCtx) Value {
						// ----- marshal fixed prefix -----
						argBufs, cleanup := marshalArgs(ip2, mod, fn, ctx)
						defer cleanup()

						// ----- non-variadic fast path -----
						if !fn.Variadic {
							// Build C argv (void**)
							var argvMem unsafe.Pointer
							if n := len(argBufs); n > 0 {
								argvMem = cAllocVoidPtrArray(n)
								if argvMem == nil {
									fail("ffi: OOM")
								}
								defer cFree(argvMem)
								argv := cVoidPtrSlice(argvMem, n)
								for i := 0; i < n; i++ {
									argv[i] = argBufs[i]
								}
							}
							// Ret buffer on C heap (min 8 bytes)
							rt := mod.reg.mustGet(fn.Ret)
							sz := rt.Size
							if sz < 8 {
								sz = 8
							}
							rbuf := cMalloc(sz)
							if rbuf == nil {
								fail("ffi: OOM")
							}
							defer cFree(rbuf)
							cFFICall(cif, fn.SymbolPtr, rbuf, argvMem)
							return unmarshalRet(mod, fn, rbuf)
						}

						// ----- variadic path (MindScript: final argument must be an array) -----
						vaVal := ctx.Arg(fmt.Sprintf("p%d", len(fn.Params)))
						if vaVal.Tag != VTArray {
							fail("variadic function expects final argument to be an array")
						}
						vaElems := vaVal.Data.(*ArrayObject).Elems

						// Promote varargs + buffers and collect opaque ffi_type* for each vararg
						var vaBufs []unsafe.Pointer
						var vaTypePtrs []unsafe.Pointer
						release := []func(){}
						promInt := &ffiType{Kind: ffiInt, Bits: 32, Signed: true}
						promDbl := &ffiType{Kind: ffiFloat, Bits: 64}
						for i, a := range vaElems {
							switch a.Tag {
							case VTInt, VTBool:
								buf := cMalloc(8)
								if buf == nil {
									fail("ffi: OOM")
								}
								writeValue(mod.reg, promInt, buf, a)
								vaBufs = append(vaBufs, buf)
								vaTypePtrs = append(vaTypePtrs, ffiTypeSint32Ptr())
								release = append(release, func() { cFree(buf) })
							case VTNum:
								buf := cMalloc(8)
								if buf == nil {
									fail("ffi: OOM")
								}
								writeValue(mod.reg, promDbl, buf, a)
								vaBufs = append(vaBufs, buf)
								vaTypePtrs = append(vaTypePtrs, ffiTypeDoublePtr())
								release = append(release, func() { cFree(buf) })
							case VTHandle:
								buf := cMalloc(uintptr(unsafe.Sizeof(uintptr(0))))
								if buf == nil {
									fail("ffi: OOM")
								}
								*(*unsafe.Pointer)(buf) = expectPtr(a)
								vaBufs = append(vaBufs, buf)
								vaTypePtrs = append(vaTypePtrs, ffiTypePointerPtr())
								release = append(release, func() { cFree(buf) })
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
						argvMem := cAllocVoidPtrArray(total)
						if argvMem == nil {
							fail("ffi: OOM")
						}
						defer cFree(argvMem)
						argv := cVoidPtrSlice(argvMem, total)
						copy(argv, argBufs)
						copy(argv[len(argBufs):], vaBufs)

						// Build combined ffi_type* array for ffi_prep_cif_var
						typeMem := cAllocFFITypeArray(total)
						if typeMem == nil {
							fail("ffi: OOM")
						}
						defer cFree(typeMem)
						// Fill fixed portion from registry keys
						if err := fillFFITypesFromKeys(mod.reg, typeMem, fn.Params); err != nil {
							fail("ffi: " + err.Error())
						}
						// Append vararg promoted types
						for i, ty := range vaTypePtrs {
							setFFITypeAt(typeMem, len(fn.Params)+i, ty)
						}

						// Prepare CIF (variadic) using heap-allocated cif
						rty, err := ffiReturnTypePtr(mod.reg, fn.Ret)
						if err != nil {
							fail("ffi: " + err.Error())
						}
						cifVar := cAllocCIF()
						if cifVar == nil {
							fail("ffi_prep_cif_var: OOM")
						}
						defer cFree(unsafe.Pointer(cifVar))
						if err := cFFIPrepCIFVarOpaque(cifVar, len(fn.Params), total, rty, typeMem); err != nil {
							fail(err.Error())
						}

						// Return buffer
						rt := mod.reg.mustGet(fn.Ret)
						sz := rt.Size
						if sz < 8 {
							sz = 8
						}
						rbuf := cMalloc(sz)
						if rbuf == nil {
							fail("ffi: OOM")
						}
						defer cFree(rbuf)

						cFFICall(cifVar, fn.SymbolPtr, rbuf, argvMem)
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
  • Aggregates by value are not supported yet.
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
