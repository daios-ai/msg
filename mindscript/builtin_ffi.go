//go:build !tinygo && (linux || darwin) && cgo

package mindscript

import (
	"fmt"
	"runtime"
	"sort"
	"strings"
	"sync"
	"unsafe"
)

// callFFI centralizes return-buffer sizing, ffi_call, and result unmarshalling.
// keep... are values/slices that must remain alive across the ccall boundary.
func callFFI(mod *ffiModule, fn *ffiFunction, cf cif, av unsafe.Pointer, keep ...interface{}) Value {
	rt := mod.reg.mustGet(fn.Ret)
	slot := int(abiSlotSize(rt))
	if slot <= 8 {
		var r8 [8]byte
		cFFICall(cf, fn.SymbolPtr, unsafe.Pointer(&r8[0]), av)
		for _, k := range keep {
			runtime.KeepAlive(k)
		}
		return unmarshalRet(mod, fn, unsafe.Pointer(&r8[0]))
	}
	rb, put := getBytes(slot)
	defer put()
	cFFICall(cf, fn.SymbolPtr, unsafe.Pointer(&rb[0]), av)
	for _, k := range keep {
		runtime.KeepAlive(k)
	}
	return unmarshalRet(mod, fn, unsafe.Pointer(&rb[0]))
}

// tempCStringFromStr builds a temporary NUL-terminated copy of s on the C heap.
// NOTE: Use ONLY for C APIs that require char*/unsigned char* parameters.
func tempCStringFromStr(s string) (unsafe.Pointer, func()) {
	n := len(s) + 1
	p := cMalloc(uintptr(n))
	if p == nil {
		fail("ffi: OOM")
	}
	buf, put := getBytes(n)
	copy(buf, s)
	buf[len(s)] = 0
	cMemcpy(p, unsafe.Pointer(&buf[0]), uintptr(n))
	put()
	return p, func() { cFree(p) }
}

// ---------- pools & tiny helpers (hot path) ----------

var (
	argvPool = sync.Pool{
		New: func() any {
			b := make([]uintptr, 0, 16) // stores raw addresses (no Go pointers)
			return &b
		},
	}
	argsPool = sync.Pool{
		New: func() any {
			b := make([]uint64, 0, 16) // 8-byte ABI slots
			return &b
		},
	}
	bytePool = sync.Pool{
		New: func() any {
			b := make([]byte, 0, 1024) // small scratch buffers (ret >8, vararg agg copies, etc.)
			return &b
		},
	}
)

// getArgv returns a []uintptr of length n with capacity >= n and a put() cleanup.
func getArgv(n int) ([]uintptr, func()) {
	if n <= 0 {
		return nil, func() {}
	}
	p := argvPool.Get().(*[]uintptr)
	if cap(*p) < n {
		*p = make([]uintptr, n)
	}
	out := (*p)[:n]
	// zero out to avoid holding stale pointers
	for i := range out {
		out[i] = 0
	}
	return out, func() { argvPool.Put(p) }
}

// getArgsSlots returns a []uint64 of length n (ABI 8-byte slots) and put().
func getArgsSlots(n int) ([]uint64, func()) {
	if n <= 0 {
		return nil, func() {}
	}
	p := argsPool.Get().(*[]uint64)
	if cap(*p) < n {
		*p = make([]uint64, n)
	}
	out := (*p)[:n]
	// zeroing for safety
	for i := range out {
		out[i] = 0
	}
	return out, func() { argsPool.Put(p) }
}

// getBytes returns a []byte of length n and a put(); pooled if n<=1024, fresh otherwise.
func getBytes(n int) ([]byte, func()) {
	if n <= 0 {
		return nil, func() {}
	}
	if n <= 1024 {
		p := bytePool.Get().(*[]byte)
		if cap(*p) < n {
			*p = make([]byte, n)
		}
		out := (*p)[:n]
		for i := range out {
			out[i] = 0
		}
		return out, func() { bytePool.Put(p) }
	}
	b := make([]byte, n)
	return b, func() {}
}

// writeScalarIntoSlot writes a scalar/pointer value into an 8-byte ABI slot.
func writeScalarIntoSlot(reg *ffiRegistry, t *ffiType, slot *uint64, v Value) {
	switch t.Kind {
	case ffiInt:
		x := mustIntRange(v, t)
		switch t.Bits {
		case 8:
			*slot = (*slot &^ 0xFF) | (uint64(uint8(x)))
		case 16:
			*slot = (*slot &^ 0xFFFF) | (uint64(uint16(x)))
		case 32:
			*slot = (*slot &^ 0xFFFFFFFF) | (uint64(uint32(x)))
		case 64:
			*slot = uint64(uint64(x))
		}
	case ffiFloat:
		switch t.Bits {
		case 32:
			f := float32(mustFloat(v))
			u := *(*uint32)(unsafe.Pointer(&f))
			*slot = (*slot &^ 0xFFFFFFFF) | uint64(u)
		case 64:
			f := float64(mustFloat(v))
			u := *(*uint64)(unsafe.Pointer(&f))
			*slot = u
		}
	case ffiEnum:
		base := reg.mustGet(t.EnumBase)
		writeScalarIntoSlot(reg, base, slot, v)
	case ffiPointer, ffiHandle, ffiFuncPtr:
		if v.Tag == VTNull {
			*slot = 0
			return
		}
		p := expectPtr(v)
		*slot = uint64(uintptr(p))
	default:
		fail("unsupported scalar slot type")
	}
}

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
	// Aggregates-by-value: always boxed → return a pointer handle to copied storage.
	switch rt.Kind {
	case ffiStruct, ffiUnion, ffiArray:
		p := mustMalloc(rt.Size)
		cMemcpy(p, rbuf, rt.Size)
		// Tag handle using the canonical key for this type.
		return HandleVal(canonicalPtrTagKey(mod.reg, rt), p)
	default:
		return readValue(mod.reg, rt, rbuf)
	}
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

			// ------------- Pre-validate function signatures --------------------
			// Validate types (including rejecting flexible arrays in by-value positions)
			// BEFORE performing any dlsym, so users see type errors rather than symbol errors.
			for _, f := range funDecls {
				if _, err := ffiTypeFor(reg, f.Ret); err != nil {
					fail("ffiOpen: " + err.Error())
				}
				for i, p := range f.Params {
					if _, err := ffiTypeFor(reg, p); err != nil {
						fail(fmt.Sprintf("ffiOpen: function %s param[%d]: %v", f.Name, i, err))
					}
				}
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
			//runtime.LockOSThread()

			// ------------- dlsym functions/variables ---------------------------
			for _, f := range funDecls {
				libH, err := mod.openLib(f.Lib, lib, h)
				if err != nil {
					fail(fmt.Sprintf("ffiOpen: %s", err.Error()))
				}
				ptr, err := cDlsymClear(libH, f.Name)
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
				ptr, err := cDlsymClear(libH, v.Name)
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
					//runtime.UnlockOSThread()
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
					// free cached aggregate ffi_type allocations and callbacks
					freeModuleFFIResources(mod)
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
				}, "Read bytes from char* (NUL-terminated or fixed len).")
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
						// Go-managed C-string: avoid C.CString; ensure NUL-terminated buffer during call
						s := sv.Data.(string)
						buf, put := getBytes(len(s))
						defer put()
						copy(buf, s)
						// caller promises n bytes; if n > len(s), we still respect n and may copy trailing zeros
						cMemcpy(dst, unsafe.Pointer(&buf[0]), uintptr(n))
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

			// box(T, initMap?) -> PtrHandle
			// Allocate storage for an aggregate type and optionally initialize fields/elements.
			registerMem("box",
				[]ParamSpec{{Name: "T", Type: S{"id", "Any"}}, {Name: "init", Type: S{"id", "Any"}}},
				S{"id", "Any"},
				func(ctx CallCtx) Value {
					t := resolveTypeArg(ctx.Arg("T"))
					if !(t.Kind == ffiStruct || t.Kind == ffiUnion || t.Kind == ffiArray) {
						fail("box: aggregate type required (struct/union/array)")
					}
					if t.Kind == ffiArray && t.Len < 0 {
						fail("box: flexible arrays require explicit size; use __mem.new with count")
					}
					if t.Size == 0 {
						fail("box: type has zero size")
					}
					p := cMalloc(t.Size)
					if p == nil {
						fail("box: OOM")
					}
					// Zero-initialize
					cMemset(p, 0, t.Size)

					initv := ctx.Arg("init")
					switch t.Kind {
					case ffiStruct:
						if initv.Tag == VTMap {
							m := initv.Data.(*MapObject)
							for i, f := range t.Fields {
								if v, ok := m.Entries[f.Name]; ok {
									ft := reg.mustGet(f.Type)
									dst := unsafe.Pointer(uintptr(p) + t.Offsets[i])
									writeValue(reg, ft, dst, v)
								}
							}
						} else if initv.Tag != VTNull {
							fail("box: struct init must be a map or null")
						}
					case ffiUnion:
						if initv.Tag == VTMap {
							m := initv.Data.(*MapObject)
							var set bool
							for _, f := range t.Fields {
								if v, ok := m.Entries[f.Name]; ok {
									ft := reg.mustGet(f.Type)
									writeValue(reg, ft, p, v) // unions start at offset 0
									set = true
									break
								}
							}
							if !set && len(m.Keys) > 0 {
								fail("box: union init must specify exactly one known field")
							}
						} else if initv.Tag != VTNull {
							fail("box: union init must be a map or null")
						}
					case ffiArray:
						if initv.Tag == VTArray {
							elem := reg.mustGet(t.Elem)
							avs := initv.Data.(*ArrayObject).Elems
							n := len(avs)
							if n > t.Len {
								n = t.Len
							}
							// Only scalar/pointer elements supported for initialization.
							if elem.Kind == ffiStruct || elem.Kind == ffiUnion || elem.Kind == ffiArray {
								fail("box: array element initialization for aggregates is not supported")
							}
							for i := 0; i < n; i++ {
								dst := unsafe.Pointer(uintptr(p) + uintptr(i)*elem.Size)
								writeValue(reg, elem, dst, avs[i])
							}
						} else if initv.Tag != VTNull {
							fail("box: array init must be an array or null")
						}
					}
					return HandleVal(typeKey(reg, t), p)
				},
				"Allocate storage for an aggregate T and optionally initialize fields/elements; returns a tagged pointer.",
			)

			// getf/setf (unchanged functionality; returns handles for aggregate fields)
			registerMem("getf",
				[]ParamSpec{
					{Name: "T", Type: S{"id", "Any"}},
					{Name: "ptr", Type: S{"id", "Any"}},
					{Name: "field", Type: S{"id", "Str"}},
				},
				S{"id", "Any"},
				func(ctx CallCtx) Value {
					t := resolveTypeArg(ctx.Arg("T"))
					p := expectPtr(ctx.Arg("ptr"))
					name := ctx.Arg("field").Data.(string)
					switch t.Kind {
					case ffiStruct:
						for i, f := range t.Fields {
							if f.Name == name {
								ft := reg.mustGet(f.Type)
								src := unsafe.Pointer(uintptr(p) + t.Offsets[i])
								if ft.Kind == ffiStruct || ft.Kind == ffiUnion || ft.Kind == ffiArray {
									return HandleVal(typeKey(reg, ft), src)
								}
								return readValue(reg, ft, src)
							}
						}
						fail("getf: unknown struct field: " + name)
					case ffiUnion:
						for _, f := range t.Fields {
							if f.Name == name {
								ft := reg.mustGet(f.Type)
								if ft.Kind == ffiStruct || ft.Kind == ffiUnion || ft.Kind == ffiArray {
									return HandleVal(typeKey(reg, ft), p)
								}
								return readValue(reg, ft, p) // unions start at offset 0
							}
						}
						fail("getf: unknown union field: " + name)
					default:
						fail("getf: T must be a struct or union")
					}
					return Null
				},
				"Read a field from a struct/union instance.",
			)

			// copyAggregateFromHandle performs a byte copy from a handle's storage into dst.
			// The caller supplies the error text to preserve exact messages.
			copyAggregateFromHandle := func(t *ffiType, dst unsafe.Pointer, v Value, errText string) {
				if v.Tag != VTHandle {
					fail(errText)
				}
				src := expectPtr(v)
				cMemcpy(dst, src, t.Size)
			}

			registerMem("setf",
				[]ParamSpec{
					{Name: "T", Type: S{"id", "Any"}},
					{Name: "ptr", Type: S{"id", "Any"}},
					{Name: "field", Type: S{"id", "Str"}},
					{Name: "value", Type: S{"id", "Any"}},
				},
				S{"id", "Null"},
				func(ctx CallCtx) Value {
					t := resolveTypeArg(ctx.Arg("T"))
					p := expectPtr(ctx.Arg("ptr"))
					name := ctx.Arg("field").Data.(string)
					val := ctx.Arg("value")
					switch t.Kind {
					case ffiStruct:
						for i, f := range t.Fields {
							if f.Name == name {
								ft := reg.mustGet(f.Type)
								dst := unsafe.Pointer(uintptr(p) + t.Offsets[i])
								if ft.Kind == ffiStruct || ft.Kind == ffiUnion || ft.Kind == ffiArray {
									copyAggregateFromHandle(ft, dst, val, "setf: aggregate field requires handle to compatible storage")
									return Null
								}
								writeValue(reg, ft, dst, val)
								return Null
							}
						}
						fail("setf: unknown struct field: " + name)
					case ffiUnion:
						for _, f := range t.Fields {
							if f.Name == name {
								ft := reg.mustGet(f.Type)
								if ft.Kind == ffiStruct || ft.Kind == ffiUnion || ft.Kind == ffiArray {
									copyAggregateFromHandle(ft, p, val, "setf: aggregate field requires handle to compatible storage")
									return Null
								}
								writeValue(reg, ft, p, val) // unions start at offset 0
								return Null
							}
						}
						fail("setf: unknown union field: " + name)
					default:
						fail("setf: T must be a struct or union")
					}
					return Null
				},
				"Write a field in a struct/union instance.",
			)

			// ---- Minimal packed-bit helpers (little-endian only, x86-64/SysV) ----
			registerMem("readBits",
				[]ParamSpec{
					{Name: "ptr", Type: S{"id", "Any"}},
					{Name: "byteOffset", Type: S{"id", "Int"}},
					{Name: "bitOffset", Type: S{"id", "Int"}},
					{Name: "width", Type: S{"id", "Int"}},
					{Name: "signed", Type: S{"id", "Any"}},
				},
				S{"id", "Int"},
				func(ctx CallCtx) Value {
					p := expectPtr(ctx.Arg("ptr"))
					bo := ctx.Arg("byteOffset").Data.(int64)
					bi := ctx.Arg("bitOffset").Data.(int64)
					w := ctx.Arg("width").Data.(int64)
					if bo < 0 || bi < 0 {
						fail("readBits: offsets must be >= 0")
					}
					if w < 1 || w > 64 {
						fail("readBits: width must be in 1..64")
					}
					sgn := false
					if s := ctx.Arg("signed"); s.Tag == VTBool {
						sgn = s.Data.(bool)
					} else if s.Tag != VTNull {
						fail("readBits: signed must be bool or null")
					}

					start := int(bi % 8)
					need := int((int64(start) + w + 7) / 8) // 1..9
					if need < 1 || need > 9 {
						fail("readBits: internal length check failed")
					}

					base := unsafe.Pointer(uintptr(p) + uintptr(bo))
					var buf [9]byte
					cMemcpy(unsafe.Pointer(&buf[0]), base, uintptr(need))

					// assemble little-endian chunk
					var x uint64
					for i := 0; i < need; i++ {
						x |= uint64(buf[i]) << (8 * i)
					}
					x >>= uint(start)

					var mask uint64
					if w == 64 {
						mask = ^uint64(0)
					} else {
						mask = (uint64(1) << uint(w)) - 1
					}
					val := x & mask
					if sgn && w < 64 && ((val>>(uint(w)-1))&1) == 1 {
						val |= ^mask
					} // sign-extend
					return Int(int64(val))
				},
				"Read up to 64 bits from a packed little-endian field: readBits(ptr, byteOffset, bitOffset, width, signed?).",
			)
			registerMem("writeBits",
				[]ParamSpec{
					{Name: "ptr", Type: S{"id", "Any"}},
					{Name: "byteOffset", Type: S{"id", "Int"}},
					{Name: "bitOffset", Type: S{"id", "Int"}},
					{Name: "width", Type: S{"id", "Int"}},
					{Name: "value", Type: S{"id", "Int"}},
				},
				S{"id", "Null"},
				func(ctx CallCtx) Value {
					p := expectPtr(ctx.Arg("ptr"))
					bo := ctx.Arg("byteOffset").Data.(int64)
					bi := ctx.Arg("bitOffset").Data.(int64)
					w := ctx.Arg("width").Data.(int64)
					if bo < 0 || bi < 0 {
						fail("writeBits: offsets must be >= 0")
					}
					if w < 1 || w > 64 {
						fail("writeBits: width must be in 1..64")
					}
					v := ctx.Arg("value").Data.(int64)
					if v < 0 {
						fail("writeBits: negative value not allowed")
					}

					var mask uint64
					if w == 64 {
						mask = ^uint64(0)
					} else {
						mask = (uint64(1) << uint(w)) - 1
					}
					if uint64(v) > mask {
						fail("writeBits: value does not fit width")
					}

					start := int(bi % 8)
					need := int((int64(start) + w + 7) / 8) // 1..9
					if need < 1 || need > 9 {
						fail("writeBits: internal length check failed")
					}

					base := unsafe.Pointer(uintptr(p) + uintptr(bo))
					var buf [9]byte
					cMemcpy(unsafe.Pointer(&buf[0]), base, uintptr(need))

					var x uint64
					for i := 0; i < need; i++ {
						x |= uint64(buf[i]) << (8 * i)
					}
					field := mask << uint(start)
					x &^= field
					x |= (uint64(v) & mask) << uint(start)
					for i := 0; i < need; i++ {
						buf[i] = byte((x >> (8 * i)) & 0xFF)
					}
					cMemcpy(base, unsafe.Pointer(&buf[0]), uintptr(need))
					return Null
				},
				"Write up to 64 bits into a packed little-endian field: writeBits(ptr, byteOffset, bitOffset, width, value).",
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
				// Keep the extra validation to preserve historical error text locality.
				if fn.RetAsStr {
					rt := mod.reg.mustGet(fn.Ret)
					if !((rt.Kind == ffiPointer || rt.Kind == ffiHandle) && isCharPtr(mod.reg, rt)) {
						// exact string preserved (do not change)
						fail("ffi: " + fn.Name + ": ret_as_str valid only for char*/unsigned char*")
					}
				}

				ip.RegisterRuntimeBuiltin(
					modEnv,
					name,
					buildParamSpecs(mod, fn),
					S{"id", "Any"},
					func(ip2 *Interpreter, ctx CallCtx) Value {
						// ================== FAST HOT PATH ==================

						nFixed := len(fn.Params)

						// ---- Small stack fast path for common arities ----
						var argvSmall [8]uintptr
						var argsSmall [8]uint64

						var argv []uintptr
						var putArgv func()
						if nFixed <= len(argvSmall) {
							argv, _ = argvSmall[:nFixed], func() {}
							for i := range argv {
								argv[i] = 0
							}
						} else {
							argv, putArgv = getArgv(nFixed)
							defer putArgv()
						}

						var args []uint64
						var putArgs func()
						if nFixed <= len(argsSmall) {
							args, _ = argsSmall[:nFixed], func() {}
							for i := range args {
								args[i] = 0
							}
						} else {
							args, putArgs = getArgsSlots(nFixed)
							defer putArgs()
						}

						// buffers we must keep alive until after ffi_call (Go heap)
						var keepBytes [][]byte
						var cleanups []func() // for callback closures and temp C strings

						// ----- marshal fixed prefix -----
						for i, key := range fn.Params {
							pt := mod.reg.mustGet(key)
							val := ctx.Arg(fmt.Sprintf("p%d", i))

							// Reject aggregate literals for any aggregate parameter (pointer or by-value).
							if _, ok := isPtrToAggregate(mod.reg, pt); ok && (val.Tag == VTMap || val.Tag == VTArray) {
								fail(fmt.Sprintf("ffi: %s: p%d: aggregate parameter requires handle to compatible storage", fn.Name, i))
							}

							// char*/uchar* bridge for strings (allow embedded NUL)
							if (pt.Kind == ffiPointer || pt.Kind == ffiHandle) && isCharPtr(mod.reg, pt) && val.Tag == VTStr {
								cbuf, free := tempCStringFromStr(val.Data.(string))
								args[i] = uint64(uintptr(cbuf))
								argv[i] = uintptr(unsafe.Pointer(&args[i]))
								cleanups = append(cleanups, free)
								continue
							}

							// funcptr: accept MindScript callable → build libffi closure; or accept pointer handle/null.
							if pt.Kind == ffiFuncPtr {
								fnptr, cl := makeFuncptrArg(ip2, mod, pt, val)
								if cl != nil {
									cleanups = append(cleanups, cl)
								}
								args[i] = uint64(uintptr(fnptr))
								argv[i] = uintptr(unsafe.Pointer(&args[i]))
								continue
							}

							// By-value aggregates: require a handle to storage; copy bytes into a scratch buffer.
							if pt.Kind == ffiStruct || pt.Kind == ffiUnion || pt.Kind == ffiArray {
								if val.Tag != VTHandle {
									fail(fmt.Sprintf("ffi: %s: p%d: aggregate parameter requires handle to compatible storage", fn.Name, i))
								}
								src := expectPtr(val)
								need := int(abiSlotSize(pt)) // libffi may read full slot-size
								buf, put := getBytes(need)
								// copy only the aggregate's size (rest already zeroed)
								if pt.Size > 0 {
									cMemcpy(unsafe.Pointer(&buf[0]), src, pt.Size)
								}
								argv[i] = uintptr(unsafe.Pointer(&buf[0]))
								keepBytes = append(keepBytes, buf)
								defer put()
								continue
							}

							// Scalar / pointer
							writeScalarIntoSlot(mod.reg, pt, &args[i], val)
							argv[i] = uintptr(unsafe.Pointer(&args[i]))
						}

						// Ensure kept values survive until after ffi_call
						defer func() {
							for _, f := range cleanups {
								f()
							}
							// Keep slices alive
							for _, b := range keepBytes {
								runtime.KeepAlive(b)
							}
							runtime.KeepAlive(argv)
							runtime.KeepAlive(args)
						}()

						// ----- non-variadic fast path -----
						if !fn.Variadic {
							var av unsafe.Pointer
							if nFixed > 0 {
								av = unsafe.Pointer(&argv[0])
							}
							return callFFI(mod, fn, cif, av, args, argv)
						}

						// ----- variadic path (MindScript: final argument must be an array) -----
						vaVal := ctx.Arg(fmt.Sprintf("p%d", nFixed))
						if vaVal.Tag != VTArray {
							fail("variadic function expects final argument to be an array")
						}
						vaElems := vaVal.Data.(*ArrayObject).Elems

						// Build promoted vararg values (Go memory only; no Go pointers will go into C).
						nVA := len(vaElems)
						var vaArgsSmall [8]uint64
						var vaArgs []uint64
						var putVArgs func()
						if nVA <= len(vaArgsSmall) {
							vaArgs, _ = vaArgsSmall[:nVA], func() {}
							for i := range vaArgs {
								vaArgs[i] = 0
							}
						} else {
							vaArgs, putVArgs = getArgsSlots(nVA)
							defer putVArgs()
						}

						// Collect ffi_type* for varargs
						vaTypePtrs := make([]unsafe.Pointer, 0, nVA)
						for i, a := range vaElems {
							switch a.Tag {
							case VTInt, VTBool:
								writeScalarIntoSlot(mod.reg, promInt, &vaArgs[i], a)
								vaTypePtrs = append(vaTypePtrs, ffiTypeSint32Ptr())
							case VTNum:
								writeScalarIntoSlot(mod.reg, promDbl, &vaArgs[i], a)
								vaTypePtrs = append(vaTypePtrs, ffiTypeDoublePtr())
							case VTHandle:
								p := expectPtr(a)
								vaArgs[i] = uint64(uintptr(p))
								vaTypePtrs = append(vaTypePtrs, ffiTypePointerPtr())
							case VTNull:
								vaArgs[i] = 0
								vaTypePtrs = append(vaTypePtrs, ffiTypePointerPtr())
							default:
								fail(fmt.Sprintf("unsupported variadic arg[%d]", i))
							}
						}

						// Grow argv to hold fixed + varargs and plug pointers to our Go slots.
						total := nFixed + nVA
						if total > len(argv) {
							// Need a larger argv; copy existing fixed entries
							newArgv, put2 := getArgv(total)
							defer put2()
							copy(newArgv[:nFixed], argv[:nFixed])
							argv = newArgv
						} else {
							argv = argv[:total]
						}
						// Point varargs entries at vaArgs slots
						for j := 0; j < nVA; j++ {
							argv[nFixed+j] = uintptr(unsafe.Pointer(&vaArgs[j]))
						}

						// Build combined ffi_type* array for ffi_prep_cif_var (C memory for the array only)
						typeMem := allocPtrArray(total)
						defer cFree(typeMem)
						// Fill fixed portion from registry keys
						if err := fillFFITypesFromKeys(mod.reg, typeMem, fn.Params); err != nil {
							fail("ffi: " + err.Error())
						}
						// Append vararg promoted types
						for i, ty := range vaTypePtrs {
							setFFITypeAt(typeMem, nFixed+i, ty)
						}

						// Prepare CIF (variadic) using heap-allocated cif
						rty, err := ffiReturnTypePtr(mod.reg, fn.Ret)
						if err != nil {
							fail("ffi: " + err.Error())
						}
						cifVar := cAllocCIF()
						defer cFree(unsafe.Pointer(cifVar))
						if err := cFFIPrepCIFVarOpaque(cifVar, nFixed, total, rty, typeMem); err != nil {
							fail(err.Error())
						}

						// avalue points to Go-managed argv (void**)
						var av unsafe.Pointer
						if total > 0 {
							av = unsafe.Pointer(&argv[0])
						}

						return callFFI(mod, fn, cifVar, av, args, vaArgs, argv)
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
						switch t.Kind {
						case ffiStruct, ffiUnion, ffiArray:
							if t.Size == 0 {
								fail("ffi: get(): aggregate has zero size")
							}
							p := mustMalloc(t.Size)
							cMemcpy(p, v.SymbolPtr, t.Size)
							return HandleVal(canonicalPtrTagKey(mod.reg, t), p)
						default:
							return readValue(mod.reg, t, v.SymbolPtr)
						}
					},
				)
				// set()
				ip.RegisterRuntimeBuiltin(
					vEnv, "set",
					[]ParamSpec{{Name: "value", Type: S{"id", "Any"}}},
					S{"id", "Null"},
					func(_ *Interpreter, ctx CallCtx) Value {
						t := mod.reg.mustGet(v.Type)
						// Aggregates require a handle value whose bytes are copied into the variable.
						switch t.Kind {
						case ffiStruct, ffiUnion, ffiArray:
							if t.Size == 0 {
								fail("ffi: set(): aggregate has zero size")
							}
							copyAggregateFromHandle(t, v.SymbolPtr, ctx.Arg("value"),
								"ffi: set(): aggregate variable requires handle to compatible storage")
							return Null
						default:
							writeValue(mod.reg, t, v.SymbolPtr, ctx.Arg("value"))
							return Null
						}
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
    new, cast, string, copy, fill, errno, box, getf, setf
  • Functions are real callables (scalars/pointers); vars expose get/set/addr

Performance:
  • No per-call C allocations for argv/arg slots/return buffers in the common path.
  • Small stack fast-paths for up to 8 fixed params and short char* values.
  • sync.Pool reuse for argv/slots/byte scratch; varargs slots are in Go memory.

Limitations:
   • Bitfields in structs are not supported (use readBits/writeBits for packed fields).
   • Variadics are supported (MindScript last arg = array; C default promotions applied).
   • Aggregates: by-value params require handles; by-value returns are boxed and returned as handles.
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
