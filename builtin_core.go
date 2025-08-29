package mindscript

import (
	"fmt"
	"strings"
)

// Opaque handle for environment snapshots (unexported).
type envHandle struct {
	env *Env
}

// ---- core built-ins ----------------------------------------------------

func registerCoreBuiltins(ip *Interpreter) {
	ip.RegisterNative(
		"fail",
		[]ParamSpec{{Name: "message", Type: S{"unop", "?", S{"id", "Str"}}}},
		S{"id", "Null"},
		func(_ *Interpreter, ctx CallCtx) Value {
			mv := ctx.MustArg("message")
			msg := "error"
			if mv.Tag == VTStr {
				msg = mv.Data.(string)
			}
			fail(msg) // raises rtErr; no return
			return Null
		},
	)
	setBuiltinDoc(ip, "fail", `Fail: throw a runtime error (hard fault).

Params:
  message: Str? — optional message (default "error")

Returns:
  Null (never returns)`)

	// try(f: () -> Any) -> { ok: Bool, value: Any, error: Str? }
	ip.RegisterNative(
		"try",
		[]ParamSpec{{Name: "f", Type: S{"id", "Any"}}},
		S{"map"}, // returns a map with ok/value/error
		func(ip *Interpreter, ctx CallCtx) Value {
			fv := ctx.MustArg("f")
			if fv.Tag != VTFun {
				fail("try expects a function") // contractual → hard error
			}

			out := Map(map[string]Value{
				"ok":    Bool(false),
				"value": Null,
				"error": Null,
			})

			var hardErr error
			var res Value
			func() {
				defer func() {
					if r := recover(); r != nil {
						switch sig := r.(type) {
						case rtErr:
							hardErr = fmt.Errorf("%s", sig.msg)
						default:
							hardErr = fmt.Errorf("runtime panic: %v", r)
						}
					}
				}()
				res = ip.Call0(fv)
			}()

			if hardErr != nil {
				out.Data.(*MapObject).Entries["ok"] = Bool(false)
				out.Data.(*MapObject).Entries["error"] = Str(hardErr.Error())
				out.Data.(*MapObject).Entries["value"] = Null
				return out
			}

			// treat annotated-null as failure-with-message (soft)
			if res.Tag == VTNull && res.Annot != "" {
				out.Data.(*MapObject).Entries["ok"] = Bool(false)
				out.Data.(*MapObject).Entries["error"] = Str(res.Annot)
				out.Data.(*MapObject).Entries["value"] = Null
				return out
			}

			// success
			out.Data.(*MapObject).Entries["ok"] = Bool(true)
			out.Data.(*MapObject).Entries["error"] = Null
			out.Data.(*MapObject).Entries["value"] = res
			return out
		},
	)
	setBuiltinDoc(ip, "try", `Run a function and capture hard failures.

Returns:
  { ok: Bool, value: Any, error: Str? }

Notes:
  • Hard faults (e.g., division by zero, fail(...)) set ok=false and error.
  • If the function returns an annotated null, ok=false and error is that annotation.
  • On success, ok=true and value is the function's result.`)

	// typeOf(x: Any) -> Type
	ip.RegisterNative(
		"typeOf",
		[]ParamSpec{{Name: "x", Type: S{"id", "Any"}}},
		S{"id", "Type"},
		func(ip *Interpreter, ctx CallCtx) Value {
			x := ctx.MustArg("x")
			return TypeVal(ip.ValueToType(x, ctx.Env()))
		},
	)
	setBuiltinDoc(ip, "typeOf", `Return the dynamic Type of a value.

This inspects a runtime value and produces its structural Type.
Useful together with isType/isSubtype for ad-hoc validation.

Params:
  x: Any — a runtime value

Returns: Type`)

	ip.RegisterNative(
		"clone",
		[]ParamSpec{{Name: "x", Type: S{"id", "Any"}}},
		S{"id", "Any"},
		func(_ *Interpreter, ctx CallCtx) Value {
			return cloneValue(ctx.MustArg("x"))
		},
	)
	setBuiltinDoc(ip, "clone", `Clone a value (deep-copy).

For maps, preserves key order and per-key annotations. Primitive values are
returned as-is. Functions, modules, and handles are not duplicated (identity
is preserved).

Params:
  x: Any

Returns:
  Any — a structurally independent copy for arrays/maps`)

	// snapshot(_: Null) -> Any (VTHandle)
	// Returns an opaque handle to a flattened, cloned snapshot of the visible env.
	ip.RegisterNative(
		"snapshot",
		[]ParamSpec{{Name: "_", Type: S{"id", "Null"}}},
		S{"id", "Any"},
		func(_ *Interpreter, ctx CallCtx) Value {
			snap := snapshotEnv(ctx.Env())
			return Value{Tag: VTHandle, Data: &envHandle{env: snap}}
		},
	)
	setBuiltinDoc(ip, "snapshot", `Return an opaque handle to a snapshot of the current environment.

Behavior:
  • Captures a flattened view of all visible frames (inner shadows outer).
  • Values are deep-copied where applicable (arrays/maps preserve order/annotations).
  • Returned handle is opaque (VTHandle).

Params:
  _: Null

Returns:
  Any — an opaque handle (VTHandle) representing the snapshot`)

	// NOTE: Do NOT register noteGet here — it's already provided in the
	// introspection builtins. Keeping a single authoritative definition avoids
	// double registration and doc drift.

	ip.RegisterNative(
		"isType",
		[]ParamSpec{
			{Name: "x", Type: S{"id", "Any"}},
			{Name: "T", Type: S{"id", "Type"}},
		},
		S{"id", "Bool"},
		func(ip *Interpreter, ctx CallCtx) Value {
			x := ctx.MustArg("x")
			Tv := ctx.MustArg("T")
			if Tv.Tag != VTType {
				fail("isType expects a Type as second argument") // contractual → hard
			}
			return Bool(ip.IsType(x, ip.resolveTypeValue(Tv, ctx.Env()), ctx.Env()))
		},
	)
	setBuiltinDoc(ip, "isType", `Check whether a value conforms to a Type.

Params:
  x: Any   — value to check
  T: Type  — type to check against (must be a Type value, e.g. type Int)

Returns: Bool`)

	// isSubtype(A: Type, B: Type) -> Bool
	ip.RegisterNative(
		"isSubtype",
		[]ParamSpec{
			{Name: "A", Type: S{"id", "Type"}},
			{Name: "B", Type: S{"id", "Type"}},
		},
		S{"id", "Bool"},
		func(ip *Interpreter, ctx CallCtx) Value {
			Av := ctx.MustArg("A")
			Bv := ctx.MustArg("B")
			if Av.Tag != VTType || Bv.Tag != VTType {
				fail("isSubtype expects Types as both arguments") // contractual → hard
			}
			A := ip.resolveTypeValue(Av, ctx.Env())
			B := ip.resolveTypeValue(Bv, ctx.Env())
			return Bool(ip.IsSubtype(A, B, ctx.Env()))
		},
	)
	setBuiltinDoc(ip, "isSubtype", `Structural subtype test: A <: B.

Function types are compared structurally:
- Parameters are contravariant
- Return types are covariant
- Arrows associate to the right (A -> B -> C == A -> (B -> C))

Params:
  A: Type — candidate subtype
  B: Type — candidate supertype

Returns: Bool`)

	// import(path: Str) -> Module
	ip.RegisterNative(
		"import",
		[]ParamSpec{{Name: "path", Type: S{"id", "Str"}}},
		S{"id", "Any"},
		func(ip *Interpreter, ctx CallCtx) Value {
			pv := ctx.MustArg("path")
			if pv.Tag != VTStr {
				fail("import expects a string path")
			}
			importer := ""
			if n := len(ip.loadStack); n > 0 {
				importer = ip.loadStack[n-1]
			}
			mod, err := ip.importFile(pv.Data.(string), importer)
			if err != nil {
				msg := err.Error()
				if strings.HasPrefix(strings.ToLower(msg), "parse error in ") ||
					strings.Contains(strings.ToLower(msg), "parse error") {
					fail(msg) // hard
				}
				return annotNull(msg) // soft
			}
			return mod
		},
	)

	// importCode(name: Str, src: Str) -> Module
	ip.RegisterNative(
		"importCode",
		[]ParamSpec{
			{Name: "name", Type: S{"id", "Str"}},
			{Name: "src", Type: S{"id", "Str"}},
		},
		S{"id", "Any"},
		func(ip *Interpreter, ctx CallCtx) Value {
			nv := ctx.MustArg("name")
			sv := ctx.MustArg("src")
			if nv.Tag != VTStr || sv.Tag != VTStr {
				fail("importCode expects (name: Str, src: Str)")
			}

			name := nv.Data.(string)
			src := sv.Data.(string)

			modVal, err := ip.importCode("mem:"+name, src)
			if err != nil {
				msg := err.Error()
				lc := strings.ToLower(msg)
				if strings.HasPrefix(lc, "parse error in ") || strings.Contains(lc, "parse error") {
					fail(msg) // hard
				}
				return annotNull(msg) // soft
			}
			return modVal
		},
	)

	// mapHas(obj: {}, key: Str) -> Bool
	ip.RegisterNative(
		"mapHas",
		[]ParamSpec{{Name: "obj", Type: S{"map"}}, {Name: "key", Type: S{"id", "Str"}}},
		S{"id", "Bool"},
		func(_ *Interpreter, ctx CallCtx) Value {
			v := ctx.MustArg("obj")
			k := ctx.MustArg("key").Data.(string)
			if v.Tag != VTMap {
				fail("mapHas expects a map") // contractual → hard
			}
			mo := v.Data.(*MapObject)
			_, ok := mo.Entries[k]
			return Bool(ok)
		},
	)
	setBuiltinDoc(ip, "mapHas", `Return true if a key exists in a map.

Params:
  obj: {}  — a map value
  key: Str — property name

Returns:
  Bool`)

	// mapDelete(obj: {}, key: Str) -> {}
	ip.RegisterNative(
		"mapDelete",
		[]ParamSpec{{Name: "obj", Type: S{"map"}}, {Name: "key", Type: S{"id", "Str"}}},
		S{"map"}, // returns the (mutated) input map
		func(_ *Interpreter, ctx CallCtx) Value {
			v := ctx.MustArg("obj")
			k := ctx.MustArg("key").Data.(string)
			if v.Tag != VTMap {
				fail("mapDelete expects a map") // contractual → hard
			}
			mo := v.Data.(*MapObject)
			if _, ok := mo.Entries[k]; !ok {
				return v // no-op
			}
			// delete from Entries
			delete(mo.Entries, k)
			// delete annotation if present
			delete(mo.KeyAnn, k)
			// remove from Keys while preserving order
			keys := mo.Keys[:0]
			for _, kk := range mo.Keys {
				if kk != k {
					keys = append(keys, kk)
				}
			}
			mo.Keys = keys
			return v
		},
	)
	setBuiltinDoc(ip, "mapDelete", `Delete a property from a map (in place).

Preserves the key order and per-key annotations for the remaining entries.

Params:
  obj: {}  — a map value (mutated)
  key: Str — property name to remove

Returns:
  {} — the same map value`)
}

// --- Deep copy & snapshot for isolated worlds --------------------------------

func cloneValue(v Value) Value {
	switch v.Tag {
	case VTNull, VTBool, VTInt, VTNum, VTStr, VTType, VTFun:
		return v
	case VTArray:
		xs := v.Data.([]Value)
		cp := make([]Value, len(xs))
		for i := range xs {
			cp[i] = cloneValue(xs[i])
		}
		return Arr(cp)
	case VTMap:
		mo := v.Data.(*MapObject)
		// Deep-copy entries
		entries := make(map[string]Value, len(mo.Entries))
		for k, vv := range mo.Entries {
			entries[k] = cloneValue(vv)
		}
		// Preserve insertion order and per-key annotations
		keys := make([]string, len(mo.Keys))
		copy(keys, mo.Keys)
		keyAnn := make(map[string]string, len(mo.KeyAnn))
		for k, ann := range mo.KeyAnn {
			keyAnn[k] = ann
		}
		return Value{
			Tag: VTMap,
			Data: &MapObject{
				Entries: entries,
				KeyAnn:  keyAnn,
				Keys:    keys,
			},
		}
	default:
		// Userdata/modules/handles are NOT copied (identity preserved).
		return v
	}
}

func snapshotEnv(e *Env) *Env {
	// Flatten chain into one level (shadowing by nearer scopes wins).
	flat := map[string]Value{}
	for cur := e; cur != nil; cur = cur.parent {
		for k, v := range cur.table {
			if _, exists := flat[k]; !exists {
				flat[k] = cloneValue(v)
			}
		}
	}
	cp := NewEnv(nil)
	for k, v := range flat {
		cp.Define(k, v)
	}
	return cp
}
