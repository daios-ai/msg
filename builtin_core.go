package mindscript

import (
	"fmt"
	"sort"
)

// ---- core built-ins ----------------------------------------------------

func registerCoreBuiltins(ip *Interpreter) {
	// panic(message?: Str) -> Null (never returns; hard runtime error)
	ip.RegisterNative(
		"panic",
		[]ParamSpec{{Name: "message", Type: S{"unop", "?", S{"id", "Str"}}}},
		S{"id", "Null"},
		func(_ *Interpreter, ctx CallCtx) Value {
			mv := ctx.Arg("message")
			msg := "error"
			if mv.Tag == VTStr {
				msg = mv.Data.(string)
			}
			fail(msg) // raises rtErr; no return
			return Null
		},
	)
	setBuiltinDoc(ip, "panic", `Fail: throw a runtime error (hard fault).

Params:
  message: Str? — optional message (default "error")

Returns:
  Null (never returns)`)

	// try(f: (Null -> Any)) -> { ok: Bool, value: Any }
	ip.RegisterNative(
		"try",
		[]ParamSpec{{Name: "f", Type: S{"binop", "->", S{"id", "Null"}, S{"id", "Any"}}}},
		S{"map", S{"pair!", S{"id", "ok"}, S{"id", "Bool"}},
			S{"pair!", S{"id", "value"}, S{"id", "Any"}}},
		func(ip *Interpreter, ctx CallCtx) Value {
			fv := ctx.Arg("f") // type-checked by runtime to be (Null -> Any)

			out := Map(map[string]Value{
				"ok":    Bool(false),
				"value": Null,
			})

			var pretty string
			var res Value
			func() {
				defer func() {
					if r := recover(); r != nil {
						switch sig := r.(type) {
						case *Error:
							// Engine error: format with carets.
							pretty = FormatError(sig)
						case rtErr:
							// Structured runtime error from VM/native.
							if sig.src != nil && sig.line > 0 && sig.col > 0 {
								e := &Error{Kind: DiagRuntime, Msg: sig.msg, Src: sig.src, Line: sig.line, Col: sig.col}
								pretty = FormatError(e)
							} else {
								pretty = sig.msg
							}
						default:
							pretty = fmt.Sprintf("runtime panic: %v", r)
						}
					}
				}()
				res = ip.Call0(fv)
			}()

			// If we recovered, 'pretty' is set — treat as failure.
			if pretty != "" {
				out.Data.(*MapObject).Entries["ok"] = Bool(false)
				out.Data.(*MapObject).Entries["value"] = annotNull(pretty)
				return out
			}

			// Success.
			out.Data.(*MapObject).Entries["ok"] = Bool(true)
			out.Data.(*MapObject).Entries["value"] = res
			return out
		},
	)
	setBuiltinDoc(ip, "try", `Run a zero-arg function and capture panics.

Signature:
  try(f: (Null -> Any)) -> { ok: Bool, value: Any }

Returns:
  { ok: Bool, value: Any }

Notes:
  • Panics (e.g., division by zero, panic(...)) set panic=true value to an
    error-annotated null.
  • Otherwise, ok=true and value is the function's result. Note the value
    could still be an error.`)

	// clone(x: Any) -> Any
	ip.RegisterNative(
		"clone",
		[]ParamSpec{{Name: "x", Type: S{"id", "Any"}}},
		S{"id", "Any"},
		func(_ *Interpreter, ctx CallCtx) Value {
			return cloneValue(ctx.Arg("x"))
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

	// snapshot(_: Null) -> {}
	// Returns a flattened map of all visible bindings (inner shadows outer).
	ip.RegisterNative(
		"snapshot",
		[]ParamSpec{{Name: "_", Type: S{"id", "Null"}}},
		S{"map"},
		func(_ *Interpreter, ctx CallCtx) Value {
			return snapshotVisibleEnvAsMap(ctx.Env())
		},
	)
	setBuiltinDoc(ip, "snapshot", `Return a map snapshot of the visible environment (including built-ins).

Behavior:
  • Captures a flattened view of the current frame and its parents (Core included).
  • Inner bindings shadow outer ones.
  • Values are deep-copied where applicable (arrays/maps preserve order & per-key annotations).
  • Variable annotations are preserved on the **values themselves** (KeyAnn is not used).

Params:
  _: Null

Returns:
  {} — map of { name: value }`)

	// typeOf(x: Any) -> Type
	ip.RegisterNative(
		"typeOf",
		[]ParamSpec{{Name: "x", Type: S{"id", "Any"}}},
		S{"id", "Type"},
		func(ip *Interpreter, ctx CallCtx) Value {
			x := ctx.Arg("x")
			return TypeValIn(ip.ValueToType(x, ctx.Env()), ctx.Env())
		},
	)
	setBuiltinDoc(ip, "typeOf", `Return the dynamic Type of a value.

This inspects a runtime value and produces its structural Type.
Useful together with isType/isSubtype for ad-hoc validation.

Params:
  x: Any — a runtime value

Returns: Type`)

	// isType(x: Any, T: Type) -> Bool
	ip.RegisterNative(
		"isType",
		[]ParamSpec{
			{Name: "x", Type: S{"id", "Any"}},
			{Name: "T", Type: S{"id", "Type"}},
		},
		S{"id", "Bool"},
		func(ip *Interpreter, ctx CallCtx) Value {
			x := ctx.Arg("x")
			Tv := ctx.Arg("T")
			if Tv.Tag != VTType {
				fail("isType expects a Type as second argument")
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
			Av := ctx.Arg("A")
			Bv := ctx.Arg("B")
			if Av.Tag != VTType || Bv.Tag != VTType {
				fail("isSubtype expects Types as both arguments")
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

	// import(path: Str) -> Module (nullable on soft failure)
	ip.RegisterNative(
		"import",
		[]ParamSpec{{Name: "path", Type: S{"id", "Str"}}},
		S{"id", "Any"},
		func(ip *Interpreter, ctx CallCtx) Value {
			pv := ctx.Arg("path")
			if pv.Tag != VTStr {
				fail("import expects path: Str")
			}
			// Use the current importer identity when available (enables relative resolution).
			importer := ""
			if n := len(ip.loadStack); n > 0 {
				importer = ip.loadStack[n-1]
			} else if ip.currentSrc != nil && ip.currentSrc.Name != "" {
				importer = ip.currentSrc.Name
			}

			v, err := ip.ImportFile(pv.Data.(string), importer)
			if err != nil {
				// HARD: preserve original diagnostic kind (Lex/Parse/Runtime/Incomplete).
				if e, ok := err.(*Error); ok {
					panic(e)
				}
				fail(err.Error())
			}
			// SOFT: resolve/fetch issues come back as annotated null with err == nil.
			return v
		},
	)
	setBuiltinDoc(ip, "import", `Load a module from filesystem or HTTP(S).

Resolution rules:
  - Files: resolve relative to the importer's directory, then CWD, then MSGPATH.
  - HTTP(S): only absolute URLs; if no extension, ".ms" is appended automatically.

Params:
  path: Str — filesystem path or absolute URL to the module (".ms" assumed if missing).

Returns:
  Module (nullable) — the loaded module value; or null with an error annotation on soft failures.`)

	// importCode(name: Str, src: Str) -> Module (nullable if user code returns annotated null)
	ip.RegisterNative(
		"importCode",
		[]ParamSpec{
			{Name: "name", Type: S{"id", "Str"}},
			{Name: "src", Type: S{"id", "Str"}},
		},
		S{"id", "Any"},
		func(ip *Interpreter, ctx CallCtx) Value {
			nv := ctx.Arg("name")
			sv := ctx.Arg("src")
			if nv.Tag != VTStr || sv.Tag != VTStr {
				fail("importCode expects (name: Str, src: Str)")
			}
			name := nv.Data.(string)
			src := sv.Data.(string)

			// The loader assigns a synthetic identity "mem:<name>" internally.
			v, err := ip.ImportCode(name, src)
			if err != nil {
				// HARD: preserve original diagnostic kind (Lex/Parse/Runtime/Incomplete).
				if e, ok := err.(*Error); ok {
					panic(e)
				}
				fail(err.Error())
			}
			// Module body may deliberately produce annotated null; propagate as-is (SOFT).
			return v
		},
	)
	setBuiltinDoc(ip, "importCode", `Evaluate source text as a module in memory.

Parses 'src' and evaluates it as a module named 'name' (no caching).
The module's environment is fresh and parented to Core.
The synthetic identity "mem:<name>" is used for cycle detection.

Params:
  name: Str — display name for diagnostics and identity ("mem:<name>").
  src:  Str — MindScript source code.

Returns:
  Module (nullable) — the created module value; or null with an error annotation if the
  user code intentionally returns a soft failure.`)

	// mapHas(obj: {}, key: Str) -> Bool
	ip.RegisterNative(
		"mapHas",
		[]ParamSpec{{Name: "obj", Type: S{"map"}}, {Name: "key", Type: S{"id", "Str"}}},
		S{"id", "Bool"},
		func(_ *Interpreter, ctx CallCtx) Value {
			v := ctx.Arg("obj")
			k := ctx.Arg("key").Data.(string)
			if v.Tag != VTMap {
				fail("mapHas expects a map")
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
			v := ctx.Arg("obj")
			k := ctx.Arg("key").Data.(string)
			if v.Tag != VTMap {
				fail("mapDelete expects a map")
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

	// --- Arrays: push/unshift/pop/shift (mutating) ------------------------

	// push(arr: [Any], v: Any) -> [Any]
	ip.RegisterNative(
		"push",
		[]ParamSpec{{Name: "arr", Type: S{"array", S{"id", "Any"}}}, {Name: "v", Type: S{"id", "Any"}}},
		S{"array", S{"id", "Any"}},
		func(_ *Interpreter, ctx CallCtx) Value {
			a := ctx.Arg("arr")
			if a.Tag != VTArray {
				fail("push expects array")
			}
			ao := a.Data.(*ArrayObject)
			ao.Elems = append(ao.Elems, ctx.Arg("v"))
			return a
		},
	)
	setBuiltinDoc(ip, "push", `Append an element to an array (in place).

Params:
  arr: [Any] — array to mutate
  v:   Any   — element to append

Returns:
  [Any] — the same array (mutated)`)

	// unshift(arr: [Any], v: Any) -> [Any]
	ip.RegisterNative(
		"unshift",
		[]ParamSpec{{Name: "arr", Type: S{"array", S{"id", "Any"}}}, {Name: "v", Type: S{"id", "Any"}}},
		S{"array", S{"id", "Any"}},
		func(_ *Interpreter, ctx CallCtx) Value {
			a := ctx.Arg("arr")
			if a.Tag != VTArray {
				fail("unshift expects array")
			}
			ao := a.Data.(*ArrayObject)
			v := ctx.Arg("v")
			ao.Elems = append([]Value{v}, ao.Elems...)
			return a
		},
	)
	setBuiltinDoc(ip, "unshift", `Prepend an element to an array (in place).

Params:
  arr: [Any] — array to mutate
  v:   Any   — element to prepend

Returns:
  [Any] — the same array (mutated)`)

	// pop(arr: [Any]) -> Any   (HARD error on empty)
	ip.RegisterNative(
		"pop",
		[]ParamSpec{{Name: "arr", Type: S{"array", S{"id", "Any"}}}},
		S{"id", "Any"},
		func(_ *Interpreter, ctx CallCtx) Value {
			a := ctx.Arg("arr")
			if a.Tag != VTArray {
				fail("pop expects array")
			}
			ao := a.Data.(*ArrayObject)
			if len(ao.Elems) == 0 {
				fail("pop on empty array")
			}
			v := ao.Elems[len(ao.Elems)-1]
			ao.Elems = ao.Elems[:len(ao.Elems)-1]
			return v
		},
	)
	setBuiltinDoc(ip, "pop", `Remove and return the last element of an array.

Errors:
  • Throws a runtime error if the array is empty.

Params:
  arr: [Any] — array to mutate

Returns:
  Any — the removed element`)

	// shift(arr: [Any]) -> Any   (HARD error on empty)
	ip.RegisterNative(
		"shift",
		[]ParamSpec{{Name: "arr", Type: S{"array", S{"id", "Any"}}}},
		S{"id", "Any"},
		func(_ *Interpreter, ctx CallCtx) Value {
			a := ctx.Arg("arr")
			if a.Tag != VTArray {
				fail("shift expects array")
			}
			ao := a.Data.(*ArrayObject)
			if len(ao.Elems) == 0 {
				fail("shift on empty array")
			}
			v := ao.Elems[0]
			ao.Elems = ao.Elems[1:]
			return v
		},
	)
	setBuiltinDoc(ip, "shift", `Remove and return the first element of an array.

Errors:
  • Throws a runtime error if the array is empty.

Params:
  arr: [Any] — array to mutate

Returns:
  Any — the removed element`)
}

// --- Deep copy & snapshot for isolated worlds --------------------------------

// cloneValue deep-copies arrays/maps (preserving per-key annotations and order)
// and **preserves Value.Annot** on the cloned container/value. No special-casing.
func cloneValue(v Value) Value {
	switch v.Tag {
	case VTNull, VTBool, VTInt, VTNum, VTStr, VTType, VTFun, VTModule, VTHandle:
		return v

	case VTArray:
		ao := v.Data.(*ArrayObject)
		cp := make([]Value, len(ao.Elems))
		for i := range ao.Elems {
			cp[i] = cloneValue(ao.Elems[i])
		}
		out := Arr(cp)
		out.Annot = v.Annot // preserve array-level annotation
		return out

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
			Tag:   VTMap,
			Data:  &MapObject{Entries: entries, KeyAnn: keyAnn, Keys: keys},
			Annot: v.Annot, // preserve map-level annotation
		}

	default:
		// Other userdata-like cases: identity (and their Annot) preserved.
		return v
	}
}

// snapshotVisibleEnvAsMap flattens the current env and parents (nearest wins)
// into a deterministic, ordered map.
//
// Annotations: we do NOT mix key-annotations with value annotations.
//   - Entries[name] = cloned value (with Value.Annot preserved ON THE VALUE)
//   - KeyAnn[name] remains empty
//   - Keys ordered inner→outer; names sorted within each frame for stability.
func snapshotVisibleEnvAsMap(e *Env) Value {
	entries := map[string]Value{}
	keyAnn := map[string]string{} // intentionally left empty
	var order []string

	for cur := e; cur != nil; cur = cur.parent {
		// Stable per-frame iteration
		names := make([]string, 0, len(cur.table))
		for k := range cur.table {
			names = append(names, k)
		}
		sort.Strings(names)

		for _, k := range names {
			if _, seen := entries[k]; seen {
				continue // inner binding already won
			}
			entries[k] = cloneValue(cur.table[k]) // preserve value annotations on the value itself
			order = append(order, k)
		}
	}

	return Value{
		Tag:  VTMap,
		Data: &MapObject{Entries: entries, KeyAnn: keyAnn, Keys: order},
	}
}
