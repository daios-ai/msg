package mindscript

import (
	"fmt"
	"strings"
)

// ---- standard built-ins ----------------------------------------------------

func registerStandardBuiltins(ip *Interpreter) {
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
		S{"id", "Any"},
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
				// Use public API; does not rely on internals.
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
			// Contractual (ParamSpec enforces Str already). Keep hard error if ever violated.
			if pv.Tag != VTStr {
				fail("import expects a string path")
			}
			importer := ""
			if n := len(ip.loadStack); n > 0 {
				importer = ip.loadStack[n-1]
			}
			mod, err := ip.importFile(pv.Data.(string), importer)
			if err != nil {
				// NEW POLICY:
				// - Parse errors → HARD (Go error via fail)
				// - Everything else (I/O, not found, runtime during init) → SOFT (annotated null)
				msg := err.Error()
				// Be robust to wording; modules.go formats as "parse error in <display>:\n..."
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
			// Contract violations → HARD.
			if nv.Tag != VTStr || sv.Tag != VTStr {
				fail("importCode expects (name: Str, src: Str)")
			}

			name := nv.Data.(string)
			src := sv.Data.(string)

			modVal, err := ip.importCode("mem:"+name, src)
			if err != nil {
				// NEW POLICY:
				// - Parse errors → HARD
				// - Runtime during module init → SOFT (annotated null)
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

}

// --- Map manipulation ----------------------------------------------------

// Map helpers (object utilities that must be native due to ordered/annotated maps).
func registerMapBuiltins(ip *Interpreter) {
	// mapHas(obj, key) -> Bool
	ip.RegisterNative(
		"mapHas",
		[]ParamSpec{{"obj", S{"id", "Any"}}, {"key", S{"id", "Str"}}},
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

Returns: Bool`)

	// mapDelete(obj, key) -> {}
	ip.RegisterNative(
		"mapDelete",
		[]ParamSpec{{"obj", S{"id", "Any"}}, {"key", S{"id", "Str"}}},
		S{"id", "Any"}, // returns the (mutated) input map
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

Returns: {} (the same map value)`)
}
