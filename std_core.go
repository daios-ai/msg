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
				fail("try expects a function")
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

			// treat annotated-null as failure-with-message
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
			Av := ctx.MustArg("A")
			Bv := ctx.MustArg("B")
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

	// import(path: Str) -> Module
	ip.RegisterNative(
		"import",
		[]ParamSpec{{Name: "path", Type: S{"id", "Str"}}},
		S{"id", "Any"},
		func(ip *Interpreter, ctx CallCtx) Value {
			pv := ctx.MustArg("path")
			if pv.Tag != VTStr {
				return annotNull("import expects a string path")
			}
			importer := ""
			if n := len(ip.loadStack); n > 0 {
				importer = ip.loadStack[n-1]
			}
			mod, err := ip.importFile(pv.Data.(string), importer)
			if err != nil {
				return annotNull(err.Error())
			}
			return mod
		},
	)
	setBuiltinDoc(ip, "import", `Load a module by URL or file path.

File search order:
  1) Directory of the importing module
  2) Current working directory
  3) MINDSCRIPT_PATH (if set)
A default extension may be added if omitted.

Params:
  path: Str — file path or URL

Returns: Module (as a value with exported bindings)`)

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
			// Type system already checks, but keep a defensive guard:
			if nv.Tag != VTStr || sv.Tag != VTStr {
				return annotNull("importCode expects (name: Str, src: Str)")
			}

			name := nv.Data.(string)
			src := sv.Data.(string)

			// Reuse the refactored entry point. This returns a VTModule Value.
			modVal, err := ip.importCode("mem:"+name, src)
			if err != nil {
				// Mirror existing UX: return annotated-null with the formatted error
				return annotNull(err.Error())
			}
			// Name was already set by importCode via importAST → buildModuleFromAST.
			return modVal
		},
	)

	setBuiltinDoc(ip, "importCode", `Evaluate a source string as a module.

	The code is executed in an isolated environment parented to Core.
	This does **not** populate the module cache; a later import(name) won’t find it.

	Params:
	  name: Str — logical module name (for diagnostics)
	  src:  Str — MindScript source code

	Returns: Module`)
}

// --- Introspection & docs ----------------------------------------------------

func registerIntrospectionBuiltins(ip *Interpreter) {
	// getEnv() -> {}  (returns a map of bindings; nearest scope wins)
	// Preserves insertion order using MapObject (inner-to-outer).
	ip.RegisterNative(
		"getEnv",
		nil,
		S{"id", "Any"}, // returns a map; keeping return as Any avoids over-constraining
		func(_ *Interpreter, ctx CallCtx) Value {
			seen := make(map[string]struct{})
			mo := &MapObject{
				Entries: map[string]Value{},
				KeyAnn:  map[string]string{},
				Keys:    []string{},
			}
			// Walk from current scope to parents; first seen (nearest) wins
			for e := ctx.Env(); e != nil; e = e.parent {
				for k, v := range e.table {
					if _, ok := seen[k]; ok {
						continue
					}
					seen[k] = struct{}{}
					mo.Entries[k] = v
					mo.Keys = append(mo.Keys, k)
				}
			}
			return Value{Tag: VTMap, Data: mo}
		},
	)
	setBuiltinDoc(ip, "getEnv", `Return the current lexical environment as a map.

The result contains bindings visible at the call site. Inner scopes shadow
outer ones. Key order reflects shadowing order (nearest first).

Returns: {Str: Any}`)

	// funInfo(f: Any) -> {params:[{name, type}], return:Type, doc:Str}
	ip.RegisterNative(
		"funInfo",
		[]ParamSpec{{Name: "f", Type: S{"id", "Any"}}},
		S{"map",
			S{"pair!", S{"str", "params"}, S{"array", S{"map",
				S{"pair!", S{"str", "name"}, S{"id", "Str"}},
				S{"pair!", S{"str", "type"}, S{"id", "Type"}},
			}}},
			S{"pair!", S{"str", "return"}, S{"id", "Type"}},
			S{"pair", S{"str", "doc"}, S{"id", "Str"}},
		},
		func(ip *Interpreter, ctx CallCtx) Value {
			fv := ctx.MustArg("f")
			callable, ok := ip.FunMeta(fv)
			if !ok {
				fail("funInfo expects a function")
			}
			ps := callable.ParamSpecs()
			params := make([]Value, 0, len(ps))
			for _, p := range ps {
				params = append(params, Map(map[string]Value{
					"name": Str(p.Name),
					"type": TypeVal(ip.ResolveType(p.Type, callable.ClosureEnv())),
				}))
			}
			doc := callable.Doc()
			if doc == "" && fv.Annot != "" {
				doc = fv.Annot
			}
			return Map(map[string]Value{
				"params": Arr(params),
				"return": TypeVal(ip.ResolveType(callable.ReturnType(), callable.ClosureEnv())),
				"doc":    Str(doc),
			})
		},
	)
	setBuiltinDoc(ip, "funInfo", `Return metadata for a function.

The "params" array lists each parameter (in order), with its name and Type.
The "return" field is the declared return Type of the function body.
Use funType to obtain the full A -> B -> C arrow chain.

Params:
  f: Any — must be a function value

Returns: {params:[{name:Str, type:Type}], return:Type, doc:Str}`)

	// funType(f: Any) -> Type
	ip.RegisterNative(
		"funType",
		[]ParamSpec{{Name: "f", Type: S{"id", "Any"}}},
		S{"id", "Type"},
		func(ip *Interpreter, ctx CallCtx) Value {
			fv := ctx.MustArg("f")
			return TypeVal(ip.ValueToType(fv, ctx.Env()))
		},
	)
	setBuiltinDoc(ip, "funType", `Return the Type of a function.

Multi-parameter function types are represented as right-associative arrows,
e.g. (x:Int, y:Int) -> Int is written as Int -> Int -> Int.

Params:
  f: Any — must be a function value

Returns: Type`)

	// typeEquals(a: Type, b: Type) -> Bool
	ip.RegisterNative(
		"typeEquals",
		[]ParamSpec{{Name: "a", Type: S{"id", "Type"}}, {Name: "b", Type: S{"id", "Type"}}},
		S{"id", "Bool"},
		func(ip *Interpreter, ctx CallCtx) Value {
			av := ctx.MustArg("a")
			bv := ctx.MustArg("b")
			A := ip.resolveTypeValue(av, ctx.Env())
			B := ip.resolveTypeValue(bv, ctx.Env())
			return Bool(equalS(A, B))
		},
	)
	setBuiltinDoc(ip, "typeEquals", `Structural equality on Types.

Resolves aliases before comparing. Arrow chains are compared structurally
and right-associatively.

Params:
  a: Type
  b: Type

Returns: Bool`)

	// typeFields(t: Type) -> [{name, type, required}]
	ip.RegisterNative(
		"typeFields",
		[]ParamSpec{{Name: "t", Type: S{"id", "Type"}}},
		S{"array", S{"map",
			S{"pair!", S{"str", "name"}, S{"id", "Str"}},
			S{"pair!", S{"str", "type"}, S{"id", "Type"}},
			S{"pair!", S{"str", "required"}, S{"id", "Bool"}},
		}},
		func(ip *Interpreter, ctx CallCtx) Value {
			tv := ctx.MustArg("t")
			t := ip.resolveTypeValue(tv, ctx.Env())
			if len(t) == 0 || t[0].(string) != "map" {
				return Arr(nil)
			}
			fs := mapTypeFields(t)
			out := make([]Value, 0, len(fs))
			for k, fi := range fs {
				out = append(out, Map(map[string]Value{
					"name":     Str(k),
					"type":     TypeVal(fi.typ),
					"required": Bool(fi.required),
				}))
			}
			return Arr(out)
		},
	)
	setBuiltinDoc(ip, "typeFields", `Return the declared fields of a map Type.

Each item includes:
  - name:     field name (Str)
  - type:     field Type (Type)
  - required: whether the field is required (Bool)

Params:
  t: Type — expected to be a map Type

Returns: [{name:Str, type:Type, required:Bool}]`)

	// arrayElemType(t: Type) -> Type?
	ip.RegisterNative(
		"arrayElemType",
		[]ParamSpec{{Name: "t", Type: S{"id", "Type"}}},
		S{"unop", "?", S{"id", "Type"}},
		func(ip *Interpreter, ctx CallCtx) Value {
			t := ip.resolveTypeValue(ctx.MustArg("t"), ctx.Env())
			if len(t) == 2 && t[0].(string) == "array" {
				return TypeVal(t[1].(S))
			}
			return Null
		},
	)
	setBuiltinDoc(ip, "arrayElemType", `If t is an array type [T], return T; otherwise null.

Params:
  t: Type

Returns: Type?`)

	// isNullable(t: Type) -> Bool
	ip.RegisterNative(
		"isNullable",
		[]ParamSpec{{Name: "t", Type: S{"id", "Type"}}},
		S{"id", "Bool"},
		func(ip *Interpreter, ctx CallCtx) Value {
			t := ip.resolveTypeValue(ctx.MustArg("t"), ctx.Env())
			return Bool(len(t) >= 3 && t[0].(string) == "unop" && t[1].(string) == "?")
		},
	)
	setBuiltinDoc(ip, "isNullable", `Return true if t is nullable (i.e., T?).

Params:
  t: Type

Returns: Bool`)

	ip.RegisterNative(
		"baseType",
		[]ParamSpec{{Name: "t", Type: S{"id", "Type"}}},
		S{"id", "Type"},
		func(ip *Interpreter, ctx CallCtx) Value {
			// Resolve aliases first, using the type value's own env if provided.
			t := ip.resolveTypeValue(ctx.MustArg("t"), ctx.Env())

			// If nullable, strip the '?' wrapper: ("unop","?", X) => X
			if len(t) >= 3 {
				if tag, ok := t[0].(string); ok && tag == "unop" {
					if op, ok := t[1].(string); ok && op == "?" {
						if inner, ok := t[2].(S); ok {
							return TypeVal(inner)
						}
					}
				}
			}

			// (Optional) keep historical behavior: Null -> Any.
			if len(t) >= 2 {
				if tag, ok := t[0].(string); ok && tag == "id" {
					if name, ok := t[1].(string); ok && name == "Null" {
						return TypeVal(S{"id", "Any"})
					}
				}
			}

			// Otherwise, return the resolved type unchanged.
			return TypeVal(t)
		},
	)
	setBuiltinDoc(ip, "baseType", `Strip nullable from T? and return the base Type.

	Params:
	  t: Type

	Returns: Type`)

	// doc(x: Any) -> Str?
	ip.RegisterNative(
		"doc",
		[]ParamSpec{{Name: "x", Type: S{"id", "Any"}}},
		S{"unop", "?", S{"id", "Str"}},
		func(ip *Interpreter, ctx CallCtx) Value {
			x := ctx.MustArg("x")
			if x.Annot == "" {
				return Null
			}
			ln := strings.SplitN(x.Annot, "\n", 2)[0]
			return Str(ln)
		},
	)
	setBuiltinDoc(ip, "doc", `Return the first line of a value's docstring, or null if absent.

Params:
  x: Any — a value (functions often carry docstrings)

Returns: Str?`)

	// help(x: Any) -> Str?
	ip.RegisterNative(
		"help",
		[]ParamSpec{{Name: "x", Type: S{"id", "Any"}}},
		S{"unop", "?", S{"id", "Str"}},
		func(ip *Interpreter, ctx CallCtx) Value {
			x := ctx.MustArg("x")
			if x.Annot == "" {
				return Null
			}
			return Str(x.Annot)
		},
	)
	setBuiltinDoc(ip, "help", `Return the full docstring attached to a value, or null if absent.

Params:
  x: Any

Returns: Str?`)
}

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

Returns: {} (the same map value)`)
}
