package mindscript

// introspection.go — encode/decode, reflection, and validation for runtime S-expressions ([])
//
// FINAL SURFACE (per policy)
// --------------------------
//   IxToS(ast S) Value
//     • Convert internal AST (S) → runtime-S Value (VTArray with leading VTStr).
//     • If encoding encounters an unsupported atom, returns a SOFT error as
//       annotated-null: ["annot", ["str", <msg>], ["null"]].
//
//   IxFromS(rt Value) (S, error)
//     • Strictly decodes runtime-S shape → AST (S) only.
//     • On malformed/unsupported shapes, returns a HARD error (no partial S).
//     • Does **not** perform semantic validation (use IxValidate for that).
//
//   IxReflect(v Value) Value
//     • Return CONSTRUCTOR CODE as runtime-S (a program AST-in-[] that, when
//       evaluated, rebuilds v). Always emits **canonical modules**:
//       ["module", ["str", name], ["block", ("assign", ("decl", k), ctor)*]].
//       - Scalars/arrays/maps → literal nodes
//       - VTType → ["type", <typeAst>]
//       - Fun/oracle → ["fun"/"oracle", params, ret/out, body]
//       - Native functions → ["id", nativeName]
//       - VTHandle/unknown → SOFT error (annotated-null)
//     • All annotations are preserved by wrapping constructed node(s) with
//       ["annot", ["str", txt], node].
//
//   IxValidateS(s S) Value
//     • Strict **semantic validator** for *canonical* S-programs.
//     • Returns a **VTArray of VTMap error objects**; **empty array means success**.
//     • **Enums:** validator accepts **arbitrary expression** members (not just literals).
//       During evaluation (e.g. via IxReify), those expressions must evaluate to
//       **literal** values; consequently, the runtime type system (types.go) treats
//       enums as sets of literals.
//
//   (*Interpreter) IxReify(rt Value) (Value, error)
//     • Decodes (IxFromS) then **evaluates** (like EvalPersistent).
//     • **No validation is performed** here — call IxValidateS first if needed.
//     • HARD-fails on decode or evaluation failures.
//
// DESIGN NOTES
// ------------
// • Encoding is non-throwing; decoding is strict; validation is strict for the
//   *canonical (sugar-free)* runtime grammar (no parser sugar).
// • Value maps must NEVER contain "pair!" (requiredness is type-only).
// • Oracle 4th child is always an **expression** (not a block).
// • **Enums:** Members in canonical runtime S may be **expressions**; at evaluation
//   time they must produce **literal** values (null/bool/int/num/str/array/map). The
//   runtime type system (types.go) therefore operates on **literal** members only.
// • **Annotations are not nestable** in canonical runtime S.
// • Modules are always canonicalized (i.e. lowered) in runtime S.
// • Negative numbers encode directly as ["int",-2]/["num",-1.5].
//
// Dependencies: parser.go (type S), interpreter.go (Value API), types.go (type ASTs).
//
// Tabs for indentation per project rules.

import (
	"fmt"
)

// ==============================
// ========== PUBLIC ============
// ==============================

// IxToS converts an internal AST node (S) into a runtime S-expression ([]).
// Soft-fails with an annotated-null runtime-S if an unsupported atom is found.
func IxToS(n S) Value { return ixToS_Soft(n) }

// IxFromS validates a runtime S-expression ([]) and converts it back to S.
// Hard-fails on malformed shapes or unsupported scalar kinds.
func IxFromS(rt Value) (S, error) { return ixFromS_Strict(rt) }

// IxReflect returns CONSTRUCTOR CODE as a runtime S-expression ([]).
// See the header for mapping. Preserves annotations on v and all nested values.
// Modules are emitted in **canonical** form only.
func IxReflect(v Value) Value {
	s, ok := ixConstructorS_ForValue(v)
	if !ok {
		return rtSoftError("cannot reflect value to constructor code")
	}
	return IxToS(s) // S → runtime-S
}

// IxValidateS validates a S program.
// Returns a VTArray of VTMap error objects; empty array means success (no errors).
func IxValidateS(s S) Value {
	return ValidateCanonicalS(s)
}

// IxReify decodes a runtime-S program and evaluates it in this interpreter.
// **No validation is performed here** — call IxValidateS first if validation is required.
// HARD-fails on decode or evaluation failures (per interpreter semantics).
func (ip *Interpreter) IxReify(rt Value) (Value, error) {
	ast, err := IxFromS(rt)
	if err != nil {
		return Value{}, err
	}
	// Evaluate in Global (same as EvalPersistent).
	return ip.EvalPersistent(ast)
}

//// END_OF_PUBLIC

// ==============================
// ========== PRIVATE ===========
// ==============================

// ---------- small shared helpers ----------

// annWrapS wraps node with an ["annot", ["str", txt], node] if txt != "".
func annWrapS(txt string, node S) S {
	if txt == "" {
		return node
	}
	return S{"annot", S{"str", txt}, node}
}

// rtSoftError produces an annotated-null runtime-S.
func rtSoftError(msg string) Value {
	return Arr([]Value{
		Str("annot"),
		Arr([]Value{Str("str"), Str(msg)}),
		Arr([]Value{Str("null")}),
	})
}

// ---------- S → runtime-S (soft) ----------

func ixToS_Soft(n S) Value {
	if len(n) == 0 {
		return annotNull("empty AST node")
	}
	tag, ok := n[0].(string)
	if !ok {
		return rtSoftError("AST tag is not string")
	}
	out := make([]Value, 0, len(n))
	out = append(out, Str(tag))
	for i := 1; i < len(n); i++ {
		switch x := n[i].(type) {
		case S:
			out = append(out, ixToS_Soft(x))
		case string:
			out = append(out, Str(x))
		case int64:
			out = append(out, Int(x))
		case float64:
			out = append(out, Num(x))
		case bool:
			out = append(out, Bool(x))
		default:
			return rtSoftError(fmt.Sprintf("unsupported AST atom %T", x))
		}
	}
	return Arr(out)
}

// ---------- runtime-S → S (strict shape) ----------

func ixFromS_Strict(v Value) (S, error) {
	if v.Tag != VTArray {
		return nil, fmt.Errorf("expected VTArray, got %v", v.Tag)
	}
	xs := v.Data.(*ArrayObject).Elems
	if len(xs) == 0 {
		return nil, fmt.Errorf("empty node (missing tag)")
	}
	if xs[0].Tag != VTStr {
		return nil, fmt.Errorf("node[0] must be VTStr tag, got %v", xs[0].Tag)
	}
	tag := xs[0].Data.(string)

	out := make(S, 0, len(xs))
	out = append(out, tag)

	for i := 1; i < len(xs); i++ {
		c := xs[i]
		switch c.Tag {
		case VTArray:
			sub, err := ixFromS_Strict(c)
			if err != nil {
				return nil, err
			}
			out = append(out, sub)
		case VTStr:
			out = append(out, c.Data.(string))
		case VTInt:
			out = append(out, c.Data.(int64))
		case VTNum:
			out = append(out, c.Data.(float64))
		case VTBool:
			out = append(out, c.Data.(bool))
		default:
			return nil, fmt.Errorf("unsupported scalar tag %v", c.Tag)
		}
	}
	return out, nil
}

// ---------- Value → constructor S (annotation-preserving, canonical modules) ----------

func ixConstructorS_ForValue(v Value) (S, bool) {
	node, ok := ixConstructorS_core(v)
	if !ok {
		return nil, false
	}
	return annWrapS(v.Annot, node), true
}

func ixConstructorS_core(v Value) (S, bool) {
	switch v.Tag {
	case VTNull:
		return S{"null"}, true
	case VTBool:
		return S{"bool", v.Data.(bool)}, true
	case VTInt:
		return S{"int", v.Data.(int64)}, true
	case VTNum:
		return S{"num", v.Data.(float64)}, true
	case VTStr:
		return S{"str", v.Data.(string)}, true

	case VTArray:
		elems := v.Data.(*ArrayObject).Elems
		out := S{"array"}
		for _, e := range elems {
			se, ok := ixConstructorS_ForValue(e)
			if !ok {
				return nil, false
			}
			out = append(out, se)
		}
		return out, true

	case VTMap:
		mo := v.Data.(*MapObject)
		out := S{"map"}
		for _, k := range mo.Keys {
			val := mo.Entries[k]
			ctor, ok := ixConstructorS_ForValue(val)
			if !ok {
				return nil, false
			}
			// Value maps never carry "pair!" (type-only). Keys are plain strings.
			keyNode := S{"str", k}
			out = append(out, S{"pair", keyNode, ctor})
		}
		return out, true

	case VTFun:
		f := v.Data.(*Fun)
		if f.NativeName != "" {
			return S{"id", f.NativeName}, true
		}
		par := S{"array"}
		for i, name := range f.Params {
			var t S
			if i < len(f.ParamTypes) && f.ParamTypes[i] != nil {
				t = f.ParamTypes[i]
			} else {
				t = S{"id", "Any"}
			}
			par = append(par, S{"pair", S{"id", name}, t})
		}
		ret := f.ReturnType
		if ret == nil {
			ret = S{"id", "Any"}
		}
		if f.IsOracle {
			return S{"oracle", par, ret, f.Body}, true // sourceExpr is an expression
		}
		return S{"fun", par, ret, f.Body}, true

	case VTType:
		tv := v.Data.(*TypeValue)
		return S{"type", tv.Ast}, true

	case VTModule:
		name := ""
		if m, ok := v.Data.(*Module); ok && m != nil && m.Name != "" {
			name = m.Name
		}
		mv := AsMapValue(v)        // normalize exports
		mo := mv.Data.(*MapObject) // map view of module
		body := S{"block"}         // lowered: block of assigns
		for _, k := range mo.Keys {
			ctor, ok := ixConstructorS_ForValue(mo.Entries[k])
			if !ok {
				return nil, false
			}
			body = append(body, S{"assign", S{"decl", k}, ctor})
		}
		return S{"module", S{"str", name}, body}, true

	case VTHandle:
		return nil, false

	default:
		return nil, false
	}
}

// ==============================
// ========== VALIDATOR =========
// ==============================
//
// ValidateCanonicalS enforces the canonical runtime grammar on S (not runtime-S).
// Robustness goals:
//   - Accepts hand-built S: checks arity, tag names, and **payload kinds**
//     (e.g., ["int", <int64>], ["num", <float64>], ["str", <string>], etc.).
//   - Distinguishes value vs type contexts; value maps must use "pair" (no "pair!"),
//     type maps may use "pair!".
//   - Enum members are **arbitrary expressions** (validated as exprs), not just literals.
//   - Returns VTArray of VTMap error objects with keys: path!, code!, message!, got!, expect!.
//     Empty array means "valid".
func ValidateCanonicalS(s S) Value {
	errs := []Value{} // VTMap errors

	// ---- small helpers (local, no bloat) -----------------------------------

	push := func(path, code, msg, got, expect string) {
		errs = append(errs, Map(map[string]Value{
			"path":    Str(path),
			"code":    Str(code),
			"message": Str(msg),
			"got":     Str(got),
			"expect":  Str(expect),
		}))
	}

	tagOf := func(n any) string {
		if ss, ok := n.(S); ok && len(ss) > 0 {
			if t, ok := ss[0].(string); ok && t != "" {
				return t
			}
		}
		return "<node>"
	}
	arity := func(n S) string { return fmt.Sprintf("%d", len(n)) }

	fmtIdx := func(base, tag string, i int) string {
		if base == "" || base == "/" {
			return fmt.Sprintf("/%s[%d]", tag, i)
		}
		return fmt.Sprintf("%s/%s[%d]", base, tag, i)
	}

	expectNode := func(s string) string { return s } // readability

	// Payload-kind checks for hand-built S
	checkScalarPayload := func(path string, n S, want string) {
		ok := false
		switch want {
		case "str":
			ok = len(n) == 2
			if ok {
				_, ok = n[1].(string)
			}
		case "int":
			ok = len(n) == 2
			if ok {
				_, ok = n[1].(int64)
			}
		case "num":
			ok = len(n) == 2
			if ok {
				_, ok = n[1].(float64)
			}
		case "bool":
			ok = len(n) == 2
			if ok {
				_, ok = n[1].(bool)
			}
		case "id":
			ok = len(n) == 2
			if ok {
				_, ok = n[1].(string)
			}
		default:
			ok = true
		}
		if !ok {
			push(path, "E_PAYLOAD_KIND", "invalid payload kind for "+n[0].(string),
				arity(n)+"/"+fmt.Sprintf("%T", n[1]), want+" payload")
		}
	}

	// ["annot", ["str", text], node]; forbid nested annot child
	checkAnnot := func(n S, path string) bool {
		if len(n) != 3 {
			push(path, "E_ANNOT_ARITY", "annotation must have 3 items", arity(n), `["annot",["str",text],node]`)
			return false
		}
		strNode, ok := n[1].(S)
		if !ok || len(strNode) != 2 || tagOf(strNode) != "str" {
			push(path+"/annot[1]", "E_ANNOT_TEXT", "annotation text must be a string node", tagOf(n[1]), `["str",text]`)
			return false
		}
		if child, ok := n[2].(S); ok && tagOf(child) == "annot" {
			push(path+"/annot[2]", "E_ANNOT_NESTED", "nested annotations are not allowed", "annot", "node")
			return false
		}
		return true
	}

	// Assignable LHS (value context)
	assignableS := func(n S) bool {
		if len(n) > 0 && tagOf(n) == "annot" && len(n) == 3 {
			if inner, ok := n[2].(S); ok {
				n = inner
			}
		}
		if len(n) == 0 {
			return false
		}
		switch tagOf(n) {
		case "id", "get", "idx", "decl", "darr", "dobj":
			return true
		default:
			return false
		}
	}

	// Expression-ness (block is not an expr)
	isExpr := func(n S) bool { return len(n) > 0 && tagOf(n) != "block" }

	// Types used in fun/oracle/type nodes
	var walkType func(S, string)

	// Parameter list (for fun/oracle)
	checkParams := func(arr S, path string) {
		if len(arr) == 0 || tagOf(arr) != "array" {
			push(path, "E_PARAMS_ARRAY", "params must be an array node", tagOf(arr), expectNode(`["array", ...]`))
			return
		}
		for i := 1; i < len(arr); i++ {
			pi, ok := arr[i].(S)
			pPath := fmtIdx(path, "param", i-1)
			if !ok || len(pi) != 3 || tagOf(pi) != "pair" {
				push(pPath, "E_PARAM_PAIR", "param must be a pair(name, type)", tagOf(arr[i]), `["pair",["id",name],Type]`)
				continue
			}
			idn, ok := pi[1].(S)
			if !ok || tagOf(idn) != "id" {
				push(pPath+"/name", "E_PARAM_NAME", "param name must be an id node", tagOf(pi[1]), `["id",name]`)
			} else {
				checkScalarPayload(pPath+"/name", idn, "id")
			}
			if tt, ok := pi[2].(S); ok {
				walkType(tt, pPath+"/type")
			} else {
				push(pPath+"/type", "E_NODE_SHAPE", "type must be a node", fmt.Sprintf("%T", pi[2]), "node")
			}
		}
	}

	// Main walkers ------------------------------------------------------------

	var walk func(S, string, bool)

	// Type walker (structural)
	walkType = func(t S, path string) {
		if len(t) == 0 {
			push(path, "E_EMPTY_NODE", "empty type node", "", "tagged node")
			return
		}
		switch tagOf(t) {

		case "get":
			// Qualified type paths: ["get", base, ["str", key]]
			if len(t) != 3 {
				push(path, "E_TYPE_GET_ARITY", "qualified type must have base and key", arity(t), `3`)
				return
			}
			// base can be id|get|annot; recurse structurally so nested gets are fine
			base, ok := t[1].(S)
			if !ok {
				push(path+"/get/base", "E_NODE_SHAPE", "qualified type base must be a node", fmt.Sprintf("%T", t[1]), "node")
			} else {
				walkType(base, path+"/get/base")
			}
			key, ok := t[2].(S)
			if !ok || tagOf(key) != "str" {
				push(path+"/get/key", "E_TYPE_GET_KEY", "qualified type key must be a string node", tagOf(t[2]), `["str",key]`)
				return
			}
			checkScalarPayload(path+"/get/key", key, "str")

		case "id":
			checkScalarPayload(path, t, "id")

		case "unop":
			if len(t) != 3 || t[1] != "?" {
				push(path, "E_TYPE_UNOP", "only nullable ('?') is supported", tagOf(t), `["unop","?",T]`)
				return
			}
			if sub, ok := t[2].(S); ok {
				walkType(sub, path+`/?`)
			} else {
				push(path+`/?`, "E_NODE_SHAPE", "type must be a node", fmt.Sprintf("%T", t[2]), "node")
			}

		case "array":
			// Canonical: exactly one element type (["array", T])
			if len(t) != 2 {
				push(path, "E_TYPE_ARRAY_ELEM_ARITY", "array type requires exactly one element type", arity(t), "2")
				return
			}
			if sub, ok := t[1].(S); ok {
				walkType(sub, path+"/array")
			} else {
				push(path+"/array", "E_NODE_SHAPE", "element type must be a node", fmt.Sprintf("%T", t[1]), "node")
			}

		case "map":
			for i := 1; i < len(t); i++ {
				p, ok := t[i].(S)
				idx := fmtIdx(path, "map", i-1)
				if !ok || len(p) != 3 {
					push(idx, "E_PAIR_SHAPE", "type field must be a triple", arity(p), "3")
					continue
				}
				ptag := tagOf(p)
				if ptag != "pair" && ptag != "pair!" {
					push(idx, "E_PAIR_TAG", "type map entries must be 'pair' or 'pair!'", ptag, "pair|pair!")
					continue
				}
				key, ok := p[1].(S)
				if !ok || (tagOf(key) != "str" && tagOf(key) != "annot") {
					push(idx+"/key", "E_KEY_SHAPE", "field key must be a string (optionally annotated)", tagOf(p[1]), `["str",k] or ["annot",...]`)
					continue
				}
				// annotated key → unwrap and re-check
				if tagOf(key) == "annot" {
					if !checkAnnot(key, idx+"/key") {
						continue
					}
					key = key[2].(S)
				}
				if tagOf(key) != "str" {
					push(idx+"/key", "E_KEY_STR", "annotated key must wrap a string node", tagOf(key), `["str",k]`)
					continue
				}
				checkScalarPayload(idx+"/key", key, "str")

				// field type
				if tt, ok := p[2].(S); ok {
					walkType(tt, idx+"/type")
				} else {
					push(idx+"/type", "E_NODE_SHAPE", "field type must be a node", fmt.Sprintf("%T", p[2]), "node")
				}
			}

		case "enum":
			// NEW: enum members are arbitrary **expressions**, validated as exprs.
			for i := 1; i < len(t); i++ {
				member, ok := t[i].(S)
				mPath := fmtIdx(path, "enum", i-1)
				if !ok {
					push(mPath, "E_NODE_SHAPE", "enum member must be a node", fmt.Sprintf("%T", t[i]), "node")
					continue
				}
				// Must be an expression (not a block)
				if !isExpr(member) {
					push(mPath, "E_ENUM_EXPR", "enum member must be an expression node", tagOf(member), "expr")
					continue
				}
				walk(member, mPath, false)
			}

		case "binop":
			if len(t) != 4 || t[1] != "->" {
				push(path, "E_TYPE_BINOP", "function type must be ['binop','->',A,B]", tagOf(t), `["binop","->",A,B]`)
				return
			}
			if a, ok := t[2].(S); ok {
				walkType(a, path+"/->/A")
			} else {
				push(path+"/->/A", "E_NODE_SHAPE", "A must be a type node", fmt.Sprintf("%T", t[2]), "node")
			}
			if b, ok := t[3].(S); ok {
				walkType(b, path+"/->/B")
			} else {
				push(path+"/->/B", "E_NODE_SHAPE", "B must be a type node", fmt.Sprintf("%T", t[3]), "node")
			}

		case "annot":
			if !checkAnnot(t, path) {
				return
			}
			if sub, ok := t[2].(S); ok {
				walkType(sub, path+"/annot[2]")
			} else {
				push(path+"/annot[2]", "E_NODE_SHAPE", "annotated child must be a node", fmt.Sprintf("%T", t[2]), "node")
			}

		default:
			push(path, "E_UNKNOWN_TYPE_TAG", "unknown type tag", tagOf(t), "known type tag")
		}
	}

	// Value/expression walker
	walk = func(n S, path string, inType bool) {
		if len(n) == 0 {
			push(path, "E_EMPTY_NODE", "empty node", "", "tagged node")
			return
		}
		tag := tagOf(n)

		// Disallow "handle" in canonical runtime S
		if tag == "handle" {
			push(path, "E_HANDLE_FORBIDDEN", "'handle' constructs are not allowed", "handle", "n/a")
			return
		}

		switch tag {
		// ---- scalars & ids (payload-kind robust checks) ----
		case "null":
			if len(n) != 1 {
				push(path, "E_ARITY", "null takes no payload", arity(n), "1")
			}
		case "noop":
			if len(n) != 1 {
				push(path, "E_ARITY", "noop takes no payload", arity(n), "1")
			}
		case "bool":
			checkScalarPayload(path, n, "bool")
		case "int":
			checkScalarPayload(path, n, "int")
		case "num":
			checkScalarPayload(path, n, "num")
		case "str":
			checkScalarPayload(path, n, "str")
		case "id":
			checkScalarPayload(path, n, "id")

		// ---- wrappers ----
		case "annot":
			if !checkAnnot(n, path) {
				return
			}
			if child, ok := n[2].(S); ok {
				walk(child, path+"/annot[2]", inType)
			} else {
				push(path+"/annot[2]", "E_NODE_SHAPE", "annotated child must be a node", fmt.Sprintf("%T", n[2]), "node")
			}

		// ---- containers ----
		case "array":
			for i := 1; i < len(n); i++ {
				sub, ok := n[i].(S)
				if !ok {
					push(fmtIdx(path, "array", i-1), "E_NODE_SHAPE", "array element must be a node", fmt.Sprintf("%T", n[i]), "node")
					continue
				}
				walk(sub, fmtIdx(path, "array", i-1), inType)
			}

		case "map":
			for i := 1; i < len(n); i++ {
				p, ok := n[i].(S)
				idx := fmtIdx(path, "map", i-1)
				if !ok || len(p) != 3 {
					push(idx, "E_PAIR_SHAPE", "map entry must be a triple", arity(p), "3")
					continue
				}
				ptag := tagOf(p)
				if inType {
					if ptag != "pair" && ptag != "pair!" {
						push(idx, "E_PAIR_TAG", "type map entries must be 'pair' or 'pair!'", ptag, "pair|pair!")
						continue
					}
				} else {
					if ptag != "pair" {
						push(idx, "E_VALUE_MAP_REQUIREDNESS", "required fields ('pair!') are only allowed in TYPE maps", ptag, "pair")
						continue
					}
				}
				key, ok := p[1].(S)
				if !ok || (tagOf(key) != "str" && tagOf(key) != "annot") {
					push(idx+"/key", "E_KEY_SHAPE", "key must be a string (optionally annotated)", tagOf(p[1]), `["str",k] or ["annot",...]`)
					continue
				}
				if tagOf(key) == "annot" {
					if !checkAnnot(key, idx+"/key") {
						continue
					}
					key = key[2].(S)
				}
				if tagOf(key) != "str" {
					push(idx+"/key", "E_KEY_STR", "annotated key must wrap a string node", tagOf(key), `["str",k]`)
					continue
				}
				checkScalarPayload(idx+"/key", key, "str")

				val, ok := p[2].(S)
				if !ok {
					push(idx+"/val", "E_NODE_SHAPE", "value must be a node", fmt.Sprintf("%T", p[2]), "node")
					continue
				}
				walk(val, idx+"/val", inType)
			}

		// ---- calls / access / ops ----
		case "call":
			if len(n) < 2 {
				push(path, "E_ARITY", "call requires at least callee", arity(n), ">=2")
				return
			}
			if c, ok := n[1].(S); ok {
				walk(c, path+"/call[0]", inType)
			} else {
				push(path+"/call[0]", "E_NODE_SHAPE", "callee must be a node", fmt.Sprintf("%T", n[1]), "node")
			}
			for i := 2; i < len(n); i++ {
				if a, ok := n[i].(S); ok {
					walk(a, fmtIdx(path, "call", i-2), inType)
				} else {
					push(fmtIdx(path, "call", i-2), "E_NODE_SHAPE", "argument must be a node", fmt.Sprintf("%T", n[i]), "node")
				}
			}

		case "get":
			if len(n) != 3 {
				push(path, "E_ARITY", "get requires 2 children", arity(n), "3")
				return
			}
			if obj, ok := n[1].(S); ok {
				walk(obj, path+"/get/obj", inType)
			} else {
				push(path+"/get/obj", "E_NODE_SHAPE", "object must be a node", fmt.Sprintf("%T", n[1]), "node")
			}
			prop, ok := n[2].(S)
			if !ok || tagOf(prop) != "str" {
				push(path+"/get/prop", "E_PROP_STR", "property name must be a string node", tagOf(n[2]), `["str",name]`)
				return
			}
			checkScalarPayload(path+"/get/prop", prop, "str")

		case "idx":
			if len(n) != 3 {
				push(path, "E_ARITY", "idx requires 2 children", arity(n), "3")
				return
			}
			if obj, ok := n[1].(S); ok {
				walk(obj, path+"/idx/obj", inType)
			} else {
				push(path+"/idx/obj", "E_NODE_SHAPE", "object must be a node", fmt.Sprintf("%T", n[1]), "node")
			}
			if ix, ok := n[2].(S); ok {
				walk(ix, path+"/idx/ix", inType)
			} else {
				push(path+"/idx/ix", "E_NODE_SHAPE", "index must be a node", fmt.Sprintf("%T", n[2]), "node")
			}

		case "unop":
			if len(n) != 3 {
				push(path, "E_ARITY", "unop requires op and rhs", arity(n), "3")
				return
			}
			if rhs, ok := n[2].(S); ok {
				walk(rhs, path+"/unop/rhs", inType)
			} else {
				push(path+"/unop/rhs", "E_NODE_SHAPE", "rhs must be a node", fmt.Sprintf("%T", n[2]), "node")
			}

		case "binop":
			if len(n) != 4 {
				push(path, "E_ARITY", "binop requires op, lhs, rhs", arity(n), "4")
				return
			}
			if l, ok := n[2].(S); ok {
				walk(l, path+"/binop/lhs", inType)
			} else {
				push(path+"/binop/lhs", "E_NODE_SHAPE", "lhs must be a node", fmt.Sprintf("%T", n[2]), "node")
			}
			if r, ok := n[3].(S); ok {
				walk(r, path+"/binop/rhs", inType)
			} else {
				push(path+"/binop/rhs", "E_NODE_SHAPE", "rhs must be a node", fmt.Sprintf("%T", n[3]), "node")
			}

		case "assign":
			if len(n) != 3 {
				push(path, "E_ARITY", "assign requires target and value", arity(n), "3")
				return
			}
			if !assignableS(n[1].(S)) {
				push(path+"/assign/lhs", "E_ASSIGN_TARGET", "invalid assignment target", tagOf(n[1]), "id|get|idx|decl|darr|dobj")
			}
			if rhs, ok := n[2].(S); ok {
				walk(rhs, path+"/assign/rhs", inType)
			} else {
				push(path+"/assign/rhs", "E_NODE_SHAPE", "rhs must be a node", fmt.Sprintf("%T", n[2]), "node")
			}

		// ---- patterns ----
		case "decl":
			checkScalarPayload(path, n, "id")
		case "darr":
			for i := 1; i < len(n); i++ {
				sub, ok := n[i].(S)
				if !ok {
					push(fmtIdx(path, "darr", i-1), "E_NODE_SHAPE", "array pattern element must be a node", fmt.Sprintf("%T", n[i]), "node")
					continue
				}
				walk(sub, fmtIdx(path, "darr", i-1), inType)
			}
		case "dobj":
			for i := 1; i < len(n); i++ {
				p, ok := n[i].(S)
				idx := fmtIdx(path, "dobj", i-1)
				if !ok || len(p) != 3 || tagOf(p) != "pair" {
					push(idx, "E_PAIR_SHAPE", "object pattern entries must be pair triples", tagOf(p), `["pair", keyStr, subPattern]`)
					continue
				}
				key, ok := p[1].(S)
				if !ok || (tagOf(key) != "str" && tagOf(key) != "annot") {
					push(idx+"/key", "E_KEY_SHAPE", "key must be a string (optionally annotated)", tagOf(p[1]), `["str",k] or ["annot",...]`)
					continue
				}
				if tagOf(key) == "annot" {
					if !checkAnnot(key, idx+"/key") {
						continue
					}
					key = key[2].(S)
				}
				if tagOf(key) != "str" {
					push(idx+"/key", "E_KEY_STR", "annotated key must wrap a string node", tagOf(key), `["str",k]`)
				} else {
					checkScalarPayload(idx+"/key", key, "str")
				}
				if sub, ok := p[2].(S); ok {
					walk(sub, idx+"/val", inType)
				} else {
					push(idx+"/val", "E_NODE_SHAPE", "subpattern must be a node", fmt.Sprintf("%T", p[2]), "node")
				}
			}

		// ---- statements / blocks ----
		case "block":
			for i := 1; i < len(n); i++ {
				sub, ok := n[i].(S)
				if !ok {
					push(fmtIdx(path, "block", i-1), "E_NODE_SHAPE", "block child must be a node", fmt.Sprintf("%T", n[i]), "node")
					continue
				}
				walk(sub, fmtIdx(path, "block", i-1), inType)
			}

		case "return", "break", "continue":
			if len(n) != 2 {
				push(path, "E_ARITY", tag+` requires exactly one payload (use ["null"] for empty)`, arity(n), "2")
				return
			}
			if arg, ok := n[1].(S); ok {
				walk(arg, path+"/"+tag+"/arg", inType)
			} else {
				push(path+"/"+tag+"/arg", "E_NODE_SHAPE", "argument must be a node", fmt.Sprintf("%T", n[1]), "node")
			}

		case "if":
			if len(n) < 2 {
				push(path, "E_ARITY", "if requires at least one arm", arity(n), ">=2")
				return
			}
			for i := 1; i < len(n); i++ {
				arm, ok := n[i].(S)
				aPath := fmtIdx(path, "if", i-1)
				if !ok || len(arm) == 0 {
					push(aPath, "E_NODE_SHAPE", "if arm must be a node", tagOf(arm), "pair|block")
					continue
				}
				// all but last → pair(cond, block)
				if i < len(n)-1 || (i == len(n)-1 && tagOf(arm) == "pair") {
					if len(arm) != 3 || tagOf(arm) != "pair" {
						push(aPath, "E_IF_ARM", "if arm must be a pair(cond, block)", tagOf(arm), "pair")
						continue
					}
					if c, ok := arm[1].(S); ok {
						walk(c, aPath+"/cond", inType)
					} else {
						push(aPath+"/cond", "E_NODE_SHAPE", "cond must be a node", fmt.Sprintf("%T", arm[1]), "node")
					}
					tb, ok := arm[2].(S)
					if !ok || tagOf(tb) != "block" {
						push(aPath+"/then", "E_EXPECT_BLOCK", "then arm must be a block", tagOf(arm[2]), "block")
						continue
					}
					walk(tb, aPath+"/then", inType)
				} else {
					// last may be else block
					if tagOf(arm) != "block" {
						push(aPath+"/else", "E_EXPECT_BLOCK", "else arm must be a block", tagOf(arm), "block")
						continue
					}
					walk(arm, aPath+"/else", inType)
				}
			}

		case "while":
			if len(n) != 3 {
				push(path, "E_ARITY", "while requires cond and body", arity(n), "3")
				return
			}
			if c, ok := n[1].(S); ok {
				walk(c, path+"/while/cond", inType)
			} else {
				push(path+"/while/cond", "E_NODE_SHAPE", "cond must be a node", fmt.Sprintf("%T", n[1]), "node")
			}
			b, ok := n[2].(S)
			if !ok || tagOf(b) != "block" {
				push(path+"/while/body", "E_EXPECT_BLOCK", "while body must be a block", tagOf(n[2]), "block")
				return
			}
			walk(b, path+"/while/body", inType)

		case "for":
			if len(n) != 4 {
				push(path, "E_ARITY", "for requires target, iter, body", arity(n), "4")
				return
			}
			if t, ok := n[1].(S); ok {
				walk(t, path+"/for/target", inType)
			} else {
				push(path+"/for/target", "E_NODE_SHAPE", "target must be a node", fmt.Sprintf("%T", n[1]), "node")
			}
			if it, ok := n[2].(S); ok {
				walk(it, path+"/for/iter", inType)
			} else {
				push(path+"/for/iter", "E_NODE_SHAPE", "iter must be a node", fmt.Sprintf("%T", n[2]), "node")
			}
			b, ok := n[3].(S)
			if !ok || tagOf(b) != "block" {
				push(path+"/for/body", "E_EXPECT_BLOCK", "for body must be a block", tagOf(n[3]), "block")
				return
			}
			walk(b, path+"/for/body", inType)

		case "fun":
			if len(n) != 4 {
				push(path, "E_ARITY", "fun requires params, retType, body", arity(n), "4")
				return
			}
			if ps, ok := n[1].(S); ok {
				checkParams(ps, path+"/fun/params")
			} else {
				push(path+"/fun/params", "E_NODE_SHAPE", "params must be an array node", fmt.Sprintf("%T", n[1]), `["array", ...]`)
			}
			if rt, ok := n[2].(S); ok {
				walkType(rt, path+"/fun/ret")
			} else {
				push(path+"/fun/ret", "E_NODE_SHAPE", "return type must be a node", fmt.Sprintf("%T", n[2]), "node")
			}
			b, ok := n[3].(S)
			if !ok || tagOf(b) != "block" {
				push(path+"/fun/body", "E_EXPECT_BLOCK", "fun body must be a block", tagOf(n[3]), "block")
				return
			}
			walk(b, path+"/fun/body", inType)

		case "oracle":
			if len(n) != 4 {
				push(path, "E_ARITY", "oracle requires params, outType, sourceExpr", arity(n), "4")
				return
			}
			if ps, ok := n[1].(S); ok {
				checkParams(ps, path+"/oracle/params")
			} else {
				push(path+"/oracle/params", "E_NODE_SHAPE", "params must be an array node", fmt.Sprintf("%T", n[1]), `["array", ...]`)
			}
			if ot, ok := n[2].(S); ok {
				walkType(ot, path+"/oracle/out")
			} else {
				push(path+"/oracle/out", "E_NODE_SHAPE", "output type must be a node", fmt.Sprintf("%T", n[2]), "node")
			}
			src, ok := n[3].(S)
			if !ok || !isExpr(src) {
				push(path+"/oracle/src", "E_ORACLE_SRC_NOT_EXPR", "oracle source must be an expression", tagOf(n[3]), "expr")
				return
			}
			walk(src, path+"/oracle/src", inType)

		case "module":
			if len(n) != 3 {
				push(path, "E_ARITY", "module requires nameExpr and bodyBlock", arity(n), "3")
				return
			}
			if name, ok := n[1].(S); ok {
				walk(name, path+"/module/name", inType)
			} else {
				push(path+"/module/name", "E_NODE_SHAPE", "name must be a node", fmt.Sprintf("%T", n[1]), "node")
			}
			b, ok := n[2].(S)
			if !ok || tagOf(b) != "block" {
				push(path+"/module/body", "E_EXPECT_BLOCK", "module body must be a block", tagOf(n[2]), "block")
				return
			}
			walk(b, path+"/module/body", inType)

		case "type":
			if len(n) != 2 {
				push(path, "E_ARITY", "type requires one child", arity(n), "2")
				return
			}
			if tt, ok := n[1].(S); ok {
				walkType(tt, path+"/type")
			} else {
				push(path+"/type", "E_NODE_SHAPE", "type must be a node", fmt.Sprintf("%T", n[1]), "node")
			}

		default:
			push(path, "E_UNKNOWN_TAG", "unknown tag", tag, "known tag")
		}
	}

	// Run and return
	walk(s, "/", false)
	return Arr(errs)
}
