package mindscript

// introspection.go — encode/ decode & reflection for runtime S-expressions ([])
//
// FINAL SURFACE (per policy)
// --------------------------
//   IxToS(ast S) Value
//     • Convert internal AST (S) → runtime-S Value (VTArray with leading VTStr).
//     • If encoding encounters an unsupported atom, returns a SOFT error as
//       annotated-null: ["annot", ["str", <msg>], ["null"]].
//
//   IxFromS(rt Value) (S, error)
//     • Validate runtime-S shape strictly, then convert → AST (S).
//     • On malformed/unsupported shapes, returns a HARD error (no partial S).
//
//   IxReflect(v Value) Value
//     • Return CONSTRUCTOR CODE as runtime-S (a program AST-in-[] that, when
//       evaluated, rebuilds v as best as possible).
//       - Scalars/arrays/maps → literal nodes
//       - VTType → ["type", <typeAst>]   (constructor for a Type value)
//       - VTFun/oracle → ["fun"/"oracle", params, ret/out, body]
//       - VTModule → ["module", ["str", name], ("pair", ["id", export], <ctor>)*]
//       - Native functions → ["id", nativeName]
//       - VTHandle/unknown → SOFT error (annotated-null)
//     • All annotations on values (including POST markers like "<...") are
//       preserved by wrapping the constructed node(s) with ("annot", ("str", txt), node).
//
//   (*Interpreter) IxReify(rt Value) (Value, error)
//     • Decode (IxFromS) then EVALUATE (like EvalPersistent) in the host.
//     • HARD errors if:
//         - the program contains a ["module", ...] capsule and the host hasn't
//           provided an installer yet (this file does not install modules);
//         - a ["handle", ...] construct is encountered; or
//         - runtime evaluation fails.
//
// DESIGN NOTES
// ------------
// • Encoding is non-throwing (tools-friendly); decoding/reify are strict.
// • No snapshotting of data trees—everything is constructor code.
// • Requiredness in maps: we pass through "pair!" whenever present; semantics
//   remain interpreter/type-engine territory.
// • Negative numbers encode directly as ["int",-2]/["num",-1.5] (runtime view).
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
func IxReflect(v Value) Value {
	s, ok := ixConstructorS_ForValue(v)
	if !ok {
		return rtSoftError("cannot reflect value to constructor code")
	}
	return IxToS(s) // S → runtime-S
}

// IxReify decodes a runtime-S program and evaluates it in this interpreter,
// equivalent to EvalPersistent. Hard-fails if the program contains a "handle"
// construct, or if evaluation fails. Module capsules are lowered to inline form.
func (ip *Interpreter) IxReify(rt Value) (Value, error) {
	ast, err := IxFromS(rt)
	if err != nil {
		return Value{}, err
	}
	// Proactively reject unsupported constructs before evaluation.
	if ixContainsTag(ast, "handle") {
		return Value{}, fmt.Errorf("cannot reify handle constructs")
	}
	// Rewrite any ("module", ["str", name], ("pair", ["id", k], ctor)*) capsules
	// into the inline module form understood by the interpreter:
	// ("module", ["str", name], ("block", ("assign", ("decl", k), ctor)*))
	lowered := ixLowerModuleCapsules(ast)
	// Evaluate in Global (same as EvalPersistent).
	return ip.EvalPersistent(lowered)
}

//// END_OF_PUBLIC

// ==============================
// ========== PRIVATE ===========
// ==============================

// ---------- small shared helpers ----------

// annWrapS wraps node with an ("annot", ("str", txt), node) if txt != "".
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
		return rtSoftError("empty AST node")
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

// ---------- runtime-S → S (strict) ----------

func ixFromS_Strict(v Value) (S, error) {
	if v.Tag != VTArray {
		return nil, fmt.Errorf("IxFromS: expected VTArray, got %v", v.Tag)
	}
	xs := v.Data.([]Value)
	if len(xs) == 0 {
		return nil, fmt.Errorf("IxFromS: empty node (missing tag)")
	}
	if xs[0].Tag != VTStr {
		return nil, fmt.Errorf("IxFromS: node[0] must be VTStr tag, got %v", xs[0].Tag)
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
			return nil, fmt.Errorf("IxFromS: unsupported scalar tag %v", c.Tag)
		}
	}
	return out, nil
}

// ---------- Value → constructor S (annotation-preserving, no snapshotting) ----------

func ixConstructorS_ForValue(v Value) (S, bool) {
	// Build the unannotated node first, then wrap with v.Annot (if any).
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
		elems := v.Data.([]Value)
		out := S{"array"}
		for _, e := range elems {
			se, ok := ixConstructorS_ForValue(e) // child wrapper includes its own .Annot
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
			ctor, ok := ixConstructorS_ForValue(val) // preserves value annotations
			if !ok {
				return nil, false
			}
			// Requiredness policy: pass through as "pair!" if present in KeyAnn.
			ptag := "pair"
			if ann := mo.KeyAnn[k]; ann != "" && containsBang(ann) {
				ptag = "pair!"
			}

			// Key node with optional docs (excluding "!") as PRE/POST annot.
			keyNode := S{"str", k}
			if extra := keyDocWithoutBang(mo.KeyAnn[k]); extra != "" {
				keyNode = annWrapS(extra, keyNode)
			}

			out = append(out, S{ptag, keyNode, ctor})
		}
		return out, true

	case VTFun:
		f := v.Data.(*Fun)
		// Native? Emit identifier to be resolved by host env.
		if f.NativeName != "" {
			return S{"id", f.NativeName}, true
		}
		// Params: ("array", ("pair", ("id", name), typeAst), ...)
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
			return S{"oracle", par, ret, f.Body}, true
		}
		return S{"fun", par, ret, f.Body}, true

	case VTType:
		// Constructor for Type values uses the "type" node.
		tv := v.Data.(*TypeValue)
		return S{"type", tv.Ast}, true

	case VTModule:
		// Module capsule:
		// ["module", ["str", moduleName],
		//    ("pair", ["id", export], <ctor>)* ]
		name := ""
		if m, ok := v.Data.(*Module); ok {
			if m != nil && m.Name != "" {
				name = m.Name
			}
		}
		mv := AsMapValue(v)
		mo := mv.Data.(*MapObject)
		out := S{"module", S{"str", name}}
		for _, k := range mo.Keys {
			ctor, ok := ixConstructorS_ForValue(mo.Entries[k]) // preserves export value annotations
			if !ok {
				return nil, false
			}
			out = append(out, S{"pair", S{"id", k}, ctor})
		}
		return out, true

	case VTHandle:
		// Not representable—use soft error (handled by caller).
		return nil, false

	default:
		return nil, false
	}
}

func containsBang(s string) bool {
	for i := 0; i < len(s); i++ {
		if s[i] == '!' {
			return true
		}
	}
	return false
}

func keyDocWithoutBang(s string) string {
	// Strip a lone "!" (requiredness marker). Preserve any other docs.
	if s == "!" {
		return ""
	}
	// If it contains "!", drop just that char and trim a trailing space.
	if containsBang(s) {
		out := make([]byte, 0, len(s))
		for i := 0; i < len(s); i++ {
			if s[i] != '!' {
				out = append(out, s[i])
			}
		}
		// trim trailing space
		if n := len(out); n > 0 && out[n-1] == ' ' {
			out = out[:n-1]
		}
		return string(out)
	}
	return s
}

// ---------- AST inspection helpers ----------

func ixContainsTag(n S, want string) bool {
	if len(n) == 0 {
		return false
	}
	tag, _ := n[0].(string)
	if tag == want {
		return true
	}
	for i := 1; i < len(n); i++ {
		if sub, ok := n[i].(S); ok && ixContainsTag(sub, want) {
			return true
		}
	}
	return false
}

// ---------- Module capsule lowering ----------
// A "module capsule" (from IxReflect) looks like:
//
//	("module", ("str", name), ("pair", ("id", export), ctor)*)
//
// We lower it to the inline VM form:
//
//	("module", ("str", name), ("block", ("assign", ("decl", export), ctor)*))
//
// This lets the normal module pipeline (caching, cycle detection, span rooting)
// handle installation via nativeMakeModule.
func ixLowerModuleCapsules(n S) S {
	if len(n) == 0 {
		return n
	}
	if isModuleCapsule(n) {
		name := "" // safe default
		if hdr, ok := n[1].(S); ok && len(hdr) >= 2 {
			if t, _ := hdr[0].(string); t == "str" {
				if s, _ := hdr[1].(string); s != "" {
					name = s
				}
			}
		}
		body := S{"block"}
		for i := 2; i < len(n); i++ {
			pair, ok := n[i].(S)
			if !ok || len(pair) < 3 {
				continue
			}
			ptag, _ := pair[0].(string)
			if ptag != "pair" && ptag != "pair!" {
				continue
			}
			idNode, _ := pair[1].(S)
			if len(idNode) < 2 {
				continue
			}
			if idTag, _ := idNode[0].(string); idTag != "id" {
				continue
			}
			key, _ := idNode[1].(string)
			ctor, _ := pair[2].(S)
			// Recursively lower nested capsules inside constructor code.
			ctor = ixLowerModuleCapsules(ctor)
			body = append(body, S{"assign", S{"decl", key}, ctor})
		}
		return S{"module", S{"str", name}, body}
	}
	// Generic deep copy + rewrite.
	out := make(S, 0, len(n))
	out = append(out, n[0])
	for i := 1; i < len(n); i++ {
		switch c := n[i].(type) {
		case S:
			out = append(out, ixLowerModuleCapsules(c))
		default:
			out = append(out, c)
		}
	}
	return out
}

func isModuleCapsule(n S) bool {
	if len(n) < 2 {
		return false
	}
	if tag, _ := n[0].(string); tag != "module" {
		return false
	}
	// Second element must be ("str", name)
	hdr, ok := n[1].(S)
	if !ok || len(hdr) < 2 {
		return false
	}
	if t, _ := hdr[0].(string); t != "str" {
		return false
	}
	// Remaining elements must all be ("pair" | "pair!", ("id", k), ctor)
	for i := 2; i < len(n); i++ {
		p, ok := n[i].(S)
		if !ok || len(p) < 3 {
			return false
		}
		ptag, _ := p[0].(string)
		if ptag != "pair" && ptag != "pair!" {
			return false
		}
		idn, ok := p[1].(S)
		if !ok || len(idn) < 2 {
			return false
		}
		if it, _ := idn[0].(string); it != "id" {
			return false
		}
	}
	return true
}
