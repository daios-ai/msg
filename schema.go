// schema.go
package mindscript

import (
	"encoding/json"
	"fmt"
	"strings"
)

// Public API (value-centric)
// --------------------------

// TypeValueToJSONSchema converts a MindScript **Type Value** (VTType) into a JSON Schema object.
// The Value.Annot (if non-empty) is emitted as the top-level "description".
// Aliases referenced by ("id","Name") that are bound in env to VTType are emitted
// as {"$ref":"#/$defs/Name"} and materialized under "$defs".
func (ip *Interpreter) TypeValueToJSONSchema(tv Value, env *Env) map[string]any {
	// Pull S-expr, but also accept a top-level S-annot from literals.
	tS, _ := tv.Data.(S)
	sAnnot, base := popTopAnnotIfAny(tS)

	defs := map[string]any{}
	vis := map[string]bool{} // for cycle-guard on defs
	root := ip.msTypeToSchema(base, env, defs, vis)

	// Top-level description: prefer Value.Annot; fallback to S-annot.
	if tv.Annot != "" {
		root["description"] = tv.Annot
	} else if sAnnot != "" {
		root["description"] = sAnnot
	}

	if len(defs) > 0 {
		// Attach $defs at top-level
		root["$defs"] = defs
	}
	return root
}

// JSONSchemaToTypeValue converts a JSON Schema (root object) into a MindScript **Type Value**.
// The returned Value has Tag=VTType, Data=S (the type S-expr), and Annot filled from the
// schema's top-level "description" (if present). Unsupported features widen to Any.
func (ip *Interpreter) JSONSchemaToTypeValue(doc map[string]any) Value {
	if doc == nil {
		return TypeVal(S{"id", "Any"})
	}
	t := ip.schemaNodeToMSType(doc, doc, map[string]bool{})
	annot := ""
	if d, ok := doc["description"].(string); ok && d != "" {
		annot = d
	}
	return withAnnot(TypeVal(t), annot)
}

// --- Tiny helpers: string -> S-expr type, string -> JSON object ---

// TypeStringToS parses a MindScript *type* written as a single expression
// into its S-expr form. It uses the existing Pratt parser.
// Examples: "Int", "Str?", "[Int]", "{name!: Str}"
func TypeStringToS(src string) (S, error) {
	ast, err := ParseSExpr(src)
	if err != nil {
		return nil, err
	}
	// ParseSExpr returns ("block", expr1, expr2, ...)
	if len(ast) >= 2 {
		if tag, _ := ast[0].(string); tag == "block" {
			if len(ast) == 2 {
				if sub, ok := ast[1].(S); ok {
					return sub, nil
				}
			}
			// If there are multiple expressions, treat that as an error for a type.
			return nil, fmt.Errorf("expected a single type expression")
		}
	}
	// If it's not a block (unlikely), return as-is.
	return ast, nil
}

// JSONSchemaStringToObject parses a JSON Schema string into a Go map object
// (suitable for JSONSchemaToTypeValue). Unsupported/extra fields are preserved.
func JSONSchemaStringToObject(jsonStr string) (map[string]any, error) {
	var m map[string]any
	dec := json.NewDecoder(strings.NewReader(jsonStr))
	// NOTE: do NOT UseNumber here — downstream expects float64 for JSON numbers.
	if err := dec.Decode(&m); err != nil {
		return nil, err
	}
	return m, nil
}

// Internal tiny helper used above.
// Extracts a single leading ("annot", ("str", text), inner) if present.
func popTopAnnotIfAny(t S) (string, S) {
	if len(t) >= 3 {
		if tag, _ := t[0].(string); tag == "annot" {
			if s, ok := t[1].(S); ok && len(s) >= 2 && s[0].(string) == "str" {
				return s[1].(string), t[2].(S)
			}
		}
	}
	return "", t
}

// -----------------------------------------------------------------------------
// MindScript Type (S) -> JSON Schema
// -----------------------------------------------------------------------------

func (ip *Interpreter) msTypeToSchema(t S, env *Env, defs map[string]any, visiting map[string]bool) map[string]any {
	// REMOVE this line:
	// t = ip.resolveType(t, env)
	if len(t) == 0 {
		return map[string]any{}
	}

	switch t[0].(string) {
	case "annot":
		// t = ("annot", ("str", text), subj)
		text := ""
		if s, ok := t[1].(S); ok && len(s) >= 2 && s[0].(string) == "str" {
			text = s[1].(string)
		}
		subj := t[2].(S)
		sch := ip.msTypeToSchema(subj, env, defs, visiting)
		if text != "" {
			if _, exists := sch["description"]; !exists {
				sch["description"] = text
			}
		}
		return sch

	case "id":
		name := t[1].(string)
		// Builtins
		switch name {
		case "Any":
			return map[string]any{}
		case "Null":
			return map[string]any{"type": "null"}
		case "Bool":
			return map[string]any{"type": "boolean"}
		case "Int":
			return map[string]any{"type": "integer"}
		case "Num":
			return map[string]any{"type": "number"}
		case "Str":
			return map[string]any{"type": "string"}
		case "Type":
			return map[string]any{}
		default:
			// Env alias → $ref + $defs expansion (cycle-guarded)
			if env != nil {
				if v, err := env.Get(name); err == nil && v.Tag == VTType {
					if visiting[name] {
						return map[string]any{"$ref": "#/$defs/" + name}
					}
					if _, ok := defs[name]; !ok {
						visiting[name] = true
						// Expand the DEFINITION using the aliased body,
						// but DO NOT resolve the *reference* itself.
						defs[name] = ip.msTypeToSchema(v.Data.(S), env, defs, visiting)
						delete(visiting, name)
					}
					return map[string]any{"$ref": "#/$defs/" + name}
				}
			}
			// Unknown id → widen
			return map[string]any{}
		}

	case "unop":
		if len(t) >= 3 && t[1].(string) == "?" {
			base := t[2].(S)
			// Only treat **builtin** primitives specially; do NOT resolve aliases here.
			if jt, ok := simpleJSONTypeName(base); ok {
				return map[string]any{"type": []any{jt, "null"}}
			}
			return map[string]any{
				"anyOf": []any{
					ip.msTypeToSchema(base, env, defs, visiting), // will $ref if base is an alias like ("id","User")
					map[string]any{"type": "null"},
				},
			}
		}
		return map[string]any{}

	case "array":
		if len(t) == 2 {
			return map[string]any{
				"type":  "array",
				"items": ip.msTypeToSchema(t[1].(S), env, defs, visiting),
			}
		}
		// No element type → Any
		return map[string]any{
			"type":  "array",
			"items": map[string]any{},
		}

	case "map":
		props := map[string]any{}
		var req []any

		for i := 1; i < len(t); i++ {
			p := t[i].(S) // ("pair"|"pair!", keyNode, valType)
			ptag := p[0].(string)
			keyNode := p[1].(S)
			valT := p[2].(S)

			// 1) name (handles "str" and "annot(..., str)")
			name := unwrapKeyStr(keyNode)

			// 2) schema for value
			ps := ip.msTypeToSchema(valT, env, defs, visiting)

			// 3) description from key annotation, if present
			if len(keyNode) > 0 && keyNode[0].(string) == "annot" {
				if desc, ok := keyNode[1].(S); ok && len(desc) >= 2 && desc[0].(string) == "str" {
					ps["description"] = desc[1].(string)
				}
			}

			props[name] = ps
			if ptag == "pair!" {
				req = append(req, name)
			}
		}

		out := map[string]any{"type": "object", "properties": props}
		if len(req) > 0 {
			out["required"] = req
		}
		return out

	case "enum":
		// Convert each literal S to its JSON literal
		en := []any{}
		for i := 1; i < len(t); i++ {
			if jv, ok := sLiteralToJSON(t[i].(S)); ok {
				en = append(en, jv)
			}
		}
		if len(en) == 0 {
			return map[string]any{}
		}
		return map[string]any{"enum": en}

	case "binop":
		// Functions / arrows have no JSON Schema analog → Any
		return map[string]any{}
	default:
		return map[string]any{}
	}
}

func simpleJSONTypeName(t S) (string, bool) {
	if len(t) >= 2 && t[0].(string) == "id" {
		switch t[1].(string) {
		case "Bool":
			return "boolean", true
		case "Int":
			return "integer", true
		case "Num":
			return "number", true
		case "Str":
			return "string", true
		case "Null":
			return "null", true
		}
	}
	return "", false
}

func sLiteralToJSON(lit S) (any, bool) {
	if len(lit) == 0 {
		return nil, false
	}
	switch lit[0].(string) {
	case "null":
		return nil, true
	case "bool":
		return lit[1].(bool), true
	case "int":
		return lit[1].(int64), true
	case "num":
		return lit[1].(float64), true
	case "str":
		return lit[1].(string), true
	case "array":
		out := make([]any, 0, len(lit)-1)
		for i := 1; i < len(lit); i++ {
			jv, ok := sLiteralToJSON(lit[i].(S))
			if !ok {
				return nil, false
			}
			out = append(out, jv)
		}
		return out, true
	case "map":
		m := map[string]any{}
		for i := 1; i < len(lit); i++ {
			p := lit[i].(S) // ("pair"| "pair!", ("str",k), vLit)
			if len(p) < 3 {
				return nil, false
			}
			key := p[1].(S)
			if len(key) < 2 || key[0].(string) != "str" {
				return nil, false
			}
			jv, ok := sLiteralToJSON(p[2].(S))
			if !ok {
				return nil, false
			}
			m[key[1].(string)] = jv
		}
		return m, true
	default:
		return nil, false
	}
}

// -----------------------------------------------------------------------------
// JSON Schema -> MindScript Type (S)
// -----------------------------------------------------------------------------

func (ip *Interpreter) schemaNodeToMSType(node any, root map[string]any, seen map[string]bool) S {
	// Most nodes should be JSON objects
	m, ok := node.(map[string]any)
	if !ok {
		// Enums handle literal arrays/values elsewhere; bare literal here → Any
		return S{"id", "Any"}
	}

	// $ref first
	if refRaw, has := m["$ref"]; has {
		if ref, ok := refRaw.(string); ok {
			// Local pointers we understand: "#/$defs/Name" or "#/definitions/Name"
			if strings.HasPrefix(ref, "#/") {
				segs := splitJSONPointer(ref[2:])
				// common layouts
				if len(segs) == 2 && (segs[0] == "$defs" || segs[0] == "definitions") {
					name := segs[1]
					// Map to ("id","Name") — the env aliasing will handle actual resolution downstream.
					return S{"id", name}
				}
				// If it points to some other place in the same doc, try to resolve the node
				target, ok := resolveJSONPointer(root, segs)
				if ok {
					// Convert that target; if recursion detected → Any
					ptr := strings.Join(segs, "/")
					if seen[ptr] {
						return S{"id", "Any"}
					}
					seen[ptr] = true
					t := ip.schemaNodeToMSType(target, root, seen)
					delete(seen, ptr)
					return t
				}
			}
			// Remote refs or unknown shapes → Any
			return S{"id", "Any"}
		}
	}

	// enum
	if ev, has := m["enum"]; has {
		if arr, ok := ev.([]any); ok {
			out := S{"enum"}
			for _, jv := range arr {
				if lit, ok := jsonLiteralToS(jv); ok {
					out = append(out, lit)
				}
			}
			if len(out) > 1 {
				return out
			}
			return S{"id", "Any"}
		}
	}

	// OpenAPI nullable: true (best-effort)
	if nb, ok := m["nullable"].(bool); ok && nb {
		inner := ip.schemaNodeToMSType(without(m, "nullable"), root, seen)
		return S{"unop", "?", inner}
	}

	// anyOf / oneOf : accept the two-branch (T, null) shape for nullable
	for _, key := range []string{"anyOf", "oneOf"} {
		if v, has := m[key]; has {
			if arr, ok := v.([]any); ok && len(arr) == 2 {
				a := ip.schemaNodeToMSType(arr[0], root, seen)
				b := ip.schemaNodeToMSType(arr[1], root, seen)
				if isId(a, "Null") {
					return S{"unop", "?", b}
				}
				if isId(b, "Null") {
					return S{"unop", "?", a}
				}
				// Not a nullable pattern → unsupported union → Any
				return S{"id", "Any"}
			}
			// Complex unions → Any
			return S{"id", "Any"}
		}
	}

	// type
	switch tv := m["type"].(type) {
	case string:
		switch tv {
		case "null":
			return S{"id", "Null"}
		case "boolean":
			return S{"id", "Bool"}
		case "integer":
			return S{"id", "Int"}
		case "number":
			return S{"id", "Num"}
		case "string":
			return S{"id", "Str"}
		case "array":
			items := S{"id", "Any"}
			if it, ok := m["items"]; ok {
				items = ip.schemaNodeToMSType(it, root, seen)
			}
			return S{"array", items}
		case "object":
			props := map[string]any{}
			if p, ok := m["properties"].(map[string]any); ok {
				props = p
			}
			reqSet := map[string]bool{}
			if r, ok := m["required"].([]any); ok {
				for _, x := range r {
					if s, ok := x.(string); ok {
						reqSet[s] = true
					}
				}
			}
			out := S{"map"}
			for k, sub := range props {
				tk := ip.schemaNodeToMSType(sub, root, seen)
				tag := "pair"
				if reqSet[k] {
					tag = "pair!"
				}
				out = append(out, S{tag, S{"str", k}, tk})
			}
			return out
		default:
			return S{"id", "Any"}
		}

	case []any:
		// Nullable via type: ["T","null"] (or null+T)
		if len(tv) == 2 {
			var a, b S
			if s0, ok := stringOf(tv[0]); ok {
				a = ip.schemaNodeToMSType(map[string]any{"type": s0}, root, seen)
			} else {
				a = ip.schemaNodeToMSType(tv[0], root, seen)
			}
			if s1, ok := stringOf(tv[1]); ok {
				b = ip.schemaNodeToMSType(map[string]any{"type": s1}, root, seen)
			} else {
				b = ip.schemaNodeToMSType(tv[1], root, seen)
			}
			if isId(a, "Null") {
				return S{"unop", "?", b}
			}
			if isId(b, "Null") {
				return S{"unop", "?", a}
			}
			return S{"id", "Any"}
		}
		// Complex unions → Any
		return S{"id", "Any"}
	}

	// If we reach here, we either had constraints we don't model or unknown shapes.
	return S{"id", "Any"}
}

func without(m map[string]any, key string) map[string]any {
	out := map[string]any{}
	for k, v := range m {
		if k != key {
			out[k] = v
		}
	}
	return out
}

func stringOf(x any) (string, bool) {
	s, ok := x.(string)
	return s, ok
}

func jsonLiteralToS(j any) (S, bool) {
	switch v := j.(type) {
	case nil:
		return S{"null"}, true
	case bool:
		return S{"bool", v}, true
	case float64:
		// JSON numbers are floats; detect integral values for Int
		if v == float64(int64(v)) {
			return S{"int", int64(v)}, true
		}
		return S{"num", v}, true
	case int: // in case caller passed non-JSON-normalized map
		return S{"int", int64(v)}, true
	case int64:
		return S{"int", v}, true
	case string:
		return S{"str", v}, true
	case []any:
		out := S{"array"}
		for _, it := range v {
			if s, ok := jsonLiteralToS(it); ok {
				out = append(out, s)
			} else {
				return S{}, false
			}
		}
		return out, true
	case map[string]any:
		out := S{"map"}
		for k, it := range v {
			if s, ok := jsonLiteralToS(it); ok {
				out = append(out, S{"pair", S{"str", k}, s})
			} else {
				return S{}, false
			}
		}
		return out, true
	default:
		return S{}, false
	}
}

// JSON Pointer helpers (local refs)
// ----------------------------------

func splitJSONPointer(ptr string) []string {
	if ptr == "" {
		return []string{}
	}
	parts := strings.Split(ptr, "/")
	for i := range parts {
		parts[i] = strings.ReplaceAll(strings.ReplaceAll(parts[i], "~1", "/"), "~0", "~")
	}
	return parts
}

func resolveJSONPointer(doc map[string]any, segs []string) (any, bool) {
	var cur any = doc
	for _, s := range segs {
		obj, ok := cur.(map[string]any)
		if !ok {
			return nil, false
		}
		nxt, ok := obj[s]
		if !ok {
			return nil, false
		}
		cur = nxt
	}
	return cur, true
}
