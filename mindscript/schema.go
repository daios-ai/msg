// schema.go: bidirectional conversion between MindScript types and JSON Schema.
//
// Overview
// --------
// This file implements the value-centric bridge between MindScript’s type system
// (expressed as S-expressions and runtime VTType values) and JSON Schema. It
// provides:
//   - S-expr/VTType → JSON Schema (with alias preservation via $ref + $defs)
//   - JSON Schema   → MindScript S-expr/VTType (best-effort, widening unknowns)
//   - Two small helpers to parse a type string and parse a JSON Schema string.
//
// Key design choices
// ------------------
//   - Alias preservation: We *do not* resolve type aliases during S→Schema
//     conversion. A type reference ("id","User") becomes {"$ref":"#/$defs/User"},
//     and we simultaneously materialize its definition under "$defs". This keeps
//     schemas stable and avoids inlining large graphs. Cycles are guarded.
//   - Open-world objects: MindScript objects allow extra fields; we therefore do
//     not emit "additionalProperties": false. (Callers can post-process if needed.)
//   - Nullable handling: For built-ins we prefer the short form
//     "type": ["<kind>", "null"]. Otherwise we emit anyOf [ <schema(T)>, {type:null} ]
//     so that references remain references instead of getting inlined.
//   - Functions: There is no faithful JSON-Schema analog for function types;
//     we widen to Any ({}) on export and accept {} back as Any on import.
//   - Enums: Literal enums can contain scalars, arrays, or maps. We deep-convert
//     these both ways.
//
// Dependencies (within this package)
// ----------------------------------
//   - interpreter.go:   Value, ValueTag (VT*), TypeValue, withAnnot, TypeVal,
//     Env, Interpreter, and the helper unwrapKeyStr (used to
//     obtain a map key name from a type AST key node).
//   - types.go:         S (the S-expr alias) and helper isId.
//   - parser.go:        ParseSExpr (used by TypeStringToS).
//
// Public API surface (this file)
// ------------------------------
//
//	func (ip *Interpreter) TypeValueToJSONSchema(tv Value, env *Env) map[string]any
//	func (ip *Interpreter) JSONSchemaToTypeValue(doc map[string]any) Value
//	func TypeStringToS(src string) (S, error)
//	func JSONSchemaStringToObject(jsonStr string) (map[string]any, error)
//
// Notes
// -----
//   - The methods above are the supported public surface. All other identifiers
//     in this file are private implementation details.
//   - Behavior is documented thoroughly below so the API can be used without
//     reading the private section.
package mindscript

import (
	"encoding/json"
	"fmt"
	"strings"
)

/* =======================================================================================
   PUBLIC API
   ======================================================================================= */

// TypeValueToJSONSchema converts a MindScript **type value** (Value.Tag == VTType)
// into a JSON Schema root object.
//
// Input
//
//	tv   : a Value whose Tag is VTType. Its Data may be either *TypeValue (preferred)
//	       or, for legacy payloads, an S (type S-expr). If tv.Annot is non-empty,
//	       it becomes the schema "description" (unless a top-level type annotation
//	       provides one; Value.Annot wins).
//	env  : the environment used to resolve ("id","Alias") references into $ref/$defs.
//	       If env == nil and tv.Data is *TypeValue with a non-nil Env, that Env is
//	       used instead.
//
// Output
//
//	A map[string]any representing the JSON Schema root. When the converter encounters
//	("id","Name") and env binds Name to a VTType, it emits {"$ref":"#/$defs/Name"}
//	and materializes the aliased schema under "$defs"."Name". Cycles are handled by
//	a visiting guard.
//
// Mapping summary
//
//	("id","Any")         → {}
//	("id","Null")        → {"type":"null"}
//	("id","Bool")        → {"type":"boolean"}
//	("id","Int")         → {"type":"integer"}
//	("id","Num")         → {"type":"number"}
//	("id","Str")         → {"type":"string"}
//	("unop","?", T)      → if T is builtin then {"type":[kind,"null"]}
//	                       else {"anyOf":[ schema(T), {"type":"null"} ]}
//	("array", T)         → {"type":"array","items":schema(T)}   (items:{} if T missing)
//	("map", pairs...)    → {"type":"object","properties":{...},"required":[...]}.
//	                       Key annotations (annot(...,"str")) become property "description".
//	("enum", lits...)    → {"enum":[ json(lit)... ]}
//	functions (A -> B)   → {}   (widened to Any)
//	top-level ("annot", ("str",desc), T) lifts "description" when not provided by tv.Annot.
//
// Open-world: "additionalProperties" is intentionally omitted.
//
// Panics: none. This function only returns a map.
//
// Examples
//
//		// alias User := {name!: Str, age: Int?}
//	 ip.TypeValueToJSONSchema(TypeValIn(S{"id","User"}, env), env) ==>
//		  {"$ref":"#/$defs/User","$defs":{"User":{"type":"object", ...}}}
func (ip *Interpreter) TypeValueToJSONSchema(tv Value, env *Env) map[string]any {
	tS := typeAstFromValueData(tv.Data)

	// Prefer the type’s defining env if caller didn’t supply one.
	if env == nil {
		if tvv, ok := tv.Data.(*TypeValue); ok && tvv.Env != nil {
			env = tvv.Env
		}
	}

	sAnnot, base := popTopAnnotIfAny(tS)

	defs := map[string]any{}
	vis := map[string]bool{}
	root := ip.msTypeToSchema(base, env, defs, vis)

	if tv.Annot != "" {
		root["description"] = tv.Annot
	} else if sAnnot != "" {
		root["description"] = sAnnot
	}
	if len(defs) > 0 {
		root["$defs"] = defs
	}
	return root
}

// JSONSchemaToTypeValue converts a JSON Schema root object into a MindScript
// **type value** (Value{Tag: VTType}).
//
// Input
//
//	doc : root JSON object (decoded into map[string]any). Extra/unknown fields are
//	      ignored; unsupported features widen to Any. If doc == nil, returns Any.
//
// Output
//
//	A Value with Tag = VTType whose Data is an S (type S-expr). If the JSON Schema
//	contains a top-level "description" string, it is propagated into Value.Annot.
//
// Mapping summary (subset)
//
//	{"type":"null"}        → ("id","Null")
//	{"type":"boolean"}     → ("id","Bool")
//	{"type":"integer"}     → ("id","Int")
//	{"type":"number"}      → ("id","Num")
//	{"type":"string"}      → ("id","Str")
//	{"type":"array",items:X} → ("array", S(X))           (items missing ⇒ Any)
//	{"type":"object", properties:{k:X}, required:[...]} → ("map", ("pair"| "pair!", ("str",k), S(X))...)
//	{"enum":[...]}         → ("enum", litS...)
//	{"nullable":true, ...} → S(...)?
//	{"anyOf":[A,{"type":"null"}]} (or oneOf) → S(A)?
//	{"type":["T","null"]}  → S(T)?
//	{"$ref":"#/$defs/Name"}|{"$ref":"#/definitions/Name"} → ("id","Name")
//	Other/remote $ref or complex unions → Any.
//
// Panics: none. This function only returns a Value.
func (ip *Interpreter) JSONSchemaToTypeValue(doc map[string]any) Value {
	if doc == nil {
		// Always pin to an environment; schema-derived types do not depend on caller scopes.
		return TypeValIn(S{"id", "Any"}, ip.Core)
	}
	t := ip.schemaNodeToMSType(doc, doc, map[string]bool{})
	annot := ""
	if d, ok := doc["description"].(string); ok && d != "" {
		annot = d
	}
	// Pin the produced type to ip.Core to preserve the invariant “no env-less types”.
	return withAnnot(TypeValIn(t, ip.Core), annot)
}

// TypeStringToS parses a MindScript *type* written as a single expression into
// its S-expr representation. It uses the normal Pratt parser.
//
// Input
//
//	src : a single type expression (e.g., "Int", "Str?", "[Int]", "{name!: Str}").
//
// Output
//
//	The S-expr for that type. If the parsed source contains more than one
//	top-level expression, an error is returned.
//
// Panics: none. This function returns an error for invalid syntax.
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
			return nil, fmt.Errorf("expected a single type expression")
		}
	}
	return ast, nil
}

// JSONSchemaStringToObject decodes a JSON Schema document from a string into
// a Go map suitable for JSONSchemaToTypeValue. Unknown fields are preserved.
//
// Note: by design the decoder does *not* use UseNumber; numeric literals are
// decoded as float64 to match downstream expectations.
func JSONSchemaStringToObject(jsonStr string) (map[string]any, error) {
	var m map[string]any
	dec := json.NewDecoder(strings.NewReader(jsonStr))
	if err := dec.Decode(&m); err != nil {
		return nil, err
	}
	return m, nil
}

//// END_OF_PUBLIC

/* =======================================================================================
   PRIVATE (implementation details)
   ======================================================================================= */

// popTopAnnotIfAny extracts a single leading ("annot", ("str", text), inner) if present.
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

// typeAstFromValueData unwraps TypeValue payloads to their AST (and supports legacy S).
func typeAstFromValueData(data any) S {
	switch tv := data.(type) {
	case *TypeValue:
		return tv.Ast
	case S: // legacy payload
		return tv
	default:
		return S{} // sentinel "empty"
	}
}

// ----- MindScript Type (S) → JSON Schema -----

func (ip *Interpreter) msTypeToSchema(t S, env *Env, defs map[string]any, visiting map[string]bool) map[string]any {
	// Intentionally DO NOT resolve aliases here — we want $ref + $defs, not inlining.
	// t = ip.resolveType(t, env)
	if len(t) == 0 {
		return map[string]any{}
	}

	switch t[0].(string) {
	case "annot":
		// ("annot", ("str", text), subj)
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

	// inside func (ip *Interpreter) msTypeToSchema(t S, env *Env, defs map[string]any, visiting map[string]bool) map[string]any
	case "id":
		name := t[1].(string)
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

						// --- NEW: preserve alias annotations into $defs ---
						aliasAst := typeAstFromValueData(v.Data)
						sAnnot, base := popTopAnnotIfAny(aliasAst)
						defSch := ip.msTypeToSchema(base, env, defs, visiting)
						// Value.Annot wins; otherwise use top-level AST annot if present and no description yet.
						if v.Annot != "" {
							defSch["description"] = v.Annot
						} else if sAnnot != "" {
							if _, exists := defSch["description"]; !exists {
								defSch["description"] = sAnnot
							}
						}
						defs[name] = defSch
						// --- END NEW ---

						delete(visiting, name)
					}
					return map[string]any{"$ref": "#/$defs/" + name}
				}
			}
			return map[string]any{}
		}

	case "unop":
		if len(t) >= 3 && t[1].(string) == "?" {
			base := t[2].(S)
			if jt, ok := simpleJSONTypeName(base); ok {
				return map[string]any{"type": []any{jt, "null"}}
			}
			return map[string]any{
				"anyOf": []any{
					ip.msTypeToSchema(base, env, defs, visiting),
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
		return map[string]any{"type": "array", "items": map[string]any{}}

	case "map":
		props := map[string]any{}
		var req []any
		for i := 1; i < len(t); i++ {
			p := t[i].(S) // ("pair"|"pair!", keyNode, valType)
			ptag := p[0].(string)
			keyNode := p[1].(S)
			valT := p[2].(S)

			name := unwrapKeyStr(keyNode)
			ps := ip.msTypeToSchema(valT, env, defs, visiting)

			// key annotation → property description
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
		// functions have no JSON Schema analog
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

// ----- JSON Schema → MindScript Type (S) -----

func (ip *Interpreter) schemaNodeToMSType(node any, root map[string]any, seen map[string]bool) S {
	m, ok := node.(map[string]any)
	if !ok {
		return S{"id", "Any"}
	}

	// $ref first
	if refRaw, has := m["$ref"]; has {
		if ref, ok := refRaw.(string); ok {
			// Local pointers we understand: "#/$defs/Name" or "#/definitions/Name"
			if strings.HasPrefix(ref, "#/") {
				segs := splitJSONPointer(ref[2:])
				if len(segs) == 2 && (segs[0] == "$defs" || segs[0] == "definitions") {
					name := segs[1]
					return S{"id", name}
				}
				// If it points elsewhere in the same doc, try to resolve the node
				target, ok := resolveJSONPointer(root, segs)
				if ok {
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

	// OpenAPI nullable: true
	if nb, ok := m["nullable"].(bool); ok && nb {
		inner := ip.schemaNodeToMSType(without(m, "nullable"), root, seen)
		return S{"unop", "?", inner}
	}

	// anyOf / oneOf : accept 2-branch (T, null) as nullable
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
				return S{"id", "Any"}
			}
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
		// type: ["T", "null"] (or reverse) → nullable
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
		return S{"id", "Any"}
	}

	// Unknown/unsupported → Any
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
	case int:
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
