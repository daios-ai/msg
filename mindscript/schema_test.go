package mindscript

import (
	"encoding/json"
	"reflect"
	"strings"
	"testing"
)

// Helper: extract the S-expr AST from a VTType Value using *TypeValue.
func typeSFromValue(t *testing.T, v Value) S {
	t.Helper()
	if v.Tag != VTType {
		t.Fatalf("expected VTType, got %#v", v)
	}
	tv, ok := v.Data.(*TypeValue)
	if !ok || tv == nil {
		t.Fatalf("expected *TypeValue payload, got %#v", v.Data)
	}
	return tv.Ast
}

// -----------------------------
// MindScript Type -> JSON Schema
// -----------------------------

func Test_Schema_MSPrimitives_ToJSON(t *testing.T) {
	ip, _ := NewInterpreter()
	env := NewEnv(nil)

	cases := []struct {
		t S
		j map[string]any
	}{
		{S{"id", "Any"}, map[string]any{}},
		{S{"id", "Null"}, map[string]any{"type": "null"}},
		{S{"id", "Bool"}, map[string]any{"type": "boolean"}},
		{S{"id", "Int"}, map[string]any{"type": "integer"}},
		{S{"id", "Num"}, map[string]any{"type": "number"}},
		{S{"id", "Str"}, map[string]any{"type": "string"}},
		{S{"id", "Type"}, map[string]any{}}, // widened
	}
	for _, c := range cases {
		got := ip.TypeValueToJSONSchema(TypeValIn(c.t, env), env)
		// strip $defs if present
		delete(got, "$defs")
		if !reflect.DeepEqual(got, c.j) {
			t.Fatalf("TypeToJSONSchema(%v) = %#v; want %#v", c.t, got, c.j)
		}
	}
}

func Test_Schema_MSNullable_ToJSON(t *testing.T) {
	ip, _ := NewInterpreter()
	env := NewEnv(nil)

	// Primitive nullable uses type: ["<t>", "null"]
	prim := S{"unop", "?", S{"id", "Int"}}
	got := ip.TypeValueToJSONSchema(TypeValIn(prim, env), env)
	delete(got, "$defs")
	want := map[string]any{"type": []any{"integer", "null"}}
	if !reflect.DeepEqual(got, want) {
		t.Fatalf("nullable primitive got %#v; want %#v", got, want)
	}

	// Complex nullable uses anyOf
	complex := S{"unop", "?", S{"array", S{"id", "Str"}}}
	got = ip.TypeValueToJSONSchema(TypeValIn(complex, env), env)
	delete(got, "$defs")
	_, okType := got["anyOf"]
	if !okType {
		t.Fatalf("nullable complex should use anyOf; got %#v", got)
	}
}

func Test_Schema_MSArray_ToJSON(t *testing.T) {
	ip, _ := NewInterpreter()
	env := NewEnv(nil)

	tarr := S{"array", S{"id", "Num"}}
	got := ip.TypeValueToJSONSchema(TypeValIn(tarr, env), env)
	delete(got, "$defs")
	want := map[string]any{"type": "array", "items": map[string]any{"type": "number"}}
	if !reflect.DeepEqual(got, want) {
		t.Fatalf("array got %#v; want %#v", got, want)
	}

	// Missing element type -> items: {}
	tarr2 := S{"array", S{"id", "Any"}}
	got = ip.TypeValueToJSONSchema(TypeValIn(tarr2, env), env)
	delete(got, "$defs")
	want = map[string]any{"type": "array", "items": map[string]any{}}
	if !reflect.DeepEqual(got, want) {
		t.Fatalf("array[Any] default got %#v; want %#v", got, want)
	}
}

func Test_Schema_MSObject_ToJSON(t *testing.T) {
	ip, _ := NewInterpreter()
	env := NewEnv(nil)

	tobj := S{"map",
		S{"pair", S{"str", "name"}, S{"id", "Str"}},
		S{"pair!", S{"str", "age"}, S{"id", "Int"}},
	}
	got := ip.TypeValueToJSONSchema(TypeValIn(tobj, env), env)
	delete(got, "$defs")
	props := map[string]any{
		"name": map[string]any{"type": "string"},
		"age":  map[string]any{"type": "integer"},
	}
	want := map[string]any{"type": "object", "properties": props, "required": []any{"age"}}
	if !reflect.DeepEqual(got, want) {
		t.Fatalf("object got %#v; want %#v", got, want)
	}
	// Ensure we do NOT emit additionalProperties (open-world)
	if _, has := got["additionalProperties"]; has {
		t.Fatalf("should not emit additionalProperties; got %#v", got)
	}
}

func Test_Schema_MSEnum_ToJSON(t *testing.T) {
	ip, _ := NewInterpreter()
	env := NewEnv(nil)

	tenum := S{"enum",
		S{"str", "a"},
		S{"int", int64(1)},
		S{"num", 2.5},
		S{"null"},
		S{"array", S{"int", int64(3)}},
		S{"map", S{"pair", S{"str", "k"}, S{"bool", true}}},
	}
	got := ip.TypeValueToJSONSchema(TypeValIn(tenum, env), env)
	delete(got, "$defs")
	// Only check that it's an enum with the right length and some key shapes.
	ev, ok := got["enum"].([]any)
	if !ok || len(ev) != 6 {
		t.Fatalf("enum got %#v", got)
	}
}

func Test_Schema_MSAlias_ToJSON_WithDefs(t *testing.T) {
	ip, _ := NewInterpreter()
	env := NewEnv(nil)

	// type User = { name!: Str, friend: User? }
	user := S{"map",
		S{"pair!", S{"str", "name"}, S{"id", "Str"}},
		S{"pair", S{"str", "friend"}, S{"unop", "?", S{"id", "User"}}},
	}
	env.Define("User", TypeValIn(user, env))

	root := ip.TypeValueToJSONSchema(TypeValIn(S{"id", "User"}, env), env)

	// Expect root: {"$ref":"#/$defs/User", "$defs": { "User": {type:"object", ...} } }
	if ref, ok := root["$ref"].(string); !ok || ref != "#/$defs/User" {
		t.Fatalf("root $ref missing/invalid: %#v", root)
	}
	defs, ok := root["$defs"].(map[string]any)
	if !ok {
		t.Fatalf("$defs missing: %#v", root)
	}
	u, ok := defs["User"].(map[string]any)
	if !ok {
		t.Fatalf("$defs.User missing: %#v", root)
	}
	// Confirm recursive friend -> $ref User
	props := u["properties"].(map[string]any)
	friend := props["friend"].(map[string]any)
	anyOf, ok := friend["anyOf"].([]any)
	if !ok || len(anyOf) != 2 {
		t.Fatalf("friend nullable anyOf missing: %#v", friend)
	}
	// one branch should be $ref to User
	hasRef := false
	for _, br := range anyOf {
		if mm, ok := br.(map[string]any); ok {
			if r, ok := mm["$ref"].(string); ok && r == "#/$defs/User" {
				hasRef = true
			}
		}
	}
	if !hasRef {
		t.Fatalf("friend anyOf should reference #/$defs/User; got %#v", anyOf)
	}
}

func Test_Schema_MSFunction_ToJSON_Widens(t *testing.T) {
	ip, _ := NewInterpreter()
	env := NewEnv(nil)
	fn := S{"binop", "->", S{"id", "Int"}, S{"id", "Str"}}
	got := ip.TypeValueToJSONSchema(TypeValIn(fn, env), env)
	delete(got, "$defs")
	if len(got) != 0 {
		t.Fatalf("function should widen to empty schema, got %#v", got)
	}
}

// -----------------------------
// JSON Schema -> MindScript Type
// -----------------------------

func Test_Schema_JSONPrimitives_ToMS(t *testing.T) {
	ip, _ := NewInterpreter()

	cases := []struct {
		js map[string]any
		ms S
	}{
		{map[string]any{"type": "null"}, S{"id", "Null"}},
		{map[string]any{"type": "boolean"}, S{"id", "Bool"}},
		{map[string]any{"type": "integer"}, S{"id", "Int"}},
		{map[string]any{"type": "number"}, S{"id", "Num"}},
		{map[string]any{"type": "string"}, S{"id", "Str"}},
	}
	for _, c := range cases {
		got := typeSFromValue(t, ip.JSONSchemaToTypeValue(c.js))
		if !equalLiteralS(got, c.ms) {
			t.Fatalf("JSON->MS %v => %v; want %v", c.js, got, c.ms)
		}
	}
}

func Test_Schema_JSONNullablePatterns_ToMS(t *testing.T) {
	ip, _ := NewInterpreter()

	// type: ["integer", "null"]
	m1 := map[string]any{"type": []any{"integer", "null"}}
	got := typeSFromValue(t, ip.JSONSchemaToTypeValue(m1))
	want := S{"unop", "?", S{"id", "Int"}}
	if !equalLiteralS(got, want) {
		t.Fatalf("nullable via type[] got %v; want %v", got, want)
	}

	// anyOf: [ {type:string}, {type:null} ]
	m2 := map[string]any{"anyOf": []any{map[string]any{"type": "string"}, map[string]any{"type": "null"}}}
	got = typeSFromValue(t, ip.JSONSchemaToTypeValue(m2))
	want = S{"unop", "?", S{"id", "Str"}}
	if !equalLiteralS(got, want) {
		t.Fatalf("nullable via anyOf got %v; want %v", got, want)
	}

	// oneOf: [ {type:number}, {type:null} ]
	m3 := map[string]any{"oneOf": []any{map[string]any{"type": "number"}, map[string]any{"type": "null"}}}
	got = typeSFromValue(t, ip.JSONSchemaToTypeValue(m3))
	want = S{"unop", "?", S{"id", "Num"}}
	if !equalLiteralS(got, want) {
		t.Fatalf("nullable via oneOf got %v; want %v", got, want)
	}

	// OpenAPI nullable: true
	m4 := map[string]any{"type": "string", "nullable": true}
	got = typeSFromValue(t, ip.JSONSchemaToTypeValue(m4))
	want = S{"unop", "?", S{"id", "Str"}}
	if !equalLiteralS(got, want) {
		t.Fatalf("nullable:true got %v; want %v", got, want)
	}
}

func Test_Schema_JSONArray_ToMS(t *testing.T) {
	ip, _ := NewInterpreter()

	js := map[string]any{"type": "array", "items": map[string]any{"type": "number"}}
	got := typeSFromValue(t, ip.JSONSchemaToTypeValue(js))
	want := S{"array", S{"id", "Num"}}
	if !equalLiteralS(got, want) {
		t.Fatalf("array got %v; want %v", got, want)
	}

	// Missing items -> array[Any]
	got = typeSFromValue(t, ip.JSONSchemaToTypeValue(map[string]any{"type": "array"}))
	want = S{"array", S{"id", "Any"}}
	if !equalLiteralS(got, want) {
		t.Fatalf("array default items Any got %v; want %v", got, want)
	}
}

func Test_Schema_JSONObject_ToMS(t *testing.T) {
	ip, _ := NewInterpreter()

	schema := map[string]any{
		"type": "object",
		"properties": map[string]any{
			"name": map[string]any{"type": "string"},
			"age":  map[string]any{"type": "integer"},
		},
		"required": []any{"age"},
	}

	ms := typeSFromValue(t, ip.JSONSchemaToTypeValue(schema))
	if len(ms) == 0 || ms[0].(string) != "map" {
		t.Fatalf("expected map; got %v", ms)
	}

	fields := mapTypeFields(ms)
	if len(fields) != 2 {
		t.Fatalf("expected 2 fields; got %d", len(fields))
	}

	// name: optional Str
	nameF, ok := fields["name"]
	if !ok {
		t.Fatalf("missing field 'name'")
	}
	if nameF.required {
		t.Fatalf("'name' should be optional")
	}
	if !equalLiteralS(nameF.typ, S{"id", "Str"}) {
		t.Fatalf("'name' type mismatch: got %v", nameF.typ)
	}

	// age: required Int
	ageF, ok := fields["age"]
	if !ok {
		t.Fatalf("missing field 'age'")
	}
	if !ageF.required {
		t.Fatalf("'age' should be required")
	}
	if !equalLiteralS(ageF.typ, S{"id", "Int"}) {
		t.Fatalf("'age' type mismatch: got %v", ageF.typ)
	}
}

func Test_Schema_JSONEnum_ToMS(t *testing.T) {
	ip, _ := NewInterpreter()

	js := map[string]any{
		"enum": []any{
			nil,
			true,
			"hi",
			float64(2),
			[]any{float64(3)},
			map[string]any{"k": "v"},
		},
	}
	got := typeSFromValue(t, ip.JSONSchemaToTypeValue(js))
	// only verify tag and length
	if len(got) == 0 || got[0].(string) != "enum" || len(got) != 7 {
		t.Fatalf("enum conversion unexpected: %v", got)
	}
}

func Test_Schema_JSONRefs_ToMS(t *testing.T) {
	ip, _ := NewInterpreter()

	// #/$defs/Name -> ("id","Name")
	doc := map[string]any{
		"$defs": map[string]any{
			"Pet": map[string]any{"type": "object"},
		},
		"$ref": "#/$defs/Pet",
	}
	got := typeSFromValue(t, ip.JSONSchemaToTypeValue(doc))
	want := S{"id", "Pet"}
	if !equalLiteralS(got, want) {
		t.Fatalf("$ref #/$defs/Pet -> %v; want %v", got, want)
	}

	// Other local pointer that we can resolve: #/components/schemas/Thing
	doc = map[string]any{
		"components": map[string]any{
			"schemas": map[string]any{
				"Thing": map[string]any{"type": "string"},
			},
		},
		"$ref": "#/components/schemas/Thing",
	}
	got = typeSFromValue(t, ip.JSONSchemaToTypeValue(doc))
	want = S{"id", "Str"}
	if !equalLiteralS(got, want) {
		t.Fatalf("resolving local pointer -> %v; want %v", got, want)
	}
}

func Test_Schema_JSONUnsupported_Unions_ToAny(t *testing.T) {
	ip, _ := NewInterpreter()

	// anyOf with 3 branches => Any
	js := map[string]any{
		"anyOf": []any{
			map[string]any{"type": "string"},
			map[string]any{"type": "null"},
			map[string]any{"type": "integer"},
		},
	}
	got := typeSFromValue(t, ip.JSONSchemaToTypeValue(js))
	if !testIsId(got, "Any") {
		t.Fatalf("complex anyOf should map to Any; got %v", got)
	}
}

func Test_Schema_JSONConstraints_Ignored(t *testing.T) {
	ip, _ := NewInterpreter()
	// pattern/minimum etc. should not force Any; base type remains
	js := map[string]any{
		"type":    "string",
		"pattern": "^[a-z]+$",
	}
	got := typeSFromValue(t, ip.JSONSchemaToTypeValue(js))
	if !testIsId(got, "Str") {
		t.Fatalf("string with pattern should still be Str; got %v", got)
	}
}

// -----------------------------
// Round-trip sanity (lossy OK)
// -----------------------------

func Test_Schema_Roundtrip_MS_to_JSON_to_MS(t *testing.T) {
	ip, _ := NewInterpreter()
	env := NewEnv(nil)

	ms := S{"map",
		S{"pair!", S{"str", "id"}, S{"id", "Int"}},
		S{"pair", S{"str", "tags"}, S{"array", S{"id", "Str"}}},
	}
	js := ip.TypeValueToJSONSchema(TypeValIn(ms, env), env)
	got := typeSFromValue(t, ip.JSONSchemaToTypeValue(js))
	// We expect equal shape (order-insensitive by our equality)
	if !equalLiteralS(got, ms) {
		t.Fatalf("round-trip mismatch: got %v; want %v", got, ms)
	}
}

func Test_Schema_Roundtrip_JSON_to_MS_to_JSON(t *testing.T) {
	ip, _ := NewInterpreter()

	js := map[string]any{
		"type": "object",
		"properties": map[string]any{
			"name": map[string]any{"type": "string"},
			"age":  map[string]any{"type": "integer"},
		},
		"required": []any{"name"},
	}
	msV := ip.JSONSchemaToTypeValue(js)

	out := ip.TypeValueToJSONSchema(msV, nil)
	// Ignore $defs for comparison
	delete(out, "$defs")

	// We don't check strict equality because of normalization, but structure should match
	if out["type"] != "object" {
		t.Fatalf("expected object in out schema, got %#v", out)
	}
	props, ok := out["properties"].(map[string]any)
	if !ok || props["name"] == nil || props["age"] == nil {
		t.Fatalf("properties missing in out schema: %#v", out)
	}
	req, _ := out["required"].([]any)
	if len(req) != 1 || req[0] != "name" {
		t.Fatalf("required mismatch: %#v", out)
	}
}

// -----------------------------
// Tiny helpers
// -----------------------------

func Test_Schema_Helper_TypeStringToS(t *testing.T) {
	srcs := []string{
		"Int",
		"Str?",
		"[Num]",
		"{name!: Str, tags: [Str]}",
		"Enum[1, 2, 3]",
	}
	for _, s := range srcs {
		ast, err := TypeStringToS(s)
		if err != nil {
			t.Fatalf("TypeStringToS(%q) error: %v", s, err)
		}
		if len(ast) == 0 {
			t.Fatalf("TypeStringToS(%q) returned empty AST", s)
		}
	}

	// Multiple expressions should error
	if _, err := TypeStringToS("Int Str"); err == nil {
		t.Fatalf("expected error on multiple expressions")
	}
}

func Test_Schema_Helper_JSONSchemaStringToObject(t *testing.T) {
	src := `{
		"type":"object",
		"properties": { "x": { "type":"integer" } },
		"required": ["x"]
	}`
	m, err := JSONSchemaStringToObject(src)
	if err != nil {
		t.Fatalf("JSONSchemaStringToObject error: %v", err)
	}
	if m["type"] != "object" {
		t.Fatalf("parsed object missing type: %#v", m)
	}

	// Also check that invalid JSON errors out
	if _, err := JSONSchemaStringToObject("{"); err == nil {
		t.Fatalf("expected error for invalid JSON")
	}
}

// -----------------------------
// JSON number literal handling
// -----------------------------

func Test_Schema_Enum_JSONNumber_IntVsNum(t *testing.T) {
	// Ensure jsonLiteralToS: integral float -> Int; non-integral -> Num
	ip, _ := NewInterpreter()

	js := map[string]any{"enum": []any{float64(2), float64(2.5)}}
	ms := typeSFromValue(t, ip.JSONSchemaToTypeValue(js))
	if len(ms) != 3 || ms[0].(string) != "enum" {
		t.Fatalf("unexpected ms enum: %v", ms)
	}
	a := ms[1].(S)
	b := ms[2].(S)
	if !(a[0].(string) == "int" && b[0].(string) == "num") &&
		!(a[0].(string) == "num" && b[0].(string) == "int") {
		t.Fatalf("expected one int and one num; got %v, %v", a, b)
	}
}

// -----------------------------
// Alias / $defs roundtrip smoke
// -----------------------------

func Test_Schema_Alias_Roundtrip_Smoke(t *testing.T) {
	ip, _ := NewInterpreter()
	env := NewEnv(nil)

	order := S{"map",
		S{"pair!", S{"str", "id"}, S{"id", "Int"}},
		S{"pair", S{"str", "child"}, S{"unop", "?", S{"id", "Order"}}},
	}
	env.Define("Order", TypeValIn(order, env))

	// MS -> JSON ($defs) -> MS
	js := ip.TypeValueToJSONSchema(TypeValIn(S{"id", "Order"}, env), env)
	ms := typeSFromValue(t, ip.JSONSchemaToTypeValue(js))

	// The top-level will typically be ("id","Order") due to $ref handling
	if !testIsId(ms, "Order") {
		// Fallback: allow expanded object as well (depending on pointer resolution)
		if !(len(ms) > 0 && ms[0].(string) == "map") {
			t.Fatalf("expected id(Order) or expanded map; got %v", ms)
		}
	}
}

// -----------------------------
// JSON round-trip utility for debugging (not a test)
// -----------------------------

func marshalPretty(x any) string {
	b, _ := json.MarshalIndent(x, "", "  ")
	return string(b)
}

func Test_Schema_JSONPointer_EncodingDecoding(t *testing.T) {
	// Verify that ~0 and ~1 decodings behave in our split function
	ptr := "#/a~1b/~0c"
	segs := splitJSONPointer(ptr[2:])
	if len(segs) != 2 || segs[0] != "a/b" || segs[1] != "~c" {
		t.Fatalf("splitJSONPointer failed: %#v", segs)
	}

	// Ensure resolveJSONPointer works on nested maps
	doc := map[string]any{
		"a/b": map[string]any{
			"~c": map[string]any{"type": "string"},
		},
	}
	target, ok := resolveJSONPointer(doc, segs)
	if !ok {
		t.Fatalf("resolveJSONPointer failed")
	}
	if !reflect.DeepEqual(target, map[string]any{"type": "string"}) {
		t.Fatalf("resolveJSONPointer wrong target: %#v", target)
	}
}

// -----------------------------
// Safety: complex unions become Any
// -----------------------------

func Test_Schema_JSON_ComplexOneOf_ToAny(t *testing.T) {
	ip, _ := NewInterpreter()
	doc := map[string]any{
		"oneOf": []any{
			map[string]any{"type": "string"},
			map[string]any{"type": "integer"},
			map[string]any{"type": "null"},
		},
	}
	ms := typeSFromValue(t, ip.JSONSchemaToTypeValue(doc))
	if !testIsId(ms, "Any") {
		t.Fatalf("complex oneOf should map to Any; got %v", ms)
	}
}

// -----------------------------
// Helpers: small sanity on string parser integration
// -----------------------------

func Test_Schema_Helper_TypeStringToS_Integration(t *testing.T) {
	// Leading "# a doc" should become a top-level ("annot", ("str","a doc"), <type>)
	src := `
		# a doc
		{ name!: Str, age: Int?, tags: [Str], meta: { likes: Int } }
	`
	s, err := TypeStringToS(src)
	if err != nil {
		t.Fatalf("TypeStringToS error: %v", err)
	}

	// Expect top-level annotation
	if len(s) == 0 || s[0].(string) != "annot" {
		t.Fatalf("expected top-level annot node; got %v", s)
	}
	// Check description text
	descNode, ok := s[1].(S)
	if !ok || len(descNode) < 2 || descNode[0].(string) != "str" || descNode[1].(string) != "a doc" {
		t.Fatalf("expected description 'a doc'; got %v", descNode)
	}
	// Unwrap to the underlying type and assert it's a map
	inner, ok := s[2].(S)
	if !ok || len(inner) == 0 || inner[0].(string) != "map" {
		t.Fatalf("expected inner map; got %v", inner)
	}
	// Ensure there's a required field "name"
	found := false
	for i := 1; i < len(inner); i++ {
		p := inner[i].(S)
		if p[0].(string) == "pair!" && p[1].(S)[1].(string) == "name" {
			found = true
			break
		}
	}
	if !found {
		t.Fatalf("required name not found in %v", inner)
	}
}

func Test_Schema_Helper_JSONSchemaStringToObject_Integration(t *testing.T) {
	src := `{
		"$defs": {
			"User": {
				"type":"object",
				"properties": { "name": { "type":"string" } }
			}
		},
		"$ref":"#/$defs/User"
	}`
	m, err := JSONSchemaStringToObject(src)
	if err != nil {
		t.Fatalf("parse JSON schema string: %v", err)
	}
	ip, _ := NewInterpreter()
	ms := typeSFromValue(t, ip.JSONSchemaToTypeValue(m))
	// For $ref to #/$defs/User, we map to ("id","User")
	if !testIsId(ms, "User") {
		t.Fatalf("expected id(User); got %v", ms)
	}
}

// Ensure equalS behaves sanely in our assertions (guard test)
func Test_Schema_Internal_equalS_Sanity(t *testing.T) {
	a := S{"map", S{"pair", S{"str", "x"}, S{"id", "Int"}}}
	b := S{"map", S{"pair", S{"str", "x"}, S{"id", "Int"}}}
	if !equalLiteralS(a, b) {
		t.Fatalf("equalS should consider these equal")
	}
	c := S{"map", S{"pair", S{"str", "x"}, S{"id", "Str"}}}
	if equalLiteralS(a, c) {
		t.Fatalf("equalS should consider these different")
	}
}

// Small smoke to ensure JSON reading preserves floats and string arrays in our tests
func Test_Schema_JSON_Decoding_Smoke(t *testing.T) {
	raw := `{"type":"array","items":{"type":"number"},"required":["x"],"enum":[1,2.5,"ok",null]}`
	m, err := JSONSchemaStringToObject(raw)
	if err != nil {
		t.Fatalf("JSONSchemaStringToObject: %v", err)
	}
	if m["type"] != "array" {
		t.Fatalf("type not array: %#v", m["type"])
	}
	if _, ok := m["enum"].([]any); !ok {
		t.Fatalf("enum not []any")
	}
	if !strings.Contains(marshalPretty(m), `"number"`) {
		t.Fatalf("pretty json should include number")
	}
}

func Test_Schema_Annot_TopLevel_JSONDescription(t *testing.T) {
	ip, _ := NewInterpreter()

	ms := S{"annot", S{"str", "type for a person"},
		S{"map",
			S{"pair!", S{"annot", S{"str", "the name"}, S{"str", "name"}}, S{"id", "Str"}},
			S{"pair", S{"annot", S{"str", "availability"}, S{"str", "avail"}},
				S{"enum", S{"str", "yes"}, S{"str", "no"}},
			},
		},
	}

	js := ip.TypeValueToJSONSchema(TypeValIn(ms, ip.Global), ip.Global) // js is map[string]any

	if desc, _ := js["description"].(string); desc != "type for a person" {
		t.Fatalf("missing/incorrect root description: %v", js["description"])
	}

	props, ok := js["properties"].(map[string]any)
	if !ok {
		t.Fatalf("properties missing or wrong type: %T", js["properties"])
	}

	if d, _ := props["name"].(map[string]any)["description"].(string); d != "the name" {
		t.Fatalf("name description mismatch: %v", d)
	}
	if d, _ := props["avail"].(map[string]any)["description"].(string); d != "availability" {
		t.Fatalf("avail description mismatch: %v", d)
	}

	// required should include "name"
	req, ok := js["required"].([]any)
	if !ok {
		t.Fatalf("required missing or wrong type: %T", js["required"])
	}
	found := false
	for _, v := range req {
		if s, _ := v.(string); s == "name" {
			found = true
			break
		}
	}
	if !found {
		t.Fatalf(`"name" not found in required: %v`, req)
	}
}

func Test_Schema_AliasDescriptions_In_Defs(t *testing.T) {
	ip, _ := NewInterpreter()
	env := NewEnv(nil)

	// Define aliases *as if* they were declared in MindScript with preceding # docs.
	userAst := S{"map",
		S{"pair!", S{"str", "name"}, S{"id", "Str"}},
		S{"pair", S{"str", "age"}, S{"id", "Int"}},
	}
	hobbiesAst := S{"array", S{"id", "Str"}}
	statusAst := S{"enum", S{"str", "ready"}, S{"str", "running"}, S{"str", "done"}}

	env.Define("User", withAnnot(TypeValIn(userAst, env), "A user record."))
	env.Define("Hobbies", withAnnot(TypeValIn(hobbiesAst, env), "Hobbies."))
	env.Define("Status", withAnnot(TypeValIn(statusAst, env), "Status of execution."))

	// Ask for a schema of something that references all three aliases.
	root := S{"map",
		S{"pair!", S{"str", "u"}, S{"id", "User"}},
		S{"pair!", S{"str", "h"}, S{"id", "Hobbies"}},
		S{"pair!", S{"str", "output"}, S{"id", "Status"}},
	}
	got := ip.TypeValueToJSONSchema(TypeValIn(root, env), env)

	defs, ok := got["$defs"].(map[string]any)
	if !ok {
		t.Fatalf("schema missing $defs: %#v", got)
	}
	// User
	uDef, ok := defs["User"].(map[string]any)
	if !ok {
		t.Fatalf("$defs.User missing: %#v", defs)
	}
	if desc, _ := uDef["description"].(string); desc != "A user record." {
		t.Fatalf("User.description = %q; want %q", desc, "A user record.")
	}
	// Hobbies
	hDef, ok := defs["Hobbies"].(map[string]any)
	if !ok {
		t.Fatalf("$defs.Hobbies missing: %#v", defs)
	}
	if desc, _ := hDef["description"].(string); desc != "Hobbies." {
		t.Fatalf("Hobbies.description = %q; want %q", desc, "Hobbies.")
	}
	// Status
	sDef, ok := defs["Status"].(map[string]any)
	if !ok {
		t.Fatalf("$defs.Status missing: %#v", defs)
	}
	if desc, _ := sDef["description"].(string); desc != "Status of execution." {
		t.Fatalf("Status.description = %q; want %q", desc, "Status of execution.")
	}

	// Ensure root uses $ref for these properties.
	props := got["properties"].(map[string]any)
	for _, k := range []string{"u", "h", "output"} {
		prop := props[k].(map[string]any)
		if _, ok := prop["$ref"]; !ok {
			t.Fatalf("property %q should be a $ref, got %#v", k, prop)
		}
	}
}

func Test_Schema_MapKeyAnnotation_To_PropertyDescription(t *testing.T) {
	ip, _ := NewInterpreter()
	env := NewEnv(nil)

	// { #(Age in years.) age: Int }
	typ := S{"map",
		S{"pair",
			S{"annot", S{"str", "Age in years."}, S{"str", "age"}},
			S{"id", "Int"},
		},
	}
	got := ip.TypeValueToJSONSchema(TypeValIn(typ, env), env)
	props := got["properties"].(map[string]any)
	age := props["age"].(map[string]any)
	if desc, _ := age["description"].(string); desc != "Age in years." {
		t.Fatalf("property description = %q; want %q", desc, "Age in years.")
	}
	if typN, _ := age["type"].(string); typN != "integer" {
		t.Fatalf("property type = %q; want %q", typN, "integer")
	}
}

func Test_Schema_NullableAlias_AnyOf_With_Ref(t *testing.T) {
	ip, _ := NewInterpreter()
	env := NewEnv(nil)

	// Status alias (enum) with description.
	statusAst := S{"enum", S{"str", "ready"}, S{"str", "running"}, S{"str", "done"}}
	env.Define("Status", withAnnot(TypeValIn(statusAst, env), "Status of execution."))

	// Use Status? (nullable) as a field type.
	root := S{"map",
		S{"pair!", S{"str", "result"}, S{"unop", "?", S{"id", "Status"}}},
	}
	got := ip.TypeValueToJSONSchema(TypeValIn(root, env), env)

	// result should be {"anyOf":[{"$ref":"#/$defs/Status"}, {"type":"null"}]}
	props := got["properties"].(map[string]any)
	res := props["result"].(map[string]any)
	anyOf, ok := res["anyOf"].([]any)
	if !ok || len(anyOf) != 2 {
		t.Fatalf("nullable alias should use anyOf; got %#v", res)
	}
	arm0, _ := anyOf[0].(map[string]any)
	arm1, _ := anyOf[1].(map[string]any)
	if _, ok := arm0["$ref"]; !ok {
		t.Fatalf("first anyOf arm should be $ref; got %#v", anyOf)
	}
	if typ, _ := arm1["type"].(string); typ != "null" {
		t.Fatalf("second anyOf arm should be type:null; got %#v", anyOf)
	}

	// And $defs.Status must exist with a description.
	defs := got["$defs"].(map[string]any)
	sDef := defs["Status"].(map[string]any)
	if desc, _ := sDef["description"].(string); desc != "Status of execution." {
		t.Fatalf("Status.description = %q; want %q", desc, "Status of execution.")
	}
	if _, ok := sDef["enum"]; !ok {
		t.Fatalf("Status enum missing in $defs.Status: %#v", sDef)
	}
}

func Test_Schema_JSON_To_MS_TopLevelDescription_Preserved(t *testing.T) {
	ip, _ := NewInterpreter()

	doc := map[string]any{
		"type": "object",
		"properties": map[string]any{
			"name": map[string]any{"type": "string"},
		},
		"required":    []any{"name"},
		"description": "Top-level doc.",
	}
	tv := ip.JSONSchemaToTypeValue(doc)
	if tv.Tag != VTType {
		t.Fatalf("expected VTType, got %#v", tv)
	}
	if tv.Annot != "Top-level doc." {
		t.Fatalf("top-level description not preserved, got %q", tv.Annot)
	}
	// quick sanity on shape
	tS := typeSFromValue(t, tv)
	if len(tS) == 0 || tS[0] != "map" {
		t.Fatalf("expected a map type S, got %#v", tS)
	}
}
