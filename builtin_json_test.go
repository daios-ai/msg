package mindscript

import (
	"encoding/json"
	"strings"
	"testing"
)

func entriesOf(t *testing.T, v Value) map[string]Value {
	t.Helper()
	mo, ok := v.Data.(*MapObject)
	if !ok {
		t.Fatalf("expected VTMap/*MapObject, got %#v", v)
	}
	return mo.Entries
}

// ---------------- JSON: parse & stringify ----------------

func Test_Builtin_Json_Parse_And_Stringify_Roundtrip_Object(t *testing.T) {
	ip, _ := NewRuntime()

	v := evalWithIP(t, ip, `
		let obj = {name:"Ada", age: 37, tags: ["a","b"], ok: true, misc: null}
		let s = jsonStringify(obj)
		let back = jsonParse(s)
		{ name: back.name, age: back.age, ok: back.ok, tags: back.tags, misc: back.misc }
	`)
	m := entriesOf(t, v)

	if m["name"].Tag != VTStr || m["name"].Data.(string) != "Ada" {
		t.Fatalf("roundtrip name mismatch: %#v", m["name"])
	}
	if m["age"].Tag != VTInt || m["age"].Data.(int64) != 37 {
		t.Fatalf("roundtrip age mismatch: %#v", m["age"])
	}
	if m["ok"].Tag != VTBool || m["ok"].Data.(bool) != true {
		t.Fatalf("roundtrip ok mismatch: %#v", m["ok"])
	}
	if m["tags"].Tag != VTArray || len(m["tags"].Data.(*ArrayObject).Elems) != 2 {
		t.Fatalf("roundtrip tags mismatch: %#v", m["tags"])
	}
	if m["misc"].Tag != VTNull {
		t.Fatalf("roundtrip misc mismatch: %#v", m["misc"])
	}
}

func Test_Builtin_Json_Parse_Invalid_YieldsAnnotatedNull(t *testing.T) {
	ip, _ := NewRuntime()
	v := evalWithIP(t, ip, `jsonParse("{ bad json }")`)
	wantAnnotatedContains(t, v, "invalid json")
}

func Test_Builtin_Json_Number_Int_And_Float_Mapping(t *testing.T) {
	ip, _ := NewRuntime()

	// JSON numbers: integral → Int, non-integral → Num (float64)
	out := evalWithIP(t, ip, `
		let m = jsonParse("{\"i\":1,\"x\":1.5}")
		{ i: m.i, x: m.x }
	`)
	m := entriesOf(t, out)
	if m["i"].Tag != VTInt || m["i"].Data.(int64) != 1 {
		t.Fatalf("expected Int(1) for i, got %#v", m["i"])
	}
	if m["x"].Tag != VTNum || m["x"].Data.(float64) != 1.5 {
		t.Fatalf("expected Num(1.5) for x, got %#v", m["x"])
	}
}

func Test_Builtin_Json_Stringify_Behavior(t *testing.T) {
	ip, _ := NewRuntime()

	t.Run("SupportedValues_RoundTrip", func(t *testing.T) {
		v := evalWithIP(t, ip, `
			let s = jsonStringify({ n: 1 })
			let back = jsonParse(s)
			{ n: back.n }
		`)
		m := entriesOf(t, v)
		if m["n"].Tag != VTInt || m["n"].Data.(int64) != 1 {
			t.Fatalf("numeric field lost: %#v", m["n"])
		}
	})

	t.Run("UnsupportedValues_SoftError", func(t *testing.T) {
		v := evalWithIP(t, ip, `
			let f = fun() do 0 end
			jsonStringify({ f: f, n: 1 })
		`)
		if v.Tag != VTNull || v.Annot == "" {
			t.Fatalf("expected soft-error null from jsonStringify, got %#v", v)
		}
	})
}

// ---------------- Type ⇄ JSON Schema ----------------

func Test_Builtin_Json_TypeToJSONSchema_DescriptionsAndRequired(t *testing.T) {
	ip, _ := NewRuntime()

	src := `
		let T = 
		# Type describing a person
		type {
			# the name
			name!: Str,
			# availability
			avail: Enum["yes","no"]
		}
		typeToJSONSchema(T)
	`
	out, err := ip.EvalSource(src)
	if err != nil {
		t.Fatalf("Eval error: %v", err)
	}

	root := mustMap(t, out)

	// Root description from Value-level annotation
	if desc := root.Entries["description"]; desc.Tag != VTStr || desc.Data.(string) != "Type describing a person" {
		t.Fatalf("root description mismatch: %v", desc)
	}

	propsV, ok := mget(root, "properties")
	if !ok {
		t.Fatalf("properties missing")
	}
	props := mustMap(t, propsV)

	// name property
	nameV, ok := mget(props, "name")
	if !ok {
		t.Fatalf("name property missing")
	}
	name := mustMap(t, nameV)

	if typ, ok := mget(name, "type"); !ok || typ.Tag != VTStr || typ.Data.(string) != "string" {
		t.Fatalf("name.type mismatch: %v", typ)
	}
	if d, ok := mget(name, "description"); !ok || d.Tag != VTStr || d.Data.(string) != "the name" {
		t.Fatalf("name.description mismatch: %v", d)
	}

	// avail property
	availV, ok := mget(props, "avail")
	if !ok {
		t.Fatalf("avail property missing")
	}
	avail := mustMap(t, availV)
	if d, ok := mget(avail, "description"); !ok || d.Tag != VTStr || d.Data.(string) != "availability" {
		t.Fatalf("avail.description mismatch: %v", d)
	}
	if ev, ok := mget(avail, "enum"); !ok || ev.Tag != VTArray {
		t.Fatalf("avail.enum missing or wrong type: %v", ev)
	}

	// required contains "name"
	reqV, ok := mget(root, "required")
	if !ok || reqV.Tag != VTArray {
		t.Fatalf("required missing or wrong type: %v", reqV)
	}
	found := false
	for _, it := range reqV.Data.(*ArrayObject).Elems {
		if it.Tag == VTStr && it.Data.(string) == "name" {
			found = true
			break
		}
	}
	if !found {
		t.Fatalf(`"name" not found in required: %v`, reqV)
	}
}

func Test_Builtin_Json_TypeStringToJSONSchema_Convenience(t *testing.T) {
	ip, _ := NewRuntime()

	src := `
		typeStringToJSONSchema("# the doc\n{ name!: Str, age: Int? }")
	`
	out, err := ip.EvalSource(src)
	if err != nil {
		t.Fatalf("Eval error: %v", err)
	}
	root := mustMap(t, out)

	// Root description preserved
	if d, ok := mget(root, "description"); !ok || d.Tag != VTStr || d.Data.(string) != "the doc" {
		t.Fatalf("root description mismatch: %v", d)
	}

	props := mustMap(t, root.Entries["properties"])
	// name is required string
	name := mustMap(t, props.Entries["name"])
	if typ := name.Entries["type"]; typ.Tag != VTStr || typ.Data.(string) != "string" {
		t.Fatalf("name.type mismatch: %v", typ)
	}
	// age is nullable integer → allow either anyOf or ["integer","null"]
	age := mustMap(t, props.Entries["age"])
	if _, ok := age.Entries["anyOf"]; !ok {
		if tv, ok2 := age.Entries["type"]; !(ok2 && tv.Tag == VTArray) {
			t.Fatalf("age should be nullable pattern; got %v", age)
		}
	}
}

func Test_Builtin_Json_JSONSchemaStringToType_Convenience(t *testing.T) {
	ip, _ := NewRuntime()
	out, err := ip.EvalSource(`jsonSchemaStringToType("{\"type\":\"array\",\"items\":{\"type\":\"number\"}}")`)
	if err != nil {
		t.Fatalf("Eval error: %v", err)
	}
	if out.Tag != VTType {
		t.Fatalf("expected Type; got %v", out.Tag)
	}
	if got := strings.TrimSpace(FormatValue(out)); got != "<type: [Num]>" {
		t.Fatalf("expected <type: [Num]>; got %q", got)
	}
}

func Test_Builtin_Json_TypeToJSONSchema_WithAliasAndDefs(t *testing.T) {
	ip, _ := NewRuntime()

	// Person references itself via a nullable field, forcing $defs + $ref.
	src := `
		let Person = type {
			name!: Str,
			friend: Person?
		}
		typeToJSONSchema(Person)
	`
	out, err := ip.EvalSource(src)
	if err != nil {
		t.Fatalf("Eval error: %v", err)
	}
	root := mustMap(t, out)

	// Expect $defs.Person and properties.friend referencing it with nullability
	defsV, ok := mget(root, "$defs")
	if !ok {
		t.Fatalf("$defs missing")
	}
	defs := mustMap(t, defsV)
	if _, ok := defs.Entries["Person"]; !ok {
		t.Fatalf("$defs.Person missing")
	}

	props := mustMap(t, root.Entries["properties"])
	friend := mustMap(t, props.Entries["friend"])

	// Should be anyOf: [{$ref...}, {"type":"null"}]
	anyOf, ok := friend.Entries["anyOf"]
	if !ok || anyOf.Tag != VTArray {
		t.Fatalf("friend should be nullable anyOf with $ref: %v", friend)
	}
	branches := anyOf.Data.(*ArrayObject).Elems
	if len(branches) != 2 {
		t.Fatalf("expected two anyOf branches; got %d", len(branches))
	}
	refObj := mustMap(t, branches[0])
	if ref, ok := refObj.Entries["$ref"]; !ok || ref.Tag != VTStr || ref.Data.(string) == "" {
		t.Fatalf("missing $ref in first anyOf branch: %v", refObj)
	}
}

func Test_Builtin_Json_Roundtrip_JSON_WithDefsAndRefs(t *testing.T) {
	ip, _ := NewRuntime()

	src := `
		let schema = {
			description: "root doc",
			type: "object",
			properties: {
				person: {
					anyOf: [
						{ "$ref": "#/$defs/Person" },
						{ type: "null" }
					]
				}
			},
			"$defs": {
				Person: {
					type: "object",
					properties: { name: { type: "string" } },
					required: ["name"]
				}
			}
		}
		let T = jsonSchemaToType(schema)
		typeToJSONSchema(T)
	`
	out, err := ip.EvalSource(src)
	if err != nil {
		t.Fatalf("Eval error: %v", err)
	}
	root := mustMap(t, out)

	// root description should survive
	if d, ok := mget(root, "description"); !ok || d.Tag != VTStr || d.Data.(string) != "root doc" {
		t.Fatalf("root description mismatch: %v", d)
	}

	// $defs.Person present after roundtrip
	defs, ok := mget(root, "$defs")
	if !ok || defs.Tag != VTMap {
		t.Fatalf("$defs missing after roundtrip")
	}
	if _, ok := defs.Data.(*MapObject).Entries["Person"]; !ok {
		t.Fatalf("$defs.Person missing after roundtrip")
	}
}

// Error paths

func Test_Builtin_Json_typeStringToJSONSchema_BadType_YieldsAnnotatedNull(t *testing.T) {
	ip, _ := NewRuntime()

	// Intentionally malformed type text (unterminated object) — soft failure (annotated null)
	src := `typeStringToJSONSchema("{ name!: Str, ")`
	out, err := ip.EvalSource(src)
	if err != nil {
		t.Fatalf("Eval error: %v", err)
	}
	if out.Tag != VTNull || out.Annot == "" {
		t.Fatalf("expected annotated null error; got Tag=%v Annot=%q", out.Tag, out.Annot)
	}
}

func Test_Builtin_Json_JSONSchemaStringToType_BadJSON(t *testing.T) {
	ip, _ := NewRuntime()

	// Bad JSON surfaces as a HARD error (native returns annotated null but signature expects Type)
	_, err := ip.EvalSource(`jsonSchemaStringToType("{ invalid json }")`)
	wantErrContains(t, err, "return type mismatch")
}

func Test_Builtin_Json_jsonSchemaToType_NonObjectInput_YieldsAny(t *testing.T) {
	ip, _ := NewRuntime()

	out, err := ip.EvalSource(`jsonSchemaToType("not an object")`)
	if err != nil {
		t.Fatalf("Eval error: %v", err)
	}
	if out.Tag != VTType || strings.TrimSpace(FormatValue(out)) != "<type: Any>" {
		t.Fatalf("expected <type: Any>; got %v", FormatValue(out))
	}
}

func Test_Builtin_Json_JSONSchemaToType_ImportsDefs_AsTypeAliases(t *testing.T) {
	ip, _ := NewRuntime()

	// $defs should be imported into the current environment as type aliases.
	out, err := ip.EvalSource(`
		let schema = {
			type: "integer",
			"$defs": {
				Age: { "type": "integer" }
			}
		}
		let _ = jsonSchemaToType(schema)
		isType(5, Age)
	`)
	if err != nil {
		t.Fatalf("Eval error: %v", err)
	}
	if out.Tag != VTBool || out.Data.(bool) != true {
		t.Fatalf("expected isType(5, Age) to be true; got %#v", out)
	}
}

// ---------------- JSON: builtin jsonRepair ----------------

func Test_Builting_Json_Repair_Fence_Comments_Unquoted_SingleQuotes(t *testing.T) {
	ip, _ := NewRuntime()

	src := "```json\n{ a: 'z', nums: [1, 2,], /* trailing */ ok: true }\n```"
	lit, err := json.Marshal(src) // produce a JSON-escaped string literal
	if err != nil {
		t.Fatalf("marshal failed: %v", err)
	}
	out := evalWithIP(t, ip, `
		let s = `+string(lit)+`
	let v = jsonRepair(s)
		{ a: v.a, ok: v.ok, nums: v.nums }
	`)
	m := entriesOf(t, out)

	if m["a"].Tag != VTStr || m["a"].Data.(string) != "z" {
		t.Fatalf("expected a == \"z\", got %#v", m["a"])
	}
	if m["ok"].Tag != VTBool || m["ok"].Data.(bool) != true {
		t.Fatalf("expected ok == true, got %#v", m["ok"])
	}
	nums := m["nums"]
	if nums.Tag != VTArray {
		t.Fatalf("expected nums to be array, got %#v", nums)
	}
	elems := nums.Data.(*ArrayObject).Elems
	if len(elems) != 2 || elems[0].Tag != VTInt || elems[0].Data.(int64) != 1 || elems[1].Data.(int64) != 2 {
		t.Fatalf("expected nums == [1,2], got %#v", elems)
	}
}

func Test_Builting_Json_Repair_BracketBalancing(t *testing.T) {
	ip, _ := NewRuntime()

	out := evalWithIP(t, ip, `
		let v = jsonRepair("{\"a\":[1,2,3}")
		{ a: v.a }
	`)
	m := entriesOf(t, out)
	a := m["a"]
	if a.Tag != VTArray {
		t.Fatalf("expected a to be array after repair, got %#v", a)
	}
	elems := a.Data.(*ArrayObject).Elems
	if len(elems) != 3 || elems[0].Data.(int64) != 1 || elems[1].Data.(int64) != 2 || elems[2].Data.(int64) != 3 {
		t.Fatalf("expected a == [1,2,3], got %#v", elems)
	}
}

func Test_Builting_Json_Repair_Invalid_YieldsAnnotatedNull(t *testing.T) {
	ip, _ := NewRuntime()
	v := evalWithIP(t, ip, `jsonRepair("not json at all")`)
	wantAnnotatedContains(t, v, "invalid json")
}

func Test_Builting_Json_Repair_NumberNormalization(t *testing.T) {
	ip, _ := NewRuntime()

	// Leading dot, trailing dot, leading plus — normalize to valid numbers.
	out := evalWithIP(t, ip, `
		let v = jsonRepair("{\"n\": [ .5, 1., +1, +0 ] }")
		{ n: v.n }
	`)
	m := entriesOf(t, out)
	n := m["n"]
	if n.Tag != VTArray {
		t.Fatalf("expected n to be array, got %#v", n)
	}
	elems := n.Data.(*ArrayObject).Elems
	// Expect [0.5, 1, 1, 0] with Int where integral.
	if len(elems) != 4 {
		t.Fatalf("expected 4 elements, got %d", len(elems))
	}
	if elems[0].Tag != VTNum || elems[0].Data.(float64) != 0.5 {
		t.Fatalf("elem0 got %#v; want Num(0.5)", elems[0])
	}
	if elems[1].Tag != VTInt || elems[1].Data.(int64) != 1 {
		t.Fatalf("elem1 got %#v; want Int(1)", elems[1])
	}
	if elems[2].Tag != VTInt || elems[2].Data.(int64) != 1 {
		t.Fatalf("elem2 got %#v; want Int(1)", elems[2])
	}
	if elems[3].Tag != VTInt || elems[3].Data.(int64) != 0 {
		t.Fatalf("elem3 got %#v; want Int(0)", elems[3])
	}
}
