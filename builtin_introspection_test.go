package mindscript

import (
	"strings"
	"testing"
)

// ----------------------------- tests -----------------------------

func Test_Builtin_Introspection_noteGet_and_noteSet(t *testing.T) {
	ip, _ := NewRuntime()

	// noteGet on unannotated value → null
	v := evalWithIP(t, ip, `noteGet(42)`)
	if v.Tag != VTNull {
		t.Fatalf("noteGet(42) should be null, got %#v", v)
	}

	// noteSet attaches; noteGet returns the text; value data remains intact
	v2 := evalWithIP(t, ip, `
		let x = noteSet("hello", 42)
		noteGet(x)
	`)
	if v2.Tag != VTStr || v2.Data.(string) != "hello" {
		t.Fatalf("noteGet should return 'hello', got %#v", v2)
	}

	// Works for composite values too (array)
	v3 := evalWithIP(t, ip, `
		let x = noteSet("arr", [1,2,3])
		noteGet(x)
	`)
	if v3.Tag != VTStr || v3.Data.(string) != "arr" {
		t.Fatalf("noteGet(array) should return 'arr', got %#v", v3)
	}
}

func Test_Builtin_Introspection_bindings_order_and_shadowing(t *testing.T) {
	ip, _ := NewRuntime()

	// Force a NEW FRAME via a function so snapshot() is taken in the inner scope.
	v := evalWithIP(t, ip, `
		let a = 1
		let z = 9

		let inner = fun() do
			let b = 2
			let a = 3
			snapshot()
		end

		inner()
	`)
	m := mustMap(t, v)

	only := func(keys []string, allowed map[string]bool) []string {
		out := make([]string, 0, len(keys))
		for _, k := range keys {
			if k == "_" { // implementation detail key, skip
				continue
			}
			if allowed[k] {
				out = append(out, k)
			}
		}
		return out
	}

	// snapshot(): merged (inner → outer)
	keys := only(m.Keys, map[string]bool{"a": true, "b": true, "z": true})
	if len(keys) != 3 || keys[0] != "a" || keys[1] != "b" || keys[2] != "z" {
		t.Fatalf("snapshot() keys want [a b z], got %#v", keys)
	}

	// Shadowing preserved: inner 'a' should win in merged view.
	av, ok := m.Entries["a"]
	if !ok {
		t.Fatalf("snapshot() missing key 'a'")
	}
	if av.Tag != VTInt || av.Data.(int64) != 3 {
		t.Fatalf("snapshot().a should be 3 (inner), got %#v", av)
	}
}

func Test_Builtin_Introspection_astParse_and_astEval_roundtrip(t *testing.T) {
	ip, _ := NewRuntime()

	// Parse then eval simple expression
	v := evalWithIP(t, ip, `
		let ast = astParse("1 + 2")
		astEval(ast)
	`)
	if v.Tag != VTInt || v.Data.(int64) != 3 {
		t.Fatalf("astEval(astParse('1+2')) should be 3, got %#v", v)
	}

	// astParse error path returns a SOFT error (annotated null value).
	v2 := evalWithIP(t, ip, `
  let a = astParse("let")
  noteGet(a)
`)
	if v2.Tag != VTStr || v2.Data.(string) == "" {
		t.Fatalf("astParse error should be a soft error with a message, got %#v", v2)
	}
}

func Test_Builtin_Introspection_astEval_operates_in_caller_scope(t *testing.T) {
	ip, _ := NewRuntime()

	// astEval should mutate caller's env (not a fresh child)
	v := evalWithIP(t, ip, `
		let x = 1
		let rt = astParse("x = x + 1")
		astEval(rt)
		x
	`)
	if v.Tag != VTInt || v.Data.(int64) != 2 {
		t.Fatalf("astEval should update caller scope; want x=2, got %#v", v)
	}
}

func Test_Builtin_Introspection_reflect_and_reify_roundtrip(t *testing.T) {
	ip, _ := NewRuntime()

	// Round-trip a composite value
	v := evalWithIP(t, ip, `
		let orig = {a: 1, b: [2,3]}
		let rt   = reflect(orig)
		let v2   = reify(rt)
		str(orig) == str(v2)
	`)
	if v.Tag != VTBool || v.Data.(bool) != true {
		t.Fatalf("reflect → reify should round-trip, got %#v", v)
	}
}

func Test_Builtin_Introspection_reify_persistent_scope(t *testing.T) {
	ip, _ := NewRuntime()

	// reify evaluates in persistent/global scope; definition should persist
	v := evalWithIP(t, ip, `
		let rt = astParse("let y = 7")
		reify(rt)
		y
	`)
	if v.Tag != VTInt || v.Data.(int64) != 7 {
		t.Fatalf("reify should install into Global; want y=7, got %#v", v)
	}
}

func Test_Builtin_Introspection_reify_malformed_is_hard_error(t *testing.T) {
	ip, _ := NewRuntime()

	// Malformed runtime-S: ["id"] (missing name) → IxReify must HARD-fail.
	_, err := ip.EvalSource(`reify(["id"])`)
	if err == nil {
		t.Fatalf("expected hard error from reify on malformed runtime-S")
	}
	lc := strings.ToLower(err.Error())
	if !strings.Contains(lc, "reify") && !strings.Contains(lc, "invalid") && !strings.Contains(lc, "id") {
		t.Fatalf("unexpected error from reify: %v", err)
	}
}

// IxToS ↔ IxFromS round-trip on hand-built S (no parsing involved).
func Test_Builtin_Introspection_IxRoundTrip(t *testing.T) {
	orig := S{
		"assign",
		S{"decl", "x"},
		S{"array",
			S{"int", int64(1)},
			S{"map",
				S{"pair", S{"str", "a"}, S{"bool", true}},
			},
		},
	}
	rt := IxToS(orig)
	back, err := IxFromS(rt)
	if err != nil {
		t.Fatalf("IxFromS failed: %v", err)
	}
	if !equalS(orig, back) {
		t.Fatalf("round-trip mismatch\norig=%#v\nback=%#v", orig, back)
	}
}

// Annotation preservation through reflect/reify (container + nested).
func Test_Builtin_Introspection_Annotations_ReflectReify_Preserve(t *testing.T) {
	ip, _ := NewRuntime()
	v := evalWithIP(t, ip, `
		let v = {a: noteSet("A", [noteSet("B", 1), 2])}
		let rt = reflect(v)
		let w  = reify(rt)
		[ noteGet(v.a), noteGet(w.a), noteGet(v.a[0]), noteGet(w.a[0]) ]
	`)
	if v.Tag != VTArray {
		t.Fatalf("expected array of notes, got %#v", v)
	}
	xs := v.Data.(*ArrayObject).Elems
	want := []string{"A", "A", "B", "B"}
	for i := range want {
		if xs[i].Tag != VTStr || xs[i].Data.(string) != want[i] {
			t.Fatalf("note[%d] want %q, got %#v", i, want[i], xs[i])
		}
	}
}

// Canonical module lowering in reflect; reify exports.
func Test_Builtin_Introspection_Reflect_Module_CanonicalAssigns(t *testing.T) {
	ip, _ := NewRuntime()
	rt := evalWithIP(t, ip, `
		reflect(module "M" do
			let a = 1
			let b = 2
		end)
	`)
	if rt.Tag != VTArray {
		t.Fatalf("reflect(module) should return runtime-S array, got %#v", rt)
	}
	// Decode to S and assert: ["module", ["str","M"], ["block", ("assign", ...)*]]
	s, err := IxFromS(rt)
	if err != nil {
		t.Fatalf("IxFromS failed: %v", err)
	}
	if s[0] != "module" {
		t.Fatalf("top tag want module, got %v", s[0])
	}
	body := s[2].(S)
	if body[0] != "block" {
		t.Fatalf("module body want block, got %v", body[0])
	}
	hasAssign := false
	for i := 1; i < len(body); i++ {
		if tag := body[i].(S)[0].(string); tag == "assign" {
			hasAssign = true
			break
		}
	}
	if !hasAssign {
		t.Fatalf("module body should contain assign nodes")
	}
	// Reify and access fields
	v := evalWithIP(t, ip, `
		let m = reify(reflect(module "M" do let a = 1 end))
		m.a
	`)
	if v.Tag != VTInt || v.Data.(int64) != 1 {
		t.Fatalf("reified module should export a=1, got %#v", v)
	}
}

// Function reflection: scripted vs native; defaults to Any.
func Test_Builtin_Introspection_Reflect_Functions_ScriptedAndNativeDefaults(t *testing.T) {
	ip, _ := NewRuntime()
	scripted := evalWithIP(t, ip, `reflect(fun(x) do x end)`)
	native := evalWithIP(t, ip, `reflect(str)`)

	// Scripted
	sf, err := IxFromS(scripted)
	if err != nil {
		t.Fatalf("IxFromS(fun) failed: %v", err)
	}
	if sf[0] != "fun" {
		t.Fatalf("reflect(fun) top tag want fun, got %v", sf[0])
	}
	params := sf[1].(S)
	if params[0] != "array" || len(params) != 2 {
		t.Fatalf("params want one pair in array, got %#v", params)
	}
	pair := params[1].(S)
	if pair[0] != "pair" || pair[1].(S)[0] != "id" || pair[2].(S)[0] != "id" || pair[2].(S)[1] != "Any" {
		t.Fatalf("param should default to Any, got %#v", pair)
	}
	if sf[2].(S)[0] != "id" || sf[2].(S)[1] != "Any" {
		t.Fatalf("return type should default to Any, got %#v", sf[2])
	}

	// Native may be wrapped in ["annot", ["str",doc], ["id","str"]]
	nf, err := IxFromS(native)
	if err != nil {
		t.Fatalf("IxFromS(native) failed: %v", err)
	}
	if nf[0] == "annot" {
		nf = nf[2].(S) // unwrap top-level annot
	}
	if nf[0] != "id" || nf[1] != "str" {
		t.Fatalf(`reflect(str) want ["id","str"] (optionally wrapped in annot), got %#v`, nf)
	}
}

// Oracle reflection: 4th child is an **expression** (not a block).
func Test_Builtin_Introspection_Reflect_Oracle_SourceIsExpr(t *testing.T) {
	ip, _ := NewRuntime()
	// Use a literal so reflection doesn't try to evaluate a free var.
	rt := evalWithIP(t, ip, `reflect(oracle(x: Int) -> Int from [[1,1]])`)
	s, err := IxFromS(rt)
	if err != nil {
		t.Fatalf("IxFromS(oracle) failed: %v", err)
	}
	if s[0] != "oracle" {
		t.Fatalf("top tag want oracle, got %v", s[0])
	}
	src := s[3].(S)
	if src[0] == "block" {
		t.Fatalf("oracle source must be an expression, got block")
	}
}

// Value maps never carry requiredness ("pair!" forbidden).
func Test_Builtin_Introspection_Reflect_Map_NoPairBang(t *testing.T) {
	ip, _ := NewRuntime()
	rt := evalWithIP(t, ip, `reflect({a: 1, b: 2})`)
	s, err := IxFromS(rt)
	if err != nil {
		t.Fatalf("IxFromS(map) failed: %v", err)
	}
	if s[0] != "map" {
		t.Fatalf("top tag want map, got %v", s[0])
	}
	for i := 1; i < len(s); i++ {
		if s[i].(S)[0] == "pair!" {
			t.Fatalf("value map must not use 'pair!' (requiredness is type-only)")
		}
	}
}

// Validator: nested ["annot", ...] is rejected; code = E_ANNOT_NESTED.
func Test_Builtin_Introspection_Validator_NestedAnnot_Rejected(t *testing.T) {
	nested := S{"annot", S{"str", "A"}, S{"annot", S{"str", "B"}, S{"int", int64(1)}}}
	errs := IxValidateS(nested)
	if errs.Tag != VTArray {
		t.Fatalf("validator must return array, got %#v", errs)
	}
	found := false
	for _, e := range errs.Data.(*ArrayObject).Elems {
		if e.Tag == VTMap {
			m := e.Data.(*MapObject).Entries
			if c, ok := m["code"]; ok && c.Tag == VTStr && c.Data.(string) == "E_ANNOT_NESTED" {
				found = true
				break
			}
		}
	}
	if !found {
		t.Fatalf("expected E_ANNOT_NESTED in validator errors, got %#v", errs)
	}
}

// Validator: enum members may be expressions.
func Test_Builtin_Introspection_Validator_Enum_MemberExprOK(t *testing.T) {
	ty := S{"type", S{"enum",
		S{"int", int64(1)},
		S{"binop", "+", S{"int", int64(1)}, S{"int", int64(2)}},
	}}
	errs := IxValidateS(ty)
	if errs.Tag != VTArray || len(errs.Data.(*ArrayObject).Elems) != 0 {
		t.Fatalf("enum with expression member should validate (no errors), got %#v", errs)
	}
}

// Type/subtyping sanity check (spot checks).
func Test_Builtin_Introspection_Types_Subtyping_Sanity(t *testing.T) {
	ip, _ := NewRuntime()
	is := func(a, b S) bool { return ip.IsSubtype(a, b, nil) }

	// Int <: Num
	if !is(S{"id", "Int"}, S{"id", "Num"}) {
		t.Fatalf("Int <: Num expected")
	}
	// [Int] <: [Num] (covariant)
	if !is(S{"array", S{"id", "Int"}}, S{"array", S{"id", "Num"}}) {
		t.Fatalf("[Int] <: [Num] expected (covariant)")
	}

	// Requiredness: stronger (required) <: weaker (optional) — OK
	a := S{"map", S{"pair!", S{"str", "k"}, S{"id", "Int"}}}
	b := S{"map", S{"pair", S{"str", "k"}, S{"id", "Num"}}}
	if !is(a, b) {
		t.Fatalf(`{"k!": Int} <: {"k": Num} should hold (cannot relax on RHS)`)
	}
	// But the reverse should fail
	if is(b, a) {
		t.Fatalf(`{"k": Num} </: {"k!": Int} expected`)
	}

	// Function contra/co: (Num -> Int) <: (Int -> Num)
	f1 := S{"binop", "->", S{"id", "Num"}, S{"id", "Int"}}
	f2 := S{"binop", "->", S{"id", "Int"}, S{"id", "Num"}}
	if !is(f1, f2) {
		t.Fatalf("(Num->Int) <: (Int->Num) expected (contra/co)")
	}
}

// Printer: elisions (Any) and POST-after-comma newline behavior.
func Test_Builtin_Introspection_Printer_ElisionsAndPost(t *testing.T) {
	// fun with default Any param/ret should omit types
	fn := S{"fun",
		S{"array", S{"pair", S{"id", "x"}, S{"id", "Any"}}},
		S{"id", "Any"},
		S{"block"},
	}
	out := FormatSExpr(fn)
	if strings.Contains(out, "->") || !strings.Contains(out, "fun(x) do") {
		t.Fatalf("fun defaults should elide Any; got:\n%s", out)
	}

	// array with last element POST should place closing ']' next line (no extra blank)
	arr := S{"array",
		S{"annot", S{"str", "<last"}, S{"int", int64(1)}},
	}
	src := FormatSExpr(arr)
	if !strings.Contains(src, "# last") || !strings.Contains(src, "]") {
		t.Fatalf("array last POST should render inline comment and close next line; got:\n%s", src)
	}
}
