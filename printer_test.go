// printer_test.go
package mindscript

import (
	"reflect"
	"strings"
	"testing"
)

func pretty(t *testing.T, src string) string {
	t.Helper()
	out, err := Pretty(src)
	if err != nil {
		t.Fatalf("Pretty error: %v\nsource:\n%s", err, src)
	}
	return out
}

func parse(t *testing.T, src string) S {
	t.Helper()
	sexpr, err := ParseSExpr(src)
	if err != nil {
		t.Fatalf("Parse error: %v\nsource:\n%s", err, src)
	}
	return sexpr
}

func norm(s string) string { return strings.TrimSpace(s) }

func eq(t *testing.T, got, want string) {
	t.Helper()
	if strings.TrimSpace(got) != strings.TrimSpace(want) {
		t.Fatalf("pretty mismatch:\n--- got ---\n%s\n--- want ---\n%s", got, want)
	}
}

// ---------- Core expression/precedence & chaining ----------

func Test_Printer_Operators_And_Grouping(t *testing.T) {
	cases := []struct{ in, want string }{
		{`1 + 2 * 3`, `1 + 2 * 3`},
		{`(1 + 2) * 3`, `(1 + 2) * 3`},
		{`- (a + b)`, `-(a + b)`},
		{`not a and b`, `not a and b`}, // (not a) and b
		{`a < b == c`, `a < b == c`},   // (< a b) == c
	}
	for _, tc := range cases {
		got := pretty(t, tc.in)
		if norm(got) != tc.want {
			t.Fatalf("pretty mismatch\nin:   %q\nwant: %q\ngot:  %q", tc.in, tc.want, got)
		}
	}
}

func Test_Printer_Chaining_Call_Idx_Get(t *testing.T) {
	in := `obj.name(1, 2)[i]."weird"`
	want := `obj.name(1, 2)[i].weird`
	got := pretty(t, in)
	if norm(got) != want {
		t.Fatalf("pretty chain mismatch\nwant: %q\ngot:  %q", want, got)
	}
}

// Dot-number shorthand: obj.12 → obj[12]
func Test_Printer_Dot_Number_Indexing_Shorthand(t *testing.T) {
	eq(t, pretty(t, "obj.12"), "obj[12]")
}

// Computed dot -> indexing: a.(1+2) → a[1 + 2]
func Test_Printer_ComputedDot_NormalizesToIndexing(t *testing.T) {
	in := `a.(1 + 2)`
	want := `a[1 + 2]`
	got := pretty(t, in)
	eq(t, got, want)
}

// Enum rendered as an expression
func Test_Printer_Enum_Expression_Format(t *testing.T) {
	eq(t, pretty(t, `Enum["a", 1, {k: 2}]`), `Enum["a", 1, {k: 2}]`)
}

// ---------- Functions, oracles, control flow, modules ----------

func Test_Printer_Function_And_Oracle(t *testing.T) {
	in := `fun(a: Str) -> Str do return("hi " + a) end
oracle() -> Str from["web","docs"]`
	want := "fun(a: Str) -> Str do\n" +
		"\treturn \"hi \" + a\n" +
		"end\n" +
		"oracle() -> Str from [\"web\", \"docs\"]"
	got := pretty(t, in)
	if norm(got) != norm(want) {
		t.Fatalf("pretty fun/oracle mismatch\nwant:\n%s\n---\ngot:\n%s", want, got)
	}
}

// Ensure fun omits -> Any and oracle omits default 'from'
func Test_Printer_Fun_And_Oracle_Default_Headers(t *testing.T) {
	in := `fun(a: Int) do end
oracle()`
	want := "fun(a: Int) do\n\nend\noracle()"
	got := pretty(t, in)
	eq(t, got, want)
}

func Test_Printer_If_Elif_Else_And_For(t *testing.T) {
	in := `if a then x elif b then y else z end
for let x in xs do break 0 end`
	want := "if a then\n" +
		"\tx\n" +
		"elif b then\n" +
		"\ty\n" +
		"else\n" +
		"\tz\n" +
		"end\n" +
		"for x in xs do\n" +
		"\tbreak 0\n" +
		"end"
	got := pretty(t, in)
	if norm(got) != norm(want) {
		t.Fatalf("pretty if/for mismatch\nwant:\n%s\n---\ngot:\n%s", want, got)
	}
}

func Test_Printer_While(t *testing.T) {
	in := `while a < b do a = a + 1 end`
	want := "while a < b do\n" +
		"\ta = a + 1\n" +
		"end"
	got := pretty(t, in)
	eq(t, got, want)
}

func Test_Printer_Module_Variants(t *testing.T) {
	// Basic
	in := `module MyLib do x = 1 end`
	want := "module MyLib do\n" +
		"\tx = 1\n" +
		"end"
	eq(t, pretty(t, in), want)

	// String name + empty body
	eq(t, pretty(t, `module "my mod" do end`), "module \"my mod\" do\n\nend")

	// Name as expression (keeps minimal parens)
	eq(t, pretty(t, `module "a" + "b" do end`), "module \"a\" + \"b\" do\n\nend")

	// With pre-annotation
	in4 := `# about this module
module M do
end`
	want4 := "# about this module\nmodule M do\n\nend"
	eq(t, pretty(t, in4), want4)

	// Module amid other statements
	in5 := `let x = 1
module M do
  x = 2
end
return x`
	want5 := "let x = 1\nmodule M do\n\tx = 2\nend\nreturn x"
	eq(t, pretty(t, in5), want5)
}

// ---------- Maps, arrays, strings, destructuring ----------

func Test_Printer_Maps_Arrays_Strings(t *testing.T) {
	// Note: Go string literal contains an actual newline in "Jo\nhn".
	in := "{\n  \"name\":\"Jo\nhn\", age:25\n} [1,2,3] []"
	want := "{name: \"Jo\\nhn\", age: 25}\n[1, 2, 3]\n[]"
	got := pretty(t, in)
	if norm(got) != norm(want) {
		t.Fatalf("pretty map/array mismatch\nwant:\n%s\n---\ngot:\n%s", want, got)
	}
}

func Test_Printer_Maps_And_Arrays_And_Quoting(t *testing.T) {
	in := `{ok: 1, "weird key": 2, also_ok: [1,2,3]}`
	want := `{ok: 1, "weird key": 2, also_ok: [1, 2, 3]}`
	got := pretty(t, in)
	eq(t, got, want)
}

// Keywords permitted as map keys & pattern fields (bare, not quoted)
func Test_Printer_Keywords_As_Keys_In_Maps_And_Patterns(t *testing.T) {
	src := `
let {if: x, else: y, type: z} = {if: 1, else: 2, type: 3}
`
	out := pretty(t, src)
	want := `
let {if: x, else: y, type: z} = {if: 1, else: 2, type: 3}
`
	eq(t, out, want)
}

func Test_Printer_Destructuring_Assign(t *testing.T) {
	in := `let {"x": a, y: b} = m`
	// Identifier-like keys are unquoted by the printer.
	want := `let {x: a, y: b} = m`
	got := pretty(t, in)
	eq(t, got, want)
}

func Test_Printer_Let_Array_And_Object_Patterns(t *testing.T) {
	src := `
let [x, y] = [1,2]
let {name: n, age: a} = {name: "Bob", age: 40}
`
	out := pretty(t, src)
	want := `
let [x, y] = [1, 2]
let {name: n, age: a} = {name: "Bob", age: 40}
`
	eq(t, out, want)
}

func Test_Printer_Let_ObjectPattern_With_Annotations_Multiline(t *testing.T) {
	// Uses PRE hash-line annotations for value positions in an object pattern.
	in := "let {\n" +
		"\tname: \n" +
		"\t# username\n" +
		"\tn,\n" +
		"\tage: \n" +
		"\t# yearsOld\n" +
		"\ta\n" +
		"} = {name: \"Bob\", age: 40}"
	want := "let {\n" +
		"\tname:\n" +
		"\t\t# username\n" +
		"\t\tn,\n" +
		"\tage:\n" +
		"\t\t# yearsOld\n" +
		"\t\ta\n" +
		"} = {name: \"Bob\", age: 40}"
	got := pretty(t, in)
	eq(t, got, want)
}

// Assignment stays assignment (no desugaring into let)
func Test_Printer_Assign_NonDecl_Remains_Assignment(t *testing.T) {
	src := `
a = 1
(a).b = 2
`
	out := pretty(t, src)
	want := `
a = 1
a.b = 2
`
	eq(t, out, want)
}

// ---------- Control forms adjacency & grouping ----------

func Test_Printer_Control_Forms_Are_Adjacent(t *testing.T) {
	in := `return(1) break(0) continue(null)`
	want := `return 1
break 0
continue`
	got := pretty(t, in)
	if norm(got) != norm(want) {
		t.Fatalf("pretty control mismatch\nwant:\n%s\n---\ngot:\n%s", want, got)
	}
}

func Test_Printer_ReturnBreakContinue_SameLineVsNewline(t *testing.T) {
	// Same-line expression → carry the expression.
	casesSame := []struct{ in, want string }{
		{`return 1`, `return 1`},
		{`break  x`, `break x`},
		{`continue "z"`, `continue "z"`},
	}
	for _, tc := range casesSame {
		got := pretty(t, tc.in)
		eq(t, got, tc.want)
	}

	// Next token on the next line → implicit null.
	in := `return
x`
	want := `return
x`
	got := pretty(t, in)
	eq(t, got, want)
}

func Test_Printer_Grouping_With_Postfix_Question(t *testing.T) {
	in := `(a + b)?`
	want := `(a + b)?`
	got := pretty(t, in)
	eq(t, got, want)
}

// ---------- Property name normalization ----------

func Test_Printer_Property_Name_Normalization_And_Quoting(t *testing.T) {
	// Keywords after '.' normalize to bare identifiers.
	eq(t, pretty(t, `obj."then"`), `obj.then`)
	// Non-identifier names remain quoted.
	eq(t, pretty(t, `obj."not ident"`), `obj."not ident"`)
}

// ---------- Annotations (PRE/POST) ----------

func Test_Printer_Annotations_PRE(t *testing.T) {
	in := `# hello
x`
	got := pretty(t, in)
	if norm(got) != norm(in) {
		t.Fatalf("pretty annotation mismatch:\n%q", got)
	}
}

func Test_Printer_Annotations_LineBlocks(t *testing.T) {
	in := `# first
# second
x`
	want := `# first
# second
x`
	got := pretty(t, in)
	eq(t, got, want)
}

func Test_Printer_Annotations_TwoSeparateBlocks(t *testing.T) {
	in := `# a

# b
x`
	// Two separate annotations should stay as two header lines.
	want := `# a

# b
x`
	got := pretty(t, in)
	eq(t, got, want)
}

func Test_Printer_Annotations_Post_Trailing_Inline(t *testing.T) {
	// Same-line hash after an expr becomes a POST annotation
	eq(t, pretty(t, `x # after`), `x # after`)
}

func Test_Printer_Map_Value_Post_Inline(t *testing.T) {
	// POST after a map value consumes the rest of the line,
	// so the closing brace must move to the next line.
	got := pretty(t, "{a: 1 # note\n}")
	want := "{\n\ta: 1 # note\n}"
	eq(t, got, want)
}

// ---------- Pretty/Standardize roundtrip & idempotence ----------

func Test_Printer_Idempotent_And_Roundtrip(t *testing.T) {
	src := `
# demo
let Person = type {name!: Str?, hobbies: [Str]?, age: Int}
fun(g: Str) -> Str do
  return("hi " + g)
end
oracle() from["web"]`

	once := pretty(t, src)
	twice := pretty(t, once)
	if once != twice {
		t.Fatalf("pretty not idempotent\nonce:\n%s\n---\ntwice:\n%s", once, twice)
	}

	// AST roundtrip: parse original and pretty'ed, ensure same AST
	ast1 := parse(t, once)
	ast2 := parse(t, twice)
	if !reflect.DeepEqual(ast1, ast2) {
		t.Fatalf("AST roundtrip mismatch after pretty")
	}
}

func Test_Printer_RoundTrip_Samples(t *testing.T) {
	samples := []string{
		`f ( x )`,      // grouping vs call (space before '(')
		`f(x , 1,  2)`, // sloppy arg spacing
		`arr [ i ]`,    // spacing before '[' breaks indexing
		`# note
f(x)`,
		`if a then x elif b then y else z end`,
		`{ok:1,"bad key":2}`,
	}
	for _, src := range samples {
		p1 := pretty(t, src)
		p2 := pretty(t, p1)
		if p2 != p1 {
			t.Fatalf("pretty not idempotent for:\n%s\n---1---\n%s\n---2---\n%s", src, p1, p2)
		}
		// Parse equality (AST)
		a1 := parse(t, src)
		a2 := parse(t, p1)
		if !reflect.DeepEqual(a1, a2) {
			t.Fatalf("AST mismatch after pretty for:\n%s\n---a1---\n%v\n---a2---\n%v", src, a1, a2)
		}
	}
}

func Test_Printer_RoundTrip_Standardize(t *testing.T) {
	cases := []string{
		// Simple expressions and ops
		`x=1+2*3`,

		// Calls / idx / get
		`f( a, b )[i].name(3)`,

		// Array with pre-annotation on the next element (comma-before-comment form)
		`arr = [1, # note about 2
2]`,

		// Map with a key annotation and multiple fields
		`m = { a: 1, # key doc
b: 2 }`,

		// Function + control flow in a block
		`do
	x = 1 + 2 * 3
	if x < 10 then
		return(x)
	elif x == 10 then
		break(null)
	else
		continue(false)
	end
end`,

		// Oracle with non-empty from expression (any expr that evaluates to an array)
		`res = oracle(a: Int) -> Str from sources()`,

		// Destructuring with annotated key in an object pattern
		`let { # id of the user
userId: id, profile: { name: n } } = obj`,
	}

	for i, in := range cases {
		std1, err := Standardize(in)
		if err != nil {
			t.Fatalf("case %d: Standardize(in) error: %v\nin:\n%s", i, err, in)
		}

		ast1 := parse(t, std1)

		std2, err := Standardize(std1)
		if err != nil {
			t.Fatalf("case %d: Standardize(std1) error: %v\nstd1:\n%s", i, err, std1)
		}

		// The standardized source should be idempotent.
		eq(t, std2, std1)

		// And the ASTs should be structurally equal.
		ast2 := parse(t, std2)
		if !reflect.DeepEqual(ast1, ast2) {
			t.Fatalf("case %d: AST mismatch after second pass\n--- ast1 (from std1) ---\n%#v\n--- ast2 (from std2) ---\n%#v\nstd1:\n%s\nstd2:\n%s", i, ast1, ast2, std1, std2)
		}
	}
}

func Test_Standardize_Trailing_Newline_And_Idempotence(t *testing.T) {
	src := `fun(a: Str) -> Str do return("hi " + a) end`
	std1, err := Standardize(src)
	if err != nil {
		t.Fatal(err)
	}
	if !strings.HasSuffix(std1, "\n") {
		t.Fatalf("missing trailing newline:\n%q", std1)
	}

	std2, err := Standardize(std1)
	if err != nil {
		t.Fatal(err)
	}
	if std2 != std1 {
		t.Fatalf("not idempotent:\n---1---\n%q\n---2---\n%q", std1, std2)
	}
}

// ---------- Indentation policy ----------

func Test_Printer_TabsOnly_Indentation(t *testing.T) {
	got := pretty(t, `if a then x else y end`)
	// Ensure no leading spaces are used for indentation on any line.
	lines := strings.Split(got, "\n")
	for i, ln := range lines {
		// Trim trailing empty last line if any
		if ln == "" && i == len(lines)-1 {
			continue
		}
		for j := 0; j < len(ln) && (ln[j] == ' ' || ln[j] == '\t'); j++ {
			if ln[j] == ' ' {
				t.Fatalf("found space in indentation at line %d: %q", i+1, ln)
			}
		}
	}
}

// ---------- FormatValue (runtime values) ----------

func Test_Printer_FormatValue_Scalars_And_AnnotatedNull(t *testing.T) {
	// construct values directly
	vNull := withAnnot(Null, "division by zero")
	vInt := Int(42)
	vStr := Str("hi")

	outNull := FormatValue(vNull)
	outInt := FormatValue(vInt)
	outStr := FormatValue(vStr)

	// Annotation should be a header line (single '#') before the value.
	if !strings.Contains(outNull, "# division by zero") || !strings.Contains(outNull, "\nnull") {
		t.Fatalf("annotated null not rendered properly:\n%s", outNull)
	}
	if strings.TrimSpace(outInt) != "42" {
		t.Fatalf("int not rendered: %q", outInt)
	}
	if strings.TrimSpace(outStr) != `"hi"` {
		t.Fatalf("str not rendered/quoted: %q", outStr)
	}
}

func Test_Printer_FormatValue_Array_And_Map(t *testing.T) {
	// Array with mixed values, including annotated null
	arr := Arr([]Value{Int(1), withAnnot(Null, "missing"), Str("x")})
	outArr := FormatValue(arr)

	// Should render as multi-line because an element has a header annotation
	if !strings.Contains(outArr, "[") || !strings.Contains(outArr, "]") || !strings.Contains(outArr, "\n") {
		t.Fatalf("array not formatted multi-line: %q", outArr)
	}
	if !strings.Contains(outArr, "# missing") {
		t.Fatalf("annotation in array element missing:\n%s", outArr)
	}

	// Map should render keys in stable (sorted) order
	m := Map(map[string]Value{
		"b": Int(2),
		"a": Int(1),
	})
	outMap := FormatValue(m)
	// Expect "a" before "b"
	if strings.Index(outMap, "a:") > strings.Index(outMap, "b:") {
		t.Fatalf("map keys not sorted:\n%s", outMap)
	}
	// Basic structure
	if !strings.Contains(outMap, "{") || !strings.Contains(outMap, "}") {
		t.Fatalf("map not braced: %q", outMap)
	}
}

// Inline array + numeric decimal rendering
func Test_Printer_FormatValue_Array_Inline_And_NumDecimal(t *testing.T) {
	outArr := FormatValue(Arr([]Value{Int(1), Int(2)}))
	eq(t, outArr, "[1, 2]")

	outNum := FormatValue(Num(3))
	eq(t, outNum, "3.0")
}

// PRE placement (key + value) inside map values
func Test_Printer_Value_Map_KeyAndValuePREPlacement(t *testing.T) {
	mo := &MapObject{
		Entries: map[string]Value{},
		KeyAnn:  map[string]string{},
		Keys:    []string{"name"},
	}
	v := Str("John McCarthy")
	v.Annot = "John's name" // value PRE
	mo.Entries["name"] = v
	mo.KeyAnn["name"] = "The name" // key PRE

	got := FormatValue(Value{Tag: VTMap, Data: mo})
	want := "{\n\t# The name\n\tname:\n\t\t# John's name\n\t\t\"John McCarthy\"\n}"
	if norm(got) != norm(want) {
		t.Fatalf("value map PRE placement mismatch\nwant:\n%s\n---\ngot:\n%s", want, got)
	}
}

// POST ordering: value POST then key POST when both present (inline form)
func Test_Printer_Value_Map_Post_Order(t *testing.T) {
	mo := &MapObject{
		Entries: map[string]Value{},
		KeyAnn:  map[string]string{},
		Keys:    []string{"a"},
	}
	v := Int(1)
	v.Annot = "<v" // value POST
	mo.Entries["a"] = v
	mo.KeyAnn["a"] = "<k" // key POST
	got := FormatValue(Value{Tag: VTMap, Data: mo})
	want := "{\n\ta: # k\n\t\t1 # v\n}"
	eq(t, got, want)
}

// ---------- Value rendering: Fun/Oracle/Type/Module/Handle ----------

func Test_Printer_Fun_Rendering_ZeroArg(t *testing.T) {
	f := &Fun{
		Params:     nil,
		ParamTypes: nil,
		ReturnType: S{"id", "Any"},
	}
	got := FormatValue(FunVal(f))
	want := "<fun: _:Null -> Any>"
	if got != want {
		t.Fatalf("fun rendering mismatch:\n got:  %q\n want: %q", got, want)
	}
}

func Test_Printer_Fun_Rendering_With_Params(t *testing.T) {
	f := &Fun{
		Params:     []string{"a", "b"},
		ParamTypes: []S{S{"id", "Int"}, S{"id", "Str"}},
		ReturnType: S{"id", "Bool"},
	}
	got := FormatValue(FunVal(f))
	want := "<fun: a:Int -> b:Str -> Bool>"
	if got != want {
		t.Fatalf("fun rendering (with params) mismatch:\n got:  %q\n want: %q", got, want)
	}
}

func Test_Printer_Oracle_Rendering(t *testing.T) {
	f := &Fun{
		Params:     nil,
		ParamTypes: nil,
		ReturnType: S{"id", "Str"},
		IsOracle:   true,
	}
	got := FormatValue(FunVal(f))
	want := "<oracle: _:Null -> Str>"
	if got != want {
		t.Fatalf("oracle rendering mismatch:\n got:  %q\n want: %q", got, want)
	}
}

func Test_Printer_Type_Rendering(t *testing.T) {
	// { age: Int, name: Str } — order is sorted by printer
	env := NewEnv(nil)
	typ := S{
		"map",
		S{"pair", S{"str", "name"}, S{"id", "Str"}},
		S{"pair", S{"str", "age"}, S{"id", "Int"}},
	}
	got := FormatValue(TypeValIn(typ, env))
	want := "<type: {age: Int, name: Str}>"
	if got != want {
		t.Fatalf("type rendering mismatch:\n got:  %q\n want: %q", got, want)
	}
}

// Type printer: basic arrow/optional + enum literal
func Test_FormatType_Basics(t *testing.T) {
	// T? where T = [Int] -> Str
	T := S{"binop", "->", S{"array", S{"id", "Int"}}, S{"id", "Str"}}
	opt := S{"unop", "?", T}
	got := FormatType(opt)
	want := `[Int] -> Str?`
	if norm(got) != norm(want) {
		t.Fatalf("FormatType mismatch\nwant: %q\ngot:  %q", want, got)
	}

	// Enum type with scalars and a map literal
	enum := S{"enum",
		S{"str", "a"},
		S{"int", int64(1)},
		S{"map", S{"pair", S{"str", "k"}, S{"num", 1.5}}},
	}
	got2 := FormatType(enum)
	want2 := `Enum["a", 1, {k: 1.5}]`
	if norm(got2) != norm(want2) {
		t.Fatalf("FormatType enum mismatch\nwant: %q\ngot:  %q", want2, got2)
	}
}

// Type printer: required field + PRE/POST annotations
func Test_FormatType_Required_And_Annotations(t *testing.T) {
	typ := S{"map",
		S{"pair!", // required field
			S{"annot", S{"str", "Key pre"}, S{"str", "id"}},     // key PRE
			S{"annot", S{"str", "<Value post"}, S{"id", "Int"}}, // value POST
		},
	}
	got := FormatType(typ)
	want := "{\n\t# Key pre\n\tid!: Int # Value post\n}"
	eq(t, got, want)
}

func Test_Printer_Module_Rendering(t *testing.T) {
	// Minimal module with a display name that should pass through prettySpec unchanged.
	m := &Module{Name: "MyMod"}
	got := FormatValue(Value{Tag: VTModule, Data: m})
	want := "<module: MyMod>"
	if got != want {
		t.Fatalf("module rendering mismatch:\n got:  %q\n want: %q", got, want)
	}
}

func Test_Printer_Handle_Rendering(t *testing.T) {
	h := HandleVal("token", 123)
	got := FormatValue(h)
	want := "<handle: token>"
	if got != want {
		t.Fatalf("handle rendering mismatch:\n got:  %q\n want: %q", got, want)
	}
}

func Test_Printer_Value_Type_EmptyObject_NoSpace(t *testing.T) {
	env := NewEnv(nil)
	got := FormatValue(TypeValIn(S{"map"}, env))
	want := "<type: {}>"
	if norm(got) != norm(want) {
		t.Fatalf("empty object type spacing mismatch\nwant: %q\ngot:  %q", want, got)
	}
}

// ---------- POST annotations after comma/colon ----------

// Arrays (expressions): POST should render *after the comma*.
func Test_Printer_Array_Post_After_Comma(t *testing.T) {
	in := `let a = [1, # here
2]`
	want := "let a = [\n" +
		"\t1, # here\n" +
		"\t2\n" +
		"]"
	eq(t, pretty(t, in), want)
}

// Enums (expressions): same comma behavior inside Enum[...].
func Test_Printer_Enum_Post_After_Comma(t *testing.T) {
	in := `Enum[1, # post
2]`
	want := "Enum[\n" +
		"\t1, # post\n" +
		"\t2\n" +
		"]"
	eq(t, pretty(t, in), want)
}

// Maps (expressions): value POST should render *after the comma* that follows the entry.
// Key POST should render *after the colon* on the key line.
func Test_Printer_Map_Posts_After_Comma_And_After_Colon(t *testing.T) {
	// Value POST after comma
	in1 := `let a = {x: 1, # vpost
y: 2}`
	want1 := "let a = {\n" +
		"\tx: 1, # vpost\n" +
		"\ty: 2\n" +
		"}"
	eq(t, pretty(t, in1), want1)

	// Key POST after colon; value on next line, comma after the value line
	in2 := `let a = {x: # kpost
1, y: 2}`
	want2 := "let a = {\n" +
		"\tx: # kpost\n" +
		"\t\t1,\n" +
		"\ty: 2\n" +
		"}"
	eq(t, pretty(t, in2), want2)
}

// Type maps: same rules as maps — value POST after comma; key POST after colon.
func Test_Printer_TypeMap_Posts_After_Comma_And_After_Colon(t *testing.T) {
	// Value POST after comma
	in1 := `type {x: Int, # vpost
y: Str}`
	want1 := "type {\n" +
		"\tx: Int, # vpost\n" +
		"\ty: Str\n" +
		"}"
	eq(t, pretty(t, in1), want1)

	// Key POST after colon; type value on next line, comma after that line
	in2 := `type {x: # kpost
Int, y: Str}`
	want2 := "type {\n" +
		"\tx: # kpost\n" +
		"\t\tInt,\n" +
		"\ty: Str\n" +
		"}"
	eq(t, pretty(t, in2), want2)
}

// Array destructuring patterns: POST should render *after the comma*.
func Test_Printer_ArrayPattern_Post_After_Comma(t *testing.T) {
	in := `let [a, # post
b] = xs`
	want := "let [\n" +
		"\ta, # post\n" +
		"\tb\n" +
		"] = xs"
	eq(t, pretty(t, in), want)
}

// Object destructuring patterns: value POST after comma; key POST after colon.
func Test_Printer_ObjectPattern_Posts_After_Comma_And_After_Colon(t *testing.T) {
	// Value POST after comma between entries
	in1 := `let {x: p, # vpost
y: q} = xs`
	want1 := "let {\n" +
		"\tx: p, # vpost\n" +
		"\ty: q\n" +
		"} = xs"
	eq(t, pretty(t, in1), want1)

	// Key POST after colon; pattern value on next line, comma after that line
	in2 := `let {x: # kpost
p, y: q} = xs`
	want2 := "let {\n" +
		"\tx: # kpost\n" +
		"\t\tp,\n" +
		"\ty: q\n" +
		"} = xs"
	eq(t, pretty(t, in2), want2)
}

func Test_Printer_FunctionType_Parentheses(t *testing.T) {
	in := `fun(f: (Int -> Int) -> Int, g: Int -> Int -> Int) do end`
	want := `
fun(f: (Int -> Int) -> Int, g: Int -> Int -> Int) do

end`
	eq(t, pretty(t, in), want)
}

func Test_Printer_FunValue_FunctionTypeParam_Parens(t *testing.T) {
	// <fun: f:(Any -> Any) -> it:(Null -> Any) -> Null -> Any>
	f := &Fun{
		Params: []string{"f", "it"},
		ParamTypes: []S{
			{"binop", "->", S{"id", "Any"}, S{"id", "Any"}},
			{"binop", "->", S{"id", "Null"}, S{"id", "Any"}},
		},
		ReturnType: S{"binop", "->", S{"id", "Null"}, S{"id", "Any"}},
	}
	got := FormatValue(Value{Tag: VTFun, Data: f})
	want := "<fun: f:(Any -> Any) -> it:(Null -> Any) -> Null -> Any>"
	if strings.TrimSpace(got) != want {
		t.Fatalf("fun render mismatch\n--- got ---\n%s\n--- want ---\n%s", got, want)
	}
}

func Test_FormatType_RightAssociative_Arrows(t *testing.T) {
	tests := []struct {
		name string
		in   S
		want string
	}{
		{
			name: "single arg",
			in:   S{"binop", "->", S{"id", "Int"}, S{"id", "Int"}},
			want: "Int -> Int",
		},
		{
			name: "left is arrow: (Int -> Int) -> Int",
			in:   S{"binop", "->", S{"binop", "->", S{"id", "Int"}, S{"id", "Int"}}, S{"id", "Int"}},
			want: "(Int -> Int) -> Int",
		},
		{
			name: "right is arrow: Int -> (Int -> Int) prints as chain",
			in:   S{"binop", "->", S{"id", "Int"}, S{"binop", "->", S{"id", "Int"}, S{"id", "Int"}}},
			want: "Int -> Int -> Int",
		},
		{
			name: "long chain right-assoc",
			in: S{"binop", "->",
				S{"id", "A"},
				S{"binop", "->",
					S{"id", "B"},
					S{"binop", "->", S{"id", "C"}, S{"id", "R"}},
				},
			},
			want: "A -> B -> C -> R",
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			got := strings.TrimSpace(FormatType(tc.in))
			if got != tc.want {
				t.Fatalf("got %q want %q", got, tc.want)
			}
		})
	}
}

func Test_Printer_FunctionParam_Parentheses(t *testing.T) {
	// helper keeps consistency with existing pretty() + eq() style
	check := func(in, want string) {
		eq(t, pretty(t, in), want)
	}

	// fun(m: Int -> Int, n: Int) ==> fun(m: (Int -> Int), n: Int)
	check(
		`fun(m: Int -> Int, n: Int) do end`,
		`
fun(m: Int -> Int, n: Int) do

end`,
	)

	// fun(m: (Int -> Int) -> Int) ==> fun(m: ((Int -> Int) -> Int))
	check(
		`fun(m: (Int -> Int) -> Int) do end`,
		`
fun(m: (Int -> Int) -> Int) do

end`,
	)

	// Mixed case from earlier test: keeps right-assoc and wraps params
	check(
		`fun(f: (Int -> Int) -> Int, g: Int -> Int -> Int) do end`,
		`
fun(f: (Int -> Int) -> Int, g: Int -> Int -> Int) do

end`,
	)
}

func Test_Printer_Value_Cycle_ArraySelf(t *testing.T) {
	// a = []; a[0] = a
	ao := &ArrayObject{}
	a := Value{Tag: VTArray, Data: ao}
	ao.Elems = []Value{a}

	got := FormatValue(a)
	want := "[[...]]" // Python-style cycle marker inside array, rendered inline
	eq(t, got, want)
}

func Test_Printer_Value_Cycle_MapSelf(t *testing.T) {
	// m = {}; m.self = m
	mo := &MapObject{
		Entries: map[string]Value{},
		KeyAnn:  map[string]string{},
		Keys:    []string{},
	}
	m := Value{Tag: VTMap, Data: mo}
	mo.Entries["self"] = m
	mo.Keys = append(mo.Keys, "self")

	got := FormatValue(m)
	// Keys are sorted; single key "self" prints inline.
	want := "{self: {...}}"
	eq(t, got, want)
}

func Test_Printer_Value_Cycle_Cross_ArrayToMap(t *testing.T) {
	// a = [m]; m = { k: a }  (cross-reference; printing 'a' should show k: [...] )
	ao := &ArrayObject{}
	mo := &MapObject{
		Entries: map[string]Value{},
		KeyAnn:  map[string]string{},
		Keys:    []string{"k"},
	}

	a := Value{Tag: VTArray, Data: ao}
	m := Value{Tag: VTMap, Data: mo}

	ao.Elems = []Value{m}
	mo.Entries["k"] = a

	got := FormatValue(a)
	want := "[{k: [...]}]"
	eq(t, got, want)
}

func Test_Printer_Value_Cycle_ArrayElement_PostAfterComma(t *testing.T) {
	// a = [a # post, 1]  — POST should print after the comma of the first element and force newline.
	ao := &ArrayObject{}
	a := Value{Tag: VTArray, Data: ao}
	// First element is the array itself, *with* a POST annotation.
	first := a
	first.Annot = "< post here"
	ao.Elems = []Value{first, Int(1)}

	got := FormatValue(a)

	// Expect multi-line due to POST (HardLine).
	// Tabs for indentation (as the printer uses).
	want := "[\n\t[...], # post here\n\t1\n]"
	if strings.TrimSpace(got) != strings.TrimSpace(want) {
		t.Fatalf("cycle array with POST mismatch:\n--- got ---\n%s\n--- want ---\n%s", got, want)
	}
}

func Test_Printer_Value_Cycle_MapValue_PostAfterComma(t *testing.T) {
	// m = { k: m # post } — value POST prints after following comma;
	// with a single entry there's no following comma, so it trails the value.
	mo := &MapObject{
		Entries: map[string]Value{},
		KeyAnn:  map[string]string{},
		Keys:    []string{"k"},
	}
	m := Value{Tag: VTMap, Data: mo}
	val := m
	val.Annot = "<v-post"
	mo.Entries["k"] = val

	got := FormatValue(m)
	// Single entry: inline with value POST trailing.
	want := "{\n\tk: {...} # v-post\n}"
	eq(t, got, want)
}

func Test_Printer_Value_Cycle_NestedAnnots_PreservePreAndPost(t *testing.T) {
	// Array with PRE on container element and POST on value:
	// [  # pre
	//   {...} # post
	// ]
	mo := &MapObject{
		Entries: map[string]Value{},
		KeyAnn:  map[string]string{},
		Keys:    []string{"x"},
	}
	m := Value{Tag: VTMap, Data: mo}
	mo.Entries["x"] = Int(1)

	ao := &ArrayObject{}
	elem := m
	a := Value{Tag: VTArray, Data: ao}
	ao.Elems = []Value{withAnnot(elem, "head")} // PRE on element via Annot without '<'

	got := FormatValue(a)

	// Expect multi-line due to PRE; POST remains trailing on the value line.
	want := "[\n\t# head\n\t{x: 1}\n]"
	if strings.TrimSpace(got) != strings.TrimSpace(want) {
		t.Fatalf("nested PRE/POST mismatch:\n--- got ---\n%s\n--- want ---\n%s", got, want)
	}
}

func Test_Printer_Value_Cycle_ArrayToArray(t *testing.T) {
	// a = [b]; b = [a]  (2-cycle through arrays)
	aoA := &ArrayObject{}
	aoB := &ArrayObject{}
	a := Value{Tag: VTArray, Data: aoA}
	b := Value{Tag: VTArray, Data: aoB}

	aoA.Elems = []Value{b}
	aoB.Elems = []Value{a}

	got := FormatValue(a)
	// Outer a contains b, which contains a (elided) → [[[...]]]
	want := "[[[...]]]"
	eq(t, got, want)
}

func Test_Printer_Value_Cycle_MapValue_PostAfterCommaBetweenEntries(t *testing.T) {
	// m = { a: m # post, b: 1 } — value POST for 'a' prints after the comma
	mo := &MapObject{
		Entries: map[string]Value{},
		KeyAnn:  map[string]string{},
		Keys:    []string{"a", "b"},
	}
	m := Value{Tag: VTMap, Data: mo}
	val := m
	val.Annot = "<first" // value POST
	mo.Entries["a"] = val
	mo.Entries["b"] = Int(1)

	got := FormatValue(m)
	want := "{\n\ta: {...}, # first\n\tb: 1\n}"
	eq(t, got, want)
}

func Test_Printer_Value_SharedSubstructure_ElidesSecondOccurrence(t *testing.T) {
	// b = [1]; a = [b, b] — same identity appears twice; second occurrence elides.
	aoB := &ArrayObject{Elems: []Value{Int(1)}}
	b := Value{Tag: VTArray, Data: aoB}

	aoA := &ArrayObject{Elems: []Value{b, b}}
	a := Value{Tag: VTArray, Data: aoA}

	got := FormatValue(a)
	want := "[[1], [...]]"
	eq(t, got, want)
}
