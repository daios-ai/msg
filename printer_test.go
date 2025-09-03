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

func Test_Printer_Function_And_Oracle(t *testing.T) {
	in := `fun(a: Str) -> Str do return("hi " + a) end
oracle() -> Str from["web","docs"]`
	want := "fun(a: Str) -> Str do\n" +
		"\treturn(\"hi \" + a)\n" +
		"end\n" +
		"oracle() -> Str from [\"web\", \"docs\"]"
	got := pretty(t, in)
	if norm(got) != norm(want) {
		t.Fatalf("pretty fun/oracle mismatch\nwant:\n%s\n---\ngot:\n%s", want, got)
	}
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
		"for let x in xs do\n" +
		"\tbreak(0)\n" +
		"end"
	got := pretty(t, in)
	if norm(got) != norm(want) {
		t.Fatalf("pretty if/for mismatch\nwant:\n%s\n---\ngot:\n%s", want, got)
	}
}

func Test_Printer_Maps_Arrays_Strings(t *testing.T) {
	// Note: Go string literal contains an actual newline in "Jo\nhn".
	in := "{\n  \"name\":\"Jo\nhn\", age:25\n} [1,2,3] []"
	want := "{name: \"Jo\\nhn\", age: 25}\n[1, 2, 3]\n[]"
	got := pretty(t, in)
	if norm(got) != norm(want) {
		t.Fatalf("pretty map/array mismatch\nwant:\n%s\n---\ngot:\n%s", want, got)
	}
}

func Test_Printer_Control_Forms_Are_Adjacent(t *testing.T) {
	in := `return(1) break(0) continue(null)`
	want := `return(1)
break(0)
continue(null)`
	got := pretty(t, in)
	if norm(got) != norm(want) {
		t.Fatalf("pretty control mismatch\nwant:\n%s\n---\ngot:\n%s", want, got)
	}
}

func Test_Printer_Annotations(t *testing.T) {
	in := `# hello
x`
	got := pretty(t, in)
	if norm(got) != norm(in) {
		t.Fatalf("pretty annotation mismatch:\n%q", got)
	}
}

func Test_Printer_Types_Arrow_And_Optional(t *testing.T) {
	in := `type Int -> Str -> Bool
type Str? -> Int
type [Str]?
type {name: Str?, hobbies: [Str]?, age: Int}`
	want := `type Int -> Str -> Bool
type Str? -> Int
type [Str]?
type {name: Str?, hobbies: [Str]?, age: Int}`
	got := pretty(t, in)
	if norm(got) != norm(want) {
		t.Fatalf("pretty types mismatch\nwant:\n%s\n---\ngot:\n%s", want, got)
	}
}

func Test_Printer_Idempotent_And_Roundtrip(t *testing.T) {
	src := `
# demo
let Person = type {name: Str?, hobbies: [Str]?, age: Int}
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
	ast1 := parse(t, src)
	ast2 := parse(t, once)
	if !reflect.DeepEqual(ast1, ast2) {
		t.Fatalf("AST roundtrip mismatch after pretty")
	}
}

func Test_Printer_Let_Array_And_Object_Patterns(t *testing.T) {
	src := `
let [x, y] = [1,2]
let {name: n, age: a} = {name: "Bob", age: 40}
`
	out, err := Pretty(src)
	if err != nil {
		t.Fatalf("Pretty error: %v", err)
	}
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
		"\tname: \n" +
		"\t# username\n" +
		"\tname: n,\n" +
		"\tage: \n" +
		"\t# yearsOld\n" +
		"\tage: a\n" +
		"} = {name: \"Bob\", age: 40}"
	got := pretty(t, in)
	eq(t, got, want)
}

func Test_Printer_Keywords_As_Keys_In_Maps_And_Patterns(t *testing.T) {
	src := `
let {if: x, else: y, type: z} = {if: 1, else: 2, type: 3}
`
	out, err := Pretty(src)
	if err != nil {
		t.Fatalf("Pretty error: %v", err)
	}
	want := `
let {if: x, else: y, type: z} = {if: 1, else: 2, type: 3}
`
	eq(t, out, want)
}

func Test_Printer_Assign_NonDecl_Remains_Assignment(t *testing.T) {
	src := `
a = 1
(a).b = 2
`
	out, err := Pretty(src)
	if err != nil {
		t.Fatalf("Pretty error: %v", err)
	}
	want := `
a = 1
a.b = 2
`
	eq(t, out, want)
}

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

// NOTE: Removed Test_Printer_Annotations_InlineParens_NormalizesToHeader
// because inline annotations #( ... ) are no longer supported.

func Test_Printer_ReturnBreakContinue_SameLineVsNewline(t *testing.T) {
	// Same-line expression → carry the expression.
	casesSame := []struct{ in, want string }{
		{`return 1`, `return(1)`},
		{`break  x`, `break(x)`},
		{`continue "z"`, `continue("z")`},
	}
	for _, tc := range casesSame {
		got := pretty(t, tc.in)
		eq(t, got, tc.want)
	}

	// Next token on the next line → implicit null.
	in := `return
x`
	want := `return(null)
x`
	got := pretty(t, in)
	eq(t, got, want)
}

func Test_Printer_ComputedDot_NormalizesToIndexing(t *testing.T) {
	in := `a.(1 + 2)`
	want := `a[1 + 2]`
	got := pretty(t, in)
	eq(t, got, want)
}

func Test_Printer_Maps_And_Arrays_And_Quoting(t *testing.T) {
	in := `{ok: 1, "weird key": 2, also_ok: [1,2,3]}`
	want := `{ok: 1, "weird key": 2, also_ok: [1, 2, 3]}`
	got := pretty(t, in)
	eq(t, got, want)
}

func Test_Printer_Destructuring_Assign(t *testing.T) {
	in := `let {"x": a, y: b} = m`
	// Our printer emits identifier-like keys unquoted → x is unquoted.
	want := `let {x: a, y: b} = m`
	got := pretty(t, in)
	eq(t, got, want)
}

func Test_Printer_Idempotent(t *testing.T) {
	src := `fun(a: Str) -> Str do return("hi " + a) end`
	once := pretty(t, src)
	twice := pretty(t, once)
	eq(t, twice, once)
}

func Test_Printer_If_Elif_Else_Blocking(t *testing.T) {
	in := `if a then x elif b then y else z end`
	want := "if a then\n" +
		"\tx\n" +
		"elif b then\n" +
		"\ty\n" +
		"else\n" +
		"\tz\n" +
		"end"
	got := pretty(t, in)
	eq(t, got, want)
}

func Test_Printer_While_And_For(t *testing.T) {
	in := `while a < b do a = a + 1 end`
	want := "while a < b do\n" +
		"\ta = a + 1\n" +
		"end"
	got := pretty(t, in)
	eq(t, got, want)

	in2 := `for [k,v] in m do return k end`
	want2 := "for [k, v] in m do\n" +
		"\treturn(k)\n" +
		"end"
	got2 := pretty(t, in2)
	eq(t, got2, want2)
}

func Test_Printer_Grouping_With_Postfix_Question(t *testing.T) {
	in := `(a + b)?`
	want := `(a + b)?`
	got := pretty(t, in)
	eq(t, got, want)
}

func Test_FormatSExpr_Direct_AST(t *testing.T) {
	// Build ("call", ("id","f"), ("int",1), ("int",2))
	ast := S{"call", S{"id", "f"}, S{"int", int64(1)}, S{"int", int64(2)}}
	got := FormatSExpr(ast)
	want := `f(1, 2)`
	eq(t, got, want)
}

func Test_FormatType_Basics(t *testing.T) {
	// T? where T = [Int] -> Str
	T := S{"binop", "->", S{"array", S{"id", "Int"}}, S{"id", "Str"}}
	opt := S{"unop", "?", T}
	got := FormatType(opt)
	// Prints (A) -> B for arrows; then '?'
	want := `([Int]) -> Str?`
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

func Test_Printer_Property_Name_Quoting(t *testing.T) {
	in := `obj."not ident"`
	want := `obj."not ident"`
	got := pretty(t, in)
	eq(t, got, want)

	in2 := `obj."then"`
	// After standardization, keyword-looking property prints bare.
	want2 := `obj.then`
	got2 := pretty(t, in2)
	eq(t, got2, want2)
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

func Test_Printer_Uses_Tabs(t *testing.T) {
	got := pretty(t, `do x end`)
	if strings.Contains(got, "  ") { // two spaces
		t.Fatalf("expected tabs, found spaces:\n%s", got)
	}
}

func Test_Printer_Property_Name_Normalization(t *testing.T) {
	// Keywords after '.' normalize to bare identifiers.
	eq(t, pretty(t, `obj."then"`), `obj.then`)
	// Non-identifier names remain quoted.
	eq(t, pretty(t, `obj."not ident"`), `obj."not ident"`)
}

func Test_Printer_Standardize_Trailing_Newline_And_Idempotence(t *testing.T) {
	src := `fun(a: Str) -> Str do return("hi " + a) end`
	std1, err := Standardize(src)
	if err != nil {
		t.Fatalf("Standardize error: %v", err)
	}
	if !strings.HasSuffix(std1, "\n") {
		t.Fatalf("missing trailing newline:\n%q", std1)
	}
	std2, err := Standardize(std1)
	if err != nil {
		t.Fatalf("Standardize error (second pass): %v", err)
	}
	if std2 != std1 {
		t.Fatalf("not idempotent:\n---1---\n%q\n---2---\n%q", std1, std2)
	}
}

func Test_Printer_TabsOnly_Indentation(t *testing.T) {
	got := pretty(t, `if a then x else y end`)
	// Ensure no leading spaces are used for indentation on any line.
	for i, ln := range strings.Split(got, "\n") {
		// Trim trailing empty last line if any
		if ln == "" && i == len(strings.Split(got, "\n"))-1 {
			continue
		}
		for j := 0; j < len(ln) && (ln[j] == ' ' || ln[j] == '\t'); j++ {
			if ln[j] == ' ' {
				t.Fatalf("found space in indentation at line %d: %q", i+1, ln)
			}
		}
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
func Test_Printer_Module_Basic(t *testing.T) {
	in := `module MyLib do x = 1 end`
	want := "module MyLib do\n" +
		"\tx = 1\n" +
		"end"
	got := pretty(t, in)
	eq(t, got, want)
}

func Test_Printer_Module_StringName_EmptyBody(t *testing.T) {
	in := `module "my mod" do end`
	want := "module \"my mod\" do\n" +
		"end"
	got := pretty(t, in)
	eq(t, got, want)
}

func Test_Printer_Module_NameExpression(t *testing.T) {
	// Name can be any expression; ensure precedence/parentheses are handled.
	in := `module "a" + "b" do end`
	// (+) has lower precedence than a primary, so printer should parenthesize.
	want := "module \"a\" + \"b\" do\n" + // name position uses normal expr printing
		"end"
	got := pretty(t, in)
	// Note: the header prints the name expression with minimal parens;
	// for simple "+", no extra parens are introduced because it sits
	// directly after "module ".
	eq(t, got, want)
}

func Test_Printer_Module_With_PreAnnotation(t *testing.T) {
	in := `# about this module
module M do
end`
	want := "# about this module\n" +
		"module M do\n" +
		"end"
	got := pretty(t, in)
	eq(t, got, want)
}

func Test_Printer_Module_Amid_Other_Statements(t *testing.T) {
	in := `let x = 1
module M do
  x = 2
end
return x`
	want := "let x = 1\n" +
		"module M do\n" +
		"\tx = 2\n" +
		"end\n" +
		"return(x)"
	got := pretty(t, in)
	eq(t, got, want)
}
func Test_Printer_Fun_Rendering(t *testing.T) {
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

func Test_Printer_Oracle_Rendering(t *testing.T) {
	f := &Fun{
		Params:     nil,
		ParamTypes: nil,
		ReturnType: S{"id", "Str"},
		IsOracle:   true,
	}
	got := FormatValue(FunVal(f))
	want := "<oracle: Str>"
	if got != want {
		t.Fatalf("oracle rendering mismatch:\n got:  %q\n want: %q", got, want)
	}
}

func Test_Printer_Type_Rendering(t *testing.T) {
	// { age: Int, name: Str } — order is sorted by printer
	typ := S{
		"map",
		S{"pair", S{"str", "name"}, S{"id", "Str"}},
		S{"pair", S{"str", "age"}, S{"id", "Int"}},
	}
	got := FormatValue(TypeVal(typ))
	want := "<type: { age: Int, name: Str }>"
	if got != want {
		t.Fatalf("type rendering mismatch:\n got:  %q\n want: %q", got, want)
	}
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

func Test_Printer_Value_Type_EmptyObject_NoSpace(t *testing.T) {
	got := FormatValue(TypeVal(S{"map"}))
	want := "<type: {}>"
	if norm(got) != norm(want) {
		t.Fatalf("empty object type spacing mismatch\nwant: %q\ngot:  %q", want, got)
	}
}
