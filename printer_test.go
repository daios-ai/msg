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
	want := `fun(a: Str) -> Str do
  return("hi " + a)
end
oracle() -> Str from ["web", "docs"]`
	got := pretty(t, in)
	if norm(got) != norm(want) {
		t.Fatalf("pretty fun/oracle mismatch\nwant:\n%s\n---\ngot:\n%s", want, got)
	}
}

func Test_Printer_If_Elif_Else_And_For(t *testing.T) {
	in := `
if a then
  x
elif b then
  y
else
  z
end

for let x in xs do
  break(0)
end`
	want := `if a then
  x
elif b then
  y
else
  z
end
for let x in xs do
  break(0)
end`
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
	src := `
let {
  name:
    # username
    n,
  age:
    # yearsOld
    a
} = {name: "Bob", age: 40}
`
	out, err := Pretty(src)
	if err != nil {
		t.Fatalf("Pretty error: %v", err)
	}
	// Expect multiline object pattern with comment lines preserved
	want := `
let {
  name: 
  # username
  n,
  age: 
  # yearsOld
  a
} = {name: "Bob", age: 40}
`
	// The pretty-printer prints "# ..." without extra indentation before '#'
	// and aligns subsequent pattern lines with current padding.
	// Normalize consecutive spaces around '#' to be tolerant of small spacing differences.
	eq(t, out, want)
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

