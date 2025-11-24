// analysis_test.go
package main

import (
	"strings"
	"testing"

	mindscript "github.com/DAIOS-AI/msg/internal/mindscript"
)

// -----------------------------------------------------------------------------
// Harness
// -----------------------------------------------------------------------------

// runIndex runs the pure analyzer over a single in-memory file.
func runIndex(t *testing.T, uri, src string) *FileIndex {
	t.Helper()
	a := &Analyzer{} // For now, no ambient IP; ambient tests can inject if needed.
	res := a.Analyze(uri, src)
	if res == nil {
		t.Fatalf("Analyze returned nil")
	}
	return res
}

// runIndexWithIP runs analyze with a specific interpreter (for ambient tests).
func runIndexWithIP(t *testing.T, uri, src string, ip *mindscript.Interpreter) *FileIndex {
	t.Helper()
	a := &Analyzer{IP: ip}
	res := a.Analyze(uri, src)
	if res == nil {
		t.Fatalf("Analyze returned nil")
	}
	return res
}

func hasDiag(idx *FileIndex, code string) bool {
	for _, d := range idx.Diags {
		if d.Code == code {
			return true
		}
	}
	return false
}

func getDiag(idx *FileIndex, code string) (Diag, bool) {
	for _, d := range idx.Diags {
		if d.Code == code {
			return d, true
		}
	}
	return Diag{}, false
}

func mustHaveDiag(t *testing.T, idx *FileIndex, code string) Diag {
	t.Helper()
	if d, ok := getDiag(idx, code); ok {
		return d
	}
	var got []string
	for _, d := range idx.Diags {
		got = append(got, d.Code)
	}
	t.Fatalf("expected diag %q not found; got=%v", code, got)
	return Diag{}
}

func mustHaveDiagOneOf(t *testing.T, idx *FileIndex, codes ...string) {
	t.Helper()
	got := map[string]bool{}
	for _, d := range idx.Diags {
		got[d.Code] = true
	}
	for _, c := range codes {
		if got[c] {
			return
		}
	}
	var want []string
	want = append(want, codes...)
	t.Fatalf("expected one of %v, got codes=%v", want, got)
}

func mustNotHaveDiag(t *testing.T, idx *FileIndex, code string) {
	t.Helper()
	if hasDiag(idx, code) {
		var got []string
		for _, d := range idx.Diags {
			got = append(got, d.Code)
		}
		t.Fatalf("unexpected diag %q present; got=%v", code, got)
	}
}

// findBinding fetches a binding by name from RootEnv.
// It returns the stored Value and its analysis Symbol (when Tag==VTSymbol).
// Ambient/runtime values (non-VTSymbol) return (zero, nil).
func findBinding(idx *FileIndex, name string) (mindscript.Value, *Symbol) {
	if idx == nil || idx.RootEnv == nil {
		return mindscript.Value{}, nil
	}

	v, err := idx.RootEnv.Get(name)
	if err != nil {
		return mindscript.Value{}, nil
	}

	if v.Tag == mindscript.VTSymbol {
		if s, ok := v.Data.(*Symbol); ok && s != nil {
			return v, s
		}
	}

	// Non-VTSymbol binding → treat as ambient/builtin, not an analysis symbol.
	return mindscript.Value{}, nil
}

func testAnalyze(src string) *FileIndex {
	var a Analyzer
	return a.Analyze("file:///test.ms", src)
}

func testFmtType(ip *mindscript.Interpreter, env *mindscript.Env, v mindscript.Value) string {
	if ip == nil {
		return ""
	}
	return mindscript.FormatType(ip.ResolveType(typeOf(ip, env, v), env))
}

func testTokensWithText(ti TokenIndex, s string) []TokenEntry {
	var out []TokenEntry
	for _, e := range ti.Entries {
		if e.Text == s {
			out = append(out, e)
		}
	}
	return out
}

func testFindTokenAtOffset(ti TokenIndex, off int) *TokenEntry {
	return ti.Find(off)
}

func testByteOffset(src, needle string, nth int) int {
	start := 0
	for i := 0; i < nth; i++ {
		pos := strings.Index(src[start:], needle)
		if pos < 0 {
			return -1
		}
		start += pos
		if i < nth-1 {
			start += len(needle)
		}
	}
	return start
}

// -----------------------------------------------------------------------------
// 1) Layout-sensitive token lints
// -----------------------------------------------------------------------------

// TODO: implement

// -----------------------------------------------------------------------------
// 2) Names & assignment targets
// -----------------------------------------------------------------------------

func Test_Analysis_Unknown_Name(t *testing.T) {
	const uri = "mem://names.ms"
	const src = "y = x\n" // x never declared

	idx := runIndex(t, uri, src)
	if !hasDiag(idx, "MS-UNKNOWN-NAME") {
		var got []string
		for _, d := range idx.Diags {
			got = append(got, d.Code)
		}
		t.Fatalf("expected MS-UNKNOWN-NAME, got=%v", got)
	}
}

func Test_Analysis_Invalid_Assign_Target(t *testing.T) {
	const uri = "mem://invalid-assign.ms"
	const src = "(1 + 2) = 3\n"
	idx := runIndex(t, uri, src)

	got := map[string]bool{}
	for _, d := range idx.Diags {
		got[d.Code] = true
	}
	if !(got["MS-INVALID-ASSIGN-TARGET"] || got["MS-PARSE"]) {
		t.Fatalf("expected MS-INVALID-ASSIGN-TARGET or MS-PARSE, got=%v", got)
	}
}

// -----------------------------------------------------------------------------
// 3) Arrays & operators
// -----------------------------------------------------------------------------

func Test_Analysis_Mod_By_Zero_Const(t *testing.T) {
	const uri = "mem://ops-mod.ms"
	src := `3 % 0`
	idx := runIndex(t, uri, src)
	mustHaveDiag(t, idx, "MS-DIV-BY-ZERO-CONST")
}

func Test_Analysis_Bitwise_On_NonInt(t *testing.T) {
	const uri = "mem://ops-bit.ms"
	src := `1.0 & 3`
	idx := runIndex(t, uri, src)
	mustHaveDiag(t, idx, "MS-BITWISE-NONINT")
}

// -----------------------------------------------------------------------------
// 4) Bindings, functions, and oracle types
// -----------------------------------------------------------------------------

func Test_Analysis_Fun_Binding_Arrow_Type(t *testing.T) {
	const uri = "mem://binds-fun.ms"
	src := `
let add = fun(a: Int, b: Int) -> Int do a + b end
`
	idx := runIndex(t, uri, src)

	val, s := findBinding(idx, "add")
	if s == nil {
		t.Fatalf("binding for fun 'add' not found")
	}
	ty := mindscript.FormatType(s.Type)
	if !strings.Contains(ty, "Int") || !strings.Contains(ty, "->") {
		t.Fatalf("expected arrow type ending in Int, got %q", ty)
	}
	_ = val
}

func Test_Analysis_Oracle_Return_Exposed_As_Nullable(t *testing.T) {
	const uri = "mem://binds-oracle.ms"
	src := `
let next = oracle(seed: Int) -> Int
`
	idx := runIndex(t, uri, src)

	val, s := findBinding(idx, "next")
	if s == nil {
		t.Fatalf("binding for oracle 'next' not found")
	}
	ty := mindscript.FormatType(s.Type)
	if !strings.Contains(ty, "Int?") {
		t.Fatalf("expected oracle return to be nullable Int?, got %q", ty)
	}
	_ = val
}

// -----------------------------------------------------------------------------
// 5) AST & spans sanity
// -----------------------------------------------------------------------------

func Test_Analysis_AST_And_Spans_Sanity(t *testing.T) {
	const uri = "mem://spans.ms"
	src := `
let p = { name: "Ada", age: 36 }
p.name
`
	idx := runIndex(t, uri, src)

	if idx.AST == nil || len(idx.AST) == 0 || idx.Spans == nil {
		t.Fatalf("expected AST and spans to be present")
	}
	// Further SpanIndex assertions can be added once APIs are finalized.
}

// -----------------------------------------------------------------------------
// 6) Calls, currying & arity
// -----------------------------------------------------------------------------

func Test_Analysis_Call_Arg_Overflow(t *testing.T) {
	const uri = "mem://arity-overflow.ms"
	src := `
let f = fun(a: Int, b: Int) -> Int do a + b end
f(1,2,3)
`
	idx := runIndex(t, uri, src)
	mustHaveDiag(t, idx, "MS-ARG-OVERFLOW")
}

func Test_Analysis_Call_Arg_Type_Mismatch(t *testing.T) {
	const uri = "mem://arity-mismatch.ms"
	src := `
let f = fun(a: Int, b: Int) -> Int do a + b end
f("a", 2)
`
	idx := runIndex(t, uri, src)
	mustHaveDiag(t, idx, "MS-ARG-TYPE-MISMATCH")
}

func Test_Analysis_Currying_Residual_Arrow(t *testing.T) {
	const uri = "mem://curry.ms"
	src := `
let add = fun(a: Int, b: Int) -> Int do a + b end
let g = add(1)
`
	idx := runIndex(t, uri, src)

	mustNotHaveDiag(t, idx, "MS-ARG-OVERFLOW")
	mustNotHaveDiag(t, idx, "MS-ARG-TYPE-MISMATCH")

	_, s := findBinding(idx, "g")
	if s == nil {
		t.Fatalf("binding g not found")
	}
	ft := mindscript.FormatType(s.Type)
	if !strings.Contains(ft, "-> Int") {
		t.Fatalf("want residual arrow ending in Int, got %q", ft)
	}
}

// -----------------------------------------------------------------------------
// 7) Return type checks
// -----------------------------------------------------------------------------

func Test_Analysis_Return_Type_Mismatch_Bare(t *testing.T) {
	const uri = "mem://ret-bare.ms"
	src := `
let k = fun() -> Int do
  return
end
`
	idx := runIndex(t, uri, src)
	mustHaveDiag(t, idx, "MS-RET-TYPE-MISMATCH")
}

func Test_Analysis_Return_Type_Mismatch_Value(t *testing.T) {
	const uri = "mem://ret-val.ms"
	src := `
let k = fun() -> Int do
  return "x"
end
`
	idx := runIndex(t, uri, src)
	mustHaveDiag(t, idx, "MS-RET-TYPE-MISMATCH")
}

// -----------------------------------------------------------------------------
// 8) Indexing & properties
// -----------------------------------------------------------------------------

func Test_Analysis_Index_Must_Be_Int(t *testing.T) {
	const uri = "mem://idx-int.ms"
	src := `
let xs = [1,2,3]
xs["0"]
`
	idx := runIndex(t, uri, src)
	mustHaveDiag(t, idx, "MS-ARG-TYPE-MISMATCH")
}

func Test_Analysis_Map_Missing_Key_Warning_On_Value_Map(t *testing.T) {
	const uri = "mem://map-missing.ms"
	src := `
let p = { name: "Ada" }
p.age
`
	idx := runIndex(t, uri, src)
	mustHaveDiag(t, idx, "MS-MAP-MISSING-KEY")
}

func Test_Analysis_ValueMap_Known_Key_No_Warning(t *testing.T) {
	const uri = "mem://valmap-present.ms"
	src := `
let p = { name: "Ada", age: 36 }
p.name
`
	idx := runIndex(t, uri, src)
	mustNotHaveDiag(t, idx, "MS-MAP-MISSING-KEY")
}

// -----------------------------------------------------------------------------
// 9) Block result typing (last expression)
// -----------------------------------------------------------------------------

func Test_Analysis_Block_Last_Expression_Binding_Type(t *testing.T) {
	const uri = "mem://block-last.ms"
	src := `
let x = do
  1
  "s"
end
`
	idx := runIndex(t, uri, src)

	_, s := findBinding(idx, "x")
	if s == nil {
		t.Fatalf("binding x not found")
	}
	if got := mindscript.FormatType(s.Type); got != "Enum[\"s\"]" {
		t.Fatalf("want Enum[\"s\"], got %q", got)
	}
}

// -----------------------------------------------------------------------------
// 10) Division typing
// -----------------------------------------------------------------------------

func Test_Analysis_Division_Typing(t *testing.T) {
	const uri = "mem://div-typing.ms"

	t.Run("int_div_int_is_int_no_error", func(t *testing.T) {
		src := `1 / 2`
		idx := runIndex(t, uri, src)
		if len(idx.Diags) != 0 {
			t.Fatalf("unexpected diagnostics: %+v", idx.Diags)
		}
	})

	t.Run("int_div_num_is_num_no_error", func(ttesting *testing.T) {
		src := `1 / 2.0`
		idx := runIndex(ttesting, uri, src)
		if len(idx.Diags) != 0 {
			ttesting.Fatalf("unexpected diagnostics: %+v", idx.Diags)
		}
	})
}

// -----------------------------------------------------------------------------
// 11) Array + if-expression LUB sanity
// -----------------------------------------------------------------------------

func Test_Analysis_Array_LUB_Empty_Is_Any(t *testing.T) {
	const uri = "mem://arr-empty.ms"
	src := `let xs = []`
	idx := runIndex(t, uri, src)

	_, s := findBinding(idx, "xs")
	if s == nil {
		t.Fatalf("binding xs not found")
	}
	if got := mindscript.FormatType(s.Type); got != "[Any]" {
		t.Fatalf("want [Any], got %q", got)
	}
}

func Test_Analysis_Array_LUB_Homogeneous_Int(t *testing.T) {
	const uri = "mem://arr-int.ms"
	src := `let xs = [1, 2, 3]`
	idx := runIndex(t, uri, src)

	_, s := findBinding(idx, "xs")
	if s == nil {
		t.Fatalf("binding xs not found")
	}
	if got := mindscript.FormatType(s.Type); got != "[Enum[1, 2, 3]]" {
		t.Fatalf("want [Enum[1, 2, 3]], got %q", got)
	}
}

func Test_Analysis_If_LUB_Int_Vs_Num_Becomes_Num(t *testing.T) {
	const uri = "mem://if-lub.ms"
	src := `
let y =
  if true then 1 else 2.0 end
`
	idx := runIndex(t, uri, src)

	_, s := findBinding(idx, "y")
	if s == nil {
		t.Fatalf("binding y not found")
	}
	if got := mindscript.FormatType(s.Type); got != "Enum[1, 2]" {
		t.Fatalf("want Enum[1, 2], got %q", got)
	}
}

// -----------------------------------------------------------------------------
// 12) Expression typing — tiny, focused tests
// -----------------------------------------------------------------------------

func Test_Analysis_Literals_Primitives(t *testing.T) {
	const uri = "mem://lit-prim.ms"

	tests := []struct {
		name string
		src  string
		want string
	}{
		{"int", `let x = 1`, "Enum[1]"},
		{"num", `let x = 1.5`, "Enum[1.5]"},
		{"str", `let x = "hi"`, "Enum[\"hi\"]"},
		{"bool", `let x = true`, "Enum[true]"},
		{"null", `let x = null`, "Enum[null]"},
	}

	for _, tc := range tests {
		tc := tc
		t.Run(tc.name, func(t *testing.T) {
			idx := runIndex(t, uri, tc.src)
			_, s := findBinding(idx, "x")
			if s == nil {
				t.Fatalf("binding x not found")
			}
			if got := mindscript.FormatType(s.Type); got != tc.want {
				t.Fatalf("want %s, got %q", tc.want, got)
			}
			if len(idx.Diags) != 0 {
				t.Fatalf("unexpected diags: %+v", idx.Diags)
			}
		})
	}
}

func Test_Analysis_Literals_Unknown_Name(t *testing.T) {
	const uri = "mem://lit-unknown.ms"
	src := `let x = y`
	idx := runIndex(t, uri, src)

	mustHaveDiag(t, idx, "MS-UNKNOWN-NAME")

	_, s := findBinding(idx, "x")
	if s == nil {
		t.Fatalf("binding x not found")
	}
	// Unknown RHS widens to Any.
	if got := mindscript.FormatType(s.Type); got != "Any" {
		t.Fatalf("want Any for x, got %q", got)
	}
}

func Test_Analysis_Array_LUB_Mixed_NumNullable(t *testing.T) {
	const uri = "mem://arr-mixed.ms"
	src := `let xs = [1, 2.0, null]`
	idx := runIndex(t, uri, src)

	_, s := findBinding(idx, "xs")
	if s == nil {
		t.Fatalf("binding xs not found")
	}
	if got := mindscript.FormatType(s.Type); got != "[Enum[1, 2, null]]" {
		t.Fatalf("want [Enum[1, 2, null]], got %q", got)
	}
}

func Test_Analysis_Maps_Value_Shape(t *testing.T) {
	const uri = "mem://map-shape.ms"
	src := `let m = {a: 1, b: "x"}`
	idx := runIndex(t, uri, src)

	_, s := findBinding(idx, "m")
	if s == nil {
		t.Fatalf("binding m not found")
	}
	want := `{a!: Enum[1], b!: Enum["x"]}`
	if got := mindscript.FormatType(s.Type); got != want {
		t.Fatalf("want %s, got %q", want, got)
	}
}

func Test_Analysis_Property_Get_Known_And_Unknown(t *testing.T) {
	const uri = "mem://get-known-unknown.ms"
	src := `
let m = {a: 1}
let x = m.a
let y = m.b
`
	idx := runIndex(t, uri, src)

	mustHaveDiag(t, idx, "MS-MAP-MISSING-KEY") // for m.b

	_, x := findBinding(idx, "x")
	if x == nil {
		t.Fatalf("binding x not found")
	}
	if got := mindscript.FormatType(x.Type); got != "Enum[1]" {
		t.Fatalf("want Enum[1] for x, got %q", got)
	}

	_, y := findBinding(idx, "y")
	if y == nil {
		t.Fatalf("binding y not found")
	}
	if got := mindscript.FormatType(y.Type); got != "Any" {
		t.Fatalf("want Any for y, got %q", got)
	}
}

func Test_Analysis_Index_Array_ElementType(t *testing.T) {
	const uri = "mem://idx-array.ms"
	src := `
let xs = [1]
let x = xs[0]
`
	idx := runIndex(t, uri, src)

	mustNotHaveDiag(t, idx, "MS-ARG-TYPE-MISMATCH")

	_, s := findBinding(idx, "x")
	if s == nil {
		t.Fatalf("binding x not found")
	}
	if got := mindscript.FormatType(s.Type); got != "Enum[1]" {
		t.Fatalf("want Enum[1], got %q", got)
	}
}

// -----------------------------------------------------------------------------
// 13) If-expressions & loops — LUB and nullability
// -----------------------------------------------------------------------------

func Test_Analysis_If_LUB_Branches(t *testing.T) {
	const uri = "mem://if-basic.ms"

	t.Run("int_int", func(t *testing.T) {
		src := `let x = if true then 1 else 2 end`
		idx := runIndex(t, uri, src)
		_, s := findBinding(idx, "x")
		if s == nil {
			t.Fatalf("binding x not found")
		}
		if got := mindscript.FormatType(s.Type); got != "Enum[1, 2]" {
			t.Fatalf("want Enum[1, 2], got %q", got)
		}
	})

	t.Run("int_str_any", func(t *testing.T) {
		src := `let x = if true then 1 else "no" end`
		idx := runIndex(t, uri, src)
		_, s := findBinding(idx, "x")
		if s == nil {
			t.Fatalf("binding x not found")
		}
		if got := mindscript.FormatType(s.Type); got != "Enum[1, \"no\"]" {
			t.Fatalf("want Enum[1, \"no\"], got %q", got)
		}
	})

	t.Run("missing_else_makes_nullable", func(t *testing.T) {
		src := `let x = if true then 1 end`
		idx := runIndex(t, uri, src)
		_, s := findBinding(idx, "x")
		if s == nil {
			t.Fatalf("binding x not found")
		}
		if got := mindscript.FormatType(s.Type); got != "Enum[1]?" {
			t.Fatalf("want Enum[1]?, got %q", got)
		}
	})

	t.Run("else_null_makes_nullable", func(t *testing.T) {
		src := `let x = if true then 1 else null end`
		idx := runIndex(t, uri, src)
		_, s := findBinding(idx, "x")
		if s == nil {
			t.Fatalf("binding x not found")
		}
		if got := mindscript.FormatType(s.Type); got != "Enum[1, null]" {
			t.Fatalf("want Enum[1, null], got %q", got)
		}
	})
}

func Test_Analysis_Loop_Result_Nullable(t *testing.T) {
	const uri = "mem://loop-nullable.ms"
	src := `
let x = while false do
  1
end
`
	idx := runIndex(t, uri, src)

	_, s := findBinding(idx, "x")
	if s == nil {
		t.Fatalf("binding x not found")
	}
	// Spec: loop may not run → body ⊔ Null ⇒ Int?
	if got := mindscript.FormatType(s.Type); got != "Enum[1]?" {
		t.Fatalf("want Enum[1]?, got %q", got)
	}
}

// -----------------------------------------------------------------------------
// 14) Functions & oracles — arrow shapes & nullability
// -----------------------------------------------------------------------------

func Test_Analysis_Fun_And_Oracle_Arrow_Shapes(t *testing.T) {
	const uri = "mem://fun-oracle-arrows.ms"

	t.Run("plain_fun_arrow", func(t *testing.T) {
		src := `let f = fun(x: Int) -> Str do "ok" end`
		idx := runIndex(t, uri, src)
		_, s := findBinding(idx, "f")
		if s == nil {
			t.Fatalf("binding f not found")
		}
		if got := mindscript.FormatType(s.Type); got != "Int -> Str" {
			t.Fatalf("want Int -> Str, got %q", got)
		}
	})

	t.Run("oracle_return_made_nullable", func(t *testing.T) {
		src := `let f = oracle(x: Int) -> Str`
		idx := runIndex(t, uri, src)
		_, s := findBinding(idx, "f")
		if s == nil {
			t.Fatalf("binding f not found")
		}
		// Spec: Int -> Str?
		if got := mindscript.FormatType(s.Type); got != "Int -> Str?" {
			t.Fatalf("want Int -> Str?, got %q", got)
		}
	})

	t.Run("oracle_already_nullable_stable", func(t *testing.T) {
		src := `let f = oracle(x: Int) -> Str?`
		idx := runIndex(t, uri, src)
		_, s := findBinding(idx, "f")
		if s == nil {
			t.Fatalf("binding f not found")
		}
		// Spec: still Int -> Str? (no ??)
		if got := mindscript.FormatType(s.Type); got != "Int -> Str?" {
			t.Fatalf("want Int -> Str?, got %q", got)
		}
	})

	t.Run("zero_arg_fun_is_null_param", func(t *testing.T) {
		src := `let f = fun() -> Str do "hi" end`
		idx := runIndex(t, uri, src)
		_, s := findBinding(idx, "f")
		if s == nil {
			t.Fatalf("binding f not found")
		}
		// Spec: Null -> Str (0-arg sugar)
		if got := mindscript.FormatType(s.Type); got != "Null -> Str" {
			t.Fatalf("want Null -> Str, got %q", got)
		}
	})

	t.Run("untyped_fun_any_to_any", func(t *testing.T) {
		src := `let f = fun(x) do x end`
		idx := runIndex(t, uri, src)
		_, s := findBinding(idx, "f")
		if s == nil {
			t.Fatalf("binding f not found")
		}
		if got := mindscript.FormatType(s.Type); got != "Any -> Any" {
			t.Fatalf("want Any -> Any, got %q", got)
		}
	})
}

func Test_Analysis_Return_Type_Mismatch_Enum(t *testing.T) {
	const uri = "mem://ret-enum.ms"
	src := `let f = fun() -> Enum[1, 2] do 3 end`
	idx := runIndex(t, uri, src)

	mustHaveDiag(t, idx, "MS-RET-TYPE-MISMATCH")
}

// -----------------------------------------------------------------------------
// 15) Calls & currying — extra cases
// -----------------------------------------------------------------------------

func Test_Analysis_Call_Enum_Arg_Mismatch(t *testing.T) {
	const uri = "mem://call-enum.ms"
	src := `
let f = fun(x: Enum["a", "b"]) -> Str do x end
f("d")
`
	idx := runIndex(t, uri, src)
	// Prefer enum-specific, but accept generic mismatch until implemented.
	mustHaveDiagOneOf(t, idx, "MS-ENUM-VALUE-NOT-MEMBER", "MS-ARG-TYPE-MISMATCH")
}

func Test_Analysis_Currying_Residual_Arrow_Shape(t *testing.T) {
	const uri = "mem://curry-residual.ms"
	src := `
let f = fun(a: Int, b: Str) -> Bool do true end
let g = f(1)
`
	idx := runIndex(t, uri, src)

	_, s := findBinding(idx, "g")
	if s == nil {
		t.Fatalf("binding g not found")
	}
	// Expect residual Str -> Bool.
	if got := mindscript.FormatType(s.Type); got != "Str -> Bool" {
		t.Fatalf("want Str -> Bool, got %q", got)
	}
}

// -----------------------------------------------------------------------------
// 16) Typed maps & alias-ish behavior
// -----------------------------------------------------------------------------

func Test_Analysis_Type_Alias_And_Map_Use(t *testing.T) {
	const uri = "mem://type-alias.ms"
	src := `
let Point = type {x!: Int, y!: Int}
let f = fun(p: Point) -> Point do p end
let p = {x: 1, y: 2}
let q = f(p)
`
	idx := runIndex(t, uri, src)

	// We do NOT require p to be inferred as type Point.
	// We only care that using Point as a schema in the function
	// signature does not produce spurious type errors.
	mustNotHaveDiag(t, idx, "MS-ARG-TYPE-MISMATCH")
	mustNotHaveDiag(t, idx, "MS-RET-TYPE-MISMATCH")

	// q should have some sensible, non-empty type (map or alias),
	// but we don't pin the exact pretty-printed shape here.
	_, q := findBinding(idx, "q")
	if q == nil {
		t.Fatalf("binding q not found")
	}
	if got := mindscript.FormatType(q.Type); got == "" {
		t.Fatalf("expected some type for q, got empty")
	}
}

// -----------------------------------------------------------------------------
// 17) Ambient functions (requires real interpreter)
// -----------------------------------------------------------------------------

func Test_Analysis_Ambient_Len_Success_And_Mismatch(t *testing.T) {
	ip, err := mindscript.NewInterpreter()
	if err != nil {
		t.Fatalf("NewInterpreter: %v", err)
	}

	t.Run("len_on_array_ok", func(t *testing.T) {
		const uri = "mem://ambient-len-ok.ms"
		src := `
let xs = [1,2,3]
let n = len(xs)
`
		idx := runIndexWithIP(t, uri, src, ip)

		// Call is well-typed: len takes Any, so array is fine.
		mustNotHaveDiag(t, idx, "MS-ARG-TYPE-MISMATCH")

		_, n := findBinding(idx, "n")
		if n == nil {
			t.Fatalf("binding n not found")
		}
		// len returns Int? (null signals failure), so statically Int?.
		if got := mindscript.FormatType(n.Type); got != "Int?" {
			t.Fatalf("want Int?, got %q", got)
		}
	})

	t.Run("len_on_non_array_still_typed", func(t *testing.T) {
		const uri = "mem://ambient-len-bad.ms"
		src := `
let n = len(1)
`
		idx := runIndexWithIP(t, uri, src, ip)

		// len accepts Any, so no static mismatch.
		mustNotHaveDiag(t, idx, "MS-ARG-TYPE-MISMATCH")

		_, n := findBinding(idx, "n")
		if n == nil {
			t.Fatalf("binding n not found")
		}
		// Statically still Int? (runtime may yield null).
		if got := mindscript.FormatType(n.Type); got != "Int?" {
			t.Fatalf("want Int?, got %q", got)
		}
	})
}

// -----------------------------------------------------------------------------
// 18) Modules
// -----------------------------------------------------------------------------

func Test_Analysis_Modules_Export_And_Field(t *testing.T) {
	const uri = "mem://modules-export.ms"
	src := `
module "M" do
  let x = 1
end

let y = M.x
`
	idx := runIndex(t, uri, src)

	_, s := findBinding(idx, "y")
	if s == nil {
		t.Fatalf("binding y not found")
	}
	if got := mindscript.FormatType(s.Type); got != "Int" {
		t.Fatalf("want y: Int, got %q", got)
	}
	mustNotHaveDiag(t, idx, "MS-MAP-MISSING-KEY")
}

func Test_Analysis_Modules_Missing_Field_Diag(t *testing.T) {
	const uri = "mem://modules-missing-field.ms"
	src := `
module "M" do
  let x = 1
end

let y = M.y
`
	idx := runIndex(t, uri, src)

	mustHaveDiag(t, idx, "MS-MAP-MISSING-KEY")

	_, s := findBinding(idx, "y")
	if s == nil {
		t.Fatalf("binding y not found")
	}
	// We only assert it widens; concrete shape is analyzer-defined.
	if got := mindscript.FormatType(s.Type); got == "" {
		t.Fatalf("expected some type for y, got empty")
	}
}

func Test_Analysis_Modules_As_Value_Field_Access(t *testing.T) {
	const uri = "mem://modules-as-value.ms"
	src := `
module "M" do
  let x = 1
end

let m = M
let z = m.x
`
	idx := runIndex(t, uri, src)

	_, s := findBinding(idx, "z")
	if s == nil {
		t.Fatalf("binding z not found")
	}
	if got := mindscript.FormatType(s.Type); got != "Int" {
		t.Fatalf("want z: Int via module-as-map, got %q", got)
	}
}

func Test_Analysis_Modules_Binding_Only(t *testing.T) {
	const uri = "mem://modules-binding-only.ms"
	src := `
module "M" do
  let x = 1
end

let m = M
`
	idx := runIndex(t, uri, src)

	_, s := findBinding(idx, "m")
	if s == nil {
		t.Fatalf("binding m not found")
	}
	// We don't over-constrain the exact printed type, only that it's non-empty.
	if got := mindscript.FormatType(s.Type); got == "" {
		t.Fatalf("expected some type for m, got empty")
	}
}

// -----------------------------------------------------------------------------
// 19) Lexical environments & shadowing
// -----------------------------------------------------------------------------

func Test_Analysis_Shadowing_Inner_Shadows_Outer(t *testing.T) {
	const uri = "mem://shadow-inner.ms"
	src := `
let x = 1
let y
do
  let x = "s"
  y = x
end
`
	idx := runIndex(t, uri, src)

	// Outer x should remain Enum[1].
	_, x := findBinding(idx, "x")
	if x == nil {
		t.Fatalf("binding x not found")
	}
	if got := mindscript.FormatType(x.Type); got != "Enum[1]" {
		t.Fatalf("want outer x: Enum[1], got %q", got)
	}

	// y becomes Enum["s"] (assigned from inner x).
	_, y := findBinding(idx, "y")
	if y == nil {
		t.Fatalf("binding y not found")
	}
	if got := mindscript.FormatType(y.Type); got != "Enum[\"s\"]" {
		t.Fatalf("want y: Enum[\"s\"] (Null then inner literal), got %q", got)
	}
}

func Test_Analysis_Shadowing_Innermost_Wins(t *testing.T) {
	const uri = "mem://shadow-innermost.ms"
	src := `
let x = 1
let z
do
  let x = true
  z = x
end
`
	idx := runIndex(t, uri, src)

	// Outer x should remain Enum[1].
	_, x := findBinding(idx, "x")
	if x == nil {
		t.Fatalf("binding x not found")
	}
	if got := mindscript.FormatType(x.Type); got != "Enum[1]" {
		t.Fatalf("want outer x: Enum[1], got %q", got)
	}

	// z becomes Enum[true] (assigned from inner x).
	_, z := findBinding(idx, "z")
	if z == nil {
		t.Fatalf("binding z not found")
	}
	if got := mindscript.FormatType(z.Type); got != "Enum[true]" {
		t.Fatalf("want z: Enum[true] (Null then inner literal), got %q", got)
	}
}

func Test_Analysis_Shadowing_Outer_Survives_Block(t *testing.T) {
	const uri = "mem://shadow-outer.ms"
	src := `
let x = 1
do
  let x = "s"
end
let y = x
`
	idx := runIndex(t, uri, src)

	_, x := findBinding(idx, "x")
	if x == nil {
		t.Fatalf("binding x not found")
	}
	if got := mindscript.FormatType(x.Type); got != "Enum[1]" {
		t.Fatalf("want outer x: Enum[1], got %q", got)
	}

	_, y := findBinding(idx, "y")
	if y == nil {
		t.Fatalf("binding y not found")
	}
	if got := mindscript.FormatType(y.Type); got != "Enum[1]" {
		t.Fatalf("want y: Enum[1] (from outer x), got %q", got)
	}
}

func Test_Analysis_Shadowing_Ambient_Builtin_With_Local(t *testing.T) {
	const uri = "mem://shadow-ambient.ms"
	src := `
let len = fun(s: Str) -> Int do 1 end
let n = len("hi")
`

	// Use a real interpreter to populate ambient, per spec.
	ip, err := mindscript.NewInterpreter()
	if err != nil {
		t.Fatalf("NewInterpreter: %v", err)
	}
	idx := runIndexWithIP(t, uri, src, ip)

	_, n := findBinding(idx, "n")
	if n == nil {
		t.Fatalf("binding n not found")
	}
	if got := mindscript.FormatType(n.Type); got != "Int" {
		t.Fatalf("want n: Int, got %q", got)
	}
	mustNotHaveDiag(t, idx, "MS-ARG-TYPE-MISMATCH")
	mustNotHaveDiag(t, idx, "MS-UNKNOWN-NAME")
}

// -----------------------------------------------------------------------------
// 20) Annotations (docs)
// -----------------------------------------------------------------------------

func Test_Analysis_Annotations_Leading_On_Let(t *testing.T) {
	const uri = "mem://annot-leading.ms"
	src := `
# x is the answer
let x = 1
`
	idx := runIndex(t, uri, src)

	val, x := findBinding(idx, "x")
	if x == nil {
		t.Fatalf("binding x not found")
	}
	if got := mindscript.FormatType(x.Type); got != "Enum[1]" {
		t.Fatalf("want x: Enum[1], got %q", got)
	}
	if val.Annot != "x is the answer" {
		t.Fatalf("want doc %q, got %q", "x is the answer", val.Annot)
	}
}

func Test_Analysis_Annotations_On_RHS_Expression(t *testing.T) {
	const uri = "mem://annot-rhs.ms"
	src := `
let x =
  # important number
  1
`
	idx := runIndex(t, uri, src)

	val, x := findBinding(idx, "x")
	if x == nil {
		t.Fatalf("binding x not found")
	}
	if got := mindscript.FormatType(x.Type); got != "Enum[1]" {
		t.Fatalf("want x: Enum[1], got %q", got)
	}
	if val.Annot != "important number" {
		t.Fatalf("want doc %q, got %q", "important number", val.Annot)
	}
}

func Test_Analysis_Annotations_Multiline_Chain(t *testing.T) {
	const uri = "mem://annot-multiline.ms"
	src := `
let x =
  # line one
  # line two
  1
`
	idx := runIndex(t, uri, src)

	val, x := findBinding(idx, "x")
	if x == nil {
		t.Fatalf("binding x not found")
	}
	if got := mindscript.FormatType(x.Type); got != "Enum[1]" {
		t.Fatalf("want x: Enum[1], got %q", got)
	}
	wantDoc := "line one\nline two"
	if val.Annot != wantDoc {
		t.Fatalf("want doc %q, got %q", wantDoc, val.Annot)
	}
}

func Test_Analysis_Annotations_NonSemantic_For_Types(t *testing.T) {
	const uri = "mem://annot-nonsemantic.ms"
	src := `
# not a type
let x = "hi"
`
	idx := runIndex(t, uri, src)

	_, x := findBinding(idx, "x")
	if x == nil {
		t.Fatalf("binding x not found")
	}
	if got := mindscript.FormatType(x.Type); got != "Enum[\"hi\"]" {
		t.Fatalf("want x: Enum[\"hi\"], got %q", got)
	}
}

func Test_Analysis_Annotations_Propagate_Through_Alias_Assign(t *testing.T) {
	const uri = "mem://annot-propagate.ms"
	src := `
# outer doc
let x = 1
let y = x
`
	idx := runIndex(t, uri, src)

	valY, y := findBinding(idx, "y")
	if y == nil {
		t.Fatalf("binding y not found")
	}
	if got := mindscript.FormatType(y.Type); got != "Enum[1]" {
		t.Fatalf("want y: Enum[1], got %q", got)
	}
	if valY.Annot != "outer doc" {
		t.Fatalf("want propagated doc %q on y, got %q", "outer doc", valY.Annot)
	}
}

// -----------------------------------------------------------------------------
// 21) Destructuring
// -----------------------------------------------------------------------------

func Test_Analysis_Destructuring_Array_Exact(t *testing.T) {
	const uri = "mem://destr-arr-exact.ms"
	src := `
let [a, b] = [1, 2]
`
	idx := runIndex(t, uri, src)

	_, a := findBinding(idx, "a")
	_, b := findBinding(idx, "b")
	if a == nil || b == nil {
		t.Fatalf("expected bindings a and b")
	}
	if got := mindscript.FormatType(a.Type); got != "Enum[1]" {
		t.Fatalf("want a: Enum[1], got %q", got)
	}
	if got := mindscript.FormatType(b.Type); got != "Enum[2]" {
		t.Fatalf("want b: Enum[2], got %q", got)
	}
}

func Test_Analysis_Destructuring_Array_Extra_RHS_Elems(t *testing.T) {
	const uri = "mem://destr-arr-extra.ms"
	src := `
let [a, b] = [1, 2, 3]
`
	idx := runIndex(t, uri, src)

	_, a := findBinding(idx, "a")
	_, b := findBinding(idx, "b")
	if a == nil || b == nil {
		t.Fatalf("expected bindings a and b")
	}
	if got := mindscript.FormatType(a.Type); got != "Enum[1]" {
		t.Fatalf("want a: Enum[1], got %q", got)
	}
	if got := mindscript.FormatType(b.Type); got != "Enum[2]" {
		t.Fatalf("want b: Enum[2], got %q", got)
	}
}

func Test_Analysis_Destructuring_Array_Too_Short_RHS_Diagnostic(t *testing.T) {
	const uri = "mem://destr-arr-short.ms"
	src := `
let [a, b, c] = [1, 2]
`
	idx := runIndex(t, uri, src)

	// Spec: do not silently invent c = null; report a diagnostic.
	mustHaveDiag(t, idx, "MS-INVALID-ASSIGN-TARGET")
}

func Test_Analysis_Destructuring_Object_Exact(t *testing.T) {
	const uri = "mem://destr-obj-exact.ms"
	src := `
let p = {x: 1, y: 2}
let {x: a, y: b} = p
`
	idx := runIndex(t, uri, src)

	_, a := findBinding(idx, "a")
	_, b := findBinding(idx, "b")
	if a == nil || b == nil {
		t.Fatalf("expected bindings a and b")
	}
	if got := mindscript.FormatType(a.Type); got != "Enum[1]" {
		t.Fatalf("want a: Enum[1], got %q", got)
	}
	if got := mindscript.FormatType(b.Type); got != "Enum[2]" {
		t.Fatalf("want b: Enum[2], got %q", got)
	}
	mustNotHaveDiag(t, idx, "MS-MAP-MISSING-KEY")
}

func Test_Analysis_Destructuring_Object_Missing_Key_Becomes_Null_With_Doc(t *testing.T) {
	const uri = "mem://destr-obj-missing.ms"
	src := `
let p = {x: 1}
let {x: a, y: b} = p
`
	idx := runIndex(t, uri, src)

	_, a := findBinding(idx, "a")
	_, b := findBinding(idx, "b")
	if a == nil || b == nil {
		t.Fatalf("expected bindings a and b")
	}
	if got := mindscript.FormatType(a.Type); got != "Enum[1]" {
		t.Fatalf("want a: Enum[1], got %q", got)
	}
	if got := mindscript.FormatType(b.Type); got != "Null" {
		t.Fatalf("want b: Null (missing key), got %q", got)
	}
	if v, s := findBinding(idx, "b"); s == nil || v.Annot != "object pattern: missing key 'y'" {
		t.Fatalf("want doc %q on b, got %q", "object pattern: missing key 'y'", v.Annot)
	}
}

// -----------------------------------------------------------------------------
// Token index basics
// -----------------------------------------------------------------------------

// --- Helpers (minimal; test-only) --------------------------------------------

func test_analysis_posOf(s, needle string, nth int) (start, end int) {
	if nth <= 0 {
		nth = 1
	}
	i := 0
	from := 0
	for {
		j := strings.Index(s[from:], needle)
		if j < 0 {
			return -1, -1
		}
		i++
		if i == nth {
			start = from + j
			end = start + len(needle)
			return
		}
		from = from + j + len(needle)
	}
}

func test_analysis_findIdentTokens(idx *FileIndex, name string) []TokenEntry {
	var out []TokenEntry
	for _, t := range idx.Tokens.Entries {
		if t.Kind == TokIdentifier && t.Text == name {
			out = append(out, t)
		}
	}
	return out
}

func test_analysis_findTokenCovering(idx *FileIndex, pos int) *TokenEntry {
	for i := range idx.Tokens.Entries {
		t := &idx.Tokens.Entries[i]
		if t.Start <= pos && pos < t.End {
			return t
		}
	}
	return nil
}

func test_analysis_getDiag(idx *FileIndex, code string) (Diag, bool) {
	for _, d := range idx.Diags {
		if d.Code == code {
			return d, true
		}
	}
	return Diag{}, false
}

// --- A. Tokens & def/use ------------------------------------------------------

func Test_Analysis_Tokens_Ident_Decl_And_Span(t *testing.T) {
	const uri = "mem://tok-decl-span.ms"
	const src = `let x = 1`
	idx := runIndex(t, uri, src)
	toks := test_analysis_findIdentTokens(idx, "x")
	if len(toks) != 1 {
		t.Fatalf("want 1 'x' token, got %d", len(toks))
	}
	start, end := test_analysis_posOf(src, "x", 1)
	if toks[0].IsDecl != true {
		t.Fatalf("want IsDecl=true")
	}
	if toks[0].Start != start || toks[0].End != end {
		t.Fatalf("want span [%d,%d), got [%d,%d)", start, end, toks[0].Start, toks[0].End)
	}
	if toks[0].Payload.Tag != mindscript.VTSymbol {
		t.Fatalf("want VTSymbol payload for decl")
	}
}

func Test_Analysis_Tokens_DefUse_SharedSym(t *testing.T) {
	const uri = "mem://tok-defuse.ms"
	const src = `
let x = 1
let y = x
`
	idx := runIndex(t, uri, src)
	xToks := test_analysis_findIdentTokens(idx, "x")
	if len(xToks) < 2 {
		t.Fatalf("want at least 2 'x' tokens (decl+use), got %d", len(xToks))
	}
	var decl, use *TokenEntry
	for i := range xToks {
		if xToks[i].IsDecl {
			decl = &xToks[i]
		} else {
			use = &xToks[i]
		}
	}
	if decl == nil || use == nil {
		t.Fatalf("need decl and use tokens")
	}
	if decl.Payload.Tag != mindscript.VTSymbol || use.Payload.Tag != mindscript.VTSymbol || decl.Payload.Data != use.Payload.Data {
		t.Fatalf("decl/use should share the same Symbol payload")
	}
}

func Test_Analysis_Tokens_MultipleUses_FindAllRefs(t *testing.T) {
	const uri = "mem://tok-refs.ms"
	const src = `
let x = 1
x
x
`
	idx := runIndex(t, uri, src)
	xToks := test_analysis_findIdentTokens(idx, "x")
	if len(xToks) != 3 {
		t.Fatalf("want 3 'x' tokens (1 decl + 2 uses), got %d", len(xToks))
	}
	decl := -1
	for i := range xToks {
		if xToks[i].IsDecl {
			decl = i
			break
		}
	}
	if decl < 0 {
		t.Fatalf("missing decl token")
	}
	for i := range xToks {
		if i == decl {
			continue
		}
		if xToks[i].Payload.Tag != mindscript.VTSymbol || xToks[decl].Payload.Data != xToks[i].Payload.Data {
			t.Fatalf("all uses should link to decl Sym")
		}
	}
}

func Test_Analysis_Tokens_Shadowing_SymSeparatesAndRefsWork(t *testing.T) {
	const uri = "mem://tok-shadow.ms"
	const src = `
let x = 1
do
  let x = "s"
  let y = x
end
let z = x
`
	idx := runIndex(t, uri, src)
	xToks := test_analysis_findIdentTokens(idx, "x")
	// Expect: 2 decls + 2 uses
	if len(xToks) < 4 {
		t.Fatalf("want at least 4 x tokens, got %d", len(xToks))
	}
	var outerDecl, innerDecl *TokenEntry
	for i := range xToks {
		if xToks[i].IsDecl {
			if outerDecl == nil {
				outerDecl = &xToks[i]
			} else {
				innerDecl = &xToks[i]
			}
		}
	}
	if outerDecl == nil || innerDecl == nil || outerDecl.Payload.Data == innerDecl.Payload.Data {
		t.Fatalf("decls must exist and have different Sym")
	}
	// Use before end of block should link to inner
	var innerUse *TokenEntry
	for i := range idx.Tokens.Entries {
		tok := &idx.Tokens.Entries[i]
		if tok.Text == "x" && !tok.IsDecl {
			// heuristic: find the first use inside block by byte range (“ x\nend” region),
			// but we only need to assert at least one use links to inner.
			innerUse = tok
			break
		}
	}
	if innerUse == nil || innerUse.Payload.Data != innerDecl.Payload.Data {
		t.Fatalf("inner use should link to inner decl")
	}
	// Use after block (z = x) should link to outer
	zToks := test_analysis_findIdentTokens(idx, "z")
	if len(zToks) == 0 {
		t.Fatalf("missing z token")
	}
	// Find the 'x' token near 'z = x'
	var outerUse *TokenEntry
	startZ, _ := test_analysis_posOf(src, "z", 1)
	for i := range idx.Tokens.Entries {
		tok := &idx.Tokens.Entries[i]
		if tok.Text == "x" && !tok.IsDecl && tok.Start > startZ {
			outerUse = tok
			break
		}
	}
	if outerUse == nil || outerUse.Payload.Data != outerDecl.Payload.Data {
		t.Fatalf("outer use should link to outer decl")
	}
}

func Test_Analysis_Tokens_Reassignment_DefSiteStable(t *testing.T) {
	const uri = "mem://tok-reassign.ms"
	const src = `
let x
x = 1
x = "ok"
`
	idx := runIndex(t, uri, src)
	toks := test_analysis_findIdentTokens(idx, "x")
	if len(toks) != 3 {
		t.Fatalf("want 3 x tokens (1 decl, 2 uses), got %d", len(toks))
	}
	var decl *TokenEntry
	for i := range toks {
		if toks[i].IsDecl {
			decl = &toks[i]
			break
		}
	}
	if decl == nil {
		t.Fatalf("no decl token found")
	}
	for i := range toks {
		if toks[i].IsDecl {
			continue
		}
		if toks[i].Payload.Tag != mindscript.VTSymbol || toks[i].Payload.Data != decl.Payload.Data {
			t.Fatalf("reassign uses must link to the original decl Sym")
		}
	}
}

// --- B. Unknown & ambient -----------------------------------------------------

func Test_Analysis_Unknown_Name_Tokenized_WithSpan(t *testing.T) {
	const uri = "mem://tok-unknown-span.ms"
	const src = `y = x`
	idx := runIndex(t, uri, src)

	// Token for 'x' exists and has no Sym
	xToks := test_analysis_findIdentTokens(idx, "x")
	if len(xToks) != 1 || xToks[0].IsDecl {
		t.Fatalf("want one use token for x")
	}
	if xToks[0].Payload.Tag == mindscript.VTSymbol {
		t.Fatalf("unknown name should not carry VTSymbol payload")
	}
	start, end := test_analysis_posOf(src, "x", 1)
	if xToks[0].Start != start || xToks[0].End != end {
		t.Fatalf("want token span [%d,%d), got [%d,%d)", start, end, xToks[0].Start, xToks[0].End)
	}
	// Diag anchors x
	d, ok := test_analysis_getDiag(idx, "MS-UNKNOWN-NAME")
	if !ok {
		t.Fatalf("missing MS-UNKNOWN-NAME diag")
	}
	if d.StartByte != start || d.EndByte != end {
		t.Fatalf("diag should cover 'x' [%d,%d), got [%d,%d)", start, end, d.StartByte, d.EndByte)
	}
}

func Test_Analysis_Ambient_Name_Token_NoDiag(t *testing.T) {
	ip, err := mindscript.NewInterpreter()
	if err != nil {
		t.Fatalf("NewInterpreter: %v", err)
	}
	const uri = "mem://tok-ambient.ms"
	const src = `len([1,2])`
	idx := runIndexWithIP(t, uri, src, ip)

	lenToks := test_analysis_findIdentTokens(idx, "len")
	if len(lenToks) != 1 || lenToks[0].IsDecl {
		t.Fatalf("want one use token for ambient 'len'")
	}
	if lenToks[0].Payload.Tag == mindscript.VTSymbol {
		// ambient should not carry a VTSymbol (not a local binding)
		t.Fatalf("ambient name should not have VTSymbol payload")
	}
	if hasDiag(idx, "MS-UNKNOWN-NAME") {
		t.Fatalf("ambient name should not trigger unknown-name diag")
	}
}

func Test_Analysis_Local_Shadows_Ambient_SymLinks(t *testing.T) {
	ip, err := mindscript.NewInterpreter()
	if err != nil {
		t.Fatalf("NewInterpreter: %v", err)
	}
	const uri = "mem://tok-shadow-ambient.ms"
	const src = `
let len = fun(s: Str) -> Int do 1 end
len("hi")
`
	idx := runIndexWithIP(t, uri, src, ip)

	lenToks := test_analysis_findIdentTokens(idx, "len")
	if len(lenToks) != 2 {
		t.Fatalf("want 2 len tokens (decl + call), got %d", len(lenToks))
	}
	var decl, use *TokenEntry
	for i := range lenToks {
		if lenToks[i].IsDecl {
			decl = &lenToks[i]
		} else {
			use = &lenToks[i]
		}
	}
	if decl == nil || use == nil || decl.Payload.Tag != mindscript.VTSymbol || use.Payload.Tag != mindscript.VTSymbol || decl.Payload.Data != use.Payload.Data {
		t.Fatalf("local shadow should link decl/use via same Sym")
	}
	mustNotHaveDiag(t, idx, "MS-ARG-TYPE-MISMATCH")
	mustNotHaveDiag(t, idx, "MS-UNKNOWN-NAME")
}

// --- C. Params & fun/oracle ---------------------------------------------------

func Test_Analysis_Params_Are_Decls_WithSpans(t *testing.T) {
	const uri = "mem://params-decls.ms"
	const src = `let f = fun(a: Int, b: Int) -> Int do a + b end`
	idx := runIndex(t, uri, src)

	aToks := test_analysis_findIdentTokens(idx, "a")
	bToks := test_analysis_findIdentTokens(idx, "b")
	if len(aToks) < 2 || len(bToks) < 2 {
		t.Fatalf("want decl+use for both params")
	}
	// find decls by IsDecl
	var aDecl, bDecl *TokenEntry
	for i := range aToks {
		if aToks[i].IsDecl {
			aDecl = &aToks[i]
			break
		}
	}
	for i := range bToks {
		if bToks[i].IsDecl {
			bDecl = &bToks[i]
			break
		}
	}
	if aDecl == nil || bDecl == nil || aDecl.Payload.Tag != mindscript.VTSymbol || bDecl.Payload.Tag != mindscript.VTSymbol {
		t.Fatalf("param decl tokens must exist and carry VTSymbol payload")
	}
	// Spans cover the param names
	aStart, _ := test_analysis_posOf(src, "a:", 1)
	if aDecl.End != aStart+1 || aDecl.Start != aStart {
		t.Fatalf("param a span should cover 'a'")
	}
	bStart, _ := test_analysis_posOf(src, "b:", 1)
	if bDecl.Start != bStart || bDecl.End != bStart+1 {
		t.Fatalf("param b span should cover 'b'")
	}
}

func Test_Analysis_Oracle_Param_Decl_Tokenized(t *testing.T) {
	const uri = "mem://oracle-param.ms"
	const src = `let g = oracle(x: Int) -> Str`
	idx := runIndex(t, uri, src)

	xToks := test_analysis_findIdentTokens(idx, "x")
	if len(xToks) != 1 || !xToks[0].IsDecl || xToks[0].Payload.Tag != mindscript.VTSymbol {
		t.Fatalf("oracle param 'x' should be a decl with Sym")
	}
	start, end := test_analysis_posOf(src, "x:", 1)
	if xToks[0].Start != start || xToks[0].End != start+1 || end != start+2 {
		t.Fatalf("param 'x' span should be exactly the 'x' byte")
	}
}

// --- D. Destructuring ---------------------------------------------------------

func Test_Analysis_Destructuring_Array_Decl_Tokens_Spans(t *testing.T) {
	const uri = "mem://destr-arr-decl-spans.ms"
	const src = `let [a, b] = [1, 2]`
	idx := runIndex(t, uri, src)

	aToks := test_analysis_findIdentTokens(idx, "a")
	bToks := test_analysis_findIdentTokens(idx, "b")
	if len(aToks) != 1 || len(bToks) != 1 {
		t.Fatalf("want decl tokens for a and b")
	}
	if !aToks[0].IsDecl || !bToks[0].IsDecl || aToks[0].Payload.Tag != mindscript.VTSymbol || bToks[0].Payload.Tag != mindscript.VTSymbol {
		t.Fatalf("destructured names should be decls with Sym")
	}
	aStart, aEnd := test_analysis_posOf(src, "a", 1)
	bStart, bEnd := test_analysis_posOf(src, "b", 1)
	if aToks[0].Start != aStart || aToks[0].End != aEnd {
		t.Fatalf("a span mismatch")
	}
	if bToks[0].Start != bStart || bToks[0].End != bEnd {
		t.Fatalf("b span mismatch")
	}
}

func Test_Analysis_Destructuring_Object_Decl_Tokens_Spans_MissingKeyDoc(t *testing.T) {
	const uri = "mem://destr-obj-decl-spans.ms"
	const src = `
let p = {x: 1}
let {x: a, y: b} = p
`
	idx := runIndex(t, uri, src)

	aToks := test_analysis_findIdentTokens(idx, "a")
	bToks := test_analysis_findIdentTokens(idx, "b")
	if len(aToks) != 1 || len(bToks) != 1 {
		t.Fatalf("want decl tokens for a and b")
	}
	if !aToks[0].IsDecl || !bToks[0].IsDecl {
		t.Fatalf("a/b should be decls")
	}
	// b binding doc should say missing key 'y' (checked in existing tests),
	// here we only ensure tokens have spans:
	aStart, aEnd := test_analysis_posOf(src, "a", 1)
	bStart, bEnd := test_analysis_posOf(src, "b", 1)
	if aToks[0].Start != aStart || aToks[0].End != aEnd {
		t.Fatalf("a span mismatch")
	}
	if bToks[0].Start != bStart || bToks[0].End != bEnd {
		t.Fatalf("b span mismatch")
	}
}

// --- E. Diagnostic span accuracy ---------------------------------------------

func Test_Analysis_Spans_Diag_ArgTypeMismatch_On_Arg(t *testing.T) {
	const uri = "mem://diag-arg-mismatch-span.ms"
	const src = `
let f = fun(x: Int) -> Int do x end
f("oops")
`
	idx := runIndex(t, uri, src)
	d, ok := test_analysis_getDiag(idx, "MS-ARG-TYPE-MISMATCH")
	if !ok {
		t.Fatalf("missing MS-ARG-TYPE-MISMATCH diag")
	}
	start, end := test_analysis_posOf(src, `"oops"`, 1)
	if d.StartByte != start || d.EndByte != end {
		t.Fatalf("diag should cover offending arg [%d,%d), got [%d,%d)", start, end, d.StartByte, d.EndByte)
	}
}

func Test_Analysis_Spans_Diag_IndexMustBeInt_On_Index(t *testing.T) {
	const uri = "mem://diag-idx-span.ms"
	const src = `
let xs = [1,2,3]
xs["0"]
`
	idx := runIndex(t, uri, src)
	d, ok := test_analysis_getDiag(idx, "MS-ARG-TYPE-MISMATCH")
	if !ok {
		t.Fatalf("missing MS-ARG-TYPE-MISMATCH for non-int index")
	}
	start, end := test_analysis_posOf(src, `"0"`, 1)
	if d.StartByte != start || d.EndByte != end {
		t.Fatalf("diag should cover index expr [%d,%d), got [%d,%d)", start, end, d.StartByte, d.EndByte)
	}
}

func Test_Analysis_Spans_Diag_ArgOverflow_On_FirstExtraArg(t *testing.T) {
	const uri = "mem://diag-overflow-span.ms"
	const src = `
let f = fun(a: Int, b: Int) -> Int do a + b end
f(1,2,3)
`
	idx := runIndex(t, uri, src)
	d, ok := test_analysis_getDiag(idx, "MS-ARG-OVERFLOW")
	if !ok {
		t.Fatalf("missing MS-ARG-OVERFLOW diag")
	}
	start, end := test_analysis_posOf(src, "3", 1)
	if d.StartByte != start || d.EndByte != end {
		t.Fatalf("overflow diag should cover first extra arg [%d,%d), got [%d,%d)", start, end, d.StartByte, d.EndByte)
	}
}

func Test_Analysis_Spans_Diag_DivByZero_On_RHSZero(t *testing.T) {
	const uri = "mem://diag-divzero-span.ms"
	const src = `3 % 0`
	idx := runIndex(t, uri, src)
	d, ok := test_analysis_getDiag(idx, "MS-DIV-BY-ZERO-CONST")
	if !ok {
		t.Fatalf("missing MS-DIV-BY-ZERO-CONST diag")
	}
	start, end := test_analysis_posOf(src, "0", 1)
	if d.StartByte != start || d.EndByte != end {
		t.Fatalf("div-by-zero diag should cover RHS zero [%d,%d), got [%d,%d)", start, end, d.StartByte, d.EndByte)
	}
}
