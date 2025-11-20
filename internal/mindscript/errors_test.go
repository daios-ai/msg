package mindscript

import (
	"strconv"
	"strings"
	"testing"
)

func mustContain(t *testing.T, s, sub string) {
	t.Helper()
	if !strings.Contains(s, sub) {
		t.Fatalf("expected output to contain %q\n--- output ---\n%s", sub, s)
	}
}

// Accepts either:
//
//	"RUNTIME ERROR at <line>:"
//
// or
//
//	"RUNTIME ERROR in <anything> at <line>:"
func mustRuntimeAtLine(t *testing.T, msg string, line int) {
	t.Helper()
	// Old format
	old := "RUNTIME ERROR at " + strconv.Itoa(line) + ":"
	if strings.Contains(msg, old) {
		return
	}
	// New format: look for "RUNTIME ERROR in ... at <line>:"
	prefix := "RUNTIME ERROR in "
	if i := strings.Index(msg, prefix); i >= 0 {
		wantTail := " at " + strconv.Itoa(line) + ":"
		if strings.Contains(msg[i+len(prefix):], wantTail) {
			return
		}
	}
	t.Fatalf("expected runtime error to report line %d\n--- output ---\n%s", line, msg)
}

// Accepts either:
//
//	"<KIND> at ..."
//
// or
//
//	"<KIND> in <anything> at ..."
func mustHaveHeader(t *testing.T, msg, kind string) {
	t.Helper()
	if strings.Contains(msg, kind+" in ") || strings.Contains(msg, kind+" at") {
		return
	}
	t.Fatalf("expected header to contain %q (with optional 'in <src>')\n--- output ---\n%s", kind, msg)
}

func Test_ErrorWrap_Parse_ShowsCaretAndContext(t *testing.T) {
	// Two lines; parse error on line 2: missing ')'
	src := `let x = 1
f(1`

	// Pretty calls ParseSExpr under the hood and now wraps with WrapErrorWithSource
	_, err := Pretty(src)
	if err == nil {
		t.Fatalf("expected parse error, got nil")
	}
	msg := err.Error()

	// Header (allow both old/new)
	mustHaveHeader(t, msg, "PARSE ERROR")
	// Context lines (line numbers + source)
	mustContain(t, msg, "   1 | let x = 1")
	mustContain(t, msg, "   2 | f(1")
	// Caret line
	mustContain(t, msg, "     | ")
	mustContain(t, msg, "^")
}

func Test_ErrorWrap_Lex_ShowsCaretAndContext(t *testing.T) {
	// Two lines; lex error on line 2: invalid \\u escape (non-hex)
	src := "let ok = 1\n\"bad \\u12GZ\""

	// Pretty triggers lexing as well
	_, err := Pretty(src)
	if err == nil {
		t.Fatalf("expected lex error, got nil")
	}
	msg := err.Error()

	// Header (allow both old/new)
	mustHaveHeader(t, msg, "LEXICAL ERROR")
	// Context lines
	mustContain(t, msg, "   1 | let ok = 1")
	mustContain(t, msg, "   2 | \"bad \\u12GZ\"")
	// Caret line
	mustContain(t, msg, "     | ")
	mustContain(t, msg, "^")

	// A bit of the lexer message to be sure it's the unicode escape path
	if !(strings.Contains(strings.ToLower(msg), "unicode") || strings.Contains(strings.ToLower(msg), "escape")) {
		t.Fatalf("expected unicode escape related message, got:\n%s", msg)
	}
}

func Test_Errors_Binop_DivZero_RHSLine(t *testing.T) {
	// RHS is on line 2; caret should point there (we mark at RHS for the op).
	src := "10 /\n 0"
	ip, _ := NewInterpreter()
	_, err := ip.EvalSource(src)
	if err == nil {
		t.Fatalf("expected runtime error, got nil")
	}
	msg := err.Error()

	mustRuntimeAtLine(t, msg, 2)
	mustContain(t, msg, "   1 | 10 /")
	mustContain(t, msg, "   2 |  0")
	mustContain(t, msg, "division by zero")
	// caret presence
	mustContain(t, msg, "     | ")
	mustContain(t, msg, "^")
}

func Test_Errors_Unary_NotOperandLine(t *testing.T) {
	// Operand is on line 2; caret should point at the operand (not the 'not').
	src := "not\n  1"
	ip, _ := NewInterpreter()
	_, err := ip.EvalSource(src)
	if err == nil {
		t.Fatalf("expected runtime error, got nil")
	}
	msg := err.Error()

	mustRuntimeAtLine(t, msg, 2)
	mustContain(t, msg, "   1 | not")
	mustContain(t, msg, "   2 |   1")
	mustContain(t, msg, "not expects boolean")
	mustContain(t, msg, "^")
}

func Test_Errors_If_ConditionLine(t *testing.T) {
	// The condition is non-boolean on line 1; caret should point to the condition.
	src := `if 1 then
  do 42 end
else
  do 0 end
end`
	ip, _ := NewInterpreter()
	_, err := ip.EvalSource(src)
	if err == nil {
		t.Fatalf("expected runtime error, got nil")
	}
	msg := err.Error()

	mustRuntimeAtLine(t, msg, 1)
	mustContain(t, msg, "   1 | if 1 then")
	mustContain(t, msg, "condition must be boolean")
	mustContain(t, msg, "^")
}

func Test_Errors_Assign_Index_LHSLine(t *testing.T) {
	// Out-of-range assignment target is on line 2; caret should blame the LHS (arr[1]).
	src := `let arr = [1]
arr[1] = 0`
	ip, _ := NewInterpreter()
	_, err := ip.EvalSource(src)
	if err == nil {
		t.Fatalf("expected runtime error, got nil")
	}
	msg := err.Error()

	mustRuntimeAtLine(t, msg, 2)
	mustContain(t, msg, "   1 | let arr = [1]")
	mustContain(t, msg, "   2 | arr[1] = 0")
	mustContain(t, msg, "array index out of range")
	mustContain(t, msg, "^")
}

func Test_Errors_Call_ArgTypeLine(t *testing.T) {
	// Type mismatch for parameter 'x' where the argument lives on line 3.
	src := `let f = fun(x: Int) -> Int do x end
f(
  true
)`
	ip, _ := NewInterpreter()
	_, err := ip.EvalSource(src)
	if err == nil {
		t.Fatalf("expected runtime error, got nil")
	}
	msg := err.Error()

	mustRuntimeAtLine(t, msg, 3)
	mustContain(t, msg, "type mismatch in parameter 'x'")
	mustContain(t, msg, "   2 | f(")
	mustContain(t, msg, "   3 |   true")
	mustContain(t, msg, "^")
}

func Test_Errors_ReturnTypeMismatch_ReturnExprLine(t *testing.T) {
	// Inside a user function, return type mismatch should point to the return expr line.
	src := `let f = fun() -> Int do
  return "bad"
end
f()`
	ip, _ := NewInterpreter()
	_, err := ip.EvalSource(src)
	if err == nil {
		t.Fatalf("expected runtime error, got nil")
	}
	msg := err.Error()

	// The offending subexpression ("\"bad\"") is on line 2 of the whole source.
	mustRuntimeAtLine(t, msg, 2)
	mustContain(t, msg, `   2 |   return "bad"`)
	mustContain(t, msg, "return type mismatch")
	mustContain(t, msg, "^")
}

func Test_Errors_And_LHS_ConditionLine(t *testing.T) {
	// Non-boolean LHS for 'and' should point at the LHS line.
	src := "1 and\n true"
	ip, _ := NewInterpreter()
	_, err := ip.EvalSource(src)
	if err == nil {
		t.Fatalf("expected runtime error, got nil")
	}
	msg := err.Error()

	mustRuntimeAtLine(t, msg, 1)
	mustContain(t, msg, "   1 | 1 and")
	mustContain(t, msg, "condition must be boolean")
	mustContain(t, msg, "^")
}

func Test_Errors_MapProperty_UnknownKey_PropSite(t *testing.T) {
	src := `let m = {a:1}
m.nope`

	ip, _ := NewInterpreter()
	_, err := ip.EvalSource(src)
	if err == nil {
		t.Fatalf("expected runtime error, got nil")
	}
	msg := err.Error()

	mustRuntimeAtLine(t, msg, 2)
	mustContain(t, msg, `   2 | m.nope`)
	mustContain(t, msg, "unknown property")
	mustContain(t, msg, `"nope"`)
	mustContain(t, msg, "     | ")
	mustContain(t, msg, "^")
}

func Test_Errors_MapIndex_UnknownKey_IndexSite(t *testing.T) {
	src := "let m = {a:1}\n" + `m["nope"]`

	ip, _ := NewInterpreter()
	_, err := ip.EvalSource(src)
	if err == nil {
		t.Fatalf("expected runtime error, got nil")
	}
	msg := err.Error()

	mustRuntimeAtLine(t, msg, 2)
	mustContain(t, msg, `   2 | m["nope"]`)
	mustContain(t, msg, "unknown key")
	mustContain(t, msg, `"nope"`)
	mustContain(t, msg, "     | ")
	mustContain(t, msg, "^")
}

func Test_Errors_For_IteratorAnnotatedNull_BlamedOnIterHeader(t *testing.T) {
	src := `let it = fun(_: Null) -> Any? do
  # BOOM
  null
end
for x in
  it
do
  1
end`

	ip, _ := NewInterpreter()
	_, err := ip.EvalSource(src)
	if err == nil {
		t.Fatalf("expected runtime error, got nil")
	}
	msg := err.Error()

	// Error currently blames the 'for x in' header (line 5), not the iter expr line.
	mustRuntimeAtLine(t, msg, 6)
	mustContain(t, msg, "BOOM")
	mustContain(t, msg, "   5 | for x in")
	mustContain(t, msg, "   6 |   it")
	mustContain(t, msg, "     | ")
	mustContain(t, msg, "^")
}

func Test_Errors_ArrayIndex_NegativeWrap_NoError(t *testing.T) {
	src := `let a = [10, 20, 30]
a[-1]`

	ip, _ := NewInterpreter()
	v, err := ip.EvalSource(src)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if v.Tag != VTInt {
		t.Fatalf("expected VTInt, got %v", v.Tag)
	}
	if v.Data.(int64) != 30 {
		t.Fatalf("expected 30 from negative index wrap, got %v", v.Data)
	}
}

func Test_Errors_EmptyArrayIndex_CaretOnIndex(t *testing.T) {
	src := `let a = []
a[0]`

	ip, _ := NewInterpreter()
	_, err := ip.EvalSource(src)
	if err == nil {
		t.Fatalf("expected runtime error, got nil")
	}
	msg := err.Error()

	// Index expression is on line 2; caret should point there.
	mustRuntimeAtLine(t, msg, 2)
	mustContain(t, msg, "index on empty array")
	mustContain(t, msg, "   2 | a[0]")
	mustContain(t, msg, "^")
}

func Test_Errors_ModZero_IntAndFloat_RHSLine(t *testing.T) {
	tests := []struct {
		name string
		src  string
	}{
		{"int_zero_rhs", "10 %\n 0"},
		{"float_zero_rhs", "10 %\n 0.0"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			ip, _ := NewInterpreter()
			_, err := ip.EvalSource(tc.src)
			if err == nil {
				t.Fatalf("expected runtime error, got nil")
			}
			msg := err.Error()
			mustRuntimeAtLine(t, msg, 2)
			mustContain(t, msg, "modulo by zero")
			mustContain(t, msg, "     | ")
			mustContain(t, msg, "^")
		})
	}
}

func Test_Errors_And_BadRHS_CaretOnRHSLine(t *testing.T) {
	// LHS true, RHS triggers a unary type error; blame should be on RHS line.
	src := "true and\n not 1"
	ip, _ := NewInterpreter()
	_, err := ip.EvalSource(src)
	if err == nil {
		t.Fatalf("expected runtime error, got nil")
	}
	msg := err.Error()

	mustRuntimeAtLine(t, msg, 2)
	mustContain(t, msg, "not expects boolean")
	mustContain(t, msg, "   1 | true and")
	mustContain(t, msg, "   2 |  not 1")
	mustContain(t, msg, "^")
}

func Test_Errors_Or_BadRHS_CaretOnRHSLine(t *testing.T) {
	// LHS false, RHS triggers a unary type error; blame should be on RHS line.
	src := "false or\n not 1"
	ip, _ := NewInterpreter()
	_, err := ip.EvalSource(src)
	if err == nil {
		t.Fatalf("expected runtime error, got nil")
	}
	msg := err.Error()

	mustRuntimeAtLine(t, msg, 2)
	mustContain(t, msg, "not expects boolean")
	mustContain(t, msg, "   1 | false or")
	mustContain(t, msg, "   2 |  not 1")
	mustContain(t, msg, "^")
}

func Test_Errors_Assign_PropertyOnNonMap_LHSLine(t *testing.T) {
	// LHS is not a map/module; assignment should blame the LHS site.
	src := `1.x = 2`
	ip, _ := NewInterpreter()
	_, err := ip.EvalSource(src)
	if err == nil {
		t.Fatalf("expected runtime error, got nil")
	}
	msg := err.Error()

	mustRuntimeAtLine(t, msg, 1)
	mustContain(t, msg, "undefined variable")
	mustContain(t, msg, "   1 | 1.x = 2")
	mustContain(t, msg, "^")
}

func Test_Errors_ComputedIndex_OnNonIndexable_BlamedOnIndexExprLine(t *testing.T) {
	// obj.(1) when obj is not array/map; caret should point at the computed index expr site.
	src := `let x = 1
x.(1)`
	ip, _ := NewInterpreter()
	_, err := ip.EvalSource(src)
	if err == nil {
		t.Fatalf("expected runtime error, got nil")
	}
	msg := err.Error()

	mustRuntimeAtLine(t, msg, 2)
	mustContain(t, msg, "index requires array[int] or map[string]")
	mustContain(t, msg, "   2 | x.(1)")
	mustContain(t, msg, "^")
}

func Test_Errors_Curried_TooManyArgs_BlamedOnExtraArg(t *testing.T) {
	src := `let f = fun(x: Int) -> Int do x end
f(1,
  2)`
	ip, _ := NewInterpreter()
	_, err := ip.EvalSource(src)
	if err == nil {
		t.Fatalf("expected runtime error, got nil")
	}
	msg := err.Error()

	// Extra argument '2' is on line 3.
	mustRuntimeAtLine(t, msg, 3)
	mustContain(t, msg, "not a function")
	mustContain(t, msg, "   2 | f(1,")
	mustContain(t, msg, "   3 |   2)")
	mustContain(t, msg, "^")
}
func Test_Error_Parse_MissingRParen_CaretColumn(t *testing.T) {
	// Single-line; missing ')' should anchor *after* the last completed node (after '1')
	src := "f(1"
	_, err := Pretty(src)
	if err == nil {
		t.Fatalf("expected parse error, got nil")
	}
	msg := err.Error()
	mustHaveHeader(t, msg, "PARSE ERROR")
	mustContain(t, msg, "   1 | f(1")
	mustContain(t, msg, "     | ")
	mustContain(t, msg, "^")

	// Verify caret column == len("f(1") (no tabs here, so spaces == bytes)
	var codeLine, caretLine string
	for _, ln := range strings.Split(msg, "\n") {
		switch {
		case strings.HasPrefix(ln, "   1 | "):
			codeLine = strings.TrimPrefix(ln, "   1 | ")
		case strings.HasPrefix(ln, "     | "):
			caretLine = strings.TrimPrefix(ln, "     | ")
		}
	}
	if codeLine == "" || caretLine == "" {
		t.Fatalf("missing code/caret lines\n--- output ---\n%s", msg)
	}
	pad := strings.Index(caretLine, "^")
	if pad < 0 {
		t.Fatalf("no caret found\n--- output ---\n%s", msg)
	}
	if pad != len(codeLine) {
		t.Fatalf("caret column = %d, want %d (after %q)", pad, len(codeLine), codeLine)
	}
}

func Test_Error_Parse_UnexpectedToken_CaretAtTokenStart(t *testing.T) {
	// "let 123" → caret at the start of the unexpected token '1' (after "let ")
	src := "let 123"
	_, err := Pretty(src)
	if err == nil {
		t.Fatalf("expected parse error, got nil")
	}
	msg := err.Error()
	mustHaveHeader(t, msg, "PARSE ERROR")
	mustContain(t, msg, "   1 | let 123")
	mustContain(t, msg, "     | ")
	mustContain(t, msg, "^")

	var codeLine, caretLine string
	for _, ln := range strings.Split(msg, "\n") {
		switch {
		case strings.HasPrefix(ln, "   1 | "):
			codeLine = strings.TrimPrefix(ln, "   1 | ")
		case strings.HasPrefix(ln, "     | "):
			caretLine = strings.TrimPrefix(ln, "     | ")
		}
	}
	if codeLine == "" || caretLine == "" {
		t.Fatalf("missing code/caret lines\n--- output ---\n%s", msg)
	}
	pad := strings.Index(caretLine, "^")
	if pad < 0 {
		t.Fatalf("no caret found\n--- output ---\n%s", msg)
	}
	// caret must sit exactly under the first byte of "123"
	want := strings.Index(codeLine, "1")
	if want < 0 {
		t.Fatalf("test sanity: no '1' in code line: %q", codeLine)
	}
	if pad != want {
		t.Fatalf("caret column = %d, want %d (under first digit of unexpected token)", pad, want)
	}
}

func Test_Error_Runtime_Call_SecondArgCaret_LineAndColumn(t *testing.T) {
	ip, _ := NewInterpreter()
	src := `
let f = fun(x: Int, y: Int) -> Int do x end
f(1, "oops")
`
	_, err := ip.EvalSource(src)
	if err == nil {
		t.Fatalf("expected runtime error, got nil")
	}
	msg := err.Error()

	// Must be on the second line (the call site)
	mustRuntimeAtLine(t, msg, 3)
	mustContain(t, msg, `   3 | f(1, "oops")`)
	mustContain(t, msg, "     | ")
	mustContain(t, msg, "^")

	// Caret should sit under the start of the second argument (just after "f(1, ")
	var codeLine, caretLine string
	for _, ln := range strings.Split(msg, "\n") {
		switch {
		case strings.HasPrefix(ln, "   3 | "):
			codeLine = strings.TrimPrefix(ln, "   3 | ")
		case strings.HasPrefix(ln, "     | "):
			caretLine = strings.TrimPrefix(ln, "     | ")
		}
	}
	if codeLine == "" || caretLine == "" {
		t.Fatalf("missing code/caret lines\n--- output ---\n%s", msg)
	}
	pad := strings.Index(caretLine, "^")
	if pad < 0 {
		t.Fatalf("no caret found\n--- output ---\n%s", msg)
	}
	want := strings.Index(codeLine, `"oops"`)
	if want < 0 {
		t.Fatalf("test sanity: second argument not found in code line: %q", codeLine)
	}
	if pad != want {
		t.Fatalf("caret column = %d, want %d (start of second argument)", pad, want)
	}
}

func Test_Error_Runtime_TabCaret_Preserved(t *testing.T) {
	ip, _ := NewInterpreter()
	// Two real tabs, then an out-of-bounds index to trigger a runtime error;
	// caret padding must preserve the tabs (no expansion).
	src := "\t\t[][0]"
	_, err := ip.EvalSource(src)
	if err == nil {
		t.Fatalf("expected runtime error, got nil")
	}
	msg := err.Error()

	mustHaveHeader(t, msg, "RUNTIME ERROR")
	mustContain(t, msg, "   1 | \t\t[][0]")
	mustContain(t, msg, "     | ")

	// Ensure the caret padding begins with two tabs (byte-accurate, tab-preserving)
	var caretLine string
	for _, ln := range strings.Split(msg, "\n") {
		if strings.HasPrefix(ln, "     | ") {
			caretLine = strings.TrimPrefix(ln, "     | ")
			break
		}
	}
	if caretLine == "" {
		t.Fatalf("missing caret line\n--- output ---\n%s", msg)
	}
	// Everything before '^'
	i := strings.Index(caretLine, "^")
	if i < 0 {
		t.Fatalf("no caret found\n--- output ---\n%s", msg)
	}
	prefix := caretLine[:i]
	if !strings.HasPrefix(prefix, "\t\t") {
		t.Fatalf("caret padding did not preserve leading tabs; got %q", prefix)
	}
}

func Test_Errors_Module_IfAssert_Typed_CondLine(t *testing.T) {
	// Repro from M1: inside a module, the failing caret for `if assert(x)` should
	// land on the line with the condition (line 3 here).
	src := `let M1 = module "M1" do
  let f = fun(x: Any) do
    if assert(x) then 1 else 0 end
  end
end
M1.f(0)`

	ip, _ := NewInterpreter()
	_, err := ip.EvalSource(src)
	if err == nil {
		t.Fatalf("expected runtime error, got nil")
	}
	msg := err.Error()

	mustHaveHeader(t, msg, "RUNTIME ERROR")
	mustRuntimeAtLine(t, msg, 3)
	mustContain(t, msg, "   3 |     if assert(x) then 1 else 0 end")
	mustContain(t, msg, "     | ")
	mustContain(t, msg, "^")
}

func Test_Errors_Module_IfAssert_Typed_CondLine_WithEarlierDef(t *testing.T) {
	// Repro from M2: same as above, but with an earlier definition in the module.
	src := `let M2 = module "M2" do
  let noop = fun() do 0 end

  let f = fun(x: Any) do
    if assert(x) then 1 else 0 end
  end
end
M2.f(0)`

	ip, _ := NewInterpreter()
	_, err := ip.EvalSource(src)
	if err == nil {
		t.Fatalf("expected runtime error, got nil")
	}
	msg := err.Error()

	mustHaveHeader(t, msg, "RUNTIME ERROR")
	// The failing caret still points to the `if assert(x)` line (line 5).
	mustRuntimeAtLine(t, msg, 5)
	mustContain(t, msg, "   5 |     if assert(x) then 1 else 0 end")
	mustContain(t, msg, "     | ")
	mustContain(t, msg, "^")
}

func Test_Errors_Module_CallArgMark_Typed_SecondArgLine(t *testing.T) {
	// Repro from M3 (typed): assert(true, x); with x=0 the *second* call fails
	// ("not a function"), caret should be on the assert-call line (line 5).
	src := `let M3 = module "M3" do
  let noop = fun() do 0 end

  let f = fun(x: Any) do
    assert(true, x)
  end
end
M3.f(0)`

	ip, _ := NewInterpreter()
	_, err := ip.EvalSource(src)
	if err == nil {
		t.Fatalf("expected runtime error, got nil")
	}
	msg := err.Error()

	mustHaveHeader(t, msg, "RUNTIME ERROR")
	mustRuntimeAtLine(t, msg, 5)
	// We also assert the specific failure mode to ensure we marked arg#2.
	mustContain(t, msg, "not a function")
	mustContain(t, msg, "   5 |     assert(true, x)")
	mustContain(t, msg, "     | ")
	mustContain(t, msg, "^")
}

func Test_Errors_Module_CallArgMark_Untyped_SecondArgLine(t *testing.T) {
	// Repro from M3P (untyped): same as above but without the explicit Any.
	// Ensures the synthetic/real type node unification doesn’t shift spans.
	src := `let M3P = module "M3P" do
    let noop = fun() do 0 end
    
    let f = fun(x) do
        assert(true, x)
    end
end
M3P.f(0)`

	ip, _ := NewInterpreter()
	_, err := ip.EvalSource(src)
	if err == nil {
		t.Fatalf("expected runtime error, got nil")
	}
	msg := err.Error()

	mustHaveHeader(t, msg, "RUNTIME ERROR")
	mustRuntimeAtLine(t, msg, 5)
	mustContain(t, msg, "not a function")
	mustContain(t, msg, "   5 |         assert(true, x)")
	mustContain(t, msg, "     | ")
	mustContain(t, msg, "^")
}

func countNodes(n S) int {
	if len(n) == 0 {
		return 0
	}
	c := 1 // count this node
	for i := 1; i < len(n); i++ {
		if ch, ok := n[i].(S); ok {
			c += countNodes(ch)
		}
	}
	return c
}

func Test_Errors_Oracle_From_NoDupSpans(t *testing.T) {
	src := `let o = oracle(x) -> Any from [(1 + 2)]`
	ast, idx, err := ParseSExprWithSpans(src)
	if err != nil {
		t.Fatalf("parse failed: %v", err)
	}
	if ast == nil || idx == nil {
		t.Fatalf("expected ast and spans")
	}
	nodes := countNodes(ast)
	spans := len(idx.byPath) // same package; access is fine
	if nodes != spans {
		t.Fatalf("oracle-with-FROM: nodes (%d) != spans (%d); possible duplicate placeholder span", nodes, spans)
	}
}

func Test_Errors_Oracle_NoFrom_SpansMatch(t *testing.T) {
	src := `let o = oracle(x) -> Any`
	ast, idx, err := ParseSExprWithSpans(src)
	if err != nil {
		t.Fatalf("parse failed: %v", err)
	}
	if ast == nil || idx == nil {
		t.Fatalf("expected ast and spans")
	}
	nodes := countNodes(ast)
	spans := len(idx.byPath)
	if nodes != spans {
		t.Fatalf("oracle-without-FROM: nodes (%d) != spans (%d)", nodes, spans)
	}
}
