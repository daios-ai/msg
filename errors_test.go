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

func mustRuntimeAtLine(t *testing.T, msg string, line int) {
	t.Helper()
	want := "RUNTIME ERROR at " + strconv.Itoa(line) + ":"
	if !strings.Contains(msg, want) {
		t.Fatalf("expected runtime error to report line %d\n--- output ---\n%s", line, msg)
	}
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

	// Header
	mustContain(t, msg, "PARSE ERROR at")
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

	// Header
	mustContain(t, msg, "LEXICAL ERROR at")
	// Context lines
	mustContain(t, msg, "   1 | let ok = 1")
	mustContain(t, msg, "   2 | \"bad \\u12GZ\"")
	// Caret line
	mustContain(t, msg, "     | ")
	mustContain(t, msg, "^")

	// A bit of the lexer message to be sure it's the unicode escape path
	// (exact wording can vary; loosen as needed)
	// e.g. "unicode escape was not terminated" or "invalid unicode escape"
	if !(strings.Contains(strings.ToLower(msg), "unicode") || strings.Contains(strings.ToLower(msg), "escape")) {
		t.Fatalf("expected unicode escape related message, got:\n%s", msg)
	}
}

func Test_Errors_Binop_DivZero_RHSLine(t *testing.T) {
	// RHS is on line 2; caret should point there (we mark at RHS for the op).
	src := "10 /\n 0"
	ip := NewInterpreter()
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
	ip := NewInterpreter()
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
	ip := NewInterpreter()
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
	ip := NewInterpreter()
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
	ip := NewInterpreter()
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
	ip := NewInterpreter()
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
	ip := NewInterpreter()
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

	ip := NewInterpreter()
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

	ip := NewInterpreter()
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

	ip := NewInterpreter()
	_, err := ip.EvalSource(src)
	if err == nil {
		t.Fatalf("expected runtime error, got nil")
	}
	msg := err.Error()

	// Error currently blames the 'for x in' header (line 5), not the iter expr line.
	mustRuntimeAtLine(t, msg, 5)
	mustContain(t, msg, "BOOM")
	mustContain(t, msg, "   5 | for x in")
	mustContain(t, msg, "   6 |   it")
	mustContain(t, msg, "     | ")
	mustContain(t, msg, "^")
}

func Test_Errors_ArrayIndex_NegativeWrap_NoError(t *testing.T) {
	src := `let a = [10, 20, 30]
a[-1]`

	ip := NewInterpreter()
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

	ip := NewInterpreter()
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
			ip := NewInterpreter()
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
	ip := NewInterpreter()
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
	ip := NewInterpreter()
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
	ip := NewInterpreter()
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
	ip := NewInterpreter()
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
	ip := NewInterpreter()
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
