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
