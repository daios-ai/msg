// errors.go
package mindscript

import (
	"fmt"
	"strings"
)

// max is handy for caret padding
func max(a, b int) int {
	if a > b {
		return a
	}
	return b
}

// prettyErrorString builds a Python-like snippet with a red header and a red caret.
// It shows up to one previous and one next line if available.
func prettyErrorString(src, header string, line, col int, msg string) string {
	lines := strings.Split(src, "\n")
	if line < 1 {
		line = 1
	}
	if col < 1 {
		col = 1
	}
	if len(lines) == 0 {
		lines = []string{""}
	}
	if line > len(lines) {
		line = len(lines)
	}

	lineTxt := lines[line-1]

	var b strings.Builder
	// Plain header, no ANSI
	fmt.Fprintf(&b, "%s at %d:%d: %s\n\n", header, line, col, msg)

	// Optional previous line
	if line > 1 {
		fmt.Fprintf(&b, "%4d | %s\n", line-1, lines[line-2])
	}
	// Error line
	fmt.Fprintf(&b, "%4d | %s\n", line, lineTxt)

	// Caret (plain)
	caretPad := max(0, col-1)
	caret := strings.Repeat(" ", caretPad) + "^"
	fmt.Fprintf(&b, "     | %s\n", caret)

	// Optional next line
	if line < len(lines) {
		fmt.Fprintf(&b, "%4d | %s\n", line+1, lines[line])
	}
	return b.String()
}

// WrapErrorWithSource returns a new error that includes a caret snippet.
// If err is neither *LexError nor *ParseError, returns err unchanged.
func WrapErrorWithSource(err error, src string) error {
	switch e := err.(type) {
	case *LexError:
		return fmt.Errorf("%s", prettyErrorString(src, "LEXICAL ERROR", e.Line, e.Col, e.Msg))
	case *ParseError:
		return fmt.Errorf("%s", prettyErrorString(src, "PARSE ERROR", e.Line, e.Col, e.Msg))
	default:
		return err
	}
}
