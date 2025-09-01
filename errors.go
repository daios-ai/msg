// errors.go: user-facing error wrapping and caret-snippet rendering
//
// What this file does
// -------------------
// This module turns low-level lexer/parser diagnostics into readable,
// Python-style error snippets with a caret pointing at the offending column.
// The primary entry point is `WrapErrorWithSource`, which recognizes
// `*LexError` (from lexer.go) and `*ParseError` (from parser.go), formats
// them, and returns a new `error` that contains a multi-line snippet:
//
//	PARSE ERROR at 3:12: unexpected token ')'
//
//	   2 | let x = (1 + 2
//	   3 |              )
//	       |            ^
//	   4 | end
//
// The snippet includes up to one line of context before and after the error,
// numbers the lines, and places a caret under the 1-based column.
//
// Dependencies (other files)
// --------------------------
//   - lexer.go: defines `*LexError { Line, Col, Msg }`, produced by the lexer.
//   - parser.go: defines `*ParseError { Line, Col, Msg }`, produced by the parser.
//     Both carry 1-based Line and Col coordinates. `WrapErrorWithSource` depends
//     on those types to detect when it should render a caret snippet.
//
// Scope of the public API
// -----------------------
// Public:   `WrapErrorWithSource(err error, src string) error`
// Private:  caret-snippet renderer and tiny helpers.
//
// Behavior guarantees
// -------------------
//   - If `err` is a `*LexError` or `*ParseError`, the returned error’s message
//     is a fully formatted, plain-text snippet (no ANSI colors).
//   - If `err` is anything else, it is returned unchanged.
//   - Line/column are treated as 1-based. If out of range, they are clamped so
//     the caret can be rendered safely. Empty/short source strings are handled.
//
// This utility is intentionally independent of the interpreter/VM. It can be
// used anywhere you want to surface lex/parse errors with helpful context.
package mindscript

import (
	"fmt"
	"strings"
)

/* ===========================
   PUBLIC API
   =========================== */

// WrapErrorWithSource returns an error augmented with a caret-annotated snippet
// of the provided source. It recognizes lexer/parser errors and leaves other
// errors untouched.
//
// Inputs
//   - err: The original error. If it is a *LexError (lexer.go) or *ParseError
//     (parser.go), the function renders a multi-line snippet describing the
//     error location and message. Otherwise, `err` is returned as-is.
//   - src: The full source text that was being lexed/parsed.
//
// Output
//   - error: If `err` was a lex/parse error, a new error whose .Error() string
//     is a human-readable snippet with:
//   - a header: "LEXICAL ERROR" or "PARSE ERROR"
//   - the 1-based line/column and the original message
//   - up to one previous and one next line of context
//   - a caret aligned under the 1-based column
//     For all other error kinds, the original `err` is returned.
//
// Notes
//   - Line/column bounds are clamped to the source; missing lines/columns do
//     not crash rendering.
//   - Output is plain text (no ANSI escapes), suitable for logs and terminals.
func WrapErrorWithSource(err error, src string) error {
	// Fall back to a name-less header (won’t show "in <src>").
	return WrapErrorWithName(err, "", src)
}

func WrapErrorWithName(err error, srcName string, src string) error {
	switch e := err.(type) {
	case *LexError:
		// Lex/parse Col are 0-based; render as 1-based.
		return fmt.Errorf("%s", prettyErrorStringLabeled(src, "LEXICAL ERROR", srcName, e.Line, e.Col+1, e.Msg))
	case *ParseError:
		return fmt.Errorf("%s", prettyErrorStringLabeled(src, "PARSE ERROR", srcName, e.Line, e.Col+1, e.Msg))
	case *RuntimeError:
		// RuntimeError is already 1-based.
		return fmt.Errorf("%s", prettyErrorStringLabeled(src, "RUNTIME ERROR", srcName, e.Line, e.Col, e.Msg))
	default:
		return err
	}
}

//// END_OF_PUBLIC

/* ===========================
   PRIVATE: helpers & rendering
   =========================== */

// max returns the greater of a and b (used for caret padding).
func max(a, b int) int {
	if a > b {
		return a
	}
	return b
}

// prettyErrorString builds a Python-like snippet with a header and a caret.
// It shows at most one previous and one next line when available.
// Coordinates are treated as 1-based and clamped to the source bounds.
func prettyErrorStringLabeled(src, header, name string, line, col int, msg string) string {
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
	if name != "" {
		fmt.Fprintf(&b, "%s in %s at %d:%d: %s\n\n", header, name, line, col, msg)
	} else {
		fmt.Fprintf(&b, "%s at %d:%d: %s\n\n", header, line, col, msg) // legacy
	}
	if line > 1 {
		fmt.Fprintf(&b, "%4d | %s\n", line-1, lines[line-2])
	}
	fmt.Fprintf(&b, "%4d | %s\n", line, lineTxt)
	caretPad := col - 1
	if caretPad < 0 {
		caretPad = 0
	}
	fmt.Fprintf(&b, "     | %s^\n", strings.Repeat(" ", caretPad))
	if line < len(lines) {
		fmt.Fprintf(&b, "%4d | %s\n", line+1, lines[line])
	}
	return b.String()
}
