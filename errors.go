// errors.go: unified diagnostics and pretty-print rendering (public-facing)
//
// Overview
// --------
// MindScript distinguishes between:
//   • SOFT errors (operational) → represented as annotated-null Values (not here).
//   • HARD errors (contractual) → represented by *Error and bubbled to the API.
//
// This module defines the single diagnostic type (*Error) and a pretty-printer
// used ONLY by public entry points. Engine internals should construct *Error
// (with correct src/line/col) and return it upward WITHOUT formatting.

package mindscript

import (
	"fmt"
	"strings"
)

/* ===========================
   PUBLIC TYPES & HELPERS
   =========================== */

// DiagKind classifies a hard diagnostic.
type DiagKind int

const (
	DiagLex DiagKind = iota
	DiagParse
	DiagRuntime
	DiagIncomplete // REPL “need more input” (hard signal to prompt for more)
)

// Error is the single hard diagnostic that bubbles up to public APIs.
// Msg is plain text (no snippets); pretty-printing happens at the API boundary.
type Error struct {
	Kind DiagKind
	Msg  string
	Src  *SourceRef // preferred source for rendering (<main>, <repl>, module)
	Line int        // 1-based
	Col  int        // 1-based
}

// Error implements the error interface, returning ONLY the message.
// Pretty snippets are produced by FormatError at the API boundary.
func (e *Error) Error() string { return e.Msg }

// IsIncomplete reports whether err represents an incomplete-input condition.
func IsIncomplete(err error) bool {
	if e, ok := err.(*Error); ok {
		return e.Kind == DiagIncomplete
	}
	return false
}

// FormatError renders a human-friendly, caret-annotated snippet for a *Error.
// Call this ONLY at public entry points (Eval*/Parse*), never in engine internals.
func FormatError(e *Error) string {
	if e == nil {
		return ""
	}
	header := diagHeader(e.Kind)

	var name, src string
	if e.Src != nil {
		name = e.Src.Name
		src = e.Src.Src
	}
	return prettyErrorStringLabeled(src, header, name, e.Line, e.Col, e.Msg)
}

/* ===========================
   PRIVATE: rendering helpers
   =========================== */

func diagHeader(k DiagKind) string {
	switch k {
	case DiagLex:
		return "LEXICAL ERROR"
	case DiagParse:
		return "PARSE ERROR"
	case DiagRuntime:
		return "RUNTIME ERROR"
	case DiagIncomplete:
		return "INCOMPLETE"
	default:
		return "ERROR"
	}
}

// prettyErrorStringLabeled builds a Python-like snippet with a header and caret.
// It shows at most one previous and one next line when available.
// Coordinates are treated as 1-based and clamped to the source bounds.
func prettyErrorStringLabeled(src, header, name string, line, col int, msg string) string {
	lines := strings.Split(src, "\n")
	if len(lines) == 0 {
		lines = []string{""}
	}
	if line < 1 {
		line = 1
	}
	if line > len(lines) {
		line = len(lines)
	}
	if col < 1 {
		col = 1
	}

	lineTxt := lines[line-1]

	var b strings.Builder
	if name != "" {
		fmt.Fprintf(&b, "%s in %s at %d:%d: %s\n\n", header, name, line, col, msg)
	} else {
		fmt.Fprintf(&b, "%s at %d:%d: %s\n\n", header, line, col, msg)
	}
	if line > 1 {
		fmt.Fprintf(&b, "%4d | %s\n", line-1, lines[line-2])
	}
	fmt.Fprintf(&b, "%4d | %s\n", line, lineTxt)

	// Clamp caret padding to the line length (defensive).
	caretPad := col - 1
	if caretPad < 0 {
		caretPad = 0
	}
	if caretPad > len(lineTxt) {
		caretPad = len(lineTxt)
	}
	fmt.Fprintf(&b, "     | %s^\n", strings.Repeat(" ", caretPad))

	if line < len(lines) {
		fmt.Fprintf(&b, "%4d | %s\n", line+1, lines[line])
	}
	return b.String()
}
