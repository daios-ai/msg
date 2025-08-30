// parser_spans_test.go
package mindscript

import "testing"

func mustParseWithSpans(t *testing.T, src string) (S, *SpanIndex) {
	t.Helper()
	ast, idx, err := ParseSExprWithSpans(src)
	if err != nil {
		t.Fatalf("parse error: %v", err)
	}
	if idx == nil {
		t.Fatalf("nil SpanIndex")
	}
	return ast, idx
}

func slice(src string, sp Span) string {
	if sp.StartByte < 0 || sp.EndByte < 0 || sp.EndByte > len(src) || sp.StartByte > sp.EndByte {
		return ""
	}
	return src[sp.StartByte:sp.EndByte]
}

func assertSpanText(t *testing.T, idx *SpanIndex, path NodePath, src, want string) {
	t.Helper()
	got, ok := idx.Get(path)
	if !ok {
		t.Fatalf("missing span for path %v", path)
	}
	text := slice(src, got)
	if text != want {
		t.Fatalf("span text mismatch at path %v:\n  got : %q\n  want: %q\n  span: %+v", path, text, want, got)
	}
}

// a.b.c  →  ("get", ("get", ("id","a"), ("str","b")), ("str","c"))
func Test_Spans_GetChain(t *testing.T) {
	src := "a.b.c"
	_, idx := mustParseWithSpans(t, src)

	// Root block should span whole input
	assertSpanText(t, idx, NodePath{}, src, "a.b.c")

	// Outer get
	assertSpanText(t, idx, NodePath{0}, src, "a.b.c")
	// Inner get
	assertSpanText(t, idx, NodePath{0, 0}, src, "a.b")
	// Base id "a"
	assertSpanText(t, idx, NodePath{0, 0, 0}, src, "a")
	// Property "b"
	assertSpanText(t, idx, NodePath{0, 0, 1}, src, "b")
	// Property "c"
	assertSpanText(t, idx, NodePath{0, 1}, src, "c")
}

// arr[i].x  →  ("get", ("idx", ("id","arr"), ("id","i")), ("str","x"))
func Test_Spans_IdxChain(t *testing.T) {
	src := "arr[i].x"
	_, idx := mustParseWithSpans(t, src)

	// Outer get spans entire expression
	assertSpanText(t, idx, NodePath{0}, src, "arr[i].x")
	// The idx node spans "arr[i]"
	assertSpanText(t, idx, NodePath{0, 0}, src, "arr[i]")
	// Base "arr"
	assertSpanText(t, idx, NodePath{0, 0, 0}, src, "arr")
	// Index "i"
	assertSpanText(t, idx, NodePath{0, 0, 1}, src, "i")
	// Property "x"
	assertSpanText(t, idx, NodePath{0, 1}, src, "x")
}

// obj.(x + y).z  →  ("get", ("idx", ("id","obj"), ("binop","+...", ...)), ("str","z"))
func Test_Spans_ComputedIndexGrouping(t *testing.T) {
	src := "obj.(x + y).z"
	_, idx := mustParseWithSpans(t, src)

	// Outer get spans entire expression
	assertSpanText(t, idx, NodePath{0}, src, "obj.(x + y).z")
	// idx spans the object plus grouped index (including parentheses)
	assertSpanText(t, idx, NodePath{0, 0}, src, "obj.(x + y)")
	// Base object id
	assertSpanText(t, idx, NodePath{0, 0, 0}, src, "obj")
	// Inner binop "x + y"
	assertSpanText(t, idx, NodePath{0, 0, 1}, src, "x + y")
	// Left/right operands
	assertSpanText(t, idx, NodePath{0, 0, 1, 1}, src, "x") // note: binop's first S-child is at index 1
	assertSpanText(t, idx, NodePath{0, 0, 1, 2}, src, "y")
	// Property "z"
	assertSpanText(t, idx, NodePath{0, 1}, src, "z")
}

// obj."then"  →  ("get", ("id","obj"), ("str","then"))
// After '.' a quoted string is tokenized as ID (with Lexeme including quotes);
// we still require the ("str", ...) child to have a span that covers the quoted text.
func Test_Spans_StringProperty(t *testing.T) {
	src := `obj."then"`
	_, idx := mustParseWithSpans(t, src)

	// Whole get
	assertSpanText(t, idx, NodePath{0}, src, `obj."then"`)
	// Base id
	assertSpanText(t, idx, NodePath{0, 0}, src, "obj")
	// Property child ("str","then") must span the quoted token, including quotes
	assertSpanText(t, idx, NodePath{0, 1}, src, `"then"`)
}

// a.12  →  ("idx", ("id","a"), ("int",12))
func Test_Spans_NumericIndexDot(t *testing.T) {
	src := "a.12"
	_, idx := mustParseWithSpans(t, src)

	// The numeric idx spans entire expression
	assertSpanText(t, idx, NodePath{0}, src, "a.12")
	// Base id "a"
	assertSpanText(t, idx, NodePath{0, 0}, src, "a")
	// Numeric index "12"
	assertSpanText(t, idx, NodePath{0, 1}, src, "12")
}
