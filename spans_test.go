// more_spans_test.go
package mindscript

import "testing"

// local helpers with unique names to avoid collisions
func mustParseWithSpansMS(t *testing.T, src string) (S, *SpanIndex) {
	t.Helper()
	ast, idx, err := ParseSExprWithSpans(src)
	if err != nil {
		t.Fatalf("parse error: %v", err)
	}
	if idx == nil {
		t.Fatalf("nil SpanIndex")
	}
	_ = ast
	return ast, idx
}
func sliceMS(src string, sp Span) string {
	if sp.StartByte < 0 || sp.EndByte < 0 || sp.EndByte > len(src) || sp.StartByte > sp.EndByte {
		return ""
	}
	return src[sp.StartByte:sp.EndByte]
}
func assertSpanTextMS(t *testing.T, idx *SpanIndex, path NodePath, src, want string) {
	t.Helper()
	got, ok := idx.Get(path)
	if !ok {
		t.Fatalf("missing span for path %v", path)
	}
	text := sliceMS(src, got)
	if text != want {
		t.Fatalf("span text mismatch at path %v:\n  got : %q\n  want: %q\n  span: %+v", path, text, want, got)
	}
}

// let {a: x, b: [y]} = rhs
// Covers: object pattern ("dobj"), inner ("pair")s, nested array pattern ("darr"), and decl spans.
// NOTE: destructuring let requires an assignment; the pattern is the LHS of ("assign", ...).
func Test_Spans_DeclPatterns(t *testing.T) {
	src := "let {a: x, b: [y]} = rhs"
	_, idx := mustParseWithSpansMS(t, src)

	// Root is ("assign" ...); the pattern is the LHS at path {0,0}
	assertSpanTextMS(t, idx, NodePath{0, 0}, src, "{a: x, b: [y]}")

	// First pair "a: x"
	assertSpanTextMS(t, idx, NodePath{0, 0, 0}, src, "a: x")
	//   key "a"
	assertSpanTextMS(t, idx, NodePath{0, 0, 0, 0}, src, "a")
	//   decl "x"
	assertSpanTextMS(t, idx, NodePath{0, 0, 0, 1}, src, "x")

	// Second pair "b: [y]"
	assertSpanTextMS(t, idx, NodePath{0, 0, 1}, src, "b: [y]")
	//   key "b"
	assertSpanTextMS(t, idx, NodePath{0, 0, 1, 0}, src, "b")
	//   darr "[y]"
	assertSpanTextMS(t, idx, NodePath{0, 0, 1, 1}, src, "[y]")
	//     inner decl "y"
	assertSpanTextMS(t, idx, NodePath{0, 0, 1, 1, 0}, src, "y")
}

// let
// #p
// [x, y] = v
// Covers: PRE-annotation wrapper in pattern contexts.
// NOTE: PRE-annotation must be on its own line, otherwise it captures the rest of that line.
func Test_Spans_PatternPreAnnotation(t *testing.T) {
	src := "let\n#p\n[x, y] = v"
	_, idx := mustParseWithSpansMS(t, src)

	// LHS of the assign is the annotated pattern
	assertSpanTextMS(t, idx, NodePath{0, 0}, src, "#p\n[x, y]")

	//   child ("str","p") — the annotation token slice
	assertSpanTextMS(t, idx, NodePath{0, 0, 0}, src, "#p")

	//   wrapped pattern "[x, y]"
	assertSpanTextMS(t, idx, NodePath{0, 0, 1}, src, "[x, y]")

	//     inner decls "x" and "y"
	assertSpanTextMS(t, idx, NodePath{0, 0, 1, 0}, src, "x")
	assertSpanTextMS(t, idx, NodePath{0, 0, 1, 1}, src, "y")
}

// {
// #A
// k: 1
// }
// Covers: PRE-annotation in key position (readKeyString recursion): ("annot", ("str","#A"), ("str","k"))
// NOTE: annotation on its own line so it doesn't swallow the key.
func Test_Spans_KeyPreAnnotation(t *testing.T) {
	src := "{\n#A\nk: 1\n}"
	_, idx := mustParseWithSpansMS(t, src)

	// Whole map
	assertSpanTextMS(t, idx, NodePath{0}, src, "{\n#A\nk: 1\n}")

	// The only pair spans from annotation through value
	assertSpanTextMS(t, idx, NodePath{0, 0}, src, "#A\nk: 1")

	//   key is an ("annot", ("str","#A"), ("str","k")) → spans "#A\nk"
	assertSpanTextMS(t, idx, NodePath{0, 0, 0}, src, "#A\nk")
	//     child annotation text ("str","#A")
	assertSpanTextMS(t, idx, NodePath{0, 0, 0, 0}, src, "#A")
	//     base key ("str","k")
	assertSpanTextMS(t, idx, NodePath{0, 0, 0, 1}, src, "k")

	//   value "1"
	assertSpanTextMS(t, idx, NodePath{0, 0, 1}, src, "1")
}
