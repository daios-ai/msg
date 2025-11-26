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
// Under the new annotation model, #p decorates the pattern [x, y] itself.
func Test_Spans_PatternPreAnnotation(t *testing.T) {
	src := "let\n#p\n[x, y] = v"
	ast, idx := mustParseWithSpansMS(t, src)

	// Root is ("block", <assign>).
	assign := ast[1].(S)
	if tag := assign[0].(string); tag != "assign" {
		t.Fatalf("root expr tag = %q, want %q", tag, "assign")
	}

	// LHS is an annot wrapping the pattern [x, y].
	lhs := assign[1].(S)
	if tag := lhs[0].(string); tag != "annot" {
		t.Fatalf("lhs tag = %q, want %q", tag, "annot")
	}

	// Annotation string spans the source "#p".
	assertSpanTextMS(t, idx, NodePath{0, 0, 0}, src, "#p")

	// The wrapped pattern and its inner decls retain their spans.
	assertSpanTextMS(t, idx, NodePath{0, 0, 1}, src, "[x, y]")
	assertSpanTextMS(t, idx, NodePath{0, 0, 1, 0}, src, "x")
	assertSpanTextMS(t, idx, NodePath{0, 0, 1, 1}, src, "y")

	// RHS value node spans the value itself.
	assertSpanTextMS(t, idx, NodePath{0, 1}, src, "v")
}

// {
// #A
// k: 1
// }
// After the new annotation model: key is bare "k"; value expression is "1",
// wrapped by an annot whose string span covers "#A".
func Test_Spans_KeyPreAnnotation(t *testing.T) {
	src := "{\n#A\nk: 1\n}"
	_, idx := mustParseWithSpansMS(t, src)

	// Whole map unchanged.
	assertSpanTextMS(t, idx, NodePath{0}, src, "{\n#A\nk: 1\n}")

	// Pair spans from the annotation through the value.
	assertSpanTextMS(t, idx, NodePath{0, 0}, src, "#A\nk: 1")

	// Key is just "k" (no annot wrapper on the key).
	assertSpanTextMS(t, idx, NodePath{0, 0, 0}, src, "k")

	// Value expression span is the literal; annotation wraps it.
	assertSpanTextMS(t, idx, NodePath{0, 0, 1}, src, "1")

	// The inner annot string now spans the source "#A".
	assertSpanTextMS(t, idx, NodePath{0, 0, 1, 0}, src, "#A")
}

func Test_Spans_Module_Whole_And_Parts(t *testing.T) {
	src := `module "M" do let x = 1 end`
	_, idx := mustParseWithSpansMS(t, src)

	// The module node (root child) covers the whole construct
	assertSpanTextMS(t, idx, NodePath{0}, src, `module "M" do let x = 1 end`)

	// Name expression is the string literal (including quotes)
	assertSpanTextMS(t, idx, NodePath{0, 0}, src, `"M"`)

	// Body block span covers only the contents between 'do' and 'end'
	assertSpanTextMS(t, idx, NodePath{0, 1}, src, `let x = 1`)
}

func Test_Spans_Module_NameExpression_And_EmptyBody(t *testing.T) {
	// Note: grouping parens belong to the *parent* construct span, not the child.
	// So the name expression span excludes '(' and ')', mirroring computed props.
	src := `module ("M" + "1") do end`
	_, idx := mustParseWithSpansMS(t, src)

	// Name expression (without parentheses)
	assertSpanTextMS(t, idx, NodePath{0, 0}, src, `"M" + "1"`)

	// Empty body block has an empty span
	assertSpanTextMS(t, idx, NodePath{0, 1}, src, ``)

	// Whole module span is the full construct (including parens around the name)
	assertSpanTextMS(t, idx, NodePath{0}, src, `module ("M" + "1") do end`)
}

func Test_Spans_PropertyAfterDot_ModuleKeyword(t *testing.T) {
	src := `obj.module`
	_, idx := mustParseWithSpansMS(t, src)

	// ("get" obj "module") spans the entire chain
	assertSpanTextMS(t, idx, NodePath{0}, src, `obj.module`)

	//   object id
	assertSpanTextMS(t, idx, NodePath{0, 0}, src, `obj`)
	//   property name ("str") — taken from the ID token after '.'
	assertSpanTextMS(t, idx, NodePath{0, 1}, src, `module`)
}
