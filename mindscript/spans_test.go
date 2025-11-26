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
// After normalization: LHS is bare pattern; RHS value carries a zero-length annot pinned at value start.
func Test_Spans_PatternPreAnnotation(t *testing.T) {
	src := "let\n#p\n[x, y] = v"
	_, idx := mustParseWithSpansMS(t, src)

	// LHS of the assign is just the pattern (no annot wrapper).
	assertSpanTextMS(t, idx, NodePath{0, 0}, src, "[x, y]")

	// Inner decls "x" and "y".
	assertSpanTextMS(t, idx, NodePath{0, 0, 0}, src, "x")
	assertSpanTextMS(t, idx, NodePath{0, 0, 1}, src, "y")

	// RHS value node spans the value itself.
	assertSpanTextMS(t, idx, NodePath{0, 1}, src, "v")

	// Annot child is a synthetic zero-length span at the start of the value.
	as, ok := idx.Get(NodePath{0, 1, 0}) // annot "str"
	if !ok {
		t.Fatal("missing annot span")
	}
	vs, ok := idx.Get(NodePath{0, 1, 1}) // value leaf "v"
	if !ok {
		t.Fatal("missing value span")
	}
	if as.StartByte != vs.StartByte || as.EndByte != as.StartByte {
		t.Fatalf("want zero-length annot at value start, got annot=%+v value=%+v", as, vs)
	}

	// Wrapped value leaf (under annot) is still "v".
	assertSpanTextMS(t, idx, NodePath{0, 1, 1}, src, "v")
}

// {
// #A
// k: 1
// }
// After normalization: key is bare "k"; value carries a zero-length annot pinned at value start.
// The pair's span starts at the pre-annotation.
func Test_Spans_KeyPreAnnotation(t *testing.T) {
	src := "{\n#A\nk: 1\n}"
	_, idx := mustParseWithSpansMS(t, src)

	// Whole map unchanged.
	assertSpanTextMS(t, idx, NodePath{0}, src, "{\n#A\nk: 1\n}")

	// Pair spans from the annotation through the value.
	assertSpanTextMS(t, idx, NodePath{0, 0}, src, "#A\nk: 1")

	// Key is just "k" (no annot wrapper on the key).
	assertSpanTextMS(t, idx, NodePath{0, 0, 0}, src, "k")

	// Value node span is the literal.
	assertSpanTextMS(t, idx, NodePath{0, 0, 1}, src, "1")

	// Annot child is a synthetic zero-length span at the start of the value.
	as, ok := idx.Get(NodePath{0, 0, 1, 0}) // annot "str"
	if !ok {
		t.Fatal("missing annot span")
	}
	vs, ok := idx.Get(NodePath{0, 0, 1, 1}) // value leaf "1"
	if !ok {
		t.Fatal("missing value span")
	}
	if as.StartByte != vs.StartByte || as.EndByte != as.StartByte {
		t.Fatalf("want zero-length annot at value start, got annot=%+v value=%+v", as, vs)
	}

	// Wrapped value leaf (under annot) is still "1".
	assertSpanTextMS(t, idx, NodePath{0, 0, 1, 1}, src, "1")
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

func Test_Spans_Assign_AB_AnchorsToB(t *testing.T) {
	// A before target, B after '=', then value
	src := "#A\nlet x =\n#B\n42"
	ast, idx := mustParseWithSpansMS(t, src)

	// Span of the annot child is zero-length at the value start → empty slice.
	assertSpanTextMS(t, idx, NodePath{0, 1, 0}, src, "")

	// But the annot node's own text must remain (A then B).
	// AST path: ("block" <0>, ("assign" <1>, ("decl" "x") <1>, ("annot" <2>, ("str" TEXT) <1>, ("int" 42) <2>)))
	annotStr := ast[1].(S)[2].(S)[1].(S) // block→assign→rhs(annot)→child0(str)
	if got := annotStr[1].(string); got != "A\nB" {
		t.Fatalf("annot text mismatch: got %q want %q", got, "A\nB")
	}
}

func Test_Spans_MapValue_BD_AnchorsToD(t *testing.T) {
	// Map with B after colon, D after value (same-line ok)
	src := "{ k: \n#B\n1 #D\n }"
	ast, idx := mustParseWithSpansMS(t, src)

	// Span of the annot child is zero-length at the value start → empty slice.
	assertSpanTextMS(t, idx, NodePath{0, 0, 1, 0}, src, "")

	// But the annot node's own text must remain (B then D).
	// AST path: ("block" <0>, ("map" <1>, ("pair" <1>, ("str" "k") <1>, ("annot" <2>, ("str" TEXT) <1>, ("int" 1) <2>))))
	annotStr := ast[1].(S)[1].(S)[2].(S)[1].(S) // block→map→pair→value(annot)→child0(str)
	if got := annotStr[1].(string); got != "B\nD" {
		t.Fatalf("annot text mismatch: got %q want %q", got, "B\nD")
	}
}
