// spans_test.go
package mindscript

import "testing"

// Test_Span_PostOrderBinding verifies that BuildSpanIndexPostOrder binds spans
// to NodePaths using a deterministic post-order walk (children before parent).
func Test_Span_PostOrderBinding(t *testing.T) {
	// AST:
	// ("block",
	//   ("call",
	//     ("id","f"),
	//     ("int",1),
	//     ("binop","+",
	//       ("int",2),
	//       ("int",3))))
	ast := S{
		"block",
		S{
			"call",
			S{"id", "f"},       // path {0,0}
			S{"int", int64(1)}, // path {0,1}
			S{"binop", "+", // path {0,2}
				S{"int", int64(2)}, // path {0,2,0}
				S{"int", int64(3)}, // path {0,2,1}
			},
		}, // path {0}
	}

	// Spans in post-order for the tree above:
	// {0,0}, {0,1}, {0,2,0}, {0,2,1}, {0,2}, {0}, []
	post := []Span{
		{1, 2}, // callee "f"         → {0,0}
		{3, 4}, // int(1)             → {0,1}
		{5, 6}, // int(2)             → {0,2,0}
		{7, 8}, // int(3)             → {0,2,1}
		{5, 8}, // (2 + 3)            → {0,2}
		{0, 9}, // call f(1, 2+3)     → {0}
		{0, 9}, // block               → []
	}

	idx := BuildSpanIndexPostOrder(ast, post)

	check := func(path NodePath, want Span) {
		got, ok := idx.Get(path)
		if !ok {
			t.Fatalf("missing span for path %v", path)
		}
		if got != want {
			t.Fatalf("span mismatch for path %v: got=%+v want=%+v", path, got, want)
		}
	}

	check(NodePath{0, 0}, Span{1, 2})
	check(NodePath{0, 1}, Span{3, 4})
	check(NodePath{0, 2, 1}, Span{5, 6}) // left operand of binop
	check(NodePath{0, 2, 2}, Span{7, 8}) // right operand of binop
	check(NodePath{0, 2}, Span{5, 8})
	check(NodePath{0}, Span{0, 9})
	check(NodePath{}, Span{0, 9})
}
