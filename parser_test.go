// parser_test.go
package mindscript

import (
	"encoding/json"
	"strings"
	"testing"
)

// --- helpers ---------------------------------------------------------------

func mustParse(t *testing.T, src string) S {
	t.Helper()
	sexpr, err := ParseSExpr(src)
	if err != nil {
		t.Fatalf("Parse error: %v\nsource:\n%s", err, src)
	}
	return sexpr
}

func wantTag(t *testing.T, n S, tag string) {
	t.Helper()
	if len(n) == 0 {
		t.Fatalf("empty node, want tag %q", tag)
	}
	if got := n[0].(string); got != tag {
		b, _ := json.MarshalIndent(n, "", "  ")
		t.Fatalf("want tag %q, got %q\nnode:\n%s", tag, got, string(b))
	}
}

// kids usually start at index 1, e.g. ["block", child1, child2, ...],
// but NOT for nodes with an operator payload:
//
//	["binop", OP, LHS, RHS] and ["unop", OP, EXPR]
//
// For those, index into the slice directly.
func kid(n S, i int) S { return n[i+1].(S) } // children start at index 1 (L("tag", kids...))

func head(n S) string { return n[0].(string) }

// pretty for failures
func dump(n S) string {
	b, _ := json.MarshalIndent(n, "", "  ")
	return string(b)
}

// --- tests -----------------------------------------------------------------

func Test_Parser_Literals_And_Id(t *testing.T) {
	src := `42 0.5 "hi" true false null x`
	root := mustParse(t, src)
	wantTag(t, root, "block")
	children := root[1:]
	tags := []string{"int", "num", "str", "bool", "bool", "null", "id"}
	if len(children) != len(tags) {
		t.Fatalf("want %d children, got %d\n%s", len(tags), len(children), dump(root))
	}
	for i, tag := range tags {
		wantTag(t, children[i].(S), tag)
	}
	if children[0].(S)[1].(int64) != 42 {
		t.Fatalf("int literal mismatch: %v", children[0])
	}
	if children[1].(S)[1].(float64) != 0.5 {
		t.Fatalf("num literal mismatch: %v", children[1])
	}
	if children[2].(S)[1].(string) != "hi" {
		t.Fatalf("str literal mismatch: %v", children[2])
	}
}

func Test_Parser_Unary_And_Binary_Precedence(t *testing.T) {
	// 1 + 2 * 3  ==>  (+ 1 (* 2 3))
	root := mustParse(t, `1 + 2 * 3`)
	expr := kid(root, 0)
	wantTag(t, expr, "binop")
	if expr[1].(string) != "+" {
		t.Fatalf("want '+', got %v", expr[1])
	}
	rhs := kid(expr, 2)
	wantTag(t, rhs, "binop")
	if rhs[1].(string) != "*" {
		t.Fatalf("want '*', got %v", rhs[1])
	}

	// -(a + b)
	root2 := mustParse(t, `- (a + b)`)
	expr2 := kid(root2, 0)
	wantTag(t, expr2, "unop")
	if expr2[1].(string) != "-" {
		t.Fatalf("want unary '-', got %v", expr2[1])
	}
}

func Test_Parser_Assignment_RightAssociative(t *testing.T) {
	root := mustParse(t, `a = b = 1`)
	assign1 := kid(root, 0)
	wantTag(t, assign1, "assign")
	assign2 := kid(assign1, 1)
	wantTag(t, assign2, "assign")
}

func Test_Parser_Comparison_And_Equality_Binding(t *testing.T) {
	// a < b == c  ==> (== (< a b) c)
	root := mustParse(t, `a < b == c`)
	eq := kid(root, 0)
	wantTag(t, eq, "binop")
	if eq[1].(string) != "==" {
		t.Fatalf("want '==', got %v", eq[1])
	}
	cmp := kid(eq, 1)
	wantTag(t, cmp, "binop")
	if cmp[1].(string) != "<" {
		t.Fatalf("want '<', got %v", cmp[1])
	}
}

func Test_Parser_Call_Index_Get_Chaining(t *testing.T) {
	src := `obj.name(1, 2)[i]."weird"`
	root := mustParse(t, src)
	e := kid(root, 0)
	// (((get (idx (call (get obj "name") 1 2) i) "weird")))
	wantTag(t, e, "get")
	wantTag(t, kid(e, 0), "idx")
	in1 := kid(e, 0)
	wantTag(t, kid(in1, 0), "call")
}

func Test_Parser_Grouping_Affects_Precedence(t *testing.T) {
	// (1 + 2) * 3  ==>  (* (+ 1 2) 3)
	root := mustParse(t, `(1 + 2) * 3`)
	top := kid(root, 0) // first child in the block
	wantTag(t, top, "binop")
	if top[1].(string) != "*" {
		t.Fatalf("want '*', got %v", top[1])
	}
	// binop layout: ["binop", op, lhs, rhs]
	lhs := top[2].(S) // NOT kid(top, 0) — index 1 is the operator!
	wantTag(t, lhs, "binop")
	if lhs[1].(string) != "+" {
		t.Fatalf("want '+', got %v", lhs[1])
	}
}

func Test_Parser_Block_Do_End(t *testing.T) {
	root := mustParse(t, `do x end`)
	blk := kid(root, 0)
	wantTag(t, blk, "block")
	if len(blk) != 2 || head(kid(blk, 0)) != "id" {
		t.Fatalf("unexpected block: %s", dump(blk))
	}
}

func Test_Parser_If_Elif_Else(t *testing.T) {
	src := `
if a then
  x
elif b then
  y
else
  z
end`
	root := mustParse(t, src)
	ifNode := kid(root, 0)
	wantTag(t, ifNode, "if")
	// children: ("pair" cond block) ... optional else-block
	if head(kid(ifNode, 0)) != "pair" || head(kid(ifNode, 1)) != "pair" {
		t.Fatalf("if arms malformed: %s", dump(ifNode))
	}
	if last := ifNode[len(ifNode)-1].(S); head(last) != "block" {
		t.Fatalf("if else tail malformed: %s", dump(ifNode))
	}
}

func Test_Parser_For_Targets(t *testing.T) {
	// decl target
	root := mustParse(t, `for let x in xs do end`)
	forNode := kid(root, 0)
	wantTag(t, forNode, "for")
	if head(kid(forNode, 0)) != "decl" {
		t.Fatalf("for target not decl: %s", dump(forNode))
	}

	// index target
	root2 := mustParse(t, `for a[i] in xs do end`)
	if head(kid(root2, 0)) != "for" || head(kid(kid(root2, 0), 0)) != "idx" {
		t.Fatalf("for index target not parsed: %s", dump(root2))
	}

	// invalid target
	_, err := ParseSExpr(`for 1 in xs do end`)
	if err == nil || !strings.Contains(err.Error(), "invalid for-target") {
		t.Fatalf("expected invalid for-target error, got %v", err)
	}
}

func Test_Parser_Control_Return_Break_Continue(t *testing.T) {
	root := mustParse(t, `return(1) break(0) continue(null)`)
	ch := root[1:]
	want := []string{"return", "break", "continue"}
	for i, tag := range want {
		wantTag(t, ch[i].(S), tag)
	}
}

func Test_Parser_Function_And_Oracle(t *testing.T) {
	// function with param type expr and return type expr
	root := mustParse(t, `fun(a: Str) -> Str do end`)
	fn := kid(root, 0)
	wantTag(t, fn, "fun")
	// children: params(array), ret(expr), body(block)
	if head(kid(fn, 0)) != "array" || head(kid(fn, 2)) != "block" {
		t.Fatalf("fun shape wrong: %s", dump(fn))
	}
	if head(kid(fn, 1)) != "id" || kid(fn, 1)[1].(string) != "Str" {
		t.Fatalf("fun return type expr wrong: %s", dump(fn))
	}

	// oracle with from [...]
	root2 := mustParse(t, `oracle() from ["web","docs"]`)
	orc := kid(root2, 0)
	wantTag(t, orc, "oracle")
	if head(kid(orc, 2)) != "array" {
		t.Fatalf("oracle sources not array: %s", dump(orc))
	}
}

func Test_Parser_Type_Statement(t *testing.T) {
	root := mustParse(t, `type [Int]`)
	node := kid(root, 0)
	wantTag(t, node, "type")
	if head(kid(node, 0)) != "array" {
		t.Fatalf("type payload not expression/array: %s", dump(node))
	}
}

func Test_Parser_Annotations_Wrap_Expressions(t *testing.T) {
	src := `# hello
x`
	root := mustParse(t, src)
	ann := kid(root, 0)
	wantTag(t, ann, "annot")
	if inner := kid(ann, 1); head(inner) != "id" {
		t.Fatalf("annotation did not wrap expression: %s", dump(ann))
	}
}

func Test_Parser_Arrays_And_Maps(t *testing.T) {
	root := mustParse(t, `[] [1,2,3] {"name": "John", age: 25}`)
	if len(root) != 4 {
		t.Fatalf("unexpected top-level arity: %s", dump(root))
	}
	wantTag(t, kid(root, 0), "array")
	wantTag(t, kid(root, 1), "array")
	m := kid(root, 2)
	wantTag(t, m, "map")
	if head(kid(m, 0)) != "pair" {
		t.Fatalf("map not pair-shaped: %s", dump(m))
	}
}

func Test_Parser_Assign_To_Get_And_Index(t *testing.T) {
	root := mustParse(t, `a.b = 1 a[0] = 2`)
	assign1 := kid(root, 0)
	assign2 := kid(root, 1)
	wantTag(t, assign1, "assign")
	wantTag(t, assign2, "assign")
	if head(kid(assign1, 0)) != "get" || head(kid(assign2, 0)) != "idx" {
		t.Fatalf("assignment targets wrong: %s", dump(root))
	}
}

func Test_Parser_Error_Missing_RParen(t *testing.T) {
	_, err := ParseSExpr(`f(1`)
	if err == nil || !strings.Contains(strings.ToLower(err.Error()), "expected ')'") {
		t.Fatalf("expected missing ')' error, got %v", err)
	}
}

func Test_Parser_Control_MustUseTildeParen(t *testing.T) {
	// Space → '(' (grouping) → should error for control keywords
	if _, err := ParseSExpr(`return (1)`); err == nil {
		t.Fatalf("expected error when using spaced '(' after return")
	}
	// Adjacent → '~(' tokenized as CLROUND → should succeed
	if _, err := ParseSExpr(`return(1)`); err != nil {
		t.Fatalf("unexpected: %v", err)
	}
}

func Test_Parser_Oracle_From_NoSpace(t *testing.T) {
	if _, err := ParseSExpr(`oracle() from["web","docs"]`); err != nil {
		t.Fatalf("unexpected: %v", err)
	}
}

func Test_Parser_ArrayLiteral_AfterArrayLiteral(t *testing.T) {
	if _, err := ParseSExpr(`[] [1,2]`); err != nil {
		t.Fatalf("unexpected: %v", err)
	}
}

func Test_Parser_Type_Optional_Postfix_Simple(t *testing.T) {
	root := mustParse(t, `type Str?`)
	ty := kid(root, 0)
	wantTag(t, ty, "type")

	opt := ty[1].(S) // type payload
	wantTag(t, opt, "unop")
	if op := opt[1].(string); op != "?" {
		t.Fatalf("want postfix '?', got %q\n%s", op, dump(opt))
	}
	base := opt[2].(S) // unop's child
	wantTag(t, base, "id")
	if base[1].(string) != "Str" {
		t.Fatalf("want Str, got %v", base)
	}
}

func Test_Parser_Type_Optional_Postfix_Array(t *testing.T) {
	root := mustParse(t, `type [Str]?`)
	ty := kid(root, 0)
	wantTag(t, ty, "type")

	opt := ty[1].(S)
	wantTag(t, opt, "unop")
	if opt[1].(string) != "?" {
		t.Fatalf("want '?', got %v", opt[1])
	}
	arr := opt[2].(S)
	wantTag(t, arr, "array")
	if head(arr[1].(S)) != "id" || arr[1].(S)[1].(string) != "Str" {
		t.Fatalf("want array of Str, got %s", dump(arr))
	}
}

func Test_Parser_Type_Optional_In_Record(t *testing.T) {
	root := mustParse(t, `type {name: Str?, hobbies: [Str]?}`)
	ty := kid(root, 0)
	wantTag(t, ty, "type")

	rec := ty[1].(S)
	wantTag(t, rec, "map")
	if head(kid(rec, 0)) != "pair" || head(kid(rec, 1)) != "pair" {
		t.Fatalf("map fields not pairs: %s", dump(rec))
	}
	// name: Str?
	namePair := kid(rec, 0)
	val1 := namePair[2].(S) // pair value
	wantTag(t, val1, "unop")
	if val1[1].(string) != "?" {
		t.Fatalf("name field not optional: %s", dump(val1))
	}
	// hobbies: [Str]?
	hPair := kid(rec, 1)
	val2 := hPair[2].(S)
	wantTag(t, val2, "unop")
	if val2[1].(string) != "?" || head(val2[2].(S)) != "array" {
		t.Fatalf("hobbies field not optional array: %s", dump(val2))
	}
}

func Test_Parser_Type_Arrow_RightAssociative(t *testing.T) {
	root := mustParse(t, `type Int -> Str -> Bool`)
	ty := kid(root, 0)
	wantTag(t, ty, "type")

	arrow := ty[1].(S)
	wantTag(t, arrow, "binop")
	if arrow[1].(string) != "->" {
		t.Fatalf("top op not '->': %v", arrow[1])
	}
	// left = Int
	l := arrow[2].(S) // binop lhs
	wantTag(t, l, "id")
	if l[1].(string) != "Int" {
		t.Fatalf("left not Int: %v", l)
	}
	// right = (Str -> Bool)
	r := arrow[3].(S) // binop rhs
	wantTag(t, r, "binop")
	if r[1].(string) != "->" {
		t.Fatalf("right not nested '->': %v", r)
	}
}

func Test_Parser_Type_Optional_Binds_Tighter_Than_Arrow(t *testing.T) {
	root := mustParse(t, `type Str? -> Int`)
	ty := kid(root, 0)
	wantTag(t, ty, "type")

	arrow := ty[1].(S)
	wantTag(t, arrow, "binop")
	if arrow[1].(string) != "->" {
		t.Fatalf("want '->', got %v", arrow[1])
	}
	// left should be (Str?) — postfix optional binds tighter than ->
	left := arrow[2].(S)
	wantTag(t, left, "unop")
	if left[1].(string) != "?" || head(left[2].(S)) != "id" || left[2].(S)[1].(string) != "Str" {
		t.Fatalf("left not Str?: %s", dump(left))
	}
}

func Test_Parser_Type_ArrayOptional_Binds_Tighter_Than_Arrow(t *testing.T) {
	root := mustParse(t, `type [Str]? -> Int`)
	ty := kid(root, 0)
	wantTag(t, ty, "type")

	arrow := ty[1].(S)
	wantTag(t, arrow, "binop")
	if arrow[1].(string) != "->" {
		t.Fatalf("want '->', got %v", arrow[1])
	}
	left := arrow[2].(S)
	wantTag(t, left, "unop")
	if left[1].(string) != "?" || head(left[2].(S)) != "array" {
		t.Fatalf("left not [Str]?: %s", dump(left))
	}
}

func Test_Parser_Number_TrailingDot_ParsesAsFloat(t *testing.T) {
	root := mustParse(t, `5.`)
	wantTag(t, root, "block")
	if len(root) != 2 {
		t.Fatalf("unexpected top-level arity: %s", dump(root))
	}
	n := kid(root, 0)
	wantTag(t, n, "num")
	if v := n[1].(float64); v != 5.0 {
		t.Fatalf("want 5.0, got %v\nnode: %s", v, dump(n))
	}
}

func Test_Parser_DotNumber_Desugars_To_Index(t *testing.T) {
	root := mustParse(t, `obj.name.field(5).90`)
	top := kid(root, 0)
	wantTag(t, top, "idx")

	// index must be the integer 90
	idx := kid(top, 1)
	wantTag(t, idx, "int")
	if v := idx[1].(int64); v != 90 {
		t.Fatalf("want index 90, got %v\nnode: %s", v, dump(idx))
	}

	// target must be the call: obj.name.field(5)
	call := kid(top, 0)
	wantTag(t, call, "call")

	// callee: get(get(obj, "name"), "field")
	callee := kid(call, 0)
	wantTag(t, callee, "get")
	propField := kid(callee, 1)
	wantTag(t, propField, "str")
	if propField[1].(string) != "field" {
		t.Fatalf("want callee .field, got %s", dump(propField))
	}

	innerGet := kid(callee, 0)
	wantTag(t, innerGet, "get")
	propName := kid(innerGet, 1)
	wantTag(t, propName, "str")
	if propName[1].(string) != "name" {
		t.Fatalf("want inner .name, got %s", dump(propName))
	}
	base := kid(innerGet, 0)
	wantTag(t, base, "id")
	if base[1].(string) != "obj" {
		t.Fatalf("want base id obj, got %s", dump(base))
	}

	// one argument: 5
	if len(call) != 3 {
		t.Fatalf("call arity unexpected: %s", dump(call))
	}
	arg := call[2].(S) // call = ["call", callee, arg1, ...]
	wantTag(t, arg, "int")
	if arg[1].(int64) != 5 {
		t.Fatalf("want arg 5, got %v", arg[1])
	}
}

func Test_Parser_Map_Required_Field_Bang(t *testing.T) {
	root := mustParse(t, `type {name!: Str, age: Int}`)
	ty := kid(root, 0)
	wantTag(t, ty, "type")

	rec := ty[1].(S)
	wantTag(t, rec, "map")
	if len(rec) != 3 {
		t.Fatalf("unexpected map arity: %s", dump(rec))
	}

	// first field: pair!
	f1 := kid(rec, 0)
	if head(f1) != "pair!" {
		t.Fatalf("first field should be pair!: %s", dump(f1))
	}
	// key "name"
	k1 := f1[1].(S)
	wantTag(t, k1, "str")
	if k1[1].(string) != "name" {
		t.Fatalf("want key 'name', got %s", dump(k1))
	}
	// value Str
	v1 := f1[2].(S)
	wantTag(t, v1, "id")
	if v1[1].(string) != "Str" {
		t.Fatalf("want Str, got %s", dump(v1))
	}

	// second field: normal pair
	f2 := kid(rec, 1)
	if head(f2) != "pair" {
		t.Fatalf("second field should be pair: %s", dump(f2))
	}
	k2 := f2[1].(S)
	wantTag(t, k2, "str")
	if k2[1].(string) != "age" {
		t.Fatalf("want key 'age', got %s", dump(k2))
	}
	v2 := f2[2].(S)
	wantTag(t, v2, "id")
	if v2[1].(string) != "Int" {
		t.Fatalf("want Int, got %s", dump(v2))
	}
}

func Test_Parser_Let_SimpleDecl_StillWorks(t *testing.T) {
	root := mustParse(t, `let x`)
	stmt := kid(root, 0)
	wantTag(t, stmt, "decl")
	if stmt[1].(string) != "x" {
		t.Fatalf("want decl x, got %s", dump(stmt))
	}
}

func Test_Parser_Let_ArrayDestructuring_AssignShape(t *testing.T) {
	root := mustParse(t, `let [x, y] = [1, 2]`)
	assign := kid(root, 0)
	wantTag(t, assign, "assign")

	lhs := kid(assign, 0)
	wantTag(t, lhs, "darr")
	if len(lhs) != 3 || head(lhs[1].(S)) != "decl" || head(lhs[2].(S)) != "decl" {
		t.Fatalf("array pattern shape wrong: %s", dump(lhs))
	}

	rhs := kid(assign, 1)
	wantTag(t, rhs, "array")
}

func Test_Parser_Let_ObjectDestructuring_AssignShape(t *testing.T) {
	root := mustParse(t, `let {name: x, age: y} = obj`)
	assign := kid(root, 0)
	wantTag(t, assign, "assign")

	lhs := kid(assign, 0)
	wantTag(t, lhs, "dobj")
	if len(lhs) != 3 {
		t.Fatalf("object pattern arity wrong: %s", dump(lhs))
	}
	p1 := kid(lhs, 0)
	p2 := kid(lhs, 1)
	if head(p1) != "pair" || head(p2) != "pair" {
		t.Fatalf("object pattern pairs wrong: %s", dump(lhs))
	}
	if ks := p1[1].(S)[1].(string); ks != "name" {
		t.Fatalf("want key 'name', got %q", ks)
	}
	if ks := p2[1].(S)[1].(string); ks != "age" {
		t.Fatalf("want key 'age', got %q", ks)
	}
	if head(p1[2].(S)) != "decl" || head(p2[2].(S)) != "decl" {
		t.Fatalf("object pair values not decls: %s / %s", dump(p1[2].(S)), dump(p2[2].(S)))
	}
}

func Test_Parser_Let_NestedPatterns(t *testing.T) {
	root := mustParse(t, `let {pt: [x, y]} = m`)
	assign := kid(root, 0)
	wantTag(t, assign, "assign")

	obj := kid(assign, 0)
	wantTag(t, obj, "dobj")
	p := kid(obj, 0)
	if p[1].(S)[1].(string) != "pt" {
		t.Fatalf("want key 'pt', got %s", dump(p))
	}
	arr := p[2].(S)
	wantTag(t, arr, "darr")
	if !(head(kid(arr, 0)) == "decl" && head(kid(arr, 1)) == "decl") {
		t.Fatalf("nested array decls missing: %s", dump(arr))
	}
}

func Test_Parser_Let_Destructuring_RequiresEquals(t *testing.T) {
	if _, err := ParseSExpr(`let [x, y]`); err == nil {
		t.Fatalf("expected error for missing '=' after destructuring let")
	}
}

// parser_test.go
func Test_Parser_Keywords_As_Map_Keys(t *testing.T) {
	root := mustParse(t, `{if: 1, else: 2, fun: 3, type: 4}`)
	m := kid(root, 0)
	wantTag(t, m, "map")
	if k := kid(m, 0)[1].(S)[1].(string); k != "if" {
		t.Fatalf("want 'if', got %q", k)
	}
	if k := kid(m, 1)[1].(S)[1].(string); k != "else" {
		t.Fatalf("want 'else', got %q", k)
	}
	if k := kid(m, 2)[1].(S)[1].(string); k != "fun" {
		t.Fatalf("want 'fun', got %q", k)
	}
	if k := kid(m, 3)[1].(S)[1].(string); k != "type" {
		t.Fatalf("want 'type', got %q", k)
	}
}

func Test_Parser_While_Basic_Shape(t *testing.T) {
	root := mustParse(t, `while true do x end`)
	node := kid(root, 0)
	wantTag(t, node, "while")
	if head(kid(node, 0)) != "bool" {
		t.Fatalf("cond not bool: %s", dump(node))
	}
	if head(kid(node, 1)) != "block" {
		t.Fatalf("body not block: %s", dump(node))
	}
}

func Test_Parser_While_With_Grouping(t *testing.T) {
	root := mustParse(t, `while (1 < 2) do end`)
	node := kid(root, 0)
	wantTag(t, node, "while")
	cond := kid(node, 0)
	wantTag(t, cond, "binop")
	if cond[1].(string) != "<" {
		t.Fatalf("want '<' in condition, got %v", cond[1])
	}
}
