// parser_test.go
package mindscript

import (
	"encoding/json"
	"reflect"
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

func mustParseInteractive(t *testing.T, src string) S {
	t.Helper()
	sexpr, err := ParseSExprInteractive(src)
	if err != nil {
		t.Fatalf("Parse (interactive) error: %v\nsource:\n%s", err, src)
	}
	return sexpr
}

func mustIncomplete(t *testing.T, src string) {
	t.Helper()
	_, err := ParseSExprInteractive(src)
	if err == nil || !IsIncomplete(err) {
		t.Fatalf("expected IncompleteError, got %v\nsource:\n%s", err, src)
	}
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

func mustFailParseContains(t *testing.T, src string, substr string) {
	t.Helper()
	_, err := ParseSExpr(src)
	if err == nil {
		t.Fatalf("expected parse error containing %q, got nil\nsource:\n%s", substr, src)
	}
	if substr != "" && !strings.Contains(err.Error(), substr) {
		t.Fatalf("expected error containing %q, got %v\nsource:\n%s", substr, err, src)
	}
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
func Test_Parser_Control_Return_Break_Continue_FormsEquivalent(t *testing.T) {
	a1 := mustParse(t, `return(1) break(0) continue(null)`)
	a2 := mustParse(t, `return 1 break 0 continue null`)

	// Compare the three statements pairwise
	for i := 0; i < 3; i++ {
		n1 := a1[i+1].(S)
		n2 := a2[i+1].(S)
		if !reflect.DeepEqual(n1, n2) {
			t.Fatalf("ASTs differ for statement %d:\nwith parens: %s\nno parens:  %s", i, dump(n1), dump(n2))
		}
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

func Test_Parser_Control_SameLine_Grouping_OK(t *testing.T) {
	r1 := mustParse(t, `return(1)`)
	r2 := mustParse(t, `return (1)`)
	j1, _ := json.Marshal(kid(r1, 0))
	j2, _ := json.Marshal(kid(r2, 0))
	if string(j1) != string(j2) {
		t.Fatalf("ASTs should match for return(1) vs return (1)\n%s\n%s", string(j1), string(j2))
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
		t.Fatalf("right not nested '->': %v", r[1])
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

func Test_Parser_Map_KeyAndValue_Annotations(t *testing.T) {
	src := `{
# the name
name: "Mo",
# the age
age: 47,
available: "yes" # status
}`
	root := mustParse(t, src)
	m := kid(root, 0)
	wantTag(t, m, "map")
	if len(m) != 4 { // "map" + 3 fields
		t.Fatalf("unexpected map arity: %s", dump(m))
	}

	// 1) "the name" annotates KEY "name" (PRE)
	p1 := kid(m, 0)
	if head(p1) != "pair" {
		t.Fatalf("field1 should be pair: %s", dump(p1))
	}
	k1 := p1[1].(S)
	wantTag(t, k1, "annot")
	if s := k1[1].(S); head(s) != "str" || s[1].(string) != "the name" {
		t.Fatalf("bad key annotation payload: %s", dump(k1))
	}
	if inner := k1[2].(S); head(inner) != "str" || inner[1].(string) != "name" {
		t.Fatalf("annot did not wrap key 'name': %s", dump(k1))
	}
	v1 := p1[2].(S)
	wantTag(t, v1, "str")
	if v1[1].(string) != "Mo" {
		t.Fatalf("want value 'Mo', got %s", dump(v1))
	}

	// 2) "the age" annotates KEY "age" (PRE)
	p2 := kid(m, 1)
	if head(p2) != "pair" {
		t.Fatalf("field2 should be pair: %s", dump(p2))
	}
	k2 := p2[1].(S)
	wantTag(t, k2, "annot")
	if s := k2[1].(S); head(s) != "str" || s[1].(string) != "the age" {
		t.Fatalf("bad key annotation payload: %s", dump(k2))
	}
	if inner := k2[2].(S); head(inner) != "str" || inner[1].(string) != "age" {
		t.Fatalf("annot did not wrap key 'age': %s", dump(k2))
	}
	v2 := p2[2].(S)
	wantTag(t, v2, "int")
	if v2[1].(int64) != 47 {
		t.Fatalf("want 47, got %v", v2[1])
	}

	// 3) "status" annotates VALUE "yes" (POST → encoded as "<status")
	p3 := kid(m, 2)
	if head(p3) != "pair" {
		t.Fatalf("field3 should be pair: %s", dump(p3))
	}
	k3 := p3[1].(S)
	wantTag(t, k3, "str")
	if k3[1].(string) != "available" {
		t.Fatalf("want key 'available', got %s", dump(k3))
	}
	v3 := p3[2].(S)
	wantTag(t, v3, "annot")
	if s := v3[1].(S); head(s) != "str" || s[1].(string) != "<status" {
		t.Fatalf("bad value annotation payload (expect '<status'): %s", dump(v3))
	}
	if inner := v3[2].(S); head(inner) != "str" || inner[1].(string) != "yes" {
		t.Fatalf("annot did not wrap value 'yes': %s", dump(v3))
	}
}

func Test_Parser_AnyWord_As_Map_Key(t *testing.T) {
	root := mustParse(t, `{if:1, else:2, for:3, type:4, Enum:5, Int:6, Str:7, true:8, null:9}`)
	m := kid(root, 0)
	wantTag(t, m, "map")
	keys := []string{"if", "else", "for", "type", "Enum", "Int", "Str", "true", "null"}
	for i, want := range keys {
		p := kid(m, i)
		if head(p) != "pair" {
			t.Fatalf("pair %d missing", i)
		}
		k := p[1].(S)
		wantTag(t, k, "str")
		if k[1].(string) != want {
			t.Fatalf("key %d: want %q, got %q", i, want, k[1].(string))
		}
	}
}

func Test_Parser_Let_Array_Destructuring(t *testing.T) {
	src := `let [x, y] = arr`
	root := mustParse(t, src)

	// block with one child: assign(darr(...), id(arr))
	wantTag(t, root, "block")
	assign := kid(root, 0)
	wantTag(t, assign, "assign")

	lhs := kid(assign, 0)
	wantTag(t, lhs, "darr")
	if len(lhs) != 3 { // "darr", ("decl","x"), ("decl","y")
		t.Fatalf("want 2 elements in array pattern, got %d\n%s", len(lhs)-1, dump(lhs))
	}
	if head(lhs[1].(S)) != "decl" || lhs[1].(S)[1].(string) != "x" {
		t.Fatalf("lhs[0] not decl x: %s", dump(lhs[1].(S)))
	}
	if head(lhs[2].(S)) != "decl" || lhs[2].(S)[1].(string) != "y" {
		t.Fatalf("lhs[1] not decl y: %s", dump(lhs[2].(S)))
	}

	rhs := kid(assign, 1)
	wantTag(t, rhs, "id")
	if rhs[1].(string) != "arr" {
		t.Fatalf("rhs id != arr: %s", dump(rhs))
	}
}

func Test_Parser_For_ArrayPattern_NoLet(t *testing.T) {
	src := `for [k, v] in obj do end`
	root := mustParse(t, src)

	wantTag(t, root, "block")
	forNode := kid(root, 0)
	wantTag(t, forNode, "for")

	// target
	tgt := kid(forNode, 0)
	wantTag(t, tgt, "darr")
	if len(tgt) != 3 {
		t.Fatalf("want 2 elems in pattern, got %d\n%s", len(tgt)-1, dump(tgt))
	}
	if head(tgt[1].(S)) != "decl" || tgt[1].(S)[1].(string) != "k" {
		t.Fatalf("first pattern not decl k: %s", dump(tgt[1].(S)))
	}
	if head(tgt[2].(S)) != "decl" || tgt[2].(S)[1].(string) != "v" {
		t.Fatalf("second pattern not decl v: %s", dump(tgt[2].(S)))
	}

	// iter
	iter := kid(forNode, 1)
	wantTag(t, iter, "id")
	if iter[1].(string) != "obj" {
		t.Fatalf("iter id != obj: %s", dump(iter))
	}

	// body
	body := kid(forNode, 2)
	wantTag(t, body, "block")
}

func Test_Parser_For_ArrayPattern_WithLet(t *testing.T) {
	src := `for let [k, v] in obj do end`
	root := mustParse(t, src)

	wantTag(t, root, "block")
	forNode := kid(root, 0)
	wantTag(t, forNode, "for")

	tgt := kid(forNode, 0)
	wantTag(t, tgt, "darr")
	if head(tgt[1].(S)) != "decl" || tgt[1].(S)[1].(string) != "k" {
		t.Fatalf("first pattern not decl k: %s", dump(tgt[1].(S)))
	}
	if head(tgt[2].(S)) != "decl" || tgt[2].(S)[1].(string) != "v" {
		t.Fatalf("second pattern not decl v: %s", dump(tgt[2].(S)))
	}
}

func Test_Parser_For_ObjectPattern(t *testing.T) {
	src := `for {name: n, age: a} in people do end`
	root := mustParse(t, src)

	wantTag(t, root, "block")
	forNode := kid(root, 0)
	wantTag(t, forNode, "for")

	tgt := kid(forNode, 0)
	wantTag(t, tgt, "dobj")
	if len(tgt) != 3 { // "dobj", pair(name->decl n), pair(age->decl a)
		t.Fatalf("want 2 pairs, got %d\n%s", len(tgt)-1, dump(tgt))
	}

	// check first pair: ("pair", ("str","name"), ("decl","n"))
	p1 := tgt[1].(S)
	wantTag(t, p1, "pair")
	key1 := p1[1].(S)
	wantTag(t, key1, "str")
	if key1[1].(string) != "name" {
		t.Fatalf("first key != name: %s", dump(key1))
	}
	val1 := p1[2].(S)
	wantTag(t, val1, "decl")
	if val1[1].(string) != "n" {
		t.Fatalf("first value decl != n: %s", dump(val1))
	}

	// second pair: ("pair", ("str","age"), ("decl","a"))
	p2 := tgt[2].(S)
	wantTag(t, p2, "pair")
	key2 := p2[1].(S)
	wantTag(t, key2, "str")
	if key2[1].(string) != "age" {
		t.Fatalf("second key != age: %s", dump(key2))
	}
	val2 := p2[2].(S)
	wantTag(t, val2, "decl")
	if val2[1].(string) != "a" {
		t.Fatalf("second value decl != a: %s", dump(val2))
	}

	iter := kid(forNode, 1)
	wantTag(t, iter, "id")
	if iter[1].(string) != "people" {
		t.Fatalf("iter id != people: %s", dump(iter))
	}

	body := kid(forNode, 2)
	wantTag(t, body, "block")
}

func Test_Parser_For_BareId_ImplicitDecl(t *testing.T) {
	src := `for k in obj do end`
	root := mustParse(t, src)

	wantTag(t, root, "block")
	forNode := kid(root, 0)
	wantTag(t, forNode, "for")

	tgt := kid(forNode, 0)
	wantTag(t, tgt, "decl")
	if tgt[1].(string) != "k" {
		t.Fatalf("target not decl k: %s", dump(tgt))
	}
	iter := kid(forNode, 1)
	wantTag(t, iter, "id")
	if iter[1].(string) != "obj" {
		t.Fatalf("iter id != obj: %s", dump(iter))
	}
	body := kid(forNode, 2)
	wantTag(t, body, "block")
}

func Test_Parser_Control_NewlineMeansNullAndNextExpr(t *testing.T) {
	src := "return\n(1 + 2)"
	root := mustParse(t, src)
	if len(root) != 3 {
		t.Fatalf("expected two top-level expressions, got %d\n%s", len(root)-1, dump(root))
	}
	ret := kid(root, 0)
	wantTag(t, ret, "return")
	if head(ret[1].(S)) != "null" {
		t.Fatalf("newline 'return' should default to null, got %s", dump(ret))
	}
	expr := kid(root, 1)
	wantTag(t, expr, "binop")
	if expr[1].(string) != "+" {
		t.Fatalf("want '+', got %v", expr[1])
	}
}

// Newline after a control keyword ⇒ defaults to null, and starts a new statement.
func Test_Parser_Control_BareOnNewlines_DefaultsToNull(t *testing.T) {
	src := "return\nbreak\ncontinue\n"
	root := mustParse(t, src)

	// block + 3 children
	if len(root) != 4 {
		t.Fatalf("want three statements, got %d\n%s", len(root)-1, dump(root))
	}
	for i, tag := range []string{"return", "break", "continue"} {
		stmt := kid(root, i)
		wantTag(t, stmt, tag)
		wantTag(t, stmt[1].(S), "null")
	}
}

// Same-line controls chain as a single expression: return (break (continue null))
func Test_Parser_Control_SameLine_ChainsAsSingleExpr(t *testing.T) {
	root := mustParse(t, `return break continue`)

	// block + 1 child (the chained expression)
	if len(root) != 2 {
		t.Fatalf("want one statement, got %d\n%s", len(root)-1, dump(root))
	}
	r := kid(root, 0)
	wantTag(t, r, "return")
	b := r[1].(S)
	wantTag(t, b, "break")
	c := b[1].(S)
	wantTag(t, c, "continue")
	wantTag(t, c[1].(S), "null")
}

func Test_Interactive_Incomplete_Grouping_IsIncomplete(t *testing.T) {
	mustIncomplete(t, "let x = (")
}

func Test_Interactive_Incomplete_Block_IsIncomplete(t *testing.T) {
	mustIncomplete(t, "do x")
	mustIncomplete(t, "if a then x")
}

func Test_Interactive_Incomplete_Params_IsIncomplete(t *testing.T) {
	mustIncomplete(t, "fun(a: Str")
}

func Test_Interactive_Completes_WhenClosed(t *testing.T) {
	full := "let x = (\n  1 + 1\n)\n"
	a := mustParseInteractive(t, full)
	b := mustParse(t, full)
	ja, _ := json.Marshal(a)
	jb, _ := json.Marshal(b)
	if string(ja) != string(jb) {
		t.Fatalf("interactive vs normal AST differ:\n%s\n%s", string(ja), string(jb))
	}
}

func Test_Parser_Control_Return_Forms_SameAST(t *testing.T) {
	r1 := mustParse(t, `return 1`)
	r2 := mustParse(t, `return(1)`)
	j1, _ := json.Marshal(kid(r1, 0))
	j2, _ := json.Marshal(kid(r2, 0))
	if string(j1) != string(j2) {
		t.Fatalf("return forms not equivalent:\n%s\n%s", string(j1), string(j2))
	}
}
func Test_Parser_ComputedDotIndex_Read_And_Assign(t *testing.T) {
	// obj.("na"+"me")       → idx(obj, ("binop","+","na","me"))
	root := mustParse(t, `obj.("na" + "me")`)
	expr := kid(root, 0)
	wantTag(t, expr, "idx")
	wantTag(t, kid(expr, 0), "id") // obj
	ix := kid(expr, 1)
	wantTag(t, ix, "binop")
	if ix[1].(string) != "+" {
		t.Fatalf("want '+', got %v", ix[1])
	}

	// obj.("na"+"me") = "Juan"   → assign( idx(obj, binop+), "Juan")
	root2 := mustParse(t, `obj.("na" + "me") = "Juan"`)
	assign := kid(root2, 0)
	wantTag(t, assign, "assign")
	lhs := kid(assign, 0)
	wantTag(t, lhs, "idx")
	wantTag(t, kid(lhs, 0), "id")    // obj
	wantTag(t, kid(lhs, 1), "binop") // ("+" ...)
	wantTag(t, kid(assign, 1), "str")
}

func Test_Parser_ComputedDotIndex_Array(t *testing.T) {
	// arr.(0 + 1)            → idx(arr, ("binop","+","0","1"))
	root := mustParse(t, `arr.(0 + 1)`)
	expr := kid(root, 0)
	wantTag(t, expr, "idx")
	wantTag(t, kid(expr, 0), "id") // arr
	ix := kid(expr, 1)
	wantTag(t, ix, "binop")
	if ix[1].(string) != "+" {
		t.Fatalf("want '+', got %v", ix[1])
	}

	// arr.(0 + 1) = "Juan"   → assign(idx(arr, binop+), "Juan")
	root2 := mustParse(t, `arr.(0 + 1) = "Juan"`)
	assign := kid(root2, 0)
	wantTag(t, assign, "assign")
	lhs := kid(assign, 0)
	wantTag(t, lhs, "idx")
	wantTag(t, kid(lhs, 0), "id")    // arr
	wantTag(t, kid(lhs, 1), "binop") // ("+" ...)
	wantTag(t, kid(assign, 1), "str")
}

func Test_Parser_Annotations_Noops_DoBlock(t *testing.T) {
	src := `
# annot 1
# annot 2

# annot 3
do
  let x
  
  # annot 4
  # annot 5
  lex y
end
`
	root := mustParse(t, src)
	wantTag(t, root, "block")

	// Top-level: two children:
	// 1) ("annot", ("str","annot 1\nannot 2"), ("noop"))
	// 2) ("annot", ("str","annot 3"), ("block", ...))
	if len(root) != 1+2 {
		t.Fatalf("want 2 top-level children, got %d\n%s", len(root)-1, dump(root))
	}

	// Child 0: annot 1+2 wrapping NOOP
	ann12 := kid(root, 0)
	wantTag(t, ann12, "annot")
	ann12Text := kid(ann12, 0)
	wantTag(t, ann12Text, "str")
	if ann12Text[1].(string) != "annot 1\nannot 2" {
		t.Fatalf("annot 1+2 text mismatch: %q", ann12Text[1])
	}
	no := kid(ann12, 1)
	wantTag(t, no, "noop")

	// Child 1: annot 3 wrapping the do-block
	ann3 := kid(root, 1)
	wantTag(t, ann3, "annot")
	ann3Text := kid(ann3, 0)
	wantTag(t, ann3Text, "str")
	if ann3Text[1].(string) != "annot 3" {
		t.Fatalf("annot 3 text mismatch: %q", ann3Text[1])
	}
	blk := kid(ann3, 1)
	wantTag(t, blk, "block")

	// Block must have 4 children:
	//   0) ("decl","x")
	//   1) ("noop")
	//   2) ("annot", ("str","annot 4\nannot 5"), ("id","lex"))
	//   3) ("id","y")
	if len(blk) != 1+4 {
		t.Fatalf("want 4 block children, got %d\n%s", len(blk)-1, dump(root))
	}

	declX := kid(blk, 0)
	wantTag(t, declX, "decl")
	if declX[1].(string) != "x" {
		t.Fatalf("decl name mismatch: %q", declX[1])
	}

	no2 := kid(blk, 1)
	wantTag(t, no2, "noop")

	ann45 := kid(blk, 2)
	wantTag(t, ann45, "annot")
	ann45Text := kid(ann45, 0)
	wantTag(t, ann45Text, "str")
	if ann45Text[1].(string) != "annot 4\nannot 5" {
		t.Fatalf("annot 4+5 text mismatch: %q", ann45Text[1])
	}
	wrappedLex := kid(ann45, 1)
	wantTag(t, wrappedLex, "id")
	if wrappedLex[1].(string) != "lex" {
		t.Fatalf("wrapped id mismatch: %q", wrappedLex[1])
	}

	idY := kid(blk, 3)
	wantTag(t, idY, "id")
	if idY[1].(string) != "y" {
		t.Fatalf("final id mismatch: %q", idY[1])
	}
}

// Multiline PRE annotation wraps the next expression (let x = 0)
func Test_Parser_Annot_Pre_Multiline_Wraps_Let(t *testing.T) {
	src := "do\n# 1. line\n# 2. line\nlet x = 0\nend"
	root := mustParse(t, src)
	blk := kid(root, 0)
	wantTag(t, blk, "block")

	e1 := kid(blk, 0)
	wantTag(t, e1, "annot")

	// PRE annotations are stored as-is (no leading "<"), with lines joined by '\n'.
	txt := e1[1].(S)
	wantTag(t, txt, "str")
	if got := txt[1].(string); got != "1. line\n2. line" {
		t.Fatalf("expected PRE annot text %q, got %q", "1. line\n2. line", got)
	}

	// Wrapped node is still the 2nd child (index 1 via kid).
	asn := kid(e1, 1)
	wantTag(t, asn, "assign")

	lhs := kid(asn, 0)
	rhs := kid(asn, 1)
	wantTag(t, lhs, "decl")
	if lhs[1].(string) != "x" {
		t.Fatalf("want decl x, got %s", dump(lhs))
	}
	wantTag(t, rhs, "int")
	if rhs[1].(int64) != 0 {
		t.Fatalf("want 0, got %v", rhs[1])
	}
}

// Trailing POST annotation attaches to the RHS literal `1`
func Test_Parser_Annot_Post_Trailing_Attaches_RHSLiteral(t *testing.T) {
	src := "do\nlet y = 1 # 3. post\nend"
	root := mustParse(t, src)
	blk := kid(root, 0)
	asn := kid(blk, 0)
	wantTag(t, asn, "assign")

	rhs := kid(asn, 1)
	wantTag(t, rhs, "annot")
	txt := rhs[1].(S)
	wantTag(t, txt, "str")
	if s := txt[1].(string); s != "<3. post" {
		t.Fatalf("expected POST annot text %q, got %q", "<3. post", s)
	}
	inner := kid(rhs, 1)
	wantTag(t, inner, "int")
	if inner[1].(int64) != 1 {
		t.Fatalf("want 1, got %v", inner[1])
	}
}

// PRE annotation before a map: attaches to the map on the RHS
func Test_Parser_Annot_Pre_Before_Map_RHS(t *testing.T) {
	src := "let obj = \n# 4. pre map\n{ a: 1 }"
	root := mustParse(t, src)
	asn := kid(root, 0)
	wantTag(t, asn, "assign")

	val := kid(asn, 1)
	wantTag(t, val, "annot")
	txt := val[1].(S)
	wantTag(t, txt, "str")
	if s := txt[1].(string); s != "4. pre map" {
		t.Fatalf("expected PRE annot on map, got %q", s)
	}
	mp := kid(val, 1)
	wantTag(t, mp, "map")
}

// PRE annotation on a key inside a map
func Test_Parser_Annot_Pre_On_Key(t *testing.T) {
	src := "let obj = { \n# 5. pre key\nname: 1 \n}"
	root := mustParse(t, src)
	asn := kid(root, 0)
	mp := kid(asn, 1)
	wantTag(t, mp, "map")

	p := kid(mp, 0)
	wantTag(t, p, "pair")
	k := kid(p, 0)
	wantTag(t, k, "annot")
	kt := k[1].(S)
	wantTag(t, kt, "str")
	if s := kt[1].(string); s != "5. pre key" {
		t.Fatalf("expected PRE annot text %q, got %q", "5. pre key", s)
	}
	kstr := kid(k, 1)
	wantTag(t, kstr, "str")
	if kstr[1].(string) != "name" {
		t.Fatalf("want key 'name', got %v", kstr[1])
	}
}

// POST annotation on an integer literal value
func Test_Parser_Annot_Post_On_Int_Value(t *testing.T) {
	src := `let m = { age: 17 # 7. post 
	}`
	root := mustParse(t, src)
	asn := kid(root, 0)
	mp := kid(asn, 1)
	p := kid(mp, 0)
	val := kid(p, 1)

	wantTag(t, val, "annot")
	vt := val[1].(S)
	wantTag(t, vt, "str")
	// Source has trailing space after "post ", so accept prefix.
	if s := vt[1].(string); !strings.HasPrefix(s, "<7. post") {
		t.Fatalf("expected POST annot starting with %q, got %q", "<7. post", s)
	}
	inner := kid(val, 1)
	wantTag(t, inner, "int")
	if inner[1].(int64) != 17 {
		t.Fatalf("want 17, got %v", inner[1])
	}
}

func Test_Parser_Annotations_Keys_And_Required(t *testing.T) {
	src := `{
# key
name!: 1
}`
	root := mustParse(t, src)
	mp := kid(root, 0)
	wantTag(t, mp, "map")
	if len(mp) != 2 {
		t.Fatalf("want 1 pair, got %d\n%s", len(mp)-1, dump(mp))
	}
	p := kid(mp, 0)
	wantTag(t, p, "pair!")
	k := kid(p, 0)
	wantTag(t, k, "annot")
	kt := k[1].(S)
	wantTag(t, kt, "str")
	if s := kt[1].(string); s != "key" {
		t.Fatalf("expected PRE annot text %q, got %q", "key", s)
	}
	kstr := kid(k, 1)
	wantTag(t, kstr, "str")
	if kstr[1].(string) != "name" {
		t.Fatalf("want key 'name', got %v", kstr[1])
	}
	v := kid(p, 1)
	wantTag(t, v, "int")
	if v[1].(int64) != 1 {
		t.Fatalf("want 1, got %v", v[1])
	}
}

func Test_Parser_Annotations_Postfix_Chain_With_Post(t *testing.T) {
	src := `obj.name(1)[i] # post `
	root := mustParse(t, src)
	e := kid(root, 0)
	wantTag(t, e, "annot")

	txt := e[1].(S)
	wantTag(t, txt, "str")
	// There’s a trailing space after "post " in the source; accept prefix.
	if s := txt[1].(string); !strings.HasPrefix(s, "<post") {
		t.Fatalf("expected POST annot starting with %q, got %q", "<post", s)
	}

	inner := kid(e, 1)
	// The inner should be the whole chain; last op is index => "idx"
	wantTag(t, inner, "idx")
	// Sanity: idx's object is a "call(...)" on a "get"
	obj := kid(inner, 0)
	wantTag(t, obj, "call")
	cal := obj
	get := kid(cal, 0)
	wantTag(t, get, "get")
}

func Test_Parser_Annotations_Return_Newline_Sensitivity(t *testing.T) {
	// Same-line PRE annotation after 'return' → wraps following expr (value on next line)
	src1 := "return # pre\nx"
	r1 := mustParse(t, src1)
	ret1 := kid(r1, 0)
	wantTag(t, ret1, "return")
	arg1 := kid(ret1, 0)
	wantTag(t, arg1, "annot")
	t1 := arg1[1].(S)
	wantTag(t, t1, "str")
	if s := t1[1].(string); s != "pre" {
		t.Fatalf("expected PRE annot text %q, got %q", "pre", s)
	}
	if head(kid(arg1, 1)) != "id" || kid(arg1, 1)[1].(string) != "x" {
		t.Fatalf("unexpected return payload: %s", dump(arg1))
	}

	// POST annotation on the returned expr
	src2 := `return x # post`
	r2 := mustParse(t, src2)
	ret2 := kid(r2, 0)
	wantTag(t, ret2, "return")
	arg2 := kid(ret2, 0)
	wantTag(t, arg2, "annot")
	t2 := arg2[1].(S)
	wantTag(t, t2, "str")
	if s := t2[1].(string); s != "<post" {
		t.Fatalf("expected POST annot text %q, got %q", "<post", s)
	}
	if head(kid(arg2, 1)) != "id" || kid(arg2, 1)[1].(string) != "x" {
		t.Fatalf("unexpected return payload: %s", dump(arg2))
	}

	// Newline → return takes implicit null; annotated expr is separate stmt
	src3 := "return\n# pre\nx"
	r3 := mustParse(t, src3)
	if len(r3) != 3 {
		t.Fatalf("want 2 statements, got %d\n%s", len(r3)-1, dump(r3))
	}
	ret3 := kid(r3, 0)
	wantTag(t, ret3, "return")
	if head(kid(ret3, 0)) != "null" {
		t.Fatalf("want return null, got %s", dump(ret3))
	}
	annStmt := kid(r3, 1)
	wantTag(t, annStmt, "annot")
	t3 := annStmt[1].(S)
	wantTag(t, t3, "str")
	if s := t3[1].(string); s != "pre" {
		t.Fatalf("expected PRE annot text %q, got %q", "pre", s)
	}
	if head(kid(annStmt, 1)) != "id" || kid(annStmt, 1)[1].(string) != "x" {
		t.Fatalf("unexpected annotated stmt: %s", dump(annStmt))
	}
}

func Test_SkipNoop_And_TrailingComma_InArray(t *testing.T) {
	src := `[
		
		1,
		
		2,
	]`
	root := mustParse(t, src)
	wantTag(t, root, "block")
	arr := kid(root, 0)
	wantTag(t, arr, "array")
	if len(arr) != 1+2 {
		t.Fatalf("want 2 elements, got %d\n%s", len(arr)-1, dump(root))
	}
	wantTag(t, kid(arr, 0), "int")
	wantTag(t, kid(arr, 1), "int")
}

func Test_SkipNoop_And_TrailingComma_InMap(t *testing.T) {
	src := `{
		
		name: "John",
		
		age: 17,
	}`
	root := mustParse(t, src)
	wantTag(t, root, "block")
	m := kid(root, 0)
	wantTag(t, m, "map")
	if len(m) != 1+2 {
		t.Fatalf("want 2 pairs, got %d\n%s", len(m)-1, dump(root))
	}
	// pair 0
	p0 := kid(m, 0)
	wantTag(t, p0, "pair")
	k0 := kid(p0, 0)
	v0 := kid(p0, 1)
	wantTag(t, k0, "str")
	wantTag(t, v0, "str")
	if k0[1].(string) != "name" || v0[1].(string) != "John" {
		t.Fatalf("bad first pair: %s", dump(p0))
	}
	// pair 1
	p1 := kid(m, 1)
	wantTag(t, p1, "pair")
	k1 := kid(p1, 0)
	v1 := kid(p1, 1)
	wantTag(t, k1, "str")
	wantTag(t, v1, "int")
	if k1[1].(string) != "age" || v1[1].(int64) != 17 {
		t.Fatalf("bad second pair: %s", dump(p1))
	}
}

func Test_TrailingComma_CallArgs(t *testing.T) {
	src := `f(1,
	     2,)`
	root := mustParse(t, src)
	call := kid(root, 0)
	wantTag(t, call, "call")
	callee := kid(call, 0)
	wantTag(t, callee, "id")
	if callee[1].(string) != "f" {
		t.Fatalf("callee not 'f': %s", dump(call))
	}
	if len(call) != 1+1+2 { // tag + callee + args(2)
		t.Fatalf("want 2 args, got %d\n%s", len(call)-2, dump(root))
	}
	wantTag(t, call[2].(S), "int")
	wantTag(t, call[3].(S), "int")
}

func Test_TrailingComma_FunctionParams(t *testing.T) {
	src := `fun(x: Int, 
	      y: Str,) do end`
	root := mustParse(t, src)
	fun := kid(root, 0)
	wantTag(t, fun, "fun")
	params := kid(fun, 0)
	ret := kid(fun, 1)
	body := kid(fun, 2)
	wantTag(t, params, "array")
	wantTag(t, ret, "id")
	wantTag(t, body, "block")
	if ret[1].(string) != "Any" {
		t.Fatalf("default return type not Any: %s", dump(fun))
	}
	if len(params) != 1+2 {
		t.Fatalf("want 2 params, got %d\n%s", len(params)-1, dump(fun))
	}
	p0 := kid(params, 0)
	p1 := kid(params, 1)
	wantTag(t, p0, "pair")
	wantTag(t, p1, "pair")
	if head(kid(p0, 0)) != "id" || kid(p0, 0)[1].(string) != "x" {
		t.Fatalf("bad first param: %s", dump(p0))
	}
	if head(kid(p1, 0)) != "id" || kid(p1, 0)[1].(string) != "y" {
		t.Fatalf("bad second param: %s", dump(p1))
	}
}

func Test_TrailingComma_InPatterns(t *testing.T) {
	src := `let [a, 
	         b,] = [1, 2]`
	root := mustParse(t, src)
	asn := kid(root, 0)
	wantTag(t, asn, "assign")
	lhs := asn[1].(S)
	rhs := asn[2].(S)
	wantTag(t, lhs, "darr")
	wantTag(t, rhs, "array")
	if len(lhs) != 1+2 || len(rhs) != 1+2 {
		t.Fatalf("bad sizes: lhs %d rhs %d\n%s", len(lhs)-1, len(rhs)-1, dump(root))
	}
	if head(kid(lhs, 0)) != "decl" || kid(lhs, 0)[1].(string) != "a" {
		t.Fatalf("first pattern element: %s", dump(lhs))
	}
	if head(kid(lhs, 1)) != "decl" || kid(lhs, 1)[1].(string) != "b" {
		t.Fatalf("second pattern element: %s", dump(lhs))
	}
}

func Test_SkipNoop_InIndex_And_ComputedProperty(t *testing.T) {
	srcIdx := "arr[\n  0 \n]"
	root := mustParse(t, srcIdx)
	idx := kid(root, 0)
	wantTag(t, idx, "idx")
	wantTag(t, kid(idx, 0), "id")
	wantTag(t, kid(idx, 1), "int")

	srcComp := "obj.(\n x + 1\n)"
	root = mustParse(t, srcComp)
	idx = kid(root, 0)
	wantTag(t, idx, "idx")
	obj := kid(idx, 0)
	ex := kid(idx, 1)
	wantTag(t, obj, "id")
	wantTag(t, ex, "binop")
	if obj[1].(string) != "obj" {
		t.Fatalf("object name mismatch: %s", dump(obj))
	}
	// binop "+"
	if ex[1].(string) != "+" {
		t.Fatalf("expected '+', got %q\n%s", ex[1], dump(ex))
	}
}

func Test_Dot_Zero_Name_Chain(t *testing.T) {
	// Requires the lexer fix that keeps '.' after property numeric index,
	// so token stream is: ID(arr) PERIOD INTEGER(0) PERIOD ID(name)
	src := `arr.0.name`
	root := mustParse(t, src)
	get := kid(root, 0)
	wantTag(t, get, "get")
	if head(kid(get, 1)) != "str" || kid(get, 1)[1].(string) != "name" {
		t.Fatalf("bad property tail: %s", dump(get))
	}
	idx := kid(get, 0)
	wantTag(t, idx, "idx")
	base := kid(idx, 0)
	num := kid(idx, 1)
	wantTag(t, base, "id")
	wantTag(t, num, "int")
	if base[1].(string) != "arr" || num[1].(int64) != 0 {
		t.Fatalf("bad base or index: %s", dump(get))
	}
}

func Test_CallVsGrouping_And_IndexVsArray(t *testing.T) {
	// call: CLROUND (no space)
	srcCall := `f( x )`
	root := mustParse(t, srcCall)
	call := kid(root, 0)
	wantTag(t, call, "call")
	wantTag(t, kid(call, 0), "id")
	wantTag(t, kid(call, 1), "id")

	// grouping (space forces LROUND) → two top-level exprs: "f" and "x"
	srcGroup := `f (x)`
	root = mustParse(t, srcGroup)
	wantTag(t, root, "block")
	if len(root) != 1+2 {
		t.Fatalf("want two top-level nodes, got %d\n%s", len(root)-1, dump(root))
	}
	wantTag(t, kid(root, 0), "id")
	wantTag(t, kid(root, 1), "id")

	// indexing: CLSQUARE (no space)
	srcIdx := `arr[i]`
	root = mustParse(t, srcIdx)
	idx := kid(root, 0)
	wantTag(t, idx, "idx")

	// array literal due to LSQUARE (space)
	srcArr := `arr [i]`
	root = mustParse(t, srcArr)
	if len(root) != 1+2 {
		t.Fatalf("want two top-level nodes, got %d\n%s", len(root)-1, dump(root))
	}
	wantTag(t, kid(root, 0), "id")
	wantTag(t, kid(root, 1), "array")
}

func Test_Interactive_Incomplete_WithTrailingComma(t *testing.T) {
	// Interactive mode should report incomplete if the closing delimiter is missing,
	// even with a trailing comma and intervening blank line.
	src := "[\n  1,\n"
	mustIncomplete(t, src)

	src = "{\n  a: 1,\n"
	mustIncomplete(t, src)

	src = "f(\n  1,\n"
	mustIncomplete(t, src)

	src = "fun(x: Int,\n"
	mustIncomplete(t, src)
}

// Trailing comma in OBJECT destructuring pattern (lhs) and in object literal (rhs).
func Test_TrailingComma_InObjectPattern(t *testing.T) {
	src := `let {a: x, 
	           b: y,} = {a: 1, b: 2,}`
	root := mustParse(t, src)
	asn := kid(root, 0)
	wantTag(t, asn, "assign")

	lhs := kid(asn, 0)
	rhs := kid(asn, 1)
	wantTag(t, lhs, "dobj")
	wantTag(t, rhs, "map")

	// LHS: two pairs a: x, b: y
	if len(lhs) != 1+2 {
		t.Fatalf("lhs object pattern size: want 2, got %d\n%s", len(lhs)-1, dump(lhs))
	}
	p0 := kid(lhs, 0)
	p1 := kid(lhs, 1)
	wantTag(t, p0, "pair")
	wantTag(t, p1, "pair")
	if k := kid(p0, 0)[1].(string); k != "a" {
		t.Fatalf("lhs first key != 'a': %q", k)
	}
	if head(kid(p0, 1)) != "decl" || kid(p0, 1)[1].(string) != "x" {
		t.Fatalf("lhs first val != decl x: %s", dump(kid(p0, 1)))
	}
	if k := kid(p1, 0)[1].(string); k != "b" {
		t.Fatalf("lhs second key != 'b': %q", k)
	}
	if head(kid(p1, 1)) != "decl" || kid(p1, 1)[1].(string) != "y" {
		t.Fatalf("lhs second val != decl y: %s", dump(kid(p1, 1)))
	}

	// RHS map: a:1, b:2 (with trailing comma)
	if len(rhs) != 1+2 {
		t.Fatalf("rhs map size: want 2, got %d\n%s", len(rhs)-1, dump(rhs))
	}
	q0 := kid(rhs, 0)
	q1 := kid(rhs, 1)
	wantTag(t, q0, "pair")
	wantTag(t, q1, "pair")
	if kid(q0, 0)[1].(string) != "a" || kid(q0, 1)[1].(int64) != 1 {
		t.Fatalf("rhs first pair mismatch: %s", dump(q0))
	}
	if kid(q1, 0)[1].(string) != "b" || kid(q1, 1)[1].(int64) != 2 {
		t.Fatalf("rhs second pair mismatch: %s", dump(q1))
	}
}

// Blank lines (NOOP) inside call argument lists are ignored like whitespace.
func Test_SkipNoop_Inside_CallArgs(t *testing.T) {
	src := "f(\n\n  1,\n  \n  2\n)"
	root := mustParse(t, src)
	call := kid(root, 0)
	wantTag(t, call, "call")
	if len(call) != 1+1+2 { // tag + callee + 2 args
		t.Fatalf("want 2 args, got %d\n%s", len(call)-2, dump(call))
	}
	wantTag(t, call[2].(S), "int")
	wantTag(t, call[3].(S), "int")
}

// Blank lines (NOOP) inside function parameter lists are ignored like whitespace.
func Test_SkipNoop_Inside_Params(t *testing.T) {
	src := "fun(\n\n  x: Int,\n  \n  y: Str\n) do end"
	root := mustParse(t, src)
	fn := kid(root, 0)
	wantTag(t, fn, "fun")
	params := kid(fn, 0)
	wantTag(t, params, "array")
	if len(params) != 1+2 {
		t.Fatalf("want 2 params, got %d\n%s", len(params)-1, dump(params))
	}
	// names x, y
	p0 := kid(params, 0)
	p1 := kid(params, 1)
	if head(kid(p0, 0)) != "id" || kid(p0, 0)[1].(string) != "x" {
		t.Fatalf("first param not x: %s", dump(p0))
	}
	if head(kid(p1, 0)) != "id" || kid(p1, 0)[1].(string) != "y" {
		t.Fatalf("second param not y: %s", dump(p1))
	}
}

func Test_Parser_Module_SimpleStringName_EmptyBody(t *testing.T) {
	root := mustParse(t, `module "M" do end`)
	wantTag(t, root, "block")
	if len(root) != 2 {
		t.Fatalf("want 1 child in root block, got %d\n%s", len(root)-1, dump(root))
	}
	mod := kid(root, 0)
	wantTag(t, mod, "module")
	name := kid(mod, 0)
	wantTag(t, name, "str")
	if name[1].(string) != "M" {
		t.Fatalf("module name mismatch, got %v", name[1])
	}
	body := kid(mod, 1)
	wantTag(t, body, "block")
	if len(body) != 1 { // empty body → only the "block" tag
		t.Fatalf("expected empty module body, got %s", dump(body))
	}
}

func Test_Parser_Module_NameIsExpression(t *testing.T) {
	root := mustParse(t, `module ("M" + "1") do end`)
	mod := kid(root, 0)
	wantTag(t, mod, "module")
	name := kid(mod, 0)
	wantTag(t, name, "binop")
	if name[1].(string) != "+" {
		t.Fatalf("want '+' in module name expression, got %v", name[1])
	}
}

func Test_Parser_Module_WithBody_Content(t *testing.T) {
	src := `
module "Core" do
  let x = 1
  return x
end`
	root := mustParse(t, src)
	mod := kid(root, 0)
	wantTag(t, mod, "module")

	// name
	name := kid(mod, 0)
	wantTag(t, name, "str")
	if name[1].(string) != "Core" {
		t.Fatalf("module name mismatch: %v", name[1])
	}

	// body
	body := kid(mod, 1)
	wantTag(t, body, "block")
	children := body[1:]
	if len(children) != 2 {
		t.Fatalf("want 2 items in body, got %d\n%s", len(children), dump(body))
	}

	assign := children[0].(S)
	wantTag(t, assign, "assign")
	tgt := kid(assign, 0)
	wantTag(t, tgt, "decl")
	if tgt[1].(string) != "x" {
		t.Fatalf("decl target mismatch: %v", tgt[1])
	}
	val := kid(assign, 1)
	wantTag(t, val, "int")
	if val[1].(int64) != 1 {
		t.Fatalf("assign value mismatch: %v", val[1])
	}

	ret := children[1].(S)
	wantTag(t, ret, "return")
	retExpr := kid(ret, 0)
	wantTag(t, retExpr, "id")
	if retExpr[1].(string) != "x" {
		t.Fatalf("return id mismatch: %v", retExpr[1])
	}
}

func Test_Parser_Module_Interactive_Incomplete_NoName(t *testing.T) {
	mustIncomplete(t, `module`)
	mustIncomplete(t, `module `)
}

func Test_Parser_Module_Interactive_Incomplete_NoDo(t *testing.T) {
	mustIncomplete(t, `module M`)
}

func Test_Parser_Module_Interactive_Incomplete_NoEnd(t *testing.T) {
	mustIncomplete(t, "module M do\n  let x = 1")
}

func Test_Parser_MapKey_AllowsModuleKeyword(t *testing.T) {
	root := mustParse(t, `{ module: 1 }`)
	obj := kid(root, 0)
	wantTag(t, obj, "map")
	items := obj[1:]
	if len(items) != 1 {
		t.Fatalf("want 1 pair in map, got %d\n%s", len(items), dump(obj))
	}
	pair := items[0].(S)
	if head(pair) != "pair" && head(pair) != "pair!" {
		t.Fatalf("want 'pair' node, got %q\n%s", head(pair), dump(pair))
	}
	key := kid(pair, 0)
	wantTag(t, key, "str")
	if key[1].(string) != "module" {
		t.Fatalf("key literal mismatch: %v", key[1])
	}
	val := kid(pair, 1)
	wantTag(t, val, "int")
	if val[1].(int64) != 1 {
		t.Fatalf("value mismatch: %v", val[1])
	}
}

func Test_Parser_PropertyAfterDot_ModuleKeywordForcedID(t *testing.T) {
	root := mustParse(t, `obj.module`)
	get := kid(root, 0)
	wantTag(t, get, "get")
	obj := kid(get, 0)
	wantTag(t, obj, "id")
	if obj[1].(string) != "obj" {
		t.Fatalf("object mismatch: %v", obj[1])
	}
	prop := kid(get, 1)
	wantTag(t, prop, "str")
	if prop[1].(string) != "module" {
		t.Fatalf("property name mismatch: %v", prop[1])
	}
}
