// parser_test.go
package mindscript

import (
	"encoding/json"
	"reflect"
	"strings"
	"testing"
)

//
// ──────────────────────────────── Helpers ─────────────────────────────────
//

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

func head(n S) string { return n[0].(string) }

// children start at index 1 (L("tag", kids...))
// For nodes with operator payloads (["binop", OP, LHS, RHS], ["unop", OP, EXPR]),
// index directly into the slice instead of using kid().
func kid(n S, i int) S { return n[i+1].(S) }

// pretty for failures
func dump(n S) string {
	b, _ := json.MarshalIndent(n, "", "  ")
	return string(b)
}

func checkParseErr(t *testing.T, src string, wantLine, wantCol int, wantSubstr string) {
	t.Helper()
	_, err := ParseSExpr(src)
	if err == nil {
		t.Fatalf("expected parse error, got nil\nsource:\n%q", src)
	}
	e, ok := err.(*Error)
	if !ok {
		t.Fatalf("expected *Error, got %T: %v", err, err)
	}
	if e.Kind != DiagParse {
		t.Fatalf("expected DiagParse, got kind=%v msg=%q", e.Kind, e.Msg)
	}
	if e.Line != wantLine || e.Col != wantCol {
		t.Fatalf("wrong location: want %d:%d, got %d:%d (msg=%q)\nsource:\n%q", wantLine, wantCol, e.Line, e.Col, e.Msg, src)
	}
	if wantSubstr != "" && !strings.Contains(e.Msg, wantSubstr) {
		t.Fatalf("error message does not contain %q: %q", wantSubstr, e.Msg)
	}
}

func checkParseInc(t *testing.T, src string, wantLine, wantCol int, wantSubstr string) {
	t.Helper()
	_, err := ParseSExprInteractive(src)
	if err == nil {
		t.Fatalf("expected incomplete parse error, got nil\nsource:\n%q", src)
	}
	e, ok := err.(*Error)
	if !ok {
		t.Fatalf("expected *Error, got %T: %v", err, err)
	}
	if e.Kind != DiagIncomplete {
		t.Fatalf("expected DiagIncomplete, got kind=%v msg=%q", e.Kind, e.Msg)
	}
	if e.Line != wantLine || e.Col != wantCol {
		t.Fatalf("wrong location: want %d:%d, got %d:%d (msg=%q)\nsource:\n%q", wantLine, wantCol, e.Line, e.Col, e.Msg, src)
	}
	if wantSubstr != "" && !strings.Contains(e.Msg, wantSubstr) {
		t.Fatalf("error message does not contain %q: %q", wantSubstr, e.Msg)
	}
}

// convenience
func first(root S) S { return kid(root, 0) }

func isStr(n S, s string) bool {
	return head(n) == "str" && n[1].(string) == s
}

func asAnnot(t *testing.T, n S) (txt string, wrapped S) {
	t.Helper()
	wantTag(t, n, "annot")
	txtNode := kid(n, 0)
	wantTag(t, txtNode, "str")
	raw := txtNode[1].(string)
	txt = raw
	wrapped = kid(n, 1)
	return
}

//
// ─────────────────────────── Literals & IDs ───────────────────────────────
//

func Test_Parser_Literal_Int_TagAndValue(t *testing.T) {
	root := mustParse(t, `42`)
	n := first(root)
	wantTag(t, n, "int")
	if n[1].(int64) != 42 {
		t.Fatalf("want 42, got %v", n[1])
	}
}

func Test_Parser_Literal_Num_TrailingDot_IsFloat(t *testing.T) {
	root := mustParse(t, `5.`)
	n := first(root)
	wantTag(t, n, "num")
	if n[1].(float64) != 5.0 {
		t.Fatalf("want 5.0, got %v", n[1])
	}
}

func Test_Parser_Literal_Str_TagAndValue(t *testing.T) {
	root := mustParse(t, `"hi"`)
	n := first(root)
	wantTag(t, n, "str")
	if n[1].(string) != "hi" {
		t.Fatalf("want 'hi', got %v", n[1])
	}
}

func Test_Parser_Literal_Bool_True_False(t *testing.T) {
	root := mustParse(t, `true false`)
	if !isBool(first(root), true) {
		t.Fatalf("first not true: %s", dump(root))
	}
	if !isBool(kid(root, 1), false) {
		t.Fatalf("second not false: %s", dump(root))
	}
}
func isBool(n S, v bool) bool { return head(n) == "bool" && n[1].(bool) == v }

func Test_Parser_Literal_Null_Tag(t *testing.T) {
	root := mustParse(t, `null`)
	wantTag(t, first(root), "null")
}

func Test_Parser_Identifier_Basic(t *testing.T) {
	root := mustParse(t, `x`)
	if !isId(first(root), "x") {
		t.Fatalf("want id x, got %s", dump(first(root)))
	}
}

//
// ─────────────────────── Precedence & Associativity ───────────────────────
//

func Test_Parser_Binary_Precedence_MulOverAdd(t *testing.T) {
	root := mustParse(t, `1 + 2 * 3`)
	expr := first(root)
	wantTag(t, expr, "binop")
	if expr[1].(string) != "+" {
		t.Fatalf("want '+', got %v", expr[1])
	}
	rhs := expr[3].(S)
	wantTag(t, rhs, "binop")
	if rhs[1].(string) != "*" {
		t.Fatalf("want '*', got %v", rhs[1])
	}
}

func Test_Parser_Unary_Negation_Shape(t *testing.T) {
	root := mustParse(t, `- (a + b)`)
	expr := first(root)
	wantTag(t, expr, "unop")
	if expr[1].(string) != "-" {
		t.Fatalf("want '-', got %v", expr[1])
	}
}

func Test_Parser_Assign_RightAssociative(t *testing.T) {
	root := mustParse(t, `a = b = 1`)
	a1 := first(root)
	wantTag(t, a1, "assign")
	a2 := a1[2].(S)
	wantTag(t, a2, "assign")
}

func Test_Parser_Comparison_Vs_Equality_Binding(t *testing.T) {
	root := mustParse(t, `a < b == c`)
	eq := first(root)
	wantTag(t, eq, "binop")
	if eq[1].(string) != "==" {
		t.Fatalf("want '==', got %v", eq[1])
	}
	cmp := eq[2].(S)
	wantTag(t, cmp, "binop")
	if cmp[1].(string) != "<" {
		t.Fatalf("want '<', got %v", cmp[1])
	}
}

func Test_Parser_OptionalPostfix_BindsTighterThanArrow(t *testing.T) {
	root := mustParse(t, `type Str? -> Int`)
	ty := first(root)
	wantTag(t, ty, "type")
	arrow := ty[1].(S)
	wantTag(t, arrow, "binop")
	if arrow[1].(string) != "->" {
		t.Fatalf("want '->', got %v", arrow[1])
	}
	left := arrow[2].(S)
	wantTag(t, left, "unop")
	if left[1].(string) != "?" || head(left[2].(S)) != "id" || left[2].(S)[1].(string) != "Str" {
		t.Fatalf("left not Str?: %s", dump(left))
	}
}

func Test_Parser_Arrow_RightAssociative_InType(t *testing.T) {
	root := mustParse(t, `type Int -> Str -> Bool`)
	ty := first(root)
	wantTag(t, ty, "type")
	arrow := ty[1].(S)
	wantTag(t, arrow, "binop")
	if arrow[1].(string) != "->" {
		t.Fatalf("top op not '->'")
	}
	if !isId(arrow[2].(S), "Int") {
		t.Fatalf("left not Int")
	}
	right := arrow[3].(S)
	wantTag(t, right, "binop")
	if right[1].(string) != "->" {
		t.Fatalf("right not nested '->'")
	}
}

//
// ─────────────────── Call/Grouping & Index/Array Split ────────────────────
//

func Test_Parser_Call_NoSpaceBeforeParen(t *testing.T) {
	root := mustParse(t, `f(x)`)
	call := first(root)
	wantTag(t, call, "call")
	if !isId(kid(call, 0), "f") || !isId(kid(call, 1), "x") {
		t.Fatalf("bad call: %s", dump(call))
	}
}

func Test_Parser_Grouping_SpaceBeforeParen_SplitsTopLevel(t *testing.T) {
	root := mustParse(t, `f (x)`)
	if len(root) != 1+2 {
		t.Fatalf("want two top-level exprs, got %d", len(root)-1)
	}
	if !isId(kid(root, 0), "f") || head(kid(root, 1)) != "id" {
		t.Fatalf("bad grouping split: %s", dump(root))
	}
}

func Test_Parser_Index_NoSpaceBeforeBracket(t *testing.T) {
	root := mustParse(t, `arr[i]`)
	wantTag(t, first(root), "idx")
}

func Test_Parser_ArrayLiteral_SpaceBeforeBracket(t *testing.T) {
	root := mustParse(t, `arr [i]`)
	if len(root) != 1+2 {
		t.Fatalf("want two top-level exprs, got %d", len(root)-1)
	}
	if !isId(kid(root, 0), "arr") || head(kid(root, 1)) != "array" {
		t.Fatalf("bad array literal due to LSQUARE: %s", dump(root))
	}
}

//
// ─────────────────────────── Dot / Property / Index ───────────────────────
//

func Test_Parser_Dot_Property_Get(t *testing.T) {
	root := mustParse(t, `obj.name`)
	get := first(root)
	wantTag(t, get, "get")
	if !isId(kid(get, 0), "obj") || !isStr(kid(get, 1), "name") {
		t.Fatalf("bad get: %s", dump(get))
	}
}

func Test_Parser_Dot_ComputedProperty_IndexExpr(t *testing.T) {
	root := mustParse(t, `obj.("na" + "me")`)
	idx := first(root)
	wantTag(t, idx, "idx")
	wantTag(t, kid(idx, 1), "binop")
}

func Test_Parser_Dot_Integer_DesugarsToIndex(t *testing.T) {
	root := mustParse(t, `obj.name.field(5).90`)
	top := first(root)
	wantTag(t, top, "idx")
	idx := kid(top, 1)
	wantTag(t, idx, "int")
	if idx[1].(int64) != 90 {
		t.Fatalf("want 90, got %v", idx[1])
	}
	call := kid(top, 0)
	wantTag(t, call, "call")
}

func Test_Parser_Dot_Zero_Then_Name_Chain(t *testing.T) {
	root := mustParse(t, `arr.0.name`)
	get := first(root)
	wantTag(t, get, "get")
	if !isStr(kid(get, 1), "name") {
		t.Fatalf("bad property tail: %s", dump(get))
	}
	idx := kid(get, 0)
	wantTag(t, idx, "idx")
	if !isId(kid(idx, 0), "arr") {
		t.Fatalf("base not arr: %s", dump(idx))
	}
	if head(kid(idx, 1)) != "int" || kid(idx, 1)[1].(int64) != 0 {
		t.Fatalf("index not 0: %s", dump(idx))
	}
}

func Test_Parser_PropertyAfterDot_KeywordForcedID(t *testing.T) {
	root := mustParse(t, `obj.module`)
	get := first(root)
	wantTag(t, get, "get")
	if !isStr(kid(get, 1), "module") {
		t.Fatalf("property name mismatch")
	}
}

//
// ───────────────────────── Collections (Arrays/Maps) ──────────────────────
//

func Test_Parser_Array_Empty(t *testing.T) {
	root := mustParse(t, `[]`)
	arr := first(root)
	wantTag(t, arr, "array")
	if len(arr) != 1 {
		t.Fatalf("want empty array, got %s", dump(arr))
	}
}

func Test_Parser_Array_Elements(t *testing.T) {
	root := mustParse(t, `[1,2,3]`)
	arr := first(root)
	wantTag(t, arr, "array")
	if len(arr) != 1+3 {
		t.Fatalf("want 3 elems, got %d", len(arr)-1)
	}
}

func Test_Parser_Map_BasicPairs(t *testing.T) {
	root := mustParse(t, `{name: "John", age: 25}`)
	m := first(root)
	wantTag(t, m, "map")
	if len(m) != 1+2 {
		t.Fatalf("want 2 pairs, got %d", len(m)-1)
	}
}

func Test_Parser_Map_RequiredField_Bang(t *testing.T) {
	root := mustParse(t, `type {name!: Str, age: Int}`)
	ty := first(root)
	wantTag(t, ty, "type")
	rec := ty[1].(S)
	wantTag(t, rec, "map")
	f1 := kid(rec, 0)
	if head(f1) != "pair!" {
		t.Fatalf("first field should be pair!: %s", dump(f1))
	}
}

func Test_Parser_Keywords_As_Map_Keys(t *testing.T) {
	root := mustParse(t, `{if:1, else:2, for:3, type:4, Enum:5, Int:6, Str:7, true:8, null:9}`)
	m := first(root)
	wantTag(t, m, "map")
	keys := []string{"if", "else", "for", "type", "Enum", "Int", "Str", "true", "null"}
	for i, want := range keys {
		k := kid(kid(m, i), 0)
		wantTag(t, k, "str")
		if k[1].(string) != want {
			t.Fatalf("key %d want %q, got %q", i, want, k[1].(string))
		}
	}
}

func Test_Parser_MapKey_AllowsModuleKeyword(t *testing.T) {
	root := mustParse(t, `{ module: 1 }`)
	obj := first(root)
	wantTag(t, obj, "map")
	pair := kid(obj, 0)
	key := kid(pair, 0)
	wantTag(t, key, "str")
	if key[1].(string) != "module" {
		t.Fatalf("key literal mismatch")
	}
}

//
// ───────────────────────── Trailing commas (atomic) ───────────────────────
//

func Test_Parser_TrailingComma_Array(t *testing.T) {
	root := mustParse(t, "[\n  1,\n  2,\n]")
	arr := first(root)
	wantTag(t, arr, "array")
	if len(arr) != 1+2 {
		t.Fatalf("want 2 elems, got %d", len(arr)-1)
	}
}

func Test_Parser_TrailingComma_Map(t *testing.T) {
	root := mustParse(t, "{\n  a: 1,\n  b: 2,\n}")
	m := first(root)
	wantTag(t, m, "map")
	if len(m) != 1+2 {
		t.Fatalf("want 2 pairs, got %d", len(m)-1)
	}
}

func Test_Parser_TrailingComma_CallArgs(t *testing.T) {
	root := mustParse(t, "f(1,\n  2,)")
	call := first(root)
	wantTag(t, call, "call")
	if len(call) != 1+1+2 {
		t.Fatalf("want 2 args, got %d", len(call)-2)
	}
}

func Test_Parser_TrailingComma_FunctionParams(t *testing.T) {
	root := mustParse(t, "fun(x: Int,\n  y: Str,) do end")
	fun := first(root)
	wantTag(t, fun, "fun")
	params := kid(fun, 0)
	wantTag(t, params, "array")
	if len(params) != 1+2 {
		t.Fatalf("want 2 params, got %d", len(params)-1)
	}
}

func Test_Parser_TrailingComma_InPatterns_ObjectAndArray(t *testing.T) {
	root := mustParse(t, `let {a: x, b: y,} = {a: 1, b: 2,}`)
	asn := first(root)
	wantTag(t, asn, "assign")
	lhs := kid(asn, 0)
	rhs := kid(asn, 1)
	wantTag(t, lhs, "dobj")
	wantTag(t, rhs, "map")
	if len(lhs) != 1+2 || len(rhs) != 1+2 {
		t.Fatalf("bad sizes: lhs=%d rhs=%d", len(lhs)-1, len(rhs)-1)
	}
}

//
// ─────────────────────── Blank lines (NOOP) handling ──────────────────────
//

func Test_Parser_NOOP_TopLevel_And_InsideBlock(t *testing.T) {
	src := `
# annot 1
# annot 2

# annot 3
do
  let x
  
  # annot 4
  # annot 5
  let y
end
`
	root := mustParse(t, src)
	// Top-level should have 2 children: annot wrapping NOOP, and annot wrapping block
	if len(root) != 1+2 {
		t.Fatalf("want 2 top-level children, got %d\n%s", len(root)-1, dump(root))
	}
	annNoop := kid(root, 0)
	_, wrapped := asAnnot(t, annNoop)
	wantTag(t, wrapped, "noop")

	annBlock := kid(root, 1)
	_, wrapped2 := asAnnot(t, annBlock)
	wantTag(t, wrapped2, "block")

	blk := wrapped2
	if len(blk) != 1+3 {
		t.Fatalf("want 3 block kids, got %d", len(blk)-1)
	}
	wantTag(t, kid(blk, 1), "noop")
}

func Test_Parser_NOOP_Inside_CallArgs_Ignored(t *testing.T) {
	root := mustParse(t, "f(\n\n  1,\n  \n  2\n)")
	call := first(root)
	wantTag(t, call, "call")
	if len(call) != 1+1+2 {
		t.Fatalf("want 2 args, got %d", len(call)-2)
	}
}

func Test_Parser_NOOP_Inside_Params_Ignored(t *testing.T) {
	root := mustParse(t, "fun(\n\n  x: Int,\n  \n  y: Str\n) do end")
	fn := first(root)
	wantTag(t, fn, "fun")
	params := kid(fn, 0)
	if len(params) != 1+2 {
		t.Fatalf("want 2 params, got %d", len(params)-1)
	}
}

func Test_Parser_NOOP_Inside_Index_And_ComputedProperty_Ignored(t *testing.T) {
	root := mustParse(t, "arr[\n  0 \n]")
	idx := first(root)
	wantTag(t, idx, "idx")
	root = mustParse(t, "obj.(\n x + 1\n)")
	idx = first(root)
	wantTag(t, idx, "idx")
}

//
// ───────────────────────────── Control forms ──────────────────────────────
//

func Test_Parser_Control_Newline_DefaultsToNull_And_NextExpr(t *testing.T) {
	root := mustParse(t, "return\n(1 + 2)")
	if len(root) != 1+2 {
		t.Fatalf("want two top-level exprs, got %d", len(root)-1)
	}
	ret := kid(root, 0)
	wantTag(t, ret, "return")
	wantTag(t, ret[1].(S), "null")
}

func Test_Parser_Control_BareOnNewlines_DefaultsToNull(t *testing.T) {
	root := mustParse(t, "return\nbreak\ncontinue\n")
	if len(root) != 1+3 {
		t.Fatalf("want 3 stmts, got %d", len(root)-1)
	}
	for i, tag := range []string{"return", "break", "continue"} {
		stmt := kid(root, i)
		wantTag(t, stmt, tag)
		wantTag(t, stmt[1].(S), "null")
	}
}

func Test_Parser_Control_SameLine_ChainsAsSingleExpr(t *testing.T) {
	root := mustParse(t, `return break continue`)
	if len(root) != 1+1 {
		t.Fatalf("want single top-level chained expr")
	}
	r := first(root)
	wantTag(t, r, "return")
	b := r[1].(S)
	wantTag(t, b, "break")
	c := b[1].(S)
	wantTag(t, c, "continue")
	wantTag(t, c[1].(S), "null")
}

func Test_Parser_Control_Return_ParensAndSpacing_AreEquivalent(t *testing.T) {
	a := mustParse(t, `return 1`)
	b := mustParse(t, `return(1)`)
	ja, _ := json.Marshal(first(a))
	jb, _ := json.Marshal(first(b))
	if string(ja) != string(jb) {
		t.Fatalf("ASTs differ:\n%s\n%s", string(ja), string(jb))
	}
}

//
// ─────────────────────────── Functions & Oracles ──────────────────────────
//

func Test_Parser_Function_ParamType_And_DefaultReturnAny(t *testing.T) {
	root := mustParse(t, `fun(a: Str) do end`)
	fn := first(root)
	wantTag(t, fn, "fun")
	if head(kid(fn, 1)) != "id" || kid(fn, 1)[1].(string) != "Any" {
		t.Fatalf("default return must be Any")
	}
}

func Test_Parser_Function_WithReturnTypeExpr(t *testing.T) {
	root := mustParse(t, `fun(a: Str) -> Str do end`)
	fn := first(root)
	wantTag(t, fn, "fun")
	ret := kid(fn, 1)
	wantTag(t, ret, "id")
	if ret[1].(string) != "Str" {
		t.Fatalf("return type mismatch")
	}
}

func Test_Parser_Oracle_From_Array_NoSpaceOK(t *testing.T) {
	root := mustParse(t, `oracle() from["web","docs"]`)
	orc := first(root)
	wantTag(t, orc, "oracle")
	if head(kid(orc, 2)) != "array" {
		t.Fatalf("oracle sources not array")
	}
}

//
// ─────────────────────── Type wrapper & optional postfix ──────────────────
//

func Test_Parser_Type_WrapsExpression_Array(t *testing.T) {
	root := mustParse(t, `type [Int]`)
	ty := first(root)
	wantTag(t, ty, "type")
	wantTag(t, ty[1].(S), "array")
}

func Test_Parser_Type_Optional_Postfix_Simple(t *testing.T) {
	root := mustParse(t, `type Str?`)
	ty := first(root)
	wantTag(t, ty, "type")
	opt := ty[1].(S)
	wantTag(t, opt, "unop")
	if opt[1].(string) != "?" || !isId(opt[2].(S), "Str") {
		t.Fatalf("bad optional Str?: %s", dump(opt))
	}
}

func Test_Parser_Type_Optional_Postfix_Array(t *testing.T) {
	root := mustParse(t, `type [Str]?`)
	ty := first(root)
	wantTag(t, ty, "type")
	opt := ty[1].(S)
	wantTag(t, opt, "unop")
	if opt[1].(string) != "?" || head(opt[2].(S)) != "array" {
		t.Fatalf("bad optional [Str]?: %s", dump(opt))
	}
}

func Test_Parser_Type_Optional_Inside_Record_Fields(t *testing.T) {
	root := mustParse(t, `type {name: Str?, hobbies: [Str]?}`)
	mp := first(root)[1].(S)
	wantTag(t, mp, "map")
	// name: Str?
	p := kid(mp, 0)
	val := p[2].(S)
	wantTag(t, val, "unop")
	if val[1].(string) != "?" {
		t.Fatalf("name not optional")
	}
	// hobbies: [Str]?
	p = kid(mp, 1)
	val = p[2].(S)
	wantTag(t, val, "unop")
	if val[1].(string) != "?" || head(val[2].(S)) != "array" {
		t.Fatalf("hobbies not optional array")
	}
}

//
// ───────────────────────── Destructuring patterns ─────────────────────────
//

func Test_Parser_Let_SimpleDecl(t *testing.T) {
	root := mustParse(t, `let x`)
	decl := first(root)
	wantTag(t, decl, "decl")
	if decl[1].(string) != "x" {
		t.Fatalf("decl != x")
	}
}

func Test_Parser_Let_ArrayDestructuring_AssignShape(t *testing.T) {
	root := mustParse(t, `let [x, y] = [1, 2]`)
	asn := first(root)
	wantTag(t, asn, "assign")
	wantTag(t, kid(asn, 0), "darr")
	wantTag(t, kid(asn, 1), "array")
}

func Test_Parser_Let_ObjectDestructuring_AssignShape(t *testing.T) {
	root := mustParse(t, `let {name: x, age: y} = obj`)
	asn := first(root)
	wantTag(t, asn, "assign")
	wantTag(t, kid(asn, 0), "dobj")
	if !isId(kid(asn, 1), "obj") {
		t.Fatalf("rhs not obj")
	}
}

func Test_Parser_Let_NestedPatterns(t *testing.T) {
	root := mustParse(t, `let {pt: [x, y]} = m`)
	asn := first(root)
	obj := kid(asn, 0)
	wantTag(t, obj, "dobj")
	p := kid(obj, 0)
	if kid(p, 0)[1].(string) != "pt" {
		t.Fatalf("key != pt")
	}
	wantTag(t, kid(p, 1), "darr")
}

func Test_Parser_Let_Destructuring_MustHaveEquals(t *testing.T) {
	if _, err := ParseSExpr(`let [x, y]`); err == nil {
		t.Fatalf("expected error for missing '=' after destructuring let")
	}
}

//
// ─────────────────────────────── For/While ────────────────────────────────
//

func Test_Parser_For_Target_Decl(t *testing.T) {
	root := mustParse(t, `for let x in xs do end`)
	fr := first(root)
	wantTag(t, fr, "for")
	wantTag(t, kid(fr, 0), "decl")
}

func Test_Parser_For_Target_Index(t *testing.T) {
	root := mustParse(t, `for a[i] in xs do end`)
	fr := first(root)
	wantTag(t, fr, "for")
	wantTag(t, kid(fr, 0), "idx")
}

func Test_Parser_For_InvalidTarget_Number(t *testing.T) {
	_, err := ParseSExpr(`for 1 in xs do end`)
	if err == nil || !strings.Contains(err.Error(), "invalid for-target") {
		t.Fatalf("expected invalid for-target error, got %v", err)
	}
}

func Test_Parser_For_ArrayPattern_NoLet(t *testing.T) {
	root := mustParse(t, `for [k, v] in obj do end`)
	fr := first(root)
	wantTag(t, fr, "for")
	wantTag(t, kid(fr, 0), "darr")
}

func Test_Parser_For_ArrayPattern_WithLet(t *testing.T) {
	root := mustParse(t, `for let [k, v] in obj do end`)
	fr := first(root)
	wantTag(t, fr, "for")
	wantTag(t, kid(fr, 0), "darr")
}

func Test_Parser_For_ObjectPattern(t *testing.T) {
	root := mustParse(t, `for {name: n, age: a} in people do end`)
	fr := first(root)
	wantTag(t, fr, "for")
	wantTag(t, kid(fr, 0), "dobj")
}

func Test_Parser_While_Basic_Shape(t *testing.T) {
	root := mustParse(t, `while true do x end`)
	w := first(root)
	wantTag(t, w, "while")
	wantTag(t, kid(w, 0), "bool")
	wantTag(t, kid(w, 1), "block")
}

func Test_Parser_While_With_Grouping(t *testing.T) {
	root := mustParse(t, `while (1 < 2) do end`)
	w := first(root)
	wantTag(t, w, "while")
	wantTag(t, kid(w, 0), "binop")
}

//
// ─────────────────────────────── Modules ──────────────────────────────────
//

func Test_Parser_Module_SimpleName_EmptyBody(t *testing.T) {
	root := mustParse(t, `module "M" do end`)
	mod := first(root)
	wantTag(t, mod, "module")
	wantTag(t, kid(mod, 0), "str")
	wantTag(t, kid(mod, 1), "block")
	if kid(mod, 0)[1].(string) != "M" {
		t.Fatalf("module name mismatch")
	}
	if len(kid(mod, 1)) != 1 {
		t.Fatalf("expected empty module body")
	}
}

func Test_Parser_Module_NameIsExpression(t *testing.T) {
	root := mustParse(t, `module ("M" + "1") do end`)
	mod := first(root)
	wantTag(t, kid(mod, 0), "binop")
}

func Test_Parser_Module_WithBody_Content(t *testing.T) {
	root := mustParse(t, `
module "Core" do
  let x = 1
  return x
end`)
	mod := first(root)
	wantTag(t, mod, "module")
	body := kid(mod, 1)
	if len(body) != 1+2 {
		t.Fatalf("want 2 items in body")
	}
	assign := kid(body, 0)
	ret := kid(body, 1)
	wantTag(t, assign, "assign")
	wantTag(t, ret, "return")
}

func Test_Parser_Module_Interactive_Incomplete_NoNameOrDoOrEnd(t *testing.T) {
	mustIncomplete(t, `module`)
	mustIncomplete(t, `module `)
	mustIncomplete(t, `module M`)
	mustIncomplete(t, "module M do\n  let x = 1")
}

//
// ───────────────────────────── Annotations ────────────────────────────────
//

func Test_Parser_Annot_PRE_WrapsNextExpr(t *testing.T) {
	root := mustParse(t, "# hello\nx")
	ann := first(root)
	txt, wrapped := asAnnot(t, ann)
	if txt != "hello" {
		t.Fatalf("want PRE 'hello'")
	}
	wantTag(t, wrapped, "id")
}

func Test_Parser_Annot_POST_WrapsPreviousExpr_TopLevel(t *testing.T) {
	root := mustParse(t, `(4+5)*8 # This`)
	ann := first(root)
	txt, wrapped := asAnnot(t, ann)
	if txt != "This" {
		t.Fatalf("want POST 'This'")
	}
	wantTag(t, wrapped, "binop")
	if wrapped[1].(string) != "*" {
		t.Fatalf("wrapped not product")
	}
}

func Test_Parser_Annot_POST_AttachesToWholeCallChain(t *testing.T) {
	root := mustParse(t, `obj.name(1)[i] # post `)
	ann := first(root)
	txt, wrapped := asAnnot(t, ann)
	if !strings.HasPrefix(txt, "post") {
		t.Fatalf("want POST 'post'")
	}
	wantTag(t, wrapped, "idx")
}

func Test_Parser_Annot_PRE_On_RHS_Map(t *testing.T) {
	root := mustParse(t, "let obj = \n# 4. pre map\n{ a: 1 }")
	asn := first(root)
	val := kid(asn, 1)
	txt, wrapped := asAnnot(t, val)
	if txt != "4. pre map" {
		t.Fatalf("want PRE '4. pre map'")
	}
	wantTag(t, wrapped, "map")
}

func Test_Parser_Annot_Control_PRE_BeforeReturn_BindsToValue(t *testing.T) {
	root := mustParse(t, "# this\nreturn 0")
	ret := first(root)
	wantTag(t, ret, "return")
	valAnn := ret[1].(S)
	txt, wrapped := asAnnot(t, valAnn)
	if txt != "this" || head(wrapped) != "int" {
		t.Fatalf("bad PRE on return value")
	}
}

func Test_Parser_Annot_Control_PRE_InlineValue_BindsToValue(t *testing.T) {
	root := mustParse(t, "return # this\n0")
	ret := first(root)
	valAnn := ret[1].(S)
	txt, wrapped := asAnnot(t, valAnn)
	if txt != "this" || head(wrapped) != "int" {
		t.Fatalf("bad PRE on return inline value")
	}
}

//
// ───────────────────────────── AST equivalences ───────────────────────────
//

func Test_Parser_Equivalence_Return_BriefVsParensChain(t *testing.T) {
	// One explicit equivalence test is enough.
	a1 := mustParse(t, `return(1) break(0) continue(null)`)
	a2 := mustParse(t, `return 1 break 0 continue null`)
	for i := 0; i < 3; i++ {
		n1 := a1[i+1].(S)
		n2 := a2[i+1].(S)
		if !reflect.DeepEqual(n1, n2) {
			t.Fatalf("ASTs differ for statement %d:\nwith parens: %s\nno parens:  %s", i, dump(n1), dump(n2))
		}
	}
}

//
// ───────────────────────────── Assign targets ─────────────────────────────
//

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

//
// ────────────────────────────── Error cases ───────────────────────────────
//

func Test_Parser_Error_UnexpectedToken_AtStart(t *testing.T) {
	checkParseErr(t, "!", 1, 1, "unexpected token '!'")
}

func Test_Parser_Error_Missing_RParen_InCall(t *testing.T) {
	_, err := ParseSExpr(`f(1`)
	if err == nil || !strings.Contains(strings.ToLower(err.Error()), "expected ')'") {
		t.Fatalf("expected missing ')' error, got %v", err)
	}
}

func Test_Parser_Error_Group_Missing_RPAREN(t *testing.T) {
	src := "(a"
	checkParseErr(t, src, 1, len(src)+1, "expected ')'")
}

func Test_Parser_Error_Array_Missing_RSQUARE(t *testing.T) {
	src := "[1,2"
	checkParseErr(t, src, 1, len(src)+1, "expected ']'")
}

func Test_Parser_Error_If_Missing_THEN_At_Do(t *testing.T) {
	src := "if x do end"
	checkParseErr(t, src, 1, 6, "expected 'then'")
}

func Test_Parser_Error_InvalidAssignmentTarget_AtEquals(t *testing.T) {
	src := "1 = 2"
	checkParseErr(t, src, 1, 3, "invalid assignment target")
}

func Test_Parser_Error_Map_Missing_Colon_After_Key_AtValue(t *testing.T) {
	src := "{x 1}"
	checkParseErr(t, src, 1, 4, "expected ':'")
}

func Test_Parser_Error_Fun_Params_Require_CLROUND(t *testing.T) {
	src := "fun (x: Int) do end"
	checkParseErr(t, src, 1, 5, "expected '(' to start parameters")
}

func Test_Parser_Error_Params_Missing_RPAREN_At_Do(t *testing.T) {
	src := "fun(x:Int do end"
	checkParseErr(t, src, 1, 11, "expected ')' after parameters")
}

func Test_Parser_Error_ComputedProperty_Missing_RPAREN(t *testing.T) {
	src := "a.(b"
	checkParseErr(t, src, 1, len(src)+1, "expected ')' after computed property")
}

func Test_Parser_Error_For_Target_Invalid_AtNumber(t *testing.T) {
	src := "for 1 in xs do end"
	checkParseErr(t, src, 1, 5, "invalid for-target")
}

func Test_Parser_Error_While_Missing_DO_AtEnd(t *testing.T) {
	src := "while x end"
	checkParseErr(t, src, 1, 9, "expected 'do'")
}

//
// ─────────────────────── Incomplete (interactive) ─────────────────────────
//

func Test_Parser_Incomplete_Grouping_IsIncomplete(t *testing.T) {
	mustIncomplete(t, "let x = (")
}

func Test_Parser_Incomplete_Block_IsIncomplete(t *testing.T) {
	mustIncomplete(t, "do x")
	mustIncomplete(t, "if a then x")
}

func Test_Parser_Incomplete_Params_IsIncomplete(t *testing.T) {
	mustIncomplete(t, "fun(a: Str")
}

func Test_Parser_Incomplete_WithTrailingComma_InDelimiters(t *testing.T) {
	mustIncomplete(t, "[\n  1,\n")
	mustIncomplete(t, "{\n  a: 1,\n")
	mustIncomplete(t, "f(\n  1,\n")
	mustIncomplete(t, "fun(x: Int,\n")
}

func Test_Parser_Incomplete_Vs_Normal_Completes_WhenClosed(t *testing.T) {
	full := "let x = (\n  1 + 1\n)\n"
	a := mustParseInteractive(t, full)
	b := mustParse(t, full)
	ja, _ := json.Marshal(a)
	jb, _ := json.Marshal(b)
	if string(ja) != string(jb) {
		t.Fatalf("interactive vs normal AST differ:\n%s\n%s", string(ja), string(jb))
	}
}

// --- add-backs: focused tests for unique behaviors that were missing --------

func Test_Parser_Grouping_Affects_Precedence_Restored(t *testing.T) {
	// (1 + 2) * 3  ==>  (* (+ 1 2) 3)
	root := mustParse(t, `(1 + 2) * 3`)
	top := kid(root, 0)
	wantTag(t, top, "binop")
	if top[1].(string) != "*" {
		t.Fatalf("want '*', got %v", top[1])
	}
	lhs := top[2].(S) // binop lhs
	wantTag(t, lhs, "binop")
	if lhs[1].(string) != "+" {
		t.Fatalf("want '+', got %v", lhs[1])
	}
}

func Test_Parser_Call_Index_Get_Chaining_WithQuotedTail(t *testing.T) {
	// obj.name(1, 2)[i]."weird"
	root := mustParse(t, `obj.name(1, 2)[i]."weird"`)
	e := kid(root, 0)
	wantTag(t, e, "get")
	prop := kid(e, 1)
	wantTag(t, prop, "str")
	if prop[1].(string) != "weird" {
		t.Fatalf("want quoted prop '\"weird\"', got %s", dump(prop))
	}
	in1 := kid(e, 0)
	wantTag(t, in1, "idx")
	call := kid(in1, 0)
	wantTag(t, call, "call")
	callee := kid(call, 0)
	wantTag(t, callee, "get")
}

func Test_Parser_Block_Do_End_Minimal(t *testing.T) {
	root := mustParse(t, `do x end`)
	blk := kid(root, 0)
	wantTag(t, blk, "block")
	if len(blk) != 2 || head(kid(blk, 0)) != "id" {
		t.Fatalf("unexpected block: %s", dump(blk))
	}
}

func Test_Parser_If_Elif_Else_ArmsShape(t *testing.T) {
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

func Test_Parser_Control_SameLine_Grouping_OK_SpacedCall(t *testing.T) {
	// return(1) vs return (1)
	r1 := mustParse(t, `return(1)`)
	r2 := mustParse(t, `return (1)`)
	j1, _ := json.Marshal(kid(r1, 0))
	j2, _ := json.Marshal(kid(r2, 0))
	if string(j1) != string(j2) {
		t.Fatalf("ASTs should match for return(1) vs return (1)\n%s\n%s", string(j1), string(j2))
	}
}

func Test_Parser_ArrayLiteral_AfterArrayLiteral_Adjacent(t *testing.T) {
	root := mustParse(t, `[] [1,2]`)
	if len(root) != 3 {
		t.Fatalf("unexpected top-level arity: %s", dump(root))
	}
	wantTag(t, kid(root, 0), "array")
	wantTag(t, kid(root, 1), "array")
}

func Test_Parser_ComputedDotIndex_Assign_Object(t *testing.T) {
	// obj.("na" + "me") = "Juan"   → assign(idx(obj, binop+), "Juan")
	root := mustParse(t, `obj.("na" + "me") = "Juan"`)
	assign := kid(root, 0)
	wantTag(t, assign, "assign")
	lhs := kid(assign, 0)
	wantTag(t, lhs, "idx")
	wantTag(t, kid(lhs, 0), "id")    // obj
	wantTag(t, kid(lhs, 1), "binop") // ("+" ...)
	wantTag(t, kid(assign, 1), "str")
}

func Test_Parser_ComputedDotIndex_Assign_Array(t *testing.T) {
	// arr.(0 + 1) = "Juan"   → assign(idx(arr, binop+), "Juan")
	root := mustParse(t, `arr.(0 + 1) = "Juan"`)
	assign := kid(root, 0)
	wantTag(t, assign, "assign")
	lhs := kid(assign, 0)
	wantTag(t, lhs, "idx")
	wantTag(t, kid(lhs, 0), "id")    // arr
	wantTag(t, kid(lhs, 1), "binop") // ("+" ...)
	wantTag(t, kid(assign, 1), "str")
}

func Test_Parser_Control_Newline_SplitsThenAnnotatedExpr(t *testing.T) {
	// "return" on its own line, then an annotated expr statement.
	r := mustParse(t, "return\n# pre\nx")
	if len(r) != 3 {
		t.Fatalf("want 2 statements, got %d", len(r)-1)
	}
	wantTag(t, kid(r, 0), "return")
	wantTag(t, kid(r, 1), "annot")
}

func Test_Parser_TrailingComma_InArrayPattern_LHS(t *testing.T) {
	src := `let [a, 
	         b,] = [1, 2]`
	root := mustParse(t, src)
	asn := kid(root, 0)
	wantTag(t, asn, "assign")
	lhs := kid(asn, 0)
	rhs := kid(asn, 1)
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

func Test_Parser_Incomplete_BinaryOp_RHS_AnchoredAtOp(t *testing.T) {
	// "1 +" → incomplete; anchored at '+' (column 3).
	src := "1 +"
	checkParseInc(t, src, 1, 3, "expected expression after operator")
}

func Test_Parser_ComputedDotIndex_Read_Shape(t *testing.T) {
	// obj.("na" + "me") → idx(obj, ("binop","+","na","me"))
	root := mustParse(t, `obj.("na" + "me")`)
	expr := kid(root, 0)
	wantTag(t, expr, "idx")
	wantTag(t, kid(expr, 0), "id") // obj
	ix := kid(expr, 1)
	wantTag(t, ix, "binop")
	if ix[1].(string) != "+" {
		t.Fatalf("want '+', got %v", ix[1])
	}
}

func Test_Parser_Oracle_From_NoSpace_OK(t *testing.T) {
	// Parser should allow no space between 'from' and '['.
	if _, err := ParseSExpr(`oracle() from["web","docs"]`); err != nil {
		t.Fatalf("unexpected parse error: %v", err)
	}
}

func Test_Parser_Annot_PRE_POST_Merge_ToSinglePRE(t *testing.T) {
	root := mustParse(t, "# pre\nx # post")
	ann := first(root)
	txt, wrapped := asAnnot(t, ann)
	if txt != "pre\npost" {
		t.Fatalf("merge text mismatch: %q", txt)
	}
	wantTag(t, wrapped, "id")
}

func Test_Parser_Annot_AfterBlankLine_WrapsNOOP(t *testing.T) {
	root := mustParse(t, "x\n\n# post")
	if len(root) != 1+3 {
		t.Fatalf("want 3 top-level exprs, got %d", len(root)-1)
	}
	wantTag(t, kid(root, 0), "id")   // x
	wantTag(t, kid(root, 1), "noop") // the blank line run
	ann := kid(root, 2)
	txt, wrapped := asAnnot(t, ann)
	if txt != "post" {
		t.Fatalf("expected PRE 'post', got: %s", dump(ann))
	}
	wantTag(t, wrapped, "noop") // lone PRE at EOF wraps NOOP
}

func Test_Parser_Let_Destructuring_MissingEquals_Anchored(t *testing.T) {
	src := "let [x, y]"
	checkParseErr(t, src, 1, len(src)+1, "expected '=' after destructuring let pattern")
}

func Test_Parser_Oracle_Defaults_NoFrom_NoArrow(t *testing.T) {
	root := mustParse(t, `oracle()`)
	orc := first(root)
	wantTag(t, orc, "oracle")
	// params: empty array
	wantTag(t, kid(orc, 0), "array")
	if len(kid(orc, 0)) != 1 {
		t.Fatalf("params should be empty array")
	}
	// out type defaults to Any
	if !isId(kid(orc, 1), "Any") {
		t.Fatalf("default out type should be Any")
	}
	// src defaults to empty array
	wantTag(t, kid(orc, 2), "array")
	if len(kid(orc, 2)) != 1 {
		t.Fatalf("default src should be empty array")
	}
}

func Test_Parser_Enum_Literal_Array(t *testing.T) {
	root := mustParse(t, `Enum["A","B"]`)
	en := first(root)
	wantTag(t, en, "enum")
	if len(en) != 1+2 {
		t.Fatalf("want 2 enum items, got %d", len(en)-1)
	}
	if !isStr(kid(en, 0), "A") || !isStr(kid(en, 1), "B") {
		t.Fatalf("enum items mismatch: %s", dump(en))
	}
}

// 2) Annotation after a blank line is PRE on next stmt (doesn't attach back)
func Test_Parser_Annot_AfterBlankLine_IsPREOnNextStmt(t *testing.T) {
	root := mustParse(t, "x\n\n# note\ny")
	if len(root) != 1+3 {
		t.Fatalf("want 3 top-level exprs, got %d", len(root)-1)
	}
	wantTag(t, kid(root, 0), "id")
	wantTag(t, kid(root, 1), "noop") // the blank-line NOOP
	ann := kid(root, 2)
	txt, wrapped := asAnnot(t, ann)
	if txt != "note" || !isId(wrapped, "y") {
		t.Fatalf("expected PRE 'note' wrapping y: %s", dump(ann))
	}
}

// 5) Enum literal basic shape
func Test_Parser_Enum_Literal(t *testing.T) {
	root := mustParse(t, `Enum["A","B"]`)
	en := first(root)
	wantTag(t, en, "enum")
	if len(en) != 3 || !isStr(kid(en, 0), "A") || !isStr(kid(en, 1), "B") {
		t.Fatalf("enum items mismatch: %s", dump(en))
	}
}

func Test_Parser_Annot_TrailingPRE_WrapsNOOP(t *testing.T) {
	root := mustParse(t, "# note") // lone PRE at EOF
	ann := first(root)
	txt, wrapped := asAnnot(t, ann)
	if txt != "note" {
		t.Fatalf("expected PRE 'note'")
	}
	wantTag(t, wrapped, "noop")
}

func Test_Parser_Return_Null_When_ImmediateEnd(t *testing.T) {
	root := mustParse(t, `do return end`)
	blk := first(root)
	wantTag(t, blk, "block")
	stmt := kid(blk, 0)
	wantTag(t, stmt, "return")
	val := kid(stmt, 0)
	wantTag(t, val, "null")
}

func Test_Parser_Break_Null_When_ImmediateEnd(t *testing.T) {
	root := mustParse(t, `do break end`)
	blk := first(root)
	wantTag(t, blk, "block")
	stmt := kid(blk, 0)
	wantTag(t, stmt, "break")
	val := kid(stmt, 0)
	wantTag(t, val, "null")
}

func Test_Parser_Continue_Null_When_ImmediateEnd(t *testing.T) {
	root := mustParse(t, `do continue end`)
	blk := first(root)
	wantTag(t, blk, "block")
	stmt := kid(blk, 0)
	wantTag(t, stmt, "continue")
	val := kid(stmt, 0)
	wantTag(t, val, "null")
}

func Test_Parser_Params_Type_PRE_AnnotationOnType(t *testing.T) {
	src := `
fun(
  n:
    # must be positive
    Int
) -> Any do end
`
	root := mustParse(t, src)
	fn := first(root)
	wantTag(t, fn, "fun")
	params := kid(fn, 0)
	wantTag(t, params, "array")

	p0 := kid(params, 0)
	wantTag(t, p0, "pair")

	// Type position should be a PRE annot wrapping id "Int"
	typ := kid(p0, 1)
	txt, wrapped := asAnnot(t, typ)
	if txt != "must be positive" {
		t.Fatalf("want PRE text %q, got %q", "must be positive", txt)
	}
	wantTag(t, wrapped, "id")
	if wrapped[1].(string) != "Int" {
		t.Fatalf("want type Int, got %v", wrapped[1])
	}
}

func Test_Parser_Normalize_Assign_Annotations_MergeToRHS(t *testing.T) {
	src := `
# A
x = # B
    # C
    true # D
`
	root := mustParse(t, src)
	asn := first(root)
	wantTag(t, asn, "assign")

	// LHS is id "x"
	wantTag(t, kid(asn, 0), "id")
	if kid(asn, 0)[1].(string) != "x" {
		t.Fatalf("lhs id mismatch: %v", dump(kid(asn, 0)))
	}

	// RHS is annot("A\nB\nC\nD", true)
	rhs := kid(asn, 1)
	txt, wrapped := asAnnot(t, rhs)
	if txt != "A\nB\nC\nD" {
		t.Fatalf("want PRE merged 'A\\nB\\nC\\nD', got txt=%q", txt)
	}
	wantTag(t, wrapped, "bool")
	if wrapped[1].(bool) != true {
		t.Fatalf("wrapped value mismatch")
	}
}

func Test_Parser_Normalize_LetAssign_Annotations_MergeToRHS(t *testing.T) {
	src := `
# A
let x = # B
        # C
        true # D
`
	root := mustParse(t, src)
	asn := first(root)
	wantTag(t, asn, "assign")

	// LHS is decl "x"
	wantTag(t, kid(asn, 0), "decl")
	if kid(asn, 0)[1].(string) != "x" {
		t.Fatalf("lhs decl mismatch: %v", dump(kid(asn, 0)))
	}

	// RHS is annot("A\nB\nC\nD", true)
	rhs := kid(asn, 1)
	txt, wrapped := asAnnot(t, rhs)
	if txt != "A\nB\nC\nD" {
		t.Fatalf("want PRE merged 'A\\nB\\nC\\nD', got txt=%q", txt)
	}
	wantTag(t, wrapped, "bool")
	if wrapped[1].(bool) != true {
		t.Fatalf("wrapped value mismatch")
	}
}

func Test_Parser_Normalize_MapPair_Annotations_MergeToValue(t *testing.T) {
	src := `
{
  # A
  x: # B
     # C
     true # D
}
`
	root := mustParse(t, src)
	obj := first(root)
	wantTag(t, obj, "map")
	p := kid(obj, 0)
	wantTag(t, p, "pair")

	// key "x"
	key := kid(p, 0)
	wantTag(t, key, "str")
	if key[1].(string) != "x" {
		t.Fatalf("key mismatch: %v", dump(key))
	}

	// value is annot("A\nB\nC\nD", true)
	val := p[2].(S)
	txt, wrapped := asAnnot(t, val)
	if txt != "A\nB\nC\nD" {
		t.Fatalf("want PRE merged 'A\\nB\\nC\\nD', got txt=%q", txt)
	}
	wantTag(t, wrapped, "bool")
}

func Test_Parser_Normalize_TypeRecordField_Annotations_MergeToType(t *testing.T) {
	src := `
type {
  # A
  x: # B
     # C
     Str # D
}
`
	root := mustParse(t, src)
	ty := first(root)
	wantTag(t, ty, "type")
	m := ty[1].(S)
	wantTag(t, m, "map")
	p := kid(m, 0)
	wantTag(t, p, "pair")

	// key "x"
	key := kid(p, 0)
	wantTag(t, key, "str")
	if key[1].(string) != "x" {
		t.Fatalf("key mismatch: %v", dump(key))
	}

	// value is annot("A\nB\nC\nD", id "Str")
	val := p[2].(S)
	txt, wrapped := asAnnot(t, val)
	if txt != "A\nB\nC\nD" {
		t.Fatalf("want PRE merged 'A\\nB\\nC\\nD', got  txt=%q", txt)
	}
	wantTag(t, wrapped, "id")
	if wrapped[1].(string) != "Str" {
		t.Fatalf("wrapped type mismatch: %v", dump(wrapped))
	}
}

func Test_Parser_Normalize_ParamType_Annotations_MergeToType(t *testing.T) {
	src := `
fun(
  # A
  x: # B
     # C
     Str # D
) do true end
`
	root := mustParse(t, src)
	fn := first(root)
	wantTag(t, fn, "fun")
	params := kid(fn, 0)
	wantTag(t, params, "array")
	p := kid(params, 0)
	wantTag(t, p, "pair")

	// name "x"
	name := kid(p, 0)
	wantTag(t, name, "id")
	if name[1].(string) != "x" {
		t.Fatalf("param name mismatch: %v", dump(name))
	}

	// param type value is annot("A\nB\nC\nD", id "Str")
	val := p[2].(S)
	txt, wrapped := asAnnot(t, val)
	if txt != "A\nB\nC\nD" {
		t.Fatalf("want PRE merged 'A\\nB\\nC\\nD', got txt=%q", txt)
	}
	wantTag(t, wrapped, "id")
	if wrapped[1].(string) != "Str" {
		t.Fatalf("wrapped type mismatch: %v", dump(wrapped))
	}
}

func Test_Parser_Array_Items_Annotations_AttachAroundComma(t *testing.T) {
	src := `[
  1, # note for 1
  2 # note for 2
]`
	root := mustParse(t, src)
	arr := first(root)
	wantTag(t, arr, "array")
	if len(arr) != 1+2 {
		t.Fatalf("want 2 elements, got %d", len(arr)-1)
	}

	// First element should have annotation coming AFTER the comma.
	e0 := kid(arr, 0)
	txt0, wrapped0 := asAnnot(t, e0)
	if txt0 != "note for 1" {
		t.Fatalf("element 0 annotation mismatch: txt=%q", txt0)
	}
	wantTag(t, wrapped0, "int")
	if wrapped0[1].(int64) != 1 {
		t.Fatalf("element 0 value mismatch: %s", dump(wrapped0))
	}

	// Second element should have annotation directly trailing the value.
	e1 := kid(arr, 1)
	txt1, wrapped1 := asAnnot(t, e1)
	if txt1 != "note for 2" {
		t.Fatalf("element 1 annotation mismatch: txt=%q", txt1)
	}
	wantTag(t, wrapped1, "int")
	if wrapped1[1].(int64) != 2 {
		t.Fatalf("element 1 value mismatch: %s", dump(wrapped1))
	}
}

func Test_Parser_Map_Items_Annotations_AttachAroundComma(t *testing.T) {
	src := `{
  a: 1, # note for 1
  b: 2 # note for 2
}`
	root := mustParse(t, src)
	obj := first(root)
	wantTag(t, obj, "map")
	if len(obj) != 1+2 {
		t.Fatalf("want 2 pairs, got %d", len(obj)-1)
	}

	// First pair: value annotated AFTER the comma.
	p0 := kid(obj, 0)
	wantTag(t, p0, "pair")
	key0 := kid(p0, 0)
	wantTag(t, key0, "str")
	if key0[1].(string) != "a" {
		t.Fatalf("key0 mismatch: %s", dump(key0))
	}
	val0 := p0[2].(S)
	txt0, wrapped0 := asAnnot(t, val0)
	if txt0 != "note for 1" {
		t.Fatalf("pair 0 value annotation mismatch: txt=%q", txt0)
	}
	wantTag(t, wrapped0, "int")
	if wrapped0[1].(int64) != 1 {
		t.Fatalf("pair 0 value mismatch: %s", dump(wrapped0))
	}

	// Second pair: value annotated directly trailing the value.
	p1 := kid(obj, 1)
	wantTag(t, p1, "pair")
	key1 := kid(p1, 0)
	wantTag(t, key1, "str")
	if key1[1].(string) != "b" {
		t.Fatalf("key1 mismatch: %s", dump(key1))
	}
	val1 := p1[2].(S)
	txt1, wrapped1 := asAnnot(t, val1)
	if txt1 != "note for 2" {
		t.Fatalf("pair 1 value annotation mismatch:  txt=%q", txt1)
	}
	wantTag(t, wrapped1, "int")
	if wrapped1[1].(int64) != 2 {
		t.Fatalf("pair 1 value mismatch: %s", dump(wrapped1))
	}
}

// 1) No leakage of PRE annotations across blank lines between statements.
func Test_Parser_Assign_Annot_NoLeak_AcrossBlankLine_BetweenStatements(t *testing.T) {
	src := `
# A
let X = 1

# B
let Y = 2
`
	root := mustParse(t, src)
	if len(root) != 1+3 {
		t.Fatalf("want assign, NOOP, assign; got %d\n%s", len(root)-1, dump(root))
	}
	asn1 := kid(root, 0)
	wantTag(t, kid(root, 1), "noop")
	asn2 := kid(root, 2)
	wantTag(t, asn1, "assign")
	wantTag(t, asn2, "assign")

	// RHS of first is annot("A", 1)
	rhs1 := kid(asn1, 1)
	txt1, w1 := asAnnot(t, rhs1)
	if txt1 != "A" {
		t.Fatalf("first RHS annot mismatch: txt=%q", txt1)
	}
	wantTag(t, w1, "int")
	if w1[1].(int64) != 1 {
		t.Fatalf("first RHS value mismatch: %s", dump(w1))
	}

	// RHS of second is annot("B", 2)
	rhs2 := kid(asn2, 1)
	txt2, w2 := asAnnot(t, rhs2)
	if txt2 != "B" {
		t.Fatalf("second RHS annot mismatch: txt=%q", txt2)
	}
	wantTag(t, w2, "int")
	if w2[1].(int64) != 2 {
		t.Fatalf("second RHS value mismatch: %s", dump(w2))
	}
}

// 2) Trailing annotation (D) must NOT cross a blank line; it becomes a PRE on a NOOP.
func Test_Parser_Assign_TrailingAnnot_DoesNotCrossBlankLine(t *testing.T) {
	src := `x = 1

# D`
	root := mustParse(t, src)
	// Expect: assign, NOOP, annot(PRE, NOOP)
	if len(root) != 1+3 {
		t.Fatalf("want 3 top-level statements, got %d\n%s", len(root)-1, dump(root))
	}
	asn := kid(root, 0)
	wantTag(t, asn, "assign")
	// RHS should be plain int (no annot merged, because blank line breaks D)
	rhs := kid(asn, 1)
	wantTag(t, rhs, "int")
	if rhs[1].(int64) != 1 {
		t.Fatalf("rhs value mismatch: %s", dump(rhs))
	}
	// Middle is NOOP
	wantTag(t, kid(root, 1), "noop")
	// Trailing annot wraps NOOP
	ann := kid(root, 2)
	txt, wrapped := asAnnot(t, ann)
	if txt != "D" {
		t.Fatalf("trailing PRE mismatch: txt=%q", txt)
	}
	wantTag(t, wrapped, "noop")
}

// 3) Floating comment inside an empty block should synthesize a NOOP and be valid.
func Test_Parser_Block_FloatingAnnot_WrapsNOOP_BeforeEnd(t *testing.T) {
	src := "do\n# a\nend"
	root := mustParse(t, src)
	blk := first(root)
	wantTag(t, blk, "block")
	if len(blk) != 1+1 {
		t.Fatalf("want 1 statement in block, got %d\n%s", len(blk)-1, dump(blk))
	}
	ann := kid(blk, 0)
	txt, wrapped := asAnnot(t, ann)
	if txt != "a" {
		t.Fatalf("block PRE mismatch: txt=%q", txt)
	}
	wantTag(t, wrapped, "noop")
}

// 4) Two PRE annotations split by a blank line: first wraps NOOP, second wraps the next block.
func Test_Parser_Annot_TwoPRE_SplitByBlankLine_DoWrapsBlock(t *testing.T) {
	src := `
# a1

# a2
do end
`
	root := mustParse(t, src)
	if len(root) != 1+2 {
		t.Fatalf("want 2 top-level nodes, got %d\n%s", len(root)-1, dump(root))
	}
	annNoop := kid(root, 0)
	txt1, w1 := asAnnot(t, annNoop)
	if txt1 != "a1" {
		t.Fatalf("first PRE mismatch: txt=%q", txt1)
	}
	wantTag(t, w1, "noop")

	annBlk := kid(root, 1)
	txt2, w2 := asAnnot(t, annBlk)
	if txt2 != "a2" {
		t.Fatalf("second PRE mismatch: txt=%q", txt2)
	}
	wantTag(t, w2, "block")
}

// 6) Inside params: blank lines between ":" and PRE do not break adjacency; PRE attaches to type.
func Test_Parser_Params_Annot_IgnoresNoopsAroundType(t *testing.T) {
	src := `
fun(
  x:

  # C
  Str
) do end
`
	root := mustParse(t, src)
	fn := first(root)
	wantTag(t, fn, "fun")
	params := kid(fn, 0)
	wantTag(t, params, "array")
	p := kid(params, 0)
	wantTag(t, p, "pair")

	val := p[2].(S)
	txt, wrapped := asAnnot(t, val)
	if txt != "C" {
		t.Fatalf("param type PRE mismatch: txt=%q", txt)
	}
	wantTag(t, wrapped, "id")
	if wrapped[1].(string) != "Str" {
		t.Fatalf("param type value mismatch: %s", dump(wrapped))
	}
}

func Test_Parser_Array_Annot_Post_AfterComma_SameLine(t *testing.T) {
	root := mustParse(t, "[\n  1, # note\n  2\n]")
	arr := first(root)
	e0 := kid(arr, 0)
	txt, wrapped := asAnnot(t, e0)
	if txt != "note" || head(wrapped) != "int" || wrapped[1].(int64) != 1 {
		t.Fatalf("want POST 'note' on first elem: %s", dump(e0))
	}
}

func Test_Parser_Array_Annot_Pre_NextElem_NextLine(t *testing.T) {
	root := mustParse(t, "[\n  1,\n  # note\n  2\n]")
	arr := first(root)
	e1 := kid(arr, 1)
	txt, wrapped := asAnnot(t, e1)
	if txt != "note" || head(wrapped) != "int" || wrapped[1].(int64) != 2 {
		t.Fatalf("want PRE 'note' on second elem: %s", dump(e1))
	}
}

func Test_Parser_Array_Annot_Pre_AllowsBlankLines_BeforeComment(t *testing.T) {
	root := mustParse(t, "[\n  1,\n\n  # note\n  2\n]")
	arr := first(root)
	e1 := kid(arr, 1)
	txt, wrapped := asAnnot(t, e1)
	if txt != "note" || head(wrapped) != "int" || wrapped[1].(int64) != 2 {
		t.Fatalf("want PRE 'note' on second elem across blank line before: %s", dump(e1))
	}
}

// ─────────────────────── Updated (stale) array tests ───────────────────────

func Test_Parser_Array_Annot_Pre_AllowsBlankLines(t *testing.T) {
	// Blank line (NOOP) BETWEEN the annotation and the next element breaks adjacency.
	// The note must NOT attach to 2.
	root := mustParse(t, "[\n  1,\n  # note\n\n  2\n]")
	arr := first(root)
	wantTag(t, arr, "array")

	if len(arr) != 1+2 {
		t.Fatalf("want 2 elems, got %d\n%s", len(arr)-1, dump(arr))
	}

	// First element remains an int
	e0 := kid(arr, 0)
	wantTag(t, e0, "int")
	if e0[1].(int64) != 1 {
		t.Fatalf("elem 0 mismatch: %s", dump(e0))
	}

	// Second element should be a plain int (annotation not attached due to intervening NOOP)
	e1 := kid(arr, 1)
	wantTag(t, e1, "int")
	if e1[1].(int64) != 2 {
		t.Fatalf("elem 1 mismatch: %s", dump(e1))
	}
}

func Test_Parser_Array_Annot_Dangling_Pre_BeforeClose_SynthNoop(t *testing.T) {
	// Annotation at the end (right before ']') must synthesize a NOOP to attach to,
	// rather than erroring.
	root := mustParse(t, "[\n  1\n\n  # note\n]")
	arr := first(root)
	wantTag(t, arr, "array")

	// We expect two items: 1 and annot("note", noop).
	if len(arr) != 1+2 {
		t.Fatalf("want 2 elems, got %d\n%s", len(arr)-1, dump(arr))
	}

	// First item
	e0 := kid(arr, 0)
	wantTag(t, e0, "int")
	if e0[1].(int64) != 1 {
		t.Fatalf("elem 0 mismatch: %s", dump(e0))
	}

	// Second item: annotation wrapping NOOP
	e1 := kid(arr, 1)
	txt, wrapped := asAnnot(t, e1)
	if txt != "note" {
		t.Fatalf("dangling PRE text mismatch: %q", txt)
	}
	wantTag(t, wrapped, "noop")
}

// ───────────────────────── Tiny, orthogonal map tests ──────────────────────

func Test_Parser_Map_Annot_Pre_NextPair_NextLine(t *testing.T) {
	// After-comma annotation on the NEXT line attaches to the NEXT pair's value (PRE),
	// not the previous pair (POST only allowed on same line).
	src := "{\n  a: 1,\n  # note\n  b: 2\n}"
	root := mustParse(t, src)
	obj := first(root)
	wantTag(t, obj, "map")
	if len(obj) != 1+2 {
		t.Fatalf("want 2 pairs, got %d\n%s", len(obj)-1, dump(obj))
	}

	// First pair remains unannotated value 1
	p0 := kid(obj, 0)
	wantTag(t, p0, "pair")
	v0 := p0[2].(S)
	wantTag(t, v0, "int")
	if v0[1].(int64) != 1 {
		t.Fatalf("pair 0 value mismatch: %s", dump(v0))
	}

	// Second pair's value carries PRE "note"
	p1 := kid(obj, 1)
	wantTag(t, p1, "pair")
	v1 := p1[2].(S)
	txt, wrapped := asAnnot(t, v1)
	if txt != "note" {
		t.Fatalf("pair 1 PRE text mismatch: %q", txt)
	}
	wantTag(t, wrapped, "int")
	if wrapped[1].(int64) != 2 {
		t.Fatalf("pair 1 wrapped value mismatch: %s", dump(wrapped))
	}
}

func Test_Parser_Map_Annot_Pre_BrokenByBlankLine(t *testing.T) {
	// Blank line (NOOP) between the annotation and the next pair breaks adjacency.
	src := "{\n  a: 1,\n  # note\n\n  b: 2\n}"
	root := mustParse(t, src)
	obj := first(root)
	wantTag(t, obj, "map")
	if len(obj) != 1+2 {
		t.Fatalf("want 2 pairs, got %d\n%s", len(obj)-1, dump(obj))
	}

	// First pair remains value 1
	p0 := kid(obj, 0)
	wantTag(t, p0, "pair")
	v0 := p0[2].(S)
	wantTag(t, v0, "int")
	if v0[1].(int64) != 1 {
		t.Fatalf("pair 0 value mismatch: %s", dump(v0))
	}

	// Second pair's value should be a plain int (annotation did NOT attach)
	p1 := kid(obj, 1)
	wantTag(t, p1, "pair")
	v1 := p1[2].(S)
	wantTag(t, v1, "int")
	if v1[1].(int64) != 2 {
		t.Fatalf("pair 1 value mismatch: %s", dump(v1))
	}
}
