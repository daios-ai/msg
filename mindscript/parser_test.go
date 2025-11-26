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
	sexpr, _, err := ParseSExprInteractiveWithSpans(src)
	if err != nil {
		t.Fatalf("Parse (interactive) error: %v\nsource:\n%s", err, src)
	}
	return sexpr
}

func mustIncomplete(t *testing.T, src string) {
	t.Helper()
	_, _, err := ParseSExprInteractiveWithSpans(src)
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
	_, _, err := ParseSExprInteractiveWithSpans(src)
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

func Test_Parser_NOOP_Inside_Params_Ignored(t *testing.T) {
	// NOOPs inside param lists are preserved. Count only ("pair", ...) entries.
	root := mustParse(t, "fun(\n\n  x: Int,\n  \n  y: Str\n) do end")
	fn := first(root)
	wantTag(t, fn, "fun")
	params := kid(fn, 0) // ("array", entries...)
	wantTag(t, params, "array")

	pairs := 0
	for i := 0; i < len(params)-1; i++ {
		if head(kid(params, i)) == "pair" {
			pairs++
		}
	}
	if pairs != 2 {
		t.Fatalf("want 2 param pairs, got %d\n%s", pairs, dump(params))
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
	ann := first(root)
	txt, wrapped := asAnnot(t, ann)
	if txt != "this" {
		t.Fatalf("want PRE 'this'")
	}
	wantTag(t, wrapped, "return")
	val := wrapped[1].(S)
	wantTag(t, val, "int")
	if val[1].(int64) != 0 {
		t.Fatalf("return value mismatch: %s", dump(val))
	}
}

func Test_Parser_Annot_Control_PRE_InlineValue_BindsToValue(t *testing.T) {
	root := mustParse(t, "return # this\n0")
	if len(root) != 1+2 {
		t.Fatalf("want 2 top-level exprs, got %d\n%s", len(root)-1, dump(root))
	}

	ann := kid(root, 0)
	txt, wrapped := asAnnot(t, ann)
	if txt != "this" {
		t.Fatalf("want PRE 'this'")
	}
	wantTag(t, wrapped, "return")
	val := wrapped[1].(S)
	wantTag(t, val, "null")

	nxt := kid(root, 1)
	wantTag(t, nxt, "int")
	if nxt[1].(int64) != 0 {
		t.Fatalf("second stmt not int 0: %s", dump(nxt))
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
	outer := first(root)
	txtOuter, wrappedOuter := asAnnot(t, outer)
	if txtOuter != "pre" {
		t.Fatalf("outer annotation text mismatch: %q", txtOuter)
	}
	txtInner, innerWrapped := asAnnot(t, wrappedOuter)
	if txtInner != "post" {
		t.Fatalf("inner annotation text mismatch: %q", txtInner)
	}
	wantTag(t, innerWrapped, "id")
	if innerWrapped[1].(string) != "x" {
		t.Fatalf("wrapped id mismatch: %s", dump(innerWrapped))
	}
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

	ann1 := kid(root, 0)
	wantTag(t, kid(root, 1), "noop")
	ann2 := kid(root, 2)

	// First statement: annot("A", assign(decl X, 1))
	txt1, w1 := asAnnot(t, ann1)
	if txt1 != "A" {
		t.Fatalf("first PRE annot mismatch: txt=%q", txt1)
	}
	wantTag(t, w1, "assign")
	lhs1 := kid(w1, 0)
	rhs1 := kid(w1, 1)
	wantTag(t, lhs1, "decl")
	if lhs1[1].(string) != "X" {
		t.Fatalf("first LHS decl mismatch: %s", dump(lhs1))
	}
	wantTag(t, rhs1, "int")
	if rhs1[1].(int64) != 1 {
		t.Fatalf("first RHS value mismatch: %s", dump(rhs1))
	}

	// Second statement: annot("B", assign(decl Y, 2))
	txt2, w2 := asAnnot(t, ann2)
	if txt2 != "B" {
		t.Fatalf("second PRE annot mismatch: txt=%q", txt2)
	}
	wantTag(t, w2, "assign")
	lhs2 := kid(w2, 0)
	rhs2 := kid(w2, 1)
	wantTag(t, lhs2, "decl")
	if lhs2[1].(string) != "Y" {
		t.Fatalf("second LHS decl mismatch: %s", dump(lhs2))
	}
	wantTag(t, rhs2, "int")
	if rhs2[1].(int64) != 2 {
		t.Fatalf("second RHS value mismatch: %s", dump(rhs2))
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

func Test_Parser_NOOP_Inside_CallArgs_Ignored(t *testing.T) {
	root := mustParse(t, "f(\n\n  1,\n  \n  2\n)")
	call := first(root)
	wantTag(t, call, "call")
	// ("call", id "f", NOOP, 1, NOOP, 2)
	if len(call) != 6 {
		t.Fatalf("want callee + 4 arg nodes (incl NOOPs), got %d", len(call)-1)
	}
	if !isId(kid(call, 0), "f") {
		t.Fatalf("callee not f")
	}
	wantTag(t, kid(call, 1), "noop")
	if head(kid(call, 2)) != "int" || kid(call, 2)[1].(int64) != 1 {
		t.Fatalf("arg1 mismatch")
	}
	wantTag(t, kid(call, 3), "noop")
	if head(kid(call, 4)) != "int" || kid(call, 4)[1].(int64) != 2 {
		t.Fatalf("arg2 mismatch")
	}
}

func Test_Parser_Array_Annot_Pre_AllowsBlankLines_BeforeComment(t *testing.T) {
	root := mustParse(t, "[\n  1,\n\n  # note\n  2\n]")
	arr := first(root)
	wantTag(t, arr, "array")
	// Expect: 1, NOOP, annot("note", 2)
	if len(arr) != 1+3 {
		t.Fatalf("want 3 elems, got %d\n%s", len(arr)-1, dump(arr))
	}
	wantTag(t, kid(arr, 0), "int")
	wantTag(t, kid(arr, 1), "noop") // <— updated
	txt, wrapped := asAnnot(t, kid(arr, 2))
	if txt != "note" || head(wrapped) != "int" || wrapped[1].(int64) != 2 {
		t.Fatalf("want PRE 'note' on 2, got %s", dump(kid(arr, 2)))
	}
}

func Test_Parser_Array_Annot_Pre_AllowsBlankLines(t *testing.T) {
	root := mustParse(t, "[\n  1,\n  # note\n\n  2\n]")
	arr := first(root)
	wantTag(t, arr, "array")
	// Expect: 1, annot("note", NOOP), 2
	if len(arr) != 1+3 {
		t.Fatalf("want 3 elems, got %d\n%s", len(arr)-1, dump(arr))
	}
	wantTag(t, kid(arr, 0), "int")
	txt, wrapped := asAnnot(t, kid(arr, 1))
	if txt != "note" || head(wrapped) != "noop" {
		t.Fatalf("want annot(note, noop), got %s", dump(kid(arr, 1)))
	}
	wantTag(t, kid(arr, 2), "int")
}

func Test_Parser_Array_Annot_Dangling_Pre_BeforeClose_SynthNoop(t *testing.T) {
	root := mustParse(t, "[\n  1\n\n  # note\n]")
	arr := first(root)
	wantTag(t, arr, "array")
	// Expect: 1, NOOP, annot("note", NOOP)
	if len(arr) != 1+3 {
		t.Fatalf("want 3 elems, got %d\n%s", len(arr)-1, dump(arr))
	}
	wantTag(t, kid(arr, 0), "int")
	wantTag(t, kid(arr, 1), "noop")
	txt, wrapped := asAnnot(t, kid(arr, 2))
	if txt != "note" || head(wrapped) != "noop" {
		t.Fatalf("want annot(note, noop), got %s", dump(kid(arr, 2)))
	}
}

func Test_Parser_Map_Annot_Pre_BrokenByBlankLine(t *testing.T) {
	src := "{\n  a: 1,\n  # note\n\n  b: 2\n}"
	root := mustParse(t, src)
	obj := first(root)
	wantTag(t, obj, "map")
	// Expect: pair(a:1), annot(note, noop), pair(b:2)
	if len(obj) != 1+3 {
		t.Fatalf("want 3 entries, got %d\n%s", len(obj)-1, dump(obj))
	}
	p0 := kid(obj, 0)
	wantTag(t, p0, "pair")
	if kid(p0, 0)[1].(string) != "a" || head(p0[2].(S)) != "int" {
		t.Fatalf("pair a mismatch")
	}

	ann := kid(obj, 1)
	txt, wrapped := asAnnot(t, ann)
	if txt != "note" || head(wrapped) != "noop" {
		t.Fatalf("want annot(note, noop), got %s", dump(ann))
	}

	p1 := kid(obj, 2)
	wantTag(t, p1, "pair")
	if kid(p1, 0)[1].(string) != "b" || head(p1[2].(S)) != "int" {
		t.Fatalf("pair b mismatch")
	}
}

func Test_Parser_Function_Params_NOOPs_And_Annotations(t *testing.T) {
	src := `fun(

  x: Int, # fast

  # important
  y: Str
) do end`
	root := mustParse(t, src)
	fn := first(root)
	wantTag(t, fn, "fun")
	params := kid(fn, 0)
	wantTag(t, params, "array")

	// params: NOOP, pair(x, annot("fast", Int)), NOOP, pair(y, annot("important", Str))
	wantTag(t, kid(params, 0), "noop")

	p0 := kid(params, 1)
	wantTag(t, p0, "pair")
	typ0 := p0[2].(S)
	txt0, w0 := asAnnot(t, typ0)
	if txt0 != "fast" || head(w0) != "id" || w0[1].(string) != "Int" {
		t.Fatalf("param x type annot mismatch: %s", dump(typ0))
	}

	wantTag(t, kid(params, 2), "noop")

	p1 := kid(params, 3)
	wantTag(t, p1, "pair")
	typ1 := p1[2].(S)
	txt1, w1 := asAnnot(t, typ1)
	if txt1 != "important" || head(w1) != "id" || w1[1].(string) != "Str" {
		t.Fatalf("param y type annot mismatch: %s", dump(typ1))
	}
}

func Test_Parser_Function_Params_Trailing_Dangling_PRE_SynthNoop(t *testing.T) {
	src := `fun(
  x: Int,
  # about to end
) do end`
	root := mustParse(t, src)
	fn := first(root)
	wantTag(t, fn, "fun")
	params := kid(fn, 0)
	wantTag(t, params, "array")

	// Last element is annot("about to end", noop)
	last := kid(params, len(params)-2) // params is ("array", ...); last elem at len-2
	txt, wrapped := asAnnot(t, last)
	if txt != "about to end" || head(wrapped) != "noop" {
		t.Fatalf("dangling PRE did not synthesize NOOP: %s", dump(last))
	}
}

func Test_Parser_Oracle_Sources_Annotations(t *testing.T) {
	src := `oracle(a: Int, b: Str) -> Bool from[
  "web", # primary
  # fallback
  "docs"
]`
	root := mustParse(t, src)
	orc := first(root)
	wantTag(t, orc, "oracle")

	srcs := kid(orc, 2)
	wantTag(t, srcs, "array")
	if len(srcs) != 1+2 {
		t.Fatalf("want 2 sources, got %d", len(srcs)-1)
	}

	// "web" has POST annot on same line
	s0 := kid(srcs, 0)
	txt0, w0 := asAnnot(t, s0)
	if txt0 != "primary" || head(w0) != "str" || w0[1].(string) != "web" {
		t.Fatalf("source 0 mismatch: %s", dump(s0))
	}

	// "docs" has PRE annot on the previous line
	s1 := kid(srcs, 1)
	txt1, w1 := asAnnot(t, s1)
	if txt1 != "fallback" || head(w1) != "str" || w1[1].(string) != "docs" {
		t.Fatalf("source 1 mismatch: %s", dump(s1))
	}
}

func Test_Parser_Function_ReturnType_PRE_Annot(t *testing.T) {
	src := `fun() ->
  # note about return
  Str
do end`
	root := mustParse(t, src)
	fn := first(root)
	wantTag(t, fn, "fun")
	ret := kid(fn, 1)
	txt, wrapped := asAnnot(t, ret)
	if txt != "note about return" || head(wrapped) != "id" || wrapped[1].(string) != "Str" {
		t.Fatalf("return type PRE annot mismatch: %s", dump(ret))
	}
}

func Test_Parser_Params_Annot_AfterBlankLine_WrapsNOOP(t *testing.T) {
	src := `fun(
  x: Int,

  # note

  y: Str
) do end`
	root := mustParse(t, src)
	fn := first(root)
	wantTag(t, fn, "fun")
	params := kid(fn, 0) // ("array", ...)
	wantTag(t, params, "array")

	// Expect: pair(x,Int), NOOP, annot("note", NOOP), pair(y,Str)
	if len(params) != 1+4 {
		t.Fatalf("want 4 param entries, got %d\n%s", len(params)-1, dump(params))
	}

	// 0: first parameter pair
	p0 := kid(params, 0)
	wantTag(t, p0, "pair")

	// 1: blank-line NOOP between params
	wantTag(t, kid(params, 1), "noop")

	// 2: annotation wrapping a synthesized NOOP
	mid := kid(params, 2)
	txt, w := asAnnot(t, mid)
	if txt != "note" || head(w) != "noop" {
		t.Fatalf("mid param entry should be annot(note, noop): %s", dump(mid))
	}

	// 3: second parameter pair
	p1 := kid(params, 3)
	wantTag(t, p1, "pair")
}

// --- Dangling PRE before closers (uniform across contexts) -----------------

func Test_Parser_Array_DanglingPre_SynthNoop(t *testing.T) {
	root := mustParse(t, "[\n  1\n  # note\n]")
	arr := first(root)
	wantTag(t, arr, "array")
	if len(arr) != 1+2 {
		t.Fatalf("want [1, annot(note, noop)], got %s", dump(arr))
	}
	wantTag(t, kid(arr, 0), "int")
	txt, wrapped := asAnnot(t, kid(arr, 1))
	if txt != "note" || head(wrapped) != "noop" {
		t.Fatalf("want annot(note, noop), got %s", dump(kid(arr, 1)))
	}
}

func Test_Parser_Map_DanglingPre_SynthNoop(t *testing.T) {
	root := mustParse(t, "{\n  a: 1,\n  # note\n}")
	m := first(root)
	wantTag(t, m, "map")
	if len(m) != 1+2 {
		t.Fatalf("want pair(a:1), annot(note, noop), got %s", dump(m))
	}
	wantTag(t, kid(m, 0), "pair")
	txt, wrapped := asAnnot(t, kid(m, 1))
	if txt != "note" || head(wrapped) != "noop" {
		t.Fatalf("want annot(note, noop), got %s", dump(kid(m, 1)))
	}
}

func Test_Parser_CallArgs_DanglingPre_SynthNoop(t *testing.T) {
	root := mustParse(t, "f(\n  1\n  # note\n)")
	call := first(root)
	wantTag(t, call, "call")
	// call = ("call", callee, 1, annot(note, noop))
	if len(call) != 1+1+2 {
		t.Fatalf("want callee + 2 args, got %d\n%s", len(call)-1, dump(call))
	}
	if !isId(kid(call, 0), "f") {
		t.Fatalf("callee not f")
	}
	wantTag(t, kid(call, 1), "int")
	txt, wrapped := asAnnot(t, kid(call, 2))
	if txt != "note" || head(wrapped) != "noop" {
		t.Fatalf("dangling PRE should synthesize NOOP, got %s", dump(kid(call, 2)))
	}
}

func Test_Parser_CallArgs_EmptyWithDanglingPre_SynthNoop(t *testing.T) {
	root := mustParse(t, "f(\n  # note\n)")
	call := first(root)
	wantTag(t, call, "call")
	// ("call", callee, annot(note, noop))
	if len(call) != 1+1+1 {
		t.Fatalf("want callee + 1 arg (annot), got %d\n%s", len(call)-1, dump(call))
	}
	txt, wrapped := asAnnot(t, kid(call, 1))
	if txt != "note" || head(wrapped) != "noop" {
		t.Fatalf("want annot(note, noop), got %s", dump(kid(call, 1)))
	}
}

func Test_Parser_Params_EmptyWithDanglingPre_SynthNoop(t *testing.T) {
	root := mustParse(t, "fun(\n  # note\n) do end")
	fn := first(root)
	wantTag(t, fn, "fun")
	params := kid(fn, 0)
	wantTag(t, params, "array")
	if len(params) != 1+1 {
		t.Fatalf("want one entry (annot(note, noop)), got %s", dump(params))
	}
	txt, wrapped := asAnnot(t, kid(params, 0))
	if txt != "note" || head(wrapped) != "noop" {
		t.Fatalf("want annot(note, noop), got %s", dump(kid(params, 0)))
	}
}

// --- Destructuring patterns: dangling PRE before ']' / '}' -----------------

func Test_Parser_ArrayPattern_DanglingPre_SynthNoop(t *testing.T) {
	root := mustParse(t, "let [a,\n  # note\n] = xs")
	asn := first(root)
	wantTag(t, asn, "assign")
	lhs := kid(asn, 0)
	wantTag(t, lhs, "darr")
	if len(lhs) != 1+2 {
		t.Fatalf("want decl(a), annot(note, noop), got %s", dump(lhs))
	}
	wantTag(t, kid(lhs, 0), "decl")
	txt, wrapped := asAnnot(t, kid(lhs, 1))
	if txt != "note" || head(wrapped) != "noop" {
		t.Fatalf("want annot(note, noop), got %s", dump(kid(lhs, 1)))
	}
}

func Test_Parser_ObjectPattern_DanglingPre_SynthNoop(t *testing.T) {
	root := mustParse(t, "let {\n  a: x,\n  # note\n} = m")
	asn := first(root)
	wantTag(t, asn, "assign")
	lhs := kid(asn, 0)
	wantTag(t, lhs, "dobj")
	if len(lhs) != 1+2 {
		t.Fatalf("want pair(a:x), annot(note, noop), got %s", dump(lhs))
	}
	wantTag(t, kid(lhs, 0), "pair")
	txt, wrapped := asAnnot(t, kid(lhs, 1))
	if txt != "note" || head(wrapped) != "noop" {
		t.Fatalf("want annot(note, noop), got %s", dump(kid(lhs, 1)))
	}
}

//
// ───────────────────────── Incomplete (interactive) ─────────────────────────
//

func Test_Parser_Incomplete_Assign_RHS_OnlyGaps(t *testing.T) {
	// After '=', only blank lines → should be Incomplete (not a parsed noop).
	mustIncomplete(t, "let x =\n\n")
}

func Test_Parser_Incomplete_AfterAnnotation_OnlyGaps(t *testing.T) {
	// Standalone hash line in interactive mode should request more input,
	// not parse as annot(noop).
	mustIncomplete(t, "# hi\n")
}

func Test_Parser_Incomplete_AfterDot_OnlyGaps(t *testing.T) {
	// Property access started but no property given, only gaps → Incomplete.
	mustIncomplete(t, "obj.\n\n")
}

func Test_Parser_Incomplete_ParamTypeAfterColon(t *testing.T) {
	// Inside params, after ':', only gaps → Incomplete (needs a type expr).
	mustIncomplete(t, "fun(x: \n")
}

func Test_Parser_Incomplete_MapValueAfterColon(t *testing.T) {
	// In map literal, after ':', only gaps → Incomplete (needs a value).
	mustIncomplete(t, "{ \"k\": \n")
}

func Test_Parser_Incomplete_AfterTypeKeyword(t *testing.T) {
	// 'type' must be followed by an expression; gaps-to-EOF → Incomplete.
	mustIncomplete(t, "type \n")
}

func Test_Parser_Incomplete_FunctionArrowType(t *testing.T) {
	// After '->' in fun header, only gaps → Incomplete.
	mustIncomplete(t, "fun(x) -> \n")
}

func Test_Parser_Incomplete_OracleFromExpr(t *testing.T) {
	// After 'from' in oracle, only gaps → Incomplete.
	mustIncomplete(t, "oracle(x) from \n")
}

func Test_Parser_Incomplete_ForPattern_OpenArray(t *testing.T) {
	// Pattern started but incomplete (only gaps before EOF) → Incomplete.
	mustIncomplete(t, "for let [a, \n")
}

func Test_Parser_Incomplete_Idx_OpenBracket(t *testing.T) {
	// Index opened but value missing (gaps) → Incomplete expects ']'.
	mustIncomplete(t, "arr[\n")
}

func Test_Parser_Incomplete_Call_OpenParen(t *testing.T) {
	// Call opened but no close and only gaps → Incomplete expects ')'.
	mustIncomplete(t, "f(\n")
}

func Test_Parser_Incomplete_ComputedProperty_OpenParen(t *testing.T) {
	// Computed property 'obj.( ...' started, gaps to EOF → Incomplete ')'.
	mustIncomplete(t, "obj.(\n")
}

func Test_Parser_Incomplete_ObjectKey_Missing(t *testing.T) {
	// Object literal with only '{' and gaps → Incomplete (needs key or '}').
	mustIncomplete(t, "{\n")
}

func Test_Parser_Incomplete_EnumOpenBracket(t *testing.T) {
	// Enum literal started but only gaps → Incomplete expects ']'.
	mustIncomplete(t, "Enum[\n")
}

//
// ─────────────── Behavior that should NOT be incomplete (interactive) ───────
//

func Test_Parser_Interactive_Return_Newline_YieldsNull(t *testing.T) {
	// Same-line rule: 'return' followed by newline means return null (valid).
	root := mustParseInteractive(t, "return\n")
	stmt := first(root)
	wantTag(t, stmt, "return")
	wantTag(t, kid(stmt, 0), "null")
}

func Test_Parser_Interactive_Assign_RHS_WithFollowingExpr_IsOK(t *testing.T) {
	// If there is a real token after gaps, it's NOT incomplete.
	root := mustParseInteractive(t, "let x =\n\n42\n")
	stmt := first(root)
	wantTag(t, stmt, "assign")
	if head(kid(stmt, 1)) != "int" || kid(stmt, 1)[1].(int64) != 42 {
		t.Fatalf("rhs not 42: %s", dump(stmt))
	}
}

func Test_Parser_Interactive_Annotation_FollowedByExpr_IsOK(t *testing.T) {
	// Annotation line followed by a value on a later line parses as annot(value).
	root := mustParseInteractive(t, "# hi\n42\n")
	stmt := first(root)
	// Parser normalizes annotations to wrap values.
	txt, wrapped := asAnnot(t, stmt)
	if txt != "hi" {
		t.Fatalf("want annotation text 'hi', got %q", txt)
	}
	wantTag(t, wrapped, "int")
	if wrapped[1].(int64) != 42 {
		t.Fatalf("want 42, got %v", wrapped[1])
	}
}

func Test_Parser_Incomplete_Table(t *testing.T) {
	cases := []struct {
		name     string
		src      string
		wantLine int
		wantCol  int
		wantSub  string
	}{
		// ── Missing-but-desired centralized gap checks (folded from your list) ──

		// 1) Grouping: after '(' needs an expression (anchor at '(')
		{
			name:     "Grouping_NeedsExpr_AfterOpenParen",
			src:      "(\n",
			wantLine: 1, wantCol: 1,
			wantSub: "expected expression after '('",
		},

		// 2) Computed property: after '.(' needs an expression (anchor at '(')
		{
			name:     "ComputedProperty_NeedsExpr_AfterOpenParen",
			src:      "obj.(\n", // o=1 b=2 j=3 .=4 ( =5 → Col=5
			wantLine: 1, wantCol: 5,
			wantSub: "expected expression after '('",
		},

		// 3) Index: after '[' needs an expression (anchor at '[')
		{
			name:     "Index_NeedsExpr_AfterOpenBracket",
			src:      "arr[\n", // a=1 r=2 r=3 [=4 → Col=4
			wantLine: 1, wantCol: 4,
			wantSub: "expected index expression after '['",
		},

		// 4) Map literal: after ':' needs a value (anchor at ':')
		{
			name:     "MapValue_NeedsExpr_AfterColon",
			src:      "{a:\n", // {=1 a=2 :=3 → Col=3
			wantLine: 1, wantCol: 3,
			wantSub: "expected value after ':'",
		},

		// 5) If: needs a condition after 'if' (anchor at 'if')
		{
			name:     "If_NeedsCondition",
			src:      "if \n",
			wantLine: 1, wantCol: 1,
			wantSub: "expected condition after 'if'",
		},

		// 6) Elif: needs a condition after 'elif' (anchor at 'elif')
		{
			name:     "Elif_NeedsCondition",
			src:      "if a then x\nelif \n", // 'elif' starts at line 2, column 1
			wantLine: 2, wantCol: 1,
			wantSub: "expected condition after 'elif'",
		},

		// 7) While: needs a condition after 'while' (anchor at 'while')
		{
			name:     "While_NeedsCondition",
			src:      "while \n",
			wantLine: 1, wantCol: 1,
			wantSub: "expected condition after 'while'",
		},

		// 8) For: needs an iterator expression after 'in' (anchor at 'in')
		{
			name: "ForIn_NeedsExpr",
			src:  "for x in \n", // f=1 o=2 r=3 sp=4 x=5 sp=6 i=7 n=8 → Col=7
			// NOTE: current implementation may anchor at 'in' or just after it.
			wantLine: 1, wantCol: 7,
			wantSub: "expected expression after 'in'",
		},

		// ── After-condition gaps before required token (folded from your list) ──

		// IF: condition present, only gaps before required 'then'
		{
			name:     "If_AfterCondition_NeedsThen",
			src:      "if x\n", // "if␠x" → anchor after x (col 5)
			wantLine: 1, wantCol: 5,
			wantSub: "expected 'then'",
		},

		// ELIF: condition present, only gaps before required 'then'
		{
			name: "Elif_AfterCondition_NeedsThen",
			src:  "if a then b\nelif c\n", // line 2, after 'c' (col 7)
			// NOTE: exact col may differ slightly if implementation changes span math.
			wantLine: 2, wantCol: 7,
			wantSub: "expected 'then'",
		},

		// WHILE: condition present, only gaps before required 'do'
		{
			name:     "While_AfterCondition_NeedsDo",
			src:      "while x\n", // "while␠x" → anchor after x (col 8)
			wantLine: 1, wantCol: 8,
			wantSub: "expected 'do'",
		},

		// FOR: target present, only gaps before required 'in'
		{
			name:     "For_AfterTarget_NeedsIn",
			src:      "for x \n", // "for␠x␠" → after x (col 6)
			wantLine: 1, wantCol: 6,
			wantSub: "expected 'in'",
		},

		// FOR: iterator present, only gaps before required 'do'
		{
			name:     "For_AfterIter_NeedsDo",
			src:      "for x in y\n", // "for␠x␠in␠y" → after y (col 11)
			wantLine: 1, wantCol: 11,
			wantSub: "expected 'do'",
		},

		// MODULE: name expr present, only gaps before required 'do'
		{
			name:     "Module_AfterName_NeedsDo",
			src:      "module M\n", // "module␠M" → after 'M' (col 9)
			wantLine: 1, wantCol: 9,
			wantSub: "expected 'do'",
		},

		// ── After-expr gaps before closing delimiter (folded from your list) ──

		// Grouping: expr present, only gaps before ')'
		{
			name:     "Grouping_AfterExpr_MissingRParen",
			src:      "(1\n", // after '1' (col 3)
			wantLine: 1, wantCol: 3,
			wantSub: "expected ')'",
		},

		// Computed property: expr present, only gaps before ')'
		{
			name:     "ComputedProperty_AfterExpr_MissingRParen",
			src:      "obj.(1\n", // after '1' (col 7)
			wantLine: 1, wantCol: 7,
			wantSub: "expected ')'",
		},

		// Index: expr present, only gaps before ']'
		{
			name:     "Index_AfterExpr_MissingRSquare",
			src:      "arr[1\n", // after '1' (col 6)
			wantLine: 1, wantCol: 6,
			wantSub: "expected ']'",
		},

		// ── Extra core operators (nice-to-have coverage) ──

		// Infix: needs RHS after '+'
		{
			name:     "InfixPlus_MissingRHS",
			src:      "a +\n", // '+' at col 3
			wantLine: 1, wantCol: 3,
			wantSub: "expected expression after operator",
		},

		// Infix: needs RHS after '->'
		{
			name:     "InfixArrow_MissingRHS",
			src:      "a ->\n", // '-' at col 3
			wantLine: 1, wantCol: 3,
			wantSub: "expected expression after operator",
		},

		// Infix: needs RHS after '='
		{
			name:     "InfixAssign_MissingRHS",
			src:      "a =\n", // '=' at col 3
			wantLine: 1, wantCol: 3,
			wantSub: "expected expression after operator",
		},

		// Prefix: needs operand after 'type'
		{
			name:     "Type_MissingOperand",
			src:      "type \n",
			wantLine: 1, wantCol: 1,
			wantSub: "expected type expression after 'type'",
		},

		// Fun header: missing params closer ')'
		{
			name: "Fun_Params_MissingRParen",
			src:  "fun(\n",
			// need(...) for ')' anchors after last span (the '(') → line 1 col 5
			wantLine: 2, wantCol: 1,
			wantSub: "expected parameter name",
		},

		// Oracle header: arrow present but missing type
		{
			name:     "Oracle_Arrow_MissingType",
			src:      "oracle() ->\n",
			wantLine: 1, wantCol: 10, // '-' in '->' is 10
			wantSub: "expected output type after '->'",
		},

		// Enum literal: missing closing ']'
		{
			name: "Enum_MissingClose",
			src:  "Enum[\n",
			// Current behavior: element-first parse falls to EOF in expr → incomplete at 2:1
			wantLine: 2, wantCol: 1,
			wantSub: "unexpected end of input",
		},

		// ── Regression: destructuring let must be incomplete if '=' is missing ──

		// Destructuring let with only gaps (NOOPs) before EOF: MUST be Incomplete.
		{
			name: "Let_Destructuring_MissingAssign_WithBlankLines",
			src:  "let [a,b]\n\n",
			// posAfterLastSpan() → after ']' on line 1; "let␠[a,b]" → col 10
			wantLine: 1, wantCol: 10,
			wantSub: "expected '=' after destructuring let pattern",
		},
	}

	for _, tc := range cases {
		tc := tc
		t.Run("Incomplete_"+tc.name, func(t *testing.T) {
			checkParseInc(t, tc.src, tc.wantLine, tc.wantCol, tc.wantSub)
		})
	}
}

// --- New tests for exponentiation, bitwise ops, and precedence/assoc ---

func Test_Parser_Pow_RightAssociative(t *testing.T) {
	root := mustParse(t, `2 ** 3 ** 2`)
	top := first(root)
	wantTag(t, top, "binop")
	if top[1].(string) != "**" {
		t.Fatalf("top op not **: %s", dump(top))
	}
	rhs := top[3].(S)
	wantTag(t, rhs, "binop")
	if rhs[1].(string) != "**" {
		t.Fatalf("rhs not power: %s", dump(rhs))
	}
}

func Test_Parser_Unary_Neg_BindsLooserThanPow(t *testing.T) {
	root := mustParse(t, `-3 ** 2`)
	top := first(root)
	wantTag(t, top, "unop")
	if top[1].(string) != "-" {
		t.Fatalf("top not unary -: %s", dump(top))
	}
	inner := top[2].(S)
	wantTag(t, inner, "binop")
	if inner[1].(string) != "**" {
		t.Fatalf("inner not power: %s", dump(inner))
	}
}

func Test_Parser_Pow_Vs_Mult_Precedence(t *testing.T) {
	root := mustParse(t, `2 ** 3 * 4`)
	top := first(root)
	wantTag(t, top, "binop")
	if top[1].(string) != "*" {
		t.Fatalf("top not '*': %s", dump(top))
	}
	left := top[2].(S)
	wantTag(t, left, "binop")
	if left[1].(string) != "**" {
		t.Fatalf("left not '**': %s", dump(left))
	}
}

func Test_Parser_Shift_Precedence_Vs_Add(t *testing.T) {
	root := mustParse(t, `1 + 2 << 3`)
	top := first(root)
	wantTag(t, top, "binop")
	if top[1].(string) != "<<" {
		t.Fatalf("top not '<<': %s", dump(top))
	}
	left := top[2].(S)
	wantTag(t, left, "binop")
	if left[1].(string) != "+" {
		t.Fatalf("left of '<<' not '+': %s", dump(left))
	}
}

func Test_Parser_Bitwise_Tiers_Order(t *testing.T) {
	root := mustParse(t, `a & b ^ c | d`)
	top := first(root)
	wantTag(t, top, "binop")
	if top[1].(string) != "|" {
		t.Fatalf("top not '|': %s", dump(top))
	}
	lhs := top[2].(S)
	wantTag(t, lhs, "binop")
	if lhs[1].(string) != "^" {
		t.Fatalf("lhs of '|' not '^': %s", dump(lhs))
	}
	lhslhs := lhs[2].(S)
	wantTag(t, lhslhs, "binop")
	if lhslhs[1].(string) != "&" {
		t.Fatalf("left of '^' not '&': %s", dump(lhslhs))
	}
}

func Test_Parser_BitwiseNot_Prefix(t *testing.T) {
	root := mustParse(t, `~x & y`)
	top := first(root)
	wantTag(t, top, "binop")
	if top[1].(string) != "&" {
		t.Fatalf("top not '&': %s", dump(top))
	}
	lhs := top[2].(S)
	wantTag(t, lhs, "unop")
	if lhs[1].(string) != "~" {
		t.Fatalf("lhs not unary '~': %s", dump(lhs))
	}
}

func Test_Parser_Shift_Tokenization_PrefersShiftOverRelEq(t *testing.T) {
	root := mustParse(t, `a << b <= c`)
	top := first(root)
	wantTag(t, top, "binop")
	if top[1].(string) != "<=" {
		t.Fatalf("top not '<=': %s", dump(top))
	}
	lhs := top[2].(S)
	wantTag(t, lhs, "binop")
	if lhs[1].(string) != "<<" {
		t.Fatalf("lhs of '<=' not '<<': %s", dump(lhs))
	}
}

func Test_Parser_Pow_Chains_With_Add_And_Shift(t *testing.T) {
	root := mustParse(t, `2 ** 3 + 4 << 1`)
	top := first(root)
	wantTag(t, top, "binop")
	if top[1].(string) != "<<" {
		t.Fatalf("top not '<<': %s", dump(top))
	}
	left := top[2].(S)
	wantTag(t, left, "binop")
	if left[1].(string) != "+" {
		t.Fatalf("left of '<<' not '+': %s", dump(left))
	}
	addLeft := left[2].(S)
	wantTag(t, addLeft, "binop")
	if addLeft[1].(string) != "**" {
		t.Fatalf("left of '+' not '**': %s", dump(addLeft))
	}
}

func Test_Parser_Bitwise_Combine_With_Logical(t *testing.T) {
	root := mustParse(t, `a & b | c and d`)
	top := first(root)
	wantTag(t, top, "binop")
	if top[1].(string) != "and" {
		t.Fatalf("top not 'and': %s", dump(top))
	}
	left := top[2].(S)
	wantTag(t, left, "binop")
	if left[1].(string) != "|" {
		t.Fatalf("left of 'and' not '|': %s", dump(left))
	}
	lhs2 := left[2].(S)
	wantTag(t, lhs2, "binop")
	if lhs2[1].(string) != "&" {
		t.Fatalf("left of '|' not '&': %s", dump(lhs2))
	}
}
