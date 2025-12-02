package mindscript

import (
	"fmt"
	"strings"
	"testing"
	"time"
)

// --- small helpers ----------------------------------------------------------

func evalWithIP(t *testing.T, ip *Interpreter, src string) Value {
	t.Helper()
	v, err := ip.EvalSource(src)
	if err != nil {
		t.Fatalf("EvalSource error: %v\nsource:\n%s", err, src)
	}
	return v
}

// When a contractual mistake occurs (e.g., type/arity mismatch), EvalSource should return a Go error.
// This helper asserts that and checks the error message contains `substr` (case-insensitive).
func evalExpectError(t *testing.T, ip *Interpreter, src string, substr string) {
	t.Helper()
	v, err := ip.EvalSource(src)
	if err == nil {
		t.Fatalf("expected error containing %q, got value: %#v\nsource:\n%s", substr, v, src)
	}
	if !strings.Contains(strings.ToLower(err.Error()), strings.ToLower(substr)) {
		t.Fatalf("expected error to contain %q, got: %v", substr, err)
	}
}

// parse `type <expr>` and return the underlying type S (the AST stored in VTType)
func typeS(t *testing.T, ip *Interpreter, src string) S {
	t.Helper()
	v := evalWithIP(t, ip, "type "+src)
	if v.Tag != VTType {
		t.Fatalf("want VTType, got %#v", v)
	}
	tv, ok := v.Data.(*TypeValue)
	if !ok {
		t.Fatalf("VTType payload was %T (want *TypeValue)", v.Data)
	}
	return tv.Ast
}

// --- isType (runtime checking) ----------------------------------------------

func Test_Types_IsType_ArrayNullable(t *testing.T) {
	ip, _ := NewInterpreter()
	// value: [1, 3, null]
	val := evalWithIP(t, ip, "[1,3,null]")
	// type: [Int?]
	ts := typeS(t, ip, "[Int?]")

	if !ip.isType(val, ts, ip.Global) {
		t.Fatalf("expected [1,3,null] to match type [Int?]")
	}
}

func Test_Types_IsType_Map_Required_Optional(t *testing.T) {
	ip, _ := NewInterpreter()

	// Matches: required field present and typed; optional may be absent
	val1 := evalWithIP(t, ip, `{name: "Ada"}`)
	t1 := typeS(t, ip, `{name!: Str, age: Int?}`)
	if !ip.isType(val1, t1, ip.Global) {
		t.Fatalf("expected {name:\"Ada\"} to match {name!: Str, age: Int?}")
	}

	// Fails: missing required field
	val2 := evalWithIP(t, ip, `{age: 42}`)
	if ip.isType(val2, t1, ip.Global) {
		t.Fatalf("did not expect {age:42} to match {name!: Str, age: Int?}")
	}
}

// --- isSubtype (structural subtyping) ---------------------------------------

func Test_Types_Subtype_Arrays_And_Nullable(t *testing.T) {
	ip, _ := NewInterpreter()
	intArr := typeS(t, ip, `[Int]`)
	numArr := typeS(t, ip, `[Num]`)
	if !ip.isSubtype(intArr, numArr, ip.Global) {
		t.Fatalf("[Int] <: [Num] expected")
	}
	intOpt := typeS(t, ip, `Int?`)
	numOpt := typeS(t, ip, `Num?`)
	if !ip.isSubtype(intOpt, numOpt, ip.Global) {
		t.Fatalf("Int? <: Num? expected")
	}
}

func Test_Types_Subtype_Objects_RequiredPropagation(t *testing.T) {
	ip, _ := NewInterpreter()
	// required(super) ⊆ required(sub)
	sub := typeS(t, ip, `{name!: Str, age: Int}`) // requires name
	super := typeS(t, ip, `{name!: Str}`)         // requires name
	if !ip.isSubtype(sub, super, ip.Global) {
		t.Fatalf("{name!:Str,age:Int} <: {name!:Str} expected")
	}

	// super requires name!, sub only has optional name → not a subtype
	sub2 := typeS(t, ip, `{name: Str}`) // optional name
	if ip.isSubtype(sub2, super, ip.Global) {
		t.Fatalf("{name:Str} </: {name!:Str} expected")
	}
}

func Test_Types_Subtype_Functions_ContraParams_CoReturn(t *testing.T) {
	ip, _ := NewInterpreter()
	// Parameter contravariance: Num->Str <: Int->Str (since Int <: Num).
	sub := typeS(t, ip, `Num -> Str`)
	super := typeS(t, ip, `Int -> Str`)
	if !ip.isSubtype(sub, super, ip.Global) {
		t.Fatalf("Num->Str <: Int->Str expected (param contravariance)")
	}

	// Return covariance: Int -> Int  <:  Int -> Num
	sub2 := typeS(t, ip, `Int -> Int`)
	super2 := typeS(t, ip, `Int -> Num`)
	if !ip.isSubtype(sub2, super2, ip.Global) {
		t.Fatalf("Int->Int <: Int->Num expected")
	}
}

// --- Unification via valueToTypeS (typeOf-like inference) -------------------

func Test_Types_Unify_Array_Inference_To_IntOptional(t *testing.T) {
	ip, _ := NewInterpreter()
	v := evalWithIP(t, ip, `[1, 3, null]`)
	got := ip.valueToTypeS(v, ip.Global)

	want := typeS(t, ip, `[Int?]`)
	if !ip.isSubtype(got, want, ip.Global) || !ip.isSubtype(want, got, ip.Global) {
		t.Fatalf("inferred array type mismatch: got %#v, want %#v", got, want)
	}
}

func Test_Types_Unify_Objects_Fieldwise(t *testing.T) {
	ip, _ := NewInterpreter()
	// Unify [{"name":"a"}, {"age":1}]  ~> {name: Str, age: Int} (both optional)
	v := evalWithIP(t, ip, `[ {name:"a"}, {age:1} ]`)
	// Infer element type by unifying the two map element types:
	arrT := ip.valueToTypeS(v, ip.Global) // should be ("array", elemT)
	if len(arrT) != 2 || arrT[0].(string) != "array" {
		t.Fatalf("expected array type, got %#v", arrT)
	}
	elemT := arrT[1].(S)

	want := typeS(t, ip, `{name: Str, age: Int}`)
	if !ip.isSubtype(elemT, want, ip.Global) || !ip.isSubtype(want, elemT, ip.Global) {
		t.Fatalf("inferred object element type mismatch: got %#v, want %#v", elemT, want)
	}
}

// --- Runtime enforcement at call sites (param and return) -------------------

// Was: ...AnnotatedNull; now contractual type mismatch → Go error.
func Test_Types_Runtime_Param_Mismatch_GoError(t *testing.T) {
	ip, _ := NewInterpreter()
	// (fun(x:Int)->Int do x end)("str")
	src := `(fun(x: Int) -> Int do x end)("str")`
	evalExpectError(t, ip, src, "type mismatch")
}

// Was: ...AnnotatedNull; now return type mismatch → Go error.
func Test_Types_Runtime_Return_Mismatch_GoError(t *testing.T) {
	ip, _ := NewInterpreter()
	// fun()->Int returns "nope" (Str) → Go error
	src := `(fun() -> Int do "nope" end)()`
	evalExpectError(t, ip, src, "return type mismatch")
}

func Test_Types_Alias_Resolution_And_CycleGuard(t *testing.T) {
	ip, _ := NewInterpreter()

	// Persist the alias so resolveType can find it in ip.Global
	if _, err := ip.EvalPersistentSource(`let Age = type Int`); err != nil {
		t.Fatalf("setup error: %v", err)
	}

	// Using the alias should now type-check and pass 1 through
	v := evalWithIP(t, ip, `(fun(x: Age) -> Age do x end)(1)`)
	if v.Tag != VTInt || v.Data.(int64) != 1 {
		t.Fatalf("alias not respected, got %#v", v)
	}

	// Cycle guard: defining T = type T should not infinitely expand
	if _, err := ip.EvalPersistentSource(`let T = type T`); err != nil {
		t.Fatalf("setup error: %v", err)
	}

	// type T should remain an unresolved id node ("id","T")
	typ := typeS(t, ip, `T`)
	if typ[0].(string) != "id" || typ[1].(string) != "T" {
		t.Fatalf("cycle guard failed, got %#v", typ)
	}

	// Unknown id that didn't resolve => closed world: false
	if ip.isType(Int(1), typ, ip.Global) {
		t.Fatalf("unresolved id should not match values")
	}
}

func Test_Types_Map_ExtraFieldsAllowed(t *testing.T) {
	ip, _ := NewInterpreter()
	val := evalWithIP(t, ip, `{name:"Ada", age:36, lang:"EN"}`)
	typ := typeS(t, ip, `{name!: Str}`)
	if !ip.isType(val, typ, ip.Global) {
		t.Fatalf("extra fields should be allowed")
	}
}

func Test_Types_IsType_FunctionValueAgainstType(t *testing.T) {
	ip, _ := NewInterpreter()
	// With contravariant params: Num->Str <: Int->Str
	fn := evalWithIP(t, ip, `fun(x: Num) -> Str do "ok" end`)
	want := typeS(t, ip, `Int -> Str`)
	if !ip.isType(fn, want, ip.Global) { // Num->Str <: Int->Str
		t.Fatalf("function value should satisfy type via param contravariance")
	}
}

func Test_Types_Unify_NullableRules(t *testing.T) {
	ip, _ := NewInterpreter()
	u1 := ip.unifyTypes(typeS(t, ip, `Null`), typeS(t, ip, `Int`), ip.Global)
	if fmt.Sprintf("%#v", u1) != fmt.Sprintf("%#v", typeS(t, ip, `Int?`)) {
		t.Fatalf("Null ⊔ Int = Int?; got %#v", u1)
	}
	u2 := ip.unifyTypes(typeS(t, ip, `Int?`), typeS(t, ip, `Str?`), ip.Global)
	// (Int ⊔ Str) = Any → drop '?'
	if !ip.isSubtype(u2, typeS(t, ip, `Any`), ip.Global) || !ip.isSubtype(typeS(t, ip, `Any`), u2, ip.Global) {
		t.Fatalf("Int? ⊔ Str? should widen to Any (not Any?)")
	}
}

func Test_Types_FunctionChains_SubtypeAndUnify(t *testing.T) {
	ip, _ := NewInterpreter()
	// Param contravariance across chains:
	sub := typeS(t, ip, `Num -> Str -> Num`)
	super := typeS(t, ip, `Int -> Str -> Num`)
	if !ip.isSubtype(sub, super, ip.Global) {
		t.Fatalf("chained function subtype failed")
	}
	// LUB(Int->Int->Int, Num->Num->Num) = Int -> Int -> Num
	u := ip.unifyTypes(typeS(t, ip, `Int -> Int -> Int`), typeS(t, ip, `Num -> Num -> Num`), ip.Global)
	want := typeS(t, ip, `Int -> Int -> Num`)
	if !equalLiteralS(u, want) {
		t.Fatalf("function unify mismatch:\n got  %#v\n want %#v", u, want)
	}
	// And the result must be a supertype of both inputs
	if !ip.isSubtype(typeS(t, ip, `Int -> Int -> Int`), u, ip.Global) ||
		!ip.isSubtype(typeS(t, ip, `Num -> Num -> Num`), u, ip.Global) {
		t.Fatalf("unify result must be a supertype of both inputs; got %#v", u)
	}
}

func Test_Types_Unify_Functions_ParamGLB_ReturnLUB(t *testing.T) {
	ip, _ := NewInterpreter()
	f1 := typeS(t, ip, `Int -> Int`)
	f2 := typeS(t, ip, `Num -> Num`)
	u := ip.unifyTypes(f1, f2, ip.Global)

	// GLB(param) = Int, LUB(return) = Num  =>  Int -> Num
	want := typeS(t, ip, `Int -> Num`)
	if !equalLiteralS(u, want) {
		t.Fatalf("function unify mismatch: got %#v, want %#v", u, want)
	}
	if !ip.isSubtype(f1, u, ip.Global) || !ip.isSubtype(f2, u, ip.Global) {
		t.Fatalf("unify result must be a supertype of both inputs")
	}
}

func Test_Types_Unify_Functions_UnrelatedParams_YieldsAny(t *testing.T) {
	ip, _ := NewInterpreter()
	u := ip.unifyTypes(typeS(t, ip, `Int -> Num`), typeS(t, ip, `Str -> Num`), ip.Global)
	if !equalLiteralS(u, typeS(t, ip, `Any`)) {
		t.Fatalf("unifying functions with unrelated params should yield Any, got %#v", u)
	}
}

func Test_Types_Unify_Functions_EnumParam_Meet(t *testing.T) {
	ip, _ := NewInterpreter()
	u := ip.unifyTypes(typeS(t, ip, `Str -> Str`), typeS(t, ip, `Enum["a","b"] -> Str`), ip.Global)
	want := typeS(t, ip, `Enum["a","b"] -> Str`)
	if !equalLiteralS(u, want) {
		t.Fatalf("function unify (enum param) mismatch: got %#v, want %#v", u, want)
	}
	// Supertype property
	if !ip.isSubtype(typeS(t, ip, `Str -> Str`), u, ip.Global) ||
		!ip.isSubtype(typeS(t, ip, `Enum["a","b"] -> Str`), u, ip.Global) {
		t.Fatalf("unify result must be a supertype of both function types")
	}
}

func Test_Types_Unify_Functions_NestedNullableReturns(t *testing.T) {
	ip, _ := NewInterpreter()
	u := ip.unifyTypes(typeS(t, ip, `Int -> (Int -> Int?)`), typeS(t, ip, `Num -> (Num -> Null)`), ip.Global)
	// Param GLB: Int; inner param GLB: Int; inner return LUB: Int?
	want := typeS(t, ip, `Int -> Int -> Int?`)
	if !equalLiteralS(u, want) {
		t.Fatalf("nested function unify mismatch: got %#v, want %#v", u, want)
	}
}

func Test_Types_ArrayShape_Robustness(t *testing.T) {
	ip, _ := NewInterpreter()
	v := evalWithIP(t, ip, `[1,2,3]`)
	// Simulate malformed type node: ("array", Int, Num)
	mt := S{"array", S{"id", "Int"}, S{"id", "Num"}}
	if !ip.isType(v, mt, ip.Global) { // current resolveType keeps children; isType should default elem to Any or handle 1st
		t.Fatalf("array shape robustness")
	}
}

func Test_Types_Runtime_Param_AliasMismatch(t *testing.T) {
	ip, _ := NewInterpreter()
	evalWithIP(t, ip, `let Age = type Int`)
	evalExpectError(t, ip, `(fun(x: Age) -> Int do x end)("nope")`, "type mismatch")
}

// --- Enums -------------------------------------------------------------------

func Test_Types_Enum_IsType_StringLiterals(t *testing.T) {
	ip, _ := NewInterpreter()

	v1 := evalWithIP(t, ip, `"a"`)
	v2 := evalWithIP(t, ip, `"b"`)
	v3 := evalWithIP(t, ip, `"z"`)

	// Enum of strings
	et := typeS(t, ip, `Enum["a","b","c"]`)

	if !ip.isType(v1, et, ip.Global) || !ip.isType(v2, et, ip.Global) {
		t.Fatalf(`expected "a" and "b" to be in Enum["a","b","c"]`)
	}
	if ip.isType(v3, et, ip.Global) {
		t.Fatalf(`"z" should not be in Enum["a","b","c"]`)
	}
}

func Test_Types_Enum_IsType_IntLiterals(t *testing.T) {
	ip, _ := NewInterpreter()

	v1 := evalWithIP(t, ip, `42`)
	v2 := evalWithIP(t, ip, `7`)

	et := typeS(t, ip, `Enum[42, 7]`)

	if !ip.isType(v1, et, ip.Global) || !ip.isType(v2, et, ip.Global) {
		t.Fatalf("expected 42 and 7 to be in Enum[42,7]")
	}
}

func Test_Types_Enum_Subtype_SetInclusion(t *testing.T) {
	ip, _ := NewInterpreter()

	sub := typeS(t, ip, `Enum["a","b"]`)
	super := typeS(t, ip, `Enum["a","b","c"]`)
	if !ip.isSubtype(sub, super, ip.Global) {
		t.Fatalf(`Enum["a","b"] <: Enum["a","b","c"] expected`)
	}

	// Not a subtype when sub has a value absent in super
	notSub := typeS(t, ip, `Enum["x"]`)
	if ip.isSubtype(notSub, super, ip.Global) {
		t.Fatalf(`Enum["x"] </: Enum["a","b","c"] expected`)
	}
}

func Test_Types_Enum_Unify_Union(t *testing.T) {
	ip, _ := NewInterpreter()

	a := typeS(t, ip, `Enum["red","green"]`)
	b := typeS(t, ip, `Enum["green","blue"]`)
	u := ip.unifyTypes(a, b, ip.Global)

	want := typeS(t, ip, `Enum["red","green","blue"]`)
	if !ip.isSubtype(u, want, ip.Global) || !ip.isSubtype(want, u, ip.Global) {
		t.Fatalf("enum union mismatch: got %#v, want %#v", u, want)
	}
}

func Test_Types_Enum_Unify_WithStr_WidensToStr(t *testing.T) {
	ip, _ := NewInterpreter()

	e := typeS(t, ip, `Enum["x","y"]`)
	s := typeS(t, ip, `Str`)
	u := ip.unifyTypes(e, s, ip.Global)

	if !ip.isSubtype(u, s, ip.Global) || !ip.isSubtype(s, u, ip.Global) {
		t.Fatalf("Enum ⊔ Str should unify to Str, got %#v", u)
	}
}

func Test_Types_Enum_Nullable(t *testing.T) {
	ip, _ := NewInterpreter()

	// value may be null or one of the enum values
	v1 := evalWithIP(t, ip, `null`)
	v2 := evalWithIP(t, ip, `"go"`)

	tEnumOpt := typeS(t, ip, `Enum["go","py"]?`)

	if !ip.isType(v1, tEnumOpt, ip.Global) || !ip.isType(v2, tEnumOpt, ip.Global) {
		t.Fatalf("nullable enum acceptance failed")
	}
}

// --- Enums with runtime enforcement -----------------------------------------

func Test_Types_Runtime_Param_Enum_Ok_And_Mismatch(t *testing.T) {
	ip, _ := NewInterpreter()

	ok := evalWithIP(t, ip, `(fun(x: Enum["small","large"]) -> Str do "ok" end)("small")`)
	if ok.Tag != VTStr || ok.Data.(string) != "ok" {
		t.Fatalf("enum param should accept matching literal, got %#v", ok)
	}

	evalExpectError(t, ip, `(fun(x: Enum["small","large"]) -> Str do "ok" end)("medium")`, "type mismatch")
}

// --- Aliases to enums --------------------------------------------------------

func Test_Types_Alias_To_Enum(t *testing.T) {
	ip, _ := NewInterpreter()

	if _, err := ip.EvalPersistentSource(`let Color = type Enum["red","blue"]`); err != nil {
		t.Fatalf("setup error: %v", err)
	}

	v := evalWithIP(t, ip, `(fun(x: Color) -> Str do "ok" end)("red")`)
	if v.Tag != VTStr {
		t.Fatalf("alias to enum should work, got %#v", v)
	}

	evalExpectError(t, ip, `(fun(x: Color) -> Str do "ok" end)("green")`, "type mismatch")
}

// --- Arrays, nullables, and inference ---------------------------------------

func Test_Types_ArrayOfEnum_IsType_And_Inference(t *testing.T) {
	ip, _ := NewInterpreter()

	val := evalWithIP(t, ip, `["a","b","a"]`)
	tt := typeS(t, ip, `[Enum["a","b"]]`)
	if !ip.isType(val, tt, ip.Global) {
		t.Fatalf(`["a","b","a"] should match [Enum["a","b"]]`)
	}

	// Inference for ["a","b"] should yield an array whose element type
	// is <= Str (it may be exactly Str, which is fine).
	val2 := evalWithIP(t, ip, `["a","b"]`)
	arrT := ip.valueToTypeS(val2, ip.Global)
	if len(arrT) != 2 || arrT[0].(string) != "array" {
		t.Fatalf("expected array type, got %#v", arrT)
	}
	elemT := arrT[1].(S)
	if !ip.isSubtype(elemT, typeS(t, ip, `Str`), ip.Global) {
		t.Fatalf(`element type should be <= Str, got %#v`, elemT)
	}
}

func Test_Types_NestedNullableArrays(t *testing.T) {
	ip, _ := NewInterpreter()

	v := evalWithIP(t, ip, `[[1, null], null]`)
	tok := typeS(t, ip, `[ [Int?]? ]`) // outer array of (nullable array of Int?)
	if !ip.isType(v, tok, ip.Global) {
		t.Fatalf("nested nullable arrays should match")
	}
}

// --- Maps with enums and required/optional ----------------------------------

func Test_Types_Map_Field_Enum_Required_Optional(t *testing.T) {
	ip, _ := NewInterpreter()

	v1 := evalWithIP(t, ip, `{name:"Ada", color:"red"}`)
	v2 := evalWithIP(t, ip, `{name:"Ada"}`)

	// color is optional enum; name is required string
	tmap := typeS(t, ip, `{name!: Str, color: Enum["red","blue"]}`)
	if !ip.isType(v1, tmap, ip.Global) {
		t.Fatalf("optional enum field present should pass")
	}
	if !ip.isType(v2, tmap, ip.Global) {
		t.Fatalf("optional enum field absent should pass")
	}
}

func Test_Types_Unify_Maps_Required_AND_Rule(t *testing.T) {
	ip, _ := NewInterpreter()
	m1 := typeS(t, ip, `{a!: Int}`)
	m2 := typeS(t, ip, `{a:  Num}`)
	u := ip.unifyTypes(m1, m2, ip.Global)

	want := typeS(t, ip, `{a: Num}`) // AND → optional
	if !ip.isSubtype(u, want, ip.Global) || !ip.isSubtype(want, u, ip.Global) {
		t.Fatalf("map unify required AND rule failed: got %#v, want %#v", u, want)
	}
}

// --- Functions + enums, chained arrows --------------------------------------

func Test_Types_Function_Param_Enum_Contravariance(t *testing.T) {
	ip, _ := NewInterpreter()

	// Param contravariance with enums:
	// Str -> Str  <:  Enum["a"] -> Str  (since Enum["a"] <: Str)
	sub := typeS(t, ip, `Str -> Str`)
	super := typeS(t, ip, `Enum["a"] -> Str`)
	if !ip.isSubtype(sub, super, ip.Global) {
		t.Fatalf("Str->Str <: Enum[\"a\"]->Str expected (param contravariance)")
	}
}

func Test_Types_FunctionValue_Against_EnumParamType(t *testing.T) {
	ip, _ := NewInterpreter()

	// Provide a function with a broader param (Str),
	// check it against a type expecting a narrower Enum[...] param.
	f := evalWithIP(t, ip, `fun(x: Str) -> Str do "ok" end`)
	ty := typeS(t, ip, `Enum["dev","prod"] -> Str`)
	if !ip.isType(f, ty, ip.Global) { // Str->Str <: Enum[...] -> Str
		t.Fatalf("function value should satisfy Enum[...] -> Str via param contravariance")
	}
}

func Test_Types_Alias_State_With_Enum(t *testing.T) {
	ip, _ := NewInterpreter()

	// Define the alias persistently so resolveType can find it.
	if _, err := ip.EvalPersistentSource(`let State = type {id: Int, status!: Enum["running","terminated"]}`); err != nil {
		t.Fatalf("setup error: %v", err)
	}

	// 1) Value matches the alias (id optional, status required, enum ok)
	okVal := evalWithIP(t, ip, `{id: 1, status: "running"}`)
	if !ip.isType(okVal, typeS(t, ip, `State`), ip.Global) {
		t.Fatalf(`expected {id:1,status:"running"} to match State`)
	}

	// 2) Enum mismatch should fail isType
	badEnum := evalWithIP(t, ip, `{id: 1, status: "paused"}`)
	if ip.isType(badEnum, typeS(t, ip, `State`), ip.Global) {
		t.Fatalf(`did not expect {id:1,status:"paused"} to match State`)
	}

	// 3) Missing required status → fail
	missingStatus := evalWithIP(t, ip, `{id: 1}`)
	if ip.isType(missingStatus, typeS(t, ip, `State`), ip.Global) {
		t.Fatalf(`did not expect {id:1} to match State (status! missing)`)
	}

	// 4) Using State in a function parameter (ok)
	okCall := evalWithIP(t, ip, `(fun(s: State) -> Str do s.status end)({status: "terminated"})`)
	if okCall.Tag != VTStr || okCall.Data.(string) != "terminated" {
		t.Fatalf(`expected function to return "terminated", got %#v`, okCall)
	}

	// 5) Using State in a function parameter (enum mismatch → Go error)
	evalExpectError(t, ip, `(fun(s: State) -> Str do s.status end)({status: "paused"})`, "type mismatch")
}

// --- VTModule structural equivalence to maps --------------------------------

func Test_Types_IsType_Module_As_Map(t *testing.T) {
	ip, _ := NewInterpreter()

	// Build a module with export: foo = 1
	mo := &MapObject{
		Entries: map[string]Value{"foo": Int(1)},
		Keys:    []string{"foo"},
	}
	mod := &Module{Map: mo, Env: NewEnv(ip.Global)}
	v := Value{Tag: VTModule, Data: mod}

	// Module should satisfy a map type that requires foo:Int
	tOK := typeS(t, ip, `{foo!: Int}`)
	if !ip.isType(v, tOK, ip.Global) {
		t.Fatalf("expected VTModule with {foo:1} to match {foo!: Int}")
	}

	// But it must fail if a *required* missing field is demanded
	tMissing := typeS(t, ip, `{foo!: Int, bar!: Num}`)
	if ip.isType(v, tMissing, ip.Global) {
		t.Fatalf("did not expect VTModule with {foo:1} to match {foo!: Int, bar!: Num}")
	}

	// Optional extra field should not be required
	tOptional := typeS(t, ip, `{foo!: Int, bar: Num}`)
	if !ip.isType(v, tOptional, ip.Global) {
		t.Fatalf("expected VTModule with {foo:1} to match {foo!: Int, bar: Num}")
	}
}

func Test_Types_ValueToType_Module_Inference(t *testing.T) {
	ip, _ := NewInterpreter()

	// Build a module with exports: x:"hi", n:2
	mo := &MapObject{
		Entries: map[string]Value{"x": Str("hi"), "n": Int(2)},
		Keys:    []string{"x", "n"},
	}
	mod := &Module{Map: mo, Env: NewEnv(ip.Global)}
	v := Value{Tag: VTModule, Data: mod}

	// ValueToType for a VTModule should:
	// - Treat the module as a map (AsMapValue),
	// - Mark observed exports as required fields (pair!),
	// - Remain open-world (extra fields still allowed by the type system).
	typ := ip.ValueToType(v, ip.Global)
	if len(typ) == 0 || typ[0].(string) != "map" {
		t.Fatalf("expected ValueToType(VTModule) to yield a 'map' type, got: %#v", typ)
	}

	fields := mapTypeFields(typ)

	fx, ok := fields["x"]
	if !ok || !testIsId(fx.typ, "Str") || !fx.required {
		t.Fatalf("expected field x: Str (required), got: %#v", fx)
	}

	fn, ok := fields["n"]
	if !ok || !testIsId(fn.typ, "Int") || !fn.required {
		t.Fatalf("expected field n: Int (required), got: %#v", fn)
	}
}

func Test_Types_IsType_Map_FieldAnnotations_Single(t *testing.T) {
	ip, _ := NewInterpreter()

	// Value with one field
	val := evalWithIP(t, ip, `{root: "."}`)

	// Type whose field type is annotated (annotation should be ignored)
	ts := typeS(t, ip, `{root:
# directory to search
Str}`)

	if !ip.isType(val, ts, ip.Global) {
		t.Fatalf("expected {root:\".\"} to match annotated type {root: #(doc) Str}")
	}
}

func Test_Types_IsType_Map_FieldAnnotations_Combo(t *testing.T) {
	ip, _ := NewInterpreter()

	// Three values with increasing fields present
	v0 := evalWithIP(t, ip, `{}`)
	v1 := evalWithIP(t, ip, `{root: "."}`)
	v2 := evalWithIP(t, ip, `{root: ".", parallel: true, quiet: false}`)

	// Type with all-optional fields whose inner types are annotated
	ts := typeS(t, ip, `{
  root:
# root directory (default ".")
  Str,
  parallel:
# reserved; currently ignored
  Bool,
  quiet:
# suppress discovery logs
  Bool
}`)

	for i, v := range []Value{v0, v1, v2} {
		if !ip.isType(v, ts, ip.Global) {
			t.Fatalf("case %d: expected value to conform to annotated opts type", i)
		}
	}
}

func Test_Types_Subtype_And_Unify_AnnotationTransparent(t *testing.T) {
	ip, _ := NewInterpreter()

	// Same shape; left has annotation on field type, right does not.
	a := typeS(t, ip, `{x:
# doc
Int}`)
	b := typeS(t, ip, `{x: Int}`)

	// Subtyping should treat annotations as no-ops (both directions).
	if !ip.isSubtype(a, b, ip.Global) {
		t.Fatalf("expected annotated a <: b")
	}
	if !ip.isSubtype(b, a, ip.Global) {
		t.Fatalf("expected b <: annotated a")
	}

	// Unification should also drop annotations (result == unannotated shape).
	u := ip.unifyTypes(a, b, ip.Global)
	if !equalLiteralS(u, b) {
		t.Fatalf("expected unify(a,b) to equal unannotated type; got %v", u)
	}
}

func Test_Types_ValueToType_Array_SelfCycle(t *testing.T) {
	ip, _ := NewInterpreter()

	// Build a self-referential array:
	//   let a = [null]; a[0] = a; a
	v := evalWithIP(t, ip, "let a = [null]\na[0] = a\na")

	got := ip.valueToTypeS(v, ip.Global)
	// Expect conservative widening to [Any] rather than infinite recursion.
	want := typeS(t, ip, `[Any]`)
	if !equalLiteralS(got, want) {
		t.Fatalf("valueToTypeS on cyclic array: got %#v, want %#v", got, want)
	}
}

func Test_Types_ValueToType_Map_SelfCycle(t *testing.T) {
	ip, _ := NewInterpreter()

	// Self-referential map with an extra normal field:
	//   let m = {}; m.self = m; m.x = 1; m
	v := evalWithIP(t, ip, "let m = {}\nm.self = m\nm.x = 1\nm")

	got := ip.valueToTypeS(v, ip.Global)
	if len(got) == 0 || got[0].(string) != "map" {
		t.Fatalf("expected map type, got %#v", got)
	}

	fs := mapTypeFields(got)
	// self: Any (widened due to cycle), x: Int (optional/open-world)
	if f, ok := fs["self"]; !ok || !testIsId(f.typ, "Any") {
		t.Fatalf("expected field self: Any (from cycle), got %#v", f)
	}
	if f, ok := fs["x"]; !ok || !testIsId(f.typ, "Int") {
		t.Fatalf("expected field x: Int, got %#v", f)
	}
}

func Test_Types_IsType_Array_SelfCycle_NoHang(t *testing.T) {
	ip, _ := NewInterpreter()

	// Cyclic array again
	v := evalWithIP(t, ip, "let a = [null]\na[0] = a\na")

	// Should trivially pass against [Any] without recursing forever.
	tAny := typeS(t, ip, `[Any]`)
	if !ip.isType(v, tAny, ip.Global) {
		t.Fatalf("cyclic array should satisfy [Any]")
	}

	// And should fail (quickly) against [Int].
	tInt := typeS(t, ip, `[Int]`)
	if ip.isType(v, tInt, ip.Global) {
		t.Fatalf("cyclic array should not satisfy [Int]")
	}
}

func Test_Types_IsType_Map_SelfCycle_NoHang(t *testing.T) {
	ip, _ := NewInterpreter()

	// Self-referential map with a valid required field
	v := evalWithIP(t, ip, "let m = {}\nm.self = m\nm.x = 1\nm")

	// Type only requires x!: Int; presence of self (cyclic) must not hang.
	tReq := typeS(t, ip, `{x!: Int}`)
	if !ip.isType(v, tReq, ip.Global) {
		t.Fatalf("self-referential map should satisfy {x!: Int}")
	}
}

func Test_Types_ResolveType_FunctionalCycle_NoHang(t *testing.T) {
	ip, _ := NewInterpreter()

	// F = Int -> F
	if _, err := ip.EvalPersistentSource(`let F = type Int -> F`); err != nil {
		t.Fatalf("setup error: %v", err)
	}

	// Public ResolveType(id) now returns an alias node.
	res := ip.ResolveType(typeS(t, ip, `F`), ip.Global)
	if len(res) < 2 || res[0].(string) != "alias" {
		t.Fatalf("expected alias node, got %#v", res)
	}

	// Quick sanity: payload is a *TypeValue whose AST is "Int -> F".
	tv, ok := res[1].(*TypeValue)
	if !ok || tv == nil {
		t.Fatalf("expected *TypeValue payload, got %#v", res[1])
	}
	ast := tv.Ast
	if !(len(ast) >= 4 && ast[0].(string) == "binop" && ast[1].(string) == "->") {
		t.Fatalf("alias AST should be a function type, got %#v", ast)
	}
}

// --- recursion / no-hang guards ---------------------------------------------

func Test_Types_Runtime_Return_Check_Recursive_NoHang(t *testing.T) {
	ip, _ := NewInterpreter()

	src := `
let T = type { f!: Null -> T }

let mk = fun() -> T do
  let o = {}
  o.f = fun(_: Null) -> T do
    return o
  end
  return o
end

mk()
`
	done := make(chan struct{})
	go func() {
		// We only assert it *returns* (success or error) without hanging.
		_, _ = ip.EvalSource(src)
		close(done)
	}()

	select {
	case <-done:
		// ok: did not hang
	case <-time.After(500 * time.Millisecond):
		t.Fatal("runtime return type check on recursive T hung")
	}
}

func Test_Types_IsSubtype_Recursive_Function_Type_NoHang(t *testing.T) {
	ip, _ := NewInterpreter()

	// Persist the recursive alias: T = { f!: Null -> T }
	if _, err := ip.EvalPersistentSource(`let T = type { f!: Null -> T }`); err != nil {
		t.Fatalf("setup error: %v", err)
	}

	// Check subtyping on the recursive arrow type does not hang.
	a := typeS(t, ip, `Null -> T`)
	b := typeS(t, ip, `Null -> T`)

	done := make(chan struct{})
	go func() {
		_ = ip.isSubtype(a, b, ip.Global) // result value unimportant; liveness matters
		close(done)
	}()

	select {
	case <-done:
		// ok: did not hang
	case <-time.After(500 * time.Millisecond):
		t.Fatal("isSubtype on recursive function type hung")
	}
}

func Test_Types_Recursive_MapFun_Coinductive(t *testing.T) {
	ip, _ := NewInterpreter()

	// Recursive alias: T = { f!: Null -> T }
	if _, err := ip.EvalPersistentSource(`let T = type { f!: Null -> T }`); err != nil {
		t.Fatalf("setup error: %v", err)
	}

	// Function builds a self-referential object of type T and returns it.
	src := `
let mk = fun() -> T do
  let o = {}
  o.f = fun(_: Null) -> T do
    return o
  end
  return o
end
mk()
`
	v := evalWithIP(t, ip, src)

	// The returned value should conform to T.
	typ := typeS(t, ip, `T`)
	if !ip.isType(v, typ, ip.Global) {
		t.Fatalf("expected mk() to yield value of type T, got %#v", v)
	}
}

func Test_Types_Module_QualifiedType_Equals_Local(t *testing.T) {
	src := `
let M = module "M" do
  let T = type { v!: Int }
  let use = fun(x: T) -> Int do
    x.v
  end
end

# Local alias path (control)
let T = M.T
let ok = fun(x: T) -> Int do M.use(x) end
ok({v: 1})

# Qualified path must be accepted by callee expecting local T
let boom = fun(x: M.T) -> Int do M.use(x) end
boom({v: 2})
`
	wantInt(t, evalSrc(t, src), 2)
}

func Test_Types_QualifiedType_Alias_Outside_Module(t *testing.T) {
	src := `
let M = module "M" do
  let T = type { v!: Int }
  let use = fun(x: T) -> Int do x.v end
end

# Alias M.T into caller scope; should behave exactly like local T
let N_T = M.T
let f = fun(x: N_T) -> Int do M.use(x) end
f({v: 3})
`
	wantInt(t, evalSrc(t, src), 3)
}

func Test_Types_QualifiedType_Unknown_Export_Errors_Nicely(t *testing.T) {
	src := `
let M = module "M" do
  let T = type { v!: Int }
end

let f = fun(x: M.U) -> Int do 0 end
f({v: 1})
`
	err := evalSrcExpectError(t, src)
	// Accept either the property lookup error OR the parameter type-mismatch path.
	msg := strings.ToLower(err.Error())
	if !strings.Contains(msg, "unknown property") && !strings.Contains(msg, "type mismatch") {
		t.Fatalf("want error mentioning unknown property or type mismatch, got: %v", err)
	}
}

// --- module aliases & recursive types ---------------------------------------

func Test_Types_ModuleAlias_EqualityAndSubtype(t *testing.T) {
	ip, _ := NewInterpreter()

	// Make the module persistent so later evals & typeS can see M.
	if _, err := ip.EvalPersistentSource(`
let M = module "M" do
  let A = type { x!: Int }
end`); err != nil {
		t.Fatalf("EvalPersistentSource error: %v", err)
	}

	// Local alias of the exported type in Global (also persistent).
	if _, err := ip.EvalPersistentSource(`let A = M.A`); err != nil {
		t.Fatalf("EvalPersistentSource error: %v", err)
	}

	// Types as written
	tMod := typeS(t, ip, `M.A`)
	tLocal := typeS(t, ip, `A`)

	// They should be structurally equal under alias canonicalization…
	if !equalLiteralS(ip.resolveType(tMod, ip.Global), ip.resolveType(tLocal, ip.Global)) {
		t.Fatalf("expected resolveType(M.A) == resolveType(A)")
	}

	// …and mutually subtypes.
	if !ip.isSubtype(tMod, tLocal, ip.Global) || !ip.isSubtype(tLocal, tMod, ip.Global) {
		t.Fatalf("expected M.A and local A to be mutual subtypes")
	}
}

func Test_Types_ModuleQualified_RecursiveParams_Accepts(t *testing.T) {
	ip, _ := NewInterpreter()

	// Define M persistently so later calls see it.
	if _, err := ip.EvalPersistentSource(`
let M = module "M" do
  let A = type { x!: Int }
  let B = type { k: Null -> B }     # recursive
  let S = type A -> B -> Null
  let t = fun(h: S) -> Bool do true end
end`); err != nil {
		t.Fatalf("EvalPersistentSource error: %v", err)
	}

	// a function using fully-qualified params must satisfy M.S without local aliasing
	evalWithIP(t, ip, `
let f1 = fun(a: M.A, b: M.B) -> Null do null end
M.t(f1)
`)
}

func Test_Types_InlineFunctionType_ExportedParam(t *testing.T) {
	ip, _ := NewInterpreter()

	if _, err := ip.EvalPersistentSource(`
let M = module "M" do
  let A = type { x!: Int }
  let w = fun(h: A -> Any) -> Bool do true end
end`); err != nil {
		t.Fatalf("EvalPersistentSource error: %v", err)
	}

	// Return Any? is fine when the param expects Any (covariant returns)
	evalWithIP(t, ip, `
let f = fun(a: M.A) -> Any? do 0 end
M.w(f)
`)
}

func Test_Types_Function_IsType_WithModuleAliases(t *testing.T) {
	ip, _ := NewInterpreter()

	// Persist the module first so typeS("M.S") can resolve it later.
	if _, err := ip.EvalPersistentSource(`
let M = module "M" do
  let A = type { x!: Int }
  let B = type { k: Null -> B }
  let S = type A -> B -> Null
end`); err != nil {
		t.Fatalf("EvalPersistentSource error: %v", err)
	}

	// Produce a function value referencing M.A/M.B in a regular (ephemeral) run.
	fv := evalWithIP(t, ip, `
fun(a: M.A, b: M.B) -> Null do null end
`)

	// Expected type is the exported alias M.S (A -> B -> Null) — typeS uses EvalSource,
	// which now resolves against Global where M is bound.
	ts := typeS(t, ip, `M.S`)

	if !ip.isType(fv, ts, ip.Global) {
		t.Fatalf("expected function (a:M.A,b:M.B)->Null to conform to type M.S")
	}
}

func Test_Types_ModuleAlias_LocalAliasCallsOK(t *testing.T) {
	ip, _ := NewInterpreter()
	src := `
let M = module "M" do
  let A = type { x!: Int }
  let S = type A -> Any
  let t = fun(h: S) -> Str do "OK" end
end

let A2 = M.A

[
  M.t(fun(a: M.A) -> Any do 0 end),
  M.t(fun(a: A2)  -> Any do 0 end)
]
`
	v := evalWithIP(t, ip, src)
	if v.Tag != VTArray {
		t.Fatalf("want array result, got %#v", v)
	}
	xs := v.Data.(*ArrayObject).Elems
	if len(xs) != 2 || xs[0].Tag != VTStr || xs[1].Tag != VTStr ||
		xs[0].Data.(string) != "OK" || xs[1].Data.(string) != "OK" {
		t.Fatalf("expected [\"OK\",\"OK\"], got %#v", v)
	}
}

func Test_Types_NestedModule_ResolveAndCallOK(t *testing.T) {
	ip, _ := NewInterpreter()
	src := `
let M = module "M" do
  let T = module "T" do
    let A = type { x!: Int }
  end
  let S = type T.A -> Any
  let t = fun(h: S) -> Str do "OK" end
end

M.t(fun(a: M.T.A) -> Any do 0 end)
`
	v := evalWithIP(t, ip, src)
	if v.Tag != VTStr || v.Data.(string) != "OK" {
		t.Fatalf(`expected "OK", got %#v`, v)
	}
}

func Test_Types_ModuleAlias_LocalAliasEquality(t *testing.T) {
	ip, _ := NewInterpreter()

	// Persistent setup (matches REPL)
	if _, err := ip.EvalPersistentSource(`
let M = module "M" do
  let A = type { x!: Int }
  let S = type A -> Any
end
let S2 = M.S   # top-level alias to a module-exported type
`); err != nil {
		t.Fatalf("setup error: %v", err)
	}

	// Ask the interpreter for the VALUES.
	mSVal, err := ip.EvalPersistentSource(`M.S`)
	if err != nil {
		t.Fatalf("get M.S: %v", err)
	}
	if mSVal.Tag != VTType {
		t.Fatalf("M.S: expected VTType, got %v", mSVal.Tag)
	}

	s2Val, err := ip.EvalPersistentSource(`S2`)
	if err != nil {
		t.Fatalf("get S2: %v", err)
	}
	if s2Val.Tag != VTType {
		t.Fatalf("S2: expected VTType, got %v", s2Val.Tag)
	}

	// Build alias nodes inline so the type engine uses the type’s own Env.
	mS := S{"alias", mSVal.Data.(*TypeValue)}
	s2 := S{"alias", s2Val.Data.(*TypeValue)}

	// Semantic equality: mutual subtyping
	if !ip.isSubtype(mS, s2, ip.Global) || !ip.isSubtype(s2, mS, ip.Global) {
		t.Fatalf("expected M.S and S2 to be semantically equivalent (mutual subtypes)")
	}
}

func Test_Types_ModuleAlias_LocalAliasEquality_Min(t *testing.T) {
	ip, _ := NewInterpreter()

	if _, err := ip.EvalPersistentSource(`
let M = module "M" do
  let A = type { x!: Int }
  let S = type A -> Any
end
let S2 = M.S
`); err != nil {
		t.Fatalf("setup error: %v", err)
	}

	mSVal, err := ip.EvalPersistentSource(`M.S`)
	if err != nil {
		t.Fatalf("get M.S: %v", err)
	}
	if mSVal.Tag != VTType {
		t.Fatalf("M.S: expected VTType, got %v", mSVal.Tag)
	}

	s2Val, err := ip.EvalPersistentSource(`S2`)
	if err != nil {
		t.Fatalf("get S2: %v", err)
	}
	if s2Val.Tag != VTType {
		t.Fatalf("S2: expected VTType, got %v", s2Val.Tag)
	}

	mS := S{"alias", mSVal.Data.(*TypeValue)}
	s2 := S{"alias", s2Val.Data.(*TypeValue)}

	if !ip.isSubtype(mS, s2, ip.Global) || !ip.isSubtype(s2, mS, ip.Global) {
		t.Fatalf("expected M.S and S2 to be semantically equivalent (mutual subtypes)")
	}
}

func Test_Types_NestedModule_TypePathResolves(t *testing.T) {
	ip, _ := NewInterpreter()

	if _, err := ip.EvalPersistentSource(`
let M = module "M" do
  let T = module "T" do
    let A = type { x!: Int }
  end
end
`); err != nil {
		t.Fatalf("setup error: %v", err)
	}

	// Ask the interpreter to build the *type values* (each is VTType with its own Env).
	pathFunVal, err := ip.EvalPersistentSource(`type M.T.A -> Any`)
	if err != nil {
		t.Fatalf("build path fun type: %v", err)
	}
	if pathFunVal.Tag != VTType {
		t.Fatalf("path fun type: expected VTType, got %v", pathFunVal.Tag)
	}

	shapeFunVal, err := ip.EvalPersistentSource(`type { x!: Int } -> Any`)
	if err != nil {
		t.Fatalf("build shape fun type: %v", err)
	}
	if shapeFunVal.Tag != VTType {
		t.Fatalf("shape fun type: expected VTType, got %v", shapeFunVal.Tag)
	}

	got := S{"alias", pathFunVal.Data.(*TypeValue)}
	want := S{"alias", shapeFunVal.Data.(*TypeValue)}

	// Mutual subtyping = semantic equivalence
	if !ip.isSubtype(got, want, ip.Global) || !ip.isSubtype(want, got, ip.Global) {
		t.Fatalf("M.T.A -> Any should be semantically equivalent to {x!: Int} -> Any (mutual subtypes)")
	}
}

func Test_Types_ModuleAlias_ReexportViaGlobalAlias(t *testing.T) {
	ip, _ := NewInterpreter()

	if _, err := ip.EvalPersistentSource(`
let M = module "M" do
  let A = type { x!: Int }
  let S = type A -> Any
end
let A2 = M.A
let S2 = M.S
`); err != nil {
		t.Fatalf("setup error: %v", err)
	}

	mSVal, err := ip.EvalPersistentSource(`M.S`)
	if err != nil {
		t.Fatalf("get M.S: %v", err)
	}
	if mSVal.Tag != VTType {
		t.Fatalf("M.S: expected VTType, got %v", mSVal.Tag)
	}

	gSVal, err := ip.EvalPersistentSource(`S2`)
	if err != nil {
		t.Fatalf("get S2: %v", err)
	}
	if gSVal.Tag != VTType {
		t.Fatalf("S2: expected VTType, got %v", gSVal.Tag)
	}

	mS := S{"alias", mSVal.Data.(*TypeValue)}
	gS := S{"alias", gSVal.Data.(*TypeValue)}

	if !ip.isSubtype(mS, gS, ip.Global) || !ip.isSubtype(gS, mS, ip.Global) {
		t.Fatalf("expected M.S and S2 to be semantically equivalent (mutual subtypes)")
	}
}

// --- validator (shape) ------------------------------------------------------

func Test_Types_Validate_Map_KeyAndValue_Annotations_OK(t *testing.T) {
	// { #(Age in years.) "age": #(doc) Int }
	typ := S{
		"map",
		S{"pair",
			S{"annot", S{"str", "Age in years."}, S{"str", "age"}}, // annotated KEY
			S{"annot", S{"str", "doc"}, S{"id", "Int"}},            // annotated VALUE
		},
	}
	if msg := validateTypeShape(typ); msg != "" {
		t.Fatalf("expected OK, got validator error: %s", msg)
	}

	// Sanity: mapTypeFields should see the unwrapped key "age" and Int
	fs := mapTypeFields(typ)
	f, ok := fs["age"]
	if !ok {
		t.Fatalf("expected key 'age' to be present after stripping annotation")
	}
	if f.required {
		t.Fatalf("expected 'age' to be optional")
	}
	if !equalLiteralS(f.typ, S{"id", "Int"}) {
		t.Fatalf("expected field type Int, got %v", f.typ)
	}
}

func Test_Types_Validate_Map_PairAnnotation_Reject(t *testing.T) {
	// #(doc) ("pair", "age", Int)  — NOT allowed: cannot annotate the pair itself
	typ := S{
		"map",
		S{"annot", S{"str", "doc"},
			S{"pair", S{"str", "age"}, S{"id", "Int"}},
		},
	}
	msg := validateTypeShape(typ)
	if msg == "" {
		t.Fatalf("expected validator to reject annotation wrapping the pair node")
	}
	if !strings.Contains(strings.ToLower(msg), "pair") {
		t.Fatalf("expected error mentioning pair requirement, got: %s", msg)
	}
}

func Test_Types_Validate_Enum_JSONOnly_OK(t *testing.T) {
	// Enum with only JSON literals: "yes", 1, {"k":"v"}, [null]
	typ := S{
		"enum",
		S{"str", "yes"},
		S{"int", int64(1)},
		S{"map", S{"pair", S{"str", "k"}, S{"str", "v"}}},
		S{"array", S{"null"}},
	}
	if msg := validateTypeShape(typ); msg != "" {
		t.Fatalf("expected JSON-only enum to pass, got: %s", msg)
	}
}

func Test_Types_Validate_Enum_JSONOnly_Reject_NonLiteral(t *testing.T) {
	// Enum containing a non-literal (type identifier "Str") should be rejected.
	typ1 := S{"enum", S{"id", "Str"}}
	if msg := validateTypeShape(typ1); msg == "" {
		t.Fatalf("expected enum with non-literal member (id Str) to be rejected")
	}

	// Also reject something obviously non-literal, like a function type node.
	typ2 := S{"enum", S{"binop", "->", S{"id", "Int"}, S{"id", "Int"}}}
	if msg := validateTypeShape(typ2); msg == "" {
		t.Fatalf("expected enum with function-type member to be rejected")
	}
}

func Test_Types_Validate_Map_DuplicateKeys_WithAnnotations_Reject(t *testing.T) {
	// Two fields that both resolve to key "age" after stripping key annotations.
	typ := S{
		"map",
		S{"pair", S{"annot", S{"str", "doc1"}, S{"str", "age"}}, S{"id", "Int"}},
		S{"pair", S{"annot", S{"str", "doc2"}, S{"str", "age"}}, S{"id", "Int"}},
	}
	msg := validateTypeShape(typ)
	if msg == "" {
		t.Fatalf("expected duplicate annotated keys to be rejected")
	}
	if !strings.Contains(strings.ToLower(msg), "duplicate field 'age'") {
		t.Fatalf("expected duplicate-key message for 'age', got: %s", msg)
	}
}

// (Optional) quick smoke test that a simple, valid map passes.
func Test_Types_Validate_Map_Simple_OK(t *testing.T) {
	typ := S{
		"map",
		S{"pair", S{"str", "name"}, S{"id", "Str"}},
		S{"pair!", S{"str", "age"}, S{"id", "Int"}},
	}
	if msg := validateTypeShape(typ); msg != "" {
		t.Fatalf("expected OK, got validator error: %s", msg)
	}
}

func Test_Types_Handle_IsType_SameKind(t *testing.T) {
	ip, _ := NewInterpreter()
	v := evalWithIP(t, ip, `chanOpen(1)`)
	ts := typeS(t, ip, `Handle."chan"`)
	if !ip.isType(v, ts, ip.Global) {
		t.Fatalf("expected handle to match Handle.\"chan\"")
	}
}

func Test_Types_Handle_IsType_DifferentKind(t *testing.T) {
	ip, _ := NewInterpreter()
	v := evalWithIP(t, ip, `chanOpen(1)`)
	ts := typeS(t, ip, `Handle."opengroup"`)
	if ip.isType(v, ts, ip.Global) {
		t.Fatalf("did not expect Handle.\"chan\" value to match Handle.\"opengroup\"")
	}
}

func Test_Types_Handle_Subtype_Same_Diff_Any_Nullable(t *testing.T) {
	ip, _ := NewInterpreter()
	hChan := typeS(t, ip, `Handle."chan"`)
	hOther := typeS(t, ip, `Handle."opengroup"`)

	// same kind
	if !ip.isSubtype(hChan, hChan, ip.Global) {
		t.Fatalf("Handle.\"chan\" <: Handle.\"chan\" expected")
	}
	// different kinds
	if ip.isSubtype(hChan, hOther, ip.Global) {
		t.Fatalf("Handle.\"chan\" </: Handle.\"opengroup\" expected")
	}
	// Any is top
	if !ip.isSubtype(hChan, typeS(t, ip, `Any`), ip.Global) {
		t.Fatalf("Handle.\"chan\" <: Any expected")
	}
	// Null <: Handle.kind?
	if !ip.isSubtype(typeS(t, ip, `Null`), typeS(t, ip, `Handle."chan"?`), ip.Global) {
		t.Fatalf("Null <: Handle.\"chan\"? expected")
	}
}

func Test_Types_Handle_Unify_Same_Diff_Nullable(t *testing.T) {
	ip, _ := NewInterpreter()
	hChan := typeS(t, ip, `Handle."chan"`)
	hOther := typeS(t, ip, `Handle."opengroup"`)
	gotSame := ip.unifyTypes(hChan, hChan, ip.Global)
	if !ip.isSubtype(gotSame, hChan, ip.Global) || !ip.isSubtype(hChan, gotSame, ip.Global) {
		t.Fatalf("LUB same-kinds mismatch: got %#v", gotSame)
	}
	gotDiff := ip.unifyTypes(hChan, hOther, ip.Global)
	if !ip.isSubtype(typeS(t, ip, `Any`), gotDiff, ip.Global) || !ip.isSubtype(gotDiff, typeS(t, ip, `Any`), ip.Global) {
		t.Fatalf("LUB different-kinds should be Any, got %#v", gotDiff)
	}
	gotNull := ip.unifyTypes(typeS(t, ip, `Null`), hChan, ip.Global)
	wantNull := typeS(t, ip, `Handle."chan"?`)
	if !ip.isSubtype(gotNull, wantNull, ip.Global) || !ip.isSubtype(wantNull, gotNull, ip.Global) {
		t.Fatalf("LUB with Null mismatch: got %#v, want %#v", gotNull, wantNull)
	}
}

func Test_Types_Handle_Infer_Solo_And_ArrayNullable(t *testing.T) {
	ip, _ := NewInterpreter()
	// Single handle infers its kind
	h := evalWithIP(t, ip, `chanOpen(1)`)
	got := ip.valueToTypeS(h, ip.Global)
	want := typeS(t, ip, `Handle."chan"`)
	if !ip.isSubtype(got, want, ip.Global) || !ip.isSubtype(want, got, ip.Global) {
		t.Fatalf("inferred handle type mismatch: got %#v, want %#v", got, want)
	}

	// Array unifies to [Handle.kind?] with null
	arr := evalWithIP(t, ip, `[chanOpen(1), null]`)
	gotArr := ip.valueToTypeS(arr, ip.Global)
	wantArr := typeS(t, ip, `[Handle."chan"?]`)
	if !ip.isSubtype(gotArr, wantArr, ip.Global) || !ip.isSubtype(wantArr, gotArr, ip.Global) {
		t.Fatalf("inferred array type mismatch: got %#v, want %#v", gotArr, wantArr)
	}
}

func Test_Types_Handle_ReservedName_ShadowingForbidden(t *testing.T) {
	ip, _ := NewInterpreter()
	// Interpreter should reject binding the reserved name "Handle" (mirrors Int/Num/etc.)
	evalExpectError(t, ip, `let Handle = type Int`, "reserved")
}

func Test_Types_Handle_PropertyName_Allowed(t *testing.T) {
	ip, _ := NewInterpreter()
	v := evalWithIP(t, ip, `{Handle: 1}.Handle`)
	if v.Tag != VTInt || v.Data.(int64) != 1 {
		t.Fatalf("expected property access to yield Int(1), got %#v", v)
	}
}

func Test_Types_Handle_ArrayOfSameKind(t *testing.T) {
	ip, _ := NewInterpreter()
	arr := evalWithIP(t, ip, `[chanOpen(1), chanOpen(2)]`)
	got := ip.valueToTypeS(arr, ip.Global)
	want := typeS(t, ip, `[Handle."chan"]`)
	if !ip.isSubtype(got, want, ip.Global) || !ip.isSubtype(want, got, ip.Global) {
		t.Fatalf("array of same-kind handles inference mismatch: got %#v, want %#v", got, want)
	}
}

func Test_Types_Handle_IsType_Nullable_AcceptsValueAndNull(t *testing.T) {
	ip, _ := NewInterpreter()
	// Non-null handle should match Handle.kind?
	val := evalWithIP(t, ip, `chanOpen(1)`)
	tn := typeS(t, ip, `Handle."chan"?`)
	if !ip.isType(val, tn, ip.Global) {
		t.Fatalf("expected Handle.\"chan\" to match nullable Handle.\"chan\"?")
	}
	// Null should also match Handle.kind?
	nv := evalWithIP(t, ip, `null`)
	if !ip.isType(nv, tn, ip.Global) {
		t.Fatalf("expected null to match nullable Handle.\"chan\"?")
	}
}
