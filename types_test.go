package mindscript

import (
	"fmt"
	"strings"
	"testing"
	"time"
)

// --- small helpers ----------------------------------------------------------

func newIP() *Interpreter { return NewInterpreter() }

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
	ip := newIP()
	// value: [1, 3, null]
	val := evalWithIP(t, ip, "[1,3,null]")
	// type: [Int?]
	ts := typeS(t, ip, "[Int?]")

	if !ip.isType(val, ts, ip.Global) {
		t.Fatalf("expected [1,3,null] to match type [Int?]")
	}
}

func Test_Types_IsType_Map_Required_Optional(t *testing.T) {
	ip := newIP()

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
	ip := newIP()
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
	ip := newIP()
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
	ip := newIP()
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
	ip := newIP()
	v := evalWithIP(t, ip, `[1, 3, null]`)
	got := ip.valueToTypeS(v, ip.Global)

	want := typeS(t, ip, `[Int?]`)
	if !ip.isSubtype(got, want, ip.Global) || !ip.isSubtype(want, got, ip.Global) {
		t.Fatalf("inferred array type mismatch: got %#v, want %#v", got, want)
	}
}

func Test_Types_Unify_Objects_Fieldwise(t *testing.T) {
	ip := newIP()
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
	ip := newIP()
	// (fun(x:Int)->Int do x end)("str")
	src := `(fun(x: Int) -> Int do x end)("str")`
	evalExpectError(t, ip, src, "type mismatch")
}

// Was: ...AnnotatedNull; now return type mismatch → Go error.
func Test_Types_Runtime_Return_Mismatch_GoError(t *testing.T) {
	ip := newIP()
	// fun()->Int returns "nope" (Str) → Go error
	src := `(fun() -> Int do "nope" end)()`
	evalExpectError(t, ip, src, "return type mismatch")
}

func Test_Types_Alias_Resolution_And_CycleGuard(t *testing.T) {
	ip := newIP()

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
	ip := newIP()
	val := evalWithIP(t, ip, `{name:"Ada", age:36, lang:"EN"}`)
	typ := typeS(t, ip, `{name!: Str}`)
	if !ip.isType(val, typ, ip.Global) {
		t.Fatalf("extra fields should be allowed")
	}
}

func Test_Types_IsType_FunctionValueAgainstType(t *testing.T) {
	ip := newIP()
	// With contravariant params: Num->Str <: Int->Str
	fn := evalWithIP(t, ip, `fun(x: Num) -> Str do "ok" end`)
	want := typeS(t, ip, `Int -> Str`)
	if !ip.isType(fn, want, ip.Global) { // Num->Str <: Int->Str
		t.Fatalf("function value should satisfy type via param contravariance")
	}
}

func Test_Types_Unify_NullableRules(t *testing.T) {
	ip := newIP()
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
	ip := newIP()
	// Param contravariance across chains:
	sub := typeS(t, ip, `Num -> Str -> Num`)
	super := typeS(t, ip, `Int -> Str -> Num`)
	if !ip.isSubtype(sub, super, ip.Global) {
		t.Fatalf("chained function subtype failed")
	}
	// LUB(Int->Int->Int, Num->Num->Num) = Int -> Int -> Num
	u := ip.unifyTypes(typeS(t, ip, `Int -> Int -> Int`), typeS(t, ip, `Num -> Num -> Num`), ip.Global)
	want := typeS(t, ip, `Int -> Int -> Num`)
	if !equalS(u, want) {
		t.Fatalf("function unify mismatch:\n got  %#v\n want %#v", u, want)
	}
	// And the result must be a supertype of both inputs
	if !ip.isSubtype(typeS(t, ip, `Int -> Int -> Int`), u, ip.Global) ||
		!ip.isSubtype(typeS(t, ip, `Num -> Num -> Num`), u, ip.Global) {
		t.Fatalf("unify result must be a supertype of both inputs; got %#v", u)
	}
}

func Test_Types_Unify_Functions_ParamGLB_ReturnLUB(t *testing.T) {
	ip := newIP()
	f1 := typeS(t, ip, `Int -> Int`)
	f2 := typeS(t, ip, `Num -> Num`)
	u := ip.unifyTypes(f1, f2, ip.Global)

	// GLB(param) = Int, LUB(return) = Num  =>  Int -> Num
	want := typeS(t, ip, `Int -> Num`)
	if !equalS(u, want) {
		t.Fatalf("function unify mismatch: got %#v, want %#v", u, want)
	}
	if !ip.isSubtype(f1, u, ip.Global) || !ip.isSubtype(f2, u, ip.Global) {
		t.Fatalf("unify result must be a supertype of both inputs")
	}
}

func Test_Types_Unify_Functions_UnrelatedParams_YieldsAny(t *testing.T) {
	ip := newIP()
	u := ip.unifyTypes(typeS(t, ip, `Int -> Num`), typeS(t, ip, `Str -> Num`), ip.Global)
	if !equalS(u, typeS(t, ip, `Any`)) {
		t.Fatalf("unifying functions with unrelated params should yield Any, got %#v", u)
	}
}

func Test_Types_Unify_Functions_EnumParam_Meet(t *testing.T) {
	ip := newIP()
	u := ip.unifyTypes(typeS(t, ip, `Str -> Str`), typeS(t, ip, `Enum["a","b"] -> Str`), ip.Global)
	want := typeS(t, ip, `Enum["a","b"] -> Str`)
	if !equalS(u, want) {
		t.Fatalf("function unify (enum param) mismatch: got %#v, want %#v", u, want)
	}
	// Supertype property
	if !ip.isSubtype(typeS(t, ip, `Str -> Str`), u, ip.Global) ||
		!ip.isSubtype(typeS(t, ip, `Enum["a","b"] -> Str`), u, ip.Global) {
		t.Fatalf("unify result must be a supertype of both function types")
	}
}

func Test_Types_Unify_Functions_NestedNullableReturns(t *testing.T) {
	ip := newIP()
	u := ip.unifyTypes(typeS(t, ip, `Int -> (Int -> Int?)`), typeS(t, ip, `Num -> (Num -> Null)`), ip.Global)
	// Param GLB: Int; inner param GLB: Int; inner return LUB: Int?
	want := typeS(t, ip, `Int -> Int -> Int?`)
	if !equalS(u, want) {
		t.Fatalf("nested function unify mismatch: got %#v, want %#v", u, want)
	}
}

func Test_Types_ArrayShape_Robustness(t *testing.T) {
	ip := newIP()
	v := evalWithIP(t, ip, `[1,2,3]`)
	// Simulate malformed type node: ("array", Int, Num)
	mt := S{"array", S{"id", "Int"}, S{"id", "Num"}}
	if !ip.isType(v, mt, ip.Global) { // current resolveType keeps children; isType should default elem to Any or handle 1st
		t.Fatalf("array shape robustness")
	}
}

func Test_Types_Runtime_Param_AliasMismatch(t *testing.T) {
	ip := newIP()
	evalWithIP(t, ip, `let Age = type Int`)
	evalExpectError(t, ip, `(fun(x: Age) -> Int do x end)("nope")`, "type mismatch")
}

// --- Enums -------------------------------------------------------------------

func Test_Types_Enum_IsType_StringLiterals(t *testing.T) {
	ip := newIP()

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
	ip := newIP()

	v1 := evalWithIP(t, ip, `42`)
	v2 := evalWithIP(t, ip, `7`)

	et := typeS(t, ip, `Enum[42, 7]`)

	if !ip.isType(v1, et, ip.Global) || !ip.isType(v2, et, ip.Global) {
		t.Fatalf("expected 42 and 7 to be in Enum[42,7]")
	}
}

func Test_Types_Enum_Subtype_SetInclusion(t *testing.T) {
	ip := newIP()

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
	ip := newIP()

	a := typeS(t, ip, `Enum["red","green"]`)
	b := typeS(t, ip, `Enum["green","blue"]`)
	u := ip.unifyTypes(a, b, ip.Global)

	want := typeS(t, ip, `Enum["red","green","blue"]`)
	if !ip.isSubtype(u, want, ip.Global) || !ip.isSubtype(want, u, ip.Global) {
		t.Fatalf("enum union mismatch: got %#v, want %#v", u, want)
	}
}

func Test_Types_Enum_Unify_WithStr_WidensToStr(t *testing.T) {
	ip := newIP()

	e := typeS(t, ip, `Enum["x","y"]`)
	s := typeS(t, ip, `Str`)
	u := ip.unifyTypes(e, s, ip.Global)

	if !ip.isSubtype(u, s, ip.Global) || !ip.isSubtype(s, u, ip.Global) {
		t.Fatalf("Enum ⊔ Str should unify to Str, got %#v", u)
	}
}

func Test_Types_Enum_Nullable(t *testing.T) {
	ip := newIP()

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
	ip := newIP()

	ok := evalWithIP(t, ip, `(fun(x: Enum["small","large"]) -> Str do "ok" end)("small")`)
	if ok.Tag != VTStr || ok.Data.(string) != "ok" {
		t.Fatalf("enum param should accept matching literal, got %#v", ok)
	}

	evalExpectError(t, ip, `(fun(x: Enum["small","large"]) -> Str do "ok" end)("medium")`, "type mismatch")
}

// --- Aliases to enums --------------------------------------------------------

func Test_Types_Alias_To_Enum(t *testing.T) {
	ip := newIP()

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
	ip := newIP()

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
	ip := newIP()

	v := evalWithIP(t, ip, `[[1, null], null]`)
	tok := typeS(t, ip, `[ [Int?]? ]`) // outer array of (nullable array of Int?)
	if !ip.isType(v, tok, ip.Global) {
		t.Fatalf("nested nullable arrays should match")
	}
}

// --- Maps with enums and required/optional ----------------------------------

func Test_Types_Map_Field_Enum_Required_Optional(t *testing.T) {
	ip := newIP()

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

func Test_Types_Unify_Maps_Required_OR_Rule(t *testing.T) {
	ip := newIP()

	// Required flag should OR on unify
	m1 := typeS(t, ip, `{a!: Int}`) // a required
	m2 := typeS(t, ip, `{a:  Num}`) // a optional
	u := ip.unifyTypes(m1, m2, ip.Global)

	// expect {a!: Num}  (type unified to Num, required ORed)
	want := typeS(t, ip, `{a!: Num}`)
	if !ip.isSubtype(u, want, ip.Global) || !ip.isSubtype(want, u, ip.Global) {
		t.Fatalf("map unify required OR rule failed: got %#v, want %#v", u, want)
	}
}

// --- Functions + enums, chained arrows --------------------------------------

func Test_Types_Function_Param_Enum_Contravariance(t *testing.T) {
	ip := newIP()

	// Param contravariance with enums:
	// Str -> Str  <:  Enum["a"] -> Str  (since Enum["a"] <: Str)
	sub := typeS(t, ip, `Str -> Str`)
	super := typeS(t, ip, `Enum["a"] -> Str`)
	if !ip.isSubtype(sub, super, ip.Global) {
		t.Fatalf("Str->Str <: Enum[\"a\"]->Str expected (param contravariance)")
	}
}

func Test_Types_FunctionValue_Against_EnumParamType(t *testing.T) {
	ip := newIP()

	// Provide a function with a broader param (Str),
	// check it against a type expecting a narrower Enum[...] param.
	f := evalWithIP(t, ip, `fun(x: Str) -> Str do "ok" end`)
	ty := typeS(t, ip, `Enum["dev","prod"] -> Str`)
	if !ip.isType(f, ty, ip.Global) { // Str->Str <: Enum[...] -> Str
		t.Fatalf("function value should satisfy Enum[...] -> Str via param contravariance")
	}
}

func Test_Types_Alias_State_With_Enum(t *testing.T) {
	ip := newIP()

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
	ip := newIP()

	// Build a module with export: foo = 1
	mo := &MapObject{
		Entries: map[string]Value{"foo": Int(1)},
		KeyAnn:  map[string]string{},
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
	ip := newIP()

	// Build a module with exports: x:"hi", n:2
	mo := &MapObject{
		Entries: map[string]Value{"x": Str("hi"), "n": Int(2)},
		KeyAnn:  map[string]string{},
		Keys:    []string{"x", "n"},
	}
	mod := &Module{Map: mo, Env: NewEnv(ip.Global)}
	v := Value{Tag: VTModule, Data: mod}

	// ValueToType should infer an open-world map schema with optional fields
	typ := ip.ValueToType(v, ip.Global)
	if len(typ) == 0 || typ[0].(string) != "map" {
		t.Fatalf("expected ValueToType(VTModule) to yield a 'map' type, got: %#v", typ)
	}

	fields := mapTypeFields(typ)
	fx, ok := fields["x"]
	if !ok || !isId(fx.typ, "Str") || fx.required {
		t.Fatalf("expected field x: Str (optional), got: %#v", fx)
	}
	fn, ok := fields["n"]
	if !ok || !isId(fn.typ, "Int") || fn.required {
		t.Fatalf("expected field n: Int (optional), got: %#v", fn)
	}
}
func Test_Types_IsType_Map_FieldAnnotations_Single(t *testing.T) {
	ip := newIP()

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
	ip := newIP()

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
	ip := newIP()

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
	if !equalS(u, b) {
		t.Fatalf("expected unify(a,b) to equal unannotated type; got %v", u)
	}
}

func Test_Types_ValueToType_Array_SelfCycle(t *testing.T) {
	ip := newIP()

	// Build a self-referential array:
	//   let a = [null]; a[0] = a; a
	v := evalWithIP(t, ip, "let a = [null]\na[0] = a\na")

	got := ip.valueToTypeS(v, ip.Global)
	// Expect conservative widening to [Any] rather than infinite recursion.
	want := typeS(t, ip, `[Any]`)
	if !equalS(got, want) {
		t.Fatalf("valueToTypeS on cyclic array: got %#v, want %#v", got, want)
	}
}

func Test_Types_ValueToType_Map_SelfCycle(t *testing.T) {
	ip := newIP()

	// Self-referential map with an extra normal field:
	//   let m = {}; m.self = m; m.x = 1; m
	v := evalWithIP(t, ip, "let m = {}\nm.self = m\nm.x = 1\nm")

	got := ip.valueToTypeS(v, ip.Global)
	if len(got) == 0 || got[0].(string) != "map" {
		t.Fatalf("expected map type, got %#v", got)
	}

	fs := mapTypeFields(got)
	// self: Any (widened due to cycle), x: Int (optional/open-world)
	if f, ok := fs["self"]; !ok || !isId(f.typ, "Any") {
		t.Fatalf("expected field self: Any (from cycle), got %#v", f)
	}
	if f, ok := fs["x"]; !ok || !isId(f.typ, "Int") {
		t.Fatalf("expected field x: Int, got %#v", f)
	}
}

func Test_Types_IsType_Array_SelfCycle_NoHang(t *testing.T) {
	ip := newIP()

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
	ip := newIP()

	// Self-referential map with a valid required field
	v := evalWithIP(t, ip, "let m = {}\nm.self = m\nm.x = 1\nm")

	// Type only requires x!: Int; presence of self (cyclic) must not hang.
	tReq := typeS(t, ip, `{x!: Int}`)
	if !ip.isType(v, tReq, ip.Global) {
		t.Fatalf("self-referential map should satisfy {x!: Int}")
	}
}

func Test_Types_ResolveType_FunctionalCycle_NoHang(t *testing.T) {
	ip := newIP()

	// Recursive alias: F = Int -> F
	if _, err := ip.EvalPersistentSource("let F = type Int -> F"); err != nil {
		t.Fatalf("setup error: %v", err)
	}

	// Resolving "F" should terminate and yield a function node whose return
	// still contains the unresolved id "F" (cycle guarded).
	res := ip.ResolveType(typeS(t, ip, `F`), ip.Global)

	if len(res) < 4 || res[0].(string) != "binop" || res[1].(string) != "->" {
		t.Fatalf("expected resolved F to be a function type, got %#v", res)
	}
	// Check param == Int
	param := res[2].(S)
	if !isId(param, "Int") {
		t.Fatalf("expected param Int, got %#v", param)
	}
	// Return should still reference id "F" (not infinitely expanded).
	ret := res[3].(S)
	if !(len(ret) >= 2 && ret[0].(string) == "id" && ret[1].(string) == "F") {
		t.Fatalf("expected return to be id F (cycle-guarded), got %#v", ret)
	}
}

// --- recursion / no-hang guards ---------------------------------------------

func Test_Types_Runtime_Return_Check_Recursive_NoHang(t *testing.T) {
	ip := newIP()

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
	ip := newIP()

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
