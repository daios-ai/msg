package mindscript

import (
	"fmt"
	"strings"
	"testing"
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

// assert helpers for annotated-null expectations
func wantAnnotatedContains(t *testing.T, v Value, substr string) {
	t.Helper()
	if v.Tag != VTNull {
		t.Fatalf("want annotated null, got %#v", v)
	}
	if v.Annot == "" || !strings.Contains(strings.ToLower(v.Annot), strings.ToLower(substr)) {
		t.Fatalf("want annotated null containing %q, got %#v", substr, v)
	}
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

func Test_Types_Runtime_Param_Mismatch_AnnotatedNull(t *testing.T) {
	ip := newIP()
	// (fun(x:Int)->Int do x end)("str")
	src := `(fun(x: Int) -> Int do x end)("str")`
	v := evalWithIP(t, ip, src)
	wantAnnotatedContains(t, v, "type mismatch")
}

func Test_Types_Runtime_Return_Mismatch_AnnotatedNull(t *testing.T) {
	ip := newIP()
	// fun()->Int returns "nope" (Str) → annotated null
	src := `(fun() -> Int do "nope" end)()`
	v := evalWithIP(t, ip, src)
	wantAnnotatedContains(t, v, "return type mismatch")
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
	u := ip.unifyTypes(typeS(t, ip, `Int -> Int -> Int`), typeS(t, ip, `Num -> Num -> Num`), ip.Global)
	if !ip.isSubtype(u, typeS(t, ip, `Num -> Num -> Num`), ip.Global) {
		t.Fatalf("function unify failed, got %#v", u)
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
	v := evalWithIP(t, ip, `(fun(x: Age) -> Int do x end)("nope")`)
	wantAnnotatedContains(t, v, "type mismatch")
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

	bad := evalWithIP(t, ip, `(fun(x: Enum["small","large"]) -> Str do "ok" end)("medium")`)
	wantAnnotatedContains(t, bad, "type mismatch")
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

	bad := evalWithIP(t, ip, `(fun(x: Color) -> Str do "ok" end)("green")`)
	wantAnnotatedContains(t, bad, "type mismatch")
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

	// 5) Using State in a function parameter (enum mismatch → annotated null)
	badCall := evalWithIP(t, ip, `(fun(s: State) -> Str do s.status end)({status: "paused"})`)
	wantAnnotatedContains(t, badCall, "type mismatch")
}
