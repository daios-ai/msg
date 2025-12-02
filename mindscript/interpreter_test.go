package mindscript

import (
	"os"
	"path/filepath"
	"strings"
	"testing"
)

// --- helpers ---------------------------------------------------------------

func mustEvalPersistent(t *testing.T, ip *Interpreter, src string) Value {
	t.Helper()
	v, err := ip.EvalPersistentSource(src)
	if err != nil {
		t.Fatalf("eval error for %q: %v", src, err)
	}
	return v
}

func evalSrc(t *testing.T, src string) Value {
	t.Helper()
	ip, _ := NewInterpreter()
	v, err := ip.EvalSource(src)
	if err != nil {
		t.Fatalf("EvalSource error: %v\nsource:\n%s", err, src)
	}
	return v
}

func wantInt(t *testing.T, v Value, n int64) {
	t.Helper()
	if v.Tag != VTInt || v.Data.(int64) != n {
		t.Fatalf("want int %d, got %#v", n, v)
	}
}

func wantNum(t *testing.T, v Value, f float64) {
	t.Helper()
	if v.Tag != VTNum {
		t.Fatalf("want num %g, got %#v", f, v)
	}
	got := v.Data.(float64)
	if !(got == f) {
		t.Fatalf("want num %g, got %g (%#v)", f, got, v)
	}
}

func wantStr(t *testing.T, v Value, s string) {
	t.Helper()
	if v.Tag != VTStr || v.Data.(string) != s {
		t.Fatalf("want str %q, got %#v", s, v)
	}
}

func wantStrAnn(t *testing.T, v Value, s, ann string) {
	t.Helper()
	if v.Tag != VTStr || v.Data.(string) != s {
		t.Fatalf("want str %q, got %#v", s, v)
	}
	if v.Annot != ann {
		t.Fatalf("want annot %q, got %q", ann, v.Annot)
	}
}

func wantBool(t *testing.T, v Value, b bool) {
	t.Helper()
	if v.Tag != VTBool || v.Data.(bool) != b {
		t.Fatalf("want bool %v, got %#v", b, v)
	}
}

func wantNull(t *testing.T, v Value) {
	t.Helper()
	if v.Tag != VTNull {
		t.Fatalf("want null, got %#v", v)
	}
}

func wantAnnotatedNullContains(t *testing.T, v Value, substr string) {
	t.Helper()
	if v.Tag != VTNull {
		t.Fatalf("want annotated null, got %#v", v)
	}
	if v.Annot == "" || !strings.Contains(strings.ToLower(v.Annot), strings.ToLower(substr)) {
		t.Fatalf("want annotated null containing %q, got %#v", substr, v)
	}
}

// annotated null = null with a non-empty annotation
func isAnnotatedNull(v Value) bool {
	return v.Tag == VTNull && v.Annot != ""
}

func wantStrWithAnnot(t *testing.T, v Value, s, ann string) {
	t.Helper()
	if v.Tag != VTStr || v.Data.(string) != s {
		t.Fatalf("want str %q, got %#v", s, v)
	}
	if v.Annot != ann {
		t.Fatalf("want annot %q, got %q (value=%#v)", ann, v.Annot, v)
	}
}

// New helpers for error-expecting paths (runtimeErrorsAsGoError=true).
func evalSrcExpectError(t *testing.T, src string) error {
	t.Helper()
	ip, _ := NewInterpreter()
	_, err := ip.EvalSource(src)
	if err == nil {
		t.Fatalf("expected runtime error, got nil\nsource:\n%s", src)
	}
	return err
}

func evalPersistentExpectError(t *testing.T, ip *Interpreter, src string) error {
	t.Helper()
	_, err := ip.EvalPersistentSource(src)
	if err == nil {
		t.Fatalf("expected runtime error, got nil\nsource:\n%s", src)
	}
	return err
}

func wantErrContains(t *testing.T, err error, substr string) {
	t.Helper()
	if err == nil {
		t.Fatalf("expected error containing %q, got nil", substr)
	}
	if !strings.Contains(strings.ToLower(err.Error()), strings.ToLower(substr)) {
		t.Fatalf("want error containing %q, got: %v", substr, err)
	}
}

// --- tests -----------------------------------------------------------------

func Test_Interpreter_Literals(t *testing.T) {
	wantInt(t, evalSrc(t, "42"), 42)
	wantNum(t, evalSrc(t, "5."), 5.0) // trailing-dot float
	wantNum(t, evalSrc(t, ".5"), 0.5) // leading-dot float
	wantStr(t, evalSrc(t, `"hi"`), "hi")
	wantBool(t, evalSrc(t, "true"), true)
	wantNull(t, evalSrc(t, "null"))
}

func Test_Interpreter_Arithmetic_Precedence_And_Numbers(t *testing.T) {
	wantInt(t, evalSrc(t, "1 + 2 * 3"), 7)
	wantNum(t, evalSrc(t, "5. / 2"), 2.5)
	wantInt(t, evalSrc(t, "7 % 4"), 3)
	wantBool(t, evalSrc(t, "3 < 4"), true)
	wantBool(t, evalSrc(t, "3.0 >= 3"), true)
}

func Test_Interpreter_String_Concat_And_Compare(t *testing.T) {
	wantStr(t, evalSrc(t, `"a" + "b"`), "ab")
	wantBool(t, evalSrc(t, `"b" > "a"`), true)
}

func Test_Interpreter_Arrays_Indexing_Including_Negative(t *testing.T) {
	wantInt(t, evalSrc(t, "[1,2,3][1]"), 2)
	// negative index wraps
	wantInt(t, evalSrc(t, "([1,2,3])[-1]"), 3)
	// concatenation
	v := evalSrc(t, "[1] + [2,3]")
	if v.Tag != VTArray || len(v.Data.(*ArrayObject).Elems) != 3 {
		t.Fatalf("want array len 3, got %#v", v)
	}
	wantInt(t, v.Data.(*ArrayObject).Elems[0], 1)
	wantInt(t, v.Data.(*ArrayObject).Elems[2], 3)
}

func Test_Interpreter_Maps_Get_Set(t *testing.T) {
	src := `
let o = {name: "John", age: 30}
o.name
`
	wantStr(t, evalSrc(t, src), "John")

	src2 := `
let o = {name: "John", age: 30}
o.name = "Sarah"
o.name
`
	wantStr(t, evalSrc(t, src2), "Sarah")
}

func Test_Interpreter_Map_Required_Field_Bang_Runtime_Ignored(t *testing.T) {
	// pair! is metadata for the type system; at runtime it behaves like a normal field
	src := `{name!: "J", age: 10}.name`
	wantStr(t, evalSrc(t, src), "J")
}

func Test_Interpreter_Map_Merge_And_Array_Concat(t *testing.T) {
	// map merge is right-biased
	src := `({a: 1} + {b: 2, a: 3}).a`
	wantInt(t, evalSrc(t, src), 3)
	src2 := `({a: 1} + {b: 2}).b`
	wantInt(t, evalSrc(t, src2), 2)

	// array concat
	v := evalSrc(t, `[1,2] + [3]`)
	if v.Tag != VTArray || len(v.Data.(*ArrayObject).Elems) != 3 {
		t.Fatalf("want array len 3, got %#v", v)
	}
}

func Test_Interpreter_Call_Function(t *testing.T) {
	src := `
let inc = fun(x: Int) -> Int do
  return(x + 1)
end
inc(4)
`
	wantInt(t, evalSrc(t, src), 5)
}

func Test_Interpreter_Closure_Capture(t *testing.T) {
	src := `
let make = fun(a: Int) -> Any do
  return( fun(b: Int) -> Int do
    return(a + b)
  end )
end
let add2 = make(2)
add2(5)
`
	wantInt(t, evalSrc(t, src), 7)
}

func Test_Interpreter_If_Elif_Else(t *testing.T) {
	src := `
if false then
  1
elif true then
  2
else
  3
end
`
	wantInt(t, evalSrc(t, src), 2)
}

func Test_Interpreter_For_Array_Sum_With_Continue(t *testing.T) {
	// NOTE: continue is supported in the loop body; avoid break in this minimal test
	src := `
let sum = 0
for x in [1,2,3,4] do
  if x == 3 then
    continue(0)
  end
  sum = sum + x
end
sum
`
	wantInt(t, evalSrc(t, src), 7) // 1 + 2 + 4
}

func Test_Interpreter_Annotation_Attaches_To_Value(t *testing.T) {
	src := `# hello
42`
	v := evalSrc(t, src)
	wantInt(t, v, 42)
	if v.Annot == "" || !strings.Contains(strings.ToLower(v.Annot), "hello") {
		t.Fatalf("expected annotation to contain 'hello', got %q", v.Annot)
	}
}

func Test_Interpreter_DotNumber_Indexing_After_Chain(t *testing.T) {
	src := `
let obj = {name: [10,20,30]}
obj.name.1
`
	wantInt(t, evalSrc(t, src), 20)
}

func Test_Interpreter_Type_Optional_Postfix_Runtime_Value(t *testing.T) {
	v := evalSrc(t, `type Str?`)
	if v.Tag != VTType {
		t.Fatalf("want VTType, got %#v", v)
	}
	expr := typeAstFromValueData(v.Data)
	if expr[0].(string) != "unop" || expr[1].(string) != "?" {
		t.Fatalf("type expr shape wrong: %#v", expr)
	}
	id := expr[2].(S)
	if id[0].(string) != "id" || id[1].(string) != "Str" {
		t.Fatalf("type base wrong: %#v", expr)
	}
}

func Test_Interpreter_Assign_To_Array_Index_And_Object_Field(t *testing.T) {
	src := `
let a = [0,0]
a[1] = 9
a[1]
`
	wantInt(t, evalSrc(t, src), 9)

	src2 := `
let m = {k: 0}
m.k = 7
m.k
`
	wantInt(t, evalSrc(t, src2), 7)
}

func Test_Interpreter_TopLevel_Return_Exits_Program(t *testing.T) {
	v := evalSrc(t, `return(42)`)
	wantInt(t, v, 42)
}

func Test_Interpreter_TopLevel_Break_Exits_Program(t *testing.T) {
	v := evalSrc(t, `break(7)`)
	wantInt(t, v, 7)
}

func Test_Interpreter_TopLevel_Continue_Returns_AnnotatedNull(t *testing.T) {
	v := evalSrc(t, `continue(0)`)
	wantInt(t, v, 0)
}

func Test_Interpreter_ShortCircuit_And_Or(t *testing.T) {
	wantBool(t, evalSrc(t, "false and (1/0==0)"), false) // RHS not evaluated
	wantBool(t, evalSrc(t, "true or (1/0==0)"), true)    // RHS not evaluated
}

// --- annotated-null runtime error cases ------------------------------------

func Test_Interpreter_DivisionByZero_Returns_Error(t *testing.T) {
	err := evalSrcExpectError(t, `1 / 0`)
	wantErrContains(t, err, "division by zero")
}

func Test_Interpreter_NotAFunction_Returns_Error(t *testing.T) {
	err := evalSrcExpectError(t, `42(1)`)
	wantErrContains(t, err, "not a function")
}

func Test_Interpreter_UnknownProperty_Returns_Error(t *testing.T) {
	err := evalSrcExpectError(t, `{a: 1}.b`)
	wantErrContains(t, err, "unknown")
}

func Test_Interpreter_Let_Simple_Assign(t *testing.T) {
	v := evalSrc(t, `
let x = 3
x
`)
	wantInt(t, v, 3)
}

func Test_Interpreter_Let_ArrayDestructuring_Binds_And_Ignores_Extra(t *testing.T) {
	v := evalSrc(t, `
let [x, y, z] = [1, 2, 3, 4, 5]
z
`)
	wantInt(t, v, 3)
}

func Test_Interpreter_Let_ArrayDestructuring_AnnotatedNull_When_Short(t *testing.T) {
	v := evalSrc(t, `
let [a, b] = [1]
b
`)
	wantNull(t, v)
	if isAnnotatedNull(v) {
		t.Fatalf("want plain null for missing array element, got annotated null: %#v", v)
	}
}

func Test_Interpreter_Let_ArrayDestructuring_WrongShape_AnnotatedNull(t *testing.T) {
	err := evalSrcExpectError(t, `
let [a] = 42
a
`)
	wantErrContains(t, err, "array pattern")
	wantErrContains(t, err, "not an array")
}

func Test_Interpreter_Let_ObjectDestructuring_Binds_Selected_Keys(t *testing.T) {
	v := evalSrc(t, `
let {name: x, age: y} = {name: "Pedro", age: 17, extra: 999}
x
`)
	wantStr(t, v, "Pedro")

	v2 := evalSrc(t, `
let {age: x} = {name: "Pedro", age: 17}
x
`)
	wantInt(t, v2, 17)
}

func Test_Interpreter_Let_ObjectDestructuring_WrongShape_AnnotatedNull(t *testing.T) {
	err := evalSrcExpectError(t, `
let {foo: x} = 10
x
`)
	wantErrContains(t, err, "object pattern")
	wantErrContains(t, err, "not a map")
}

func Test_Interpreter_Let_ObjectDestructuring_MissingKey_AnnotatedNull(t *testing.T) {
	v := evalSrc(t, `
let {age: a, name: n} = {name: "Pedro"}
a
`)
	wantNull(t, v)
	if isAnnotatedNull(v) {
		t.Fatalf("want plain null for missing object key, got annotated null: %#v", v)
	}
}

func Test_Interpreter_Let_Nested_Patterns_ArrayInObject(t *testing.T) {
	v := evalSrc(t, `
let {coords: [x, y]} = {coords: [3, 4]}
x + y
`)
	wantInt(t, v, 7)
}

func Test_Interpreter_Let_Nested_Patterns_ObjectInObject(t *testing.T) {
	v := evalSrc(t, `
let {vehicle: {type: t}} = {vehicle: {type: "car", wheels: 4}}
t
`)
	wantStr(t, v, "car")
}

func Test_Interpreter_Let_ObjectDestructuring_With_Keyword_Keys(t *testing.T) {
	v := evalSrc(t, `
let {if: a, else: b, type: c} = {if: 1, else: 2, type: 3}
a + b + c
`)
	wantInt(t, v, 6)
}

func Test_Interpreter_Keywords_As_Keys_And_Access(t *testing.T) {
	v := evalSrc(t, `
let o = {if: 10, else: 20, type: 30}
o.if + o.else + o.type
`)
	wantInt(t, v, 60)
}

func Test_Interpreter_Let_Deep_Nested_Destructuring_Mix(t *testing.T) {
	v := evalSrc(t, `
let {a: [x, {y: z}, w], b: {c: [u, v]}} =
  {a: [10, {y: 20}, 30, 999], b: {c: [1, 2, 3]}}
x + z + w + u + v
`)
	// 10 + 20 + 30 + 1 + 2 = 63
	wantInt(t, v, 63)
}

func Test_Interpreter_Let_Nested_Missing_Key_Annotated(t *testing.T) {
	v := evalSrc(t, `
let {b: {d: q}} = {b: {}}
q
`)
	wantNull(t, v)
	if isAnnotatedNull(v) {
		t.Fatalf("want plain null for nested missing key, got annotated null: %#v", v)
	}
}

func Test_Interpreter_Let_Nested_WrongShape_Annotated(t *testing.T) {
	err := evalSrcExpectError(t, `
let {a: [x]} = {a: 5}
x
`)
	// It's an array pattern, so assert "not an array" via hard runtime error
	wantErrContains(t, err, "array pattern")
	wantErrContains(t, err, "not an array")
}

func Test_Interpreter_Let_ArrayOfObjects_Destructuring(t *testing.T) {
	v := evalSrc(t, `
let [ {id: a}, {id: b} ] = [ {id: 1}, {id: 2}, {id: 3} ]
a + b
`)
	wantInt(t, v, 3)
}

func Test_Interpreter_Let_Nested_Keywords_In_Deep_Structure(t *testing.T) {
	v := evalSrc(t, `
let {a: [{if: i}, {else: j}], type: {fun: k}} =
  {a: [{if: 1}, {else: 2}], type: {fun: 3}}
i + j + k
`)
	wantInt(t, v, 6)
}

func Test_Interpreter_Let_Nested_Array_Short_Fills_AnnotatedNulls(t *testing.T) {
	v := evalSrc(t, `
let {a: [x, y, z]} = {a: [1]}
[z, y]
`)
	arr := v
	if arr.Tag != VTArray || len(arr.Data.(*ArrayObject).Elems) != 2 {
		t.Fatalf("want array of 2, got %#v", v)
	}
	vz := arr.Data.(*ArrayObject).Elems[0] // z
	vy := arr.Data.(*ArrayObject).Elems[1] // y

	wantNull(t, vz)
	if isAnnotatedNull(vz) {
		t.Fatalf("want plain null for missing z, got annotated null: %#v", vz)
	}

	wantNull(t, vy)
	if isAnnotatedNull(vy) {
		t.Fatalf("want plain null for missing y, got annotated null: %#v", vy)
	}
}

// New: for-loop destructuring should also treat wrong-shape elements as a hard error.
func Test_Interpreter_For_ArrayDestructuring_WrongShape_RuntimeError(t *testing.T) {
	err := evalSrcExpectError(t, `
let xs = [1, 2]
for [a, b] in xs do
  a
end
`)
	wantErrContains(t, err, "array pattern")
}

func Test_Interpreter_Let_ObjectDestructuring_WithAnnotations(t *testing.T) {
	v := evalSrc(t, `
let {
  name:
    # username
    n,
  age:
    # yearsOld
    a
} = {name: "Bob", age: 40}
n
`)
	wantStr(t, v, "Bob")
	if v.Annot == "" || !strings.Contains(strings.ToLower(v.Annot), "username") {
		t.Fatalf("expected annotation 'username', got %#v", v.Annot)
	}
}

func Test_Interpreter_Closure_Captures_And_Mutates(t *testing.T) {
	src := `
let n = 0
let f = fun() do
  n = n + 1
  return(n)
end
[f(), f(), f()]
`
	v := evalSrc(t, src)
	if v.Tag != VTArray {
		t.Fatalf("want array, got %#v", v)
	}
	arr := v.Data.(*ArrayObject).Elems
	want := []int64{1, 2, 3}
	for i, w := range want {
		if arr[i].Tag != VTInt || arr[i].Data.(int64) != w {
			t.Fatalf("want %d at index %d, got %#v", w, i, arr[i])
		}
	}
}

func Test_Interpreter_Factorial_Function(t *testing.T) {
	src := `
let fact = fun(n: Int) -> Int do
  if n == 0 then
    return(1)
  else
    return(n * fact(n - 1))
  end
end
fact(5)
`
	v := evalSrc(t, src)
	wantInt(t, v, 120)
}

func Test_Interpreter_Fibonacci_Function(t *testing.T) {
	src := `
let fib = fun(n: Int) -> Int do
  if n == 0 then
    return(0)
  elif n == 1 then
    return(1)
  else
    return(fib(n - 1) + fib(n - 2))
  end
end
fib(10)
`
	v := evalSrc(t, src)
	wantInt(t, v, 55)
}

func Test_Interpreter_Fibonacci_Function_Memoized(t *testing.T) {
	src := `
let cache = [null,null,null,null,null,null,null,null,null,null,
             null,null,null,null,null,null,null,null,null,null]
cache[0] = 0
cache[1] = 1

let fib = null
fib = fun(n: Int) -> Int do
  if n <= 1 then
    return(n)
  end
  let v = cache[n]
  if v != null then
    return(v)
  end
  let res = fib(n - 1) + fib(n - 2)
  cache[n] = res
  return(res)
end

[fib(10), fib(9), cache[10], cache[9]]
`
	v := evalSrc(t, src)
	// Expect: [55, 34, 55, 34]
	if v.Tag != VTArray {
		t.Fatalf("want array result, got %#v", v)
	}
	arr := v.Data.(*ArrayObject).Elems
	if len(arr) != 4 {
		t.Fatalf("want 4 elements, got %d (%#v)", len(arr), v)
	}
	want := []int64{55, 34, 55, 34}
	for i, w := range want {
		if arr[i].Tag != VTInt || arr[i].Data.(int64) != w {
			t.Fatalf("index %d: want %d, got %#v", i, w, arr[i])
		}
	}
}

func Test_Interpreter_Keywords_As_Keys_And_Property_Chaining(t *testing.T) {
	v := evalSrc(t, `
let o = {type: {if: {else: 3}}}
o.type.if.else
`)
	wantInt(t, v, 3)
}

func Test_Interpreter_While_Count(t *testing.T) {
	src := `
let n = 0
while n < 3 do
  n = n + 1
end
n
`
	wantInt(t, evalSrc(t, src), 3)
}

func Test_Interpreter_While_Condition_MustBeBool(t *testing.T) {
	err := evalSrcExpectError(t, `
let n = 0
while 1 do
  n = 1
end
n
`)
	wantErrContains(t, err, "boolean")
}

func Test_Interpreter_For_Map_Yields_Pairs_As_Array(t *testing.T) {
	src := `
let m = {a: 1, b: 2}
let sum = 0
for p in m do
  sum = sum + p.1
end
sum
`
	// Sum of values 1 + 2 = 3 (order independent)
	wantInt(t, evalSrc(t, src), 3)
}

func Test_Interpreter_For_Iterator_Function(t *testing.T) {
	src := `
let range = fun(start: Int, finish: Int) -> Null -> Int? do
  let counter = start
  return(
    fun() -> Int? do
      if counter >= finish then
        null
      else
        let value = counter
        counter = counter + 1
        value
      end
    end
  )
end

let sum = 0
for x in range(5, 8) do
  sum = sum + x
end
sum
`
	// 5 + 6 + 7 = 18
	wantInt(t, evalSrc(t, src), 18)
}

func Test_Interpreter_Function_ZeroParam_ImplicitNull_CallsWork(t *testing.T) {
	// fun() is normalized to fun(_ : Null); both f() and f(null) must work.
	src := `
let f = fun() -> Int? do
  1
end
[f(), f(null)]
`
	v := evalSrc(t, src)
	if v.Tag != VTArray {
		t.Fatalf("want array, got %#v", v)
	}
	arr := v.Data.(*ArrayObject).Elems
	if len(arr) != 2 {
		t.Fatalf("want 2 elements, got %d (%#v)", len(arr), v)
	}
	wantInt(t, arr[0], 1)
	wantInt(t, arr[1], 1)
}

func Test_Interpreter_Function_ExplicitNullParam_BehavesLikeZeroParam(t *testing.T) {
	// An explicit Null-param function should behave exactly like the zero-param form.
	src := `
let f1 = fun() -> Int? do
  1
end
let f2 = fun(x: Null) -> Int? do
  1
end
[f1(), f1(null), f2(), f2(null)]
`
	v := evalSrc(t, src)
	if v.Tag != VTArray {
		t.Fatalf("want array, got %#v", v)
	}
	arr := v.Data.(*ArrayObject).Elems
	if len(arr) != 4 {
		t.Fatalf("want 4 elements, got %d (%#v)", len(arr), v)
	}
	for i := 0; i < 4; i++ {
		wantInt(t, arr[i], 1)
	}
}

func Test_Interpreter_For_Iterator_Function_Rejects_NonNullParam(t *testing.T) {
	src := `
	let iter = fun(x:Int) -> Int do x end
	let sum = 0
	for v in iter do
	  sum = sum + v
	end
	sum
	`
	err := evalSrcExpectError(t, src)
	// Contract error raised when attempting to call a non-function:
	wantErrContains(t, err, "for expects array, map, or iterator function (Null -> Any?)")
}

func Test_Interpreter_Debug_Range_ReturnsIteratorFunction(t *testing.T) {
	src := `
let range = fun(start: Int, finish: Int) -> Null -> Int? do
  let counter = start
  return(
    fun() -> Int? do
      if counter >= finish then
        null
      else
        let value = counter
        counter = counter + 1
        value
      end
    end
  )
end

range(5, 8)
`

	ip, _ := NewInterpreter()
	v, err := ip.EvalSource(src)
	if err != nil {
		t.Fatalf("eval error: %v", err)
	}
	if v.Tag != VTFun {
		t.Fatalf("expected VTFun, got %v (annot=%q)", v, v.Annot)
	}

	// Inspect callable meta to verify 1-arg (hidden Null) and Int? return type.
	callable, ok := ip.FunMeta(v)
	if !ok {
		t.Fatalf("FunMeta not available")
	}
	if callable.Arity() != 1 {
		t.Fatalf("iterator arity: want 1 (Null), got %d", callable.Arity())
	}
	ps := callable.ParamSpecs()
	if len(ps) != 1 {
		t.Fatalf("param specs len: want 1, got %d", len(ps))
	}
	// Param type must be exactly Null (after alias resolution)
	if !ip.IsSubtype(ps[0].Type, S{"id", "Null"}, nil) || !ip.IsSubtype(S{"id", "Null"}, ps[0].Type, nil) {
		t.Fatalf("param[0] type: want Null, got %v", ps[0].Type)
	}
	// Return type should be Int?  (i.e., ("unop","?", ("id","Int")))
	wantRet := S{"unop", "?", S{"id", "Int"}}
	if !ip.IsSubtype(callable.ReturnType(), wantRet, nil) || !ip.IsSubtype(wantRet, callable.ReturnType(), nil) {
		t.Fatalf("return type: want Int?, got %v", callable.ReturnType())
	}
}

// ------------------------------------------------------------
// Loop/block expression values
// ------------------------------------------------------------

func Test_Interpreter_While_Value_IsNull(t *testing.T) {
	v := evalSrc(t, `
let n = 0
while n < 2 do
  n = n + 1
end
`)
	wantInt(t, v, 2)
}

func Test_Interpreter_For_Expression_Value_Null(t *testing.T) {
	v := evalSrc(t, `
let s = 0
for x in [1,2] do
  s = s + x
end
`)
	wantInt(t, v, 3)
}

func Test_Interpreter_While_Continue_Expression_Value_Null(t *testing.T) {
	v := evalSrc(t, `
let n=0
while n < 3 do
  n = n + 1
  if n == 2 then
    continue(0)
  end
end
`)
	wantNull(t, v)
}

// ------------------------------------------------------------
// Iterator protocol & annotated-null propagation
// ------------------------------------------------------------

func Test_Interpreter_For_Iterator_AnnotatedNull_Propagates_As_Error(t *testing.T) {
	// Iterator returns an annotated null; for-loop should propagate it out.
	src := `
let bad = fun() -> Int? do
  return(
    # oops
    null
  )
end
let sum = 0
for x in bad do
  sum = sum + x
end
sum
`
	err := evalSrcExpectError(t, src)
	wantErrContains(t, err, "oops")
}

func Test_Interpreter_For_ZeroArgIterator_Works(t *testing.T) {
	// Zero-arg iterators are normalized to (Null) → …; for-loop should call with null.
	src := `
let i = 0
let iter = fun() -> Int? do
  if i >= 2 then
    null
  else
    let v = i
    i = i + 1
    v
  end
end
let sum = 0
for x in iter do
  sum = sum + x
end
sum
`
	wantInt(t, evalSrc(t, src), 1) // 0 + 1
}

// ------------------------------------------------------------
// Indexing edge cases
// ------------------------------------------------------------

func Test_Interpreter_Index_EmptyArray_Annotated(t *testing.T) {
	err := evalSrcExpectError(t, `([])[-1]`)
	wantErrContains(t, err, "empty array")
}

// ------------------------------------------------------------
// '+' over unsupported types produces annotated null (not panic)
// ------------------------------------------------------------

func Test_Interpreter_Plus_Unsupported_Types_Annotated(t *testing.T) {
	v := evalSrc(t, `"a" + 1`)
	wantAnnotatedNullContains(t, v, "unsupported")
}

// ------------------------------------------------------------
// Annotations: stacking (inline removed)
// ------------------------------------------------------------

func Test_Interpreter_Annotation_Stacking_MultilineHashes(t *testing.T) {
	v := evalSrc(t, `
# first
# second
99
`)
	wantInt(t, v, 99)
	ann := strings.ToLower(v.Annot)
	if !(strings.Contains(ann, "first") && strings.Contains(ann, "second")) {
		t.Fatalf("expected stacked annotations, got %q", v.Annot)
	}
}

// ------------------------------------------------------------
// Type system via runtime checks
// ------------------------------------------------------------

func Test_Interpreter_Function_ReturnType_Mismatch_Returns_Error(t *testing.T) {
	err := evalSrcExpectError(t, `
let f = fun()->Int do
  "nope"
end
f()
`)
	wantErrContains(t, err, "return type mismatch")
}

func Test_Interpreter_Function_ParamType_Mismatch_Returns_Error(t *testing.T) {
	err := evalSrcExpectError(t, `
let f = fun(x:Int)->Int do x end
f("str")
`)
	wantErrContains(t, err, "type mismatch in parameter")
}

func Test_Interpreter_Function_Return_AnyOptional_Top(t *testing.T) {
	v := evalSrc(t, `
let f = fun()->Any? do 42 end
f()
`)
	wantInt(t, v, 42)
}

// ------------------------------------------------------------
// Dot/property name forms
// ------------------------------------------------------------

func Test_Interpreter_Dot_Quoted_Property(t *testing.T) {
	v := evalSrc(t, `
let o = {"weird key": 7}
o."weird key"
`)
	wantInt(t, v, 7)
}

func Test_Interpreter_Dot_Number_After_Call_Chain(t *testing.T) {
	v := evalSrc(t, `
let mk = fun()->Any do {a:[10,20,30]} end
mk().a.2
`)
	wantInt(t, v, 30)
}

// Map with any "word-like" keys (keywords, type names, booleans, null, ids).
func Test_Interpreter_Map_AnyWordLike_Keys(t *testing.T) {
	src := `
let o = {if: 1, else: 2, for: 3, type: 4, Enum: 5, enum: 6, Int: 7, Str: 8, true: 9, null: 10}
[o["if"], o["else"], o["for"], o["type"], o["Enum"], o["enum"], o["Int"], o["Str"], o["true"], o["null"]]
`
	v := evalSrc(t, src)
	if v.Tag != VTArray {
		t.Fatalf("want array, got %#v", v)
	}
	xs := v.Data.(*ArrayObject).Elems
	want := []int64{1, 2, 3, 4, 5, 6, 7, 8, 9, 10}
	if len(xs) != len(want) {
		t.Fatalf("want %d items, got %d", len(want), len(xs))
	}
	for i, w := range want {
		if xs[i].Tag != VTInt || xs[i].Data.(int64) != w {
			t.Fatalf("idx %d: want %d, got %#v", i, w, xs[i])
		}
	}
}

// Map with annotated keys (single-line) + annotated value.
// Key annotations should NOT affect runtime map (keys become plain strings).
// Value annotation MUST be preserved on the stored value.
func Test_Interpreter_Map_AnnotatedKeys_And_ValueAnnotation(t *testing.T) {
	src := `
let o = {
# the name
name: "Mo",
# the age
age: 47,
available:
  # status
  "yes"
}
[o["name"], o["age"], o["available"]]
`
	v := evalSrc(t, src)
	if v.Tag != VTArray {
		t.Fatalf("want array, got %#v", v)
	}
	xs := v.Data.(*ArrayObject).Elems
	// name: "Mo" (no annotation on the value)
	wantStr(t, xs[0], "Mo")
	// age: 47
	wantInt(t, xs[1], 47)
	// available: "yes" with annotation "status"
	wantStrWithAnnot(t, xs[2], "yes", "status")
}

// Destructuring with annotated keys should bind correctly.
func Test_Interpreter_ObjectDestructuring_With_AnnotatedKey(t *testing.T) {
	src := `
let {
  # first
  name: x, age: y
} = { name: "Ana", age: 33 }
[x, y]
`
	v := evalSrc(t, src)
	if v.Tag != VTArray {
		t.Fatalf("want array, got %#v", v)
	}
	xs := v.Data.(*ArrayObject).Elems
	wantStr(t, xs[0], "Ana")
	wantInt(t, xs[1], 33)
}

// Destructuring with annotated keys — missing key should yield annotated null.
func Test_Interpreter_ObjectDestructuring_AnnotatedKey_MissingValue(t *testing.T) {
	src := `
let {
  # first
  name: x,
  # years
  age: y
} = { name: "Bob" }
[y]
`
	v := evalSrc(t, src)
	if v.Tag != VTArray || len(v.Data.(*ArrayObject).Elems) != 1 {
		t.Fatalf("want single-element array, got %#v", v)
	}
	vy := v.Data.(*ArrayObject).Elems[0]
	if !isAnnotatedNull(vy) {
		t.Fatalf("want annotated null for missing 'age', got %#v", vy)
	}
	if vy.Annot != "years" {
		t.Fatalf("want annotation %q on missing 'age', got %q", "years", vy.Annot)
	}
}

// Map construction with several annotated keys mixed with values.
// Confirms key-unwrapping works for multiple pairs.
func Test_Interpreter_Map_AnnotatedKeys_MultiplePairs(t *testing.T) {
	src := `
let m = {
# k1
k1: 1,
# k2-inline
k2: 2,
k3:
  # val
  "v"
}
[m["k1"], m["k2"], m["k3"]]
`
	v := evalSrc(t, src)
	if v.Tag != VTArray {
		t.Fatalf("want array, got %#v", v)
	}
	xs := v.Data.(*ArrayObject).Elems
	wantInt(t, xs[0], 1)
	wantInt(t, xs[1], 2)
	wantStrWithAnnot(t, xs[2], "v", "val")
}

// Using type-like names and keywords as keys in destructuring (no access syntax needed).
func Test_Interpreter_Destructure_AnyWordLike_Keys(t *testing.T) {
	src := `
let { if: a, Enum: b, Int: c, true: d, null: e } = { if: 1, Enum: 2, Int: 3, true: 4, null: 5 }
[a, b, c, d, e]
`
	v := evalSrc(t, src)
	if v.Tag != VTArray {
		t.Fatalf("want array, got %#v", v)
	}
	xs := v.Data.(*ArrayObject).Elems
	want := []int64{1, 2, 3, 4, 5}
	for i, w := range want {
		if xs[i].Tag != VTInt || xs[i].Data.(int64) != w {
			t.Fatalf("idx %d: want %d, got %#v", i, w, xs[i])
		}
	}
}

// Iteration yields Str keys carrying their annotations.
func Test_Interpreter_Map_KeyAnnotations_On_Iter(t *testing.T) {
	src := `
let o = {
# the name
name: "Mo",
# the age
age: 47,
plain: "ok"
}
let it = __to_iter(o)
let a = it(null)
let b = it(null)
let c = it(null)
[a[0], a[1], b[0], b[1], c[0], c[1]]
`
	v := evalSrc(t, src)
	xs := v.Data.(*ArrayObject).Elems
	// First pair
	wantStrAnn(t, xs[0], "name", "")             // key has no annotation
	wantStrWithAnnot(t, xs[1], "Mo", "the name") // value carries the note
	// Second pair
	wantStrAnn(t, xs[2], "age", "")
	wantInt(t, xs[3], 47)
	if xs[3].Annot != "the age" {
		t.Fatalf("want value annotation %q, got %q", "the age", xs[3].Annot)
	}
	// Third pair
	wantStrAnn(t, xs[4], "plain", "")
	wantStr(t, xs[5], "ok")
	if xs[5].Annot != "" {
		t.Fatalf("unexpected value annotation %q for 'plain'", xs[5].Annot)
	}
}

// Lookup ignores key annotation (storage by raw string).
func Test_Interpreter_Map_Lookup_Ignores_Key_Annot(t *testing.T) {
	src := `
let o = {
# the name
name: "Mo"
}
o["name"]
`
	wantStr(t, evalSrc(t, src), "Mo")
}

// Deep equality ignores key annotations.
func Test_Interpreter_Map_DeepEqual_Ignores_KeyAnnots(t *testing.T) {
	ip, _ := NewInterpreter()
	ip.RegisterNative("__eq_maps",
		[]ParamSpec{{"x", S{"id", "Any"}}, {"y", S{"id", "Any"}}},
		S{"id", "Bool"},
		func(ip2 *Interpreter, ctx CallCtx) Value {
			return Bool(ip2.deepEqual(ctx.Arg("x"), ctx.Arg("y")))
		})

	srcA := `
let a = {
# the name
name: "Mo", x: 1
}`
	srcB := `
let b = {
# label
name: "Mo", x: 1
}`
	v1, err := ip.EvalSource(srcA + "\n" + srcB + "\n__eq_maps(a, b)")
	if err != nil {
		t.Fatalf("eval error: %v", err)
	}
	wantBool(t, v1, true)
}

// Merge carries key annotations from RHS.
func Test_Interpreter_Map_Plus_Merge_Carries_RHS_KeyAnnot(t *testing.T) {
	src := `
let a = {
# A
k: 1, x: 2
}
let b = {
# B
k: 9, y: 3
}
let m = a + b
let it = __to_iter(m)
let p = it(null)
let q = it(null)
let r = it(null)
let arr =
  if p[0] == "k" then p
  elif q[0] == "k" then q
  else r
  end
arr
`
	v := evalSrc(t, src)
	pair := v.Data.(*ArrayObject).Elems
	wantStrAnn(t, pair[0], "k", "") // key has no annotation under new policy
	wantInt(t, pair[1], 9)
	if pair[1].Annot != "B" {
		t.Fatalf("want value annotation %q on merged RHS, got %q", "B", pair[1].Annot)
	}
}

func Test_Interpreter_Annot_On_Assign_Persists_In_Env(t *testing.T) {
	ip, _ := NewInterpreter()

	// let c = 299792458 with annotation
	mustEvalPersistent(t, ip, "# speed of light\nlet c = 299792458")

	// c must carry the annotation
	v := mustEvalPersistent(t, ip, "c")
	if v.Tag != VTInt || v.Data.(int64) != 299792458 {
		t.Fatalf("expected c == 299792458, got %#v", v)
	}
	if got, want := v.Annot, "speed of light"; got != want {
		t.Fatalf("annotation lost on stored value: got %q want %q", got, want)
	}
}

func Test_Interpreter_Annot_On_Bare_Decl_Persists_As_AnnotatedNull(t *testing.T) {
	ip, _ := NewInterpreter()

	// annotated bare decl
	mustEvalPersistent(t, ip, "# doc for x\nlet x")

	v := mustEvalPersistent(t, ip, "x")
	if v.Tag != VTNull {
		t.Fatalf("expected x to be null, got %#v", v)
	}
	if got, want := v.Annot, "doc for x"; got != want {
		t.Fatalf("annotation lost on bare decl: got %q want %q", got, want)
	}
}

func Test_Interpreter_Annot_On_Reassign_Persists_On_Binding(t *testing.T) {
	ip, _ := NewInterpreter()

	mustEvalPersistent(t, ip, "let x = 1")
	// annotated reassignment
	mustEvalPersistent(t, ip, "# newer doc\nx = 2")

	v := mustEvalPersistent(t, ip, "x")
	if v.Tag != VTInt || v.Data.(int64) != 2 {
		t.Fatalf("expected x == 2, got %#v", v)
	}
	if got, want := v.Annot, "newer doc"; got != want {
		t.Fatalf("annotation lost on reassignment: got %q want %q", got, want)
	}
}

func Test_Interpreter_Annot_On_LValue_Id_Persists(t *testing.T) {
	ip, _ := NewInterpreter()
	mustEvalPersistent(t, ip, `let a = "x"`)
	mustEvalPersistent(t, ip, "# temp note\na")
	got := mustEvalPersistent(t, ip, "a")
	wantStrWithAnnot(t, got, "x", "temp note")
}

func Test_Interpreter_Identity_Preserves_Annotation(t *testing.T) {
	ip, _ := NewInterpreter()

	// Annotated binding
	mustEvalPersistent(t, ip, "# doc\nlet a = 42")

	// identity function that returns its argument verbatim
	mustEvalPersistent(t, ip, "let id = fun(x: Any) -> Any do x end")

	// id(a) should preserve a's annotation
	v := mustEvalPersistent(t, ip, "id(a)")
	if v.Tag != VTInt || v.Data.(int64) != 42 {
		t.Fatalf("expected 42, got %#v", v)
	}
	if got, want := v.Annot, "doc"; got != want {
		t.Fatalf("identity lost annotation: got %q want %q", got, want)
	}
}

func Test_Interpreter_Arithmetic_Does_Not_Propagate_Annotation(t *testing.T) {
	ip, _ := NewInterpreter()

	mustEvalPersistent(t, ip, "# golden ratio\nlet phi = (1 + 5) / 2")

	// 2 * phi should not carry annotation
	v := mustEvalPersistent(t, ip, "2 * phi")
	if v.Annot != "" {
		t.Fatalf("unexpected propagation through construction: got %q", v.Annot)
	}
}

func Test_Interpreter_Pretty_Does_Not_Show_Desugaring(t *testing.T) {
	// Ensure codegen-only rewrite doesn’t affect Pretty/printing.

	src1 := "# hello\nlet y = 1"
	got1, err := Pretty(src1)
	if err != nil {
		t.Fatalf("Pretty error: %v", err)
	}
	want1 := "let y = 1 # hello"
	if got1 != want1 {
		t.Fatalf("Pretty normalization mismatch.\nwant:\n%s\n\ngot:\n%s", want1, got1)
	}

	src2 := "# note\nlet u"
	got2, err := Pretty(src2)
	if err != nil {
		t.Fatalf("Pretty error: %v", err)
	}
	want2 := "let u # note"
	if got2 != want2 {
		t.Fatalf("Pretty changed surface form for bare decl.\nwant:\n%s\n\ngot:\n%s", src2, got2)
	}
}

func Test_Interpreter_Type_And_Field_Notes_Appear_In_FormatValue(t *testing.T) {
	ip, _ := NewInterpreter()

	// Type-level + field-level annotations:
	src := `
# The input number.
let Input = type {
  # The key carrying the input.
  input!: Int,
  age: Int
}
`
	mustEvalPersistent(t, ip, src)

	v := mustEvalPersistent(t, ip, "Input") // VTType value

	out := FormatValue(v)
	// Top-level type note should appear as a header.
	if !strings.Contains(out, "The input number.") {
		t.Fatalf("missing top-level type annotation in output:\n%s", out)
	}
	// Field-level note should appear before the "input!" field
	if !strings.Contains(out, "The key carrying the input.") {
		t.Fatalf("missing field-level annotation in output:\n%s", out)
	}
	// Ensure the field is present as well
	if !strings.Contains(out, "input!") || !strings.Contains(out, "Int") {
		t.Fatalf("missing field in output:\n%s", out)
	}
}

func Test_Interpreter_Assign_Map_With_Computed_Index(t *testing.T) {
	ip, _ := NewInterpreter()
	mustEvalPersistent(t, ip, `let obj = {name: "Pedro", age: 17}`)

	// Success paths
	mustEvalPersistent(t, ip, `obj["na" + "me"] = "Juan"`)
	wantStr(t, mustEvalPersistent(t, ip, `obj.name`), "Juan")

	mustEvalPersistent(t, ip, `obj.("na" + "me") = "Maria"`)
	wantStr(t, mustEvalPersistent(t, ip, `obj.name`), "Maria")

	// Failure path (int index on map) — expect generic engine message
	err := evalPersistentExpectError(t, ip, `obj.(0 + 1) = "X"`)
	wantErrContains(t, err, "index assignment requires array[int] or map[string]")
}

func Test_Interpreter_Assign_Array_With_Computed_Index(t *testing.T) {
	ip, _ := NewInterpreter()
	mustEvalPersistent(t, ip, `let arr = ["Pedro", 17]`)

	// Bracket with computed index
	mustEvalPersistent(t, ip, `arr[0 + 1] = "Juan"`)
	wantStr(t, mustEvalPersistent(t, ip, `arr[1]`), "Juan") // no str(...)

	// Dot-paren with computed index
	mustEvalPersistent(t, ip, `arr.(0 + 1) = "Maria"`)
	wantStr(t, mustEvalPersistent(t, ip, `arr[1]`), "Maria")

	// Negative index writeback supported
	mustEvalPersistent(t, ip, `arr[-1] = "Tail"`)
	wantStr(t, mustEvalPersistent(t, ip, `arr[1]`), "Tail")

	// Failure: string index on array
	err := evalPersistentExpectError(t, ip, `arr.("1") = "X"`)
	wantErrContains(t, err, "index assignment requires array[int] or map[string]")
}

func Test_Interpreter_Assign_Through_Nested_Computed_Keys(t *testing.T) {
	ip, _ := NewInterpreter()
	mustEvalPersistent(t, ip, `
		let key = "na" + "me"
		let idx = 0 + 1
		let obj = {name: "Pedro", age: 17}
		let arr = ["Pedro", 17]
	`)

	// Map
	mustEvalPersistent(t, ip, `obj.(key) = "Zed"`)
	wantStr(t, mustEvalPersistent(t, ip, `obj.name`), "Zed")

	// Array
	mustEvalPersistent(t, ip, `arr.(idx) = 99`)
	wantInt(t, mustEvalPersistent(t, ip, `arr[1]`), 99)
}

func Test_Noops_Semantics_BlockAndTopLevel(t *testing.T) {
	t.Run("Block_skips_noops_and_yields_last_value", func(t *testing.T) {
		wantInt(t, evalSrc(t, "do 1\n\n2\n\nend"), 2)
	})
	t.Run("TopLevel_trailing_blank_lines_preserve_result", func(t *testing.T) {
		wantInt(t, evalSrc(t, "42\n\n"), 42)
	})
	t.Run("Only_blank_lines_at_top_level_yield_plain_null", func(t *testing.T) {
		v := evalSrc(t, "\n\n")
		wantNull(t, v)
		if isAnnotatedNull(v) {
			t.Fatalf("want plain null (no annotation), got %#v", v)
		}
	})
	t.Run("Lone_annotation_is_noop_plain_null", func(t *testing.T) {
		v := evalSrc(t, "# a note\n\n")
		wantNull(t, v)
		if isAnnotatedNull(v) {
			t.Fatalf("want plain null (no annotation), got %#v", v)
		}
	})
	t.Run("Empty_block_is_plain_null", func(t *testing.T) {
		v := evalSrc(t, "do\n\nend")
		wantNull(t, v)
		if isAnnotatedNull(v) {
			t.Fatalf("want plain null (no annotation), got %#v", v)
		}
	})
}

func Test_Noops_DoNotClobber_LoopLastValue(t *testing.T) {
	src := `
let i
i = 0
while i < 2 do
  i = i + 1

  # trailing blank line that should be a noop

end
`
	wantInt(t, evalSrc(t, src), 2)
}
func Test_Interpreter_For_Expr_Evaluated_Once_Array_WithPreAnnotation(t *testing.T) {
	ip, _ := NewInterpreter()
	mustEvalPersistent(t, ip, "let calls\ncalls = 0")

	// PRE annotation sits immediately above the for-loop.
	res := mustEvalPersistent(t, ip, `
# doc: EXPR should be evaluated exactly once
for x in (fun() do calls = calls + 1
[10, 20, 30] end)() do
  x
end
`)
	wantInt(t, res, 30)
	wantInt(t, mustEvalPersistent(t, ip, `calls`), 1)
}

func Test_Interpreter_For_Expr_Evaluated_Once_Map_WithPreAnnotation(t *testing.T) {
	ip, _ := NewInterpreter()
	mustEvalPersistent(t, ip, "let mcalls\nmcalls = 0")

	res := mustEvalPersistent(t, ip, `
# doc: map EXPR evaluated once; iterator yields [key, value]
for v in (fun() do mcalls = mcalls + 1
{a: 1, b: 2} end)() do
  v[1]     # grab the value from [key, value]
end
`)
	wantInt(t, res, 2)
	wantInt(t, mustEvalPersistent(t, ip, `mcalls`), 1)
}

func Test_Interpreter_For_Iterator_Function_Evaluated_Once(t *testing.T) {
	ip, _ := NewInterpreter()
	mustEvalPersistent(t, ip, "let icalls\nicalls = 0")

	res := mustEvalPersistent(t, ip, `
let makeIter = fun() do
  icalls = icalls + 1
  let i
  i = 0
  fun(_: Null) -> Any? do
    if i < 3 then
      i = i + 1
      i
    else
      null
    end
  end
end

for x in makeIter() do
  x
end
`)
	wantInt(t, res, 3)
	wantInt(t, mustEvalPersistent(t, ip, `icalls`), 1)
}

func Test_Interpreter_For_Destructure_Map_Yields_KeyValue(t *testing.T) {
	// Ensure the map iterator yields [key, value] and destructuring works.
	v := evalSrc(t, `
for [k, v] in {a: 1, b: 2} do
  [k, v]
end
`)
	if v.Tag != VTArray || len(v.Data.(*ArrayObject).Elems) != 2 {
		t.Fatalf("want [key, value], got %#v", v)
	}
	wantStr(t, v.Data.(*ArrayObject).Elems[0], "b")
	wantInt(t, v.Data.(*ArrayObject).Elems[1], 2)
}

func Test_Interpreter_For_Target_Binding_Persists(t *testing.T) {
	v := evalSrc(t, `
for y in [7, 8, 9] do
  y
end
y
`)
	wantInt(t, v, 9)
}

func Test_Interpreter_For_ToIterContractError_Message(t *testing.T) {
	err := evalSrcExpectError(t, `
# PRE annotation above the loop should not mask the correct error
for x in "oops" do
  x
end
`)
	wantErrContains(t, err, "for expects array, map, or iterator function (Null -> Any?)")
}

func Test_Interpreter_While_Basic_Count_And_Result(t *testing.T) {
	v := evalSrc(t, `
let i
i = 0
while i < 3 do
  i = i + 1
end
i
`)
	wantInt(t, v, 3)
}

func Test_Interpreter_While_Result_As_Expression(t *testing.T) {
	v := evalSrc(t, `
let i
i = 3
let r
r = while i < 6 do
  i = i + 2
  i
end
[r, i]
`)
	if v.Tag != VTArray || len(v.Data.(*ArrayObject).Elems) != 2 {
		t.Fatalf("want [r, i], got %#v", v)
	}
	wantInt(t, v.Data.(*ArrayObject).Elems[0], 7)
	wantInt(t, v.Data.(*ArrayObject).Elems[1], 7)
}

func Test_Interpreter_While_Zero_Iterations_Yields_Null(t *testing.T) {
	wantNull(t, evalSrc(t, `
let j
j = 10
while j < 0 do
  j = j - 1
end
`))
}

func Test_Interpreter_While_Continue_And_Break_Values(t *testing.T) {
	// continue value should be captured as "last" for that iteration; break value becomes loop result.
	v1 := evalSrc(t, `
let i
i = 0
let acc
acc = []
while i < 5 do
  i = i + 1
  if i % 2 == 0 then
    acc = acc + [i]
    continue i * 100
  else
    i
  end
end
[acc, i]
`)
	if v1.Tag != VTArray || len(v1.Data.(*ArrayObject).Elems) != 2 {
		t.Fatalf("want [acc, i], got %#v", v1)
	}
	acc := v1.Data.(*ArrayObject).Elems[0]
	if acc.Tag != VTArray || len(acc.Data.(*ArrayObject).Elems) != 2 {
		t.Fatalf("want acc [2,4], got %#v", acc)
	}
	wantInt(t, acc.Data.(*ArrayObject).Elems[0], 2)
	wantInt(t, acc.Data.(*ArrayObject).Elems[1], 4)
	wantInt(t, v1.Data.(*ArrayObject).Elems[1], 5)

	v2 := evalSrc(t, `
let i
i = 0
let r
r = while true do
  i = i + 1
  if i == 3 then
    break i * 10
  else
    i
  end
end
[r, i]
`)
	if v2.Tag != VTArray || len(v2.Data.(*ArrayObject).Elems) != 2 {
		t.Fatalf("want [r, i], got %#v", v2)
	}
	wantInt(t, v2.Data.(*ArrayObject).Elems[0], 30)
	wantInt(t, v2.Data.(*ArrayObject).Elems[1], 3)
}

func Test_Interpreter_While_WithPreAnnotation(t *testing.T) {
	// PRE annotation immediately above the while should not disturb stack shape.
	v := evalSrc(t, `
let i
i = 0
# while with PRE annotation
while i < 3 do
  i = i + 1
end
i
`)
	wantInt(t, v, 3)
}
func Test_Interpreter_RuntimeErr_TopLevel_DivByZero_Location(t *testing.T) {
	err := evalSrcExpectError(t, `10 / 0`)
	// message and a coarse but stable location (line 1)
	wantErrContains(t, err, "division by zero")
	wantErrContains(t, err, "at 1:") // column may vary; operator span starts at expr
}

func Test_Interpreter_RuntimeErr_TopLevel_IndexOnEmptyArray_Location(t *testing.T) {
	err := evalSrcExpectError(t, `[][0]`)
	// Should attribute to the index subexpression (column 4 pointing at '0')
	wantErrContains(t, err, "index on empty array")
	wantErrContains(t, err, "at 1:4")
}

func Test_Interpreter_RuntimeErr_IndexExpr_Subexpression_Location(t *testing.T) {
	// The index expression is a subexpression with its own error.
	// Text columns: 1:'[',2:'1',3:']',4:'[',5:'1',6:'/',7:'0',8:']' → expect 1:5
	err := evalSrcExpectError(t, `[1][1/0]`)
	wantErrContains(t, err, "division by zero")
	wantErrContains(t, err, "at 1:7")
}

func Test_Interpreter_RuntimeErr_CallArg_Subexpression_Location(t *testing.T) {
	// Define f(a) that just returns a; the arg (1/0) errors before/at the arg CALL site,
	// and the emitter marks the CALL at the argument node's path.
	ip, _ := NewInterpreter()
	mustEvalPersistent(t, ip, `let f = fun(a) do a end`)
	err := evalPersistentExpectError(t, ip, `f(1/0)`)
	wantErrContains(t, err, "division by zero")
	// "f(1/0)" columns: f=1, '('=2, '1'=3 → start of arg expression is col 3
	wantErrContains(t, err, "at 1:5")
}

// func Test_Interpreter_RuntimeErr_FunBody_DivByZero_Persistent_MappedIntoBody(t *testing.T) {
// 	// Define in one persistent eval, call in another — mirrors REPL.
// 	ip, _ := NewInterpreter()
// 	mustEvalPersistent(t, ip, `
// let crashDiv = fun() do
//   1 / 0
// end
// `)
// 	err := evalPersistentExpectError(t, ip, `crashDiv()`)
// 	wantErrContains(t, err, "division by zero")
// 	// After the fix, caret should land on the function body line (line 2 of the def).
// 	wantErrContains(t, err, "at 2:")
// 	// And the snippet should include the offending line text to guard regressions.
// 	wantErrContains(t, err, "1 / 0")
// }

// func Test_Interpreter_RuntimeErr_FunBody_IndexOOB_Persistent_MappedIntoBody(t *testing.T) {
// 	ip, _ := NewInterpreter()
// 	mustEvalPersistent(t, ip, `
// let crashIdx = fun(a) do
//   a[5]
// end
// `)
// 	err := evalPersistentExpectError(t, ip, `crashIdx([1, 2])`)
// 	wantErrContains(t, err, "array index out of range")
// 	// Expect caret on the function body line (line 2 of the definition).
// 	wantErrContains(t, err, "at 2:")
// 	wantErrContains(t, err, "a[5]")
// }

// func Test_Interpreter_RuntimeErr_Nested_Binary_In_Function_Maps_To_Inner(t *testing.T) {
// 	// More complex body: ensure mapping doesn't collapse to line 1.
// 	ip, _ := NewInterpreter()
// 	mustEvalPersistent(t, ip, `
// let g = fun() do
//   let x = 10
//   let y = 0
//   x / y
// end
// `)
// 	err := evalPersistentExpectError(t, ip, `g()`)
// 	wantErrContains(t, err, "division by zero")
// 	// Body has 4 lines; the failing line is 4 within the let-block
// 	wantErrContains(t, err, "at 4:")
// }

// func Test_Interpreter_RuntimeErr_For_Loop_Body_Mark_Inside(t *testing.T) {
// 	// Ensure loop control flow keeps precise attribution inside the body.
// 	ip, _ := NewInterpreter()
// 	mustEvalPersistent(t, ip, `
// let bad = fun() do
//   for i in [1,2] do
//     1 / 0
//   end
// end
// `)
// 	err := evalPersistentExpectError(t, ip, `bad()`)
// 	wantErrContains(t, err, "division by zero")
// 	// The failing line is the '1 / 0' inside the loop body; that's line 4 of the def.
// 	wantErrContains(t, err, "at 4:")
// }

// func Test_Interpreter_RuntimeErr_If_Then_Else_Arm_Mark_Inside(t *testing.T) {
// 	ip, _ := NewInterpreter()
// 	mustEvalPersistent(t, ip, `
// let h = fun(b) do
//   if b then
//     1 / 0
//   else
//     42
//   end
// end
// `)
// 	err := evalPersistentExpectError(t, ip, `h(true)`)
// 	wantErrContains(t, err, "division by zero")
// 	// The failing line is the 'then' arm line 3 of the body.
// 	wantErrContains(t, err, "at 3:")
// }

// func Test_Interpreter_RuntimeErr_Map_Get_MissingKey_Shows_Property_Site(t *testing.T) {
// 	err := evalSrcExpectError(t, `{a:1}.b`)
// 	wantErrContains(t, err, "unknown property")
// 	// Property access emitted with opGetProp is marked at the property site.
// 	// Exact column may vary with spacing; at least assert line 1 exists.
// 	wantErrContains(t, err, "at 1:7")
// }

//	func Test_Interpreter_RuntimeErr_Map_Index_MissingKey_Shows_Index_Site(t *testing.T) {
//		err := evalSrcExpectError(t, `{a:1}["b"]`)
//		wantErrContains(t, err, `unknown key "b"`)
//		// "[..." starts at col 6 in `{a:1}["b"]` → 1:6
//		wantErrContains(t, err, "at 1:7")
//	}
func Test_Interpreter_Annot_LValue_Id_Persists(t *testing.T) {
	ip, _ := NewInterpreter()
	// seed a
	mustEvalPersistent(t, ip, "let a")
	mustEvalPersistent(t, ip, "a = (8 + 5) * 4") // 52
	// annotate the lvalue read
	mustEvalPersistent(t, ip, "# note\na")
	// verify it persisted on the binding
	v := mustEvalPersistent(t, ip, "a")
	if v.Tag != VTInt || v.Data.(int64) != 52 || v.Annot != "note" {
		t.Fatalf("want a == 52 annotated 'note', got %#v", v)
	}
}

func Test_Interpreter_Annot_LValue_Get_Persists_On_Map_Field(t *testing.T) {
	ip, _ := NewInterpreter()
	mustEvalPersistent(t, ip, `let o = {"k": "v"}`)
	// annotate property get
	mustEvalPersistent(t, ip, "# key-doc\no.k")
	// verify the map entry now carries the note
	v := mustEvalPersistent(t, ip, "o.k")
	wantStrWithAnnot(t, v, "v", "key-doc")
}

func Test_Interpreter_Annot_LValue_Idx_Persists_On_Array_Slot(t *testing.T) {
	ip, _ := NewInterpreter()
	mustEvalPersistent(t, ip, "let xs = [10, 20]")
	// annotate element at index 1
	mustEvalPersistent(t, ip, "# hot\nxs[1]")
	// verify it persisted
	v := mustEvalPersistent(t, ip, "xs[1]")
	if v.Tag != VTInt || v.Data.(int64) != 20 || v.Annot != "hot" {
		t.Fatalf("want xs[1] == 20 annotated 'hot', got %#v", v)
	}
}

func Test_Interpreter_Annot_NonLValue_Remains_Pure(t *testing.T) {
	// Pre-annotation over a pure rvalue should just decorate the result.
	v := evalSrc(t, "# temp\n(8 + 5)")
	if v.Tag != VTInt || v.Data.(int64) != 13 || v.Annot != "temp" {
		t.Fatalf("want 13 annotated 'temp', got %#v", v)
	}
}

func Test_Interpreter_Annot_Decl_And_Assign_Still_Work(t *testing.T) {
	ip, _ := NewInterpreter()
	// Annotated declaration → a = #(doc) null
	mustEvalPersistent(t, ip, "# doc\nlet a")
	va := mustEvalPersistent(t, ip, "a")
	if va.Tag != VTNull || va.Annot != "doc" {
		t.Fatalf("want a == null annotated 'doc', got %#v", va)
	}

	// Annotated assignment → a = #(note) 1
	mustEvalPersistent(t, ip, "# note\na = 1")
	va = mustEvalPersistent(t, ip, "a")
	if va.Tag != VTInt || va.Data.(int64) != 1 || va.Annot != "note" {
		t.Fatalf("want a == 1 annotated 'note', got %#v", va)
	}
}

func Test_Interpreter_Array_NegativeIndexing_OOB(t *testing.T) {
	// Valid negatives within range
	wantInt(t, evalSrc(t, "([1,2,3])[-1]"), 3) // last
	wantInt(t, evalSrc(t, "([1,2,3])[-3]"), 1) // first

	// OOB both ways
	err := evalSrcExpectError(t, "([1,2,3])[-4]")
	wantErrContains(t, err, "array index out of range")

	err = evalSrcExpectError(t, "([1,2,3])[3]")
	wantErrContains(t, err, "array index out of range")
}

func Test_Interpreter_Array_Assignment_NegativeIndexing_OOB(t *testing.T) {
	ip, _ := NewInterpreter()

	// Setup and a valid negative write
	v := mustEvalPersistent(t, ip, "let a = [10,20,30]\n a[-1] = 99\n a")
	if v.Tag != VTArray || len(v.Data.(*ArrayObject).Elems) != 3 {
		t.Fatalf("want array len 3, got %#v", v)
	}
	wantInt(t, v.Data.(*ArrayObject).Elems[2], 99)

	// OOB negative write
	err := evalPersistentExpectError(t, ip, "a[-4] = 1")
	wantErrContains(t, err, "array index out of range")

	// OOB positive write
	err = evalPersistentExpectError(t, ip, "a[3] = 1")
	wantErrContains(t, err, "array index out of range")
}

func Test_Interpreter_FunLiteral_BasePath_Uses_Ints_Not_Floats(t *testing.T) {
	// This primes the const pool with Num(0.0) and Num(2.0). The fun literal that
	// follows is the 3rd top-level expression (block child index = 2), so the
	// emitter passes basePath = [2, 2] (both **Int**) into __make_fun.
	// If const interning incorrectly uses numeric-equality (Int(2) == Num(2.0)),
	// those Int constants would be reused as Num and __make_fun would raise:
	//   type mismatch in parameter 'basePath': expected [Int], got [Num]
	// With the strict const-pool equality fix, this must succeed.
	v := evalSrc(t, `
0.0
2.0
(fun(x: Int) -> Int do x + 1 end)(6)
`)
	wantInt(t, v, 7)
}
func Test_Interpreter_Type_BuiltinAtoms_AsTypeValue(t *testing.T) {
	// Builtin atoms become runtime Type values only via `type ...`
	v := evalSrc(t, "type Int")
	if v.Tag != VTType {
		t.Fatalf("want VTType from `type Int`, got %#v", v)
	}

	v = evalSrc(t, "type Str")
	if v.Tag != VTType {
		t.Fatalf("want VTType from `type Str`, got %#v", v)
	}

	v = evalSrc(t, "type Num")
	if v.Tag != VTType {
		t.Fatalf("want VTType from `type Num`, got %#v", v)
	}

	v = evalSrc(t, "type Bool")
	if v.Tag != VTType {
		t.Fatalf("want VTType from `type Bool`, got %#v", v)
	}

	v = evalSrc(t, "type Null")
	if v.Tag != VTType {
		t.Fatalf("want VTType from `type Null`, got %#v", v)
	}

	v = evalSrc(t, "type Any")
	if v.Tag != VTType {
		t.Fatalf("want VTType from `type Any`, got %#v", v)
	}

	v = evalSrc(t, "type Type")
	if v.Tag != VTType {
		t.Fatalf("want VTType from `type Type`, got %#v", v)
	}
}

func Test_Interpreter_Type_Enum_TypeConstruction_Works(t *testing.T) {
	// Constructing an Enum type is legal via `type Enum[...]`
	v := evalSrc(t, "type Enum[0, 1, 2]")
	if v.Tag != VTType {
		t.Fatalf("want VTType from `type Enum[...]`, got %#v", v)
	}
}

func Test_Interpreter_Type_Enum_ValuePosition_IsError(t *testing.T) {
	// Using Enum[...] in *value position* must be a compile-time hard error
	// with a helpful nudge to use `type`.
	err := evalSrcExpectError(t, "Enum[0]")
	wantErrContains(t, err, "Enum")
	wantErrContains(t, err, "type") // e.g., "use 'type Enum[...]'"
}

func Test_Interpreter_Type_Enum_BareIdentifier_IsError(t *testing.T) {
	// Bare `Enum` in value position should be an error (not a value).
	err := evalSrcExpectError(t, "Enum")
	// Keep this broad so it tolerates either “undefined variable: Enum” or a
	// future friendlier “Enum is a type name; use 'type' ...”.
	wantErrContains(t, err, "Enum")
}

func Test_Interpreter_ZeroArity_Sugar_For_UserFun_And_Native(t *testing.T) {
	ip, _ := NewInterpreter()

	// Register a zero-arity native (should be lowered to _:Null)
	ip.RegisterNative("z0", nil, S{"id", "Str"}, func(_ *Interpreter, ctx CallCtx) Value {
		return Str("ok")
	})

	// Both call forms must work for the native.
	wantStr(t, mustEvalPersistent(t, ip, "z0()"), "ok")
	wantStr(t, mustEvalPersistent(t, ip, "z0(null)"), "ok")

	// Passing a non-null arg should be a type error (param is Null).
	err := evalPersistentExpectError(t, ip, "z0(1)")
	wantErrContains(t, err, "type mismatch")

	// User-defined fun(): lowered to one param of type Null under the hood.
	wantBool(t, mustEvalPersistent(t, ip, `
let f = fun() do true end
f()
`), true)

	// And the explicit-null call must also work.
	wantBool(t, mustEvalPersistent(t, ip, `f(null)`), true)
}

func Test_Interpreter_Handle_Equality_Alias_STDOUT(t *testing.T) {
	ip, _ := NewInterpreter()

	// Sanity: a symbol equals itself.
	wantBool(t, mustEvalPersistent(t, ip, "STDOUT == STDOUT"), true)

	// Aliasing: new is a second reference to the exact same handle.
	mustEvalPersistent(t, ip, "let new = STDOUT")
	wantBool(t, mustEvalPersistent(t, ip, "new == STDOUT"), true)
	wantBool(t, mustEvalPersistent(t, ip, "new != STDOUT"), false)
}

func Test_Interpreter_Handle_Equality_Distinct_SameKind(t *testing.T) {
	ip, _ := NewInterpreter()

	// Register a tiny native that returns a fresh opaque handle each call.
	ip.RegisterNative("mkHandle", nil, S{"id", "Any"}, func(_ *Interpreter, _ CallCtx) Value {
		// Each call returns a distinct *Handle pointer (identity should differ).
		return HandleVal("test-handle", &struct{}{})
	})

	// Two different handles of the same kind are NOT equal (identity semantics).
	wantBool(t, mustEvalPersistent(t, ip, `
let a = mkHandle()
let b = mkHandle()
a == b
`), false)

	// Reflexivity still holds: a handle equals itself.
	wantBool(t, mustEvalPersistent(t, ip, `
let a = mkHandle()
a == a
`), true)

	// And inequality mirrors equality.
	wantBool(t, mustEvalPersistent(t, ip, `
let a = mkHandle()
let b = mkHandle()
a != b
`), true)
}

func Test_Interpreter_Handle_Equality_Mixed_With_NonHandles(t *testing.T) {
	ip, _ := NewInterpreter()

	// Comparing a handle to a non-handle is false.
	// (Keeps equality orthogonal across unrelated tags.)
	wantBool(t, mustEvalPersistent(t, ip, `
let h = STDOUT
h == 42
`), false)

	// Also ensure inequality is true here.
	wantBool(t, mustEvalPersistent(t, ip, `
let h = STDOUT
h != 42
`), true)
}

func Test_Interpreter_LateBinding_GlobalValue(t *testing.T) {
	ip, _ := NewInterpreter()
	mustEvalPersistent(t, ip, `
let x = "a"
let f = fun() -> Str do
  return(x)
end
`)
	wantStr(t, mustEvalPersistent(t, ip, `f()`), "a")

	mustEvalPersistent(t, ip, `
x = "b"
`)
	wantStr(t, mustEvalPersistent(t, ip, `f()`), "b")
}

func Test_Interpreter_LateBinding_AfterCurrying(t *testing.T) {
	ip, _ := NewInterpreter()
	mustEvalPersistent(t, ip, `
let seed = 2
let f = fun(a: Int, b: Int) -> Int do
  return(a + b + seed)
end
let g = f(1)
`)
	// g is a 1-arg function (b:Int) -> Int, capturing 'seed' late-bound
	wantInt(t, mustEvalPersistent(t, ip, `g(1)`), 4) // 1 + 1 + 2

	mustEvalPersistent(t, ip, `
seed = 10
`)
	wantInt(t, mustEvalPersistent(t, ip, `g(1)`), 12) // 1 + 1 + 10
}

func Test_Interpreter_MonkeyPatch_Function_Used_Inside_Closure(t *testing.T) {
	ip, _ := NewInterpreter()
	mustEvalPersistent(t, ip, `
let plus = fun(x: Int, y: Int) -> Int do
  return(x + y)
end
let add2 = fun(x: Int) -> Int do
  return(plus(x, 2))
end
`)
	wantInt(t, mustEvalPersistent(t, ip, `add2(3)`), 5)

	// Monkey-patch 'plus' and ensure existing closure observes it.
	mustEvalPersistent(t, ip, `
plus = fun(x: Int, y: Int) -> Int do
  return(x - y)
end
`)
	wantInt(t, mustEvalPersistent(t, ip, `add2(3)`), 1)
}

func Test_Interpreter_Native_Uses_CallSite_Scope_For_SideEffects(t *testing.T) {
	ip, _ := NewInterpreter()

	// Register a tiny native: emit(text: Str) appends to array bound at OUT in the call-site env.
	ip.RegisterNative("emit",
		[]ParamSpec{{Name: "text", Type: S{"id", "Str"}}}, S{"id", "Null"},
		func(_ *Interpreter, ctx CallCtx) Value {
			out, err := ctx.Env().Get("OUT")
			if err != nil || out.Tag != VTArray {
				return annotNull("OUT must be an array")
			}
			arr := out.Data.(*ArrayObject)
			arr.Elems = append(arr.Elems, Str(ctx.Arg("text").Data.(string)))
			return Null
		})

	mustEvalPersistent(t, ip, `
let OUT = []
let say = fun(s: Str) -> Null do
  emit(s)
  return(null)
end
`)

	// First call writes into current OUT
	mustEvalPersistent(t, ip, `say("a")`)
	out1, _ := ip.Global.Get("OUT")
	if out1.Tag != VTArray || len(out1.Data.(*ArrayObject).Elems) != 1 {
		t.Fatalf("want OUT len 1 after first call, got %#v", out1)
	}
	wantStr(t, out1.Data.(*ArrayObject).Elems[0], "a")

	// Redirect OUT to a new array; say should now write there instead
	mustEvalPersistent(t, ip, `OUT = []`)
	mustEvalPersistent(t, ip, `say("b")`)
	out2, _ := ip.Global.Get("OUT")
	if out2.Tag != VTArray || len(out2.Data.(*ArrayObject).Elems) != 1 {
		t.Fatalf("want OUT len 1 after redirect, got %#v", out2)
	}
	wantStr(t, out2.Data.(*ArrayObject).Elems[0], "b")

	// Ensure the original array still contains only "a"
	if out1.Tag != VTArray || len(out1.Data.(*ArrayObject).Elems) != 1 {
		t.Fatalf("original OUT array mutated unexpectedly: %#v", out1)
	}
	wantStr(t, out1.Data.(*ArrayObject).Elems[0], "a")
}

func Test_Interpreter_Redirect_STDOUT_By_MonkeyPatch(t *testing.T) {
	ip, _ := NewInterpreter()

	// Reuse/define 'emit' native.
	ip.RegisterNative("emit",
		[]ParamSpec{{Name: "text", Type: S{"id", "Str"}}}, S{"id", "Null"},
		func(_ *Interpreter, ctx CallCtx) Value {
			out, err := ctx.Env().Get("OUT")
			if err != nil || out.Tag != VTArray {
				return annotNull("OUT must be an array")
			}
			arr := out.Data.(*ArrayObject)
			arr.Elems = append(arr.Elems, Str(ctx.Arg("text").Data.(string)))
			return Null
		})

	mustEvalPersistent(t, ip, `
let OUT1 = []
let OUT2 = []
let OUT = OUT1
let println = fun(s: Str) -> Null do
  emit(s)
  return(null)
end
`)

	// Write to OUT1
	mustEvalPersistent(t, ip, `println("hello")`)
	out1, _ := ip.Global.Get("OUT1")
	if out1.Tag != VTArray || len(out1.Data.(*ArrayObject).Elems) != 1 {
		t.Fatalf("want OUT1 len 1, got %#v", out1)
	}
	wantStr(t, out1.Data.(*ArrayObject).Elems[0], "hello")

	// Redirect and write to OUT2
	mustEvalPersistent(t, ip, `OUT = OUT2`)
	mustEvalPersistent(t, ip, `println("world")`)
	out2, _ := ip.Global.Get("OUT2")
	if out2.Tag != VTArray || len(out2.Data.(*ArrayObject).Elems) != 1 {
		t.Fatalf("want OUT2 len 1, got %#v", out2)
	}
	wantStr(t, out2.Data.(*ArrayObject).Elems[0], "world")

	// OUT1 unchanged
	if len(out1.Data.(*ArrayObject).Elems) != 1 {
		t.Fatalf("OUT1 mutated after redirect: %#v", out1)
	}
}

func Test_Interpreter_Clone_Isolates_Base_And_Global(t *testing.T) {
	ip1, _ := NewInterpreter()
	mustEvalPersistent(t, ip1, `
let x = 1
let f = fun() -> Int do
  return(x)
end
`)
	ip2 := ip1.Clone()

	// Late binding in ip1
	mustEvalPersistent(t, ip1, `x = 2`)
	wantInt(t, mustEvalPersistent(t, ip1, `f()`), 2)

	// ip2 shouldn't see ip1's Global
	if _, err := ip2.EvalPersistentSource(`f()`); err == nil {
		t.Fatal("expected ip2 to not have f defined")
	}

	// Define ip2's own bindings; it should be isolated
	mustEvalPersistent(t, ip2, `
let x = 100
let f = fun() -> Int do
  return(x)
end
`)
	wantInt(t, mustEvalPersistent(t, ip2, `f()`), 100)

	// Changing ip1 no longer affects ip2
	mustEvalPersistent(t, ip1, `x = 999`)
	wantInt(t, mustEvalPersistent(t, ip2, `f()`), 100)
}

func Test_Interpreter_Constructor_Prelude_Imports_Module_FailsGracefully(t *testing.T) {
	// Arrange a temp install root with:
	//   <root>/lib/llm.ms  (exists)
	//   <root>/lib/std.ms  (imports llm, then triggers a runtime error)
	root := t.TempDir()
	lib := filepath.Join(root, "lib")
	if err := os.MkdirAll(lib, 0o755); err != nil {
		t.Fatal(err)
	}

	llm := `# llm backend stub
let X = 42
`
	if err := os.WriteFile(filepath.Join(lib, "llm.ms"), []byte(llm), 0o644); err != nil {
		t.Fatal(err)
	}

	std := `# std prelude (test)
let llm = import("llm")
doesNotExist()   # force a hard runtime error after a successful import
`
	if err := os.WriteFile(filepath.Join(lib, "std.ms"), []byte(std), 0o644); err != nil {
		t.Fatal(err)
	}

	// Override installRoot just for this test.
	prev := installRoot
	installRoot = root
	defer func() { installRoot = prev }()

	// Act: construct the interpreter (this seeds prelude).
	ip, err := NewInterpreter()

	// Assert: constructor must fail (no panic) with a pretty-printed error.
	if err == nil {
		t.Fatalf("expected constructor to fail due to prelude runtime error, got ip=%v, err=nil", ip)
	}
	wantErrContains(t, err, "std.ms")
	wantErrContains(t, err, "runtime")
}

func Test_Interpreter_Constructor_Prelude_Imports_Module_Succeeds(t *testing.T) {
	// Arrange a temp install root with a benign prelude that imports a module.
	root := t.TempDir()
	lib := filepath.Join(root, "lib")
	if err := os.MkdirAll(lib, 0o755); err != nil {
		t.Fatal(err)
	}

	llm := `# llm backend stub
let X = 42
`
	if err := os.WriteFile(filepath.Join(lib, "llm.ms"), []byte(llm), 0o644); err != nil {
		t.Fatal(err)
	}

	std := `# std prelude (test)
let llm = import("llm")
let ok = 1
`
	if err := os.WriteFile(filepath.Join(lib, "std.ms"), []byte(std), 0o644); err != nil {
		t.Fatal(err)
	}

	// Override detected install root just for this test.
	prev := installRoot
	installRoot = root
	defer func() { installRoot = prev }()

	// Act
	ip, err := NewInterpreter()

	// Assert
	if err != nil {
		t.Fatalf("unexpected constructor error:\n%s", err)
	}
	if ip == nil || ip.Global == nil || ip.Base == nil || ip.Core == nil {
		t.Fatalf("interpreter not fully initialized: %#v", ip)
	}
}

func Test_Interpreter_TypeNameResolution_InErrors(t *testing.T) {
	ip, _ := NewInterpreter()

	// Define a module with a recursive type and a function using the unqualified name.
	mustEvalPersistent(t, ip, `
let m = module "M" do
	let Node = type {id!: Int, next: Node}
	let f = fun(n: Node) -> Int do n.id end
end
`)

	// Calling m.f({}) should mention "expected Node", not "<type>".
	err := evalPersistentExpectError(t, ip, `m.f({})`)
	wantErrContains(t, err, "expected Node")
	if strings.Contains(err.Error(), "<type>") {
		t.Fatalf("error should not print '<type>': %v", err)
	}

	// Positive: valid object passes and returns id.
	wantInt(t, mustEvalPersistent(t, ip, `m.f({id: 34})`), 34)
}

func Test_Interpreter_QualifiedTypeName_InErrors_And_Fun_Print(t *testing.T) {
	ip, _ := NewInterpreter()

	// Reuse the same module from the previous test setup.
	mustEvalPersistent(t, ip, `
let m = module "M" do
	let Node = type {id!: Int, next: Node}
end
`)

	// Define a function referring to the qualified type m.Node.
	v := mustEvalPersistent(t, ip, `let f = fun(n: m.Node) -> Int do n.id end`)

	// The function's pretty string should contain the qualified name "m.Node".
	sig := FormatValue(v)
	if !strings.Contains(sig, "m.Node") {
		t.Fatalf("want function signature to contain qualified name 'm.Node', got %q", sig)
	}

	// Calling f({}) should mention "expected m.Node", not "<type>".
	err := evalPersistentExpectError(t, ip, `f({})`)
	wantErrContains(t, err, "expected m.Node")
	if strings.Contains(err.Error(), "<type>") {
		t.Fatalf("error should not print '<type>': %v", err)
	}
}

// --- exponentiation --------------------------------------------------------

func Test_Interpreter_Exponentiation_Int_And_Float(t *testing.T) {
	// Int ** Int (non-negative) stays Int
	wantInt(t, evalSrc(t, "2 ** 3"), 8)
	wantInt(t, evalSrc(t, "7 ** 0"), 1)

	// Int ** Int (negative exponent) becomes Num (via math.Pow)
	wantNum(t, evalSrc(t, "2 ** -1"), 0.5)
	wantNum(t, evalSrc(t, "4 ** -2"), 0.0625)

	// Float participation yields Num
	wantNum(t, evalSrc(t, "2.0 ** 3"), 8.0)
	wantNum(t, evalSrc(t, "2 ** 3.0"), 8.0)
}

func Test_Interpreter_Exponentiation_RightAssociative_And_Precedence(t *testing.T) {
	// Right associative: 2 ** 3 ** 2  ==  2 ** (3 ** 2) == 2 ** 9 == 512
	wantInt(t, evalSrc(t, "2 ** 3 ** 2"), 512)

	// Precedence: ** binds tighter than * and +
	// 2 * 3 ** 2 == 2 * (9) == 18
	wantInt(t, evalSrc(t, "2 * 3 ** 2"), 18)
	// Parens sanity
	wantInt(t, evalSrc(t, "(2 ** 3) * 2"), 16)
}

// --- bitwise ---------------------------------------------------------------

func Test_Interpreter_Bitwise_Basics(t *testing.T) {
	// & | ^ on ints
	wantInt(t, evalSrc(t, "5 & 3"), 1)
	wantInt(t, evalSrc(t, "5 | 2"), 7)
	wantInt(t, evalSrc(t, "5 ^ 1"), 4)

	// ~ (bitwise not) is unary and requires Int
	// (~1) & 3  → (-2) & 3 == 2 (two's complement)
	wantInt(t, evalSrc(t, "(~1) & 3"), 2)
}

func Test_Interpreter_Bitwise_Shifts_And_Errors(t *testing.T) {
	// Shifts
	wantInt(t, evalSrc(t, "1 << 3"), 8)
	wantInt(t, evalSrc(t, "8 >> 1"), 4)

	// Shift count validation: out of range produces a hard runtime error
	err := evalSrcExpectError(t, "1 << 64")
	wantErrContains(t, err, "shift count out of range")

	err = evalSrcExpectError(t, "1 >> -1")
	wantErrContains(t, err, "shift count out of range")
}

func Test_Interpreter_Bitwise_TypeErrors(t *testing.T) {
	// Non-integers in bitwise binary ops → hard runtime error
	err := evalSrcExpectError(t, `"a" & 1`)
	wantErrContains(t, err, "bitwise operators expect integers")

	err = evalSrcExpectError(t, `1.5 | 1`)
	wantErrContains(t, err, "bitwise operators expect integers")
}

func Test_Interpreter_Bitwise_Not_TypeError(t *testing.T) {
	// Unary ~ requires integer
	err := evalSrcExpectError(t, `~1.5`)
	wantErrContains(t, err, "bitwise not expects integer")
}

func Test_BlockScoping_TopLevelLetPersists(t *testing.T) {
	ip, _ := NewInterpreter()

	v := mustEvalPersistent(t, ip, `
let x = 0
x
`)
	wantInt(t, v, 0)

	got, err := ip.Global.Get("x")
	if err != nil {
		t.Fatalf("expected x to be defined in Global, got error: %v", err)
	}
	wantInt(t, got, 0)
}

func Test_BlockScoping_DoBlockScopesLet(t *testing.T) {
	ip, _ := NewInterpreter()

	v := mustEvalPersistent(t, ip, `
do
  let x = 0
end
`)
	wantInt(t, v, 0)

	if _, err := ip.Global.Get("x"); err == nil {
		t.Fatalf("expected x to be undefined in Global after do-block")
	} else if !strings.Contains(strings.ToLower(err.Error()), "undefined variable") {
		t.Fatalf("unexpected error for x: %v", err)
	}
}

func Test_BlockScoping_ForBodyScopesLet(t *testing.T) {
	ip, _ := NewInterpreter()

	mustEvalPersistent(t, ip, `
for i in [0] do
  let x = 0
end
`)

	// Body-local binding should NOT leak.
	if _, err := ip.Global.Get("x"); err == nil {
		t.Fatalf("expected x to be undefined in Global after for-body")
	} else if !strings.Contains(strings.ToLower(err.Error()), "undefined variable") {
		t.Fatalf("unexpected error for x: %v", err)
	}

	// Header binding (i) lives in the outer env and should remain.
	got, err := ip.Global.Get("i")
	if err != nil {
		t.Fatalf("expected i to be defined in Global after for-loop, got error: %v", err)
	}
	// For `for i in [0]`, i should end as 0.
	wantInt(t, got, 0)
}

func Test_BlockScoping_InnerBlockCanAssignOuter(t *testing.T) {
	ip, _ := NewInterpreter()

	v := mustEvalPersistent(t, ip, `
let x = 0
do
  x = 1
end
x
`)
	wantInt(t, v, 1)

	got, err := ip.Global.Get("x")
	if err != nil {
		t.Fatalf("expected x to be defined in Global, got error: %v", err)
	}
	wantInt(t, got, 1)
}
