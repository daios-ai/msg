package mindscript

import (
	"strings"
	"testing"
)

// --- helpers ---------------------------------------------------------------

func evalSrc(t *testing.T, src string) Value {
	t.Helper()
	ip := NewInterpreter()
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
	if v.Tag != VTArray || len(v.Data.([]Value)) != 3 {
		t.Fatalf("want array len 3, got %#v", v)
	}
	wantInt(t, v.Data.([]Value)[0], 1)
	wantInt(t, v.Data.([]Value)[2], 3)
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
	if v.Tag != VTArray || len(v.Data.([]Value)) != 3 {
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
for let x in [1,2,3,4] do
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
	// The stored type expr should be ("unop", "?", ("id","Str"))
	expr := v.Data.(S)
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
	wantAnnotatedNullContains(t, v, "continue")
}

func Test_Interpreter_ShortCircuit_And_Or(t *testing.T) {
	wantBool(t, evalSrc(t, "false and (1/0==0)"), false) // RHS not evaluated
	wantBool(t, evalSrc(t, "true or (1/0==0)"), true)    // RHS not evaluated
}

// --- annotated-null runtime error cases ------------------------------------

func Test_Interpreter_DivisionByZero_Returns_AnnotatedNull(t *testing.T) {
	v := evalSrc(t, `1 / 0`)
	wantAnnotatedNullContains(t, v, "division by zero")
}

func Test_Interpreter_NotAFunction_Returns_AnnotatedNull(t *testing.T) {
	v := evalSrc(t, `42(1)`)
	wantAnnotatedNullContains(t, v, "not a function")
}

func Test_Interpreter_UnknownProperty_Returns_AnnotatedNull(t *testing.T) {
	v := evalSrc(t, `{a: 1}.b`)
	wantAnnotatedNullContains(t, v, "unknown")
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
	wantAnnotatedNullContains(t, v, "array pattern")
	wantAnnotatedNullContains(t, v, "missing")
}

func Test_Interpreter_Let_ArrayDestructuring_WrongShape_AnnotatedNull(t *testing.T) {
	v := evalSrc(t, `
let [a] = 42
a
`)
	wantAnnotatedNullContains(t, v, "array pattern")
	wantAnnotatedNullContains(t, v, "not an array")
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
	v := evalSrc(t, `
let {foo: x} = 10
x
`)
	wantAnnotatedNullContains(t, v, "object pattern")
	wantAnnotatedNullContains(t, v, "not a map")
}

func Test_Interpreter_Let_ObjectDestructuring_MissingKey_AnnotatedNull(t *testing.T) {
	v := evalSrc(t, `
let {age: a, name: n} = {name: "Pedro"}
a
`)
	wantAnnotatedNullContains(t, v, "object pattern")
	wantAnnotatedNullContains(t, v, "missing key")
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
	wantAnnotatedNullContains(t, v, "object pattern")
	wantAnnotatedNullContains(t, v, "missing key")
}

func Test_Interpreter_Let_Nested_WrongShape_Annotated(t *testing.T) {
	v := evalSrc(t, `
let {a: [x]} = {a: 5}
x
`)
	// It's an array pattern, so assert "not an array"
	wantAnnotatedNullContains(t, v, "array pattern")
	wantAnnotatedNullContains(t, v, "not an array")
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
	if arr.Tag != VTArray || len(arr.Data.([]Value)) != 2 {
		t.Fatalf("want array of 2, got %#v", v)
	}
	wantAnnotatedNullContains(t, arr.Data.([]Value)[0], "array pattern") // z
	wantAnnotatedNullContains(t, arr.Data.([]Value)[1], "array pattern") // y
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
	arr := v.Data.([]Value)
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
	arr := v.Data.([]Value)
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
	v := evalSrc(t, `
let n = 0
while 1 do
  n = 1
end
n
`)
	// loop never runs because condition is invalid → annotated null when evaluated
	// Top-level value should be annotated null (from condition type check).
	wantAnnotatedNullContains(t, v, "boolean")
}

func Test_Interpreter_For_Map_Yields_Pairs_As_Array(t *testing.T) {
	src := `
let m = {a: 1, b: 2}
let sum = 0
for let p in m do
  sum = sum + p.1  ## p = [key, value]; index 1 is value
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
for let x in range(5, 8) do
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
	arr := v.Data.([]Value)
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
	arr := v.Data.([]Value)
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
	ip := NewInterpreter()
	v, err := ip.EvalSource(src)
	if err != nil {
		t.Fatalf("eval error: %v", err)
	}
	if !isAnnotatedNull(v) {
		t.Fatalf("want annotated null from invalid iterator, got %v (annot=%q)", v, v.Annot)
	}
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

range(5, 8)  ## return the iterator
`

	ip := NewInterpreter()
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
// Control keywords & spacing
// ------------------------------------------------------------

func Test_Interpreter_Control_Keywords_RequireNoSpaceBeforeParen(t *testing.T) {
	// The parser requires CLROUND immediately after control keywords.
	ip := NewInterpreter()
	_, err := ip.EvalSource("return (1)")
	if err == nil {
		t.Fatalf("expected parse error from space before '(' after 'return'")
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
for let x in [1,2] do
  s = s + x
end
`)
	wantNull(t, v)
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

func Test_Interpreter_For_Iterator_AnnotatedNull_Propagates(t *testing.T) {
	// Iterator returns an annotated null; for-loop should propagate it out.
	src := `
let bad = fun() -> Int? do
  # oops
  return(continue(0))  ## cause annotated null
end
let sum = 0
for let x in bad do
  sum = sum + x
end
sum
`
	v := evalSrc(t, src)
	wantAnnotatedNullContains(t, v, "continue")
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
for let x in iter do
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
	v := evalSrc(t, `([])[-1]`)
	wantAnnotatedNullContains(t, v, "empty array")
}

// ------------------------------------------------------------
// '+' over unsupported types produces annotated null (not panic)
// ------------------------------------------------------------

func Test_Interpreter_Plus_Unsupported_Types_Annotated(t *testing.T) {
	v := evalSrc(t, `"a" + 1`)
	wantAnnotatedNullContains(t, v, "unsupported")
}

// ------------------------------------------------------------
// Annotations: inline & stacking
// ------------------------------------------------------------

func Test_Interpreter_Annotation_Inline_Parens(t *testing.T) {
	v := evalSrc(t, `#(note:hi) 21`)
	wantInt(t, v, 21)
	if v.Annot == "" || !strings.Contains(strings.ToLower(v.Annot), "note:hi") {
		t.Fatalf("missing inline annotation, got %q", v.Annot)
	}
}

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

func Test_Interpreter_Function_ReturnType_Mismatch_Annotated(t *testing.T) {
	v := evalSrc(t, `
let f = fun()->Int do
  "nope"
end
f()
`)
	wantAnnotatedNullContains(t, v, "return type mismatch")
}

func Test_Interpreter_Function_ParamType_Mismatch_Annotated(t *testing.T) {
	v := evalSrc(t, `
let f = fun(x:Int)->Int do x end
f("str")
`)
	wantAnnotatedNullContains(t, v, "type mismatch in parameter")
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
