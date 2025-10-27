// === FILE: builtin_single_thread_test.go ===
package mindscript

import (
	"testing"
	"time"
)

func Test_Builtin_SingleThread_Wrap_ReturnsHandle(t *testing.T) {
	ip, _ := NewInterpreter()

	v := evalWithIP(t, ip, `ownerWrap(42, false)`)
	if v.Tag != VTHandle {
		t.Fatalf("ownerWrap should return a handle, got %#v", v)
	}
	h, ok := v.Data.(*Handle)
	if !ok || h == nil || h.Kind != "owner" {
		t.Fatalf("expected Handle(kind='owner'), got %#v", v)
	}
}

func Test_Builtin_SingleThread_Wrap_PinOptional(t *testing.T) {
	ip, _ := NewInterpreter()

	v1 := evalWithIP(t, ip, `ownerWrap("x", false)`)
	v2 := evalWithIP(t, ip, `ownerWrap("x", true)`)
	for i, v := range []Value{v1, v2} {
		if v.Tag != VTHandle {
			t.Fatalf("ownerWrap[%d] should return a handle, got %#v", i, v)
		}
		h, ok := v.Data.(*Handle)
		if !ok || h == nil || h.Kind != "owner" {
			t.Fatalf("ownerWrap[%d] expected Handle(kind='owner'), got %#v", i, v)
		}
	}
}

func Test_Builtin_SingleThread_Serialization_Order(t *testing.T) {
	ip, _ := NewInterpreter()

	// Two runs that append 1 then 2 to m.log; first sleeps to test strict ordering.
	v := evalWithIP(t, ip, `
let o = ownerWrap({log: []}, false)
ownerRun(o, fun(m) do sleep(5) m.log = m.log + [1] end)
ownerRun(o, fun(m) do            m.log = m.log + [2] end)
ownerRun(o, fun(m) -> Any do m.log end)
	`)
	if v.Tag != VTArray {
		t.Fatalf("expected array log, got %#v", v)
	}
	xs := v.Data.(*ArrayObject).Elems
	if len(xs) != 2 || xs[0].Tag != VTInt || xs[0].Data.(int64) != 1 || xs[1].Tag != VTInt || xs[1].Data.(int64) != 2 {
		t.Fatalf("log ordering expected [1,2], got %#v", v)
	}
}

func Test_Builtin_SingleThread_Args_AreCloned_NotAliased(t *testing.T) {
	ip, _ := NewInterpreter()

	// Mutate argument inside owner; caller's arr should remain unchanged.
	v := evalWithIP(t, ip, `
let arr = [1]
let o = ownerWrap(0, false)  # wrapped value unused here
let ret = ownerCall(o, fun(a) do a[0] = 99 a end, [arr])
{ ret: ret, arr: arr }
	`)
	m := mustMap(t, v)
	ret, _ := mget(m, "ret")
	arr, _ := mget(m, "arr")

	if ret.Tag != VTArray || len(ret.Data.(*ArrayObject).Elems) != 1 || ret.Data.(*ArrayObject).Elems[0].Data.(int64) != 99 {
		t.Fatalf("ownerCall should see mutated clone [99], got %#v", ret)
	}
	if arr.Tag != VTArray || len(arr.Data.(*ArrayObject).Elems) != 1 || arr.Data.(*ArrayObject).Elems[0].Data.(int64) != 1 {
		t.Fatalf("caller arr should remain [1], got %#v", arr)
	}
}

func Test_Builtin_SingleThread_GetSet_OnMap(t *testing.T) {
	ip, _ := NewInterpreter()

	v := evalWithIP(t, ip, `
let o = ownerWrap({}, false)
ownerSet(o, "a", 1)
ownerGet(o, "a")
	`)
	if v.Tag != VTInt || v.Data.(int64) != 1 {
		t.Fatalf("ownerGet should return 1, got %#v", v)
	}
}

func Test_Builtin_SingleThread_Get_MissingKey_Annotated(t *testing.T) {
	ip, _ := NewInterpreter()

	v := evalWithIP(t, ip, `
let o = ownerWrap({}, false)
ownerGet(o, "missing")
	`)
	wantAnnotatedContains(t, v, "missing key")
}

func Test_Builtin_SingleThread_Set_NoDuplicateKeys(t *testing.T) {
	ip, _ := NewInterpreter()

	// Set same key twice, then extract keys via ownerRun that builds them in iteration order.
	v := evalWithIP(t, ip, `
let o = ownerWrap({}, false)
ownerSet(o, "x", 1)
ownerSet(o, "x", 2)
ownerRun(o, fun(m) do
  let keys = []
  for kv in m do keys = keys + [kv[0]] end
  keys
end)
	`)
	if v.Tag != VTArray {
		t.Fatalf("expected keys array, got %#v", v)
	}
	keys := v.Data.(*ArrayObject).Elems
	cnt := 0
	for _, k := range keys {
		if k.Tag == VTStr && k.Data.(string) == "x" {
			cnt++
		}
	}
	if cnt != 1 {
		t.Fatalf("expected one 'x' in keys after overwrite, got %d (keys=%#v)", cnt, v)
	}
}

func Test_Builtin_SingleThread_GetSet_RejectsNonMap(t *testing.T) {
	ip, _ := NewInterpreter()

	// Wrap a non-map and attempt get/set
	v1 := evalWithIP(t, ip, `
let o = ownerWrap(42, false)
ownerGet(o, "a")
	`)
	wantAnnotatedContains(t, v1, "wrapped value is not a map")

	v2 := evalWithIP(t, ip, `
let o = ownerWrap(42, false)
ownerSet(o, "a", 1)
	`)
	wantAnnotatedContains(t, v2, "wrapped value is not a map")
}

func Test_Builtin_SingleThread_PanicsBecomeAnnotatedNull(t *testing.T) {
	ip, _ := NewInterpreter()

	v := evalWithIP(t, ip, `
let o = ownerWrap({}, false)
ownerRun(o, fun(m) do panic("boom") end)
	`)
	wantAnnotatedContains(t, v, "boom")
}

func Test_Builtin_SingleThread_RunArgValidation(t *testing.T) {
	ip, _ := NewInterpreter()

	v := evalWithIP(t, ip, `
let o = ownerWrap(0, false)
ownerRun(o, 123)
	`)
	wantAnnotatedContains(t, v, "f must be a function")
}

func Test_Builtin_SingleThread_CallArgValidation(t *testing.T) {
	ip, _ := NewInterpreter()

	v1 := evalWithIP(t, ip, `
let o = ownerWrap(0, false)
ownerCall(o, 123, [])
	`)
	wantAnnotatedContains(t, v1, "f must be a function")

	// wrong 'args' type produces a hard error from the type system; ensure it's an error
	_, err := ip.EvalSource(`
let o = ownerWrap(0, false)
ownerCall(o, fun() do null end, 1)
	`)
	if err == nil {
		t.Fatalf("ownerCall with non-array args should be a hard error")
	}
}

func Test_Builtin_SingleThread_Close_Idempotent_And_AfterClose(t *testing.T) {
	ip, _ := NewInterpreter()

	v := evalWithIP(t, ip, `
let o = ownerWrap({}, false)
let first = ownerClose(o)
let second = ownerClose(o)
{ first: first, second: second }
	`)
	m := mustMap(t, v)
	first, _ := mget(m, "first")
	second, _ := mget(m, "second")
	if first.Tag != VTBool || first.Data.(bool) != true {
		t.Fatalf("first close should return true, got %#v", first)
	}
	wantAnnotatedContains(t, second, "already closed")

	// After close, enqueues should return annotated null.
	v2 := evalWithIP(t, ip, `
let o = ownerWrap({}, false)
ownerClose(o)
ownerRun(o, fun(m) do 1 end)
	`)
	wantAnnotatedContains(t, v2, "owner closed")
}

func Test_Builtin_SingleThread_Close_WaitsForInFlightWork(t *testing.T) {
	ip, _ := NewInterpreter()

	// Long-running op + concurrent close; we at least ensure that run completes
	// and close returns successfully (true).
	v := evalWithIP(t, ip, `
let o = ownerWrap({}, false)
# start a long run
let _ = ownerRun(o, fun(m) do sleep(10) m end)
# close should wait for in-flight work then return true
ownerClose(o)
	`)
	if v.Tag != VTBool || v.Data.(bool) != true {
		t.Fatalf("ownerClose should return true, got %#v", v)
	}
	// Give goroutine a moment to finish shutdown (best-effort, like other suites).
	time.Sleep(5 * time.Millisecond)
}

func Test_Builtin_SingleThread_RunVsCall_ArityParity(t *testing.T) {
	ip, _ := NewInterpreter()

	// ownerRun passes exactly one arg (the wrapped value); ownerCall passes user args.
	v := evalWithIP(t, ip, `
let o = ownerWrap({ a: 1 }, false)
let ar = ownerRun(o, fun(m) -> Int do
  1  # path marker
end)
let ac = ownerCall(o, fun(x, y) -> Int do 2 end, [10, 20])
{ ar: ar, ac: ac }
	`)
	m := mustMap(t, v)
	ar, _ := mget(m, "ar")
	ac, _ := mget(m, "ac")
	if ar.Tag != VTInt || ar.Data.(int64) != 1 {
		t.Fatalf("ownerRun expected path marker 1, got %#v", ar)
	}
	if ac.Tag != VTInt || ac.Data.(int64) != 2 {
		t.Fatalf("ownerCall expected path marker 2, got %#v", ac)
	}
}

// Wrapped value is deep-cloned at wrap-time (no aliasing with caller's value)
func Test_Builtin_SingleThread_WrappedValue_IsCloned_NotAliased(t *testing.T) {
	ip, _ := NewInterpreter()

	v := evalWithIP(t, ip, `
let m = { x: 1 }
let o = ownerWrap(m, false)
m.x = 2                 # mutate caller-side after wrap
ownerRun(o, fun(mm) -> Int do mm.x end)
	`)
	if v.Tag != VTInt || v.Data.(int64) != 1 {
		t.Fatalf("owner should see snapshot x=1, got %#v", v)
	}
}

// Insertion order is preserved by ownerSet for new keys
func Test_Builtin_SingleThread_Set_OrderPreserved(t *testing.T) {
	ip, _ := NewInterpreter()

	v := evalWithIP(t, ip, `
let o = ownerWrap({}, false)
ownerSet(o, "b", 2)
ownerSet(o, "a", 1)
ownerRun(o, fun(m) do
  let keys = []
  for kv in m do keys = keys + [kv[0]] end
  keys
end)
	`)
	if v.Tag != VTArray {
		t.Fatalf("expected keys array, got %#v", v)
	}
	keys := v.Data.(*ArrayObject).Elems
	if len(keys) != 2 || keys[0].Tag != VTStr || keys[0].Data.(string) != "b" ||
		keys[1].Tag != VTStr || keys[1].Data.(string) != "a" {
		t.Fatalf("expected insertion order [\"b\",\"a\"], got %#v", v)
	}
}

// After close: ownerGet/ownerSet return annotated Null (not hard errors)
func Test_Builtin_SingleThread_Close_AfterClose_GetSetAnnotated(t *testing.T) {
	ip, _ := NewInterpreter()

	v1 := evalWithIP(t, ip, `
let o = ownerWrap({}, false)
ownerClose(o)
ownerGet(o, "x")
	`)
	wantAnnotatedContains(t, v1, "owner closed")

	v2 := evalWithIP(t, ip, `
let o = ownerWrap({}, false)
ownerClose(o)
ownerSet(o, "x", 1)
	`)
	wantAnnotatedContains(t, v2, "owner closed")
}

// ownerCall passes user args by value with correct content (not just arity)
func Test_Builtin_SingleThread_Call_PassesArgsValues(t *testing.T) {
	ip, _ := NewInterpreter()

	v := evalWithIP(t, ip, `
let o = ownerWrap(0, false)
ownerCall(o, fun(x: Int, y: Int) -> Int do x + y end, [3, 4])
	`)
	if v.Tag != VTInt || v.Data.(int64) != 7 {
		t.Fatalf("ownerCall should pass args by value (3+4=7), got %#v", v)
	}
}

// ownerRun actually provides the wrapped map as its single argument
func Test_Builtin_SingleThread_Run_PassesWrappedValue(t *testing.T) {
	ip, _ := NewInterpreter()

	v := evalWithIP(t, ip, `
let o = ownerWrap({ a: 5 }, false)
ownerRun(o, fun(m) -> Int do m.a end)
	`)
	if v.Tag != VTInt || v.Data.(int64) != 5 {
		t.Fatalf("ownerRun should pass wrapped value; expected 5, got %#v", v)
	}
}
