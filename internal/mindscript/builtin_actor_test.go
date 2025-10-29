// === FILE: builtin_actor_test.go ===
package mindscript

import (
	"testing"
	"time"
)

func Test_Builtin_Actor_Start_ReturnsHandle(t *testing.T) {
	ip, _ := NewInterpreter()

	v := evalWithIP(t, ip, `actorStart(42, false)`)
	if v.Tag != VTHandle {
		t.Fatalf("actorStart should return a handle, got %#v", v)
	}
	h, ok := v.Data.(*Handle)
	if !ok || h == nil || h.Kind != "actor" {
		t.Fatalf("expected Handle(kind='actor'), got %#v", v)
	}
}

func Test_Builtin_Actor_Start_PinOptional(t *testing.T) {
	ip, _ := NewInterpreter()

	v1 := evalWithIP(t, ip, `actorStart("x", false)`)
	v2 := evalWithIP(t, ip, `actorStart("x", true)`)
	for i, v := range []Value{v1, v2} {
		if v.Tag != VTHandle {
			t.Fatalf("actorStart[%d] should return a handle, got %#v", i, v)
		}
		h, ok := v.Data.(*Handle)
		if !ok || h == nil || h.Kind != "actor" {
			t.Fatalf("actorStart[%d] expected Handle(kind='actor'), got %#v", i, v)
		}
	}
}

func Test_Builtin_Actor_Serialization_Order(t *testing.T) {
	ip, _ := NewInterpreter()

	// Two runs that append 1 then 2 to m.log; first sleeps to test strict ordering.
	v := evalWithIP(t, ip, `
let a = actorStart({log: []}, false)
actorRun(a, fun(m) do sleep(5) m.log = m.log + [1] end)
actorRun(a, fun(m) do            m.log = m.log + [2] end)
actorRun(a, fun(m) -> Any do m.log end)
	`)
	if v.Tag != VTArray {
		t.Fatalf("expected array log, got %#v", v)
	}
	xs := v.Data.(*ArrayObject).Elems
	if len(xs) != 2 || xs[0].Tag != VTInt || xs[0].Data.(int64) != 1 || xs[1].Tag != VTInt || xs[1].Data.(int64) != 2 {
		t.Fatalf("log ordering expected [1,2], got %#v", v)
	}
}

func Test_Builtin_Actor_Args_AreCloned_NotAliased(t *testing.T) {
	ip, _ := NewInterpreter()

	// Mutate argument inside actor; caller's arr should remain unchanged.
	v := evalWithIP(t, ip, `
let arr = [1]
let a = actorStart(0, false)  # wrapped value unused here
let ret = actorCall(a, fun(a) do a[0] = 99 a end, [arr])
{ ret: ret, arr: arr }
	`)
	m := mustMap(t, v)
	ret, _ := mget(m, "ret")
	arr, _ := mget(m, "arr")

	if ret.Tag != VTArray || len(ret.Data.(*ArrayObject).Elems) != 1 || ret.Data.(*ArrayObject).Elems[0].Data.(int64) != 99 {
		t.Fatalf("actorCall should see mutated clone [99], got %#v", ret)
	}
	if arr.Tag != VTArray || len(arr.Data.(*ArrayObject).Elems) != 1 || arr.Data.(*ArrayObject).Elems[0].Data.(int64) != 1 {
		t.Fatalf("caller arr should remain [1], got %#v", arr)
	}
}

func Test_Builtin_Actor_GetSet_OnMap(t *testing.T) {
	ip, _ := NewInterpreter()

	v := evalWithIP(t, ip, `
let a = actorStart({}, false)
actorSet(a, "a", 1)
actorGet(a, "a")
	`)
	if v.Tag != VTInt || v.Data.(int64) != 1 {
		t.Fatalf("actorGet should return 1, got %#v", v)
	}
}

func Test_Builtin_Actor_Get_MissingKey_Annotated(t *testing.T) {
	ip, _ := NewInterpreter()

	v := evalWithIP(t, ip, `
let a = actorStart({}, false)
actorGet(a, "missing")
	`)
	wantAnnotatedContains(t, v, "missing key")
}

func Test_Builtin_Actor_Set_NoDuplicateKeys(t *testing.T) {
	ip, _ := NewInterpreter()

	// Set same key twice, then extract keys via actorRun that builds them in iteration order.
	v := evalWithIP(t, ip, `
let a = actorStart({}, false)
actorSet(a, "x", 1)
actorSet(a, "x", 2)
actorRun(a, fun(m) do
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

func Test_Builtin_Actor_GetSet_RejectsNonMap(t *testing.T) {
	ip, _ := NewInterpreter()

	// Wrap a non-map and attempt get/set
	v1 := evalWithIP(t, ip, `
let a = actorStart(42, false)
actorGet(a, "a")
	`)
	wantAnnotatedContains(t, v1, "wrapped value is not a map")

	v2 := evalWithIP(t, ip, `
let a = actorStart(42, false)
actorSet(a, "a", 1)
	`)
	wantAnnotatedContains(t, v2, "wrapped value is not a map")
}

func Test_Builtin_Actor_PanicsBecomeAnnotatedNull(t *testing.T) {
	ip, _ := NewInterpreter()

	v := evalWithIP(t, ip, `
let a = actorStart({}, false)
actorRun(a, fun(m) do panic("boom") end)
	`)
	wantAnnotatedContains(t, v, "boom")
}

func Test_Builtin_Actor_RunArgValidation(t *testing.T) {
	ip, _ := NewInterpreter()

	v := evalWithIP(t, ip, `
let a = actorStart(0, false)
actorRun(a, 123)
	`)
	wantAnnotatedContains(t, v, "f must be a function")
}

func Test_Builtin_Actor_CallArgValidation(t *testing.T) {
	ip, _ := NewInterpreter()

	v1 := evalWithIP(t, ip, `
let a = actorStart(0, false)
actorCall(a, 123, [])
	`)
	wantAnnotatedContains(t, v1, "f must be a function")

	// wrong 'args' type produces a hard error from the type system; ensure it's an error
	_, err := ip.EvalSource(`
let a = actorStart(0, false)
actorCall(a, fun() do null end, 1)
	`)
	if err == nil {
		t.Fatalf("actorCall with non-array args should be a hard error")
	}
}

func Test_Builtin_Actor_Close_Idempotent_And_AfterClose(t *testing.T) {
	ip, _ := NewInterpreter()

	v := evalWithIP(t, ip, `
let a = actorStart({}, false)
let first = actorClose(a)
let second = actorClose(a)
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
let a = actorStart({}, false)
actorClose(a)
actorRun(a, fun(m) do 1 end)
	`)
	wantAnnotatedContains(t, v2, "actor closed")
}

func Test_Builtin_Actor_Close_WaitsForInFlightWork(t *testing.T) {
	ip, _ := NewInterpreter()

	// Long-running op + concurrent close; we at least ensure that run completes
	// and close returns successfully (true).
	v := evalWithIP(t, ip, `
let a = actorStart({}, false)
# start a long run
let _ = actorRun(a, fun(m) do sleep(10) m end)
# close should wait for in-flight work then return true
actorClose(a)
	`)
	if v.Tag != VTBool || v.Data.(bool) != true {
		t.Fatalf("actorClose should return true, got %#v", v)
	}
	// Give goroutine a moment to finish shutdown (best-effort, like other suites).
	time.Sleep(5 * time.Millisecond)
}

func Test_Builtin_Actor_RunVsCall_ArityParity(t *testing.T) {
	ip, _ := NewInterpreter()

	// actorRun passes exactly one arg (the wrapped value); actorCall passes user args.
	v := evalWithIP(t, ip, `
let a = actorStart({ a: 1 }, false)
let ar = actorRun(a, fun(m) -> Int do
  1  # path marker
end)
let ac = actorCall(a, fun(x, y) -> Int do 2 end, [10, 20])
{ ar: ar, ac: ac }
	`)
	m := mustMap(t, v)
	ar, _ := mget(m, "ar")
	ac, _ := mget(m, "ac")
	if ar.Tag != VTInt || ar.Data.(int64) != 1 {
		t.Fatalf("actorRun expected path marker 1, got %#v", ar)
	}
	if ac.Tag != VTInt || ac.Data.(int64) != 2 {
		t.Fatalf("actorCall expected path marker 2, got %#v", ac)
	}
}

// Wrapped value is deep-cloned at start-time (no aliasing with caller's value)
func Test_Builtin_Actor_WrappedValue_IsCloned_NotAliased(t *testing.T) {
	ip, _ := NewInterpreter()

	v := evalWithIP(t, ip, `
let m = { x: 1 }
let a = actorStart(m, false)
m.x = 2                 # mutate caller-side after start
actorRun(a, fun(mm) -> Int do mm.x end)
	`)
	if v.Tag != VTInt || v.Data.(int64) != 1 {
		t.Fatalf("actor should see snapshot x=1, got %#v", v)
	}
}

// Insertion order is preserved by actorSet for new keys
func Test_Builtin_Actor_Set_OrderPreserved(t *testing.T) {
	ip, _ := NewInterpreter()

	v := evalWithIP(t, ip, `
let a = actorStart({}, false)
actorSet(a, "b", 2)
actorSet(a, "a", 1)
actorRun(a, fun(m) do
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

// After close: actorGet/actorSet return annotated Null (not hard errors)
func Test_Builtin_Actor_Close_AfterClose_GetSetAnnotated(t *testing.T) {
	ip, _ := NewInterpreter()

	v1 := evalWithIP(t, ip, `
let a = actorStart({}, false)
actorClose(a)
actorGet(a, "x")
	`)
	wantAnnotatedContains(t, v1, "actor closed")

	v2 := evalWithIP(t, ip, `
let a = actorStart({}, false)
actorClose(a)
actorSet(a, "x", 1)
	`)
	wantAnnotatedContains(t, v2, "actor closed")
}

// actorCall passes user args by value with correct content (not just arity)
func Test_Builtin_Actor_Call_PassesArgsValues(t *testing.T) {
	ip, _ := NewInterpreter()

	v := evalWithIP(t, ip, `
let a = actorStart(0, false)
actorCall(a, fun(x: Int, y: Int) -> Int do x + y end, [3, 4])
	`)
	if v.Tag != VTInt || v.Data.(int64) != 7 {
		t.Fatalf("actorCall should pass args by value (3+4=7), got %#v", v)
	}
}

// actorRun actually provides the wrapped map as its single argument
func Test_Builtin_Actor_Run_PassesWrappedValue(t *testing.T) {
	ip, _ := NewInterpreter()

	v := evalWithIP(t, ip, `
let a = actorStart({ a: 5 }, false)
actorRun(a, fun(m) -> Int do m.a end)
	`)
	if v.Tag != VTInt || v.Data.(int64) != 5 {
		t.Fatalf("actorRun should pass wrapped value; expected 5, got %#v", v)
	}
}
