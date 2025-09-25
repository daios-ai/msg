// === FILE: std_sys_concurrency_test.go ===
package mindscript

import (
	"fmt"
	"testing"
	"time"
)

// ---------- local helpers (kept minimal; mirror other suites) ----------

func wantErr(t *testing.T, err error) {
	t.Helper()
	if err == nil {
		t.Fatalf("wanted error, got <nil>")
	}
}

// evalWithIP is provided in the shared test helpers in this repo. We rely on it here.

// ---------------- Concurrency ergonomics tests ----------------

func Test_Builtin_Concurrency_ChanOpen_BufferedAndUnbuffered(t *testing.T) {
	ip, _ := NewRuntime()

	// Unbuffered: try-recv should be empty, blocking send/recv via procSpawn works.
	v := evalWithIP(t, ip, `
let c = chanOpen()
let s0 = chanTrySend(c, "x")           # no receiver yet → false
let worker = fun() do chanSend(c, "hello") end
let p = procSpawn(worker)
let r = chanRecv(c)                    # should receive "hello"
procJoin(p)
{ s0: s0, r: r }
	`)
	m := mustMap(t, v)
	if s0, _ := mget(m, "s0"); s0.Tag != VTBool || s0.Data.(bool) {
		t.Fatalf("unbuffered chanTrySend expected false, got %#v", s0)
	}
	if r, _ := mget(m, "r"); r.Tag != VTStr || r.Data.(string) != "hello" {
		t.Fatalf("chanRecv expected 'hello', got %#v", r)
	}

	// Buffered: capacity 1 → first trySend true, second false; then tryRecv yields first value.
	v2 := evalWithIP(t, ip, `
let c = chanOpen(1)
let s1 = chanTrySend(c, "a")           # buffer has room → true
let s2 = chanTrySend(c, "b")           # buffer full → false
let r1 = chanTryRecv(c)                # ok → true, value → "a"
let r2 = chanTryRecv(c)                # empty → ok=false
{ s1: s1, s2: s2, r1: r1, r2: r2 }
	`)
	m2 := mustMap(t, v2)
	if s1, _ := mget(m2, "s1"); s1.Tag != VTBool || s1.Data.(bool) != true {
		t.Fatalf("buffered s1 expected true, got %#v", s1)
	}
	if s2, _ := mget(m2, "s2"); s2.Tag != VTBool || s2.Data.(bool) != false {
		t.Fatalf("buffered s2 expected false, got %#v", s2)
	}
	r1 := mustMap(t, m2.Entries["r1"])
	if ok, _ := mget(r1, "ok"); ok.Tag != VTBool || ok.Data.(bool) != true {
		t.Fatalf("r1.ok expected true, got %#v", ok)
	}
	if val, _ := mget(r1, "value"); val.Tag != VTStr || val.Data.(string) != "a" {
		t.Fatalf("r1.value expected 'a', got %#v", val)
	}
	r2 := mustMap(t, m2.Entries["r2"])
	if ok, _ := mget(r2, "ok"); ok.Tag != VTBool || ok.Data.(bool) != false {
		t.Fatalf("r2.ok expected false, got %#v", ok)
	}
	if val, _ := mget(r2, "value"); val.Tag != VTNull || val.Annot != "" {
		t.Fatalf("r2.value expected bare null, got %#v", val)
	}
}

func Test_Builtin_Concurrency_TryRecv_OnClosedChannel(t *testing.T) {
	ip, _ := NewRuntime()

	v := evalWithIP(t, ip, `
let c = chanOpen()
chanClose(c)
chanTryRecv(c)
	`)
	m := mustMap(t, v)
	okv, _ := mget(m, "ok")
	valv, _ := mget(m, "value")
	if okv.Tag != VTBool || okv.Data.(bool) != true {
		t.Fatalf("ok should be true on closed channel, got %#v", okv)
	}
	wantAnnotatedContains(t, valv, "channel closed")

	// Blocking receive after close should also return annotated null.
	v2 := evalWithIP(t, ip, `
let c = chanOpen()
chanClose(c)
chanRecv(c)
	`)
	wantAnnotatedContains(t, v2, "channel closed")
}

func Test_Builtin_Concurrency_SendOnClosed_NoHardError(t *testing.T) {
	ip, _ := NewRuntime()

	// chanTrySend after close should NOT be a hard error.
	_, err := ip.EvalSource(`
let c = chanOpen()
chanClose(c)
chanTrySend(c, 1)
	`)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	// chanSend after close should NOT be a hard error either (it returns Null(err)).
	_, err = ip.EvalSource(`
let c = chanOpen()
chanClose(c)
chanSend(c, 1)
	`)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
}

func Test_Builtin_Concurrency_ChanOpen_NegativeCap_HardError(t *testing.T) {
	ip, _ := NewRuntime()
	_, err := ip.EvalSource(`chanOpen(-1)`)
	wantErrContains(t, err, "cap must be >= 0")
}

func Test_Builtin_Concurrency_TimerAfter(t *testing.T) {
	ip, _ := NewRuntime()

	// timerAfter emits one tick then closes.
	v := evalWithIP(t, ip, `
let c = timerAfter(20)
let first = chanRecv(c)
let second = chanRecv(c)
{ first: first, second: second }
	`)
	m := mustMap(t, v)
	first, _ := mget(m, "first")
	second, _ := mget(m, "second")
	if first.Tag != VTInt {
		t.Fatalf("first tick should be Int millis, got %#v", first)
	}
	// After closure, chanRecv returns annotated null with "channel closed".
	wantAnnotatedContains(t, second, "channel closed")
}

func Test_Builtin_Concurrency_Ticker_StopOnClose(t *testing.T) {
	ip, _ := NewRuntime()

	// Read two ticks, ensure monotonic, then close and confirm closed behavior.
	v := evalWithIP(t, ip, `
let c = ticker(10)
let a = chanRecv(c)
let b = chanRecv(c)
chanClose(c)
let after = chanRecv(c)
{ a: a, b: b, after: after }
	`)
	m := mustMap(t, v)
	a, _ := mget(m, "a")
	b, _ := mget(m, "b")
	after, _ := mget(m, "after")

	if a.Tag != VTInt || b.Tag != VTInt {
		t.Fatalf("ticks should be Int millis, got a=%#v b=%#v", a, b)
	}
	if b.Data.(int64) < a.Data.(int64) {
		t.Fatalf("ticks must be non-decreasing, got a=%v b=%v", a.Data.(int64), b.Data.(int64))
	}
	wantAnnotatedContains(t, after, "channel closed")

	// Give goroutine a moment to observe closure (best-effort).
	time.Sleep(10 * time.Millisecond)
}

func Test_Builtin_Concurrency_ProcCancel_Smoke(t *testing.T) {
	ip, _ := NewRuntime()

	// Spawn something that sleeps, then cancel; join should still eventually return.
	out := evalWithIP(t, ip, `
let sleeper = fun() do
  sleep(10)
  42
end
let p = procSpawn(sleeper)
procCancel(p)    # best-effort; not observable directly here
procJoin(p)
	`)
	if out.Tag != VTInt || out.Data.(int64) != 42 {
		t.Fatalf("procCancel/procJoin unexpected result: %#v", out)
	}
}

// ---------------- procJoinAll / procJoinAny ----------------

func Test_Concurrency_ProcJoinAll_Basic(t *testing.T) {
	ip, _ := NewRuntime()

	v := evalWithIP(t, ip, `
let f1 = fun() -> Int do 1 end
let f2 = fun() -> Int do 
	sleep(5)
	2 
end
let f3 = fun() -> Int do 3 end
let p1 = procSpawn(f1)
let p2 = procSpawn(f2)
let p3 = procSpawn(f3)
procJoinAll([p1, p2, p3])
	`)
	// Expect results aligned with input order: [1,2,3]
	if v.Tag != VTArray {
		t.Fatalf("procJoinAll should return [Any], got %#v", v)
	}
	xs := v.Data.(*ArrayObject).Elems
	if len(xs) != 3 {
		t.Fatalf("want 3 results, got %d", len(xs))
	}
	want := []int64{1, 2, 3}
	for i := range want {
		if xs[i].Tag != VTInt || xs[i].Data.(int64) != want[i] {
			t.Fatalf("index %d: want %d, got %#v", i, want[i], xs[i])
		}
	}
}

func Test_Concurrency_ProcJoinAll_WithFailure(t *testing.T) {
	ip, _ := NewRuntime()

	v := evalWithIP(t, ip, `
let ok = fun() -> Int do 7 end
let bad = fun() do panic("boom") end
let ps = [ procSpawn(ok), procSpawn(bad) ]
procJoinAll(ps)
	`)
	if v.Tag != VTArray {
		t.Fatalf("procJoinAll should return [Any], got %#v", v)
	}
	xs := v.Data.(*ArrayObject).Elems
	if len(xs) != 2 {
		t.Fatalf("want 2 results, got %d", len(xs))
	}
	// xs[0] == 7
	if xs[0].Tag != VTInt || xs[0].Data.(int64) != 7 {
		t.Fatalf("xs[0] expected 7, got %#v", xs[0])
	}
	// xs[1] should be annotated null containing "boom!"
	wantAnnotatedContains(t, xs[1], "boom")
}

func Test_Concurrency_ProcJoinAny_FirstFinisher(t *testing.T) {
	ip, _ := NewRuntime()

	v := evalWithIP(t, ip, `
let slow = fun() -> Int do
	sleep(20)
	1 
end
let fast = fun() -> Int do
	sleep(5)
	2 
end
let p1 = procSpawn(slow)
let p2 = procSpawn(fast)
procJoinAny([p1, p2])
	`)
	// Expect a map: { index: Int, value: Any }
	if v.Tag != VTMap {
		t.Fatalf("procJoinAny should return {index:Int, value:Any}, got %#v", v)
	}
	m := v.Data.(*MapObject)
	idx, _ := mget(m, "index")
	val, _ := mget(m, "value")
	if idx.Tag != VTInt {
		t.Fatalf("index must be Int, got %#v", idx)
	}
	if val.Tag != VTInt || val.Data.(int64) != 2 {
		t.Fatalf("value should be 2 (fast), got %#v", val)
	}
	if idx.Data.(int64) != 1 {
		t.Fatalf("index should be 1 (fast was second in input), got %d", idx.Data.(int64))
	}
}

// ---------------- stress: many isolates in parallel (no races) ----------------

func Test_Concurrency_Stress_ProcJoinAll_NoDataRaces(t *testing.T) {
	ip, _ := NewRuntime()

	const N = 60
	// Build a small program that spawns N zero-arg funcs returning their index,
	// then procJoinAll, then sums the results to verify we got all of them.
	// (sum 0..N-1) = N*(N-1)/2
	src := "let ps = [\n"
	for i := 0; i < N; i++ {
		src += fmt.Sprintf("  procSpawn(fun() -> Int do sleep(%d) %d end)%s\n",
			i%5, i, map[bool]string{true: ",", false: ""}[i != N-1])
	}
	src += "]\n"
	src += "let rs = procJoinAll(ps)\n"
	src += "let s = 0\n"
	src += "for x in rs do s = s + x end\n"
	src += "s\n"

	v := evalWithIP(t, ip, src)
	if v.Tag != VTInt {
		t.Fatalf("sum should be Int, got %#v", v)
	}
	want := int64(N * (N - 1) / 2)
	if v.Data.(int64) != want {
		t.Fatalf("want sum=%d, got %d", want, v.Data.(int64))
	}
}

// ---------------- stress: channel fan-in (producers/consumers) ----------------

func Test_Concurrency_Stress_Channel_FanIn(t *testing.T) {
	ip, _ := NewRuntime()

	const P = 6    // producers
	const K = 20   // messages per producer
	const cap = 64 // buffer

	// Each producer sends base..base+K-1, base multiples avoid collisions.
	expect := int64(0)
	for p := 0; p < P; p++ {
		base := p * 1000
		for i := 0; i < K; i++ {
			expect += int64(base + i)
		}
	}

	src := fmt.Sprintf(`
let c = chanOpen(%d)

let mk = fun(base: Int, count: Int) do
  let i = 0
  while i < count do
    chanSend(c, base + i)
    i = i + 1
  end
  null
end
`, cap)

	// spawn producers
	src += "let ps = [\n"
	for p := 0; p < P; p++ {
		base := p * 1000
		src += fmt.Sprintf("  procSpawn(fun() do mk(%d, %d) end)%s\n",
			base, K, map[bool]string{true: ",", false: ""}[p != P-1])
	}
	src += "]\n"

	// receive exactly P*K values and sum them
	src += fmt.Sprintf("let need = %d\n", P*K)
	src += "let got = 0\n"
	src += "let sum = 0\n"
	src += "while got < need do\n"
	src += "  sum = sum + chanRecv(c)\n"
	src += "  got = got + 1\n"
	src += "end\n"
	src += "chanClose(c)\n"
	src += "procJoinAll(ps)\n"
	src += "sum\n"

	v := evalWithIP(t, ip, src)
	if v.Tag != VTInt {
		t.Fatalf("sum should be Int, got %#v", v)
	}
	if v.Data.(int64) != expect {
		t.Fatalf("fan-in sum mismatch: want %d, got %d", expect, v.Data.(int64))
	}

	// Give goroutines a tiny grace period to observe close (helps -race stability).
	time.Sleep(5 * time.Millisecond)
}

// ---------------- procCancel remains best-effort ----------------

func Test_Concurrency_ProcCancel_StillBestEffort(t *testing.T) {
	ip, _ := NewRuntime()

	// This test just exercises the API surface; cancellation is cooperative.
	out := evalWithIP(t, ip, `
let sleeper = fun() -> Int do
  sleep(10)
  99
end
let p = procSpawn(sleeper)
procCancel(p)    # best-effort; not observable directly here
procJoin(p)
	`)
	if out.Tag != VTInt || out.Data.(int64) != 99 {
		t.Fatalf("procCancel/procJoin unexpected result: %#v", out)
	}
}

// 1) chanTryRecv on empty, unbuffered → {ok:false, value:null}
func Test_Builtin_Concurrency_TryRecv_OnEmptyUnbuffered(t *testing.T) {
	ip, _ := NewRuntime()
	v := evalWithIP(t, ip, `
let c = chanOpen()
chanTryRecv(c)
	`)
	m := mustMap(t, v)
	okv, _ := mget(m, "ok")
	valv, _ := mget(m, "value")
	if okv.Tag != VTBool || okv.Data.(bool) != false {
		t.Fatalf("ok should be false on empty channel, got %#v", okv)
	}
	if valv.Tag != VTNull || valv.Annot != "" {
		t.Fatalf("value should be bare null, got %#v", valv)
	}
}

// 2) chanClose is idempotent
func Test_Builtin_Concurrency_ChanClose_Idempotent(t *testing.T) {
	ip, _ := NewRuntime()
	v := evalWithIP(t, ip, `
let c = chanOpen()
chanClose(c)
chanClose(c)
42
	`)
	if v.Tag != VTInt || v.Data.(int64) != 42 {
		t.Fatalf("second chanClose should be a no-op, got %#v", v)
	}
}

// 3) chanOpen type validation (wrong types → hard error)
func Test_Builtin_Concurrency_ChanOpen_TypeValidation(t *testing.T) {
	ip, _ := NewRuntime()
	_, err := ip.EvalSource(`chanOpen("1")`)
	wantErr(t, err)
	_, err = ip.EvalSource(`chanOpen(1.5)`)
	wantErr(t, err)
}

// 4) procSpawn argument validation (non-function → hard error)
func Test_Builtin_Concurrency_ProcSpawn_ArgValidation(t *testing.T) {
	ip, _ := NewRuntime()
	_, err := ip.EvalSource(`procSpawn(123)`)
	wantErr(t, err)
}

// 5) procJoinAll([]) → []
func Test_Builtin_Concurrency_ProcJoinAll_Empty(t *testing.T) {
	ip, _ := NewRuntime()
	v := evalWithIP(t, ip, `procJoinAll([])`)
	if v.Tag != VTArray || len(v.Data.(*ArrayObject).Elems) != 0 {
		t.Fatalf("procJoinAll([]) should return empty array, got %#v", v)
	}
}

// 6) procJoinAny([]) → annotated null ("empty list")
func Test_Builtin_Concurrency_ProcJoinAny_Empty(t *testing.T) {
	ip, _ := NewRuntime()
	v := evalWithIP(t, ip, `procJoinAny([])`)
	wantAnnotatedContains(t, v, "empty list")
}

// 7) timerAfter(0) emits one tick immediately, then closed
func Test_Builtin_Concurrency_TimerAfter_Zero(t *testing.T) {
	ip, _ := NewRuntime()
	v := evalWithIP(t, ip, `
let c = timerAfter(0)
let first = chanRecv(c)
let second = chanRecv(c)
{ first: first, second: second }
	`)
	m := mustMap(t, v)
	first, _ := mget(m, "first")
	second, _ := mget(m, "second")
	if first.Tag != VTInt {
		t.Fatalf("first tick should be Int millis, got %#v", first)
	}
	wantAnnotatedContains(t, second, "channel closed")
}

// 8) ticker invalid ms (0 and negative) → hard errors
func Test_Builtin_Concurrency_Ticker_InvalidArgs(t *testing.T) {
	ip, _ := NewRuntime()
	_, err := ip.EvalSource(`ticker(0)`)
	wantErrContains(t, err, "must be > 0")
	_, err = ip.EvalSource(`ticker(-5)`)
	wantErrContains(t, err, "must be > 0")
}

// 9) chanTryRecv after close with buffered data
func Test_Builtin_Concurrency_TryRecv_AfterCloseWithBufferedData(t *testing.T) {
	ip, _ := NewRuntime()
	v := evalWithIP(t, ip, `
let c = chanOpen(1)
chanSend(c, "x")
chanClose(c)
let r1 = chanTryRecv(c)   # should get "x"
let r2 = chanTryRecv(c)   # channel closed
{ r1: r1, r2: r2 }
	`)
	m := mustMap(t, v)

	r1 := mustMap(t, m.Entries["r1"])
	ok1, _ := mget(r1, "ok")
	val1, _ := mget(r1, "value")
	if ok1.Tag != VTBool || ok1.Data.(bool) != true {
		t.Fatalf("r1.ok expected true, got %#v", ok1)
	}
	if val1.Tag != VTStr || val1.Data.(string) != "x" {
		t.Fatalf("r1.value expected 'x', got %#v", val1)
	}

	r2 := mustMap(t, m.Entries["r2"])
	ok2, _ := mget(r2, "ok")
	val2, _ := mget(r2, "value")
	if ok2.Tag != VTBool || ok2.Data.(bool) != true {
		t.Fatalf("r2.ok expected true (closed channel), got %#v", ok2)
	}
	wantAnnotatedContains(t, val2, "channel closed")
}

// 10) procJoinAny single element
func Test_Builtin_Concurrency_ProcJoinAny_Single(t *testing.T) {
	ip, _ := NewRuntime()
	v := evalWithIP(t, ip, `
let p = procSpawn(fun() -> Int do 123 end)
procJoinAny([p])
	`)
	if v.Tag != VTMap {
		t.Fatalf("procJoinAny should return {index:Int, value:Any}, got %#v", v)
	}
	m := v.Data.(*MapObject)
	idx, _ := mget(m, "index")
	val, _ := mget(m, "value")
	if idx.Tag != VTInt || idx.Data.(int64) != 0 {
		t.Fatalf("index should be 0, got %#v", idx)
	}
	if val.Tag != VTInt || val.Data.(int64) != 123 {
		t.Fatalf("value should be 123, got %#v", val)
	}
}
