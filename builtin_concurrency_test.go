// === FILE: std_sys_concurrency_test.go ===
package mindscript

import (
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

func Test_Builtin_Concurrency_SendOnClosed_HardError(t *testing.T) {
	ip, _ := NewRuntime()

	// chanTrySend after close must be a hard error (send on closed).
	_, err := ip.EvalSource(`
let c = chanOpen()
chanClose(c)
chanTrySend(c, 1)
	`)
	wantErr(t, err)

	// chanSend after close must be a hard error as well.
	_, err = ip.EvalSource(`
let c = chanOpen()
chanClose(c)
chanSend(c, 1)
	`)
	wantErr(t, err)
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
