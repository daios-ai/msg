package mindscript

import (
	"net"
	"os"
	"path/filepath"
	"strings"
	"testing"
	"time"
)

func newRT() *Interpreter { return NewRuntime() }

// Pick an ephemeral loopback TCP address like "127.0.0.1:54321"
func freeLocalAddr(t *testing.T) string {
	t.Helper()
	ln, err := net.Listen("tcp", "127.0.0.1:0")
	if err != nil {
		t.Fatalf("cannot pick free port: %v", err)
	}
	addr := ln.Addr().String()
	_ = ln.Close()
	return addr
}

func Test_RT_Builtins_isType_And_typeOf(t *testing.T) {
	ip := NewRuntime()

	// isType with explicit Type value
	v := evalWithIP(t, ip, `isType(42, type Int)`)
	if v.Tag != VTBool || v.Data.(bool) != true {
		t.Fatal("isType should return true")
	}

	// typeOf returns a Type value
	tt := evalWithIP(t, ip, `typeOf([1, 2.0, "x"])`)
	if tt.Tag != VTType {
		t.Fatalf("typeOf should return VTType, got %#v", tt)
	}

	// Passing a bare identifier (not a Type value) yields "undefined variable"
	bad := evalWithIP(t, ip, `isType(42, Int)`)
	wantAnnotatedContains(t, bad, "undefined variable")
}

func Test_RT_Builtins_import_FailurePath(t *testing.T) {
	ip := NewRuntime()

	// With no ModuleLoader configured, import should produce a clear annotated-null
	v := evalWithIP(t, ip, `import("does-not-exist")`)
	wantAnnotatedNullContains(t, v, "module not found")
}

// ---------------- Concurrency ----------------

func Test_RT_Spawn_Join_ClosureIsolation(t *testing.T) {
	ip := newRT()

	// capture a value into a zero-arg fun and spawn it
	fn := evalWithIP(t, ip, `
		let x = 41
		# adds one
		fun() do x + 1 end
	`)
	ip.Global.Define("f", fn)

	// spawn + join entirely in MindScript (no unused Go var)
	out := evalWithIP(t, ip, `
		let p = spawn(f)
		join(p)
	`)
	if out.Tag != VTInt || out.Data.(int64) != 42 {
		t.Fatalf("spawn/join wrong result: %#v", out)
	}

	// cancel smoke test (no observable effect yet)
	_ = evalWithIP(t, ip, `
		let p = spawn(f)
		cancel(p)
		join(p)
	`)

	// Channel ping-pong
	v := evalWithIP(t, ip, `
let c = chan()
let worker = fun() do
  let m = chanRecv(c)
  chanSend(c, m + " world")
  "ok"
end
let p = spawn(worker)
chanSend(c, "hello")
let r = chanRecv(c)
join(p)
r
	`)
	if v.Tag != VTStr || v.Data.(string) != "hello world" {
		t.Fatalf("channel ping-pong failed: %#v", v)
	}
}

func Test_RT_Channel_Close_RecvAfterClose(t *testing.T) {
	ip := newRT()
	v := evalWithIP(t, ip, `
		let c = chan()
		chanClose(c)
		chanRecv(c)        ## should yield annotated null "channel closed"
	`)
	wantAnnotatedContains(t, v, "channel closed")
}

// ---------------- Networking ----------------

func Test_RT_Net_Listen_Accept_Echo(t *testing.T) {
	ip := newRT()

	addr := freeLocalAddr(t)
	ip.Global.Define("ADDR", Str(addr))

	// Server and client both in MindScript, using primitives:
	// netListen, netAccept, netConnect, readLine, write, flush, close.
	v := evalWithIP(t, ip, `
		let ln = netListen(ADDR)

		let srv = fun() do
		  let c = netAccept(ln)
		  let s = readLine(c)
		  write(c, s + "-ok\n")
		  flush(c)
		  close(c)
		  close(ln)
		  "done"
		end

		let p = spawn(srv)

		let c = netConnect(ADDR)
		write(c, "hi\n")
		flush(c)
		let r = readLine(c)
		close(c)

		join(p)
		r
	`)
	if v.Tag != VTStr || v.Data.(string) != "hi-ok" {
		t.Fatalf("network echo failed: %#v", v)
	}
}

// ---------------- I/O ----------------

func Test_RT_IO_File_ReadWrite_ListDir(t *testing.T) {
	ip := newRT()

	dir := t.TempDir()
	p := filepath.Join(dir, "a.txt")
	q := filepath.Join(dir, "b.txt")

	// writeFile + readFile
	ip.Global.Define("pathA", Str(p))
	ip.Global.Define("pathB", Str(q))

	w := evalWithIP(t, ip, `writeFile(pathA, "line1\nline2")`)
	if w.Tag != VTNull {
		t.Fatalf("writeFile expected null, got %#v", w)
	}
	all := evalWithIP(t, ip, `readFile(pathA)`)
	if all.Tag != VTStr || !strings.Contains(all.Data.(string), "line2") {
		t.Fatalf("readFile wrong content: %#v", all)
	}

	// open/readLine/write/flush/close
	v := evalWithIP(t, ip, `
		let f = open(pathB, "w")
		write(f, "A\nB\n")
		flush(f)
		close(f)
		let r = open(pathB, "r")
		let a = readLine(r)
		let b = readLine(r)
		let c = readLine(r)   # null at EOF
		close(r)
		{a:a, b:b, c:c}
	`)
	m := v.Data.(map[string]Value)
	if m["a"].Data.(string) != "A" || m["b"].Data.(string) != "B" || m["c"].Tag != VTNull {
		t.Fatalf("readLine sequence unexpected: %#v", v)
	}

	// listDir
	_ = os.WriteFile(filepath.Join(dir, "z.dat"), []byte("x"), 0o644)
	ip.Global.Define("dir", Str(dir))
	ls := evalWithIP(t, ip, `listDir(dir)`)
	if ls.Tag != VTArray || len(ls.Data.([]Value)) < 2 {
		t.Fatalf("listDir expected >=2 entries, got %#v", ls)
	}
}

func Test_RT_IO_ReadN_ReadAll(t *testing.T) {
	ip := newRT()
	p := filepath.Join(t.TempDir(), "buf.txt")
	_ = os.WriteFile(p, []byte("abcdef"), 0o644)
	ip.Global.Define("p", Str(p))

	v := evalWithIP(t, ip, `
		let r = open(p, "r")
		let x = readN(r, 3)
		let y = readAll(r)
		close(r)
		{x:x, y:y}
	`)
	m := v.Data.(map[string]Value)
	if m["x"].Data.(string) != "abc" || m["y"].Data.(string) != "def" {
		t.Fatalf("readN/readAll mismatch: %#v", v)
	}
}

// ---------------- Introspection & Docs ----------------

func Test_RT_Introspection_funInfo_funType_TypeUtils_Docs(t *testing.T) {
	ip := newRT()

	// Annotated function with param types & return type
	fn := evalWithIP(t, ip, `
# Adds one to an integer
# This is a longer description.
fun(x:Int) -> Int do x + 1 end
`)
	ip.Global.Define("f", fn)

	info := evalWithIP(t, ip, `funInfo(f)`)
	if info.Tag != VTMap {
		t.Fatalf("funInfo should return map, got %#v", info)
	}
	// params[0].name == "x"
	nm := evalWithIP(t, ip, `funInfo(f).params[0].name`)
	if nm.Tag != VTStr || nm.Data.(string) != "x" {
		t.Fatalf("funInfo param name wrong: %#v", nm)
	}
	// doc/docline
	d1 := evalWithIP(t, ip, `doc(f)`)
	if d1.Tag != VTStr || !strings.Contains(strings.ToLower(d1.Data.(string)), "adds one") {
		t.Fatalf("doc first line wrong: %#v", d1)
	}
	help := evalWithIP(t, ip, `help(f)`)
	if help.Tag != VTStr || !strings.Contains(help.Data.(string), "longer") {
		t.Fatalf("help expected full docstring: %#v", help)
	}

	// funType roundtrip (A->B)
	ft := evalWithIP(t, ip, `funType(f)`)
	if ft.Tag != VTType {
		t.Fatalf("funType should return Type, got %#v", ft)
	}

	// Type utils
	tmap := evalWithIP(t, ip, `type {name!: Str, age: Int?}`)
	ip.Global.Define("T", tmap)
	fields := evalWithIP(t, ip, `typeFields(T)`)
	if fields.Tag != VTArray || len(fields.Data.([]Value)) != 2 {
		t.Fatalf("typeFields count wrong: %#v", fields)
	}
	isN := evalWithIP(t, ip, `isNullable(type Int?)`)
	if isN.Tag != VTBool || !isN.Data.(bool) {
		t.Fatalf("isNullable failed: %#v", isN)
	}
	bt := evalWithIP(t, ip, `baseType(type Int?)`)
	if bt.Tag != VTType || bt.Data.(S)[0].(string) != "id" || bt.Data.(S)[1].(string) != "Int" {
		t.Fatalf("baseType wrong: %#v", bt)
	}
	ae := evalWithIP(t, ip, `arrayElemType(type [Str])`)
	if ae.Tag != VTType || ae.Data.(S)[1].(string) != "Str" {
		t.Fatalf("arrayElemType wrong: %#v", ae)
	}
	teq := evalWithIP(t, ip, `typeEquals(type Int, type Int)`)
	if teq.Tag != VTBool || !teq.Data.(bool) {
		t.Fatalf("typeEquals false negative: %#v", teq)
	}
}

// ---------------- Utilities: time/rand/json ----------------

func Test_RT_Utilities_Time_Rand_JSON(t *testing.T) {
	ip := newRT()

	// nowMillis & sleep: ensure time advances
	t0 := evalWithIP(t, ip, `nowMillis()`)
	time.Sleep(20 * time.Millisecond)
	t1 := evalWithIP(t, ip, `nowMillis()`)
	if !(t0.Tag == VTInt && t1.Tag == VTInt && t1.Data.(int64) >= t0.Data.(int64)) {
		t.Fatalf("nowMillis non-monotonic: %#v, %#v", t0, t1)
	}

	// seeded randomness
	v := evalWithIP(t, ip, `
		seedRand(123)
		{a: randInt(10), b: randFloat()}
	`)
	m := v.Data.(map[string]Value)
	if m["a"].Tag != VTInt || m["a"].Data.(int64) < 0 || m["a"].Data.(int64) >= 10 {
		t.Fatalf("randInt out of range: %#v", m["a"])
	}
	if m["b"].Tag != VTNum || m["b"].Data.(float64) < 0.0 || m["b"].Data.(float64) >= 1.0 {
		t.Fatalf("randFloat out of range: %#v", m["b"])
	}

	// JSON round-trip
	j := evalWithIP(t, ip, `
		let obj = {name:"Ada", age: 37, tags: ["a","b"], ok: true, misc: null}
		let s = jsonStringify(obj)
		let back = jsonParse(s)
		back.name
	`)
	if j.Tag != VTStr || j.Data.(string) != "Ada" {
		t.Fatalf("json roundtrip failed, got %#v", j)
	}
}
