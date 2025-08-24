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

func entriesOf(t *testing.T, v Value) map[string]Value {
	t.Helper()
	mo, ok := v.Data.(*MapObject)
	if !ok {
		t.Fatalf("expected VTMap/*MapObject, got %#v", v)
	}
	return mo.Entries
}

// Small helpers for reading MindScript map values in tests.
func mustMap(t *testing.T, v Value) *MapObject {
	t.Helper()
	if v.Tag != VTMap {
		t.Fatalf("expected map; got %v", v.Tag)
	}
	return v.Data.(*MapObject)
}
func mget(m *MapObject, k string) (Value, bool) { v, ok := m.Entries[k]; return v, ok }

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
		let p = procSpawn(f)
		procJoin(p)
	`)
	if out.Tag != VTInt || out.Data.(int64) != 42 {
		t.Fatalf("procSpawn/procJoin wrong result: %#v", out)
	}

	// cancel smoke test (no observable effect yet)
	_ = evalWithIP(t, ip, `
		let p = procSpawn(f)
		procCancel(p)
		procJoin(p)
	`)

	// Channel ping-pong
	v := evalWithIP(t, ip, `
let c = chan()
let worker = fun() do
  let m = chanRecv(c)
  chanSend(c, m + " world")
  "ok"
end
let p = procSpawn(worker)
chanSend(c, "hello")
let r = chanRecv(c)
procJoin(p)
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
		chanRecv(c)
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

		let p = procSpawn(srv)

		let c = netConnect(ADDR)
		write(c, "hi\n")
		flush(c)
		let r = readLine(c)
		close(c)

		procJoin(p)
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
	m := entriesOf(t, v)
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
	m := entriesOf(t, v)
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
	eqBase := evalWithIP(t, ip, `typeEquals(baseType(type Int?), type Int)`)
	if eqBase.Tag != VTBool || !eqBase.Data.(bool) {
		t.Fatalf("baseType wrong: %#v", eqBase)
	}
	eqElem := evalWithIP(t, ip, `typeEquals(arrayElemType(type [Str]), type Str)`)
	if eqElem.Tag != VTBool || !eqElem.Data.(bool) {
		t.Fatalf("arrayElemType wrong: %#v", eqElem)
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
	m := entriesOf(t, v)
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

// ---------------- std_core: types & import/env/map ----------------

// Variance: (Num->Int) <: (Int->Num) is actually TRUE
func Test_RT_IsSubtype_Variance_And_Basics(t *testing.T) {
	ip := newRT()

	// (Num -> Int) <: (Int -> Num)  — params contravariant, returns covariant → true
	v := evalWithIP(t, ip, `isSubtype(type (Num -> Int), type (Int -> Num))`)
	if v.Tag != VTBool || v.Data.(bool) != true {
		t.Fatalf("(Num->Int) <: (Int->Num) should be true, got %#v", v)
	}

	// A <: A is true
	self := evalWithIP(t, ip, `isSubtype(type Int, type Int)`)
	if self.Tag != VTBool || !self.Data.(bool) {
		t.Fatalf("reflexivity failed: %#v", self)
	}
}

// importCode: success + parse error + runtime error
func Test_RT_ImportCode_Success_ParseError_RuntimeError(t *testing.T) {
	ip := newRT()

	// success: returns a Module with an exported binding
	mod := evalWithIP(t, ip, `
		importCode("mok", "
			let inc = fun(x:Int) -> Int do x + 1 end
		")
	`)
	if mod.Tag != VTModule {
		t.Fatalf("importCode success should return Module, got %#v", mod)
	}
	// Call the exported function to be sure it compiled
	ip.Global.Define("M", mod)
	out := evalWithIP(t, ip, `M.inc(41)`)
	if out.Tag != VTInt || out.Data.(int64) != 42 {
		t.Fatalf("imported function wrong result: %#v", out)
	}

	// parse error: bad syntax inside the source string
	perr := evalWithIP(t, ip, `importCode("bad", "fun x:Int -> Int do x end")`)
	wantAnnotatedContains(t, perr, "parse error in mem:bad")

	// runtime error: reference to undefined variable in module code
	rterr := evalWithIP(t, ip, `importCode("boom", "y + 1")`)
	wantAnnotatedContains(t, rterr, "runtime error in mem:boom")
}

func Test_RT_Import_TypeChecks_And_StringPath(t *testing.T) {
	ip := newRT()

	// wrong type for 'path'
	v := evalWithIP(t, ip, `import(42)`)
	wantAnnotatedContains(t, v, "type mismatch in parameter 'path'")

	// ok path, but module not found should annotate clearly
	v2 := evalWithIP(t, ip, `import("nope")`)
	wantAnnotatedNullContains(t, v2, "module not found")
}

func Test_RT_GetEnv_Shadows_And_Order(t *testing.T) {
	ip := newRT()
	out := evalWithIP(t, ip, `
      let x = "outer"
      do
        let x = "inner"
        let y = 1
        let m = getEnv()
        {a: m["x"], b: m["y"]}
      end
    `)
	m := entriesOf(t, out)
	if m["a"].Tag != VTStr || m["a"].Data.(string) != "inner" {
		t.Fatalf("getEnv shadowing failed: %#v", out)
	}
	if m["b"].Tag != VTInt || m["b"].Data.(int64) != 1 {
		t.Fatalf("getEnv missing y: %#v", out)
	}
}

func Test_RT_MapHas_And_MapDelete_Order_And_Ann(t *testing.T) {
	ip := newRT()
	v := evalWithIP(t, ip, `
      let m = {#(first) a:1, b:2, c:3}
      let _ = mapDelete(m, "b")
      {h1: mapHas(m,"b"), h2: mapHas(m,"z"), pairs: __collect_for_elems(m)}
	`)
	// {h1:false, h2:false, pairs:[[k,v]...]} with keys in order a,c
	m := entriesOf(t, v)
	if m["h1"].Tag != VTBool || m["h1"].Data.(bool) != false {
		t.Fatalf("expected h1 = false, got %#v", m["h1"])
	}
	if m["h2"].Tag != VTBool || m["h2"].Data.(bool) != false {
		t.Fatalf("expected h2 = false, got %#v", m["h2"])
	}
	pairs := m["pairs"]
	if pairs.Tag != VTArray {
		t.Fatalf("pairs not array: %#v", pairs)
	}
	xs := pairs.Data.([]Value)
	if len(xs) != 2 {
		t.Fatalf("expected 2 pairs after delete, got %d", len(xs))
	}
	k0 := xs[0].Data.([]Value)[0] // first key
	k1 := xs[1].Data.([]Value)[0] // second key
	if k0.Tag != VTStr || k0.Data.(string) != "a" || k1.Tag != VTStr || k1.Data.(string) != "c" {
		t.Fatalf("order not preserved a,c: %#v", pairs)
	}
	// key annotation on "a" should be present (doc on key shows up as Annot on the Str key)
	if k0.Annot == "" {
		t.Fatalf("expected annotation on key 'a'")
	}
}

// ---------------- std_io_net: file/net edge cases ----------------

func Test_RT_IO_Open_Modes_And_WriteCount(t *testing.T) {
	ip := newRT()
	path := filepath.Join(t.TempDir(), "w.txt")
	ip.Global.Define("p", Str(path))

	v := evalWithIP(t, ip, `
      let f = open(p, "w")
      let n = write(f, "A")
      flush(f)
      close(f)
      n
    `)
	if v.Tag != VTInt || v.Data.(int64) != 1 {
		t.Fatalf("write should return 1 byte, got %#v", v)
	}
}

func Test_RT_IO_ReadWrite_Permissions_And_Errors(t *testing.T) {
	ip := newRT()
	path := filepath.Join(t.TempDir(), "g.txt")
	ip.Global.Define("p", Str(path))

	_ = evalWithIP(t, ip, `writeFile(p, "")`)

	// Not writable: open "r" then write should annotate
	v1 := evalWithIP(t, ip, `
      let f = open(p, "r")
      write(f, "x")
    `)
	wantAnnotatedContains(t, v1, "not writable")

	// Not readable: open "w" then readLine should annotate
	v2 := evalWithIP(t, ip, `
      let f = open(p, "w")
      readLine(f)
    `)
	wantAnnotatedContains(t, v2, "not readable")

	// readN with n=0 → empty string
	v3 := evalWithIP(t, ip, `
      let f = open(p, "r")
      let s = readN(f, 0)
      close(f)
      s
    `)
	if v3.Tag != VTStr || v3.Data.(string) != "" {
		t.Fatalf("readN(0) should return empty string, got %#v", v3)
	}

	// Direct Go: unsupported handle for close()
	uh := HandleVal("weird", nil)
	ip.Global.Define("H", uh)
	cv := evalWithIP(t, ip, `close(H)`)
	wantAnnotatedContains(t, cv, "unsupported handle for close")
}

func Test_RT_Net_Error_Paths(t *testing.T) {
	ip := newRT()
	// Connect to an unused local port
	addr := freeLocalAddr(t) // frees immediately; likely unused
	ip.Global.Define("ADDR", Str(addr))
	v := evalWithIP(t, ip, `netConnect(ADDR)`)
	wantAnnotatedContains(t, v, "connect") // message contains "connect" or similar
}

// ---------------- std_sys: clone, cast, strings, math, date, cancel ----------------

func Test_RT_Clone_DeepCopy_Arrays_And_Maps(t *testing.T) {
	ip := newRT()
	// Build a map with an annotated key and a small array; clone both and mutate originals
	v := evalWithIP(t, ip, `
      let m = {#(first) a:1, b:2}
      let a = [10, 20]
      let mc = clone(m)
      let ac = clone(a)
      m.a = 99
      a[0] = 77
      {mc: mc, ac: ac, mcPairs: __collect_for_elems(mc)}
    `)

	m := entriesOf(t, v)
	// cloned array not affected by mutation of original
	ac := m["ac"]
	if ac.Tag != VTArray || len(ac.Data.([]Value)) != 2 || ac.Data.([]Value)[0].Data.(int64) != 10 {
		t.Fatalf("array clone not independent: %#v", ac)
	}

	// cloned map still has a:1, b:2 and preserves order a,b; "a" key annotated
	mc := m["mc"]
	if mc.Tag != VTMap {
		t.Fatalf("mc not a map: %#v", mc)
	}
	pairs := m["mcPairs"]
	xs := pairs.Data.([]Value)
	if len(xs) != 2 {
		t.Fatalf("mcPairs wrong length: %#v", pairs)
	}
	k0 := xs[0].Data.([]Value)[0]
	v0 := xs[0].Data.([]Value)[1]
	k1 := xs[1].Data.([]Value)[0]
	v1 := xs[1].Data.([]Value)[1]
	if k0.Tag != VTStr || k0.Data.(string) != "a" || v0.Data.(int64) != 1 || k1.Data.(string) != "b" || v1.Data.(int64) != 2 {
		t.Fatalf("map clone content/order wrong: %#v", pairs)
	}
	if k0.Annot == "" {
		t.Fatalf("expected annotation on cloned key 'a'")
	}
}
func Test_RT_Sleep_Increases_NowMillis(t *testing.T) {
	ip := newRT()
	v := evalWithIP(t, ip, `
      let t0 = nowMillis()
      sleep(5)
      let t1 = nowMillis()
      t1 - t0
    `)
	if v.Tag != VTInt || v.Data.(int64) < 0 {
		t.Fatalf("sleep/nowMillis unexpected: %#v", v)
	}
}

func Test_RT_Cast_Str_Int_Num_Bool(t *testing.T) {
	ip := newRT()

	// str on arrays/maps → JSON
	s := evalWithIP(t, ip, `str({x:1, y:[2,3]})`)
	if s.Tag != VTStr || !strings.Contains(s.Data.(string), `"x":`) {
		t.Fatalf("str(map) should be JSON-ish, got %#v", s)
	}

	// int conversions
	i1 := evalWithIP(t, ip, `int(3.9)`)
	if i1.Tag != VTInt || i1.Data.(int64) != 3 {
		t.Fatalf("int(3.9) truncation failed: %#v", i1)
	}
	i2 := evalWithIP(t, ip, `int("xyz")`)
	if i2.Tag != VTNull {
		t.Fatalf("int(\"xyz\") should be null, got %#v", i2)
	}

	// num conversions
	n1 := evalWithIP(t, ip, `num(7)`)
	if n1.Tag != VTNum || n1.Data.(float64) != 7.0 {
		t.Fatalf("num(7) failed: %#v", n1)
	}
	n2 := evalWithIP(t, ip, `num("oops")`)
	if n2.Tag != VTNull {
		t.Fatalf("num(\"oops\") should be null, got %#v", n2)
	}

	// bool truthiness
	b1 := evalWithIP(t, ip, `bool([])`)
	if b1.Tag != VTBool || b1.Data.(bool) {
		t.Fatalf("bool([]) should be false, got %#v", b1)
	}
	b2 := evalWithIP(t, ip, `bool({})`)
	if b2.Tag != VTBool || b2.Data.(bool) {
		t.Fatalf("bool({}) should be false, got %#v", b2)
	}
	b3 := evalWithIP(t, ip, `bool("hi")`)
	if b3.Tag != VTBool || !b3.Data.(bool) {
		t.Fatalf("bool(\"hi\") should be true, got %#v", b3)
	}
}

func Test_RT_String_Utilities(t *testing.T) {
	ip := newRT()
	out := evalWithIP(t, ip, `
      {
        sub: substr("héllo", 1, 4),
        lo:  toLower("HeLLo"),
        up:  toUpper("HeLLo"),
        st:  strip("  x  "),
        ls:  lstrip("  x  "),
        rs:  rstrip("x  "),
        sp:  split("a,b,c", ","),
        jn:  join(["a","b","c"], "-")
      }
    `)
	m := entriesOf(t, out)
	if m["sub"].Data.(string) != "éll" {
		t.Fatalf("substr failed: %#v", m["sub"])
	}
	if m["lo"].Data.(string) != "hello" || m["up"].Data.(string) != "HELLO" {
		t.Fatalf("case conversions failed: lo=%#v up=%#v", m["lo"], m["up"])
	}
	if m["st"].Data.(string) != "x" || m["ls"].Data.(string) != "x  " || m["rs"].Data.(string) != "x" {
		t.Fatalf("strip/lstrip/rstrip failed: st=%#v ls=%#v rs=%#v", m["st"], m["ls"], m["rs"])
	}
	if sp := m["sp"]; sp.Tag != VTArray || len(sp.Data.([]Value)) != 3 {
		t.Fatalf("split failed: %#v", sp)
	}
	if m["jn"].Data.(string) != "a-b-c" {
		t.Fatalf("join failed: %#v", m["jn"])
	}
}

func Test_RT_Math_And_Constants(t *testing.T) {
	ip := newRT()
	v := evalWithIP(t, ip, `
      { s: sin(0.0), c: cos(0.0), e: exp(0.0), p: pow(2.0,3.0), pi: PI, ee: E }
    `)
	m := entriesOf(t, v)
	if m["s"].Tag != VTNum || m["c"].Tag != VTNum || m["e"].Tag != VTNum || m["p"].Tag != VTNum {
		t.Fatalf("math funcs types wrong: %#v", v)
	}
	if m["e"].Data.(float64) != 1.0 || m["p"].Data.(float64) != 8.0 {
		t.Fatalf("exp/pow values wrong: %#v", v)
	}
	if m["pi"].Tag != VTNum || m["ee"].Tag != VTNum {
		t.Fatalf("constants missing: %#v", v)
	}
}

func Test_RT_DateNow_Structure(t *testing.T) {
	ip := newRT()
	v := evalWithIP(t, ip, `dateNow()`)
	if v.Tag != VTMap {
		t.Fatalf("dateNow should return map, got %#v", v)
	}
	m := v.Data.(*MapObject).Entries
	for _, k := range []string{"year", "month", "day", "hour", "minute", "second", "millisecond"} {
		if _, ok := m[k]; !ok {
			t.Fatalf("dateNow missing key %q", k)
		}
	}
}

func Test_RT_ProcCancel_Smoke(t *testing.T) {
	ip := newRT()

	fn := evalWithIP(t, ip, `fun() do 0 end`)
	ip.Global.Define("f", fn)

	v := evalWithIP(t, ip, `
      let p = procSpawn(f)
      procCancel(p)
      procJoin(p)
    `)
	if v.Tag != VTInt || v.Data.(int64) != 0 {
		t.Fatalf("procCancel/procJoin smoke failed: %#v", v)
	}
}

func Test_RT_IO_Open_RW_Mode_ReadAfterWrite(t *testing.T) {
	ip := newRT()
	path := filepath.Join(t.TempDir(), "rw.txt")
	ip.Global.Define("p", Str(path))

	v := evalWithIP(t, ip, `
      let f = open(p, "rw")
      write(f, "XY")
      flush(f)
      let r = readN(f, 2)  # position is at end; readN should return ""
      close(f)
      r
    `)
	if v.Tag != VTStr || v.Data.(string) != "" {
		t.Fatalf("expected empty because cursor at end after write; got %#v", v)
	}
}

// ---------- error(message) ----------

func Test_RT_Error_Raises_RuntimeError(t *testing.T) {
	ip := newRT()

	v := evalWithIP(t, ip, `error("boom")`)
	wantAnnotatedContains(t, v, "boom")

	// Optional: null/empty message still raises a hard error (message can be empty)
	v2 := evalWithIP(t, ip, `error(null)`)
	if v2.Tag != VTNull || v2.Annot == "" {
		t.Fatalf("error(null) should still be an annotated null, got %#v", v2)
	}
}

// ---------- sprintf / printf ----------

func Test_RT_Sprintf_Basics_And_Literals(t *testing.T) {
	ip := newRT()

	// %s and %d basics; args passed as an array
	v := evalWithIP(t, ip, `
		let s = sprintf("hi %s %d", ["x", 3])
		s
	`)
	if v.Tag != VTStr || v.Data.(string) != "hi x 3" {
		t.Fatalf("sprintf basic failed, got %#v", v)
	}

	// literal %%
	v2 := evalWithIP(t, ip, `
		let s = sprintf("rate: 100%% ok", [])
		s
	`)
	if v2.Tag != VTStr || v2.Data.(string) != "rate: 100% ok" {
		t.Fatalf("sprintf %% literal failed, got %#v", v2)
	}
}

func Test_RT_Printf_Returns_Printed_Value(t *testing.T) {
	ip := newRT()

	// Basic %s / %d formatting with array args
	v := evalWithIP(t, ip, `printf("%s-%d", ["hi", 7])`)
	if v.Tag != VTStr || v.Data.(string) != "hi-7" {
		t.Fatalf("printf should return formatted string, got %#v", v)
	}

	// Float formatting (keeps it simple with %v to avoid precision surprises)
	v2 := evalWithIP(t, ip, `printf("%v/%v", [3, 2.5])`)
	if v2.Tag != VTStr || v2.Data.(string) != "3/2.5" {
		t.Fatalf("printf formatting mismatch, got %#v", v2)
	}
}
func Test_RT_Size_On_Arr_Map_Str_And_Other(t *testing.T) {
	ip := newRT()

	// array
	vArr := evalWithIP(t, ip, `
		let a = [1,2,3]
		size(a)
	`)
	if vArr.Tag != VTInt || vArr.Data.(int64) != 3 {
		t.Fatalf("size([..]) wrong, got %#v", vArr)
	}

	// map
	vMap := evalWithIP(t, ip, `
		let m = {"a":1, "b":2}
		size(m)
	`)
	if vMap.Tag != VTInt || vMap.Data.(int64) != 2 {
		t.Fatalf("size({..}) wrong, got %#v", vMap)
	}

	// string
	vStr := evalWithIP(t, ip, `
		size("test")
	`)
	if vStr.Tag != VTInt || vStr.Data.(int64) != 4 {
		t.Fatalf(`size("test") wrong, got %#v`, vStr)
	}

	// other → null
	vOther := evalWithIP(t, ip, `
		size(123)
	`)
	if vOther.Tag != VTNull {
		t.Fatalf("size(other) should be null, got %#v", vOther)
	}
}

func Test_RT_Regex_Match_And_Replace(t *testing.T) {
	ip := newRT()

	// match → [Str]
	vm := evalWithIP(t, ip, `
		match("[a-z]+", "a12bc3")
	`)
	if vm.Tag != VTArray {
		t.Fatalf("match should return array, got %#v", vm)
	}
	xs := vm.Data.([]Value)
	if len(xs) != 2 || xs[0].Tag != VTStr || xs[1].Tag != VTStr || xs[0].Data.(string) != "a" || xs[1].Data.(string) != "bc" {
		t.Fatalf("match results wrong: %#v", vm)
	}

	// replace
	vr := evalWithIP(t, ip, `
		replace("[0-9]+", "#", "abc123def45")
	`)
	if vr.Tag != VTStr || vr.Data.(string) != "abc#def#" {
		t.Fatalf("replace wrong, got %#v", vr)
	}
}

func Test_RT_Sprintf_Does_Not_Interfere_With_IO(t *testing.T) {
	ip := newRT()

	path := filepath.Join(t.TempDir(), "out.txt")
	ip.Global.Define("p", Str(path))

	v := evalWithIP(t, ip, `
		let s = sprintf("X=%d", [5])
		let f = open(p, "w")
		write(f, s)
		flush(f)
		close(f)
		readFile(p)
	`)
	if v.Tag != VTStr || strings.TrimSpace(v.Data.(string)) != "X=5" {
		t.Fatalf("sprintf+IO failed, got %#v", v)
	}
}
func Test_RT_Import_Retry_After_Parse_Error(t *testing.T) {
	ip := newRT()

	dir := t.TempDir()
	path := filepath.Join(dir, "mod.ms")
	ip.Global.Define("P", Str(path))

	// 1) Write a parse-broken module, import should fail with annotated null
	if err := os.WriteFile(path, []byte("let x = \n"), 0o644); err != nil {
		t.Fatal(err)
	}
	v1 := evalWithIP(t, ip, `
		import(P)
	`)
	wantAnnotatedNullContains(t, v1, "parse")

	// 2) Fix the file and re-import; should succeed (cache must NOT be poisoned)
	if err := os.WriteFile(path, []byte("let answer = 42\n"), 0o644); err != nil {
		t.Fatal(err)
	}
	v2 := evalWithIP(t, ip, `
		import(P).answer
	`)
	if v2.Tag != VTInt || v2.Data.(int64) != 42 {
		t.Fatalf("import retry (parse fix) failed, got %#v", v2)
	}
}

func Test_RT_Import_Retry_After_Runtime_Error(t *testing.T) {
	ip := newRT()

	dir := t.TempDir()
	path := filepath.Join(dir, "mod.ms")
	ip.Global.Define("P", Str(path))

	// 1) Module parses but fails at runtime (division by zero)
	if err := os.WriteFile(path, []byte("let bad = 1 / 0\n"), 0o644); err != nil {
		t.Fatal(err)
	}
	v1 := evalWithIP(t, ip, `
		import(P)
	`)
	// The exact wording may vary; "division by zero" is VM's message.
	wantAnnotatedNullContains(t, v1, "division by zero")

	// 2) Fix the module and re-import; should succeed
	if err := os.WriteFile(path, []byte("let answer = 7\n"), 0o644); err != nil {
		t.Fatal(err)
	}
	v2 := evalWithIP(t, ip, `
		import(P).answer
	`)
	if v2.Tag != VTInt || v2.Data.(int64) != 7 {
		t.Fatalf("import retry (runtime fix) failed, got %#v", v2)
	}
}

// ---- Runtime smoke: builtins are wired and preserve key behaviors ----

// Builtin: typeToJSONSchema — honors value-level and key-level descriptions + required
func Test_RT_TypeToJSONSchema_DescriptionsAndRequired(t *testing.T) {
	ip := NewRuntime()

	src := `
		let T = #(Type describing a person) type {
			#(the name) name!: Str,
			#(availability) avail: Enum["yes","no"]
		}
		typeToJSONSchema(T)
	`
	out, err := ip.EvalSource(src)
	if err != nil {
		t.Fatalf("Eval error: %v", err)
	}

	root := mustMap(t, out)

	// Root description from Value-level annotation
	if desc := root.Entries["description"]; desc.Tag != VTStr || desc.Data.(string) != "Type describing a person" {
		t.Fatalf("root description mismatch: %v", desc)
	}

	propsV, ok := mget(root, "properties")
	if !ok {
		t.Fatalf("properties missing")
	}
	props := mustMap(t, propsV)

	// name property
	nameV, ok := mget(props, "name")
	if !ok {
		t.Fatalf("name property missing")
	}
	name := mustMap(t, nameV)

	if typ, ok := mget(name, "type"); !ok || typ.Tag != VTStr || typ.Data.(string) != "string" {
		t.Fatalf("name.type mismatch: %v", typ)
	}
	if d, ok := mget(name, "description"); !ok || d.Tag != VTStr || d.Data.(string) != "the name" {
		t.Fatalf("name.description mismatch: %v", d)
	}

	// avail property
	availV, ok := mget(props, "avail")
	if !ok {
		t.Fatalf("avail property missing")
	}
	avail := mustMap(t, availV)
	if d, ok := mget(avail, "description"); !ok || d.Tag != VTStr || d.Data.(string) != "availability" {
		t.Fatalf("avail.description mismatch: %v", d)
	}
	if ev, ok := mget(avail, "enum"); !ok || ev.Tag != VTArray {
		t.Fatalf("avail.enum missing or wrong type: %v", ev)
	}

	// required contains "name"
	reqV, ok := mget(root, "required")
	if !ok || reqV.Tag != VTArray {
		t.Fatalf("required missing or wrong type: %v", reqV)
	}
	found := false
	for _, it := range reqV.Data.([]Value) {
		if it.Tag == VTStr && it.Data.(string) == "name" {
			found = true
			break
		}
	}
	if !found {
		t.Fatalf(`"name" not found in required: %v`, reqV)
	}
}

// Builtin: typeStringToJSONSchema — end-to-end parse→convert
func Test_RT_TypeStringToJSONSchema_Convenience(t *testing.T) {
	ip := NewRuntime()

	src := `
		typeStringToJSONSchema("# the doc\n{ name!: Str, age: Int? }")
	`
	out, err := ip.EvalSource(src)
	if err != nil {
		t.Fatalf("Eval error: %v", err)
	}
	root := mustMap(t, out)

	// Root description preserved
	if d, ok := mget(root, "description"); !ok || d.Tag != VTStr || d.Data.(string) != "the doc" {
		t.Fatalf("root description mismatch: %v", d)
	}

	props := mustMap(t, root.Entries["properties"])
	// name is required string
	name := mustMap(t, props.Entries["name"])
	if typ := name.Entries["type"]; typ.Tag != VTStr || typ.Data.(string) != "string" {
		t.Fatalf("name.type mismatch: %v", typ)
	}
	// age is nullable integer → allow either anyOf or ["integer","null"]
	age := mustMap(t, props.Entries["age"])
	if _, ok := age.Entries["anyOf"]; !ok {
		if tv, ok2 := age.Entries["type"]; !(ok2 && tv.Tag == VTArray) {
			t.Fatalf("age should be nullable pattern; got %v", age)
		}
	}
}

// Builtin: jsonSchemaStringToType — end-to-end parse→convert
func Test_RT_JSONSchemaStringToType_Convenience(t *testing.T) {
	ip := NewRuntime()
	out, err := ip.EvalSource(`jsonSchemaStringToType("{\"type\":\"array\",\"items\":{\"type\":\"number\"}}")`)
	if err != nil {
		t.Fatalf("Eval error: %v", err)
	}
	if out.Tag != VTType {
		t.Fatalf("expected Type; got %v", out.Tag)
	}
	if got := strings.TrimSpace(FormatValue(out)); got != "[Num]" {
		t.Fatalf("expected [Num]; got %q", got)
	}
}

// Builtin: typeToJSONSchema — aliases produce $defs + $ref
func Test_RT_TypeToJSONSchema_WithAliasAndDefs(t *testing.T) {
	ip := NewRuntime()

	// Person references itself via a nullable field, forcing $defs + $ref.
	src := `
		let Person = type {
			name!: Str,
			friend: Person?
		}
		typeToJSONSchema(Person)
	`
	out, err := ip.EvalSource(src)
	if err != nil {
		t.Fatalf("Eval error: %v", err)
	}
	root := mustMap(t, out)

	// Expect $defs.Person and properties.friend referencing it with nullability
	defsV, ok := mget(root, "$defs")
	if !ok {
		t.Fatalf("$defs missing")
	}
	defs := mustMap(t, defsV)
	if _, ok := defs.Entries["Person"]; !ok {
		t.Fatalf("$defs.Person missing")
	}

	props := mustMap(t, root.Entries["properties"])
	friend := mustMap(t, props.Entries["friend"])

	// Should be anyOf: [{$ref...}, {"type":"null"}]
	anyOf, ok := friend.Entries["anyOf"]
	if !ok || anyOf.Tag != VTArray {
		t.Fatalf("friend should be nullable anyOf with $ref: %v", friend)
	}
	branches := anyOf.Data.([]Value)
	if len(branches) != 2 {
		t.Fatalf("expected two anyOf branches; got %d", len(branches))
	}
	refObj := mustMap(t, branches[0])
	if ref, ok := refObj.Entries["$ref"]; !ok || ref.Tag != VTStr || ref.Data.(string) == "" {
		t.Fatalf("missing $ref in first anyOf branch: %v", refObj)
	}
}

// Roundtrip: JSON with $defs/$ref -> Type -> JSON, preserve $defs + description
func Test_RT_Roundtrip_JSON_WithDefsAndRefs(t *testing.T) {
	ip := NewRuntime()

	src := `
		let schema = {
			description: "root doc",
			type: "object",
			properties: {
				person: {
					anyOf: [
						{ "$ref": "#/$defs/Person" },
						{ type: "null" }
					]
				}
			},
			"$defs": {
				Person: {
					type: "object",
					properties: { name: { type: "string" } },
					required: ["name"]
				}
			}
		}
		let T = jsonSchemaToType(schema)
		typeToJSONSchema(T)
	`
	out, err := ip.EvalSource(src)
	if err != nil {
		t.Fatalf("Eval error: %v", err)
	}
	root := mustMap(t, out)

	// root description should survive
	if d, ok := mget(root, "description"); !ok || d.Tag != VTStr || d.Data.(string) != "root doc" {
		t.Fatalf("root description mismatch: %v", d)
	}

	// $defs.Person present after roundtrip
	defs, ok := mget(root, "$defs")
	if !ok || defs.Tag != VTMap {
		t.Fatalf("$defs missing after roundtrip")
	}
	if _, ok := defs.Data.(*MapObject).Entries["Person"]; !ok {
		t.Fatalf("$defs.Person missing after roundtrip")
	}
}

// Error paths: bad inputs handled by builtins

func Test_RT_typeStringToJSONSchema_BadType_YieldsAnnotatedNull(t *testing.T) {
	ip := NewRuntime()

	// Intentionally malformed type text (unterminated object)
	src := `typeStringToJSONSchema("{ name!: Str, ")`
	out, err := ip.EvalSource(src)
	if err != nil {
		t.Fatalf("Eval error: %v", err)
	}
	if out.Tag != VTNull || out.Annot == "" {
		t.Fatalf("expected annotated null error; got Tag=%v Annot=%q", out.Tag, out.Annot)
	}
}

func Test_RT_JSONSchemaStringToType_BadJSON_YieldsAnnotatedNull(t *testing.T) {
	ip := NewRuntime()

	src := `jsonSchemaStringToType("{ invalid json }")`
	out, err := ip.EvalSource(src)
	if err != nil {
		t.Fatalf("Eval error: %v", err)
	}
	if out.Tag != VTNull || out.Annot == "" {
		t.Fatalf("expected annotated null error; got Tag=%v Annot=%q", out.Tag, out.Annot)
	}
}

func Test_RT_jsonSchemaToType_NonObjectInput_YieldsAny(t *testing.T) {
	ip := NewRuntime()

	out, err := ip.EvalSource(`jsonSchemaToType("not an object")`)
	if err != nil {
		t.Fatalf("Eval error: %v", err)
	}
	if out.Tag != VTType || strings.TrimSpace(FormatValue(out)) != "Any" {
		t.Fatalf("expected Any; got %v", FormatValue(out))
	}
}

func Test_RT_BaseType_StripsNullable_AndResolvesAliases(t *testing.T) {
	ip := NewRuntime()
	src := `let T = type Int?
	baseType(T)
	`
	// Define alias persistently
	out, err := ip.EvalSource(src)
	if err != nil {
		t.Fatalf("setup error: %v", err)
	}

	// baseType(T) -> Int
	if out.Tag != VTType {
		t.Fatalf("want VTType, got %#v", FormatValue(out))
	}
	tv := out.Data.(*TypeValue)
	if !equalS(tv.Ast, typeS(t, ip, `Int`)) {
		t.Fatalf("baseType failed, got %#v", tv.Ast)
	}
}
