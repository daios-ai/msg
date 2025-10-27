package mindscript

import (
	"bufio"
	"io"
	"net"
	"net/http"
	"net/http/httptest"
	"os"
	"path/filepath"
	"strings"
	"testing"
	"time"
)

// ---------- helpers (mirroring style from your other suites) ----------

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

func mustMap(t *testing.T, v Value) *MapObject {
	t.Helper()
	if v.Tag != VTMap {
		t.Fatalf("expected map; got %#v", v)
	}
	return v.Data.(*MapObject)
}

func mget(m *MapObject, k string) (Value, bool) { v, ok := m.Entries[k]; return v, ok }

// ---------- tests ----------

func Test_Builtin_Net_Connect_Echo(t *testing.T) {
	ip, _ := NewInterpreter()

	addr := freeLocalAddr(t)

	// Start a tiny Go echo server (line-based).
	ln, err := net.Listen("tcp", addr)
	if err != nil {
		t.Fatalf("listen: %v", err)
	}
	defer ln.Close()

	done := make(chan struct{})
	go func() {
		defer close(done)
		c, err := ln.Accept()
		if err != nil {
			return
		}
		defer c.Close()
		r := bufio.NewReader(c)
		w := bufio.NewWriter(c)
		line, _ := r.ReadString('\n')
		_, _ = w.WriteString(line)
		_ = w.Flush()
	}()

	src := `
		let c = netConnect(` + msq(addr) + `)
		write(c, "ping\n")
		flush(c)
		let got = readLine(c)
		close(c)
		got
	`
	v := evalWithIP(t, ip, src)
	assertStr(t, v, "ping")

	<-done
}

func Test_Builtin_Net_ListenAccept_GoClient(t *testing.T) {
	ip, _ := NewInterpreter()

	addr := freeLocalAddr(t)

	type result struct {
		v   Value
		err error
	}
	ch := make(chan result, 1)

	// Run blocking accept/read in a goroutine via MindScript.
	go func() {
		v, err := ip.EvalSource(`
			let l = netListen(` + msq(addr) + `)
			let c = netAccept(l)
			let line = readLine(c)
			close(c)
			close(l)
			line
		`)
		ch <- result{v, err}
	}()

	// Give listener a moment to bind.
	time.Sleep(50 * time.Millisecond)

	// Go client connects and sends a line.
	conn, err := net.Dial("tcp", addr)
	if err != nil {
		t.Fatalf("dial: %v", err)
	}
	_, _ = conn.Write([]byte("hello\n"))
	_ = conn.Close()

	res := <-ch
	if res.err != nil {
		t.Fatalf("eval error: %v", res.err)
	}
	assertStr(t, res.v, "hello")
}

func Test_Builtin_Net_HTTP_GET_Buffered(t *testing.T) {
	ip, _ := NewInterpreter()

	ts := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		w.Header()["X-Multi"] = []string{"a", "b"}
		w.Header().Set("X-Test", "1")
		w.WriteHeader(200)
		_, _ = w.Write([]byte("OK"))
	}))
	defer ts.Close()

	src := `
		let resp = http({ "url": ` + msq(ts.URL) + ` })
		resp
	`
	v := evalWithIP(t, ip, src)
	m := mustMap(t, v)

	// status
	if got, ok := mget(m, "status"); !ok || got.Tag != VTInt || got.Data.(int64) != 200 {
		t.Fatalf("status missing/wrong: %#v", got)
	}
	// body
	if got, ok := mget(m, "body"); !ok {
		t.Fatalf("body missing")
	} else {
		assertStr(t, got, "OK")
	}
	// headers
	hv, ok := mget(m, "headers")
	if !ok {
		t.Fatalf("headers missing")
	}
	hm := mustMap(t, hv)
	if x, ok := mget(hm, "X-Test"); !ok {
		t.Fatalf("X-Test header missing")
	} else {
		assertStr(t, x, "1")
	}
	// multi-value joined by ", "
	if x, ok := mget(hm, "X-Multi"); !ok {
		t.Fatalf("X-Multi header missing")
	} else if x.Tag != VTStr || x.Data.(string) != "a, b" {
		t.Fatalf("X-Multi want %q, got %#v", "a, b", x)
	}
}

func Test_Builtin_Net_HTTP_POST_Body_Str(t *testing.T) {
	ip, _ := NewInterpreter()

	var gotMethod, gotBody string
	ts := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		b, _ := io.ReadAll(r.Body)
		_ = r.Body.Close()
		gotMethod = r.Method
		gotBody = string(b)
		w.WriteHeader(201)
		_, _ = w.Write([]byte("echo:" + gotBody))
	}))
	defer ts.Close()

	src := `
		http({
			"url": ` + msq(ts.URL) + `,
			"method": "POST",
			"headers": { "Content-Type": "text/plain" },
			"body": "ABC"
		})
	`
	v := evalWithIP(t, ip, src)
	m := mustMap(t, v)

	// status
	if s, ok := mget(m, "status"); !ok || s.Tag != VTInt || s.Data.(int64) != 201 {
		t.Fatalf("status want 201, got %#v", s)
	}
	// body echo
	if b, ok := mget(m, "body"); !ok || b.Tag != VTStr || !strings.HasPrefix(b.Data.(string), "echo:") {
		t.Fatalf("response echo missing/wrong: %#v", b)
	}

	if gotMethod != "POST" || gotBody != "ABC" {
		t.Fatalf("server saw method/body %q/%q", gotMethod, gotBody)
	}
}

func Test_Builtin_Net_HTTP_POST_Body_Handle(t *testing.T) {
	ip, _ := NewInterpreter()

	// Prepare a file with content to upload via bodyH.
	dir := t.TempDir()
	path := filepath.Join(dir, "up.txt")
	_ = os.WriteFile(path, []byte("file-contents"), 0o644)

	var gotBody string
	ts := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		b, _ := io.ReadAll(r.Body)
		_ = r.Body.Close()
		gotBody = string(b)
		w.WriteHeader(200)
		_, _ = w.Write([]byte("ok"))
	}))
	defer ts.Close()

	src := `
		let h = open(` + msq(path) + `, "r")
		http({
			"url": ` + msq(ts.URL) + `,
			"method": "POST",
			"bodyH": h
		})
	`
	v := evalWithIP(t, ip, src)
	_ = v // just ensure call succeeded
	if gotBody != "file-contents" {
		t.Fatalf("server saw %q", gotBody)
	}
}

func Test_Builtin_Net_HTTP_Stream_ReadAll(t *testing.T) {
	ip, _ := NewInterpreter()

	body := strings.Repeat("Z", 1024)

	ts := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		// Send a medium body to encourage streaming paths
		w.WriteHeader(200)
		_, _ = w.Write([]byte(body))
	}))
	defer ts.Close()

	src := `
		let r = httpStream({ "url": ` + msq(ts.URL) + ` })
		let h = r.bodyH
		let b = readAll(h)
		close(h)
		# return tuple-ish array for checks
		[r.status, r.proto, b]
	`
	v := evalWithIP(t, ip, src)
	arr := mustArray(t, v)
	if len(arr) != 3 {
		t.Fatalf("want 3 elements, got %d", len(arr))
	}
	assertInt(t, arr[0], 200)
	if arr[2].Tag != VTStr || arr[2].Data.(string) != body {
		t.Fatalf("streamed body mismatch")
	}
}

func Test_Builtin_Net_HTTP_Timeout_AnnotatedNull(t *testing.T) {
	ip, _ := NewInterpreter()

	ts := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		time.Sleep(200 * time.Millisecond) // longer than client timeout
		w.WriteHeader(200)
	}))
	defer ts.Close()

	v := evalWithIP(t, ip, `
		http({ "url": `+msq(ts.URL)+`, "timeoutMs": 50 })
	`)
	assertNullAnnotated(t, v)
}

func Test_Builtin_Net_Connect_InvalidAddr_AnnotatedNull(t *testing.T) {
	ip, _ := NewInterpreter()

	// Assuming this port is closed; connection should fail quickly.
	v := evalWithIP(t, ip, `netConnect("127.0.0.1:1")`)
	assertNullAnnotated(t, v)
}
