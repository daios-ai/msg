// modules_autoload_test.go
package mindscript

import (
	"net/http"
	"net/http/httptest"
	"os"
	"path/filepath"
	"strings"
	"testing"
)

// --- local helpers ----------------------------------------------------------

func withTempDir(t *testing.T) (dir string, cleanup func()) {
	t.Helper()
	d, err := os.MkdirTemp("", "msmod-*")
	if err != nil {
		t.Fatalf("mkdtemp: %v", err)
	}
	return d, func() { _ = os.RemoveAll(d) }
}

func write(t *testing.T, dir, name, src string) string {
	t.Helper()
	p := filepath.Join(dir, name)
	if err := os.WriteFile(p, []byte(src), 0o644); err != nil {
		t.Fatalf("write %s: %v", p, err)
	}
	return p
}

func chdir(t *testing.T, dir string) func() {
	t.Helper()
	old, _ := os.Getwd()
	if err := os.Chdir(dir); err != nil {
		t.Fatalf("chdir %s: %v", dir, err)
	}
	return func() { _ = os.Chdir(old) }
}

// NOTE: We intentionally rely on existing helpers elsewhere in the test suite:
//   - evalWithIP(t, ip, src)
//   - wantAnnotatedNullContains(t, v, substr)
//   - wantInt(t, v, n)
//   - wantStr(t, v, s)

// --- tests ------------------------------------------------------------------

// importCode(name, src) builds an isolated module (no cache registration).
func Test_ImportCode_Simple(t *testing.T) {
	ip, _ := NewInterpreter()

	v := evalWithIP(t, ip, `
let m = importCode("mem.calc", "
  let x = 41
  let inc = fun(n: Int) -> Int do
    return(n + 1)
  end
")
m.inc(m.x)`)
	wantInt(t, v, 42)
}

// Filesystem import from CWD; default extension (.ms) applied when missing.
func Test_FileImport_Simple_And_DefaultExt(t *testing.T) {
	dir, done := withTempDir(t)
	defer done()
	defer chdir(t, dir)()

	_ = write(t, dir, "m1.ms", `let x = 41
let inc = fun(n: Int) -> Int do return(n + 1) end
`)

	ip, _ := NewInterpreter()

	// import("m1") should find m1.ms via default extension
	v := evalWithIP(t, ip, `
let m = import("m1")
m.inc(m.x)`)
	wantInt(t, v, 42)
}

// Relative resolution uses the importer's directory.
func Test_FileImport_Relative_FromImporterDir(t *testing.T) {
	dir, done := withTempDir(t)
	defer done()
	defer chdir(t, dir)()

	_ = write(t, dir, "b.ms", `let x = 10`)
	_ = write(t, dir, "a.ms", `
let b = import("b")
let y = b.x + 2
`)

	ip, _ := NewInterpreter()
	v := evalWithIP(t, ip, `
let a = import("a")
a.y`)
	wantInt(t, v, 12)
}

// Search via MindScriptPath when not found in importer dir or CWD.
func Test_FileImport_Search_MindScriptPath_LibSubdir(t *testing.T) {
	root, done := withTempDir(t)
	defer done()

	// Create SDK-like layout: <root>/lib
	libDir := filepath.Join(root, "lib")
	if err := os.MkdirAll(libDir, 0o755); err != nil {
		t.Fatalf("mkdir lib: %v", err)
	}

	_ = write(t, libDir, "util.ms", `let name = "Bob"`)
	_ = write(t, libDir, "std.ms", ``) // tiny valid program

	// Empty CWD; rely on MSGPATH roots (loader appends /lib)
	old := os.Getenv(MindScriptPath)
	_ = os.Setenv(MindScriptPath, root)
	defer os.Setenv(MindScriptPath, old)

	ip, err := NewInterpreter()
	if err != nil {
		t.Fatalf("NewRuntime failed: %v", err)
	}

	v := evalWithIP(t, ip, `
let u = import("util")
u.name`)
	wantStr(t, v, "Bob")
}

// Import cycles are contractual mistakes → HARD (propagate as errors).
func Test_FileImport_Cycle_TwoModules_Is_HardError(t *testing.T) {
	dir, done := withTempDir(t)
	defer done()
	defer chdir(t, dir)()

	_ = write(t, dir, "A.ms", `let b = import("B")`)
	_ = write(t, dir, "B.ms", `let a = import("A")`)

	ip, _ := NewInterpreter()
	_, err := ip.EvalSource(`import("A")`)
	if err == nil {
		t.Fatalf("expected hard error (cycle), got nil")
	}
	if !strings.Contains(strings.ToLower(err.Error()), "import cycle") {
		t.Fatalf("want error mentioning import cycle; got: %v", err)
	}
	if !strings.Contains(err.Error(), "A -> B -> A") {
		t.Fatalf("want cycle chain 'A -> B -> A' in error; got: %v", err)
	}
}

// Parse errors are HARD; runtime contract errors propagate as HARD; operational
// loader errors (resolve/fetch) are SOFT (annotated null).
func Test_FileImport_Parse_Runtime_And_Resolve_Errors(t *testing.T) {
	dir, done := withTempDir(t)
	defer done()
	defer chdir(t, dir)()

	_ = write(t, dir, "bad.ms", `let x = (1 +`)
	_ = write(t, dir, "boom.ms", `1 / 0`)

	ip, _ := NewInterpreter()

	// ──────────────────────────────────────────────────────────────────────────
	// 1) Parse error → HARD (Go error). Must mention the file and "parse error".
	// ──────────────────────────────────────────────────────────────────────────
	_, perr := ip.EvalSource(`import("bad")`)
	if perr == nil {
		t.Fatalf("expected parse error (hard), got nil")
	}
	perrStr := strings.ToLower(perr.Error())
	if !strings.Contains(perr.Error(), "bad.ms") || !strings.Contains(perrStr, "parse error") {
		t.Fatalf("want error mentioning bad.ms and parse error; got: %v", perr)
	}

	// ──────────────────────────────────────────────────────────────────────────
	// 2) Runtime contract failure (division-by-zero) → HARD (Go error).
	//    The interpreter should propagate the runtime error, not soften it.
	// ──────────────────────────────────────────────────────────────────────────
	_, rterr := ip.EvalSource(`import("boom")`)
	if rterr == nil {
		t.Fatalf("expected hard runtime error for division by zero, got nil")
	}
	rtStr := strings.ToLower(rterr.Error())
	if !strings.Contains(rtStr, "division by zero") {
		t.Fatalf("want error mentioning division by zero; got: %v", rterr)
	}

	// ──────────────────────────────────────────────────────────────────────────
	// 3) Contractual mistake: wrong arity → HARD (Go error).
	// ──────────────────────────────────────────────────────────────────────────
	_, aerr := ip.EvalSource(`fun(x: Int) -> Int do x end(1, 2)`)
	if aerr == nil {
		t.Fatalf("expected hard error for too many arguments, got nil")
	}
	aStr := strings.ToLower(aerr.Error())
	if !strings.Contains(aStr, "arity") && !strings.Contains(aStr, "not a function") && !strings.Contains(aStr, "too many") {
		t.Fatalf("want hard error mentioning arity/too many arguments; got: %v", aerr)
	}

	// ──────────────────────────────────────────────────────────────────────────
	// 4) Contractual mistake: type mismatch → HARD (Go error).
	// ──────────────────────────────────────────────────────────────────────────
	_, terr := ip.EvalSource(`fun(x: Int) -> Int do x end("oops")`)
	if terr == nil {
		t.Fatalf("expected hard error for type mismatch, got nil")
	}
	if !strings.Contains(strings.ToLower(terr.Error()), "type mismatch") && !strings.Contains(strings.ToLower(terr.Error()), "parameter 'x'") {
		t.Fatalf("want hard error indicating type mismatch for param x; got: %v", terr)
	}

	// ──────────────────────────────────────────────────────────────────────────
	// 5) Operational failure (missing file) → SOFT (annotated null).
	// ──────────────────────────────────────────────────────────────────────────
	v, ferr := ip.EvalSource(`import("no_such_file")`)
	if ferr != nil {
		t.Fatalf("expected soft error (annotated null) for missing file, got hard error: %v", ferr)
	}
	if v.Tag != VTNull {
		t.Fatalf("expected VTNull soft error value for missing file, got: %#v", v)
	}
	if v.Annot == "" || (!strings.Contains(strings.ToLower(v.Annot), "no_such_file") &&
		!strings.Contains(strings.ToLower(v.Annot), "not found") &&
		!strings.Contains(strings.ToLower(v.Annot), "module") &&
		!strings.Contains(strings.ToLower(v.Annot), "import")) {
		t.Fatalf("want annotation indicating missing file; got: %#v", v)
	}
}

// Contractual mistakes (arity/type) are hard errors.
func Test_ContractualMistakes_Are_Hard(t *testing.T) {
	ip, _ := NewInterpreter()

	// Too many arguments → hard error.
	// (fun(x:Int) -> Int do x end)(1, 2)
	_, errArity := ip.EvalSource(`(fun(x: Int) -> Int do x end)(1, 2)`)
	if errArity == nil {
		t.Fatalf("expected hard error for too many arguments, got nil")
	}
	if !strings.Contains(strings.ToLower(errArity.Error()), "not a function") &&
		!strings.Contains(strings.ToLower(errArity.Error()), "arity") {
		t.Fatalf("want 'too many arguments' / arity in error; got: %v", errArity)
	}

	// Wrong type in argument → hard error.
	// (fun(x:Int) -> Int do x end)("hi")
	_, errType := ip.EvalSource(`(fun(x: Int) -> Int do x end)("hi")`)
	if errType == nil {
		t.Fatalf("expected hard error for type mismatch, got nil")
	}
	if !strings.Contains(strings.ToLower(errType.Error()), "type mismatch") {
		t.Fatalf("want 'type mismatch' in error; got: %v", errType)
	}
}

// Operational/runtime loader issues (resolve/fetch) are soft errors.
func Test_SoftOperationalErrors_Are_Soft(t *testing.T) {
	dir, done := withTempDir(t)
	defer done()
	defer chdir(t, dir)()

	ip, _ := NewInterpreter()

	// Import of non-existent module → soft error (annotated null).
	v, err := ip.EvalSource(`import("does_not_exist")`)
	if err != nil {
		t.Fatalf("expected soft error (annotated null), got hard error: %v", err)
	}
	if v.Tag != VTNull {
		t.Fatalf("expected VTNull soft error value, got: %#v", v)
	}
	if v.Annot == "" || (!strings.Contains(strings.ToLower(v.Annot), "does_not_exist") &&
		!strings.Contains(strings.ToLower(v.Annot), "not found") &&
		!strings.Contains(strings.ToLower(v.Annot), "import")) {
		t.Fatalf("want annotation mentioning missing module; got: %#v", v)
	}
}

// --- modules are map-like & writable --------------------------

// Writable module fields, and NO namespace collision with caller globals.
func Test_Module_MapLike_Writable_NoNamespaceCollision(t *testing.T) {
	dir, done := withTempDir(t)
	defer done()
	defer chdir(t, dir)()

	_ = write(t, dir, "m.ms", `
let x = 2
`)

	ip, _ := NewInterpreter()
	// Use block-form if/then/else with 'end', per grammar.
	v := evalWithIP(t, ip, `
let x = 1
let m = import("m")
let before = m.x
m.x = 3
let after = m.x
if (x == 1) and (before == 2) and (after == 3) then
  1
else
  0
end
`)
	wantInt(t, v, 1)
}

// Patching a module variable is visible to closures defined inside the module.
func Test_Module_Patch_Visible_To_Module_Closures(t *testing.T) {
	dir, done := withTempDir(t)
	defer done()
	defer chdir(t, dir)()

	_ = write(t, dir, "m.ms", `
let x = 5
let get = fun() -> Int do return(x) end
`)

	ip, _ := NewInterpreter()
	// Expect a*10 + b == 57 where a=get() before patch (5) and b after patch (7).
	v := evalWithIP(t, ip, `
let m = import("m")
let a = m.get()
m.x = 7
let b = m.get()
a * 10 + b
`)
	wantInt(t, v, 57)
}

// Destructuring and len() work because modules behave like maps.
func Test_Module_Destructure_And_Len(t *testing.T) {
	dir, done := withTempDir(t)
	defer done()
	defer chdir(t, dir)()

	_ = write(t, dir, "m.ms", `
let x = 41
let inc = fun(n: Int) -> Int do return(n + 1) end
`)

	ip, _ := NewInterpreter()

	// Object pattern requires "key : pattern" entries (no shorthand).
	v := evalWithIP(t, ip, `
let {x: x, inc: inc} = import("m")
inc(x)`)
	wantInt(t, v, 42)

	// len(m) == 2 (x and inc)
	v2 := evalWithIP(t, ip, `
let m = import("m")
len(m)`)
	wantInt(t, v2, 2)
}

// Iteration over module exports (order not asserted here; just sums values).
func Test_Module_Iterate(t *testing.T) {
	dir, done := withTempDir(t)
	defer done()
	defer chdir(t, dir)()

	_ = write(t, dir, "m.ms", `
let a = 1
let b = 2
`)

	ip, _ := NewInterpreter()
	v := evalWithIP(t, ip, `
let m = import("m")
let total = 0
for [k, v] in m do
  total = total + v
end
total`)
	wantInt(t, v, 3)
}

// Builtins are visible inside module env (Core is parent).
func Test_FileImport_Builtins_Accessible(t *testing.T) {
	dir, done := withTempDir(t)
	defer done()
	defer chdir(t, dir)()

	_ = write(t, dir, "util.ms", `
let js = jsonStringify({a: 1})
`)

	ip, _ := NewInterpreter()
	v := evalWithIP(t, ip, `
let u = import("util")
u.js`)
	wantStr(t, v, `{"a":1}`)
}

// Module env is isolated from caller global; nothing leaks back.
func Test_Isolation_No_Leak_To_User_Global(t *testing.T) {
	dir, done := withTempDir(t)
	defer done()
	defer chdir(t, dir)()

	_ = write(t, dir, "mod.ms", `let hidden = 123`)

	ip, _ := NewInterpreter()
	// Import should not define "hidden" in the caller's env; referencing it is a hard error now.
	_, err := ip.EvalSource(`
let _ = import("mod")
hidden`)
	if err == nil {
		t.Fatalf("expected undefined variable error, got nil")
	}
	if !strings.Contains(strings.ToLower(err.Error()), "undefined variable") {
		t.Fatalf("want error mentioning undefined variable; got: %v", err)
	}
}

// HTTP import via absolute URL (with/without default extension).
func Test_HTTP_Import_Simple(t *testing.T) {
	srv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		switch r.URL.Path {
		case "/m1.ms", "/m2.ms":
			w.Write([]byte(`let x = 5`))
		default:
			http.NotFound(w, r)
		}
	}))
	defer srv.Close()

	ip, _ := NewInterpreter()

	// Explicit .ms
	v1 := evalWithIP(t, ip, `
let m = import("`+srv.URL+`/m1.ms")
m.x`)
	wantInt(t, v1, 5)

	// Without extension — the autoloader appends defaultModuleExt for http(s)
	v2 := evalWithIP(t, ip, `
let m = import("`+srv.URL+`/m2")
m.x`)
	wantInt(t, v2, 5)
}

// HTTP 404/resolve errors are operational → SOFT annotated null.
func Test_HTTP_Import_404_Is_Soft(t *testing.T) {
	srv := httptest.NewServer(http.NotFoundHandler())
	defer srv.Close()

	ip, _ := NewInterpreter()
	v, err := ip.EvalSource(`import("` + srv.URL + `/nope")`)
	if err != nil {
		t.Fatalf("expected soft error (annotated null) for 404, got hard error: %v", err)
	}
	if v.Tag != VTNull {
		t.Fatalf("expected VTNull soft error for 404, got: %#v", v)
	}
	if v.Annot == "" || !strings.Contains(strings.ToLower(v.Annot), "http") {
		t.Fatalf("want annotation mentioning http fetch failure; got: %#v", v)
	}
}

// Three-module cycle A -> B -> C -> A should be a HARD error with a readable chain.
func Test_Module_Cycle_Three_Is_HardError(t *testing.T) {
	dir, done := withTempDir(t)
	defer done()
	defer chdir(t, dir)()

	_ = write(t, dir, "A.ms", `let b = import("B")`)
	_ = write(t, dir, "B.ms", `let c = import("C")`)
	_ = write(t, dir, "C.ms", `let a = import("A")`)

	ip, _ := NewInterpreter()
	_, err := ip.EvalSource(`import("A")`)
	if err == nil {
		t.Fatalf("expected hard error (cycle), got nil")
	}
	wantErrContains(t, err, "import cycle")
	wantErrContains(t, err, "A -> B -> C -> A")
}

// importCode-driven cycle mem:A -> mem:B -> mem:A should be a HARD error.
func Test_Module_ImportCode_Cycle_Is_HardError(t *testing.T) {
	ip, _ := NewInterpreter()

	// The body of A attempts to importCode B, whose body attempts to importCode A.
	// Cycle detection uses canonical identities "mem:A" and "mem:B".
	_, err := ip.EvalSource(`
importCode("A", "let b = importCode(\"B\", \"let a = importCode(\\\"A\\\", \\\"1\\\")\")")
`)
	if err == nil {
		t.Fatalf("expected hard error (cycle via importCode), got nil")
	}
	wantErrContains(t, err, "import cycle")
	// Chain formatting is best-effort; at minimum it should include A and B.
	wantErrContains(t, err, "A")
	wantErrContains(t, err, "B")
}

// Re-importing the same file-backed module should return the cached instance.
// Mutations to the module surface are visible after re-import.
func Test_Module_Cache_Reimport_Sees_Mutation(t *testing.T) {
	dir, done := withTempDir(t)
	defer done()
	defer chdir(t, dir)()

	_ = write(t, dir, "m.ms", `let x = 7`)

	ip, _ := NewInterpreter()
	v := evalWithIP(t, ip, `
let m1 = import("m")
let before = m1.x
m1.x = 99
let m2 = import("m")
m2.x`)
	// m2.x should observe the mutation (same cached module instance).
	wantInt(t, v, 99)
}

// HTTP 500 from server is an operational error → soft annotated null from import("...").
func Test_Module_HTTP_500_Is_Soft(t *testing.T) {
	srv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		w.WriteHeader(500)
		_, _ = w.Write([]byte("boom"))
	}))
	defer srv.Close()

	ip, _ := NewInterpreter()
	v, err := ip.EvalSource(`import("` + srv.URL + `/oops")`)
	if err != nil {
		t.Fatalf("expected soft error (annotated null) for HTTP 500, got hard error: %v", err)
	}
	if v.Tag != VTNull {
		t.Fatalf("expected VTNull soft error for HTTP 500, got: %#v", v)
	}
	wantAnnotatedContains(t, v, "http 500")
}

// A mixed import chain: file module importing an HTTP module (with default .ms extension) works.
func Test_Module_File_Imports_HTTP_DefaultExt(t *testing.T) {
	// HTTP server serves /r.ms
	srv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		switch r.URL.Path {
		case "/r.ms":
			_, _ = w.Write([]byte(`let val = 123`))
		default:
			http.NotFound(w, r)
		}
	}))
	defer srv.Close()

	dir, done := withTempDir(t)
	defer done()
	defer chdir(t, dir)()

	// a.ms imports the HTTP module WITHOUT extension; loader should append .ms automatically.
	src := `let r = import("` + srv.URL + `/r")
let x = r.val`
	_ = write(t, dir, "a.ms", src)

	ip, _ := NewInterpreter()
	v := evalWithIP(t, ip, `
let a = import("a")
a.x`)
	wantInt(t, v, 123)
}

// Exported type values are pinned to the module environment.
// We verify by exporting a function type and calling a closure that captures module state,
// then mutating the state and ensuring the closure sees the mutation.
func Test_Module_Closures_See_Env(t *testing.T) {
	dir, done := withTempDir(t)
	defer done()
	defer chdir(t, dir)()

	// No explicit return type on mk — just return a closure capturing `n`.
	_ = write(t, dir, "pin.ms", `
let n = 10
let mk = fun() do
  return(fun() -> Int do n end)
end
`)

	ip, _ := NewInterpreter()
	v := evalWithIP(t, ip, `
let m = import("pin")
let f = m.mk()
let a = f()
m.n = 20
let b = f()
a * 100 + b`)
	// a=10, b=20 (after mutation), so 10*100+20 = 1020
	wantInt(t, v, 1020)
}

// Parse error inside an HTTP module is a HARD error (propagates with caret).
func Test_Module_HTTP_ParseError_Is_Hard(t *testing.T) {
	srv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		// Serve invalid MindScript
		_, _ = w.Write([]byte(`let x = (`))
	}))
	defer srv.Close()

	ip, _ := NewInterpreter()
	_, err := ip.EvalSource(`import("` + srv.URL + `/bad.ms")`)
	if err == nil {
		t.Fatalf("expected hard parse error from HTTP import, got nil")
	}
	// Error string should at least mention "parse error".
	wantErrContains(t, err, "parse error")
}

// Import resolution should prefer the importer's directory over CWD/MSGPATH.
func Test_Module_Resolution_Prefers_Importer_Dir(t *testing.T) {
	dir, done := withTempDir(t)
	defer done()
	defer chdir(t, dir)()

	sub := filepath.Join(dir, "sub")
	if err := os.MkdirAll(sub, 0o755); err != nil {
		t.Fatalf("mkdir: %v", err)
	}
	// Create b.ms in subdir and also in root; importer a.ms in subdir should resolve to sub/b.ms
	_ = write(t, dir, "b.ms", `let x = 1`)
	_ = write(t, sub, "b.ms", `let x = 2`)
	_ = write(t, sub, "a.ms", `let b = import("b")
let y = b.x`)

	ip, _ := NewInterpreter()
	v := evalWithIP(t, ip, `
let a = import("sub/a")
a.y`)
	// Should have used sub/b.ms → x = 2
	wantInt(t, v, 2)
}

// Failed inline module should NOT leave a stale "loading" record.
// Retrying with the same name must NOT report an import cycle.
// After a failure, using the same name for a successful module should work.
func Test_Modules_InlineFailure_DoesNotStaleCacheAndRetry(t *testing.T) {
	ip, _ := NewInterpreter()

	bad := `let m = module "M" do 1/0 end`

	// First attempt: real runtime error from body.
	if _, err := ip.EvalPersistentSource(bad); err == nil {
		t.Fatalf("expected runtime error on first construction")
	} else if msg := err.Error(); !strings.Contains(msg, "division by zero") {
		t.Fatalf("want division-by-zero error, got: %s", msg)
	} else if strings.Contains(msg, "import cycle") {
		t.Fatalf("unexpected import cycle on first attempt: %s", msg)
	}

	// Second attempt with the same name should NOT report a cycle.
	if _, err := ip.EvalPersistentSource(bad); err == nil {
		t.Fatalf("expected runtime error on second construction")
	} else if msg := err.Error(); !strings.Contains(msg, "division by zero") {
		t.Fatalf("want division-by-zero error again, got: %s", msg)
	} else if strings.Contains(msg, "import cycle") {
		t.Fatalf("BUG: stale modLoading caused false cycle on retry: %s", msg)
	}

	// Now succeed with the same name; cache must be clean after failures.
	ok := `let m = module "M" do let x = 7 end m.x`
	v, err := ip.EvalPersistentSource(ok)
	if err != nil {
		t.Fatalf("unexpected error building module after prior failure: %v", err)
	}
	wantInt(t, v, 7)
}

// Successful inline modules are cached by canonical name: the second construction
// with the same name returns the cached module (does not re-run the body).
func Test_Modules_Inline_Success_CachesByName(t *testing.T) {
	ip, _ := NewInterpreter()

	v1, err := ip.EvalPersistentSource(`let m1 = module "C" do let x = 1 end m1.x`)
	if err != nil {
		t.Fatalf("unexpected error on first inline module: %v", err)
	}
	wantInt(t, v1, 1)

	// Body sets x = 2, but since "C" is already cached, result must still be 1.
	v2, err := ip.EvalPersistentSource(`let m2 = module "C" do let x = 2 end m2.x`)
	if err != nil {
		t.Fatalf("unexpected error on second inline module: %v", err)
	}
	wantInt(t, v2, 1)
}
