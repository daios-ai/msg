// modules_test.go
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
//   - wantAnnotatedContains(t, v, substr)
//   - wantErrContains(t, err, substr)
//   - wantInt(t, v, n)
//   - wantStr(t, v, s)

// --- tests ------------------------------------------------------------------

// importCode(name, src) builds a module in memory.
func Test_Module_ImportCode_Simple(t *testing.T) {
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

// Filesystem import from CWD resolves a single-file module (m1.ms) when importing "m1".
func Test_Module_ImportFile_FromCWD_ResolvesSingleFile(t *testing.T) {
	dir, done := withTempDir(t)
	defer done()
	defer chdir(t, dir)()

	_ = write(t, dir, "m1.ms", `let x = 41
let inc = fun(n: Int) -> Int do return(n + 1) end
`)

	ip, _ := NewInterpreter()

	v := evalWithIP(t, ip, `
let m = import("m1")
m.inc(m.x)`)
	wantInt(t, v, 42)
}

// Relative resolution prefers the importer's directory.
func Test_Module_ImportFile_Relative_UsesImporterDir(t *testing.T) {
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

// Stdlib fallback: search <install-root>/lib when not found in importer dir or CWD.
func Test_Module_ImportFile_StdlibFallback_LibDir(t *testing.T) {
	root, done := withTempDir(t)
	defer done()

	libDir := filepath.Join(root, "lib")
	if err := os.MkdirAll(libDir, 0o755); err != nil {
		t.Fatalf("mkdir lib: %v", err)
	}

	_ = write(t, libDir, "util.ms", `let name = "Bob"`)
	_ = write(t, libDir, "std.ms", ``)

	oldInstall := installRoot
	installRoot = root
	defer func() { installRoot = oldInstall }()

	ip, err := NewInterpreter()
	if err != nil {
		t.Fatalf("NewRuntime failed: %v", err)
	}

	v := evalWithIP(t, ip, `
let u = import("util")
u.name`)
	wantStr(t, v, "Bob")
}

// Import cycles are contractual mistakes → HARD.
func Test_Module_ImportFile_Cycle_TwoModules_IsHardError(t *testing.T) {
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
	if !strings.Contains(strings.ToLower(err.Error()), "import cycle detected") {
		t.Fatalf("want error mentioning import cycle; got: %v", err)
	}
}

// Parse/runtime contract errors are HARD; resolver/fetch failures are HARD (clean resolver).
func Test_Module_ImportFile_ErrorClassification_ParseRuntimeOperational(t *testing.T) {
	dir, done := withTempDir(t)
	defer done()
	defer chdir(t, dir)()

	_ = write(t, dir, "bad.ms", `let x = (1 +`)
	_ = write(t, dir, "boom.ms", `1 / 0`)

	ip, _ := NewInterpreter()

	// 1) Parse error → HARD.
	_, perr := ip.EvalSource(`import("bad")`)
	if perr == nil {
		t.Fatalf("expected parse error (hard), got nil")
	}
	perrStr := strings.ToLower(perr.Error())
	if !strings.Contains(perr.Error(), "bad") || !strings.Contains(perrStr, "parse error") {
		t.Fatalf("want error mentioning bad and parse error; got: %v", perr)
	}

	// 2) Runtime error (division-by-zero) → HARD.
	_, rterr := ip.EvalSource(`import("boom")`)
	if rterr == nil {
		t.Fatalf("expected hard runtime error for division by zero, got nil")
	}
	if !strings.Contains(strings.ToLower(rterr.Error()), "division by zero") {
		t.Fatalf("want error mentioning division by zero; got: %v", rterr)
	}

	// 3) Wrong arity → HARD.
	_, aerr := ip.EvalSource(`fun(x: Int) -> Int do x end(1, 2)`)
	if aerr == nil {
		t.Fatalf("expected hard error for too many arguments, got nil")
	}
	aStr := strings.ToLower(aerr.Error())
	if !strings.Contains(aStr, "arity") && !strings.Contains(aStr, "not a function") && !strings.Contains(aStr, "too many") {
		t.Fatalf("want hard error mentioning arity/too many arguments; got: %v", aerr)
	}

	// 4) Type mismatch → HARD.
	_, terr := ip.EvalSource(`fun(x: Int) -> Int do x end("oops")`)
	if terr == nil {
		t.Fatalf("expected hard error for type mismatch, got nil")
	}
	lo := strings.ToLower(terr.Error())
	if !strings.Contains(lo, "type mismatch") && !strings.Contains(lo, "parameter 'x'") {
		t.Fatalf("want hard error indicating type mismatch for param x; got: %v", terr)
	}

	// 5) Missing file/resolve error → HARD (clean resolver).
	_, ferr := ip.EvalSource(`import("no_such_file")`)
	if ferr == nil {
		t.Fatalf("expected hard error for missing module, got nil")
	}
	flo := strings.ToLower(ferr.Error())
	if !strings.Contains(flo, "no_such_file") && !strings.Contains(flo, "not found") {
		t.Fatalf("want error mentioning missing module; got: %v", ferr)
	}
}

// Contractual mistakes (arity/type) are HARD.
func Test_Module_ContractualMistakes_AreHard(t *testing.T) {
	ip, _ := NewInterpreter()

	_, errArity := ip.EvalSource(`(fun(x: Int) -> Int do x end)(1, 2)`)
	if errArity == nil {
		t.Fatalf("expected hard error for too many arguments, got nil")
	}
	if !strings.Contains(strings.ToLower(errArity.Error()), "not a function") &&
		!strings.Contains(strings.ToLower(errArity.Error()), "arity") {
		t.Fatalf("want 'too many arguments' / arity in error; got: %v", errArity)
	}

	_, errType := ip.EvalSource(`(fun(x: Int) -> Int do x end)("hi")`)
	if errType == nil {
		t.Fatalf("expected hard error for type mismatch, got nil")
	}
	if !strings.Contains(strings.ToLower(errType.Error()), "type mismatch") {
		t.Fatalf("want 'type mismatch' in error; got: %v", errType)
	}
}

func Test_Module_OperationalErrors(t *testing.T) {
	dir, done := withTempDir(t)
	defer done()
	defer chdir(t, dir)()

	ip, _ := NewInterpreter()

	_, err := ip.EvalSource(`import("does_not_exist")`)
	if err == nil {
		t.Fatalf("expected hard error for missing module, got nil")
	}
	lo := strings.ToLower(err.Error())
	if !strings.Contains(lo, "does_not_exist") && !strings.Contains(lo, "not found") {
		t.Fatalf("want error mentioning missing module; got: %v", err)
	}
}

// Writable module fields, and no namespace collision with caller globals.
func Test_Module_MapLike_Writable_NoGlobalCollision(t *testing.T) {
	dir, done := withTempDir(t)
	defer done()
	defer chdir(t, dir)()

	_ = write(t, dir, "m.ms", `
let x = 2
`)

	ip, _ := NewInterpreter()
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
func Test_Module_Mutation_VisibleToClosures(t *testing.T) {
	dir, done := withTempDir(t)
	defer done()
	defer chdir(t, dir)()

	_ = write(t, dir, "m.ms", `
let x = 5
let get = fun() -> Int do return(x) end
`)

	ip, _ := NewInterpreter()
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
func Test_Module_AsMap_Destructure_AndLen(t *testing.T) {
	dir, done := withTempDir(t)
	defer done()
	defer chdir(t, dir)()

	_ = write(t, dir, "m.ms", `
let x = 41
let inc = fun(n: Int) -> Int do return(n + 1) end
`)

	ip, _ := NewInterpreter()

	v := evalWithIP(t, ip, `
let {x: x, inc: inc} = import("m")
inc(x)`)
	wantInt(t, v, 42)

	v2 := evalWithIP(t, ip, `
let m = import("m")
len(m)`)
	wantInt(t, v2, 2)
}

// Iteration over module exports.
func Test_Module_AsMap_IterateExports(t *testing.T) {
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
func Test_Module_ImportFile_BuiltinsAccessible(t *testing.T) {
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
func Test_Module_Isolation_NoLeakToCallerGlobal(t *testing.T) {
	dir, done := withTempDir(t)
	defer done()
	defer chdir(t, dir)()

	_ = write(t, dir, "mod.ms", `let hidden = 123`)

	ip, _ := NewInterpreter()
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

// HTTP import via absolute URL (extensionless only).
func Test_Module_ImportHTTP_Extensionless_AndLoad(t *testing.T) {
	srv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		switch r.URL.Path {
		case "/m1.ms", "/m2.ms":
			_, _ = w.Write([]byte(`let x = 5`))
		default:
			http.NotFound(w, r)
		}
	}))
	defer srv.Close()

	ip, _ := NewInterpreter()

	// Specs are treated as stems; no extensionless enforcement.
	// import("<...>/m1.ms") probes "<...>/m1.ms.ms" and "<...>/m1.ms/init.ms" → not found.
	_, err := ip.EvalSource(`import("` + srv.URL + `/m1.ms")`)
	if err == nil {
		t.Fatalf("expected hard error for missing module, got nil")
	}
	wantErrContains(t, err, "module not found")
	wantErrContains(t, err, "m1.ms")

	v := evalWithIP(t, ip, `
let m = import("`+srv.URL+`/m2")
m.x`)
	wantInt(t, v, 5)
}

// HTTP 404 is an operational loader error → HARD (clean resolver).
func Test_Module_ImportHTTP_404(t *testing.T) {
	srv := httptest.NewServer(http.NotFoundHandler())
	defer srv.Close()

	ip, _ := NewInterpreter()

	_, err := ip.EvalSource(`import("` + srv.URL + `/nope")`)
	if err == nil {
		t.Fatalf("expected hard error for HTTP 404, got nil")
	}

	msg := strings.ToLower(err.Error())
	if !strings.Contains(msg, "module not found") {
		t.Fatalf("want error containing %q, got: %v", "module not found", err)
	}
	if !strings.Contains(err.Error(), srv.URL) || !strings.Contains(err.Error(), "/nope") {
		t.Fatalf("want error mentioning url/path, got: %v", err)
	}
}

// Three-module cycle A -> B -> C -> A should be a HARD error with readable chain.
func Test_Module_ImportFile_Cycle_ThreeModules_IsHardError(t *testing.T) {
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
	wantErrContains(t, err, "import cycle detected")
}

// importCode-driven cycle A -> B -> A should be a HARD error.
func Test_Module_ImportCode_Cycle_IsHardError(t *testing.T) {
	ip, _ := NewInterpreter()

	_, err := ip.EvalSource(`
importCode("A", "let b = importCode(\"B\", \"let a = importCode(\\\"A\\\", \\\"1\\\")\")")
`)
	if err == nil {
		t.Fatalf("expected hard error (cycle via importCode), got nil")
	}
	wantErrContains(t, err, "import cycle")
	wantErrContains(t, err, "A")
	wantErrContains(t, err, "B")
}

// Re-importing the same file-backed module should return the cached instance.
func Test_Module_Cache_Reimport_SeesMutation(t *testing.T) {
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
	wantInt(t, v, 99)
}

// HTTP 500 is an operational loader error → HARD (clean resolver).
func Test_Module_ImportHTTP_500(t *testing.T) {
	srv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		w.WriteHeader(500)
		_, _ = w.Write([]byte("boom"))
	}))
	defer srv.Close()

	ip, _ := NewInterpreter()

	_, err := ip.EvalSource(`import("` + srv.URL + `/oops")`)
	if err == nil {
		t.Fatalf("expected hard error for HTTP 500, got nil")
	}
	wantErrContains(t, err, "http")
	wantErrContains(t, err, "500")
}

// Mixed chain: file module importing an HTTP module (extensionless URL) works.
func Test_Module_ImportFile_ImportsHTTP_ExtensionlessURL(t *testing.T) {
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

	src := `let r = import("` + srv.URL + `/r")
let x = r.val`
	_ = write(t, dir, "a.ms", src)

	ip, _ := NewInterpreter()
	v := evalWithIP(t, ip, `
let a = import("a")
a.x`)
	wantInt(t, v, 123)
}

// Closures capturing module env see mutations via module writes.
func Test_Module_Closure_CapturesEnv_SeesMutation(t *testing.T) {
	dir, done := withTempDir(t)
	defer done()
	defer chdir(t, dir)()

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
	wantInt(t, v, 1020)
}

// Parse error inside an HTTP module is HARD.
func Test_Module_ImportHTTP_ParseError_IsHard(t *testing.T) {
	srv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		switch r.URL.Path {
		case "/bad.ms":
			_, _ = w.Write([]byte(`let x = (`))
		default:
			http.NotFound(w, r)
		}
	}))
	defer srv.Close()

	ip, _ := NewInterpreter()
	_, err := ip.EvalSource(`import("` + srv.URL + `/bad")`)
	if err == nil {
		t.Fatalf("expected hard parse error from HTTP import, got nil")
	}
	wantErrContains(t, err, "parse error")
}

// Filesystem ambiguity (mod.ms vs mod/init.ms) is a HARD error.
func Test_Module_ImportFile_Ambiguous_FileVsPackage_IsHardError(t *testing.T) {
	dir, done := withTempDir(t)
	defer done()
	defer chdir(t, dir)()

	_ = write(t, dir, "m.ms", `let x = 1`)
	if err := os.MkdirAll(filepath.Join(dir, "m"), 0o755); err != nil {
		t.Fatalf("mkdir: %v", err)
	}
	_ = write(t, filepath.Join(dir, "m"), "init.ms", `let x = 2`)

	ip, _ := NewInterpreter()
	_, err := ip.EvalSource(`import("m")`)
	if err == nil {
		t.Fatalf("expected hard error for ambiguous module, got nil")
	}
	wantErrContains(t, err, "ambiguous")
}

// Package entry resolves via init.ms and presents as the imported package name.
func Test_Module_ImportFile_PackageInit_Resolves_AndDisplaysAsPackage(t *testing.T) {
	dir, done := withTempDir(t)
	defer done()
	defer chdir(t, dir)()

	if err := os.MkdirAll(filepath.Join(dir, "p"), 0o755); err != nil {
		t.Fatalf("mkdir: %v", err)
	}
	_ = write(t, filepath.Join(dir, "p"), "init.ms", `let name = "p"`)

	ip, _ := NewInterpreter()
	v := evalWithIP(t, ip, `
let m = import("p")
m.name`)
	wantStr(t, v, "p")

	// Specs are treated as stems; no extensionless enforcement.
	// import("p.ms") probes "p.ms.ms" and "p.ms/init.ms" → not found.
	_, err := ip.EvalSource(`import("p.ms")`)
	if err == nil {
		t.Fatalf("expected hard error for missing module, got nil")
	}
	wantErrContains(t, err, "module not found")
	wantErrContains(t, err, "p.ms")
}

// HTTP ambiguity (<url>.ms vs <url>/init.ms) is a HARD error.
func Test_Module_ImportHTTP_Ambiguous_FileVsPackage_IsHardError(t *testing.T) {
	srv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		switch r.URL.Path {
		case "/amb.ms", "/amb/init.ms":
			_, _ = w.Write([]byte(`let x = 1`))
		default:
			http.NotFound(w, r)
		}
	}))
	defer srv.Close()

	ip, _ := NewInterpreter()
	_, err := ip.EvalSource(`import("` + srv.URL + `/amb")`)
	if err == nil {
		t.Fatalf("expected hard error for ambiguous http module, got nil")
	}
	wantErrContains(t, err, "ambiguous")
}

// Import resolution should prefer the importer's directory over CWD/stdlib.
func Test_Module_Resolution_PrefersImporterDir(t *testing.T) {
	dir, done := withTempDir(t)
	defer done()
	defer chdir(t, dir)()

	sub := filepath.Join(dir, "sub")
	if err := os.MkdirAll(sub, 0o755); err != nil {
		t.Fatalf("mkdir: %v", err)
	}

	_ = write(t, dir, "b.ms", `let x = 1`)
	_ = write(t, sub, "b.ms", `let x = 2`)
	_ = write(t, sub, "a.ms", `let b = import("b")
let y = b.x`)

	ip, _ := NewInterpreter()
	v := evalWithIP(t, ip, `
let a = import("sub/a")
a.y`)
	wantInt(t, v, 2)
}

// Failed inline module should not leave a stale "loading" record.
func Test_Module_Inline_Failure_DoesNotStaleCache(t *testing.T) {
	ip, _ := NewInterpreter()

	bad := `let m = module "M" do 1/0 end`

	if _, err := ip.EvalPersistentSource(bad); err == nil {
		t.Fatalf("expected runtime error on first construction")
	} else if msg := err.Error(); !strings.Contains(msg, "division by zero") {
		t.Fatalf("want division-by-zero error, got: %s", msg)
	} else if strings.Contains(msg, "import cycle") {
		t.Fatalf("unexpected import cycle on first attempt: %s", msg)
	}

	if _, err := ip.EvalPersistentSource(bad); err == nil {
		t.Fatalf("expected runtime error on second construction")
	} else if msg := err.Error(); !strings.Contains(msg, "division by zero") {
		t.Fatalf("want division-by-zero error again, got: %s", msg)
	} else if strings.Contains(msg, "import cycle") {
		t.Fatalf("BUG: stale modLoading caused false cycle on retry: %s", msg)
	}

	ok := `let m = module "M" do let x = 7 end m.x`
	v, err := ip.EvalPersistentSource(ok)
	if err != nil {
		t.Fatalf("unexpected error building module after prior failure: %v", err)
	}
	wantInt(t, v, 7)
}

// Successful inline modules are cached by canonical name.
func Test_Module_Inline_Success_CachesByName(t *testing.T) {
	ip, _ := NewInterpreter()

	v1, err := ip.EvalPersistentSource(`let m1 = module "C" do let x = 1 end m1.x`)
	if err != nil {
		t.Fatalf("unexpected error on first inline module: %v", err)
	}
	wantInt(t, v1, 1)

	v2, err := ip.EvalPersistentSource(`let m2 = module "C" do let x = 2 end m2.x`)
	if err != nil {
		t.Fatalf("unexpected error on second inline module: %v", err)
	}
	wantInt(t, v2, 1)
}
