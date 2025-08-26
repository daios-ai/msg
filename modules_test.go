// modules_autoload_test.go
package mindscript

import (
	"net/http"
	"net/http/httptest"
	"os"
	"path/filepath"
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
	ip := NewRuntime()

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

	ip := NewRuntime()

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

	ip := NewRuntime()
	v := evalWithIP(t, ip, `
let a = import("a")
a.y`)
	wantInt(t, v, 12)
}

// Search via MINDSCRIPT_PATH when not found in importer dir or CWD.
func Test_FileImport_Search_MINDSCRIPT_PATH(t *testing.T) {
	lib, done := withTempDir(t)
	defer done()

	_ = write(t, lib, "util.ms", `let name = "Bob"`)

	// Empty CWD, rely on `MindScriptPath`
	old := os.Getenv(MindScriptPath)
	_ = os.Setenv(MindScriptPath, lib)
	defer os.Setenv(MindScriptPath, old)

	ip := NewRuntime()
	v := evalWithIP(t, ip, `
let u = import("util")
u.name`)
	wantStr(t, v, "Bob")
}

// Two modules importing each other -> annotated cycle.
func Test_FileImport_Cycle_TwoModules(t *testing.T) {
	dir, done := withTempDir(t)
	defer done()
	defer chdir(t, dir)()

	_ = write(t, dir, "A.ms", `let b = import("B")`)
	_ = write(t, dir, "B.ms", `let a = import("A")`)

	ip := NewRuntime()
	v := evalWithIP(t, ip, `import("A")`)
	wantAnnotatedNullContains(t, v, "import cycle")
	wantAnnotatedNullContains(t, v, "A -> B -> A")
}

// Parse/runtime errors surface the file path and message.
func Test_FileImport_Parse_And_Runtime_Errors(t *testing.T) {
	dir, done := withTempDir(t)
	defer done()
	defer chdir(t, dir)()

	_ = write(t, dir, "bad.ms", `let x = (1 +`)
	_ = write(t, dir, "boom.ms", `1 / 0`)

	ip := NewRuntime()

	p := evalWithIP(t, ip, `import("bad")`)
	wantAnnotatedNullContains(t, p, "parse error")
	wantAnnotatedNullContains(t, p, "bad.ms")

	r := evalWithIP(t, ip, `import("boom")`)
	wantAnnotatedNullContains(t, r, "runtime error")
	wantAnnotatedNullContains(t, r, "boom.ms")
	wantAnnotatedNullContains(t, r, "division by zero")
}

// --- NEW behavior: modules are map-like & writable --------------------------

// Writable module fields, and NO namespace collision with caller globals.
func Test_Module_MapLike_Writable_NoNamespaceCollision(t *testing.T) {
	dir, done := withTempDir(t)
	defer done()
	defer chdir(t, dir)()

	_ = write(t, dir, "m.ms", `
let x = 2
`)

	ip := NewRuntime()
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

	ip := NewRuntime()
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

	ip := NewRuntime()

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

	ip := NewRuntime()
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

	ip := NewRuntime()
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

	ip := NewRuntime()
	// Import should not define "hidden" in the caller's env.
	v := evalWithIP(t, ip, `
let _ = import("mod")
hidden`)
	wantAnnotatedNullContains(t, v, "undefined variable")
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

	ip := NewRuntime()

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
