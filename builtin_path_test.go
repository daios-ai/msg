// path_builtins_test.go
package mindscript

import (
	"fmt"
	"os"
	"path/filepath"
	"strconv"
	"testing"
)

// wantStrVal asserts v is a VTStr with the expected content.
func wantStrVal(t *testing.T, v Value, want string) {
	t.Helper()
	if v.Tag != VTStr {
		t.Fatalf("want VTStr, got %v (%#v)", v.Tag, v)
	}
	if got := v.Data.(string); got != want {
		t.Fatalf("str mismatch:\n  got:  %q\n  want: %q", got, want)
	}
}

func Test_PathBuiltins_pathJoin(t *testing.T) {
	ip, _ := NewRuntime()

	// Simple join
	v := evalWithIP(t, ip, `pathJoin(["a", "b", "c"])`)
	wantStrVal(t, v, filepath.Join("a", "b", "c"))

	// Join with empty and dot segments (filepath.Join cleans)
	v = evalWithIP(t, ip, `pathJoin(["", "a", ".", "b", "..", "c"])`)
	wantStrVal(t, v, filepath.Join("", "a", ".", "b", "..", "c"))

	// Contract: non-array → parameter type mismatch (caught by native signature)
	_, err := ip.EvalSource(`pathJoin(42)`)
	wantErrContains(t, err, "type mismatch in parameter 'parts'")

	// Contract: wrong element type also mismatches the parameter (array of Str)
	_, err = ip.EvalSource(`pathJoin(["ok", 1, "x"])`)
	wantErrContains(t, err, "type mismatch in parameter 'parts'")
}

func Test_PathBuiltins_pathBase(t *testing.T) {
	ip, _ := NewRuntime()

	// Empty path mirrors filepath.Base("")
	v := evalWithIP(t, ip, `pathBase("")`)
	wantStrVal(t, v, filepath.Base(""))

	// Normal path
	p := filepath.Join("foo", "bar", "baz.txt")
	code := fmt.Sprintf(`pathBase(%s)`, strconv.Quote(p))
	v = evalWithIP(t, ip, code)
	wantStrVal(t, v, filepath.Base(p))

	// Trailing separator is ignored
	dir := filepath.Join("foo", "bar") + string(os.PathSeparator)
	code = fmt.Sprintf(`pathBase(%s)`, strconv.Quote(dir))
	v = evalWithIP(t, ip, code)
	wantStrVal(t, v, filepath.Base(dir))

	// Contract: wrong type → parameter mismatch
	_, err := ip.EvalSource(`pathBase(123)`)
	wantErrContains(t, err, "type mismatch in parameter 'path'")
}

func Test_PathBuiltins_pathDir(t *testing.T) {
	ip, _ := NewRuntime()

	// Empty path
	v := evalWithIP(t, ip, `pathDir("")`)
	wantStrVal(t, v, filepath.Dir(""))

	// Typical path
	p := filepath.Join("alpha", "beta", "gamma.txt")
	code := fmt.Sprintf(`pathDir(%s)`, strconv.Quote(p))
	v = evalWithIP(t, ip, code)
	wantStrVal(t, v, filepath.Dir(p))

	// Contract: wrong type → parameter mismatch
	_, err := ip.EvalSource(`pathDir(null)`)
	wantErrContains(t, err, "type mismatch in parameter 'path'")
}

func Test_PathBuiltins_pathExt(t *testing.T) {
	ip, _ := NewRuntime()

	// A few common cases
	for _, s := range []string{
		"file.txt",
		"archive.tar.gz",
		".bashrc",
		"noext",
		"trailingdot.",
	} {
		code := fmt.Sprintf(`pathExt(%s)`, strconv.Quote(s))
		v := evalWithIP(t, ip, code)
		wantStrVal(t, v, filepath.Ext(s))
	}

	// Contract: wrong type → parameter mismatch
	_, err := ip.EvalSource(`pathExt({})`)
	wantErrContains(t, err, "type mismatch in parameter 'path'")
}

func Test_PathBuiltins_pathClean(t *testing.T) {
	ip, _ := NewRuntime()

	// Relative cleanup
	s := filepath.Join("a", ".", "b", "..", "c", ".", ".", "d", "..")
	code := fmt.Sprintf(`pathClean(%s)`, strconv.Quote(s))
	v := evalWithIP(t, ip, code)
	wantStrVal(t, v, filepath.Clean(s))

	// Already clean
	s2 := filepath.Join("x", "y", "z")
	code = fmt.Sprintf(`pathClean(%s)`, strconv.Quote(s2))
	v = evalWithIP(t, ip, code)
	wantStrVal(t, v, filepath.Clean(s2))

	// Contract: wrong type → parameter mismatch
	_, err := ip.EvalSource(`pathClean([] )`)
	wantErrContains(t, err, "type mismatch in parameter 'path'")
}
