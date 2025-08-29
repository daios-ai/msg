package mindscript

import (
	"os"
	"path/filepath"
	"sort"
	"strconv"
	"testing"
)

// msq quotes a Go string as a MindScript "Str" literal.
func msq(s string) string { return strconv.Quote(s) }

// assertStr asserts a VTStr with expected content.
func assertStr(t *testing.T, v Value, want string) {
	t.Helper()
	if v.Tag != VTStr {
		t.Fatalf("expected VTStr, got %#v", v)
	}
	if v.Data.(string) != want {
		t.Fatalf("got %q, want %q", v.Data.(string), want)
	}
}

// assertNullAnnotated asserts an annotated null (soft error), without checking the message.
func assertNullAnnotated(t *testing.T, v Value) {
	t.Helper()
	if v.Tag != VTNull || v.Annot == "" {
		t.Fatalf("expected annotated null, got %#v", v)
	}
}

// assertInt asserts a VTInt with expected value.
func assertInt(t *testing.T, v Value, want int64) {
	t.Helper()
	if v.Tag != VTInt {
		t.Fatalf("expected VTInt, got %#v", v)
	}
	if v.Data.(int64) != want {
		t.Fatalf("got %d, want %d", v.Data.(int64), want)
	}
}

// mustArray returns underlying []Value or fails.
func mustArray(t *testing.T, v Value) []Value {
	t.Helper()
	if v.Tag != VTArray {
		t.Fatalf("expected array; got %#v", v)
	}
	return v.Data.([]Value)
}

func Test_Builtin_Core_File_readFile_writeFile_RoundTrip(t *testing.T) {
	ip, _ := NewRuntime()
	dir := t.TempDir()
	path := filepath.Join(dir, "hello.txt")
	content := "héllo, 世界"

	src := `
		writeFile(` + msq(path) + `, ` + msq(content) + `)
		readFile(` + msq(path) + `)
	`
	v := evalWithIP(t, ip, src)
	assertStr(t, v, content)
}

func Test_Builtin_Core_File_OpenWrite_ReadBack(t *testing.T) {
	ip, _ := NewRuntime()
	dir := t.TempDir()
	path := filepath.Join(dir, "a.txt")

	src := `
		let h = open(` + msq(path) + `, "w")
		write(h, "abc")
		flush(h)
		close(h)
		readFile(` + msq(path) + `)
	`
	v := evalWithIP(t, ip, src)
	assertStr(t, v, "abc")
}

func Test_Builtin_Core_File_CloseFlushes(t *testing.T) {
	ip, _ := NewRuntime()
	dir := t.TempDir()
	path := filepath.Join(dir, "b.txt")

	src := `
		let h = open(` + msq(path) + `, "w")
		write(h, "xyz")
		# no flush here on purpose
		close(h)                  # should flush implicitly
		readFile(` + msq(path) + `)
	`
	v := evalWithIP(t, ip, src)
	assertStr(t, v, "xyz")
}

func Test_Builtin_Core_File_ReadN_EOF_Behavior(t *testing.T) {
	ip, _ := NewRuntime()
	dir := t.TempDir()
	path := filepath.Join(dir, "c.txt")
	_ = os.WriteFile(path, []byte("hello"), 0o644)

	src := `
		let h = open(` + msq(path) + `, "r")
		let a = readN(h, 2)
		let b = readN(h, 10)
		[a, b]
	`
	v := evalWithIP(t, ip, src)
	arr := mustArray(t, v)
	if len(arr) != 2 {
		t.Fatalf("want 2 results, got %d", len(arr))
	}
	assertStr(t, arr[0], "he")
	assertStr(t, arr[1], "llo")
}

func Test_Builtin_Core_File_ReadN_Negative_HardError(t *testing.T) {
	ip, _ := NewRuntime()
	dir := t.TempDir()
	path := filepath.Join(dir, "d.txt")
	_ = os.WriteFile(path, []byte("data"), 0o644)

	_, err := ip.EvalSource(`
		let h = open(` + msq(path) + `, "r")
		readN(h, -1)
	`)
	wantErrContains(t, err, "readN expects n >= 0")
}

func Test_Builtin_Core_File_ReadLine_Simple(t *testing.T) {
	ip, _ := NewRuntime()
	dir := t.TempDir()
	path := filepath.Join(dir, "e.txt")
	_ = os.WriteFile(path, []byte("a\nb\n"), 0o644)

	src := `
		let h = open(` + msq(path) + `, "r")
		let a = readLine(h)
		let b = readLine(h)
		let c = readLine(h)
		[a, b, c]
	`
	v := evalWithIP(t, ip, src)
	arr := mustArray(t, v)
	if len(arr) != 3 {
		t.Fatalf("want 3 results, got %d", len(arr))
	}
	assertStr(t, arr[0], "a")
	assertStr(t, arr[1], "b")
	if arr[2].Tag != VTNull { // EOF yields null (not annotated)
		t.Fatalf("expected null at EOF, got %#v", arr[2])
	}
}

func Test_Builtin_Core_File_OpenMode_ReadWrite_Contracts(t *testing.T) {
	ip, _ := NewRuntime()
	dir := t.TempDir()
	pathR := filepath.Join(dir, "r.txt")
	pathW := filepath.Join(dir, "w.txt")
	_ = os.WriteFile(pathR, []byte("r-only content"), 0o644)

	// Reading from "w" should hard-error ("not readable").
	_, err1 := ip.EvalSource(`
		let h = open(` + msq(pathW) + `, "w")
		readAll(h)
	`)
	wantErrContains(t, err1, "not readable")

	// Writing to "r" should hard-error ("not writable").
	_, err2 := ip.EvalSource(`
		let h = open(` + msq(pathR) + `, "r")
		write(h, "boom")
	`)
	wantErrContains(t, err2, "not writable")
}

func Test_Builtin_Core_File_OpenModes_TruncateAndAppend(t *testing.T) {
	ip, _ := NewRuntime()
	dir := t.TempDir()
	p := filepath.Join(dir, "modes.txt")

	// Start with some content
	_ = os.WriteFile(p, []byte("seed"), 0o644)

	// "w" truncates
	v1 := evalWithIP(t, ip, `
		let h = open(`+msq(p)+`, "w")
		write(h, "X")
		close(h)
		readFile(`+msq(p)+`)
	`)
	assertStr(t, v1, "X")

	// "a" appends
	v2 := evalWithIP(t, ip, `
		let h = open(`+msq(p)+`, "a")
		write(h, "Y")
		close(h)
		readFile(`+msq(p)+`)
	`)
	assertStr(t, v2, "XY")

	// "rw" allows read then write
	v3 := evalWithIP(t, ip, `
		let h = open(`+msq(p)+`, "rw")
		let before = readAll(h)
		write(h, "Z")
		flush(h)
		close(h)
		readFile(`+msq(p)+`)
	`)
	assertStr(t, v3, "XYZ")
}

func Test_Builtin_Core_File_dirList_ContainsFiles(t *testing.T) {
	ip, _ := NewRuntime()
	dir := t.TempDir()
	a := "a.txt"
	b := "b.txt"
	c := "subdir"
	_ = os.WriteFile(filepath.Join(dir, a), []byte("1"), 0o644)
	_ = os.WriteFile(filepath.Join(dir, b), []byte("2"), 0o644)
	_ = os.Mkdir(filepath.Join(dir, c), 0o755)

	v := evalWithIP(t, ip, `dirList(`+msq(dir)+`)`)
	arr := mustArray(t, v)
	names := make([]string, 0, len(arr))
	for _, it := range arr {
		if it.Tag != VTStr {
			t.Fatalf("dirList entries must be Str, got %#v", it)
		}
		names = append(names, it.Data.(string))
	}
	sort.Strings(names)
	// Expect filenames (directories are included by name; that's fine).
	if !contains(names, a) || !contains(names, b) || !contains(names, c) {
		t.Fatalf("dirList(%q) missing expected entries in %v", dir, names)
	}
}

func Test_Builtin_Core_File_readFile_NotExist_AnnotatedNull(t *testing.T) {
	ip, _ := NewRuntime()
	dir := t.TempDir()
	path := filepath.Join(dir, "nope.txt")

	v := evalWithIP(t, ip, `readFile(`+msq(path)+`)`)
	assertNullAnnotated(t, v)
}

func contains(xs []string, s string) bool {
	for _, x := range xs {
		if x == s {
			return true
		}
	}
	return false
}
