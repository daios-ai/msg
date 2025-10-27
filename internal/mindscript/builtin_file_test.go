package mindscript

import (
	"os"
	"path/filepath"
	"sort"
	"strconv"
	"strings"
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
	return v.Data.(*ArrayObject).Elems
}

func contains(xs []string, s string) bool {
	for _, x := range xs {
		if x == s {
			return true
		}
	}
	return false
}

func Test_Builtin_Core_File_readFile_writeFile_RoundTrip(t *testing.T) {
	ip, _ := NewInterpreter()
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
	ip, _ := NewInterpreter()
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
	ip, _ := NewInterpreter()
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
	ip, _ := NewInterpreter()
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
	ip, _ := NewInterpreter()
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
	ip, _ := NewInterpreter()
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
	ip, _ := NewInterpreter()
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
	ip, _ := NewInterpreter()
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
	ip, _ := NewInterpreter()
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
	ip, _ := NewInterpreter()
	dir := t.TempDir()
	path := filepath.Join(dir, "nope.txt")

	v := evalWithIP(t, ip, `readFile(`+msq(path)+`)`)
	assertNullAnnotated(t, v)
}

// open: invalid mode -> type error from enum check
func Test_Builtin_File_Open_InvalidMode_TypeError(t *testing.T) {
	ip, _ := NewInterpreter()
	dir := t.TempDir()
	p := filepath.Join(dir, "x.txt")
	_, err := ip.EvalSource(`open(` + msq(p) + `, "x")`)
	// Param type checker fires before open() body.
	wantErrContains(t, err, "type mismatch in parameter 'mode'")
}

// write: byte count is returned (multibyte)
func Test_Builtin_File_Write_ByteCount_Multibyte(t *testing.T) {
	ip, _ := NewInterpreter()
	dir := t.TempDir()
	p := filepath.Join(dir, "b.txt")

	v := evalWithIP(t, ip, `
		let h = open(`+msq(p)+`, "w")
		write(h, "abc😊")   # 3 ASCII + 1 emoji (4 bytes) = 7 bytes
	`)
	assertInt(t, v, 7)
}

// flush: success path
func Test_Builtin_File_Flush_Success(t *testing.T) {
	ip, _ := NewInterpreter()
	dir := t.TempDir()
	p := filepath.Join(dir, "c.txt")
	v := evalWithIP(t, ip, `
		let h = open(`+msq(p)+`, "w")
		write(h, "ok")
		flush(h)
	`)
	if v.Tag != VTBool || v.Data.(bool) != true {
		t.Fatalf("expected Bool(true) from flush, got %#v", v)
	}
}

// readN: edge sizes (0 and exact size)
func Test_Builtin_File_ReadN_Zero_And_Exact(t *testing.T) {
	ip, _ := NewInterpreter()
	dir := t.TempDir()
	p := filepath.Join(dir, "d.txt")
	_ = os.WriteFile(p, []byte("hello"), 0o644)

	v := evalWithIP(t, ip, `
		let h = open(`+msq(p)+`, "r")
		let a = readN(h, 0)
		let b = readN(h, 5)
		[a, b]
	`)
	arr := mustArray(t, v)
	if len(arr) != 2 {
		t.Fatalf("want 2 results, got %d", len(arr))
	}
	assertStr(t, arr[0], "")
	assertStr(t, arr[1], "hello")
}

// readAll: empty file → ""
func Test_Builtin_File_ReadAll_EmptyFile(t *testing.T) {
	ip, _ := NewInterpreter()
	dir := t.TempDir()
	p := filepath.Join(dir, "e.txt")
	_ = os.WriteFile(p, []byte(""), 0o644)

	v := evalWithIP(t, ip, `
		let h = open(`+msq(p)+`, "r")
		readAll(h)
	`)
	assertStr(t, v, "")
}

// readLine: CRLF trimming
func Test_Builtin_File_ReadLine_CRLF(t *testing.T) {
	ip, _ := NewInterpreter()
	dir := t.TempDir()
	p := filepath.Join(dir, "f.txt")
	_ = os.WriteFile(p, []byte("a\r\nb\r\n"), 0o644)

	v := evalWithIP(t, ip, `
		let h = open(`+msq(p)+`, "r")
		let a = readLine(h)
		let b = readLine(h)
		let c = readLine(h)
		[a, b, c]
	`)
	arr := mustArray(t, v)
	if len(arr) != 3 {
		t.Fatalf("want 3 results, got %d", len(arr))
	}
	assertStr(t, arr[0], "a")
	assertStr(t, arr[1], "b")
	if arr[2].Tag != VTNull {
		t.Fatalf("expected null at EOF, got %#v", arr[2])
	}
}

// sprintf: mismatch returns marker string (current behavior) + valid case
func Test_Builtin_File_Sprintf_Mismatch_ReturnsMarker(t *testing.T) {
	ip, _ := NewInterpreter()

	vBad := evalWithIP(t, ip, `sprintf("%d", ["x"])`)
	if vBad.Tag != VTStr {
		t.Fatalf("expected VTStr marker, got %#v", vBad)
	}
	if !strings.Contains(vBad.Data.(string), "%!") {
		t.Fatalf("expected sprintf marker '%%!' in %q", vBad.Data.(string))
	}

	vOk := evalWithIP(t, ip, `sprintf("x=%d", [3])`)
	assertStr(t, vOk, "x=3")
}

// printf: returns printed string
func Test_Builtin_File_Printf_ReturnsString(t *testing.T) {
	ip, _ := NewInterpreter()
	v := evalWithIP(t, ip, `printf("", [])`)
	assertStr(t, v, "")
}

// writeFile: length results (empty and non-empty)
func Test_Builtin_File_WriteFile_Lengths(t *testing.T) {
	ip, _ := NewInterpreter()
	dir := t.TempDir()
	p1 := filepath.Join(dir, "g1.txt")
	p2 := filepath.Join(dir, "g2.txt")

	v1 := evalWithIP(t, ip, `writeFile(`+msq(p1)+`, "")`)
	assertInt(t, v1, 0)

	v2 := evalWithIP(t, ip, `writeFile(`+msq(p2)+`, "abc")`)
	assertInt(t, v2, 3)
}

// stat: file vs dir and missing path
func Test_Builtin_File_Stat_FileDirAndMissing(t *testing.T) {
	ip, _ := NewInterpreter()
	dir := t.TempDir()
	fp := filepath.Join(dir, "h.txt")
	_ = os.WriteFile(fp, []byte("hi"), 0o644)

	vDir := evalWithIP(t, ip, `stat(`+msq(dir)+`)`)
	if vDir.Tag != VTMap {
		t.Fatalf("stat(dir) must return map, got %#v", vDir)
	}
	if !vDir.Data.(*MapObject).Entries["isDir"].Data.(bool) {
		t.Fatalf("stat(dir).isDir must be true")
	}

	vFile := evalWithIP(t, ip, `stat(`+msq(fp)+`)`)
	if vFile.Tag != VTMap {
		t.Fatalf("stat(file) must return map, got %#v", vFile)
	}
	m := vFile.Data.(*MapObject).Entries
	if m["isDir"].Data.(bool) {
		t.Fatalf("stat(file).isDir must be false")
	}
	if m["size"].Data.(int64) != 2 {
		t.Fatalf("stat(file).size want 2, got %v", m["size"])
	}
	if m["mode"].Tag != VTInt || m["modTimeMillis"].Tag != VTInt {
		t.Fatalf("mode and modTimeMillis must be Ints")
	}

	vMissing := evalWithIP(t, ip, `stat("does-not-exist-xyz")`)
	assertNullAnnotated(t, vMissing)
}

// mkdir: creates parents
func Test_Builtin_File_Mkdir_Parents(t *testing.T) {
	ip, _ := NewInterpreter()
	base := t.TempDir()
	deep := filepath.Join(base, "a", "b", "c")

	v := evalWithIP(t, ip, `mkdir(`+msq(deep)+`)`)
	if v.Tag != VTBool || v.Data.(bool) != true {
		t.Fatalf("mkdir returned %#v, want Bool(true)", v)
	}
	// Confirm directory exists
	if st, err := os.Stat(deep); err != nil || !st.IsDir() {
		t.Fatalf("expected directory created: %v, err=%v", st, err)
	}
}

// rename + remove semantics
func Test_Builtin_File_Rename_And_Remove(t *testing.T) {
	ip, _ := NewInterpreter()
	dir := t.TempDir()
	oldP := filepath.Join(dir, "i_old.txt")
	newP := filepath.Join(dir, "i_new.txt")
	_ = os.WriteFile(oldP, []byte("x"), 0o644)

	v1 := evalWithIP(t, ip, `rename(`+msq(oldP)+`, `+msq(newP)+`)`)
	if v1.Tag != VTBool || v1.Data.(bool) != true {
		t.Fatalf("rename returned %#v, want Bool(true)", v1)
	}
	// old should be gone
	v2 := evalWithIP(t, ip, `stat(`+msq(oldP)+`)`)
	assertNullAnnotated(t, v2)

	// remove(non-empty dir) → annotated null (non-recursive)
	subdir := filepath.Join(dir, "nd")
	_ = os.Mkdir(subdir, 0o755)
	_ = os.WriteFile(filepath.Join(subdir, "k.txt"), []byte("k"), 0o644)
	v3 := evalWithIP(t, ip, `remove(`+msq(subdir)+`)`)
	assertNullAnnotated(t, v3)
}

// cwd / chdir / tempDir
func Test_Builtin_File_Cwd_Chdir_TempDir(t *testing.T) {
	ip, _ := NewInterpreter()
	dir := t.TempDir()
	sub := filepath.Join(dir, "sub")
	_ = os.Mkdir(sub, 0o755)

	// Save cwd, chdir to sub, verify, then go back.
	_, err := ip.EvalSource(`
		let old = cwd()
		chdir(` + msq(sub) + `)
		# check suffix via sprintf to avoid OS-specific separators in tests
		sprintf("%s", [cwd()])
		chdir(old)
	`)
	if err != nil {
		t.Fatalf("EvalSource error: %v", err)
	}
	// Just basic sanity on tempDir: non-empty string
	v := evalWithIP(t, ip, `tempDir()`)
	if v.Tag != VTStr || v.Data.(string) == "" {
		t.Fatalf("tempDir() must return non-empty Str, got %#v", v)
	}
}

// STDOUT: close (flush only) and flush explicitly
func Test_Builtin_File_STDOUT_CloseAndFlush(t *testing.T) {
	ip, _ := NewInterpreter()
	v1 := evalWithIP(t, ip, `close(STDOUT)`)
	if v1.Tag != VTBool || v1.Data.(bool) != true {
		t.Fatalf("close(STDOUT) expected Bool(true), got %#v", v1)
	}
	v2 := evalWithIP(t, ip, `flush(STDOUT)`)
	if v2.Tag != VTBool || v2.Data.(bool) != true {
		t.Fatalf("flush(STDOUT) expected Bool(true), got %#v", v2)
	}
}
