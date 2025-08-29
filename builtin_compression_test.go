package mindscript

import (
	"os"
	"path/filepath"
	"strconv"
	"strings"
	"testing"
)

func Test_Builtin_Compression_RoundTrip_SmallText(t *testing.T) {
	ip, _ := NewRuntime()

	v := evalWithIP(t, ip, `
		let s = "hello, world"
		let z = gzipCompress(s)
		let u = gzipDecompress(z)
		u
	`)
	if v.Tag != VTStr || v.Data.(string) != "hello, world" {
		t.Fatalf("round-trip failed, got %#v", v)
	}
}

func Test_Builtin_Compression_RoundTrip_Empty(t *testing.T) {
	ip, _ := NewRuntime()

	v := evalWithIP(t, ip, `
		let s = ""
		let z = gzipCompress(s)
		let u = gzipDecompress(z)
		u
	`)
	if v.Tag != VTStr || v.Data.(string) != "" {
		t.Fatalf("empty round-trip failed, got %#v", v)
	}
}

func Test_Builtin_Compression_RoundTrip_UnicodeAndNUL(t *testing.T) {
	ip, _ := NewRuntime()

	// Uses NUL (\u0000) and 😀 as surrogate-pair escapes (\uD83D\uDE00).
	v := evalWithIP(t, ip, `
		let s = "A\u0000B\uD83D\uDE00C"
		let z = gzipCompress(s)
		let u = gzipDecompress(z)
		u
	`)
	want := "A\x00B😀C"
	if v.Tag != VTStr || v.Data.(string) != want {
		t.Fatalf("unicode+NUL round-trip failed, got %#v", v)
	}
}

func Test_Builtin_Compression_RoundTrip_LargeFile(t *testing.T) {
	ip, _ := NewRuntime()

	// Create a large temporary file (~512 KiB) with repetitive content.
	td := t.TempDir()
	path := filepath.Join(td, "big.txt")
	var b strings.Builder
	chunk := strings.Repeat("0123456789abcdef", 1024) // 16*1024 = 16384 bytes/chunk
	for i := 0; i < 32; i++ {                         // ~524288 bytes total
		b.WriteString(chunk)
	}
	if err := os.WriteFile(path, []byte(b.String()), 0644); err != nil {
		t.Fatalf("write temp file: %v", err)
	}

	// Read → compress → decompress → ensure equality.
	src := `let s = readFile(` + strconv.Quote(path) + `)
	        let z = gzipCompress(s)
	        let u = gzipDecompress(z)
	        u`
	v := evalWithIP(t, ip, src)
	if v.Tag != VTStr {
		t.Fatalf("expected VTStr, got %#v", v)
	}
	got := v.Data.(string)

	data, err := os.ReadFile(path)
	if err != nil {
		t.Fatalf("read back temp file: %v", err)
	}
	if got != string(data) {
		t.Fatalf("large round-trip mismatch: len(got)=%d len(want)=%d", len(got), len(data))
	}
}

func Test_Builtin_Compression_Decompress_InvalidInput(t *testing.T) {
	ip, _ := NewRuntime()

	// Invalid gzip payload should return annotated null (soft error).
	v := evalWithIP(t, ip, `gzipDecompress("not a gzip stream")`)
	wantAnnotatedContains(t, v, "gzip")
}

func Test_Builtin_Compression_Compress_ContractViolation(t *testing.T) {
	ip, _ := NewRuntime()

	// Type contract violation is caught by the interpreter's param checker.
	_, err := ip.EvalSource(`gzipCompress(42)`)
	wantErrContains(t, err, "type mismatch in parameter 'data'")
}

func Test_Builtin_Compression_Decompress_ContractViolation(t *testing.T) {
	ip, _ := NewRuntime()

	// Same here: native body isn't entered.
	_, err := ip.EvalSource(`gzipDecompress(123)`)
	wantErrContains(t, err, "type mismatch in parameter 'data'")
}
