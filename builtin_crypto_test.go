package mindscript

import (
	"crypto/hmac"
	"crypto/sha256"
	"testing"
)

// mustStr extracts Go string from a MindScript Str Value or fails the test.
func mustStr(t *testing.T, v Value) string {
	t.Helper()
	if v.Tag != VTStr {
		t.Fatalf("expected VTStr, got %#v", v)
	}
	return v.Data.(string)
}

func Test_Builtin_Crypto_sha256_KnownVectors(t *testing.T) {
	ip, _ := NewRuntime()

	// sha256("") (empty)
	v0 := evalWithIP(t, ip, `sha256("")`)
	got0 := []byte(mustStr(t, v0))
	want0 := sha256.Sum256([]byte(""))
	if string(got0) != string(want0[:]) {
		t.Fatalf("sha256(\"\") mismatch")
	}

	// sha256("abc")
	v1 := evalWithIP(t, ip, `sha256("abc")`)
	got1 := []byte(mustStr(t, v1))
	want1 := sha256.Sum256([]byte("abc"))
	if string(got1) != string(want1[:]) {
		t.Fatalf("sha256(\"abc\") mismatch")
	}

	// sha256("The quick brown fox jumps over the lazy dog")
	v2 := evalWithIP(t, ip, `sha256("The quick brown fox jumps over the lazy dog")`)
	got2 := []byte(mustStr(t, v2))
	want2 := sha256.Sum256([]byte("The quick brown fox jumps over the lazy dog"))
	if string(got2) != string(want2[:]) {
		t.Fatalf("sha256(fox) mismatch")
	}
}

func Test_Builtin_Crypto_hmacSha256_KnownVectors(t *testing.T) {
	ip, _ := NewRuntime()

	// HMAC-SHA256("key", "The quick brown fox jumps over the lazy dog")
	v1 := evalWithIP(t, ip, `hmacSha256("key", "The quick brown fox jumps over the lazy dog")`)
	got1 := []byte(mustStr(t, v1))
	m1 := hmac.New(sha256.New, []byte("key"))
	_, _ = m1.Write([]byte("The quick brown fox jumps over the lazy dog"))
	want1 := m1.Sum(nil)
	if string(got1) != string(want1) {
		t.Fatalf("hmacSha256(key, fox) mismatch")
	}

	// HMAC-SHA256("key", "")
	v2 := evalWithIP(t, ip, `hmacSha256("key", "")`)
	got2 := []byte(mustStr(t, v2))
	m2 := hmac.New(sha256.New, []byte("key"))
	_, _ = m2.Write([]byte(""))
	want2 := m2.Sum(nil)
	if string(got2) != string(want2) {
		t.Fatalf("hmacSha256(key, empty) mismatch")
	}
}

func Test_Builtin_Crypto_ctEqual_Basic(t *testing.T) {
	ip, _ := NewRuntime()

	// Equal strings
	v1 := evalWithIP(t, ip, `ctEqual("abc", "abc")`)
	if v1.Tag != VTBool || v1.Data.(bool) != true {
		t.Fatalf("ctEqual should be true for equal inputs")
	}

	// Different by one char
	v2 := evalWithIP(t, ip, `ctEqual("abc", "abd")`)
	if v2.Tag != VTBool || v2.Data.(bool) != false {
		t.Fatalf("ctEqual should be false for different inputs")
	}

	// Different lengths
	v3 := evalWithIP(t, ip, `ctEqual("a", "aa")`)
	if v3.Tag != VTBool || v3.Data.(bool) != false {
		t.Fatalf("ctEqual should be false for different lengths")
	}

	// Works with raw bytes (compare sha256 outputs)
	vh1 := evalWithIP(t, ip, `sha256("x")`)
	vh2 := evalWithIP(t, ip, `sha256("x")`)
	vh3 := evalWithIP(t, ip, `sha256("y")`)
	ok1 := evalWithIP(t, ip, `ctEqual(sha256("x"), sha256("x"))`)
	ok2 := evalWithIP(t, ip, `ctEqual(sha256("x"), sha256("y"))`)
	if ok1.Tag != VTBool || !ok1.Data.(bool) {
		t.Fatalf("ctEqual(sha256(x), sha256(x)) should be true; got %#v / %#v", vh1, vh2)
	}
	if ok2.Tag != VTBool || ok2.Data.(bool) {
		t.Fatalf("ctEqual(sha256(x), sha256(y)) should be false; got %#v / %#v", vh1, vh3)
	}
}

func Test_Builtin_Crypto_randBytes_LengthAndError(t *testing.T) {
	ip, _ := NewRuntime()

	// n = 0 → empty string
	v0 := evalWithIP(t, ip, `randBytes(0)`)
	if mustStr(t, v0) != "" {
		t.Fatalf("randBytes(0) should return empty string")
	}

	// n = 32 → length is 32
	v32 := evalWithIP(t, ip, `randBytes(32)`)
	if l := len(mustStr(t, v32)); l != 32 {
		t.Fatalf("randBytes(32) length = %d, want 32", l)
	}

	// Hard error on negative n
	_, err := ip.EvalSource(`randBytes(-1)`)
	wantErrContains(t, err, "n must be >=")
}

func Test_Builtin_Crypto_randBytes_NonDeterminism(t *testing.T) {
	ip, _ := NewRuntime()

	// Two successive calls should almost surely differ
	v1 := evalWithIP(t, ip, `randBytes(32)`)
	v2 := evalWithIP(t, ip, `randBytes(32)`)
	if string(mustStr(t, v1)) == string(mustStr(t, v2)) {
		t.Fatalf("randBytes produced identical 32-byte outputs twice (extremely unlikely)")
	}
}
