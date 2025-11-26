// encoding_url_builtins_test.go
package mindscript

import (
	"strings"
	"testing"
)

func arrStrs(t *testing.T, v Value) []string {
	t.Helper()
	if v.Tag != VTArray {
		t.Fatalf("expected array, got %#v", v)
	}
	xs := v.Data.(*ArrayObject).Elems
	out := make([]string, len(xs))
	for i, it := range xs {
		if it.Tag != VTStr {
			t.Fatalf("expected array of Str, got element %#v", it)
		}
		out[i] = it.Data.(string)
	}
	return out
}

func getStrField(t *testing.T, m *MapObject, k string) string {
	t.Helper()
	v, ok := m.Entries[k]
	if !ok || v.Tag != VTStr {
		t.Fatalf("missing or non-Str field %q: %#v", k, v)
	}
	return v.Data.(string)
}

func getOptIntField(t *testing.T, m *MapObject, k string) (int64, bool) {
	t.Helper()
	v, ok := m.Entries[k]
	if !ok || v.Tag == VTNull {
		return 0, false
	}
	if v.Tag != VTInt {
		t.Fatalf("field %q must be Int, got %#v", k, v)
	}
	return v.Data.(int64), true
}

func wantAnnotatedContains(t *testing.T, v Value, substr string) {
	t.Helper()
	if v.Tag != VTNull {
		t.Fatalf("want annotated null, got %#v", v)
	}
	if v.Annot == "" || !strings.Contains(strings.ToLower(v.Annot), strings.ToLower(substr)) {
		t.Fatalf("want annotated null containing %q, got %#v", substr, v)
	}
}

func Test_Builtin_Url_Enc_Base64_Roundtrip_And_Invalid(t *testing.T) {
	ip, _ := NewInterpreter()

	// Roundtrip simple ASCII
	v := evalWithIP(t, ip, `base64Decode(base64Encode("hello"))`)
	if v.Tag != VTStr || v.Data.(string) != "hello" {
		t.Fatalf("roundtrip failed, got %#v", v)
	}

	// Roundtrip with NUL and non-ASCII (U+00FF)
	v2 := evalWithIP(t, ip, `base64Decode(base64Encode("\u0000hi\u00FF"))`)
	if v2.Tag != VTStr || v2.Data.(string) != "\x00hi\xC3\xBF" && v2.Data.(string) != "\u0000hi\u00FF" {
		// Depending on internal representation; both reflect the same Unicode content.
		t.Fatalf("roundtrip (binary-ish) failed, got %#v", v2)
	}

	// Invalid input → annotated null
	v3 := evalWithIP(t, ip, `base64Decode("??")`)
	wantAnnotatedContains(t, v3, "invalid")
}

func Test_Builtin_Url_Enc_Hex_Roundtrip_And_Invalid(t *testing.T) {
	ip, _ := NewInterpreter()

	// Encode
	e := evalWithIP(t, ip, `hexEncode("AB")`)
	if e.Tag != VTStr || strings.ToLower(e.Data.(string)) != "4142" {
		t.Fatalf("hexEncode unexpected: %#v", e)
	}

	// Decode (case-insensitive)
	d := evalWithIP(t, ip, `hexDecode("4142")`)
	if d.Tag != VTStr || d.Data.(string) != "AB" {
		t.Fatalf("hexDecode unexpected: %#v", d)
	}
	d2 := evalWithIP(t, ip, `hexDecode("4f")`)
	if d2.Tag != VTStr || d2.Data.(string) != "O" {
		t.Fatalf("hexDecode case-insensitive failed: %#v", d2)
	}

	// Invalid hex (odd length)
	bad := evalWithIP(t, ip, `hexDecode("F")`)
	wantAnnotatedContains(t, bad, "invalid")

	// Invalid hex (non-hex chars)
	bad2 := evalWithIP(t, ip, `hexDecode("GG")`)
	wantAnnotatedContains(t, bad2, "invalid")
}

func Test_Builtin_Url_Enc_Query_Parse_Stringify_Roundtrip(t *testing.T) {
	ip, _ := NewInterpreter()

	// Parse with leading '?' and multiplicity; also percent-decoding
	v := evalWithIP(t, ip, `urlQueryParse("?a=1&a=2&b=x&c=%E2%9C%93")`)
	m := mustMap(t, v)

	av := m.Entries["a"]
	bv := m.Entries["b"]
	cv := m.Entries["c"]

	if got := arrStrs(t, av); len(got) != 2 || got[0] != "1" || got[1] != "2" {
		t.Fatalf("want a=[1,2], got %#v", av)
	}
	if got := arrStrs(t, bv); len(got) != 1 || got[0] != "x" {
		t.Fatalf("want b=[x], got %#v", bv)
	}
	if got := arrStrs(t, cv); len(got) != 1 || got[0] != "✓" {
		t.Fatalf("want c=[✓], got %#v", cv)
	}

	// Stringify accepts Str or [Str]; roundtrip via parse
	v2 := evalWithIP(t, ip, `urlQueryParse(urlQueryString({a: ["1","2"], b: "x"}))`)
	m2 := mustMap(t, v2)
	if got := arrStrs(t, m2.Entries["a"]); len(got) != 2 || got[0] != "1" || got[1] != "2" {
		t.Fatalf("roundtrip a failed, got %#v", m2.Entries["a"])
	}
	if got := arrStrs(t, m2.Entries["b"]); len(got) != 1 || got[0] != "x" {
		t.Fatalf("roundtrip b failed, got %#v", m2.Entries["b"])
	}
}

func Test_Builtin_Url_Enc_Parse_Struct(t *testing.T) {
	ip, _ := NewInterpreter()

	src := `urlParse("https://[2001:db8::1]:8443/a%20b?x=1&x=2&y=z#frag")`
	v := evalWithIP(t, ip, src)
	m := mustMap(t, v)

	if scheme := getStrField(t, m, "scheme"); scheme != "https" {
		t.Fatalf("scheme mismatch: %q", scheme)
	}
	if host := getStrField(t, m, "host"); host != "2001:db8::1" {
		t.Fatalf("host mismatch: %q", host)
	}
	if p, ok := getOptIntField(t, m, "port"); !ok || p != 8443 {
		t.Fatalf("port mismatch: %v %v", p, ok)
	}
	if path := getStrField(t, m, "path"); path != "/a%20b" {
		t.Fatalf("path mismatch: %q", path)
	}
	qv, ok := mget(m, "query")
	if !ok || qv.Tag != VTMap {
		t.Fatalf("query missing or not map: %#v", qv)
	}
	q := qv.Data.(*MapObject)
	ax := arrStrs(t, q.Entries["x"])
	if len(ax) != 2 || ax[0] != "1" || ax[1] != "2" {
		t.Fatalf("query x mismatch: %#v", q.Entries["x"])
	}
	ay := arrStrs(t, q.Entries["y"])
	if len(ay) != 1 || ay[0] != "z" {
		t.Fatalf("query y mismatch: %#v", q.Entries["y"])
	}
	if frag, ok := mget(m, "fragment"); !ok || frag.Tag != VTStr || frag.Data.(string) != "frag" {
		t.Fatalf("fragment mismatch: %#v", frag)
	}
}

func Test_Builtin_Url_Enc_Build_Struct(t *testing.T) {
	ip, _ := NewInterpreter()

	// Expect sorted keys and preserved value order for duplicates.
	src := `
urlBuild({
	scheme: "https",
	host: "2001:db8::1",
	port: 8443,
	path: "/a%20b",
	query: { x: ["1","2"], y: "z" },
	fragment: "frag"
})`
	v := evalWithIP(t, ip, src)
	if v.Tag != VTStr {
		t.Fatalf("urlBuild should return Str, got %#v", v)
	}
	want := "https://[2001:db8::1]:8443/a%20b?x=1&x=2&y=z#frag"
	if v.Data.(string) != want {
		t.Fatalf("urlBuild mismatch:\nwant %s\ngot  %s", want, v.Data.(string))
	}

	// Missing required fields → hard error
	_, err := ip.EvalSource(`urlBuild({host: "example.com"})`)
	wantErrContains(t, err, "missing required field: scheme")
	_, err = ip.EvalSource(`urlBuild({scheme: "https"})`)
	wantErrContains(t, err, "missing required field: host")
}

func Test_Builtin_Url_Enc_Roundtrip_Parse_Build(t *testing.T) {
	ip, _ := NewInterpreter()

	s := `https://example.com:8080/p/a%20t/h?b=1&a=2&a=3#frag`
	// Build from parse and ensure components are stable after re-parse.
	v := evalWithIP(t, ip, `urlBuild(urlParse("`+s+`"))`)
	if v.Tag != VTStr {
		t.Fatalf("urlBuild(parse) returned non-Str: %#v", v)
	}

	// Compare components via parse again.
	v1 := evalWithIP(t, ip, `urlParse("`+s+`")`)
	v2 := evalWithIP(t, ip, `urlParse(urlBuild(urlParse("`+s+`")))`)
	m1 := mustMap(t, v1)
	m2 := mustMap(t, v2)

	// scheme/host/port/path equality
	if getStrField(t, m1, "scheme") != getStrField(t, m2, "scheme") {
		t.Fatal("scheme changed after roundtrip")
	}
	if getStrField(t, m1, "host") != getStrField(t, m2, "host") {
		t.Fatal("host changed after roundtrip")
	}
	if p1, ok1 := getOptIntField(t, m1, "port"); p1 != 8080 || !ok1 {
		t.Fatalf("port parse failed: %v %v", p1, ok1)
	}
	if p2, ok2 := getOptIntField(t, m2, "port"); p2 != 8080 || !ok2 {
		t.Fatalf("port roundtrip failed: %v %v", p2, ok2)
	}
	if getStrField(t, m1, "path") != getStrField(t, m2, "path") {
		t.Fatal("path changed after roundtrip")
	}

	// Compare queries by serializing (Encode sorts keys)
	q1 := evalWithIP(t, ip, `urlQueryString(urlParse("`+s+`").query)`)
	q2 := evalWithIP(t, ip, `urlQueryString(urlParse(urlBuild(urlParse("`+s+`"))).query)`)
	if q1.Tag != VTStr || q2.Tag != VTStr || q1.Data.(string) != q2.Data.(string) {
		t.Fatalf("query changed after roundtrip: %q vs %q", q1, q2)
	}
}

func Test_Builtin_Url_Enc_Parse_And_Query_InvalidInputs(t *testing.T) {
	ip, _ := NewInterpreter()

	// Invalid URL (%zz in path)
	badURL := evalWithIP(t, ip, `urlParse("http://example.com/%zz")`)
	wantAnnotatedContains(t, badURL, "invalid")

	// Invalid query escape
	badQ := evalWithIP(t, ip, `urlQueryParse("a=%zz")`)
	wantAnnotatedContains(t, badQ, "invalid")
}
