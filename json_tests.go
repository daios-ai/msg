package mindscript

import (
	"encoding/json"
	"strings"
	"testing"
)

// -----------------------------
// jsonRepair: permissive parser
// -----------------------------

func canonJSONString(t *testing.T, v any) string {
	t.Helper()
	b, err := json.Marshal(v)
	if err != nil {
		t.Fatalf("marshal failed: %v", err)
	}
	return string(b)
}

func mustAsMap(t *testing.T, v any) map[string]any {
	t.Helper()
	m, ok := v.(map[string]any)
	if !ok {
		t.Fatalf("expected map[string]any, got %#v", v)
	}
	return m
}

func mustAsSlice(t *testing.T, v any) []any {
	t.Helper()
	a, ok := v.([]any)
	if !ok {
		t.Fatalf("expected []any, got %#v", v)
	}
	return a
}

func Test_JSON_FenceExtraction(t *testing.T) {
	src := "Prose above.\n\n```json\n{ \"a\": 1 }\n```\n\nProse below.\n"
	v, fixed, warns := jsonRepair(src)
	if len(warns) == 0 {
		t.Fatalf("expected at least one warning about fence extraction, got none")
	}
	if strings.TrimSpace(fixed) != `{"a":1}` {
		t.Fatalf("fixed = %q; want %q", fixed, `{"a":1}`)
	}
	m := mustAsMap(t, v)
	if got := canonJSONString(t, m); got != `{"a":1}` {
		t.Fatalf("value json %s; want %s", got, `{"a":1}`)
	}
}

func Test_JSON_CommentsAndTrailingCommas(t *testing.T) {
	src := `[
	  1, // one
	  2, /* two */
	]`
	v, fixed, _ := jsonRepair(src)
	if strings.Contains(fixed, "//") || strings.Contains(fixed, "/*") || strings.Contains(fixed, ",]") {
		t.Fatalf("comments/trailing commas not removed; fixed=%q", fixed)
	}
	arr := mustAsSlice(t, v)
	if canonJSONString(t, arr) != `[1,2]` {
		t.Fatalf("array got %v", arr)
	}
}

func Test_JSON_UnquotedKeysAndSingleQuotes(t *testing.T) {
	src := `{ a: 'x', b:'y' }`
	v, fixed, _ := jsonRepair(src)
	if !strings.Contains(fixed, `"a":`) || !strings.Contains(fixed, `"b":`) || strings.Contains(fixed, `'`) {
		t.Fatalf("keys not quoted or quotes not normalized; fixed=%q", fixed)
	}
	m := mustAsMap(t, v)
	if canonJSONString(t, m) != `{"a":"x","b":"y"}` && canonJSONString(t, m) != `{"b":"y","a":"x"}` {
		t.Fatalf("map got %v", m)
	}
}

func Test_JSON_NumberNormalization(t *testing.T) {
	src := `{ "n": [ .5, 1., +1 ] }`
	v, _, _ := jsonRepair(src)
	m := mustAsMap(t, v)
	a := mustAsSlice(t, m["n"])
	// Normalize by marshaling back to JSON string for stable compare.
	if canonJSONString(t, a) != `[0.5,1,1]` {
		t.Fatalf("numbers normalized got %v", a)
	}
}

func Test_JSON_NonFiniteToNull(t *testing.T) {
	src := `[NaN, Infinity, -Infinity]`
	v, fixed, _ := jsonRepair(src)
	if !strings.Contains(fixed, "null") {
		t.Fatalf("expected nulls in fixed output; fixed=%q", fixed)
	}
	a := mustAsSlice(t, v)
	if canonJSONString(t, a) != `[null,null,null]` {
		t.Fatalf("non-finite mapped to %v; want [null,null,null]", a)
	}
}

func Test_JSON_BracketBalancing_ObjectArray(t *testing.T) {
	// Missing closing ']' and a balanced '}' should be appended.
	src := `{"a": [1, 2, 3}`
	v, fixed, warns := jsonRepair(src)
	if !strings.HasSuffix(strings.TrimSpace(fixed), "]}") && !strings.HasSuffix(strings.TrimSpace(fixed), "}]") {
		// Depending on whitespace compaction
		t.Fatalf("expected appended closers in fixed; fixed=%q", fixed)
	}
	foundAppend := false
	for _, w := range warns {
		if strings.Contains(w, "appended") {
			foundAppend = true
			break
		}
	}
	if !foundAppend {
		t.Fatalf("expected an 'appended' warning, got %v", warns)
	}
	m := mustAsMap(t, v)
	a := mustAsSlice(t, m["a"])
	if canonJSONString(t, a) != `[1,2,3]` {
		t.Fatalf("balanced array got %v", a)
	}
}

func Test_JSON_UnmatchedClosersDropped(t *testing.T) {
	src := `]]  { "x": 1 }`
	v, fixed, warns := jsonRepair(src)
	if strings.Contains(fixed, "]") && strings.Index(fixed, "]") < strings.Index(fixed, "{") {
		t.Fatalf("leading unmatched closers should be dropped; fixed=%q", fixed)
	}
	dropped := false
	for _, w := range warns {
		if strings.Contains(w, "dropped unmatched") {
			dropped = true
			break
		}
	}
	if !dropped {
		t.Fatalf("expected 'dropped unmatched' warning, got %v", warns)
	}
	m := mustAsMap(t, v)
	if canonJSONString(t, m) != `{"x":1}` {
		t.Fatalf("map got %v", m)
	}
}

func Test_JSON_CloseUnterminatedString(t *testing.T) {
	src := `{"s":"hello}`
	v, fixed, warns := jsonRepair(src)
	closed := false
	for _, w := range warns {
		if strings.Contains(w, "closed unterminated string") {
			closed = true
			break
		}
	}
	if !closed {
		t.Fatalf("expected unterminated string to be closed; warns=%v", warns)
	}
	if !strings.Contains(fixed, `"s":"hello"`) {
		t.Fatalf("fixed should contain closed string; fixed=%q", fixed)
	}
	m := mustAsMap(t, v)
	if canonJSONString(t, m) != `{"s":"hello"}` {
		t.Fatalf("map got %v", m)
	}
}

func Test_JSON_DecodeErrorSurfaced(t *testing.T) {
	src := `not json at all`
	v, fixed, warns := jsonRepair(src)
	if v != nil {
		t.Fatalf("expected nil value on irreparable input; got %#v", v)
	}
	if strings.TrimSpace(fixed) == "" {
		t.Fatalf("fixed should not be empty")
	}
	found := false
	for _, w := range warns {
		if strings.HasPrefix(w, "decode error: ") {
			found = true
			break
		}
	}
	if !found {
		t.Fatalf("expected decode error warning; got %v", warns)
	}
}

func Test_JSON_UseNumberPreservesBigInt(t *testing.T) {
	// 2^53 + 1 cannot be represented exactly in float64.
	src := `{"n":9007199254740993}`
	v, _, _ := jsonRepair(src)
	m := mustAsMap(t, v)

	// The decoder uses UseNumber(), so numbers are json.Number by default.
	num, ok := m["n"].(json.Number)
	if !ok {
		t.Fatalf("expected json.Number, got %#v", m["n"])
	}
	if string(num) != "9007199254740993" {
		t.Fatalf("json.Number string = %q; want %q", string(num), "9007199254740993")
	}
}

func Test_JSON_KeyEqualsToColon(t *testing.T) {
	src := `{"a" = 1, "b"=2}`
	v, fixed, _ := jsonRepair(src)
	if strings.Contains(fixed, "=") {
		t.Fatalf("expected '=' to be normalized to ':'; fixed=%q", fixed)
	}
	m := mustAsMap(t, v)
	// Order not guaranteed
	if canonJSONString(t, m) != `{"a":1,"b":2}` && canonJSONString(t, m) != `{"b":2,"a":1}` {
		t.Fatalf("map got %v", m)
	}
}

func Test_JSON_LeadingPlusAndLeadingDotInArray(t *testing.T) {
	src := `[+3, .25, 1., +0]`
	v, _, _ := jsonRepair(src)
	a := mustAsSlice(t, v)
	if canonJSONString(t, a) != `[3,0.25,1,0]` {
		t.Fatalf("normalized numbers got %v", a)
	}
}

func Test_JSON_SmartQuotesNormalize(t *testing.T) {
	// “smart” quotes around keys/values
	src := "{" + "“k”" + ":" + "“v”" + "}"
	v, fixed, warns := jsonRepair(src)
	if !strings.Contains(fixed, `"k":"v"`) {
		t.Fatalf("smart quotes not normalized; fixed=%q", fixed)
	}
	if len(warns) == 0 {
		t.Fatalf("expected a normalization warning")
	}
	m := mustAsMap(t, v)
	if canonJSONString(t, m) != `{"k":"v"}` {
		t.Fatalf("map got %v", m)
	}
}

func Test_JSON_ExtractFirstBalancedRegion_NoFence(t *testing.T) {
	src := `
	blah blah
	start { "ok": true } end
	noise { "ignored": 1
	`
	v, fixed, warns := jsonRepair(src)
	if !strings.Contains(strings.Join(warns, ";"), "extracted first balanced JSON region") {
		t.Fatalf("expected balanced region extraction warning; got %v", warns)
	}
	m := mustAsMap(t, v)
	if canonJSONString(t, m) != `{"ok":true}` {
		t.Fatalf("map got %v", m)
	}
	if fixed != `{"ok":true}` {
		t.Fatalf("fixed=%q; want %q", fixed, `{"ok":true}`)
	}
}
func Test_JSON_TrailingCommaInObject(t *testing.T) {
	src := `{"a":1,}`
	v, fixed, _ := jsonRepair(src)
	if strings.Contains(fixed, ",}") {
		t.Fatalf("trailing comma not removed; fixed=%q", fixed)
	}
	m := mustAsMap(t, v)
	if canonJSONString(t, m) != `{"a":1}` {
		t.Fatalf("map got %v", m)
	}
}

func Test_JSON_DuplicateKeysKeepLast(t *testing.T) {
	src := `{"a":1,"a":2}`
	v, _, warns := jsonRepair(src)
	m := mustAsMap(t, v)
	if canonJSONString(t, m) != `{"a":2}` {
		t.Fatalf("duplicate key policy not 'keep last'; got %v", m)
	}
	found := false
	for _, w := range warns {
		if strings.Contains(w, "duplicate key") {
			found = true
			break
		}
	}
	if !found {
		t.Fatalf("expected duplicate key warning; got %v", warns)
	}
}

func Test_JSON_BooleansAndNullVariants(t *testing.T) {
	src := `{ ok: True, nope: False, none: None }`
	v, fixed, _ := jsonRepair(src)
	if !strings.Contains(fixed, `"ok":true`) || !strings.Contains(fixed, `"nope":false`) || !strings.Contains(fixed, `"none":null`) {
		t.Fatalf("did not normalize True/False/None; fixed=%q", fixed)
	}
	m := mustAsMap(t, v)
	if canonJSONString(t, m) != `{"none":null,"nope":false,"ok":true}` && canonJSONString(t, m) != `{"ok":true,"nope":false,"none":null}` {
		t.Fatalf("map got %v", m)
	}
}

func Test_JSON_NumberUnderscoresAndLeadingPlus(t *testing.T) {
	src := `{"nums":[1_000, +42, 3_2_1]}`
	v, fixed, _ := jsonRepair(src)
	if strings.Contains(fixed, "_") || strings.Contains(fixed, "+") {
		t.Fatalf("underscores/leading plus not normalized; fixed=%q", fixed)
	}
	m := mustAsMap(t, v)
	a := mustAsSlice(t, m["nums"])
	if canonJSONString(t, a) != `[1000,42,321]` {
		t.Fatalf("normalized numbers got %v", a)
	}
}

func Test_JSON_NonDecNumericsBecomeNull(t *testing.T) {
	// If policy is to reject non-JSON numeric literals, ensure they become null.
	src := `[0x10, 0b101, 0o77]`
	v, fixed, _ := jsonRepair(src)
	if strings.Contains(fixed, "0x") || strings.Contains(fixed, "0b") || strings.Contains(fixed, "0o") {
		t.Fatalf("non-dec numerics not normalized; fixed=%q", fixed)
	}
	a := mustAsSlice(t, v)
	if canonJSONString(t, a) != `[null,null,null]` {
		t.Fatalf("non-dec numerics mapped to %v; want [null,null,null]", a)
	}
}

func Test_JSON_UnquotedKeyWithDash(t *testing.T) {
	src := `{ my-key: 1 }`
	v, fixed, _ := jsonRepair(src)
	if !strings.Contains(fixed, `"my-key":`) {
		t.Fatalf("unquoted dashed key not quoted; fixed=%q", fixed)
	}
	m := mustAsMap(t, v)
	if canonJSONString(t, m) != `{"my-key":1}` {
		t.Fatalf("map got %v", m)
	}
}

func Test_JSON_InsertMissingCommas_Object(t *testing.T) {
	src := `{"a":1 "b":2}`
	v, fixed, _ := jsonRepair(src)
	if strings.Contains(fixed, `"a":1 "b":2`) {
		t.Fatalf("missing comma not inserted; fixed=%q", fixed)
	}
	m := mustAsMap(t, v)
	if canonJSONString(t, m) != `{"a":1,"b":2}` && canonJSONString(t, m) != `{"b":2,"a":1}` {
		t.Fatalf("map got %v", m)
	}
}

func Test_JSON_InsertMissingCommas_Array(t *testing.T) {
	src := `[1 2 3]`
	v, fixed, _ := jsonRepair(src)
	if strings.Contains(fixed, "1 2 3") {
		t.Fatalf("missing commas not inserted; fixed=%q", fixed)
	}
	a := mustAsSlice(t, v)
	if canonJSONString(t, a) != `[1,2,3]` {
		t.Fatalf("array got %v", a)
	}
}

func Test_JSON_StripBOM(t *testing.T) {
	src := "\uFEFF{ \"x\": 1 }"
	v, fixed, warns := jsonRepair(src)
	if !strings.Contains(strings.Join(warns, ";"), "BOM") {
		t.Fatalf("expected BOM warning; got %v", warns)
	}
	if strings.TrimSpace(fixed) != `{"x":1}` {
		t.Fatalf("fixed=%q", fixed)
	}
	m := mustAsMap(t, v)
	if canonJSONString(t, m) != `{"x":1}` {
		t.Fatalf("map got %v", m)
	}
}

func Test_JSON_CodeFenceNoLang(t *testing.T) {
	src := "```\n{\"a\":1}\n```"
	v, fixed, warns := jsonRepair(src)
	if len(warns) == 0 {
		t.Fatalf("expected fence warning, got none")
	}
	if strings.TrimSpace(fixed) != `{"a":1}` {
		t.Fatalf("fixed=%q", fixed)
	}
	m := mustAsMap(t, v)
	if canonJSONString(t, m) != `{"a":1}` {
		t.Fatalf("map got %v", m)
	}
}

func Test_JSON_KeyEqualsNormalizedInUnquotedKeys(t *testing.T) {
	src := `{ a = 1, b : 2 }`
	v, fixed, _ := jsonRepair(src)
	if strings.Contains(fixed, "=") {
		t.Fatalf("expected '=' normalized to ':'; fixed=%q", fixed)
	}
	m := mustAsMap(t, v)
	if canonJSONString(t, m) != `{"a":1,"b":2}` && canonJSONString(t, m) != `{"b":2,"a":1}` {
		t.Fatalf("map got %v", m)
	}
}

func Test_JSON_QuoteKeyAfterColonlessPair(t *testing.T) {
	src := `{ foo 1, bar 2 }`
	v, fixed, _ := jsonRepair(src)
	if strings.Contains(fixed, "foo 1") || strings.Contains(fixed, "bar 2") {
		t.Fatalf("key/value pairs not normalized; fixed=%q", fixed)
	}
	m := mustAsMap(t, v)
	if canonJSONString(t, m) != `{"foo":1,"bar":2}` && canonJSONString(t, m) != `{"bar":2,"foo":1}` {
		t.Fatalf("map got %v", m)
	}
}

func Test_JSON_TrimLeadingGarbage_NoFence(t *testing.T) {
	src := `noise noise [1,2] trailing`
	v, fixed, warns := jsonRepair(src)
	if !strings.Contains(strings.Join(warns, ";"), "extracted first balanced JSON region") {
		t.Fatalf("expected balanced region extraction warning; got %v", warns)
	}
	a := mustAsSlice(t, v)
	if canonJSONString(t, a) != `[1,2]` {
		t.Fatalf("array got %v", a)
	}
	if fixed != `[1,2]` {
		t.Fatalf("fixed=%q; want %q", fixed, `[1,2]`)
	}
}
