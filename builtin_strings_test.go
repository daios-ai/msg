package mindscript

import (
	"strings"
	"testing"
)

func Test_Builtin_Strings_Substr_Clamp_And_Unicode(t *testing.T) {
	ip, _ := NewRuntime()

	out := evalWithIP(t, ip, `
      {
        u: substr("héllo", 1, 4),
        a: substr("abc", -5, 2),
        b: substr("abc", 2, 1),
        c: substr("abc", 1, 99),
        d: substr("abc", 99, 100)
      }
    `)
	m := entriesOf(t, out)

	if got := m["u"].Data.(string); got != "éll" {
		t.Fatalf(`substr unicode failed: got %q`, got)
	}
	if got := m["a"].Data.(string); got != "ab" {
		t.Fatalf(`substr clamp(start<0) failed: got %q`, got)
	}
	if got := m["b"].Data.(string); got != "" {
		t.Fatalf(`substr when j<i should be "": got %q`, got)
	}
	if got := m["c"].Data.(string); got != "bc" {
		t.Fatalf(`substr clamp(end>len) failed: got %q`, got)
	}
	if got := m["d"].Data.(string); got != "" {
		t.Fatalf(`substr when i>len should be "": got %q`, got)
	}
}

func Test_Builtin_Strings_Trim_UnicodeSpace(t *testing.T) {
	ip, _ := NewRuntime()

	out := evalWithIP(t, ip, `
      {
        strip:  strip("\u2003 x \u2003"),
        lstrip: lstrip("\u2003x  "),
        rstrip: rstrip("x\u2003")
      }
    `)
	m := entriesOf(t, out)

	if got := m["strip"].Data.(string); got != "x" {
		t.Fatalf("strip failed: got %q", got)
	}
	if got := m["lstrip"].Data.(string); got != "x  " {
		t.Fatalf("lstrip failed: got %q", got)
	}
	if got := m["rstrip"].Data.(string); got != "x" {
		t.Fatalf("rstrip failed: got %q", got)
	}
}

func Test_Builtin_Strings_Split_Empty_And_NotFound(t *testing.T) {
	ip, _ := NewRuntime()

	out := evalWithIP(t, ip, `
      {
        runes: split("hé", ""),
        none:  split("abc", "|")
      }
    `)
	m := entriesOf(t, out)

	runes := m["runes"]
	if runes.Tag != VTArray {
		t.Fatalf("split should return array, got %#v", runes)
	}
	rs := runes.Data.([]Value)
	// Expect exactly the code points, no empty strings at the ends.
	if len(rs) != 2 ||
		rs[0].Tag != VTStr || rs[0].Data.(string) != "h" ||
		rs[1].Tag != VTStr || rs[1].Data.(string) != "é" {
		t.Fatalf(`split("", "hé") should produce ["h","é"], got %#v`, runes)
	}

	none := m["none"]
	if none.Tag != VTArray || len(none.Data.([]Value)) != 1 ||
		none.Data.([]Value)[0].Data.(string) != "abc" {
		t.Fatalf(`split on absent sep should return ["abc"], got %#v`, none)
	}
}

func Test_Builtin_Strings_Join_Basics(t *testing.T) {
	ip, _ := NewRuntime()

	out := evalWithIP(t, ip, `
      {
        hyphen: join(["a","b","c"], "-"),
        empty:  join(["x","y"], "")
      }
    `)
	m := entriesOf(t, out)

	if got := m["hyphen"].Data.(string); got != "a-b-c" {
		t.Fatalf("join with '-' failed: got %q", got)
	}
	if got := m["empty"].Data.(string); got != "xy" {
		t.Fatalf("join with '' failed: got %q", got)
	}
}

func Test_Builtin_Strings_ToLower_ToUpper(t *testing.T) {
	ip, _ := NewRuntime()

	out := evalWithIP(t, ip, `
      {
        lo: toLower("HeLLo"),
        up: toUpper("HeLLo")
      }
    `)
	m := entriesOf(t, out)

	if m["lo"].Tag != VTStr || m["lo"].Data.(string) != "hello" {
		t.Fatalf("toLower failed: %#v", m["lo"])
	}
	if m["up"].Tag != VTStr || m["up"].Data.(string) != "HELLO" {
		t.Fatalf("toUpper failed: %#v", m["up"])
	}
}

func Test_Builtin_Strings_Regex_Match_And_Replace(t *testing.T) {
	ip, _ := NewRuntime()

	vm := evalWithIP(t, ip, `match("[a-z]+", "a12bc3")`)
	if vm.Tag != VTArray {
		t.Fatalf("match should return array, got %#v", vm)
	}
	xs := vm.Data.([]Value)
	if len(xs) != 2 || xs[0].Tag != VTStr || xs[1].Tag != VTStr ||
		xs[0].Data.(string) != "a" || xs[1].Data.(string) != "bc" {
		t.Fatalf("match results wrong: %#v", vm)
	}

	vr := evalWithIP(t, ip, `replace("[0-9]+", "#", "abc123def45")`)
	if vr.Tag != VTStr || vr.Data.(string) != "abc#def#" {
		t.Fatalf("replace wrong, got %#v", vr)
	}
}

func Test_Builtin_Strings_Regex_InvalidPattern_ReturnTypeMismatch(t *testing.T) {
	ip, _ := NewRuntime()

	// Current implementation returns annotated null on compile error,
	// but the declared return types are non-optional, so the runtime
	// surfaces a HARD error: "return type mismatch".
	_, err := ip.EvalSource(`match("[", "abc")`)
	wantErrContains(t, err, "return type mismatch")

	_, err = ip.EvalSource(`replace("(", "#", "abc")`)
	wantErrContains(t, err, "return type mismatch")

	// Sanity: a successful call still returns Str
	ok := evalWithIP(t, ip, `replace("a+", "X", "caaab")`)
	if ok.Tag != VTStr || !strings.Contains(ok.Data.(string), "X") {
		t.Fatalf("replace sanity check failed: %#v", ok)
	}
}

func Test_Builtin_Strings_Smoke_Combined(t *testing.T) {
	ip, _ := NewRuntime()

	out := evalWithIP(t, ip, `
      {
        sub: substr("héllo", 1, 4),
        st:  strip("  x  "),
        ls:  lstrip("  x  "),
        rs:  rstrip("x  "),
        sp:  split("a,b,c", ","),
        jn:  join(["a","b","c"], "-")
      }
    `)
	m := entriesOf(t, out)

	if m["sub"].Data.(string) != "éll" {
		t.Fatalf("substr failed: %#v", m["sub"])
	}
	if m["st"].Data.(string) != "x" || m["ls"].Data.(string) != "x  " || m["rs"].Data.(string) != "x" {
		t.Fatalf("strip/lstrip/rstrip failed: st=%#v ls=%#v rs=%#v", m["st"], m["ls"], m["rs"])
	}
	if sp := m["sp"]; sp.Tag != VTArray || len(sp.Data.([]Value)) != 3 {
		t.Fatalf("split failed: %#v", sp)
	}
	if m["jn"].Data.(string) != "a-b-c" {
		t.Fatalf("join failed: %#v", m["jn"])
	}
}
