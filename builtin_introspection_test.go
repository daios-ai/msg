package mindscript

import (
	"strings"
	"testing"
)

// --- tiny local helpers (kept minimal to match suite style) ---

func filterOutUnderscore(keys []string) []string {
	out := make([]string, 0, len(keys))
	for _, k := range keys {
		if k == "_" {
			continue
		}
		out = append(out, k)
	}
	return out
}

// ----------------------------- tests -----------------------------

func Test_Builtin_Introspection_noteGet_and_noteSet(t *testing.T) {
	ip, _ := NewRuntime()

	// noteGet on unannotated value → null
	v := evalWithIP(t, ip, `noteGet(42)`)
	if v.Tag != VTNull {
		t.Fatalf("noteGet(42) should be null, got %#v", v)
	}

	// noteSet attaches; noteGet returns the text; value data remains intact
	v2 := evalWithIP(t, ip, `
		let x = noteSet("hello", 42)
		noteGet(x)
	`)
	if v2.Tag != VTStr || v2.Data.(string) != "hello" {
		t.Fatalf("noteGet should return 'hello', got %#v", v2)
	}

	// Works for composite values too (array)
	v3 := evalWithIP(t, ip, `
		let x = noteSet("arr", [1,2,3])
		noteGet(x)
	`)
	if v3.Tag != VTStr || v3.Data.(string) != "arr" {
		t.Fatalf("noteGet(array) should return 'arr', got %#v", v3)
	}
}

func Test_Builtin_Introspection_bindings_order_and_localOnly(t *testing.T) {
	ip, _ := NewRuntime()

	// Force a NEW FRAME via a function so localOnly=true shows only inner lets.
	v := evalWithIP(t, ip, `
		let a = 1
		let z = 9

		let inner = fun() do
			let b = 2
			let a = 3
			let m1 = bindings(true)   # current frame only
			let m2 = bindings(false)  # merged, inner → outer
			{ m1: m1, m2: m2 }
		end

		inner()
	`)
	m := mustMap(t, v)

	only := func(keys []string, allowed map[string]bool) []string {
		out := make([]string, 0, len(keys))
		for _, k := range keys {
			if k == "_" {
				continue
			}
			if allowed[k] {
				out = append(out, k)
			}
		}
		return out
	}

	// ----- m1: localOnly=true (inner frame only) -----
	m1v, ok := mget(m, "m1")
	if !ok {
		t.Fatalf("bindings(true) result 'm1' missing")
	}
	m1 := mustMap(t, m1v)

	m1Keys := only(m1.Keys, map[string]bool{"a": true, "b": true})
	if len(m1Keys) != 2 || m1Keys[0] != "a" || m1Keys[1] != "b" {
		t.Fatalf("bindings(true) keys want [a b], got %#v", m1Keys)
	}

	// ----- m2: merged (inner → outer) -----
	m2v, ok := mget(m, "m2")
	if !ok {
		t.Fatalf("bindings(false) result 'm2' missing")
	}
	m2 := mustMap(t, m2v)

	m2Keys := only(m2.Keys, map[string]bool{"a": true, "b": true, "z": true})
	if len(m2Keys) != 3 || m2Keys[0] != "a" || m2Keys[1] != "b" || m2Keys[2] != "z" {
		t.Fatalf("bindings(false) keys want [a b z], got %#v", m2Keys)
	}

	// Shadowing preserved: inner 'a' should win in merged view.
	av, ok := m2.Entries["a"]
	if !ok {
		t.Fatalf("bindings(false) missing key 'a'")
	}
	if av.Tag != VTInt || av.Data.(int64) != 3 {
		t.Fatalf("bindings(false).a should be 3 (inner), got %#v", av)
	}
}

func Test_Builtin_Introspection_astParse_and_astEval_roundtrip(t *testing.T) {
	ip, _ := NewRuntime()

	// Parse then eval simple expression
	v := evalWithIP(t, ip, `
		let ast = astParse("1 + 2")
		astEval(ast)
	`)
	if v.Tag != VTInt || v.Data.(int64) != 3 {
		t.Fatalf("astEval(astParse('1+2')) should be 3, got %#v", v)
	}

	// astParse error path returns annotated runtime-S: ["annot", ["str", ...], ["null"]]
	// Check first element equals "annot" by indexing from MindScript
	v2 := evalWithIP(t, ip, `
		let a = astParse("let")
		a[0]
	`)
	if v2.Tag != VTStr || v2.Data.(string) != "annot" {
		t.Fatalf("astParse error should return runtime-S starting with 'annot', got %#v", v2)
	}
}

func Test_Builtin_Introspection_astEval_operates_in_caller_scope(t *testing.T) {
	ip, _ := NewRuntime()

	// astEval should mutate caller's env (not a fresh child)
	v := evalWithIP(t, ip, `
		let x = 1
		let rt = astParse("x = x + 1")
		astEval(rt)
		x
	`)
	if v.Tag != VTInt || v.Data.(int64) != 2 {
		t.Fatalf("astEval should update caller scope; want x=2, got %#v", v)
	}
}

func Test_Builtin_Introspection_reflect_and_reify_roundtrip(t *testing.T) {
	ip, _ := NewRuntime()

	// Round-trip a composite value
	v := evalWithIP(t, ip, `
		let orig = {a: 1, b: [2,3]}
		let rt   = reflect(orig)
		let v2   = reify(rt)
		str(orig) == str(v2)
	`)
	if v.Tag != VTBool || v.Data.(bool) != true {
		t.Fatalf("reflect → reify should round-trip, got %#v", v)
	}
}

func Test_Builtin_Introspection_reify_persistent_scope(t *testing.T) {
	ip, _ := NewRuntime()

	// reify evaluates in persistent/global scope; definition should persist
	v := evalWithIP(t, ip, `
		let rt = astParse("let y = 7")
		reify(rt)
		y
	`)
	if v.Tag != VTInt || v.Data.(int64) != 7 {
		t.Fatalf("reify should install into Global; want y=7, got %#v", v)
	}
}

func Test_Builtin_Introspection_reify_malformed_is_hard_error(t *testing.T) {
	ip, _ := NewRuntime()

	// Use a DEFINITELY invalid runtime-S shape (missing payload):
	// "id" nodes require a name; ["id"] is malformed and IxFromS must hard-fail.
	_, err := ip.EvalSource(`reify(["id"])`)
	if err == nil {
		t.Fatalf("expected hard error from reify on malformed runtime-S")
	}
	lc := strings.ToLower(err.Error())
	if !strings.Contains(lc, "reify") && !strings.Contains(lc, "invalid") && !strings.Contains(lc, "id") {
		// Keep the check loose to tolerate phrasing changes.
		t.Fatalf("unexpected error from reify: %v", err)
	}
}
