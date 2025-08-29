package mindscript

import (
	"strings"
	"testing"
)

func Test_Builtin_Core_fail_and_try(t *testing.T) {
	ip, _ := NewRuntime()

	// fail(...) should surface a hard error.
	_, err := ip.EvalSource(`fail("boom")`)
	wantErrContains(t, err, "boom")

	// try(fun() do fail("boom") end) should capture as ok=false with error text.
	v := evalWithIP(t, ip, `
		let f = fun() do fail("boom") end
		try(f)
	`)
	m := mustMap(t, v)
	ok := m.Entries["ok"]
	errv := m.Entries["error"]
	val := m.Entries["value"]
	if ok.Tag != VTBool || ok.Data.(bool) != false {
		t.Fatalf("try.ok = false expected, got %#v", ok)
	}
	if errv.Tag != VTStr || !strings.Contains(errv.Data.(string), "boom") {
		t.Fatalf("try.error should contain boom, got %#v", errv)
	}
	if val.Tag != VTNull {
		t.Fatalf("try.value should be null on error, got %#v", val)
	}

	// try capturing an annotated-null (soft failure) from readFile(nonexistent)
	v2 := evalWithIP(t, ip, `
		let g = fun() do readFile("__definitely_not_exists__") end
		try(g)
	`)
	m2 := mustMap(t, v2)
	if m2.Entries["ok"].Data.(bool) != false {
		t.Fatalf("try.ok should be false for annotated null")
	}
	if m2.Entries["error"].Tag != VTStr {
		t.Fatalf("try.error should be Str for annotated null, got %#v", m2.Entries["error"])
	}
	if m2.Entries["value"].Tag != VTNull {
		t.Fatalf("try.value should be null for annotated null, got %#v", m2.Entries["value"])
	}

	// try on success
	v3 := evalWithIP(t, ip, `
		let h = fun() do 40 + 2 end
		try(h)
	`)
	m3 := mustMap(t, v3)
	if m3.Entries["ok"].Data.(bool) != true {
		t.Fatalf("try.ok should be true on success")
	}
	if m3.Entries["error"].Tag != VTNull {
		t.Fatalf("try.error should be null on success, got %#v", m3.Entries["error"])
	}
	if m3.Entries["value"].Tag != VTInt || m3.Entries["value"].Data.(int64) != 42 {
		t.Fatalf("try.value should be 42, got %#v", m3.Entries["value"])
	}
}

func Test_Builtin_Core_typeOf_and_isType_and_isSubtype(t *testing.T) {
	ip, _ := NewRuntime()

	// isType true/false paths
	v := evalWithIP(t, ip, `isType(42, type Int)`)
	if v.Tag != VTBool || v.Data.(bool) != true {
		t.Fatal("isType should return true for (42 : Int)")
	}
	v2 := evalWithIP(t, ip, `isType(42.0, type Int)`)
	if v2.Tag != VTBool || v2.Data.(bool) != false {
		t.Fatal("isType should return false for (42.0 : Int)")
	}

	// typeOf returns a Type
	tt := evalWithIP(t, ip, `typeOf([1, 2.0, "x"])`)
	if tt.Tag != VTType {
		t.Fatalf("typeOf should return VTType, got %#v", tt)
	}

	// isSubtype(Int, Num) = true
	sub := evalWithIP(t, ip, `isSubtype(type Int, type Num)`)
	if sub.Tag != VTBool || sub.Data.(bool) != true {
		t.Fatal("isSubtype(Int, Num) should be true")
	}

	// Contract violation: second arg must be a Type value (hard error).
	_, err := ip.EvalSource(`isType(42, Int)`)
	wantErrContains(t, err, "undefined variable")
}

func Test_Builtin_Core_clone_deepcopy(t *testing.T) {
	ip, _ := NewRuntime()

	// Clone must deep-copy arrays/maps and preserve order/annotations.
	// We mutate the original after cloning and expect the clone to remain unchanged.
	v := evalWithIP(t, ip, `
		do
			let orig = { a: { b: 1 }, c: 9 }
			let cp = clone(orig)
			orig.a = { b: 2 }   # mutate original
			cp
		end
	`)
	cp := mustMap(t, v)

	// cp.a.b must still be 1
	av, ok := cp.Entries["a"]
	if !ok || av.Tag != VTMap {
		t.Fatalf("clone: missing or wrong 'a' in clone: %#v", av)
	}
	bv, ok := av.Data.(*MapObject).Entries["b"]
	if !ok || bv.Tag != VTInt || bv.Data.(int64) != 1 {
		t.Fatalf("clone: nested value should remain 1, got %#v", bv)
	}
}

func Test_Builtin_Core_snapshot_returns_handle(t *testing.T) {
	ip, _ := NewRuntime()

	// Should return a VTHandle (opaque)
	v := evalWithIP(t, ip, `snapshot(null)`)
	if v.Tag != VTHandle || v.Data == nil {
		t.Fatalf("snapshot should return an opaque handle (VTHandle), got %#v", v)
	}
}

func Test_Builtin_Core_mapHas_and_mapDelete(t *testing.T) {
	ip, _ := NewRuntime()

	// mapHas present/absent
	tr := evalWithIP(t, ip, `mapHas({ a: 1 }, "a")`)
	if tr.Tag != VTBool || tr.Data.(bool) != true {
		t.Fatalf("mapHas should return true for existing key")
	}
	fa := evalWithIP(t, ip, `mapHas({ a: 1 }, "b")`)
	if fa.Tag != VTBool || fa.Data.(bool) != false {
		t.Fatalf("mapHas should return false for missing key")
	}

	// mapDelete should remove the key and preserve order of the rest.
	mv := evalWithIP(t, ip, `
		do
			let m = { a: 1, b: 2, c: 3 }
			mapDelete(m, "b")
			m
		end
	`)
	m := mustMap(t, mv)
	if _, ok := m.Entries["b"]; ok {
		t.Fatalf("mapDelete should remove key 'b'")
	}
	// Ensure order is [a, c]
	keys := m.Keys
	if len(keys) != 2 || keys[0] != "a" || keys[1] != "c" {
		t.Fatalf("mapDelete should preserve order of remaining keys [a,c], got %#v", keys)
	}
}

func Test_Builtin_Core_import_and_importCode_paths(t *testing.T) {
	ip, _ := NewRuntime()

	// import: with no loader configured, must return annotated null
	v := evalWithIP(t, ip, `import("does-not-exist")`)
	wantAnnotatedContains(t, v, "module not found")

	// importCode: parse error should be a hard error
	_, err := ip.EvalSource(`importCode("m", "let")`)
	wantErrContains(t, err, "parse error")

	// importCode: runtime failure during init becomes annotated null
	v2 := evalWithIP(t, ip, `importCode("m2", "fail(\"boom\")")`)
	wantAnnotatedContains(t, v2, "boom")

	// importCode: a simple valid module should succeed
	mod := evalWithIP(t, ip, `importCode("m3", "let answer = 42")`)
	if mod.Tag != VTModule {
		t.Fatalf("importCode should return a module (VTModule), got %#v", mod)
	}
}
