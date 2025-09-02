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

func Test_Builtin_Core_snapshot_returns_env(t *testing.T) {
	ip, _ := NewRuntime()

	// Should now return a VTMap (flattened visible env, including builtins)
	v := evalWithIP(t, ip, `snapshot(null)`)
	if v.Tag != VTMap || v.Data == nil {
		t.Fatalf("snapshot should return a map (VTMap), got %#v", v)
	}

	mo, ok := v.Data.(*MapObject)
	if !ok {
		t.Fatalf("snapshot value.Data should be *MapObject, got %T", v.Data)
	}

	// Must include builtins from Core; check a couple of known ones.
	failVal, ok := mo.Entries["fail"]
	if !ok {
		t.Fatalf("snapshot should include builtins (missing 'fail')")
	}
	if failVal.Tag != VTFun {
		t.Fatalf("'fail' should be a function value, got %#v", failVal)
	}

	typeOfVal, ok := mo.Entries["typeOf"]
	if !ok {
		t.Fatalf("snapshot should include builtins (missing 'typeOf')")
	}
	if typeOfVal.Tag != VTFun {
		t.Fatalf("'typeOf' should be a function value, got %#v", typeOfVal)
	}

	// Per-key annotations should carry the variable's Value.Annot (docs set by setBuiltinDoc).
	if ann, ok := mo.KeyAnn["fail"]; !ok || ann == "" {
		t.Fatalf("'fail' should have a non-empty per-key annotation (doc), got %#v", ann)
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
	_, err2 := ip.EvalSource(`importCode("m2", "fail(\"boom\")")`)
	wantErrContains(t, err2, "boom")

	// importCode: a simple valid module should succeed
	mod := evalWithIP(t, ip, `importCode("m3", "let answer = 42")`)
	if mod.Tag != VTModule {
		t.Fatalf("importCode should return a module (VTModule), got %#v", mod)
	}
}

func Test_Builtin_Core_assign_builtin_friendly_error(t *testing.T) {
	ip, _ := NewRuntime()

	// Attempt to override a Core builtin without let → friendly error.
	_, err := ip.EvalSource(`import = 1`)
	wantErrContains(t, err, "cannot assign to builtin: import")
}

func Test_Builtin_Core_assign_undefined_in_sealed_global(t *testing.T) {
	ip, _ := NewRuntime()

	// Assign to an unknown name in a sealed Global should say "undefined variable".
	_, err := ip.EvalSource(`totally_unknown_name = 1`)
	wantErrContains(t, err, "undefined variable: totally_unknown_name")
}

func Test_Builtin_Core_let_shadow_builtin_ok(t *testing.T) {
	ip, _ := NewRuntime()

	// Shadowing a builtin with let is allowed (local to the eval scope).
	if _, err := ip.EvalSource(`let import = 123`); err != nil {
		t.Fatalf("let shadow of builtin should succeed, got error: %v", err)
	}

	// And the real builtin remains available in a fresh eval.
	if _, err := ip.EvalSource(`import("std")`); err != nil {
		t.Fatalf("builtin import should remain usable, got error: %v", err)
	}
}

func Test_Builtin_Core_assign_builtin_persistent_repl(t *testing.T) {
	ip, _ := NewRuntime()

	// In persistent (Global) scope, assigning to a Core name should also be blocked
	// with the friendly message.
	_, err := ip.EvalPersistentSource(`import = 1`)
	wantErrContains(t, err, "cannot assign to builtin: import")

	// But explicit let in Global is allowed (shadow locally in Global).
	if _, err := ip.EvalPersistentSource(`let import = 123`); err != nil {
		t.Fatalf("let shadow of builtin in Global should succeed, got error: %v", err)
	}
}
func Test_Builtin_Core_module_cannot_assign_builtin(t *testing.T) {
	ip, _ := NewRuntime()

	// Inline module attempts to overwrite a Core builtin → friendly error.
	_, err := ip.EvalSource(`module "M" do import = 1 end`)
	wantErrContains(t, err, "cannot assign to builtin: import")
}

func Test_Builtin_Core_module_let_shadow_builtin_ok(t *testing.T) {
	ip, _ := NewRuntime()

	// Inline module shadows a Core builtin with let → allowed.
	v := evalWithIP(t, ip, `module "M2" do let import = 123 end`)
	m := mustMap(t, AsMapValue(v))
	got := m.Entries["import"]
	if got.Tag != VTInt || got.Data.(int64) != 123 {
		t.Fatalf("module export import should be 123, got %#v", got)
	}

	// Core builtin remains intact after module evaluation.
	if _, err := ip.EvalSource(`import("std")`); err != nil {
		t.Fatalf("builtin import should remain usable: %v", err)
	}
}
