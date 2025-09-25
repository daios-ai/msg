package mindscript

import (
	"strings"
	"testing"
)

func Test_Builtin_Core_fail_and_try(t *testing.T) {
	ip, _ := NewRuntime()

	// fail(...) should surface a hard error when not wrapped in try.
	_, err := ip.EvalSource(`panic("boom")`)
	wantErrContains(t, err, "boom")

	// try(fun() do fail("boom") end) captures a PANIC → ok=false, value=annotated null.
	v := evalWithIP(t, ip, `
		let f = fun() do panic("boom") end
		try(f)
	`)
	m := mustMap(t, v)
	ok := m.Entries["ok"]
	val := m.Entries["value"]

	if ok.Tag != VTBool || ok.Data.(bool) != false {
		t.Fatalf("try.ok = false expected for panic, got %#v", ok)
	}
	if val.Tag != VTNull || !strings.Contains(val.Annot, "boom") {
		t.Fatalf("try.value should be annotated null containing 'boom' on panic, got %#v (annot=%q)", val, val.Annot)
	}

	// try capturing an annotated-null (soft failure) from readFile(nonexistent):
	// This is NOT a panic; ok should be true and value is the annotated null.
	v2 := evalWithIP(t, ip, `
		let g = fun() do readFile("__definitely_not_exists__") end
		try(g)
	`)
	m2 := mustMap(t, v2)
	ok2 := m2.Entries["ok"]
	val2 := m2.Entries["value"]

	if ok2.Tag != VTBool || ok2.Data.(bool) != true {
		t.Fatalf("try.ok should be true for non-panicking annotated-null, got %#v", ok2)
	}
	if val2.Tag != VTNull || val2.Annot == "" {
		t.Fatalf("try.value should be annotated null (soft error) on success path, got %#v (annot=%q)", val2, val2.Annot)
	}

	// try on success (non-null, no annotation)
	v3 := evalWithIP(t, ip, `
		let h = fun() do 40 + 2 end
		try(h)
	`)
	m3 := mustMap(t, v3)
	ok3 := m3.Entries["ok"]
	val3 := m3.Entries["value"]

	if ok3.Tag != VTBool || ok3.Data.(bool) != true {
		t.Fatalf("try.ok should be true on success, got %#v", ok3)
	}
	if val3.Tag != VTInt || val3.Data.(int64) != 42 {
		t.Fatalf("try.value should be 42, got %#v", val3)
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
	wantErrContains(t, err, "is a type expression")
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

	v := evalWithIP(t, ip, `snapshot(null)`)
	if v.Tag != VTMap || v.Data == nil {
		t.Fatalf("snapshot should return a map (VTMap), got %#v", v)
	}

	mo, ok := v.Data.(*MapObject)
	if !ok {
		t.Fatalf("snapshot value.Data should be *MapObject, got %T", v.Data)
	}

	// Builtins should be present and functions.
	failVal, ok := mo.Entries["panic"]
	if !ok {
		t.Fatalf("snapshot should include builtins (missing 'panic')")
	}
	if failVal.Tag != VTFun {
		t.Fatalf("'panic' should be a function value, got %#v", failVal)
	}

	typeOfVal, ok := mo.Entries["typeOf"]
	if !ok {
		t.Fatalf("snapshot should include builtins (missing 'typeOf')")
	}
	if typeOfVal.Tag != VTFun {
		t.Fatalf("'typeOf' should be a function value, got %#v", typeOfVal)
	}

	// Value annotations stay on the values; key annotations are independent.
	if failVal.Annot == "" {
		t.Fatalf("'fail' should carry docs in its value annotation (Annot); got empty")
	}
	if ann, ok := mo.KeyAnn["fail"]; ok && ann != "" {
		t.Fatalf("per-key annotation for 'fail' should be empty; got %q", ann)
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

func Test_Builtin_Core_push_smoke(t *testing.T) {
	ip, _ := NewRuntime()
	v := evalWithIP(t, ip, `
		let a = [1]
		push(a, 2)
		a
	`)
	if v.Tag != VTArray {
		t.Fatalf("expected array, got %#v", v)
	}
	elems := v.Data.(*ArrayObject).Elems
	if len(elems) != 2 || elems[0].Data.(int64) != 1 || elems[1].Data.(int64) != 2 {
		t.Fatalf("push should yield [1,2], got %#v", elems)
	}
}

func Test_Builtin_Core_unshift_smoke(t *testing.T) {
	ip, _ := NewRuntime()
	v := evalWithIP(t, ip, `
		let a = [2]
		unshift(a, 1)
		a
	`)
	if v.Tag != VTArray {
		t.Fatalf("expected array, got %#v", v)
	}
	elems := v.Data.(*ArrayObject).Elems
	if len(elems) != 2 || elems[0].Data.(int64) != 1 || elems[1].Data.(int64) != 2 {
		t.Fatalf("unshift should yield [1,2], got %#v", elems)
	}
}

func Test_Builtin_Core_pop_smoke_and_contract(t *testing.T) {
	ip, _ := NewRuntime()
	// normal pop
	v := evalWithIP(t, ip, `
		let a = [10, 20]
		let x = pop(a)
		[x, a]
	`)
	if v.Tag != VTArray {
		t.Fatalf("expected outer array, got %#v", v)
	}
	out := v.Data.(*ArrayObject).Elems
	if out[0].Tag != VTInt || out[0].Data.(int64) != 20 {
		t.Fatalf("pop should return 20, got %#v", out[0])
	}
	arr := out[1]
	elems := arr.Data.(*ArrayObject).Elems
	if len(elems) != 1 || elems[0].Data.(int64) != 10 {
		t.Fatalf("array after pop should be [10], got %#v", elems)
	}
	// contractual error on empty
	_, err := ip.EvalSource(`pop([])`)
	wantErrContains(t, err, "pop on empty array")
}

func Test_Builtin_Core_shift_smoke_and_contract(t *testing.T) {
	ip, _ := NewRuntime()
	// normal shift
	v := evalWithIP(t, ip, `
		let a = [10, 20]
		let x = shift(a)
		[x, a]
	`)
	if v.Tag != VTArray {
		t.Fatalf("expected outer array, got %#v", v)
	}
	out := v.Data.(*ArrayObject).Elems
	if out[0].Tag != VTInt || out[0].Data.(int64) != 10 {
		t.Fatalf("shift should return 10, got %#v", out[0])
	}
	arr := out[1]
	elems := arr.Data.(*ArrayObject).Elems
	if len(elems) != 1 || elems[0].Data.(int64) != 20 {
		t.Fatalf("array after shift should be [20], got %#v", elems)
	}
	// contractual error on empty
	_, err := ip.EvalSource(`shift([])`)
	wantErrContains(t, err, "shift on empty array")
}
