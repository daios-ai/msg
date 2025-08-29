package mindscript

import (
	"errors"
	"reflect"
	"testing"
)

// ---------- small helpers ----------

func vArray(elems ...Value) Value { return Arr(elems) }
func vStr(s string) Value         { return Str(s) }
func vInt(n int64) Value          { return Int(n) }
func vNum(f float64) Value        { return Num(f) }
func vBool(b bool) Value          { return Bool(b) }

func rtAnnotNull(msg string) Value {
	return vArray(
		vStr("annot"),
		vArray(vStr("str"), vStr(msg)),
		vArray(vStr("null")),
	)
}

func valueDeepEqual(a, b Value) bool {
	if a.Tag != b.Tag {
		return false
	}
	switch a.Tag {
	case VTNull:
		return true
	case VTBool, VTInt, VTNum, VTStr:
		return reflect.DeepEqual(a.Data, b.Data)
	case VTArray:
		ax := a.Data.([]Value)
		bx := b.Data.([]Value)
		if len(ax) != len(bx) {
			return false
		}
		for i := range ax {
			if !valueDeepEqual(ax[i], bx[i]) {
				return false
			}
		}
		return true
	default:
		// We only compare runtime-S (arrays/scalars) in these tests.
		return reflect.DeepEqual(a, b)
	}
}

func mustEqValue(t *testing.T, got, want Value) {
	t.Helper()
	if !valueDeepEqual(got, want) {
		t.Fatalf("\nVALUE MISMATCH\n got : %#v\n want: %#v", got, want)
	}
}

func mustErr(t *testing.T, err error) {
	t.Helper()
	if err == nil {
		t.Fatalf("expected error, got nil")
	}
}

// ---------- tests ----------

func TestIxToS_SoftErrorOnUnsupportedAtom(t *testing.T) {
	// Construct an AST S with an unsupported atom (int32).
	bad := S{"block", S{"int", int64(1)}, int32(7)}
	got := IxToS(bad)
	// We only assert that it is an annotated-null soft error.
	// Message text is implementation-defined but must be present.
	if got.Tag != VTArray {
		t.Fatalf("expected VTArray, got %v", got.Tag)
	}
	top := got.Data.([]Value)
	if len(top) != 3 || top[0].Tag != VTStr || top[0].Data.(string) != "annot" {
		t.Fatalf("expected annotated-null; got %#v", got)
	}
	// Final child must be ["null"]
	nullNode := top[2]
	wantNull := vArray(vStr("null"))
	mustEqValue(t, nullNode, wantNull)
}

func TestIxFromS_NoValidation_IxReifyHardFails(t *testing.T) {
	// Malformed runtime-S: ["array", ["int", ["weird"]]]
	rt := Arr([]Value{
		Str("array"),
		Arr([]Value{Str("int"), Arr([]Value{Str("weird")})}),
	})

	// 1) Structural decode should NOT error.
	s, err := IxFromS(rt)
	if err != nil {
		t.Fatalf("IxFromS should not validate by tag/arity; got error: %v", err)
	}

	// Structural sanity checks (no string formatting).
	if len(s) != 2 || s[0] != "array" {
		t.Fatalf("expected top-level ['array', ...], got %v", s)
	}
	elem, ok := s[1].(S)
	if !ok || len(elem) != 2 || elem[0] != "int" {
		t.Fatalf("expected ['int', ...] element, got %v", s[1])
	}
	weird, ok := elem[1].(S)
	if !ok || len(weird) != 1 || weird[0] != "weird" {
		t.Fatalf("expected ['weird'] as bogus payload, got %v", elem[1])
	}

	// 2) Reify/eval should hard-fail in the interpreter.
	ip := NewInterpreter()
	if _, err := ip.IxReify(rt); err == nil {
		t.Fatalf("IxReify should fail when evaluating malformed S (hard error), got nil")
	}
}

func TestReflect_ScalarsAndNegatives(t *testing.T) {
	cases := []struct {
		val  Value
		want Value
	}{
		{Null, vArray(vStr("null"))},
		{vBool(true), vArray(vStr("bool"), vBool(true))},
		{vInt(-2), vArray(vStr("int"), vInt(-2))},
		{vNum(-1.5), vArray(vStr("num"), vNum(-1.5))},
		{vStr("hi"), vArray(vStr("str"), vStr("hi"))},
	}
	for _, c := range cases {
		got := IxToS(ixMustCtorS(c.val))
		mustEqValue(t, got, c.want)
	}
}

func TestReflect_ArrayMap_WithRequirednessAndKeyAnn(t *testing.T) {
	mo := &MapObject{
		Entries: map[string]Value{},
		KeyAnn:  map[string]string{},
		Keys:    []string{},
	}
	// insertion order: a, b
	mo.Keys = append(mo.Keys, "a")
	mo.Entries["a"] = vInt(1)
	mo.KeyAnn["a"] = "!" // required

	mo.Keys = append(mo.Keys, "b")
	mo.Entries["b"] = vStr("x")
	mo.KeyAnn["b"] = "doc for b" // plain doc

	mv := Value{Tag: VTMap, Data: mo}

	got := IxReflect(mv) // constructor code → to runtime-S via IxToS
	want := vArray(
		vStr("map"),
		// pair! for required
		vArray(vStr("pair!"),
			vArray(vStr("str"), vStr("a")),
			vArray(vStr("int"), vInt(1)),
		),
		// pair with annotated key for docs
		vArray(vStr("pair"),
			vArray(vStr("annot"),
				vArray(vStr("str"), vStr("doc for b")),
				vArray(vStr("str"), vStr("b")),
			),
			vArray(vStr("str"), vStr("x")),
		),
	)
	mustEqValue(t, got, want)
}

func TestReflect_Fun_Native_Id(t *testing.T) {
	// Native function reflects to ["id", name]
	fn := &Fun{NativeName: "now"}
	nv := FunVal(fn)

	got := IxReflect(nv)
	want := vArray(vStr("id"), vStr("now"))
	mustEqValue(t, got, want)
}

func TestReflect_Fun_UserConstructor(t *testing.T) {
	f := &Fun{
		Params:     []string{"x"},
		ParamTypes: []S{S{"id", "Int"}},
		ReturnType: S{"id", "Int"},
		Body:       S{"binop", "+", S{"id", "x"}, S{"int", int64(1)}},
	}
	got := IxReflect(FunVal(f))
	// ["fun", ["array", ["pair", ["id","x"], ["id","Int"]]], ["id","Int"], <body>]
	want := vArray(
		vStr("fun"),
		vArray(
			vStr("array"),
			vArray(vStr("pair"),
				vArray(vStr("id"), vStr("x")),
				vArray(vStr("id"), vStr("Int")),
			),
		),
		vArray(vStr("id"), vStr("Int")),
		vArray(
			vStr("binop"), vStr("+"),
			vArray(vStr("id"), vStr("x")),
			vArray(vStr("int"), vInt(1)),
		),
	)
	mustEqValue(t, got, want)
}

func TestReflect_Type_AsConstructor(t *testing.T) {
	tv := TypeVal(S{"array", S{"id", "Int"}}) // Type value
	got := IxReflect(tv)
	want := vArray(
		vStr("type"),
		vArray(vStr("array"), vArray(vStr("id"), vStr("Int"))),
	)
	mustEqValue(t, got, want)
}

func TestReflect_Handle_SoftError(t *testing.T) {
	h := Value{Tag: VTHandle, Data: "opaque"}
	got := IxReflect(h)
	// We only assert it's an annotated-null soft error.
	top := got.Data.([]Value)
	if len(top) != 3 || top[0].Data.(string) != "annot" {
		t.Fatalf("expected annotated-null for handle; got %#v", got)
	}
}

func TestIxReify_SimpleProgram_Evaluates(t *testing.T) {
	ip := NewInterpreter()
	// Program: ("binop","+ ", 2, 3) ⇒ 5
	rt := vArray(
		vStr("binop"), vStr("+"),
		vArray(vStr("int"), vInt(2)),
		vArray(vStr("int"), vInt(3)),
	)
	val, err := ip.IxReify(rt)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if val.Tag != VTInt || val.Data.(int64) != 5 {
		t.Fatalf("want 5, got %#v", val)
	}
}

func TestIxReify_Rejects_ModuleAndHandle(t *testing.T) {
	ip := NewInterpreter()

	rtModule := vArray(
		vStr("module"),
		vArray(vStr("str"), vStr("math")),
		// empty exports ok syntactically
	)
	_, err := ip.IxReify(rtModule)
	mustErr(t, err)

	rtHandle := vArray(vStr("handle"))
	_, err = ip.IxReify(rtHandle)
	mustErr(t, err)
}

func TestRoundTrip_ToS_FromS(t *testing.T) {
	ast := S{
		"map",
		S{"pair", S{"str", "k"}, S{"array", S{"int", int64(1)}, S{"bool", true}}},
	}
	rt := IxToS(ast)
	back, err := IxFromS(rt)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if !equalS(ast, back) {
		t.Fatalf("round-trip mismatch\nast : %#v\nback: %#v", ast, back)
	}
}

// ---------- internals used by tests ----------

// ixMustCtorS returns constructor S for v or panics (tests expect success).
func ixMustCtorS(v Value) S {
	s, ok := ixConstructorS_ForValue(v)
	if !ok {
		panic(errors.New("no constructor S for value"))
	}
	return s
}
