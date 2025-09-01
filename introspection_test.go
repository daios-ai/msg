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

func Test_Introspection_ScalarsAndNegatives(t *testing.T) {
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

func Test_Introspection_ArrayMap_WithRequirednessAndKeyAnn(t *testing.T) {
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

func Test_Introspection_Fun_Native_Id(t *testing.T) {
	// Native function reflects to ["id", name]
	fn := &Fun{NativeName: "now"}
	nv := FunVal(fn)

	got := IxReflect(nv)
	want := vArray(vStr("id"), vStr("now"))
	mustEqValue(t, got, want)
}

func Test_Introspection_Fun_UserConstructor(t *testing.T) {
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

func Test_Introspection_Type_AsConstructor(t *testing.T) {
	tv := TypeVal(S{"array", S{"id", "Int"}}) // Type value
	got := IxReflect(tv)
	want := vArray(
		vStr("type"),
		vArray(vStr("array"), vArray(vStr("id"), vStr("Int"))),
	)
	mustEqValue(t, got, want)
}

func Test_Introspection_Handle_SoftError(t *testing.T) {
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

func TestIxReify_Rejects_Handle(t *testing.T) {
	ip := NewInterpreter()

	rtHandle := vArray(vStr("handle"))
	_, err := ip.IxReify(rtHandle)
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
func Test_Introspection_IxReflect_ModuleCapsule(t *testing.T) {
	ip := NewInterpreter()

	// Build a simple module via source, then reflect it.
	src := `module "Refl" do
  let a = 1
end`
	v, err := ip.EvalPersistentSource(src)
	if err != nil {
		t.Fatalf("EvalPersistentSource failed: %v", err)
	}
	if v.Tag != VTModule {
		t.Fatalf("expected VTModule, got %v", v.Tag)
	}

	got := IxReflect(v)

	// Expect a module capsule:
	// ["module", ["str","Refl"], ("pair", ["id","a"], ["int",1])]
	want := vArray(
		vStr("module"),
		vArray(vStr("str"), vStr("Refl")),
		vArray(
			vStr("pair"),
			vArray(vStr("id"), vStr("a")),
			vArray(vStr("int"), vInt(1)),
		),
	)
	mustEqValue(t, got, want)
}

func Test_Introspection_IxReify_ModuleCapsule_LowersAndCaches(t *testing.T) {
	ip := NewInterpreter()

	// Register a native "tick" that increments ip.Global["counter"] and returns the new value.
	ip.Global.Define("counter", vInt(0))
	ip.RegisterNative(
		"tick",
		[]ParamSpec{},
		S{"id", "Int"},
		func(ip *Interpreter, ctx CallCtx) Value {
			// read
			cv, err := ip.Global.Get("counter")
			if err != nil {
				ip.Global.Define("counter", vInt(0))
				cv = vInt(0)
			}
			n := cv.Data.(int64) + 1
			_ = ip.Global.Set("counter", vInt(n)) // Set must succeed (binding exists)
			return vInt(n)
		},
	)

	// Capsule:
	// ["module", ["str","M"], ("pair", ["id","x"], ["call", ["id","tick"]])]
	rt := vArray(
		vStr("module"),
		vArray(vStr("str"), vStr("M")),
		vArray(
			vStr("pair"),
			vArray(vStr("id"), vStr("x")),
			vArray(vStr("call"), vArray(vStr("id"), vStr("tick"))),
		),
	)

	// First reify: should execute body (tick once), produce a VTModule with x = 1.
	v1, err := ip.IxReify(rt)
	if err != nil {
		t.Fatalf("IxReify failed: %v", err)
	}
	if v1.Tag != VTModule {
		t.Fatalf("expected VTModule, got %v", v1.Tag)
	}
	mv1 := AsMapValue(v1)
	if mv1.Tag != VTMap {
		t.Fatalf("AsMapValue should coerce module to map, got %v", mv1.Tag)
	}
	mo1 := mv1.Data.(*MapObject)
	x1, ok := mo1.Entries["x"]
	if !ok {
		t.Fatalf("expected export 'x'")
	}
	if x1.Tag != VTInt || x1.Data.(int64) != 1 {
		t.Fatalf("expected x=1, got %#v", x1)
	}
	cv1, _ := ip.Global.Get("counter")
	if cv1.Data.(int64) != 1 {
		t.Fatalf("expected counter=1 after first install, got %v", cv1)
	}

	// Second reify of the SAME capsule: must hit cache (no second tick).
	v2, err := ip.IxReify(rt)
	if err != nil {
		t.Fatalf("IxReify failed on second install: %v", err)
	}
	if v2.Tag != VTModule {
		t.Fatalf("expected VTModule, got %v", v2.Tag)
	}
	// Same Module pointer proves the cache was used.
	m1 := v1.Data.(*Module)
	m2 := v2.Data.(*Module)
	if m1 != m2 {
		t.Fatalf("expected cached module pointer; got different instances")
	}
	cv2, _ := ip.Global.Get("counter")
	if cv2.Data.(int64) != 1 {
		t.Fatalf("expected counter to remain 1 (cache hit), got %v", cv2)
	}
}

func Test_Introspection_IxReify_ModuleCapsule_Nested(t *testing.T) {
	ip := NewInterpreter()

	// Outer capsule exports "sub" which itself is a module capsule "Inner" exporting y=2.
	rt := vArray(
		vStr("module"),
		vArray(vStr("str"), vStr("Outer")),
		vArray(
			vStr("pair"),
			vArray(vStr("id"), vStr("sub")),
			vArray(
				vStr("module"),
				vArray(vStr("str"), vStr("Inner")),
				vArray(
					vStr("pair"),
					vArray(vStr("id"), vStr("y")),
					vArray(vStr("int"), vInt(2)),
				),
			),
		),
	)

	v, err := ip.IxReify(rt)
	if err != nil {
		t.Fatalf("IxReify failed: %v", err)
	}
	if v.Tag != VTModule {
		t.Fatalf("expected VTModule for outer, got %v", v.Tag)
	}

	outer := AsMapValue(v).Data.(*MapObject)
	sub, ok := outer.Entries["sub"]
	if !ok || sub.Tag != VTModule {
		t.Fatalf("expected outer.sub to be a module, got %#v", sub)
	}
	inner := AsMapValue(sub).Data.(*MapObject)
	y, ok := inner.Entries["y"]
	if !ok || y.Tag != VTInt || y.Data.(int64) != 2 {
		t.Fatalf("expected inner.y = 2, got %#v", y)
	}
}
