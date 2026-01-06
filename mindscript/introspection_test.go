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

// valueDeepEqual performs a structural comparison of Values.
// It is intentionally strict about Tag and Annot, and compares Array/Map
// contents by structure (not pointer identity).
//
// NOTE: This is for tests that compare runtime-S (arrays/scalars) and some
// direct VTMap comparisons. It does not attempt to handle cycles.
func valueDeepEqual(a, b Value) bool {
	if a.Tag != b.Tag {
		return false
	}
	if a.Annot != b.Annot {
		return false
	}

	switch a.Tag {
	case VTNull:
		return true

	case VTBool:
		av, aok := a.Data.(bool)
		bv, bok := b.Data.(bool)
		return aok && bok && av == bv

	case VTInt:
		av, aok := a.Data.(int64)
		bv, bok := b.Data.(int64)
		return aok && bok && av == bv

	case VTNum:
		av, aok := a.Data.(float64)
		bv, bok := b.Data.(float64)
		return aok && bok && av == bv

	case VTStr:
		av, aok := a.Data.(string)
		bv, bok := b.Data.(string)
		return aok && bok && av == bv

	case VTArray:
		ao, aok := a.Data.(*ArrayObject)
		bo, bok := b.Data.(*ArrayObject)
		if !aok || !bok {
			return false
		}
		if len(ao.Elems) != len(bo.Elems) {
			return false
		}
		for i := range ao.Elems {
			if !valueDeepEqual(ao.Elems[i], bo.Elems[i]) {
				return false
			}
		}
		return true

	case VTMap:
		am, aok := a.Data.(*MapObject)
		bm, bok := b.Data.(*MapObject)
		if !aok || !bok {
			return false
		}
		if len(am.Keys) != len(bm.Keys) {
			return false
		}
		for i := range am.Keys {
			if am.Keys[i] != bm.Keys[i] {
				return false
			}
			av, aok := am.Entries[am.Keys[i]]
			bv, bok := bm.Entries[bm.Keys[i]]
			if aok != bok {
				return false
			}
			if !valueDeepEqual(av, bv) {
				return false
			}
		}
		return true

	default:
		// Tests in this file compare runtime-S and a few VTMap views.
		// If you need more tags here, extend explicitly.
		return false
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

func Test_Introspection_IxToS_SoftErrorOnUnsupportedAtom(t *testing.T) {
	// Construct an AST S with an unsupported atom (int32).
	bad := S{"block", S{"int", int64(1)}, int32(7)}
	got := IxToS(bad)
	// We only assert that it is an annotated-null soft error.
	// Message text is implementation-defined but must be present.
	if got.Tag != VTArray {
		t.Fatalf("expected VTArray, got %v", got.Tag)
	}
	top := got.Data.(*ArrayObject).Elems
	if len(top) != 3 || top[0].Tag != VTStr || top[0].Data.(string) != "annot" {
		t.Fatalf("expected annotated-null; got %#v", got)
	}
	// Final child must be ["null"]
	nullNode := top[2]
	wantNull := vArray(vStr("null"))
	mustEqValue(t, nullNode, wantNull)
}

func Test_Introspection_IxFromS_NoValidation_IxReifyHardFails(t *testing.T) {
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
	ip, _ := NewInterpreter()
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

func Test_Introspection_ArrayMap_WithKeyAnn_NoRequirednessInValues(t *testing.T) {
	// Value maps never carry requiredness in reflection.
	mo := &MapObject{
		Entries: map[string]Value{},
		Keys:    []string{},
	}
	mo.Keys = append(mo.Keys, "a")
	mo.Entries["a"] = vInt(1)

	mo.Keys = append(mo.Keys, "b")
	mo.Entries["b"] = vStr("x")

	got := IxReflect(Value{Tag: VTMap, Data: mo})
	want := vArray(
		vStr("map"),
		// "a" is a plain pair (no requiredness in value maps)
		vArray(vStr("pair"),
			vArray(vStr("str"), vStr("a")),
			vArray(vStr("int"), vInt(1)),
		),
		// "b" plain key
		vArray(vStr("pair"),
			vArray(vStr("str"), vStr("b")),
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
	tv := TypeValIn(S{"array", S{"id", "Int"}}, NewEnv(nil)) // pinned env
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
	if got.Tag != VTNull {
		t.Fatalf("expected annotated-null for handle; got %#v", got)
	}
}

func TestIxReify_SimpleProgram_Evaluates(t *testing.T) {
	ip, _ := NewInterpreter()
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
	ip, _ := NewInterpreter()
	_, err := ip.IxReify(vArray(vStr("handle")))
	mustErr(t, err) // validator soft; IxReify escalates to hard error
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
	if !equalLiteralS(ast, back) {
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

func Test_Introspection_IxReflect_Module_Canonical(t *testing.T) {
	ip, _ := NewInterpreter()

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

	// Prefer the actual stored module name to avoid coupling tests to naming policy.
	modName := ""
	if m, ok := v.Data.(*Module); ok && m != nil {
		modName = m.Name
	}

	// Expect CANONICAL inline form:
	// ["module", ["str","Refl"],
	//   ["block",
	//     ["assign", ["let", ["id","a"]], ["int",1]]
	//   ]
	// ]
	want := vArray(
		vStr("module"),
		vArray(vStr("str"), vStr(modName)),
		vArray(
			vStr("block"),
			vArray(
				vStr("assign"),
				vArray(
					vStr("let"),
					vArray(vStr("id"), vStr("a")),
				),
				vArray(vStr("int"), vInt(1)),
			),
		),
	)

	mustEqValue(t, got, want)
}

func Test_Introspection_IxReify_Module_Lowered_Form_Caches(t *testing.T) {
	ip, _ := NewInterpreter()

	// Native "tick" increments Global["counter"].
	ip.Global.Define("counter", vInt(0))
	ip.RegisterNative(
		"tick",
		[]ParamSpec{},
		S{"id", "Int"},
		func(ip *Interpreter, ctx CallCtx) Value {
			cv, _ := ip.Global.Get("counter")
			n := cv.Data.(int64) + 1
			_ = ip.Global.Set("counter", vInt(n))
			return vInt(n)
		},
	)

	// Canonical module form:
	// ["module", ["str","M"],
	//   ["block",
	//     ["assign", ["let", ["id","x"]], ["call", ["id","tick"]]]
	//   ]
	// ]
	rt := vArray(
		vStr("module"),
		vArray(vStr("str"), vStr("M")),
		vArray(
			vStr("block"),
			vArray(
				vStr("assign"),
				vArray(
					vStr("let"),
					vArray(vStr("id"), vStr("x")),
				),
				vArray(vStr("call"), vArray(vStr("id"), vStr("tick"))),
			),
		),
	)

	v1, err := ip.IxReify(rt)
	if err != nil {
		t.Fatalf("IxReify failed: %v", err)
	}
	if v1.Tag != VTModule {
		t.Fatalf("expected VTModule, got %v", v1.Tag)
	}
	mvv1 := AsMapValue(v1)
	mv1 := mvv1.Data.(*MapObject)
	x1 := mv1.Entries["x"]
	if x1.Tag != VTInt || x1.Data.(int64) != 1 {
		t.Fatalf("expected x=1, got %#v", x1)
	}
	cv1, _ := ip.Global.Get("counter")
	if cv1.Data.(int64) != 1 {
		t.Fatalf("expected counter=1 after first install, got %v", cv1)
	}

	// Reify again → should hit cache (counter unchanged).
	v2, err := ip.IxReify(rt)
	if err != nil {
		t.Fatalf("IxReify failed on second install: %v", err)
	}
	if v2.Tag != VTModule {
		t.Fatalf("expected VTModule, got %v", v2.Tag)
	}
	mvv2 := AsMapValue(v2)
	mv2 := mvv2.Data.(*MapObject)
	x2 := mv2.Entries["x"]
	if x2.Tag != VTInt || x2.Data.(int64) != 1 {
		t.Fatalf("expected x=1 after second install, got %#v", x2)
	}

	// Structural check: module exports are the same.
	if !valueDeepEqual(mvv1, mvv2) {
		t.Fatalf("expected cached module exports to be structurally identical")
	}

	// Cache-hit signal: counter unchanged.
	cv2, _ := ip.Global.Get("counter")
	if cv2.Data.(int64) != 1 {
		t.Fatalf("expected counter to remain 1 (cache hit), got %v", cv2)
	}
}

func Test_Introspection_IxReify_Module_Nested_Canonical_Form(t *testing.T) {
	ip, _ := NewInterpreter()

	// Outer canonical exporting sub = (lowered inner module).
	rt := vArray(
		vStr("module"),
		vArray(vStr("str"), vStr("Outer")),
		vArray(
			vStr("block"),
			vArray(
				vStr("assign"),
				vArray(
					vStr("let"),
					vArray(vStr("id"), vStr("sub")),
				),
				vArray(
					vStr("module"),
					vArray(vStr("str"), vStr("Inner")),
					vArray(
						vStr("block"),
						vArray(
							vStr("assign"),
							vArray(
								vStr("let"),
								vArray(vStr("id"), vStr("y")),
							),
							vArray(vStr("int"), vInt(2)),
						),
					),
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
	sub := outer.Entries["sub"]
	if sub.Tag != VTModule {
		t.Fatalf("expected outer.sub to be a module, got %#v", sub)
	}
	inner := AsMapValue(sub).Data.(*MapObject)
	y := inner.Entries["y"]
	if y.Tag != VTInt || y.Data.(int64) != 2 {
		t.Fatalf("expected inner.y = 2, got %#v", y)
	}
}

func wrapAnnot(node Value, txt string) Value {
	return vArray(
		vStr("annot"),
		vArray(vStr("str"), vStr(txt)),
		node,
	)
}

// Array element POST annotation should be preserved by reflect(), and survive reify(reflect(...)).
func Test_Introspection_ArrayElementAnnotation_PreservedInReflect(t *testing.T) {
	// Build a runtime array value: [1, 2] with a POST annotation on element 0 ("<hi").
	e0 := vInt(1)
	e0.Annot = "<hi"
	e1 := vInt(2)
	val := Arr([]Value{e0, e1})

	got := IxReflect(val)

	// Expect: ["array", ["annot", ["str","<hi"], ["int",1]], ["int",2]]
	want := vArray(
		vStr("array"),
		wrapAnnot(vArray(vStr("int"), vInt(1)), "<hi"),
		vArray(vStr("int"), vInt(2)),
	)
	mustEqValue(t, got, want)
}

func Test_Introspection_Array_ReifyRoundtrip_PreservesAnnotations(t *testing.T) {
	// Same setup as above.
	e0 := vInt(1)
	e0.Annot = "hi"
	e1 := vInt(2)
	val := Arr([]Value{e0, e1})

	rt := IxReflect(val)

	ip, _ := NewInterpreter()
	got, err := ip.IxReify(rt)
	if err != nil {
		t.Fatalf("IxReify error: %v", err)
	}
	if got.Tag != VTArray {
		t.Fatalf("expected VTArray after reify, got %v", got.Tag)
	}
	elems := got.Data.(*ArrayObject).Elems
	if len(elems) != 2 {
		t.Fatalf("expected 2 elements, got %d", len(elems))
	}
	if elems[0].Annot != "hi" {
		t.Fatalf("expected first element Annot 'hi', got %q", elems[0].Annot)
	}
	if elems[1].Annot != "" {
		t.Fatalf("expected second element Annot empty, got %q", elems[1].Annot)
	}
}

// Map key/requiredness + key/value annotations should be preserved by reflect().
func Test_Introspection_Map_KeyAndValueAnnotations_PreservedInReflect(t *testing.T) {
	// Value map with value doc.
	mo := &MapObject{
		Keys: []string{"a", "b"},
		Entries: map[string]Value{
			"a": vInt(1),
			"b": func() Value { v := vInt(2); v.Annot = "vpost"; return v }(),
		},
	}
	got := IxReflect(Value{Tag: VTMap, Data: mo})

	want := vArray(
		vStr("map"),
		vArray(
			vStr("pair"),
			vArray(vStr("str"), vStr("a")),
			vArray(vStr("int"), vInt(1)),
		),
		vArray(
			vStr("pair"),
			vArray(vStr("str"), vStr("b")),
			wrapAnnot(vArray(vStr("int"), vInt(2)), "vpost"),
		),
	)
	mustEqValue(t, got, want)
}

// Nested arrays/maps should propagate annotations recursively in reflect().
func Test_Introspection_NestedContainers_AnnotationsPropagate(t *testing.T) {
	// Value: [ { x: 1 }, 2 ] with:
	//  - POST on value 1 (val ann "v1")
	//  - POST on outer array element 1 ("outer")
	innerMO := &MapObject{
		Keys: []string{"x"},
		Entries: map[string]Value{
			"x": func() Value { v := vInt(1); v.Annot = "v1"; return v }(),
		},
	}
	innerMap := Value{Tag: VTMap, Data: innerMO}
	elem1 := vInt(2)
	elem1.Annot = "outer"

	val := Arr([]Value{innerMap, elem1})

	got := IxReflect(val)

	// Expected (no key annotation reflected):
	// ["array",
	//   ["map", ["pair", ["str","x"], ["annot", ["str","v1"], ["int",1]]]],
	//   ["annot", ["str","outer"], ["int",2]]
	// ]
	want := vArray(
		vStr("array"),
		vArray(
			vStr("map"),
			vArray(
				vStr("pair"),
				vArray(vStr("str"), vStr("x")),
				wrapAnnot(vArray(vStr("int"), vInt(1)), "v1"),
			),
		),
		wrapAnnot(vArray(vStr("int"), vInt(2)), "outer"),
	)

	mustEqValue(t, got, want)
}

// ────────────────────────────────────────────────────────────────────────────
// IxFromS (strict decode) & IxToS (soft encode)
// ────────────────────────────────────────────────────────────────────────────

func Test_Introspection_IxFromS_HardErrors_OnBadShapes(t *testing.T) {
	// non-array root
	if _, err := IxFromS(Str("x")); err == nil {
		t.Fatalf("want error for non-array root")
	}
	// empty node
	if _, err := IxFromS(Arr(nil)); err == nil {
		t.Fatalf("want error for empty node")
	}
	// non-string tag
	if _, err := IxFromS(vArray(vInt(1))); err == nil {
		t.Fatalf("want error for non-string tag")
	}
	// unsupported scalar kind in payload (VTNull is not allowed as scalar)
	if _, err := IxFromS(vArray(vStr("int"), Null)); err == nil {
		t.Fatalf("want error for unsupported scalar kind")
	}
}

func Test_Introspection_IxToS_SoftError_DeepInside(t *testing.T) {
	// Unsupported atom nested inside array → annotated-null soft error
	bad := S{"array", S{"int", int64(1)}, int32(7)}
	rt := IxToS(bad)
	top := rt.Data.(*ArrayObject).Elems
	if rt.Tag != VTArray || len(top) != 3 || top[0].Data.(string) != "annot" {
		t.Fatalf("expected annotated-null soft error, got %#v", rt)
	}
}

// ────────────────────────────────────────────────────────────────────────────
// IxReflect coverage (oracle, defaults, annotations, types)
// ────────────────────────────────────────────────────────────────────────────

func Test_Introspection_Reflect_Oracle(t *testing.T) {
	f := &Fun{
		IsOracle:   true,
		Params:     []string{"q"},
		ParamTypes: []S{S{"id", "Str"}},
		ReturnType: S{"id", "Str"},
		Body:       S{"id", "source"},
	}
	got := IxReflect(FunVal(f))
	want := vArray(
		vStr("oracle"),
		vArray(vStr("array"),
			vArray(vStr("pair"), vArray(vStr("id"), vStr("q")), vArray(vStr("id"), vStr("Str"))),
		),
		vArray(vStr("id"), vStr("Str")),
		vArray(vStr("id"), vStr("source")),
	)
	mustEqValue(t, got, want)
}

func Test_Introspection_Reflect_Fun_DefaultsToAny(t *testing.T) {
	f := &Fun{
		Params: []string{"x"}, // no ParamTypes/ReturnType set
		Body:   S{"block"},
	}
	got := IxReflect(FunVal(f))
	// param "x: Any", return Any
	if got.Tag != VTArray {
		t.Fatalf("unexpected shape")
	}
	arr := got.Data.(*ArrayObject).Elems
	if arr[0].Data.(string) != "fun" {
		t.Fatalf("want fun, got %#v", got)
	}
	params := arr[1]
	ret := arr[2]
	mustEqValue(t,
		params,
		vArray(vStr("array"), vArray(vStr("pair"), vArray(vStr("id"), vStr("x")), vArray(vStr("id"), vStr("Any")))),
	)
	mustEqValue(t, ret, vArray(vStr("id"), vStr("Any")))
}

func Test_Introspection_Reflect_Annotation_OnScalar(t *testing.T) {
	v := vInt(1)
	v.Annot = "<p"
	got := IxReflect(v)
	want := vArray(
		vStr("annot"),
		vArray(vStr("str"), vStr("<p")),
		vArray(vStr("int"), vInt(1)),
	)
	mustEqValue(t, got, want)
}

func Test_Introspection_Reflect_Annotation_OnFunAndModule(t *testing.T) {
	// Function annotation
	f := FunVal(&Fun{Body: S{"block"}})
	f.Annot = "pre"
	gotF := IxReflect(f)
	if gotF.Data.(*ArrayObject).Elems[0].Data.(string) != "annot" {
		t.Fatalf("function reflect should wrap in annot, got %#v", gotF)
	}

	// Module annotation
	ip, _ := NewInterpreter()
	mv, _ := ip.EvalPersistentSource(`module "M" do end`)
	mv.Annot = "<post"
	gotM := IxReflect(mv)
	if gotM.Data.(*ArrayObject).Elems[0].Data.(string) != "annot" {
		t.Fatalf("module reflect should wrap in annot, got %#v", gotM)
	}
}

func Test_Introspection_Reflect_Type_WithEnum(t *testing.T) {
	tv := TypeValIn(S{"enum", S{"int", int64(1)}, S{"str", "a"}}, NewEnv(nil))
	got := IxReflect(tv)
	want := vArray(
		vStr("type"),
		vArray(
			vStr("enum"),
			vArray(vStr("int"), vInt(1)),
			vArray(vStr("str"), vStr("a")),
		),
	)
	mustEqValue(t, got, want)
}

// ────────────────────────────────────────────────────────────────────────────
// IxValidateS (semantic validator) — spot checks
// ────────────────────────────────────────────────────────────────────────────

func Test_Introspection_Validate_Success_EmptyErrors(t *testing.T) {
	errs := IxValidateS(S{"binop", "+", S{"int", int64(1)}, S{"int", int64(2)}})
	if errs.Tag != VTArray || len(errs.Data.(*ArrayObject).Elems) != 0 {
		t.Fatalf("expected no errors, got %#v", errs)
	}
}

func Test_Introspection_Validate_ValueMap_ForbidsPairBang(t *testing.T) {
	errs := IxValidateS(S{"map", S{"pair!", S{"str", "x"}, S{"int", int64(1)}}})
	if len(errs.Data.(*ArrayObject).Elems) == 0 {
		t.Fatalf("expected an error for pair! in value map")
	}
}

func Test_Introspection_Validate_Annot_NotNestable(t *testing.T) {
	bad := S{"annot", S{"str", "x"}, S{"annot", S{"str", "y"}, S{"int", int64(1)}}}
	errs := IxValidateS(bad)
	if len(errs.Data.(*ArrayObject).Elems) == 0 {
		t.Fatalf("expected E_ANNOT_NESTED")
	}
}

func Test_Introspection_Validate_AssignableLHS(t *testing.T) {
	errs := IxValidateS(S{"assign", S{"int", int64(0)}, S{"int", int64(1)}})
	if len(errs.Data.(*ArrayObject).Elems) == 0 {
		t.Fatalf("expected E_ASSIGN_TARGET")
	}
}

func Test_Introspection_Validate_ControlArity(t *testing.T) {
	errs := IxValidateS(S{"return"}) // arity != 2
	if len(errs.Data.(*ArrayObject).Elems) == 0 {
		t.Fatalf("expected arity error for return")
	}
}

func Test_Introspection_Validate_OracleSource_MustBeExpr(t *testing.T) {
	errs := IxValidateS(S{"oracle", S{"array"}, S{"id", "Any"}, S{"block"}})
	if len(errs.Data.(*ArrayObject).Elems) == 0 {
		t.Fatalf("expected E_ORACLE_SRC_NOT_EXPR")
	}
}

func Test_Introspection_Validate_IfElse_Shape(t *testing.T) {
	// else must be a block
	errs := IxValidateS(S{
		"if",
		S{"pair", S{"bool", true}, S{"block"}},
		S{"int", int64(1)},
	})
	if len(errs.Data.(*ArrayObject).Elems) == 0 {
		t.Fatalf("expected E_EXPECT_BLOCK for else")
	}
}

func Test_Introspection_Validate_EnumMembers_AreExprs(t *testing.T) {
	// validate inside a type node
	errs := IxValidateS(S{"type", S{"enum", S{"block"}}})
	if len(errs.Data.(*ArrayObject).Elems) == 0 {
		t.Fatalf("expected E_ENUM_EXPR")
	}
}

func Test_Introspection_Validate_Handle_Forbidden(t *testing.T) {
	errs := IxValidateS(S{"handle"})
	if len(errs.Data.(*ArrayObject).Elems) == 0 {
		t.Fatalf("expected E_HANDLE_FORBIDDEN")
	}
}

// ────────────────────────────────────────────────────────────────────────────
// Validation-vs-Reify separation (validator reports, reify hard-fails)
// ────────────────────────────────────────────────────────────────────────────

func Test_Introspection_ValidateReports_ReifyHardFails(t *testing.T) {
	// Non-assignable LHS is invalid and should also fail at evaluation.
	rt := IxToS(S{"assign", S{"int", int64(0)}, S{"int", int64(1)}})
	if len(IxValidateS(S{"assign", S{"int", int64(0)}, S{"int", int64(1)}}).Data.(*ArrayObject).Elems) == 0 {
		t.Fatalf("expected validator errors")
	}
	ip, _ := NewInterpreter()
	if _, err := ip.IxReify(rt); err == nil {
		t.Fatalf("expected hard failure in IxReify")
	}
}

func Test_Introspection_Noop_RoundTrip_EncodeDecode(t *testing.T) {
	ast := S{"noop"}

	// Encode S -> runtime-S
	rt := IxToS(ast)
	want := vArray(vStr("noop"))
	mustEqValue(t, rt, want)

	// Decode runtime-S -> S
	back, err := IxFromS(rt)
	if err != nil {
		t.Fatalf("IxFromS failed for noop: %v", err)
	}
	if len(back) != 1 || back[0] != "noop" {
		t.Fatalf("roundtrip mismatch; got %v, want %v", back, ast)
	}
}

func Test_Introspection_Noop_Validate_Solo(t *testing.T) {
	// ValidateCanonicalS should accept ["noop"] as a valid expression/stmt.
	errs := IxValidateS(S{"noop"})
	if errs.Tag != VTArray {
		t.Fatalf("expected VTArray of errors, got %v", errs.Tag)
	}
	if n := len(errs.Data.(*ArrayObject).Elems); n != 0 {
		t.Fatalf("expected zero validation errors for noop, got %d", n)
	}
}

func Test_Introspection_Noop_Validate_Annotated(t *testing.T) {
	// #(doc) noop is valid; no nested annot allowed, but single level is fine.
	s := S{"annot", S{"str", "doc"}, S{"noop"}}
	errs := IxValidateS(s)
	if errs.Tag != VTArray {
		t.Fatalf("expected VTArray of errors, got %v", errs.Tag)
	}
	if n := len(errs.Data.(*ArrayObject).Elems); n != 0 {
		t.Fatalf("expected zero validation errors for annotated noop, got %d", n)
	}
}

func Test_Introspection_Noop_Validate_BadArity(t *testing.T) {
	// ["noop", "extra"] should be rejected (noop takes no payload).
	s := S{"noop", "extra"}
	errs := IxValidateS(s)
	if errs.Tag != VTArray {
		t.Fatalf("expected VTArray of errors, got %v", errs.Tag)
	}
	if n := len(errs.Data.(*ArrayObject).Elems); n == 0 {
		t.Fatalf("expected at least one validation error for bad-arity noop")
	}
}

func Test_Interpreter_DeepEqual_ArraySelfCycle(t *testing.T) {
	ip, _ := NewInterpreter()

	// Build a self-cyclic array: a = []; a[0] = a
	aObj := &ArrayObject{Elems: make([]Value, 1)}
	a := Value{Tag: VTArray, Data: aObj}
	aObj.Elems[0] = a

	// Another, structurally identical self-cyclic array
	bObj := &ArrayObject{Elems: make([]Value, 1)}
	b := Value{Tag: VTArray, Data: bObj}
	bObj.Elems[0] = b

	// A different array (no cycle)
	c := Arr([]Value{Null})

	if !ip.deepEqual(a, b) {
		t.Fatalf("expected self-cyclic arrays to be equal")
	}
	if ip.deepEqual(a, c) {
		t.Fatalf("expected cyclic array not equal to non-cyclic array")
	}
}

func Test_Interpreter_DeepEqual_MapSelfCycle(t *testing.T) {
	ip, _ := NewInterpreter()

	// Build a self-cyclic map: m = {}; m['self'] = m
	m1 := &MapObject{
		Entries: map[string]Value{},
		Keys:    []string{"self"},
	}
	mv1 := Value{Tag: VTMap, Data: m1}
	m1.Entries["self"] = mv1

	// Another, structurally identical self-cyclic map
	m2 := &MapObject{
		Entries: map[string]Value{},
		Keys:    []string{"self"},
	}
	mv2 := Value{Tag: VTMap, Data: m2}
	m2.Entries["self"] = mv2

	// Different map: same key but different value
	m3 := &MapObject{
		Entries: map[string]Value{"self": Null},
		Keys:    []string{"self"},
	}
	mv3 := Value{Tag: VTMap, Data: m3}

	if !ip.deepEqual(mv1, mv2) {
		t.Fatalf("expected self-cyclic maps to be equal")
	}
	if ip.deepEqual(mv1, mv3) {
		t.Fatalf("expected cyclic map not equal to different map")
	}
}

func Test_Interpreter_DeepEqual_IntVsNum(t *testing.T) {
	ip, _ := NewInterpreter()

	if !ip.deepEqual(Int(3), Num(3.0)) {
		t.Fatalf("expected Int(3) == Num(3.0)")
	}
	if ip.deepEqual(Int(3), Num(3.1)) {
		t.Fatalf("did not expect Int(3) == Num(3.1)")
	}
}

func Test_Introspection_Handle_Type_Get_Validate_And_Reify(t *testing.T) {
	// S: type Handle.file  — should validate and reify to a VTType with that AST.
	s := S{"type", S{"get", S{"id", "Handle"}, S{"str", "file"}}}

	// Validate: expect empty error array.
	verrs := IxValidateS(s)
	if verrs.Tag != VTArray || len(verrs.Data.(*ArrayObject).Elems) != 0 {
		t.Fatalf("expected no validation errors, got %#v", verrs)
	}

	// Reify via runtime-S roundtrip.
	rt := IxToS(s)
	ip, _ := NewInterpreter()
	v, err := ip.IxReify(rt)
	if err != nil {
		t.Fatalf("IxReify failed: %v", err)
	}
	if v.Tag != VTType {
		t.Fatalf("expected VTType, got %v", v.Tag)
	}
	tv := v.Data.(*TypeValue)
	wantAst := S{"get", S{"id", "Handle"}, S{"str", "file"}}
	if !reflect.DeepEqual(stripAnnot(tv.Ast), wantAst) {
		t.Fatalf("type AST mismatch\n got : %#v\n want: %#v", tv.Ast, wantAst)
	}
}

func Test_Introspection_Handle_Type_NestedGet_Validate_And_Reify(t *testing.T) {
	// S: type M.T.U — nested get chain should validate and reify unchanged.
	s := S{"type", S{"get", S{"get", S{"id", "M"}, S{"str", "T"}}, S{"str", "U"}}}

	verrs := IxValidateS(s)
	if verrs.Tag != VTArray || len(verrs.Data.(*ArrayObject).Elems) != 0 {
		t.Fatalf("expected no validation errors, got %#v", verrs)
	}

	rt := IxToS(s)
	ip, _ := NewInterpreter()
	v, err := ip.IxReify(rt)
	if err != nil {
		t.Fatalf("IxReify failed: %v", err)
	}
	if v.Tag != VTType {
		t.Fatalf("expected VTType, got %v", v.Tag)
	}
	tv := v.Data.(*TypeValue)
	wantAst := S{"get", S{"get", S{"id", "M"}, S{"str", "T"}}, S{"str", "U"}}
	if !reflect.DeepEqual(stripAnnot(tv.Ast), wantAst) {
		t.Fatalf("type AST mismatch\n got : %#v\n want: %#v", tv.Ast, wantAst)
	}
}

func Test_Introspection_Fun_Param_Allows_HandleKind_Type(t *testing.T) {
	// fun(h: Handle.socket) -> Any { } must validate.
	s := S{
		"fun",
		S{"array",
			S{"pair",
				S{"id", "h"},
				S{"get", S{"id", "Handle"}, S{"str", "socket"}},
			},
		},
		S{"id", "Any"},
		S{"block"},
	}
	verrs := IxValidateS(s)
	if verrs.Tag != VTArray || len(verrs.Data.(*ArrayObject).Elems) != 0 {
		t.Fatalf("expected no validation errors, got %#v", verrs)
	}
}

func Test_Introspection_Enum_Members_Accept_Expressions_In_Types(t *testing.T) {
	// type Enum[ 1 + 2 ] — validator accepts expression members (evaluation will enforce literal).
	s := S{"type", S{"enum", S{"binop", "+", S{"int", int64(1)}, S{"int", int64(2)}}}}
	verrs := IxValidateS(s)
	if verrs.Tag != VTArray || len(verrs.Data.(*ArrayObject).Elems) != 0 {
		t.Fatalf("expected no validation errors, got %#v", verrs)
	}
}
