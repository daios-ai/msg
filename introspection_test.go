package mindscript

import (
	"testing"
)

// --- tests -----------------------------------------------------------------

// Value ↔ runtime-S round-trip with annotation + arrays/maps + VTType inside.
func Test_Introspection_Ix_Value_Annot_Map_Array_Type_RoundTrip(t *testing.T) {
	mo := &MapObject{
		Entries: map[string]Value{
			"req": Str("A"),
			"ann": Str("B"),
			"arr": Arr([]Value{Int(1), Num(2), Bool(true)}),
		},
		KeyAnn: map[string]string{"req": "!", "ann": "note"},
		Keys:   []string{"req", "ann", "arr"},
	}
	v := Value{Tag: VTMap, Data: mo, Annot: "root"}

	// Direct reflect→reify must preserve Value.Annot.
	rs := IxReflect(v)
	round, err := IxReify(rs)
	if err != nil {
		t.Fatalf("IxReify: %v", err)
	}
	if round.Annot != "root" {
		t.Fatalf("want annot 'root', got %q", round.Annot)
	}

	// Check map invariants.
	if round.Tag != VTMap {
		t.Fatalf("want VTMap, got %#v", round)
	}
	rm := round.Data.(*MapObject)
	if rm.KeyAnn["req"] != "!" {
		t.Fatalf("want req required '!'")
	}
	if rm.KeyAnn["ann"] != "note" {
		t.Fatalf("want ann key annotation 'note', got %q", rm.KeyAnn["ann"])
	}
	arr := rm.Entries["arr"].Data.([]Value)
	wantInt(t, arr[0], 1)
	wantNum(t, arr[1], 2)
	wantBool(t, arr[2], true)

	// Insert a VTType into the array and round-trip again.
	typ := TypeVal(S{"unop", "?", S{"id", "Int"}})
	rm.Entries["arr"] = Arr([]Value{rm.Entries["arr"], typ})
	rs2 := IxReflect(round)
	round2, err := IxReify(rs2)
	if err != nil {
		t.Fatalf("IxReify (with type in array): %v", err)
	}
	rm2 := round2.Data.(*MapObject)
	arr2 := rm2.Entries["arr"].Data.([]Value)
	if arr2[1].Tag != VTType {
		t.Fatalf("want VTType in array, got %#v", arr2[1])
	}
}

// Function: reflect → rewrite (“int 1”→“int 2”) → reify → execute.
func Test_Introspection_Ix_Function_Rewrite_And_Run(t *testing.T) {
	// fun (x: Int) -> Int do x + 1 end
	params := S{"array", S{"pair", S{"id", "x"}, S{"id", "Int"}}}
	body := S{"block", S{"binop", "+", S{"id", "x"}, S{"int", int64(1)}}}
	funNode := S{"fun", params, S{"id", "Int"}, body}

	rs := IxToS(funNode)

	// Rewrite ("int", 1) → ("int", 2) everywhere.
	rs2 := IxRewrite(rs, func(n Value) (Value, bool) {
		if n.Tag != VTArray {
			return Value{}, false
		}
		a := n.Data.([]Value)
		if len(a) == 2 && a[0].Tag == VTStr && a[0].Data.(string) == "int" &&
			a[1].Tag == VTInt && a[1].Data.(int64) == 1 {
			return IxToS(S{"int", int64(2)}), true
		}
		return Value{}, false
	})

	// Reify to a function and run it.
	ip := NewInterpreter()
	val, err := IxReifyIn(rs2, ip.Global)
	if err != nil {
		t.Fatalf("IxReifyIn: %v", err)
	}
	if val.Tag != VTFun {
		t.Fatalf("want fun, got %#v", val)
	}
	// x=40 → 40+2 = 42
	wantInt(t, ip.Apply(val, []Value{Int(40)}), 42)
}

// Oracle + Type (with annot) + interactive parse + error stubs.
func Test_Introspection_Ix_Oracle_Type_Interactive_And_Errors(t *testing.T) {
	// Oracle AST: oracle (x: Int) -> Num from [1]
	params := S{"array", S{"pair", S{"id", "x"}, S{"id", "Int"}}}
	source := S{"array", S{"int", int64(1)}}
	oracleNode := S{"oracle", params, S{"id", "Num"}, source}
	rs := IxToS(oracleNode)

	ip := NewInterpreter()
	fv, err := IxReifyIn(rs, ip.Global)
	if err != nil {
		t.Fatalf("IxReifyIn oracle: %v", err)
	}
	if fv.Tag != VTFun || !fv.Data.(*Fun).IsOracle {
		t.Fatalf("want oracle fun, got %#v", fv)
	}

	// Type with annotation wrapper → VTType with .Annot preserved.
	Ts := S{"annot", S{"str", "doc"}, S{"unop", "?", S{"id", "Int"}}}
	Tval, err := IxReify(IxToS(Ts))
	if err != nil {
		t.Fatalf("IxReify type: %v", err)
	}
	if Tval.Tag != VTType || Tval.Annot != "doc" {
		t.Fatalf("type reify annot lost: %#v", Tval)
	}
	// And the pretty type string should be Int?
	if got := FormatType(Tval.Data.(*TypeValue).Ast); got != "Int?" {
		t.Fatalf("want 'Int?', got %q", got)
	}

	// Interactive parse should yield IncompleteError.
	if _, err := IxFromSourceInteractive("if true then 1"); err == nil || !IsIncomplete(err) {
		t.Fatalf("expected IncompleteError, got %v", err)
	}

	// Reify error paths: native/module/handle.
	ip.RegisterNative("inc", []ParamSpec{{Name: "x", Type: S{"id", "Int"}}}, S{"id", "Int"},
		func(ip *Interpreter, ctx CallCtx) Value { return Int(ctx.MustArg("x").Data.(int64) + 1) })
	nv, _ := ip.Core.Get("inc")
	if _, err := IxReify(IxReflect(nv)); err == nil {
		t.Fatalf("expected error on reifying native")
	}
	if _, err := IxReify(Arr([]Value{Str("module")})); err == nil {
		t.Fatalf("expected error on reifying module stub")
	}
	if _, err := IxReify(Arr([]Value{Str("handle")})); err == nil {
		t.Fatalf("expected error on reifying handle stub")
	}
}

// Source helpers + AST↔runtimeS↔AST stability (smoke).
func Test_Introspection_Ix_Source_Helpers_And_Stability(t *testing.T) {
	ast, err := IxFromSource("do 1 + 2 end")
	if err != nil {
		t.Fatalf("IxFromSource: %v", err)
	}

	// round-trip AST ↔ runtime-S ↔ AST
	rs := IxToS(ast)
	ast2, err := IxFromS(rs)
	if err != nil {
		t.Fatalf("IxFromS: %v", err)
	}

	// Both should format to the same stable source.
	s1 := IxToSource(ast)
	s2 := IxToSource(ast2)
	if s1 != s2 {
		t.Fatalf("AST round-trip changed formatting:\norig: %q\nafter: %q", s1, s2)
	}

	// Evaluate to ensure AST is executable.
	ip := NewInterpreter()
	v, err := ip.Eval(ast)
	if err != nil {
		t.Fatalf("Eval: %v", err)
	}
	wantInt(t, v, 3)
}

// Multiple annotation wrappers merge correctly (order-agnostic).
func Test_Introspection_Ix_Annot_Merge(t *testing.T) {
	// Build AST: ("annot","a", ("annot","b", ("str","x")))
	Sannot := S{
		"annot",
		S{"str", "a"},
		S{"annot", S{"str", "b"}, S{"str", "x"}},
	}
	rs := IxToS(Sannot)
	v, err := IxReify(rs)
	if err != nil {
		t.Fatalf("IxReify: %v", err)
	}
	wantStr(t, v, "x")
	// Depending on unwrap order, annotation may be "a\nb" or "b\na".
	if v.Annot != "a\nb" && v.Annot != "b\na" {
		t.Fatalf("want merged annot 'a\\nb' or 'b\\na', got %q", v.Annot)
	}
}

// Enum[...] round-trip: source → AST → runtime-S → AST, and reify as VTType.
func Test_Introspection_Ix_Enum_RoundTrip_Type(t *testing.T) {
	ast, err := IxFromSource(`Enum[ 1, 2, "a" ]`)
	if err != nil {
		t.Fatalf("IxFromSource enum: %v", err)
	}

	// AST ↔ runtime-S ↔ AST should be stable
	rs := IxToS(ast)
	ast2, err := IxFromS(rs)
	if err != nil {
		t.Fatalf("IxFromS: %v", err)
	}
	s1 := IxToSource(ast)
	s2 := IxToSource(ast2)
	if s1 != s2 {
		t.Fatalf("enum round-trip changed formatting:\norig: %q\nafter: %q", s1, s2)
	}

	// Reify the ("enum", ...) node itself should yield VTType.
	// Program AST is ("block", enumNode); grab child 1.
	if len(ast2) < 2 {
		t.Fatalf("unexpected AST shape: %#v", ast2)
	}
	enumNode, ok := ast2[1].(S)
	if !ok {
		t.Fatalf("unexpected enum node type: %#v", ast2[1])
	}
	v, err := IxReify(IxToS(enumNode))
	if err != nil {
		t.Fatalf("IxReify enum: %v", err)
	}
	if v.Tag != VTType {
		t.Fatalf("enum should reify to VTType, got %#v", v)
	}
}

// Pretty/printer idempotence across a small corpus.
func Test_Introspection_Ix_Pretty_Idempotence_Corpus(t *testing.T) {
	corpus := []string{
		"1 + 2 * 3",
		"do 1 + 2 end",
		"fun(x: Int) -> Int do x + 1 end",
		"if true then 1 else 2 end",
		`{ name!: "Ada", age: 42 }`,
	}

	for i, src := range corpus {
		ast, err := IxFromSource(src)
		if err != nil {
			t.Fatalf("[%d] parse: %v\nsrc:\n%s", i, err, src)
		}
		s1 := IxToSource(ast)

		ast2, err := IxFromSource(s1) // parse what we just printed
		if err != nil {
			t.Fatalf("[%d] reparse: %v\nprinted:\n%s", i, err, s1)
		}
		s2 := IxToSource(ast2)

		if s1 != s2 {
			t.Fatalf("[%d] pretty not idempotent:\nfirst:  %q\nsecond: %q", i, s1, s2)
		}
	}
}
