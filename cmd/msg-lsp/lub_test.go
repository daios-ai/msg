package main

import (
	"reflect"
	"testing"
)

func Test_LUB_Primitives(t *testing.T) {
	t.Run("equal", func(t *testing.T) {
		if g, w := LUB([]any{"id", "Int"}, []any{"id", "Int"}), []any{"id", "Int"}; !reflect.DeepEqual(g, w) {
			t.Fatalf("%#v != %#v", g, w)
		}
		if g, w := LUB([]any{"id", "Str"}, []any{"id", "Str"}), []any{"id", "Str"}; !reflect.DeepEqual(g, w) {
			t.Fatalf("%#v != %#v", g, w)
		}
	})
	t.Run("Int_to_Num", func(t *testing.T) {
		if g, w := LUB([]any{"id", "Int"}, []any{"id", "Num"}), []any{"id", "Num"}; !reflect.DeepEqual(g, w) {
			t.Fatalf("%#v != %#v", g, w)
		}
		if g, w := LUB([]any{"id", "Num"}, []any{"id", "Int"}), []any{"id", "Num"}; !reflect.DeepEqual(g, w) {
			t.Fatalf("%#v != %#v", g, w)
		}
	})
	t.Run("unrelated_to_Any", func(t *testing.T) {
		if g, w := LUB([]any{"id", "Bool"}, []any{"id", "Str"}), []any{"id", "Any"}; !reflect.DeepEqual(g, w) {
			t.Fatalf("%#v != %#v", g, w)
		}
	})
	t.Run("Any_dominates", func(t *testing.T) {
		if g, w := LUB([]any{"id", "Any"}, []any{"id", "Int"}), []any{"id", "Any"}; !reflect.DeepEqual(g, w) {
			t.Fatalf("%#v != %#v", g, w)
		}
		if g, w := LUB([]any{"id", "Str"}, []any{"id", "Any"}), []any{"id", "Any"}; !reflect.DeepEqual(g, w) {
			t.Fatalf("%#v != %#v", g, w)
		}
	})
}

func Test_LUB_Nullability(t *testing.T) {
	nul := []any{"id", "Null"}
	wrap := func(tn []any) []any { return []any{"unop", "?", tn} }

	t.Run("T_or_Null_is_nullable", func(t *testing.T) {
		if g, w := LUB([]any{"id", "Int"}, nul), wrap([]any{"id", "Int"}); !reflect.DeepEqual(g, w) {
			t.Fatalf("%#v != %#v", g, w)
		}
	})
	t.Run("Null_or_Null_is_Null", func(t *testing.T) {
		if g, w := LUB(nul, nul), nul; !reflect.DeepEqual(g, w) {
			t.Fatalf("%#v != %#v", g, w)
		}
	})
	t.Run("pull_and_rehydrate", func(t *testing.T) {
		if g, w := LUB(wrap([]any{"id", "Int"}), []any{"id", "Num"}), wrap([]any{"id", "Num"}); !reflect.DeepEqual(g, w) {
			t.Fatalf("%#v != %#v", g, w)
		}
		if g, w := LUB(wrap([]any{"id", "Int"}), wrap([]any{"id", "Num"})), wrap([]any{"id", "Num"}); !reflect.DeepEqual(g, w) {
			t.Fatalf("%#v != %#v", g, w)
		}
	})
}

func Test_LUB_Arrays(t *testing.T) {
	arr := func(el []any) []any { return []any{"array", el} }

	t.Run("same_elem", func(t *testing.T) {
		if g, w := LUB(arr([]any{"id", "Int"}), arr([]any{"id", "Int"})), arr([]any{"id", "Int"}); !reflect.DeepEqual(g, w) {
			t.Fatalf("%#v != %#v", g, w)
		}
	})
	t.Run("widen_elem", func(t *testing.T) {
		if g, w := LUB(arr([]any{"id", "Int"}), arr([]any{"id", "Num"})), arr([]any{"id", "Num"}); !reflect.DeepEqual(g, w) {
			t.Fatalf("%#v != %#v", g, w)
		}
	})
	t.Run("nullable_arrays", func(t *testing.T) {
		n := func(x []any) []any { return []any{"unop", "?", x} }
		if g, w := LUB(n(arr([]any{"id", "Int"})), arr([]any{"id", "Int"})), n(arr([]any{"id", "Int"})); !reflect.DeepEqual(g, w) {
			t.Fatalf("%#v != %#v", g, w)
		}
	})
}

func Test_LUB_Maps(t *testing.T) {
	pair := func(k string, tnode []any) []any { return []any{"pair", []any{"str", k}, tnode} }
	pairReq := func(k string, tnode []any) []any { return []any{"pair!", []any{"str", k}, tnode} }
	m := func(fields ...[]any) []any {
		out := []any{"map"}
		for _, f := range fields {
			out = append(out, f)
		}
		return out
	}

	a := m(pair("a", []any{"id", "Int"}), pair("b", []any{"id", "Str"}))
	b := m(pair("a", []any{"id", "Num"}), pairReq("c", []any{"id", "Bool"}))

	t.Run("LUB_intersection_and_required_if_both", func(t *testing.T) {
		got := LUB(a, b)
		want := m(pair("a", []any{"id", "Num"})) // only 'a'; Int ⊔ Num = Num; optional
		if !reflect.DeepEqual(got, want) {
			t.Fatalf("%#v != %#v", got, want)
		}
	})

	t.Run("LUB_required_when_both_required", func(t *testing.T) {
		a2 := m(pairReq("x", []any{"id", "Int"}))
		b2 := m(pairReq("x", []any{"id", "Int"}))
		got := LUB(a2, b2)
		want := m(pairReq("x", []any{"id", "Int"}))
		if !reflect.DeepEqual(got, want) {
			t.Fatalf("%#v != %#v", got, want)
		}
	})

	t.Run("GLB_union_keys_required_if_either_with_Any_fallback", func(t *testing.T) {
		got, ok := GLB(a, b)
		if !ok {
			t.Fatalf("expected GLB to exist")
		}

		// Compare by content (order-independent) using mapFields from package.
		f := mapFields(got)
		if len(f) != 3 {
			t.Fatalf("want 3 keys, got %d", len(f))
		}

		ga, ok := f["a"]
		// FIX: 'a' is optional (neither side had pair!)
		if !ok || ga.required || !reflect.DeepEqual(ga.typ, []any{"id", "Int"}) {
			t.Fatalf("key a mismatch: %#v", ga)
		}
		gb, ok := f["b"]
		if !ok || gb.required || !reflect.DeepEqual(gb.typ, []any{"id", "Any"}) {
			t.Fatalf("key b mismatch: %#v", gb)
		}
		gc, ok := f["c"]
		if !ok || !gc.required || !reflect.DeepEqual(gc.typ, []any{"id", "Any"}) {
			t.Fatalf("key c mismatch: %#v", gc)
		}
	})

	t.Run("GLB_field_conflict_fallback_Any", func(t *testing.T) {
		x := m(pair("k", []any{"id", "Str"}))
		y := m(pair("k", []any{"id", "Bool"}))
		got, ok := GLB(x, y)
		if !ok {
			t.Fatalf("expected GLB to exist")
		}
		f := mapFields(got)
		fi := f["k"]
		if fi.required != false || !reflect.DeepEqual(fi.typ, []any{"id", "Any"}) {
			t.Fatalf("k mismatch: %#v", fi)
		}
	})
}

func Test_LUB_Enums(t *testing.T) {
	enumStr := func(vals ...string) []any {
		out := []any{"enum"}
		for _, v := range vals {
			out = append(out, []any{"str", v})
		}
		return out
	}
	enumInt := func(vals ...int64) []any {
		out := []any{"enum"}
		for _, v := range vals {
			out = append(out, []any{"int", v})
		}
		return out
	}

	t.Run("LUB_union_same_kind", func(t *testing.T) {
		if g, w := LUB(enumStr("a", "b"), enumStr("b", "c")), enumStr("a", "b", "c"); !reflect.DeepEqual(g, w) {
			t.Fatalf("%#v != %#v", g, w)
		}
	})
	t.Run("LUB_different_kinds_to_Any", func(t *testing.T) {
		if g, w := LUB(enumStr("a"), enumInt(1)), []any{"id", "Any"}; !reflect.DeepEqual(g, w) {
			t.Fatalf("%#v != %#v", g, w)
		}
	})
	t.Run("LUB_enum_with_base_widens_to_base", func(t *testing.T) {
		if g, w := LUB(enumStr("a", "b"), []any{"id", "Str"}), []any{"id", "Str"}; !reflect.DeepEqual(g, w) {
			t.Fatalf("%#v != %#v", g, w)
		}
		if g, w := LUB([]any{"id", "Int"}, enumInt(1, 2)), []any{"id", "Int"}; !reflect.DeepEqual(g, w) {
			t.Fatalf("%#v != %#v", g, w)
		}
	})
	t.Run("GLB_intersection_same_kind", func(t *testing.T) {
		got, ok := GLB(enumStr("a", "b"), enumStr("b", "c"))
		if !ok {
			t.Fatalf("expected GLB")
		}
		if w := enumStr("b"); !reflect.DeepEqual(got, w) {
			t.Fatalf("%#v != %#v", got, w)
		}
	})
	t.Run("GLB_empty_intersection_no_glb", func(t *testing.T) {
		if _, ok := GLB(enumStr("a"), enumStr("b")); ok {
			t.Fatalf("expected no GLB")
		}
	})
	t.Run("GLB_enum_with_base_is_enum", func(t *testing.T) {
		got, ok := GLB(enumInt(1, 2), []any{"id", "Int"})
		if !ok {
			t.Fatalf("expected GLB")
		}
		if w := enumInt(1, 2); !reflect.DeepEqual(got, w) {
			t.Fatalf("%#v != %#v", got, w)
		}
	})
}

func Test_LUB_Arrows(t *testing.T) {
	arrow := func(p, r []any) []any { return []any{"arrow", p, r} }
	n := func(x []any) []any { return []any{"unop", "?", x} }

	t.Run("param_GLB_success_return_LUB", func(t *testing.T) {
		got := LUB(arrow([]any{"id", "Int"}, []any{"id", "Str"}), arrow([]any{"id", "Num"}, []any{"id", "Str"}))
		want := arrow([]any{"id", "Int"}, []any{"id", "Str"})
		if !reflect.DeepEqual(got, want) {
			t.Fatalf("%#v != %#v", got, want)
		}
	})
	t.Run("param_GLB_fail_widens_to_Any", func(t *testing.T) {
		got := LUB(arrow([]any{"id", "Str"}, []any{"id", "Int"}), arrow([]any{"id", "Bool"}, []any{"id", "Int"}))
		if !reflect.DeepEqual(got, []any{"id", "Any"}) {
			t.Fatalf("%#v != Any", got)
		}
	})
	t.Run("curried_right_assoc", func(t *testing.T) {
		a := arrow([]any{"id", "Int"}, arrow([]any{"id", "Str"}, []any{"id", "Num"}))
		b := arrow([]any{"id", "Num"}, arrow([]any{"id", "Str"}, []any{"id", "Int"}))
		got := LUB(a, b)
		want := arrow([]any{"id", "Int"}, arrow([]any{"id", "Str"}, []any{"id", "Num"}))
		if !reflect.DeepEqual(got, want) {
			t.Fatalf("%#v != %#v", got, want)
		}
	})
	t.Run("nullable_return_propagates", func(t *testing.T) {
		a := arrow([]any{"id", "Int"}, n([]any{"id", "Str"}))
		b := arrow([]any{"id", "Int"}, []any{"id", "Str"})
		got := LUB(a, b)
		// FIX: only the return becomes nullable, not the whole arrow.
		want := arrow([]any{"id", "Int"}, n([]any{"id", "Str"}))
		if !reflect.DeepEqual(got, want) {
			t.Fatalf("%#v != %#v", got, want)
		}
	})
}

func Test_LUB_OpaqueAliases(t *testing.T) {
	T := []any{"id", "MyAlias"}
	S := []any{"id", "OtherAlias"}

	t.Run("same_alias_returns_itself", func(t *testing.T) {
		if g, w := LUB(T, T), T; !reflect.DeepEqual(g, w) {
			t.Fatalf("%#v != %#v", g, w)
		}
	})
	t.Run("different_aliases_widen_to_Any", func(t *testing.T) {
		if g, w := LUB(T, S), []any{"id", "Any"}; !reflect.DeepEqual(g, w) {
			t.Fatalf("%#v != %#v", g, w)
		}
	})
	t.Run("alias_vs_primitive_widens_to_Any", func(t *testing.T) {
		if g, w := LUB(T, []any{"id", "Int"}), []any{"id", "Any"}; !reflect.DeepEqual(g, w) {
			t.Fatalf("%#v != %#v", g, w)
		}
	})
}

func Test_LUB_Nullability_ShapeMix(t *testing.T) {
	n := []any{"id", "Null"}
	arr := func(el []any) []any { return []any{"array", el} }
	wrap := func(x []any) []any { return []any{"unop", "?", x} }
	pair := func(k string, tnode []any) []any { return []any{"pair", []any{"str", k}, tnode} }
	m := func(fields ...[]any) []any {
		out := []any{"map"}
		for _, f := range fields {
			out = append(out, f)
		}
		return out
	}

	t.Run("Null_vs_Array", func(t *testing.T) {
		got := LUB(n, arr([]any{"id", "Int"}))
		want := wrap(arr([]any{"id", "Int"}))
		if !reflect.DeepEqual(got, want) {
			t.Fatalf("%#v != %#v", got, want)
		}
	})
	t.Run("Nullable_vs_Map", func(t *testing.T) {
		got := LUB(wrap(m(pair("k", []any{"id", "Int"}))), m(pair("k", []any{"id", "Num"})))
		want := wrap(m(pair("k", []any{"id", "Num"})))
		if !reflect.DeepEqual(got, want) {
			t.Fatalf("%#v != %#v", got, want)
		}
	})
}

func Test_LUB_Laws_Sanity(t *testing.T) {
	A := []any{"id", "Int"}
	B := []any{"id", "Num"}
	C := []any{"id", "Str"}

	// Idempotent
	if g := LUB(A, A); !reflect.DeepEqual(g, A) {
		t.Fatalf("idempotent failed: %#v", g)
	}

	// Commutative
	if g1, g2 := LUB(A, B), LUB(B, A); !reflect.DeepEqual(g1, g2) {
		t.Fatalf("commutative failed: %#v vs %#v", g1, g2)
	}

	// Associative
	if g1, g2 := LUB(A, LUB(B, C)), LUB(LUB(A, B), C); !reflect.DeepEqual(g1, g2) {
		t.Fatalf("associative failed: %#v vs %#v", g1, g2)
	}
}

func Test_LUB_MixedShapes(t *testing.T) {
	arr := func(el []any) []any { return []any{"array", el} }
	pair := func(k string, tnode []any) []any { return []any{"pair", []any{"str", k}, tnode} }
	m := func(fields ...[]any) []any {
		out := []any{"map"}
		for _, f := range fields {
			out = append(out, f)
		}
		return out
	}
	n := func(x []any) []any { return []any{"unop", "?", x} }

	t.Run("Array_vs_Map_widens_to_Any", func(t *testing.T) {
		got := LUB(arr([]any{"id", "Int"}), m(pair("k", []any{"id", "Int"})))
		want := []any{"id", "Any"}
		if !reflect.DeepEqual(got, want) {
			t.Fatalf("%#v != %#v", got, want)
		}
	})

	t.Run("Nullable_mismatch_wraps_Any", func(t *testing.T) {
		got := LUB(n([]any{"id", "Int"}), []any{"id", "Str"})
		want := n([]any{"id", "Any"})
		if !reflect.DeepEqual(got, want) {
			t.Fatalf("%#v != %#v", got, want)
		}
	})
}

func Test_LUB_Maps_EmptyIntersection(t *testing.T) {
	pair := func(k string, tnode []any) []any { return []any{"pair", []any{"str", k}, tnode} }
	m := func(fields ...[]any) []any {
		out := []any{"map"}
		for _, f := range fields {
			out = append(out, f)
		}
		return out
	}

	a := m(pair("x", []any{"id", "Int"}))
	b := m(pair("y", []any{"id", "Int"}))
	got := LUB(a, b)
	want := m() // empty map (intersection only)
	if !reflect.DeepEqual(got, want) {
		t.Fatalf("%#v != %#v", got, want)
	}
}

func Test_LUB_Enums_WithNullability(t *testing.T) {
	enumStr := func(vals ...string) []any {
		out := []any{"enum"}
		for _, v := range vals {
			out = append(out, []any{"str", v})
		}
		return out
	}
	wrap := func(x []any) []any { return []any{"unop", "?", x} }

	t.Run("Enum_or_Null_is_nullable_Enum", func(t *testing.T) {
		got := LUB(enumStr("a", "b"), []any{"id", "Null"})
		want := wrap(enumStr("a", "b"))
		if !reflect.DeepEqual(got, want) {
			t.Fatalf("%#v != %#v", got, want)
		}
	})
}

func Test_GLB_Primitive_AnyIdentity(t *testing.T) {
	got, ok := GLB([]any{"id", "Any"}, []any{"id", "Str"})
	if !ok {
		t.Fatalf("expected GLB")
	}
	if w := []any{"id", "Str"}; !reflect.DeepEqual(got, w) {
		t.Fatalf("%#v != %#v", got, w)
	}
}

func Test_GLB_Nullability_Rules(t *testing.T) {
	n := []any{"id", "Null"}
	wrap := func(x []any) []any { return []any{"unop", "?", x} }

	t.Run("Both_nullable_inner_GLB_exists", func(t *testing.T) {
		got, ok := GLB(wrap([]any{"id", "Num"}), wrap([]any{"id", "Int"}))
		if !ok {
			t.Fatalf("expected GLB")
		}
		want := wrap([]any{"id", "Int"})
		if !reflect.DeepEqual(got, want) {
			t.Fatalf("%#v != %#v", got, want)
		}
	})
	t.Run("Mixed_nullable_requires_inner_GLB", func(t *testing.T) {
		if _, ok := GLB(wrap([]any{"id", "Str"}), []any{"id", "Bool"}); ok {
			t.Fatalf("expected no GLB for Str? vs Bool")
		}
	})
	t.Run("Null_and_Null", func(t *testing.T) {
		got, ok := GLB(n, n)
		if !ok {
			t.Fatalf("expected GLB")
		}
		if !reflect.DeepEqual(got, n) {
			t.Fatalf("%#v != %#v", got, n)
		}
	})
	t.Run("Null_and_T_no_GLB", func(t *testing.T) {
		if _, ok := GLB(n, []any{"id", "Int"}); ok {
			t.Fatalf("expected no GLB for Null vs Int")
		}
	})
}

func Test_GLB_Arrays_NoMeet(t *testing.T) {
	arr := func(el []any) []any { return []any{"array", el} }
	if _, ok := GLB(arr([]any{"id", "Str"}), arr([]any{"id", "Bool"})); ok {
		t.Fatalf("expected no GLB for [Str] vs [Bool]")
	}
}

func Test_LUB_Arrow_NullabilityOnWholeArrow(t *testing.T) {
	arrow := func(p, r []any) []any { return []any{"arrow", p, r} }
	wrap := func(x []any) []any { return []any{"unop", "?", x} }

	// Null ⊔ (Int -> Str) = (Int -> Str)?
	got := LUB([]any{"id", "Null"}, arrow([]any{"id", "Int"}, []any{"id", "Str"}))
	want := wrap(arrow([]any{"id", "Int"}, []any{"id", "Str"}))
	if !reflect.DeepEqual(got, want) {
		t.Fatalf("%#v != %#v", got, want)
	}
}

func Test_LUB_OpaqueAliases_WithNullability(t *testing.T) {
	T := []any{"id", "MyAlias"}
	wrap := func(x []any) []any { return []any{"unop", "?", x} }

	t.Run("nullable_alias_with_same_alias", func(t *testing.T) {
		if g, w := LUB(wrap(T), T), wrap(T); !reflect.DeepEqual(g, w) {
			t.Fatalf("%#v != %#v", g, w)
		}
	})
	t.Run("nullable_alias_with_primitive_widens_to_nullable_Any", func(t *testing.T) {
		if g, w := LUB(wrap(T), []any{"id", "Int"}), wrap([]any{"id", "Any"}); !reflect.DeepEqual(g, w) {
			t.Fatalf("%#v != %#v", g, w)
		}
	})
}

func Test_GLB_Maps_RequiredIfEither(t *testing.T) {
	pair := func(k string, tnode []any) []any { return []any{"pair", []any{"str", k}, tnode} }
	pairReq := func(k string, tnode []any) []any { return []any{"pair!", []any{"str", k}, tnode} }
	m := func(fields ...[]any) []any {
		out := []any{"map"}
		for _, f := range fields {
			out = append(out, f)
		}
		return out
	}

	a := m(pairReq("x", []any{"id", "Int"}))
	b := m(pair("x", []any{"id", "Int"}))
	got, ok := GLB(a, b)
	if !ok {
		t.Fatalf("expected GLB")
	}
	f := mapFields(got)
	fi := f["x"]
	if !fi.required || !reflect.DeepEqual(fi.typ, []any{"id", "Int"}) {
		t.Fatalf("x mismatch: %#v", fi)
	}
}

func Test_LUB_Primitives_NullableVsNullableDifferent(t *testing.T) {
	wrap := func(x []any) []any { return []any{"unop", "?", x} }
	got := LUB(wrap([]any{"id", "Str"}), wrap([]any{"id", "Bool"}))
	want := wrap([]any{"id", "Any"})
	if !reflect.DeepEqual(got, want) {
		t.Fatalf("%#v != %#v", got, want)
	}
}

func Test_LUB_Enums_Dedup(t *testing.T) {
	enumStr := func(vals ...string) []any {
		out := []any{"enum"}
		for _, v := range vals {
			out = append(out, []any{"str", v})
		}
		return out
	}
	t.Run("union_deduplicates", func(t *testing.T) {
		if g, w := LUB(enumStr("a", "a"), enumStr("a")), enumStr("a"); !reflect.DeepEqual(g, w) {
			t.Fatalf("%#v != %#v", g, w)
		}
	})
}

func Test_GLB_Arrays_Success(t *testing.T) {
	arr := func(el []any) []any { return []any{"array", el} }
	t.Run("[Num] ⋂ [Int] = [Int]", func(t *testing.T) {
		got, ok := GLB(arr([]any{"id", "Num"}), arr([]any{"id", "Int"}))
		if !ok {
			t.Fatalf("expected GLB")
		}
		want := arr([]any{"id", "Int"})
		if !reflect.DeepEqual(got, want) {
			t.Fatalf("%#v != %#v", got, want)
		}
	})
}

func Test_GLB_Arrays_AnyIdentity(t *testing.T) {
	arr := func(el []any) []any { return []any{"array", el} }
	t.Run("Any ⋂ [Int] = [Int]", func(t *testing.T) {
		got, ok := GLB([]any{"id", "Any"}, arr([]any{"id", "Int"}))
		if !ok {
			t.Fatalf("expected GLB")
		}
		want := arr([]any{"id", "Int"})
		if !reflect.DeepEqual(got, want) {
			t.Fatalf("%#v != %#v", got, want)
		}
	})
}

func Test_GLB_OpaqueAliases(t *testing.T) {
	A := []any{"id", "AliasA"}
	B := []any{"id", "AliasB"}

	t.Run("same_alias_glb_is_alias", func(t *testing.T) {
		got, ok := GLB(A, A)
		if !ok {
			t.Fatalf("expected GLB")
		}
		if !reflect.DeepEqual(got, A) {
			t.Fatalf("%#v != %#v", got, A)
		}
	})

	t.Run("different_aliases_no_glb", func(t *testing.T) {
		if _, ok := GLB(A, B); ok {
			t.Fatalf("expected no GLB")
		}
	})
}

func Test_LUB_Arrows_ReturnLUB_Widen(t *testing.T) {
	arrow := func(p, r []any) []any { return []any{"arrow", p, r} }
	t.Run("same_param_returns_widen", func(t *testing.T) {
		a := arrow([]any{"id", "Int"}, []any{"id", "Int"})
		b := arrow([]any{"id", "Int"}, []any{"id", "Num"})
		got := LUB(a, b)
		want := arrow([]any{"id", "Int"}, []any{"id", "Num"}) // return LUB(Int,Num)=Num
		if !reflect.DeepEqual(got, want) {
			t.Fatalf("%#v != %#v", got, want)
		}
	})
	t.Run("param_glb_ok_but_returns_disjoint_yield_Any_return", func(t *testing.T) {
		a := arrow([]any{"id", "Int"}, []any{"id", "Str"})
		b := arrow([]any{"id", "Num"}, []any{"id", "Bool"})
		got := LUB(a, b)
		// param GLB = Int, return LUB(Str,Bool)=Any
		want := arrow([]any{"id", "Int"}, []any{"id", "Any"})
		if !reflect.DeepEqual(got, want) {
			t.Fatalf("%#v != %#v", got, want)
		}
	})
}

func Test_LUB_Arrows_ParamNullableGLB(t *testing.T) {
	arrow := func(p, r []any) []any { return []any{"arrow", p, r} }
	n := func(x []any) []any { return []any{"unop", "?", x} }

	t.Run("Int? vs Int → param GLB is Int (non-nullable)", func(t *testing.T) {
		a := arrow(n([]any{"id", "Int"}), []any{"id", "Str"})
		b := arrow([]any{"id", "Int"}, []any{"id", "Str"})
		got := LUB(a, b)
		want := arrow([]any{"id", "Int"}, []any{"id", "Str"})
		if !reflect.DeepEqual(got, want) {
			t.Fatalf("%#v != %#v", got, want)
		}
	})
}

func Test_LUB_Arrows_vs_NonArrow_WidensToAny(t *testing.T) {
	arrow := func(p, r []any) []any { return []any{"arrow", p, r} }
	t.Run("arrow_vs_map", func(t *testing.T) {
		mapEmpty := []any{"map"}
		got := LUB(arrow([]any{"id", "Int"}, []any{"id", "Str"}), mapEmpty)
		want := []any{"id", "Any"}
		if !reflect.DeepEqual(got, want) {
			t.Fatalf("%#v != %#v", got, want)
		}
	})
	t.Run("nullable_arrow_vs_primitive_wraps_Any", func(t *testing.T) {
		wrap := func(x []any) []any { return []any{"unop", "?", x} }
		got := LUB(wrap(arrow([]any{"id", "Int"}, []any{"id", "Str"})), []any{"id", "Bool"})
		want := wrap([]any{"id", "Any"})
		if !reflect.DeepEqual(got, want) {
			t.Fatalf("%#v != %#v", got, want)
		}
	})
}

func Test_LUB_Alias_vs_Any(t *testing.T) {
	A := []any{"id", "AliasX"}
	t.Run("Any_dominates_alias", func(t *testing.T) {
		if g, w := LUB([]any{"id", "Any"}, A), []any{"id", "Any"}; !reflect.DeepEqual(g, w) {
			t.Fatalf("%#v != %#v", g, w)
		}
		if g, w := LUB(A, []any{"id", "Any"}), []any{"id", "Any"}; !reflect.DeepEqual(g, w) {
			t.Fatalf("%#v != %#v", g, w)
		}
	})
}

func Test_LUB_Enums_PrimitiveBothWays(t *testing.T) {
	enumInt := func(vals ...int64) []any {
		out := []any{"enum"}
		for _, v := range vals {
			out = append(out, []any{"int", v})
		}
		return out
	}
	t.Run("Enum ⊔ Int = Int (left)", func(t *testing.T) {
		if g, w := LUB(enumInt(1, 2), []any{"id", "Int"}), []any{"id", "Int"}; !reflect.DeepEqual(g, w) {
			t.Fatalf("%#v != %#v", g, w)
		}
	})
	t.Run("Int ⊔ Enum = Int (right)", func(t *testing.T) {
		if g, w := LUB([]any{"id", "Int"}, enumInt(1, 2)), []any{"id", "Int"}; !reflect.DeepEqual(g, w) {
			t.Fatalf("%#v != %#v", g, w)
		}
	})
}
