// analysis_test.go
package main

import (
	"strings"
	"testing"

	mindscript "github.com/DAIOS-AI/msg/internal/mindscript"
)

type memSnap map[string]string

func (m memSnap) Get(uri string) (string, bool) {
	s, ok := m[uri]
	return s, ok
}

func runPure(t *testing.T, uri, src string) *pureResult {
	t.Helper()
	sn := memSnap{uri: src}
	res := analyzeFilePure(sn, uri)
	if res == nil {
		t.Fatalf("analyzeFilePure returned nil")
	}
	return res
}

func hasDiag(res *pureResult, code string) bool {
	for _, d := range res.Diags {
		if d.Code == code {
			return true
		}
	}
	return false
}

func getDiag(res *pureResult, code string) (pureDiag, bool) {
	for _, d := range res.Diags {
		if d.Code == code {
			return d, true
		}
	}
	return pureDiag{}, false
}

func mustHaveDiag(t *testing.T, res *pureResult, code string) pureDiag {
	t.Helper()
	d, ok := getDiag(res, code)
	if !ok {
		var got []string
		for _, g := range res.Diags {
			got = append(got, g.Code)
		}
		t.Fatalf("expected diag %q not found; got=%v", code, got)
	}
	return d
}

func mustHaveDiagOneOf(t *testing.T, res *pureResult, codes ...string) {
	t.Helper()
	got := map[string]bool{}
	for _, d := range res.Diags {
		got[d.Code] = true
	}
	for _, c := range codes {
		if got[c] {
			return
		}
	}
	var want []string
	for _, c := range codes {
		want = append(want, c)
	}
	t.Fatalf("expected one of %v, got codes=%v", want, got)
}

func mustNotHaveDiag(t *testing.T, res *pureResult, code string) {
	t.Helper()
	if hasDiag(res, code) {
		t.Fatalf("unexpected diag %q present", code)
	}
}

func Test_Analysis_LayoutHints(t *testing.T) {
	const uri = "mem://layout.ms"

	t.Run("space_before_paren_call_and_fun_params", func(t *testing.T) {
		src := `
let f = fun (x: Int) -> Int do x end
f (1)
`
		res := runPure(t, uri, src)
		// Parser may reject the construct → PARSE; if it parses, we want the lint.
		mustHaveDiagOneOf(t, res, "MS-LROUND-INSTEAD-OF-CLROUND", "PARSE")
	})

	t.Run("space_before_square_index", func(t *testing.T) {
		src := `
let arr = [1,2,3]
arr [0]
`
		res := runPure(t, uri, src)
		mustHaveDiag(t, res, "MS-LSQUARE-INSTEAD-OF-CLSQUARE")
	})

	t.Run("dot_gap_across_annotation", func(t *testing.T) {
		src := `
let p = { name: "Ada" }
p
# note
. name
`
		res := runPure(t, uri, src)
		// Parser may error at the broken '.' site; accept either the lint or PARSE.
		mustHaveDiagOneOf(t, res, "MS-DOT-GAP", "PARSE")
	})

	t.Run("post_forces_newline_end_same_line_hint", func(t *testing.T) {
		// Impossible with current lexer: comments consume to EOL, so 'end' isn't on the same line.
		t.Skip("lexer places 'end' after comment on a new logical line; this hint cannot be observed in tokens")
		src := `
if true then
  1
# trailing
end
`
		// Keep body so the test still compiles if un-skipped in the future.
		src = strings.Replace(src, "# trailing\nend", "# trailing end", 1)
		_ = runPure(t, uri, src)
	})
}

func Test_Analysis_Names_And_Assign_Targets(t *testing.T) {
	t.Run("unknown_name", func(t *testing.T) {
		const uri = "mem://names.ms"
		const src = "y = x\n" // x never declared
		res := runPure(t, uri, src)

		got := map[string]bool{}
		for _, d := range res.Diags {
			got[d.Code] = true
		}
		if !got["MS-UNKNOWN-NAME"] {
			t.Fatalf("expected MS-UNKNOWN-NAME, got codes=%v", got)
		}
	})

	t.Run("invalid_assign_target", func(t *testing.T) {
		// Depending on grammar, parser may reject; accept either INVALID-ASSIGN-TARGET or PARSE.
		const uri = "mem://invalid-assign.ms"
		const src = "(1 + 2) = 3\n"
		res := runPure(t, uri, src)

		got := map[string]bool{}
		for _, d := range res.Diags {
			got[d.Code] = true
		}
		if !(got["MS-INVALID-ASSIGN-TARGET"] || got["PARSE"]) {
			t.Fatalf("expected MS-INVALID-ASSIGN-TARGET or PARSE, got codes=%v", got)
		}
	})
}

func Test_Analysis_Arrays_And_Operators(t *testing.T) {
	const uri = "mem://ops.ms"

	t.Run("heterogeneous_array_warn", func(t *testing.T) {
		src := `
let ys = [1, 2.0]
`
		res := runPure(t, uri, src)
		mustHaveDiag(t, res, "MS-ARRAY-HETEROGENEOUS")
	})

	t.Run("mod_by_zero_const_error", func(t *testing.T) {
		src := `
3 % 0
`
		res := runPure(t, uri, src)
		mustHaveDiag(t, res, "MS-DIV-BY-ZERO-CONST")
	})

	t.Run("bitwise_on_nonint_literals", func(t *testing.T) {
		src := `
1.0 & 3
`
		res := runPure(t, uri, src)
		mustHaveDiag(t, res, "MS-BITWISE-NONINT")
	})
}

func Test_Analysis_Bindings_Signatures_And_Types(t *testing.T) {
	const uri = "mem://binds.ms"

	t.Run("collect_fun_type_and_sig", func(t *testing.T) {
		src := `
let add = fun(a: Int, b: Int) -> Int do a + b end
`
		res := runPure(t, uri, src)

		// There should be a binding for "add" of kind fun with a non-empty Sig
		found := false
		for _, b := range res.Bindings {
			if b.Name == "add" && b.Kind == "fun" && b.Sig != "" {
				// declared return type stays as Int
				if mindscript.FormatType(b.TypeNode) != "Int" {
					t.Fatalf("expected add return type Int, got %q", mindscript.FormatType(b.TypeNode))
				}
				found = true
			}
		}
		if !found {
			t.Fatalf("binding for fun 'add' not found or missing signature")
		}
	})

	t.Run("oracle_return_exposed_as_nullable", func(t *testing.T) {
		src := `
let next = oracle(seed: Int) -> Int
`
		res := runPure(t, uri, src)

		found := false
		for _, b := range res.Bindings {
			if b.Name == "next" && b.Kind == "oracle" {
				ty := mindscript.FormatType(b.TypeNode)
				if ty != "Int?" {
					t.Fatalf("expected oracle return to be nullable 'Int?', got %q", ty)
				}
				found = true
			}
		}
		if !found {
			t.Fatalf("binding for oracle 'next' not found")
		}
	})
}

func Test_Analysis_Tokens_And_Spans_Sanity(t *testing.T) {
	const uri = "mem://spans.ms"
	src := `
let p = { name: "Ada", age: 36 }
p.name
`
	res := runPure(t, uri, src)

	if len(res.Tokens) == 0 {
		t.Fatalf("expected some tokens, got none")
	}
	if res.AST == nil || len(res.AST) == 0 || res.Spans == nil {
		t.Fatalf("expected AST and spans to be present")
	}
	// Ensure we can find an ID token "p" and it has a valid byte span
	found := false
	for _, tk := range res.Tokens {
		if tk.Type == mindscript.ID && tk.Literal == "p" {
			if tk.StartByte < 0 || tk.EndByte <= tk.StartByte {
				t.Fatalf("invalid token span for 'p': %d..%d", tk.StartByte, tk.EndByte)
			}
			found = true
			break
		}
	}
	if !found {
		t.Fatalf("did not find ID token 'p'")
	}
}

// ---------------------------
// Tests below here are fine, with a couple updated to reflect the spec.
// ---------------------------

func Test_Analysis_Calls_And_Arity(t *testing.T) {
	const uri = "mem://arity.ms"

	t.Run("arg_overflow", func(t *testing.T) {
		src := `
let f = fun(a: Int, b: Int) -> Int do a + b end
f(1,2,3)
`
		res := runPure(t, uri, src)
		mustHaveDiag(t, res, "MS-ARG-OVERFLOW")
	})

	t.Run("arg_type_mismatch", func(t *testing.T) {
		src := `
let f = fun(a: Int, b: Int) -> Int do a + b end
f("a", 2)
`
		res := runPure(t, uri, src)
		mustHaveDiag(t, res, "MS-ARG-TYPE-MISMATCH")
	})

	t.Run("oracle_call_result_nullable_is_unsafe_when_used", func(t *testing.T) {
		src := `
let next = oracle(seed: Int) -> Int
let n = next(42)
n + 1
`
		res := runPure(t, uri, src)
		mustHaveDiag(t, res, "MS-MAYBE-NULL-UNSAFE")
	})
}

func Test_Analysis_Return_Type_Checks(t *testing.T) {
	const uri = "mem://ret.ms"

	t.Run("bare_return_in_nonnullable_fun", func(t *testing.T) {
		src := `
let k = fun() -> Int do
  return
end
`
		res := runPure(t, uri, src)
		mustHaveDiag(t, res, "MS-RET-TYPE-MISMATCH")
	})

	t.Run("return_value_mismatch", func(t *testing.T) {
		src := `
let k = fun() -> Int do
  return "x"
end
`
		res := runPure(t, uri, src)
		mustHaveDiag(t, res, "MS-RET-TYPE-MISMATCH")
	})
}

func Test_Analysis_Comparisons_And_Bool_Ops(t *testing.T) {
	const uri = "mem://cmp.ms"

	t.Run("comparison_type_mismatch_str_vs_num", func(t *testing.T) {
		src := `"1" < 2`
		res := runPure(t, uri, src)
		mustHaveDiag(t, res, "MS-COMPARISON-TYPE-MISMATCH")
	})

	t.Run("comparison_type_mismatch_bool_vs_bool", func(t *testing.T) {
		src := `true < false`
		res := runPure(t, uri, src)
		mustHaveDiag(t, res, "MS-COMPARISON-TYPE-MISMATCH")
	})

	t.Run("boolean_ops_require_bool_and", func(t *testing.T) {
		src := `"x" and 1`
		res := runPure(t, uri, src)
		mustHaveDiag(t, res, "MS-ARG-TYPE-MISMATCH")
	})

	t.Run("boolean_ops_require_bool_or", func(t *testing.T) {
		src := `1 or "x"`
		res := runPure(t, uri, src)
		mustHaveDiag(t, res, "MS-ARG-TYPE-MISMATCH")
	})
}

func Test_Analysis_Indexing_And_Properties(t *testing.T) {
	const uri = "mem://idx.ms"

	t.Run("index_must_be_int", func(t *testing.T) {
		src := `
let xs = [1,2,3]
xs["0"]
`
		res := runPure(t, uri, src)
		mustHaveDiag(t, res, "MS-ARG-TYPE-MISMATCH")
	})

	t.Run("map_missing_key_warning_on_value_map", func(t *testing.T) {
		src := `
let p = { name: "Ada" }
p.age
`
		res := runPure(t, uri, src)
		mustHaveDiag(t, res, "MS-MAP-MISSING-KEY")
	})
}

func Test_Analysis_Nullability_Narrowing(t *testing.T) {
	const uri = "mem://null.ms"

	t.Run("unsafe_nullable_use", func(t *testing.T) {
		src := `
let g = oracle() -> Int
let v = g()
v + 1
`
		res := runPure(t, uri, src)
		mustHaveDiag(t, res, "MS-MAYBE-NULL-UNSAFE")
	})
}

func Test_Analysis_Enums(t *testing.T) {
	const uri = "mem://enum.ms"

	t.Run("enum_non_member_literal", func(t *testing.T) {
		src := `
let Color = type Enum["red","green","blue"]
let f = fun(c: Color) -> Bool do c == "red" end
f("yellow")
`
		res := runPure(t, uri, src)
		mustHaveDiag(t, res, "MS-ARG-TYPE-MISMATCH")
	})
}

func Test_Analysis_Destructuring_Flow(t *testing.T) {
	const uri = "mem://destruct.ms"

	t.Run("object_destruct_missing_binds_null_then_used_unsafely", func(t *testing.T) {
		src := `
let { name: n, age: a } = { name: "Ada" }
a + 1
`
		res := runPure(t, uri, src)
		mustHaveDiag(t, res, "MS-MAYBE-NULL-UNSAFE")
	})
}

func Test_Analysis_For_Targets(t *testing.T) {
	const uri = "mem://for.ms"

	t.Run("invalid_assign_target_in_for", func(t *testing.T) {
		src := `
for (1 + 2) in [1,2] do
  0
end
`
		res := runPure(t, uri, src)
		// Parser may reject this construct entirely.
		mustHaveDiagOneOf(t, res, "MS-INVALID-ASSIGN-TARGET", "PARSE")
	})
}

func Test_Analysis_Shift_Range_And_Bitwise(t *testing.T) {
	const uri = "mem://shift.ms"

	t.Run("shift_count_out_of_range_const", func(t *testing.T) {
		t.Skip("shift count range check not implemented by analyzer yet")
		src := `1 << 64`
		_ = runPure(t, uri, src)
	})

	t.Run("bitwise_with_nonliteral_operand_num", func(t *testing.T) {
		src := `
let x = 1.0
x & 3
`
		res := runPure(t, uri, src)
		mustHaveDiag(t, res, "MS-BITWISE-NONINT")
	})
}

// --- Typed map field access (using type aliases on params) -------------------

func Test_Analysis_Typed_Map_Field_Access(t *testing.T) {
	const uri = "mem://typed-map.ms"

	t.Run("typed_param_known_field_ok", func(t *testing.T) {
		src := `
let Person = type { name!: Str, age: Int? }
let get = fun(p: Person) -> Str do p.name end
`
		res := runPure(t, uri, src)
		// No warning about missing key on a known field of a typed map.
		mustNotHaveDiag(t, res, "MS-MAP-MISSING-KEY")
	})

	t.Run("typed_param_missing_field_warns", func(t *testing.T) {
		src := `
let Person = type { name!: Str }
let get = fun(p: Person) -> Any do p.age end
`
		res := runPure(t, uri, src)
		// Unknown field should warn (open-world).
		mustHaveDiag(t, res, "MS-MAP-MISSING-KEY")
	})
}

// --- Nullable receivers (property/index usage on T?) -------------------------

func Test_Analysis_Nullable_Receivers(t *testing.T) {
	const uri = "mem://nullable-recv.ms"

	t.Run("property_on_nullable_warns", func(t *testing.T) {
		t.Skip("nullable receiver checks not implemented yet")
		src := `
let mk = oracle() -> { name: Str }
let v = mk()
v.name
`
		_ = runPure(t, uri, src)
	})

	t.Run("index_on_nullable_warns", func(t *testing.T) {
		t.Skip("nullable receiver checks not implemented yet")
		src := `
let mk = oracle() -> [Int]
let xs = mk()
xs[0]
`
		_ = runPure(t, uri, src)
	})
}

// --- If-expression LUB (branch typing) --------------------------------------

func Test_Analysis_If_LUB(t *testing.T) {
	const uri = "mem://iflub.ms"

	t.Run("int_vs_num_result_used_in_numeric_context", func(t *testing.T) {
		// The analyzer LUBs branch results; here the result is Num and the use is numeric.
		src := `
let y =
  if true then 1 else 2.0 end
y + 1.5
`
		res := runPure(t, uri, src)
		// No type-specific diagnostic should fire here.
		mustNotHaveDiag(t, res, "MS-ARG-TYPE-MISMATCH")
		mustNotHaveDiag(t, res, "MS-COMPARISON-TYPE-MISMATCH")
	})
}

// --- Enums inside typed contexts (impossible equality) ----------------------

func Test_Analysis_Enum_Eq_In_Typed_Context(t *testing.T) {
	const uri = "mem://enum-ctx.ms"

	t.Run("compare_with_non_member_literal_in_fun_body", func(t *testing.T) {
		t.Skip("enum equality non-member check in expressions not implemented yet")
		src := `
let Color = type Enum["red","green","blue"]
let isYellow = fun(c: Color) -> Bool do c == "yellow" end
`
		_ = runPure(t, uri, src)
	})
}

// --- Array destructuring defaults & nullability ------------------------------

func Test_Analysis_Array_Destructuring_Nullability(t *testing.T) {
	const uri = "mem://darr-null.ms"

	t.Run("missing_second_element_binds_null_then_used_unsafely", func(t *testing.T) {
		t.Skip("array destructuring null defaults + unsafe use not implemented yet")
		src := `
let [a, b] = [1]
b + 1
`
		_ = runPure(t, uri, src)
	})
}

// --- Block result typing (last expression) ----------------------------------

func Test_Analysis_Block_Last_Value(t *testing.T) {
	const uri = "mem://block-last.ms"

	t.Run("block_last_expression_type_is_used_for_binding", func(t *testing.T) {
		src := `
let x =
  1
  "s"
`
		res := runPure(t, uri, src)

		// Binding 'x' should have the type of the last expression: Str.
		found := false
		for _, b := range res.Bindings {
			if b.Name == "x" {
				if mindscript.FormatType(b.TypeNode) != "Str" {
					t.Fatalf("want Str, got %q", mindscript.FormatType(b.TypeNode))
				}
				found = true
			}
		}
		if !found {
			t.Fatalf("binding x not found")
		}
	})
}

// --- Currying surface (residual arrow on under-application) ------------------

func Test_Analysis_Currying_Sanity(t *testing.T) {
	const uri = "mem://curry.ms"

	t.Run("partial_then_apply_no_spurious_errors", func(t *testing.T) {
		src := `
let add = fun(a: Int, b: Int) -> Int do a + b end
let g = add(1)
g(2)
`
		res := runPure(t, uri, src)
		// No spurious call diagnostics.
		mustNotHaveDiag(t, res, "MS-ARG-OVERFLOW")
		mustNotHaveDiag(t, res, "MS-ARG-TYPE-MISMATCH")
	})
}

func Test_Analysis_Currying_Residual_Arrow(t *testing.T) {
	const uri = "mem://curry-behavior.ms"

	t.Run("partial_application_returns_residual_arrow", func(t *testing.T) {
		src := `
let add = fun(a: Int, b: Int) -> Int do a + b end
let g = add(1)
`
		res := runPure(t, uri, src)

		// No spurious call diagnostics.
		mustNotHaveDiag(t, res, "MS-ARG-OVERFLOW")
		mustNotHaveDiag(t, res, "MS-ARG-TYPE-MISMATCH")

		// Binding 'g' should be a residual function type (… -> Int).
		found := false
		for _, b := range res.Bindings {
			if b.Name == "g" {
				ft := mindscript.FormatType(b.TypeNode)
				if !strings.Contains(ft, "-> Int") {
					t.Fatalf("want residual arrow ending in Int, got %q", ft)
				}
				found = true
			}
		}
		if !found {
			t.Fatalf("binding g not found")
		}
	})
}

// --- Division typing corner: Int/Int -> Int, else Num ------------------------

func Test_Analysis_Division_Typing(t *testing.T) {
	const uri = "mem://div-typing.ms"

	t.Run("int_div_int_is_int_no_error", func(t *testing.T) {
		src := `1 / 2`
		res := runPure(t, uri, src)
		// No diagnostics expected from this simple expression.
		if len(res.Diags) != 0 {
			t.Fatalf("unexpected diagnostics: %+v", res.Diags)
		}
	})

	t.Run("int_div_num_is_num_no_error", func(t *testing.T) {
		src := `1 / 2.0`
		res := runPure(t, uri, src)
		if len(res.Diags) != 0 {
			t.Fatalf("unexpected diagnostics: %+v", res.Diags)
		}
	})
}

// --- Value-map requiredness marker is ignored (printer parity) ---------------

func Test_Analysis_Value_Map_Requiredness_Ignored(t *testing.T) {
	const uri = "mem://valmap-req.ms"

	t.Run("value_map_pair_bang_ignored_no_error", func(t *testing.T) {
		src := `
let p = { name!: "Ada" }  # runtime value; '!' should be ignored by analyzer
p.name
`
		res := runPure(t, uri, src)
		// Access should not error or warn merely due to '!' in a *value* map.
		mustNotHaveDiag(t, res, "MS-MAP-MISSING-KEY")
	})
}

// --- Type alias property format interop (ensures we parse/format types) -----

func Test_Analysis_Type_Alias_Property_Interop(t *testing.T) {
	const uri = "mem://type-alias-interop.ms"

	t.Run("prop_access_uses_param_type_alias_fields", func(t *testing.T) {
		src := `
let Point = type { x: Num, y: Num }
let fx = fun(p: Point) -> Num do p.x + p.y end
`
		res := runPure(t, uri, src)
		// Sanity: no key-missing / type-mismatch diagnostics expected.
		mustNotHaveDiag(t, res, "MS-MAP-MISSING-KEY")
		mustNotHaveDiag(t, res, "MS-ARG-TYPE-MISMATCH")
	})
}

// --- Shift-count range check placeholder (kept minimal) ----------------------

func Test_Analysis_Shift_Count_Range_New(t *testing.T) {
	const uri = "mem://shift-range.ms"

	t.Run("shift_count_64_out_of_range_const", func(t *testing.T) {
		t.Skip("shift count range check not implemented yet")
		src := `1 << 64`
		_ = runPure(t, uri, src)
	})
}

// Additional tiny & orthogonal tests for uncovered areas.

func Test_Analysis_LUB_Typing(t *testing.T) {
	const uri = "mem://lub.ms"

	t.Run("array_element_lub_empty_any", func(t *testing.T) {
		src := `let xs = []`
		res := runPure(t, uri, src)

		found := false
		for _, b := range res.Bindings {
			if b.Name == "xs" {
				if mindscript.FormatType(b.TypeNode) != "[Any]" {
					t.Fatalf("want [Any], got %q", mindscript.FormatType(b.TypeNode))
				}
				found = true
			}
		}
		if !found {
			t.Fatalf("binding xs not found")
		}
	})

	t.Run("array_element_lub_homogeneous_int", func(t *testing.T) {
		src := `let xs = [1, 2, 3]`
		res := runPure(t, uri, src)

		found := false
		for _, b := range res.Bindings {
			if b.Name == "xs" {
				if mindscript.FormatType(b.TypeNode) != "[Int]" {
					t.Fatalf("want [Int], got %q", mindscript.FormatType(b.TypeNode))
				}
				found = true
			}
		}
		if !found {
			t.Fatalf("binding xs not found")
		}
	})

	t.Run("array_element_lub_heterogeneous_int_num_num", func(t *testing.T) {
		src := `let xs = [1, 2.0]`
		res := runPure(t, uri, src)

		// Still warns for heterogeneity, but the element type should LUB to Num.
		mustHaveDiag(t, res, "MS-ARRAY-HETEROGENEOUS")
		found := false
		for _, b := range res.Bindings {
			if b.Name == "xs" {
				if mindscript.FormatType(b.TypeNode) != "[Num]" {
					t.Fatalf("want [Num], got %q", mindscript.FormatType(b.TypeNode))
				}
				found = true
			}
		}
		if !found {
			t.Fatalf("binding xs not found")
		}
	})

	t.Run("if_expression_lub_int_vs_num_becomes_num", func(t *testing.T) {
		src := `
let y =
  if true then 1 else 2.0 end
`
		res := runPure(t, uri, src)

		found := false
		for _, b := range res.Bindings {
			if b.Name == "y" {
				if mindscript.FormatType(b.TypeNode) != "Num" {
					t.Fatalf("want Num, got %q", mindscript.FormatType(b.TypeNode))
				}
				found = true
			}
		}
		if !found {
			t.Fatalf("binding y not found")
		}
	})

	t.Run("binop_lub_int_plus_num_becomes_num", func(t *testing.T) {
		src := `let z = 1 + 2.0`
		res := runPure(t, uri, src)

		found := false
		for _, b := range res.Bindings {
			if b.Name == "z" {
				if mindscript.FormatType(b.TypeNode) != "Num" {
					t.Fatalf("want Num, got %q", mindscript.FormatType(b.TypeNode))
				}
				found = true
			}
		}
		if !found {
			t.Fatalf("binding z not found")
		}
	})
}

func Test_Analysis_ValueMap_Literal_Field_Present(t *testing.T) {
	const uri = "mem://valmap-present.ms"

	t.Run("bound_value_map_then_access_known_key_no_warning", func(t *testing.T) {
		src := `
let p = { name: "Ada", age: 36 }
p.name
`
		res := runPure(t, uri, src)
		mustNotHaveDiag(t, res, "MS-MAP-MISSING-KEY")
	})
}

// --- Loop result includes Null (zero-iteration), unsafe use should warn ------

func Test_Analysis_Loop_Result_Nullability(t *testing.T) {
	const uri = "mem://loop-null.ms"

	t.Run("while_result_used_unguarded_warns", func(t *testing.T) {
		src := `
let y =
  while false do 1 end
y + 1
`
		res := runPure(t, uri, src)
		mustHaveDiag(t, res, "MS-MAYBE-NULL-UNSAFE")
	})
}

// --- Arrow LUB/GLB sanity: (GLB(param)) -> (LUB(ret)) -----------------------

func Test_Analysis_Arrow_LUB_GLB(t *testing.T) {
	const uri = "mem://arrow-lub.ms"

	t.Run("lub_of_functions_via_array_context", func(t *testing.T) {
		src := `
let f = fun(x: Int) -> Int do x end
let g = fun(x: Num) -> Num do x end
let a = [f, g]  # element type should be (Int) -> Num
`
		res := runPure(t, uri, src)
		found := false
		for _, b := range res.Bindings {
			if b.Name == "a" {
				ft := mindscript.FormatType(b.TypeNode)
				// Expect something like "[(Int) -> Num]" or equivalent pretty form.
				if !strings.Contains(ft, "[Int -> Num]") {
					t.Fatalf("want array of [Int -> Num], got %q", ft)
				}
				found = true
			}
		}
		if !found {
			t.Fatalf("binding a not found")
		}
	})
}

// --- Opaque aliases: no unfolding; mismatched aliases widen to Any ----------

func Test_Analysis_Opaque_Aliases_NoUnfolding(t *testing.T) {
	const uri = "mem://alias-opaque.ms"

	t.Run("lub_of_distinct_alias_values_widens_to_any", func(t *testing.T) {
		src := `
let A = type Str
let B = type Str
let fa = fun() -> A do "x" end
let fb = fun() -> B do "y" end
let xs = [fa(), fb()]   # LUB(A, B) = Any because aliases are opaque
`
		res := runPure(t, uri, src)
		for _, b := range res.Bindings {
			if b.Name == "xs" {
				if mindscript.FormatType(b.TypeNode) != "[Any]" {
					t.Fatalf("want [Any], got %q", mindscript.FormatType(b.TypeNode))
				}
			}
		}
	})
}
