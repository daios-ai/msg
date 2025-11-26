// analysis_token_test.go
package main

import (
	"strings"
	"testing"

	mindscript "github.com/daios-ai/msg/internal/mindscript"
)

//
// A. TokenIndex construction & search
//

func Test_Analysis_TokenIndex_Basics(t *testing.T) {
	t.Run("A1_BuildsAndSortsAndKinds", func(t *testing.T) {
		src := "let x = 1 + 2"
		env := mindscript.NewEnv(nil)
		ti := TokenIndexFromLexer(src, env)
		// Strictly increasing by Start.
		for i := 1; i < len(ti.Entries); i++ {
			if !(ti.Entries[i-1].Start < ti.Entries[i].Start) {
				t.Fatalf("entries not strictly increasing by Start at %d", i)
			}
		}
		// Spot-check kinds we expect to be present.
		var haveLetKW, haveXID, haveEqOp, haveLit1, havePlus, haveLit2 bool
		for _, e := range ti.Entries {
			switch {
			case e.Text == "let" && e.Kind == TokKeyword:
				haveLetKW = true
			case e.Text == "x" && e.Kind == TokIdentifier:
				haveXID = true
			case e.Text == "=" && e.Kind == TokOperator:
				haveEqOp = true
			case e.Text == "1" && e.Kind == TokLiteral:
				haveLit1 = true
			case e.Text == "+" && e.Kind == TokOperator:
				havePlus = true
			case e.Text == "2" && e.Kind == TokLiteral:
				haveLit2 = true
			}
		}
		if !(haveLetKW && haveXID && haveEqOp && haveLit1 && havePlus && haveLit2) {
			t.Fatalf("missing expected tokens/kinds; got=%+v", ti.Entries)
		}
	})

	t.Run("A2_BinarySearch_Find", func(t *testing.T) {
		src := "let x = 1 + 2"
		env := mindscript.NewEnv(nil)
		ti := TokenIndexFromLexer(src, env)

		// Inside "x"
		off := testByteOffset(src, "x", 1)
		if off < 0 {
			t.Fatal("failed to locate 'x'")
		}
		if tok := testFindTokenAtOffset(ti, off); tok == nil || tok.Text != "x" {
			t.Fatalf("Find at 'x' offset returned %v", tok)
		}

		// In whitespace between "x" and "=" → nil
		offWS := testByteOffset(src, "x", 1) + 1 // the space after x
		if tok := testFindTokenAtOffset(ti, offWS); tok != nil {
			t.Fatalf("Find in whitespace should be nil, got %v", tok)
		}

		// Exactly at End of "x" (half-open) → should NOT return "x"
		offEndX := testByteOffset(src, "x", 1) + len("x")
		if tok := testFindTokenAtOffset(ti, offEndX); tok != nil && tok.Text == "x" {
			t.Fatalf("Find at token End should not return that token; got %v", tok)
		}
	})

	t.Run("A3_Literals_PreTyped", func(t *testing.T) {
		src := `["s", 1, 1.2, true, null]`
		env := mindscript.NewEnv(nil)
		ti := TokenIndexFromLexer(src, env)
		var a Analyzer
		ip := a.ensureIP()

		want := map[string]string{
			`"s"`:  "Str",
			"1":    "Int",
			"1.2":  "Num",
			"true": "Bool",
			"null": "Null",
		}
		got := map[string]bool{}
		for _, e := range ti.Entries {
			if typ, ok := want[e.Text]; ok {
				if e.Payload == (mindscript.Value{}) {
					t.Fatalf("literal %q missing payload", e.Text)
				}
				if have := testFmtType(ip, env, e.Payload); have != typ {
					t.Fatalf("literal %q type: want %s, got %s", e.Text, typ, have)
				}
				got[e.Text] = true
			}
		}
		for k := range want {
			if !got[k] {
				t.Fatalf("did not see literal %q in index", k)
			}
		}
	})
}

//
// B. emitIdent span alignment & enrichment via Analyze
//

func Test_Analysis_EmitIdent_Alignment(t *testing.T) {
	t.Run("B1_LetDecl_EnrichesExistingToken", func(t *testing.T) {
		src := "let x = 1"
		idx := testAnalyze(src)
		var a Analyzer
		ip := a.ensureIP()

		// Find the only 'x'
		var xTok *TokenEntry
		for i := range idx.Tokens.Entries {
			if idx.Tokens.Entries[i].Text == "x" {
				xTok = &idx.Tokens.Entries[i]
				break
			}
		}
		if xTok == nil {
			t.Fatalf("missing token 'x'")
		}
		if !xTok.IsDecl {
			t.Fatalf("'x' should be a declaration")
		}
		if got := testFmtType(ip, idx.RootEnv, xTok.Payload); got != "Enum[1]" {
			t.Fatalf("decl 'x' type: want Int, got %s", got)
		}
	})

	t.Run("B2_UseSite_Enriched_NonDecl", func(t *testing.T) {
		src := "let x = 1\nx\n"
		idx := testAnalyze(src)
		var uses []TokenEntry
		for _, e := range idx.Tokens.Entries {
			if e.Text == "x" && !e.IsDecl {
				uses = append(uses, e)
			}
		}
		if len(uses) == 0 {
			t.Fatalf("expected a non-decl use of x")
		}
		var a Analyzer
		ip := a.ensureIP()
		if got := testFmtType(ip, idx.RootEnv, uses[0].Payload); got != "Enum[1]" {
			t.Fatalf("use-site 'x' type: want Int, got %s", got)
		}
	})

	t.Run("B3_Params_ExactIds_Enriched", func(t *testing.T) {
		src := "let f = fun(a: Int, b: Int) -> Int do a + b end"
		idx := testAnalyze(src)
		// We should see 'a' and 'b' with IsDecl=true and type Int.
		var aTok, bTok *TokenEntry
		for i := range idx.Tokens.Entries {
			e := &idx.Tokens.Entries[i]
			if e.Text == "a" && e.IsDecl {
				aTok = e
			}
			if e.Text == "b" && e.IsDecl {
				bTok = e
			}
		}
		if aTok == nil || bTok == nil {
			t.Fatalf("missing param tokens a/b (decl)")
		}
		var an Analyzer
		ip := an.ensureIP()
		if got := testFmtType(ip, idx.RootEnv, aTok.Payload); got != "Int" {
			t.Fatalf("param a type: want Int, got %s", got)
		}
		if got := testFmtType(ip, idx.RootEnv, bTok.Payload); got != "Int" {
			t.Fatalf("param b type: want Int, got %s", got)
		}
	})

	t.Run("B4_Destructuring_Object_Enriched", func(t *testing.T) {
		src := `let { name: n, age: a } = { name: "Ada", age: 36 }`
		idx := testAnalyze(src)
		var nTok, aTok *TokenEntry
		for i := range idx.Tokens.Entries {
			e := &idx.Tokens.Entries[i]
			if e.Text == "n" && e.IsDecl {
				nTok = e
			}
			if e.Text == "a" && e.IsDecl {
				aTok = e
			}
		}
		if nTok == nil || aTok == nil {
			t.Fatalf("missing destructured decl tokens n/a")
		}
		var an Analyzer
		ip := an.ensureIP()
		// name: "Ada" is now a singleton enum
		if got := testFmtType(ip, idx.RootEnv, nTok.Payload); got != "Enum[\"Ada\"]" {
			t.Fatalf("n type: want Str, got %s", got)
		}
		if got := testFmtType(ip, idx.RootEnv, aTok.Payload); got != "Enum[36]" {
			t.Fatalf("a type: want Enum[36], got %s", got)
		}
	})

	t.Run("B5_Destructuring_Array_Enriched", func(t *testing.T) {
		src := `let [a, b] = [1, 2]`
		idx := testAnalyze(src)
		var aTok, bTok *TokenEntry
		for i := range idx.Tokens.Entries {
			e := &idx.Tokens.Entries[i]
			if e.Text == "a" && e.IsDecl {
				aTok = e
			}
			if e.Text == "b" && e.IsDecl {
				bTok = e
			}
		}
		if aTok == nil || bTok == nil {
			t.Fatalf("missing array-destructured decl tokens a/b")
		}
		var an Analyzer
		ip := an.ensureIP()
		if got := testFmtType(ip, idx.RootEnv, aTok.Payload); got != "Enum[1]" {
			t.Fatalf("a type: want Enum[1], got %s", got)
		}
		if got := testFmtType(ip, idx.RootEnv, bTok.Payload); got != "Enum[2]" {
			t.Fatalf("b type: want Enum[2], got %s", got)
		}
	})

	t.Run("B6_Assignment_Updates_NoExtraTokens", func(t *testing.T) {
		src := "let x = 1\nx = 2\n"
		idx := testAnalyze(src)
		// We expect exactly 2 'x' tokens: one decl, one use.
		xs := testTokensWithText(idx.Tokens, "x")
		if len(xs) != 2 {
			t.Fatalf("expected exactly 2 tokens 'x' (decl+use), got %d", len(xs))
		}
		// The second one should be non-decl with Int type.
		var an Analyzer
		ip := an.ensureIP()
		var use *TokenEntry
		for i := range xs {
			if !xs[i].IsDecl {
				use = &xs[i]
				break
			}
		}
		if use == nil {
			t.Fatalf("missing non-decl use of x on assignment line")
		}
		if got := testFmtType(ip, idx.RootEnv, use.Payload); got != "Enum[2]" {
			t.Fatalf("use-site 'x' after assignment: want Enum[2], got %s", got)
		}
	})
}

//
// C. Negative: NO fallback append on span mismatch (drives removal of fallback)
//

func Test_Analysis_NoFallbackAppend(t *testing.T) {
	src := "let x = 1"
	env := mindscript.NewEnv(nil)
	idx := &FileIndex{
		Text:    src,
		Tokens:  TokenIndexFromLexer(src, env),
		RootEnv: env,
	}
	nBefore := len(idx.Tokens.Entries)

	// Fabricate a wrong span for 'x': shift start by +1.
	for _, e := range idx.Tokens.Entries {
		if e.Text == "x" {
			wrongStart := e.Start + 1
			// Call emitIdent with wrong span (should NOT append anything).
			idx.emitIdentAtStart(wrongStart, "x", true, newSymbolValIn(env, mindscript.S{"id", "Int"}, ""))
			break
		}
	}
	nAfter := len(idx.Tokens.Entries)
	if nAfter != nBefore {
		t.Fatalf("emitIdent must not append on span mismatch (before=%d, after=%d)", nBefore, nAfter)
	}
}

//
// D. Types propagate through expressions visible via tokens
//

func Test_Analysis_Types_Propagation(t *testing.T) {
	src := "let xs = [1,2]\nlet y = xs[0]\n"
	idx := testAnalyze(src)
	var an Analyzer
	ip := an.ensureIP()

	var yTok *TokenEntry
	var xsUse *TokenEntry
	for i := range idx.Tokens.Entries {
		e := &idx.Tokens.Entries[i]
		if e.Text == "y" && e.IsDecl {
			yTok = e
		}
		if e.Text == "xs" && !e.IsDecl {
			xsUse = e
		}
	}
	if yTok == nil {
		t.Fatalf("missing decl token y")
	}
	if xsUse == nil {
		t.Fatalf("missing use token xs on second line")
	}
	if got := testFmtType(ip, idx.RootEnv, yTok.Payload); got != "Enum[1, 2]" {
		t.Fatalf("y type: want Enum[1, 2], got %s", got)
	}
	// xs should at least be array-of-Int; allow either "[Int]" or structural form.
	if got := testFmtType(ip, idx.RootEnv, xsUse.Payload); got != "[Int]" && got != "[Enum[1, 2]]" && !strings.Contains(got, "array") {
		t.Fatalf("xs use type: want [Int] or [Enum[1, 2]] (or structural array), got %s", got)
	}
}

// -----------------------------------------------------------------------------
// TokenIndex: boundaries, gaps, order, adjacency (ASCII-only)
// -----------------------------------------------------------------------------

func Test_Analysis_TokenIndex_Find_Boundaries_And_Gaps(t *testing.T) {
	t.Run("HalfOpen_BeginEnd_And_Outside", func(t *testing.T) {
		src := "let x=1"
		env := mindscript.NewEnv(nil)
		ti := TokenIndexFromLexer(src, env)

		startX := testByteOffset(src, "x", 1)
		if startX < 0 {
			t.Fatal("could not locate 'x'")
		}
		endX := startX + len("x")

		// Inside 'x' → hit 'x'
		if tok := testFindTokenAtOffset(ti, startX); tok == nil || tok.Text != "x" {
			t.Fatalf("Find(startX) = %v, want 'x'", tok)
		}
		// Exactly at end of 'x' (half-open) → must NOT return 'x'
		if tok := testFindTokenAtOffset(ti, endX); tok != nil && tok.Text == "x" {
			t.Fatalf("Find(endX) returned %v; must not be 'x'", tok)
		}
		// Out-of-range: -1 and len(src) → nil
		if tok := testFindTokenAtOffset(ti, -1); tok != nil {
			t.Fatalf("Find(-1) should be nil, got %v", tok)
		}
		if tok := testFindTokenAtOffset(ti, len(src)); tok != nil {
			t.Fatalf("Find(len(src)) should be nil, got %v", tok)
		}
	})

	t.Run("Multiple_Gaps_Spaces_Tabs_Newlines", func(t *testing.T) {
		src := "let x  =\t[ 1 , 2 ]\n\n"
		env := mindscript.NewEnv(nil)
		ti := TokenIndexFromLexer(src, env)

		// Probe several gap offsets (2 spaces, a tab, spaces in brackets, blank line)
		offsets := []int{
			testByteOffset(src, "x", 1) + 1,   // the first of two spaces after x
			testByteOffset(src, "=\t", 1) + 1, // the tab position
			testByteOffset(src, "[ ", 1) + 1,  // space after '['
			testByteOffset(src, "1 ,", 1) + 1, // space before comma
			len(src) - 1,                      // last '\n' blank line
		}
		for i, off := range offsets {
			if off < 0 || off >= len(src) {
				t.Fatalf("bad probe offset %d: %d", i, off)
			}
			if tok := testFindTokenAtOffset(ti, off); tok != nil {
				t.Fatalf("gap %d: expected nil, got %v", i, tok)
			}
		}
	})

	t.Run("CRLF_Newlines", func(t *testing.T) {
		src := "let x = 1\r\nlet y = x\r\n"
		env := mindscript.NewEnv(nil)
		ti := TokenIndexFromLexer(src, env)

		// Find the *second* 'x' by counting 'x' occurrences, not "x\r\n".
		offUseX := testByteOffset(src, "x", 2)
		if offUseX < 0 {
			t.Fatal("failed to locate second 'x'")
		}
		tok := testFindTokenAtOffset(ti, offUseX)
		if tok == nil || tok.Text != "x" || tok.IsDecl {
			t.Fatalf("Find(second x) = %v; want non-decl 'x'", tok)
		}

		// Sanity: the CR and LF positions themselves are gaps
		cr := testByteOffset(src, "\r", 1)
		lf := cr + 1
		if tok := testFindTokenAtOffset(ti, cr); tok != nil {
			t.Fatalf("expected nil on CR, got %v", tok)
		}
		if tok := testFindTokenAtOffset(ti, lf); tok != nil {
			t.Fatalf("expected nil on LF, got %v", tok)
		}
	})

	t.Run("Adjacency_NoSpace", func(t *testing.T) {
		src := "let x=1\nx+1\n"
		env := mindscript.NewEnv(nil)
		ti := TokenIndexFromLexer(src, env)

		offXDecl := testByteOffset(src, "x=1", 1) // the 'x' before '='
		offXUse := testByteOffset(src, "x+1", 1)  // the 'x' before '+'

		if tok := testFindTokenAtOffset(ti, offXDecl); tok == nil || tok.Text != "x" {
			t.Fatalf("decl 'x' not found at adjacency: %v", tok)
		}
		if tok := testFindTokenAtOffset(ti, offXUse); tok == nil || tok.Text != "x" {
			t.Fatalf("use 'x' not found at adjacency: %v", tok)
		}
	})

	t.Run("Order_NoOverlap", func(t *testing.T) {
		src := "let a=1+2"
		env := mindscript.NewEnv(nil)
		ti := TokenIndexFromLexer(src, env)

		for i := 1; i < len(ti.Entries); i++ {
			prev, cur := ti.Entries[i-1], ti.Entries[i]
			if !(prev.Start < cur.Start) {
				t.Fatalf("tokens not strictly increasing by Start at %d: %+v -> %+v", i, prev, cur)
			}
			if prev.End > cur.Start {
				t.Fatalf("overlapping tokens at %d: %+v overlaps %+v", i, prev, cur)
			}
		}
	})
}

// -----------------------------------------------------------------------------
// emitIdent: disambiguation (duplicate names, same-prefix), idempotence
// -----------------------------------------------------------------------------

func Test_Analysis_emitIdent_Disambiguation_And_Idempotence(t *testing.T) {
	t.Run("Duplicate_Identifiers_In_One_Line", func(t *testing.T) {
		src := "let x = 1; x = x + 2"
		idx := testAnalyze(src)

		// Collect all 'x' tokens and classify decl/use.
		var decl *TokenEntry
		var uses []*TokenEntry
		for i := range idx.Tokens.Entries {
			e := &idx.Tokens.Entries[i]
			if e.Text == "x" {
				if e.IsDecl {
					decl = e
				} else {
					uses = append(uses, e)
				}
			}
		}
		if decl == nil || len(uses) < 2 {
			t.Fatalf("want decl x and at least 2 uses; got decl=%v uses=%d", decl != nil, len(uses))
		}
		// All uses must link to the same Sym as decl.
		for _, u := range uses {
			if u.Payload.Tag != mindscript.VTSymbol || decl.Payload.Data != u.Payload.Data {
				t.Fatalf("use %v does not link to decl sym %v", u.Payload, decl.Payload)
			}
		}
	})

	t.Run("SamePrefix_xx_vs_x_Disambiguation_By_Span", func(t *testing.T) {
		src := "let xx = 1\nlet x = xx\n"
		idx := testAnalyze(src)

		// Find the 'xx' use on second line by exact offset.
		offXX2 := testByteOffset(src, "xx\n", 1)
		if offXX2 < 0 {
			t.Fatal("failed to locate 'xx' on second line")
		}
		tok := testFindTokenAtOffset(idx.Tokens, offXX2)
		if tok == nil || tok.Text != "xx" || tok.IsDecl {
			t.Fatalf("expected use token 'xx' at off=%d, got %v", offXX2, tok)
		}
		// Ensure it is not confused with 'x'.
		if tok.Text == "x" {
			t.Fatalf("span confusion: enriched 'x' instead of 'xx'")
		}
	})

	t.Run("Idempotent_Enrichment_Same_Span_No_Dup_No_Clobber", func(t *testing.T) {
		src := "let a = 1"
		idx := testAnalyze(src)

		// Locate decl token for 'a'
		var aTok *TokenEntry
		for i := range idx.Tokens.Entries {
			if idx.Tokens.Entries[i].Text == "a" && idx.Tokens.Entries[i].IsDecl {
				aTok = &idx.Tokens.Entries[i]
				break
			}
		}
		if aTok == nil {
			t.Fatalf("missing decl token for 'a'")
		}
		beforeN := len(idx.Tokens.Entries)
		beforeKind, beforeText := aTok.Kind, aTok.Text
		beforePayload := aTok.Payload

		// Re-emit the same ident span & payload.
		idx.emitIdentAtStart(aTok.Start, "a", true, beforePayload)

		afterN := len(idx.Tokens.Entries)
		if afterN != beforeN {
			t.Fatalf("idempotent emitIdent must not change token count; before=%d after=%d", beforeN, afterN)
		}
		// Ensure kind/text not clobbered.
		if aTok.Kind != beforeKind || aTok.Text != beforeText {
			t.Fatalf("emitIdent clobbered Kind/Text: now (%v,%q), was (%v,%q)", aTok.Kind, aTok.Text, beforeKind, beforeText)
		}
		// Ensure payload pointer remains the same
		if aTok.Payload.Data != beforePayload.Data || aTok.Payload.Tag != beforePayload.Tag {
			t.Fatalf("emitIdent altered payload")
		}
	})
}

// -----------------------------------------------------------------------------
// Annotations (comments): spans exist and analysis still aligns tokens
// -----------------------------------------------------------------------------

// Helper to find the NodePath of the first node whose tag == wantTag.
func findNodePathByTag(n mindscript.S, wantTag string) (mindscript.NodePath, bool) {
	var dfs func(path mindscript.NodePath, s mindscript.S) (mindscript.NodePath, bool)
	dfs = func(path mindscript.NodePath, s mindscript.S) (mindscript.NodePath, bool) {
		if len(s) == 0 {
			return nil, false
		}
		if tag, _ := s[0].(string); tag == wantTag {
			return path, true
		}
		for i := 1; i < len(s); i++ {
			if child, ok := s[i].(mindscript.S); ok {
				if p, ok := dfs(append(path, i-1), child); ok {
					return p, true
				}
			}
		}
		return nil, false
	}
	return dfs(nil, n)
}

func Test_Analysis_Annotations_Spans_And_Token_Alignment(t *testing.T) {
	t.Run("Annot_Node_Has_Span_And_Sets_Doc", func(t *testing.T) {
		src := `
# first
# second
let x = 1
`
		idx := runIndex(t, "mem://annot-spans.ms", src)

		// 1) The AST should contain an "annot" node with a span.
		path, ok := findNodePathByTag(idx.AST, "annot")
		if !ok {
			t.Fatalf("expected an 'annot' node in AST")
		}
		if sp, ok := idx.Spans.Get(path); !ok || !(sp.EndByte > sp.StartByte) {
			t.Fatalf("annot node missing or empty span")
		}

		// 2) The doc should have been attached to 'x' binding (existing tests assert similar).
		val, sym := findBinding(idx, "x")
		if sym == nil {
			t.Fatalf("binding x not found")
		}
		if val.Annot != "first\nsecond" {
			t.Fatalf("want doc 'first\\nsecond', got %q", val.Annot)
		}

		// 3) Token alignment around annotations: 'x' span still matches.
		startX := testByteOffset(src, "x", 1)
		if startX < 0 {
			t.Fatal("could not locate 'x'")
		}
		tok := testFindTokenAtOffset(idx.Tokens, startX)
		if tok == nil || tok.Text != "x" || !tok.IsDecl {
			t.Fatalf("expected decl token 'x' at its byte start; got %v", tok)
		}
	})
}

// -----------------------------------------------------------------------------
// Exhaustive byte sweep on a tiny string to catch fenceposts everywhere
// -----------------------------------------------------------------------------

func Test_Analysis_TokenIndex_Find_Exhaustive_Tiny(t *testing.T) {
	src := "let x = 1 + 2"
	env := mindscript.NewEnv(nil)
	ti := TokenIndexFromLexer(src, env)

	// Build expected intervals from the index itself.
	type interval struct{ s, e int }
	var intervals []interval
	for _, e := range ti.Entries {
		intervals = append(intervals, interval{e.Start, e.End})
	}

	isInside := func(i int) bool {
		for _, iv := range intervals {
			if iv.s <= i && i < iv.e {
				return true
			}
		}
		return false
	}

	for i := 0; i < len(src); i++ {
		gotTok := testFindTokenAtOffset(ti, i)
		wantInside := isInside(i)
		if wantInside && gotTok == nil {
			t.Fatalf("offset %d: expected a token, got nil", i)
		}
		if !wantInside && gotTok != nil {
			t.Fatalf("offset %d: expected nil, got token %q", i, gotTok.Text)
		}
	}
}

// -----------------------------------------------------------------------------
// Properties & module fields: identifier token enrichment (terminal nodes only)
// -----------------------------------------------------------------------------

func Test_Analysis_Property_Tokens(t *testing.T) {
	t.Run("Dot_Identifier_Field_Token_Enriched", func(t *testing.T) {
		src := `
let p = { name: 1, age: 2 }
let x = p.name
`
		idx := testAnalyze(src)

		// Locate the "p.name" sequence and position the cursor on the 'n' of name.
		startPN, _ := test_analysis_posOf(src, "p.name", 1)
		if startPN < 0 {
			t.Fatalf("failed to locate 'p.name'")
		}
		startName := startPN + len("p.")
		tok := testFindTokenAtOffset(idx.Tokens, startName)
		if tok == nil {
			t.Fatalf("no token at field-use offset %d", startName)
		}
		if tok.Text != "name" || tok.IsDecl {
			t.Fatalf("expected non-decl field-use token 'name', got %+v", tok)
		}
		// Ensure we didn’t accidentally grab the map-literal key:
		// the literal key appears earlier in the file, so its Start must be < startName.
		if tok.Start < startName {
			t.Fatalf("picked literal key 'name' instead of field-use; tok.Start=%d, want >= %d", tok.Start, startName)
		}

		var a Analyzer
		ip := a.ensureIP()
		got := testFmtType(ip, idx.RootEnv, tok.Payload)
		if got != "Enum[1]" {
			t.Fatalf("field token payload: want Enum[1], got %s", got)
		}
	})

	t.Run("Module_Field_Token_Enriched", func(t *testing.T) {
		src := `
module "M" do
  let x = 1
end

let y = M.x
`
		idx := testAnalyze(src)

		// Locate the "M.x" sequence and position on the 'x'.
		startMx, _ := test_analysis_posOf(src, "M.x", 1)
		if startMx < 0 {
			t.Fatalf("failed to locate 'M.x'")
		}
		startX := startMx + len("M.")
		tok := testFindTokenAtOffset(idx.Tokens, startX)
		if tok == nil {
			t.Fatalf("no token at module-field offset %d", startX)
		}
		if tok.Text != "x" || tok.IsDecl {
			t.Fatalf("expected non-decl module field-use token 'x', got %+v", tok)
		}
	})
}

// -----------------------------------------------------------------------------
// Annotation wrappers around uses: still enrich the correct terminal leaf
// -----------------------------------------------------------------------------

func Test_Analysis_Annotated_Use_Token_Enriched(t *testing.T) {
	const src = `
let x = 1
let y =
  # doc wraps the next value (which is 'x')
  x
`
	idx := testAnalyze(src)

	// Find use token for 'x' (non-decl) — must exist and be enriched as the same Sym type.
	xToks := test_analysis_findIdentTokens(idx, "x")
	if len(xToks) < 2 {
		t.Fatalf("want decl + use tokens for x, got %d", len(xToks))
	}
	var decl, use *TokenEntry
	for i := range xToks {
		if xToks[i].IsDecl {
			decl = &xToks[i]
		} else {
			use = &xToks[i]
		}
	}
	if decl == nil || use == nil {
		t.Fatalf("need both decl and use for x")
	}
	// Use should carry a VTSymbol payload and (ideally) link to same symbol as decl.
	if use.Payload.Tag != mindscript.VTSymbol {
		t.Fatalf("use token 'x' must carry VTSymbol payload")
	}
	if decl.Payload.Tag == mindscript.VTSymbol && decl.Payload.Data != use.Payload.Data {
		t.Fatalf("use token 'x' should link to the same symbol as decl")
	}
	// And the binding y should be Int, proving type propagation through the annot wrapper.
	_, y := findBinding(idx, "y")
	if y == nil || mindscript.FormatType(y.Type) != "Enum[1]" {
		t.Fatalf("binding y: want Enum[1], got %v", y)
	}
}

// -----------------------------------------------------------------------------
// For-loop targets: tokens come only from terminal decl leaves
// -----------------------------------------------------------------------------

func Test_Analysis_For_Target_Tokens(t *testing.T) {
	t.Run("Id_Target_Decl_Token", func(t *testing.T) {
		const src = `
for i in [1, 2, 3] do
  i
end
`
		idx := testAnalyze(src)

		iToks := test_analysis_findIdentTokens(idx, "i")
		if len(iToks) == 0 {
			t.Fatalf("missing 'i' tokens")
		}
		var decl *TokenEntry
		for i := range iToks {
			if iToks[i].IsDecl {
				decl = &iToks[i]
				break
			}
		}
		if decl == nil {
			t.Fatalf("for-target 'i' should be a declaration token")
		}
		// Payload should be a VTSymbol (loop-local); we don't overconstrain exact element type here.
		if decl.Payload.Tag != mindscript.VTSymbol {
			t.Fatalf("for-target decl token must carry VTSymbol payload")
		}
	})

	t.Run("Destructuring_Array_Target_Decl_Tokens", func(t *testing.T) {
		const src = `
for [k, v] in { a: 1, b: 2 } do
  k
  v
end
`
		idx := testAnalyze(src)

		kToks := test_analysis_findIdentTokens(idx, "k")
		vToks := test_analysis_findIdentTokens(idx, "v")
		if len(kToks) == 0 || len(vToks) == 0 {
			t.Fatalf("missing k/v tokens")
		}
		var kDecl, vDecl *TokenEntry
		for i := range kToks {
			if kToks[i].IsDecl {
				kDecl = &kToks[i]
				break
			}
		}
		for i := range vToks {
			if vToks[i].IsDecl {
				vDecl = &vToks[i]
				break
			}
		}
		if kDecl == nil || vDecl == nil {
			t.Fatalf("for-target destructured names must be decl tokens")
		}
		if kDecl.Payload.Tag != mindscript.VTSymbol || vDecl.Payload.Tag != mindscript.VTSymbol {
			t.Fatalf("k/v decl tokens must carry VTSymbol payloads")
		}
	})
}
