// lexer_test.go
package mindscript

import (
	"reflect"
	"strings"
	"testing"
)

func toks(t *testing.T, src string) []Token {
	t.Helper()
	l := NewLexer(src)
	ts, err := l.Scan()
	if err != nil {
		t.Fatalf("Scan error: %v", err)
	}
	return ts
}

func typesWithoutEOF(tokens []Token) []TokenType {
	if len(tokens) == 0 {
		return nil
	}
	end := len(tokens)
	if tokens[end-1].Type == EOF {
		end--
	}
	out := make([]TokenType, 0, end)
	for i := 0; i < end; i++ {
		out = append(out, tokens[i].Type)
	}
	return out
}

func wantTypes(t *testing.T, src string, want []TokenType) []Token {
	t.Helper()
	got := toks(t, src)
	gotTypes := typesWithoutEOF(got)
	if !reflect.DeepEqual(gotTypes, want) {
		t.Fatalf("\nsource:\n%s\nwant types:\n%v\ngot types:\n%v\n", src, want, gotTypes)
	}
	return got
}

func Test_Lexer_Examples_HelloWorld_Formal(t *testing.T) {
	src := `
# Say "Hello, world!"
let greet1 = fun() -> Str do
    return("Hello, world!")
end
`
	want := []TokenType{
		ANNOTATION,
		LET, ID, ASSIGN, FUNCTION, CLROUND, RROUND,
		ARROW, TYPE, DO,
		RETURN, CLROUND, STRING, RROUND,
		END,
	}
	wantTypes(t, src, want)
}

func Test_Lexer_Examples_HelloWorld_Oracle(t *testing.T) {
	src := `
# Say "Hello, world!"
let greet2 = oracle() -> Str
`
	want := []TokenType{
		ANNOTATION,
		LET, ID, ASSIGN, ORACLE, CLROUND, RROUND, ARROW, TYPE,
	}
	wantTypes(t, src, want)
}

func Test_Lexer_Examples_Sentiment_Enum(t *testing.T) {
	src := `let Sentiment = type Enum ["positive", "negative"]`
	got := wantTypes(t, src, []TokenType{
		LET, ID, ASSIGN, TYPECONS, ENUM, LSQUARE, STRING, COMMA, STRING, RSQUARE,
	})
	if got[6].Literal.(string) != "positive" || got[8].Literal.(string) != "negative" {
		t.Fatalf("enum strings not parsed as expected: %v, %v", got[6].Literal, got[8].Literal)
	}
}

func Test_Lexer_Examples_ExtractPrice_Snippet(t *testing.T) {
	src := `
# Given the HTML of a web page, extract the price of a given cryptocurrency.
let extractPrice = oracle(html: Str, currency: Str) -> Num
let page = www("https://coinmarketcap.com")
let ethPrice = extractPrice(page, "ETH")
`
	ts := toks(t, src)
	if len(ts) == 0 {
		t.Fatalf("no tokens")
	}
	var sawWWW, sawURL bool
	for _, tok := range ts {
		if tok.Type == ID && tok.Literal == "www" {
			sawWWW = true
		}
		if tok.Type == STRING && strings.HasPrefix(tok.Literal.(string), "https://coinmarketcap.com") {
			sawURL = true
		}
	}
	if !sawWWW || !sawURL {
		t.Fatalf("did not see expected www(...) call and URL; sawWWW=%v sawURL=%v", sawWWW, sawURL)
	}
}

func Test_Lexer_Examples_Fibonacci_Function(t *testing.T) {
	src := `
# Compute the n-th Fibonacci number.
let fib = fun(n: Int) do
  if n == 0 then
    return(0)
  elif n == 1 then
    return(1)
  end
  return( fib(n-1) + fib(n-2) )
end
`
	ts := toks(t, src)
	var seen = map[TokenType]bool{}
	wantSome := []TokenType{LET, ID, ASSIGN, FUNCTION, IF, EQ, THEN, RETURN, ELIF, RETURN, END, RETURN, PLUS, END}
	for _, w := range wantSome {
		seen[w] = false
	}
	for _, tok := range ts {
		if _, ok := seen[tok.Type]; ok {
			seen[tok.Type] = true
		}
	}
	for k, v := range seen {
		if !v {
			t.Fatalf("expected to see token type %v in Fibonacci example", k)
		}
	}
}

func Test_Lexer_Annotation_SingleLine_And_Consecutive(t *testing.T) {
	src := `
# first line
# second line
let x = 1
`
	ts := toks(t, src)

	// Expect a single merged ANNOTATION, then: let x = 1
	if len(ts) < 5 {
		t.Fatalf("unexpected token count: %d", len(ts))
	}
	if ts[0].Type != ANNOTATION {
		t.Fatalf("first token should be ANNOTATION, got %v", ts[0].Type)
	}
	text := strings.TrimSpace(ts[0].Literal.(string))
	if !strings.Contains(text, "first line") {
		t.Fatalf("missing 'first line' in annotation: %q", text)
	}
	if !strings.Contains(text, "second line") {
		t.Fatalf("missing 'second line' in annotation: %q", text)
	}

	exp := []TokenType{ANNOTATION, LET, ID, ASSIGN, INTEGER}
	gotTypes := typesWithoutEOF(ts)
	if !reflect.DeepEqual(gotTypes[:len(exp)], exp) {
		t.Fatalf("unexpected token prefix after annotation:\n%v", gotTypes[:len(exp)])
	}
}

func Test_Lexer_Strings_JSONEscapes_And_Unicode(t *testing.T) {
	src := `"a\/b\n\u0041"  '# ok too'`
	got := wantTypes(t, src, []TokenType{STRING, STRING})
	if got[0].Literal.(string) != "a/b\nA" {
		t.Fatalf("bad first string literal: %q", got[0].Literal)
	}
	if got[1].Literal.(string) != "# ok too" {
		t.Fatalf("bad second string literal: %q", got[1].Literal)
	}
}

func Test_Lexer_Period_PropertyAccess_With_QuotedKey(t *testing.T) {
	src := `obj."weird-key"`
	got := wantTypes(t, src, []TokenType{ID, PERIOD, ID})
	if got[2].Literal.(string) != "weird-key" {
		t.Fatalf("quoted key should become ID: %v", got[2])
	}
}

func Test_Lexer_Numbers_IntVsFloat_And_Exponents(t *testing.T) {
	src := `0 42 .5 3.14e-2 1.0`
	// Only check types here (no need to bind result to a var)
	wantTypes(t, src, []TokenType{
		INTEGER, INTEGER, NUMBER, NUMBER, NUMBER,
	})
	// value checks
	int0 := toks(t, "0")[0]
	if int0.Literal.(int64) != 0 {
		t.Fatalf("0 should be INTEGER literal 0, got %v", int0.Literal)
	}
	nums := toks(t, `.5 3.14e-2 1.0`)
	if nums[0].Literal.(float64) != 0.5 {
		t.Fatalf(".5 should be 0.5, got %v", nums[0].Literal)
	}
	if got := nums[1].Literal.(float64); got < 0.031399 || got > 0.031401 {
		t.Fatalf("3.14e-2 ≈ 0.0314, got %v", nums[1].Literal)
	}
	if nums[2].Literal.(float64) != 1.0 {
		t.Fatalf("1.0 should be 1.0, got %v", nums[2].Literal)
	}
}

func Test_Lexer_Brackets_WhitespaceSensitive(t *testing.T) {
	src := ` (x) a(x) `
	// Just assert the type sequence; no need to capture the return value
	wantTypes(t, src, []TokenType{
		LROUND, ID, RROUND, ID, CLROUND, ID, RROUND,
	})
}

func Test_Lexer_Identifiers_And_Keywords(t *testing.T) {
	src := `true false null Type Int Num Bool Any and or not`
	got := wantTypes(t, src, []TokenType{
		BOOLEAN, BOOLEAN, NULL, TYPE, TYPE, TYPE, TYPE, TYPE, AND, OR, NOT,
	})
	if got[0].Literal.(bool) != true || got[1].Literal.(bool) != false {
		t.Fatalf("boolean literals wrong: %v %v", got[0].Literal, got[1].Literal)
	}
	if got[2].Literal != nil {
		t.Fatalf("null literal should be nil, got %v", got[2].Literal)
	}
}

func Test_Lexer_ObjectLiteral_And_Set_Get(t *testing.T) {
	src := `let person = {name: "John", age: 25}
person.name = "Sarah"`
	ts := toks(t, src)
	var sawDotChain bool
	for i := 0; i+4 < len(ts); i++ {
		if ts[i].Type == ID && ts[i].Literal == "person" &&
			ts[i+1].Type == PERIOD &&
			ts[i+2].Type == ID && ts[i+2].Literal == "name" &&
			ts[i+3].Type == ASSIGN &&
			ts[i+4].Type == STRING && ts[i+4].Literal == "Sarah" {
			sawDotChain = true
			break
		}
	}
	if !sawDotChain {
		t.Fatalf("did not find `person.name = \"Sarah\"` sequence in tokens")
	}
}

func Test_Lexer_Object_Annotations_TokenFlow(t *testing.T) {
	src := `{
# the name
name: "Raffaella",
# the age
age: 29,
# status
available: "yes"
}`

	ts := toks(t, src)
	gotTypes := typesWithoutEOF(ts)

	// Expected token *types* sequence with line-leading (PRE) annotations
	want := []TokenType{
		LCURLY,
		ANNOTATION, ID, COLON, STRING, COMMA,
		ANNOTATION, ID, COLON, INTEGER, COMMA,
		ANNOTATION, ID, COLON, STRING,
		RCURLY,
	}
	if !reflect.DeepEqual(gotTypes, want) {
		t.Fatalf("unexpected token sequence:\nwant %v\ngot  %v", want, gotTypes)
	}

	// Collect annotation payloads in the order they appear.
	var anns []string
	for _, tok := range ts {
		if tok.Type == ANNOTATION {
			anns = append(anns, strings.TrimSpace(tok.Literal.(string)))
		}
	}
	if len(anns) != 3 {
		t.Fatalf("expected 3 annotations, got %d: %v", len(anns), anns)
	}
	if anns[0] != "the name" || anns[1] != "the age" || anns[2] != "status" {
		t.Fatalf("unexpected annotation texts: %v", anns)
	}
}

// --- NEW: interactive-mode lexer behavior ----------------------------------

func Test_Lexer_Interactive_Unterminated_String_IsIncomplete(t *testing.T) {
	l := NewLexerInteractive(`"hello`)
	_, err := l.Scan()
	if err == nil || !IsIncomplete(err) {
		t.Fatalf("expected IncompleteError for unterminated string, got %v", err)
	}
}

func Test_Lexer_BlockAnnotation_LeavesFinalNewline(t *testing.T) {
	src := "" +
		"# first line\n" +
		"# second line\n" + // <- this newline must remain unconsumed by scanAnnotation
		"let x = 1\n"

	got := wantTypes(t, src, []TokenType{
		ANNOTATION,
		LET, ID, ASSIGN, INTEGER,
	})

	ann := got[0]
	if ann.Type != ANNOTATION {
		t.Fatalf("first token must be ANNOTATION, got %v", ann.Type)
	}

	// 1) The ANNOTATION's text (Literal) should have joined lines and trimmed trailing '\n'.
	wantText := "first line\nsecond line"
	if s, ok := ann.Literal.(string); !ok || s != wantText {
		t.Fatalf("annotation text mismatch\nwant: %q\ngot:  %q (ok=%v)", wantText, ann.Literal, ok)
	}

	// 2) The lexer must NOT consume the final '\n' of the block annotation.
	//    That means the token's EndByte should point at a '\n' in the original source.
	if ann.EndByte >= len(src) {
		t.Fatalf("annotation EndByte out of range: %d >= %d", ann.EndByte, len(src))
	}
	if src[ann.EndByte] != '\n' {
		t.Fatalf("expected src[EndByte] to be '\\n' after annotation; got %q at pos %d", src[ann.EndByte], ann.EndByte)
	}

	// 3) Sanity: The next token should start on the following line and be 'let'.
	letTok := got[1]
	if letTok.Type != LET {
		t.Fatalf("second token must be LET, got %v", letTok.Type)
	}
	// With two annotation lines, 'let' should start on line 3 (1-based).
	if letTok.Line != 3 {
		t.Fatalf("expected LET to start on line 3; got line %d", letTok.Line)
	}
}

func Test_Lexer_NOOP_MultiBlankLines(t *testing.T) {
	src := "let x=1\n   \n\t \nlet y=2"
	got := wantTypes(t, src, []TokenType{
		LET, ID, ASSIGN, INTEGER,
		NOOP,
		LET, ID, ASSIGN, INTEGER,
	})

	// Verify the NOOP lexeme exactly matches the whitespace run.
	var noop *Token
	for i := range got {
		if got[i].Type == NOOP {
			noop = &got[i]
			break
		}
	}
	if noop == nil {
		t.Fatalf("expected a NOOP token, found none")
	}
	if noop.Lexeme != "\n   \n\t \n" {
		t.Fatalf("NOOP lexeme mismatch:\nwant: %q\ngot:  %q", "\n   \n\t \n", noop.Lexeme)
	}
}

func Test_Lexer_NOOP_SingleNewlineIgnored(t *testing.T) {
	src := "let x=1\nlet y=2"
	wantTypes(t, src, []TokenType{
		LET, ID, ASSIGN, INTEGER,
		LET, ID, ASSIGN, INTEGER,
	})
}

func Test_Lexer_NOOP_AtStartAndEnd(t *testing.T) {
	src := "\n\nlet x=1\n\n"
	got := wantTypes(t, src, []TokenType{
		NOOP,
		LET, ID, ASSIGN, INTEGER,
		NOOP,
	})

	// Expect exactly two NOOP tokens, each being "\n\n".
	var noops []Token
	for _, tk := range got {
		if tk.Type == NOOP {
			noops = append(noops, tk)
		}
	}
	if len(noops) != 2 {
		t.Fatalf("expected 2 NOOP tokens, got %d", len(noops))
	}
	for i, tk := range noops {
		if tk.Lexeme != "\n\n" {
			t.Fatalf("NOOP #%d lexeme mismatch: want %q, got %q", i+1, "\n\n", tk.Lexeme)
		}
	}
}

func Test_Lexer_PropertyChain_IntegerAfterDot(t *testing.T) {
	src := `arr.0.name`
	got := wantTypes(t, src, []TokenType{
		ID, PERIOD, INTEGER, PERIOD, ID,
	})
	if got[2].Literal.(int64) != 0 {
		t.Fatalf("expected INTEGER literal 0, got %v", got[2].Literal)
	}
	if got[4].Lexeme != "name" || got[4].Type != ID {
		t.Fatalf("expected ID 'name' after second dot, got %v (%v)", got[4].Lexeme, got[4].Type)
	}
}

func Test_Lexer_DotStartsNumber_ContextSensitivity(t *testing.T) {
	// 1..2 should be rejected at lex time as a malformed number.
	{
		l := NewLexer(`1..2`)
		_, err := l.Scan()
		if err == nil {
			t.Fatalf("expected LexError for malformed number, got nil")
		}
		if e, ok := err.(*Error); !ok || e.Kind != DiagLex {
			t.Fatalf("expected lexical *Error for non-interactive unterminated string, got %T (%v)", err, err)
		}
		if !strings.Contains(err.Error(), "malformed number") &&
			!strings.Contains(err.Error(), "invalid") {
			t.Fatalf("error should mention malformed number, got: %v", err)
		}
	}

	// x .5  → ID, NUMBER  (whitespace lets '.' start a number)
	{
		src := `x .5`
		got := wantTypes(t, src, []TokenType{ID, NUMBER})
		if got[1].Literal.(float64) != 0.5 {
			t.Fatalf("expected NUMBER 0.5, got %v", got[1].Literal)
		}
	}

	// x.5   → ID, PERIOD, INTEGER (property numeric index)
	{
		src := `x.5`
		got := wantTypes(t, src, []TokenType{ID, PERIOD, INTEGER})
		if got[2].Literal.(int64) != 5 {
			t.Fatalf("expected INTEGER 5, got %v", got[2].Literal)
		}
	}
}

func Test_Lexer_WhitespaceSensitive_Delimiters(t *testing.T) {
	// Calls: only CLROUND is for calls (no space before '(')
	wantTypes(t, `f(x)`, []TokenType{ID, CLROUND, ID, RROUND})
	wantTypes(t, `f (x)`, []TokenType{ID, LROUND, ID, RROUND})

	// Indexing: only CLSQUARE is for indexing (no space before '[')
	wantTypes(t, `arr[i]`, []TokenType{ID, CLSQUARE, ID, RSQUARE})
	wantTypes(t, `arr [i]`, []TokenType{ID, LSQUARE, ID, RSQUARE})
}

func Test_Lexer_AfterDot_ForcesID_ForKeywordAndString(t *testing.T) {
	// Keyword after '.' must be forced to ID (not THEN)
	src := `obj.then`
	got := wantTypes(t, src, []TokenType{ID, PERIOD, ID})
	if got[2].Lexeme != "then" || got[2].Type != ID {
		t.Fatalf("expected ID 'then' after '.', got %v (%v)", got[2].Lexeme, got[2].Type)
	}

	// Quoted string after '.' also becomes ID with Literal holding the name
	src = `obj."then"`
	got = wantTypes(t, src, []TokenType{ID, PERIOD, ID})
	lit, ok := got[2].Literal.(string)
	if !ok || lit != "then" {
		t.Fatalf("expected ID literal 'then' after '.', got %T %v", got[2].Literal, got[2].Literal)
	}
}

func Test_Lexer_Annotations_BlockJoin(t *testing.T) {
	// Keep indentation after '#': only one optional space is stripped.
	src := "# a\n#   b\n   #c\nx"
	toks := toks(t, src)

	// Expect a single ANNOTATION token then ID 'x'
	if toks[0].Type != ANNOTATION {
		t.Fatalf("expected first token ANNOTATION, got %v", toks[0].Type)
	}
	text, ok := toks[0].Literal.(string)
	if !ok {
		t.Fatalf("annotation Literal not string: %T", toks[0].Literal)
	}
	// Indentation before '#' is ignored; spaces after '#' are preserved
	// except for at most one optional space.
	if text != "a\n  b\nc" {
		t.Fatalf("annotation text mismatch: want %q, got %q", "a\n  b\nc", text)
	}
	if toks[1].Type != ID || toks[1].Lexeme != "x" {
		t.Fatalf("expected ID 'x' after annotation, got %v %q", toks[1].Type, toks[1].Lexeme)
	}
}

func Test_Lexer_NOOP_BlankLines(t *testing.T) {
	// Single newline is skipped (no NOOP)
	wantTypes(t, "a\nb", []TokenType{ID, ID})

	// A run matching '\n' (hws* '\n')+ becomes a NOOP token
	got := wantTypes(t, "a\n\nb", []TokenType{ID, NOOP, ID})
	if got[1].Type != NOOP {
		t.Fatalf("expected NOOP as second token, got %v", got[1].Type)
	}

	// Newlines with spaces/tabs also count
	got = wantTypes(t, "a\n   \n\t\nb", []TokenType{ID, NOOP, ID})
	if got[1].Type != NOOP {
		t.Fatalf("expected NOOP with spaced blank lines, got %v", got[1].Type)
	}
}

func Test_Lexer_String_UnicodeSurrogatePair(t *testing.T) {
	src := `"\uD83D\uDE03"` // 😀 (U+1F603)
	toks := toks(t, src)
	if toks[0].Type != STRING {
		t.Fatalf("expected STRING, got %v", toks[0].Type)
	}
	val, ok := toks[0].Literal.(string)
	if !ok {
		t.Fatalf("STRING literal not string type: %T", toks[0].Literal)
	}
	if val != "😃" {
		t.Fatalf("expected decoded surrogate pair '😃', got %q", val)
	}
}

func Test_Lexer_Keywords_And_Types(t *testing.T) {
	// 'type' (constructor keyword) vs 'Type' (builtin type) vs 'Enum' (keyword)
	wantTypes(t, `type Type Enum`, []TokenType{TYPECONS, TYPE, ENUM})
}

func Test_Lexer_Boolean_And_Null_Literals(t *testing.T) {
	src := `true false null`
	toks := wantTypes(t, src, []TokenType{BOOLEAN, BOOLEAN, NULL})
	if toks[0].Literal != true || toks[1].Literal != false || toks[2].Literal != nil {
		t.Fatalf("boolean/null literals mismatch: %v, %v, %v", toks[0].Literal, toks[1].Literal, toks[2].Literal)
	}
}

// --- error-mode tests (direct Scan calls, not using wantTypes) ---

func Test_Lexer_UnterminatedString_NonInteractive_IsLexError(t *testing.T) {
	l := NewLexer(`"abc`)
	_, err := l.Scan()
	if err == nil {
		t.Fatalf("expected lexical error for malformed number, got nil")
	}
	if e, ok := err.(*Error); !ok || e.Kind != DiagLex {
		t.Fatalf("expected lexical *Error, got %T (%v)", err, err)
	}
}

func Test_Lexer_UnterminatedString_Interactive_IsIncomplete(t *testing.T) {
	l := NewLexerInteractive(`"abc`)
	_, err := l.Scan()
	if err == nil {
		t.Fatalf("expected error, got nil")
	}
	if !IsIncomplete(err) {
		t.Fatalf("expected IncompleteError in interactive mode, got %T (%v)", err, err)
	}
}

func Test_Lexer_Annotation_Alone_IsOk(t *testing.T) {
	// A line that is just '#' (after optional spaces) is allowed
	// and produces an empty annotation.
	l := NewLexer("#")
	toks, err := l.Scan()
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if len(toks) < 1 || toks[0].Type != ANNOTATION {
		t.Fatalf("expected leading ANNOTATION token, got %v", toks)
	}
	if s, _ := toks[0].Literal.(string); s != "" {
		t.Fatalf("expected empty annotation text, got %q", s)
	}
}

// Guard: PERIOD vs NUMBER when ambiguity exists around trailing dot
func Test_Lexer_Number_With_TrailingDot_OutsideProperty(t *testing.T) {
	// Outside property context, "1." is a NUMBER
	got := wantTypes(t, `1.`, []TokenType{NUMBER})
	if got[0].Literal.(float64) != 1.0 {
		t.Fatalf("expected NUMBER 1.0, got %v", got[0].Literal)
	}
}

func Test_Lexer_AfterDot_Integer_DoesNotFormFloat(t *testing.T) {
	// Specifically guard the regression we fixed: after '.', a digit should not
	// consume the following '.' into NUMBER("0.") when chaining.
	wantTypes(t, `arr.0.name`, []TokenType{ID, PERIOD, INTEGER, PERIOD, ID})
}

// Ensure we strip AT MOST ONE ASCII space after '#' and DO NOT strip a tab.
func Test_Lexer_Annotation_StripAtMostOneSpace_NotTab(t *testing.T) {
	src := "" +
		"#  two-space -> keep one\n" + // two spaces after '#': expect the first stripped, one remains
		"# \tnext starts with tab\n" + // one space + tab: strip the one space, preserve the '\t'
		"#\tleading tab preserved\n" + // tab immediately after '#': preserve
		"x\n"

	ts := toks(t, src)
	if len(ts) < 2 || ts[0].Type != ANNOTATION || ts[1].Type != ID {
		t.Fatalf("unexpected token sequence: %v", typesWithoutEOF(ts))
	}
	text, ok := ts[0].Literal.(string)
	if !ok {
		t.Fatalf("annotation Literal not string: %T", ts[0].Literal)
	}
	want := " two-space -> keep one\n\tnext starts with tab\n\tleading tab preserved"
	if text != want {
		t.Fatalf("annotation text mismatch:\nwant: %q\ngot:  %q", want, text)
	}
}

func Test_Lexer_Module_Keyword_Simple(t *testing.T) {
	src := `module name do end`
	want := []TokenType{
		MODULE, ID, DO, END,
	}
	wantTypes(t, src, want)
}

func Test_Lexer_Module_NameAsString(t *testing.T) {
	src := `module "MyLib" do end`
	ts := wantTypes(t, src, []TokenType{
		MODULE, STRING, DO, END,
	})
	// Sanity-check decoded literal of the STRING
	if lit, ok := ts[1].Literal.(string); !ok || lit != "MyLib" {
		t.Fatalf("expected STRING literal \"MyLib\", got %#v", ts[1].Literal)
	}
}

func Test_Lexer_Module_AfterDot_ForcesID(t *testing.T) {
	src := `obj.module`
	ts := wantTypes(t, src, []TokenType{
		ID, PERIOD, ID,
	})
	if ts[2].Type != ID || ts[2].Literal != "module" {
		t.Fatalf("property after '.' should be ID with Literal \"module\"; got %#v", ts[2])
	}
}

func Test_Lexer_Module_AfterDot_Quoted_ForcesID(t *testing.T) {
	src := `obj."module"`
	ts := wantTypes(t, src, []TokenType{
		ID, PERIOD, ID,
	})
	if ts[2].Type != ID {
		t.Fatalf("expected ID token for quoted property; got %v", ts[2].Type)
	}
	// Literal should be the decoded string without quotes; Lexeme retains quotes.
	if lit, ok := ts[2].Literal.(string); !ok || lit != "module" {
		t.Fatalf("expected forced ID literal \"module\"; got %#v", ts[2].Literal)
	}
	if ts[2].Lexeme != `"module"` {
		t.Fatalf("expected Lexeme to retain quotes, got %q", ts[2].Lexeme)
	}
}

func Test_Lexer_Module_NotPrefixOfLongerIdent(t *testing.T) {
	src := `modulex do end`
	want := []TokenType{
		ID, DO, END,
	}
	wantTypes(t, src, want)
}

func Test_Lexer_Module_CapitalizedIsNotKeyword(t *testing.T) {
	src := `Module do end`
	// 'Module' (capital M) is not a keyword; should lex as ID.
	want := []TokenType{
		ID, DO, END,
	}
	wantTypes(t, src, want)
}
