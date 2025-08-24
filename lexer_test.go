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

func Test_Lexer_Inline_Annotation_Paren(t *testing.T) {
	src := `#(inline note) 1 + 2`
	got := toks(t, src)
	if len(got) < 4 {
		t.Fatalf("too few tokens: %v", got)
	}
	if got[0].Type != ANNOTATION {
		t.Fatalf("first token should be ANNOTATION, got %v", got[0].Type)
	}
	if strings.TrimSpace(got[0].Literal.(string)) != "inline note" {
		t.Fatalf("wrong inline annotation text: %q", got[0].Literal)
	}
	// Next tokens should parse as 1 + 2: INTEGER PLUS INTEGER
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
#(the age) age: 29,
available: #(status) "yes"
}`

	ts := toks(t, src)
	gotTypes := typesWithoutEOF(ts)

	// Expected token *types* sequence (not positions of annotations)
	want := []TokenType{
		LCURLY,
		ANNOTATION, ID, COLON, STRING, COMMA,
		ANNOTATION, ID, COLON, INTEGER, COMMA,
		ID, COLON, ANNOTATION, STRING,
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
