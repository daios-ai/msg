// lexer.go: provides a whitespace-sensitive, UTF-8–aware lexer for the
// MindScript language. It converts a source string into a linear stream of
// tokens with accurate source positions and rich literal decoding.
//
// ──────────────────────────────────────────────────────────────────────────────
// HIGH-LEVEL OVERVIEW
//
// The lexer scans left→right and emits Token values, always ending with EOF.
// Each token carries:
//   - Type    — a TokenType enum
//   - Lexeme  — the exact source slice (verbatim characters from the input)
//   - Literal — the decoded value for literal tokens (e.g., bool/int/float/string)
//   - Line/Col— 1-based line and 0-based column of the token’s start
//
// The lexer decides between LROUND/CLROUND (and LSQUARE/CLSQUARE) solely by
// whether there is immediate whitespace before the delimiter:
//
//	'('  → LROUND  if there IS preceding whitespace
//	      CLROUND if there is NO preceding whitespace
//	'['  → LSQUARE if there IS preceding whitespace
//	      CLSQUARE if there is NO preceding whitespace
//
// Consequences (user-facing syntax):
//
//   - Calls and parameter lists require NO space before '('.
//     f(x)           // call: uses CLROUND
//     fun(x: T)      // function params: uses CLROUND
//     oracle(x: T)   // oracle params: uses CLROUND
//     With a space ("fun (x: T)"), '(' becomes LROUND and is NOT treated as a
//     parameter list; the parser will error.
//
//   - Indexing requires NO space before '['.
//     arr[i]         // indexing: uses CLSQUARE
//     With a space ("arr [i]"), '[' is LSQUARE and is NOT treated as indexing.
//
//   - Grouping "(expr)" is produced regardless of LROUND/CLROUND, but only
//     CLROUND participates in call/juxtaposition chains.
//
// This lets the parser distinguish grouping/indexing from juxtaposition/call-like
// forms without lookbehind in the parser.
//
// The '.' character is context-sensitive:
//   - If it begins a number (e.g., “.5” or “1.” or “1.2e3”), a NUMBER/INTEGER is
//     produced.
//   - Otherwise it is PERIOD (typically for property access).
//
// IDENTIFIERS & KEYWORDS
//
//	Identifiers match [A-Za-z_][A-Za-z0-9_]* and normally produce ID.
//	Reserved words produce dedicated TokenTypes (e.g., IF, LET, FUNCTION, etc.).
//	After a PERIOD, both identifiers *and* quoted strings are treated as
//	property names and forced to ID (even if the text is a keyword). This allows:
//	    obj."then"   // ID with Literal="then"
//	    obj.then     // ID with Literal="then"
//
// LITERALS
//   - STRING — single or double quotes, JSON-style escapes, including \uXXXX with
//     optional UTF-16 surrogate pair handling. Source must be valid UTF-8; non-ASCII
//     bytes in the lexeme are validated and decoded.
//   - INTEGER — 64-bit signed (ParseInt base 10), when no dot/exp part.
//   - NUMBER  — 64-bit float (ParseFloat), for forms with '.' and/or exponent.
//   - BOOLEAN — “true” or “false” (Literal: bool).
//   - NULL    — “null” (Literal: nil).
//
// COMMENTS vs ANNOTATIONS (hash syntax)
//   - Line comment:       "## ... <newline>"           → ignored
//   - Inline comment:     "##( ... )" (no nesting)     → ignored
//   - Inline annotation:  "#( ... )"                   → emits ANNOTATION with text
//   - Block annotations:  one or more consecutive lines where, after optional
//     indentation, the first non-space is '#'. The leading '#' (and one optional
//     following space/tab) are stripped; lines are joined with '\n', and a single
//     ANNOTATION token is emitted. A blank/non-# line ends the block.
//
// ERRORS
//   - Lexical errors (e.g., bad escape, invalid UTF-8, unexpected character) are
//     reported as *LexError* with precise location.
//   - Interactive/REPL mode: if enabled via NewLexerInteractive, unterminated
//     strings or unterminated "#(...)" / "##(...)" inline blocks produce
//     *IncompleteError* instead of LexError. Use IsIncomplete(err) to detect this
//     and prompt for more input.
//
// OUTPUT
//   - Scan returns the full token slice *including* the terminal EOF token.
//   - Each token’s Lexeme is the exact source text (e.g., a STRING’s lexeme
//     includes the quotes and escapes), while Literal carries the decoded value.
//
// ──────────────────────────────────────────────────────────────────────────────
//
// FILE ORGANIZATION
//  1. PUBLIC API  — exported enums/types/constructors/methods & their docs.
//  2. PRIVATE     — all non-exported helpers, internal tables, and scanning.
//
// The PUBLIC API docs below are intentionally exhaustive so the behavior is
// understandable without reading the implementation.
//
// ──────────────────────────────────────────────────────────────────────────────
package mindscript

import (
	"errors"
	"fmt"
	"strconv"
	"strings"
	"unicode/utf16"
	"unicode/utf8"
)

////////////////////////////////////////////////////////////////////////////////
//                               PUBLIC API
////////////////////////////////////////////////////////////////////////////////

// TokenType is the enumeration of all token kinds the lexer can emit.
// Most names are self-explanatory; groups are listed for clarity.
//
// Special:
//
//	EOF     — end-of-file sentinel (always the final token)
//	ILLEGAL — produced only for unrecoverable internal conditions (not used by Scan)
//
// Punctuation (some are whitespace-sensitive, see '(' and '[' notes below):
//
//	LROUND, CLROUND   — '(' with/without preceding whitespace respectively
//	RROUND            — ')'
//	LSQUARE, CLSQUARE — '[' with/without preceding whitespace respectively
//	RSQUARE           — ']'
//	LCURLY, RCURLY    — '{', '}'
//	COLON, COMMA, PERIOD, QUESTION — ':', ',', '.', '?'
//
// Operators:
//
//	PLUS, MINUS, MULT, DIV, MOD — '+', '-', '*', '/', '%'
//	ASSIGN                      — '='
//	EQ, NEQ                     — '==', '!='
//	LESS, LESS_EQ               — '<',  '<='
//	GREATER, GREATER_EQ         — '>',  '>='
//	BANG                        — '!' (used by the language in object/type literals)
//	ARROW                       — '->'
//
// Literals & identifiers:
//
//	ID, STRING, INTEGER, NUMBER, BOOLEAN, NULL
//
// Keywords (produced when the identifier text equals these words, except when
// forced to an ID after PERIOD/property access):
//
//	AND, OR, NOT,
//	LET, DO, END, RETURN, BREAK, CONTINUE,
//	IF, THEN, ELIF, ELSE,
//	FUNCTION, ORACLE,
//	FOR, IN, FROM, WHILE,
//	TYPECONS, TYPE, ENUM
//
// Annotation:
//
//	ANNOTATION — emitted for "#( … )" lines or multi-line blocks starting with '#'.
type TokenType int

const (
	// Special
	EOF TokenType = iota
	ILLEGAL

	// Punctuation
	LROUND   // "(" when preceded by whitespace
	CLROUND  // "(" when not preceded by whitespace (juxtaposition/call form)
	RROUND   // ")"
	LSQUARE  // "["
	CLSQUARE // "[" when not preceded by whitespace (index-close)
	RSQUARE  // "]"
	LCURLY   // "{"
	RCURLY   // "}"
	COLON    // ":"
	COMMA    // ","
	PERIOD   // "."
	QUESTION // "?"

	// Operators
	PLUS
	MINUS
	MULT
	DIV
	MOD
	ASSIGN // "="
	EQ     // "=="
	NEQ    // "!="
	LESS
	LESS_EQ
	GREATER
	GREATER_EQ
	BANG  // "!" (required-field marker in object/type literals)
	ARROW // "->"

	// Literals & identifiers
	ID
	STRING
	INTEGER
	NUMBER
	BOOLEAN
	NULL

	// Keywords
	AND
	OR
	NOT
	LET
	DO
	END
	RETURN
	BREAK
	CONTINUE
	IF
	THEN
	ELIF
	ELSE
	FUNCTION
	ORACLE
	FOR
	IN
	FROM
	WHILE
	TYPECONS
	TYPE
	ENUM

	// Annotation token (from lines starting with '#')
	ANNOTATION
)

// Token is a single lexical unit produced by the lexer.
//
// Fields:
//
//	Type    — the TokenType kind.
//	Lexeme  — the exact source slice comprising the token (verbatim, including
//	          quotes for strings, escape sequences, etc.).
//	Literal — a decoded value for literal tokens:
//	          • STRING  → Go string with escapes and surrogate pairs resolved
//	          • INTEGER → int64
//	          • NUMBER  → float64
//	          • BOOLEAN → bool
//	          • NULL    → nil
//	          Non-literal tokens usually carry nil or an unmodified string
//	          (keywords may store their text; property IDs store the property name).
//	Line    — 1-based line number at which this token starts.
//	Col     — 0-based column index at which this token starts.
type Token struct {
	Type    TokenType
	Lexeme  string
	Literal interface{}
	Line    int
	Col     int
}

// LexError reports a lexical error detected during scanning (e.g., invalid
// escape sequence, malformed number, unexpected character, invalid UTF-8).
// In non-interactive mode, unterminated strings or inline "#(...)" / "##(...)"
// also produce LexError.
//
// The error position (Line, Col) refers to the location where the lexer
// detected the problem (generally close to the token’s start).
type LexError struct {
	Line int
	Col  int
	Msg  string
}

func (e *LexError) Error() string {
	return fmt.Sprintf("LEXICAL ERROR at %d:%d: %s", e.Line, e.Col, e.Msg)
}

// IncompleteError signals that more input is required to complete a construct.
// It is returned *only* by a lexer created with NewLexerInteractive when the
// end of input is reached inside:
//   - a string literal,
//   - an inline annotation "#( ... )", or
//   - an inline comment   "##( ... )".
//
// Use IsIncomplete(err) to detect this case in REPLs and prompt the user for
// more lines instead of failing the parse.
type IncompleteError struct {
	Line int
	Col  int
	Msg  string
}

func (e *IncompleteError) Error() string {
	return fmt.Sprintf("INCOMPLETE at %d:%d: %s", e.Line, e.Col, e.Msg)
}

// IsIncomplete reports whether err is an *IncompleteError. Helpful in REPLs
// to distinguish “need more input” from real lexical errors.
func IsIncomplete(err error) bool {
	_, ok := err.(*IncompleteError)
	return ok
}

// Lexer is a streaming tokenizer for MindScript.
//
// Construction:
//   - NewLexer(src)            — normal mode. Unterminated constructs produce LexError.
//   - NewLexerInteractive(src) — REPL-friendly mode. Unterminated constructs
//     produce IncompleteError.
//
// Semantics:
//   - Scan() returns the full token slice including EOF. It never panics for
//     malformed input; instead it returns (nil, error).
//   - Whitespace is skipped, but influences '(' and '[' classification (see TokenType docs).
//   - PERIOD vs number: a '.' followed by digits begins a number IFF there is
//     either preceding whitespace *or* the previous token cannot be a left operand.
//     Otherwise '.' is PERIOD used for property access.
//   - After PERIOD, the *next* identifier or quoted string is forced to ID,
//     even if it matches a keyword.
//
// Positioning:
//   - Line numbers are 1-based; column indices are 0-based.
//   - A token’s position is captured at the start of scanning that token.
type Lexer struct {
	// public type with no exported fields; use constructors + Scan()
	src              string
	start            int // start index of current token
	cur              int // current index
	line             int // 1-based
	col              int // 0-based column within line
	tokens           []Token
	whitespaceBefore bool

	// precise token start position
	tokStartLine int
	tokStartCol  int

	// interactive mode: produce IncompleteError for unterminated constructs at EOF
	interactive bool
}

// NewLexer creates a new lexer for the given source in normal mode.
// Unterminated strings / "#(...)" / "##(...)" yield LexError.
func NewLexer(src string) *Lexer {
	return &Lexer{
		src:  src,
		line: 1,
		col:  0,
	}
}

// NewLexerInteractive creates a lexer in interactive mode.
// Unterminated strings or inline paren blocks return IncompleteError at EOF,
// allowing REPLs to request more input.
func NewLexerInteractive(src string) *Lexer {
	return &Lexer{
		src:         src,
		line:        1,
		col:         0,
		interactive: true,
	}
}

// Scan tokenizes the entire source string and returns the resulting slice of
// tokens. The returned slice always ends with EOF. On error, it returns (nil, err).
//
// Error behavior summary:
//   - Normal mode: returns *LexError on malformed input or unterminated constructs.
//   - Interactive mode: returns *IncompleteError at EOF if a construct is
//     unterminated; other issues still return *LexError.
//
// Note: Token.Lexeme is the exact source span; Token.Literal contains decoded
// values for STRING/INTEGER/NUMBER/BOOLEAN/NULL as described in Token docs.
func (l *Lexer) Scan() ([]Token, error) {
	for {
		tok, err := l.scanToken()
		if err != nil {
			return nil, err
		}
		if tok.Type == EOF {
			return l.tokens, nil
		}
	}
}

//// END_OF_PUBLIC

////////////////////////////////////////////////////////////////////////////////
//                            PRIVATE IMPLEMENTATION
////////////////////////////////////////////////////////////////////////////////

// ---------------- keywords map (private) ----------------

var keywords = map[string]TokenType{
	"null":     NULL,
	"false":    BOOLEAN,
	"true":     BOOLEAN,
	"and":      AND,
	"or":       OR,
	"not":      NOT,
	"let":      LET,
	"do":       DO,
	"end":      END,
	"return":   RETURN,
	"break":    BREAK,
	"continue": CONTINUE,
	"if":       IF,
	"then":     THEN,
	"elif":     ELIF,
	"else":     ELSE,
	"fun":      FUNCTION,
	"oracle":   ORACLE,
	"for":      FOR,
	"in":       IN,
	"from":     FROM,
	"while":    WHILE,
	"type":     TYPECONS,
	"Type":     TYPE,
	"Null":     TYPE,
	"Str":      TYPE,
	"Int":      TYPE,
	"Num":      TYPE,
	"Bool":     TYPE,
	"Any":      TYPE,
	"Enum":     ENUM,
}

// ---------------- core scanning helpers ----------------

func (l *Lexer) isAtEnd() bool { return l.cur >= len(l.src) }

func (l *Lexer) peek() (byte, bool) {
	if l.isAtEnd() {
		return 0, false
	}
	return l.src[l.cur], true
}

func (l *Lexer) peekN(n int) (byte, bool) {
	idx := l.cur + n
	if idx >= len(l.src) {
		return 0, false
	}
	return l.src[idx], true
}

func (l *Lexer) advance() (byte, bool) {
	if l.isAtEnd() {
		return 0, false
	}
	ch := l.src[l.cur]
	l.cur++
	if ch == '\n' {
		l.line++
		l.col = 0
	} else {
		l.col++
	}
	return ch, true
}

func (l *Lexer) rewindToStart() {
	// We rewind only within the bounds of the current token start; line/col are kept
	// for error arrows (OK since we set tokStartLine/Col before scanning).
	l.cur = l.start
}

func (l *Lexer) addToken(tt TokenType, lit interface{}) Token {
	lex := l.src[l.start:l.cur]
	tok := Token{
		Type:    tt,
		Lexeme:  lex,
		Literal: lit,
		Line:    l.tokStartLine,
		Col:     l.tokStartCol,
	}
	l.tokens = append(l.tokens, tok)
	l.start = l.cur
	l.whitespaceBefore = false
	return tok
}

func (l *Lexer) previousToken() *Token {
	if len(l.tokens) == 0 {
		return nil
	}
	return &l.tokens[len(l.tokens)-1]
}

func (l *Lexer) skipWhitespace() {
	l.whitespaceBefore = false
	for !l.isAtEnd() {
		ch, _ := l.peek()
		switch ch {
		case ' ', '\r', '\n', '\t':
			l.whitespaceBefore = true
			l.advance()
			l.start = l.cur
		default:
			return
		}
	}
}

// ---------------- small predicates ----------------

func canBeLeftOperand(t TokenType) bool {
	switch t {
	case ID, STRING, INTEGER, NUMBER, BOOLEAN, NULL,
		TYPE, ENUM,
		RROUND, RSQUARE, RCURLY,
		QUESTION:
		return true
	default:
		return false
	}
}

func isDigit(b byte) bool { return b >= '0' && b <= '9' }
func isHex(b byte) bool {
	return (b >= '0' && b <= '9') || (b >= 'a' && b <= 'f') || (b >= 'A' && b <= 'F')
}
func isAlpha(b byte) bool { return (b >= 'a' && b <= 'z') || (b >= 'A' && b <= 'Z') || b == '_' }
func isAlphaNum(b byte) bool {
	return (b >= 'a' && b <= 'z') ||
		(b >= 'A' && b <= 'Z') ||
		(b >= '0' && b <= '9') ||
		b == '_'
}

func (l *Lexer) afterDotIsProperty() bool {
	p := l.previousToken()
	return p != nil && p.Type == PERIOD
}

// ---------------- error builders ----------------

func (l *Lexer) err(msg string) error {
	return &LexError{Line: l.line, Col: l.col, Msg: msg}
}

func (l *Lexer) errIncomplete(msg string) error {
	return &IncompleteError{Line: l.line, Col: l.col, Msg: msg}
}

// ---------------- scanners ----------------

// scanString parses a JSON-style string literal (single or double quotes).
func (l *Lexer) scanString() (string, error) {
	del := l.src[l.start]
	if del != '"' && del != '\'' {
		return "", l.err("internal: scanString without quote")
	}
	// consume the delimiter
	l.advance()

	var out []rune
	for !l.isAtEnd() {
		ch, _ := l.advance()
		if ch == byte(del) {
			return string(out), nil
		}
		if ch == '\\' {
			if l.isAtEnd() {
				if l.interactive {
					return "", l.errIncomplete("unfinished escape sequence")
				}
				return "", l.err("unfinished escape sequence")
			}
			esc, _ := l.advance()
			switch esc {
			case '"':
				out = append(out, '"')
			case '\'':
				out = append(out, '\'')
			case '\\':
				out = append(out, '\\')
			case '/':
				out = append(out, '/')
			case 'b':
				out = append(out, '\b')
			case 'f':
				out = append(out, '\f')
			case 'n':
				out = append(out, '\n')
			case 'r':
				out = append(out, '\r')
			case 't':
				out = append(out, '\t')
			case 'u':
				// expect 4 hex digits
				var hex string
				for i := 0; i < 4; i++ {
					b, ok := l.peek()
					if !ok || !isHex(b) {
						if l.interactive {
							return "", l.errIncomplete("unicode escape was not terminated (expect 4 hex digits)")
						}
						return "", l.err("unicode escape was not terminated (expect 4 hex digits)")
					}
					hex += string(b)
					l.advance()
				}
				v, err := strconv.ParseInt(hex, 16, 32)
				if err != nil {
					return "", l.err("invalid unicode escape")
				}
				r := rune(v)

				// handle surrogate pair \uD800-\uDBFF followed by \uDC00-\uDFFF
				if 0xD800 <= r && r <= 0xDBFF {
					saveCur := l.cur
					saveLine, saveCol := l.line, l.col
					if b1, ok := l.peek(); ok && b1 == '\\' {
						l.advance()
						if b2, ok := l.peek(); ok && b2 == 'u' {
							l.advance()
							var hex2 string
							for i := 0; i < 4; i++ {
								b, ok := l.peek()
								if !ok || !isHex(b) {
									if l.interactive {
										return "", l.errIncomplete("unicode surrogate pair low was not terminated")
									}
									return "", l.err("unicode surrogate pair low was not terminated")
								}
								hex2 += string(b)
								l.advance()
							}
							v2, err := strconv.ParseInt(hex2, 16, 32)
							if err != nil {
								return "", l.err("invalid unicode surrogate pair low")
							}
							r2 := rune(v2)
							if 0xDC00 <= r2 && r2 <= 0xDFFF {
								cp := utf16.DecodeRune(r, r2)
								out = append(out, cp)
								continue
							}
						}
					}
					// not a valid pair; rewind and just emit r
					l.cur = saveCur
					l.line, l.col = saveLine, saveCol
				}
				out = append(out, r)
			default:
				return "", l.err(fmt.Sprintf("invalid escape sequence: \\%c", esc))
			}
			continue
		}
		// normal char; ensure it’s valid UTF-8 boundary (source may be UTF-8)
		if ch < utf8.RuneSelf {
			out = append(out, rune(ch))
			continue
		}
		// If we see a non-ASCII byte here, the source itself is UTF-8; back up one byte and decode rune.
		l.cur-- // step back 1 byte to let utf8.DecodeRuneInString read from correct start
		r, size := utf8.DecodeRuneInString(l.src[l.cur:])
		if r == utf8.RuneError && size == 1 {
			return "", l.err("invalid UTF-8 in source")
		}
		out = append(out, r)
		l.cur += size
		l.col += size - 1
	}
	if l.interactive {
		return "", l.errIncomplete("string was not terminated")
	}
	return "", l.err("string was not terminated")
}

// scanIdentifier parses [A-Za-z_][A-Za-z0-9_]*
func (l *Lexer) scanIdentifier() string {
	for {
		b, ok := l.peek()
		if !ok || !isAlphaNum(b) {
			break
		}
		l.advance()
	}
	return l.src[l.start:l.cur]
}

// scanNumber parses integer or float; supports .5, 1., 1.23e-4, etc.
func (l *Lexer) scanNumber() (tok TokenType, lit interface{}, err error) {
	sawDigits := false
	// optional leading digits
	for {
		b, ok := l.peek()
		if !ok || !isDigit(b) {
			break
		}
		l.advance()
		sawDigits = true
	}

	// decimal point with optional digits
	sawDot := false
	if b, ok := l.peek(); ok && b == '.' {
		if sawDigits {
			l.advance() // consume '.'
			sawDot = true
			for {
				b, ok := l.peek()
				if !ok || !isDigit(b) {
					break
				}
				l.advance()
			}
		} else if b2, ok2 := l.peekN(1); ok2 && isDigit(b2) {
			l.advance() // consume '.'
			sawDot = true
			for {
				b, ok := l.peek()
				if !ok || !isDigit(b) {
					break
				}
				l.advance()
			}
			sawDigits = true
		}
	}

	// exponent
	sawExp := false
	if b, ok := l.peek(); ok && (b == 'e' || b == 'E') {
		save := l.cur
		l.advance()
		if b2, ok := l.peek(); ok && (b2 == '+' || b2 == '-') {
			l.advance()
		}
		if b3, ok := l.peek(); ok && isDigit(b3) {
			sawExp = true
			for {
				b4, ok := l.peek()
				if !ok || !isDigit(b4) {
					break
				}
				l.advance()
			}
		} else {
			l.cur = save
		}
	}

	if !sawDigits {
		return ILLEGAL, nil, l.err("malformed number")
	}

	lex := l.src[l.start:l.cur]
	if !sawDot && !sawExp {
		v, convErr := strconv.ParseInt(lex, 10, 64)
		if convErr != nil {
			return ILLEGAL, nil, l.err("invalid integer literal")
		}
		return INTEGER, v, nil
	}
	vf, convErr := strconv.ParseFloat(lex, 64)
	if convErr != nil {
		return ILLEGAL, nil, l.err("invalid float literal")
	}
	return NUMBER, vf, nil
}

// ignoreUntilNewline eats until '\n' or EOF.
func (l *Lexer) ignoreUntilNewline() {
	for {
		b, ok := l.peek()
		if !ok || b == '\n' {
			return
		}
		l.advance()
	}
}

// scanAnnotation captures consecutive lines that start with '#' (ignoring leading spaces).
// Terminates on blank line or a line that does not begin (after spaces) with '#'.
func (l *Lexer) scanAnnotation() (string, error) {
	var bldr strings.Builder

	// Consume (indent* '#') with one optional post-# space/tab. Returns true if a line was consumed.
	consumeHashOnLine := func() (bool, error) {
		// skip leading spaces/tabs (not newline)
		for {
			b, ok := l.peek()
			if !ok || b == '\n' {
				break
			}
			if b == ' ' || b == '\t' || b == '\r' {
				l.advance()
				continue
			}
			break
		}
		b, ok := l.peek()
		if !ok || b != '#' {
			return false, nil
		}
		l.advance() // consume '#'
		// Optional single space/tab immediately after '#'
		if b2, ok2 := l.peek(); ok2 && (b2 == ' ' || b2 == '\t') {
			l.advance()
		}
		return true, nil
	}

	// We were called with current char being '#' already consumed by caller in scanToken.
	// Trim one optional space/tab after this first '#'.
	if b, ok := l.peek(); ok && (b == ' ' || b == '\t') {
		l.advance()
	}

	// Capture the rest of the first line
	for {
		b, ok := l.peek()
		if !ok || b == '\n' {
			if ok {
				l.advance() // consume newline
			}
			bldr.WriteByte('\n')
			break
		}
		bldr.WriteByte(b)
		l.advance()
	}

	// Capture subsequent lines that also start with '#'
	for {
		save := l.cur
		cont, err := consumeHashOnLine()
		if err != nil {
			return "", err
		}
		if !cont {
			l.cur = save
			break
		}
		// read until newline
		for {
			b, ok := l.peek()
			if !ok || b == '\n' {
				if ok {
					l.advance()
				}
				bldr.WriteByte('\n')
				break
			}
			bldr.WriteByte(b)
			l.advance()
		}
	}

	s := bldr.String()
	if len(s) == 0 {
		return "", errors.New("incomplete annotation")
	}
	// Trim trailing newlines (at most what we added)
	for len(s) > 0 && s[len(s)-1] == '\n' {
		s = s[:len(s)-1]
	}
	return s, nil
}

// scanInlineParens reads everything between a '(' and the matching ')'.
// Caller must ensure the next byte is '('.
// No nesting supported; runs across newlines; errors if EOF before ')'.
func (l *Lexer) scanInlineParens() (string, error) {
	// consume '('
	if b, ok := l.peek(); !ok || b != '(' {
		return "", l.err("internal: expected '(' after inline opener")
	}
	l.advance()

	var bldr strings.Builder
	for {
		b, ok := l.peek()
		if !ok {
			if l.interactive {
				return "", l.errIncomplete("inline block was not terminated with ')'")
			}
			return "", l.err("inline block was not terminated with ')'")
		}
		if b == ')' {
			l.advance() // consume ')'
			break
		}
		bldr.WriteByte(b)
		l.advance()
	}
	return bldr.String(), nil
}

// --- hash/comment helpers ---

// handleDoubleHash processes '##' comments.
// Returns (handled, err). When handled, the content is ignored and start is advanced.
func (l *Lexer) handleDoubleHash() (bool, error) {
	b1, ok := l.peek()
	if !ok || b1 != '#' {
		return false, nil
	}
	l.advance() // second '#'

	// Inline comment: ##( ... ) → ignore entirely
	if b2, ok2 := l.peek(); ok2 && b2 == '(' {
		if _, err := l.scanInlineParens(); err != nil {
			return true, err
		}
		l.start = l.cur
		return true, nil
	}

	// Line comment: ## ... \n → ignore until newline
	l.ignoreUntilNewline()
	l.start = l.cur
	return true, nil
}

// handleSingleHash processes '#' annotations (inline or multiline).
// Returns (producedAnnotation, text, err).
func (l *Lexer) handleSingleHash() (bool, string, error) {
	if b1, ok := l.peek(); ok && b1 == '(' {
		text, err := l.scanInlineParens()
		if err != nil {
			return false, "", err
		}
		return true, text, nil
	}
	annot, err := l.scanAnnotation()
	if err != nil {
		return false, "", l.err("incomplete annotation")
	}
	return true, annot, nil
}

// --- misc helpers ---

func (l *Lexer) dotStartsNumber() bool {
	b, ok := l.peek()
	if !ok || !isDigit(b) {
		return false
	}
	prev := l.previousToken()
	if l.whitespaceBefore || prev == nil || !canBeLeftOperand(prev.Type) {
		return true
	}
	return false
}

// ---------------- main tokenization ----------------

func (l *Lexer) scanToken() (Token, error) {
	for {
		l.skipWhitespace()
		l.tokStartLine = l.line
		l.tokStartCol = l.col
		l.start = l.cur

		if l.isAtEnd() {
			return l.addToken(EOF, nil), nil
		}

		ch, _ := l.advance()

		// Single-char tokens & punctuation with whitespace-sensitive "(" and "["
		switch ch {
		case '(':
			if l.whitespaceBefore {
				return l.addToken(LROUND, "("), nil
			}
			return l.addToken(CLROUND, "("), nil
		case ')':
			return l.addToken(RROUND, ")"), nil
		case '[':
			if l.whitespaceBefore {
				return l.addToken(LSQUARE, "["), nil
			}
			return l.addToken(CLSQUARE, "["), nil
		case ']':
			return l.addToken(RSQUARE, "]"), nil
		case '{':
			return l.addToken(LCURLY, "{"), nil
		case '}':
			return l.addToken(RCURLY, "}"), nil
		case '+':
			return l.addToken(PLUS, "+"), nil
		case '*':
			return l.addToken(MULT, "*"), nil
		case '/':
			return l.addToken(DIV, "/"), nil
		case '%':
			return l.addToken(MOD, "%"), nil
		case ':':
			return l.addToken(COLON, ":"), nil
		case ',':
			return l.addToken(COMMA, ","), nil
		case '?':
			return l.addToken(QUESTION, "?"), nil
		}

		// '.' : either decimal-starting float or PERIOD
		if ch == '.' {
			if l.dotStartsNumber() {
				l.rewindToStart()
				tt, lit, err := l.scanNumber()
				if err != nil {
					return Token{}, err
				}
				return l.addToken(tt, lit), nil
			}
			return l.addToken(PERIOD, "."), nil
		}

		// Two-char operators and fallbacks
		switch ch {
		case '-':
			if b, ok := l.peek(); ok && b == '>' {
				l.advance()
				return l.addToken(ARROW, "->"), nil
			}
			return l.addToken(MINUS, "-"), nil
		case '=':
			if b, ok := l.peek(); ok && b == '=' {
				l.advance()
				return l.addToken(EQ, "=="), nil
			}
			return l.addToken(ASSIGN, "="), nil
		case '!':
			if b, ok := l.peek(); ok && b == '=' {
				l.advance()
				return l.addToken(NEQ, "!="), nil
			}
			return l.addToken(BANG, "!"), nil
		case '<':
			if b, ok := l.peek(); ok && b == '=' {
				l.advance()
				return l.addToken(LESS_EQ, "<="), nil
			}
			return l.addToken(LESS, "<"), nil
		case '>':
			if b, ok := l.peek(); ok && b == '=' {
				l.advance()
				return l.addToken(GREATER_EQ, ">="), nil
			}
			return l.addToken(GREATER, ">"), nil
		}

		// Comments / Annotations
		if ch == '#' {
			// Try '##' comment forms first
			if handled, err := l.handleDoubleHash(); handled || err != nil {
				if err != nil {
					return Token{}, err
				}
				continue
			}
			// Single '#': inline '#(...)' or multiline block
			ok, text, err := l.handleSingleHash()
			if err != nil {
				return Token{}, err
			}
			if ok {
				return l.addToken(ANNOTATION, text), nil
			}
		}

		// Strings
		if ch == '"' || ch == '\'' {
			l.rewindToStart()
			text, err := l.scanString()
			if err != nil {
				return Token{}, err
			}
			// After '.' a quoted key becomes ID (property name)
			if l.afterDotIsProperty() {
				return l.addToken(ID, text), nil
			}
			return l.addToken(STRING, text), nil
		}

		// Numbers (starting with digit)
		if isDigit(ch) {
			l.rewindToStart()
			tt, lit, err := l.scanNumber()
			if err != nil {
				return Token{}, err
			}
			return l.addToken(tt, lit), nil
		}

		// Identifiers / Keywords
		if isAlpha(ch) {
			l.rewindToStart()
			lex := l.scanIdentifier()
			// After '.', treat as property name (ID)
			if l.afterDotIsProperty() {
				return l.addToken(ID, lex), nil
			}
			if tt, ok := keywords[lex]; ok {
				switch tt {
				case NULL:
					return l.addToken(NULL, nil), nil
				case BOOLEAN:
					if lex == "true" {
						return l.addToken(BOOLEAN, true), nil
					}
					return l.addToken(BOOLEAN, false), nil
				default:
					return l.addToken(tt, lex), nil
				}
			}
			return l.addToken(ID, lex), nil
		}

		return Token{}, l.err(fmt.Sprintf("unexpected character: %q", ch))
	}
}
