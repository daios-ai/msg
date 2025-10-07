// lexer.go: provides a whitespace-sensitive, UTF-8–aware lexer for the
// MindScript language. It converts a source string into a linear stream of
// tokens with accurate source positions and rich literal decoding.
//
// ──────────────────────────────────────────────────────────────────────────────
// HIGH-LEVEL OVERVIEW
//
// The lexer scans left→right and emits Token values, always ending with EOF.
// Each token carries:
//   - Type     — a TokenType enum
//   - Lexeme   — the exact source slice (verbatim characters from the input)
//   - Literal  — the decoded value for literal tokens (e.g., bool/int/float/string)
//   - Line/Col — 1-based line and 0-based column of the token’s *start*
//   - StartByte/EndByte — byte offsets [start, end) for the token’s source span
//
// Whitespace-sensitive delimiters:
//
//	The lexer decides between LROUND/CLROUND (and LSQUARE/CLSQUARE) solely by
//	whether there is immediate whitespace before the delimiter:
//
//	  '('  → LROUND  if there IS preceding whitespace
//	         CLROUND if there is NO preceding whitespace
//	  '['  → LSQUARE if there IS preceding whitespace
//	         CLSQUARE if there is NO preceding whitespace
//
// Consequences (user-facing syntax):
//   - Calls and parameter lists require NO space before '(':
//     f(x)           // call: uses CLROUND
//     fun(x: T)      // function params: uses CLROUND
//     oracle(x: T)   // oracle params: uses CLROUND
//     With a space ("fun (x: T)"), '(' becomes LROUND and is NOT treated as a
//     parameter list; the parser will error.
//   - Indexing requires NO space before '[':
//     arr[i]         // indexing: uses CLSQUARE
//     With a space ("arr [i]"), '[' is LSQUARE and is NOT treated as indexing.
//   - Grouping "(expr)" is produced regardless of LROUND/CLROUND, but only
//     CLROUND participates in call/juxtaposition chains.
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
// ANNOTATIONS
//   - Hash-line annotations: one or more consecutive lines where, after optional
//     indentation, the first non-space is '#'. The leading '#' (and at most one
//     optional following space) are stripped; lines are joined with '\n', and a
//     single ANNOTATION token is emitted. A blank/non-# line ends the block.
//
// ERRORS (start-of-token anchoring)
//   - Lexical errors (e.g., bad escape, invalid UTF-8, unexpected character) are
//     reported as *Error* with precise location anchored to the **start** of the
//     offending token (the token that is being scanned).
//   - Interactive/REPL mode: if enabled via NewLexerInteractive, unterminated
//     strings produce *IncompleteError* (still anchored to the token start).
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
//	FUNCTION, ORACLE, MODULE,
//	FOR, IN, FROM, WHILE,
//	TYPECONS, TYPE, ENUM
//
// Annotation:
//
//	ANNOTATION — emitted for multi-line blocks starting with '#'.
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
	MODULE
	FOR
	IN
	FROM
	WHILE
	TYPECONS
	TYPE
	ENUM

	// Annotation token (from lines starting with '#')
	ANNOTATION
	NOOP
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
//	StartByte / EndByte — byte offsets [start, end) for the token’s slice.
type Token struct {
	Type      TokenType
	Lexeme    string
	Literal   interface{}
	Line      int
	Col       int
	StartByte int
	EndByte   int
}

// HARD errors produced by the lexer use the unified *Error type defined in
// errors.go. See DiagLex and DiagIncomplete.
//
// In interactive mode, the lexer returns *Error with Kind=DiagIncomplete
// when input ends inside an unterminated construct (e.g., string literal).
//
// **Error locations:** All lexing errors (both hard and incomplete) are anchored
// to the **start of the offending token** (the token whose scanning raised
// the error). This makes diagnostics stable and easy to map to spans.

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
	src    string
	start  int // start index of current token
	cur    int // current index
	line   int // 1-based
	col    int // 0-based column within line
	tokens []Token

	// precise token start position
	tokStartLine int
	tokStartCol  int

	// interactive mode: produce IncompleteError for unterminated constructs at EOF
	interactive bool
}

// NewLexer creates a new lexer for the given source in normal mode.
func NewLexer(src string) *Lexer {
	return &Lexer{
		src: src,
		// very rough guess to reduce reslices on big files
		tokens: make([]Token, 0, len(src)/4),
		line:   1,
		col:    0,
	}
}

// NewLexerInteractive creates a lexer in interactive mode.
// Unterminated strings return IncompleteError at EOF, allowing REPLs to request more input.
func NewLexerInteractive(src string) *Lexer {
	return &Lexer{
		src:         src,
		tokens:      make([]Token, 0, len(src)/4),
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
	"module":   MODULE,
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
		Type:      tt,
		Lexeme:    lex,
		Literal:   lit,
		Line:      l.tokStartLine,
		Col:       l.tokStartCol,
		StartByte: l.start,
		EndByte:   l.cur,
	}
	l.tokens = append(l.tokens, tok)
	l.start = l.cur
	return tok
}

func (l *Lexer) previousToken() *Token {
	if len(l.tokens) == 0 {
		return nil
	}
	return &l.tokens[len(l.tokens)-1]
}

func (l *Lexer) skipHorizontalWhitespace() {
	for !l.isAtEnd() {
		ch, _ := l.peek()
		switch ch {
		case ' ', '\t', '\r':
			l.advance()
			l.start = l.cur
		default:
			return
		}
	}
}

// immediateWhitespaceBefore reports whether the byte immediately preceding the
// current token start is ASCII whitespace. Called *after* skipWhitespace.
func (l *Lexer) immediateWhitespaceBefore() bool {
	if l.start == 0 {
		return false
	}
	switch l.src[l.start-1] {
	case ' ', '\t', '\n', '\r', ';':
		return true
	default:
		return false
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

// ---------------- error builders (start-of-token anchored) ----------------

func (l *Lexer) err(msg string) error {
	// All lexer diagnostics are anchored to the start of the *current* token,
	// which is the most stable and helpful position for tools and users.
	return &Error{Kind: DiagLex, Msg: msg, Src: nil, Line: l.tokStartLine, Col: l.tokStartCol + 1}
}

func (l *Lexer) errIncomplete(msg string) error {
	// Report need-more-input conditions in interactive mode, anchored to token start.
	return &Error{Kind: DiagIncomplete, Msg: msg, Src: nil, Line: l.tokStartLine, Col: l.tokStartCol + 1}
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
				toHex := func(b byte) int {
					switch {
					case b >= '0' && b <= '9':
						return int(b - '0')
					case b >= 'a' && b <= 'f':
						return 10 + int(b-'a')
					case b >= 'A' && b <= 'F':
						return 10 + int(b-'A')
					default:
						return -1
					}
				}
				val := 0
				for i := 0; i < 4; i++ {
					b, ok := l.peek()
					if !ok {
						if l.interactive {
							return "", l.errIncomplete("unicode escape was not terminated (expect 4 hex digits)")
						}
						return "", l.err("unicode escape was not terminated (expect 4 hex digits)")
					}
					d := toHex(b)
					if d < 0 {
						return "", l.err("invalid unicode escape")
					}
					val = (val << 4) | d
					l.advance()
				}
				r := rune(val)

				// handle surrogate pair \uD800-\uDBFF followed by \uDC00-\uDFFF
				if 0xD800 <= r && r <= 0xDBFF {
					saveCur := l.cur
					saveLine, saveCol := l.line, l.col
					if b1, ok := l.peek(); ok && b1 == '\\' {
						l.advance()
						if b2, ok := l.peek(); ok && b2 == 'u' {
							l.advance()
							val2 := 0
							for i := 0; i < 4; i++ {
								b, ok := l.peek()
								if !ok {
									if l.interactive {
										return "", l.errIncomplete("unicode surrogate pair low was not terminated")
									}
									return "", l.err("unicode surrogate pair low was not terminated")
								}
								d := toHex(b)
								if d < 0 {
									return "", l.err("invalid unicode surrogate pair low")
								}
								val2 = (val2 << 4) | d
								l.advance()
							}
							r2 := rune(val2)
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
// Precondition: current position is at l.start and the first rune is either a digit
// or a '.' that has already been verified to start a number.
func (l *Lexer) scanNumber() (tok TokenType, lit interface{}, err error) {
	sawDigits := false
	sawDot := false
	sawExp := false

	// integral part (optional for leading '.')
	for {
		b, ok := l.peek()
		if !ok || !isDigit(b) {
			break
		}
		l.advance()
		sawDigits = true
	}

	// fractional part
	if b, ok := l.peek(); ok && b == '.' {
		// Do NOT consume '.' if we're after a property PERIOD (obj.<digits>)
		// OR if the next char is also '.' (e.g. "1..2").
		if !(l.afterDotIsProperty()) {
			if b2, ok2 := l.peekN(1); !(ok2 && b2 == '.') {
				l.advance() // consume '.'
				sawDot = true
				for {
					b2, ok2 := l.peek()
					if !ok2 || !isDigit(b2) {
						break
					}
					l.advance()
					sawDigits = true
				}
			}
		}
	}

	// exponent part: ([eE][+-]?digits)?
	if b, ok := l.peek(); ok && (b == 'e' || b == 'E') {
		save := l.cur
		l.advance() // consume e/E
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
			l.cur = save // no exponent
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

// scanAnnotation captures consecutive lines that start with '#' (ignoring leading spaces).
// Terminates on blank line or a line that does not begin (after spaces) with '#'.
func (l *Lexer) scanAnnotation() (string, error) {
	var bldr strings.Builder

	// helper: check if the *next* line (after current '\n') starts with '#'
	nextLineStartsWithHash := func() bool {
		// we are currently positioned at the '\n' (unconsumed)
		probe := l.cur + 1
		for probe < len(l.src) {
			c := l.src[probe]
			if c == ' ' || c == '\t' || c == '\r' {
				probe++
				continue
			}
			break
		}
		return probe < len(l.src) && l.src[probe] == '#'
	}

	// Trim at most one ASCII space (NOT tab) after the first '#'.
	if b, ok := l.peek(); ok && b == ' ' {
		l.advance()
	}

	// ----- capture first line, but DO NOT consume its trailing '\n' -----
	for {
		b, ok := l.peek()
		if !ok || b == '\n' {
			// do not l.advance() here; leave '\n' in the stream
			bldr.WriteByte('\n')
			break
		}
		bldr.WriteByte(b)
		l.advance()
	}

	// If there is another annotation line, consume the '\n' now and continue.
	if b, ok := l.peek(); ok && b == '\n' && nextLineStartsWithHash() {
		l.advance() // consume newline to move to start of the next line
	} else {
		// single-line block; leave the '\n' unconsumed
		s := bldr.String()
		if len(s) == 0 {
			return "", errors.New("incomplete annotation")
		}
		for len(s) > 0 && s[len(s)-1] == '\n' {
			s = s[:len(s)-1]
		}
		return s, nil
	}

	// ----- capture subsequent '#'-prefixed lines -----
	consumeHashOnLine := func() (bool, error) {
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
		l.advance() // '#'
		// Trim at most one ASCII space (NOT tab) after '#'
		if b2, ok2 := l.peek(); ok2 && b2 == ' ' {
			l.advance()
		}
		return true, nil
	}

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

		// read rest of line, but DO NOT consume trailing '\n'
		for {
			b, ok := l.peek()
			if !ok || b == '\n' {
				bldr.WriteByte('\n')
				break
			}
			bldr.WriteByte(b)
			l.advance()
		}

		// If another '#'-line follows, eat this '\n' and continue; else stop (leave '\n').
		if b, ok := l.peek(); ok && b == '\n' && nextLineStartsWithHash() {
			l.advance() // consume newline to move to next line
		} else {
			break // leave final '\n' unconsumed
		}
	}

	s := bldr.String()
	if len(s) == 0 {
		return "", errors.New("incomplete annotation")
	}
	for len(s) > 0 && s[len(s)-1] == '\n' {
		s = s[:len(s)-1]
	}
	return s, nil
}

// --- hash/comment helpers ---

// handleSingleHash processes '#' annotations (inline or multiline).
// Returns (producedAnnotation, text, err).
func (l *Lexer) handleSingleHash() (bool, string, error) {
	annot, err := l.scanAnnotation()
	if err != nil {
		return false, "", l.err("incomplete annotation")
	}
	return true, annot, nil
}

// scanNoopIfPresent emits a NOOP token if the source at the current position
// matches: '\n' ( hws* '\n' )+  where hws ∈ {' ', '\r', '\t'}.
func (l *Lexer) scanNoopIfPresent() (Token, bool) {
	b, ok := l.peek()
	if !ok || b != '\n' {
		return Token{}, false
	}

	// Snapshot so we can roll back on non-match (i.e., if we don't see at least one repetition).
	saveCur, saveLine, saveCol, saveStart := l.cur, l.line, l.col, l.start

	// Token starts here.
	l.tokStartLine = l.line
	l.tokStartCol = l.col
	l.start = l.cur

	// Consume the initial newline.
	l.advance()

	matchedReps := 0
	for {
		// Consume zero or more horizontal whitespace chars.
		for {
			nb, ok := l.peek()
			if !ok || (nb != ' ' && nb != '\t' && nb != '\r') {
				break
			}
			l.advance()
		}
		// Require a newline to complete one (hws* '\n') repetition.
		nb, ok := l.peek()
		if ok && nb == '\n' {
			l.advance()
			matchedReps++
			continue
		}
		break
	}

	if matchedReps >= 1 {
		return l.addToken(NOOP, nil), true
	}

	// Not a full match: restore and report no-op not present.
	l.cur, l.line, l.col, l.start = saveCur, saveLine, saveCol, saveStart
	return Token{}, false
}

// --- misc helpers ---

func (l *Lexer) dotStartsNumber() bool {
	b, ok := l.peek()
	if !ok || !isDigit(b) {
		return false
	}
	prev := l.previousToken()
	if l.immediateWhitespaceBefore() || prev == nil || !canBeLeftOperand(prev.Type) {
		return true
	}
	return false
}

// ---------------- main tokenization ----------------

func (l *Lexer) scanToken() (Token, error) {
	for {
		l.skipHorizontalWhitespace()
		l.tokStartLine = l.line
		l.tokStartCol = l.col
		l.start = l.cur

		if l.isAtEnd() {
			return l.addToken(EOF, nil), nil
		}

		// No-op (blank lines)
		if b, _ := l.peek(); b == '\n' {
			if tok, ok := l.scanNoopIfPresent(); ok {
				return tok, nil
			}
			// Lone newline (or newline followed by non-hws) → skip it as whitespace and loop.
			l.advance()
			l.start = l.cur
			continue
		}

		// Treat semicolons exactly like newlines for layout/same-line logic.
		// They are NOT tokens; we "lower" them to line breaks.
		if b, _ := l.peek(); b == ';' {
			// Consume ';' and advance line/col as if it were '\n'.
			l.advance()     // move past ';'
			l.line++        // start a new line
			l.col = 0       // reset column
			l.start = l.cur // next token starts after the semicolon
			continue
		}

		ch, _ := l.advance()

		// Single-char tokens & punctuation with whitespace-sensitive "(" and "["
		switch ch {
		case '(':
			if l.immediateWhitespaceBefore() {
				return l.addToken(LROUND, "("), nil
			}
			return l.addToken(CLROUND, "("), nil
		case ')':
			return l.addToken(RROUND, ")"), nil
		case '[':
			if l.immediateWhitespaceBefore() {
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

		// Annotations
		if ch == '#' {
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
					return l.addToken(tt, nil), nil
				}
			}
			return l.addToken(ID, lex), nil
		}

		return Token{}, l.err(fmt.Sprintf("unexpected character: %q", ch))
	}
}
