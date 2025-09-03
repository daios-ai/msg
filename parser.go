// parser.go — Pratt parser for MindScript that produces compact S-expressions.
//
// OVERVIEW
// --------
// This module implements the newline-aware Pratt parser for the MindScript
// language. It consumes the token stream produced by the *whitespace-sensitive*
// lexer (see lexer.go) and builds a compact, Lisp-style S-expression (AST).
//
// Design goals:
//   - Keep the grammar readable via precedence rules (Pratt parser).
//   - Encode the AST in a tiny, serialisable structure (S-expressions).
//   - Respect whitespace-sensitive signals emitted by the lexer:
//   - '(' can be LROUND or CLROUND; only CLROUND participates in calls.
//   - '[' can be LSQUARE or CLSQUARE; only CLSQUARE participates in indexing.
//   - '.' is PERIOD unless it started a number in the lexer.
//   - multi-line '#' annotations become ANNOTATION tokens.
//   - blank-line runs may be emitted as NOOP tokens.
//   - Support an "interactive" mode that surfaces *Error{Kind:DiagIncomplete}
//     at EOF instead of hard parse errors, suitable for REPLs.
//
// NEW IN THIS VERSION (liberal spacing inside delimiters + precise error locations)
// -------------------------------------------------------------------------------
//
//   - The parser **skips NOOP tokens inside delimited constructs** so that blank
//     lines are treated like whitespace within:
//     array literals:            [ … ]
//     map/object literals:       { … }
//     call argument lists:       f( … )
//     parameter lists:           fun(x: T, …)
//     bracket indices:           a[ … ]
//     computed properties:       obj.( … )
//     grouping parentheses:      ( … )
//
//     The semantics of NOOP at the top level and inside blocks are unchanged: a
//     NOOP still parses as ("noop") outside the above delimited contexts.
//
//   - **Precise error locations:** the parser now uses lexer byte offsets to place
//     carets exactly where users expect.
//
//   - If a token is **missing** (e.g., expected ')' at EOF), the error is
//     anchored **immediately after the last completed node** (using EndByte).
//
//   - If an **unexpected token** is present (wrong lookahead), the error is
//     anchored at the **start** of that token (using StartByte).
//
//   - Interactive incomplete errors follow the same policy.
//
// What the parser returns
// -----------------------
// The AST is a tree of S-expressions: []any whose first element is a string tag.
// Examples (non-exhaustive):
//
//		("block", n1, n2, ...)
//		("id",   name)               // string
//		("int",  int64)              // from INTEGER
//		("num",  float64)            // from NUMBER
//		("str",  string)             // decoded literal
//		("bool", bool)               // from BOOLEAN
//		("null")                     // from NULL
//
//		("unop", op, rhs)            // prefix "-" or "not"; postfix "?"  (op is string)
//		("binop", op, lhs, rhs)      // "+", "-", "*", "/", "%", comparisons, "==", "!=", "and", "or"
//		("assign", target, value)    // "=" (right-assoc)
//
//		("call", callee, arg1, arg2, ...)
//		("get",  obj, ("str", name))                  // property: obj.name or obj."name"
//		("idx",  obj, indexExpr)                      // obj[expr] or obj.(expr) or obj.12
//
//		("array", e1, e2, ...)
//		("map",   ("pair",  keyStrExpr, value)*)
//		("map",   ("pair!", keyStrExpr, value)*)      // required-field (key! : value)
//		("enum",  item1, item2, ...)                  // from Enum[ ... ]
//
//		("fun",     paramsArray, retTypeExprOrAny, bodyBlock)
//		("oracle",  paramsArray, outTypeExprOrAny, sourceExpr)  // optional 'from' expression
//		("module",  nameExpr, bodyBlock)             // module NAME do ... end
//
//		("if", ("pair", cond1, thenBlk1), ..., elseBlk?)       // if/elif/else
//		("while", cond, bodyBlock)
//		("for",   targetPatternOrLvalue, iterExpr, bodyBlock)
//
//		// Declaration patterns (used by 'let' and 'for' targets):
//		("decl", name)
//		("darr", p1, p2, ...)                               // array destructure
//		("dobj", ("pair", keyStrExpr, subPattern), ...)     // object destructure
//
//		// Annotations (from '#'-blocks). POST is encoded by a leading "<" in text.
//		("annot", ("str", textOr<text>), wrappedNode)
//
//	 Annotation encoding:
//	   - PRE annotations are stored as-is:        ("annot", ("str", "note"), expr/pattern)
//	   - POST annotations are prefixed with "<":  ("annot", ("str", "<note"), expr)
//
// Node spans
// ----------
// For tooling, the parser records byte spans (StartByte/EndByte) for each node
// in post-order. Use ParseSExprWithSpans to receive a sidecar *SpanIndex that
// maps each node to its original source span (see spans.go).
//
// **Implementation note (this file):**
// Spans are emitted *at construction sites* for every AST node, including
// intermediate postfix nodes like ("get") and ("idx"), computed-index grouping
// via obj.( … ), structural containers like ("pair")/("pair!"), parameter
// pairs, "decl" patterns, and PRE-annotation wrappers in pattern/key contexts.
// Children are emitted first; when a parent node is formed, its span is
// appended using the earliest token of the left-hand side and the last token
// consumed for the construct, preserving post-order.
//
// Dependencies
// ------------
//   - lexer.go
//   - NewLexer / NewLexerInteractive
//   - Token / TokenType definitions and the tokenization rules
//   - *Error (unified hard diagnostic) with DiagKind; the parser may return
//     *Error{Kind:DiagIncomplete} in interactive mode.
//   - spans.go
//   - type Span, type SpanIndex
//   - func BuildSpanIndexPostOrder(ast S, spans []Span) *SpanIndex
//
// Whitespace-sensitive behavior consumed from the lexer
// -----------------------------------------------------
//
//   - CLROUND vs LROUND:
//
//   - Prefix  (both) parse a parenthesised *grouping*.
//
//   - Postfix only CLROUND starts a *call* chain: f(x) → ("call", ...).
//     `f (x)` uses LROUND and is *not* a call.
//
//   - CLSQUARE vs LSQUARE:
//
//   - Prefix  (both) parse an array literal.
//
//   - Postfix only CLSQUARE is indexing: a[i]. `a [i]` is not indexing.
//
//   - PERIOD chains:
//
//   - After '.', the next token is coerced by the lexer to an ID if it is an
//     identifier or a quoted string, even if it lexically matches a keyword.
//     The parser accepts after '.' one of:
//     (a) LROUND/CLROUND expr RROUND   → computed property: ("idx", obj, expr)
//     (b) INTEGER                      → numeric index:    ("idx", obj, ("int", ...))
//     (c) ID or STRING                 → property:         ("get", obj, ("str", name))
//     Anything else is a parse error.
//
//   - Annotations (ANNOTATION tokens):
//
//   - Classification into PRE vs POST is *line-sensitive*:
//     If the last token on the same line before '#' can end an expression,
//     the annotation is POST (attaches to the left); otherwise it is PRE.
//
//   - General rule: multiple *consecutive PRE* annotations are disallowed
//     (parser error; suggest combining). Inside key parsing, recursion allows
//     stacked PRE annotations in front of a key.
//
//   - POST annotations are attached in the postfix chain; PRE wrap the node
//     that follows. In the AST, POST is represented by a leading "<" in the
//     stored text (see encoding above).
//
//   - NOOP (blank lines):
//     Inside delimiters (() [] {} args/params/indices/grouping): skipped like
//     whitespace. Elsewhere (top level, block bodies): parsed as ("noop").
//     Lone PRE annotation followed by a blank line:
//     ("annot", ("str", text), ("noop"), true),
//     the interpreter treats these as true no-ops; see interpreter.go.
//
// Grammar sketch (informal)
// -------------------------
//
//	program      := expr* EOF
//	expr         := prefix (postfix | infix)*
//	prefix       := literals | ids | grouping | arrays | maps | enums
//	                | unary ("-" | "not") expr
//	                | "fun"    params ["->" type] block
//	                | "oracle" params ["->" type] ["from" expr] block
//	                | "module" expr "do" block "end"
//	                | "if" cond "then" block {"elif" cond "then" block} ["else" block] "end"
//	                | "do" block "end"
//	                | "for" forTarget "in" expr block
//	                | "while" expr block
//	                | "let" declPattern
//	                | annotation (PRE) expr
//	postfix      := "?" | call | index | dot | annotation (POST)
//	call         := CLROUND [args] RROUND
//	index        := CLSQUARE expr RSQUARE
//	dot          := PERIOD ( LROUND expr RROUND | INTEGER | ID | STRING )
//	infix        := right-assoc "=" | right-assoc "->" | precedence-based binary op
//
//	declPattern  := ID | "[" [declPattern {"," declPattern}] "]"
//	                     | "{" [key ":" declPattern {"," key ":" declPattern}] "}"
//	forTarget    := ["let"] (declPattern | assignable)
//	params       := CLROUND [param {"," param}] RROUND
//	param        := ID [":" typeExpr]
//	block        := "do" blockBody "end"    // in forms that require it
//	blockBody    := expr*                    // until a listed keyword (e.g. "end")
//
// Precedence & associativity
// --------------------------
//
//	Highest …  unary ("-", "not"), postfix '?'
//	70         "*" "/" "%"
//	60         "+" "-"
//	50         "<" "<=" ">" ">="
//	40         "==" "!="
//	30         "and"
//	20         "or"
//	15         "->"     (right-assoc as general operator; also used in fun/oracle headers)
//	10         "="      (right-assoc; target must be id/get/idx/decl/darr/dobj)
//
// Errors
// ------
//   - HARD errors are returned as *Error with appropriate Kind:
//   - DiagParse for grammatical mistakes.
//   - DiagIncomplete at EOF in interactive mode (REPL continue).
//   - **Error placement (this version):**
//   - Missing token (e.g. expected ')'): caret after the last completed node.
//   - Wrong lookahead token: caret at the start of that token.
//   - The parser never pretty-prints; public entry points format via FormatError.
//   - The parser never panics for malformed input.
//
// PUBLIC API
// ----------
// Everything below (until `//// END_OF_PUBLIC`) is the public, fully documented
// surface of this module. The implementation details follow afterward.
package mindscript

import (
	"fmt"
	"strings"
)

////////////////////////////////////////////////////////////////////////////////
//                                  PUBLIC API
////////////////////////////////////////////////////////////////////////////////

// S is the S-expression node type produced by the MindScript parser.
//
// Representation
//
//	S is a []any where the first element is a string tag identifying the node
//	kind (e.g., "id", "int", "binop"). Remaining elements are the node's data
//	or children. See the file header for the comprehensive list of node forms.
//
// Invariants
//   - Tags are lowercase ASCII except the special "enum" container.
//   - For property names, keys, and annotation texts, strings are represented
//     as ("str", <go string>).
//   - Literal nodes carry decoded Go values (int64/float64/bool/""/nil).
//
// Spans
//
//	When you call ParseSExprWithSpans, a sidecar *SpanIndex is generated that
//	stores byte spans for each node in post-order traversal.
type S = []any

// L is a tiny helper to build S-expression nodes.
//
// Usage:
//
//	L("binop", "+", L("int", 1), L("id", "x"))
//
// The first argument is the tag; remaining arguments are appended as children.
// It is purely a convenience for tests and meta-tools.
func L(tag string, parts ...any) S { return append([]any{tag}, parts...) }

// ParseSExpr parses a complete MindScript source string and returns its AST.
//
// Behavior:
//   - Runs the whitespace-sensitive lexer (normal mode) and then the Pratt parser.
//   - Returns the fully formed S-expression ("block", ...root items...) even for
//     a single top-level expression.
//   - On lexical problems returns *Error{Kind:DiagLex}. On parse mistakes returns
//     *Error{Kind:DiagParse}. Never pretty-prints; callers may format with FormatError.
//
// Newline semantics (from the lexer) that affect parsing include:
//   - Only CLROUND/CLSQUARE enable call/index chains.
//   - Return/break/continue consume a value only if the next token is on the same
//     line; otherwise they default to ("null").
//
// Liberal spacing (this version):
//   - Inside (), [], {}, call/parameter lists, bracket indices, and obj.(…),
//     blank-line NOOP tokens are ignored.
//
// Spans (this version):
//   - Spans are emitted at construction sites for every node, preserving post-order.
//     This includes intermediate `get`/`idx` nodes, computed indices `.( … )`,
//     structural containers like ("pair") inside maps/patterns/params, and wrappers.
//
// Errors (this version):
//   - Missing token → caret after last completed node (byte-accurate).
//   - Unexpected token present → caret at lookahead start.
func ParseSExpr(src string) (S, error) {
	lex := NewLexer(src)
	toks, err := lex.Scan()
	if err != nil {
		return nil, err
	}
	p := &parser{toks: toks, src: src, lastSpanStartTok: -1, lastSpanEndTok: -1}
	return p.program()
}

// ParseSExprWithSpans parses like ParseSExpr and also returns a *SpanIndex.
//
// The returned SpanIndex (see spans.go) maps nodes (in post-order) to their
// original source byte ranges. This is useful for editor tooling, error
// highlighting, and source maps.
//
// Implementation note:
//
//	The parser records a post-order list of Span values while building the AST.
//	After parsing, BuildSpanIndexPostOrder(ast, p.post) is called to produce the
//	sidecar index. The spans cover every constructed node plus a top-level span
//	for the root "block".
func ParseSExprWithSpans(src string) (S, *SpanIndex, error) {
	lex := NewLexer(src)
	toks, err := lex.Scan()
	if err != nil {
		return nil, nil, err
	}
	p := &parser{toks: toks, src: src, lastSpanStartTok: -1, lastSpanEndTok: -1}
	ast, err := p.program()
	if err != nil {
		return nil, nil, err
	}
	idx := BuildSpanIndexPostOrder(ast, p.post)
	return ast, idx, nil
}

// ParseSExprInteractive parses in REPL-friendly mode.
//
// Differences from ParseSExpr:
//   - Uses NewLexerInteractive so that unterminated strings produce
//     *Error{Kind:DiagIncomplete} at EOF.
//   - The parser also returns *Error{Kind:DiagIncomplete} at EOF for unfinished constructs
//     such as: required ')' or ']' missing, a needed 'end', or after "then"/"->".
//
// Callers can detect this by IsIncomplete(err) and request more input lines.
func ParseSExprInteractive(src string) (S, error) {
	lex := NewLexerInteractive(src)
	toks, err := lex.Scan()
	if err != nil {
		return nil, err
	}
	p := &parser{toks: toks, src: src, interactive: true, lastSpanStartTok: -1, lastSpanEndTok: -1}
	return p.program()
}

//// END_OF_PUBLIC

////////////////////////////////////////////////////////////////////////////////
//                           PRIVATE IMPLEMENTATION
////////////////////////////////////////////////////////////////////////////////

// Pratt parser state and utilities are intentionally unexported.

type parser struct {
	toks        []Token
	i           int
	interactive bool

	// post collects node spans in post-order.
	post []Span

	// Track the most recently emitted node's token span so callers that just
	// parsed a subexpression (e.g., unary/binary parent) can extend to it.
	lastSpanStartTok int
	lastSpanEndTok   int

	// original source for precise byte→(line,col) mapping
	src string
}

// posAtByte returns (line, col1) for byte offset b (columns are 1-based, byte-based).
func (p *parser) posAtByte(b int) (int, int) {
	if b < 0 {
		g := p.peek()
		return g.Line, g.Col + 1
	}
	if b > len(p.src) {
		b = len(p.src)
	}
	line := 1 + strings.Count(p.src[:b], "\n")
	lastNL := strings.LastIndex(p.src[:b], "\n")
	if lastNL < 0 {
		return line, b + 1
	}
	return line, b - lastNL
}

// afterLastPos returns the position immediately after the last completed node.
func (p *parser) afterLastPos() (int, int) {
	if p.lastSpanEndTok >= 0 && p.lastSpanEndTok < len(p.toks) {
		endB := p.toks[p.lastSpanEndTok].EndByte
		return p.posAtByte(endB)
	}
	g := p.peek()
	return g.Line, g.Col + 1
}

// internal helper for PRE-annotations in pattern/key contexts.
type preAnn struct {
	txt    string
	tokIdx int
}

// skipNoops consumes any number of NOOP tokens (blank-line groups). This is
// used inside delimited contexts so that blank lines behave like whitespace.
func (p *parser) skipNoops() {
	for !p.atEnd() && p.peek().Type == NOOP {
		// Emit a span for NOOP *only* when parsed as an expression, not here.
		p.i++
	}
}

// tokenCanEndExpr is used to classify same-line annotations as POST.
func tokenCanEndExpr(tt TokenType) bool {
	switch tt {
	case ID, STRING, INTEGER, NUMBER, BOOLEAN, NULL,
		TYPE, ENUM,
		RROUND, RSQUARE, RCURLY,
		QUESTION:
		return true
	default:
		return false
	}
}

// annotationIsPreAt decides whether ANNOTATION at idx is PRE (true) or POST.
func (p *parser) annotationIsPreAt(idx int) bool {
	if idx <= 0 || idx >= len(p.toks) {
		return true
	}
	ann := p.toks[idx]
	for j := idx - 1; j >= 0; j-- {
		t := p.toks[j]
		if t.Line < ann.Line {
			break
		}
		if t.Line == ann.Line {
			return !tokenCanEndExpr(t.Type)
		}
	}
	return true
}

func tokString(t Token) string {
	if s, ok := t.Literal.(string); ok {
		return s
	}
	return t.Lexeme
}

func (p *parser) atEnd() bool { return p.peek().Type == EOF }
func (p *parser) peek() Token {
	if p.i >= len(p.toks) {
		return p.toks[len(p.toks)-1]
	}
	return p.toks[p.i]
}
func (p *parser) prev() Token { return p.toks[p.i-1] }
func (p *parser) match(tt ...TokenType) bool {
	if p.atEnd() {
		return false
	}
	for _, t := range tt {
		if p.peek().Type == t {
			p.i++
			return true
		}
	}
	return false
}
func (p *parser) need(t TokenType, msg string) (Token, error) {
	if p.match(t) {
		return p.prev(), nil
	}
	g := p.peek()
	if g.Type == EOF {
		// Missing token: anchor where it should be (after last node).
		line, col := p.afterLastPos()
		kind := DiagParse
		if p.interactive {
			kind = DiagIncomplete
		}
		return Token{}, &Error{Kind: kind, Msg: msg, Line: line, Col: col}
	}
	// Wrong token present: anchor at offending lookahead start.
	line, col := p.posAtByte(g.StartByte)
	return Token{}, &Error{Kind: DiagParse, Msg: msg, Line: line, Col: col}
}

func (p *parser) nextTokenIsOnSameLine(as Token) bool {
	if p.atEnd() {
		return false
	}
	return p.peek().Line == as.Line
}

// lbp returns left binding power. Right-assoc is handled by adjusting minBP.
func lbp(t TokenType) (int, bool) {
	switch t {
	case ARROW:
		return 15, true
	case MULT, DIV, MOD:
		return 70, true
	case PLUS, MINUS:
		return 60, true
	case LESS, LESS_EQ, GREATER, GREATER_EQ:
		return 50, true
	case EQ, NEQ:
		return 40, true
	case AND:
		return 30, true
	case OR:
		return 20, true
	case ASSIGN:
		return 10, true
	}
	return 0, false
}

// ---- span helpers ----

func (p *parser) emitSpanByTok(startTok, endTok int) {
	// Guard: allow empty span (e.g., synthetic nodes) by using -1 indexes.
	if startTok >= 0 && endTok >= startTok &&
		startTok < len(p.toks) && endTok < len(p.toks) {
		p.post = append(p.post, Span{
			StartByte: p.toks[startTok].StartByte,
			EndByte:   p.toks[endTok].EndByte,
		})
	} else {
		p.post = append(p.post, Span{})
	}
	p.lastSpanStartTok = startTok
	p.lastSpanEndTok = endTok
}

// takeOnePreAnnotation consumes at most one PRE annotation at the current point.
// Used in pattern/key contexts where we need to emit spans in proper post-order.
func (p *parser) takeOnePreAnnotation() (preAnn, bool, error) {
	if p.atEnd() || p.peek().Type != ANNOTATION || !p.annotationIsPreAt(p.i) {
		return preAnn{}, false, nil
	}
	tok := p.peek()
	p.i++
	txt := ""
	if s, ok := tok.Literal.(string); ok {
		txt = s
	}
	return preAnn{txt: txt, tokIdx: p.i - 1}, true, nil
}

func (p *parser) program() (S, error) {
	var items []any
	for !p.atEnd() {
		e, err := p.expr(0)
		if err != nil {
			return nil, err
		}
		items = append(items, e)
	}
	root := L("block", items...)

	// Top-level span covers all non-EOF tokens.
	startTok := 0
	endTok := len(p.toks) - 2
	if endTok >= startTok && len(p.toks) > 0 {
		p.post = append(p.post, Span{
			StartByte: p.toks[startTok].StartByte,
			EndByte:   p.toks[endTok].EndByte,
		})
	} else {
		p.post = append(p.post, Span{})
	}
	return root, nil
}

func (p *parser) expr(minBP int) (S, error) {
	// We'll maintain the left subtree and the token index where it began,
	// so that newly formed postfix/infix parents can span from that start.
	tokIndexOfThis := p.i // index of the next token to consume
	t := p.peek()
	p.i++

	var left S
	leftStartTok := tokIndexOfThis

	// ---- prefix ----
	switch t.Type {
	case NOOP:
		left = L("noop")
		p.emitSpanByTok(tokIndexOfThis, tokIndexOfThis)

	case ID, TYPE:
		left = L("id", tokString(t))
		p.emitSpanByTok(tokIndexOfThis, tokIndexOfThis)

	case ENUM:
		if p.peek().Type == LSQUARE || p.peek().Type == CLSQUARE {
			// Enum[ a, b ]
			p.i++ // consume '[' or CLSQUARE
			arr, err := p.arrayLiteralAfterOpen()
			if err != nil {
				return nil, err
			}
			it := make([]any, 0, len(arr)-1)
			for i := 1; i < len(arr); i++ {
				it = append(it, arr[i])
			}
			left = L("enum", it...)
			// Span: from 'Enum' token through the closing ']'
			p.emitSpanByTok(tokIndexOfThis, p.i-1)
		} else {
			left = L("id", tokString(t))
			p.emitSpanByTok(tokIndexOfThis, tokIndexOfThis)
		}

	case INTEGER:
		left = L("int", t.Literal)
		p.emitSpanByTok(tokIndexOfThis, tokIndexOfThis)
	case NUMBER:
		left = L("num", t.Literal)
		p.emitSpanByTok(tokIndexOfThis, tokIndexOfThis)
	case STRING:
		left = L("str", t.Literal)
		p.emitSpanByTok(tokIndexOfThis, tokIndexOfThis)
	case BOOLEAN:
		left = L("bool", t.Literal)
		p.emitSpanByTok(tokIndexOfThis, tokIndexOfThis)
	case NULL:
		left = L("null")
		p.emitSpanByTok(tokIndexOfThis, tokIndexOfThis)

	case MINUS, NOT:
		if p.atEnd() && p.interactive {
			line, col := p.posAtByte(t.StartByte)
			return nil, &Error{Kind: DiagIncomplete, Msg: "expected expression after unary operator", Line: line, Col: col}
		}
		r, err := p.expr(80)
		if err != nil {
			return nil, err
		}
		left = L("unop", t.Lexeme, r)
		// Span: operator token through end of RHS (recorded by child)
		endTok := p.lastSpanEndTok
		if endTok < 0 {
			endTok = tokIndexOfThis
		}
		leftStartTok = tokIndexOfThis
		p.emitSpanByTok(leftStartTok, endTok)

	case LROUND, CLROUND:
		// Grouping: no node, but future parents should include the '(' in their span.
		p.skipNoops()
		inner, err := p.expr(0)
		if err != nil {
			return nil, err
		}
		p.skipNoops()
		if _, err := p.need(RROUND, "expected ')'"); err != nil {
			return nil, err
		}
		left = inner
		leftStartTok = tokIndexOfThis // parent nodes will span from '('
		// (no span emit here; child's span already recorded)

	case LSQUARE, CLSQUARE:
		a, err := p.arrayLiteralAfterOpen()
		if err != nil {
			return nil, err
		}
		left = a
		p.emitSpanByTok(tokIndexOfThis, p.i-1) // from '[' to closing ']'
		leftStartTok = tokIndexOfThis

	case LCURLY:
		p.skipNoops()
		if p.match(RCURLY) {
			left = L("map")
			p.emitSpanByTok(tokIndexOfThis, p.i-1) // "{}"
			leftStartTok = tokIndexOfThis
		} else {
			var pairs []any
			for {
				p.skipNoops()
				pairStartTok := p.i
				k, req, err := p.keyRequired()
				if err != nil {
					return nil, err
				}
				p.skipNoops()
				if _, err := p.need(COLON, "expected ':' after key"); err != nil {
					return nil, err
				}
				p.skipNoops()
				v, err := p.expr(0)
				if err != nil {
					return nil, err
				}
				tag := "pair"
				if req {
					tag = "pair!"
				}
				pr := L(tag, k, v)
				// span for the ("pair"/"pair!") node: from key start to end of value
				endTok := p.lastSpanEndTok
				p.emitSpanByTok(pairStartTok, endTok)
				pairs = append(pairs, pr)
				p.skipNoops()

				if !p.match(COMMA) {
					break
				}
				// Trailing comma before '}'
				p.skipNoops()
				if p.peek().Type == RCURLY {
					break
				}
			}
			p.skipNoops()
			if _, err := p.need(RCURLY, "expected '}'"); err != nil {
				return nil, err
			}
			left = L("map", pairs...)
			p.emitSpanByTok(tokIndexOfThis, p.i-1)
			leftStartTok = tokIndexOfThis
		}

	case FUNCTION:
		params, err := p.params()
		if err != nil {
			return nil, err
		}
		var ret any = L("id", "Any")
		if p.match(ARROW) {
			arrowTok := p.prev()
			if p.atEnd() && p.interactive {
				line, col := p.posAtByte(arrowTok.StartByte)
				return nil, &Error{Kind: DiagIncomplete, Msg: "expected return type after '->'", Line: line, Col: col}
			}
			r, err := p.expr(0)
			if err != nil {
				return nil, err
			}
			ret = r
		}
		body, err := p.parseBlock(true)
		if err != nil {
			return nil, err
		}
		left = L("fun", params, ret, body)
		p.emitSpanByTok(tokIndexOfThis, p.i-1)
		leftStartTok = tokIndexOfThis

	case ORACLE:
		params, err := p.params()
		if err != nil {
			return nil, err
		}
		var out any = L("id", "Any")
		if p.match(ARROW) {
			arrowTok := p.prev()
			if p.atEnd() && p.interactive {
				line, col := p.posAtByte(arrowTok.StartByte)
				return nil, &Error{Kind: DiagIncomplete, Msg: "expected output type after '->'", Line: line, Col: col}
			}
			o, err := p.expr(0)
			if err != nil {
				return nil, err
			}
			out = o
		}
		var src any = L("array")
		if p.match(FROM) {
			fromTok := p.prev()
			_ = fromTok
			if p.atEnd() && p.interactive {
				line, col := p.afterLastPos()
				return nil, &Error{Kind: DiagIncomplete, Msg: "expected expression after 'from'", Line: line, Col: col}
			}
			ex, err := p.expr(0)
			if err != nil {
				return nil, err
			}
			src = ex
		}
		left = L("oracle", params, out, src)
		p.emitSpanByTok(tokIndexOfThis, p.i-1)
		leftStartTok = tokIndexOfThis

	case MODULE:
		// module NAME do ... end
		if p.atEnd() && p.interactive {
			line, col := p.posAtByte(t.StartByte)
			return nil, &Error{Kind: DiagIncomplete, Msg: "expected module name expression", Line: line, Col: col}
		}
		name, err := p.expr(0)
		if err != nil {
			return nil, err
		}
		body, err := p.parseBlock(true) // requires 'do ... end'
		if err != nil {
			return nil, err
		}
		left = L("module", name, body)
		p.emitSpanByTok(tokIndexOfThis, p.i-1)
		leftStartTok = tokIndexOfThis

	case RETURN, BREAK, CONTINUE:
		if !p.nextTokenIsOnSameLine(t) {
			// Synthesize a null child (no concrete source span).
			switch t.Type {
			case RETURN:
				left = L("return", L("null"))
			case BREAK:
				left = L("break", L("null"))
			default:
				left = L("continue", L("null"))
			}
			// child "null"
			p.emitSpanByTok(-1, -1)
			// parent span is just the keyword token
			p.emitSpanByTok(tokIndexOfThis, tokIndexOfThis)
			leftStartTok = tokIndexOfThis
		} else {
			x, err := p.expr(0)
			if err != nil {
				return nil, err
			}
			switch t.Type {
			case RETURN:
				left = L("return", x)
			case BREAK:
				left = L("break", x)
			default:
				left = L("continue", x)
			}
			p.emitSpanByTok(tokIndexOfThis, p.lastSpanEndTok)
			leftStartTok = tokIndexOfThis
		}

	case IF:
		thenIf, err := p.ifExpr()
		if err != nil {
			return nil, err
		}
		left = thenIf
		// Span: from 'if' to the corresponding 'end'
		p.emitSpanByTok(tokIndexOfThis, p.i-1)
		leftStartTok = tokIndexOfThis

	case DO:
		body, err := p.parseBlock(false)
		if err != nil {
			return nil, err
		}
		left = body
		// block span emitted inside parseBlock/blockUntil
		leftStartTok = tokIndexOfThis

	case FOR:
		f, err := p.forExpr()
		if err != nil {
			return nil, err
		}
		left = f
		p.emitSpanByTok(tokIndexOfThis, p.i-1)
		leftStartTok = tokIndexOfThis

	case WHILE:
		w, err := p.whileExpr()
		if err != nil {
			return nil, err
		}
		left = w
		p.emitSpanByTok(tokIndexOfThis, p.i-1)
		leftStartTok = tokIndexOfThis

	case LET:
		// Optional single PRE-annotation (disallowed stacking) is handled inside declPattern.
		pat, err := p.declPattern()
		if err != nil {
			return nil, err
		}
		left = pat
		// If pattern is destructuring, ensure '=' appears after optional POST annotations.
		base := unwrapAnnots(pat)
		if tag, _ := base[0].(string); tag == "darr" || tag == "dobj" {
			j := p.i
			for j < len(p.toks) && p.toks[j].Type == ANNOTATION && !p.annotationIsPreAt(j) {
				j++
			}
			if j >= len(p.toks) || p.toks[j].Type != ASSIGN {
				g := p.peek()
				if p.interactive && g.Type == EOF {
					line, col := p.afterLastPos()
					return nil, &Error{Kind: DiagIncomplete, Msg: "expected '=' after destructuring let pattern", Line: line, Col: col}
				}
				line, col := p.posAtByte(g.StartByte)
				return nil, &Error{Kind: DiagParse, Msg: "expected '=' after destructuring let pattern", Line: line, Col: col}
			}
		}
		leftStartTok = tokIndexOfThis // used if a parent wraps it

	case TYPECONS:
		if p.atEnd() && p.interactive {
			line, col := p.posAtByte(t.StartByte)
			return nil, &Error{Kind: DiagIncomplete, Msg: "expected type expression after 'type'", Line: line, Col: col}
		}
		x, err := p.expr(0)
		if err != nil {
			return nil, err
		}
		left = L("type", x)
		p.emitSpanByTok(tokIndexOfThis, p.lastSpanEndTok)
		leftStartTok = tokIndexOfThis

	case ANNOTATION:
		pre := p.annotationIsPreAt(p.i - 1)
		txt := ""
		if s, ok := t.Literal.(string); ok {
			txt = s
		}

		if pre {
			// Disallow consecutive PRE annotations at expression level.
			if !p.atEnd() && p.peek().Type == ANNOTATION && p.annotationIsPreAt(p.i) {
				next := p.peek()
				line, col := p.posAtByte(next.StartByte)
				return nil, &Error{
					Kind: DiagParse, Line: line, Col: col,
					Msg: "multiple consecutive pre-annotations are not allowed; combine them",
				}
			}

			if p.atEnd() && p.interactive {
				line, col := p.posAtByte(t.StartByte)
				return nil, &Error{Kind: DiagIncomplete, Msg: "expected expression after annotation", Line: line, Col: col}
			}
			// child ("str", txt)
			p.emitSpanByTok(tokIndexOfThis, tokIndexOfThis)
			x, err := p.expr(0)
			if err != nil {
				return nil, err
			}
			left = L("annot", L("str", txt), x)
			// parent span from annotation token through end of wrapped expr
			p.emitSpanByTok(tokIndexOfThis, p.lastSpanEndTok)
			leftStartTok = tokIndexOfThis
		} else {
			line, col := p.posAtByte(t.StartByte)
			return nil, &Error{Kind: DiagParse, Msg: "post-annotation has no preceding expression to attach", Line: line, Col: col}
		}

	default:
		if t.Type == EOF && p.interactive {
			line, col := p.afterLastPos()
			return nil, &Error{Kind: DiagIncomplete, Msg: "unexpected end of input", Line: line, Col: col}
		}
		line, col := p.posAtByte(t.StartByte)
		return nil, &Error{Kind: DiagParse, Msg: fmt.Sprintf("unexpected token '%s'", t.Lexeme), Line: line, Col: col}
	}

	// ---- postfix chain ----
	for {
		switch p.peek().Type {
		case QUESTION:
			qtok := p.i
			p.i++
			left = L("unop", "?", left)
			p.emitSpanByTok(leftStartTok, qtok)
			continue

		case CLROUND:
			openTok := p.i - 0 // record before consuming args
			p.i++
			var args []any
			p.skipNoops()
			if !p.match(RROUND) {
				for {
					p.skipNoops()
					a, err := p.expr(0)
					if err != nil {
						return nil, err
					}
					args = append(args, a)
					p.skipNoops()

					if !p.match(COMMA) {
						break
					}
					// Trailing comma before ')'
					p.skipNoops()
					if p.peek().Type == RROUND {
						break
					}
				}
				p.skipNoops()
				if _, err := p.need(RROUND, "expected ')'"); err != nil {
					return nil, err
				}
			}
			left = L("call", append([]any{left}, args...)...)
			p.emitSpanByTok(leftStartTok, p.i-1) // through ')'
			_ = openTok
			continue

		case CLSQUARE:
			openTok := p.i
			p.i++
			p.skipNoops()
			idx, err := p.expr(0)
			if err != nil {
				return nil, err
			}
			p.skipNoops()
			if _, err := p.need(RSQUARE, "expected ']'"); err != nil {
				return nil, err
			}
			left = L("idx", left, idx)
			p.emitSpanByTok(leftStartTok, p.i-1) // through ']'
			_ = openTok
			continue

		case PERIOD:
			p.i++
			if p.match(LROUND) || p.match(CLROUND) {
				p.skipNoops()
				ex, err := p.expr(0)
				if err != nil {
					return nil, err
				}
				p.skipNoops()
				if _, err := p.need(RROUND, "expected ')' after computed property"); err != nil {
					return nil, err
				}
				left = L("idx", left, ex)
				// Span covers from start of left through ')'
				p.emitSpanByTok(leftStartTok, p.i-1)
				continue
			}
			if p.match(INTEGER) {
				intTok := p.i - 1
				intNode := L("int", p.prev().Literal)
				p.emitSpanByTok(intTok, intTok) // child int
				left = L("idx", left, intNode)
				p.emitSpanByTok(leftStartTok, intTok) // parent idx through integer
				continue
			}
			if p.match(ID) || p.match(STRING) {
				propTok := p.i - 1
				prop := L("str", tokString(p.prev()))
				p.emitSpanByTok(propTok, propTok) // child str (property name)
				left = L("get", left, prop)
				p.emitSpanByTok(leftStartTok, propTok) // parent get through prop token
				continue
			}
			g := p.peek()
			line, col := p.posAtByte(g.StartByte)
			return nil, &Error{Kind: DiagParse, Msg: "expected property name, integer, or '(expr)' after '.'", Line: line, Col: col}

		case ANNOTATION:
			// POST attaches to 'left'; PRE belongs to the next thing.
			if p.annotationIsPreAt(p.i) {
				break
			}
			a := p.peek()
			aTok := p.i
			p.i++
			txt := ""
			if s, ok := a.Literal.(string); ok {
				txt = s
			}
			// child ("str", "<txt")
			child := L("str", "<"+txt)
			p.emitSpanByTok(aTok, aTok)
			left = L("annot", child, left)
			// parent spans from start of left through the annotation token
			p.emitSpanByTok(leftStartTok, aTok)
			continue
		}
		break
	}

	// ---- infix ops ----
	for {
		op := p.peek()
		bp, ok := lbp(op.Type)
		if !ok || bp < minBP {
			break
		}
		p.i++

		nextBP := bp + 1
		if op.Type == ASSIGN || op.Type == ARROW {
			nextBP = bp // right-assoc
		}

		if op.Type == ASSIGN && !assignable(left) {
			line, col := p.posAtByte(op.StartByte)
			return nil, &Error{Kind: DiagParse, Msg: "invalid assignment target", Line: line, Col: col}
		}

		if p.atEnd() && p.interactive {
			line, col := p.posAtByte(op.StartByte)
			return nil, &Error{Kind: DiagIncomplete, Msg: "expected expression after operator", Line: line, Col: col}
		}
		right, err := p.expr(nextBP)
		if err != nil {
			return nil, err
		}
		endTok := p.lastSpanEndTok
		if op.Type == ASSIGN {
			left = L("assign", left, right)
			p.emitSpanByTok(leftStartTok, endTok)
		} else {
			left = L("binop", op.Lexeme, left, right)
			p.emitSpanByTok(leftStartTok, endTok)
		}
		// leftStartTok remains the same for continued chaining
	}

	return left, nil
}

func (p *parser) arrayLiteralAfterOpen() (S, error) {
	p.skipNoops()
	if p.match(RSQUARE) {
		// [] — emit span at caller site (which knows the '[' token index)
		return L("array"), nil
	}
	var elems []any
	for {
		p.skipNoops()
		it, err := p.expr(0)
		if err != nil {
			return nil, err
		}
		elems = append(elems, it)
		p.skipNoops()

		// If there is no comma, list ends.
		if !p.match(COMMA) {
			break
		}

		// Trailing comma: allow COMMA followed by the closing ']'.
		p.skipNoops()
		if p.peek().Type == RSQUARE {
			break
		}
	}
	p.skipNoops()
	if _, err := p.need(RSQUARE, "expected ']'"); err != nil {
		return nil, err
	}
	// Caller emits array span (we don't, to avoid mismatches in Enum[...] sugar).
	return L("array", elems...), nil
}

func (p *parser) params() (S, error) {
	openTok := -1
	if tok, err := p.need(CLROUND, "expected '(' to start parameters"); err != nil {
		return nil, err
	} else {
		_ = tok
		openTok = p.i - 1
	}
	p.skipNoops()
	if p.match(RROUND) {
		arr := L("array")
		// '()' — span from '(' to ')'
		p.emitSpanByTok(openTok, p.i-1)
		return arr, nil
	}
	var ps []any
	for {
		p.skipNoops()
		idTok, err := p.need(ID, "expected parameter name")
		if err != nil {
			return nil, err
		}
		idIdx := p.i - 1
		// child id node (parameter name)
		_ = idTok
		p.emitSpanByTok(idIdx, idIdx)
		var t any = L("id", "Any")
		endTokForPair := idIdx
		p.skipNoops()
		if p.match(COLON) {
			if p.atEnd() && p.interactive {
				line, col := p.posAtByte(idTok.StartByte)
				return nil, &Error{Kind: DiagIncomplete, Msg: "expected type after ':'", Line: line, Col: col}
			}
			p.skipNoops()
			e, err := p.expr(0)
			if err != nil {
				return nil, err
			}
			t = e
			endTokForPair = p.lastSpanEndTok
		}
		pr := L("pair", L("id", tokString(idTok)), t)
		p.emitSpanByTok(idIdx, endTokForPair) // span for parameter pair
		ps = append(ps, pr)
		p.skipNoops()

		if !p.match(COMMA) {
			break
		}
		// Trailing comma before ')'
		p.skipNoops()
		if p.peek().Type == RROUND {
			break
		}
	}
	p.skipNoops()
	if _, err := p.need(RROUND, "expected ')' after parameters"); err != nil {
		return nil, err
	}
	arr := L("array", ps...)
	p.emitSpanByTok(openTok, p.i-1)
	return arr, nil
}

// ifExpr builds: ("if", ("pair", cond1, thenBlk1), ... [, elseBlk?])
// (child nodes emit spans internally; the enclosing ("if", ...) span is emitted
// by the caller in expr(), which knows the 'if' token index.)
func (p *parser) ifExpr() (S, error) {
	condStartTok := p.i
	cond, err := p.expr(0)
	if err != nil {
		return nil, err
	}
	if _, err := p.need(THEN, "expected 'then'"); err != nil {
		return nil, err
	}
	thenBlk, err := p.blockUntil(END, ELIF, ELSE)
	if err != nil {
		return nil, err
	}
	arm := L("pair", cond, thenBlk)
	// span for this arm: from cond start through end of then-block
	p.emitSpanByTok(condStartTok, p.lastSpanEndTok)
	arms := []any{arm}

	for p.match(ELIF) {
		condStartTok = p.i
		c, err := p.expr(0)
		if err != nil {
			return nil, err
		}
		if _, err := p.need(THEN, "expected 'then'"); err != nil {
			return nil, err
		}
		b, err := p.blockUntil(END, ELIF, ELSE)
		if err != nil {
			return nil, err
		}
		arm := L("pair", c, b)
		p.emitSpanByTok(condStartTok, p.lastSpanEndTok)
		arms = append(arms, arm)
	}

	var elseTail []any
	if p.match(ELSE) {
		b, err := p.blockUntil(END)
		if err != nil {
			return nil, err
		}
		elseTail = []any{b}
	}
	if _, err := p.need(END, "expected 'end'"); err != nil {
		return nil, err
	}
	return L("if", append(arms, elseTail...)...), nil
}

func (p *parser) parseBlock(requireDo bool) (S, error) {
	if requireDo {
		if _, err := p.need(DO, "expected 'do'"); err != nil {
			return nil, err
		}
	}
	b, err := p.blockUntil(END)
	if err != nil {
		return nil, err
	}
	if _, err := p.need(END, "expected 'end'"); err != nil {
		return nil, err
	}
	return b, nil
}

func (p *parser) blockUntil(stops ...TokenType) (S, error) {
	stop := map[TokenType]bool{}
	for _, s := range stops {
		stop[s] = true
	}
	var items []any
	startTok := p.i
	consumedAny := false
	for !p.atEnd() && !stop[p.peek().Type] {
		e, err := p.expr(0)
		if err != nil {
			return nil, err
		}
		_ = e
		items = append(items, e)
		consumedAny = true
	}
	node := L("block", items...)
	if consumedAny {
		p.emitSpanByTok(startTok, p.i-1)
	} else {
		// Empty block body: emit empty span.
		p.emitSpanByTok(-1, -1)
	}
	return node, nil
}

func (p *parser) forExpr() (S, error) {
	// for <target> in <expr> do ... end
	tgt, err := p.forTarget()
	if err != nil {
		return nil, err
	}
	if _, err := p.need(IN, "expected 'in'"); err != nil {
		return nil, err
	}
	iter, err := p.expr(0)
	if err != nil {
		return nil, err
	}
	body, err := p.parseBlock(true)
	if err != nil {
		return nil, err
	}
	return L("for", tgt, iter, body), nil
}

func (p *parser) whileExpr() (S, error) {
	cond, err := p.expr(0)
	if err != nil {
		return nil, err
	}
	body, err := p.parseBlock(true)
	if err != nil {
		return nil, err
	}
	return L("while", cond, body), nil
}

func (p *parser) forTarget() (S, error) {
	// Allow explicit `let <pattern>`
	if p.match(LET) {
		return p.declPattern()
	}

	// Try to parse a pattern (including annotation-wrapped) with backtracking.
	switch p.peek().Type {
	case LSQUARE, CLSQUARE, LCURLY, ANNOTATION:
		save := p.i
		pt, err := p.declPattern()
		if err == nil {
			return pt, nil
		}
		if p.interactive && IsIncomplete(err) {
			return nil, err
		}
		p.i = save
	}

	save := p.i
	e, err := p.expr(90)
	if err != nil {
		return nil, err
	}
	if !assignable(e) {
		p.i = save
		g := p.peek()
		line, col := p.posAtByte(g.StartByte)
		return nil, &Error{Kind: DiagParse, Msg: "invalid for-target (must be id/get/idx/decl/pattern)", Line: line, Col: col}
	}

	if e[0].(string) == "id" {
		decl := L("decl", e[1].(string))
		// Give the decl the same span as the parsed id.
		p.emitSpanByTok(p.lastSpanStartTok, p.lastSpanEndTok)
		return decl, nil
	}
	return e, nil
}

// assignable unwraps outer "annot" nodes and tests the base shape.
func assignable(n S) bool {
	cur := n
	for len(cur) > 0 {
		tag, _ := cur[0].(string)
		if tag == "annot" {
			if inner, ok := cur[2].(S); ok {
				cur = inner
				continue
			}
		}
		break
	}
	if len(cur) == 0 {
		return false
	}
	switch cur[0] {
	case "id", "get", "idx", "decl", "darr", "dobj":
		return true
	default:
		return false
	}
}

// unwrapAnnots strips any number of outer "annot" wrappers.
func unwrapAnnots(n S) S {
	cur := n
	for len(cur) > 0 {
		tag, _ := cur[0].(string)
		if tag != "annot" {
			break
		}
		inner, ok := cur[2].(S)
		if !ok {
			break
		}
		cur = inner
	}
	return cur
}

// ----- Declaration patterns (with optional PRE-annotation wrapper) -----

func (p *parser) declPattern() (S, error) {
	// Allow a single PRE annotation (multiple consecutive PRE annotations at
	// one point are not allowed for patterns).
	if ann, ok, err := p.takeOnePreAnnotation(); err != nil {
		return nil, err
	} else if ok {
		// child ("str", text) BEFORE parsing the wrapped pattern (to maintain post-order)
		p.emitSpanByTok(ann.tokIdx, ann.tokIdx)
		sub, err := p.declPattern()
		if err != nil {
			return nil, err
		}
		node := L("annot", L("str", ann.txt), sub)
		p.emitSpanByTok(ann.tokIdx, p.lastSpanEndTok)
		return node, nil
	}

	if p.match(ID) {
		idTok := p.i - 1
		decl := L("decl", tokString(p.prev()))
		// span for the decl (use the id token)
		p.emitSpanByTok(idTok, idTok)
		return decl, nil
	}
	if p.match(LSQUARE, CLSQUARE) {
		return p.arrayDeclPattern()
	}
	if p.match(LCURLY) {
		return p.objectDeclPattern()
	}
	g := p.peek()
	if p.interactive && g.Type == EOF {
		line, col := p.afterLastPos()
		return nil, &Error{Kind: DiagIncomplete, Msg: "expected let pattern (id, [], or {})", Line: line, Col: col}
	}
	line, col := p.posAtByte(g.StartByte)
	return nil, &Error{Kind: DiagParse, Msg: "expected let pattern (id, [], or {})", Line: line, Col: col}
}

// --- array destructuring pattern: [p1, p2,] ---
func (p *parser) arrayDeclPattern() (S, error) {
	openTok := p.i - 1 // '[' already consumed
	p.skipNoops()
	if p.match(RSQUARE) {
		node := L("darr")
		p.emitSpanByTok(openTok, p.i-1)
		return node, nil
	}
	var parts []any
	for {
		p.skipNoops()
		pt, err := p.declPattern()
		if err != nil {
			return nil, err
		}
		parts = append(parts, pt)
		p.skipNoops()

		if !p.match(COMMA) {
			break
		}
		// Trailing comma before ']'
		p.skipNoops()
		if p.peek().Type == RSQUARE {
			break
		}
	}
	p.skipNoops()
	if _, err := p.need(RSQUARE, "expected ']' in array pattern"); err != nil {
		return nil, err
	}
	node := L("darr", parts...)
	// Span from '[' to ']'
	p.emitSpanByTok(openTok, p.i-1)
	return node, nil
}

// --- object destructuring pattern: {k: p, ... ,} ---
func (p *parser) objectDeclPattern() (S, error) {
	openTok := p.i - 1 // '{' already consumed
	p.skipNoops()
	if p.match(RCURLY) {
		node := L("dobj")
		p.emitSpanByTok(openTok, p.i-1)
		return node, nil
	}
	var pairs []any
	for {
		p.skipNoops()
		pairStartTok := p.i
		key, err := p.readKeyString()
		if err != nil {
			return nil, err
		}
		p.skipNoops()
		if _, err := p.need(COLON, "expected ':' after key in object pattern"); err != nil {
			return nil, err
		}
		p.skipNoops()
		pt, err := p.declPattern()
		if err != nil {
			return nil, err
		}
		pr := L("pair", key, pt)
		endTok := p.lastSpanEndTok
		p.emitSpanByTok(pairStartTok, endTok) // span for ("pair", key, pt)
		pairs = append(pairs, pr)
		p.skipNoops()

		if !p.match(COMMA) {
			break
		}
		// Trailing comma before '}'
		p.skipNoops()
		if p.peek().Type == RCURLY {
			break
		}
	}
	p.skipNoops()
	if _, err := p.need(RCURLY, "expected '}' in object pattern"); err != nil {
		return nil, err
	}
	node := L("dobj", pairs...)
	p.emitSpanByTok(openTok, p.i-1)
	return node, nil
}

// readKeyString parses a key for object patterns/maps, allowing stacked PRE-annotation
// wrapping via recursion (each level allows at most one PRE, but recursion stacks).
func (p *parser) readKeyString() (S, error) {
	// Allow stacked PRE annotation(s) directly in front of the key by recursion:
	// we consume one annotation, emit its ("str") child span, recurse to read the base key,
	// then emit the parent ("annot", ...) span.
	if ann, ok, err := p.takeOnePreAnnotation(); err != nil {
		return nil, err
	} else if ok {
		// child ("str", text)
		p.emitSpanByTok(ann.tokIdx, ann.tokIdx)
		k, err := p.readKeyString()
		if err != nil {
			return nil, err
		}
		node := L("annot", L("str", ann.txt), k)
		p.emitSpanByTok(ann.tokIdx, p.lastSpanEndTok)
		return node, nil
	}

	if p.match(STRING) {
		leaf := L("str", p.prev().Literal)
		// key comes from a single STRING token → emit span for that token
		p.emitSpanByTok(p.i-1, p.i-1)
		return leaf, nil
	}

	t := p.peek()
	if isWordLike(t.Type) {
		p.i++
		name := t.Lexeme
		if s, ok := t.Literal.(string); ok {
			name = s
		}
		leaf := L("str", name)
		// key comes from a single word-like token → emit span for that token
		p.emitSpanByTok(p.i-1, p.i-1)
		return leaf, nil
	}

	g := p.peek()
	if p.interactive && g.Type == EOF {
		line, col := p.afterLastPos()
		return nil, &Error{Kind: DiagIncomplete, Msg: "expected key", Line: line, Col: col}
	}
	line, col := p.posAtByte(g.StartByte)
	return nil, &Error{Kind: DiagParse, Msg: "expected key", Line: line, Col: col}
}

func isWordLike(tt TokenType) bool {
	switch tt {
	case ID, TYPE, ENUM, BOOLEAN, NULL,
		AND, OR, NOT, LET, DO, END, RETURN, BREAK, CONTINUE,
		IF, THEN, ELIF, ELSE, FUNCTION, ORACLE, MODULE, FOR, IN, FROM,
		TYPECONS:
		return true
	}
	return false
}

func (p *parser) keyRequired() (key S, required bool, err error) {
	k, err := p.readKeyString()
	if err != nil {
		return nil, false, err
	}
	req := p.match(BANG)
	return k, req, nil
}
