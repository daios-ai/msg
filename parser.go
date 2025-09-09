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
// *** Code-size simplifications (this version) ***
//  1. Single postfix dispatcher: one function handles '?', call, index, and dot.
//  2. Single delimited-list path: arrays, call-args, params, array patterns all
//     use the same `bracketed` + `parseCommaList` helpers.
//  3. Centralized trailing-POST handling via `afterExprMaybePost` using the
//     existing PRE/POST classification/merge logic.
//
// Annotation model (lowest precedence):
//   - PRE annotation decorates the expression to its right.
//   - POST annotation decorates the expression to its left and can also appear
//     after a comma or a colon (binding to the element/key/value on the left).
//   - PRE and POST cannot stack. If both apply to the same expression, they are
//     merged into a single PRE with text "pre\npost".
//
// Nodes & Spans
// -------------
// The AST is a tree of S-expressions: []any whose first element is a string tag.
// **This list is the most important reference.**
//
//	("block", n1, n2, ...)
//	("noop")
//
// Literals & identifiers:
//
//	("id",   string)              // identifier (includes property names coerced to ID by lexer rules)
//	("int",  int64)               // from INTEGER
//	("num",  float64)             // from NUMBER
//	("str",  string)              // decoded literal
//	("bool", bool)                // from BOOLEAN
//	("null")                      // from NULL
//	("type", expr)                // from 'type' ...
//
// Operators / expressions:
//
//	("unop",  op,  rhs)           // prefix "-" or "not"; postfix "?"  (op is string)
//	("binop", op,  lhs, rhs)      // "+", "-", "*", "/", "%", comparisons, "==", "!=", "and", "or", "->"
//	("assign", target, value)     // "=" (right-assoc)
//
// Property / call / index:
//
//	("call", callee, arg1, arg2, ...)
//	("get",  obj, ("str", name))             // obj.name or obj."name"
//	("idx",  obj, indexExpr)                 // obj[expr] or obj.(expr) or obj.12
//
// Collections:
//
//	("array", e1, e2, ...)
//	("map",   ("pair",  keyStrExpr, value)*)
//	("map",   ("pair!", keyStrExpr, value)*) // required-field (key! : value)
//	("enum",  item1, item2, ...)             // from Enum[ ... ]
//
// Functions, modules, control, loops:
//
//	("fun",     paramsArray, retTypeExprOrAny, bodyBlock)
//	("oracle",  paramsArray, outTypeExprOrAny, sourceExpr)
//	("module",  nameExpr, bodyBlock)
//	("if", ("pair", cond1, thenBlk1), ..., elseBlk?)
//	("while", cond, bodyBlock)
//	("for",   targetPatternOrLvalue, iterExpr, bodyBlock)
//	("return", value)  // value may be "null" per newline semantics
//	("break",  value)  // value may be "null"
//	("continue", value)// value may be "null"
//
// Declaration patterns (used by 'let' and 'for' targets):
//
//	("decl", name)
//	("darr", p1, p2, ...)
//	("dobj", ("pair", keyStrExpr, subPattern), ...)
//
// Annotations:
//
//	("annot", ("str", textOr<text>), wrappedNode)
//	   • PRE  text stored as-is
//	   • POST text stored with leading "<"
//	   • PRE+POST becomes PRE with "pre\npost" (no POST stacking)
//
// Spans
// -----
// The parser records byte spans (StartByte/EndByte) for every AST node in
// post-order. Use ParseSExprWithSpans to receive a *SpanIndex that maps nodes
// to source spans (see spans.go). When PRE wraps/merges POST we emit a span for
// the child ("str", ...) and for the parent ("annot", ...); when merging POST
// into PRE we *do not* emit a new node or span for an extra wrapper.
//
// Dependencies
// ------------
//   - lexer.go
//   - errors.go (*Error, DiagParse, DiagIncomplete, IsIncomplete)
//   - spans.go (Span, SpanIndex, BuildSpanIndexPostOrder)
//
// Grammar sketch (informal)
// -------------------------
//
//	program      := expr* EOF
//	expr         := prefix (postfix | infix)*
//	prefix       := literals | ids | grouping | arrays | maps | enums
//	              | unary ("-" | "not") expr
//	              | "fun"    params ["->" type] block
//	              | "oracle" params ["->" type] ["from" expr] block
//	              | "module" expr "do" block "end"
//	              | "if" cond "then" block {"elif" cond "then" block} ["else" block] "end"
//	              | "do" block "end"
//	              | "for" forTarget "in" expr block
//	              | "while" expr block
//	              | "let" declPattern
//	              | annotation (PRE) expr
//	postfix      := "?" | call | index | dot
//	call         := CLROUND [args] RROUND
//	index        := CLSQUARE expr RSQUARE
//	dot          := PERIOD ( LROUND expr RROUND | INTEGER | ID | STRING )
//	infix        := right-assoc "=" | right-assoc "->" | precedence-based binary op
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
//	15         "->"     (right-assoc)
//	10         "="      (right-assoc; target must be id/get/idx/decl/darr/dobj)
//	0          (implicit) PRE/POST annotations — **lowest precedence**
package mindscript

import (
	"fmt"
	"strings"
)

////////////////////////////////////////////////////////////////////////////////
//                                  PUBLIC API
////////////////////////////////////////////////////////////////////////////////

type S = []any

func L(tag string, parts ...any) S { return append([]any{tag}, parts...) }

// ParseSExpr parses a complete MindScript source string and returns its AST.
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
func ParseSExprWithSpans(src string) (S, *SpanIndex, error) {
	lex := NewLexer(src)
	toks, err := lex.Scan()
	if err != nil {
		return nil, nil, err
	}
	p := &parser{toks: toks, src: src, lastSpanStartTok: -1, lastSpanEndTok: -1}
	ast, perr := p.program()
	if perr != nil {
		return nil, nil, perr
	}
	idx := BuildSpanIndexPostOrder(ast, p.post)
	return ast, idx, nil
}

// ParseSExprInteractive parses in REPL-friendly mode.
// Unterminated constructs at EOF produce *Error{Kind:DiagIncomplete}.
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

type parser struct {
	toks        []Token
	i           int
	interactive bool

	post             []Span
	lastSpanStartTok int
	lastSpanEndTok   int
	src              string
}

// ───────────────────────────── basics / tokens ─────────────────────────────

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

// expect/expectClose: centralized "skip noops then need".
func (p *parser) expect(t TokenType, msg string) (Token, error) {
	p.skipNoops()
	return p.need(t, msg)
}
func (p *parser) expectClose(t TokenType, msg string) error {
	p.skipNoops()
	_, err := p.need(t, msg)
	return err
}

func (p *parser) need(t TokenType, msg string) (Token, error) {
	if p.match(t) {
		return p.prev(), nil
	}
	g := p.peek()
	if g.Type == EOF {
		line, col := p.posAfterLastSpan()
		kind := DiagParse
		if p.interactive {
			kind = DiagIncomplete
		}
		return Token{}, &Error{Kind: kind, Msg: msg, Line: line, Col: col}
	}
	line, col := p.posAtByte(g.StartByte)
	return Token{}, &Error{Kind: DiagParse, Msg: msg, Line: line, Col: col}
}

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
func (p *parser) posAfterLastSpan() (int, int) {
	if p.lastSpanEndTok >= 0 && p.lastSpanEndTok < len(p.toks) {
		endB := p.toks[p.lastSpanEndTok].EndByte
		return p.posAtByte(endB)
	}
	g := p.peek()
	return g.Line, g.Col + 1
}

func tokText(t Token) string {
	if s, ok := t.Literal.(string); ok {
		return s
	}
	return t.Lexeme
}

// ───────────────────────────── precedence table ────────────────────────────

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

func isRightAssoc(tt TokenType) bool { return tt == ASSIGN || tt == ARROW }

// ───────────────────────────── spans (helpers) ─────────────────────────────

func (p *parser) emitSpanByTok(startTok, endTok int) {
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

// ───────────────────────────── errors (helpers) ────────────────────────────

func (p *parser) needExprAfter(tok Token, msg string) error {
	if p.atEnd() && p.interactive {
		line, col := p.posAtByte(tok.StartByte)
		return &Error{Kind: DiagIncomplete, Msg: msg, Line: line, Col: col}
	}
	return nil
}

// ───────────────────────────── NOOP handling ───────────────────────────────

func (p *parser) skipNoops() {
	for !p.atEnd() && p.peek().Type == NOOP {
		p.i++
	}
}

// ─────────────────────── annotations: classification/merge ─────────────────
//
// Centralized trailing-POST attach/merge (`afterExprMaybePost`), built atop the
// existing PRE/POST machinery. This removes the repetitive callsites where we
// used to do: parse → skip noops → absorbOneTrailingPostAnnot → skip noops.

func tokenCanEndExpr(tt TokenType) bool {
	switch tt {
	case ID, STRING, INTEGER, NUMBER, BOOLEAN, NULL,
		TYPE, ENUM,
		RROUND, RSQUARE, RCURLY,
		QUESTION,
		END:
		return true
	default:
		return false
	}
}

func (p *parser) nextTokenIsOnSameLine(as Token) bool {
	if p.atEnd() {
		return false
	}
	return p.peek().Line == as.Line
}

func (p *parser) isPreAnnotationAt(idx int) bool {
	// POST if there is an expression to the left on the same line,
	// scanning left and skipping ',' and ':'.
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
			if t.Type == COMMA || t.Type == COLON {
				continue
			}
			return !tokenCanEndExpr(t.Type)
		}
	}
	return true
}

func isAnnot(n S) (isAnnot, isPost bool) {
	if len(n) < 2 {
		return false, false
	}
	tag, _ := n[0].(string)
	if tag != "annot" {
		return false, false
	}
	if child, ok := n[1].(S); ok && len(child) >= 2 && child[0] == "str" {
		if s, ok := child[1].(string); ok && len(s) > 0 && s[0] == '<' {
			return true, true
		}
		return true, false
	}
	return true, false
}

func mergePreWithPostText(preAnnot S, postTxt string) S {
	if len(preAnnot) >= 2 {
		if c, ok := preAnnot[1].(S); ok && len(c) >= 2 && c[0] == "str" {
			if s, ok := c[1].(string); ok {
				if postTxt != "" {
					if s == "" {
						c[1] = postTxt
					} else {
						c[1] = s + "\n" + postTxt
					}
				}
			}
		}
	}
	return preAnnot
}

// Single, centralized trailing-post attach (public helper used everywhere).
func (p *parser) afterExprMaybePost(base S, baseStartTok int) (S, error) {
	n, _, err := p.absorbOneTrailingPostAnnot(base, baseStartTok)
	return n, err
}

// absorbOneTrailingPostAnnot applies at most one POST annotation to `base`.
// Span behavior matches the original implementation.
func (p *parser) absorbOneTrailingPostAnnot(base S, baseStartTok int) (S, bool, error) {
	if p.atEnd() || p.peek().Type != ANNOTATION || p.isPreAnnotationAt(p.i) {
		return base, false, nil
	}
	aTok := p.i
	a := p.peek()
	p.i++

	postTxt := ""
	if s, ok := a.Literal.(string); ok {
		postTxt = s
	}

	// Disallow consecutive POSTs (no stacking).
	if !p.atEnd() && p.peek().Type == ANNOTATION && !p.isPreAnnotationAt(p.i) {
		line, col := p.posAtByte(p.peek().StartByte)
		return nil, false, &Error{Kind: DiagParse, Line: line, Col: col, Msg: "multiple consecutive post-annotations are not allowed; combine them"}
	}

	// If base is already PRE, merge and return the same node.
	if ok, isPost := isAnnot(base); ok && !isPost {
		base = mergePreWithPostText(base, postTxt)
		p.skipNoops()
		return base, true, nil
	}

	// Normal POST: wrap once with "<postTxt"
	child := L("str", "<"+postTxt)
	p.emitSpanByTok(aTok, aTok) // span for the annotation "str"
	base = L("annot", child, base)
	p.emitSpanByTok(baseStartTok, aTok) // widen parent to include the '#'
	p.skipNoops()
	return base, true, nil
}

// ───────────────────────────── generic bracketed lists ─────────────────────
//
// NOTE: This single path is now used by arrays, call args, params, and
// array-patterns. It replaces multiple bespoke loops and empty/closer handling.

// bracketed parses a generic comma-list whose opener has just been consumed.
func (p *parser) bracketed(
	close TokenType,
	parseElem func() (S, int, error),
) ([]any, int, int, error) {
	openTok := p.i - 1
	p.skipNoops()
	// Empty: []
	if p.match(close) {
		return nil, openTok, p.i - 1, nil
	}
	elems, err := p.parseCommaList(
		func(tt TokenType) bool { return tt == close },
		parseElem,
	)
	if err != nil {
		return nil, 0, 0, err
	}
	p.skipNoops()
	if _, err := p.need(close, "expected ')'"); err != nil {
		return nil, 0, 0, err
	}
	return elems, openTok, p.i - 1, nil
}

// parseCommaList parses elements until isClose(peek.Type) is true.
// It also performs the standardized "trailing POST binding" both directly
// after the element and after a following comma (POST-after-comma binds left).
func (p *parser) parseCommaList(
	isClose func(TokenType) bool,
	parseElem func() (S, int, error),
) ([]any, error) {
	var out []any
	for {
		p.skipNoops()
		if isClose(p.peek().Type) {
			break
		}

		elem, startTok, err := parseElem()
		if err != nil {
			return nil, err
		}
		// direct trailing POST
		elem, err = p.afterExprMaybePost(elem, startTok)
		if err != nil {
			return nil, err
		}

		p.skipNoops()
		if p.match(COMMA) {
			// POST after comma binds to the left element
			p.skipNoops()
			elem, err = p.afterExprMaybePost(elem, startTok)
			if err != nil {
				return nil, err
			}
			out = append(out, elem)
			// allow trailing comma before closer; outer loop will see closer
			p.skipNoops()
			if isClose(p.peek().Type) {
				break
			}
			continue
		}

		out = append(out, elem)
		break
	}
	return out, nil
}

// ───────────────────────────── program / blocks ────────────────────────────

func (p *parser) program() (S, error) {
	var items []any
	for !p.atEnd() {
		e, err := p.expr(0)
		if err != nil {
			return nil, err
		}
		// absolute-lowest precedence POST at top-level too
		startTok := p.lastSpanStartTok
		if ne, err := p.afterExprMaybePost(e, startTok); err != nil {
			return nil, err
		} else {
			e = ne
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

// blockUntil parses statements until encountering any of the stop tokens.
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
		// absolute-lowest precedence POST inside blocks
		baseStartTok := p.lastSpanStartTok
		if ne, err := p.afterExprMaybePost(e, baseStartTok); err != nil {
			return nil, err
		} else {
			e = ne
		}
		items = append(items, e)
		consumedAny = true
	}
	node := L("block", items...)
	if consumedAny {
		p.emitSpanByTok(startTok, p.i-1)
	} else {
		p.emitSpanByTok(-1, -1)
	}
	return node, nil
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

// ───────────────────────────── tiny helpers (nodes) ────────────────────────

func (p *parser) leaf(tag string, payload any, startTok int) S {
	n := L(tag, payload)
	p.emitSpanByTok(startTok, startTok)
	return n
}
func (p *parser) leafNull(startTok int) S {
	n := L("null")
	p.emitSpanByTok(startTok, startTok)
	return n
}

// Try to build a literal/id leaf. Returns (node, ok).
func (p *parser) tryLiteralOrId(t Token, start int) (S, bool) {
	switch t.Type {
	case ID, TYPE:
		return p.leaf("id", tokText(t), start), true
	case INTEGER:
		return p.leaf("int", t.Literal, start), true
	case NUMBER:
		return p.leaf("num", t.Literal, start), true
	case STRING:
		return p.leaf("str", t.Literal, start), true
	case BOOLEAN:
		return p.leaf("bool", t.Literal, start), true
	case NULL:
		return p.leafNull(start), true
	}
	return nil, false
}

// ───────────────────────────── prefix / postfix / infix ────────────────────

func (p *parser) expr(minBP int) (S, error) {
	tokIndexOfThis := p.i
	t := p.peek()
	p.i++

	var left S
	leftStartTok := tokIndexOfThis

	// ---- prefix ----
	if n, ok := p.tryLiteralOrId(t, tokIndexOfThis); ok {
		left = n
	} else {
		switch t.Type {
		case NOOP:
			left = L("noop")
			p.emitSpanByTok(tokIndexOfThis, tokIndexOfThis)

		case ENUM:
			if p.peek().Type == LSQUARE || p.peek().Type == CLSQUARE {
				p.i++ // consume '[' or CLSQUARE
				// enum literal uses same array parse after '['
				arr, err := p.arrayLiteralAfterOpen()
				if err != nil {
					return nil, err
				}
				items := make([]any, 0, len(arr)-1)
				for i := 1; i < len(arr); i++ {
					items = append(items, arr[i])
				}
				left = L("enum", items...)
				p.emitSpanByTok(tokIndexOfThis, p.i-1)
			} else {
				left = p.leaf("id", tokText(t), tokIndexOfThis)
			}

		case MINUS, NOT:
			if err := p.needExprAfter(t, "expected expression after unary operator"); err != nil {
				return nil, err
			}
			r, err := p.expr(80)
			if err != nil {
				return nil, err
			}
			left = L("unop", t.Lexeme, r)
			endTok := p.lastSpanEndTok
			if endTok < 0 {
				endTok = tokIndexOfThis
			}
			p.emitSpanByTok(tokIndexOfThis, endTok)

		case LROUND, CLROUND:
			inner, err := p.parseGrouping()
			if err != nil {
				return nil, err
			}
			left = inner
			leftStartTok = tokIndexOfThis

		case LSQUARE, CLSQUARE:
			a, err := p.arrayLiteralAfterOpen()
			if err != nil {
				return nil, err
			}
			left = a
			p.emitSpanByTok(tokIndexOfThis, p.i-1)
			leftStartTok = tokIndexOfThis

		case LCURLY:
			mp, err := p.mapLiteralAfterOpen(tokIndexOfThis)
			if err != nil {
				return nil, err
			}
			left = mp
			leftStartTok = tokIndexOfThis

		case FUNCTION:
			fn, endTok, err := p.funExpr(tokIndexOfThis)
			if err != nil {
				return nil, err
			}
			left = fn
			p.emitSpanByTok(tokIndexOfThis, endTok)
			leftStartTok = tokIndexOfThis

		case ORACLE:
			orc, endTok, err := p.oracleExpr(tokIndexOfThis)
			if err != nil {
				return nil, err
			}
			left = orc
			p.emitSpanByTok(tokIndexOfThis, endTok)
			leftStartTok = tokIndexOfThis

		case MODULE:
			if err := p.needExprAfter(t, "expected module name expression"); err != nil {
				return nil, err
			}
			name, err := p.expr(0)
			if err != nil {
				return nil, err
			}
			body, err := p.parseBlock(true)
			if err != nil {
				return nil, err
			}
			left = L("module", name, body)
			p.emitSpanByTok(tokIndexOfThis, p.i-1)
			leftStartTok = tokIndexOfThis

		case RETURN, BREAK, CONTINUE:
			n, err := p.parseControl(t, tokIndexOfThis)
			if err != nil {
				return nil, err
			}
			left = n
			leftStartTok = tokIndexOfThis

		case IF:
			thenIf, err := p.ifExpr()
			if err != nil {
				return nil, err
			}
			left = thenIf
			p.emitSpanByTok(tokIndexOfThis, p.i-1)
			leftStartTok = tokIndexOfThis

		case DO:
			body, err := p.parseBlock(false)
			if err != nil {
				return nil, err
			}
			left = body
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
			pat, err := p.declPattern()
			if err != nil {
				return nil, err
			}
			left = pat
			base := unwrapAnnots(pat)
			if tag, _ := base[0].(string); tag == "darr" || tag == "dobj" {
				j := p.i
				for j < len(p.toks) && p.toks[j].Type == ANNOTATION && !p.isPreAnnotationAt(j) {
					j++
				}
				if j >= len(p.toks) || p.toks[j].Type != ASSIGN {
					g := p.peek()
					if p.interactive && g.Type == EOF {
						line, col := p.posAfterLastSpan()
						return nil, &Error{Kind: DiagIncomplete, Msg: "expected '=' after destructuring let pattern", Line: line, Col: col}
					}
					line, col := p.posAtByte(g.StartByte)
					return nil, &Error{Kind: DiagParse, Msg: "expected '=' after destructuring let pattern", Line: line, Col: col}
				}
			}
			leftStartTok = tokIndexOfThis

		case TYPECONS:
			if err := p.needExprAfter(t, "expected type expression after 'type'"); err != nil {
				return nil, err
			}
			x, err := p.expr(0)
			if err != nil {
				return nil, err
			}
			left = L("type", x)
			p.emitSpanByTok(tokIndexOfThis, p.lastSpanEndTok)
			leftStartTok = tokIndexOfThis

		case ANNOTATION:
			pre := p.isPreAnnotationAt(p.i - 1)
			txt := ""
			if s, ok := t.Literal.(string); ok {
				txt = s
			}

			if pre {
				// forbid stacked PREs
				if !p.atEnd() && p.peek().Type == ANNOTATION && p.isPreAnnotationAt(p.i) {
					next := p.peek()
					line, col := p.posAtByte(next.StartByte)
					return nil, &Error{
						Kind: DiagParse, Line: line, Col: col,
						Msg: "multiple consecutive pre-annotations are not allowed; combine them",
					}
				}
				// If EOF in normal mode: PRE wraps NOOP.
				if p.atEnd() && !p.interactive {
					p.emitSpanByTok(tokIndexOfThis, tokIndexOfThis) // child ("str", txt)
					p.emitSpanByTok(-1, -1)                         // ("noop")
					node := L("annot", L("str", txt), L("noop"))
					p.emitSpanByTok(tokIndexOfThis, tokIndexOfThis) // parent span: just the annot text
					left = node
					leftStartTok = tokIndexOfThis
					break
				}
				if p.atEnd() && p.interactive {
					line, col := p.posAtByte(t.StartByte)
					return nil, &Error{Kind: DiagIncomplete, Msg: "expected expression after annotation", Line: line, Col: col}
				}

				// SPECIAL: PRE preceding a control form binds to the control's value.
				saveI := p.i
				x, err := p.expr(0)
				if err != nil {
					return nil, err
				}
				if tag, _ := x[0].(string); tag == "return" || tag == "break" || tag == "continue" {
					annChild := L("str", txt)
					valueEndTok := p.lastSpanEndTok
					p.emitSpanByTok(tokIndexOfThis, tokIndexOfThis) // PRE text
					// replace x's payload with annotated value (wrapping null if missing)
					if len(x) >= 2 {
						if val, ok := x[1].(S); ok {
							x[1] = L("annot", annChild, val)
						} else {
							x[1] = L("annot", annChild, L("null"))
						}
					} else {
						x = L(tag, L("annot", annChild, L("null")))
					}
					p.emitSpanByTok(tokIndexOfThis, valueEndTok) // annot parent
					p.emitSpanByTok(tokIndexOfThis, valueEndTok) // control node
					left = x
					leftStartTok = tokIndexOfThis
					_ = saveI
					break
				}

				// General PRE wrap (non-control)
				p.emitSpanByTok(tokIndexOfThis, tokIndexOfThis) // PRE child ("str", ...)
				operand := x
				left = L("annot", L("str", txt), operand)
				p.emitSpanByTok(tokIndexOfThis, p.lastSpanEndTok) // annot parent
				leftStartTok = tokIndexOfThis

				// Try to absorb one immediate POST into this PRE (merge txt)
				if nleft, _, err := p.absorbOneTrailingPostAnnot(left, tokIndexOfThis); err != nil {
					return nil, err
				} else {
					left = nleft
				}
				return left, nil
			}

			// POST at prefix position is an error.
			line, col := p.posAtByte(t.StartByte)
			return nil, &Error{Kind: DiagParse, Msg: "post-annotation has no preceding expression to attach", Line: line, Col: col}

		default:
			if t.Type == EOF && p.interactive {
				line, col := p.posAfterLastSpan()
				return nil, &Error{Kind: DiagIncomplete, Msg: "unexpected end of input", Line: line, Col: col}
			}
			line, col := p.posAtByte(t.StartByte)
			return nil, &Error{Kind: DiagParse, Msg: fmt.Sprintf("unexpected token '%s'", t.Lexeme), Line: line, Col: col}
		}
	}

	// ---- postfix chain ---- (single dispatcher now)
	for {
		n, ok, err := p.parseOnePostfix(left, leftStartTok)
		if err != nil {
			return nil, err
		}
		if !ok {
			break
		}
		left = n
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
		if isRightAssoc(op.Type) {
			nextBP = bp
		}

		if op.Type == ASSIGN && !assignable(left) {
			line, col := p.posAtByte(op.StartByte)
			return nil, &Error{Kind: DiagParse, Msg: "invalid assignment target", Line: line, Col: col}
		}

		if err := p.needExprAfter(op, "expected expression after operator"); err != nil {
			return nil, err
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
	}

	return left, nil
}

// parseGrouping reads '(' expr ')' for either LROUND or CLROUND in prefix position.
func (p *parser) parseGrouping() (S, error) {
	p.skipNoops()
	inner, err := p.expr(0)
	if err != nil {
		return nil, err
	}
	p.skipNoops()
	if _, err := p.need(RROUND, "expected ')'"); err != nil {
		return nil, err
	}
	return inner, nil
}

// ───────────────────────────── unified postfix dispatcher ──────────────────
//
// Handles: QUESTION (optional), CLROUND (call), CLSQUARE (index), PERIOD (dot).

func (p *parser) parseOnePostfix(left S, leftStartTok int) (S, bool, error) {
	switch p.peek().Type {
	case QUESTION:
		qtok := p.i
		p.i++
		n := L("unop", "?", left)
		p.emitSpanByTok(leftStartTok, qtok)
		return n, true, nil

	case CLROUND:
		p.i++
		p.skipNoops()
		if p.match(RROUND) {
			n := L("call", left)
			p.emitSpanByTok(leftStartTok, p.i-1)
			return n, true, nil
		}
		args, _, closeTok, err := p.bracketed(RROUND, func() (S, int, error) {
			p.skipNoops()
			start := p.i
			a, err := p.expr(0)
			if err != nil {
				return nil, 0, err
			}
			return a, start, nil
		})
		if err != nil {
			return nil, false, err
		}
		n := L("call", append([]any{left}, args...)...)
		p.emitSpanByTok(leftStartTok, closeTok)
		return n, true, nil

	case CLSQUARE:
		p.i++
		p.skipNoops()
		idx, err := p.expr(0)
		if err != nil {
			return nil, false, err
		}
		p.skipNoops()
		if _, err := p.need(RSQUARE, "expected ']'"); err != nil {
			return nil, false, err
		}
		n := L("idx", left, idx)
		p.emitSpanByTok(leftStartTok, p.i-1)
		return n, true, nil

	case PERIOD:
		p.i++ // consume '.'
		// (expr) -> idx
		if p.match(LROUND) || p.match(CLROUND) {
			p.skipNoops()
			ex, err := p.expr(0)
			if err != nil {
				return nil, false, err
			}
			p.skipNoops()
			if _, perr := p.need(RROUND, "expected ')' after computed property"); perr != nil {
				return nil, false, perr
			}
			n := L("idx", left, ex)
			p.emitSpanByTok(leftStartTok, p.i-1)
			return n, true, nil
		}
		// .<int> -> idx
		if p.match(INTEGER) {
			intTok := p.i - 1
			intNode := L("int", p.prev().Literal)
			p.emitSpanByTok(intTok, intTok)
			n := L("idx", left, intNode)
			p.emitSpanByTok(leftStartTok, intTok)
			return n, true, nil
		}
		// .id / ."str" -> get
		if p.match(ID) || p.match(STRING) {
			propTok := p.i - 1
			prop := L("str", tokText(p.prev()))
			p.emitSpanByTok(propTok, propTok)
			n := L("get", left, prop)
			p.emitSpanByTok(leftStartTok, propTok)
			return n, true, nil
		}
		g := p.peek()
		line, col := p.posAtByte(g.StartByte)
		return nil, false, &Error{Kind: DiagParse, Msg: "expected property name, integer, or '(expr)' after '.'", Line: line, Col: col}
	}
	return nil, false, nil
}

// ───────────────────────── collections / params / maps ────────────────────

func (p *parser) arrayLiteralAfterOpen() (S, error) {
	// Uses the generic bracketed+comma-list path.
	p.skipNoops()
	if p.match(RSQUARE) {
		return L("array"), nil
	}
	elems, err := p.parseCommaList(
		func(tt TokenType) bool { return tt == RSQUARE },
		func() (S, int, error) {
			p.skipNoops()
			start := p.i
			e, err := p.expr(0)
			if err != nil {
				return nil, 0, err
			}
			return e, start, nil
		},
	)
	if err != nil {
		return nil, err
	}
	p.skipNoops()
	if _, perr := p.need(RSQUARE, "expected ']'"); perr != nil {
		return nil, perr
	}
	return L("array", elems...), nil
}

// params parses (CLROUND ... RROUND) parameter pairs; POST can follow each param
// directly or after the comma. Default type is Any. Implemented with the
// unified delimited-list path.
func (p *parser) params() (S, error) {
	var openTok int
	if tok, perr := p.need(CLROUND, "expected '(' to start parameters"); perr != nil {
		return nil, perr
	} else {
		openTok = p.i - 1
		_ = tok
	}
	p.skipNoops()
	if p.match(RROUND) {
		arr := L("array")
		p.emitSpanByTok(openTok, p.i-1)
		return arr, nil
	}

	elems, err := p.parseCommaList(
		func(tt TokenType) bool { return tt == RROUND },
		func() (S, int, error) {
			p.skipNoops()
			idTok, err := p.need(ID, "expected parameter name")
			if err != nil {
				return nil, 0, err
			}
			idIdx := p.i - 1
			p.emitSpanByTok(idIdx, idIdx)
			var t any = L("id", "Any")
			endTokForPair := idIdx
			p.skipNoops()
			if p.match(COLON) {
				if err := p.needExprAfter(idTok, "expected type after ':'"); err != nil {
					return nil, 0, err
				}
				p.skipNoops()
				e, err := p.expr(0)
				if err != nil {
					return nil, 0, err
				}
				t = e
				endTokForPair = p.lastSpanEndTok
			}
			// span for ("pair", name, type)
			p.emitSpanByTok(idIdx, endTokForPair)
			return L("pair", L("id", tokText(idTok)), t), idIdx, nil
		},
	)
	if err != nil {
		return nil, err
	}
	p.skipNoops()
	if _, perr := p.need(RROUND, "expected ')' after parameters"); perr != nil {
		return nil, perr
	}
	arr := L("array", elems...)
	p.emitSpanByTok(openTok, p.i-1)
	return arr, nil
}

func (p *parser) mapLiteralAfterOpen(openTok int) (S, error) {
	p.skipNoops()
	if p.match(RCURLY) {
		node := L("map")
		p.emitSpanByTok(openTok, p.i-1)
		return node, nil
	}
	isClose := func(tt TokenType) bool { return tt == RCURLY }
	readKey := func() (S, int, bool, error) {
		k, err := p.readKeyString()
		if err != nil {
			return nil, 0, false, err
		}
		keyStartTok := p.lastSpanStartTok
		// required '!' after key (map literal variant)
		req := p.match(BANG)
		return k, keyStartTok, req, nil
	}
	parseVal := func() (S, int, error) {
		p.skipNoops()
		start := p.i
		v, err := p.expr(0)
		if err != nil {
			return nil, 0, err
		}
		return v, start, nil
	}

	pairs, err := p.parseKVPairs(isClose, readKey, parseVal)
	if err != nil {
		return nil, err
	}
	p.skipNoops()
	if _, perr := p.need(RCURLY, "expected '}'"); perr != nil {
		return nil, perr
	}
	node := L("map", pairs...)
	p.emitSpanByTok(openTok, p.i-1)
	return node, nil
}

// parseKVPairs parses content inside '{' ... '}' for map literals and object
// patterns. It reuses the centralized trailing-POST logic (after ':' and after
// comma) via `absorbOneTrailingPostAnnot` to keep behavior identical.
func (p *parser) parseKVPairs(
	isClose func(TokenType) bool,
	readKey func() (key S, keyStartTok int, required bool, err error),
	parseValue func() (val S, valStartTok int, err error),
) ([]any, error) {
	var pairs []any
	for {
		p.skipNoops()
		if isClose(p.peek().Type) {
			break
		}
		pairStartTok := p.i

		// key
		k, keyStartTok, required, err := readKey()
		if err != nil {
			return nil, err
		}

		p.skipNoops()
		if _, err := p.need(COLON, "expected ':' after key"); err != nil {
			return nil, err
		}

		// POST that belongs to the key (appears after ':')
		p.skipNoops()
		if nk, _, err := p.absorbOneTrailingPostAnnot(k, keyStartTok); err != nil {
			return nil, err
		} else {
			k = nk
		}

		// value
		p.skipNoops()
		v, vStartTok, err := parseValue()
		if err != nil {
			return nil, err
		}
		// direct value POST
		p.skipNoops()
		if nv, _, err := p.absorbOneTrailingPostAnnot(v, vStartTok); err != nil {
			return nil, err
		} else {
			v = nv
		}

		// optional trailing comma and POST-after-comma binds to value
		p.skipNoops()
		hadComma := p.match(COMMA)
		if hadComma {
			valStartTok := p.lastSpanStartTok
			p.skipNoops()
			if nv, _, err := p.absorbOneTrailingPostAnnot(v, valStartTok); err != nil {
				return nil, err
			} else {
				v = nv
			}
		}

		tag := "pair"
		if required {
			tag = "pair!"
		}
		pr := L(tag, k, v)
		endTok := p.lastSpanEndTok
		p.emitSpanByTok(pairStartTok, endTok)
		pairs = append(pairs, pr)

		p.skipNoops()
		if hadComma {
			// allow trailing comma before closer
			if isClose(p.peek().Type) {
				break
			}
			continue
		}
		// no comma → last entry
		break
	}
	return pairs, nil
}

// ─────────────────────────── control / loops / if ─────────────────────────

func (p *parser) parseControl(t Token, startTok int) (S, error) {
	if !p.nextTokenIsOnSameLine(t) {
		var n S
		switch t.Type {
		case RETURN:
			n = L("return", L("null"))
		case BREAK:
			n = L("break", L("null"))
		default:
			n = L("continue", L("null"))
		}
		p.emitSpanByTok(-1, -1) // child
		p.emitSpanByTok(startTok, startTok)
		return n, nil
	}
	// same line: parse value and absorb POST onto the value
	valStartTok := p.i
	x, err := p.expr(0)
	if err != nil {
		return nil, err
	}
	if nx, _, err := p.absorbOneTrailingPostAnnot(x, valStartTok); err != nil {
		return nil, err
	} else {
		x = nx
	}
	var n S
	switch t.Type {
	case RETURN:
		n = L("return", x)
	case BREAK:
		n = L("break", x)
	default:
		n = L("continue", x)
	}
	p.emitSpanByTok(startTok, p.lastSpanEndTok)
	return n, nil
}

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

func (p *parser) forExpr() (S, error) {
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

// ─────────────────────────── functions / oracle ───────────────────────────

func (p *parser) optionalArrowType(defaultAny any, incMsg string) (any, error) {
	if p.match(ARROW) {
		arrowTok := p.prev()
		if err := p.needExprAfter(arrowTok, incMsg); err != nil {
			return nil, err
		}
		r, err := p.expr(0)
		if err != nil {
			return nil, err
		}
		return r, nil
	}
	return defaultAny, nil
}

func (p *parser) funExpr(openTok int) (S, int, error) {
	params, err := p.params()
	if err != nil {
		return nil, 0, err
	}
	ret, err := p.optionalArrowType(L("id", "Any"), "expected return type after '->'")
	if err != nil {
		return nil, 0, err
	}
	body, perr := p.parseBlock(true)
	if perr != nil {
		return nil, 0, perr
	}
	node := L("fun", params, ret, body)
	return node, p.i - 1, nil
}

func (p *parser) oracleExpr(openTok int) (S, int, error) {
	params, err := p.params()
	if err != nil {
		return nil, 0, err
	}
	out, err := p.optionalArrowType(L("id", "Any"), "expected output type after '->'")
	if err != nil {
		return nil, 0, err
	}
	var src any = L("array")
	if p.match(FROM) {
		if err := p.needExprAfter(p.prev(), "expected expression after 'from'"); err != nil {
			return nil, 0, err
		}
		ex, err := p.expr(0)
		if err != nil {
			return nil, 0, err
		}
		src = ex
	}
	body := L("oracle", params, out, src) // will be wrapped with spans by caller
	return body, p.i - 1, nil
}

// ─────────────────────── declaration patterns (let/for) ───────────────────

type preAnn struct {
	txt    string
	tokIdx int
}

func (p *parser) takeOnePreAnnotation() (preAnn, bool, error) {
	if p.atEnd() || p.peek().Type != ANNOTATION || !p.isPreAnnotationAt(p.i) {
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

func (p *parser) declPattern() (S, error) {
	if ann, ok, err := p.takeOnePreAnnotation(); err != nil {
		return nil, err
	} else if ok {
		// Allow at most one POST to merge immediately after the pattern.
		p.emitSpanByTok(ann.tokIdx, ann.tokIdx) // child ("str", pre)
		sub, err := p.declPattern()
		if err != nil {
			return nil, err
		}
		// Direct trailing POST for the same pattern
		if nsub, _, err := p.absorbOneTrailingPostAnnot(sub, p.lastSpanStartTok); err != nil {
			return nil, err
		} else {
			sub = nsub
		}
		node := L("annot", L("str", ann.txt), sub)
		p.emitSpanByTok(ann.tokIdx, p.lastSpanEndTok)
		return node, nil
	}

	if p.match(ID) {
		idTok := p.i - 1
		decl := L("decl", tokText(p.prev()))
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
		line, col := p.posAfterLastSpan()
		return nil, &Error{Kind: DiagIncomplete, Msg: "expected let pattern (id, [], or {})", Line: line, Col: col}
	}
	line, col := p.posAtByte(g.StartByte)
	return nil, &Error{Kind: DiagParse, Msg: "expected let pattern (id, [], or {})", Line: line, Col: col}
}

// array pattern: [p1, p2,] with POST after comma (uses generic list).
func (p *parser) arrayDeclPattern() (S, error) {
	openTok := p.i - 1
	p.skipNoops()
	if p.match(RSQUARE) {
		node := L("darr")
		p.emitSpanByTok(openTok, p.i-1)
		return node, nil
	}

	parts, err := p.parseCommaList(
		func(tt TokenType) bool { return tt == RSQUARE },
		func() (S, int, error) {
			p.skipNoops()
			start := p.i
			pt, err := p.declPattern()
			if err != nil {
				return nil, 0, err
			}
			return pt, start, nil
		},
	)
	if err != nil {
		return nil, err
	}
	p.skipNoops()
	if _, perr := p.need(RSQUARE, "expected ']' in array pattern"); perr != nil {
		return nil, perr
	}
	node := L("darr", parts...)
	p.emitSpanByTok(openTok, p.i-1)
	return node, nil
}

// object pattern: {k: p, ...} with POST-after-':'/',' on key/value.
func (p *parser) objectDeclPattern() (S, error) {
	openTok := p.i - 1
	p.skipNoops()
	if p.match(RCURLY) {
		node := L("dobj")
		p.emitSpanByTok(openTok, p.i-1)
		return node, nil
	}

	isClose := func(tt TokenType) bool { return tt == RCURLY }
	readKey := func() (S, int, bool, error) {
		k, err := p.readKeyString()
		if err != nil {
			return nil, 0, false, err
		}
		return k, p.lastSpanStartTok, false, nil
	}
	parseVal := func() (S, int, error) {
		p.skipNoops()
		ptStart := p.i
		pt, err := p.declPattern()
		if err != nil {
			return nil, 0, err
		}
		return pt, ptStart, nil
	}

	pairs, err := p.parseKVPairs(isClose, readKey, parseVal)
	if err != nil {
		return nil, err
	}

	p.skipNoops()
	if _, perr := p.need(RCURLY, "expected '}' in object pattern"); perr != nil {
		return nil, perr
	}
	node := L("dobj", pairs...)
	p.emitSpanByTok(openTok, p.i-1)
	return node, nil
}

// readKeyString allows stacked PRE-annotations (handled recursively).
func (p *parser) readKeyString() (S, error) {
	if ann, ok, err := p.takeOnePreAnnotation(); err != nil {
		return nil, err
	} else if ok {
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
		p.emitSpanByTok(p.i-1, p.i-1)
		return leaf, nil
	}

	g := p.peek()
	if p.interactive && g.Type == EOF {
		line, col := p.posAfterLastSpan()
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

// ───────────────────────────── for-target helpers ─────────────────────────

func (p *parser) forTarget() (S, error) {
	if p.match(LET) {
		return p.declPattern()
	}
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
		p.emitSpanByTok(p.lastSpanStartTok, p.lastSpanEndTok)
		return decl, nil
	}
	return e, nil
}

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
