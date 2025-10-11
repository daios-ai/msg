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
// Annotation model (lowest precedence):
//   - Annotations attach to **values** (not sites/names). The parser collects
//     all nearby annotation texts around a binding site and merges them onto
//     the value node as a single wrapper:
//     ("annot", ("str", mergedText), value)
//   - The AST does not preserve whether a note was written “pre” or “post”.
//     The pretty-printer is responsible for rendering (e.g., multiline → pre,
//     single line → post) and must be deterministic for idempotency:
//     pretty(src) == pretty(pretty(src))
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
//	("annot", ("str", text), wrappedNode)   // merged text; no POST marker
//
// ─────────────────────────────────────────────────────────────────────────────
// SPAN EMISSION INVARIANT (CRITICAL)
// ----------------------------------
// **This file now centralizes AST construction and span emission.**
//
//   - Every AST node is constructed through `mk*` helpers that *atomically*
//     append exactly one span for that node.
//   - Spans are appended in strict **post-order** of the final AST (children
//     first, then parent), left-to-right among siblings.
//   - Wrapper nodes we create (e.g. "annot" from PRE/POST) obey the same rule:
//     child's span first, then the wrapper's span.
//   - Nodes that are synthesized with no concrete tokens (e.g. default type
//     `Any`) still receive a placeholder `Span{}` via `mk*` (using tok=-1).
//   - The root block’s span is appended last.
//
// The helpers in this file enforce the invariant mechanically at every construct.
//
// Dependencies
// ------------
//   - lexer.go
//   - errors.go (*Error, DiagParse, DiagIncomplete, IsIncomplete)
//   - spans.go (Span, SpanIndex, BuildSpanIndexPostOrder)
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

// ParseSExprWithSpans parses like ParseSExpr and also returns a *SpanIndex,
// with spans recorded in strict post-order per the invariant.
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
///////////////////////////// PRIVATE IMPLEMENTATION ///////////////////////////
////////////////////////////////////////////////////////////////////////////////

type parser struct {
	toks        []Token
	i           int
	interactive bool

	post             []Span // strictly post-order: one span per node, appended after children
	lastSpanStartTok int
	lastSpanEndTok   int
	src              string
}

// ─────────────────────────── token basics & helpers ─────────────────────────

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

// joinNonEmpty concatenates non-empty parts with '\n' in order.
func joinNonEmpty(parts ...string) string {
	out := ""
	for _, s := range parts {
		if s == "" {
			continue
		}
		if out == "" {
			out = s
		} else {
			out += "\n" + s
		}
	}
	return out
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

func (p *parser) skipNoops() {
	for !p.atEnd() && p.peek().Type == NOOP {
		p.i++
	}
}

// ───────────────────────── precedence / associativity ──────────────────────

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

// ───────────────────────────── span emission (core) ─────────────────────────
//
// Centralized helpers. **All** node construction goes through these, which
// also append exactly one span for the node (post-order).
//
// Rules:
//   - For leaves tied to a concrete token, pass tok≥0 (start=end=tok).
//   - For synthetic leaves (e.g. default "Any"), pass tok=-1 to emit Span{}.
//   - For parents, pass the token range [startTok, endTok] that covers the node.
//   - Helpers also update (lastSpanStartTok,lastSpanEndTok) to the node’s range,
//     so callers can compose larger parent ranges deterministically.

func (p *parser) appendNodeSpanByTok(startTok, endTok int) {
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

// mkLeaf builds a leaf node whose span is a single token (tok). If tok<0,
// a placeholder empty span is appended (keeps post-order cardinality intact).
func (p *parser) mkLeaf(tag string, tok int, parts ...any) S {
	n := L(tag, parts...)
	p.appendNodeSpanByTok(tok, tok)
	return n
}

// mk builds a parent node after its children were already constructed.
// It appends exactly one span for the parent covering [startTok,endTok].
func (p *parser) mk(tag string, startTok, endTok int, parts ...any) S {
	n := L(tag, parts...)
	p.appendNodeSpanByTok(startTok, endTok)
	return n
}

// ───────────────────────────── NOOP / annotation utils ─────────────────────

func (p *parser) nextTokenIsOnSameLine(as Token) bool {
	if p.atEnd() {
		return false
	}
	return p.peek().Line == as.Line
}

// Collect any number of adjacent ANNOTATION tokens, merging with '\n'.
func (p *parser) collectAnnots() string {
	// Collect only *immediately adjacent* ANNOTATION tokens.
	// Do NOT skip NOOPs here; a blank-line run must break annotation groups
	// and remain as its own statement.
	var parts []string
	for !p.atEnd() && p.peek().Type == ANNOTATION {
		if s, ok := p.peek().Literal.(string); ok && s != "" {
			parts = append(parts, s)
		}
		p.i++ // consume ANNOTATION
		// Intentionally NOT calling p.skipNoops() here.
	}
	return joinNonEmpty(parts...)
}

// attachInlineTrailingAnnots attaches only the trailing ANNOTATION tokens that
// start on the *same line* as the end of the node most recently parsed.
// Own-line annotations (on later lines) are left for the next parse step
// (so they bind forward or wrap a NOOP).
func (p *parser) attachInlineTrailingAnnots(e S) S {
	if p.atEnd() || p.peek().Type != ANNOTATION || p.lastSpanEndTok < 0 {
		return e
	}
	endTok := p.lastSpanEndTok
	if endTok >= len(p.toks) {
		return e
	}
	endLine, _ := p.posAtByte(p.toks[endTok].EndByte)

	var parts []string
	for !p.atEnd() && p.peek().Type == ANNOTATION && p.peek().Line == endLine {
		if s, ok := p.peek().Literal.(string); ok && s != "" {
			parts = append(parts, s)
		}
		p.i++ // consume ANNOTATION
	}
	if len(parts) == 0 {
		return e
	}
	return p.attachAnnot(e, joinNonEmpty(parts...))
}

// Merge text onto a value as a PRE-style annot (single representation in AST).
func (p *parser) attachAnnot(val S, text string) S {
	if text == "" {
		return val
	}
	// If already annotated, unwrap & merge texts, then rewrap.
	if base, have, ok := unwrapAnnot(val); ok {
		merged := joinNonEmpty(have, text)
		child := p.mkLeaf("str", -1, merged)
		return p.mk("annot", -1, -1, child, base)
	}
	child := p.mkLeaf("str", -1, text)
	return p.mk("annot", -1, -1, child, val)
}

// ───────────────────────── program / blocks ────────────────────────────

func (p *parser) program() (S, error) {
	var items []any
	for !p.atEnd() {
		e, err := p.expr(0)
		if err != nil {
			return nil, err
		}
		items = append(items, e)
	}
	rootStart := 0
	rootEnd := len(p.toks) - 2 // last non-EOF
	if rootEnd < rootStart || len(p.toks) == 0 {
		return p.mk("block", -1, -1 /*empty*/), nil
	}
	return p.mk("block", rootStart, rootEnd, items...), nil
}

// blockUntil parses statements until a stop token is seen.
// Span append happens once for the "block" node, after its children.
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
		items = append(items, e)
		consumedAny = true
	}
	if consumedAny {
		return p.mk("block", startTok, p.i-1, items...), nil
	}
	return p.mk("block", -1, -1 /*empty*/), nil
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

// ───────────────────────────── tiny node helpers ───────────────────────────

func (p *parser) tryLiteralOrId(t Token, start int) (S, bool) {
	switch t.Type {
	case ID, TYPE:
		return p.mkLeaf("id", start, tokText(t)), true
	case INTEGER:
		return p.mkLeaf("int", start, t.Literal), true
	case NUMBER:
		return p.mkLeaf("num", start, t.Literal), true
	case STRING:
		return p.mkLeaf("str", start, t.Literal), true
	case BOOLEAN:
		return p.mkLeaf("bool", start, t.Literal), true
	case NULL:
		return p.mkLeaf("null", start), true
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
			left = p.mkLeaf("noop", tokIndexOfThis)

		case ENUM:
			if p.peek().Type == LSQUARE || p.peek().Type == CLSQUARE {
				p.i++ // consume '[' or CLSQUARE
				arr, err := p.arrayLiteralAfterOpen()
				if err != nil {
					return nil, err
				}
				items := make([]any, 0, len(arr)-1)
				for i := 1; i < len(arr); i++ {
					items = append(items, arr[i])
				}
				left = p.mk("enum", tokIndexOfThis, p.i-1, items...)
			} else {
				left = p.mkLeaf("id", tokIndexOfThis, tokText(t))
			}

		case MINUS, NOT:
			if err := p.needExprAfter(t, "expected expression after unary operator"); err != nil {
				return nil, err
			}
			r, err := p.expr(80)
			if err != nil {
				return nil, err
			}
			endTok := p.lastSpanEndTok
			if endTok < 0 {
				endTok = tokIndexOfThis
			}
			left = p.mk("unop", tokIndexOfThis, endTok, t.Lexeme, r)

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
			_ = endTok
			left = fn
			leftStartTok = tokIndexOfThis

		case ORACLE:
			orc, endTok, err := p.oracleExpr(tokIndexOfThis)
			if err != nil {
				return nil, err
			}
			_ = endTok
			left = orc
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
			left = p.mk("module", tokIndexOfThis, p.i-1, name, body)
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
			left = p.mk("if", tokIndexOfThis, p.i-1, thenIf[1:]...)
			leftStartTok = tokIndexOfThis

		case DO:
			body, err := p.parseBlock(false)
			if err != nil {
				return nil, err
			}
			left = body
			leftStartTok = tokIndexOfThis

		case FOR:
			f, err := p.forExpr(tokIndexOfThis)
			if err != nil {
				return nil, err
			}
			left = f
			leftStartTok = tokIndexOfThis

		case WHILE:
			w, err := p.whileExpr(tokIndexOfThis)
			if err != nil {
				return nil, err
			}
			left = w
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
				for j < len(p.toks) && p.toks[j].Type == ANNOTATION {
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
			left = p.mk("type", tokIndexOfThis, p.lastSpanEndTok, x)
			leftStartTok = tokIndexOfThis

		case ANNOTATION:
			txt := ""
			if s, ok := t.Literal.(string); ok {
				txt = s
			}

			if p.atEnd() && p.interactive {
				line, col := p.posAtByte(t.StartByte)
				return nil, &Error{Kind: DiagIncomplete, Msg: "expected expression after annotation", Line: line, Col: col}
			}
			if p.atEnd() && !p.interactive {
				child := p.mkLeaf("str", tokIndexOfThis, txt)
				node := p.mk("annot", tokIndexOfThis, tokIndexOfThis, child, p.mkLeaf("noop", -1))
				left = node
				leftStartTok = tokIndexOfThis
				break
			}

			operand, err := p.expr(0)
			if err != nil {
				return nil, err
			}
			endTok := p.lastSpanEndTok
			if endTok < 0 {
				endTok = tokIndexOfThis
			}

			// If operand is an assignment, fold onto the RHS value (unchanged)
			if len(operand) >= 3 {
				if tag, _ := operand[0].(string); tag == "assign" {
					lhs, _ := operand[1].(S)
					rhs, _ := operand[2].(S)

					// PRE goes before any existing text on the RHS
					if base, have, ok := unwrapAnnot(rhs); ok {
						merged := joinNonEmpty(txt, have) // <--- PRE first
						child := p.mkLeaf("str", -1, merged)
						rhs = p.mk("annot", -1, -1, child, base)
					} else {
						child := p.mkLeaf("str", -1, txt)
						rhs = p.mk("annot", -1, -1, child, rhs)
					}

					left = p.mk("assign", -1, -1, lhs, rhs)
					leftStartTok = tokIndexOfThis
					return left, nil
				}
			}

			// If operand is a control form, PRE attaches to the control's VALUE
			if len(operand) >= 2 {
				if tag, _ := operand[0].(string); tag == "return" || tag == "break" || tag == "continue" {
					val := operand[1].(S)
					if base, have, ok := unwrapAnnot(val); ok {
						merged := joinNonEmpty(txt, have)
						child := p.mkLeaf("str", -1, merged)
						val = p.mk("annot", -1, -1, child, base)
					} else {
						child := p.mkLeaf("str", -1, txt)
						val = p.mk("annot", -1, -1, child, val)
					}
					left = p.mk(tag, tokIndexOfThis, endTok, val)
					leftStartTok = tokIndexOfThis
					return left, nil
				}
			}

			// Normal (non-assignment) case: wrap operand; PRE first, then any existing text
			if base, have, ok := unwrapAnnot(operand); ok {
				merged := joinNonEmpty(txt, have) // <--- PRE first
				child := p.mkLeaf("str", -1, merged)
				left = p.mk("annot", -1, -1, child, base)
			} else {
				child := p.mkLeaf("str", -1, txt)
				left = p.mk("annot", -1, -1, child, operand)
			}
			leftStartTok = tokIndexOfThis
			return left, nil

		default:
			if t.Type == EOF && p.interactive {
				line, col := p.posAfterLastSpan()
				return nil, &Error{Kind: DiagIncomplete, Msg: "unexpected end of input", Line: line, Col: col}
			}
			line, col := p.posAtByte(t.StartByte)
			return nil, &Error{Kind: DiagParse, Msg: fmt.Sprintf("unexpected token '%s'", t.Lexeme), Line: line, Col: col}
		}
	}

	// ---- postfix chain ----
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
		// === ASSIGN normalization with A+B+C+D merge ===
		// A: PRE on LHS site → collected from 'left' by unwrapping.
		// B: annotations immediately after '=' (any count) → collectAnnots.
		// C: PRE on RHS value (unwrap from parsed node).
		// D: trailing annotations after the RHS (and after a trailing comma in lists) → collectAnnots.
		rightParsed, err := p.expr(nextBP)
		if err != nil {
			return nil, err
		}

		endTok := p.lastSpanEndTok
		if op.Type == ASSIGN {
			// A
			cleanLeft, aTxt, _ := unwrapAnnot(left)
			// B (after '=')
			p.skipNoops()
			bTxt := p.collectAnnots()
			// C (unwrap from RHS)
			baseRight, cTxt, _ := unwrapAnnot(rightParsed)
			// D (after RHS and optional trailing comma in lists)
			p.skipNoops()
			dTxt := p.collectAnnots()
			merged := joinNonEmpty(aTxt, bTxt, cTxt, dTxt)
			normRight := p.attachAnnot(baseRight, merged)
			left = p.mk("assign", leftStartTok, endTok, cleanLeft, normRight)
		} else {
			left = p.mk("binop", leftStartTok, endTok, op.Lexeme, left, rightParsed)
		}
	}
	// Attach trailing *same-line* annotations (POST) only at the outermost level.
	// This ensures `… * (8 # note)` doesn't consume `# note` inside the RHS,
	// allowing the outer expression to wrap the whole `(4+5)*8`.
	if minBP == 0 {
		left = p.attachInlineTrailingAnnots(left)
	}
	return left, nil
}

func (p *parser) needExprAfter(tok Token, msg string) error {
	if p.atEnd() && p.interactive {
		line, col := p.posAtByte(tok.StartByte)
		return &Error{Kind: DiagIncomplete, Msg: msg, Line: line, Col: col}
	}
	return nil
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

// ───────────────────────── unified postfix dispatcher ──────────────────────
//
// Handles: QUESTION (optional), CLROUND (call), CLSQUARE (index), PERIOD (dot).
// **Span order** is enforced: children were already appended during their parse.
// We append exactly one span for the new wrapper node (unop '?', call, idx, get).

func (p *parser) parseOnePostfix(left S, leftStartTok int) (S, bool, error) {
	switch p.peek().Type {
	case QUESTION:
		qtok := p.i
		p.i++
		n := p.mk("unop", leftStartTok, qtok, "?", left)
		return n, true, nil

	case CLROUND:
		p.i++
		p.skipNoops()
		if p.match(RROUND) {
			n := p.mk("call", leftStartTok, p.i-1, left)
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
		n := p.mk("call", leftStartTok, closeTok, append([]any{left}, args...)...)
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
		n := p.mk("idx", leftStartTok, p.i-1, left, idx)
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
			n := p.mk("idx", leftStartTok, p.i-1, left, ex)
			return n, true, nil
		}
		// .<int> -> idx
		if p.match(INTEGER) {
			intTok := p.i - 1
			intNode := p.mkLeaf("int", intTok, p.prev().Literal)
			n := p.mk("idx", leftStartTok, intTok, left, intNode)
			return n, true, nil
		}
		// .id / ."str" -> get
		if p.match(ID) || p.match(STRING) {
			propTok := p.i - 1
			prop := p.mkLeaf("str", propTok, tokText(p.prev()))
			n := p.mk("get", leftStartTok, propTok, left, prop)
			return n, true, nil
		}
		g := p.peek()
		line, col := p.posAtByte(g.StartByte)
		return nil, false, &Error{Kind: DiagParse, Msg: "expected property name, integer, or '(expr)' after '.'", Line: line, Col: col}
	}
	return nil, false, nil
}

// ───────────────────────── collections / lists / maps ─────────────────────

// bracketed parses a generic comma-list whose opener has just been consumed.
func (p *parser) bracketed(
	close TokenType,
	parseElem func() (S, int, error),
) ([]any, int, int, error) {
	openTok := p.i - 1
	p.skipNoops()
	// Empty
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

// parseCommaList parses items until isClose(peek.Type) is true.
// It attaches any adjacent annotations before/after each element (incl. after comma) to the element value.
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
		elem, _, err := parseElem()
		if err != nil {
			return nil, err
		}
		// attach trailing annotations to the element
		if tail := p.collectAnnots(); tail != "" {
			elem = p.attachAnnot(elem, tail)
		}

		p.skipNoops()
		if p.match(COMMA) {
			// annotations after the comma also bind to the previous element
			p.skipNoops()
			if extra := p.collectAnnots(); extra != "" {
				elem = p.attachAnnot(elem, extra)
			}
			out = append(out, elem)
			// allow trailing comma
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

func (p *parser) arrayLiteralAfterOpen() (S, error) {
	p.skipNoops()
	if p.match(RSQUARE) {
		return p.mk("array", p.i-2, p.i-1 /*'[]'*/), nil
	}
	elems, err := p.parseCommaList(
		func(tt TokenType) bool { return tt == RSQUARE },
		func() (S, int, error) {
			p.skipNoops()
			e, err := p.expr(0)
			if err != nil {
				return nil, 0, err
			}
			return e, 0, nil
		},
	)
	if err != nil {
		return nil, err
	}
	p.skipNoops()
	if _, perr := p.need(RSQUARE, "expected ']'"); perr != nil {
		return nil, perr
	}
	// span: from '[' (or CLSQUARE) to ']'
	return p.mk("array", p.findMatchingOpenSquare(), p.i-1, elems...), nil
}

func (p *parser) findMatchingOpenSquare() int {
	// Best-effort: previous token (we call right after consuming ']')
	if p.i-2 >= 0 {
		// walk back to LSQUARE/CLSQUARE
		for j := p.i - 2; j >= 0; j-- {
			if p.toks[j].Type == LSQUARE || p.toks[j].Type == CLSQUARE {
				return j
			}
		}
	}
	return p.i - 1
}

// params parses (CLROUND ... RROUND) parameter pairs; POST can follow each param
// directly or after the comma. Default type is Any.
func (p *parser) params() (S, error) {
	if _, perr := p.need(CLROUND, "expected '(' to start parameters"); perr != nil {
		return nil, perr
	}
	openTok := p.i - 1

	var elems []any
	p.skipNoops()
	if p.match(RROUND) {
		return p.mk("array", openTok, p.i-1), nil
	}

	for {
		p.skipNoops()

		// Collect annotations before the parameter name (A)
		aTxt := p.collectAnnots()

		// Name
		idTok, err := p.need(ID, "expected parameter name")
		if err != nil {
			return nil, err
		}
		idIdx := p.i - 1
		nameLeaf := p.mkLeaf("id", idIdx, tokText(idTok))

		// Optional type after ':'
		p.skipNoops()
		var val S
		if p.match(COLON) {
			// B: annotations immediately after ':'
			p.skipNoops()
			bTxt := p.collectAnnots()
			// Type expression
			if err := p.needExprAfter(idTok, "expected type after ':'"); err != nil {
				return nil, err
			}
			p.skipNoops()
			tExpr, err := p.expr(0)
			if err != nil {
				return nil, err
			}
			// C: unwrap pre on type; D: after type (+ after comma)
			baseT, cTxt, _ := unwrapAnnot(tExpr)
			p.skipNoops()
			dTxt := p.collectAnnots()
			merged := joinNonEmpty(aTxt, bTxt, cTxt, dTxt)
			val = p.attachAnnot(baseT, merged)
		} else {
			// No explicit type → implicit Any; merge A + D onto it
			base := p.mkLeaf("id", -1, "Any")
			p.skipNoops()
			dTxt := p.collectAnnots()
			val = p.attachAnnot(base, joinNonEmpty(aTxt, dTxt))
		}

		pair := p.mk("pair", idIdx, p.i-1, nameLeaf, val)
		elems = append(elems, pair)

		// optional trailing comma; any annotations just after it still bind to this param
		p.skipNoops()
		if p.match(COMMA) {
			p.skipNoops()
			if extra := p.collectAnnots(); extra != "" {
				// Attach to the VALUE inside the last ("pair", name, value).
				last := elems[len(elems)-1].(S) // ("pair", nameLeaf, val)
				if len(last) >= 3 {
					val := last[2].(S)
					last[2] = p.attachAnnot(val, extra)
					elems[len(elems)-1] = last
				}
			}
			// allow trailing comma
			p.skipNoops()
			if p.peek().Type == RROUND {
				break
			}
			continue
		}
		break
	}

	p.skipNoops()
	if _, perr := p.need(RROUND, "expected ')' after parameters"); perr != nil {
		return nil, perr
	}
	return p.mk("array", openTok, p.i-1, elems...), nil
}

func (p *parser) mapLiteralAfterOpen(openTok int) (S, error) {
	p.skipNoops()
	if p.match(RCURLY) {
		return p.mk("map", openTok, p.i-1), nil
	}

	isClose := func(tt TokenType) bool { return tt == RCURLY }
	readKey := func() (S, int, bool, error) {
		k, err := p.readKeyString()
		if err != nil {
			return nil, 0, false, err
		}
		keyStartTok := p.lastSpanStartTok
		req := p.match(BANG)
		return k, keyStartTok, req, nil
	}
	parseVal := func() (S, int, error) {
		p.skipNoops()
		v, err := p.expr(0)
		if err != nil {
			return nil, 0, err
		}
		return v, 0, nil
	}

	pairs, err := p.parseKVPairs(isClose, readKey, parseVal)
	if err != nil {
		return nil, err
	}
	p.skipNoops()
	if _, perr := p.need(RCURLY, "expected '}'"); perr != nil {
		return nil, perr
	}
	return p.mk("map", openTok, p.i-1, pairs...), nil
}

// parseKVPairs collects annotations at A (key), B (after ':'), C (value's own), D (after value/comma) and merges onto the value.
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

		k, _, required, err := readKey()
		if err != nil {
			return nil, err
		}
		// A: unwrap & collect key PRE
		baseKey, aTxt, _ := unwrapAnnot(k)
		k = baseKey
		p.skipNoops()
		if _, err := p.need(COLON, "expected ':' after key"); err != nil {
			return nil, err
		}

		// B: annotations after ':' belong to value
		p.skipNoops()
		keyPostTxt := p.collectAnnots()

		// value expresion
		p.skipNoops()
		v, _, err := parseValue()
		if err != nil {
			return nil, err
		}
		// C: unwrap value PRE
		baseVal, cTxt, _ := unwrapAnnot(v)

		// D: after value (+ after comma)
		p.skipNoops()
		dTxt := p.collectAnnots()

		// optional trailing comma (+ POST-after-comma binds to value)
		p.skipNoops()
		hadComma := p.match(COMMA)
		if hadComma {
			p.skipNoops()
			dTxt = joinNonEmpty(dTxt, p.collectAnnots())
		}

		// Merge A→B→C→D onto value
		merged := joinNonEmpty(aTxt, keyPostTxt, cTxt, dTxt)
		valNode := p.attachAnnot(baseVal, merged)

		tag := "pair"
		if required {
			tag = "pair!"
		}
		endTok := p.i - 1
		pr := p.mk(tag, pairStartTok, endTok, k, valNode)
		pairs = append(pairs, pr)

		p.skipNoops()
		if hadComma {
			if isClose(p.peek().Type) {
				break
			}
			continue
		}
		break
	}
	return pairs, nil
}

// ───────────────────────── control / loops / if ───────────────────────────

func (p *parser) parseControl(t Token, startTok int) (S, error) {
	// tag: "return" | "break" | "continue"
	tag := "return"
	switch t.Type {
	case BREAK:
		tag = "break"
	case CONTINUE:
		tag = "continue"
	}

	// Newline after the control word → no value (Null).
	if !p.nextTokenIsOnSameLine(t) {
		// Do NOT consume annotations here; leave them for the next statement.
		// Returning a bare null ensures `# pre` on the following line can wrap
		// the next expression instead of being swallowed by this control.
		return p.mk(tag, startTok, startTok, p.mkLeaf("null", -1)), nil
	}

	p.skipNoops()
	// Same line but next token can't start a value → also Null.
	switch p.peek().Type {
	case END, ELSE, ELIF, THEN, RROUND, RSQUARE, RCURLY:
		return p.mk(tag, startTok, startTok, p.mkLeaf("null", -1)), nil
	}

	// Parse value and attach any adjacent annotations.
	valStartTok := p.i
	_ = valStartTok
	x, err := p.expr(0)
	if err != nil {
		return nil, err
	}
	// Attach any adjacent annotations to the control's value.
	if tail := p.collectAnnots(); tail != "" {
		x = p.attachAnnot(x, tail)
	}
	return p.mk(tag, startTok, p.lastSpanEndTok, x), nil
}

// ────────────────────── Minimal annotation normalization helpers ─────────────
//
// Goal: annotations always attach to the VALUE. We normalize at construction
// sites (ASSIGN, pairs in maps/types/params). Presentation (pre vs post) is a
// pretty-printer concern.

// unwrapAnnot collects stacked ("annot", ("str", s), base) wrappers:
// - merges stacked texts with '\n' (no POST marker semantics);
// - returns (base, mergedText, hadAnnot).
func unwrapAnnot(n S) (S, string, bool) {
	var parts []string
	cur := n
	for len(cur) >= 3 {
		tag, _ := cur[0].(string)
		if tag != "annot" {
			break
		}
		if child, ok := cur[1].(S); ok && len(child) >= 2 && child[0] == "str" {
			if s, _ := child[1].(string); s != "" {
				parts = append(parts, s)
			}
		}
		base, _ := cur[2].(S)
		cur = base
	}
	if len(parts) == 0 {
		return n, "", false
	}
	merged := parts[0]
	for i := 1; i < len(parts); i++ {
		if parts[i] != "" {
			if merged == "" {
				merged = parts[i]
			}
			if merged != "" && parts[i] != "" {
				merged += "\n" + parts[i]
			}
		}
	}
	return cur, merged, true
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
	arm := p.mk("pair", condStartTok, p.lastSpanEndTok, cond, thenBlk)
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
		arm := p.mk("pair", condStartTok, p.lastSpanEndTok, c, b)
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

func (p *parser) forExpr(openTok int) (S, error) {
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
	return p.mk("for", openTok, p.i-1, tgt, iter, body), nil
}

func (p *parser) whileExpr(openTok int) (S, error) {
	cond, err := p.expr(0)
	if err != nil {
		return nil, err
	}
	body, err := p.parseBlock(true)
	if err != nil {
		return nil, err
	}
	return p.mk("while", openTok, p.i-1, cond, body), nil
}

// ───────────────────────── functions / oracle ─────────────────────────────

func (p *parser) optionalArrowType(incMsg string) (S, error) {
	if p.match(ARROW) {
		arrowTok := p.prev()
		if err := p.needExprAfter(arrowTok, incMsg); err != nil {
			return nil, err
		}
		r, err := p.expr(0)
		if err != nil {
			return nil, err
		}
		return r, nil // parsed type (one node)
	}
	return p.mkLeaf("id", -1, "Any"), nil // single synthetic node
}

func (p *parser) funExpr(openTok int) (S, int, error) {
	params, err := p.params()
	if err != nil {
		return nil, 0, err
	}
	ret, err := p.optionalArrowType("expected return type after '->'")
	if err != nil {
		return nil, 0, err
	}
	body, perr := p.parseBlock(true)
	if perr != nil {
		return nil, 0, perr
	}
	node := p.mk("fun", openTok, p.i-1, params, ret, body)
	return node, p.i - 1, nil
}

func (p *parser) oracleExpr(openTok int) (S, int, error) {
	params, err := p.params()
	if err != nil {
		return nil, 0, err
	}
	out, err := p.optionalArrowType("expected output type after '->'")
	if err != nil {
		return nil, 0, err
	}
	var src any
	if p.match(FROM) {
		if err := p.needExprAfter(p.prev(), "expected expression after 'from'"); err != nil {
			return nil, 0, err
		}
		ex, err := p.expr(0)
		if err != nil {
			return nil, 0, err
		}
		src = ex
	} else {
		src = p.mk("array", -1, -1) // build only when needed
	}
	body := p.mk("oracle", openTok, p.i-1, params, out, src)
	return body, p.i - 1, nil
}

// ─────────────────────── declaration patterns (let/for) ───────────────────

type preAnn struct {
	txt    string
	tokIdx int
}

// takeOnePreAnnotation: in the simplified model, any ANNOTATION immediately
// before a pattern is considered PRE and consumed. Allows stacking.
func (p *parser) takeOnePreAnnotation() (preAnn, bool, error) {
	if p.atEnd() || p.peek().Type != ANNOTATION {
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
		// Collect and merge any annotations adjacent to the completed pattern.
		child := p.mkLeaf("str", ann.tokIdx, ann.txt) // PRE child ("str", pre)
		sub, err := p.declPattern()
		if err != nil {
			return nil, err
		}
		if tail := p.collectAnnots(); tail != "" {
			sub = p.attachAnnot(sub, tail)
		}
		node := p.mk("annot", ann.tokIdx, p.lastSpanEndTok, child, sub)
		return node, nil
	}

	if p.match(ID) {
		idIdx := p.i - 1
		return p.mk("decl", idIdx, idIdx, tokText(p.prev())), nil
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

func (p *parser) arrayDeclPattern() (S, error) {
	openTok := p.i - 1
	p.skipNoops()
	if p.match(RSQUARE) {
		return p.mk("darr", openTok, p.i-1), nil
	}

	parts, err := p.parseCommaList(
		func(tt TokenType) bool { return tt == RSQUARE },
		func() (S, int, error) {
			p.skipNoops()
			pt, err := p.declPattern()
			if err != nil {
				return nil, 0, err
			}
			return pt, 0, nil
		},
	)
	if err != nil {
		return nil, err
	}
	p.skipNoops()
	if _, perr := p.need(RSQUARE, "expected ']' in array pattern"); perr != nil {
		return nil, perr
	}
	return p.mk("darr", openTok, p.i-1, parts...), nil
}

func (p *parser) objectDeclPattern() (S, error) {
	openTok := p.i - 1
	p.skipNoops()
	if p.match(RCURLY) {
		return p.mk("dobj", openTok, p.i-1), nil
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
		pt, err := p.declPattern()
		if err != nil {
			return nil, 0, err
		}
		return pt, 0, nil
	}

	pairs, err := p.parseKVPairs(isClose, readKey, parseVal)
	if err != nil {
		return nil, err
	}

	p.skipNoops()
	if _, perr := p.need(RCURLY, "expected '}' in object pattern"); perr != nil {
		return nil, perr
	}
	return p.mk("dobj", openTok, p.i-1, pairs...), nil
}

// readKeyString allows stacked PRE-annotations (handled recursively).
// Span order: (1) PRE "str" child; (2) "annot" wrapper; (3) final key "str" leaf.
func (p *parser) readKeyString() (S, error) {
	if ann, ok, err := p.takeOnePreAnnotation(); err != nil {
		return nil, err
	} else if ok {
		child := p.mkLeaf("str", ann.tokIdx, ann.txt)
		k, err := p.readKeyString()
		if err != nil {
			return nil, err
		}
		return p.mk("annot", ann.tokIdx, p.lastSpanEndTok, child, k), nil
	}

	if p.match(STRING) {
		return p.mkLeaf("str", p.i-1, p.prev().Literal), nil
	}

	t := p.peek()
	if isWordLike(t.Type) {
		p.i++
		name := t.Lexeme
		if s, ok := t.Literal.(string); ok {
			name = s
		}
		return p.mkLeaf("str", p.i-1, name), nil
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
		// Reuse the existing id span for the decl node range.
		return p.mk("decl", p.lastSpanStartTok, p.lastSpanEndTok, e[1].(string)), nil
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
