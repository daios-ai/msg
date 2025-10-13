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
	// Never POST onto a NOOP (blank-line statement).
	if p.toks[endTok].Type == NOOP {
		return e
	}
	var parts []string
	for !p.atEnd() && p.peek().Type == ANNOTATION {
		// Only single-line annotations may be POST.
		s, ok := p.peek().Literal.(string)
		if !ok || s == "" || strings.Contains(s, "\n") {
			break
		}
		// Must be truly same line: no newline between previous node end and annot start.
		if strings.Contains(p.src[p.toks[endTok].EndByte:p.peek().StartByte], "\n") {
			break
		}
		parts = append(parts, s)
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

// ---- tiny shared helpers for delimited lists ----

// Consume gap tokens between elements/pairs.
// - Merges ANNOTATIONs into pending PRE text.
// - Emits NOOPs (and annot(NOOP) if PRE was pending) into out.
// Returns true if it consumed something (caller should continue the loop).
func (p *parser) takeGap(pending *string, out *[]any) bool {
	switch p.peek().Type {
	case ANNOTATION:
		*pending = joinNonEmpty(*pending, p.collectAnnots())
		return true
	case NOOP:
		if *pending != "" {
			child := p.mkLeaf("str", -1, *pending)
			*out = append(*out, p.mk("annot", -1, -1, child, p.mkLeaf("noop", -1)))
			*pending = ""
			p.i++ // consume the NOOP we wrapped
			return true
		}
		*out = append(*out, p.mkLeaf("noop", p.i))
		p.i++
		return true
	default:
		return false
	}
}

// Collect same-line annotations as a merged string (without attaching).
// Useful for strict A→B→C→D ordering in maps; pass the reference line.
func (p *parser) takeSameLineAnnots(line int) string {
	if p.atEnd() {
		return ""
	}
	if p.peek().Type != ANNOTATION || p.peek().Line != line {
		return ""
	}
	// Consume exactly one same-line annotation token.
	tok := p.peek()
	p.i++
	if s, ok := tok.Literal.(string); ok && s != "" {
		return s
	}
	return ""
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

			// Standalone comment: after skipping NOOPs, if the next token cannot start a value,
			// synthesize annot(noop) without consuming any real NOOPs.
			{
				j := p.i
				for j < len(p.toks) && p.toks[j].Type == NOOP {
					j++
				}
				next := EOF
				if j < len(p.toks) {
					next = p.toks[j].Type
				}
				switch next {
				case EOF, END, ELSE, ELIF, THEN, RROUND, RSQUARE, RCURLY:
					child := p.mkLeaf("str", tokIndexOfThis, txt)
					return p.mk("annot", tokIndexOfThis, tokIndexOfThis, child, p.mkLeaf("noop", -1)), nil
				}
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
			// B (after '=') — do NOT skip NOOPs; blank line breaks adjacency
			bTxt := p.collectAnnots()
			// C (unwrap from RHS)
			baseRight, cTxt, _ := unwrapAnnot(rightParsed)
			// D (after RHS and optional trailing comma in lists) — do NOT skip NOOPs
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
		// IMPORTANT: do not skip NOOPs here; gaps inside argument lists are first-class.
		p.i++
		if p.match(RROUND) {
			n := p.mk("call", leftStartTok, p.i-1, left)
			return n, true, nil
		}
		// Let the bracketed list own gaps/NOOPs/annotations.
		args, _, closeTok, err := p.bracketed(RROUND, func() (S, int, error) {
			// Do NOT skip NOOPs here — the comma-list will handle interstitials.
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
		// For index expressions, NOOPs inside brackets are ignored by design.
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

	// Empty *only* if the very next token is the closer — no gap skipping.
	if p.match(close) {
		return nil, openTok, p.i - 1, nil
	}

	// Parse with the uniform comma-list rules (gaps → NOOP/annot, POST same-line only).
	elems, err := p.parseCommaList(
		func(tt TokenType) bool { return tt == close },
		parseElem,
	)
	if err != nil {
		return nil, 0, 0, err
	}

	// Do not skip NOOPs before the closer; dangling PRE must synthesize NOOP.
	if _, err := p.need(close, "expected ')'"); err != nil {
		return nil, 0, 0, err
	}
	return elems, openTok, p.i - 1, nil
}

// parseCommaList parses items until isClose(peek.Type) is true.
// Rules: NOOPs are elements; PRE attaches to next element unless a NOOP appears
// (then PRE→annot(NOOP)); POST is same-line only (element end or comma);
// dangling PRE before close → annot(NOOP).
func (p *parser) parseCommaList(
	isClose func(TokenType) bool,
	parseElem func() (S, int, error),
) ([]any, error) {
	var out []any
	pending := ""

	for {
		// Close (emit dangling PRE as annot(NOOP))
		if isClose(p.peek().Type) {
			if pending != "" {
				child := p.mkLeaf("str", -1, pending)
				out = append(out, p.mk("annot", -1, -1, child, p.mkLeaf("noop", -1)))
				pending = ""
			}
			break
		}

		// Interstitial gap
		if p.takeGap(&pending, &out) {
			continue
		}

		// Element
		elem, _, err := parseElem()
		if err != nil {
			return nil, err
		}

		// POST right after element (same line only)
		endTok := p.lastSpanEndTok
		if endTok >= 0 {
			if t := p.takeSameLineAnnots(p.toks[endTok].Line); t != "" {
				elem = p.attachAnnot(elem, t)
			}
		}

		// Pending PRE (only if no NOOP before this element)
		if pending != "" {
			elem = p.attachAnnot(elem, pending)
			pending = ""
		}
		out = append(out, elem)

		// Optional comma; POST-after-comma binds back only if same line as comma
		if !p.match(COMMA) {
			break
		}
		comma := p.prev()
		if txt := p.takeSameLineAnnots(comma.Line); txt != "" {
			last := out[len(out)-1].(S)
			out[len(out)-1] = p.attachAnnot(last, txt)
		}
		// Next-line annotations (if any) will be handled as PRE via takeGap.
	}
	return out, nil
}

func (p *parser) arrayLiteralAfterOpen() (S, error) {
	// Empty array only when immediate ']' follows.
	if p.match(RSQUARE) {
		return p.mk("array", p.i-2, p.i-1 /*'[]'*/), nil
	}

	elems, err := p.parseCommaList(
		func(tt TokenType) bool { return tt == RSQUARE },
		func() (S, int, error) {
			// IMPORTANT: no skipNoops here; list gaps are handled by parseCommaList.
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

	// Do not skip NOOPs; allow dangling PRE before ']' to synthesize NOOP.
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

// params parses (CLROUND ... RROUND) parameter pairs; preserves NOOPs and
// PRE/POST rules uniformly across entries, while *ignoring* NOOPs that appear
// between ':' and the type expression (so annotations there still bind to the type).
// Critically, it NORMALIZES annotations at the binding site by merging pending/A/B/C/D
// onto the VALUE inside each ("pair", name, value), never onto the pair node itself.
func (p *parser) params() (S, error) {
	if _, perr := p.need(CLROUND, "expected '(' to start parameters"); perr != nil {
		return nil, perr
	}
	openTok := p.i - 1

	// Immediate close → empty params array
	if p.match(RROUND) {
		return p.mk("array", openTok, p.i-1), nil
	}

	var entries []any
	pending := "" // PRE text accumulated from interstitial gaps to apply to next param's VALUE

	for {
		// If we see ')', close list — but first synthesize dangling PRE → annot(noop)
		if p.peek().Type == RROUND {
			if pending != "" {
				child := p.mkLeaf("str", -1, pending)
				entries = append(entries, p.mk("annot", -1, -1, child, p.mkLeaf("noop", -1)))
				pending = ""
			}
			break
		}

		// Guard: if the next token cannot start a parameter element,
		// the parameter list wasn't properly closed. Emit the canonical
		// "expected ')' after parameters" at the unexpected token.
		switch p.peek().Type {
		case ANNOTATION, NOOP, RROUND:
			// handled by branches above
		case ID:
			// valid start of a parameter; continue to parse it
		case EOF:
			// Delegate to need(...) so interactive mode yields DiagIncomplete.
			if _, err := p.need(RROUND, "expected ')' after parameters"); err != nil {
				return nil, err
			}
			// Unreachable: need() always returns an error here.
		default:
			g := p.peek()
			line, col := p.posAtByte(g.StartByte)
			return nil, &Error{Kind: DiagParse, Msg: "expected ')' after parameters", Line: line, Col: col}
		}

		// Inter-entry gap handling:
		switch p.peek().Type {
		case ANNOTATION:
			// PRE stacks in pending; do NOT skip NOOPs here — a NOOP would break PRE and be emitted.
			pending = joinNonEmpty(pending, p.collectAnnots())
			continue
		case NOOP:
			// If we had pending PRE, it attaches to a NOOP first (annot(noop)); then emit the NOOP itself.
			if pending != "" {
				child := p.mkLeaf("str", -1, pending)
				entries = append(entries, p.mk("annot", -1, -1, child, p.mkLeaf("noop", -1)))
				pending = ""
				// consume the NOOP we just wrapped
				p.i++
				continue
			}
			entries = append(entries, p.mkLeaf("noop", p.i))
			p.i++
			continue
		}

		// --- Parse one parameter element ---
		elemStartTok := p.i

		// Name
		idTok, err := p.need(ID, "expected parameter name")
		if err != nil {
			return nil, err
		}
		idIdx := p.i - 1
		nameLeaf := p.mkLeaf("id", idIdx, tokText(idTok))

		// Optional ':' type
		var val S
		if p.match(COLON) {
			// B: annotations immediately after ':' (do not cross a NOOP boundary).
			// We ignore NOOPs *between* ':' and the type — see tests expecting that.
			for !p.atEnd() && p.peek().Type == NOOP {
				p.i++ // ignore NOOPs local to the type site
			}
			bTxt := p.collectAnnots()

			// Type expression
			if err := p.needExprAfter(p.prev(), "expected type after ':'"); err != nil {
				return nil, err
			}
			tExpr, err := p.expr(0)
			if err != nil {
				return nil, err
			}

			// C: unwrap PRE already on the parsed type
			baseT, cTxt, _ := unwrapAnnot(tExpr)

			// D: same-line POST after the type (before optional comma) attaches back
			dTxt := ""
			if p.lastSpanEndTok >= 0 {
				dTxt = p.takeSameLineAnnots(p.toks[p.lastSpanEndTok].Line)
			}

			// Merge pending (PRE from gap) + B + C + D onto VALUE
			val = p.attachAnnot(baseT, joinNonEmpty(pending, bTxt, cTxt, dTxt))
			pending = ""
		} else {
			// No explicit type → implicit Any; merge pending onto it.
			base := p.mkLeaf("id", -1, "Any")
			val = p.attachAnnot(base, pending)
			pending = ""
		}

		pair := p.mk("pair", elemStartTok, p.i-1, nameLeaf, val)
		entries = append(entries, pair)

		// Optional comma; POST-after-comma attaches back ONLY if same line as the comma.
		if p.match(COMMA) {
			comma := p.prev()
			if txt := p.takeSameLineAnnots(comma.Line); txt != "" {
				// Attach to this pair's VALUE (normalization at binding site!)
				last := entries[len(entries)-1].(S)
				if len(last) >= 3 {
					v := last[2].(S)
					last[2] = p.attachAnnot(v, txt)
					entries[len(entries)-1] = last
				}
			}
			// allow trailing comma — loop continues until we hit ')'
			continue
		}

		// No comma; next token should be ')' or gap (handled at top of loop).
	}

	// Final closer
	if _, perr := p.need(RROUND, "expected ')' after parameters"); perr != nil {
		return nil, perr
	}
	return p.mk("array", openTok, p.i-1, entries...), nil
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

// parseKVPairs honors the same adjacency rules as parseCommaList.
// Interstitial ANNOTATION/NOOP between pairs are emitted (PRE→next value unless broken by NOOP).
// POST-after-value and POST-after-comma are same-line only.
// Dangling PRE before '}' → annot(NOOP).
func (p *parser) parseKVPairs(
	isClose func(TokenType) bool,
	readKey func() (key S, keyStartTok int, required bool, err error),
	parseValue func() (val S, valStartTok int, err error),
) ([]any, error) {
	var pairs []any
	pending := ""

	for {
		// Close (emit dangling PRE)
		if isClose(p.peek().Type) {
			if pending != "" {
				child := p.mkLeaf("str", -1, pending)
				pairs = append(pairs, p.mk("annot", -1, -1, child, p.mkLeaf("noop", -1)))
				pending = ""
			}
			break
		}

		// Interstitial gap
		if p.takeGap(&pending, &pairs) {
			continue
		}

		// Key
		startTok := p.i
		k, _, req, err := readKey()
		if err != nil {
			return nil, err
		}
		baseKey, aTxt, _ := unwrapAnnot(k) // A
		k = baseKey

		if _, err := p.need(COLON, "expected ':' after key"); err != nil {
			return nil, err
		}

		// B (after ':', no NOOP skipping)
		bTxt := p.collectAnnots()

		// Value
		v, _, err := parseValue()
		if err != nil {
			return nil, err
		}
		baseVal, cTxt, _ := unwrapAnnot(v) // C

		// D = same-line POST after value (before optional comma)
		dTxt := ""
		if p.lastSpanEndTok >= 0 {
			dTxt = p.takeSameLineAnnots(p.toks[p.lastSpanEndTok].Line)
		}

		// Merge pending PRE + A + B + C + D
		val := p.attachAnnot(baseVal, joinNonEmpty(pending, aTxt, bTxt, cTxt, dTxt))
		pending = ""

		tag := "pair"
		if req {
			tag = "pair!"
		}
		pr := p.mk(tag, startTok, p.i-1, k, val)
		pairs = append(pairs, pr)

		// Optional comma; POST-after-comma same-line only, attaches back to this pair's value
		if p.match(COMMA) {
			comma := p.prev()
			if txt := p.takeSameLineAnnots(comma.Line); txt != "" {
				last := pairs[len(pairs)-1].(S)
				if len(last) >= 3 {
					v := last[2].(S)
					last[2] = p.attachAnnot(v, txt)
					pairs[len(pairs)-1] = last
				}
			}
			// Next-line annotations become PRE via the next loop (takeGap).
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
