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
//	("let",   patternExpr)        // "let P" declaration (pattern in expr form)
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
// Annotations:
//
//	("annot", ("str", text), wrappedNode)   // merged text; no POST marker
//
// ─────────────────────────────────────────────────────────────────────────────
// SPAN EMISSION INVARIANT (CRITICAL)
// ----------------------------------
// **This file now centralizes AST construction and span emission.**
//
//   - Every AST node is constructed through IR first (no spans during parse).
//   - After parsing, we materialize the AST and emit spans in strict **post-order**
//     (children first, then parent), left-to-right among siblings.
//   - Wrapper nodes we create (e.g. "annot" from PRE/POST) obey the same rule.
//   - Nodes that are synthesized with no concrete tokens (e.g. default type
//     `Any`) still receive a placeholder `Span{}` (tok=-1).
//   - The root block’s span is appended last.
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

// IR (Intermediate Representation) — built during parsing without emitting spans.

// ---------------- IR TYPES ---------------------------------------------------
type NodeID int

type IRNode struct {
	Tag      string
	Kids     []NodeID
	Value    any
	TokStart int // inclusive token index, -1 if synthetic
	TokEnd   int // inclusive token index, -1 if synthetic
}

type IRArena struct {
	Nodes []IRNode
	Root  NodeID
}

// materializePostOrder converts IR into the public AST (S-expr) and a slice of
// spans in strict post-order (children first, then parent).
func materializePostOrder(toks []Token, ir IRArena) (S, []Span) {
	spans := make([]Span, 0, len(ir.Nodes))

	var build func(NodeID) S
	build = func(id NodeID) S {
		n := ir.Nodes[id]

		// Build children first (post-order span emission later).
		childNodes := make([]S, len(n.Kids))
		for i, k := range n.Kids {
			childNodes[i] = build(k)
		}

		// Convert []S -> []any for L(...).
		parts := make([]any, 0, len(childNodes)+1)
		if n.Value != nil {
			parts = append(parts, n.Value)
		}
		for _, c := range childNodes {
			parts = append(parts, c)
		}

		node := L(n.Tag, parts...)

		// Append this node’s span (post-order).
		span := Span{}
		if n.TokStart >= 0 && n.TokEnd >= n.TokStart &&
			n.TokStart < len(toks) && n.TokEnd < len(toks) {
			span.StartByte = toks[n.TokStart].StartByte
			span.EndByte = toks[n.TokEnd].EndByte
		}
		spans = append(spans, span)
		return node
	}

	ast := build(ir.Root)
	return ast, spans
}

// ParseSExpr parses a complete MindScript source string and returns its AST.
func ParseSExpr(src string) (S, error) {
	lex := NewLexer(src)
	toks, err := lex.Scan()
	if err != nil {
		return nil, err
	}
	p := &parser{toks: toks, src: src, lastSpanStartTok: -1, lastSpanEndTok: -1}
	root, err := p.programIR()
	if err != nil {
		return nil, err
	}
	p.ir.Root = root
	ast, _ := materializePostOrder(p.toks, p.ir)
	return ast, nil
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
	root, perr := p.programIR()
	if perr != nil {
		return nil, nil, perr
	}
	p.ir.Root = root
	ast, spans := materializePostOrder(p.toks, p.ir)
	idx := BuildSpanIndexPostOrder(ast, spans)
	return ast, idx, nil
}

// ParseSExprInteractiveWithSpans parses in REPL-friendly mode and returns the AST plus
// a SpanIndex with post-order node spans. Unterminated constructs at EOF
// produce *Error{Kind:DiagIncomplete}.
func ParseSExprInteractiveWithSpans(src string) (S, *SpanIndex, error) {
	lex := NewLexerInteractive(src)
	toks, err := lex.Scan()
	if err != nil {
		return nil, nil, err
	}
	p := &parser{
		toks: toks, src: src,
		interactive:      true,
		lastSpanStartTok: -1, lastSpanEndTok: -1,
	}
	root, perr := p.programIR()
	if perr != nil {
		return nil, nil, perr
	}
	p.ir.Root = root
	ast, spans := materializePostOrder(p.toks, p.ir)
	idx := BuildSpanIndexPostOrder(ast, spans)
	return ast, idx, nil
}

//// END_OF_PUBLIC

////////////////////////////////////////////////////////////////////////////////
///////////////////////////// PRIVATE IMPLEMENTATION ///////////////////////////
////////////////////////////////////////////////////////////////////////////////

type parser struct {
	toks        []Token
	i           int
	interactive bool
	// (no span emission during parse)
	lastSpanStartTok int
	lastSpanEndTok   int
	src              string
	// IR arena (nodes + eventual root)
	ir IRArena
}

// New small shared helpers (each used ≥3 times):
// - parseSingleBetween: "( expr )" / "[ expr ]" single element with PRE + trailing gaps + need(close).
// - parseDoBlockWithLeadingGap: common "gap before `do` attaches to body".
// - parseExprAfterBP: require expression after an anchor token with PRE attachment, caller chooses BP.
// - onCommaPostAttachToValue: POST-after-comma attaches to VALUE inside a ("pair", key, value).
//
// All helpers reuse existing machinery: takeReqGap, collectGap, need, attachAnnotFrom, bracketed, etc.

// ─────────────────────── centralized annotation carrier ─────────────────────
type annSrc struct {
	txt      string
	startTok int // inclusive token index of the first contributing annotation token
	endTok   int // inclusive token index of the last contributing annotation token
	ok       bool
}

func annNone() annSrc { return annSrc{} }

func annFromSingle(tokIdx int, txt string) annSrc {
	return annSrc{txt: txt, startTok: tokIdx, endTok: tokIdx, ok: txt != ""}
}

func mergeAnn(a, b annSrc) annSrc {
	if !a.ok && !b.ok {
		return annSrc{}
	}
	if !a.ok {
		return b
	}
	if !b.ok {
		return a
	}
	txt := a.txt
	if b.txt != "" {
		if txt == "" {
			txt = b.txt
		} else {
			txt = txt + "\n" + b.txt
		}
	}
	// Compute first and last contributing token indexes, ignoring unknown (-1).
	start := a.startTok
	if start < 0 || (b.startTok >= 0 && b.startTok < start) {
		start = b.startTok
	}
	end := a.endTok
	if end < 0 || (b.endTok >= 0 && b.endTok > end) {
		end = b.endTok
	}
	return annSrc{
		txt: txt, startTok: start, endTok: end, ok: txt != "",
	}
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

// takeReqGap consumes immediately-adjacent GAP (ANNOTATION/NOOP) that appear
// before something *required* after `anchor`. It emits these gaps (returned as
// annSrc) but NEVER lets them satisfy the requirement.
// If only gaps remain to EOF, it returns an error anchored to `anchor`:
//   - interactive: DiagIncomplete(msg)
//   - noninteractive: DiagParse(msg)
func (p *parser) takeReqGap(anchor Token, incMsg string) (annSrc, error) {
	// Coalesce sequential annotations first, then eat blank-line runs.
	pre := p.collectAnnotsSrc()
	p.skipNoops()
	// If after consuming those we only have gaps to EOF, this is an incomplete requirement.
	if p.onlyGapsToEOF() {
		line, col := p.posAtByte(anchor.StartByte)
		kind := DiagParse
		if p.interactive {
			kind = DiagIncomplete
		}
		return annNone(), &Error{
			Kind: kind,
			Msg:  incMsg,
			Line: line, Col: col,
		}
	}
	return pre, nil
}

// needNoGap enforces the "GAPLESS after dot" rule: immediately after `anchor`
// (which should be a PERIOD token) there must not be ANNOTATION/NOOP.
// If a GAP is present:
//   - interactive + only gaps to EOF → DiagIncomplete anchored to `anchor`;
//   - otherwise → hard parse error anchored at the first GAP token.
//
// It does NOT consume any token.
func (p *parser) needNoGap(anchor Token, msg string) error {
	if p.atEnd() {
		// Unify message across modes: same text, different kind.
		line, col := p.posAtByte(anchor.StartByte)
		kind := DiagParse
		if p.interactive {
			kind = DiagIncomplete
		}
		return &Error{Kind: kind, Msg: msg, Line: line, Col: col}
	}
	switch p.peek().Type {
	case ANNOTATION, NOOP:
		if p.interactive && p.onlyGapsToEOF() {
			line, col := p.posAtByte(anchor.StartByte)
			return &Error{Kind: DiagIncomplete, Msg: msg, Line: line, Col: col}
		}
		line, col := p.posAtByte(p.peek().StartByte)
		return &Error{Kind: DiagParse, Msg: msg, Line: line, Col: col}
	}
	return nil
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
	case POW:
		return 80, true
	case ARROW:
		return 15, true
	case MULT, DIV, MOD:
		return 70, true
	case PLUS, MINUS:
		return 60, true
	case LSHIFT, RSHIFT:
		return 55, true
	case LESS, LESS_EQ, GREATER, GREATER_EQ:
		return 50, true
	case EQ, NEQ:
		return 40, true
	case BITAND:
		return 35, true
	case BITXOR:
		return 34, true
	case BITOR:
		return 33, true
	case AND:
		return 30, true
	case OR:
		return 20, true
	case ASSIGN:
		return 10, true
	}
	return 0, false
}
func isRightAssoc(tt TokenType) bool {
	// Assignment and arrow are right-associative; exponentiation is also commonly right-assoc.
	return tt == ASSIGN || tt == ARROW || tt == POW
}

// ───────────────────────────── IR node builders ─────────────────────────

func (p *parser) mkLeafIR(tag string, tok int, parts ...any) NodeID {
	id := NodeID(len(p.ir.Nodes))
	var val any
	if len(parts) > 0 {
		val = parts[0]
	}
	p.ir.Nodes = append(p.ir.Nodes, IRNode{
		Tag: tag, Kids: nil, Value: val,
		TokStart: tok, TokEnd: tok,
	})
	// Do not let synthetic tokens clobber anchors used for diagnostics.
	if tok >= 0 {
		p.lastSpanStartTok, p.lastSpanEndTok = tok, tok
	}
	return id
}

func (p *parser) mkIR(tag string, startTok, endTok int, kids ...NodeID) NodeID {
	id := NodeID(len(p.ir.Nodes))
	cp := make([]NodeID, len(kids))
	copy(cp, kids)
	p.ir.Nodes = append(p.ir.Nodes, IRNode{
		Tag: tag, Kids: cp,
		TokStart: startTok, TokEnd: endTok,
	})
	// Preserve last real-token anchor; ignore synthetic (-1).
	if startTok >= 0 && endTok >= 0 {
		p.lastSpanStartTok, p.lastSpanEndTok = startTok, endTok
	}
	return id
}

func (p *parser) mkIRVal(tag string, startTok, endTok int, value any, kids ...NodeID) NodeID {
	id := NodeID(len(p.ir.Nodes))
	cp := make([]NodeID, len(kids))
	copy(cp, kids)
	p.ir.Nodes = append(p.ir.Nodes, IRNode{
		Tag: tag, Kids: cp, Value: value,
		TokStart: startTok, TokEnd: endTok,
	})
	// Preserve last real-token anchor; ignore synthetic (-1).
	if startTok >= 0 && endTok >= 0 {
		p.lastSpanStartTok, p.lastSpanEndTok = startTok, endTok
	}
	return id
}

// ───────────────────────── NOOP / annotation utils ─────────────────────

// onlyGapsToEOF reports whether from the current lookahead to EOF there are
// no real tokens (only NOOP and/or ANNOTATION). This is the canonical
// "need more input" detector for interactive mode.
func (p *parser) onlyGapsToEOF() bool {
	j := p.i
	for j < len(p.toks) {
		switch p.toks[j].Type {
		case NOOP, ANNOTATION:
			j++
			continue
		case EOF:
			return true
		default:
			return false
		}
	}
	return true
}

// ───────────────────────────── shared mini-helpers ───────────────────────────

// opener already consumed. Parse exactly one expr between opener/closer,
// attach PRE (after opener) and trailing gaps (before closer), then need(closer).
// Returns the inner expression and the closer's token index.
func (p *parser) parseSingleBetween(closer TokenType, expect string) (NodeID, int, error) {
	openTok := p.toks[p.i-1]
	needMsg := "expected expression after '('"
	if closer == RSQUARE {
		needMsg = "expected index expression after '['"
	}
	pre, err := p.takeReqGap(openTok, needMsg)
	if err != nil {
		return 0, 0, err
	}
	inner, err := p.expr(0)
	if err != nil {
		return 0, 0, err
	}
	if pre.ok {
		inner = p.attachAnnotFrom(pre, inner)
	}
	if tail := p.collectGap(); tail.ok {
		inner = p.attachAnnotFrom(tail, inner)
	}
	if _, err := p.need(closer, expect); err != nil {
		return 0, 0, err
	}
	return inner, p.i - 1, nil
}

// Common "gap before do" handling for fun/oracle/module/loops.
func (p *parser) parseDoBlockWithLeadingGap() (NodeID, error) {
	pre := p.collectGap()
	b, err := p.parseBlock(true)
	if err != nil {
		return 0, err
	}
	if pre.ok {
		b = p.attachAnnotFrom(pre, b)
	}
	return b, nil
}

// Require an expression after a just-consumed anchor token, with configurable BP,
// attaching PRE to that expression.
func (p *parser) parseExprAfterBP(anchor Token, msg string, bp int) (NodeID, error) {
	pre, err := p.takeReqGap(anchor, msg)
	if err != nil {
		return 0, err
	}
	e, err := p.expr(bp)
	if err != nil {
		return 0, err
	}
	if pre.ok {
		e = p.attachAnnotFrom(pre, e)
	}
	return e, nil
}

// POST-after-comma attaches to VALUE inside ("pair", key, value)
func (p *parser) onCommaPostAttachToValue(last NodeID, comma Token) NodeID {
	n := p.ir.Nodes[last]
	switch n.Tag {
	case "pair", "pair!":
		// Re-attach POST to the value child, not the pair wrapper.
		if len(n.Kids) < 2 {
			// Defensive: malformed pair; fall back to prior behavior.
			return p.attachSameLineAnnots(last, comma.Line)
		}
		key := n.Kids[0]
		val := n.Kids[1]
		val = p.attachSameLineAnnots(val, comma.Line)
		// Rebuild the pair node with the updated value; preserve original span.
		return p.mkIR(n.Tag, n.TokStart, n.TokEnd, key, val)
	default:
		// Arrays or other lists: attach to the element itself.
		return p.attachSameLineAnnots(last, comma.Line)
	}
}

func (p *parser) nextTokenIsOnSameLine(as Token) bool {
	if p.atEnd() {
		return false
	}
	return p.peek().Line == as.Line
}

// Collect only *immediately adjacent* ANNOTATION tokens into a single source-carrying value.
// Do NOT skip NOOPs here; a blank-line run must break annotation groups.
func (p *parser) collectAnnotsSrc() annSrc {
	var out annSrc
	for !p.atEnd() && p.peek().Type == ANNOTATION {
		tok := p.peek()
		p.i++
		if s, ok := tok.Literal.(string); ok && s != "" {
			out = mergeAnn(out, annFromSingle(p.i-1, s))
		}
	}
	return out
}

// collectGap coalesces a "gap" at a join: it first consumes immediately-adjacent
// ANNOTATION tokens into a single annSrc and then skips any NOOPs (blank-line runs).
// Use this *before* requiring a keyword/delimiter (then/do/in/from/else/elif, closers, etc.).
// If callers then call need(...), EOF after this consumption will naturally become
// DiagIncomplete in interactive mode via need(...).
func (p *parser) collectGap() annSrc {
	a := p.collectAnnotsSrc()
	p.skipNoops()
	return a
}

// Attach one or more *single-line* ANNOTATION tokens that start on refLine.
func (p *parser) attachSameLineAnnots(n NodeID, refLine int) NodeID {
	if p.atEnd() || p.peek().Type != ANNOTATION {
		return n
	}
	var acc annSrc
	for !p.atEnd() && p.peek().Type == ANNOTATION && p.peek().Line == refLine {
		tok := p.peek()
		if s, ok := tok.Literal.(string); ok && s != "" && !strings.Contains(s, "\n") {
			acc = mergeAnn(acc, annFromSingle(p.i, s))
			p.i++ // consume
			continue
		}
		break
	}
	if !acc.ok {
		return n
	}
	return p.attachAnnotFrom(acc, n)
}

// IR-safe annotation attach: build ("annot", ("str", txt), base) in IR.
//   - child "str" spans = [src.startTok, src.endTok] (fallback to base span).
//   - Wrapper inherits base span.
func (p *parser) attachAnnotFrom(src annSrc, val NodeID) NodeID {
	if !src.ok || src.txt == "" {
		return val
	}

	bn := p.ir.Nodes[val]
	startTok := src.startTok
	endTok := src.endTok
	if startTok < 0 {
		startTok = bn.TokStart
	}
	if endTok < 0 {
		endTok = bn.TokEnd
	}
	str := p.mkLeafIR("str", startTok, src.txt)
	p.ir.Nodes[str].TokEnd = endTok

	return p.mkIR("annot", bn.TokStart, bn.TokEnd, str, val)
}

// ───────────────────────── program / blocks ────────────────────────────

func (p *parser) programIR() (NodeID, error) {
	var kids []NodeID
	for !p.atEnd() {
		e, err := p.expr(0)
		if err != nil {
			return 0, err
		}
		kids = append(kids, e)
	}
	if len(p.toks) <= 1 {
		return p.mkIR("block", -1, -1 /*empty*/), nil
	}
	return p.mkIR("block", 0, len(p.toks)-2, kids...), nil
}

// blockUntil parses statements until a stop token is seen.
func (p *parser) blockUntil(stops ...TokenType) (NodeID, error) {
	stop := map[TokenType]bool{}
	for _, s := range stops {
		stop[s] = true
	}
	var items []NodeID
	startTok := p.i
	consumedAny := false

	for !p.atEnd() && !stop[p.peek().Type] {
		e, err := p.expr(0)
		if err != nil {
			return 0, err
		}
		items = append(items, e)
		consumedAny = true
	}
	if consumedAny {
		return p.mkIR("block", startTok, p.i-1, items...), nil
	}
	return p.mkIR("block", -1, -1 /*empty*/), nil
}

func (p *parser) parseBlock(requireDo bool) (NodeID, error) {
	if requireDo {
		if _, err := p.need(DO, "expected 'do'"); err != nil {
			return 0, err
		}
	}
	b, err := p.blockUntil(END)
	if err != nil {
		return 0, err
	}
	if _, err := p.need(END, "expected 'end'"); err != nil {
		return 0, err
	}
	return b, nil
}

// ───────────────────────────── tiny node helpers ───────────────────────────

func (p *parser) tryLiteralOrId(t Token, start int) (NodeID, bool) {
	switch t.Type {
	case ID, TYPE:
		return p.mkLeafIR("id", start, tokText(t)), true
	case INTEGER:
		return p.mkLeafIR("int", start, t.Literal), true
	case NUMBER:
		return p.mkLeafIR("num", start, t.Literal), true
	case STRING:
		return p.mkLeafIR("str", start, t.Literal), true
	case BOOLEAN:
		return p.mkLeafIR("bool", start, t.Literal), true
	case NULL:
		return p.mkLeafIR("null", start), true
	}
	return 0, false
}

// ---- tiny shared helpers for delimited lists ----

// drainTrailingGapsUntil consumes any sequence of NOOPs and/or ANNOTATIONs
// until the given closing token is seen. Each NOOP becomes ("noop") and each
// annotation becomes ("annot", ("str", mergedText), ("noop")).
//
// It never crosses non-gap tokens. If a non-gap appears before the closer,
// it delegates to need(close, expectMsg) to produce the right diagnostic.
func (p *parser) drainTrailingGapsUntil(close TokenType, expectMsg string, out *[]NodeID) error {
	for {
		if p.atEnd() {
			// Let need(...) decide whether this is parse vs incomplete, with correct anchoring.
			if _, err := p.need(close, expectMsg); err != nil {
				return err
			}
			return nil
		}
		switch p.peek().Type {
		case close:
			return nil

		case NOOP:
			*out = append(*out, p.mkLeafIR("noop", p.i))
			p.i++

		case ANNOTATION:
			// Merge adjacent annotations and wrap a noop, preserving the annotation's source span.
			src := p.collectAnnotsSrc()
			*out = append(*out, p.attachAnnotFrom(src, p.mkLeafIR("noop", -1)))

		default:
			// Unexpected token before closer → canonical error.
			if _, err := p.need(close, expectMsg); err != nil {
				return err
			}
			return nil
		}
	}
}

// ───────────────────────────── prefix / postfix / infix ────────────────────

func (p *parser) expr(minBP int) (NodeID, error) {
	tokIndexOfThis := p.i
	t := p.peek()
	p.i++

	var left NodeID
	leftStartTok := tokIndexOfThis

	// ---- prefix ----
	if n, ok := p.tryLiteralOrId(t, tokIndexOfThis); ok {
		left = n
	} else {
		switch t.Type {
		case NOOP:
			left = p.mkLeafIR("noop", tokIndexOfThis)

		case ENUM:
			// Accept gap (annotations/newlines) between 'Enum' and '[' safely.
			saveI := p.i
			enumPre := p.collectGap()
			if p.peek().Type == LSQUARE || p.peek().Type == CLSQUARE {
				p.i++ // consume '[' or CLSQUARE
				arr, err := p.arrayLiteralAfterOpen()
				if err != nil {
					return 0, err
				}
				// Retag: ("array", e1, e2, ...) -> ("enum", e1, e2, ...)
				an := p.ir.Nodes[arr]
				n := p.mkIR("enum", tokIndexOfThis, p.i-1, an.Kids...)
				if enumPre.ok {
					n = p.attachAnnotFrom(enumPre, n)
				}
				left = n
			} else {
				// Not an Enum-literal; restore so gap remains for the next construct.
				p.i = saveI
				left = p.mkLeafIR("id", tokIndexOfThis, tokText(t))
			}

		case MINUS, NOT, BITNOT:
			r, err := p.parseExprAfterBP(t, "expected expression after unary operator", 80)
			if err != nil {
				return 0, err
			}
			endTok := p.lastSpanEndTok
			if endTok < 0 {
				endTok = tokIndexOfThis
			}
			left = p.mkIRVal("unop", tokIndexOfThis, endTok, t.Lexeme, r)

		case LROUND, CLROUND:
			inner, _, err := p.parseSingleBetween(RROUND, "expected ')'")
			if err != nil {
				return 0, err
			}
			left = inner
			leftStartTok = tokIndexOfThis

		case LSQUARE, CLSQUARE:
			a, err := p.arrayLiteralAfterOpen()
			if err != nil {
				return 0, err
			}
			left = a
			leftStartTok = tokIndexOfThis

		case LCURLY:
			mp, err := p.mapLiteralAfterOpen(tokIndexOfThis)
			if err != nil {
				return 0, err
			}
			left = mp
			leftStartTok = tokIndexOfThis

		case FUNCTION:
			fn, err := p.funExpr(tokIndexOfThis)
			if err != nil {
				return 0, err
			}
			left = fn
			leftStartTok = tokIndexOfThis

		case ORACLE:
			orc, err := p.oracleExpr(tokIndexOfThis)
			if err != nil {
				return 0, err
			}
			left = orc
			leftStartTok = tokIndexOfThis

		case MODULE:
			name, err := p.parseExprAfterBP(t, "expected module name expression", 0)
			if err != nil {
				return 0, err
			}
			body, err := p.parseDoBlockWithLeadingGap()
			if err != nil {
				return 0, err
			}
			left = p.mkIR("module", tokIndexOfThis, p.i-1, name, body)
			leftStartTok = tokIndexOfThis

		case RETURN, BREAK, CONTINUE:
			n, err := p.parseControl(t, tokIndexOfThis)
			if err != nil {
				return 0, err
			}
			left = n
			leftStartTok = tokIndexOfThis

		case IF:
			ifnode, err := p.ifExpr()
			if err != nil {
				return 0, err
			}
			in := p.ir.Nodes[ifnode]
			left = p.mkIR("if", tokIndexOfThis, p.i-1, in.Kids...)
			leftStartTok = tokIndexOfThis

		case DO:
			body, err := p.parseBlock(false)
			if err != nil {
				return 0, err
			}
			left = body
			leftStartTok = tokIndexOfThis

		case FOR:
			f, err := p.forExpr(tokIndexOfThis)
			if err != nil {
				return 0, err
			}
			left = f
			leftStartTok = tokIndexOfThis

		case WHILE:
			w, err := p.whileExpr(tokIndexOfThis)
			if err != nil {
				return 0, err
			}
			left = w
			leftStartTok = tokIndexOfThis

		case LET:
			// let P        → ("let", P)
			// let P = E    → ("assign", ("let", P), E) via infix ASSIGN
			pre, err := p.takeReqGap(t, "expected pattern after 'let'")
			if err != nil {
				return 0, err
			}
			const assignBP = 10
			pat, err := p.expr(assignBP + 1)
			if err != nil {
				return 0, err
			}
			if pre.ok {
				pat = p.attachAnnotFrom(pre, pat)
			}
			endTok := p.lastSpanEndTok
			if endTok < 0 {
				endTok = tokIndexOfThis
			}
			left = p.mkIR("let", tokIndexOfThis, endTok, pat)
			leftStartTok = tokIndexOfThis

		case TYPECONS:
			x, err := p.parseExprAfterBP(t, "expected type expression after 'type'", 1)
			if err != nil {
				return 0, err
			}
			left = p.mkIR("type", tokIndexOfThis, p.lastSpanEndTok, x)
			leftStartTok = tokIndexOfThis

		case ANNOTATION:
			// Build a source-carrying annotation payload
			src := annSrc{ok: true, txt: "", startTok: tokIndexOfThis, endTok: tokIndexOfThis}
			if s, ok := t.Literal.(string); ok {
				src.txt = s
			}

			// Floating annotation: if the very next token is a NOOP, consume exactly one
			// NOOP and return annot(noop) immediately.
			if p.match(NOOP) {
				return p.attachAnnotFrom(src, p.mkLeafIR("noop", p.i-1)), nil
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
				case EOF:
					// Interactive + only gaps to EOF ⇒ ask for more input instead of annot(noop).
					if p.interactive && p.onlyGapsToEOF() {
						line, col := p.posAtByte(t.StartByte)
						return 0, &Error{Kind: DiagIncomplete, Msg: "expected expression after annotation", Line: line, Col: col}
					}
					return p.attachAnnotFrom(src, p.mkLeafIR("noop", -1)), nil
				case END, ELSE, ELIF, THEN, RROUND, RSQUARE, RCURLY:
					return p.attachAnnotFrom(src, p.mkLeafIR("noop", -1)), nil
				}
			}

			// Interactive: only gaps after the annotation → incomplete
			if p.interactive && p.onlyGapsToEOF() {
				line, col := p.posAtByte(t.StartByte)
				return 0, &Error{Kind: DiagIncomplete, Msg: "expected expression after annotation", Line: line, Col: col}
			}
			// Non-interactive EOF → treat as annot(noop)
			if p.atEnd() && !p.interactive {
				left = p.attachAnnotFrom(src, p.mkLeafIR("noop", -1))
				leftStartTok = tokIndexOfThis
				break
			}

			// Normal case: parse the annotated operand
			operand, err := p.expr(0)
			if err != nil {
				return 0, err
			}
			left = p.attachAnnotFrom(src, operand)
			return left, nil

		default:
			if t.Type == EOF && p.interactive {
				line, col := p.posAfterLastSpan()
				return 0, &Error{Kind: DiagIncomplete, Msg: "unexpected end of input", Line: line, Col: col}
			}
			line, col := p.posAtByte(t.StartByte)
			return 0, &Error{Kind: DiagParse, Msg: fmt.Sprintf("unexpected token '%s'", t.Lexeme), Line: line, Col: col}
		}
	}

	// ---- postfix chain ----
	for {
		n, ok, err := p.parseOnePostfix(left, leftStartTok)
		if err != nil {
			return 0, err
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

		if op.Type == ASSIGN && !p.assignableIR(left) {
			line, col := p.posAtByte(op.StartByte)
			return 0, &Error{Kind: DiagParse, Msg: "invalid assignment target", Line: line, Col: col}
		}

		// Allow GAP between operator and RHS, but don't let it satisfy the need.
		preOp, err := p.takeReqGap(op, "expected expression after operator")
		if err != nil {
			return 0, err
		}

		rightParsed, err := p.expr(nextBP)
		if err != nil {
			return 0, err
		}
		endTok := p.lastSpanEndTok

		if op.Type == ASSIGN {
			right := rightParsed
			if preOp.ok {
				right = p.attachAnnotFrom(preOp, right)
			}
			if endTok >= 0 && endTok < len(p.toks) {
				right = p.attachSameLineAnnots(right, p.toks[endTok].Line)
			}
			left = p.mkIR("assign", leftStartTok, endTok, left, right)
		} else {
			left = p.mkIRVal("binop", leftStartTok, endTok, op.Lexeme, left, rightParsed)
		}
	}

	// Attach trailing *same-line* annotations (POST) only at the outermost level.
	// This ensures `… * (8 # note)` doesn't consume `# note` inside the RHS,
	// allowing the outer expression to wrap the whole `(4+5)*8`.
	if minBP == 0 {
		if p.lastSpanEndTok >= 0 && p.lastSpanEndTok < len(p.toks) {
			// Never POST onto a NOOP (blank-line statement).
			if p.toks[p.lastSpanEndTok].Type != NOOP {
				left = p.attachSameLineAnnots(left, p.toks[p.lastSpanEndTok].Line)
			}
		}
	}
	return left, nil
}

// ───────────────────────── unified postfix dispatcher ──────────────────────
//
// Handles: QUESTION (optional), CLROUND (call), CLSQUARE (index), PERIOD (dot).
// **Span order** is enforced on IR materialization.

func (p *parser) parseOnePostfix(left NodeID, leftStartTok int) (NodeID, bool, error) {
	// Enforce GAPLESS *before* '.' : if we are sitting on GAPs and the next
	// non-gap token is PERIOD, that is an error (or incomplete at EOF).
	if !p.atEnd() && (p.peek().Type == ANNOTATION || p.peek().Type == NOOP) {
		j := p.i
		firstGap := p.peek()
		for j < len(p.toks) && (p.toks[j].Type == ANNOTATION || p.toks[j].Type == NOOP) {
			j++
		}
		if j < len(p.toks) && p.toks[j].Type == PERIOD {
			// There *is* a dot after a gap → forbidden.
			line, col := p.posAtByte(firstGap.StartByte)
			return 0, false, &Error{Kind: DiagParse, Msg: "no gap allowed before '.'", Line: line, Col: col}
		}
		// Not a dot next; postfix chain stops here.
		return 0, false, nil
	}

	switch p.peek().Type {
	case QUESTION:
		qtok := p.i
		p.i++
		n := p.mkIRVal("unop", leftStartTok, qtok, "?", left)
		return n, true, nil

	case CLROUND:
		p.i++
		if p.match(RROUND) {
			n := p.mkIR("call", leftStartTok, p.i-1, left)
			return n, true, nil
		}
		// Parse argument list
		args, _, closeTok, err := p.bracketed(
			RROUND, "expected ')'",
			func(pending annSrc) (NodeID, error) {
				a, err := p.expr(0)
				if err != nil {
					return 0, err
				}
				if pending.ok {
					a = p.attachAnnotFrom(pending, a)
				}
				return a, nil
			},
			func(last NodeID, comma Token, _ string) NodeID { return p.onCommaPostAttachToValue(last, comma) },
		)
		if err != nil {
			return 0, false, err
		}
		kids := make([]NodeID, 0, 1+len(args))
		kids = append(kids, left)
		kids = append(kids, args...)
		n := p.mkIR("call", leftStartTok, closeTok, kids...)
		return n, true, nil

	case CLSQUARE:
		p.i++
		idx, closeTok, perr := p.parseSingleBetween(RSQUARE, "expected ']'")
		if perr != nil {
			return 0, false, perr
		}
		n := p.mkIR("idx", leftStartTok, closeTok, left, idx)
		return n, true, nil

	case PERIOD:
		p.i++ // consume '.'
		dotTok := p.toks[p.i-1]
		// GAPLESS after '.': forbid ANNOTATION/NOOP immediately after dot.
		if err := p.needNoGap(dotTok, "expected property name, integer, or '(expr)' immediately after '.' (no gap allowed)"); err != nil {
			return 0, false, err
		}
		if p.match(LROUND) || p.match(CLROUND) {
			ex, closeTok, perr := p.parseSingleBetween(RROUND, "expected ')' after computed property")
			if perr != nil {
				return 0, false, perr
			}
			n := p.mkIR("idx", leftStartTok, closeTok, left, ex)
			return n, true, nil
		}
		// .<int> -> idx
		if p.match(INTEGER) {
			intTok := p.i - 1
			intNode := p.mkLeafIR("int", intTok, p.prev().Literal)
			n := p.mkIR("idx", leftStartTok, intTok, left, intNode)
			return n, true, nil
		}
		// .id or coerced quoted-name (lexer coerces after PERIOD) -> get
		if p.match(ID) {
			propTok := p.i - 1
			prop := p.mkLeafIR("str", propTok, tokText(p.prev()))
			n := p.mkIR("get", leftStartTok, propTok, left, prop)
			return n, true, nil
		}
		// Fallback diagnostic (not a valid follower)
		g := p.peek()
		line, col := p.posAtByte(g.StartByte)
		return 0, false, &Error{Kind: DiagParse, Msg: "expected property name, integer, or '(expr)' after '.'", Line: line, Col: col}
	}
	return 0, false, nil
}

// ───────────────────────── collections / lists / maps ─────────────────────

// bracketed parses a comma-separated list between an already-consumed opener
// and its 'close'. It carries interstitial PRE as annSrc and emits annot(noop) with correct spans.
func (p *parser) bracketed(
	close TokenType,
	expectMsg string,
	parseElem func(pendingPRE annSrc) (NodeID, error),
	onCommaPost func(last NodeID, comma Token, txt string) NodeID,
) ([]NodeID, int, int, error) {
	openTok := p.i - 1

	// Empty only when the very next token is the closer (no gap skipping).
	if p.match(close) {
		return nil, openTok, p.i - 1, nil
	}

	var out []NodeID
	pending := annNone()

	for {
		// Close: emit dangling PRE as annot(noop)
		if p.peek().Type == close {
			if pending.ok {
				out = append(out, p.attachAnnotFrom(pending, p.mkLeafIR("noop", -1)))
				pending = annNone()
			}
			break
		}

		// Interstitial gaps
		switch p.peek().Type {
		case ANNOTATION:
			pending = mergeAnn(pending, p.collectAnnotsSrc())
			continue
		case NOOP:
			if pending.ok {
				out = append(out, p.attachAnnotFrom(pending, p.mkLeafIR("noop", -1)))
				pending = annNone()
				p.i++ // consume the NOOP we wrapped
				continue
			}
			out = append(out, p.mkLeafIR("noop", p.i))
			p.i++
			continue
		}

		// Element with current pending PRE (caller applies it appropriately)
		elem, err := parseElem(pending)
		if err != nil {
			return nil, 0, 0, err
		}
		// Same-line POST handled by IR attachment here:
		if end := p.lastSpanEndTok; end >= 0 && end < len(p.toks) {
			elem = p.attachSameLineAnnots(elem, p.toks[end].Line)
		}
		pending = annNone() // Clear pending PRE

		out = append(out, elem)

		// Optional comma; allow trailing comma
		if !p.match(COMMA) {
			break
		}
		comma := p.prev()
		last := out[len(out)-1]
		if onCommaPost != nil {
			last = onCommaPost(last, comma, "")
		} else {
			last = p.attachSameLineAnnots(last, comma.Line)
		}
		out[len(out)-1] = last
	}

	// Drain trailing gaps before closer (NOOP/ANNOTATION), then require closer.
	if err := p.drainTrailingGapsUntil(close, expectMsg, &out); err != nil {
		return nil, 0, 0, err
	}
	if _, err := p.need(close, expectMsg); err != nil {
		return nil, 0, 0, err
	}
	return out, openTok, p.i - 1, nil
}

func (p *parser) arrayLiteralAfterOpen() (NodeID, error) {
	if p.match(RSQUARE) {
		return p.mkIR("array", p.i-2, p.i-1 /* '[]' */), nil
	}
	items, openTok, closeTok, err := p.bracketed(
		RSQUARE, "expected ']'",
		func(pending annSrc) (NodeID, error) {
			e, err := p.expr(0)
			if err != nil {
				return 0, err
			}
			if pending.ok {
				e = p.attachAnnotFrom(pending, e)
			}
			return e, nil
		},
		nil, // POST-after-comma attaches to element itself
	)
	if err != nil {
		return 0, err
	}
	return p.mkIR("array", openTok, closeTok, items...), nil
}

// params parses (CLROUND ... RROUND) parameter pairs; preserves NOOPs and
// PRE/POST rules uniformly across entries, while *ignoring* NOOPs that appear
// between ':' and the type expression (so annotations there still bind to the type).
// Critically, it NORMALIZES annotations at the binding site by merging pending/A/B/C/D
// onto the VALUE inside each ("pair", name, value), never onto the pair node itself.
func (p *parser) params() (NodeID, error) {
	if _, perr := p.need(CLROUND, "expected '(' to start parameters"); perr != nil {
		return 0, perr
	}
	openTok := p.i - 1

	// Immediate close → empty params array
	if p.match(RROUND) {
		return p.mkIR("array", openTok, p.i-1), nil
	}

	// Guard: preserve the same diagnostic if a non-element appears before ')'.
	switch p.peek().Type {
	case ANNOTATION, NOOP, RROUND, ID, EOF:
	default:
		g := p.peek()
		line, col := p.posAtByte(g.StartByte)
		return 0, &Error{Kind: DiagParse, Msg: "expected ')' after parameters", Line: line, Col: col}
	}

	entries, _, closeTok, err := p.bracketed(
		RROUND, "expected ')' after parameters",
		func(pendingPRE annSrc) (NodeID, error) {
			idTok, err := p.need(ID, "expected parameter name")
			if err != nil {
				return 0, err
			}
			idIdx := p.i - 1
			nameLeaf := p.mkLeafIR("id", idIdx, tokText(idTok))

			var val NodeID
			if p.match(COLON) {
				b, err := p.takeReqGap(p.prev(), "expected type after ':'")
				if err != nil {
					return 0, err
				}
				tExpr, err := p.expr(0)
				if err != nil {
					return 0, err
				}
				ann := mergeAnn(pendingPRE, b)
				if ann.ok {
					tExpr = p.attachAnnotFrom(ann, tExpr)
				}
				val = tExpr
			} else {
				base := p.mkLeafIR("id", -1, "Any")
				val = p.attachAnnotFrom(pendingPRE, base)
			}

			pair := p.mkIR("pair", -1, -1, nameLeaf, val)
			return pair, nil
		},
		func(last NodeID, comma Token, _ string) NodeID { return p.onCommaPostAttachToValue(last, comma) },
	)
	if err != nil {
		return 0, err
	}
	return p.mkIR("array", openTok, closeTok, entries...), nil
}

func (p *parser) mapLiteralAfterOpen(openTok int) (NodeID, error) {
	p.skipNoops()
	if p.match(RCURLY) {
		return p.mkIR("map", openTok, p.i-1), nil
	}

	pairs, _, closeTok, err := p.bracketed(
		RCURLY, "expected '}'",
		func(pendingPRE annSrc) (NodeID, error) {
			elemStartTok := p.i // current token before reading key/annots
			k, aKey, err := p.readKeyString()
			if err != nil {
				return 0, err
			}

			req := p.match(BANG)
			colonTok, err := p.need(COLON, "expected ':' after key")
			if err != nil {
				return 0, err
			}

			// B = GAP after ':'; it cannot satisfy the value requirement.
			b, err := p.takeReqGap(colonTok, "expected value after ':'")
			if err != nil {
				return 0, err
			}
			v, err := p.expr(0)
			if err != nil {
				return 0, err
			}

			val := v
			ann := mergeAnn(mergeAnn(pendingPRE, aKey), b)
			if ann.ok {
				val = p.attachAnnotFrom(ann, val)
			}

			// Pair span must start at the earliest PRE (pending or key PRE) if present.
			pairStartTok := elemStartTok
			if pendingPRE.ok && (pairStartTok < 0 || pendingPRE.startTok < pairStartTok) {
				pairStartTok = pendingPRE.startTok
			}
			if aKey.ok && (pairStartTok < 0 || aKey.startTok < pairStartTok) {
				pairStartTok = aKey.startTok
			}

			tag := "pair"
			if req {
				tag = "pair!"
			}
			return p.mkIR(tag, pairStartTok, p.i-1, k, val), nil
		},
		func(last NodeID, comma Token, _ string) NodeID { return p.onCommaPostAttachToValue(last, comma) },
	)
	if err != nil {
		return 0, err
	}
	return p.mkIR("map", openTok, closeTok, pairs...), nil
}

// ───────────────────────── control / loops / if ───────────────────────────

func (p *parser) parseControl(t Token, startTok int) (NodeID, error) {
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
		return p.mkIR(tag, startTok, startTok, p.mkLeafIR("null", -1)), nil
	}

	p.skipNoops()
	// Same line but next token can't start a value → also Null.
	// NOTE: An inline annotation after a control word (e.g. `return # note`)
	// is treated as a trailing comment on the bare control, not as the start
	// of a value expression on the next line.
	switch p.peek().Type {
	case END, ELSE, ELIF, THEN, RROUND, RSQUARE, RCURLY, ANNOTATION:
		return p.mkIR(tag, startTok, startTok, p.mkLeafIR("null", -1)), nil
	}

	// Parse value and attach any adjacent annotations.
	x, err := p.expr(0)
	if err != nil {
		return 0, err
	}
	// Attach any adjacent annotations to the control's value (source-aware).
	if tail := p.collectAnnotsSrc(); tail.ok {
		x = p.attachAnnotFrom(tail, x)
	}
	return p.mkIR(tag, startTok, p.lastSpanEndTok, x), nil
}

func (p *parser) ifExpr() (NodeID, error) {
	ifTok := p.toks[p.i-1]
	condStartTok := p.i
	cond, err := p.parseExprAfterBP(ifTok, "expected condition after 'if'", 0)
	if err != nil {
		return 0, err
	}
	between := p.collectGap()
	if _, err := p.need(THEN, "expected 'then'"); err != nil {
		return 0, err
	}
	thenBlk, err := p.blockUntil(END, ELIF, ELSE)
	if err != nil {
		return 0, err
	}
	if between.ok {
		thenBlk = p.attachAnnotFrom(between, thenBlk)
	}
	arm := p.mkIR("pair", condStartTok, p.lastSpanEndTok, cond, thenBlk)
	arms := []NodeID{arm}

	// Repeated elif arms with gap-aware detection and attachment.
	for {
		saveI := p.i
		lead := p.collectGap() // annotations just before 'elif' attach to that arm's block
		if !p.match(ELIF) {
			// Not an elif: restore so annotations remain for 'else' or next construct.
			p.i = saveI
			break
		}
		elifTok := p.toks[p.i-1]
		condStartTok = p.i
		c, err := p.parseExprAfterBP(elifTok, "expected condition after 'elif'", 0)
		if err != nil {
			return 0, err
		}
		between := p.collectGap()
		if _, err := p.need(THEN, "expected 'then'"); err != nil {
			return 0, err
		}
		b, err := p.blockUntil(END, ELIF, ELSE)
		if err != nil {
			return 0, err
		}
		if lead.ok {
			b = p.attachAnnotFrom(lead, b)
		}
		if between.ok {
			b = p.attachAnnotFrom(between, b)
		}
		arm := p.mkIR("pair", condStartTok, p.lastSpanEndTok, c, b)
		arms = append(arms, arm)
	}

	var elseTail []NodeID
	// Optional else with gap-aware attachment.
	saveI := p.i
	preElse := p.collectGap()
	if !p.match(ELSE) {
		// No else: restore so annotations are not lost.
		p.i = saveI
	} else {
		b, err := p.blockUntil(END)
		if err != nil {
			return 0, err
		}
		if preElse.ok {
			b = p.attachAnnotFrom(preElse, b)
		}
		elseTail = []NodeID{b}
	}
	if _, err := p.need(END, "expected 'end'"); err != nil {
		return 0, err
	}
	return p.mkIR("if", -1, -1, append(arms, elseTail...)...), nil
}

func (p *parser) forExpr(openTok int) (NodeID, error) {
	// Ensure gaps right after 'for' are treated uniformly (including NOOPs)
	preFor, err := p.takeReqGap(p.toks[openTok], "expected for-target after 'for'")
	if err != nil {
		return 0, err
	}
	tgt, err := p.forTarget()
	if err != nil {
		return 0, err
	}
	if preFor.ok {
		tgt = p.attachAnnotFrom(preFor, tgt)
	}
	between := p.collectGap()
	inTok, err := p.need(IN, "expected 'in'")
	if err != nil {
		return 0, err
	}
	postIn, err := p.takeReqGap(inTok, "expected expression after 'in'")
	if err != nil {
		return 0, err
	}
	iter, err := p.expr(0)
	if err != nil {
		return 0, err
	}
	// Attach both the pre-'in' gap (between target and 'in') and the post-'in' gap.
	iter = p.attachAnnotFrom(mergeAnn(between, postIn), iter)
	body, err := p.parseDoBlockWithLeadingGap()
	if err != nil {
		return 0, err
	}
	return p.mkIR("for", openTok, p.i-1, tgt, iter, body), nil
}

func (p *parser) whileExpr(openTok int) (NodeID, error) {
	cond, err := p.parseExprAfterBP(p.toks[openTok], "expected condition after 'while'", 0)
	if err != nil {
		return 0, err
	}
	body, err := p.parseDoBlockWithLeadingGap()
	if err != nil {
		return 0, err
	}
	return p.mkIR("while", openTok, p.i-1, cond, body), nil
}

// ───────────────────────── functions / oracle ─────────────────────────────

func (p *parser) optionalArrowType(incMsg string) (NodeID, error) {
	// Allow a GAP between params ')' and '->'. If '->' is found AFTER the gap,
	// parse the type and attach the gap's annotations to that type. Otherwise,
	// restore so the gap can belong to the subsequent join (e.g., 'do').
	saveI := p.i
	lead := p.collectGap() // GAP between ')' and '->' (if any)
	if p.match(ARROW) {
		arrowTok := p.prev()
		post, err := p.takeReqGap(arrowTok, incMsg) // GAP immediately after '->'
		if err != nil {
			return 0, err
		}
		r, err := p.expr(0)
		if err != nil {
			return 0, err
		}
		r = p.attachAnnotFrom(mergeAnn(lead, post), r)
		return r, nil // parsed type (one node)
	}
	// No arrow: put the parser back so the gap isn't consumed.
	p.i = saveI
	return p.mkLeafIR("id", -1, "Any"), nil // single synthetic node
}

// Small shared header for fun/oracle: params + optional arrow type
type fnHeader struct{ params, arrow NodeID }

func (p *parser) parseFnHeader(kind string) (fnHeader, error) {
	ps, err := p.params()
	if err != nil {
		return fnHeader{}, err
	}
	msg := "expected return type after '->'"
	if kind == "oracle" {
		msg = "expected output type after '->'"
	}
	ar, err := p.optionalArrowType(msg)
	return fnHeader{ps, ar}, err
}

func (p *parser) funExpr(openTok int) (NodeID, error) {
	h, err := p.parseFnHeader("fun")
	if err != nil {
		return 0, err
	}
	body, err := p.parseDoBlockWithLeadingGap()
	if err != nil {
		return 0, err
	}
	node := p.mkIR("fun", openTok, p.i-1, h.params, h.arrow, body)
	return node, nil
}

func (p *parser) oracleExpr(openTok int) (NodeID, error) {
	h, err := p.parseFnHeader("oracle")
	if err != nil {
		return 0, err
	}
	var src NodeID
	// Collect annotations/newlines before optional 'from'.
	saveI := p.i
	preFrom := p.collectGap()
	matchedFrom := p.match(FROM)
	if matchedFrom {
		fromTok := p.prev()
		postFrom, err := p.takeReqGap(fromTok, "expected expression after 'from'")
		if err != nil {
			return 0, err
		}
		ex, err := p.expr(0)
		if err != nil {
			return 0, err
		}
		ex = p.attachAnnotFrom(mergeAnn(preFrom, postFrom), ex)
		src = ex
	} else {
		// No 'from': restore so the gap belongs to whatever follows the oracle.
		p.i = saveI
		src = p.mkIR("array", -1, -1) // build only when needed
	}
	body := p.mkIR("oracle", openTok, p.i-1, h.params, h.arrow, src)
	return body, nil
}

// readKeyString allows stacked PRE-annotations (handled recursively).
// Span order: (1) PRE "str" child; (2) "annot" wrapper; (3) final key "str" leaf.
func (p *parser) readKeyString() (NodeID, annSrc, error) {
	// Stackable PRE immediately before key
	var pre annSrc
	for !p.atEnd() && p.peek().Type == ANNOTATION {
		pre = mergeAnn(pre, p.collectAnnotsSrc())
	}
	if p.match(STRING) {
		return p.mkLeafIR("str", p.i-1, p.prev().Literal), pre, nil
	}
	t := p.peek()
	if isWordLike(t.Type) {
		p.i++
		name := t.Lexeme
		if s, ok := t.Literal.(string); ok {
			name = s
		}
		return p.mkLeafIR("str", p.i-1, name), pre, nil
	}
	g := p.peek()
	if p.interactive && p.onlyGapsToEOF() {
		line, col := p.posAfterLastSpan()
		return 0, annNone(), &Error{Kind: DiagIncomplete, Msg: "expected key", Line: line, Col: col}
	}
	line, col := p.posAtByte(g.StartByte)
	return 0, annNone(), &Error{Kind: DiagParse, Msg: "expected key", Line: line, Col: col}
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

func (p *parser) forTarget() (NodeID, error) {
	// 'for let P in ...'
	if p.match(LET) {
		letTokIdx := p.i - 1
		letTok := p.toks[letTokIdx]
		pre, err := p.takeReqGap(letTok, "expected pattern after 'let'")
		if err != nil {
			return 0, err
		}
		const assignBP = 10
		pat, err := p.expr(assignBP + 1)
		if err != nil {
			return 0, err
		}
		if pre.ok {
			pat = p.attachAnnotFrom(pre, pat)
		}
		endTok := p.lastSpanEndTok
		if endTok < 0 {
			endTok = letTokIdx
		}
		return p.mkIR("let", letTokIdx, endTok, pat), nil
	}

	// Otherwise parse an expression and require it to be assignable.
	save := p.i
	e, err := p.expr(90)
	if err != nil {
		return 0, err
	}
	if !p.assignableIR(e) {
		p.i = save
		g := p.peek()
		line, col := p.posAtByte(g.StartByte)
		return 0, &Error{Kind: DiagParse, Msg: "invalid for-target", Line: line, Col: col}
	}
	return e, nil
}

func (p *parser) assignableIR(id NodeID) bool {
	// unwrap annotations
	cur := id
	for {
		n := p.ir.Nodes[cur]
		if n.Tag == "annot" && len(n.Kids) >= 2 {
			cur = n.Kids[1]
			continue
		}
		break
	}
	tag := p.ir.Nodes[cur].Tag
	switch tag {
	case "id", "get", "idx", "array", "map", "let":
		return true
	default:
		return false
	}
}
