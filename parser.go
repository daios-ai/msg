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

// ─────────────────────── centralized annotation carrier ──────────────────────
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
	return annSrc{txt: txt, startTok: start, endTok: end, ok: txt != ""}
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
func (p *parser) attachSameLineAnnots(n S, refLine int) S {
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

// Attach annotation so:
//   - the child ("str", txt) uses the annotation token's span (src.tokIdx), and
//   - the wrapper ("annot", ...) inherits the *wrapped value's* span — not the child.
//
// This preserves strict post-order: base was already appended; we append the child
// next, and finally the wrapper using the base's span snapshot (not "lastSpan*",
// which now points at the child).
func (p *parser) attachAnnotFrom(src annSrc, val S) S {
	if !src.ok || src.txt == "" {
		return val
	}

	base := val
	have := ""
	if b, h, ok := unwrapAnnot(val); ok {
		base, have = b, h
	}

	// Snapshot the base's span *before* emitting the child, because appending
	// the child will update lastSpan* to the child's span.
	baseStartTok, baseEndTok := p.lastSpanStartTok, p.lastSpanEndTok
	// Build the child "str" using the annotation tokens' combined range.
	// If only one annotation token is present, startTok==endTok.
	childText := joinNonEmpty(src.txt, have)
	startTok := src.startTok
	endTok := src.endTok
	if startTok < 0 {
		startTok = baseStartTok // fallback, though callers should supply annotation tokens
	}
	if endTok < 0 {
		endTok = baseEndTok
	}
	// 1) Append child span.
	child := p.mk("str", startTok, endTok, childText)

	// 2) Reorder spans to match post-order of the *final* AST subtree:
	// We just appended: [..., base, child]. The post-order under ("annot", child, base)
	// must be: child, base, wrapper. Swap the last two so it becomes [..., child, base].
	if n := len(p.post); n >= 2 {
		p.post[n-2], p.post[n-1] = p.post[n-1], p.post[n-2]
		// Do not touch lastSpanStartTok/EndTok; wrapper uses the base snapshot we captured.
	}
	// Wrapper inherits the *base's* span snapshot.
	return p.mk("annot", baseStartTok, baseEndTok, child, base)
}

// ───────────────────── centralized A+B+C+D normalization ─────────────────────
// A: PRE from the "left" side (e.g., LHS annot, key annot, pending PRE)
// B: annotations immediately after the binding site token (e.g., '=' or ':')
// C: PRE already on the parsed right/value node
// D: annotations immediately following the right/value node (same adjacency rule as parser)
//
// Central ABCD normalization for VALUE sites (assign RHS, params value, map value, etc.).
// A and B are source-carrying; C (existing on right) is folded in; D is adjacent after right.
func (p *parser) normalizeABCD(a annSrc, b annSrc, right S) S {
	baseRight, cTxt, _ := unwrapAnnot(right) // C: text only (no reliable tokens)
	c := annSrc{txt: cTxt, startTok: -1, endTok: -1, ok: cTxt != ""}
	d := p.collectAnnotsSrc() // D: includes source tokens (startTok/endTok set)
	merged := mergeAnn(mergeAnn(a, b), mergeAnn(c, d))
	return p.attachAnnotFrom(merged, baseRight)
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

// drainTrailingGapsUntil consumes any sequence of NOOPs and/or ANNOTATIONs
// until the given closing token is seen. Each NOOP becomes ("noop") and each
// annotation becomes ("annot", ("str", mergedText), ("noop")).
//
// It never crosses non-gap tokens. If a non-gap appears before the closer,
// it delegates to need(close, expectMsg) to produce the right diagnostic.
func (p *parser) drainTrailingGapsUntil(close TokenType, expectMsg string, out *[]any) error {
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
			*out = append(*out, p.mkLeaf("noop", p.i))
			p.i++

		case ANNOTATION:
			// Merge adjacent annotations and wrap a noop, preserving the annotation's source span.
			src := p.collectAnnotsSrc()
			*out = append(*out, p.attachAnnotFrom(src, p.mkLeaf("noop", -1)))

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

func (p *parser) expr(minBP int) (S, error) {
	tokIndexOfThis := p.i
	t := p.peek()
	p.i++

	var left S
	leftAnnA := annNone()
	leftStartTok := tokIndexOfThis

	// ---- prefix ----
	if n, ok := p.tryLiteralOrId(t, tokIndexOfThis); ok {
		left = n
	} else {
		switch t.Type {
		case NOOP:
			left = p.mkLeaf("noop", tokIndexOfThis)

		case ENUM:
			// Accept gap (annotations/newlines) between 'Enum' and '[' safely.
			saveI := p.i
			enumPre := p.collectGap()
			if p.peek().Type == LSQUARE || p.peek().Type == CLSQUARE {
				p.i++ // consume '[' or CLSQUARE
				// Reuse full array machinery (gaps, PRE/POST, dangling PRE).
				arr, err := p.arrayLiteralAfterOpen()
				if err != nil {
					return nil, err
				}
				// Retag: ("array", e1, e2, ...) -> ("enum", e1, e2, ...)
				items := make([]any, 0, len(arr)-1)
				for i := 1; i < len(arr); i++ {
					items = append(items, arr[i])
				}
				n := p.mk("enum", tokIndexOfThis, p.i-1, items...)
				if enumPre.ok {
					n = p.attachAnnotFrom(enumPre, n)
				}
				left = n
			} else {
				// Not an Enum-literal; restore so gap remains for the next construct.
				p.i = saveI
				left = p.mkLeaf("id", tokIndexOfThis, tokText(t))
			}

		case MINUS, NOT:
			pre, err := p.takeReqGap(t, "expected expression after unary operator")
			if err != nil {
				return nil, err
			}
			r, err := p.expr(80)
			if err != nil {
				return nil, err
			}
			if pre.ok {
				r = p.attachAnnotFrom(pre, r)
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
			fn, err := p.funExpr(tokIndexOfThis)
			if err != nil {
				return nil, err
			}
			left = fn
			leftStartTok = tokIndexOfThis

		case ORACLE:
			orc, err := p.oracleExpr(tokIndexOfThis)
			if err != nil {
				return nil, err
			}
			left = orc
			leftStartTok = tokIndexOfThis

		case MODULE:
			pre, err := p.takeReqGap(t, "expected module name expression")
			if err != nil {
				return nil, err
			}
			name, err := p.expr(0)
			if err != nil {
				return nil, err
			}
			if pre.ok {
				name = p.attachAnnotFrom(pre, name)
			}
			// Accept annotations/newlines before 'do' and attach them to body.
			preDo := p.collectGap()
			body, err := p.parseBlock(true)
			if err != nil {
				return nil, err
			}
			if preDo.ok {
				body = p.attachAnnotFrom(preDo, body)
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
			pat, a, err := p.declPattern()
			if err != nil {
				return nil, err
			}
			left = pat
			leftAnnA = a
			leftStartTok = tokIndexOfThis

			// If destructuring, require an '=' (after any annotations).
			base, _, _ := unwrapAnnot(pat)
			if tag, _ := base[0].(string); tag == "darr" || tag == "dobj" {
				// Skip GAPs (ANNOTATION/NOOP) without letting them satisfy the requirement.
				j := p.i
				for j < len(p.toks) && (p.toks[j].Type == ANNOTATION || p.toks[j].Type == NOOP) {
					j++
				}
				// If only gaps/EOF remain, report same message across modes.
				if j >= len(p.toks) || p.toks[j].Type == EOF {
					line, col := p.posAfterLastSpan()
					kind := DiagParse
					if p.interactive {
						kind = DiagIncomplete
					}
					return nil, &Error{Kind: kind, Msg: "expected '=' after destructuring let pattern", Line: line, Col: col}
				}
				// Not EOF: require ASSIGN next; if not, hard parse error at that token.
				if p.toks[j].Type != ASSIGN {
					line, col := p.posAtByte(p.toks[j].StartByte)
					return nil, &Error{Kind: DiagParse, Msg: "expected '=' after destructuring let pattern", Line: line, Col: col}
				}
			}

		case TYPECONS:
			pre, err := p.takeReqGap(t, "expected type expression after 'type'")
			if err != nil {
				return nil, err
			}
			x, err := p.expr(1) // keep bp=1 behavior
			if err != nil {
				return nil, err
			}
			if pre.ok {
				x = p.attachAnnotFrom(pre, x)
			}
			left = p.mk("type", tokIndexOfThis, p.lastSpanEndTok, x)
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
				return p.attachAnnotFrom(src, p.mkLeaf("noop", p.i-1)), nil
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
						return nil, &Error{Kind: DiagIncomplete, Msg: "expected expression after annotation", Line: line, Col: col}
					}
					return p.attachAnnotFrom(src, p.mkLeaf("noop", -1)), nil
				case END, ELSE, ELIF, THEN, RROUND, RSQUARE, RCURLY:
					return p.attachAnnotFrom(src, p.mkLeaf("noop", -1)), nil
				}
			}

			// Interactive: only gaps after the annotation → incomplete
			if p.interactive && p.onlyGapsToEOF() {
				line, col := p.posAtByte(t.StartByte)
				return nil, &Error{Kind: DiagIncomplete, Msg: "expected expression after annotation", Line: line, Col: col}
			}
			// Non-interactive EOF → treat as annot(noop)
			if p.atEnd() && !p.interactive {
				left = p.attachAnnotFrom(src, p.mkLeaf("noop", -1))
				leftStartTok = tokIndexOfThis
				break
			}

			// Normal case: parse the annotated operand
			operand, err := p.expr(0)
			if err != nil {
				return nil, err
			}
			endTok := p.lastSpanEndTok
			if endTok < 0 {
				endTok = tokIndexOfThis
			}

			// If operand is an assignment, fold PRE onto the RHS value
			if len(operand) >= 3 {
				if tag, _ := operand[0].(string); tag == "assign" {
					lhs, _ := operand[1].(S)
					rhs, _ := operand[2].(S)
					rhs = p.attachAnnotFrom(src, rhs)
					left = p.mk("assign", -1, -1, lhs, rhs)
					return left, nil
				}
			}

			// If operand is a control form, PRE attaches to the control's VALUE
			if len(operand) >= 2 {
				if tag, _ := operand[0].(string); tag == "return" || tag == "break" || tag == "continue" {
					val := operand[1].(S)
					val = p.attachAnnotFrom(src, val)
					left = p.mk(tag, tokIndexOfThis, endTok, val)
					leftStartTok = tokIndexOfThis
					return left, nil
				}
			}

			// Normal (non-assignment, non-control) case: wrap operand with PRE
			left = p.attachAnnotFrom(src, operand)
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

		// Allow GAP between operator and RHS, but don't let it satisfy the need.
		preOp, err := p.takeReqGap(op, "expected expression after operator")
		if err != nil {
			return nil, err
		}

		var b annSrc
		if op.Type == ASSIGN {
			// Use the immediate post-'=' gap as B.
			b = preOp
		}

		rightParsed, err := p.expr(nextBP)
		if err != nil {
			return nil, err
		}
		endTok := p.lastSpanEndTok

		if op.Type == ASSIGN {
			// A (from prior 'let' pattern if any) + B + (C on right) + D
			normRight := p.normalizeABCD(leftAnnA, b, rightParsed)
			left = p.mk("assign", leftStartTok, endTok, left, normRight)
			leftAnnA = annNone()
		} else {
			left = p.mk("binop", leftStartTok, endTok, op.Lexeme, left, rightParsed)
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

// parseGrouping reads '(' expr ')' for either LROUND or CLROUND in prefix position.
func (p *parser) parseGrouping() (S, error) {
	openTok := p.toks[p.i-1]
	pre, err := p.takeReqGap(openTok, "expected expression after '('")
	if err != nil {
		return nil, err
	}
	inner, err := p.expr(0)
	if err != nil {
		return nil, err
	}
	if pre.ok {
		inner = p.attachAnnotFrom(pre, inner)
	}
	// Allow trailing annotations/newlines before ')', attach as POST to inner.
	trail := p.collectGap() // consumes ANNOTATIONs and NOOPs
	if trail.ok {
		inner = p.attachAnnotFrom(trail, inner)
	}
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
			return nil, false, &Error{Kind: DiagParse, Msg: "no gap allowed before '.'", Line: line, Col: col}
		}
		// Not a dot next; postfix chain stops here.
		return nil, false, nil
	}

	switch p.peek().Type {
	case QUESTION:
		qtok := p.i
		p.i++
		n := p.mk("unop", leftStartTok, qtok, "?", left)
		return n, true, nil

	case CLROUND:
		p.i++
		if p.match(RROUND) {
			n := p.mk("call", leftStartTok, p.i-1, left)
			return n, true, nil
		}
		args, _, closeTok, err := p.bracketed(
			RROUND, "expected ')'",
			func(pending annSrc) (S, int, error) {
				a, err := p.expr(0)
				if err != nil {
					return nil, 0, err
				}
				if pending.ok {
					a = p.attachAnnotFrom(pending, a)
				}
				return a, 0, nil
			},
			nil,
		)
		if err != nil {
			return nil, false, err
		}
		n := p.mk("call", leftStartTok, closeTok, append([]any{left}, args...)...)
		return n, true, nil

	case CLSQUARE:
		p.i++
		sqTok := p.toks[p.i-1]
		// Allow gaps after '[' but they cannot satisfy the required expr.
		preIdx, err := p.takeReqGap(sqTok, "expected index expression after '['")
		if err != nil {
			return nil, false, err
		}
		idx, err := p.expr(0)
		if err != nil {
			return nil, false, err
		}
		// Allow trailing gaps before ']'
		if trail := p.collectGap(); trail.ok {
			idx = p.attachAnnotFrom(trail, idx)
		}
		if _, err := p.need(RSQUARE, "expected ']'"); err != nil {
			return nil, false, err
		}
		_ = preIdx // gap already attached via normalize/attach above if needed
		n := p.mk("idx", leftStartTok, p.i-1, left, idx)
		return n, true, nil

	case PERIOD:
		p.i++ // consume '.'
		dotTok := p.toks[p.i-1]
		// GAPLESS after '.': forbid ANNOTATION/NOOP immediately after dot.
		if err := p.needNoGap(dotTok, "expected property name, integer, or '(expr)' immediately after '.' (no gap allowed)"); err != nil {
			return nil, false, err
		}
		if p.match(LROUND) || p.match(CLROUND) {
			parTok := p.toks[p.i-1] // '(' token just consumed
			// Allow gaps after '(' but they cannot satisfy the required expr.
			pre, err := p.takeReqGap(parTok, "expected expression after '('")
			if err != nil {
				return nil, false, err
			}
			ex, err := p.expr(0)
			if err != nil {
				return nil, false, err
			}
			// Allow trailing gaps before ')'
			if trail := p.collectGap(); trail.ok {
				ex = p.attachAnnotFrom(trail, ex)
			}
			if _, perr := p.need(RROUND, "expected ')' after computed property"); perr != nil {
				return nil, false, perr
			}
			_ = pre // attached above
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
		// .id or coerced quoted-name (lexer coerces after PERIOD) -> get
		if p.match(ID) {
			propTok := p.i - 1
			prop := p.mkLeaf("str", propTok, tokText(p.prev()))
			n := p.mk("get", leftStartTok, propTok, left, prop)
			return n, true, nil
		}
		// Fallback diagnostic (not a valid follower)
		g := p.peek()
		line, col := p.posAtByte(g.StartByte)
		return nil, false, &Error{Kind: DiagParse, Msg: "expected property name, integer, or '(expr)' after '.'", Line: line, Col: col}
	}
	return nil, false, nil
}

// ───────────────────────── collections / lists / maps ─────────────────────

// bracketed parses a comma-separated list between an already-consumed opener
// and its 'close'. It owns:
//   - interstitial PRE via ANNOTATION (pending text) and NOOP emission,
//   - POST right after an element (same line),
//   - POST after a COMMA (same line) via onCommaPost callback,
//   - dangling PRE before closer -> annot(noop),
//   - trailing gaps before closer (NOOP/ANNOTATION).
//
// bracketed parses a comma-separated list between an already-consumed opener.
// It carries interstitial PRE as annSrc and emits annot(noop) with correct spans.
func (p *parser) bracketed(
	close TokenType,
	expectMsg string,
	parseElem func(pendingPRE annSrc) (S, int, error),
	onCommaPost func(last S, commaTok Token, txt string) S,
) ([]any, int, int, error) {
	openTok := p.i - 1

	// Empty only when the very next token is the closer (no gap skipping).
	if p.match(close) {
		return nil, openTok, p.i - 1, nil
	}

	var out []any
	pending := annNone()

	for {
		// Close: emit dangling PRE as annot(noop)
		if p.peek().Type == close {
			if pending.ok {
				out = append(out, p.attachAnnotFrom(pending, p.mkLeaf("noop", -1)))
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
				out = append(out, p.attachAnnotFrom(pending, p.mkLeaf("noop", -1)))
				pending = annNone()
				p.i++ // consume the NOOP we wrapped
				continue
			}
			out = append(out, p.mkLeaf("noop", p.i))
			p.i++
			continue
		}

		// Element with current pending PRE (caller applies it appropriately)
		elem, _, err := parseElem(pending)
		if err != nil {
			return nil, 0, 0, err
		}
		// POST right after element (same line only)
		if end := p.lastSpanEndTok; end >= 0 && end < len(p.toks) {
			elem = p.attachSameLineAnnots(elem, p.toks[end].Line)
		}
		pending = annNone() // Clear pending PRE — responsibility is with parseElem.

		out = append(out, elem)

		// Optional comma; allow trailing comma
		if !p.match(COMMA) {
			break
		}
		comma := p.prev()
		last := out[len(out)-1].(S)
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

func (p *parser) arrayLiteralAfterOpen() (S, error) {
	if p.match(RSQUARE) {
		return p.mk("array", p.i-2, p.i-1 /* '[]' */), nil
	}
	items, openTok, closeTok, err := p.bracketed(
		RSQUARE, "expected ']'",
		func(pending annSrc) (S, int, error) {
			e, err := p.expr(0)
			if err != nil {
				return nil, 0, err
			}
			if pending.ok {
				e = p.attachAnnotFrom(pending, e)
			}
			return e, 0, nil
		},
		nil, // POST-after-comma attaches to element itself
	)
	if err != nil {
		return nil, err
	}
	return p.mk("array", openTok, closeTok, items...), nil
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

	// Guard: preserve the same diagnostic if a non-element appears before ')'.
	switch p.peek().Type {
	case ANNOTATION, NOOP, RROUND, ID, EOF:
	default:
		g := p.peek()
		line, col := p.posAtByte(g.StartByte)
		return nil, &Error{Kind: DiagParse, Msg: "expected ')' after parameters", Line: line, Col: col}
	}

	entries, _, closeTok, err := p.bracketed(
		RROUND, "expected ')' after parameters",
		func(pendingPRE annSrc) (S, int, error) {
			elemStartTok := p.i
			idTok, err := p.need(ID, "expected parameter name")
			if err != nil {
				return nil, 0, err
			}
			idIdx := p.i - 1
			nameLeaf := p.mkLeaf("id", idIdx, tokText(idTok))

			var val S
			if p.match(COLON) {
				b, err := p.takeReqGap(p.prev(), "expected type after ':'")
				if err != nil {
					return nil, 0, err
				}
				tExpr, err := p.expr(0)
				if err != nil {
					return nil, 0, err
				}
				val = p.normalizeABCD(pendingPRE, b, tExpr)
			} else {
				base := p.mkLeaf("id", -1, "Any")
				val = p.attachAnnotFrom(pendingPRE, base)
			}

			// Pair span starts at pending PRE if present.
			pairStartTok := elemStartTok
			if pendingPRE.ok && pendingPRE.startTok < pairStartTok {
				pairStartTok = pendingPRE.startTok
			}
			return p.mk("pair", pairStartTok, p.i-1, nameLeaf, val), 0, nil
		},
		func(last S, comma Token, _ string) S {
			if len(last) >= 3 {
				v := last[2].(S)
				v = p.attachSameLineAnnots(v, comma.Line)
				last[2] = v
				return last
			}
			return p.attachSameLineAnnots(last, comma.Line)
		},
	)
	if err != nil {
		return nil, err
	}
	return p.mk("array", openTok, closeTok, entries...), nil
}

func (p *parser) mapLiteralAfterOpen(openTok int) (S, error) {
	p.skipNoops()
	if p.match(RCURLY) {
		return p.mk("map", openTok, p.i-1), nil
	}

	pairs, _, closeTok, err := p.bracketed(
		RCURLY, "expected '}'",
		func(pendingPRE annSrc) (S, int, error) {
			elemStartTok := p.i // current token before reading key/annots
			k, aKey, err := p.readKeyString()
			if err != nil {
				return nil, 0, err
			}

			req := p.match(BANG)
			colonTok, err := p.need(COLON, "expected ':' after key")
			if err != nil {
				return nil, 0, err
			}

			// B = GAP after ':'; it cannot satisfy the value requirement.
			b, err := p.takeReqGap(colonTok, "expected value after ':'")
			if err != nil {
				return nil, 0, err
			}
			v, err := p.expr(0)
			if err != nil {
				return nil, 0, err
			}

			// Value normalization (A + B + C + D)
			val := p.normalizeABCD(mergeAnn(pendingPRE, aKey), b, v)

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
			return p.mk(tag, pairStartTok, p.i-1, k, val), 0, nil
		},
		func(last S, comma Token, _ string) S {
			// POST-after-comma attaches to VALUE; consume same-line annots.
			if len(last) >= 3 {
				v := last[2].(S)
				v = p.attachSameLineAnnots(v, comma.Line)
				last[2] = v
				return last
			}
			return p.attachSameLineAnnots(last, comma.Line)
		},
	)
	if err != nil {
		return nil, err
	}
	return p.mk("map", openTok, closeTok, pairs...), nil
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
	x, err := p.expr(0)
	if err != nil {
		return nil, err
	}
	// Attach any adjacent annotations to the control's value (source-aware).
	if tail := p.collectAnnotsSrc(); tail.ok {
		x = p.attachAnnotFrom(tail, x)
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
	return cur, joinNonEmpty(parts...), true
}

func (p *parser) ifExpr() (S, error) {
	ifTok := p.toks[p.i-1]
	condStartTok := p.i
	preIf, err := p.takeReqGap(ifTok, "expected condition after 'if'")
	if err != nil {
		return nil, err
	}
	cond, err := p.expr(0)
	if err != nil {
		return nil, err
	}
	if preIf.ok {
		cond = p.attachAnnotFrom(preIf, cond)
	}
	between := p.collectGap()
	if _, err := p.need(THEN, "expected 'then'"); err != nil {
		return nil, err
	}
	thenBlk, err := p.blockUntil(END, ELIF, ELSE)
	if err != nil {
		return nil, err
	}
	if between.ok {
		thenBlk = p.attachAnnotFrom(between, thenBlk)
	}
	arm := p.mk("pair", condStartTok, p.lastSpanEndTok, cond, thenBlk)
	arms := []any{arm}

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
		preElif, err := p.takeReqGap(elifTok, "expected condition after 'elif'")
		if err != nil {
			return nil, err
		}
		c, err := p.expr(0)
		if err != nil {
			return nil, err
		}
		if preElif.ok {
			c = p.attachAnnotFrom(preElif, c)
		}
		between := p.collectGap()
		if _, err := p.need(THEN, "expected 'then'"); err != nil {
			return nil, err
		}
		b, err := p.blockUntil(END, ELIF, ELSE)
		if err != nil {
			return nil, err
		}
		if lead.ok {
			b = p.attachAnnotFrom(lead, b)
		}
		if between.ok {
			b = p.attachAnnotFrom(between, b)
		}
		arm := p.mk("pair", condStartTok, p.lastSpanEndTok, c, b)
		arms = append(arms, arm)
	}

	var elseTail []any
	// Optional else with gap-aware attachment.
	saveI := p.i
	preElse := p.collectGap()
	if !p.match(ELSE) {
		// No else: restore so annotations are not lost.
		p.i = saveI
	} else {
		b, err := p.blockUntil(END)
		if err != nil {
			return nil, err
		}
		if preElse.ok {
			b = p.attachAnnotFrom(preElse, b)
		}
		elseTail = []any{b}
	}
	if _, err := p.need(END, "expected 'end'"); err != nil {
		return nil, err
	}
	return L("if", append(arms, elseTail...)...), nil
}

func (p *parser) forExpr(openTok int) (S, error) {
	// Ensure gaps right after 'for' are treated uniformly (including NOOPs)
	preFor, err := p.takeReqGap(p.toks[openTok], "expected for-target after 'for'")
	if err != nil {
		return nil, err
	}
	tgt, err := p.forTarget()
	if err != nil {
		return nil, err
	}
	if preFor.ok {
		tgt = p.attachAnnotFrom(preFor, tgt)
	}
	between := p.collectGap()
	inTok, err := p.need(IN, "expected 'in'")
	if err != nil {
		return nil, err
	}
	postIn, err := p.takeReqGap(inTok, "expected expression after 'in'")
	if err != nil {
		return nil, err
	}
	iter, err := p.expr(0)
	if err != nil {
		return nil, err
	}
	// Attach both the pre-'in' gap (between target and 'in') and the post-'in' gap.
	iter = p.attachAnnotFrom(mergeAnn(between, postIn), iter)
	// Annotations/newlines before 'do' attach to the loop body.
	preDo := p.collectGap()
	body, err := p.parseBlock(true)
	if err != nil {
		return nil, err
	}
	if preDo.ok {
		body = p.attachAnnotFrom(preDo, body)
	}
	return p.mk("for", openTok, p.i-1, tgt, iter, body), nil
}

func (p *parser) whileExpr(openTok int) (S, error) {
	pre, err := p.takeReqGap(p.toks[openTok], "expected condition after 'while'")
	if err != nil {
		return nil, err
	}
	cond, err := p.expr(0)
	if err != nil {
		return nil, err
	}
	if pre.ok {
		cond = p.attachAnnotFrom(pre, cond)
	}
	// Annotations/newlines before 'do' attach to the body.
	preDo := p.collectGap()
	body, err := p.parseBlock(true)
	if err != nil {
		return nil, err
	}
	if preDo.ok {
		body = p.attachAnnotFrom(preDo, body)
	}
	return p.mk("while", openTok, p.i-1, cond, body), nil
}

// ───────────────────────── functions / oracle ─────────────────────────────

func (p *parser) optionalArrowType(incMsg string) (S, error) {
	// Allow a GAP between params ')' and '->'. If '->' is found AFTER the gap,
	// parse the type and attach the gap's annotations to that type. Otherwise,
	// restore so the gap can belong to the subsequent join (e.g., 'do').
	saveI := p.i
	lead := p.collectGap() // GAP between ')' and '->' (if any)
	if p.match(ARROW) {
		arrowTok := p.prev()
		post, err := p.takeReqGap(arrowTok, incMsg) // GAP immediately after '->'
		if err != nil {
			return nil, err
		}
		r, err := p.expr(0)
		if err != nil {
			return nil, err
		}
		r = p.attachAnnotFrom(mergeAnn(lead, post), r)
		return r, nil // parsed type (one node)
	}
	// No arrow: put the parser back so the gap isn't consumed.
	p.i = saveI
	return p.mkLeaf("id", -1, "Any"), nil // single synthetic node
}

// Small shared header for fun/oracle: params + optional arrow type
type fnHeader struct{ params, arrow S }

func (p *parser) parseFnHeader(kind string) (fnHeader, error) {
	ps, err := p.params()
	if err != nil {
		return fnHeader{}, err
	}
	msg := "expected return type after '->'"
	if kind == "oracle" {
		msg = "expected output type after '->'"
	}
	// NOTE: optionalArrowType now internally accepts a gap before '->' and
	// safely restores position if '->' is not present, so no extra work here.
	ar, err := p.optionalArrowType(msg)
	return fnHeader{ps, ar}, err
}

func (p *parser) funExpr(openTok int) (S, error) {
	h, err := p.parseFnHeader("fun")
	if err != nil {
		return nil, err
	}
	// Annotations/newlines before 'do' attach to the function body.
	preDo := p.collectGap()
	body, err := p.parseBlock(true)
	if err != nil {
		return nil, err
	}
	if preDo.ok {
		body = p.attachAnnotFrom(preDo, body)
	}
	node := p.mk("fun", openTok, p.i-1, h.params, h.arrow, body)
	return node, nil
}

func (p *parser) oracleExpr(openTok int) (S, error) {
	h, err := p.parseFnHeader("oracle")
	if err != nil {
		return nil, err
	}
	var src any
	// Collect annotations/newlines before optional 'from'.
	saveI := p.i
	preFrom := p.collectGap()
	matchedFrom := p.match(FROM)
	if matchedFrom {
		fromTok := p.prev()
		postFrom, err := p.takeReqGap(fromTok, "expected expression after 'from'")
		if err != nil {
			return nil, err
		}
		ex, err := p.expr(0)
		if err != nil {
			return nil, err
		}
		ex = p.attachAnnotFrom(mergeAnn(preFrom, postFrom), ex)
		src = ex
	} else {
		// No 'from': restore so the gap belongs to whatever follows the oracle.
		p.i = saveI
		src = p.mk("array", -1, -1) // build only when needed
	}
	body := p.mk("oracle", openTok, p.i-1, h.params, h.arrow, src)
	return body, nil
}

// ─────────────────────── declaration patterns (let/for) ───────────────────

func (p *parser) declPattern() (S, annSrc, error) {
	// Gather stacked PRE immediately before pattern (do not wrap yet).
	pre := annNone()
	for !p.atEnd() && p.peek().Type == ANNOTATION {
		pre = mergeAnn(pre, p.collectAnnotsSrc())
	}
	if p.match(ID) {
		idIdx := p.i - 1
		return p.mk("decl", idIdx, idIdx, tokText(p.prev())), pre, nil
	}
	if p.match(LSQUARE, CLSQUARE) {
		n, err := p.arrayDeclPattern()
		return n, pre, err
	}
	if p.match(LCURLY) {
		n, err := p.objectDeclPattern()
		return n, pre, err
	}
	g := p.peek()
	if p.interactive && p.onlyGapsToEOF() {
		line, col := p.posAfterLastSpan()
		return nil, annNone(), &Error{Kind: DiagIncomplete, Msg: "expected let pattern (id, [], or {})", Line: line, Col: col}
	}
	line, col := p.posAtByte(g.StartByte)
	return nil, annNone(), &Error{Kind: DiagParse, Msg: "expected let pattern (id, [], or {})", Line: line, Col: col}
}

func (p *parser) arrayDeclPattern() (S, error) {
	openTok := p.i - 1
	p.skipNoops()
	if p.match(RSQUARE) {
		return p.mk("darr", openTok, p.i-1), nil
	}

	parts, _, closeTok, err := p.bracketed(
		RSQUARE, "expected ']' in array pattern",
		func(pending annSrc) (S, int, error) {
			pt, a, err := p.declPattern()
			if err != nil {
				return nil, 0, err
			}
			// attach element's own PRE + interstitial PRE to the subpattern itself
			if a.ok {
				pt = p.attachAnnotFrom(a, pt)
			}
			if pending.ok {
				pt = p.attachAnnotFrom(pending, pt)
			}
			return pt, 0, nil
		},
		nil, // POST-after-comma attaches to the element itself
	)
	if err != nil {
		return nil, err
	}
	return p.mk("darr", openTok, closeTok, parts...), nil
}

func (p *parser) objectDeclPattern() (S, error) {
	openTok := p.i - 1
	p.skipNoops()
	if p.match(RCURLY) {
		return p.mk("dobj", openTok, p.i-1), nil
	}

	pairs, _, closeTok, err := p.bracketed(
		RCURLY, "expected '}' in object pattern",
		func(pendingPRE annSrc) (S, int, error) {
			startTok := p.i
			k, aKey, err := p.readKeyString()
			if err != nil {
				return nil, 0, err
			}

			if _, err := p.need(COLON, "expected ':' after key"); err != nil {
				return nil, 0, err
			}
			// B = GAP after ':' (cannot satisfy requirement)
			b, err := p.takeReqGap(p.toks[p.i-1], "expected pattern after ':'")
			if err != nil {
				return nil, 0, err
			}
			pt, aSub, err := p.declPattern()
			if err != nil {
				return nil, 0, err
			}

			// subpattern-local PRE attaches to the pattern node itself
			if aSub.ok {
				pt = p.attachAnnotFrom(aSub, pt)
			}
			// Merge pending PRE + A(from key) + B + C + D onto VALUE (the subpattern)
			val := p.normalizeABCD(mergeAnn(pendingPRE, aKey), b, pt)
			return p.mk("pair", startTok, p.i-1, k, val), 0, nil
		},
		func(last S, comma Token, _ string) S {
			// POST-after-comma attaches to VALUE; consume same-line annots.
			if len(last) >= 3 {
				v := last[2].(S)
				v = p.attachSameLineAnnots(v, comma.Line)
				last[2] = v
				return last
			}
			return p.attachSameLineAnnots(last, comma.Line)
		},
	)
	if err != nil {
		return nil, err
	}
	return p.mk("dobj", openTok, closeTok, pairs...), nil
}

// readKeyString allows stacked PRE-annotations (handled recursively).
// Span order: (1) PRE "str" child; (2) "annot" wrapper; (3) final key "str" leaf.
func (p *parser) readKeyString() (S, annSrc, error) {
	// Stackable PRE immediately before key
	var pre annSrc
	for !p.atEnd() && p.peek().Type == ANNOTATION {
		pre = mergeAnn(pre, p.collectAnnotsSrc())
	}
	if p.match(STRING) {
		return p.mkLeaf("str", p.i-1, p.prev().Literal), pre, nil
	}
	t := p.peek()
	if isWordLike(t.Type) {
		p.i++
		name := t.Lexeme
		if s, ok := t.Literal.(string); ok {
			name = s
		}
		return p.mkLeaf("str", p.i-1, name), pre, nil
	}
	g := p.peek()
	if p.interactive && p.onlyGapsToEOF() {
		line, col := p.posAfterLastSpan()
		return nil, annNone(), &Error{Kind: DiagIncomplete, Msg: "expected key", Line: line, Col: col}
	}
	line, col := p.posAtByte(g.StartByte)
	return nil, annNone(), &Error{Kind: DiagParse, Msg: "expected key", Line: line, Col: col}
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
	// 'for let <pattern> in ...'
	if p.match(LET) {
		pt, a, err := p.declPattern()
		if err != nil {
			return nil, err
		}
		// In a for-target, attach the pattern's own PRE to the pattern itself.
		if a.ok {
			pt = p.attachAnnotFrom(a, pt)
		}
		return pt, nil
	}

	// Heuristic: pattern-looking starts — try a decl pattern first.
	switch p.peek().Type {
	case LSQUARE, CLSQUARE, LCURLY, ANNOTATION:
		save := p.i
		pt, a, err := p.declPattern()
		if err == nil {
			if a.ok {
				pt = p.attachAnnotFrom(a, pt)
			}
			return pt, nil
		}
		if p.interactive && IsIncomplete(err) {
			return nil, err
		}
		p.i = save
	}

	// Otherwise parse an expression and require it to be assignable.
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
