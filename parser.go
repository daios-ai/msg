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
// NEW IN THIS VERSION
// -------------------
// Liberal spacing inside delimiters + precise error locations **and**
// consistent POST-annotation attachment across containers.
//
//  1. The parser **skips NOOP tokens inside delimited constructs** so that blank
//     lines behave like whitespace within:
//     [ … ]  { … }  f( … )  fun( … )  a[ … ]  obj.( … )  ( … )
//     NOOP at top level or in blocks still parses as ("noop").
//
// 2) **Precise error locations:** byte-accurate carets via lexer offsets.
//
//   - Missing token (e.g. expected ')'): caret after the last completed node.
//
//   - Unexpected token present: caret at the start of that token.
//
//   - Interactive incomplete errors follow the same policy.
//
//     3. **POST annotations in containers** (arrays, maps, type maps, and
//     destructuring patterns) correctly attach to the *preceding* element/key/
//     value even when a **comma or colon** appears between the expression and
//     the '#' on the same line. Concretely:
//     [ 1, # belongs to 1
//     2 # belongs to 2 ]
//     { a: # belongs to key a
//     1, # belongs to value 1
//     b: 2 }
//     The parser accomplishes this by:
//
//   - Deciding PRE vs POST while scanning left on the same line and
//     **skipping separators ',' and ':'**.
//
//   - When inside containers, **absorbing trailing POST annotations** that
//     appear *after* the separator and re-wrapping the completed element/
//     key/value with ("annot", ("str","<text"), node).
//
// What the parser returns
// -----------------------
// The AST is a tree of S-expressions: []any whose first element is a string tag.
// Examples (non-exhaustive):
//
//	("block", n1, n2, ...)
//	("id",   name)               // string
//	("int",  int64)              // from INTEGER
//	("num",  float64)            // from NUMBER
//	("str",  string)             // decoded literal
//	("bool", bool)               // from BOOLEAN
//	("null")                     // from NULL
//
//	("unop", op, rhs)            // prefix "-" or "not"; postfix "?"  (op is string)
//	("binop", op, lhs, rhs)      // "+", "-", "*", "/", "%", comparisons, "==", "!=", "and", "or"
//	("assign", target, value)    // "=" (right-assoc)
//
//	("call", callee, arg1, arg2, ...)
//	("get",  obj, ("str", name))                  // property: obj.name or obj."name"
//	("idx",  obj, indexExpr)                      // obj[expr] or obj.(expr) or obj.12
//
//	("array", e1, e2, ...)
//	("map",   ("pair",  keyStrExpr, value)*)
//	("map",   ("pair!", keyStrExpr, value)*)      // required-field (key! : value)
//	("enum",  item1, item2, ...)                  // from Enum[ ... ]
//
//	("fun",     paramsArray, retTypeExprOrAny, bodyBlock)
//	("oracle",  paramsArray, outTypeExprOrAny, sourceExpr)
//	("module",  nameExpr, bodyBlock)
//
//	("if", ("pair", cond1, thenBlk1), ..., elseBlk?)
//	("while", cond, bodyBlock)
//	("for",   targetPatternOrLvalue, iterExpr, bodyBlock)
//
//	// Declaration patterns (used by 'let' and 'for' targets):
//	("decl", name)
//	("darr", p1, p2, ...)
//	("dobj", ("pair", keyStrExpr, subPattern), ...)
//
//	// Annotations (from '#'-blocks). POST is encoded by a leading "<" in text.
//	("annot", ("str", textOr<text>), wrappedNode)
//
// Annotation encoding
// -------------------
//   - PRE annotations are stored as-is:        ("annot", ("str", "note"), expr/pattern)
//   - POST annotations are prefixed with "<":  ("annot", ("str", "<note"), expr)
//
// Node spans
// ----------
// For tooling, the parser records byte spans (StartByte/EndByte) for each node
// in post-order. Use ParseSExprWithSpans to receive a sidecar *SpanIndex that
// maps each node to its original source span (see spans.go).
//
// Dependencies
// ------------
//   - lexer.go
//   - errors.go (*Error, DiagParse, DiagIncomplete, IsIncomplete)
//   - spans.go (Span, SpanIndex, BuildSpanIndexPostOrder)
//
// Whitespace-sensitive behavior consumed from the lexer
// -----------------------------------------------------
//   - CLROUND vs LROUND: grouping for both; only CLROUND starts a call chain.
//   - CLSQUARE vs LSQUARE: array literal for both; only CLSQUARE is indexing.
//   - PERIOD chains: after '.', accept ( (…), INTEGER, ID, STRING ) per rules.
//   - Annotation PRE vs POST is line-sensitive; see classifier policy below.
//   - NOOP: skipped like whitespace inside delimiters; parsed as ("noop") at
//     top level and inside blocks.
//
// Grammar sketch (informal)
// -------------------------
//
//	program      := expr* EOF
//	exp          := prefix (postfix | infix)*
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
//	block        := "do" blockBody "end"
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
//
// Errors
// ------
//   - HARD errors are returned as *Error with Kind=DiagParse (or DiagLex upstream).
//   - In interactive mode (ParseSExprInteractive), unfinished constructs at EOF
//     return *Error{Kind:DiagIncomplete}.
//   - Error placement:
//   - Missing token → caret after last completed node (byte-accurate).
//   - Unexpected token present → caret at lookahead start.
//
// PUBLIC API
// ----------
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
	ast, err := p.program()
	if err != nil {
		return nil, nil, err
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

// --- small utilities --------------------------------------------------------

func (p *parser) atEnd() bool { return p.peek().Type == EOF }
func (p *parser) peek() Token {
	if p.i >= len(p.toks) {
		return p.toks[len(p.toks)-1]
	}
	return p.toks[p.i]
}
func (p *parser) prev() Token { return p.toks[p.i-1] }
func (p *parser) nextTokenIsOnSameLine(as Token) bool {
	if p.atEnd() {
		return false
	}
	return p.peek().Line == as.Line
}
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
		line, col := p.afterLastPos()
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
func (p *parser) afterLastPos() (int, int) {
	if p.lastSpanEndTok >= 0 && p.lastSpanEndTok < len(p.toks) {
		endB := p.toks[p.lastSpanEndTok].EndByte
		return p.posAtByte(endB)
	}
	g := p.peek()
	return g.Line, g.Col + 1
}

// --- token / precedence helpers --------------------------------------------

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

// IMPORTANT: skip separators when classifying PRE vs POST.
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
			if t.Type == COMMA || t.Type == COLON {
				continue
			}
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

// ---- NOOP handling ---------------------------------------------------------

func (p *parser) skipNoops() {
	for !p.atEnd() && p.peek().Type == NOOP {
		p.i++
	}
}

// ---- shared annotation absorption helpers ----------------------------------

// absorbPostAnnotsToLeft rewraps 'base' with any number of POST annotations
// that appear *immediately at p.i* and are classified as POST (i.e., not PRE).
// It assumes the caller is in a context where separators (',' or ':') have
// already been handled appropriately and where such POSTs semantically belong
// to 'base'. Spans are widened from baseStartTok to each annotation token.
func (p *parser) absorbPostAnnotsToLeft(base S, baseStartTok int) S {
	for !p.atEnd() && p.peek().Type == ANNOTATION && !p.annotationIsPreAt(p.i) {
		aTok := p.i
		a := p.peek()
		p.i++
		txt := ""
		if s, ok := a.Literal.(string); ok {
			txt = s
		}
		child := L("str", "<"+txt)
		// span for child ("str")
		p.emitSpanByTok(aTok, aTok)
		base = L("annot", child, base)
		// widen to include the annotation token
		p.emitSpanByTok(baseStartTok, aTok)
		p.skipNoops()
	}
	return base
}

// ---- program / expressions -------------------------------------------------

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
	tokIndexOfThis := p.i
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
		endTok := p.lastSpanEndTok
		if endTok < 0 {
			endTok = tokIndexOfThis
		}
		leftStartTok = tokIndexOfThis
		p.emitSpanByTok(leftStartTok, endTok)

	case LROUND, CLROUND:
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
		p.skipNoops()
		if p.match(RCURLY) {
			left = L("map")
			p.emitSpanByTok(tokIndexOfThis, p.i-1)
			leftStartTok = tokIndexOfThis
		} else {
			var pairs []any
			for {
				p.skipNoops()
				pairStartTok := p.i

				// key
				k, req, err := p.keyRequired()
				if err != nil {
					return nil, err
				}
				keyStartTok := p.lastSpanStartTok

				p.skipNoops()
				if _, err := p.need(COLON, "expected ':' after key"); err != nil {
					return nil, err
				}

				// absorb POST(s) that belong to the key (appear after ':')
				p.skipNoops()
				k = p.absorbPostAnnotsToLeft(k, keyStartTok)

				// value
				p.skipNoops()
				v, err := p.expr(0)
				if err != nil {
					return nil, err
				}

				// value may have a trailing comma with POSTs after it
				p.skipNoops()
				hadComma := p.match(COMMA)
				if hadComma {
					valStartTok := p.lastSpanStartTok
					p.skipNoops()
					v = p.absorbPostAnnotsToLeft(v, valStartTok)
				}

				tag := "pair"
				if req {
					tag = "pair!"
				}
				pr := L(tag, k, v)
				endTok := p.lastSpanEndTok
				p.emitSpanByTok(pairStartTok, endTok)
				pairs = append(pairs, pr)

				p.skipNoops()
				if hadComma {
					// trailing comma before '}'
					if p.peek().Type == RCURLY {
						break
					}
					continue
				}
				// no comma: this pair must be the last
				break
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
		if p.atEnd() && p.interactive {
			line, col := p.posAtByte(t.StartByte)
			return nil, &Error{Kind: DiagIncomplete, Msg: "expected module name expression", Line: line, Col: col}
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
		if !p.nextTokenIsOnSameLine(t) {
			switch t.Type {
			case RETURN:
				left = L("return", L("null"))
			case BREAK:
				left = L("break", L("null"))
			default:
				left = L("continue", L("null"))
			}
			p.emitSpanByTok(-1, -1) // child
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
		leftStartTok = tokIndexOfThis

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
			p.emitSpanByTok(tokIndexOfThis, tokIndexOfThis) // child ("str")
			x, err := p.expr(0)
			if err != nil {
				return nil, err
			}
			left = L("annot", L("str", txt), x)
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
			openTok := p.i
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
			p.emitSpanByTok(leftStartTok, p.i-1)
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
			p.emitSpanByTok(leftStartTok, p.i-1)
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
				p.emitSpanByTok(leftStartTok, p.i-1)
				continue
			}
			if p.match(INTEGER) {
				intTok := p.i - 1
				intNode := L("int", p.prev().Literal)
				p.emitSpanByTok(intTok, intTok)
				left = L("idx", left, intNode)
				p.emitSpanByTok(leftStartTok, intTok)
				continue
			}
			if p.match(ID) || p.match(STRING) {
				propTok := p.i - 1
				prop := L("str", tokString(p.prev()))
				p.emitSpanByTok(propTok, propTok)
				left = L("get", left, prop)
				p.emitSpanByTok(leftStartTok, propTok)
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
			child := L("str", "<"+txt)
			p.emitSpanByTok(aTok, aTok)
			left = L("annot", child, left)
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
	}

	return left, nil
}

// Arrays: support POST after comma: [1, #post  …]
func (p *parser) arrayLiteralAfterOpen() (S, error) {
	p.skipNoops()
	if p.match(RSQUARE) {
		return L("array"), nil
	}
	var elems []any
	for {
		p.skipNoops()
		elemStartTok := p.i
		it, err := p.expr(0)
		if err != nil {
			return nil, err
		}
		p.skipNoops()
		if p.match(COMMA) {
			// absorb POSTs that belong to the previous element (after comma)
			p.skipNoops()
			it = p.absorbPostAnnotsToLeft(it, elemStartTok)
			elems = append(elems, it)
			p.skipNoops()
			if p.peek().Type == RSQUARE {
				break
			}
			continue
		}
		elems = append(elems, it)
		break
	}
	p.skipNoops()
	if _, err := p.need(RSQUARE, "expected ']'"); err != nil {
		return nil, err
	}
	return L("array", elems...), nil
}

// Params: unchanged logic + skip NOOP within.
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
		p.emitSpanByTok(idIdx, endTokForPair)
		ps = append(ps, pr)
		p.skipNoops()

		if !p.match(COMMA) {
			break
		}
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

// ----- Declaration patterns (with optional PRE-annotation wrapper) -----

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

type preAnn struct {
	txt    string
	tokIdx int
}

func (p *parser) declPattern() (S, error) {
	if ann, ok, err := p.takeOnePreAnnotation(); err != nil {
		return nil, err
	} else if ok {
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

// --- array destructuring pattern: [p1, p2,] with POST after comma ---
func (p *parser) arrayDeclPattern() (S, error) {
	openTok := p.i - 1
	p.skipNoops()
	if p.match(RSQUARE) {
		node := L("darr")
		p.emitSpanByTok(openTok, p.i-1)
		return node, nil
	}
	var parts []any
	for {
		p.skipNoops()
		partStartTok := p.i
		pt, err := p.declPattern()
		if err != nil {
			return nil, err
		}
		p.skipNoops()

		if p.match(COMMA) {
			p.skipNoops()
			pt = p.absorbPostAnnotsToLeft(pt, partStartTok)
			parts = append(parts, pt)
			p.skipNoops()
			if p.peek().Type == RSQUARE {
				break
			}
			continue
		}
		parts = append(parts, pt)
		break
	}
	p.skipNoops()
	if _, err := p.need(RSQUARE, "expected ']' in array pattern"); err != nil {
		return nil, err
	}
	node := L("darr", parts...)
	p.emitSpanByTok(openTok, p.i-1)
	return node, nil
}

// --- object destructuring pattern: {k: p, ...} with POST-after-':'/',' ---
func (p *parser) objectDeclPattern() (S, error) {
	openTok := p.i - 1
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
		keyStartTok := p.lastSpanStartTok

		p.skipNoops()
		if _, err := p.need(COLON, "expected ':' after key in object pattern"); err != nil {
			return nil, err
		}

		// key POST(s) after ':'
		p.skipNoops()
		key = p.absorbPostAnnotsToLeft(key, keyStartTok)

		p.skipNoops()
		pt, err := p.declPattern()
		if err != nil {
			return nil, err
		}

		p.skipNoops()
		hadComma := p.match(COMMA)
		if hadComma {
			valStartTok := p.lastSpanStartTok
			p.skipNoops()
			pt = p.absorbPostAnnotsToLeft(pt, valStartTok)
		}

		pr := L("pair", key, pt)
		endTok := p.lastSpanEndTok
		p.emitSpanByTok(pairStartTok, endTok)
		pairs = append(pairs, pr)

		p.skipNoops()
		if hadComma {
			if p.peek().Type == RCURLY {
				break
			}
			continue
		}
		break
	}
	p.skipNoops()
	if _, err := p.need(RCURLY, "expected '}' in object pattern"); err != nil {
		return nil, err
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
