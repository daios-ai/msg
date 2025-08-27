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
//   - Support an "interactive" mode that surfaces *IncompleteError* at EOF
//     instead of hard parse errors, suitable for REPLs.
//
// NEW IN THIS VERSION (liberal spacing inside delimiters)
// ------------------------------------------------------
// The parser **skips NOOP tokens inside delimited constructs** so that blank
// lines are treated like whitespace within:
//   - array literals:            [ … ]
//   - map/object literals:       { … }
//   - call argument lists:       f( … )
//   - parameter lists:           fun(x: T, …)
//   - bracket indices:           a[ … ]
//   - computed properties:       obj.( … )
//   - grouping parentheses:      ( … )
//
// The semantics of NOOP at the top level and inside blocks are unchanged: a
// NOOP still parses as ("noop") outside the above delimited contexts.
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
//	("fun",    paramsArray, retTypeExprOrAny, bodyBlock)
//	("oracle", paramsArray, outTypeExprOrAny, sourceExpr)  // optional 'from' expression
//
//	("if", ("pair", cond1, thenBlk1), ..., elseBlk?)       // if/elif/else
//	("while", cond, bodyBlock)
//	("for",   targetPatternOrLvalue, iterExpr, bodyBlock)
//
//	// Declaration patterns (used by 'let' and 'for' targets):
//	("decl", name)
//	("darr", p1, p2, ...)                               // array destructure
//	("dobj", ("pair", keyStrExpr, subPattern), ...)     // object destructure
//
//	// Annotations (from '#'-blocks). The final boolean indicates PRE vs POST.
//	("annot", ("str", text), wrappedNode, true|false)
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
//   - NewLexer / NewLexerInteractive
//   - Token / TokenType definitions and the tokenization rules
//   - LexError and IncompleteError (the parser may return IncompleteError)
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
//     that follows. The AST encodes this as ("annot", ("str", text), node, isPre).
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
//   - *ParseError* for grammatical mistakes with precise (line,col).
//   - *IncompleteError* (from lexer.go) is propagated in interactive mode at EOF
//     when a continuation is expected (e.g., after 'then', before 'end', inside
//     unmatched ')', ']', or unterminated constructs).
//   - *LexError* may be returned from the lexer prior to parsing.
//
// PUBLIC API
// ----------
// Everything below (until `//// END_OF_PUBLIC`) is the public, fully documented
// surface of this module. The implementation details follow afterward.
package mindscript

import (
	"fmt"
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

// ParseError reports a non-interactive parse failure at a specific location.
//
// Typical causes:
//   - unexpected token (e.g., missing ',' or ')')
//   - invalid assignment target
//   - forbidden placement of annotations (e.g., stacked PRE annotations)
//
// In interactive mode (see ParseSExprInteractive), many “need more input”
// situations return *IncompleteError* (from lexer.go) instead of *ParseError*.
type ParseError struct {
	Line, Col int // 1-based line, 0-based column at the error site
	Msg       string
}

func (e *ParseError) Error() string {
	return fmt.Sprintf("PARSE ERROR at %d:%d: %s", e.Line, e.Col, e.Msg)
}

// ParseSExpr parses a complete MindScript source string and returns its AST.
//
// Behavior:
//   - Runs the whitespace-sensitive lexer (normal mode) and then the Pratt parser.
//   - Returns the fully formed S-expression ("block", ...root items...) even for
//     a single top-level expression.
//   - On lexical problems returns *LexError*. On parse mistakes returns *ParseError*.
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
// Errors:
//   - Does not return *IncompleteError*; unterminated constructs are hard errors.
//   - Never panics on malformed input.
func ParseSExpr(src string) (S, error) {
	lex := NewLexer(src)
	toks, err := lex.Scan()
	if err != nil {
		return nil, err
	}
	p := &parser{toks: toks}
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
	p := &parser{toks: toks}
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
//     *IncompleteError* at EOF.
//   - The parser also returns *IncompleteError* at EOF for unfinished constructs
//     such as: required ')' or ']' missing, a needed 'end', or after "then"/"->".
//
// Callers can detect this by IsIncomplete(err) (from lexer.go) and request more
// input lines from the user.
func ParseSExprInteractive(src string) (S, error) {
	lex := NewLexerInteractive(src)
	toks, err := lex.Scan()
	if err != nil {
		return nil, err
	}
	p := &parser{toks: toks, interactive: true}
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
	post        []Span
}

// skipNoops consumes any number of NOOP tokens (blank-line groups). This is
// used inside delimited contexts so that blank lines behave like whitespace.
func (p *parser) skipNoops() {
	for !p.atEnd() && p.peek().Type == NOOP {
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
	if p.interactive && g.Type == EOF {
		return Token{}, &IncompleteError{Line: g.Line, Col: g.Col, Msg: msg}
	}
	return Token{}, &ParseError{Line: g.Line, Col: g.Col, Msg: msg}
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
	nodeStartTok := p.i

	// ---- prefix ----
	t := p.peek()
	p.i++
	var left S
	switch t.Type {
	case NOOP:
		left = L("noop")

	case ID, TYPE:
		left = L("id", tokString(t))

	case ENUM:
		if p.peek().Type == LSQUARE || p.peek().Type == CLSQUARE {
			p.i++
			arr, err := p.arrayLiteralAfterOpen()
			if err != nil {
				return nil, err
			}
			it := make([]any, 0, len(arr)-1)
			for i := 1; i < len(arr); i++ {
				it = append(it, arr[i])
			}
			left = L("enum", it...)
		} else {
			left = L("id", tokString(t))
		}

	case INTEGER:
		left = L("int", t.Literal)
	case NUMBER:
		left = L("num", t.Literal)
	case STRING:
		left = L("str", t.Literal)
	case BOOLEAN:
		left = L("bool", t.Literal)
	case NULL:
		left = L("null")

	case MINUS, NOT:
		if p.atEnd() && p.interactive {
			return nil, &IncompleteError{Line: t.Line, Col: t.Col, Msg: "expected expression after unary operator"}
		}
		r, err := p.expr(80)
		if err != nil {
			return nil, err
		}
		left = L("unop", t.Lexeme, r)

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

	case LSQUARE, CLSQUARE:
		a, err := p.arrayLiteralAfterOpen()
		if err != nil {
			return nil, err
		}
		left = a

	case LCURLY:
		p.skipNoops()
		if p.match(RCURLY) {
			left = L("map")
		} else {
			var pairs []any
			for {
				p.skipNoops()
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
				pairs = append(pairs, L(tag, k, v))
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
		}

	case FUNCTION:
		params, err := p.params()
		if err != nil {
			return nil, err
		}
		var ret any = L("id", "Any")
		if p.match(ARROW) {
			if p.atEnd() && p.interactive {
				return nil, &IncompleteError{Line: t.Line, Col: t.Col, Msg: "expected return type after '->'"}
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

	case ORACLE:
		params, err := p.params()
		if err != nil {
			return nil, err
		}
		var out any = L("id", "Any")
		if p.match(ARROW) {
			if p.atEnd() && p.interactive {
				return nil, &IncompleteError{Line: t.Line, Col: t.Col, Msg: "expected output type after '->'"}
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
				return nil, &IncompleteError{Line: t.Line, Col: t.Col, Msg: "expected expression after 'from'"}
			}
			ex, err := p.expr(0)
			if err != nil {
				return nil, err
			}
			src = ex
		}
		left = L("oracle", params, out, src)

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
		}

	case IF:
		thenIf, err := p.ifExpr()
		if err != nil {
			return nil, err
		}
		left = thenIf

	case DO:
		body, err := p.parseBlock(false)
		if err != nil {
			return nil, err
		}
		left = body

	case FOR:
		f, err := p.forExpr()
		if err != nil {
			return nil, err
		}
		left = f

	case WHILE:
		w, err := p.whileExpr()
		if err != nil {
			return nil, err
		}
		left = w

	case LET:
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
					return nil, &IncompleteError{Line: g.Line, Col: g.Col, Msg: "expected '=' after destructuring let pattern"}
				}
				return nil, &ParseError{Line: g.Line, Col: g.Col, Msg: "expected '=' after destructuring let pattern"}
			}
		}

	case TYPECONS:
		if p.atEnd() && p.interactive {
			return nil, &IncompleteError{Line: t.Line, Col: t.Col, Msg: "expected type expression after 'type'"}
		}
		x, err := p.expr(0)
		if err != nil {
			return nil, err
		}
		left = L("type", x)

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
				return nil, &ParseError{
					Line: next.Line, Col: next.Col,
					Msg: "multiple consecutive pre-annotations are not allowed; combine them",
				}
			}

			if p.atEnd() && p.interactive {
				return nil, &IncompleteError{Line: t.Line, Col: t.Col, Msg: "expected expression after annotation"}
			}
			x, err := p.expr(0)
			if err != nil {
				return nil, err
			}
			left = L("annot", L("str", txt), x, true)
		} else {
			return nil, &ParseError{Line: t.Line, Col: t.Col, Msg: "post-annotation has no preceding expression to attach"}
		}

	default:
		if t.Type == EOF && p.interactive {
			return nil, &IncompleteError{Line: t.Line, Col: t.Col, Msg: "unexpected end of input"}
		}
		return nil, &ParseError{Line: t.Line, Col: t.Col, Msg: fmt.Sprintf("unexpected token '%s'", t.Lexeme)}
	}

	// ---- postfix chain ----
	for {
		switch p.peek().Type {
		case QUESTION:
			p.i++
			left = L("unop", "?", left)
			continue
		case CLROUND:
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
			continue
		case CLSQUARE:
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
				continue
			}
			if p.match(INTEGER) {
				left = L("idx", left, L("int", p.prev().Literal))
				continue
			}
			if p.match(ID) || p.match(STRING) {
				left = L("get", left, L("str", tokString(p.prev())))
				continue
			}
			g := p.peek()
			return nil, &ParseError{Line: g.Line, Col: g.Col, Msg: "expected property name, integer, or '(expr)' after '.'"}

		case ANNOTATION:
			// POST attaches to 'left'; PRE belongs to the next thing.
			if p.annotationIsPreAt(p.i) {
				break
			}
			a := p.peek()
			p.i++
			txt := ""
			if s, ok := a.Literal.(string); ok {
				txt = s
			}
			left = L("annot", L("str", txt), left, false)
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
			return nil, &ParseError{Line: op.Line, Col: op.Col, Msg: "invalid assignment target"}
		}

		if p.atEnd() && p.interactive {
			return nil, &IncompleteError{Line: op.Line, Col: op.Col, Msg: "expected expression after operator"}
		}
		right, err := p.expr(nextBP)
		if err != nil {
			return nil, err
		}
		if op.Type == ASSIGN {
			left = L("assign", left, right)
		} else {
			left = L("binop", op.Lexeme, left, right)
		}
	}

	// Record span for this node (post-order).
	if p.i-1 >= nodeStartTok && nodeStartTok < len(p.toks) {
		endTok := p.i - 1
		if endTok >= 0 && endTok < len(p.toks) {
			p.post = append(p.post, Span{
				StartByte: p.toks[nodeStartTok].StartByte,
				EndByte:   p.toks[endTok].EndByte,
			})
		} else {
			p.post = append(p.post, Span{})
		}
	} else {
		p.post = append(p.post, Span{})
	}
	return left, nil
}

func (p *parser) arrayLiteralAfterOpen() (S, error) {
	p.skipNoops()
	if p.match(RSQUARE) {
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
	return L("array", elems...), nil
}

func (p *parser) params() (S, error) {
	if _, err := p.need(CLROUND, "expected '(' to start parameters"); err != nil {
		return nil, err
	}
	p.skipNoops()
	if p.match(RROUND) {
		return L("array"), nil
	}
	var ps []any
	for {
		p.skipNoops()
		idTok, err := p.need(ID, "expected parameter name")
		if err != nil {
			return nil, err
		}
		var t any = L("id", "Any")
		p.skipNoops()
		if p.match(COLON) {
			if p.atEnd() && p.interactive {
				return nil, &IncompleteError{Line: idTok.Line, Col: idTok.Col, Msg: "expected type after ':'"}
			}
			p.skipNoops()
			e, err := p.expr(0)
			if err != nil {
				return nil, err
			}
			t = e
		}
		ps = append(ps, L("pair", L("id", tokString(idTok)), t))
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
	return L("array", ps...), nil
}

// ifExpr builds: ("if", ("pair", cond1, thenBlk1), ... [, elseBlk?])
func (p *parser) ifExpr() (S, error) {
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
	arms := []any{L("pair", cond, thenBlk)}
	for p.match(ELIF) {
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
		arms = append(arms, L("pair", c, b))
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
	for !p.atEnd() && !stop[p.peek().Type] {
		e, err := p.expr(0)
		if err != nil {
			return nil, err
		}
		items = append(items, e)
	}
	return L("block", items...), nil
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
		return nil, &ParseError{Line: g.Line, Col: g.Col, Msg: "invalid for-target (must be id/get/idx/decl/pattern)"}
	}

	if e[0].(string) == "id" {
		return L("decl", e[1].(string)), nil
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
	// Allow line-leading PRE annotations to wrap the next pattern;
	// still disallow multiple consecutive PRE annotations at a single point.
	if anns, err := p.consumePreAnnotationsOrError(); err != nil {
		return nil, err
	} else if len(anns) > 0 {
		sub, err := p.declPattern()
		if err != nil {
			return nil, err
		}
		return p.wrapWithPreAnnotations(anns, sub), nil
	}

	if p.match(ID) {
		return L("decl", tokString(p.prev())), nil
	}
	if p.match(LSQUARE, CLSQUARE) {
		return p.arrayDeclPattern()
	}
	if p.match(LCURLY) {
		return p.objectDeclPattern()
	}
	g := p.peek()
	if p.interactive && g.Type == EOF {
		return nil, &IncompleteError{Line: g.Line, Col: g.Col, Msg: "expected let pattern (id, [], or {})"}
	}
	return nil, &ParseError{Line: g.Line, Col: g.Col, Msg: "expected let pattern (id, [], or {})"}
}

// --- array destructuring pattern: [p1, p2,] ---
func (p *parser) arrayDeclPattern() (S, error) {
	p.skipNoops()
	if p.match(RSQUARE) {
		return L("darr"), nil
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
	return L("darr", parts...), nil
}

// --- object destructuring pattern: {k: p, ... ,} ---
func (p *parser) objectDeclPattern() (S, error) {
	p.skipNoops()
	if p.match(RCURLY) {
		return L("dobj"), nil
	}
	var pairs []any
	for {
		p.skipNoops()
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
		pairs = append(pairs, L("pair", key, pt))
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
	return L("dobj", pairs...), nil
}

// readKeyString parses a key for object patterns/maps, allowing stacked PRE-annotation
// wrapping via recursion (each level allows at most one PRE, but recursion stacks).
func (p *parser) readKeyString() (S, error) {
	// Allow stacked PRE annotation(s) directly in front of the key.
	if anns, err := p.consumePreAnnotationsOrError(); err != nil {
		return nil, err
	} else if len(anns) > 0 {
		k, err := p.readKeyString()
		if err != nil {
			return nil, err
		}
		return p.wrapWithPreAnnotations(anns, k), nil
	}

	if p.match(STRING) {
		return L("str", p.prev().Literal), nil
	}

	t := p.peek()
	if isWordLike(t.Type) {
		p.i++
		name := t.Lexeme
		if s, ok := t.Literal.(string); ok {
			name = s
		}
		return L("str", name), nil
	}

	g := p.peek()
	if p.interactive && g.Type == EOF {
		return nil, &IncompleteError{Line: g.Line, Col: g.Col, Msg: "expected key"}
	}
	return nil, &ParseError{Line: g.Line, Col: g.Col, Msg: "expected key"}
}

func isWordLike(tt TokenType) bool {
	switch tt {
	case ID, TYPE, ENUM, BOOLEAN, NULL,
		AND, OR, NOT, LET, DO, END, RETURN, BREAK, CONTINUE,
		IF, THEN, ELIF, ELSE, FUNCTION, ORACLE, FOR, IN, FROM,
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

// --- small helpers for annotations ---

// consumePreAnnotationsOrError collects at most one PRE annotation at the current point.
func (p *parser) consumePreAnnotationsOrError() ([]string, error) {
	var texts []string
	for !p.atEnd() && p.peek().Type == ANNOTATION && p.annotationIsPreAt(p.i) {
		tok := p.peek()
		if len(texts) > 0 {
			return nil, &ParseError{
				Line: tok.Line, Col: tok.Col,
				Msg: "multiple consecutive pre-annotations are not allowed",
			}
		}
		p.i++
		txt := ""
		if s, ok := tok.Literal.(string); ok {
			txt = s
		}
		texts = append(texts, txt)
	}
	return texts, nil
}

// wrapWithPreAnnotations wraps node with the given texts as PRE annotations.
func (p *parser) wrapWithPreAnnotations(texts []string, node S) S {
	for i := len(texts) - 1; i >= 0; i-- {
		node = L("annot", L("str", texts[i]), node, true)
	}
	return node
}
