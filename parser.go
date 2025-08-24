// parser.go: Pratt parser for MindScript that produces Lisp-style S-expressions.
//
// OVERVIEW
// --------
// This file implements a newline-aware, interactive-ready Pratt parser for the
// MindScript language. The parser consumes the token stream produced by the
// lexer (see lexer.go) and returns a compact, Lisp-y S-expression (AST) where
// each node is encoded as []any with a leading tag string, e.g.:
//
//	("binop", "+", ("int", 1), ("id", "x"))
//
// DEPENDENCIES
// ------------
// • lexer.go — provides:
//   - Token / TokenType definitions and all token kinds used here
//   - Lexer, NewLexer, NewLexerInteractive
//   - IncompleteError and IsIncomplete(err)
//
// • standard library: fmt (for error messages)
//
// PARSING MODE & INTERACTIVITY
// ----------------------------
// Two entry points are provided:
//
//	ParseSExpr(src string)            — normal parsing (unterminated input is a ParseError)
//	ParseSExprInteractive(src string) — REPL-friendly (unterminated input becomes IncompleteError)
//
// In interactive mode, reaching EOF inside a construct (e.g., missing ')', ']', '}', 'end',
// or RHS after an operator/unary) yields IncompleteError with a precise position.
//
// OUTPUT SHAPE (AST)
// ------------------
// The result is always a top-level block:
//
//	("block", expr1, expr2, ...)
//
// Node forms (tags) the parser emits:
//
// Literals & identifiers:
//
//	("id",   name)           // identifiers and builtin TYPE tokens (e.g., "Int") used as names
//	("int",  int64)
//	("num",  float64)
//	("str",  string)
//	("bool", bool)
//	("null")
//
// Unary/postfix:
//
//	("unop", "-",   expr)    // prefix minus
//	("unop", "not", expr)    // logical not
//	("unop", "?",   expr)    // postfix optional operator
//
// Calls / property / indexing:
//
//	("call", callee, arg1, arg2, ...)
//	("idx",  object, indexExpr)                // obj[expr] or obj.<integer> or obj.(expr)
//	("get",  object, ("str", propertyName))    // obj.<id> or obj."string"
//
// Arrays & maps:
//
//	("array", elements...)
//	("map", ("pair",  keyStrExpr, valueExpr)*)
//	("map", ("pair!", keyStrExpr, valueExpr)* ) // key marked required via '!' in literal (e.g., {name!: "x"})
//
// Enum sugar:
//
//	Enum[ e1, e2, ... ] → ("enum", e1, e2, ...)
//
// Functions & oracles:
//
//	fun (p1[: T], p2[: T], ...) [-> Ret] do ... end
//	   → ("fun",   paramsArray, RetOr("id","Any"), bodyBlock)
//	oracle (p1[: T], ...) [-> Out] [from SourceExpr]
//	   → ("oracle", paramsArray, OutOr("id","Any"), SourceOr("array"))
//
// where paramsArray is ("array", ("pair", ("id", name), typeExpr), ...).
//
// Blocks and control:
//
//	do ... end                → ("block", ...)
//	if cond then … elif … else … end
//	   → ("if", ("pair", cond1, thenBlock1), ("pair", cond2, thenBlock2), [, elseBlock])
//	for <target> in <iter> do ... end → ("for", target, iter, body)
//	while cond do ... end             → ("while", cond, body)
//
// Return/break/continue (newline-sensitive):
//
//	If the next token is on the SAME line → take a following expression.
//	Else (or at EOF)                      → implicit null.
//	Examples:
//	  return 1      → ("return", ("int", 1))
//	  return\nx     → ("return", ("null"))
//
// Declaration patterns (used by `let` and `for` targets):
//
//	("decl", name)                        // let x
//	("darr", p1, p2, ...)                 // let [a, b, ...]
//	("dobj", ("pair", keyStrExpr, p), ...) // let {k: p, ...}
//
// Annotations may wrap the next pattern or expression:
//
//	("annot", ("str", text), node)
//
// Operators & precedence (higher binds tighter; '=' and '->' are right-assoc):
//
//	70: * / %
//	60: + -
//	50: < <= > >=
//	40: == !=
//	30: and
//	20: or
//	15: ->
//	10: =
//
// '=' yields ("assign", target, value) and requires an assignable target: one of
// "id", "get", "idx", "decl", "darr", "dobj".
//
// ERROR MODEL
// -----------
// • ParseError  — structural/grammar problems with location.
// • IncompleteError (interactive mode) — source ended while a construct was incomplete.
//
// NOTE ON WHITESPACE RULE FOR CALLS/PARAM LISTS
// -------------------------------------
// The lexer classifies '(' and '[' based on *immediate* preceding whitespace:
//
//   - '('  → LROUND  if there IS preceding whitespace
//     → CLROUND if there is NO preceding whitespace
//   - '['  → LSQUARE if there IS preceding whitespace
//     → CLSQUARE if there is NO preceding whitespace
//
// This distinction lets the parser treat juxtaposition forms as calls/indexing
// without lookbehind, and treat spaced forms as grouping/array literals.
//
// Consequences (important user-facing syntax):
//
//   - Calls and parameter lists require NO space before '(':
//     f(x)         // call: CLROUND
//     fun(x: T)    // function params: CLROUND
//     oracle(x: T) // oracle params: CLROUND
//     Using a space changes the token to LROUND and will NOT be seen as a call.
//     Example: "f (x)" parses as identifier 'f' followed by a grouped expression,
//     not a function call.
//
//   - Indexing requires NO space before '[':
//     arr[i]    // CLSQUARE (indexing)
//     With a space ("arr [i]"), '[' becomes LSQUARE and is NOT treated as indexing.
//
//   - Plain grouping still works with both LROUND/CLROUND:
//     (expr)   // grouping
//     but only CLROUND participates in call/postfix chains.
//
// NOTE ON TOKENS & NEWLINES
// -------------------------
// The parser relies on token positions provided by the lexer. For newline-sensitive
// behavior of return/break/continue, it checks whether the next token shares the same line.
//
// The complete behavior above is sufficient to consume this API without reading the
// implementation details below.
package mindscript

import (
	"fmt"
)

////////////////////////////////////////////////////////////////////////////////
//                                  PUBLIC API
////////////////////////////////////////////////////////////////////////////////

// S is the S-expression node representation used by the parser.
// It is a []any whose first element is a string tag (e.g., "binop", "id", "array").
// Subsequent elements are the node’s children or data.
//
// Common node forms (non-exhaustive):
//
//	("block", n1, n2, ...)
//	("id",   name)            // string name
//	("int",  int64)
//	("num",  float64)
//	("str",  string)
//	("bool", bool)
//	("null")
//	("unop", op, rhs)         // op is string: "-", "not", "?"
//	("binop", op, lhs, rhs)   // op is string: "+", "and", "==", ...
//	("assign", target, value) // "="
//	("call", callee, arg...)
//	("get",  obj, ("str", name))
//	("idx",  obj, indexExpr)
//	("array", ...)
//	("map", ("pair",  keyStrExpr, value)*)
//	("map", ("pair!", keyStrExpr, value)*)
//	("enum", ...)
//	("fun", paramsArray, retTypeExprOrAny, bodyBlock)
//	("oracle", paramsArray, outTypeExprOrAny, sourceExprOrEmptyArray)
//	("if", ("pair", cond, thenBlock)* [, elseBlock])
//	("while", cond, bodyBlock)
//	("for",   targetPatternOrLvalue, iterExpr, bodyBlock)
//	("decl", name) | ("darr", ...) | ("dobj", ("pair", keyStrExpr, pat)*)
//	("annot", ("str", text), wrappedNode)
type S = []any

// L is a small helper to build S-expression nodes. The first argument is the
// string tag; the remaining arguments are appended as children.
func L(tag string, parts ...any) S { return append([]any{tag}, parts...) }

// ParseError reports a non-interactive parse failure with a location.
// Typical causes: unexpected tokens, missing delimiters, invalid assignment target.
type ParseError struct {
	Line, Col int
	Msg       string
}

func (e *ParseError) Error() string {
	return fmt.Sprintf("PARSE ERROR at %d:%d: %s", e.Line, e.Col, e.Msg)
}

// ParseSExpr parses a MindScript source string into an S-expression AST.
// It lexes the source using NewLexer (normal mode) and parses according to the
// grammar summarized in the file header comments.
//
// Returns:
//   - S  — a top-level ("block", ...) node.
//   - error — *ParseError on grammar problems, or lexer errors propagated.
//
// Newline-sensitive control:
//
//	return/break/continue take an expression only if the next token is on the same line;
//	otherwise they default to ("null").
//
// Property/index/call chaining:
//   - Calls:           f(args...)             → ("call", f, args...)
//   - Index:           a[b]                   → ("idx", a, b)
//   - Computed dot:    a.(expr)               → ("idx", a, expr)
//   - Numeric dot:     a.42                   → ("idx", a, ("int", 42))
//   - Named property:  a.name / a."name"      → ("get", a, ("str", "name"))
//
// Operators:
//
//	Precedence and associativity are as listed in the header; "=" and "->" are right-assoc.
//	"=" yields ("assign", target, value) and requires assignable targets.
func ParseSExpr(src string) (S, error) {
	lex := NewLexer(src)
	toks, err := lex.Scan()
	if err != nil {
		return nil, err
	}
	p := &parser{toks: toks}
	return p.program()
}

// ParseSExprWithSpans parses `src` into an S-expression AST and also returns a
// SpanIndex mapping each node (addressed by NodePath) to its source byte span.
//
// Behavior:
//   - The AST is identical to ParseSExpr (stable contract).
//   - Spans are recorded in post-order (children before parent).
//   - Spans are half-open byte intervals [StartByte, EndByte) in `src`.
//   - The top-level ("block", ...) is included.
//
// Errors mirror ParseSExpr; lexer errors and parse errors are propagated unchanged.
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
	idx := BuildSpanIndexPostOrder(ast, p.post) // sidecar index (spans.go)
	return ast, idx, nil
}

// ParseSExprInteractive is identical to ParseSExpr but uses NewLexerInteractive
// and returns *IncompleteError when the input ends mid-construct (e.g., missing
// ')', ']', '}', 'end', or the RHS of an operator/unary).
//
// Use IsIncomplete(err) to detect this case in a REPL and request more input.
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

// Pratt parser state
type parser struct {
	toks        []Token
	i           int
	interactive bool
	post        []Span
}

// tokString returns a stable string for tokens used as identifier-like or key names.
// It prefers Literal when it is a string (e.g., STRING tokens after '.' keep decoded text),
// otherwise falls back to the verbatim Lexeme.
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

// precedence (higher binds tighter)
func lbp(t TokenType) (int, bool) {
	switch t {
	case ARROW:
		return 15, true // low precedence, right-assoc
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
	case ASSIGN: // '=' right-assoc
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
	// Append span for the top-level block: cover all non-EOF tokens.
	// If there are no tokens besides EOF, leave span empty (Start=End=0).
	startTok := 0
	endTok := len(p.toks) - 2 // last non-EOF
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
	// Record the token index at which this expression starts.
	nodeStartTok := p.i
	// ---- prefix ----
	t := p.peek()
	p.i++
	var left S
	switch t.Type {
	case NOOP:
		left = L("noop")

	case ID, TYPE:
		// Identifiers and capitalized built-in types behave the same in expressions.
		left = L("id", tokString(t))

	case ENUM:
		// Enum[ ... ] sugar → ("enum", ...)
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
		// unary operators require a RHS expression
		if p.atEnd() && p.interactive {
			return nil, &IncompleteError{Line: t.Line, Col: t.Col, Msg: "expected expression after unary operator"}
		}
		r, err := p.expr(80)
		if err != nil {
			return nil, err
		}
		left = L("unop", t.Lexeme, r)

	case LROUND, CLROUND:
		inner, err := p.expr(0)
		if err != nil {
			return nil, err
		}
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
		// map literal {k: v, ...} where k is id or string; id may have trailing '!' for required
		if p.match(RCURLY) {
			left = L("map")
		} else {
			var pairs []any
			for {
				k, req, err := p.keyRequired()
				if err != nil {
					return nil, err
				}
				if _, err := p.need(COLON, "expected ':' after key"); err != nil {
					return nil, err
				}
				v, err := p.expr(0)
				if err != nil {
					return nil, err
				}
				tag := "pair"
				if req {
					tag = "pair!"
				}
				pairs = append(pairs, L(tag, k, v))
				if !p.match(COMMA) {
					break
				}
			}
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
		body, err := p.parseBlock(true) // require 'do'
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
		// accept any expression after `from`
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
		// Newline-sensitive behavior; see file header.
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
		// do ... end
		body, err := p.parseBlock(false) // 'do' already consumed
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
		if tag, _ := pat[0].(string); tag == "darr" || tag == "dobj" {
			if p.peek().Type != ASSIGN {
				g := p.peek()
				// If input ended, mark as incomplete
				if p.interactive && g.Type == EOF {
					return nil, &IncompleteError{Line: g.Line, Col: g.Col, Msg: "expected '=' after destructuring let pattern"}
				}
				return nil, &ParseError{Line: g.Line, Col: g.Col, Msg: "expected '=' after destructuring let pattern"}
			}
		}

	case TYPECONS: // "type" <expr>
		if p.atEnd() && p.interactive {
			return nil, &IncompleteError{Line: t.Line, Col: t.Col, Msg: "expected type expression after 'type'"}
		}
		x, err := p.expr(0)
		if err != nil {
			return nil, err
		}
		left = L("type", x)

	case ANNOTATION:
		// wrap next expression
		if p.atEnd() && p.interactive {
			return nil, &IncompleteError{Line: t.Line, Col: t.Col, Msg: "expected expression after annotation"}
		}
		x, err := p.expr(0)
		if err != nil {
			return nil, err
		}
		txt := ""
		if s, ok := t.Literal.(string); ok {
			txt = s
		}
		left = L("annot", L("str", txt), x)

	default:
		if t.Type == EOF && p.interactive {
			return nil, &IncompleteError{Line: t.Line, Col: t.Col, Msg: "unexpected end of input"}
		}
		return nil, &ParseError{Line: t.Line, Col: t.Col, Msg: fmt.Sprintf("unexpected token '%s'", t.Lexeme)}
	}

	// ---- postfix chain: call (), index [], get . ----
	for {
		switch p.peek().Type {
		case QUESTION:
			p.i++
			left = L("unop", "?", left)
			continue
		case CLROUND:
			p.i++
			var args []any
			if !p.match(RROUND) {
				for {
					a, err := p.expr(0)
					if err != nil {
						return nil, err
					}
					args = append(args, a)
					if !p.match(COMMA) {
						break
					}
				}
				if _, err := p.need(RROUND, "expected ')'"); err != nil {
					return nil, err
				}
			}
			left = L("call", append([]any{left}, args...)...)
			continue
		case CLSQUARE:
			p.i++
			idx, err := p.expr(0)
			if err != nil {
				return nil, err
			}
			if _, err := p.need(RSQUARE, "expected ']'"); err != nil {
				return nil, err
			}
			left = L("idx", left, idx)
			continue
		case PERIOD:
			p.i++
			// obj.(EXPR) → idx(obj, EXPR)
			if p.match(LROUND) || p.match(CLROUND) {
				ex, err := p.expr(0)
				if err != nil {
					return nil, err
				}
				if _, err := p.need(RROUND, "expected ')' after computed property"); err != nil {
					return nil, err
				}
				left = L("idx", left, ex)
				continue
			}
			if p.match(INTEGER) { // obj.90 → idx(obj, 90)
				left = L("idx", left, L("int", p.prev().Literal))
				continue
			}
			if p.match(ID) || p.match(STRING) {
				left = L("get", left, L("str", tokString(p.prev())))
				continue
			}
			g := p.peek()
			return nil, &ParseError{Line: g.Line, Col: g.Col, Msg: "expected property name, integer, or '(expr)' after '.'"}
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
		if op.Type == ASSIGN || op.Type == ARROW { // right-assoc
			nextBP = bp
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
	// On completion of this node, append its span (post-order).
	// Use tokens[nodeStartTok ... p.i-1].
	if p.i-1 >= nodeStartTok && nodeStartTok < len(p.toks) {
		endTok := p.i - 1
		// Guard against EOF being the only thing consumed (shouldn't happen).
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

// ---------- helpers ----------

func (p *parser) arrayLiteralAfterOpen() (S, error) {
	// We’ve already consumed the opening '['
	if p.match(RSQUARE) {
		return L("array"), nil
	}
	var elems []any
	for {
		it, err := p.expr(0)
		if err != nil {
			return nil, err
		}
		elems = append(elems, it)
		if !p.match(COMMA) {
			break
		}
	}
	if _, err := p.need(RSQUARE, "expected ']'"); err != nil {
		return nil, err
	}
	return L("array", elems...), nil
}

func (p *parser) params() (S, error) {
	if _, err := p.need(CLROUND, "expected '(' to start parameters"); err != nil {
		return nil, err
	}
	if p.match(RROUND) {
		return L("array"), nil
	}
	var ps []any
	for {
		idTok, err := p.need(ID, "expected parameter name")
		if err != nil {
			return nil, err
		}
		var t any = L("id", "Any")
		if p.match(COLON) {
			if p.atEnd() && p.interactive {
				return nil, &IncompleteError{Line: idTok.Line, Col: idTok.Col, Msg: "expected type after ':'"}
			}
			e, err := p.expr(0) // parse type as expression
			if err != nil {
				return nil, err
			}
			t = e
		}
		ps = append(ps, L("pair", L("id", tokString(idTok)), t))
		if !p.match(COMMA) {
			break
		}
	}
	if _, err := p.need(RROUND, "expected ')' after parameters"); err != nil {
		return nil, err
	}
	return L("array", ps...), nil
}

// ifExpr builds: ("if", ("pair", cond1, thenBlk1), ("pair", cond2, thenBlk2), ..., [elseBlk?])
// NOTE: The final element, if present, is a bare ("block", ...) for 'else', not wrapped in a tag.
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

// Parse a block; if requireDo is true, assert a leading 'do'.
// Always requires a trailing 'end'.
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
	// while COND do ... end
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

	// If the next token clearly starts a pattern, parse a *declaration* pattern
	// (implicit binding) just like Python: `for [k, v] in ...` or `for {k:x} in ...`.
	switch p.peek().Type {
	case LSQUARE, CLSQUARE, LCURLY, ANNOTATION:
		save := p.i
		pt, err := p.declPattern()
		if err == nil {
			return pt, nil // e.g., ("darr", ...), ("dobj", ...)
		}
		// In interactive mode, bubble up incompleteness instead of falling back
		if p.interactive && IsIncomplete(err) {
			return nil, err
		}
		p.i = save // fall back below if pattern parse fails
	}

	// Otherwise, parse a simple prim+postfix expression and require it's assignable
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

	// Bare id → implicit declaration, same as before
	if e[0].(string) == "id" {
		return L("decl", e[1].(string)), nil
	}
	return e, nil
}

func assignable(n S) bool {
	if len(n) == 0 {
		return false
	}
	switch n[0] {
	case "id", "get", "idx", "decl", "darr", "dobj":
		return true
	default:
		return false
	}
}

// ----- Declaration patterns (with annotation wrapper) -----

func (p *parser) declPattern() (S, error) {
	// Allow stacked line-leading annotations to wrap the next pattern.
	if anns := p.consumeAnnotations(); len(anns) > 0 {
		sub, err := p.declPattern()
		if err != nil {
			return nil, err
		}
		return p.wrapWithAnnotations(anns, sub), nil
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

func (p *parser) arrayDeclPattern() (S, error) {
	if p.match(RSQUARE) {
		return L("darr"), nil
	}
	var parts []any
	for {
		pt, err := p.declPattern()
		if err != nil {
			return nil, err
		}
		parts = append(parts, pt)
		if !p.match(COMMA) {
			break
		}
	}
	if _, err := p.need(RSQUARE, "expected ']' in array pattern"); err != nil {
		return nil, err
	}
	return L("darr", parts...), nil
}

// read a key token and turn it into ("str", key)
func (p *parser) readKeyString() (S, error) {
	// Allow stacked annotation(s) directly in front of the key.
	if anns := p.consumeAnnotations(); len(anns) > 0 {
		k, err := p.readKeyString()
		if err != nil {
			return nil, err
		}
		return p.wrapWithAnnotations(anns, k), nil
	}

	// Quoted key.
	if p.match(STRING) {
		return L("str", p.prev().Literal), nil
	}

	// Any word-like token (identifier, builtin TYPE, keywords, true/false/null).
	t := p.peek()
	if isWordLike(t.Type) {
		p.i++
		name := t.Lexeme
		if s, ok := t.Literal.(string); ok { // ID/TYPE/ENUM may carry string Literal
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

// isWordLike reports tokens that can stand in for bare string keys.
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

func (p *parser) objectDeclPattern() (S, error) {
	if p.match(RCURLY) {
		return L("dobj"), nil
	}
	var pairs []any
	for {
		key, err := p.readKeyString()
		if err != nil {
			return nil, err
		}
		if _, err := p.need(COLON, "expected ':' after key in object pattern"); err != nil {
			return nil, err
		}
		pt, err := p.declPattern()
		if err != nil {
			return nil, err
		}
		pairs = append(pairs, L("pair", key, pt))
		if !p.match(COMMA) {
			break
		}
	}
	if _, err := p.need(RCURLY, "expected '}' in object pattern"); err != nil {
		return nil, err
	}
	return L("dobj", pairs...), nil
}

// --- small helpers for annotations ---
func (p *parser) consumeAnnotations() []string {
	var texts []string
	for p.match(ANNOTATION) {
		txt := ""
		if s, ok := p.prev().Literal.(string); ok {
			txt = s
		}
		texts = append(texts, txt)
	}
	return texts
}

func (p *parser) wrapWithAnnotations(texts []string, node S) S {
	// Outer to inner: the first annotation encountered should wrap outermost,
	// so we apply in reverse to keep order intuitive for stacked blocks.
	for i := len(texts) - 1; i >= 0; i-- {
		node = L("annot", L("str", texts[i]), node)
	}
	return node
}
