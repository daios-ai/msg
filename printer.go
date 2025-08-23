// printer.go: pretty-printers for MindScript ASTs, types, and runtime values.
//
// What this file does
// -------------------
// This module provides the formatting layer for MindScript. It renders three
// kinds of data to human-readable, stable strings:
//
//  1. Parsed source ASTs (S-expressions) → MindScript source code.
//     - Entry points: Pretty(src), FormatSExpr(ast).
//     - Produces whitespace- and newline-stable output with minimal
//     parentheses, based on operator precedence. It understands all
//     statement and expression tags emitted by the parser (e.g. "fun",
//     "oracle", "for", "if/elif/else", "type", "block", "assign",
//     "return/break/continue", arrays, maps, calls, indexing, properties,
//     unary and binary operators).
//     - Annotation nodes ("annot", ("str", text), <node>) become `# ...`
//     line comments immediately above the printed construct.
//     - Property names are emitted bare if they are identifier-like, otherwise
//     quoted. Destructuring declaration patterns ("decl" | "darr" | "dobj")
//     are rendered in a compact, readable form.
//     - Formatting emits no space before '(' for calls and for 'fun(...)'
//     and 'oracle(...)' parameter lists, matching the lexer’s CLROUND rule.
//
//  2. Type ASTs (S-expressions) → compact type strings.
//     - Entry point: FormatType(t).
//     - Supported forms:
//     • ("id", "Any"|"Null"|"Bool"|"Int"|"Num"|"Str"|"Type")
//     • ("unop","?", T)         → prints as `T?`
//     • ("array", T)            → prints as `[T]`
//     • ("map", ("pair"| "pair!", ("str",k), T) ...)
//     Required fields print with a trailing `!` on the key.
//     Key/value annotations (if wrapped in "annot") become `# ...` lines.
//     • ("enum", literalS... )  → prints as `Enum[ ... ]`, where members
//     may be scalars, arrays, or maps.
//     • ("binop","->", A, B)    → prints as `(A) -> B`, flattened across
//     right-associated chains.
//     - Output is stable. Multi-line maps are rendered with sorted keys to
//     avoid visual churn.
//
//  3. Runtime values (Value) → width-aware strings.
//     - Entry point: FormatValue(v).
//     - Scalars print plainly (`null`, `true/false`, numbers, quoted strings).
//     - Arrays and maps prefer a single-line rendering if it fits the
//     `MaxInlineWidth` budget and no elements/keys force multi-line; else
//     they fall back to pretty, multi-line output with indentation.
//     - Map keys are emitted bare if they’re identifier-like, otherwise quoted.
//     - Per-value annotations (Value.Annot) and per-key annotations in maps
//     (MapObject.KeyAnn[name]) are printed as `# ...` header lines.
//     - Functions print as `<fun: a:T -> b:U -> R>` (or `_:Null` for zero-arg).
//     - Types (VTType) are printed by extracting the embedded type AST and
//     delegating to FormatType.
//     - Modules print as `<module: <pretty name>>` when available.
//
// Dependencies (other files)
// --------------------------
// • parser.go
//   - S = []any (AST payload shape)
//   - ParseSExpr(string) (used by Pretty)
//   - AST tags: "block", "fun", "oracle", "for", "while", "if", "then", "elif",
//     "else", "type", "return", "break", "continue", "assign", "array", "map",
//     "pair"/"pair!", "get", "idx", "call", "id", "str", "int", "num", "bool",
//     "null", "unop", "binop", "decl", "darr", "dobj", "annot".
//
// • interpreter.go (runtime model)
//   - Value, ValueTag (VTNull, VTBool, VTInt, VTNum, VTStr, VTArray, VTMap,
//     VTFun, VTType, VTModule, VTHandle)
//   - Fun, TypeValue, MapObject (Entries/KeyAnn/Keys).
//
// • modules.go (module loader)
//   - Module struct and prettySpec(string) (used for VTModule display).
//
// • errors.go (shared errors)
//   - WrapErrorWithSource(err, src) (used by Pretty).
//
// PUBLIC vs PRIVATE layout
// ------------------------
// This file is organized in two blocks:
//  1. PUBLIC: the user-facing constants & functions with thorough docstrings.
//  2. PRIVATE: helper types and functions that implement the printers.
//
// Formatting policy highlights:
//   - Indentation uses **tabs** only (gofmt-style).
//   - Canonical output (`Standardize`) ends with exactly one trailing '\n'.
package mindscript

import (
	"fmt"
	"sort"
	"strconv"
	"strings"
	"unicode"
)

// ==============================
// ========== PUBLIC ============
// ==============================

// MaxInlineWidth controls when arrays/maps are rendered on a single line
// by FormatValue. If the one-line candidate exceeds this many characters,
// or if any element/key is annotated or multiline by nature, the value is
// rendered across multiple lines with indentation.
//
// This setting is read at call time and is safe to change between calls.
var MaxInlineWidth = 80

// IndentWithTabs controls whether pretty output uses tabs (like gofmt) for
// indentation. When false, two spaces are used.
var IndentWithTabs = true

// Pretty parses a MindScript source string and returns a formatted version.
//
// Behavior:
//   - Parses `src` via ParseSExpr (from parser.go). If parsing fails,
//     the error is wrapped with source context via WrapErrorWithSource.
//   - On success, it pretty-prints the resulting AST using FormatSExpr,
//     producing stable, whitespace-normalized code.
//   - Annotation nodes ("annot", ("str", text), X) print as `# text`
//     lines directly above X.
//   - Operators use minimal parentheses according to precedence/associativity:
//   - "->" (lowest), "+", "-", "*", "/", "%", comparisons, "==", "!=", "and", "or"
//   - postfix call/index/get bind tightest; unary ("not", "-") binds tighter than
//     binary arithmetic; assignment is the loosest among term operators.
//   - Object/map keys are emitted unquoted if they match identifier syntax
//     ([A-Za-z_][A-Za-z0-9_]*), else quoted with JSON-style escapes.
//   - Destructuring declaration patterns ("decl"/"darr"/"dobj") and
//     assignments print in a user-friendly layout.
//
// Errors:
//   - Returns a non-nil error if parsing fails; otherwise returns the formatted text.
func Pretty(src string) (string, error) {
	ast, err := ParseSExpr(src)
	if err != nil {
		return "", WrapErrorWithSource(err, src)
	}
	return FormatSExpr(ast), nil
}

// Standardize returns the canonical source form ("standard_source_code"):
//   - deterministic layout
//   - indentation using tabs (configurable via IndentWithTabs)
//   - exactly one trailing newline
func Standardize(src string) (string, error) {
	ast, err := ParseSExpr(src)
	if err != nil {
		return "", WrapErrorWithSource(err, src)
	}
	out := FormatSExpr(ast)
	if !strings.HasSuffix(out, "\n") {
		out += "\n"
	} else {
		// ensure exactly one
		out = strings.TrimRight(out, "\n") + "\n"
	}
	return out, nil
}

// FormatSExpr renders a parsed MindScript AST (S-expr) to a stable source string.
//
// Inputs:
//   - n: an AST produced by parser.go (e.g., the result of ParseSExpr).
//
// Output policy:
//   - Statements (fun/oracle/for/if/type/block/return/break/continue/assign)
//     are rendered with keywords and indentation similar to Pretty.
//   - Expressions are printed with minimal parentheses based on a fixed
//     precedence table; property access vs calls/indexing binds tightly.
//   - Arrays and maps are printed inline (AST form) without line-width heuristics;
//     however, annotations on map keys are emitted as preceding `# ...` comment lines.
//   - Annotation nodes wrap the printed construct with header comments.
//
// This function does not parse; it strictly formats the provided AST.
func FormatSExpr(n S) string {
	var b strings.Builder
	p := pp{out: out{b: &b}}
	p.printProgram(n)
	return strings.TrimRight(b.String(), "\n")
}

// FormatType renders a type S-expression into a compact, human-readable string.
//
// Supported forms (see types.go for the type system):
//   - ("id", name)              →  name
//   - ("unop","?", T)           →  T?
//   - ("array", T)              →  [T]
//   - ("map", ("pair"| "pair!", ("str",k), T) ...)
//   - Required fields print as `key!:`
//   - Keys are quoted if not identifier-like
//   - Key/value annotations (when wrapped in "annot") emit `# ...`
//     lines above the field
//   - ("enum", lit1, lit2, ...) →  Enum[lit1, lit2, ...] where literals may be
//     scalars, arrays, or maps rendered in a type-literal form
//   - ("binop","->", A, B)      →  (A) -> B ; right-assoc chains flatten
//
// Output is stable; when multi-line, map fields are sorted by key name for
// readability and determinism.
func FormatType(t S) string {
	var b strings.Builder
	o := out{b: &b}
	writeType(&o, t)
	return b.String()
}

// FormatValue renders a runtime Value into a stable, readable string.
//
// Layout policy:
//   - Scalars: `null`, booleans, ints, floats (guaranteed to show a decimal
//     point for non-scientific output), and quoted strings (with JSON-style escapes).
//   - Arrays: single-line `[ a, b, c ]` when all elements are single-line
//     and total length ≤ MaxInlineWidth; otherwise multi-line with one element per
//     line, indented, and with trailing commas omitted.
//   - Maps (MapObject):
//   - Keys are emitted in sorted order for stability.
//   - Single-line `{ k1: v1, k2: v2 }` is used when feasible under the
//     MaxInlineWidth budget and when no key carries a header annotation.
//   - Multi-line maps indent each entry; key/value annotations are emitted
//     as `# ...` lines immediately above the field.
//   - Keys are quoted when not identifier-like.
//   - Functions: printed as `<fun: name1:T1 -> name2:T2 -> R>`
//   - Zero-arg closures display `_:Null` for the single implicit unit.
//   - Types (VTType): the embedded type AST is pretty-printed via FormatType.
//   - Modules: printed as `<module: pretty-name>` if available.
//   - Value.Annot (header annotation): if non-empty, prints as leading `# ...`
//     lines above the value.
//
// Width sensitivity is only applied to runtime arrays/maps (not AST/type forms),
// and is controlled by MaxInlineWidth.
func FormatValue(v Value) string {
	var b strings.Builder
	o := out{b: &b}
	writeValue(&o, v)
	return b.String()
}

//// END_OF_PUBLIC

// ===============================
// ========= PRIVATE =============
// ===============================

/* ---------- globals & tiny helpers ---------- */

func isIdent(s string) bool {
	if s == "" {
		return false
	}
	rs := []rune(s)
	if !(unicode.IsLetter(rs[0]) || rs[0] == '_') {
		return false
	}
	for _, r := range rs[1:] {
		if !(unicode.IsLetter(r) || unicode.IsDigit(r) || r == '_') {
			return false
		}
	}
	return true
}

func quoteString(s string) string {
	var b strings.Builder
	b.WriteByte('"')
	for _, r := range s {
		switch r {
		case '\\':
			b.WriteString(`\\`)
		case '"':
			b.WriteString(`\"`)
		case '\n':
			b.WriteString(`\n`)
		case '\r':
			b.WriteString(`\r`)
		case '\t':
			b.WriteString(`\t`)
		case '\b':
			b.WriteString(`\b`)
		case '\f':
			b.WriteString(`\f`)
		default:
			b.WriteRune(r)
		}
	}
	b.WriteByte('"')
	return b.String()
}

/* ---------- small writer with indentation & annotations ---------- */

type out struct {
	b     *strings.Builder
	depth int
}

func (o *out) write(s string) { o.b.WriteString(s) }
func (o *out) nl()            { o.b.WriteByte('\n') }
func (o *out) pad() {
	for i := 0; i < o.depth; i++ {
		o.b.WriteByte('\t')
	}
}
func (o *out) line(s string)        { o.pad(); o.b.WriteString(s) }
func (o *out) withIndent(fn func()) { o.depth++; fn(); o.depth-- }
func (o *out) annot(text string) {
	if text == "" {
		return
	}
	for _, ln := range strings.Split(text, "\n") {
		o.line("# " + strings.TrimSpace(ln))
		o.nl()
	}
}

// unwraps the current VTType payload to its AST (supports legacy S as well).
func typeAst(data any) S {
	switch tv := data.(type) {
	case *TypeValue:
		return tv.Ast
	case S: // legacy fallback
		return tv
	default:
		return S{}
	}
}

/* ---------- source -> pretty (AST printer) ---------- */

type pp struct {
	out out
}

func (p *pp) write(s string) { p.out.write(s) }
func (p *pp) nl()            { p.out.nl() }
func (p *pp) sp()            { p.out.write(" ") }
func (p *pp) pad()           { p.out.pad() }

func (p *pp) printProgram(n S) {
	if tag(n) != "block" {
		p.printStmt(n)
		return
	}
	kids := children(n)
	for i, k := range kids {
		p.printStmt(k.(S))
		if i < len(kids)-1 {
			p.nl()
		}
	}
}

func (p *pp) printStmt(n S) {
	switch tag(n) {
	case "annot":
		text := getStr(child(n, 0))
		p.out.annot(text)
		p.printStmt(child(n, 1))

	case "fun":
		p.pad()
		p.write("fun(")
		p.printParams(child(n, 0))
		p.write(")")
		ret := child(n, 1)
		if !(tag(ret) == "id" && getId(ret) == "Any") {
			p.sp()
			p.write("->")
			p.sp()
			p.printExpr(ret, 0)
		}
		p.sp()
		p.write("do")
		p.nl()
		p.out.withIndent(func() { p.printBlock(child(n, 2)) })
		if len(child(n, 2)) > 1 {
			p.nl()
		}
		p.pad()
		p.write("end")

	case "oracle":
		p.pad()
		p.write("oracle(")
		p.printParams(child(n, 0))
		p.write(")")
		out := child(n, 1)
		if !(tag(out) == "id" && getId(out) == "Any") {
			p.sp()
			p.write("->")
			p.sp()
			p.printExpr(out, 0)
		}
		srcs := child(n, 2)
		if len(srcs) > 1 {
			p.sp()
			p.write("from ")
			p.printArray(srcs)
		}

	case "for":
		p.pad()
		p.write("for ")
		tgt := child(n, 0)
		// For targets that are declaration patterns, print them as patterns:
		//  - ("decl", x)    → "let x"
		//  - ("darr", ...)  → "[...]"  (no "let")
		//  - ("dobj", ...)  → "{...}"  (no "let")
		// Non-pattern assignables print as normal expressions.
		if isDeclPattern(tgt) {
			if tag(tgt) == "decl" {
				p.write("let ")
				p.printPattern(tgt) // prints just the name
			} else {
				p.printPattern(tgt)
			}
		} else {
			p.printExpr(tgt, 0)
		}
		p.sp()
		p.write("in ")
		p.printExpr(child(n, 1), 0)
		p.sp()
		p.write("do")
		p.nl()
		p.out.withIndent(func() { p.printBlock(child(n, 2)) })
		if len(child(n, 2)) > 1 {
			p.nl()
		}
		p.pad()
		p.write("end")

	case "while":
		p.pad()
		p.write("while ")
		p.printExpr(child(n, 0), 0)
		p.sp()
		p.write("do")
		p.nl()
		p.out.withIndent(func() { p.printBlock(child(n, 1)) })
		if len(child(n, 1)) > 1 {
			p.nl()
		}
		p.pad()
		p.write("end")

	case "if":
		arms := children(n)
		first := arms[0].(S)
		p.pad()
		p.write("if ")
		p.printExpr(child(first, 0), 0)
		p.sp()
		p.write("then")
		p.nl()
		p.out.withIndent(func() { p.printBlock(child(first, 1)) })
		if len(child(first, 1)) > 1 {
			p.nl()
		}
		i := 1
		for i < len(arms) && tag(arms[i].(S)) == "pair" {
			arm := arms[i].(S)
			p.pad()
			p.write("elif ")
			p.printExpr(child(arm, 0), 0)
			p.sp()
			p.write("then")
			p.nl()
			p.out.withIndent(func() { p.printBlock(child(arm, 1)) })
			if len(child(arm, 1)) > 1 {
				p.nl()
			}
			i++
		}
		if i < len(arms) {
			elseBlk := arms[i].(S)
			p.pad()
			p.write("else")
			p.nl()
			p.out.withIndent(func() { p.printBlock(elseBlk) })
			if len(elseBlk) > 1 {
				p.nl()
			}
		}
		p.pad()
		p.write("end")

	case "type":
		p.pad()
		p.write("type ")
		p.printExpr(child(n, 0), 0)

	case "return":
		p.pad()
		p.write("return(")
		p.printExpr(child(n, 0), 0)
		p.write(")")

	case "break":
		p.pad()
		p.write("break(")
		p.printExpr(child(n, 0), 0)
		p.write(")")

	case "continue":
		p.pad()
		p.write("continue(")
		p.printExpr(child(n, 0), 0)
		p.write(")")

	case "assign":
		lhs, rhs := child(n, 0), child(n, 1)
		p.pad()
		if isDeclPattern(lhs) {
			p.write("let ")
			p.printPattern(lhs)
			p.sp()
			p.write("=")
			p.sp()
			p.printExpr(rhs, 0)
		} else {
			p.printExpr(lhs, 0)
			p.sp()
			p.write("=")
			p.sp()
			p.printExpr(rhs, 0)
		}

	case "block":
		p.pad()
		p.write("do")
		p.nl()
		p.out.withIndent(func() { p.printBlock(n) })
		p.pad()
		p.write("end")

	default:
		p.pad()
		p.printExpr(n, 0)
	}
}

func (p *pp) printBlock(n S) {
	if tag(n) != "block" {
		p.printStmt(n)
		return
	}
	kids := children(n)
	for i, k := range kids {
		p.printStmt(k.(S))
		if i < len(kids)-1 {
			p.nl()
		}
	}
}

func (p *pp) printParams(arr S) {
	if tag(arr) != "array" || len(arr) == 1 {
		return
	}
	items := children(arr)
	for i, it := range items {
		pi := it.(S)
		name := getId(child(pi, 0))
		p.write(name)
		ty := child(pi, 1)
		if !(tag(ty) == "id" && getId(ty) == "Any") {
			p.write(": ")
			p.printExpr(ty, 0)
		}
		if i < len(items)-1 {
			p.write(", ")
		}
	}
}

func (p *pp) printExpr(n S, _ctx int) {
	switch tag(n) {
	case "id":
		p.write(getId(n))
	case "int":
		p.write(fmt.Sprint(n[1]))
	case "num":
		f := n[1].(float64)
		s := strconv.FormatFloat(f, 'g', -1, 64)
		if !strings.ContainsAny(s, ".eE") {
			s += ".0"
		}
		p.write(s)
	case "str":
		p.write(quoteString(getStr(n)))
	case "bool":
		if n[1].(bool) {
			p.write("true")
		} else {
			p.write("false")
		}
	case "null":
		p.write("null")
	case "unop":
		op := n[1].(string)
		if op == "?" {
			recv := n[2].(S)
			if prec(recv) < 90 {
				p.write("(")
				p.printExpr(recv, 0)
				p.write(")")
			} else {
				p.printExpr(recv, 90)
			}
			p.write("?")
			return
		}
		if op == "not" {
			p.write("not ")
		} else {
			p.write(op)
		}
		operand := n[2].(S)
		if prec(operand) < 80 {
			p.write("(")
			p.printExpr(operand, 0)
			p.write(")")
		} else {
			p.printExpr(operand, 80)
		}
	case "binop":
		op := n[1].(string)
		my := binopPrec(op)
		l, r := n[2].(S), n[3].(S)
		if prec(l) < my {
			p.write("(")
			p.printExpr(l, 0)
			p.write(")")
		} else {
			p.printExpr(l, my)
		}
		p.write(" " + op + " ")
		if prec(r) < my {
			p.write("(")
			p.printExpr(r, 0)
			p.write(")")
		} else {
			p.printExpr(r, my)
		}
	case "assign":
		l, r := child(n, 0), child(n, 1)
		if prec(l) < 10 {
			p.write("(")
			p.printExpr(l, 0)
			p.write(")")
		} else {
			p.printExpr(l, 10)
		}
		p.write(" = ")
		if prec(r) < 10 {
			p.write("(")
			p.printExpr(r, 0)
			p.write(")")
		} else {
			p.printExpr(r, 10)
		}
	case "call":
		recv := child(n, 0)
		if prec(recv) < 90 {
			p.write("(")
			p.printExpr(recv, 0)
			p.write(")")
		} else {
			p.printExpr(recv, 90)
		}
		p.write("(")
		for i := 2; i < len(n); i++ {
			if i > 2 {
				p.write(", ")
			}
			p.printExpr(n[i].(S), 0)
		}
		p.write(")")
	case "idx":
		recv := child(n, 0)
		if prec(recv) < 90 {
			p.write("(")
			p.printExpr(recv, 0)
			p.write(")")
		} else {
			p.printExpr(recv, 90)
		}
		p.write("[")
		p.printExpr(child(n, 1), 0)
		p.write("]")
	case "get":
		recv := child(n, 0)
		if prec(recv) < 90 {
			p.write("(")
			p.printExpr(recv, 0)
			p.write(")")
		} else {
			p.printExpr(recv, 90)
		}
		name := getStr(child(n, 1))
		if isIdent(name) {
			p.write("." + name)
		} else {
			p.write("." + quoteString(name))
		}
	case "array":
		p.printArray(n)
	case "map":
		p.printMap(n)

	case "enum":
		// Type expression: Enum[ <lits...> ]
		p.write("Enum[")
		items := children(n)
		for i, it := range items {
			if i > 0 {
				p.write(", ")
			}
			p.printExpr(it.(S), 0) // literals can be scalars, arrays, or maps
		}
		p.write("]")

	case "decl":
		p.write("let " + getId(n))
	case "return", "break", "continue", "fun", "oracle", "for", "while", "if", "type", "block", "annot":
		p.printStmt(n)
	default:
		p.write("<" + tag(n) + ">")
	}
}

func (p *pp) printArray(n S) {
	p.write("[")
	for i, it := range children(n) {
		if i > 0 {
			p.write(", ")
		}
		p.printExpr(it.(S), 0)
	}
	p.write("]")
}

func (p *pp) printMap(n S) {
	p.write("{")
	items := children(n)
	for i, pr := range items {
		if i > 0 {
			p.write(", ")
		}
		pair := pr.(S)
		keyNode := child(pair, 0)
		key, keyAnn := unwrapKey(keyNode)

		if keyAnn != "" {
			// put the annotation as a line comment just before the field
			p.nl()
			p.out.withIndent(func() {
				p.pad()
				p.out.annot(keyAnn)
			})
			p.pad()
		}

		if isIdent(key) {
			p.write(key)
		} else {
			p.write(quoteString(key))
		}
		p.write(": ")
		p.printExpr(child(pair, 1), 0)

		// if we emitted a leading annotation, keep items nicely lined up
		if keyAnn != "" && i < len(items)-1 {
			p.write(", ")
		}
	}
	p.write("}")
}

/* ---------- tiny AST helpers ---------- */

func tag(n S) string     { return n[0].(string) }
func children(n S) []any { return n[1:] }
func child(n S, i int) S { return n[i+1].(S) }
func getId(n S) string   { return n[1].(string) }
func getStr(n S) string  { return n[1].(string) }

func prec(n S) int {
	switch tag(n) {
	case "assign":
		return 10
	case "binop":
		return binopPrec(n[1].(string))
	case "unop":
		if n[1].(string) == "?" {
			return 90
		}
		return 80
	case "call", "idx", "get":
		return 90
	default:
		return 100
	}
}

func unwrapKey(n S) (name string, annot string) {
	for tag(n) == "annot" {
		annot += getStr(child(n, 0))
		n = child(n, 1)
	}
	return getStr(n), annot
}

func binopPrec(op string) int {
	switch op {
	case "->":
		return 15
	case "*", "/", "%":
		return 70
	case "+", "-":
		return 60
	case "<", "<=", ">", ">=":
		return 50
	case "==", "!=":
		return 40
	case "and":
		return 30
	case "or":
		return 20
	default:
		return 60
	}
}

func isDeclPattern(n S) bool {
	switch tag(n) {
	case "decl", "darr", "dobj":
		return true
	case "annot":
		return isDeclPattern(child(n, 1))
	default:
		return false
	}
}

func (p *pp) printPattern(n S) {
	switch tag(n) {
	case "decl":
		p.write(getId(n))
	case "darr":
		p.write("[")
		for i := 1; i < len(n); i++ {
			if i > 1 {
				p.write(", ")
			}
			p.printPattern(n[i].(S))
		}
		p.write("]")
	case "dobj":
		items := children(n)
		needsMultiline := false
		for _, it := range items {
			if tag(it.(S)[2].(S)) == "annot" {
				needsMultiline = true
				break
			}
		}
		if !needsMultiline {
			p.write("{")
			for i, it := range items {
				if i > 0 {
					p.write(", ")
				}
				pr := it.(S)
				key := getStr(child(pr, 0))
				if isIdent(key) {
					p.write(key)
				} else {
					p.write(quoteString(key))
				}
				p.write(": ")
				p.printPattern(child(pr, 1))
			}
			p.write("}")
			return
		}
		p.write("{")
		p.nl()
		p.out.withIndent(func() {
			for i, it := range items {
				pr := it.(S)
				p.pad()
				key := getStr(child(pr, 0))
				if isIdent(key) {
					p.write(key)
				} else {
					p.write(quoteString(key))
				}
				p.write(": ")
				val := child(pr, 1)
				if tag(val) == "annot" {
					p.nl()
					p.printPattern(val)
				} else {
					p.printPattern(val)
				}
				if i < len(items)-1 {
					p.write(",")
				}
				p.nl()
			}
		})
		p.pad()
		p.write("}")
	case "annot":
		p.out.annot(getStr(child(n, 0)))
		p.pad()
		p.printPattern(child(n, 1))
	default:
		p.printExpr(n, 0)
	}
}

/* ---------- Type pretty-printer (private helpers) ---------- */

func writeType(o *out, t S) {
	tagOf := func(x S) string {
		if len(x) == 0 {
			return ""
		}
		return x[0].(string)
	}

	switch tagOf(t) {
	case "id":
		o.write(t[1].(string))

	case "unop":
		if len(t) >= 3 && t[1].(string) == "?" {
			writeType(o, t[2].(S))
			o.write("?")
			return
		}
		o.write("<unop>")

	case "array":
		o.write("[")
		elem := S{"id", "Any"}
		if len(t) == 2 {
			elem = t[1].(S)
		}
		writeType(o, elem)
		o.write("]")

	case "enum":
		o.write("Enum[")
		for i := 1; i < len(t); i++ {
			if i > 1 {
				o.write(", ")
			}
			writeTypeLiteral(o, t[i].(S))
		}
		o.write("]")

	case "map":
		type field struct {
			name     string
			required bool
			typ      S
			keyAnnot string
			valAnnot string
		}
		fs := make([]field, 0, len(t)-1)
		for i := 1; i < len(t); i++ {
			p := t[i].(S) // ("pair"|"pair!", keyNode, T or ("annot", ("str",doc), T))
			req := p[0].(string) == "pair!"
			keyNode := p[1].(S)
			key, keyAnn := unwrapKey(keyNode)
			ft := p[2].(S)
			valAnn := ""
			if len(ft) > 0 && ft[0].(string) == "annot" {
				valAnn = ft[1].(S)[1].(string)
				ft = ft[2].(S)
			}
			fs = append(fs, field{name: key, required: req, typ: ft, keyAnnot: keyAnn, valAnnot: valAnn})
		}
		sort.Slice(fs, func(i, j int) bool { return fs[i].name < fs[j].name })

		o.write("{")
		o.nl()
		o.withIndent(func() {
			for i, f := range fs {
				if f.keyAnnot != "" {
					o.annot(f.keyAnnot)
				}
				if f.valAnnot != "" {
					o.annot(f.valAnnot)
				}
				o.pad()
				if isIdent(f.name) {
					o.write(f.name)
				} else {
					o.write(quoteString(f.name))
				}
				if f.required {
					o.write("!")
				}
				o.write(": ")
				writeType(o, f.typ)
				if i < len(fs)-1 {
					o.write(",")
				}
				o.nl()
			}
		})
		o.pad()
		o.write("}")

	case "binop":
		if len(t) >= 4 && t[1].(string) == "->" {
			params, ret := flattenArrow(t)
			o.write("(")
			for i := range params {
				if i > 0 {
					o.write(", ")
				}
				writeType(o, params[i])
			}
			o.write(") -> ")
			writeType(o, ret)
			return
		}
		o.write("<binop>")

	case "annot":
		o.annot(t[1].(S)[1].(string))
		writeType(o, t[2].(S))

	default:
		o.write("<type>")
	}
}

func writeTypeLiteral(o *out, lit S) {
	switch lit[0].(string) {
	case "null":
		o.write("null")
	case "bool":
		if lit[1].(bool) {
			o.write("true")
		} else {
			o.write("false")
		}
	case "int":
		o.write(fmt.Sprint(lit[1]))
	case "num":
		o.write(strconv.FormatFloat(lit[1].(float64), 'g', -1, 64))
	case "str":
		o.write(quoteString(lit[1].(string)))

	// Allow complex enum members (arrays/maps) for schema-like enums
	case "array":
		o.write("[")
		for i := 1; i < len(lit); i++ {
			if i > 1 {
				o.write(", ")
			}
			writeTypeLiteral(o, lit[i].(S))
		}
		o.write("]")
	case "map":
		o.write("{")
		items := children(lit)
		for i, pr := range items {
			if i > 0 {
				o.write(", ")
			}
			pair := pr.(S)
			k := getStr(child(pair, 0))
			if isIdent(k) {
				o.write(k)
			} else {
				o.write(quoteString(k))
			}
			o.write(": ")
			writeTypeLiteral(o, child(pair, 1))
		}
		o.write("}")

	default:
		o.write("<lit>")
	}
}

func flattenArrow(t S) (params []S, ret S) {
	for {
		if tag(t) == "binop" && t[1].(string) == "->" {
			params = append(params, t[2].(S))
			t = t[3].(S)
			continue
		}
		ret = t
		return
	}
}

/* ---------- Runtime value pretty-printer (private helpers) ---------- */

func mapOneLineMO(keys []string, mo *MapObject) string {
	if len(keys) == 0 {
		return "{}"
	}
	parts := make([]string, 0, len(keys))
	for _, k := range keys {
		if ann, ok := mo.KeyAnn[k]; ok && ann != "" {
			// any annotated key forces multiline
			return ""
		}
		v := mo.Entries[k]
		if isValueMultiline(v) {
			return ""
		}
		key := k
		if !isIdent(key) {
			key = quoteString(key)
		}
		var b strings.Builder
		o := out{b: &b}
		writeValue(&o, v)
		parts = append(parts, key+": "+b.String())
	}
	return "{ " + strings.Join(parts, ", ") + " }"
}

func writeValue(o *out, v Value) {
	// Header annotation (once)
	if v.Annot != "" {
		o.annot(v.Annot)
		o.pad()
	}

	switch v.Tag {

	case VTNull:
		o.write("null")

	case VTBool:
		if v.Data.(bool) {
			o.write("true")
		} else {
			o.write("false")
		}

	case VTInt:
		o.write(strconv.FormatInt(v.Data.(int64), 10))

	case VTNum:
		s := strconv.FormatFloat(v.Data.(float64), 'g', -1, 64)
		if !strings.ContainsAny(s, ".eE") {
			s += ".0"
		}
		o.write(s)

	case VTStr:
		o.write(quoteString(v.Data.(string)))

	case VTArray:
		xs := v.Data.([]Value)
		if oneline := arrayOneLine(xs); oneline != "" && len(oneline) <= MaxInlineWidth {
			o.write(oneline)
			return
		}
		o.write("[")
		o.nl()
		o.withIndent(func() {
			for i, it := range xs {
				if it.Annot == "" {
					o.pad() // pad here only when no header annot
				}
				writeValue(o, it) // annotated values handle their own padding
				if i < len(xs)-1 {
					o.write(",")
				}
				o.nl()
			}
		})
		o.pad()
		o.write("]")

	case VTMap:
		mo := v.Data.(*MapObject)
		// Build a sorted view of keys for stable output
		keys := append([]string(nil), mo.Keys...)
		sort.Strings(keys)

		// One-line candidate (only if no key is annotated)
		if oneline := mapOneLineMO(keys, mo); oneline != "" && len(oneline) <= MaxInlineWidth {
			o.write(oneline)
			return
		}

		// Multiline
		o.write("{")
		o.nl()
		o.withIndent(func() {
			for i, k := range keys {
				if ann, ok := mo.KeyAnn[k]; ok && ann != "" {
					o.annot(ann) // o.annot already pads correctly
				}
				o.pad() // align with annotation line
				if isIdent(k) {
					o.write(k)
				} else {
					o.write(quoteString(k))
				}
				o.write(": ")
				writeValue(o, mo.Entries[k])
				if i < len(keys)-1 {
					o.write(",")
				}
				o.nl()
			}
		})
		o.pad()
		o.write("}")
		return

	case VTFun:
		// Pretty-print as <fun: n1:T1 -> n2:T2 -> R> ; zero-arg as _:Null -> R
		if f, ok := v.Data.(*Fun); ok && f != nil {
			var sb strings.Builder
			sb.WriteString("<fun: ")
			if len(f.ParamTypes) == 0 {
				sb.WriteString("_:Null")
			} else {
				for i := range f.ParamTypes {
					if i > 0 {
						sb.WriteString(" -> ")
					}
					name := "_"
					if i < len(f.Params) && f.Params[i] != "" {
						name = f.Params[i]
					}
					sb.WriteString(name)
					sb.WriteString(":")
					sb.WriteString(FormatType(f.ParamTypes[i]))
				}
			}
			sb.WriteString(" -> ")
			sb.WriteString(FormatType(f.ReturnType))
			sb.WriteString(">")
			o.write(sb.String())
		} else {
			o.write("<fun>")
		}

	case VTType:
		ast := typeAst(v.Data)
		typ := FormatType(ast)
		if strings.Contains(typ, "\n") {
			lines := strings.Split(typ, "\n")
			for i, ln := range lines {
				if i == 0 {
					o.write(ln)
					continue
				}
				o.nl()
				o.pad()
				o.write(ln)
			}
			return
		}
		o.write(typ)

	case VTModule:
		name := "<module>"
		if m, ok := v.Data.(*Module); ok && m != nil && m.Name != "" {
			disp := prettySpec(m.Name)
			if disp == "" {
				disp = m.Name
			}
			name = "<module: " + disp + ">"
		}
		o.write(name)

	default:
		s := "<unknown>"
		if v.Tag == VTHandle {
			if h, ok := v.Data.(*Handle); ok {
				s = "<handle:" + h.Kind + ">"
			} else {
				s = "<handle>"
			}
		}
		o.write(s)
	}
}

/* ---------- single-line candidates ---------- */

func arrayOneLine(xs []Value) string {
	if len(xs) == 0 {
		return "[]"
	}
	parts := make([]string, 0, len(xs))
	for _, it := range xs {
		if isValueMultiline(it) {
			return ""
		}
		var b strings.Builder
		o := out{b: &b}
		writeValue(&o, it)
		parts = append(parts, b.String())
	}
	return "[ " + strings.Join(parts, ", ") + " ]"
}

func isValueMultiline(v Value) bool {
	if v.Annot != "" { // header comments force multi-line
		return true
	}
	switch v.Tag {
	case VTArray:
		xs := v.Data.([]Value)
		if len(xs) == 0 {
			return false
		}
		for _, it := range xs {
			if isValueMultiline(it) {
				return true
			}
		}
		line := arrayOneLine(xs)
		return line == "" || len(line) > MaxInlineWidth
	case VTMap:
		mo := v.Data.(*MapObject)
		if len(mo.Keys) == 0 {
			return false
		}
		keys := append([]string(nil), mo.Keys...)
		sort.Strings(keys)
		line := mapOneLineMO(keys, mo)
		return line == "" || len(line) > MaxInlineWidth

	case VTType:
		t := typeAst(v.Data)
		return len(t) > 0 && t[0].(string) == "map" && len(t) > 1

	default:
		return false
	}
}
