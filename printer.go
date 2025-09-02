// printer.go: pretty-printers for MindScript ASTs, types, and runtime values.
//
// What this file does
// -------------------
// This module provides the formatting layer for MindScript. It renders three
// kinds of data to human-readable, stable strings:
//
//  1. Parsed source ASTs (S-expressions) → MindScript source code.
//     - Entry points: Pretty, Standardize, FormatSExpr.
//     - Produces whitespace- and newline-stable output with minimal
//     parentheses, based on operator precedence. It understands all
//     statement and expression tags emitted by the parser (e.g. "fun",
//     "oracle", "for", "if/elif/else", "type", "block", "assign",
//     "return/break/continue", arrays, maps, calls, indexing, properties,
//     unary and binary operators).
//     - Annotation nodes use the simplified 3-ary form:
//     ("annot", ("str", textOr<text>), wrappedNode)
//     PRE annotations are stored as-is (text); POST annotations are encoded
//     by a leading "<" in the stored text. PRE annotations print as `# ...`
//     lines immediately above the construct; POST annotations print as a
//     trailing `# ...` inline comment on the same line as the construct.
//     - Property names are emitted bare if they are identifier-like, otherwise
//     quoted. Destructuring declaration patterns ("decl" | "darr" | "dobj")
//     are rendered in a compact, readable form.
//     - Formatting emits no space before '(' for calls and for 'fun(...)'
//     and 'oracle(...)' parameter lists, matching the lexer’s CLROUND rule.
//
//  2. Type ASTs (S-expressions) → compact type strings.
//     - Entry point: FormatType.
//     - Supported forms:
//     ("id", "Any"|"Null"|"Bool"|"Int"|"Num"|"Str"|"Type")
//     ("unop","?", T)         → prints as `T?`
//     ("array", T)            → prints as `[T]`
//     ("map", ("pair"| "pair!", ("str",k), T) ...)
//     Required fields print with a trailing `!` on the key.
//     Key/value annotations (if wrapped in "annot") are respected:
//     PRE as header lines; POST as trailing inline comments.
//     ("enum", literalS... )  → prints as `Enum[ ... ]`, where members
//     may be scalars, arrays, or maps.
//     ("binop","->", A, B)    → prints as `(A) -> B`, flattened across
//     right-associated chains.
//     - Output is stable. Multi-line maps are rendered with sorted keys to
//     avoid visual churn.
//
//  3. Runtime values (Value) → width-aware strings.
//     - Entry point: FormatValue.
//     - Scalars print plainly (`null`, `true/false`, numbers, quoted strings).
//     - Arrays and maps prefer a single-line rendering if it fits the
//     MaxInlineWidth budget and no PRE annotations force multi-line; else
//     they fall back to pretty, multi-line output with indentation.
//     - Map keys are emitted bare if they’re identifier-like, otherwise quoted.
//     - Annotations distinguish PRE vs POST using the same `<` convention:
//     • PRE (no '<' prefix): printed as `# ...` lines before the value.
//     • POST (with '<' prefix): printed as an inline trailing comment
//     on the same line as the value.
//     This applies to Value.Annot (per-value) and MapObject.KeyAnn[k] (per-key).
//     - Functions print as `<fun: a:T -> b:U -> R>` (or `_:Null` for zero-arg).
//     - Types (VTType) are printed by extracting the embedded type AST and
//     delegating to FormatType.
//     - Modules print as `<module: <pretty name>>` when available.
//
// Dependencies (other files)
// --------------------------
// • parser.go
//   - S = []any (AST payload shape)
//   - ParseSExpr(string) / ParseSExprInteractive (used by Pretty/Standardize)
//   - AST tags: "block", "fun", "oracle", "for", "while", "if",
//     "type", "return", "break", "continue", "assign", "array", "map",
//     "pair"/"pair!", "get", "idx", "call", "id", "str", "int", "num", "bool",
//     "null", "unop", "binop", "decl", "darr", "dobj", "annot", "noop".
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
//   - WrapErrorWithSource(err, src) (used by Pretty/Standardize).
//
// PUBLIC vs PRIVATE layout
// ------------------------
// This file is organized in two blocks:
//  1. PUBLIC: the user-facing constants & functions with thorough docstrings.
//  2. PRIVATE: helper types and functions that implement the printers.
//
// Formatting policy highlights
// ----------------------------
//   - Indentation uses **tabs** only (gofmt-style).
//   - Canonical output (`Standardize`) ends with exactly one trailing '\n'.
package mindscript

import (
	"fmt"
	"sort"
	"strconv"
	"strings"
)

// ==============================
// ========== PUBLIC ============
// ==============================

// MaxInlineWidth controls when arrays/maps are rendered on a single line by
// FormatValue. If the one-line candidate exceeds this many characters, or if
// any element/key has a PRE annotation (non '<' prefixed), the value is rendered
// across multiple lines with indentation. This setting is read at call time
// and may be changed between calls.
var MaxInlineWidth = 80

// Pretty parses a MindScript source string and returns a formatted version.
//
// Behavior:
//   - Parses src via ParseSExpr. If parsing fails, the error is wrapped with
//     source context via WrapErrorWithSource.
//   - On success, pretty-prints the AST using FormatSExpr, producing stable,
//     whitespace-normalized code with minimal parentheses.
//   - Supports annotations using the 3-ary form:
//     ("annot", ("str", textOr<text>), X)
//     PRE prints as `# ...` above; POST prints as trailing `# ...` on the line.
//
// Errors:
//   - Returns a non-nil error if parsing fails; otherwise returns the formatted text.
func Pretty(src string) (string, error) {
	ast, err := ParseSExpr(src)
	if err != nil {
		if e, ok := err.(*Error); ok {
			if e.Src == nil {
				e.Src = &SourceRef{Name: "<main>", Src: src}
			}
			return "", fmt.Errorf("%s", FormatError(e))
		}
		return "", err
	}
	return FormatSExpr(ast), nil
}

// Standardize returns the canonical source form:
//   - deterministic layout
//   - indentation using tabs
//   - exactly one trailing newline
//
// It is equivalent to Pretty(src), but ensures precisely one '\n' at the end.
func Standardize(src string) (string, error) {
	ast, err := ParseSExpr(src)
	if err != nil {
		if e, ok := err.(*Error); ok {
			if e.Src == nil {
				e.Src = &SourceRef{Name: "<standardize>", Src: src}
			}
			return "", fmt.Errorf("%s", FormatError(e))
		}
		return "", err
	}
	out := FormatSExpr(ast)
	if !strings.HasSuffix(out, "\n") {
		out += "\n"
	} else {
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
//     are rendered with keywords and indentation.
//   - Expressions use minimal parentheses according to a fixed precedence table;
//     property access vs calls/indexing binds tightly.
//   - Arrays and maps are printed inline (AST form); map key annotations print
//     as preceding `# ...` lines (PRE) or trailing inline comments (POST).
//   - Annotation nodes wrap the printed construct; POST becomes trailing inline.
//
// This function does not parse; it strictly formats the provided AST.
func FormatSExpr(n S) string {
	var b strings.Builder
	p := pp{out: out{b: &b}}
	p.printProgram(n)
	return strings.TrimRight(b.String(), "\n")
}

// FormatType renders a type S-expression into a compact, human-readable string.
// It respects PRE (header lines) vs POST (trailing inline) annotations in the
// same way as the AST/code printer.
func FormatType(t S) string {
	var b strings.Builder
	o := out{b: &b}
	writeType(&o, t)
	return b.String()
}

// FormatValue renders a runtime Value into a stable, readable string.
//
// Layout policy:
//   - Scalars: null, booleans, ints, floats (with a decimal point for
//     non-scientific output), and quoted strings.
//   - Arrays: single-line `[ a, b, c ]` when all elements are single-line,
//     have no PRE annotations, and total length ≤ MaxInlineWidth; otherwise
//     multi-line with indentation. POST annotations render inline trailing.
//   - Maps: keys sorted for stability; single-line `{ k: v, ... }` when short,
//     with no PRE key/value annotations; else multi-line with indentation,
//     where PRE annotations print as header lines, POST as inline trailing.
//   - Functions: `<fun: name1:T1 -> name2:T2 -> R>` (zero-arg uses `_:Null`).
//   - Types (VTType): pretty-printed via FormatType.
//   - Modules: `<module: pretty-name>` when available.
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

/* ---------- small globals & utilities ---------- */

func isIdent(s string) bool {
	if s == "" {
		return false
	}
	b := []byte(s)
	c := b[0]
	if !((c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z') || c == '_') {
		return false
	}
	for i := 1; i < len(b); i++ {
		c = b[i]
		if !((c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z') || (c >= '0' && c <= '9') || c == '_') {
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
func (o *out) withIndent(fn func()) { o.depth++; fn(); o.depth-- }

// PRE annotations (block/head) — prints as lines above current position.
func (o *out) annot(text string) {
	if text == "" {
		return
	}
	for _, ln := range strings.Split(text, "\n") {
		o.pad()
		o.b.WriteString("# " + strings.TrimSpace(ln))
		o.nl()
	}
}

// POST annotations (inline/trailing) — prints on the same line.
func (o *out) annotInline(text string) {
	if text == "" {
		return
	}
	trim := oneLine(text)
	if trim == "" {
		return
	}
	o.write(" # " + trim)
}

func oneLine(s string) string {
	s = strings.ReplaceAll(s, "\n", " ")
	return strings.TrimSpace(s)
}

// unwrap VTType payload to its AST (supports legacy S too).
func typeAst(data any) S {
	switch tv := data.(type) {
	case *TypeValue:
		return tv.Ast
	case S:
		return tv
	default:
		return S{}
	}
}

// Split annotation text into PRE vs POST according to leading "<".
func splitAnnotText(s string) (pre, post string) {
	if s == "" {
		return "", ""
	}
	if strings.HasPrefix(s, "<") {
		return "", strings.TrimPrefix(s, "<")
	}
	return s, ""
}

/* ---------- AST helpers: tags, shapes, precedence ---------- */

func tag(n S) string   { return n[0].(string) }
func getId(n S) string { return n[1].(string) }
func getStr(n S) string {
	// Used for ("str", s), but safe for ("id", name) too (both carry string at [1]).
	return n[1].(string)
}

// Shape helpers (safe indexing in one place)
func asUnop(n S) (op string, rhs S)       { return n[1].(string), n[2].(S) }
func asBinop(n S) (op string, lhs, rhs S) { return n[1].(string), n[2].(S), n[3].(S) }
func asAssign(n S) (lhs, rhs S)           { return n[1].(S), n[2].(S) }
func asCall(n S) (recv S, args []S)       { recv = n[1].(S); return recv, listS(n, 2) }
func asIdx(n S) (recv, idx S)             { return n[1].(S), n[2].(S) }
func asGet(n S) (recv S, name string)     { return n[1].(S), n[2].(S)[1].(string) }

// New: decode 3-ary ("annot", ("str", textOr<text>), wrapped) to (text, wrapped, pre?)
func asAnnot(n S) (text string, wrapped S, pre bool) {
	raw := n[1].(S)[1].(string)
	preText, postText := splitAnnotText(raw)
	if preText != "" {
		return preText, n[2].(S), true
	}
	return postText, n[2].(S), false
}

func listS(n S, from int) []S {
	if len(n) <= from {
		return nil
	}
	out := make([]S, 0, len(n)-from)
	for i := from; i < len(n); i++ {
		out = append(out, n[i].(S))
	}
	return out
}

func bracketed(o *out, open, close string, elems []S, emit func(S)) {
	o.write(open)
	for i, e := range elems {
		if i > 0 {
			o.write(", ")
		}
		emit(e)
	}
	o.write(close)
}

func keyOut(o *out, name string) {
	if isIdent(name) {
		o.write(name)
	} else {
		o.write(quoteString(name))
	}
}

var binPrec = map[string]struct {
	p     int
	right bool
}{
	"->": {15, true},
	"*":  {70, false}, "/": {70, false}, "%": {70, false},
	"+": {60, false}, "-": {60, false},
	"<": {50, false}, "<=": {50, false}, ">": {50, false}, ">=": {50, false},
	"==": {40, false}, "!=": {40, false},
	"and": {30, false},
	"or":  {20, false},
}

func precOf(n S) int {
	switch tag(n) {
	case "assign":
		return 10
	case "binop":
		if pr, ok := binPrec[n[1].(string)]; ok {
			return pr.p
		}
		return 60
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

/* ---------- source → pretty (AST printer) ---------- */

type pp struct{ out out }

func (p *pp) write(s string) { p.out.write(s) }
func (p *pp) nl()            { p.out.nl() }
func (p *pp) pad()           { p.out.pad() }
func (p *pp) sp()            { p.write(" ") }

func (p *pp) printMin(n S, need int) {
	if precOf(n) < need {
		p.write("(")
		p.printExpr(n)
		p.write(")")
		return
	}
	p.printExpr(n)
}
func (p *pp) recv90(n S) { p.printMin(n, 90) }

func (p *pp) bin(op string, l, r S) {
	my := 60
	if pr, ok := binPrec[op]; ok {
		my = pr.p
	}
	p.printMin(l, my)
	p.write(" " + op + " ")
	p.printMin(r, my)
}

func (p *pp) printProgram(n S) {
	if tag(n) != "block" {
		p.printStmt(n)
		return
	}
	kids := listS(n, 1)
	for i, k := range kids {
		p.printStmt(k)
		if i < len(kids)-1 {
			p.nl()
		}
	}
}

func (p *pp) kwCall(name string, arg S) {
	p.pad()
	p.write(name)
	p.write("(")
	p.printExpr(arg)
	p.write(")")
}

func (p *pp) kwBlock(header func(), body S) {
	p.pad()
	header()
	p.sp()
	p.write("do")
	p.nl()
	p.out.withIndent(func() { p.printBlock(body) })
	if len(body) > 1 {
		p.nl()
	}
	p.pad()
	p.write("end")
}

func (p *pp) callableHeader(kind string, params, outT S) {
	p.write(kind)
	p.write("(")
	p.printParams(params)
	p.write(")")
	if !(tag(outT) == "id" && getId(outT) == "Any") {
		p.sp()
		p.write("->")
		p.sp()
		p.printExpr(outT)
	}
}

func isEmptyArray(n S) bool { return tag(n) == "array" && len(n) == 1 }

func (p *pp) printAnnotStmt(n S) {
	text, wrapped, pre := asAnnot(n)
	if pre {
		p.out.annot(text)
		p.printStmt(wrapped)
		return
	}
	// POST: print wrapped, then trailing inline comment (no newline here).
	p.printStmt(wrapped)
	p.out.annotInline(text)
}

func (p *pp) printStmt(n S) {
	switch tag(n) {
	case "noop":
		return
	case "annot":
		p.printAnnotStmt(n)

	case "fun":
		params, ret, body := n[1].(S), n[2].(S), n[3].(S)
		p.kwBlock(func() { p.callableHeader("fun", params, ret) }, body)

	case "oracle":
		params, outT, src := n[1].(S), n[2].(S), n[3].(S)
		p.pad()
		p.callableHeader("oracle", params, outT)
		if !isEmptyArray(src) {
			p.sp()
			p.write("from ")
			p.printExpr(src)
		}

	case "for":
		tgt, iter, body := n[1].(S), n[2].(S), n[3].(S)
		p.kwBlock(func() {
			p.write("for ")
			if isDeclPattern(tgt) {
				if tag(tgt) == "decl" {
					p.write("let ")
					p.printPattern(tgt)
				} else {
					p.printPattern(tgt)
				}
			} else {
				p.printExpr(tgt)
			}
			p.sp()
			p.write("in ")
			p.printExpr(iter)
		}, body)

	case "while":
		cond, body := n[1].(S), n[2].(S)
		p.kwBlock(func() { p.write("while "); p.printExpr(cond) }, body)

	case "if":
		arms := listS(n, 1)
		first := arms[0]
		p.pad()
		p.write("if ")
		p.printExpr(first[1].(S))
		p.sp()
		p.write("then")
		p.nl()
		p.out.withIndent(func() { p.printBlock(first[2].(S)) })
		if len(first[2].(S)) > 1 {
			p.nl()
		}
		i := 1
		for i < len(arms) && tag(arms[i]) == "pair" {
			arm := arms[i]
			p.pad()
			p.write("elif ")
			p.printExpr(arm[1].(S))
			p.sp()
			p.write("then")
			p.nl()
			p.out.withIndent(func() { p.printBlock(arm[2].(S)) })
			if len(arm[2].(S)) > 1 {
				p.nl()
			}
			i++
		}
		if i < len(arms) {
			elseBlk := arms[i]
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

	case "module":
		// ("module", nameExpr, bodyBlock)
		nameExpr, body := n[1].(S), n[2].(S)
		p.kwBlock(func() {
			p.write("module ")
			p.printExpr(nameExpr)
		}, body)

	case "type":
		p.pad()
		p.write("type ")
		p.printExpr(n[1].(S))

	case "return":
		p.kwCall("return", n[1].(S))
	case "break":
		p.kwCall("break", n[1].(S))
	case "continue":
		p.kwCall("continue", n[1].(S))

	case "assign":
		lhs, rhs := asAssign(n)
		p.pad()
		if isDeclPattern(lhs) {
			p.write("let ")
			p.printPattern(lhs)
			p.sp()
			p.write("=")
			p.sp()
			p.printExpr(rhs)
		} else {
			p.printExpr(lhs)
			p.sp()
			p.write("=")
			p.sp()
			p.printExpr(rhs)
		}

	case "block":
		p.pad()
		p.write("do")
		p.nl()
		p.out.withIndent(func() { p.printBlock(n) })
		if len(n) > 1 {
			p.nl()
		}
		p.pad()
		p.write("end")

	default:
		p.pad()
		p.printExpr(n)
	}
}

func (p *pp) printBlock(n S) {
	if tag(n) != "block" {
		p.printStmt(n)
		return
	}
	kids := listS(n, 1)
	for i, k := range kids {
		p.printStmt(k)
		if i < len(kids)-1 {
			p.nl()
		}
	}
}

func (p *pp) printParams(arr S) {
	if tag(arr) != "array" || len(arr) == 1 {
		return
	}
	items := listS(arr, 1)
	for i, pi := range items {
		p.write(getId(pi[1].(S)))
		ty := pi[2].(S)
		if !(tag(ty) == "id" && getId(ty) == "Any") {
			p.write(": ")
			p.printExpr(ty)
		}
		if i < len(items)-1 {
			p.write(", ")
		}
	}
}

func (p *pp) printCommaList(xs []S, emit func(S)) {
	for i, a := range xs {
		if i > 0 {
			p.write(", ")
		}
		emit(a)
	}
}

func (p *pp) printExpr(n S) {
	switch tag(n) {
	case "id":
		p.write(getId(n))
	case "int":
		p.write(fmt.Sprint(n[1]))
	case "num":
		s := strconv.FormatFloat(n[1].(float64), 'g', -1, 64)
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
		op, rhs := asUnop(n)
		if op == "?" {
			p.recv90(rhs)
			p.write("?")
			return
		}
		if op == "not" {
			p.write("not ")
		} else {
			p.write(op)
		}
		p.printMin(rhs, 80)

	case "binop":
		op, l, r := asBinop(n)
		p.bin(op, l, r)

	case "assign":
		l, r := asAssign(n)
		p.printMin(l, 10)
		p.write(" = ")
		p.printMin(r, 10)

	case "call":
		recv, args := asCall(n)
		p.recv90(recv)
		p.write("(")
		if len(args) > 0 {
			p.printCommaList(args, func(s S) { p.printExpr(s) })
		}
		p.write(")")

	case "idx":
		recv, ix := asIdx(n)
		p.recv90(recv)
		p.write("[")
		p.printExpr(ix)
		p.write("]")

	case "get":
		recv, name := asGet(n)
		p.recv90(recv)
		if isIdent(name) {
			p.write("." + name)
		} else {
			p.write("." + quoteString(name))
		}

	case "array":
		bracketed(&p.out, "[", "]", listS(n, 1), func(s S) { p.printExpr(s) })

	case "map":
		p.printMapAST(n)

	case "enum":
		bracketed(&p.out, "Enum[", "]", listS(n, 1), func(s S) { p.printExpr(s) })

	case "decl":
		p.write("let " + getId(n))

	case "return", "break", "continue", "fun", "oracle", "for", "while", "if", "type", "block", "annot":
		p.printStmt(n)

	case "module":
		p.printStmt(n)

	default:
		p.write("<" + tag(n) + ">")
	}
}

func (p *pp) printMapAST(n S) {
	p.write("{")
	items := listS(n, 1)
	for i, pr := range items {
		if i > 0 {
			p.write(", ")
		}
		keyNode := pr[1].(S)
		valNode := pr[2].(S)

		// Unwrap key annotation (keys are PRE-only in the grammar).
		key, keyAnn := unwrapKey(keyNode)
		keyPre, keyPost := splitAnnotText(keyAnn)

		// Unwrap value annotation if present.
		valPre, valPost := "", ""
		if tag(valNode) == "annot" {
			txt, wrapped, pre := asAnnot(valNode)
			if pre {
				valPre = txt
			} else {
				valPost = txt
			}
			valNode = wrapped
		}

		// PRE annotations before the entry.
		if keyPre != "" {
			p.out.annot(keyPre)
			p.pad()
		}
		if valPre != "" {
			p.out.annot(valPre)
			p.pad()
		}

		// key: value
		p.pad()
		keyOut(&p.out, key)
		p.write(": ")
		p.printExpr(valNode)

		// POST annotations after value, inline.
		if valPost != "" {
			p.out.annotInline(valPost)
		}
		if keyPost != "" {
			p.out.annotInline(keyPost)
		}
	}
	p.write("}")
}

/* ---------- patterns ---------- */

func isDeclPattern(n S) bool {
	switch tag(n) {
	case "decl", "darr", "dobj":
		return true
	case "annot":
		return isDeclPattern(n[2].(S))
	default:
		return false
	}
}

// unwrapKey returns the bare key name and its (possibly empty) annotation text.
// The annotation, if present, is assumed to be PRE in parsed keys, but we still
// return the raw text to let the printers decide (POST would be treated inline).
func unwrapKey(n S) (name string, annot string) {
	if tag(n) == "annot" {
		return n[2].(S)[1].(string), n[1].(S)[1].(string)
	}
	return n[1].(string), ""
}

func (p *pp) printPattern(n S) {
	switch tag(n) {
	case "decl":
		p.write(getId(n))
	case "darr":
		bracketed(&p.out, "[", "]", listS(n, 1), func(s S) { p.printPattern(s) })
	case "dobj":
		items := listS(n, 1)
		multi := false
		for _, it := range items {
			_, ann := unwrapKey(it[1].(S))
			if ann != "" || tag(it[2].(S)) == "annot" {
				multi = true
				break
			}
		}
		if !multi {
			p.write("{")
			for i, it := range items {
				if i > 0 {
					p.write(", ")
				}
				key, _ := unwrapKey(it[1].(S))
				keyOut(&p.out, key)
				p.write(": ")
				p.printPattern(it[2].(S))
			}
			p.write("}")
			return
		}
		p.write("{")
		p.nl()
		p.out.withIndent(func() {
			for i, it := range items {
				key, ann := unwrapKey(it[1].(S))
				keyPre, keyPost := splitAnnotText(ann)
				if keyPre != "" {
					p.out.annot(keyPre)
				}
				p.pad()
				keyOut(&p.out, key)
				p.write(": ")
				val := it[2].(S)
				if tag(val) == "annot" {
					txt, wrapped, pre := asAnnot(val)
					if pre {
						p.nl()
						p.out.annot(txt)
						p.pad()
						keyOut(&p.out, key)
						p.write(": ")
						p.printPattern(wrapped)
					} else {
						p.printPattern(wrapped)
						p.out.annotInline(txt)
					}
				} else {
					p.printPattern(val)
				}
				if keyPost != "" {
					p.out.annotInline(keyPost)
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
		text, wrapped, pre := asAnnot(n)
		if pre {
			p.out.annot(text)
			p.pad()
			p.printPattern(wrapped)
		} else {
			p.pad()
			p.printPattern(wrapped)
			p.out.annotInline(text)
		}
	default:
		p.printExpr(n)
	}
}

/* ---------- Type pretty-printer ---------- */

func writeType(o *out, t S) {
	if len(t) == 0 {
		o.write("<type>")
		return
	}
	switch tag(t) {
	case "id":
		o.write(getStr(t))
	case "unop":
		if t[1].(string) == "?" {
			writeType(o, t[2].(S))
			o.write("?")
		} else {
			o.write("<unop>")
		}
	case "array":
		elem := S{"id", "Any"}
		if len(t) == 2 {
			elem = t[1].(S)
		}
		bracketed(o, "[", "]", []S{elem}, func(s S) { writeType(o, s) }) // prints [T]
	case "enum":
		bracketed(o, "Enum[", "]", listS(t, 1), func(s S) { writeTypeLiteral(o, s) })
	case "map":
		type fld struct {
			name     string
			req      bool
			typ      S
			kAnnPre  string
			kAnnPost string
			vAnnPre  string
			vAnnPost string
		}
		var fs []fld
		for _, raw := range listS(t, 1) {
			req := raw[0].(string) == "pair!"
			k, kAnn := unwrapKey(raw[1].(S))
			kPre, kPost := splitAnnotText(kAnn)
			ft := raw[2].(S)
			vPre, vPost := "", ""
			if len(ft) > 0 && tag(ft) == "annot" {
				txt, inner, pre := asAnnot(ft)
				if pre {
					vPre = txt
				} else {
					vPost = txt
				}
				ft = inner
			}
			fs = append(fs, fld{
				name: k, req: req, typ: ft,
				kAnnPre: kPre, kAnnPost: kPost,
				vAnnPre: vPre, vAnnPost: vPost,
			})
		}
		sort.Slice(fs, func(i, j int) bool { return fs[i].name < fs[j].name })
		o.write("{")
		o.nl()
		o.withIndent(func() {
			for i, f := range fs {
				if f.kAnnPre != "" {
					o.annot(f.kAnnPre)
				}
				if f.vAnnPre != "" {
					o.annot(f.vAnnPre)
				}
				o.pad()
				keyOut(o, f.name)
				if f.req {
					o.write("!")
				}
				o.write(": ")
				writeType(o, f.typ)
				if f.vAnnPost != "" {
					o.annotInline(f.vAnnPost)
				}
				if f.kAnnPost != "" {
					o.annotInline(f.kAnnPost)
				}
				if i < len(fs)-1 {
					o.write(",")
				}
				o.nl()
			}
		})
		o.pad()
		o.write("}")
	case "binop":
		if t[1].(string) == "->" && len(t) >= 4 {
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
		} else {
			o.write("<binop>")
		}
	case "annot":
		txt, wrapped, pre := asAnnot(t)
		if pre {
			o.annot(txt)
			writeType(o, wrapped)
		} else {
			writeType(o, wrapped)
			o.annotInline(txt)
		}
	default:
		o.write("<type>")
	}
}

func writeTypeLiteral(o *out, lit S) {
	switch tag(lit) {
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
		o.write(quoteString(getStr(lit)))
	case "array":
		bracketed(o, "[", "]", listS(lit, 1), func(s S) { writeTypeLiteral(o, s) })
	case "map":
		o.write("{")
		items := listS(lit, 1)
		for i, pr := range items {
			if i > 0 {
				o.write(", ")
			}
			k := pr[1].(S)[1].(string)
			keyOut(o, k)
			o.write(": ")
			writeTypeLiteral(o, pr[2].(S))
		}
		o.write("}")
	default:
		o.write("<lit>")
	}
}

func flattenArrow(t S) (params []S, ret S) {
	for tag(t) == "binop" && t[1].(string) == "->" && len(t) >= 4 {
		params = append(params, t[2].(S))
		t = t[3].(S)
	}
	ret = t
	return
}

/* ---------- Runtime value pretty-printer ---------- */

// Single-line candidate for a map. Respects PRE/POST:
// - PRE in key or value → force multi-line (return "").
// - POST allowed inline (appended after value).
func mapOneLineMO(keys []string, mo *MapObject) string {
	if len(keys) == 0 {
		return "{}"
	}
	parts := make([]string, 0, len(keys))
	for _, k := range keys {
		kAnn := ""
		if ann, ok := mo.KeyAnn[k]; ok {
			kAnn = ann
		}
		kPre, kPost := splitAnnotText(kAnn)
		if kPre != "" {
			return ""
		}
		v := mo.Entries[k]
		if isValueMultiline(v) { // honors PRE as multiline, POST ok
			return ""
		}
		key := k
		if !isIdent(key) {
			key = quoteString(key)
		}
		var b strings.Builder
		o := out{b: &b}
		// Render value without its PRE/POST annotations here;
		// attach POST inline after the value if present.
		vPre, vPost := splitAnnotText(v.Annot)
		if vPre != "" {
			return ""
		}
		vNoAnn := v
		vNoAnn.Annot = ""
		writeValue(&o, vNoAnn)
		valStr := b.String()
		if vPost != "" {
			valStr += " # " + oneLine(vPost)
		}
		if kPost != "" {
			valStr += " # " + oneLine(kPost)
		}
		parts = append(parts, key+": "+valStr)
	}
	return "{ " + strings.Join(parts, ", ") + " }"
}

func writeValue(o *out, v Value) {
	// PRE vs POST for the value itself.
	pre, post := splitAnnotText(v.Annot)
	if pre != "" {
		o.annot(pre)
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
			if post != "" {
				o.annotInline(post)
			}
			return
		}
		o.write("[")
		o.nl()
		o.withIndent(func() {
			for i, it := range xs {
				itPre, itPost := splitAnnotText(it.Annot)
				if itPre != "" {
					o.annot(itPre)
				}
				o.pad()
				itNoAnn := it
				itNoAnn.Annot = ""
				writeValue(o, itNoAnn)
				if itPost != "" {
					o.annotInline(itPost)
				}
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
		keys := append([]string(nil), mo.Keys...)
		sort.Strings(keys)
		if oneline := mapOneLineMO(keys, mo); oneline != "" && len(oneline) <= MaxInlineWidth {
			o.write(oneline)
			if post != "" {
				o.annotInline(post)
			}
			return
		}
		o.write("{")
		o.nl()
		o.withIndent(func() {
			for i, k := range keys {
				kAnn := ""
				if ann, ok := mo.KeyAnn[k]; ok {
					kAnn = ann
				}
				kPre, kPost := splitAnnotText(kAnn)
				vv := mo.Entries[k]
				vPre, vPost := splitAnnotText(vv.Annot)

				// PRE annotations before entry.
				if kPre != "" {
					o.annot(kPre)
				}
				if vPre != "" {
					o.annot(vPre)
				}

				o.pad()
				keyOut(o, k)
				o.write(": ")

				// Print value without its own annotations (handled above/below).
				vNoAnn := vv
				vNoAnn.Annot = ""
				writeValue(o, vNoAnn)

				// Append POST (value, then key) inline.
				if vPost != "" {
					o.annotInline(vPost)
				}
				if kPost != "" {
					o.annotInline(kPost)
				}

				if i < len(keys)-1 {
					o.write(",")
				}
				o.nl()
			}
		})
		o.pad()
		o.write("}")
	case VTFun:
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
		typ := FormatType(typeAst(v.Data))
		if strings.Contains(typ, "\n") {
			for i, ln := range strings.Split(typ, "\n") {
				if i == 0 {
					o.write(ln)
					continue
				}
				o.nl()
				o.pad()
				o.write(ln)
			}
		} else {
			o.write(typ)
		}
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

	// POST (inline) for the value itself.
	if post != "" {
		o.annotInline(post)
	}
}

/* ---------- single-line candidates ---------- */

func arrayOneLine(xs []Value) string {
	if len(xs) == 0 {
		return "[]"
	}
	parts := make([]string, 0, len(xs))
	for _, it := range xs {
		// PRE annotation in any element forces multiline.
		pre, post := splitAnnotText(it.Annot)
		if pre != "" || isValueMultiline(it) {
			return ""
		}
		// Render element without ANN and append inline POST if present.
		var b strings.Builder
		o := out{b: &b}
		itNoAnn := it
		itNoAnn.Annot = ""
		writeValue(&o, itNoAnn)
		elem := b.String()
		if post != "" {
			elem += " # " + oneLine(post)
		}
		parts = append(parts, elem)
	}
	return "[ " + strings.Join(parts, ", ") + " ]"
}

func isValueMultiline(v Value) bool {
	// PRE annotation on the value itself forces multiline.
	pre, _ := splitAnnotText(v.Annot)
	if pre != "" {
		return true
	}
	switch v.Tag {
	case VTArray:
		xs := v.Data.([]Value)
		if len(xs) == 0 {
			return false
		}
		for _, it := range xs {
			// Any PRE in nested items forces multiline.
			ipre, _ := splitAnnotText(it.Annot)
			if ipre != "" || isValueMultiline(it) {
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
		return len(t) > 0 && tag(t) == "map" && len(t) > 1
	default:
		return false
	}
}
