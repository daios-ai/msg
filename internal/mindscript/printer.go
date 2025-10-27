// printer.go: pretty-printers for MindScript ASTs, types, and runtime values.
//
// What this file does
// -------------------
// This module provides the formatting layer for MindScript. It renders two
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
//     ("annot", ("str", text), wrappedNode)
//     PRE/POST is *not* encoded by the parser anymore. All annotations are
//     attached to values (or noops), and the pretty-printer decides PRE vs POST
//     based on layout at binding sites.
//     - Formatting emits no space before '(' for calls and for 'fun(...)'
//     and 'oracle(...)' parameter lists, matching the lexer’s CLROUND rule.
//     - Control keywords render without parens:
//     return expr
//     break expr
//     continue [expr]
//     A `null` payload prints as the bare keyword (e.g., `continue`).
//
//  2. Type ASTs (S-expressions) → compact type strings.
//     - Entry point: FormatType.
//     - Supported forms:
//     ("id", "Any"|"Null"|"Bool"|"Int"|"Num"|"Str"|"Type")
//     ("unop","?", T)         → prints as `T?`
//     ("array", T)            → prints as `[T]`
//     ("map", ("pair"| "pair!", ("str",k), T) ...)
//     Required fields print with a trailing `!` on the key.
//     Value annotations (if wrapped in "annot") are respected and decided
//     PRE vs POST by the same centralized policy as expressions.
//     ("enum", literalS... )  → prints as `Enum[ ... ]`, where members
//     may be scalars, arrays, or maps.
//     ("binop","->", A, B)    → prints as `(A) -> B`, flattened across
//     right-associated chains.
//     - Output is stable. Multi-line maps are rendered with sorted keys to
//     avoid visual churn.
//     - When the last field ends with a POST, the closing `}` appears on the
//     next line without an extra blank line.
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
//   - Fun, TypeValue, MapObject (Entries/Keys).
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
//
// Requiredness in value maps
// --------------------------
// The printer never emits required fields ("pair!") in **expression/value maps**:
// requiredness is a **type-level** concept only. If the AST carried "pair!" in a
// value map (e.g., via parser sugar), it is dropped in the printed code.
//
// Canonicalizations & Omissions (parser ↔ printer contract)
// ---------------------------------------------------------
// These are deliberate simplifications made by the parser and normalized by
// the printer; users may not see certain syntactic sugar re-emitted:
//   - Param types default to `Any` and are not printed (e.g., `fun(x)` not `x: Any`).
//   - Function return type defaults to `Any` and is not printed (`fun(...) do ... end`
//     without `-> Any`).
//   - `oracle(...)` without `from` carries an empty default source; `from ...` is omitted.
//   - Bare `return` / `break` / `continue` carry an implicit `null` value and print
//     as the bare keyword (no `null`).
//   - Redundant parentheses are removed; only minimal parentheses are emitted.
//   - Calls print with no space before '(' (canonical `f(x)` form).
//   - Property indices written as `obj.(expr)` or `obj.12` are printed canonically
//     as `obj[expr]` / `obj[12]`.
//   - Trailing commas in arrays/maps/parameter lists are dropped in output.
//   - Map keys that are identifier-like print without quotes; others are quoted.
//   - **Expression maps** ignore the required marker `!` at runtime; the printer
//     therefore **drops `!` in value maps** (e.g., `{ id!: 1 }` → `{ id: 1 }`).
//     (Type maps still print required keys as `key!`.)
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
// FormatValue / FormatType / FormatSExpr. The single-line decision accounts for
// the current indentation; i.e., it uses the remaining space on the line after
// tabs (tab width = 4) and any preceding text.
var MaxInlineWidth = 80

// Pretty parses a MindScript source string and returns a formatted version.
//
// Behavior:
//   - Parses src via ParseSExpr. If parsing fails, the error is wrapped with
//     source context via WrapErrorWithSource.
//   - On success, pretty-prints the AST using FormatSExpr, producing stable,
//     whitespace-normalized code with minimal parentheses.
//   - Supports annotations using the 3-ary form:
//     ("annot", ("str", text), X)
//     PRE/POST is chosen by the pretty-printer at binding sites.
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
//   - Arrays and maps are printed inline (AST form).
//   - Annotation nodes wrap the printed construct; PRE vs POST is chosen centrally.
//   - **POST-after-separator rule** is enforced for inline cases.
//
// This function does not parse; it strictly formats the provided AST.
func FormatSExpr(n S) string {
	doc := docProgram(n)
	var b strings.Builder
	r := renderer{
		out:      &b,
		maxWidth: MaxInlineWidth,
		tabWidth: 4,
	}
	r.render(doc)
	return strings.TrimRight(b.String(), "\n")
}

// FormatType renders a type S-expression into a compact, human-readable string.
// It uses the same centralized PRE/POST policy for value annotations inside
// type maps and enum literals.
func FormatType(t S) string {
	doc := docType(t)
	var b strings.Builder
	r := renderer{
		out:      &b,
		maxWidth: MaxInlineWidth,
		tabWidth: 4,
	}
	r.render(doc)
	return b.String()
}

// FormatValue renders a runtime Value by first adapting it to the printer’s AST
// (with cycle guards and opaque fallbacks) and then delegating to FormatSExpr.
func FormatValue(v Value) string {
	ast := ValueToAST(v)
	return FormatSExpr(ast)
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

// NOTE: The parser no longer encodes PRE vs POST; all annotations live on values.
// The pretty-printer chooses PRE vs POST purely by layout at binding sites.

/* ---------- Doc engine (tiny) ---------- */

type docKind int

const (
	dText     docKind = iota
	dLine             // space if flat, newline if broken
	dSoftLine         // empty if flat, newline if broken
	dHardLine         // always newline
	dGroup
	dNest
	dConcat
)

type Doc struct {
	k      docKind
	s      string
	a      *Doc
	kids   []*Doc
	indent int // for Nest
}

func Text(s string) *Doc      { return &Doc{k: dText, s: s} }
func LineDoc() *Doc           { return &Doc{k: dLine} }
func SoftLineDoc() *Doc       { return &Doc{k: dSoftLine} }
func HardLineDoc() *Doc       { return &Doc{k: dHardLine} }
func Group(d *Doc) *Doc       { return &Doc{k: dGroup, a: d} }
func Nest(n int, d *Doc) *Doc { return &Doc{k: dNest, a: d, indent: n} }
func Concat(ds ...*Doc) *Doc  { return &Doc{k: dConcat, kids: ds} }

func Join(sep *Doc, items []*Doc) *Doc {
	if len(items) == 0 {
		return Concat()
	}
	out := make([]*Doc, 0, len(items)*2-1)
	for i, it := range items {
		if i > 0 {
			out = append(out, sep)
		}
		out = append(out, it)
	}
	return Concat(out...)
}

type renderer struct {
	out      *strings.Builder
	maxWidth int
	tabWidth int

	col         int  // current column in characters (tabs count as tabWidth)
	depth       int  // indentation depth (tabs)
	atLineStart bool // just after newline
}

func (r *renderer) writeIndentIfNeeded() {
	if r.atLineStart {
		for i := 0; i < r.depth; i++ {
			r.out.WriteByte('\t')
		}
		r.col = r.depth * r.tabWidth
		r.atLineStart = false
	}
}
func (r *renderer) writeString(s string) {
	if s == "" {
		return
	}
	r.writeIndentIfNeeded()
	r.out.WriteString(s)
	r.col += len(s)
}
func (r *renderer) newline() {
	r.out.WriteByte('\n')
	r.atLineStart = true
	// col will be set when indent is written
}

func (r *renderer) render(d *Doc) {
	r.atLineStart = false // caller controls leading indentation
	r.renderGroup(d)
}

func (r *renderer) renderGroup(d *Doc) {
	// Render a group with "flat if fits" policy.
	if r.fitsFlat(d, r.maxWidth-r.col) {
		r.renderFlat(d)
	} else {
		r.renderBroken(d)
	}
}

func (r *renderer) renderFlat(d *Doc) {
	switch d.k {
	case dText:
		r.writeString(d.s)
	case dLine:
		r.writeString(" ")
	case dSoftLine:
		// nothing
	case dHardLine:
		// hard line cannot appear in flat mode if fitsFlat was true,
		// but guard just in case: break the line.
		r.newline()
	case dGroup:
		r.renderFlat(d.a)
	case dNest:
		old := r.depth
		r.depth += d.indent
		r.renderFlat(d.a)
		r.depth = old
	case dConcat:
		for _, k := range d.kids {
			r.renderFlat(k)
		}
	}
}

func (r *renderer) renderBroken(d *Doc) {
	switch d.k {
	case dText:
		r.writeString(d.s)
	case dLine:
		r.newline()
	case dSoftLine:
		r.newline()
	case dHardLine:
		r.newline()
	case dGroup:
		// In broken mode, nested groups still try flat if they fit at this point.
		r.renderGroup(d.a)
	case dNest:
		old := r.depth
		r.depth += d.indent
		r.renderBroken(d.a)
		r.depth = old
	case dConcat:
		for _, k := range d.kids {
			r.renderBroken(k)
		}
	}
}

// fitsFlat reports whether the doc can be rendered flat within the given budget.
// Any HardLine inside makes it not flat-fit.
func (r *renderer) fitsFlat(d *Doc, budget int) bool {
	if budget < 0 {
		return false
	}
	switch d.k {
	case dText:
		return len(d.s) <= budget
	case dLine:
		return 1 <= budget
	case dSoftLine:
		return 0 <= budget
	case dHardLine:
		return false
	case dGroup:
		return r.fitsFlat(d.a, budget)
	case dNest:
		return r.fitsFlat(d.a, budget)
	case dConcat:
		for _, k := range d.kids {
			if !r.fitsFlat(k, budget) {
				return false
			}
			// reduce budget by flat width of k
			budget -= flatWidth(k)
		}
		return true
	default:
		return false
	}
}

func flatWidth(d *Doc) int {
	switch d.k {
	case dText:
		return len(d.s)
	case dLine:
		return 1
	case dSoftLine:
		return 0
	case dHardLine:
		return 1 // arbitrary; but any hardline makes fitsFlat false before using this
	case dGroup:
		return flatWidth(d.a)
	case dNest:
		return flatWidth(d.a)
	case dConcat:
		sum := 0
		for _, k := range d.kids {
			sum += flatWidth(k)
		}
		return sum
	default:
		return 0
	}
}

/* ---------- shared Doc helpers ---------- */

func idOrQuoted(name string) *Doc {
	if isIdent(name) {
		return Text(name)
	}
	return Text(quoteString(name))
}

// PRE annotations (block/head) — prints as lines above current position.
func annotPre(text string) *Doc {
	if strings.TrimSpace(text) == "" {
		return Concat()
	}
	lines := strings.Split(text, "\n")
	ds := make([]*Doc, 0, len(lines)*2)
	for _, ln := range lines {
		ln = strings.TrimSpace(ln)
		ds = append(ds, Text("# "+ln), HardLineDoc())
	}
	return Concat(ds...)
}

// POST annotations (inline/trailing) — prints on the same line.
// IMPORTANT: POST captures the rest of the line, so we force a newline here.
func annotInline(text string) *Doc {
	trim := oneLine(text)
	if trim == "" {
		return Concat()
	}
	return Concat(Text(" # "+trim), HardLineDoc())
}

func braced(open string, inside *Doc, close string) *Doc {
	return Concat(Text(open), inside, Text(close))
}

// inlineOrMultiAdvanced builds a `[ a, b ]` or multi-line with indentation.
// If endsLastLine is true, the trailing SoftLine is omitted to avoid an extra
// blank line before the closing bracket/brace.
func inlineOrMultiAdvanced(open string, elems []*Doc, close string, endsLastLine bool) *Doc {
	if len(elems) == 0 {
		// exact-empty without spaces: [] or {}
		return Text(open + close)
	}
	sep := Concat(Text(","), LineDoc())
	inside := Join(sep, elems)
	body := Concat(SoftLineDoc(), inside)
	if !endsLastLine {
		body = Concat(body, SoftLineDoc())
	}
	return Group(braced(open, Nest(1, body), close))
}

// inlineOrMulti is the default variant when the last element does not force
// a newline (or when callers don't track it).
func inlineOrMulti(open string, elems []*Doc, close string) *Doc {
	return inlineOrMultiAdvanced(open, elems, close, false)
}

// Minimal entry builder; annotation handling is centralized elsewhere.
func kvEntry(keyDoc *Doc, valDoc *Doc) *Doc {
	return Concat(keyDoc, Text(": "), valDoc)
}

/* ---------- Comma-aware joining (centralized POST-after-comma logic) ---------- */

type sepItem struct {
	main *Doc // rendered item (element or entry) without its trailing POST
	post string
}

// joinCommaWithPost joins items with commas, printing any item's POST
// *after the comma that follows that item*. The last item's POST (if any)
// prints after the item (no comma). POST forces newline via annotInline.
func joinCommaWithPost(items []sepItem) *Doc {
	if len(items) == 0 {
		return Concat()
	}
	out := make([]*Doc, 0, len(items)*3)
	for i, it := range items {
		out = append(out, it.main)
		if i < len(items)-1 {
			out = append(out, Text(","))
			if it.post != "" {
				out = append(out, annotInline(it.post))
			} else {
				out = append(out, LineDoc())
			}
		} else if it.post != "" {
			out = append(out, annotInline(it.post))
		}
	}
	return Concat(out...)
}

/* ---------- AST helpers: tags, shapes, precedence ---------- */

func tag(n S) string   { return n[0].(string) }
func getId(n S) string { return n[1].(string) }
func getStr(n S) string {
	// Used for ("str", s), but safe for ("id", name) too.
	return n[1].(string)
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

// Keys/names are not annotated; unwrap name only.
func unwrapKeyName(n S) string { return n[1].(string) }

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

func exprPrec(n S) int {
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

func parenIf(need int, d *Doc, n S) *Doc {
	if exprPrec(n) < need {
		return Concat(Text("("), d, Text(")"))
	}
	return d
}

func parenIfLE(need int, d *Doc, n S) *Doc {
	if exprPrec(n) <= need {
		return Concat(Text("("), d, Text(")"))
	}
	return d
}

/* ---------- AST → Doc ---------- */

func docProgram(n S) *Doc {
	if tag(n) != "block" {
		return docStmt(n)
	}
	kids := listS(n, 1)
	var ds []*Doc
	firstReal := true
	for _, k := range kids {
		if tag(k) == "noop" {
			continue
		}
		if !firstReal {
			ds = append(ds, HardLineDoc())
		}
		ds = append(ds, docStmt(k))
		firstReal = false
	}
	return Concat(ds...)
}

func docStmt(n S) *Doc {
	switch tag(n) {
	case "noop":
		return Concat()

	case "annot":
		// Apply the centralized inline-vs-PRE policy even at statement level.
		// If it fits, place as a trailing inline comment on the same line;
		// otherwise render as a PRE header.
		text, wrapped, _ := asAnnotASTRaw(n)
		body := docStmt(wrapped)
		main, post := attachInlineOrPre(body, text)
		if post != "" {
			// Inline comments consume the rest of the line; force newline.
			return Concat(body, annotInline(post))
		}
		return main

	case "fun":
		params, ret, body := n[1].(S), n[2].(S), n[3].(S)
		header := Concat(Text("fun("), docParams(params), Text(")"))
		if !(tag(ret) == "id" && getId(ret) == "Any") {
			header = Concat(header, Text(" -> "), docType(ret))
		}
		return Concat(
			header, Text(" do"), HardLineDoc(),
			Nest(1, docBlock(body)), HardLineDoc(),
			Text("end"),
		)

	case "oracle":
		params, outT, src := n[1].(S), n[2].(S), n[3].(S)
		header := Concat(Text("oracle("), docParams(params), Text(")"))
		if !(tag(outT) == "id" && getId(outT) == "Any") {
			header = Concat(header, Text(" -> "), docType(outT))
		}
		if !(tag(src) == "array" && len(src) == 1) {
			header = Concat(header, Text(" from "), docExpr(src))
		}
		return header

	case "for":
		tgt, iter, body := n[1].(S), n[2].(S), n[3].(S)
		// Target never prints "let" — it's implied in the surface syntax.
		head := Concat(Text("for "), docPattern(tgt), Text(" in "), docExpr(iter), Text(" do"))
		return Concat(head, HardLineDoc(), Nest(1, docBlock(body)), HardLineDoc(), Text("end"))

	case "while":
		cond, body := n[1].(S), n[2].(S)
		head := Concat(Text("while "), docExpr(cond), Text(" do"))
		return Concat(head, HardLineDoc(), Nest(1, docBlock(body)), HardLineDoc(), Text("end"))

	case "if":
		arms := listS(n, 1)
		first := arms[0]
		d := Concat(
			Text("if "), docExpr(first[1].(S)), Text(" then"), HardLineDoc(),
			Nest(1, docBlock(first[2].(S))),
		)
		for i := 1; i < len(arms) && tag(arms[i]) == "pair"; i++ {
			arm := arms[i]
			d = Concat(d, HardLineDoc(),
				Text("elif "), docExpr(arm[1].(S)), Text(" then"), HardLineDoc(),
				Nest(1, docBlock(arm[2].(S))),
			)
		}
		// possible else block
		if last := arms[len(arms)-1]; tag(last) != "pair" {
			d = Concat(d, HardLineDoc(), Text("else"), HardLineDoc(), Nest(1, docBlock(last)))
		}
		return Concat(d, HardLineDoc(), Text("end"))

	case "module":
		nameExpr, body := n[1].(S), n[2].(S)
		return Concat(Text("module "), docExpr(nameExpr), Text(" do"), HardLineDoc(),
			Nest(1, docBlock(body)), HardLineDoc(), Text("end"))

	case "type":
		return Concat(Text("type "), docType(n[1].(S)))

	case "return":
		arg := n[1].(S)
		if tag(arg) == "null" {
			return Text("return")
		}
		return Concat(Text("return "), docExpr(arg))
	case "break":
		arg := n[1].(S)
		if tag(arg) == "null" {
			return Text("break")
		}
		return Concat(Text("break "), docExpr(arg))
	case "continue":
		arg := n[1].(S)
		if tag(arg) == "null" {
			return Text("continue")
		}
		return Concat(Text("continue "), docExpr(arg))

	case "decl", "darr", "dobj":
		return Concat(Text("let "), docPattern(n))
	case "assign":
		// Decide PRE vs POST for the whole binding (let-or-assign).
		lhs, rhs := n[1].(S), n[2].(S)
		var head *Doc
		if isDeclPattern(lhs) {
			head = Concat(Text("let "), docPattern(lhs), Text(" = "))
		} else {
			head = Concat(docExprMin(lhs, 10), Text(" = "))
		}
		if txt, inner, ok := asAnnotASTRaw(rhs); ok && strings.TrimSpace(txt) != "" {
			val := docExprMin(inner, 10)
			probe := Concat(head, val)
			main, post := attachInlineOrPre(probe, txt)
			if post != "" {
				return Concat(head, val, annotInline(post))
			}
			return main
		}
		return Concat(head, docExprMin(rhs, 10))

	case "block":
		return Concat(Text("do"), HardLineDoc(), Nest(1, docBlock(n)), HardLineDoc(), Text("end"))

	default:
		return docExpr(n)
	}
}

func docBlock(n S) *Doc {
	if tag(n) != "block" {
		return docStmt(n)
	}
	kids := listS(n, 1)
	var ds []*Doc
	firstReal := true
	for _, k := range kids {
		if tag(k) == "noop" {
			continue
		}
		if !firstReal {
			ds = append(ds, HardLineDoc())
		}
		ds = append(ds, docStmt(k))
		firstReal = false
	}
	return Concat(ds...)
}

func docParams(arr S) *Doc {
	if tag(arr) != "array" || len(arr) == 1 {
		return Concat()
	}
	items := listS(arr, 1)
	var parts []*Doc
	for i, pi := range items {
		name := getId(pi[1].(S))
		ty := pi[2].(S)
		if !(tag(ty) == "id" && getId(ty) == "Any") {
			parts = append(parts, Concat(Text(name), Text(": "), docType(ty)))
		} else {
			parts = append(parts, Text(name))
		}
		if i < len(items)-1 {
			parts = append(parts, Text(", "))
		}
	}
	return Concat(parts...)
}

func docExprMin(n S, need int) *Doc {
	return parenIf(need, docExpr(n), n)
}

// (old trailing-post helpers removed; annotation policy is centralized)

func docExpr(n S) *Doc {
	switch tag(n) {
	case "id":
		return Text(getId(n))
	case "int":
		return Text(fmt.Sprint(n[1]))
	case "num":
		s := strconv.FormatFloat(n[1].(float64), 'g', -1, 64)
		if !strings.ContainsAny(s, ".eE") {
			s += ".0"
		}
		return Text(s)
	case "str":
		return Text(quoteString(getStr(n)))
	case "bool":
		if n[1].(bool) {
			return Text("true")
		}
		return Text("false")
	case "null":
		return Text("null")

	case "unop":
		op, rhs := n[1].(string), n[2].(S)
		if op == "?" {
			return Concat(docExprMin(rhs, 90), Text("?"))
		}
		if op == "not" {
			return Concat(Text("not "), docExprMin(rhs, 80))
		}
		return Concat(Text(op), docExprMin(rhs, 80))

	case "binop":
		op, l, r := n[1].(string), n[2].(S), n[3].(S)
		my, right := 60, false
		if pr, ok := binPrec[op]; ok {
			my, right = pr.p, pr.right
		}
		// Associativity-aware parentheses:
		//  - right-assoc:  paren LEFT if prec(left) <= my; RIGHT if prec(right) < my
		//  - left-assoc:   paren LEFT if prec(left) <  my; RIGHT if prec(right) <= my
		lDoc := docExpr(l)
		rDoc := docExpr(r)
		if right {
			lDoc = parenIfLE(my, lDoc, l) // inclusive on left
			rDoc = parenIf(my, rDoc, r)   // exclusive on right
		} else {
			lDoc = parenIf(my, lDoc, l)   // exclusive on left
			rDoc = parenIfLE(my, rDoc, r) // inclusive on right
		}
		return Concat(lDoc, Text(" "+op+" "), rDoc)

	case "assign":
		l, r := n[1].(S), n[2].(S)
		return Concat(docExprMin(l, 10), Text(" = "), docExprMin(r, 10))

	case "call":
		recv := n[1].(S)
		args := listS(n, 2)
		var argDocs []*Doc
		for _, a := range args {
			argDocs = append(argDocs, docExpr(a))
		}
		return Concat(docExprMin(recv, 90), Text("("), Join(Text(", "), argDocs), Text(")"))

	case "idx":
		recv, ix := n[1].(S), n[2].(S)
		// Be careful with array indices: this is indexing, not array literal.
		return Concat(docExprMin(recv, 90), Text("["), docExpr(ix), Text("]"))

	case "get":
		recv, name := n[1].(S), n[2].(S)[1].(string)
		if isIdent(name) {
			return Concat(docExprMin(recv, 90), Text("."+name))
		}
		return Concat(docExprMin(recv, 90), Text("."+quoteString(name)))

	case "array":
		elems := listS(n, 1)
		if len(elems) == 0 {
			return Text("[]")
		}
		items := make([]sepItem, 0, len(elems))
		for _, e := range elems {
			if txt, inner, ok := asAnnotASTRaw(e); ok {
				main, post := attachInlineOrPre(docExpr(inner), txt)
				items = append(items, sepItem{main: main, post: post})
			} else {
				items = append(items, sepItem{main: docExpr(e), post: ""})
			}
		}
		inside := joinCommaWithPost(items)
		lastEnds := items[len(items)-1].post != ""
		return Group(braced("[", Nest(1, Concat(SoftLineDoc(), inside, func() *Doc {
			if lastEnds {
				return Concat()
			}
			return SoftLineDoc()
		}())), "]"))

	case "map":
		items := listS(n, 1)
		if len(items) == 0 {
			return Text("{}")
		}
		joined := make([]sepItem, 0, len(items))
		for _, pr := range items {
			key := unwrapKeyName(pr[1].(S))
			val := pr[2].(S)
			if txt, inner, ok := asAnnotASTRaw(val); ok {
				joined = append(joined, entryWithAnn(idOrQuoted(key), docExpr(inner), txt))
			} else {
				joined = append(joined, sepItem{main: kvEntry(idOrQuoted(key), docExpr(val)), post: ""})
			}
		}
		inside := joinCommaWithPost(joined)
		lastEnds := joined[len(joined)-1].post != ""
		return Group(braced("{", Nest(1, Concat(SoftLineDoc(), inside, func() *Doc {
			if lastEnds {
				return Concat()
			}
			return SoftLineDoc()
		}())), "}"))

	case "enum":
		elems := listS(n, 1)
		if len(elems) == 0 {
			return Text("Enum[]")
		}
		items := make([]sepItem, 0, len(elems))
		for _, e := range elems {
			if txt, inner, ok := asAnnotASTRaw(e); ok {
				main, post := attachInlineOrPre(docExpr(inner), txt)
				items = append(items, sepItem{main: main, post: post})
			} else {
				items = append(items, sepItem{main: docExpr(e), post: ""})
			}
		}
		inside := joinCommaWithPost(items)
		lastEnds := items[len(items)-1].post != ""
		return Group(braced("Enum[", Nest(1, Concat(SoftLineDoc(), inside, func() *Doc {
			if lastEnds {
				return Concat()
			}
			return SoftLineDoc()
		}())), "]"))

	case "decl", "darr", "dobj":
		return docPattern(n)

	case "return", "break", "continue", "fun", "oracle", "for", "while", "if", "type", "block", "annot", "module":
		return docStmt(n)

	case "opaque":
		return Text(getStr(n))

	default:
		return Text("<" + tag(n) + ">")
	}
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

func docPattern(n S) *Doc {
	switch tag(n) {
	case "decl":
		return Text(getId(n))
	case "darr":
		items := listS(n, 1)
		if len(items) == 0 {
			return Text("[]")
		}
		joined := make([]sepItem, 0, len(items))
		for _, it := range items {
			if txt, inner, ok := asAnnotASTRaw(it); ok {
				main, post := attachInlineOrPre(docPattern(inner), txt)
				joined = append(joined, sepItem{main: main, post: post})
			} else {
				joined = append(joined, sepItem{main: docPattern(it), post: ""})
			}
		}
		inside := joinCommaWithPost(joined)
		lastEnds := joined[len(joined)-1].post != ""
		return Group(braced("[", Nest(1, Concat(SoftLineDoc(), inside, func() *Doc {
			if lastEnds {
				return Concat()
			}
			return SoftLineDoc()
		}())), "]"))
	case "dobj":
		items := listS(n, 1)
		if len(items) == 0 {
			return Text("{}")
		}
		joined := make([]sepItem, 0, len(items))
		for _, it := range items {
			key := unwrapKeyName(it[1].(S))
			val := it[2].(S)
			if txt, inner, ok := asAnnotASTRaw(val); ok {
				joined = append(joined, entryWithAnn(idOrQuoted(key), docPattern(inner), txt))
			} else {
				joined = append(joined, sepItem{main: kvEntry(idOrQuoted(key), docPattern(val)), post: ""})
			}
		}
		inside := joinCommaWithPost(joined)
		lastEnds := joined[len(joined)-1].post != ""
		return Group(braced("{", Nest(1, Concat(SoftLineDoc(), inside, func() *Doc {
			if lastEnds {
				return Concat()
			}
			return SoftLineDoc()
		}())), "}"))
	case "annot":
		text, wrapped, _ := asAnnotASTRaw(n)
		// Pattern wrapper: render as PRE. Entry/element sites decide inline.
		return Concat(annotPre(text), docPattern(wrapped))
	default:
		return docExpr(n)
	}
}

/* ---------- AST "annot" helpers ---------- */

// Neutral unwrap: ("annot", ("str", txt), wrapped) → (txt, wrapped, true)
func asAnnotASTRaw(n S) (text string, wrapped S, ok bool) {
	if tag(n) == "annot" {
		return n[1].(S)[1].(string), n[2].(S), true
	}
	return "", n, false
}

/* ---------- Type pretty-printer (as Doc) ---------- */

func docType(t S) *Doc {
	if len(t) == 0 {
		return Text("<type>")
	}
	switch tag(t) {
	case "id":
		return Text(getStr(t))
	case "get":
		recv := t[1].(S)
		prop := t[2].(S)[1].(string)
		// Reuse docType for the receiver so nested gets print as a.b.c
		// If the receiver were ever non-type-ish, docType will fall back gracefully.
		return Concat(docType(recv), Text("."), idOrQuoted(prop))
	case "unop":
		if t[1].(string) == "?" {
			return Concat(docType(t[2].(S)), Text("?"))
		}
		return Text("<unop>")
	case "array":
		elem := S{"id", "Any"}
		if len(t) == 2 {
			elem = t[1].(S)
		}
		return Concat(Text("["), docType(elem), Text("]"))
	case "enum":
		elems := listS(t, 1)
		if len(elems) == 0 {
			return Text("Enum[]")
		}
		var ds []*Doc
		for _, e := range elems {
			ds = append(ds, docTypeLiteral(e))
		}
		return inlineOrMulti("Enum[", ds, "]")
	case "map":
		type fld struct {
			name string
			req  bool
			typ  S
			vAnn string
		}
		var fs []fld
		for _, raw := range listS(t, 1) {
			req := raw[0].(string) == "pair!"
			k := unwrapKeyName(raw[1].(S))
			ft := raw[2].(S)
			txt, inner, ok := asAnnotASTRaw(ft)
			if ok {
				ft = inner
			}
			fs = append(fs, fld{name: k, req: req, typ: ft, vAnn: strings.TrimSpace(txt)})
		}
		sort.Slice(fs, func(i, j int) bool { return fs[i].name < fs[j].name })
		if len(fs) == 0 {
			return Text("{}")
		}
		joined := make([]sepItem, 0, len(fs))
		for _, f := range fs {
			key := idOrQuoted(f.name)
			if f.req {
				key = Concat(key, Text("!"))
			}
			if f.vAnn != "" {
				joined = append(joined, entryWithAnn(key, docType(f.typ), f.vAnn))
			} else {
				joined = append(joined, sepItem{main: kvEntry(key, docType(f.typ)), post: ""})
			}
		}
		inside := joinCommaWithPost(joined)
		lastEnds := joined[len(joined)-1].post != ""
		return Group(braced("{", Nest(1, Concat(SoftLineDoc(), inside, func() *Doc {
			if lastEnds {
				return Concat()
			}
			return SoftLineDoc()
		}())), "}"))
	case "binop":
		if t[1].(string) == "->" && len(t) >= 4 {
			left := t[2].(S)
			right := t[3].(S)
			// Right-associative printing:
			// - If LEFT is itself an arrow, parenthesize it.
			// - Always render RIGHT via docType (which will continue the chain).
			leftDoc := docType(left)
			if tag(left) == "binop" && left[1].(string) == "->" {
				leftDoc = Concat(Text("("), leftDoc, Text(")"))
			}
			return Concat(leftDoc, Text(" -> "), docType(right))
		}
		return Text("<binop>")
	case "annot":
		txt, wrapped, _ := asAnnotASTRaw(t)
		// Outside binding sites, render as PRE.
		return Concat(annotPre(txt), docType(wrapped))
	default:
		return Text("<type>")
	}
}

func docTypeLiteral(lit S) *Doc {
	switch tag(lit) {
	case "null":
		return Text("null")
	case "bool":
		if lit[1].(bool) {
			return Text("true")
		}
		return Text("false")
	case "int":
		return Text(fmt.Sprint(lit[1]))
	case "num":
		return Text(strconv.FormatFloat(lit[1].(float64), 'g', -1, 64))
	case "str":
		return Text(quoteString(getStr(lit)))
	case "array":
		items := listS(lit, 1)
		if len(items) == 0 {
			return Text("[]")
		}
		var ds []*Doc
		for _, s := range items {
			ds = append(ds, docTypeLiteral(s))
		}
		return inlineOrMulti("[", ds, "]")
	case "map":
		items := listS(lit, 1)
		if len(items) == 0 {
			return Text("{}")
		}
		joined := make([]sepItem, 0, len(items))
		for _, pr := range items {
			k := pr[1].(S)[1].(string)
			val := docTypeLiteral(pr[2].(S))
			entry := kvEntry(idOrQuoted(k), val)
			joined = append(joined, sepItem{main: entry, post: ""})
		}
		inside := joinCommaWithPost(joined)
		return Group(braced("{", Nest(1, Concat(SoftLineDoc(), inside, SoftLineDoc())), "}"))
	default:
		return Text("<lit>")
	}
}

/* ---------- Central annotation policy (single place) ---------- */

// Decide inline vs PRE for a candidate: if `candidate + " # ann"` fits flat
// under a conservative width budget, return (candidate, ann) so callers can
// trail as inline (or after-comma). Otherwise return (PRE+candidate, "").
func attachInlineOrPre(candidate *Doc, ann string) (*Doc, string) {
	ann = strings.TrimSpace(ann)
	if ann == "" {
		return candidate, ""
	}
	// Multi-line annotations always become PRE headers.
	if strings.Contains(ann, "\n") {
		return Concat(annotPre(ann), candidate), ""
	}
	// Conservative budget: avoid over-inlining.
	r := renderer{maxWidth: MaxInlineWidth - 8, tabWidth: 4}
	if r.fitsFlat(Concat(candidate, Text(" # "+oneLine(ann))), r.maxWidth) {
		return candidate, ann
	}
	return Concat(annotPre(ann), candidate), ""
}

// Entry helper applying the policy to `key: value`.
// If inline, the POST returns via .post (rendered after the following comma).
// If PRE, the header is part of .main (no trailing POST).
func entryWithAnn(keyDoc, valDoc *Doc, ann string) sepItem {
	probe := kvEntry(keyDoc, valDoc)
	main, post := attachInlineOrPre(probe, ann)
	if post != "" {
		return sepItem{main: probe, post: post}
	}
	return sepItem{main: main, post: ""}
}

/* ---------- Value → AST adapter (single source of truth) ---------- */

// ValueToAST converts a runtime Value into the printer/parser AST (S), preserving:
//   - annotations (as ("annot", ("str", ...), node))
//   - insertion order of maps (MapObject.Keys)
//   - cycle guards (arrays, maps) with python-style markers via ("opaque", "[...]") / ("opaque", "{...}")
//
// Non-source forms (functions, types, modules, handles, unknown) render as ("opaque", "<...>").
func ValueToAST(v Value) S {
	seenA := make(map[*ArrayObject]bool)
	seenM := make(map[*MapObject]bool)
	n := valueToASTRec(v, seenA, seenM)
	if s := strings.TrimSpace(v.Annot); s != "" {
		return S{"annot", S{"str", s}, n}
	}
	return n
}

func valueToASTRec(v Value, seenA map[*ArrayObject]bool, seenM map[*MapObject]bool) S {
	switch v.Tag {
	case VTNull:
		return S{"null"}
	case VTBool:
		return S{"bool", v.Data.(bool)}
	case VTInt:
		return S{"int", v.Data.(int64)}
	case VTNum:
		return S{"num", v.Data.(float64)}
	case VTStr:
		return S{"str", v.Data.(string)}

	case VTArray:
		ao := v.Data.(*ArrayObject)
		if ao == nil {
			return S{"array"} // treat nil as empty
		}
		if seenA[ao] {
			return S{"opaque", "[...]"}
		}
		seenA[ao] = true
		out := S{"array"}
		for _, ev := range ao.Elems {
			node := valueToASTRec(ev, seenA, seenM)
			if ann := strings.TrimSpace(ev.Annot); ann != "" {
				node = S{"annot", S{"str", ann}, node}
			}
			out = append(out, node)
		}
		return out

	case VTMap:
		mo := v.Data.(*MapObject)
		if mo == nil {
			return S{"map"}
		}
		if seenM[mo] {
			return S{"opaque", "{...}"}
		}
		seenM[mo] = true
		out := S{"map"}
		for _, k := range mo.Keys {
			val := mo.Entries[k]
			node := valueToASTRec(val, seenA, seenM)
			// Annotations live on the VALUE.
			if ann := strings.TrimSpace(val.Annot); ann != "" {
				node = S{"annot", S{"str", ann}, node}
			}
			out = append(out, S{"pair", S{"str", k}, node})
		}
		return out

	case VTFun, VTType, VTModule, VTHandle:
		return S{"opaque", valueOpaqueString(v)}
	default:
		return S{"opaque", valueOpaqueString(v)}
	}
}

func valueOpaqueString(v Value) string {
	switch v.Tag {
	case VTFun:
		if f, ok := v.Data.(*Fun); ok && f != nil {
			label := "fun"
			if f.IsOracle {
				label = "oracle"
			}
			var parts []string
			if len(f.ParamTypes) == 0 {
				parts = append(parts, "_:Null")
			} else {
				for i := range f.ParamTypes {
					name := "_"
					if i < len(f.Params) && f.Params[i] != "" {
						name = f.Params[i]
					}
					pt := FormatType(f.ParamTypes[i])
					// Parenthesize arrow types in param position for readability.
					if len(f.ParamTypes[i]) >= 4 && f.ParamTypes[i][0] == "binop" && f.ParamTypes[i][1] == "->" {
						pt = "(" + pt + ")"
					}
					if i > 0 {
						parts = append(parts, "-> "+name+":"+pt)
					} else {
						parts = append(parts, name+":"+pt)
					}
				}
			}
			ret := FormatType(f.ReturnType)
			if len(parts) > 0 {
				return "<" + label + ": " + strings.Join(parts, " ") + " -> " + ret + ">"
			}
			return "<" + label + ": " + ret + ">"
		}
		return "<fun>"
	case VTType:
		return "<type: " + FormatType(typeAst(v.Data)) + ">"
	case VTModule:
		name := "<module>"
		if m, ok := v.Data.(*Module); ok && m != nil && m.Name != "" {
			disp := prettySpec(m.Name)
			if disp == "" {
				disp = m.Name
			}
			name = "<module: " + disp + ">"
		}
		return name
	case VTHandle:
		if h, ok := v.Data.(*Handle); ok && h != nil && h.Kind != "" {
			return "<handle: " + h.Kind + ">"
		}
		return "<handle>"
	case VTNull:
		return "null"
	case VTBool:
		if b, _ := v.Data.(bool); b {
			return "true"
		}
		return "false"
	case VTInt:
		return strconv.FormatInt(v.Data.(int64), 10)
	case VTNum:
		s := strconv.FormatFloat(v.Data.(float64), 'g', -1, 64)
		if !strings.ContainsAny(s, ".eE") {
			s += ".0"
		}
		return s
	case VTStr:
		// Reuse the same quoting policy for visibility; the opaque string is expected raw.
		return quoteString(v.Data.(string))
	default:
		return "<unknown>"
	}
}

// Local helper to avoid depending on parser.go's joinNonEmpty.
// Concatenates non-empty strings with a single space.
func joinNonEmptyLocal(a, b string) string {
	a = strings.TrimSpace(a)
	b = strings.TrimSpace(b)
	switch {
	case a == "" && b == "":
		return ""
	case a == "":
		return b
	case b == "":
		return a
	default:
		return a + " " + b
	}
}
