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
//     **Important (POST semantics):**
//     • POST captures the rest of the current line and therefore *forces a newline*.
//     • In comma/colon separated contexts, POST attaches **after the separator**:
//     - For comma-separated lists (arrays, enums, destructuring arrays, value arrays):
//     the POST prints *after the comma that follows the element* (or after the
//     element itself if it is the last one).
//     Example:
//     [ 1, # post
//     2 ]
//     - For maps/object patterns/type maps: POST on a **key** prints after the colon
//     on the key line (`key: # post`), while POST on a **value** of a non-last
//     entry prints after the following comma (`key: value, # post`).
//     Printers account for this and avoid emitting an extra blank line before a
//     closing brace/bracket.
//     - Property names are emitted bare if they are identifier-like, otherwise
//     quoted. Destructuring declaration patterns ("decl" | "darr" | "dobj")
//     are rendered in a compact, readable form.
//     - Formatting emits no space before '(' for calls and for 'fun(...)'
//     and 'oracle(...)' parameter lists, matching the lexer’s CLROUND rule.
//     - Control keywords render without parens:
//     return expr
//     break expr
//     continue [expr]
//     A `null` payload prints as the bare keyword (e.g. `continue`).
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
//     PRE as header lines; POST as trailing inline comments that force a
//     newline (see POST semantics above). For **value POST** in non-last
//     entries, the POST prints *after the following comma*.
//     ("enum", literalS... )  → prints as `Enum[ ... ]`, where members
//     may be scalars, arrays, or maps.
//     ("binop","->", A, B)    → prints as `(A) -> B`, flattened across
//     right-associated chains.
//     - Output is stable. Multi-line maps are rendered with sorted keys to
//     avoid visual churn.
//     - When the last field ends with a POST, the closing `}` appears on the
//     next line without an extra blank line.
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
//     on the same line as the value **and forces a newline**.
//     • In comma/colon separated contexts, POST respects the same **after
//     separator** rule as the AST printers (value POST after comma).
//     When a POST ends the last element/entry line, the closing bracket/brace
//     is placed on the next line without an extra blank line.
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
//
// Canonicalizations & Omissions (parser ↔ printer contract)
// ---------------------------------------------------------
// These are deliberate simplifications made by the parser and normalized by
// the printer; users may not see certain syntactic sugar re-emitted:
//   - Param types default to `Any` and are not printed (e.g., `fun(x)` not `x: Any`).
//   - Function return type defaults to `Any` and is not printed (`fun(...) do ... end`
//     without `-> Any`).
//   - `oracle(...)` without `from` carries an empty default source; `from …` is omitted.
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
//     ("annot", ("str", textOr<text>), X)
//     PRE prints as `# ...` above; POST prints as trailing `# ...` on the line
//     and forces a newline.
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
//     POST consumes the rest of the line and forces a newline, and containers
//     avoid emitting an extra blank line before closing delimiters.
//   - Annotation nodes wrap the printed construct; POST becomes trailing inline.
//   - **POST-after-separator rule** is enforced (see file header).
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
// It respects PRE (header lines) vs POST (trailing inline that forces newline)
// in the same way as the AST/code printer, including the **POST-after-separator**
// behavior for value POSTs in maps.
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

// FormatValue renders a runtime Value into a stable, readable string.
//
// Layout policy:
//   - Scalars: null, booleans, ints, floats (with a decimal point for
//     non-scientific output), and quoted strings.
//   - Arrays: single-line `[ a, b, c ]` when the group fits and there are no
//     PRE annotations forcing multi-line; otherwise multi-line with indentation.
//     Value POST follows the **after-comma** rule.
//   - Maps: keys sorted for stability; single-line `{ k: v, ... }` when short,
//     with no PRE key/value annotations; else multi-line with indentation,
//     where PRE annotations print as header lines, value POST prints **after
//     the following comma**, and key POST prints after the colon. When the last
//     entry ends with a POST, the closing brace is on the next line without an
//     extra blank line.
//   - Functions: `<fun: name:T -> name2:U -> R>` (zero-arg uses `_:Null`).
//   - Types (VTType): pretty-printed via docType, wrapped as `<type: ...>`.
//   - Modules: `<module: pretty-name>` when available.
func FormatValue(v Value) string {
	doc := docValue(v)
	var b strings.Builder
	r := renderer{
		out:      &b,
		maxWidth: MaxInlineWidth,
		tabWidth: 4,
	}
	r.render(doc)
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

// kvEntry builds a single map entry, respecting PRE/POST placement and
// POST-newline semantics.
//   - keyPre prints above the key.
//   - If valPre is non-empty, the value is placed on its own indented line
//     under `key:` and valPre prints above the value.
//   - POSTs: key POST lives on the key line (after ':'), value POST lives on
//     the value line. Both force a newline.
//   - NOTE: Callers may choose to *not* pass a value POST here in order to
//     attach it **after the following comma** using the comma-aware joiner.
func kvEntry(keyDoc *Doc, valDoc *Doc, keyPre, valPre, keyPost, valPost string) *Doc {
	var parts []*Doc
	// key PRE above entry
	if keyPre != "" {
		parts = append(parts, annotPre(keyPre))
	}
	// key:
	parts = append(parts, keyDoc)

	// Cases:
	switch {
	case valPre != "":
		// Own line for value; key POST attaches to key line.
		if keyPost != "" {
			parts = append(parts, Text(":"), annotInline(keyPost))
		} else {
			parts = append(parts, Text(":"))
		}
		parts = append(parts, HardLineDoc(),
			Nest(1, Concat(annotPre(valPre), valDoc)))
		// Value POST on the value line.
		if valPost != "" {
			parts = append(parts, annotInline(valPost))
		}

	case keyPost != "":
		// No valPre; key has POST → put key POST on key line, then value below.
		parts = append(parts, Text(":"), annotInline(keyPost),
			Nest(1, Concat(valDoc)))
		if valPost != "" {
			parts = append(parts, annotInline(valPost))
		}

	default:
		// Same line `key: value`; only value POST trails and forces newline.
		parts = append(parts, Text(": "), valDoc)
		if valPost != "" {
			parts = append(parts, annotInline(valPost))
		}
	}

	return Concat(parts...)
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

func unwrapKeyAST(n S) (name string, annot string) {
	if tag(n) == "annot" {
		return n[2].(S)[1].(string), n[1].(S)[1].(string)
	}
	return n[1].(string), ""
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

// AST helper: does this node carry a top-level POST annotation?
func astHasPostInline(n S) bool {
	if tag(n) == "annot" {
		_, _, pre := asAnnotAST(n)
		return !pre
	}
	return false
}

/* ---------- AST → Doc ---------- */

func docProgram(n S) *Doc {
	if tag(n) != "block" {
		return docStmt(n)
	}
	kids := listS(n, 1)
	var ds []*Doc
	for i, k := range kids {
		ds = append(ds, docStmt(k))
		if i < len(kids)-1 {
			ds = append(ds, HardLineDoc())
		}
	}
	return Concat(ds...)
}

func docStmt(n S) *Doc {
	switch tag(n) {
	case "noop":
		return Concat()

	case "annot":
		text, wrapped, pre := asAnnotAST(n)
		if pre {
			return Concat(annotPre(text), docStmt(wrapped))
		}
		// POST: trailing inline — capture the remainder of the line and break.
		return Concat(docStmt(wrapped), annotInline(text))

	case "fun":
		params, ret, body := n[1].(S), n[2].(S), n[3].(S)
		header := Concat(Text("fun("), docParams(params), Text(")"))
		if !(tag(ret) == "id" && getId(ret) == "Any") {
			header = Concat(header, Text(" -> "), docExpr(ret))
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
			header = Concat(header, Text(" -> "), docExpr(outT))
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
		return Concat(Text("type "), docExpr(n[1].(S)))

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
		lhs, rhs := n[1].(S), n[2].(S)
		if isDeclPattern(lhs) {
			return Concat(Text("let "), docPattern(lhs), Text(" = "), docExpr(rhs))
		}
		return Concat(docExpr(lhs), Text(" = "), docExpr(rhs))

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
	for i, k := range kids {
		ds = append(ds, docStmt(k))
		if i < len(kids)-1 {
			ds = append(ds, HardLineDoc())
		}
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
			parts = append(parts, Concat(Text(name), Text(": "), docExpr(ty)))
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

// docExprAndTrailingPost renders n as an expression and returns any single
// outer POST annotation's text (without '<') so the caller can attach it
// after a following comma when needed.
func docExprAndTrailingPost(n S) (*Doc, string) {
	if tag(n) == "annot" {
		txt, wrapped, pre := asAnnotAST(n)
		if !pre {
			return docExpr(wrapped), txt
		}
	}
	return docExpr(n), ""
}

func docPatternAndTrailingPost(n S) (*Doc, string) {
	if tag(n) == "annot" {
		txt, wrapped, pre := asAnnotAST(n)
		if !pre {
			return docPattern(wrapped), txt
		}
	}
	return docPattern(n), ""
}

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
		my := 60
		if pr, ok := binPrec[op]; ok {
			my = pr.p
		}
		return Concat(docExprMin(l, my), Text(" "+op+" "), docExprMin(r, my))

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
			d, post := docExprAndTrailingPost(e)
			items = append(items, sepItem{main: d, post: post})
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
			keyNode := pr[1].(S)
			valNode := pr[2].(S)

			key, kAnn := unwrapKeyAST(keyNode)
			kPre, kPost := splitAnnotText(kAnn)

			vPre, vPost := "", ""
			if tag(valNode) == "annot" {
				txt, wrapped, pre := asAnnotAST(valNode)
				if pre {
					vPre = txt
				} else {
					vPost = txt // defer to after-comma placement
				}
				valNode = wrapped
			}
			entryDoc := kvEntry(idOrQuoted(key), docExpr(valNode), kPre, vPre, kPost /*valPost*/, "")
			joined = append(joined, sepItem{main: entryDoc, post: vPost})
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
			d, post := docExprAndTrailingPost(e)
			items = append(items, sepItem{main: d, post: post})
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
			d, post := docPatternAndTrailingPost(it)
			joined = append(joined, sepItem{main: d, post: post})
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
			key, ann := unwrapKeyAST(it[1].(S))
			kPre, kPost := splitAnnotText(ann)
			val := it[2].(S)
			vPre, vPost := "", ""
			if tag(val) == "annot" {
				txt, wrapped, pre := asAnnotAST(val)
				if pre {
					vPre = txt
				} else {
					vPost = txt // defer to comma
				}
				val = wrapped
			}
			entry := kvEntry(idOrQuoted(key), docPattern(val), kPre, vPre, kPost /*valPost*/, "")
			joined = append(joined, sepItem{main: entry, post: vPost})
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
		text, wrapped, pre := asAnnotAST(n)
		if pre {
			return Concat(annotPre(text), docPattern(wrapped))
		}
		return Concat(docPattern(wrapped), annotInline(text))
	default:
		return docExpr(n)
	}
}

/* ---------- AST "annot" helpers ---------- */

// New: decode 3-ary ("annot", ("str", textOr<text>), wrapped) to (text, wrapped, pre?)
func asAnnotAST(n S) (text string, wrapped S, pre bool) {
	raw := n[1].(S)[1].(string)
	preText, postText := splitAnnotText(raw)
	if preText != "" {
		return preText, n[2].(S), true
	}
	return postText, n[2].(S), false
}

/* ---------- Type pretty-printer (as Doc) ---------- */

func docType(t S) *Doc {
	if len(t) == 0 {
		return Text("<type>")
	}
	switch tag(t) {
	case "id":
		return Text(getStr(t))
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
			k, kAnn := unwrapKeyAST(raw[1].(S))
			kPre, kPost := splitAnnotText(kAnn)
			ft := raw[2].(S)
			vPre, vPost := "", ""
			if len(ft) > 0 && tag(ft) == "annot" {
				txt, inner, pre := asAnnotAST(ft)
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
		if len(fs) == 0 {
			return Text("{}")
		}
		joined := make([]sepItem, 0, len(fs))
		for _, f := range fs {
			key := idOrQuoted(f.name)
			if f.req {
				key = Concat(key, Text("!"))
			}
			entry := kvEntry(key, docType(f.typ), f.kAnnPre, f.vAnnPre, f.kAnnPost /*valPost*/, "")
			joined = append(joined, sepItem{main: entry, post: f.vAnnPost})
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
			params, ret := flattenArrow(t)
			var ps []*Doc
			for _, p := range params {
				ps = append(ps, docType(p))
			}
			return Concat(Text("("), Join(Text(", "), ps), Text(") -> "), docType(ret))
		}
		return Text("<binop>")
	case "annot":
		txt, wrapped, pre := asAnnotAST(t)
		if pre {
			return Concat(annotPre(txt), docType(wrapped))
		}
		return Concat(docType(wrapped), annotInline(txt))
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
			entry := kvEntry(idOrQuoted(k), val, "", "", "", "")
			joined = append(joined, sepItem{main: entry, post: ""})
		}
		inside := joinCommaWithPost(joined)
		return Group(braced("{", Nest(1, Concat(SoftLineDoc(), inside, SoftLineDoc())), "}"))
	default:
		return Text("<lit>")
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

/* ---------- Runtime value pretty-printer (as Doc) ---------- */

func numString(f float64) string {
	s := strconv.FormatFloat(f, 'g', -1, 64)
	if !strings.ContainsAny(s, ".eE") {
		s += ".0"
	}
	return s
}

func docValue(v Value) *Doc {
	pre, post := splitAnnotText(v.Annot)
	var head []*Doc
	if pre != "" {
		head = append(head, annotPre(pre))
	}
	body := docValueNoAnn(v)
	if post != "" {
		body = Concat(body, annotInline(post))
	}
	return Concat(append(head, body)...)
}

func docValueNoAnn(v Value) *Doc {
	switch v.Tag {
	case VTNull:
		return Text("null")
	case VTBool:
		if v.Data.(bool) {
			return Text("true")
		}
		return Text("false")
	case VTInt:
		return Text(strconv.FormatInt(v.Data.(int64), 10))
	case VTNum:
		return Text(numString(v.Data.(float64)))
	case VTStr:
		return Text(quoteString(v.Data.(string)))
	case VTArray:
		xs := v.Data.([]Value)
		if len(xs) == 0 {
			return Text("[]")
		}
		joined := make([]sepItem, 0, len(xs))
		for _, it := range xs {
			pre, post := splitAnnotText(it.Annot)
			elemDoc := docValueNoAnn(Value{Tag: it.Tag, Data: it.Data}) // without own annots
			if pre != "" {
				elemDoc = Concat(annotPre(pre), elemDoc)
			}
			joined = append(joined, sepItem{main: elemDoc, post: post})
		}
		inside := joinCommaWithPost(joined)
		lastEnds := joined[len(joined)-1].post != ""
		return Group(braced("[", Nest(1, Concat(SoftLineDoc(), inside, func() *Doc {
			if lastEnds {
				return Concat()
			}
			return SoftLineDoc()
		}())), "]"))
	case VTMap:
		mo := v.Data.(*MapObject)
		keys := append([]string(nil), mo.Keys...)
		sort.Strings(keys)
		if len(keys) == 0 {
			return Text("{}")
		}
		joined := make([]sepItem, 0, len(keys))
		for _, k := range keys {
			kAnn := mo.KeyAnn[k]
			kPre, kPost := splitAnnotText(kAnn)
			vv := mo.Entries[k]
			vPre, vPost := splitAnnotText(vv.Annot)
			entry := kvEntry(idOrQuoted(k), docValueNoAnn(Value{Tag: vv.Tag, Data: vv.Data}), kPre, vPre, kPost /*valPost*/, "")
			joined = append(joined, sepItem{main: entry, post: vPost})
		}
		inside := joinCommaWithPost(joined)
		lastEnds := joined[len(joined)-1].post != ""
		return Group(braced("{", Nest(1, Concat(SoftLineDoc(), inside, func() *Doc {
			if lastEnds {
				return Concat()
			}
			return SoftLineDoc()
		}())), "}"))
	case VTFun:
		if f, ok := v.Data.(*Fun); ok && f != nil {
			// Use "oracle" label for oracle functions, otherwise "fun".
			label := "fun"
			if f.IsOracle {
				label = "oracle"
			}

			// Build `<{label}: a:T -> b:U -> R>` (zero-arg becomes `_:Null`).
			var chain []*Doc
			if len(f.ParamTypes) == 0 {
				chain = append(chain, Text("_:Null"))
			} else {
				for i := range f.ParamTypes {
					if i > 0 {
						chain = append(chain, Text(" -> "))
					}
					name := "_"
					if i < len(f.Params) && f.Params[i] != "" {
						name = f.Params[i]
					}
					chain = append(chain, Text(name), Text(":"), docType(f.ParamTypes[i]))
				}
			}
			chain = append(chain, Text(" -> "), docType(f.ReturnType))
			return Group(Concat(Text("<"+label+": "), Concat(chain...), Text(">")))
		}
		return Text("<fun>")
	case VTType:
		t := typeAst(v.Data)
		return Group(Concat(Text("<type: "), docType(t), Text(">")))
	case VTModule:
		name := "<module>"
		if m, ok := v.Data.(*Module); ok && m != nil && m.Name != "" {
			disp := prettySpec(m.Name)
			if disp == "" {
				disp = m.Name
			}
			name = "<module: " + disp + ">"
		}
		return Text(name)
	default:
		if v.Tag == VTHandle {
			if h, ok := v.Data.(*Handle); ok {
				return Text("<handle: " + h.Kind + ">")
			}
			return Text("<handle>")
		}
		return Text("<unknown>")
	}
}
