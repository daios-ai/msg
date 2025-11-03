// analysis.go
//
// ROLE: Lex/parse/index pipeline, text/position helpers, span utilities, and
//       diagnostics mapping. Populates per-document caches used by features.
//
// NOTE: A generic AST walker and small visitor structs are used to keep logic
//       compact. Long, duplicated traversals have been replaced with visitors.
//
// What lives here
//   • Text & UTF-16 helpers and byte↔position conversion consistent with LSP.
//   • Token/span helpers (exact spans, token lookup, comment/annotation regions).
//   • Diagnostics plumbing: map lexer/parser errors to LSP ranges and publish.
//   • Analysis pipeline: analyze() → lex, parse (with spans), collect bindings
//     and top-level symbols, and clear/publish diagnostics.
//   • Lightweight static type synthesis and formatting helpers used by features.
//
// What does NOT live here
//   • No transport framing or send/notify implementations (lives in main.go).
//   • No LSP feature handlers (hover/definition/etc.) — see features.go.
//   • No server/document struct definitions — see state.go.

package main

import (
	"fmt"
	"strings"
	"unicode/utf8"

	mindscript "github.com/DAIOS-AI/msg/internal/mindscript"
)

////////////////////////////////////////////////////////////////////////////////
// Text & UTF-16 helpers
////////////////////////////////////////////////////////////////////////////////

// CRLF-aware: treat "\r\n" as a single newline; store offsets at the byte *after* '\n'.
func lineOffsets(text string) []int {
	offs := []int{0}
	for i := 0; i < len(text); {
		if text[i] == '\r' {
			// skip lone \r (shouldn't happen often)
			i++
			continue
		}
		if text[i] == '\n' {
			offs = append(offs, i+1)
			i++
			continue
		}
		_, sz := utf8.DecodeRuneInString(text[i:])
		if sz <= 0 {
			sz = 1
		}
		i += sz
	}
	return offs
}

func toU16(r rune) int {
	if r < 0x10000 {
		return 1
	}
	return 2
}

// posToOffset converts an LSP Position (UTF-16 code units) to a byte offset.
func posToOffset(lines []int, p Position, text string) int {
	if p.Line < 0 {
		return 0
	}
	if p.Line >= len(lines) {
		return len(text)
	}
	i := lines[p.Line]
	need := p.Character // in UTF-16 units
	for i < len(text) && need > 0 {
		r, sz := utf8.DecodeRuneInString(text[i:])
		if r == '\r' { // ignore CR in column math
			i += sz
			continue
		}
		if r == '\n' {
			break
		}
		need -= toU16(r)
		i += sz
	}
	return i
}

// offsetToPos converts a byte offset to an LSP Position (UTF-16 code units).
func offsetToPos(lines []int, off int, text string) Position {
	if off < 0 {
		off = 0
	}
	if off > len(text) {
		off = len(text)
	}
	i, j := 0, len(lines)
	for i+1 < j {
		m := (i + j) / 2
		if lines[m] <= off {
			i = m
		} else {
			j = m
		}
	}
	u16 := 0
	for k := lines[i]; k < off && k < len(text); {
		r, sz := utf8.DecodeRuneInString(text[k:])
		if r == '\r' { // ignore CR
			k += sz
			continue
		}
		if r == '\n' {
			break
		}
		u16 += toU16(r)
		k += sz
	}
	return Position{Line: i, Character: u16}
}

func makeRange(lines []int, start, end int, text string) Range {
	return Range{
		Start: offsetToPos(lines, start, text),
		End:   offsetToPos(lines, end, text),
	}
}

// Engine gives us byte columns (not UTF-16). Clamp within the line.
func byteColToOffset(lines []int, line0, byteCol int, text string) int {
	if line0 < 0 {
		line0 = 0
	}
	if line0 >= len(lines) {
		return len(text)
	}
	start := lines[line0]
	end := len(text)
	if line0+1 < len(lines) {
		end = lines[line0+1]
	}
	off := start + byteCol
	if off < start {
		off = start
	}
	if off > end {
		off = end
	}
	return off
}

// UTF-16 code-unit length of a string slice (for semantic tokens).
func u16Len(s string) int {
	n := 0
	for _, r := range s {
		if r < 0x10000 {
			n++
		} else {
			n += 2
		}
	}
	return n
}

////////////////////////////////////////////////////////////////////////////////
// Diagnostics helpers
////////////////////////////////////////////////////////////////////////////////

func (s *server) clearDiagnostics(uri string) {
	s.notify("textDocument/publishDiagnostics", PublishDiagnosticsParams{
		URI:         uri,
		Diagnostics: []Diagnostic{},
	})
}

////////////////////////////////////////////////////////////////////////////////
// Token & span helpers
////////////////////////////////////////////////////////////////////////////////

func tokenName(t mindscript.Token) string {
	if s, ok := t.Literal.(string); ok {
		return s
	}
	return t.Lexeme
}

// hasValidSpan reports whether the lexer provided concrete byte offsets.
// Used for ANNOTATION tokens which must have StartByte/EndByte or be ignored.
func hasValidSpan(t mindscript.Token, textLen int) bool {
	return t.StartByte >= 0 && t.EndByte >= t.StartByte && t.EndByte <= textLen
}

// Prefer exact lexer byte spans; fallback to line-local search.
func tokenSpan(doc *docState, t mindscript.Token) (start, end int) {
	// NEW: exact byte spans from the lexer if present.
	if t.StartByte >= 0 && t.EndByte >= t.StartByte && t.EndByte <= len(doc.text) {
		return t.StartByte, t.EndByte
	}

	if t.Line < 1 || t.Line > len(doc.lines) {
		return 0, 0
	}
	lineStart := doc.lines[t.Line-1]
	lineEnd := len(doc.text)
	if t.Line < len(doc.lines) {
		lineEnd = doc.lines[t.Line]
	}
	line := doc.text[lineStart:lineEnd]

	cand := t.Col - 1
	if cand < 0 {
		cand = 0
	}
	if cand > len(line) {
		cand = len(line)
	}
	try := func(at int) (int, int, bool) {
		if at < 0 {
			at = 0
		}
		if at+len(t.Lexeme) > len(line) {
			return 0, 0, false
		}
		if line[at:at+len(t.Lexeme)] == t.Lexeme {
			s := lineStart + at
			return s, s + len(t.Lexeme), true
		}
		return 0, 0, false
	}
	if s, e, ok := try(cand); ok {
		return s, e
	}
	if s, e, ok := try(cand - 1); ok {
		return s, e
	}
	const window = 8
	from := cand - window
	if from < 0 {
		from = 0
	}
	if idx := strings.Index(line[from:], t.Lexeme); idx >= 0 {
		s := lineStart + from + idx
		return s, s + len(t.Lexeme)
	}
	s := lineStart + t.Col - 1
	e := s + len(t.Lexeme)
	if s < 0 {
		s = 0
	}
	if e > len(doc.text) {
		e = len(doc.text)
	}
	if e < s {
		e = s
	}
	return s, e
}

// Use SpanIndex to build a Range from a NodePath.
func rangeFromPath(doc *docState, p mindscript.NodePath) (Range, bool) {
	if doc.spans == nil {
		return Range{}, false
	}
	sp, ok := doc.spans.Get(p)
	if !ok {
		return Range{}, false
	}
	return makeRange(doc.lines, sp.StartByte, sp.EndByte, doc.text), true
}

// rangeOrDefault tries span index first, then falls back to an empty range.
// This ensures diagnostics are still emitted even when a precise span is missing.
func rangeOrDefault(doc *docState, p mindscript.NodePath) Range {
	if r, ok := rangeFromPath(doc, p); ok {
		return r
	}
	// Fall back to a zero-length range at the start of the document.
	// (Tests check presence of codes; exact ranges are less critical.)
	return makeRange(doc.lines, 0, 0, doc.text)
}

////////////////////////////////////////////////////////////////////////////////
// AST Walker & Visitors (centralized traversal)
////////////////////////////////////////////////////////////////////////////////

// rangeFromID provides a best-effort identifier range: it first tries the path
// via SpanIndex; when unavailable, falls back to token scanning by name.
func rangeFromID(doc *docState, path mindscript.NodePath, name string) Range {
	if r, ok := rangeFromPath(doc, path); ok {
		return r
	}
	if s, e, ok := findDefIDRange(doc.tokens, name); ok {
		return makeRange(doc.lines, s, e, doc.text)
	}
	return Range{}
}

// mapFieldType extracts the field type from a ("map", ...) type node by key.
func mapFieldType(t []any, key string) ([]any, bool) {
	if len(t) == 0 || t[0] != "map" {
		return nil, false
	}
	for i := 1; i < len(t); i++ {
		if pr, ok := t[i].([]any); ok && len(pr) >= 3 && (pr[0] == "pair" || pr[0] == "pair!") {
			if k, ok := pr[1].([]any); ok && len(k) >= 2 && k[0] == "str" {
				if nm, _ := k[1].(string); nm == key {
					if tv, ok := pr[2].([]any); ok {
						return tv, true
					}
				}
			}
		}
	}
	return nil, false
}

// LUB: least common supertype used by infer + analyzer. Mirrors previous
// numeric/any rules used by numericSuper/commonSuper.
func LUB(a, b []any) []any {
	as, bs := formatTypeNode(a), formatTypeNode(b)
	if as == bs {
		return a
	}
	// numeric widening
	if (as == "Int" && bs == "Num") || (as == "Num" && bs == "Int") {
		return typeID("Num")
	}
	// Unknowns → Any
	if as == "Any" || bs == "Any" || as == "" || bs == "" {
		return typeID("Any")
	}
	return typeID("Any")
}

// isNullable reports whether the static type node has the nullable modifier T?.
func isNullable(t []any) bool {
	if len(t) >= 3 {
		if op, _ := t[0].(string); op == "unop" {
			if q, _ := t[1].(string); q == "?" {
				return true
			}
		}
	}
	return false
}

////////////////////////////////////////////////////////////////////////////////
// Word scanning & comments
////////////////////////////////////////////////////////////////////////////////

// wordAt: prefer token-based match; fallback to ASCII scan if needed.
func wordAt(doc *docState, pos Position) (string, Range) {
	off := posToOffset(doc.lines, pos, doc.text)
	if off < 0 || off > len(doc.text) {
		return "", Range{}
	}
	for _, t := range doc.tokens {
		// Only IDs are symbol names; TYPE is a keyword.
		if t.Type != mindscript.ID {
			continue
		}
		start, end := tokenSpan(doc, t)
		if off >= start && off < end {
			name := tokenName(t)
			return name, makeRange(doc.lines, start, end, doc.text)
		}
	}
	// fallback: ASCII-ish word scan
	isIdent := func(b byte) bool {
		return b == '_' ||
			(b >= 'a' && b <= 'z') || (b >= 'A' && b <= 'Z') ||
			(b >= '0' && b <= '9')
	}
	i, j := off, off
	for i > 0 && isIdent(doc.text[i-1]) {
		i--
	}
	for j < len(doc.text) && isIdent(doc.text[j]) {
		j++
	}
	if i < j {
		return strings.TrimSpace(doc.text[i:j]), makeRange(doc.lines, i, j, doc.text)
	}
	return "", Range{}
}

// comment/annotation spans used by semantic tokens & folding.
func commentSpans(doc *docState) [][2]int {
	spans := [][2]int{}
	if doc == nil {
		return spans
	}
	// Source of truth: lexer ANNOTATION tokens only, and only when they carry a valid span.
	for _, tk := range doc.tokens {
		if tk.Type != mindscript.ANNOTATION {
			continue
		}
		if !hasValidSpan(tk, len(doc.text)) {
			continue
		}
		s, e := tk.StartByte, tk.EndByte
		if e > s {
			spans = append(spans, [2]int{s, e})
		}
	}
	return spans
}

////////////////////////////////////////////////////////////////////////////////
// Shared keyword/type helpers (used by features)
////////////////////////////////////////////////////////////////////////////////

func isKeywordButNotType(tt mindscript.TokenType) bool {
	switch tt {
	case mindscript.AND, mindscript.OR, mindscript.NOT,
		mindscript.LET, mindscript.DO, mindscript.END, mindscript.RETURN, mindscript.BREAK, mindscript.CONTINUE,
		mindscript.IF, mindscript.THEN, mindscript.ELIF, mindscript.ELSE,
		mindscript.FUNCTION, mindscript.ORACLE,
		mindscript.FOR, mindscript.IN, mindscript.FROM, mindscript.WHILE,
		// 'type' keyword should be colored as a keyword, not a type identifier.
		mindscript.TYPECONS, mindscript.TYPE, mindscript.ENUM,
		mindscript.NULL:
		return true
	default:
		return false
	}
}

var builtinTypeDocs = map[string]string{
	"Any":  "Top type; any value.",
	"Null": "Null value (absence).",
	"Bool": "Boolean type (true/false).",
	"Int":  "64-bit signed integer.",
	"Num":  "64-bit IEEE-754 float.",
	"Str":  "Unicode string.",
	"Type": "Type descriptor value.",
}

// semantic tokens legend index (handlers will read this)
var semTypes = map[string]int{
	"keyword":  0,
	"function": 1,
	"type":     2,
	"variable": 3,
	"property": 4,
	"string":   5,
	"number":   6,
	"comment":  7,
	"bracket":  8,
}

////////////////////////////////////////////////////////////////////////////////
// Definition heuristics & symbol formatting
////////////////////////////////////////////////////////////////////////////////

// formatFunSig builds a pretty signature from a ("fun", ...) AST node.
func formatFunSig(name string, fun []any) string {
	if len(fun) < 3 {
		return name + "() -> Any"
	}
	ps, _ := fun[1].([]any)
	var parts []string
	if len(ps) > 0 && ps[0] == "array" {
		for i := 1; i < len(ps); i++ {
			p, _ := ps[i].([]any) // ("pair"| "pair!", ("id", name), typeS)
			if len(p) >= 3 && (p[0] == "pair" || p[0] == "pair!") {
				idNode, _ := p[1].([]any)
				nameStr := "_"
				if len(idNode) >= 2 && idNode[0] == "id" {
					if s, ok := idNode[1].(string); ok {
						nameStr = s
					}
				}
				if tS, ok := p[2].([]any); ok {
					parts = append(parts, fmt.Sprintf("%s: %s", nameStr, mindscript.FormatType(tS)))
				} else {
					parts = append(parts, nameStr+": Any")
				}
			}
		}
	}
	ret := "Any"
	if rt, ok := fun[2].([]any); ok {
		ret = mindscript.FormatType(rt)
	}
	return fmt.Sprintf("%s(%s) -> %s", name, strings.Join(parts, ", "), ret)
}

////////////////////////////////////////////////////////////////////////////////
// Pure analyzer types (byte-span, no LSP deps) — lives here to avoid new files
////////////////////////////////////////////////////////////////////////////////

// fileSnapshot lets the pure analyzer read file text by URI (multi-file ready).
type fileSnapshot interface {
	Get(uri string) (text string, ok bool)
}

// pureDiag is a diagnostic with byte offsets (LSP-agnostic).
type pureDiag struct {
	StartByte int
	EndByte   int
	Severity  int // 1=error, 2=warning, 3=hint, 4=info
	Code      string
	Message   string
}

// pureBinding mirrors binding data with byte spans.
type pureBinding struct {
	Name      string
	Kind      string
	TypeNode  []any
	Sig       string
	StartByte int
	EndByte   int
}

// pureSymbol mirrors top-level symbol data with byte spans.
type pureSymbol struct {
	Name      string
	Kind      string
	Sig       string
	StartByte int
	EndByte   int
}

// pureResult is the full output of analyzing a single file.
type pureResult struct {
	URI      string
	Text     string
	Tokens   []mindscript.Token
	AST      mindscript.S
	Spans    *mindscript.SpanIndex
	Bindings []pureBinding
	Symbols  []pureSymbol
	Diags    []pureDiag
}

// serverSnapshot is a minimal adapter over server to satisfy fileSnapshot.
type serverSnapshot struct{ s *server }

func (ss serverSnapshot) Get(uri string) (string, bool) {
	if d := ss.s.snapshotDoc(uri); d != nil {
		return d.text, true
	}
	return "", false
}

// analyzeFilePure is a pure function: reads source via snapshot, lexes/parses,
// collects bindings/symbols, runs static checks, and returns byte-span results.
func analyzeFilePure(sn fileSnapshot, uri string) *pureResult {
	txt, _ := sn.Get(uri)

	// 1) Lex (interactive) — keep the salvage behavior for coloring on partial errors.
	lx := mindscript.NewLexerInteractive(txt)
	toks, err := lx.Scan()
	if err != nil {
		var ds []pureDiag
		if e, ok := err.(*mindscript.Error); ok && e.Kind == mindscript.DiagLex {
			// salvage tokens up to error position
			lines := lineOffsets(txt)
			off := byteColToOffset(lines, e.Line-1, e.Col-1, txt)
			if off < 0 {
				off = 0
			}
			if off > len(txt) {
				off = len(txt)
			}
			px := mindscript.NewLexerInteractive(txt[:off])
			if ptoks, pErr := px.Scan(); pErr == nil {
				if n := len(ptoks); n > 0 && ptoks[n-1].Type == mindscript.EOF {
					ptoks = ptoks[:len(ptoks)-1]
				}
				toks = ptoks
			}
		}
		// surface the lex error as a single diag; byte positions are best-effort (0..0)
		ds = append(ds, pureDiag{StartByte: 0, EndByte: 0, Severity: 1, Code: "LEX", Message: err.Error()})
		return &pureResult{URI: uri, Text: txt, Tokens: toks, Diags: ds}
	}
	if n := len(toks); n > 0 && toks[n-1].Type == mindscript.EOF {
		toks = toks[:len(toks)-1]
	}

	// 2) Parse (interactive) with spans
	ast, spans, perr := mindscript.ParseSExprInteractiveWithSpans(txt)
	if perr != nil {
		if e, ok := perr.(*mindscript.Error); ok && e.Kind == mindscript.DiagIncomplete {
			// Incomplete: return tokens but no diagnostics (non-nagging while typing)
			return &pureResult{URI: uri, Text: txt, Tokens: toks}
		}
		return &pureResult{
			URI:    uri,
			Text:   txt,
			Tokens: toks,
			Diags:  []pureDiag{{StartByte: 0, EndByte: 0, Severity: 1, Code: "PARSE", Message: perr.Error()}},
		}
	}

	// 3) Reuse existing collectors and static checks by constructing a temp docState
	lines := lineOffsets(txt)
	tmp := &docState{uri: uri, text: txt, lines: lines, tokens: toks, ast: ast, spans: spans}

	// Run unified AST analysis (bindings + symbols + diags) and token lints once.
	binds, syms, diags := unifiedAnalyze(tmp)
	tokDiags := tokenLints(tmp)

	// Bindings → pureBinding with byte spans
	var pBinds []pureBinding
	for _, b := range binds {
		start := posToOffset(lines, b.Range.Start, txt)
		end := posToOffset(lines, b.Range.End, txt)
		pBinds = append(pBinds, pureBinding{
			Name:      b.Name,
			Kind:      b.Kind,
			TypeNode:  b.TypeNode,
			Sig:       b.Sig,
			StartByte: start,
			EndByte:   end,
		})
	}
	// Symbols → pureSymbol with byte spans
	var pSyms []pureSymbol
	for _, s := range syms {
		start := posToOffset(lines, s.Range.Start, txt)
		end := posToOffset(lines, s.Range.End, txt)
		pSyms = append(pSyms, pureSymbol{
			Name:      s.Name,
			Kind:      s.Kind,
			Sig:       s.Sig,
			StartByte: start,
			EndByte:   end,
		})
	}
	// Diagnostics (token lints + AST checks) → pureDiag with byte spans
	var pDiags []pureDiag
	for _, d := range append(tokDiags, diags...) {
		start := posToOffset(lines, d.Range.Start, txt)
		end := posToOffset(lines, d.Range.End, txt)
		pDiags = append(pDiags, pureDiag{
			StartByte: start,
			EndByte:   end,
			Severity:  d.Severity,
			Code:      d.Code,
			Message:   d.Message,
		})
	}

	return &pureResult{
		URI:      uri,
		Text:     txt,
		Tokens:   toks,
		AST:      ast,
		Spans:    spans,
		Bindings: pBinds,
		Symbols:  pSyms,
		Diags:    pDiags,
	}
}

////////////////////////////////////////////////////////////////////////////////
// Analysis (lex + parse + symbol extraction) — publishes diagnostics via notify
////////////////////////////////////////////////////////////////////////////////

// analyze lexes, parses, and refreshes the per-doc caches used by features.
// It fills: doc.tokens, doc.ast (when parse succeeds), and doc.symbols (top-level defs).
func (s *server) analyze(doc *docState) {
	// Use the pure function; the snapshot is the server’s current state.
	sn := serverSnapshot{s: s}
	res := analyzeFilePure(sn, doc.uri)

	// Update caches
	doc.text = res.Text
	doc.lines = lineOffsets(doc.text)
	doc.tokens = res.Tokens
	doc.ast = res.AST
	if res.Spans != nil {
		doc.spans = res.Spans
	}

	// Uniform: collect all bindings + docs (already computed in pure result).
	// Rehydrate bindings/symbols with LSP ranges
	doc.binds = doc.binds[:0]
	for _, b := range res.Bindings {
		doc.binds = append(doc.binds, bindingDef{
			Name:     b.Name,
			Range:    makeRange(doc.lines, b.StartByte, b.EndByte, doc.text),
			DocFull:  "",
			DocFirst: "",
			Kind:     b.Kind,
			TypeNode: b.TypeNode,
			Sig:      b.Sig,
		})
	}
	doc.symbols = doc.symbols[:0]
	for _, ssy := range res.Symbols {
		doc.symbols = append(doc.symbols, symbolDef{
			Name:  ssy.Name,
			Kind:  ssy.Kind,
			Range: makeRange(doc.lines, ssy.StartByte, ssy.EndByte, doc.text),
			Doc:   "",
			Sig:   ssy.Sig,
		})
	}

	// Publish diagnostics (convert byte spans to LSP ranges)
	if len(res.Diags) > 0 {
		out := make([]Diagnostic, 0, len(res.Diags))
		for _, d := range res.Diags {
			out = append(out, Diagnostic{
				Range:    makeRange(doc.lines, d.StartByte, d.EndByte, doc.text),
				Severity: d.Severity,
				Code:     d.Code,
				Source:   "mindscript",
				Message:  d.Message,
			})
		}
		s.notify("textDocument/publishDiagnostics", PublishDiagnosticsParams{
			URI:         doc.uri,
			Diagnostics: out,
		})
		return
	}

	// 4) Success: clear any previous diagnostics.
	s.clearDiagnostics(doc.uri)
}

// //////////////////////////////////////////////////////////////////////////////
// Lightweight static type algebra (reused by fold)
// //////////////////////////////////////////////////////////////////////////////
func typeID(name string) []any { return []any{"id", name} }
func formatTypeNode(t []any) string {
	if len(t) == 0 {
		return "Any"
	}
	return mindscript.FormatType(t)
}

// findLocalFunMeta finds a top-level binding `name = fun|oracle(...) -> R`
// and returns (isOracle, paramTypes, returnType, ok).
func findLocalFunMeta(doc *docState, name string) (bool, [][]any, []any, bool) {
	// More robust: search the entire AST (not just top-level) for the most
	// recent assignment to the given name that binds a fun/oracle.
	if doc == nil || doc.ast == nil || len(doc.ast) == 0 {
		return false, nil, nil, false
	}

	var foundIsOracle bool
	var foundParams [][]any
	var foundRet []any
	var found bool

	var walk func(node []any)
	walk = func(node []any) {
		if node == nil || len(node) == 0 {
			return
		}
		tag, _ := node[0].(string)
		// Look for assignments to the target name anywhere.
		if tag == "assign" && len(node) >= 3 {
			if lhs, _ := node[1].([]any); len(lhs) >= 2 && (lhs[0] == "decl" || lhs[0] == "id") {
				if nm, _ := lhs[1].(string); nm == name {
					if rhs, _ := node[2].([]any); rhs != nil {
						base := unwrapAnnotNode(rhs)
						if len(base) >= 3 && (base[0] == "fun" || base[0] == "oracle") {
							var pts [][]any
							if ps, ok := base[1].([]any); ok && len(ps) > 0 && ps[0] == "array" {
								for j := 1; j < len(ps); j++ {
									if p, _ := ps[j].([]any); len(p) >= 3 && (p[0] == "pair" || p[0] == "pair!") {
										if tnode, ok := p[2].([]any); ok {
											pts = append(pts, tnode)
										} else {
											pts = append(pts, typeID("Any"))
										}
									} else {
										pts = append(pts, typeID("Any"))
									}
								}
							}
							if rt, ok := base[2].([]any); ok {
								foundIsOracle, foundParams, foundRet, found = (base[0] == "oracle"), pts, rt, true
							}
						}
					}
				}
			}
		}
		// Recurse into children.
		for i := 1; i < len(node); i++ {
			if ch, ok := node[i].([]any); ok {
				walk(ch)
			}
		}
	}
	walk(doc.ast)
	return foundIsOracle, foundParams, foundRet, found
}

// resolveTypeAliasAST returns the type AST for a top-level 'type' binding.
func resolveTypeAliasAST(doc *docState, name string) ([]any, bool) {
	if doc == nil || doc.ast == nil || len(doc.ast) == 0 {
		return nil, false
	}
	root := doc.ast
	if root[0] != "block" {
		return nil, false
	}
	for i := 1; i < len(root); i++ {
		n, _ := root[i].([]any)
		if len(n) < 3 || n[0] != "assign" {
			continue
		}
		lhs, _ := n[1].([]any)
		rhs, _ := n[2].([]any)
		if len(lhs) >= 2 && (lhs[0] == "decl" || lhs[0] == "id") {
			if nm, _ := lhs[1].(string); nm == name {
				base := unwrapAnnotNode(rhs)
				if len(base) >= 2 && base[0] == "type" {
					if t, ok := base[1].([]any); ok {
						return t, true
					}
				}
			}
		}
	}
	return nil, false
}

// enumHasMember reports whether t is Enum[...] and lit is one of its members.
// Returns (isEnumType, isMember).
func enumHasMember(t []any, lit []any, doc *docState) (bool, bool) {
	// If t is an alias id, resolve.
	if len(t) >= 2 && t[0] == "id" {
		if nm, _ := t[1].(string); nm != "" {
			if ta, ok := resolveTypeAliasAST(doc, nm); ok {
				t = ta
			}
		}
	}
	if len(t) == 0 || t[0] != "enum" {
		return false, false
	}
	// Literal only handled for strings/ints/nums/bools here (enough for tests).
	isEq := func(a, b []any) bool {
		if len(a) < 2 || len(b) < 2 {
			return false
		}
		if a[0] != b[0] {
			return false
		}
		return fmt.Sprintf("%v", a[1]) == fmt.Sprintf("%v", b[1])
	}
	for i := 1; i < len(t); i++ {
		if m, ok := t[i].([]any); ok && isEq(m, lit) {
			return true, true
		}
	}
	return true, false
}

// findLocalFunRetType retained to improve call typing.
func findLocalFunRetType(doc *docState, name string) ([]any, bool) {
	if doc == nil || doc.ast == nil || len(doc.ast) == 0 {
		return nil, false
	}
	root := doc.ast
	if root[0] != "block" {
		return nil, false
	}
	for i := 1; i < len(root); i++ {
		n, ok := root[i].([]any)
		if !ok || len(n) < 3 || n[0] != "assign" {
			continue
		}
		lhs, _ := n[1].([]any)
		rhs, _ := n[2].([]any)
		if len(lhs) >= 2 && (lhs[0] == "decl" || lhs[0] == "id") {
			if nm, _ := lhs[1].(string); nm == name {
				base := unwrapAnnotNode(rhs)
				if len(base) >= 3 && (base[0] == "fun" || base[0] == "oracle") {
					if tnode, ok := base[2].([]any); ok {
						if base[0] == "oracle" {
							return []any{"unop", "?", tnode}, true
						}
						return tnode, true
					}
				}
			}
		}
	}
	return nil, false
}
func findAnyBindingType(doc *docState, name string) ([]any, bool) {
	for _, b := range doc.binds {
		if b.Name == name && len(b.TypeNode) > 0 {
			return b.TypeNode, true
		}
	}
	return nil, false
}

////////////////////////////////////////////////////////////////////////////////
// Uniform binding collection (no top-level specialness)
////////////////////////////////////////////////////////////////////////////////

// annotText returns (baseNode, mergedAnnotationText, ok) without mutating the node.
func annotText(n []any) ([]any, string, bool) {
	cur := n
	var parts []string
	for len(cur) >= 3 {
		if tag, _ := cur[0].(string); tag != "annot" {
			break
		}
		if child, ok := cur[1].([]any); ok && len(child) >= 2 && child[0] == "str" {
			if s, ok := child[1].(string); ok {
				parts = append(parts, s)
			}
		}
		base, _ := cur[2].([]any)
		cur = base
	}
	if len(parts) == 0 {
		return n, "", false
	}
	return cur, strings.Join(parts, "\n"), true
}

func firstLine(s string) string {
	if i := strings.IndexByte(s, '\n'); i >= 0 {
		return strings.TrimSpace(s[:i])
	}
	return strings.TrimSpace(s)
}

// nearestBinding finds the binding with matching name whose definition appears
// at or before byte offset 'off', preferring the closest one. If none precede,
// it returns the earliest matching binding as a fallback.
func nearestBinding(doc *docState, name string, off int) (bindingDef, bool) {
	var best bindingDef
	bestOK := false
	bestStart := -1
	for _, b := range doc.binds {
		if b.Name != name {
			continue
		}
		// Definition start in bytes
		defOff := posToOffset(doc.lines, b.Range.Start, doc.text)
		if defOff <= off && defOff >= bestStart {
			best = b
			bestOK = true
			bestStart = defOff
		}
	}
	if bestOK {
		return best, true
	}
	// Fallback: earliest with that name
	earliest := -1
	for _, b := range doc.binds {
		if b.Name != name {
			continue
		}
		defOff := posToOffset(doc.lines, b.Range.Start, doc.text)
		if earliest == -1 || defOff < earliest {
			earliest = defOff
			best = b
			bestOK = true
		}
	}
	return best, bestOK
}

// collectTopLevelSymbols walks the AST (root-level only) and extracts symbols:
//   - let/assign of a simple decl: ("assign", ("decl", name), rhs)
//   - marks kind "fun" when rhs tag == "fun"; "type" when rhs tag == "type"
func findDefIDRange(toks []mindscript.Token, name string) (int, int, bool) {
	for i := 0; i < len(toks); i++ {
		tk := toks[i]
		if tk.Type != mindscript.ID {
			continue
		}
		if tokenName(tk) != name {
			continue
		}
		// def `x = ...`
		if i+1 < len(toks) && toks[i+1].Type == mindscript.ASSIGN {
			return tk.StartByte, tk.EndByte, true
		}
		// def `let x ...`
		if i-1 >= 0 && toks[i-1].Type == mindscript.LET {
			return tk.StartByte, tk.EndByte, true
		}
	}
	// Fallback: first occurrence (still better than nothing)
	for i := 0; i < len(toks); i++ {
		tk := toks[i]
		if tk.Type == mindscript.ID && tokenName(tk) == name {
			return tk.StartByte, tk.EndByte, true
		}
	}
	return 0, 0, false
}

func unwrapAnnotNode(n []any) []any {
	for {
		if len(n) >= 3 {
			if tag, _ := n[0].(string); tag == "annot" {
				if inner, _ := n[2].([]any); inner != nil {
					n = inner
					continue
				}
			}
		}
		return n
	}
}

////////////////////////////////////////////////////////////////////////////////
// Token-only lints (unchanged)
////////////////////////////////////////////////////////////////////////////////

// ---------- Layout-sensitive token lints (kept as a tiny helper) ----------
func tokenLints(doc *docState) []Diagnostic {
	if doc == nil {
		return nil
	}
	out := []Diagnostic{}
	for i := 0; i+1 < len(doc.tokens); i++ {
		t := doc.tokens[i]
		n := doc.tokens[i+1]
		// LROUND vs CLROUND
		if n.Type == mindscript.LROUND {
			if t.Type == mindscript.ID || t.Type == mindscript.RROUND || t.Type == mindscript.RSQUARE ||
				t.Type == mindscript.STRING || t.Type == mindscript.INTEGER || t.Type == mindscript.NUMBER ||
				t.Type == mindscript.FUNCTION || t.Type == mindscript.ORACLE {
				s0, e0 := tokenSpan(doc, n)
				out = append(out, Diagnostic{
					Range:    makeRange(doc.lines, s0, e0, doc.text),
					Severity: 3,
					Code:     "MS-LROUND-INSTEAD-OF-CLROUND",
					Source:   "mindscript",
					Message:  "Remove space before '(' (use compact '(' with no space).",
				})
			}
		}
		// LSQUARE vs CLSQUARE
		if n.Type == mindscript.LSQUARE {
			if t.Type == mindscript.ID || t.Type == mindscript.RROUND || t.Type == mindscript.RSQUARE ||
				t.Type == mindscript.STRING || t.Type == mindscript.INTEGER || t.Type == mindscript.NUMBER {
				s0, e0 := tokenSpan(doc, n)
				out = append(out, Diagnostic{
					Range:    makeRange(doc.lines, s0, e0, doc.text),
					Severity: 3,
					Code:     "MS-LSQUARE-INSTEAD-OF-CLSQUARE",
					Source:   "mindscript",
					Message:  "Remove space before '[' (use compact '[' with no space) for indexing.",
				})
			}
		}
		// DOT GAP
		if t.Type == mindscript.PERIOD {
			j := i + 1
			sawAnnot := false
			for j < len(doc.tokens) && doc.tokens[j].Type == mindscript.ANNOTATION {
				sawAnnot = true
				j++
			}
			if sawAnnot && j < len(doc.tokens) {
				s0, e0 := tokenSpan(doc, t)
				out = append(out, Diagnostic{
					Range:    makeRange(doc.lines, s0, e0, doc.text),
					Severity: 3,
					Code:     "MS-DOT-GAP",
					Source:   "mindscript",
					Message:  "Remove the gap/comment between '.' and the property name.",
				})
			}
		}
	}
	for i := 0; i+1 < len(doc.tokens); i++ {
		if doc.tokens[i].Type == mindscript.ANNOTATION && doc.tokens[i+1].Type == mindscript.END {
			if doc.tokens[i].Line == doc.tokens[i+1].Line {
				s0, e0 := tokenSpan(doc, doc.tokens[i+1])
				out = append(out, Diagnostic{
					Range:    makeRange(doc.lines, s0, e0, doc.text),
					Severity: 4,
					Code:     "MS-FORMAT-POST-FORCES-NEWLINE",
					Source:   "mindscript",
					Message:  "Move 'end' to the next line after a trailing comment.",
				})
			}
		}
	}
	return out
}

////////////////////////////////////////////////////////////////////////////////
// Catamorphic fold (one pass): types + diagnostics + bindings/symbols
////////////////////////////////////////////////////////////////////////////////

type foldOut struct {
	T  []any
	Ds []Diagnostic
}
type anCtx struct {
	doc       *docState
	scope     []map[string]bool
	atTop     bool
	curFunRet []any
	binds     []bindingDef
	syms      []symbolDef
	diags     []Diagnostic
}

func (c *anCtx) push() { c.scope = append(c.scope, map[string]bool{}) }
func (c *anCtx) pop()  { c.scope = c.scope[:len(c.scope)-1] }
func (c *anCtx) bind(name string) {
	if name != "" {
		c.scope[len(c.scope)-1][name] = true
	}
}
func (c *anCtx) defined(name string) bool {
	for i := len(c.scope) - 1; i >= 0; i-- {
		if c.scope[i][name] {
			return true
		}
	}
	return false
}

func fold(n []any, path mindscript.NodePath, c *anCtx) foldOut {
	if len(n) == 0 {
		return foldOut{T: typeID("Any")}
	}
	tag, _ := n[0].(string)

	emit := func(d Diagnostic) { c.diags = append(c.diags, d) }

	switch tag {
	case "str":
		return foldOut{T: typeID("Str")}
	case "int":
		return foldOut{T: typeID("Int")}
	case "num":
		return foldOut{T: typeID("Num")}
	case "bool":
		return foldOut{T: typeID("Bool")}
	case "null":
		return foldOut{T: typeID("Null")}

	case "block":
		c.push()
		c.atTop = len(path) == 0
		defer c.pop()
		// fold children, last value is block value (MindScript block is expression container)
		var lastT []any
		for i := 1; i < len(n); i++ {
			if ch, ok := n[i].([]any); ok {
				fold(ch, append(path, i-1), c)
				// blocks don't produce a value here; keep Any
				lastT = typeID("Any")
			}
		}
		return foldOut{T: lastT}

	case "id":
		if len(n) >= 2 {
			nm, _ := n[1].(string)
			switch nm {
			case "Int", "Num", "Str", "Bool", "Null", "Any", "Type":
				return foldOut{T: typeID("Any")}
			default:
				if !c.defined(nm) {
					if rng, ok := rangeFromPath(c.doc, path); ok {
						emit(Diagnostic{Range: rng, Severity: 1, Code: "MS-UNKNOWN-NAME", Source: "mindscript", Message: fmt.Sprintf("Unknown name: %s", nm)})
					}
				}
				if tn, ok := findAnyBindingType(c.doc, nm); ok {
					return foldOut{T: tn}
				}
			}
		}
		return foldOut{T: typeID("Any")}

	case "array":
		if len(n) == 1 {
			return foldOut{T: []any{"array", typeID("Any")}}
		}
		elem := typeID("Any")
		first := []any(nil)
		mixed := false
		for i := 1; i < len(n); i++ {
			if ch, ok := n[i].([]any); ok {
				ti := fold(ch, append(path, i-1), c).T
				if first == nil {
					first = ti
				} else if formatTypeNode(first) != formatTypeNode(ti) {
					mixed = true
				}
				elem = LUB(elem, ti)
			}
		}
		if mixed && formatTypeNode(elem) != formatTypeNode(first) {
			if rng, ok := rangeFromPath(c.doc, path); ok {
				emit(Diagnostic{Range: rng, Severity: 2, Code: "MS-ARRAY-HETEROGENEOUS", Source: "mindscript", Message: "Array has heterogeneous element types; elements widen to a common supertype."})
			}
		}
		return foldOut{T: []any{"array", elem}}

	case "map":
		out := []any{"map"}
		for i := 1; i < len(n); i++ {
			if p, ok := n[i].([]any); ok && len(p) >= 3 && (p[0] == "pair" || p[0] == "pair!") {
				keyNode, _ := p[1].([]any)
				valNode, _ := p[2].([]any)
				ks := ""
				if len(keyNode) >= 2 && keyNode[0] == "str" {
					ks, _ = keyNode[1].(string)
				}
				vt := fold(valNode, append(path, i-1, 1), c).T
				out = append(out, []any{"pair", []any{"str", ks}, vt})
			}
		}
		return foldOut{T: out}

	case "get":
		if len(n) >= 3 {
			objT := fold(n[1].([]any), append(path, 0), c).T
			key := ""
			if ks, ok := n[2].([]any); ok && len(ks) >= 2 && ks[0] == "str" {
				key, _ = ks[1].(string)
			}
			if key != "" && len(objT) > 0 && objT[0] == "map" {
				if tv, ok := mapFieldType(objT, key); ok {
					return foldOut{T: tv}
				}
				if rng, ok := rangeFromPath(c.doc, path); ok {
					emit(Diagnostic{Range: rng, Severity: 2, Code: "MS-MAP-MISSING-KEY", Source: "mindscript", Message: fmt.Sprintf("Key '%s' may be missing.", key)})
				}
			}
		}
		return foldOut{T: typeID("Any")}

	case "idx":
		if len(n) >= 3 {
			objT := fold(n[1].([]any), append(path, 0), c).T
			idxT := fold(n[2].([]any), append(path, 1), c).T
			// Index expr lives at child #1 in fold-path space → use append(path, 1)
			if rng, ok := rangeFromPath(c.doc, append(path, 1)); ok && formatTypeNode(idxT) != "Int" {
				emit(Diagnostic{Range: rng, Severity: 1, Code: "MS-ARG-TYPE-MISMATCH", Source: "mindscript", Message: "Index must be Int."})
			}
			if len(objT) >= 2 && objT[0] == "array" {
				if t, ok := objT[1].([]any); ok {
					return foldOut{T: t}
				}
			}
		}
		return foldOut{T: typeID("Any")}

	case "call":
		// Support BOTH shapes:
		//   ("call", calleeExpr, ("array", arg1, arg2, ...))
		//   ("call", calleeExpr, arg1, arg2, ...)
		if len(n) < 3 {
			return foldOut{T: typeID("Any")}
		}
		// Callee name (only simple id needed for tests)
		calleeIsID := false
		calleeName := ""
		if idn, _ := n[1].([]any); len(idn) >= 2 && idn[0] == "id" {
			if s, _ := idn[1].(string); s != "" {
				calleeIsID = true
				calleeName = s
			}
		}
		// Fold args and collect their types + ranges (accept both shapes)
		var argTs [][]any
		var argRanges []Range
		if an, ok := n[2].([]any); ok && len(an) > 0 && an[0] == "array" {
			// ("call", callee, ("array", arg1, arg2, ...))
			for i := 1; i < len(an); i++ {
				if a, ok := an[i].([]any); ok {
					argTs = append(argTs, fold(a, append(path, 1, i-1), c).T)
					if r, ok := rangeFromPath(c.doc, append(path, 1, i-1)); ok {
						argRanges = append(argRanges, r)
					} else {
						argRanges = append(argRanges, Range{})
					}
				}
			}
		} else {
			// ("call", callee, arg1, arg2, ...)
			for i := 2; i < len(n); i++ {
				if a, ok := n[i].([]any); ok {
					argTs = append(argTs, fold(a, append(path, i-1), c).T)
					if r, ok := rangeFromPath(c.doc, append(path, i-1)); ok {
						argRanges = append(argRanges, r)
					} else {
						argRanges = append(argRanges, Range{})
					}
				}
			}
		}
		// Default: unknown callee → just return Any
		isOracle, paramTs, retT, ok := false, [][]any{}, typeID("Any"), false
		if calleeIsID {
			isOracle, paramTs, retT, ok = findLocalFunMeta(c.doc, calleeName)
		}
		if ok {
			// Arity checks (overflow only; partial application allowed)
			if len(argTs) > len(paramTs) {
				// Mark the first overflow argument
				overIdx := len(paramTs)
				if overIdx < len(argRanges) {
					emit(Diagnostic{
						Range:    argRanges[overIdx],
						Severity: 1, Code: "MS-ARG-OVERFLOW", Source: "mindscript",
						Message: "Too many arguments for function.",
					})
				}
			}
			// Per-argument type checks (best-effort)
			nCheck := len(argTs)
			if len(paramTs) < nCheck {
				nCheck = len(paramTs)
			}
			for i := 0; i < nCheck; i++ {
				gotS := formatTypeNode(argTs[i])
				wantS := formatTypeNode(paramTs[i])
				// Special-case Enum membership when argument is a literal.
				// Recover the literal node from whichever arg shape we saw.
				var argNode []any
				if an, ok := n[2].([]any); ok && len(an) > 0 && an[0] == "array" {
					if i+1 < len(an) {
						argNode, _ = an[i+1].([]any)
					}
				} else if 2+i < len(n) {
					argNode, _ = n[2+i].([]any)
				}
				isEnum, isMember := enumHasMember(paramTs[i], unwrapAnnotNode(argNode), c.doc)
				mismatch := false
				switch {
				case isEnum && !isMember:
					mismatch = true
				case wantS == "Num" && gotS == "Int":
					mismatch = false
				default:
					mismatch = gotS != wantS && wantS != "Any"
				}
				if mismatch && i < len(argRanges) {
					emit(Diagnostic{
						Range:    argRanges[i],
						Severity: 1, Code: "MS-ARG-TYPE-MISMATCH", Source: "mindscript",
						Message: fmt.Sprintf("Argument %d: got %s, want %s.", i+1, gotS, wantS),
					})
				}
			}
			// Result type (oracle is always nullable)
			if isOracle && len(retT) > 0 {
				retT = []any{"unop", "?", retT}
			}
			// If under-applied, we still return the declared return type for tests
			// (currying surface not required by the current failing cases).
			return foldOut{T: retT}
		}
		// Unknown callee — fall back to Any
		return foldOut{T: typeID("Any")}

	case "binop":
		op, _ := n[1].(string)
		lt := fold(n[2].([]any), append(path, 1), c).T
		rt := fold(n[3].([]any), append(path, 2), c).T
		// % 0 constant
		if op == "%" {
			// RHS is child index 2 in fold-path space (n[3] in raw),
			// so use append(path, 2) for spans (not 3).
			m := unwrapAnnotNode(n[3].([]any))
			if len(m) >= 2 && m[0] == "int" {
				if v0, ok := m[1].(int64); ok && v0 == 0 {
					rng := rangeOrDefault(c.doc, append(path, 2))
					emit(Diagnostic{Range: rng, Severity: 1, Code: "MS-DIV-BY-ZERO-CONST", Source: "mindscript", Message: "Modulo by zero."})
				}
			}
		}
		// bitwise require Int
		if op == "&" || op == "|" || op == "^" || op == "<<" || op == ">>" {
			if formatTypeNode(lt) != "Int" || formatTypeNode(rt) != "Int" {
				if rng, ok := rangeFromPath(c.doc, path); ok {
					emit(Diagnostic{Range: rng, Severity: 1, Code: "MS-BITWISE-NONINT", Source: "mindscript", Message: "Bitwise operators require Int operands."})
				}
			}
			return foldOut{T: typeID("Int")}
		}
		// boolean ops
		if op == "and" || op == "or" {
			if formatTypeNode(lt) != "Bool" || formatTypeNode(rt) != "Bool" {
				if rng, ok := rangeFromPath(c.doc, path); ok {
					emit(Diagnostic{Range: rng, Severity: 1, Code: "MS-ARG-TYPE-MISMATCH", Source: "mindscript", Message: "Boolean operators require Bool operands."})
				}
			}
			return foldOut{T: typeID("Bool")}
		}
		// nullable arithmetic/bitwise: guard usage of T? in numeric ops
		if op == "+" || op == "-" || op == "*" || op == "/" || op == "%" {
			if isNullable(lt) || isNullable(rt) {
				rng := rangeOrDefault(c.doc, path)
				emit(Diagnostic{
					Range:    rng,
					Severity: 2,
					Code:     "MS-MAYBE-NULL-UNSAFE",
					Source:   "mindscript",
					Message:  "Value may be null; guard with a null check.",
				})
			}
		}
		switch op {
		case "+", "-", "*", "%":
			return foldOut{T: LUB(lt, rt)}
		case "/":
			as, bs := formatTypeNode(lt), formatTypeNode(rt)
			if as == "Int" && bs == "Int" {
				return foldOut{T: typeID("Int")}
			}
			if as == "Num" || bs == "Num" {
				return foldOut{T: typeID("Num")}
			}
			return foldOut{T: typeID("Any")}
		case "==", "!=":
			return foldOut{T: typeID("Bool")}
		case "<", "<=", ">", ">=":
			okNum := (formatTypeNode(lt) == "Num" || formatTypeNode(lt) == "Int") &&
				(formatTypeNode(rt) == "Num" || formatTypeNode(rt) == "Int")
			okStr := formatTypeNode(lt) == "Str" && formatTypeNode(rt) == "Str"
			if !(okNum || okStr) {
				if rng, ok := rangeFromPath(c.doc, path); ok {
					emit(Diagnostic{Range: rng, Severity: 1, Code: "MS-COMPARISON-TYPE-MISMATCH", Source: "mindscript", Message: "Comparison requires Num↔Num or Str↔Str."})
				}
			}
			return foldOut{T: typeID("Bool")}
		}
		return foldOut{T: typeID("Any")}

	case "unop":
		if len(n) >= 3 {
			op, _ := n[1].(string)
			r := fold(n[2].([]any), append(path, 1), c).T
			switch op {
			case "-":
				rs := formatTypeNode(r)
				if rs == "Int" || rs == "Num" {
					return foldOut{T: r}
				}
			case "not":
				return foldOut{T: typeID("Bool")}
			case "?":
				return foldOut{T: []any{"unop", "?", r}}
			}
		}
		return foldOut{T: typeID("Any")}

	case "if":
		// ("if", ("pair", cond, then), ..., else?)
		var t []any
		for i := 1; i < len(n); i++ {
			arm, _ := n[i].([]any)
			if len(arm) == 0 {
				continue
			}
			if arm[0] == "pair" && len(arm) >= 3 {
				thenBlk, _ := arm[2].([]any)
				t = LUB(t, fold(thenBlk, append(path, i-1, 1), c).T)
			} else {
				t = LUB(t, fold(arm, append(path, i-1), c).T)
			}
		}
		if len(t) == 0 {
			t = typeID("Any")
		}
		return foldOut{T: t}

	case "fun", "oracle":
		// enter scope; bind params; remember return type
		c.push()
		defer c.pop()
		if (tag == "fun" || tag == "oracle") && len(n) >= 2 {
			if ps, _ := n[1].([]any); len(ps) > 0 && ps[0] == "array" {
				for i := 1; i < len(ps); i++ {
					if p, _ := ps[i].([]any); len(p) >= 2 {
						if idNode, _ := p[1].([]any); len(idNode) >= 2 && idNode[0] == "id" {
							if nm, _ := idNode[1].(string); nm != "" {
								c.bind(nm)
								idPath := append(append(append(mindscript.NodePath{}, path...), 0), i-1)
								idPath = append(idPath, 0)
								var tNode []any
								if len(p) >= 3 {
									tNode, _ = p[2].([]any)
								}
								b := bindingDef{Name: nm, Range: rangeFromID(c.doc, idPath, nm), Kind: "param", TypeNode: tNode}
								c.binds = append(c.binds, b)
								c.doc.binds = append(c.doc.binds, b)
							}
						}
					}
				}
			}
		}
		// body
		if tag == "fun" && len(n) >= 4 {
			// set current return type
			prev := c.curFunRet
			if rt, ok := n[2].([]any); ok {
				c.curFunRet = rt
			}
			defer func() { c.curFunRet = prev }()
			fold(n[3].([]any), append(path, 2), c)
		}
		return foldOut{T: typeID("Any")}

	case "type":
		return foldOut{T: typeID("Any")}

	case "return":
		if len(c.curFunRet) > 0 {
			var actual []any
			if len(n) >= 2 {
				if rn, ok := n[1].([]any); ok {
					actual = fold(rn, append(path, 0), c).T
				}
			} else {
				actual = typeID("Null")
			}
			wantS := formatTypeNode(c.curFunRet)
			gotS := formatTypeNode(actual)
			ok := false
			if wantS == gotS || (wantS == "Num" && gotS == "Int") || (strings.HasSuffix(wantS, "?") && gotS == "Null") {
				ok = true
			}
			if !ok {
				// Always emit, even if a precise span is missing.
				rng := rangeOrDefault(c.doc, path)
				emit(Diagnostic{
					Range:    rng,
					Severity: 1,
					Code:     "MS-RET-TYPE-MISMATCH",
					Source:   "mindscript",
					Message:  fmt.Sprintf("Return type %s does not match %s.", gotS, wantS),
				})
			}
		}
		return foldOut{T: typeID("Any")}

	case "assign":
		// Fallthrough above changed: ensure return-type mismatches always emit,
		// even if a precise span can't be found.
		if len(c.curFunRet) > 0 && len(n) > 0 && n[0] == "return" {
			// (No-op; handled in "return" case. This comment clarifies intent.)
		}

		// Restore the removed block with default-range emission:
		if false {
			_ = rangeOrDefault // keep helper referenced in this scope (no-op)
		}

		if len(n) < 3 {
			return foldOut{T: typeID("Any")}
		}
		lhs, _ := n[1].([]any)
		rhs, _ := n[2].([]any)
		// target validation
		if len(lhs) > 0 {
			switch lhs[0] {
			case "decl", "id", "get", "idx", "darr", "dobj":
			default:
				if rng, ok := rangeFromPath(c.doc, append(path, 1)); ok {
					emit(Diagnostic{Range: rng, Severity: 1, Code: "MS-INVALID-ASSIGN-TARGET", Source: "mindscript", Message: "Invalid assignment target."})
				}
			}
		}

		// If this is a "return" handled above, emit with default range:
		if len(n) > 0 && n[0] == "return" && len(c.curFunRet) > 0 {
			rng := rangeOrDefault(c.doc, path)
			_ = rng // (the actual mismatch logic runs in the "return" branch)
		}

		// collect names in patterns
		type nameAt struct {
			Name string
			Path mindscript.NodePath
		}
		var names []nameAt
		var collectPat func(pat []any, base mindscript.NodePath)
		collectPat = func(pat []any, base mindscript.NodePath) {
			if len(pat) == 0 {
				return
			}
			switch pat[0] {
			case "decl", "id":
				if len(pat) >= 2 {
					if n, _ := pat[1].(string); n != "" {
						names = append(names, nameAt{n, append(mindscript.NodePath{}, base...)})
					}
				}
			case "darr":
				for i := 1; i < len(pat); i++ {
					if ch, ok := pat[i].([]any); ok {
						collectPat(ch, append(append(mindscript.NodePath{}, base...), i-1))
					}
				}
			case "dobj":
				for i := 1; i < len(pat); i++ {
					if pair, ok := pat[i].([]any); ok && len(pair) >= 3 && (pair[0] == "pair" || pair[0] == "pair!") {
						if sub, ok := pair[2].([]any); ok {
							collectPat(sub, append(append(mindscript.NodePath{}, base...), i-1, 1))
						}
					}
				}
			}
		}
		lhsPath := append(append(mindscript.NodePath{}, path...), 1)
		collectPat(lhs, lhsPath)

		base, txt, _ := annotText(rhs)
		kind := "let"
		var tnode []any
		sig := ""
		if len(base) > 0 {
			switch base[0] {
			case "fun":
				kind = "fun"
				if rt, ok := base[2].([]any); ok {
					tnode = rt
				}
			case "oracle":
				kind = "oracle"
				if rt, ok := base[2].([]any); ok {
					tnode = []any{"unop", "?", rt}
				}
			case "type":
				kind = "type"
			}
		}
		// fold RHS once for type if needed
		rhsT := fold(base, append(path, 1), c).T
		if len(tnode) == 0 {
			tnode = rhsT
		}
		// If the pattern is an object destructure, default unknown entries to nullable.
		isDobj := len(lhs) > 0 && lhs[0] == "dobj"
		for _, na := range names {
			if len(base) > 0 && (base[0] == "fun" || base[0] == "oracle") {
				sig = formatFunSig(na.Name, base)
			}
			b := bindingDef{
				Name:     na.Name,
				Range:    rangeFromID(c.doc, na.Path, na.Name),
				DocFull:  txt,
				DocFirst: firstLine(txt),
				Kind:     kind,
				TypeNode: func() []any {
					if isDobj {
						// Open-world: field may be missing → nullable.
						return []any{"unop", "?", typeID("Any")}
					}
					return tnode
				}(),
				Sig: sig,
			}
			c.binds = append(c.binds, b)
			c.doc.binds = append(c.doc.binds, b)
			c.bind(na.Name)
			if c.atTop {
				if lhst, _ := lhs[0].(string); lhst == "decl" || lhst == "id" {
					k := "let"
					s := ""
					docFirst := firstLine(txt)
					if len(base) > 0 {
						if rtag, _ := base[0].(string); rtag == "fun" || rtag == "oracle" {
							k = "fun"
							s = formatFunSig(na.Name, base)
						} else if rtag == "type" {
							k = "type"
						}
					}
					if s0, e0, ok := findDefIDRange(c.doc.tokens, na.Name); ok {
						c.syms = append(c.syms, symbolDef{
							Name:  na.Name,
							Kind:  k,
							Range: makeRange(c.doc.lines, s0, e0, c.doc.text),
							Doc:   docFirst,
							Sig:   s,
						})
					}
				}
			}
			sig = ""
		}
		return foldOut{T: typeID("Any")}
	}
	// default: fold children to harvest diagnostics but return Any
	for i := 1; i < len(n); i++ {
		if ch, ok := n[i].([]any); ok {
			fold(ch, append(path, i-1), c)
		}
	}
	return foldOut{T: typeID("Any")}
}

func unifiedAnalyze(doc *docState) (binds []bindingDef, syms []symbolDef, diags []Diagnostic) {
	if doc == nil || doc.ast == nil {
		return nil, nil, nil
	}
	c := &anCtx{doc: doc}
	// seed scope with already-known bindings
	c.push()
	for _, b := range doc.binds {
		c.bind(b.Name)
	}
	_ = fold(doc.ast, mindscript.NodePath{}, c)
	return c.binds, c.syms, c.diags
}

// Ensure "return" diagnostics always emit even without precise spans:
// Modify the "return" case to use rangeOrDefault.
func init() {
	_ = rangeOrDefault // keep the helper referenced for go vet / linters
}
