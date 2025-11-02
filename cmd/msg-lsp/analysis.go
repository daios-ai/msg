// analysis.go
//
// ROLE: Lex/parse/index pipeline, text/position helpers, span utilities, and
//       diagnostics mapping. Populates per-document caches used by features.
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

func (s *server) publishError(doc *docState, err error) {
	// REPL-friendly: don't nag on incomplete constructs.
	if e, ok := err.(*mindscript.Error); ok {
		// Prefer the engine’s explicit “incomplete” signal.
		if e.Kind == mindscript.DiagIncomplete {
			s.clearDiagnostics(doc.uri)
			return
		}
		// Back-compat: older lexers/parsers may still surface text hints.
		if e.Kind == mindscript.DiagParse || e.Kind == mindscript.DiagLex {
			msg := strings.ToLower(e.Msg)
			if strings.Contains(msg, "incomplete") || strings.Contains(msg, "unterminated") {
				s.clearDiagnostics(doc.uri)
				return
			}
		}
	}

	line, col := 0, 0
	code := ""
	if e, ok := err.(*mindscript.Error); ok {
		line, col = e.Line, e.Col // 1-based positions from MindScript
		switch e.Kind {
		case mindscript.DiagParse:
			code = "PARSE"
		case mindscript.DiagLex:
			code = "LEX"
		default:
		}
	}
	if line > 0 {
		line-- // engine lines are 1-based
	}
	start := byteColToOffset(doc.lines, line, col, doc.text)
	end := start

	// Try to expand to a token on that exact start position.
	if len(doc.tokens) > 0 {
		for _, t := range doc.tokens {
			if t.Line == line+1 && t.Col == col {
				ts := byteColToOffset(doc.lines, t.Line-1, t.Col, doc.text)
				te := ts + len(t.Lexeme)
				start, end = ts, te
				break
			}
		}
	}
	if end <= start {
		if start < len(doc.text) {
			// Highlight the next rune when inside the buffer.
			_, sz := utf8.DecodeRuneInString(doc.text[start:])
			if sz <= 0 {
				sz = 1
			}
			end = start + sz
		} else if start > 0 {
			// At end-of-buffer: highlight the previous rune.
			_, sz := utf8.DecodeLastRuneInString(doc.text[:start])
			if sz <= 0 {
				sz = 1
			}
			start = start - sz
			end = start + sz
		}
	}

	s.notify("textDocument/publishDiagnostics", PublishDiagnosticsParams{
		URI: doc.uri,
		Diagnostics: []Diagnostic{{
			Range:    makeRange(doc.lines, start, end, doc.text),
			Severity: 1,
			Code:     code,
			Source:   "mindscript",
			Message:  err.Error(),
		}},
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

	cand := t.Col
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
	s := lineStart + t.Col
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

// tokenAtOffset returns the token index and span whose lexeme covers [off].
func tokenAtOffset(doc *docState, off int) (idx int, t mindscript.Token, start, end int, ok bool) {
	for i, tk := range doc.tokens {
		s, e := tokenSpan(doc, tk)
		if off >= s && off < e {
			return i, tk, s, e, true
		}
	}
	return -1, mindscript.Token{}, 0, 0, false
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

func findSymbol(doc *docState, name string) (symbolDef, bool) {
	for _, s := range doc.symbols {
		if s.Name == name {
			return s, true
		}
	}
	return symbolDef{}, false
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

func overlaps(a, b [2]int) bool { return a[0] < b[1] && b[0] < a[1] }

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
// Analysis (lex + parse + symbol extraction) — publishes diagnostics via notify
////////////////////////////////////////////////////////////////////////////////

// analyze lexes, parses, and refreshes the per-doc caches used by features.
// It fills: doc.tokens, doc.ast (when parse succeeds), and doc.symbols (top-level defs).
func (s *server) analyze(doc *docState) {
	// 1) Lex (interactive)
	lx := mindscript.NewLexerInteractive(doc.text)
	toks, err := lx.Scan()
	if err != nil {
		// Try to salvage tokens up to the error position so semantic tokens
		// still color the prefix.
		if e, ok := err.(*mindscript.Error); ok && e.Kind == mindscript.DiagLex {
			off := byteColToOffset(doc.lines, e.Line-1, e.Col-1, doc.text)
			if off < 0 {
				off = 0
			}
			if off > len(doc.text) {
				off = len(doc.text)
			}
			// Re-lex the prefix only; this should succeed.
			px := mindscript.NewLexerInteractive(doc.text[:off])
			ptoks, pErr := px.Scan()
			if pErr == nil {
				if n := len(ptoks); n > 0 && ptoks[n-1].Type == mindscript.EOF {
					ptoks = ptoks[:n-1]
				}
				doc.tokens = ptoks
			}
		}
		doc.symbols = nil
		doc.ast = nil
		s.publishError(doc, err)
		return
	}
	// Drop the terminal EOF token for downstream convenience.
	if n := len(toks); n > 0 && toks[n-1].Type == mindscript.EOF {
		toks = toks[:n-1]
	}
	doc.tokens = toks

	// 2) Parse (interactive) with exact spans for every node.
	//    Interactive mode yields DiagIncomplete for half-typed code.
	ast, spans, err := mindscript.ParseSExprInteractiveWithSpans(doc.text)
	if err != nil {
		// Parsing failed (possibly incomplete). Keep tokens, clear AST/symbols.
		// IMPORTANT: preserve last-good spans so folding/range features remain stable.
		doc.ast = nil
		doc.symbols = nil
		// doc.spans is intentionally NOT touched here.
		s.publishError(doc, err)
		return
	}
	doc.ast = ast
	doc.spans = spans

	// Uniform: collect all bindings + docs (any assignment anywhere).
	doc.binds = collectBindings(doc)

	// 3) Rebuild top-level symbols (alpha/beta/etc.). This does NOT execute user code.
	doc.symbols = collectTopLevelSymbols(doc)

	// 4) Success: clear any previous diagnostics.
	s.clearDiagnostics(doc.uri)
}

////////////////////////////////////////////////////////////////////////////////
// Lightweight static type synthesis (no execution)
////////////////////////////////////////////////////////////////////////////////

// We represent types as the same AST nodes used in function signatures:
//   ("id","Int"), ("array", T), ("map", ("pair", ("str","k"), T), ...), ("unop","?", T)
// and pretty-print with mindscript.FormatType.

func typeID(name string) []any { return []any{"id", name} }

// formatTypeNode -> human string (falls back to "Any" on nil/unknown)
func formatTypeNode(t []any) string {
	if len(t) == 0 {
		return "Any"
	}
	return mindscript.FormatType(t)
}

// numericSuper: Int + Num -> Num; Int + Int -> Int; else Any.
func numericSuper(a, b []any) []any {
	as, bs := formatTypeNode(a), formatTypeNode(b)
	if as == "Num" || bs == "Num" {
		return typeID("Num")
	}
	if as == "Int" && bs == "Int" {
		return typeID("Int")
	}
	// treat unknowns conservatively
	if as == "Any" || bs == "Any" {
		return typeID("Any")
	}
	return typeID("Any")
}

// commonSuper for conditionals/merges: a == b -> a; Int vs Num -> Num; else Any.
func commonSuper(a, b []any) []any {
	as, bs := formatTypeNode(a), formatTypeNode(b)
	if as == bs {
		return a
	}
	// numeric widening
	if (as == "Int" && bs == "Num") || (as == "Num" && bs == "Int") {
		return typeID("Num")
	}
	return typeID("Any")
}

// findLocalFunRetType returns the declared return type node of a top-level fun/oracle named 'name'.
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
				base, _ := unwrapAnnotNode(rhs), true
				if len(base) >= 3 && (base[0] == "fun" || base[0] == "oracle") {
					// fun/oracle layout: ("fun", params, ret, body) / ("oracle", params, outType, source)
					if tnode, ok := base[2].([]any); ok {
						// oracle calls return nullable; the declared *output* type lives here.
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

// inferExprType synthesizes a best-effort type for a value expression.
func inferExprType(doc *docState, n []any) []any {
	if len(n) == 0 {
		return typeID("Any")
	}
	tag, _ := n[0].(string)
	switch tag {
	case "str":
		return typeID("Str")
	case "int":
		return typeID("Int")
	case "num":
		return typeID("Num")
	case "bool":
		return typeID("Bool")
	case "null":
		return typeID("Null")

	case "array":
		// ("array", e1, e2, ...)
		if len(n) == 1 {
			return []any{"array", typeID("Any")}
		}
		elem := typeID("Any")
		for i := 1; i < len(n); i++ {
			if ch, ok := n[i].([]any); ok {
				elem = commonSuper(elem, inferExprType(doc, ch))
			}
		}
		return []any{"array", elem}

	case "map":
		// Build an open-map type with fields we see: { key: T, ... }
		out := []any{"map"}
		for i := 1; i < len(n); i++ {
			p, ok := n[i].([]any)
			if !ok || len(p) < 3 {
				continue
			}
			if p[0] != "pair" && p[0] != "pair!" {
				continue
			}
			keyNode, _ := p[1].([]any)
			valNode, _ := p[2].([]any)
			ks := ""
			if len(keyNode) >= 2 && keyNode[0] == "str" {
				ks, _ = keyNode[1].(string)
			}
			t := inferExprType(doc, valNode)
			out = append(out, []any{"pair", []any{"str", ks}, t})
		}
		return out

	case "call":
		// ("call", callee, arg1, ...)
		if len(n) >= 2 {
			callee, _ := n[1].([]any)
			// id callee?
			if len(callee) >= 2 && callee[0] == "id" {
				name, _ := callee[1].(string)
				// Prefer local fun/oracle signature
				if rt, ok := findLocalFunRetType(doc, name); ok {
					return rt
				}
			}
		}
		return typeID("Any")

	case "get":
		// ("get", obj, ("str", name))
		if len(n) >= 3 {
			objNode, _ := n[1].([]any)
			objT := inferExprType(doc, objNode)
			key := ""
			if ks, ok := n[2].([]any); ok && len(ks) >= 2 && ks[0] == "str" {
				key, _ = ks[1].(string)
			}
			// If objT is a map type with that field, return the field type.
			if len(objT) > 0 && objT[0] == "map" {
				for i := 1; i < len(objT); i++ {
					if pr, ok := objT[i].([]any); ok && len(pr) >= 3 && (pr[0] == "pair" || pr[0] == "pair!") {
						if k, ok := pr[1].([]any); ok && len(k) >= 2 && k[0] == "str" {
							if nm, _ := k[1].(string); nm == key {
								if tv, ok := pr[2].([]any); ok {
									return tv
								}
							}
						}
					}
				}
			}
		}
		return typeID("Any")

	case "idx":
		// ("idx", obj, index)
		if len(n) >= 2 {
			objNode, _ := n[1].([]any)
			objT := inferExprType(doc, objNode)
			if len(objT) >= 2 && objT[0] == "array" {
				if t, ok := objT[1].([]any); ok {
					return t
				}
			}
		}
		return typeID("Any")

	case "binop":
		// ("binop", op, lhs, rhs)
		if len(n) >= 4 {
			op, _ := n[1].(string)
			lhsNode, _ := n[2].([]any)
			rhsNode, _ := n[3].([]any)
			lhs := inferExprType(doc, lhsNode)
			rhs := inferExprType(doc, rhsNode)
			switch op {
			case "+", "-", "*", "%":
				return numericSuper(lhs, rhs)
			case "/":
				// Int/Int -> Int else Num (if either Num -> Num)
				as, bs := formatTypeNode(lhs), formatTypeNode(rhs)
				if as == "Int" && bs == "Int" {
					return typeID("Int")
				}
				if as == "Num" || bs == "Num" {
					return typeID("Num")
				}
				return typeID("Any")
			case "==", "!=":
				return typeID("Bool")
			case "<", "<=", ">", ">=":
				return typeID("Bool")
			case "and", "or", "&", "|", "^", "<<", ">>":
				if op == "and" || op == "or" {
					return typeID("Bool")
				}
				return typeID("Int")
			}
		}
		return typeID("Any")

	case "unop":
		// ("unop", op, rhs)
		if len(n) >= 3 {
			op, _ := n[1].(string)
			rNode, _ := n[2].([]any)
			r := inferExprType(doc, rNode)
			switch op {
			case "-":
				rs := formatTypeNode(r)
				if rs == "Int" || rs == "Num" {
					return r
				}
				return typeID("Any")
			case "not":
				return typeID("Bool")
			case "?":
				// nullable postfix on values shows up as unop "?" too (when used in type positions).
				return []any{"unop", "?", r}
			}
		}
		return typeID("Any")

	case "if":
		// ("if", ("pair", cond, then), ..., else?)
		// merge then/else result types
		var t []any
		for i := 1; i < len(n); i++ {
			arm, _ := n[i].([]any)
			if len(arm) == 0 {
				continue
			}
			if arm[0] == "pair" && len(arm) >= 3 {
				thenBlk, _ := arm[2].([]any)
				t = commonSuper(t, inferExprType(doc, thenBlk))
			} else {
				// else tail (single block)
				t = commonSuper(t, inferExprType(doc, arm))
			}
		}
		if len(t) == 0 {
			return typeID("Any")
		}
		return t
	}
	return typeID("Any")
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
			if s, _ := child[1].(string); s != "" {
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

// collectBindings walks the AST and records every binding of the form:
//
//	("assign", ("decl" | "id" | "darr" | "dobj", ...), value)
//
// It extracts docs from the VALUE's annotation wrapper if present, for all kinds.
func collectBindings(doc *docState) []bindingDef {
	var out []bindingDef
	if doc == nil || doc.ast == nil {
		return out
	}

	var push func(name string, rhs []any, rng Range)
	push = func(name string, rhs []any, rng Range) {
		if name == "" {
			return
		}
		// Best-effort kind + doc + type/sig: inspect rhs tag (after stripping annot).
		base, txt, _ := annotText(rhs)
		kind := ""
		var tnode []any
		sig := ""
		if len(base) > 0 {
			switch tag := base[0].(string); tag {
			case "fun":
				kind = "fun"
				// declared return type lives at base[2]
				if rt, ok := base[2].([]any); ok {
					tnode = rt
				}
				// produce pretty signature now that we have the name
				sig = formatFunSig(name, base)
			case "oracle":
				kind = "oracle"
				if rt, ok := base[2].([]any); ok {
					tnode = []any{"unop", "?", rt}
				}
				sig = formatFunSig(name, base)
			case "type":
				kind = "type"
			default:
				kind = "let"
			}
		}
		if len(tnode) == 0 {
			// Values (non fun/oracle/type): synthesize a type
			tnode = inferExprType(doc, base)
		}
		out = append(out, bindingDef{
			Name:     name,
			Range:    rng,
			DocFull:  txt,
			DocFirst: firstLine(txt),
			Kind:     kind,
			TypeNode: tnode,
			Sig:      sig,
		})
	}

	// name + exact id/decl path for SpanIndex → Range
	type nameAt struct {
		Name string
		Path mindscript.NodePath
	}

	var collectPat func(pat []any, base mindscript.NodePath, acc *[]nameAt)
	collectPat = func(pat []any, base mindscript.NodePath, acc *[]nameAt) {
		if len(pat) == 0 {
			return
		}
		tag, _ := pat[0].(string)
		switch tag {
		case "decl":
			if len(pat) >= 2 {
				if n, _ := pat[1].(string); n != "" {
					// Use the decl node span for the identifier
					*acc = append(*acc, nameAt{Name: n, Path: append(mindscript.NodePath{}, base...)})
				}
			}
		case "id":
			if len(pat) >= 2 {
				if n, _ := pat[1].(string); n != "" {
					// Path points at the ("id", name) node itself
					*acc = append(*acc, nameAt{Name: n, Path: append(mindscript.NodePath{}, base...)})
				}
			}
		case "darr":
			for i := 1; i < len(pat); i++ {
				if ch, ok := pat[i].([]any); ok {
					collectPat(ch, append(append(mindscript.NodePath{}, base...), i-1), acc)
				}
			}
		case "dobj":
			for i := 1; i < len(pat); i++ {
				if pair, ok := pat[i].([]any); ok && len(pair) >= 3 && (pair[0] == "pair" || pair[0] == "pair!") {
					if sub, ok := pair[2].([]any); ok {
						// descend into value position of the pair: child index 2 → path component (2-1)=1
						collectPat(sub, append(append(mindscript.NodePath{}, base...), i-1, 1), acc)
					}
				}
			}
		default:
			// ignore other lvalues (get/idx) in this minimal pass
		}
	}

	// Walk with path tracking so we can compute param ranges via SpanIndex.
	var walk func(n []any, path mindscript.NodePath)
	walk = func(n []any, path mindscript.NodePath) {
		if len(n) == 0 {
			return
		}
		tag, _ := n[0].(string)
		if tag == "assign" && len(n) >= 3 {
			lhs, _ := n[1].([]any)
			rhs, _ := n[2].([]any)
			// Compute the base path to LHS within this assign node.
			lhsPath := append(append(mindscript.NodePath{}, path...), 1)
			var names []nameAt
			collectPat(lhs, lhsPath, &names)
			for _, na := range names {
				// Prefer precise SpanIndex range; fallback to token heuristic only if spans missing.
				rng, ok := rangeFromPath(doc, na.Path)
				if !ok {
					if s, e, ok2 := findDefIDRange(doc.tokens, na.Name); ok2 {
						rng = makeRange(doc.lines, s, e, doc.text)
					}
				}
				push(na.Name, rhs, rng)
			}
		}
		// Collect parameters as bindings: fun/oracle(x: T, ...) -> params of kind "param"
		if (tag == "fun" || tag == "oracle") && len(n) >= 3 {
			ps, _ := n[1].([]any) // params array
			if len(ps) > 0 && ps[0] == "array" {
				for i := 1; i < len(ps); i++ {
					pair, _ := ps[i].([]any) // ("pair"| "pair!", ("id", name), typeS)
					if len(pair) >= 3 && (pair[0] == "pair" || pair[0] == "pair!") {
						idNode, _ := pair[1].([]any)
						tNode, _ := pair[2].([]any)
						name := ""
						if len(idNode) >= 2 && idNode[0] == "id" {
							if s, ok := idNode[1].(string); ok {
								name = s
							}
						}
						if name != "" {
							// Path to id child:
							//   fun := ("fun", params, ret, body)
							//   params is child 0 in NodePath
							//   pair is (i-1) within params
							//   within ("pair", ("id", name), type), the ("id", ...) is child 0
							idPath := append(append(append(mindscript.NodePath{}, path...), 0), i-1)
							idPath = append(idPath, 0)
							rng, ok := rangeFromPath(doc, idPath)
							if !ok {
								// Fallback to token heuristic if spans missing (shouldn't happen)
								if s, e, ok := findDefIDRange(doc.tokens, name); ok {
									rng = makeRange(doc.lines, s, e, doc.text)
								}
							}
							out = append(out, bindingDef{
								Name:     name,
								Range:    rng,
								DocFull:  "",
								DocFirst: "",
								Kind:     "param",
								TypeNode: tNode,
							})
						}
					}
				}
			}
		}
		// Recurse
		for i := 1; i < len(n); i++ {
			if ch, ok := n[i].([]any); ok {
				walk(ch, append(path, i-1))
			}
		}
	}
	walk(doc.ast, mindscript.NodePath{})
	return out
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
func collectTopLevelSymbols(doc *docState) []symbolDef {
	var out []symbolDef
	root := doc.ast
	if len(root) == 0 {
		return out
	}
	tag, _ := root[0].(string)
	if tag == "block" {
		for i := 1; i < len(root); i++ {
			if ch, ok := root[i].([]any); ok {
				ch = unwrapAnnotNode(ch)
				addTopLevelAssign(ch, doc, &out)
			}
		}
	} else {
		// Single-expression file that might still be an assignment.
		addTopLevelAssign(unwrapAnnotNode(root), doc, &out)
	}
	return out
}

// addTopLevelAssign adds a symbol for ("assign", ("decl"| "id", name), rhs).
func addTopLevelAssign(node []any, doc *docState, out *[]symbolDef) {
	if len(node) < 3 {
		return
	}
	tag, _ := node[0].(string)
	if tag != "assign" {
		return
	}
	lhs, _ := node[1].([]any)
	if len(lhs) < 2 {
		return
	}
	lhsTag, _ := lhs[0].(string)
	var name string
	switch lhsTag {
	case "decl":
		name, _ = lhs[1].(string)
	case "id":
		// Allow plain identifier assignments:  f = fun(...),  beta = alpha
		name, _ = lhs[1].(string)
	default:
		// Ignore complex targets (destructuring etc.) for now.
		return
	}
	if name == "" {
		return
	}

	kind := "let"
	sig := ""
	docFirst := ""
	if rhs, ok := node[2].([]any); ok && len(rhs) > 0 {
		base, txt, _ := annotText(rhs) // doc text for first-line
		docFirst = firstLine(txt)
		if rtag, _ := base[0].(string); rtag == "fun" || rtag == "oracle" {
			kind = "fun"
			sig = formatFunSig(name, base)
		} else if rtag == "type" {
			kind = "type"
		}
	}

	// Find the defining identifier token's exact byte span.
	if start, end, ok := findDefIDRange(doc.tokens, name); ok {
		rng := makeRange(doc.lines, start, end, doc.text)
		*out = append(*out, symbolDef{
			Name:  name,
			Kind:  kind,
			Range: rng,
			Doc:   docFirst,
			Sig:   sig,
		})
	}
}

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
