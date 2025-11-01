// cmd/lsp/core.go
//
// ROLE: Shared infrastructure for the LSP server: transport helpers, server
//       state, text/position math, diagnostics, token/span utilities, and the
//       analysis pipeline (lex + parse + symbol extraction).
//
// What lives here
//   • Transport helpers for framed stdio (Content-Length) and convenience
//     send/notify wrappers (used by handlers).
//   • Server model:
//        - server: global state (open docs, mutex, interpreter handle).
//        - docState: per-document caches (raw text, line starts, tokens, AST,
//          span index, lightweight symbol table).
//   • Unicode/UTF-16 column math and byte↔position conversions consistent with
//     the LSP spec (positions are UTF-16 code units).
//   • Diagnostics plumbing: mapping lexer/parser errors to LSP ranges and
//     publishing them, including “incomplete” heuristics for on-type feedback.
//   • Token & span helpers: locate tokens at offsets, compute exact byte spans,
//     comment/annotation region detection for semantic tokens & folding.
//   • Analysis pipeline (`analyze`):
//        1) Lex (interactive), 2) Parse (interactive), 3) Collect top-level
//           symbols (no code execution), 4) Publish/clear diagnostics.
//     Results populate docState so feature handlers can be fast and side-effect-free.
//
// What does NOT live here
//   • No LSP feature handlers themselves (hover, completion, etc.).
//   • No business rules about how to format responses—handlers own that.
//   • No interpreter-driven execution of user code. (We only build tokens/AST
//     and extract symbols; see features.go for the optional, guarded lookup of
//     global/builtin metadata.)
//
// Why this separation
//   • Centralizes shared mechanics so features stay small and consistent.
//   • Makes unit testing of text math and analysis independent of UI features.
//
// Dependencies
//   • Relies on internal/mindscript lexer & parser (no VM/interpreter internals).
//   • Holds an *Interpreter pointer only for feature-level metadata queries;
//     core analysis itself does not execute user programs.

// cmd/lsp/core.go
package main

import (
	"bufio"
	"bytes"
	"encoding/json"
	"fmt"
	"io"
	"os"
	"strings"
	"sync"
	"unicode/utf8"

	mindscript "github.com/DAIOS-AI/msg/internal/mindscript"
)

////////////////////////////////////////////////////////////////////////////////
// Transport (stdio framing) + send/notify
////////////////////////////////////////////////////////////////////////////////

var stdoutSink io.Writer = os.Stdout

func init() {
	// Silence unsolicited output during `go test` unless opted in.
	if strings.HasSuffix(os.Args[0], ".test") && os.Getenv("LSP_STDOUT") == "" {
		stdoutSink = io.Discard
	}
}

func readMsg(r *bufio.Reader) ([]byte, error) {
	var contentLen int
	for {
		line, err := r.ReadString('\n')
		if err != nil {
			return nil, err
		}
		line = strings.TrimRight(line, "\r\n")
		if line == "" {
			break
		}
		if i := strings.IndexByte(line, ':'); i >= 0 {
			key := strings.ToLower(strings.TrimSpace(line[:i]))
			val := strings.TrimSpace(line[i+1:])
			if key == "content-length" {
				_, _ = fmt.Sscanf(val, "%d", &contentLen)
			}
		}
	}
	if contentLen <= 0 {
		return nil, io.EOF
	}
	buf := make([]byte, contentLen)
	_, err := io.ReadFull(r, buf)
	return buf, err
}

func writeMsg(w io.Writer, v any) error {
	body, err := json.Marshal(v)
	if err != nil {
		return err
	}
	var b bytes.Buffer
	fmt.Fprintf(&b, "Content-Length: %d\r\n\r\n", len(body))
	b.Write(body)
	_, err = w.Write(b.Bytes())
	return err
}

func (s *server) sendResponse(id json.RawMessage, result any, respErr *ResponseError) {
	if respErr == nil && result == nil {
		rawNull := json.RawMessage([]byte("null"))
		_ = writeMsg(stdoutSink, Response{JSONRPC: "2.0", ID: id, Result: rawNull})
		return
	}
	_ = writeMsg(stdoutSink, Response{JSONRPC: "2.0", ID: id, Result: result, Error: respErr})
}

func (s *server) notify(method string, params any) {
	_ = writeMsg(stdoutSink, map[string]any{
		"jsonrpc": "2.0",
		"method":  method,
		"params":  params,
	})
}

////////////////////////////////////////////////////////////////////////////////
// Server state & document model
////////////////////////////////////////////////////////////////////////////////

type symbolDef struct {
	Name  string
	Kind  string // "let" | "fun" | "type"
	Range Range  // where it's declared
	Doc   string // first line, if available
	Sig   string // pretty signature for fun/oracle
}

type docState struct {
	uri     string
	text    string
	lines   []int // line start offsets (byte indices)
	symbols []symbolDef
	tokens  []mindscript.Token
	ast     mindscript.S
	spans   *mindscript.SpanIndex
}

type server struct {
	mu   sync.RWMutex
	docs map[string]*docState
	ip   *mindscript.Interpreter
}

func newServer() *server {
	ip, _ := mindscript.NewInterpreter()
	return &server{
		docs: make(map[string]*docState),
		ip:   ip,
	}
}

// snapshotDoc returns a consistent, read-only snapshot of a document.
func (s *server) snapshotDoc(uri string) *docState {
	s.mu.RLock()
	defer s.mu.RUnlock()
	d := s.docs[uri]
	if d == nil {
		return nil
	}
	cp := *d // shallow copy
	if d.lines != nil {
		cp.lines = append([]int(nil), d.lines...)
	}
	if d.tokens != nil {
		cp.tokens = append([]mindscript.Token(nil), d.tokens...)
	}
	if d.symbols != nil {
		cp.symbols = append([]symbolDef(nil), d.symbols...)
	}
	// ast/spans are immutable enough to share (spans is read-only).
	cp.ast = d.ast
	cp.spans = d.spans
	return &cp
}

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

// hasValidSpan reports whether the lexer provided concrete byte offsets.
// Used for ANNOTATION tokens which must have StartByte/EndByte or be ignored.
func hasValidSpan(t mindscript.Token, textLen int) bool {
	return t.StartByte >= 0 && t.EndByte >= t.StartByte && t.EndByte <= textLen
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

func indexOfToken(toks []mindscript.Token, tk mindscript.Token) int {
	for i, t := range toks {
		if t == tk {
			return i
		}
	}
	return -1
}

// wordAt: prefer token-based match; fallback to ASCII scan if needed.
func wordAt(doc *docState, pos Position) (string, Range) {
	off := posToOffset(doc.lines, pos, doc.text)
	if off < 0 || off > len(doc.text) {
		return "", Range{}
	}
	for _, t := range doc.tokens {
		// FIX: only IDs are symbol names; TYPE is a keyword.
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

////////////////////////////////////////////////////////////////////////////////
// Shared keyword/type helpers (used by hover/completion/semTokens)
////////////////////////////////////////////////////////////////////////////////

func isKeywordButNotType(tt mindscript.TokenType) bool {
	switch tt {
	case mindscript.AND, mindscript.OR, mindscript.NOT,
		mindscript.LET, mindscript.DO, mindscript.END, mindscript.RETURN, mindscript.BREAK, mindscript.CONTINUE,
		mindscript.IF, mindscript.THEN, mindscript.ELIF, mindscript.ELSE,
		mindscript.FUNCTION, mindscript.ORACLE,
		mindscript.FOR, mindscript.IN, mindscript.FROM, mindscript.WHILE,
		// FIX: 'type' keyword should be colored as a keyword, not a type identifier.
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

// semantic tokens type legend index (handlers will read this)
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
// Definition heuristics & symbol formatting
////////////////////////////////////////////////////////////////////////////////

// defRangeByTokens: heuristic: let <name> … OR <name> = … (same or next line)
func defRangeByTokens(doc *docState, name string) (Range, bool) {
	toks := doc.tokens
	for i := 0; i < len(toks); i++ {
		t := toks[i]
		if t.Type != mindscript.ID || tokenName(t) != name {
			continue
		}
		// let <name> …
		if i >= 1 && toks[i-1].Type == mindscript.LET && toks[i-1].Line == t.Line {
			s, e := tokenSpan(doc, t)
			return makeRange(doc.lines, s, e, doc.text), true
		}
		if i >= 2 && toks[i-2].Type == mindscript.LET && toks[i-2].Line == t.Line {
			s, e := tokenSpan(doc, t)
			return makeRange(doc.lines, s, e, doc.text), true
		}
		// <name> = … (same line)
		found := false
		line := t.Line
		for j := i + 1; j < len(toks) && toks[j].Line == line; j++ {
			if toks[j].Type == mindscript.ASSIGN {
				found = true
				break
			}
		}
		// spill to next line: <name> \n =
		if !found {
			for j := i + 1; j < len(toks); j++ {
				if toks[j].Line > line+1 {
					break
				}
				if toks[j].Type == mindscript.ASSIGN {
					found = true
					break
				}
			}
		}
		if found {
			s, e := tokenSpan(doc, t)
			return makeRange(doc.lines, s, e, doc.text), true
		}
	}
	return Range{}, false
}

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
	// 1) Lex (interactive is fine; the tests use valid input)
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
			} else {
				// If even the prefix fails, leave tokens as-is (keep previous coloring).
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

	// 3) Rebuild top-level symbols (alpha/beta/etc.). This does NOT execute user code.
	doc.symbols = collectTopLevelSymbols(doc)

	// 4) Success: clear any previous diagnostics.
	s.clearDiagnostics(doc.uri)
}

// collectTopLevelSymbols walks the AST (root-level only) and extracts symbols:
//   - let/assign of a simple decl: ("assign", ("decl", name), rhs)
//   - marks kind "fun" when rhs tag == "fun"; otherwise "let".
//
// Ranges are the byte range of the defining identifier token.
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

// addTopLevelAssign adds a symbol for ("assign", ("decl", name), rhs).
// Kind is "fun" if rhs tag == "fun"; otherwise "let".
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
	if rhs, ok := node[2].([]any); ok && len(rhs) > 0 {
		rhs = unwrapAnnotNode(rhs) // <-- strip ("annot", …)
		if rtag, _ := rhs[0].(string); rtag == "fun" {
			kind = "fun"
		}
	}

	// Find the defining identifier token's exact byte span.
	if start, end, ok := findDefIDRange(doc.tokens, name); ok {
		rng := makeRange(doc.lines, start, end, doc.text)
		*out = append(*out, symbolDef{
			Name:  name,
			Kind:  kind,
			Range: rng,
			// Sig/Doc optional for now; tests only require presence.
		})
	}
}

// findDefIDRange returns the byte span of the defining ID token `name`.
// Heuristic: choose the first ID token with that name that looks like a def:
// - immediately followed by '=' (ASSIGN), OR
// - immediately preceded by 'let' (LET)
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
