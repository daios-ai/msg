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

	mindscript "github.com/DAIOS-AI/msg"
)

////////////////////////////////////////////////////////////////////////////////
// LSP protocol types (wire structs)
////////////////////////////////////////////////////////////////////////////////

type Position struct {
	Line      int `json:"line"`
	Character int `json:"character"` // UTF-16 code units
}

type Range struct {
	Start Position `json:"start"`
	End   Position `json:"end"`
}

type Location struct {
	URI   string `json:"uri"`
	Range Range  `json:"range"`
}

type TextDocumentIdentifier struct {
	URI string `json:"uri"`
}

type TextDocumentItem struct {
	URI        string `json:"uri"`
	LanguageID string `json:"languageId"`
	Version    int    `json:"version"`
	Text       string `json:"text"`
}

type TextDocumentContentChangeEvent struct {
	Range       *Range `json:"range,omitempty"`
	RangeLength int    `json:"rangeLength,omitempty"`
	Text        string `json:"text"`
}

type InitializeParams struct {
	Capabilities any    `json:"capabilities"`
	RootURI      string `json:"rootUri,omitempty"`
}

type TextDocumentSyncOptions struct {
	OpenClose bool `json:"openClose"`
	// 1 = Full, 2 = Incremental
	Change            int  `json:"change"`
	WillSave          bool `json:"willSave"`
	WillSaveWaitUntil bool `json:"willSaveWaitUntil"`
	Save              *struct {
		IncludeText bool `json:"includeText"`
	} `json:"save,omitempty"`
}

type ServerCapabilities struct {
	TextDocumentSync   TextDocumentSyncOptions `json:"textDocumentSync"`
	HoverProvider      bool                    `json:"hoverProvider"`
	DefinitionProvider bool                    `json:"definitionProvider"`
	CompletionProvider *struct {
		TriggerCharacters []string `json:"triggerCharacters"`
	} `json:"completionProvider,omitempty"`
	DocumentSymbolProvider          bool `json:"documentSymbolProvider"`
	ReferencesProvider              bool `json:"referencesProvider"`
	WorkspaceSymbolProvider         bool `json:"workspaceSymbolProvider"`
	DocumentFormattingProvider      bool `json:"documentFormattingProvider"`
	DocumentRangeFormattingProvider bool `json:"documentRangeFormattingProvider"`
	SignatureHelpProvider           *struct {
		TriggerCharacters   []string `json:"triggerCharacters"`
		RetriggerCharacters []string `json:"retriggerCharacters"`
	} `json:"signatureHelpProvider,omitempty"`
	SemanticTokensProvider *struct {
		Legend struct {
			TokenTypes     []string `json:"tokenTypes"`
			TokenModifiers []string `json:"tokenModifiers"`
		} `json:"legend"`
		Full  bool `json:"full"`
		Range bool `json:"range"`
	} `json:"semanticTokensProvider,omitempty"`
	FoldingRangeProvider bool `json:"foldingRangeProvider"`
}

type InitializeResult struct {
	Capabilities ServerCapabilities `json:"capabilities"`
	ServerInfo   map[string]string  `json:"serverInfo,omitempty"`
}

type Request struct {
	JSONRPC string          `json:"jsonrpc"`
	ID      json.RawMessage `json:"id,omitempty"`
	Method  string          `json:"method"`
	Params  json.RawMessage `json:"params,omitempty"`
}

type Response struct {
	JSONRPC string          `json:"jsonrpc"`
	ID      json.RawMessage `json:"id,omitempty"`
	Result  any             `json:"result,omitempty"`
	Error   *ResponseError  `json:"error,omitempty"`
}

type ResponseError struct {
	Code    int    `json:"code"`
	Message string `json:"message"`
}

type Diagnostic struct {
	Range    Range  `json:"range"`
	Severity int    `json:"severity,omitempty"` // 1 = Error
	Code     string `json:"code,omitempty"`
	Source   string `json:"source,omitempty"`
	Message  string `json:"message"`
}

type PublishDiagnosticsParams struct {
	URI         string       `json:"uri"`
	Diagnostics []Diagnostic `json:"diagnostics"`
}

type Hover struct {
	Contents MarkupContent `json:"contents"`
	Range    *Range        `json:"range,omitempty"`
}

type MarkupContent struct {
	Kind  string `json:"kind"`  // "plaintext" or "markdown"
	Value string `json:"value"` // content
}

type CompletionItem struct {
	Label            string `json:"label"`
	Kind             int    `json:"kind,omitempty"`
	Detail           string `json:"detail,omitempty"`
	InsertText       string `json:"insertText,omitempty"`
	InsertTextFormat int    `json:"insertTextFormat,omitempty"`
}

type DocumentSymbol struct {
	Name           string           `json:"name"`
	Detail         string           `json:"detail,omitempty"`
	Kind           int              `json:"kind"`
	Range          Range            `json:"range"`
	SelectionRange Range            `json:"selectionRange"`
	Children       []DocumentSymbol `json:"children,omitempty"`
}

type SemanticTokensParams struct {
	TextDocument TextDocumentIdentifier `json:"textDocument"`
}

type SemanticTokensRangeParams struct {
	TextDocument TextDocumentIdentifier `json:"textDocument"`
	Range        Range                  `json:"range"`
}

type SemanticTokens struct {
	Data []uint32 `json:"data"`
}

type FoldingRange struct {
	StartLine      int     `json:"startLine"`
	StartCharacter *int    `json:"startCharacter,omitempty"`
	EndLine        int     `json:"endLine"`
	EndCharacter   *int    `json:"endCharacter,omitempty"`
	Kind           *string `json:"kind,omitempty"` // "region", "comment"
}

type SignatureHelpParams struct {
	TextDocument TextDocumentIdentifier `json:"textDocument"`
	Position     Position               `json:"position"`
}
type SignatureHelp struct {
	Signatures      []SignatureInformation `json:"signatures"`
	ActiveSignature int                    `json:"activeSignature"`
	ActiveParameter int                    `json:"activeParameter"`
}
type SignatureInformation struct {
	Label         string                 `json:"label"`
	Documentation *MarkupContent         `json:"documentation,omitempty"`
	Parameters    []ParameterInformation `json:"parameters,omitempty"`
}
type ParameterInformation struct {
	Label         string         `json:"label"`
	Documentation *MarkupContent `json:"documentation,omitempty"`
}

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
}

type server struct {
	mu   sync.RWMutex
	docs map[string]*docState
	ip   *mindscript.Interpreter
}

func newServer() *server {
	return &server{
		docs: make(map[string]*docState),
		// Hydrated runtime (builtins + prelude)
		ip: mindscript.NewRuntime(),
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
	if _, ok := err.(*mindscript.IncompleteError); ok {
		s.clearDiagnostics(doc.uri)
		return
	}
	line, col := 0, 0
	switch e := err.(type) {
	case *mindscript.ParseError:
		line, col = e.Line, e.Col
	case *mindscript.LexError:
		line, col = e.Line, e.Col
	default:
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
		_, sz := utf8.DecodeRuneInString(doc.text[start:])
		if sz <= 0 {
			sz = 1
		}
		end = start + sz
	}

	s.notify("textDocument/publishDiagnostics", PublishDiagnosticsParams{
		URI: doc.uri,
		Diagnostics: []Diagnostic{{
			Range:    makeRange(doc.lines, start, end, doc.text),
			Severity: 1,
			Source:   "mindscript",
			Message:  err.Error(),
		}},
	})
}

////////////////////////////////////////////////////////////////////////////////
// Token helpers
////////////////////////////////////////////////////////////////////////////////

func tokenName(t mindscript.Token) string {
	if s, ok := t.Literal.(string); ok {
		return s
	}
	return t.Lexeme
}

// tokenSpan returns [start,end) byte offsets of a token's lexeme in doc.text.
// Robust to occasional column off-by-ones via small local search.
func tokenSpan(doc *docState, t mindscript.Token) (start, end int) {
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
		if t.Type != mindscript.ID && t.Type != mindscript.TYPE {
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
		mindscript.TYPECONS, mindscript.ENUM,
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
}

func overlaps(a, b [2]int) bool { return a[0] < b[1] && b[0] < a[1] }

// comment/annotation spans used by semantic tokens & folding.
func commentSpans(doc *docState) [][2]int {
	text := doc.text
	spans := [][2]int{}

	// ANNOTATION tokens from the lexer
	for _, tk := range doc.tokens {
		if tk.Type == mindscript.ANNOTATION {
			s, e := tokenSpan(doc, tk)
			if e > s {
				spans = append(spans, [2]int{s, e})
			}
		}
	}

	// Build STRING spans once to avoid false-positive inline "#(" inside strings.
	stringSpans := [][2]int{}
	for _, tk := range doc.tokens {
		if tk.Type == mindscript.STRING {
			s, e := tokenSpan(doc, tk)
			if e > s {
				stringSpans = append(stringSpans, [2]int{s, e})
			}
		}
	}
	inString := func(off int) bool {
		for _, sp := range stringSpans {
			if off >= sp[0] && off < sp[1] {
				return true
			}
		}
		return false
	}

	// Lines whose first non-space is '#' or '##'
	for li := 0; li < len(doc.lines); li++ {
		lo := doc.lines[li]
		hi := len(text)
		if li+1 < len(doc.lines) {
			hi = doc.lines[li+1]
		}
		line := text[lo:hi]
		trim := strings.TrimLeft(line, " \t")
		if len(trim) == 0 {
			continue
		}
		if strings.HasPrefix(trim, "##") || strings.HasPrefix(trim, "#") {
			spans = append(spans, [2]int{lo, hi})
		}
	}

	// Inline "#(" ... ")" (best effort; no nesting)
	for start := 0; ; {
		i := strings.Index(text[start:], "#(")
		if i < 0 {
			break
		}
		i += start
		// Skip if inside a string literal
		if inString(i) {
			start = i + 2
			continue
		}
		j := strings.IndexByte(text[i+2:], ')')
		if j < 0 {
			spans = append(spans, [2]int{i, len(text)})
			break
		}
		j = i + 2 + j // inclusive ')'
		spans = append(spans, [2]int{i, j + 1})
		start = j + 1
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

func (s *server) analyze(doc *docState) {
	local := *doc
	local.tokens = nil

	// Lex first (even if parse fails) so diagnostics can highlight tokens.
	if lex := mindscript.NewLexer(local.text); lex != nil {
		if toks, err := lex.Scan(); err == nil {
			local.tokens = toks
		}
	}

	// Parse in interactive mode (REPL-friendly errors).
	ast, err := mindscript.ParseSExprInteractive(local.text)
	if err != nil {
		s.publishError(&local, err)

		// If we don't have tokens (e.g., initial lex/parse dies early), try to lex the prefix up to error position
		// so diagnostics, refs, and semTokens still have something to work with.
		if len(local.tokens) == 0 {
			line, col := 0, 0
			switch e := err.(type) {
			case *mindscript.ParseError:
				line, col = e.Line, e.Col
			case *mindscript.LexError:
				line, col = e.Line, e.Col
			case *mindscript.IncompleteError:
				line, col = e.Line, e.Col
			}
			if line > 0 {
				off := byteColToOffset(local.lines, line-1, col, local.text)
				if off > 0 && off <= len(local.text) {
					if lx := mindscript.NewLexer(local.text[:off]); lx != nil {
						if toks, lexErr := lx.Scan(); lexErr == nil {
							local.tokens = toks
						}
					}
				}
			}
		}
		// Store tokens so features still work during errors.
		s.mu.Lock()
		if live := s.docs[doc.uri]; live != nil {
			if len(local.tokens) > 0 {
				live.tokens = local.tokens
			}
			// keep existing live.symbols on error
		}
		s.mu.Unlock()
		return
	}
	s.clearDiagnostics(local.uri)

	// Build top-level symbols from the AST.
	local.symbols = nil
	appendSym := func(name, kind, docline, sig string) {
		r, ok := defRangeByTokens(&local, name)
		if !ok {
			if idx := strings.Index(local.text, name); idx >= 0 {
				r = makeRange(local.lines, idx, idx+len(name), local.text)
			}
		}
		local.symbols = append(local.symbols, symbolDef{
			Name:  name,
			Kind:  kind,
			Range: r,
			Doc:   docline,
			Sig:   sig,
		})
	}
	firstLine := func(ann string) string {
		ann = strings.TrimSpace(ann)
		if nl := strings.IndexByte(ann, '\n'); nl >= 0 {
			return strings.TrimSpace(ann[:nl])
		}
		return ann
	}

	// ast is ("block", n1, n2, ...)
	for i := 1; i < len(ast); i++ {
		n, ok := ast[i].([]any)
		if !ok || len(n) == 0 {
			continue
		}
		switch n[0] {
		case "annot":
			if len(n) < 3 {
				continue
			}
			docNode, _ := n[1].([]any)
			sub, _ := n[2].([]any)
			docStr := ""
			if len(docNode) >= 2 && docNode[0] == "str" {
				if v, ok := docNode[1].(string); ok {
					docStr = v
				}
			}
			if len(sub) == 0 {
				continue
			}
			switch sub[0] {
			case "assign":
				if len(sub) >= 3 {
					lhs, _ := sub[1].([]any)
					rhs, _ := sub[2].([]any)
					if len(lhs) >= 2 && (lhs[0] == "decl" || lhs[0] == "id") {
						if nm, ok := lhs[1].(string); ok {
							kind, sig := "let", ""
							if len(rhs) > 0 {
								switch rhs[0] {
								case "fun":
									kind = "fun"
									sig = formatFunSig(nm, rhs)
								case "oracle":
									kind = "fun"
									sig = nm + "(...) -> Any?"
								case "type":
									kind = "type"
								}
							}
							appendSym(nm, kind, firstLine(docStr), sig)
						}
					}
				}
			case "decl":
				if len(sub) >= 2 {
					if nm, ok := sub[1].(string); ok {
						appendSym(nm, "let", firstLine(docStr), "")
					}
				}
			}
		case "assign":
			if len(n) < 3 {
				continue
			}
			lhs, _ := n[1].([]any)
			rhs, _ := n[2].([]any)
			if len(lhs) >= 2 && (lhs[0] == "decl" || lhs[0] == "id") {
				if nm, ok := lhs[1].(string); ok {
					kind, sig := "let", ""
					if len(rhs) > 0 {
						switch rhs[0] {
						case "fun":
							kind = "fun"
							sig = formatFunSig(nm, rhs)
						case "oracle":
							kind = "fun"
							sig = nm + "(...) -> Any?"
						case "type":
							kind = "type"
						}
					}
					appendSym(nm, kind, "", sig)
				}
			}
		case "decl":
			if len(n) >= 2 {
				if nm, ok := n[1].(string); ok {
					appendSym(nm, "let", "", "")
				}
			}
		}
	}

	// Commit analysis back to live doc.
	s.mu.Lock()
	if live := s.docs[doc.uri]; live != nil {
		live.tokens = local.tokens
		live.symbols = local.symbols
	}
	s.mu.Unlock()
}
