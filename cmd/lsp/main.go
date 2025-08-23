// cmd/lsp/main.go
//
// MindScript Language Server (single-file, stdlib-only).
//
// Highlights vs previous version:
// - Correct MindScript keywords in completion (fun, type, Enum, built-ins).
// - Hover classifies TYPE tokens as types (not generic keywords).
// - References exclude property IDs (obj.<name>).
// - Diagnostics position fix: engine's byte-column → precise LSP range.
// - Semantic tokens include annotations/comments as "comment" tokens;
//   still exclude coloring inside those regions.
// - Folding ranges for blocks (if/elif/else/…/end, do/…/end, while/for/…/end) and
//   for {…}, […], (…); also for consecutive '#' annotation lines.
// - Signature help on call sites with active parameter inference.
// - semanticTokens/range handler in addition to /full.
// - Applies ALL incremental edits in order (not only last).
// - Initialize advertises structured TextDocumentSync options + serverInfo.
// - Analyze no longer runs under the write lock.
//
// Notes:
// - We still avoid executing user code on completion. Hover for globals
//   reads metadata if available via interpreter (safe).
// - Definition range heuristic is expanded to include `let x` and
//   `x = ...` across the next line, but not destructuring/params yet.

package main

import (
	"bufio"
	"bytes"
	"encoding/json"
	"fmt"
	"io"
	"os"
	"sort"
	"strings"
	"sync"
	"unicode/utf8"

	mindscript "github.com/DAIOS-AI/msg"
)

// --------------------------- LSP wire types (trimmed) ------------------------

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
	// 1 = Full, 2 = Incremental (LSP enum)
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
	Kind  string `json:"kind"`  // "plaintext" | "markdown"
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

// Folding ranges
type FoldingRange struct {
	StartLine      int     `json:"startLine"`
	StartCharacter *int    `json:"startCharacter,omitempty"`
	EndLine        int     `json:"endLine"`
	EndCharacter   *int    `json:"endCharacter,omitempty"`
	Kind           *string `json:"kind,omitempty"` // "region", "comment"
}

// Signature help
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

// --------------------------- Server state ------------------------------------

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
		ip:   mindscript.NewInterpreter(),
	}
}

func (s *server) snapshotDoc(uri string) *docState {
	s.mu.RLock()
	defer s.mu.RUnlock()
	d := s.docs[uri]
	if d == nil {
		return nil
	}
	cp := *d
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

// --------------------------- Transport (stdio, Content-Length) ---------------

var stdoutSink io.Writer = os.Stdout

func init() {
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

// --------------------------- UTF-16 aware text utils -------------------------

func lineOffsets(text string) []int {
	offs := []int{0}
	for i, r := range text {
		if r == '\n' {
			offs = append(offs, i+1)
		}
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
	need := p.Character
	for i < len(text) && need > 0 {
		r, sz := utf8.DecodeRuneInString(text[i:])
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

// --------------------------- Diagnostics helpers -----------------------------

func (s *server) clearDiagnostics(uri string) {
	s.notify("textDocument/publishDiagnostics", PublishDiagnosticsParams{
		URI:         uri,
		Diagnostics: []Diagnostic{},
	})
}

func (s *server) publishError(doc *docState, err error) {
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

// --------------------------- Token helpers -----------------------------------

func tokenName(t mindscript.Token) string {
	if s, ok := t.Literal.(string); ok {
		return s
	}
	return t.Lexeme
}

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

// findSymbol looks up a doc symbol by exact name.
func findSymbol(doc *docState, name string) (symbolDef, bool) {
	for _, s := range doc.symbols {
		if s.Name == name {
			return s, true
		}
	}
	return symbolDef{}, false
}

// --------------------------- Definition heuristics ---------------------------

func defRangeByTokens(doc *docState, name string) (Range, bool) {
	toks := doc.tokens
	for i := 0; i < len(toks); i++ {
		t := toks[i]
		if t.Type != mindscript.ID || tokenName(t) != name {
			continue
		}
		// Case A: let <name> ...
		if i >= 1 && toks[i-1].Type == mindscript.LET && toks[i-1].Line == t.Line {
			s, e := tokenSpan(doc, t)
			return makeRange(doc.lines, s, e, doc.text), true
		}
		if i >= 2 && toks[i-2].Type == mindscript.LET && toks[i-2].Line == t.Line {
			s, e := tokenSpan(doc, t)
			return makeRange(doc.lines, s, e, doc.text), true
		}

		// Case B: <name> = ... on same line
		found := false
		line := t.Line
		for j := i + 1; j < len(toks) && toks[j].Line == line; j++ {
			if toks[j].Type == mindscript.ASSIGN {
				found = true
				break
			}
		}
		// Case C: <name> \n = ... (spill to next line)
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

// --------------------------- Formatting helpers ------------------------------

func formatFunSig(name string, fun []any) string {
	if len(fun) < 3 {
		return name + "() -> Any"
	}
	ps, _ := fun[1].([]any)
	var parts []string
	if len(ps) > 0 && ps[0] == "array" {
		for i := 1; i < len(ps); i++ {
			p, _ := ps[i].([]any)
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

// --------------------------- Analysis ----------------------------------------

func (s *server) analyze(doc *docState) {
	local := *doc
	local.tokens = nil
	if lex := mindscript.NewLexer(local.text); lex != nil {
		if toks, err := lex.Scan(); err == nil {
			local.tokens = toks
		}
	}

	ast, err := mindscript.ParseSExprInteractive(local.text)
	if err != nil {
		s.publishError(&local, err)
		s.mu.Lock()
		if live := s.docs[doc.uri]; live != nil {
			live.tokens = local.tokens
		}
		s.mu.Unlock()
		return
	}
	s.clearDiagnostics(local.uri)

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

	s.mu.Lock()
	if live := s.docs[doc.uri]; live != nil {
		live.tokens = local.tokens
		live.symbols = local.symbols
	}
	s.mu.Unlock()
}

// --------------------------- Handlers ----------------------------------------

func (s *server) onInitialize(id json.RawMessage, _ json.RawMessage) {
	semProv := &struct {
		Legend struct {
			TokenTypes     []string `json:"tokenTypes"`
			TokenModifiers []string `json:"tokenModifiers"`
		} `json:"legend"`
		Full  bool `json:"full"`
		Range bool `json:"range"`
	}{Full: true, Range: true}
	semProv.Legend.TokenTypes = []string{
		"keyword", "function", "type", "variable", "property", "string", "number", "comment",
	}
	semProv.Legend.TokenModifiers = []string{}

	result := InitializeResult{
		Capabilities: ServerCapabilities{
			TextDocumentSync: TextDocumentSyncOptions{
				OpenClose: true,
				Change:    2, // Incremental
			},
			HoverProvider:      true,
			DefinitionProvider: true,
			CompletionProvider: &struct {
				TriggerCharacters []string `json:"triggerCharacters"`
			}{TriggerCharacters: []string{".", ":", "[", "(", ","}},
			DocumentSymbolProvider:          true,
			ReferencesProvider:              true,
			WorkspaceSymbolProvider:         false,
			DocumentFormattingProvider:      false,
			DocumentRangeFormattingProvider: false,
			SignatureHelpProvider: &struct {
				TriggerCharacters   []string `json:"triggerCharacters"`
				RetriggerCharacters []string `json:"retriggerCharacters"`
			}{
				TriggerCharacters:   []string{"(", ","},
				RetriggerCharacters: []string{","},
			},
			SemanticTokensProvider: semProv,
			FoldingRangeProvider:   true,
		},
		ServerInfo: map[string]string{"name": "mindscript-lsp", "version": "0.2.1"},
	}
	s.sendResponse(id, result, nil)
}

func (s *server) onDidOpen(raw json.RawMessage) {
	var params struct {
		TextDocument TextDocumentItem `json:"textDocument"`
	}
	_ = json.Unmarshal(raw, &params)

	doc := &docState{
		uri:   params.TextDocument.URI,
		text:  params.TextDocument.Text,
		lines: lineOffsets(params.TextDocument.Text),
	}

	s.mu.Lock()
	s.docs[doc.uri] = doc
	s.mu.Unlock()

	s.analyze(doc)
}

func (s *server) onDidChange(raw json.RawMessage) {
	var params struct {
		TextDocument struct {
			URI string `json:"uri"`
		} `json:"textDocument"`
		ContentChanges []TextDocumentContentChangeEvent `json:"contentChanges"`
	}
	_ = json.Unmarshal(raw, &params)

	s.mu.Lock()
	doc := s.docs[params.TextDocument.URI]
	s.mu.Unlock()
	if doc == nil {
		return
	}
	if len(params.ContentChanges) == 0 {
		return
	}

	text := doc.text
	lines := doc.lines
	for _, ch := range params.ContentChanges {
		if ch.Range == nil {
			text = ch.Text
			lines = lineOffsets(text)
			continue
		}
		start := posToOffset(lines, ch.Range.Start, text)
		end := posToOffset(lines, ch.Range.End, text)
		var b bytes.Buffer
		if start > 0 {
			b.WriteString(text[:start])
		}
		b.WriteString(ch.Text)
		if end < len(text) {
			b.WriteString(text[end:])
		}
		text = b.String()
		lines = lineOffsets(text)
	}

	s.mu.Lock()
	doc.text = text
	doc.lines = lines
	s.mu.Unlock()

	s.analyze(doc)
}

func (s *server) onHover(id json.RawMessage, paramsRaw json.RawMessage) {
	var params struct {
		TextDocument TextDocumentIdentifier `json:"textDocument"`
		Position     Position               `json:"position"`
	}
	_ = json.Unmarshal(paramsRaw, &params)
	doc := s.snapshotDoc(params.TextDocument.URI)
	if doc == nil {
		s.sendResponse(id, nil, nil)
		return
	}

	name, rng := wordAt(doc, params.Position)
	if name == "" {
		s.sendResponse(id, nil, nil)
		return
	}

	off := posToOffset(doc.lines, params.Position, doc.text)
	_, tk, _, _, tokOK := tokenAtOffset(doc, off)

	// TYPE token → built-in type hover
	if tokOK && tk.Type == mindscript.TYPE {
		if docTxt, ok := builtinTypeDocs[name]; ok {
			content := fmt.Sprintf("**type** `%s`\n\n%s", name, docTxt)
			s.sendResponse(id, Hover{Contents: MarkupContent{Kind: "markdown", Value: content}, Range: &rng}, nil)
			return
		}
	}

	// Keywords / literals hover
	if tokOK {
		if isKeywordButNotType(tk.Type) || tk.Type == mindscript.BOOLEAN {
			word := tk.Lexeme
			if tk.Type == mindscript.BOOLEAN {
				if b, ok := tk.Literal.(bool); ok && b {
					word = "true"
				} else {
					word = "false"
				}
			}
			content := fmt.Sprintf("**keyword** `%s`", word)
			s.sendResponse(id, Hover{Contents: MarkupContent{Kind: "markdown", Value: content}, Range: &rng}, nil)
			return
		}
		if tk.Type == mindscript.STRING {
			content := fmt.Sprintf("**string** %q", tk.Literal)
			s.sendResponse(id, Hover{Contents: MarkupContent{Kind: "markdown", Value: content}, Range: &rng}, nil)
			return
		}
		if tk.Type == mindscript.INTEGER || tk.Type == mindscript.NUMBER {
			content := fmt.Sprintf("**number** `%s`", tk.Lexeme)
			s.sendResponse(id, Hover{Contents: MarkupContent{Kind: "markdown", Value: content}, Range: &rng}, nil)
			return
		}
	}

	// Local symbols
	for _, sym := range doc.symbols {
		if sym.Name == name {
			var header string
			switch sym.Kind {
			case "fun":
				if sym.Sig != "" {
					header = fmt.Sprintf("**fun** `%s`", sym.Sig)
				} else {
					header = fmt.Sprintf("**fun** `%s`", sym.Name)
				}
			case "type":
				header = fmt.Sprintf("**type** `%s`", sym.Name)
			default:
				header = fmt.Sprintf("**let** `%s`", sym.Name)
			}
			content := header
			if txt := strings.TrimSpace(sym.Doc); txt != "" {
				content += "\n\n" + txt
			}
			s.sendResponse(id, Hover{Contents: MarkupContent{Kind: "markdown", Value: content}, Range: &rng}, nil)
			return
		}
	}

	// Builtins by name
	if tokOK && (tk.Type == mindscript.ID || tk.Type == mindscript.TYPE) {
		if docTxt, ok := builtinTypeDocs[name]; ok {
			content := fmt.Sprintf("**type** `%s`\n\n%s", name, docTxt)
			s.sendResponse(id, Hover{Contents: MarkupContent{Kind: "markdown", Value: content}, Range: &rng}, nil)
			return
		}
	}

	// Globals via interpreter
	if v, err := s.ip.Global.Get(name); err == nil {
		switch v.Tag {
		case mindscript.VTFun:
			if meta, ok := s.ip.FunMeta(v); ok {
				ps := meta.ParamSpecs()
				parts := make([]string, 0, len(ps))
				for _, p := range ps {
					parts = append(parts, fmt.Sprintf("%s: %s", p.Name, mindscript.FormatType(p.Type)))
				}
				ret := mindscript.FormatType(meta.ReturnType())
				content := fmt.Sprintf("**fun** `%s(%s) -> %s`", name, strings.Join(parts, ", "), ret)
				if doc := meta.Doc(); doc != "" {
					content += "\n\n" + strings.TrimSpace(doc)
				}
				s.sendResponse(id, Hover{Contents: MarkupContent{Kind: "markdown", Value: content}, Range: &rng}, nil)
				return
			}
		case mindscript.VTType:
			content := "**type** `" + name + "`"
			s.sendResponse(id, Hover{Contents: MarkupContent{Kind: "markdown", Value: content}, Range: &rng}, nil)
			return
		}
	}

	// Fallback classification
	if tokOK && tk.Type == mindscript.ID {
		kind := "identifier"
		idx := indexOfToken(doc.tokens, tk)
		if idx >= 1 && doc.tokens[idx-1].Type == mindscript.PERIOD {
			kind = "property"
		} else if sy, ok := findSymbol(doc, name); ok && sy.Kind != "" {
			kind = sy.Kind
		} else if idx+1 < len(doc.tokens) && doc.tokens[idx+1].Type == mindscript.CLROUND {
			kind = "fun"
		} else if v, err := s.ip.Global.Get(name); err == nil && v.Tag == mindscript.VTType {
			kind = "type"
		}
		content := fmt.Sprintf("**%s** `%s`", kind, name)
		s.sendResponse(id, Hover{Contents: MarkupContent{Kind: "markdown", Value: content}, Range: &rng}, nil)
		return
	}
	s.sendResponse(id, nil, nil)
}

func (s *server) onDefinition(id json.RawMessage, paramsRaw json.RawMessage) {
	var params struct {
		TextDocument TextDocumentIdentifier `json:"textDocument"`
		Position     Position               `json:"position"`
	}
	_ = json.Unmarshal(paramsRaw, &params)
	s.mu.RLock()
	doc := s.docs[params.TextDocument.URI]
	s.mu.RUnlock()
	if doc == nil {
		s.sendResponse(id, nil, nil)
		return
	}
	name, _ := wordAt(doc, params.Position)
	if name == "" {
		s.sendResponse(id, nil, nil)
		return
	}
	for _, sym := range doc.symbols {
		if sym.Name == name {
			s.sendResponse(id, Location{URI: doc.uri, Range: sym.Range}, nil)
			return
		}
	}
	s.sendResponse(id, nil, nil)
}

func (s *server) onCompletion(id json.RawMessage, paramsRaw json.RawMessage) {
	var params struct {
		TextDocument TextDocumentIdentifier `json:"textDocument"`
		Position     Position               `json:"position"`
	}
	_ = json.Unmarshal(paramsRaw, &params)

	s.mu.RLock()
	doc := s.docs[params.TextDocument.URI]
	s.mu.RUnlock()
	if doc == nil {
		s.sendResponse(id, []CompletionItem{}, nil)
		return
	}

	seen := map[string]bool{}
	items := make([]CompletionItem, 0, 128)

	for _, sym := range doc.symbols {
		if seen[sym.Name] {
			continue
		}
		seen[sym.Name] = true
		kind := 6 // Variable
		if sym.Kind == "fun" {
			kind = 3 // Function
		} else if sym.Kind == "type" {
			kind = 5 // Class-ish
		}
		detail := sym.Kind
		if sym.Kind == "fun" && sym.Sig != "" {
			detail = sym.Sig
		}
		items = append(items, CompletionItem{Label: sym.Name, Kind: kind, Detail: detail})
	}

	kw := []string{
		"and", "or", "not",
		"let", "do", "end", "return", "break", "continue",
		"if", "then", "elif", "else",
		"fun", "oracle",
		"for", "in", "from", "while",
		"type", "Enum",
		"null", "true", "false",
		"Any", "Null", "Bool", "Int", "Num", "Str", "Type",
	}
	for _, w := range kw {
		if !seen[w] {
			seen[w] = true
			kind := 14 // Keyword
			if _, isType := builtinTypeDocs[w]; isType {
				kind = 5 // Class-ish for types
			}
			items = append(items, CompletionItem{Label: w, Kind: kind})
		}
	}

	sort.Slice(items, func(i, j int) bool { return items[i].Label < items[j].Label })
	s.sendResponse(id, items, nil)
}

func (s *server) onSemanticTokensFull(id json.RawMessage, paramsRaw json.RawMessage) {
	var params SemanticTokensParams
	_ = json.Unmarshal(paramsRaw, &params)
	doc := s.snapshotDoc(params.TextDocument.URI)
	out := SemanticTokens{Data: s.semanticTokensData(doc, nil)}
	s.sendResponse(id, out, nil)
}

func (s *server) onSemanticTokensRange(id json.RawMessage, paramsRaw json.RawMessage) {
	var params SemanticTokensRangeParams
	_ = json.Unmarshal(paramsRaw, &params)
	doc := s.snapshotDoc(params.TextDocument.URI)
	if doc == nil {
		s.sendResponse(id, SemanticTokens{Data: nil}, nil)
		return
	}
	lo := posToOffset(doc.lines, params.Range.Start, doc.text)
	hi := posToOffset(doc.lines, params.Range.End, doc.text)
	out := SemanticTokens{Data: s.semanticTokensData(doc, &[2]int{lo, hi})}
	s.sendResponse(id, out, nil)
}

func (s *server) onDocumentSymbols(id json.RawMessage, paramsRaw json.RawMessage) {
	var params struct {
		TextDocument TextDocumentIdentifier `json:"textDocument"`
	}
	_ = json.Unmarshal(paramsRaw, &params)

	s.mu.RLock()
	doc := s.docs[params.TextDocument.URI]
	s.mu.RUnlock()
	if doc == nil {
		s.sendResponse(id, []DocumentSymbol{}, nil)
		return
	}

	out := make([]DocumentSymbol, 0, len(doc.symbols))
	for _, sym := range doc.symbols {
		kind := 13 // Variable
		switch sym.Kind {
		case "fun":
			kind = 12 // Function
		case "type":
			kind = 5 // Class-ish
		}
		detail := sym.Kind
		if sym.Kind == "fun" && sym.Sig != "" {
			detail = sym.Sig
		}
		out = append(out, DocumentSymbol{
			Name:           sym.Name,
			Detail:         detail,
			Kind:           kind,
			Range:          sym.Range,
			SelectionRange: sym.Range,
		})
	}
	s.sendResponse(id, out, nil)
}

func (s *server) onReferences(id json.RawMessage, paramsRaw json.RawMessage) {
	var params struct {
		TextDocument TextDocumentIdentifier `json:"textDocument"`
		Position     Position               `json:"position"`
		Context      struct {
			IncludeDeclaration bool `json:"includeDeclaration"`
		} `json:"context"`
	}
	_ = json.Unmarshal(paramsRaw, &params)
	s.mu.RLock()
	doc := s.docs[params.TextDocument.URI]
	s.mu.RUnlock()
	if doc == nil {
		s.sendResponse(id, []Location{}, nil)
		return
	}
	name, _ := wordAt(doc, params.Position)
	if name == "" {
		s.sendResponse(id, []Location{}, nil)
		return
	}
	locs := []Location{}
	for i, t := range doc.tokens {
		if t.Type == mindscript.ID && tokenName(t) == name {
			// Skip properties: previous token is '.'
			if i >= 1 && doc.tokens[i-1].Type == mindscript.PERIOD {
				continue
			}
			start, end := tokenSpan(doc, t)
			locs = append(locs, Location{URI: doc.uri, Range: makeRange(doc.lines, start, end, doc.text)})
		}
	}
	s.sendResponse(id, locs, nil)
}

func (s *server) onSignatureHelp(id json.RawMessage, paramsRaw json.RawMessage) {
	var params SignatureHelpParams
	_ = json.Unmarshal(paramsRaw, &params)
	doc := s.snapshotDoc(params.TextDocument.URI)
	if doc == nil || len(doc.tokens) == 0 {
		s.sendResponse(id, SignatureHelp{}, nil)
		return
	}
	cursor := posToOffset(doc.lines, params.Position, doc.text)

	// Find enclosing "(" from left to right; ensure it's CLROUND.
	openIdx := -1
	depth := 0
	for i, t := range doc.tokens {
		_, eOff := tokenSpan(doc, t) // only need eOff
		if eOff > cursor {
			break
		}
		switch t.Type {
		case mindscript.CLROUND:
			if depth == 0 {
				openIdx = i
			}
			depth++
		case mindscript.RROUND:
			if depth > 0 {
				depth--
				if depth == 0 {
					openIdx = -1
				}
			}
		}
	}
	if openIdx == -1 {
		s.sendResponse(id, SignatureHelp{}, nil)
		return
	}
	// Callee is previous token (best-effort).
	calleeName := ""
	if openIdx-1 >= 0 && doc.tokens[openIdx-1].Type == mindscript.ID {
		calleeName = tokenName(doc.tokens[openIdx-1])
	}
	// Count commas between "(" and cursor, ignoring nested ()[]{}.
	activeParam := 0
	nest := 0
	for i := openIdx + 1; i < len(doc.tokens); i++ {
		t := doc.tokens[i]
		sOff, _ := tokenSpan(doc, t) // only need sOff
		if sOff >= cursor {
			break
		}
		switch t.Type {
		case mindscript.CLROUND, mindscript.LSQUARE, mindscript.CLSQUARE, mindscript.LCURLY:
			nest++
		case mindscript.RROUND, mindscript.RSQUARE, mindscript.RCURLY:
			if nest > 0 {
				nest--
			}
		case mindscript.COMMA:
			if nest == 0 {
				activeParam++
			}
		}
	}

	// Build signature from local symbols or globals
	var sigs []SignatureInformation
	if calleeName != "" {
		if sym, ok := findSymbol(doc, calleeName); ok && sym.Kind == "fun" {
			label := sym.Sig
			if label == "" {
				label = calleeName + "(...)"
			}
			sigs = append(sigs, SignatureInformation{Label: label})
		} else if v, err := s.ip.Global.Get(calleeName); err == nil && v.Tag == mindscript.VTFun {
			if meta, ok := s.ip.FunMeta(v); ok {
				ps := meta.ParamSpecs()
				parts := make([]string, 0, len(ps))
				paramsInfo := make([]ParameterInformation, 0, len(ps))
				for _, p := range ps {
					parts = append(parts, fmt.Sprintf("%s: %s", p.Name, mindscript.FormatType(p.Type)))
					paramsInfo = append(paramsInfo, ParameterInformation{Label: p.Name + ": " + mindscript.FormatType(p.Type)})
				}
				ret := mindscript.FormatType(meta.ReturnType())
				label := fmt.Sprintf("%s(%s) -> %s", calleeName, strings.Join(parts, ", "), ret)
				sigs = append(sigs, SignatureInformation{Label: label, Parameters: paramsInfo})
			}
		}
	}
	if len(sigs) == 0 {
		s.sendResponse(id, SignatureHelp{}, nil)
		return
	}
	s.sendResponse(id, SignatureHelp{
		Signatures:      sigs,
		ActiveSignature: 0,
		ActiveParameter: activeParam,
	}, nil)
}

// --------------------------- Helpers -----------------------------------------

func indexOfToken(toks []mindscript.Token, tk mindscript.Token) int {
	for i, t := range toks {
		if t == tk {
			return i
		}
	}
	return -1
}

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

// --------------------------- Semantic Tokens ---------------------------------

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

func commentSpans(doc *docState) [][2]int {
	text := doc.text
	spans := [][2]int{}

	for _, tk := range doc.tokens {
		if tk.Type == mindscript.ANNOTATION {
			s, e := tokenSpan(doc, tk)
			if e > s {
				spans = append(spans, [2]int{s, e})
			}
		}
	}

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
	for start := 0; ; {
		i := strings.Index(text[start:], "#(")
		if i < 0 {
			break
		}
		i += start
		j := strings.IndexByte(text[i+2:], ')')
		if j < 0 {
			spans = append(spans, [2]int{i, len(text)})
			break
		}
		j = i + 2 + j
		spans = append(spans, [2]int{i, j + 1})
		start = j + 1
	}
	return spans
}

type semEntry struct {
	line, ch, lenU16, typ int
}

func (s *server) semanticTokensData(doc *docState, filter *[2]int) []uint32 {
	if doc == nil || len(doc.tokens) == 0 {
		return nil
	}

	cspans := commentSpans(doc)
	isInComment := func(sOff, eOff int) bool {
		se := [2]int{sOff, eOff}
		for _, c := range cspans {
			if overlaps(se, c) {
				return true
			}
		}
		return false
	}

	entries := []semEntry{}

	for _, c := range cspans {
		if filter != nil && (c[1] <= (*filter)[0] || c[0] >= (*filter)[1]) {
			continue
		}
		startPos := offsetToPos(doc.lines, c[0], doc.text)
		entries = append(entries, semEntry{
			line:   startPos.Line,
			ch:     startPos.Character,
			lenU16: u16Len(doc.text[c[0]:c[1]]),
			typ:    semTypes["comment"],
		})
	}

	for i := 0; i < len(doc.tokens); i++ {
		tk := doc.tokens[i]
		if tk.Type == mindscript.ANNOTATION {
			continue
		}
		sOff, eOff := tokenSpan(doc, tk)
		if eOff <= sOff {
			continue
		}
		if isInComment(sOff, eOff) {
			continue
		}
		if filter != nil && (eOff <= (*filter)[0] || sOff >= (*filter)[1]) {
			continue
		}

		typIdx := -1
		switch {
		case isKeywordButNotType(tk.Type) || tk.Type == mindscript.BOOLEAN:
			typIdx = semTypes["keyword"]
		case tk.Type == mindscript.TYPE:
			typIdx = semTypes["type"]
		case tk.Type == mindscript.STRING:
			typIdx = semTypes["string"]
		case tk.Type == mindscript.INTEGER || tk.Type == mindscript.NUMBER:
			typIdx = semTypes["number"]
		case tk.Type == mindscript.ID:
			name := tokenName(tk)
			idx := indexOfToken(doc.tokens, tk)
			if idx >= 1 && doc.tokens[idx-1].Type == mindscript.PERIOD {
				typIdx = semTypes["property"]
			} else {
				kind := ""
				for _, sy := range doc.symbols {
					if sy.Name == name {
						kind = sy.Kind
						break
					}
				}
				if kind == "fun" || (idx+1 < len(doc.tokens) && doc.tokens[idx+1].Type == mindscript.CLROUND) {
					typIdx = semTypes["function"]
				} else if kind == "type" {
					typIdx = semTypes["type"]
				} else {
					typIdx = semTypes["variable"]
				}
			}
		default:
			continue
		}

		start := offsetToPos(doc.lines, sOff, doc.text)
		entries = append(entries, semEntry{
			line:   start.Line,
			ch:     start.Character,
			lenU16: u16Len(doc.text[sOff:eOff]),
			typ:    typIdx,
		})
	}

	sort.Slice(entries, func(i, j int) bool {
		if entries[i].line != entries[j].line {
			return entries[i].line < entries[j].line
		}
		return entries[i].ch < entries[j].ch
	})
	data := make([]uint32, 0, len(entries)*5)
	prevLine, prevCh := 0, 0
	first := true
	for _, e := range entries {
		dl, dc := e.line, e.ch
		if !first {
			dl -= prevLine
			if dl == 0 {
				dc -= prevCh
			}
		}
		first = false
		prevLine, prevCh = e.line, e.ch
		data = append(data, uint32(dl), uint32(dc), uint32(e.lenU16), uint32(e.typ), 0)
	}
	return data
}

// --------------------------- Folding ranges ----------------------------------

func (s *server) onFoldingRange(id json.RawMessage, paramsRaw json.RawMessage) {
	var params struct {
		TextDocument TextDocumentIdentifier `json:"textDocument"`
	}
	_ = json.Unmarshal(paramsRaw, &params)
	doc := s.snapshotDoc(params.TextDocument.URI)
	if doc == nil {
		s.sendResponse(id, []FoldingRange{}, nil)
		return
	}
	out := computeFolding(doc)
	s.sendResponse(id, out, nil)
}

func computeFolding(doc *docState) []FoldingRange {
	var out []FoldingRange

	emit := func(sOff, eOff int, kind *string) {
		if eOff <= sOff {
			return
		}
		start := offsetToPos(doc.lines, sOff, doc.text)
		end := offsetToPos(doc.lines, eOff, doc.text)
		if end.Line <= start.Line {
			return
		}
		out = append(out, FoldingRange{StartLine: start.Line, EndLine: end.Line, Kind: kind})
	}

	// Bracket-based folding
	var stk []mindscript.Token
	push := func(t mindscript.Token) { stk = append(stk, t) }
	popMatches := func(close mindscript.Token) *mindscript.Token {
		if len(stk) == 0 {
			return nil
		}
		open := stk[len(stk)-1]
		match := (open.Type == mindscript.LCURLY && close.Type == mindscript.RCURLY) ||
			(open.Type == mindscript.LSQUARE && close.Type == mindscript.RSQUARE) ||
			((open.Type == mindscript.LROUND || open.Type == mindscript.CLROUND) && close.Type == mindscript.RROUND)
		if !match {
			return nil
		}
		stk = stk[:len(stk)-1]
		return &open
	}
	for _, t := range doc.tokens {
		switch t.Type {
		case mindscript.LCURLY, mindscript.LSQUARE, mindscript.LROUND, mindscript.CLROUND:
			push(t)
		case mindscript.RCURLY, mindscript.RSQUARE, mindscript.RROUND:
			if o := popMatches(t); o != nil {
				s1, _ := tokenSpan(doc, *o)
				_, e2 := tokenSpan(doc, t)
				emit(s1, e2, nil)
			}
		}
	}

	// Keyword blocks
	type opener struct {
		t    mindscript.Token
		kind *string
	}
	var kstk []opener
	kindRegion := func(s string) *string { return &s }
	for _, tk := range doc.tokens {
		switch tk.Type {
		case mindscript.IF, mindscript.DO, mindscript.WHILE, mindscript.FOR:
			k := "region"
			kstk = append(kstk, opener{t: tk, kind: &k})
		case mindscript.ELIF, mindscript.ELSE:
			if len(kstk) > 0 {
				o := kstk[len(kstk)-1]
				s1, _ := tokenSpan(doc, o.t)
				_, e1 := tokenSpan(doc, tk)
				emit(s1, e1, o.kind)
				kstk[len(kstk)-1] = opener{t: tk, kind: o.kind}
			}
		case mindscript.END:
			if len(kstk) > 0 {
				o := kstk[len(kstk)-1]
				kstk = kstk[:len(kstk)-1]
				s1, _ := tokenSpan(doc, o.t)
				_, e1 := tokenSpan(doc, tk)
				emit(s1, e1, o.kind)
			}
		}
	}

	// Annotation line blocks
	comment := "comment"
	lines := doc.lines
	i := 0
	for i < len(lines) {
		lo := lines[i]
		hi := len(doc.text)
		if i+1 < len(lines) {
			hi = lines[i+1]
		}
		line := doc.text[lo:hi]
		trim := strings.TrimLeft(line, " \t")
		if strings.HasPrefix(trim, "#") {
			j := i + 1
			for j < len(lines) {
				l2Start := lines[j]
				l2End := len(doc.text)
				if j+1 < len(lines) {
					l2End = lines[j+1]
				}
				l2 := doc.text[l2Start:l2End]
				trim2 := strings.TrimLeft(l2, " \t")
				if !strings.HasPrefix(trim2, "#") {
					break
				}
				j++
			}
			if j-1 > i {
				emit(lines[i], lineEndOffset(doc, j-1), kindRegion(comment))
			}
			i = j
		} else {
			i++
		}
	}

	return out
}

func lineEndOffset(doc *docState, li int) int {
	if li+1 < len(doc.lines) {
		return doc.lines[li+1]
	}
	return len(doc.text)
}

// --------------------------- Builtin type docs -------------------------------

var builtinTypeDocs = map[string]string{
	"Any":  "Top type; any value.",
	"Null": "Null value (absence).",
	"Bool": "Boolean type (true/false).",
	"Int":  "64-bit signed integer.",
	"Num":  "64-bit IEEE-754 float.",
	"Str":  "Unicode string.",
	"Type": "Type descriptor value.",
}

// --------------------------- Main loop ---------------------------------------

func main() {
	s := newServer()
	in := bufio.NewReader(os.Stdin)

	for {
		msgBytes, err := readMsg(in)
		if err != nil {
			if err != io.EOF {
				fmt.Fprintln(os.Stderr, "read error:", err)
			}
			return
		}
		var req Request
		if err := json.Unmarshal(msgBytes, &req); err != nil {
			continue
		}

		switch req.Method {
		case "initialize":
			s.onInitialize(req.ID, req.Params)
		case "initialized":
		case "shutdown":
			s.sendResponse(req.ID, nil, nil)
		case "exit":
			return

		case "textDocument/didOpen":
			s.onDidOpen(req.Params)
		case "textDocument/didChange":
			s.onDidChange(req.Params)
		case "textDocument/hover":
			s.onHover(req.ID, req.Params)
		case "textDocument/definition":
			s.onDefinition(req.ID, req.Params)
		case "textDocument/completion":
			s.onCompletion(req.ID, req.Params)
		case "textDocument/documentSymbol":
			s.onDocumentSymbols(req.ID, req.Params)
		case "textDocument/references":
			s.onReferences(req.ID, req.Params)
		case "textDocument/semanticTokens/full":
			s.onSemanticTokensFull(req.ID, req.Params)
		case "textDocument/semanticTokens/range":
			s.onSemanticTokensRange(req.ID, req.Params)
		case "textDocument/signatureHelp":
			s.onSignatureHelp(req.ID, req.Params)
		case "textDocument/foldingRange":
			s.onFoldingRange(req.ID, req.Params)
		default:
			if len(req.ID) > 0 {
				s.sendResponse(req.ID, nil, &ResponseError{Code: -32601, Message: "method not found"})
			}
		}
	}
}
