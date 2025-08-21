// cmd/mindscript-lsp/main.go
//
// Minimal Language Server Protocol (LSP) server for MindScript.
// - ONE FILE, minimal deps (std lib only).
// - Implements: initialize, didOpen/didChange, hover, definition,
//   completion, documentSymbol, references (best-effort).
// - Uses the existing mindscript package for parsing, basic semantic info.
//
// NOTE: This is a pragmatic, compact server intended to be extended.
//       It focuses on correctness and keyword-safe identifiers.

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

	// Import your engine
	mindscript "github.com/DAIOS-AI/msg"
)

// --------------------------- LSP wire types (trimmed) ------------------------

type Position struct {
	Line      int `json:"line"`
	Character int `json:"character"`
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

type ServerCapabilities struct {
	TextDocumentSync   int  `json:"textDocumentSync"`
	HoverProvider      bool `json:"hoverProvider"`
	DefinitionProvider bool `json:"definitionProvider"`
	CompletionProvider *struct {
		TriggerCharacters []string `json:"triggerCharacters"`
	} `json:"completionProvider,omitempty"`
	DocumentSymbolProvider          bool `json:"documentSymbolProvider"`
	ReferencesProvider              bool `json:"referencesProvider"`
	WorkspaceSymbolProvider         bool `json:"workspaceSymbolProvider"`
	DocumentFormattingProvider      bool `json:"documentFormattingProvider"`
	DocumentRangeFormattingProvider bool `json:"documentRangeFormattingProvider"`
}

type InitializeResult struct {
	Capabilities ServerCapabilities `json:"capabilities"`
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
	Severity int    `json:"severity,omitempty"`
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

// --------------------------- Server state ------------------------------------

type symbolDef struct {
	Name  string
	Kind  string // "let" | "fun" | "type" | "param"
	Range Range  // where it's declared (exported field name avoids 'range' keyword)
	Doc   string // first line, if available
}

// Per open document.
type docState struct {
	uri     string
	text    string
	lines   []int // line start offsets
	symbols []symbolDef
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

// --------------------------- Transport (stdio, Content-Length) ---------------

func readMsg(r *bufio.Reader) ([]byte, error) {
	// Basic LSP framing: "Content-Length: <n>\r\n\r\n<body>"
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
		if strings.HasPrefix(strings.ToLower(line), "content-length:") {
			fmt.Sscanf(line, "Content-Length: %d", &contentLen)
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
	_ = writeMsg(os.Stdout, Response{
		JSONRPC: "2.0",
		ID:      id,
		Result:  result,
		Error:   respErr,
	})
}

func (s *server) notify(method string, params any) {
	_ = writeMsg(os.Stdout, map[string]any{
		"jsonrpc": "2.0",
		"method":  method,
		"params":  params,
	})
}

// --------------------------- Text utils --------------------------------------

func lineOffsets(text string) []int {
	offs := []int{0}
	for i, r := range text {
		if r == '\n' {
			offs = append(offs, i+1)
		}
	}
	return offs
}

func posToOffset(lines []int, p Position, text string) int {
	if p.Line < 0 {
		return 0
	}
	if p.Line >= len(lines) {
		return len(text)
	}
	lineStart := lines[p.Line]
	// naive: count bytes until Character (MindScript is ASCII-ish; adjust for UTF-16 LSP if needed)
	i := lineStart
	col := 0
	for i < len(text) && col < p.Character {
		if text[i] == '\n' {
			break
		}
		i++
		col++
	}
	return i
}

func offsetToPos(lines []int, off int) Position {
	// binary search lines
	i := 0
	j := len(lines)
	for i+1 < j {
		m := (i + j) / 2
		if lines[m] <= off {
			i = m
		} else {
			j = m
		}
	}
	return Position{Line: i, Character: off - lines[i]}
}

func makeRange(lines []int, start, end int) Range {
	return Range{Start: offsetToPos(lines, start), End: offsetToPos(lines, end)}
}

// --------------------------- Cheap symbol scan -------------------------------

// Extremely lightweight symbol extraction based on S-expr parser.
// We use interpreter.ParseSExpr to get the AST, then scan top-level forms
// to collect let/def/type/oracle/fun declarations.
func (s *server) analyze(doc *docState) {
	doc.symbols = nil
	ast, err := mindscript.ParseSExpr(doc.text)
	if err != nil {
		// On parse error, publish a single diagnostic
		s.notify("textDocument/publishDiagnostics", PublishDiagnosticsParams{
			URI: doc.uri,
			Diagnostics: []Diagnostic{{
				Range:    Range{Start: Position{0, 0}, End: Position{0, 1}},
				Severity: 1,
				Source:   "mindscript",
				Message:  err.Error(),
			}},
		})
		return
	}
	// Clear diagnostics on successful parse
	s.notify("textDocument/publishDiagnostics", PublishDiagnosticsParams{URI: doc.uri})

	// The AST is []any; we do a shallow walk for top-level ("assign","decl", ...).
	// Since we don't have byte offsets from the parser, we'll approximate ranges
	// via basic text search by symbol name (best-effort).
	findName := func(name string) Range {
		if name == "" {
			return Range{}
		}
		idx := strings.Index(doc.text, name)
		if idx < 0 {
			return Range{}
		}
		return makeRange(doc.lines, idx, idx+len(name))
	}

	appendSym := func(name, kind, docline string) {
		doc.symbols = append(doc.symbols, symbolDef{
			Name:  name,
			Kind:  kind,
			Range: findName(name),
			Doc:   docline,
		})
	}

	firstLine := func(ann string) string {
		ann = strings.TrimSpace(ann)
		nl := strings.IndexByte(ann, '\n')
		if nl >= 0 {
			return strings.TrimSpace(ann[:nl])
		}
		return ann
	}

	// Walk top-level forms: ast is an S = []any
	for i := 0; i < len(ast); i++ {
		n, ok := ast[i].([]any)
		if !ok || len(n) == 0 {
			continue
		}
		tag, _ := n[0].(string)
		switch tag {
		case "annot":
			// ("annot", ("str", doc), <sub>)
			if len(n) < 3 {
				continue
			}
			docNode, _ := n[1].([]any)
			sub, _ := n[2].([]any)
			docStr := ""
			if len(docNode) >= 2 {
				if docNode[0] == "str" {
					if v, ok := docNode[1].(string); ok {
						docStr = v
					}
				}
			}
			// Look through to sub node for decl/fun/oracle/type assign
			if len(sub) == 0 {
				continue
			}
			switch sub[0] {
			case "assign":
				// ("assign", <lhs>, <rhs>)
				if len(sub) >= 2 {
					lhs, _ := sub[1].([]any)
					if len(lhs) >= 2 && (lhs[0] == "decl" || lhs[0] == "id") {
						if nm, ok := lhs[1].(string); ok {
							appendSym(nm, "let", firstLine(docStr))
						}
					}
				}
			case "decl":
				if len(sub) >= 2 {
					if nm, ok := sub[1].(string); ok {
						appendSym(nm, "let", firstLine(docStr))
					}
				}
			case "fun":
				// ("fun", params, ret, body)
				// without a name unless assigned; skip
			case "oracle":
				// same as fun
			default:
				// ignore
			}
		case "assign":
			// ("assign", <lhs>, <rhs>)
			if len(n) < 3 {
				continue
			}
			lhs, _ := n[1].([]any)
			rhs, _ := n[2].([]any)
			// lhs ("decl", name) or ("id", name)
			if len(lhs) >= 2 && (lhs[0] == "decl" || lhs[0] == "id") {
				if nm, ok := lhs[1].(string); ok {
					kind := "let"
					if len(rhs) > 0 {
						switch rhs[0] {
						case "fun":
							kind = "fun"
						case "oracle":
							kind = "fun" // treat as function for symbol listing
						case "type":
							kind = "type"
						}
					}
					appendSym(nm, kind, "")
				}
			}
		case "decl":
			if len(n) >= 2 {
				if nm, ok := n[1].(string); ok {
					appendSym(nm, "let", "")
				}
			}
		default:
			// top-level fun/oracle usually anonymous; skip
		}
	}
}

// --------------------------- Handlers ----------------------------------------

func (s *server) onInitialize(id json.RawMessage, _ json.RawMessage) {
	result := InitializeResult{
		Capabilities: ServerCapabilities{
			TextDocumentSync:   2, // Incremental
			HoverProvider:      true,
			DefinitionProvider: true,
			CompletionProvider: &struct {
				TriggerCharacters []string `json:"triggerCharacters"`
			}{TriggerCharacters: []string{".", ":", "["}},
			DocumentSymbolProvider:          true,
			ReferencesProvider:              true,
			WorkspaceSymbolProvider:         false,
			DocumentFormattingProvider:      false,
			DocumentRangeFormattingProvider: false,
		},
	}
	s.sendResponse(id, result, nil)
}

func (s *server) onDidOpen(raw json.RawMessage) {
	var params struct {
		TextDocument TextDocumentItem `json:"textDocument"`
	}
	_ = json.Unmarshal(raw, &params)
	s.mu.Lock()
	defer s.mu.Unlock()

	doc := &docState{
		uri:   params.TextDocument.URI,
		text:  params.TextDocument.Text,
		lines: lineOffsets(params.TextDocument.Text),
	}
	s.docs[doc.uri] = doc
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
	defer s.mu.Unlock()

	doc := s.docs[params.TextDocument.URI]
	if doc == nil {
		return
	}
	// Minimal: take the last change's full text (most editors send full doc)
	if len(params.ContentChanges) > 0 {
		last := params.ContentChanges[len(params.ContentChanges)-1]
		if last.Range == nil {
			doc.text = last.Text
		} else {
			// apply range edit (simple byte-slice apply)
			start := posToOffset(doc.lines, last.Range.Start, doc.text)
			end := posToOffset(doc.lines, last.Range.End, doc.text)
			var b bytes.Buffer
			b.WriteString(doc.text[:start])
			b.WriteString(last.Text)
			if end < len(doc.text) {
				b.WriteString(doc.text[end:])
			}
			doc.text = b.String()
		}
		doc.lines = lineOffsets(doc.text)
		s.analyze(doc)
	}
}

func (s *server) onHover(id json.RawMessage, paramsRaw json.RawMessage) {
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

	// Heuristic: find word under cursor, check symbols, and look up docs via Core env
	word := wordAt(doc, params.Position)
	if word == "" {
		s.sendResponse(id, nil, nil)
		return
	}
	// Search our symbol table for a matching name
	for _, sym := range doc.symbols {
		if sym.Name == word {
			kind := sym.Kind
			docline := sym.Doc
			if docline == "" {
				// Try help() if it's a function in env
				if v, err := s.ip.Global.Get(word); err == nil && v.Tag == mindscript.VTFun {
					if v.Annot != "" {
						docline = v.Annot
					}
				}
			}
			content := fmt.Sprintf("**%s** `%s`\n\n%s", kind, sym.Name, strings.TrimSpace(docline))
			s.sendResponse(id, Hover{
				Contents: MarkupContent{Kind: "markdown", Value: content},
			}, nil)
			return
		}
	}
	// Fallback: if it's a known builtin/global, show a short type signature
	if v, err := s.ip.Global.Get(word); err == nil {
		switch v.Tag {
		case mindscript.VTFun:
			if meta, ok := s.ip.FunMeta(v); ok {
				ps := meta.ParamSpecs()
				names := make([]string, 0, len(ps))
				for _, p := range ps {
					names = append(names, p.Name)
				}
				content := fmt.Sprintf("**fun** `%s(%s)`", word, strings.Join(names, ", "))
				if doc := meta.Doc(); doc != "" {
					content += "\n\n" + doc
				}
				s.sendResponse(id, Hover{Contents: MarkupContent{Kind: "markdown", Value: content}}, nil)
				return
			}
		case mindscript.VTType:
			s.sendResponse(id, Hover{Contents: MarkupContent{Kind: "markdown", Value: "**type** `" + word + "`"}}, nil)
			return
		}
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

	word := wordAt(doc, params.Position)
	if word == "" {
		s.sendResponse(id, nil, nil)
		return
	}
	for _, sym := range doc.symbols {
		if sym.Name == word {
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

	items := []CompletionItem{}
	// Document symbols
	for _, sym := range doc.symbols {
		items = append(items, CompletionItem{
			Label:  sym.Name,
			Kind:   6, // Variable
			Detail: sym.Kind,
		})
	}
	// Builtins + user globals (best-effort) from Global env (which chains to Core).
	entries, order := s.listBindings(s.ip.Global)
	for _, name := range order {
		v := entries[name]
		kind := 3 // Function by default
		if v.Tag == mindscript.VTType {
			kind = 7 // Class/type-ish for Types
		} else if v.Tag == mindscript.VTFun {
			kind = 3
		} else {
			kind = 6 // Variable for non-funs/non-types (LSP CompletionItemKind = 6)
		}
		items = append(items, CompletionItem{
			Label: name,
			Kind:  kind,
		})
	}

	s.sendResponse(id, items, nil)
}

// listBindings asks the interpreter for the visible bindings in a given env.
// It uses the public builtin `getEnv`, which returns a map of name -> value.
func (s *server) listBindings(env *mindscript.Env) (map[string]mindscript.Value, []string) {
	// Build the AST for (getEnv)
	ast := mindscript.S{"call", mindscript.S{"id", "getEnv"}}

	v, err := s.ip.EvalAST(ast, env)
	if err != nil || v.Tag != mindscript.VTMap {
		return map[string]mindscript.Value{}, nil
	}

	mo := v.Data.(*mindscript.MapObject)
	// Return both the map and the keys in a stable order.
	return mo.Entries, append([]string(nil), mo.Keys...)
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
			kind = 12
		case "type":
			kind = 7
		}
		out = append(out, DocumentSymbol{
			Name:           sym.Name,
			Detail:         sym.Kind,
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
	word := wordAt(doc, params.Position)
	if word == "" {
		s.sendResponse(id, []Location{}, nil)
		return
	}
	locs := []Location{}
	// naive scan for word matches
	start := 0
	for {
		idx := strings.Index(doc.text[start:], word)
		if idx < 0 {
			break
		}
		off := start + idx
		r := makeRange(doc.lines, off, off+len(word))
		locs = append(locs, Location{URI: doc.uri, Range: r})
		start = off + len(word)
	}
	s.sendResponse(id, locs, nil)
}

// --------------------------- Helpers -----------------------------------------

func wordAt(doc *docState, pos Position) string {
	off := posToOffset(doc.lines, pos, doc.text)
	if off < 0 || off >= len(doc.text) {
		return ""
	}
	// Expand left/right over identifier charset
	isIdent := func(b byte) bool {
		return b == '_' || b == '$' ||
			(b >= 'a' && b <= 'z') ||
			(b >= 'A' && b <= 'Z') ||
			(b >= '0' && b <= '9')
	}
	i, j := off, off
	for i > 0 && isIdent(doc.text[i-1]) {
		i--
	}
	for j < len(doc.text) && isIdent(doc.text[j]) {
		j++
	}
	return strings.TrimSpace(doc.text[i:j])
}

// --------------------------- Main loop ---------------------------------------

func main() {
	s := newServer()
	in := bufio.NewReader(os.Stdin)

	for {
		msgBytes, err := readMsg(in)
		if err != nil {
			if err != io.EOF {
				// best-effort log to stderr
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
			// ignore
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

		default:
			// respond with MethodNotFound for requests that carry an id
			if len(req.ID) > 0 {
				s.sendResponse(req.ID, nil, &ResponseError{Code: -32601, Message: "method not found"})
			}
		}
	}
}
