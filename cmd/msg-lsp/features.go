// cmd/msg-lsp/features.go
//
// ROLE: LSP feature handlers. This file advertises capabilities,
// runs the analyzer on didOpen/didChange to publish diagnostics,
// and implements hover with markdown output.

package main

import (
	"encoding/json"
	"fmt"
	"sort"
	"strings"
	"unicode/utf8"

	mindscript "github.com/DAIOS-AI/msg/internal/mindscript"
)

////////////////////////////////////////////////////////////////////////////////
// Minimal in-memory state (kept local to this file)
////////////////////////////////////////////////////////////////////////////////

var (
	_lspAnalyzer Analyzer
	_fileIdx     = map[string]*FileIndex{}
	_fileText    = map[string]string{}
)

// utf16ColToByteOffset converts an LSP (line, UTF-16 column) to a UTF-8 byte offset.
// Returns -1 if the position is out of range.
func utf16ColToByteOffset(text string, line, col int) int {
	if line < 0 || col < 0 {
		return -1
	}
	// Walk to start of the requested line.
	byteStart, cur := 0, 0
	for i := 0; i < len(text) && cur < line; i++ {
		if text[i] == '\n' {
			cur++
			byteStart = i + 1
		}
	}
	if cur != line {
		return -1
	}
	// Find end of line (or end of text).
	byteEnd := len(text)
	for i := byteStart; i < len(text); i++ {
		if text[i] == '\n' {
			byteEnd = i
			break
		}
	}
	// Advance col UTF-16 code units across this line.
	u16col, i := 0, byteStart
	for i < byteEnd {
		if u16col == col {
			return i
		}
		r, size := utf8.DecodeRuneInString(text[i:byteEnd])
		if r == utf8.RuneError && size == 1 {
			// Invalid byte: treat as one UTF-16 unit and move on.
			i++
			u16col++
			continue
		}
		if r <= 0xFFFF {
			u16col += 1
		} else {
			u16col += 2
		}
		i += size
	}
	// Exactly at end of line is valid.
	if u16col == col {
		return byteEnd
	}
	return -1
}

// buildLineIndex returns byte offsets for the start of each line in text.
// Line i starts at byte offset lineIndex[i]. Always includes 0 as the first entry.
func buildLineIndex(text string) []int {
	if len(text) == 0 {
		return []int{0}
	}
	lines := []int{0}
	for i := 0; i < len(text); i++ {
		if text[i] == '\n' {
			if i+1 < len(text) {
				lines = append(lines, i+1)
			} else {
				// Trailing newline: next line starts at len(text) (valid end position).
				lines = append(lines, i+1)
			}
		}
	}
	return lines
}

// byteOffsetToLSP converts a byte offset to (line, UTF-16 column).
// Offsets are clamped to [0, len(text)].
func byteOffsetToLSP(lineIndex []int, text string, off int) (int, int) {
	if off < 0 {
		off = 0
	} else if off > len(text) {
		off = len(text)
	}
	// Find largest i such that lineIndex[i] <= off.
	i := sort.Search(len(lineIndex), func(i int) bool { return lineIndex[i] > off }) - 1
	if i < 0 {
		i = 0
	}
	start := lineIndex[i]
	u16 := 0
	for j := start; j < off; {
		r, size := utf8.DecodeRuneInString(text[j:])
		if r == utf8.RuneError && size == 1 {
			j++
			u16++
			continue
		}
		if r <= 0xFFFF {
			u16 += 1
		} else {
			u16 += 2
		}
		j += size
	}
	return i, u16
}

// docFromTypeAst extracts the outer string annotation from a type AST:
// ("annot", ["str", <doc>], inner) → <doc>, else "".
func docFromTypeAst(t mindscript.S) string {
	if len(t) >= 3 {
		if tag, _ := t[0].(string); tag == "annot" {
			if docNode, _ := t[1].(mindscript.S); len(docNode) >= 2 {
				if dtag, _ := docNode[0].(string); dtag == "str" {
					if s, ok := docNode[1].(string); ok {
						return strings.TrimSpace(s)
					}
				}
			}
		}
	}
	return ""
}

// stripOuterTypeAnnot removes a single outer ("annot", ["str", ...], inner) from a type AST,
// returning the inner type. If there is no such wrapper, it returns t unchanged.
func stripOuterTypeAnnot(t mindscript.S) mindscript.S {
	if len(t) >= 3 {
		if tag, _ := t[0].(string); tag == "annot" {
			if inner, ok := t[2].(mindscript.S); ok {
				return inner
			}
		}
	}
	return t
}

////////////////////////////////////////////////////////////////////////////////
// Initialization.
////////////////////////////////////////////////////////////////////////////////

func (s *server) onInitialize(id json.RawMessage, params json.RawMessage) {
	var p InitializeParams
	_ = json.Unmarshal(params, &p) // best-effort; we don't rely on fields here

	res := InitializeResult{
		Capabilities: ServerCapabilities{
			TextDocumentSync: TextDocumentSyncOptions{
				OpenClose: true,
				Change:    1, // Full sync
				Save: &struct {
					IncludeText bool `json:"includeText"`
				}{IncludeText: false},
			},
			HoverProvider:      true,
			DefinitionProvider: true,
			CompletionProvider: &struct {
				TriggerCharacters []string `json:"triggerCharacters"`
			}{TriggerCharacters: nil},
			DocumentSymbolProvider:          true,
			ReferencesProvider:              true,
			WorkspaceSymbolProvider:         false,
			DocumentFormattingProvider:      false,
			DocumentRangeFormattingProvider: false,
			SignatureHelpProvider: &struct {
				TriggerCharacters   []string `json:"triggerCharacters"`
				RetriggerCharacters []string `json:"retriggerCharacters"`
			}{},
			SemanticTokensProvider: &struct {
				Legend struct {
					TokenTypes     []string `json:"tokenTypes"`
					TokenModifiers []string `json:"tokenModifiers"`
				} `json:"legend"`
				Full  bool `json:"full"`
				Range bool `json:"range"`
			}{},
			FoldingRangeProvider:   false,
			TypeDefinitionProvider: false,
			RenameProvider:         nil,
		},
		ServerInfo: map[string]string{
			"name":    "msg-lsp",
			"version": "0.0.0-stub",
		},
	}

	// Seed a tiny semantic-tokens legend so clients/tests see capabilities.
	res.Capabilities.SemanticTokensProvider.Legend.TokenTypes = []string{"variable"}
	res.Capabilities.SemanticTokensProvider.Legend.TokenModifiers = []string{}
	res.Capabilities.SemanticTokensProvider.Full = true

	s.sendResponse(id, res, nil)
}

////////////////////////////////////////////////////////////////////////////////
// Notifications (no replies)
////////////////////////////////////////////////////////////////////////////////

func (s *server) onDidOpen(params json.RawMessage) {
	// Decode and analyze the full text.
	type didOpenParams struct {
		TextDocument TextDocumentItem `json:"textDocument"`
	}
	var p didOpenParams
	if err := json.Unmarshal(params, &p); err != nil {
		return
	}
	uri := p.TextDocument.URI
	text := p.TextDocument.Text

	idx := _lspAnalyzer.Analyze(uri, text)
	_fileIdx[uri] = idx
	_fileText[uri] = text

	// Build line index once to map byte spans → LSP positions.
	lines := buildLineIndex(text)

	// Map analyzer diagnostics to LSP diagnostics.
	lsps := make([]Diagnostic, 0, len(idx.Diags))
	for _, d := range idx.Diags {
		sl, sc := byteOffsetToLSP(lines, text, d.StartByte)
		el, ec := byteOffsetToLSP(lines, text, d.EndByte)
		lsps = append(lsps, Diagnostic{
			Range:    Range{Start: Position{Line: sl, Character: sc}, End: Position{Line: el, Character: ec}},
			Severity: 1,
			Code:     d.Code,
			Source:   "msg-lsp",
			Message:  d.Message,
		})
	}
	s.notify("textDocument/publishDiagnostics", PublishDiagnosticsParams{
		URI:         uri,
		Diagnostics: lsps,
	})
}

func (s *server) onDidChange(params json.RawMessage) {
	// Decode; tests send full-document replacements.
	type didChangeParams struct {
		TextDocument struct {
			URI     string `json:"uri"`
			Version int    `json:"version"`
		} `json:"textDocument"`
		ContentChanges []struct {
			Text string `json:"text"`
		} `json:"contentChanges"`
	}
	var p didChangeParams
	if err := json.Unmarshal(params, &p); err != nil {
		return
	}
	uri := p.TextDocument.URI
	if len(p.ContentChanges) == 0 {
		return
	}
	text := p.ContentChanges[len(p.ContentChanges)-1].Text

	idx := _lspAnalyzer.Analyze(uri, text)
	_fileIdx[uri] = idx
	_fileText[uri] = text

	// Build line index once to map byte spans → LSP positions.
	lines := buildLineIndex(text)

	lsps := make([]Diagnostic, 0, len(idx.Diags))
	for _, d := range idx.Diags {
		sl, sc := byteOffsetToLSP(lines, text, d.StartByte)
		el, ec := byteOffsetToLSP(lines, text, d.EndByte)
		lsps = append(lsps, Diagnostic{
			Range:    Range{Start: Position{Line: sl, Character: sc}, End: Position{Line: el, Character: ec}},
			Severity: 1,
			Code:     d.Code,
			Source:   "msg-lsp",
			Message:  d.Message,
		})
	}
	s.notify("textDocument/publishDiagnostics", PublishDiagnosticsParams{
		URI:         uri,
		Diagnostics: lsps,
	})
}

////////////////////////////////////////////////////////////////////////////////
// Requests that return simple/nullable results
////////////////////////////////////////////////////////////////////////////////

// map TokenKind → label for language tokens
func tokenKindLabel(k TokenKind) string {
	switch k {
	case TokKeyword:
		return "keyword"
	case TokOperator:
		return "operator"
	case TokPunct:
		return "punctuation"
	case TokLiteral:
		return "literal"
	case TokIdentifier:
		return "symbol"
	default:
		return "token"
	}
}

// Build hover text (markdown) for a token entry.
func formatHoverMarkdown(te *TokenEntry) string {
	// Language token (no payload → kind + lexeme)
	if te == nil || te.Payload == (mindscript.Value{}) {
		return fmt.Sprintf("```mindscript\n%s (%s)\n```", te.Text, tokenKindLabel(te.Kind))
	}

	name := te.Text
	doc := strings.TrimSpace(te.Payload.Annot)

	switch te.Payload.Tag {
	case mindscript.VTType:
		// Type values: show Type(structure) WITHOUT resolving; printer handles names.
		tv := te.Payload.Data.(*mindscript.TypeValue)
		structure := mindscript.FormatType(tv.Ast)
		var typed string
		if structure == "" {
			typed = "Type"
		} else {
			typed = "Type (type " + structure + ")"
		}
		if doc != "" {
			return fmt.Sprintf("```mindscript\n%s: %s\n```\n\n%s", name, typed, doc)
		}
		return fmt.Sprintf("```mindscript\n%s: %s\n```", name, typed)

	case mindscript.VTSymbol:
		// Symbols: print the declared type WITHOUT resolving; provide the defining env via payload only.
		if sym, ok := asSymbol(te.Payload); ok {
			// Prefer payload doc (set by the analyzer at binding sites). If missing, fall back to the type's outer annot.
			if doc == "" {
				doc = docFromTypeAst(sym.Type)
			}
			// Render the type WITHOUT its outer annot, so the doc is shown as a separate paragraph.
			typAst := stripOuterTypeAnnot(sym.Type)
			typ := mindscript.FormatType(typAst)
			if typ == "" {
				typ = "Any"
			}
			if doc != "" {
				return fmt.Sprintf("```mindscript\n%s: %s\n```\n\n%s", name, typ, doc)
			}
			return fmt.Sprintf("```mindscript\n%s: %s\n```", name, typ)
		}
	}

	// Fallback to kind + lexeme if payload is unexpected
	return fmt.Sprintf("(%s) **%s**", tokenKindLabel(te.Kind), te.Text)
}

func (s *server) onHover(id json.RawMessage, params json.RawMessage) {
	var p struct {
		TextDocument TextDocumentIdentifier `json:"textDocument"`
		Position     Position               `json:"position"`
	}
	if err := json.Unmarshal(params, &p); err != nil {
		s.sendResponse(id, nil, nil)
		return
	}
	idx := _fileIdx[p.TextDocument.URI]
	src := _fileText[p.TextDocument.URI]
	if idx == nil || src == "" {
		s.sendResponse(id, nil, nil)
		return
	}
	off := utf16ColToByteOffset(src, p.Position.Line, p.Position.Character)
	if off < 0 {
		s.sendResponse(id, nil, nil)
		return
	}
	te := idx.Tokens.Find(off)
	if te == nil {
		s.sendResponse(id, nil, nil)
		return
	}
	text := formatHoverMarkdown(te)
	s.sendResponse(id, &Hover{
		Contents: MarkupContent{Kind: "markdown", Value: text},
	}, nil)
}

func (s *server) onDefinition(id json.RawMessage, params json.RawMessage) {
	// LSP expects Location | Location[] | null → return null for now.
	s.sendResponse(id, nil, nil)
}

func (s *server) onCompletion(id json.RawMessage, params json.RawMessage) {
	// Return an empty list for now.
	var items []CompletionItem
	s.sendResponse(id, items, nil)
}

func (s *server) onDocumentSymbols(id json.RawMessage, params json.RawMessage) {
	// Empty symbol list for now.
	var syms []DocumentSymbol
	s.sendResponse(id, syms, nil)
}

func (s *server) onReferences(id json.RawMessage, params json.RawMessage) {
	// Empty reference list for now.
	var locs []Location
	s.sendResponse(id, locs, nil)
}

func (s *server) onSignatureHelp(id json.RawMessage, params json.RawMessage) {
	// Zero-value SignatureHelp for now.
	var sh SignatureHelp
	s.sendResponse(id, sh, nil)
}

func (s *server) onFoldingRange(id json.RawMessage, params json.RawMessage) {
	// No folding ranges yet.
	var folds []FoldingRange
	s.sendResponse(id, folds, nil)
}

func (s *server) onTypeDefinition(id json.RawMessage, params json.RawMessage) {
	// No type definitions yet.
	var locs []Location
	s.sendResponse(id, locs, nil)
}

func (s *server) onDocumentFormatting(id json.RawMessage, params json.RawMessage) {
	// Not implemented: reply with null.
	s.sendResponse(id, nil, nil)
}

func (s *server) onPrepareRename(id json.RawMessage, params json.RawMessage) {
	// Not supported yet → null.
	s.sendResponse(id, nil, nil)
}

func (s *server) onRename(id json.RawMessage, params json.RawMessage) {
	// Not supported yet → null.
	s.sendResponse(id, nil, nil)
}

////////////////////////////////////////////////////////////////////////////////
// Semantic tokens (return empty tokens object rather than null)
////////////////////////////////////////////////////////////////////////////////

func (s *server) onSemanticTokensFull(id json.RawMessage, params json.RawMessage) {
	// Return an empty token stream so clients can handle it gracefully.
	res := SemanticTokens{Data: []uint32{}}
	s.sendResponse(id, res, nil)
}

func (s *server) onSemanticTokensRange(id json.RawMessage, params json.RawMessage) {
	// Same as full: empty for now.
	res := SemanticTokens{Data: []uint32{}}
	s.sendResponse(id, res, nil)
}
