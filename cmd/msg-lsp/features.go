// cmd/lsp/features.go
//
// ROLE: LSP feature implementations built on top of the caches/utilities from
//       core.go. Converts editor requests into language answers.
//
// What lives here
//   • Handlers for LSP methods:
//        - initialize: advertise capabilities and token legends.
//        - text sync (didOpen/didChange): update docState and trigger analyze.
//        - language features: hover, definition, references, completion,
//          document symbols, semantic tokens (full/range), signature help,
//          folding ranges.
//   • Heuristics that read docState (tokens, AST, spans, symbols) and format
//     LSP-shaped responses. Where useful, consults server.ip to surface
//     metadata about built-ins (e.g., function signatures) — without executing
//     user code. The optional listBindings() call that executed user code has
//     been removed to respect “don’t run user code in LSP.”
//
// What does NOT live here
//   • No transport framing or JSON-RPC loop (see main.go).
//   • No core text/position math or analysis pipeline (see core.go).
//   • No interpreter internals or VM usage; feature logic relies on lexer/parser
//     output and cached spans. Interpreter access is read-only for metadata.
//
// Why this separation
//   • Keeps feature code declarative and testable.
//   • Allows core analysis to evolve without touching user-visible features.
//
// Dependencies
//   • Consumes helpers/types from core.go and protocol.go.
//   • May read from s.ip.Global to describe built-ins; otherwise operates purely
//     on statically computed tokens/AST/symbols.

package main

import (
	"bytes"
	"encoding/json"
	"fmt"
	"sort"
	"strings"

	mindscript "github.com/DAIOS-AI/msg/internal/mindscript"
)

////////////////////////////////////////////////////////////////////////////////
// Feature-local helpers moved from analysis.go (UI-only utilities)
////////////////////////////////////////////////////////////////////////////////

// hasValidSpan reports whether the lexer provided concrete byte offsets.
// Used for ANNOTATION tokens which must have StartByte/EndByte or be ignored.
func hasValidSpan(t mindscript.Token, textLen int) bool {
	return t.StartByte >= 0 && t.EndByte >= t.StartByte && t.EndByte <= textLen
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

func isKeywordButNotType(tt mindscript.TokenType) bool {
	switch tt {
	case mindscript.AND, mindscript.OR, mindscript.NOT,
		mindscript.LET, mindscript.DO, mindscript.END, mindscript.RETURN, mindscript.BREAK, mindscript.CONTINUE,
		mindscript.IF, mindscript.THEN, mindscript.ELIF, mindscript.ELSE,
		mindscript.FUNCTION, mindscript.ORACLE,
		mindscript.FOR, mindscript.IN, mindscript.FROM, mindscript.WHILE,
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
// Initialize & text sync
////////////////////////////////////////////////////////////////////////////////

func (s *server) onInitialize(id json.RawMessage, _ json.RawMessage) {
	// Keep the token legend order in sync with semTypes in core.go
	legendTypes := []string{
		"keyword", "function", "type", "variable", "property",
		"string", "number", "comment", "bracket",
	}

	semProv := &struct {
		Legend struct {
			TokenTypes     []string `json:"tokenTypes"`
			TokenModifiers []string `json:"tokenModifiers"`
		} `json:"legend"`
		Full  bool `json:"full"`
		Range bool `json:"range"`
	}{Full: true, Range: true}
	semProv.Legend.TokenTypes = legendTypes
	semProv.Legend.TokenModifiers = []string{"b0", "b1", "b2", "b3", "b4", "b5"}

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
		ServerInfo: map[string]string{
			"name":    "mindscript-lsp",
			"version": "0.4",
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
	doc := &docState{
		uri:   params.TextDocument.URI,
		text:  params.TextDocument.Text,
		lines: lineOffsets(params.TextDocument.Text),
	}
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
	if doc == nil || len(params.ContentChanges) == 0 {
		return
	}

	// If any change is a full replace, follow LSP convention and treat it as the only change.
	fullIdx := -1
	for i, ch := range params.ContentChanges {
		if ch.Range == nil {
			fullIdx = i
			break
		}
	}
	if fullIdx >= 0 {
		doc.text = params.ContentChanges[fullIdx].Text
		doc.lines = lineOffsets(doc.text)
		s.analyze(doc)
		return
	}

	// Apply incremental edits in order; recompute line offsets after each to keep positions valid.
	for _, ch := range params.ContentChanges {
		start := posToOffset(doc.lines, ch.Range.Start, doc.text)
		end := posToOffset(doc.lines, ch.Range.End, doc.text)
		var b bytes.Buffer
		b.WriteString(doc.text[:start])
		b.WriteString(ch.Text)
		if end < len(doc.text) {
			b.WriteString(doc.text[end:])
		}
		doc.text = b.String()
		doc.lines = lineOffsets(doc.text)
	}
	s.analyze(doc)
}

////////////////////////////////////////////////////////////////////////////////
// Hover
////////////////////////////////////////////////////////////////////////////////

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

	// Keywords / boolean literals → simple hover
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
	}

	// Prefer the nearest binding (uniform; includes annotations for all kinds + type/sig)
	if b, ok := nearestBinding(doc, name, off); ok {
		var header string
		switch b.Kind {
		case "fun", "oracle":
			sig := b.Sig
			if sig == "" {
				// fallback to minimal presentation
				sig = name + "(...) -> Any"
			}
			header = fmt.Sprintf("**fun** `%s`", sig)
		case "type":
			header = fmt.Sprintf("**type** `%s`", name)
		case "param":
			ty := formatTypeNode(b.TypeNode)
			header = fmt.Sprintf("**param** `%s: %s`", name, ty)
		default:
			ty := formatTypeNode(b.TypeNode)
			if ty != "" && ty != "Any" {
				header = fmt.Sprintf("**variable** `%s: %s`", name, ty)
			} else {
				header = fmt.Sprintf("**variable** `%s`", name)
			}
		}
		content := header
		if txt := strings.TrimSpace(b.DocFull); txt != "" {
			content += "\n\n" + txt
		}
		s.sendResponse(id, Hover{Contents: MarkupContent{Kind: "markdown", Value: content}, Range: &rng}, nil)
		return
	}

	// Builtin types by ID (NOT by TYPE keyword)
	if tokOK && tk.Type == mindscript.ID {
		if docTxt, ok := builtinTypeDocs[name]; ok {
			content := fmt.Sprintf("**type** `%s`\n\n%s", name, docTxt)
			s.sendResponse(id, Hover{Contents: MarkupContent{Kind: "markdown", Value: content}, Range: &rng}, nil)
			return
		}
	}

	// Globals from interpreter (functions/types) — metadata only, no user code execution
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
				if doc := strings.TrimSpace(meta.Doc()); doc != "" {
					content += "\n\n" + doc
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

	// Fallback classification for IDs (kept for completeness)
	if tokOK && tk.Type == mindscript.ID {
		kind := "identifier"
		idx := -1
		if i, _, _, _, ok := tokenAtOffset(doc, off); ok {
			idx = i
		}
		if idx == -1 {
			for i := range doc.tokens {
				if doc.tokens[i] == tk {
					idx = i
					break
				}
			}
		}

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

////////////////////////////////////////////////////////////////////////////////
// Definition
////////////////////////////////////////////////////////////////////////////////

func (s *server) onDefinition(id json.RawMessage, paramsRaw json.RawMessage) {
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
	name, _ := wordAt(doc, params.Position)
	if name == "" {
		s.sendResponse(id, nil, nil)
		return
	}
	off := posToOffset(doc.lines, params.Position, doc.text)
	if b, ok := nearestBinding(doc, name, off); ok {
		s.sendResponse(id, Location{URI: doc.uri, Range: b.Range}, nil)
		return
	}
	// Fallback: old top-level heuristic
	for _, sym := range doc.symbols {
		if sym.Name == name {
			s.sendResponse(id, Location{URI: doc.uri, Range: sym.Range}, nil)
			return
		}
	}
	s.sendResponse(id, nil, nil)
}

////////////////////////////////////////////////////////////////////////////////
// Completion
////////////////////////////////////////////////////////////////////////////////

func (s *server) onCompletion(id json.RawMessage, paramsRaw json.RawMessage) {
	var params struct {
		TextDocument TextDocumentIdentifier `json:"textDocument"`
		Position     Position               `json:"position"`
	}
	_ = json.Unmarshal(paramsRaw, &params)

	doc := s.snapshotDoc(params.TextDocument.URI)
	if doc == nil {
		s.sendResponse(id, []CompletionItem{}, nil)
		return
	}

	// If cursor is inside a STRING or any ANNOTATION span, suppress completions.
	off := posToOffset(doc.lines, params.Position, doc.text)
	if _, tk, _, _, ok := tokenAtOffset(doc, off); ok {
		if tk.Type == mindscript.STRING {
			s.sendResponse(id, []CompletionItem{}, nil)
			return
		}
	}
	for _, sp := range commentSpans(doc) {
		if off >= sp[0] && off < sp[1] {
			s.sendResponse(id, []CompletionItem{}, nil)
			return
		}
	}

	seen := map[string]bool{}
	items := make([]CompletionItem, 0, 128)

	// Uniform: suggest from all bindings we know in this file (with kind/signature or type)
	for _, b := range doc.binds {
		if seen[b.Name] {
			continue
		}
		seen[b.Name] = true
		kind := 6 // Variable
		switch b.Kind {
		case "fun", "oracle":
			kind = 3 // Function
		case "type":
			kind = 5 // Class-ish
		case "param":
			kind = 6 // Variable (parameter)
		}
		detail := b.Kind
		if (b.Kind == "fun" || b.Kind == "oracle") && b.Sig != "" {
			detail = b.Sig
		} else {
			ty := formatTypeNode(b.TypeNode)
			if ty != "" && ty != "Any" {
				if detail != "" {
					detail = detail + " · " + ty
				} else {
					detail = ty
				}
			}
		}
		items = append(items, CompletionItem{
			Label:  b.Name,
			Kind:   kind,
			Detail: detail,
		})
	}

	// Language keywords (MindScript-specific spellings)
	keywords := []string{
		"and", "or", "not",
		"let", "do", "end", "return", "break", "continue",
		"if", "then", "elif", "else",
		"fun", "oracle",
		"for", "in", "from", "while",
		"type", "enum",
		"null", "true", "false",
	}
	for _, kw := range keywords {
		if !seen[kw] {
			seen[kw] = true
			items = append(items, CompletionItem{Label: kw, Kind: 14}) // Keyword
		}
	}

	sort.Slice(items, func(i, j int) bool { return items[i].Label < items[j].Label })
	s.sendResponse(id, items, nil)
}

////////////////////////////////////////////////////////////////////////////////
// Document symbols
////////////////////////////////////////////////////////////////////////////////

func (s *server) onDocumentSymbols(id json.RawMessage, paramsRaw json.RawMessage) {
	var params struct {
		TextDocument TextDocumentIdentifier `json:"textDocument"`
	}
	_ = json.Unmarshal(paramsRaw, &params)

	doc := s.snapshotDoc(params.TextDocument.URI)
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

////////////////////////////////////////////////////////////////////////////////
// References
////////////////////////////////////////////////////////////////////////////////

func (s *server) onReferences(id json.RawMessage, paramsRaw json.RawMessage) {
	var params struct {
		TextDocument TextDocumentIdentifier `json:"textDocument"`
		Position     Position               `json:"position"`
		Context      struct {
			IncludeDeclaration bool `json:"includeDeclaration"`
		} `json:"context"`
	}
	_ = json.Unmarshal(paramsRaw, &params)

	doc := s.snapshotDoc(params.TextDocument.URI)
	if doc == nil {
		s.sendResponse(id, []Location{}, nil)
		return
	}

	name, _ := wordAt(doc, params.Position)
	if name == "" {
		s.sendResponse(id, []Location{}, nil)
		return
	}

	// Resolve the specific binding at the query position.
	qOff := posToOffset(doc.lines, params.Position, doc.text)
	bOrigin, ok := nearestBinding(doc, name, qOff)
	if !ok {
		// fallback: all occurrences of the bare identifier (minus properties)
		locs := []Location{}
		for i, t := range doc.tokens {
			if t.Type != mindscript.ID || tokenName(t) != name {
				continue
			}
			if i-1 >= 0 && doc.tokens[i-1].Type == mindscript.PERIOD {
				continue
			}
			start, end := tokenSpan(doc, t)
			locs = append(locs, Location{
				URI:   doc.uri,
				Range: makeRange(doc.lines, start, end, doc.text),
			})
		}
		s.sendResponse(id, locs, nil)
		return
	}

	// Shadowing-aware: include only occurrences that resolve to the same nearest binding.
	locs := []Location{}
	for i, t := range doc.tokens {
		if t.Type != mindscript.ID || tokenName(t) != name {
			continue
		}
		// Exclude property names: immediately preceded by '.'
		if i-1 >= 0 && doc.tokens[i-1].Type == mindscript.PERIOD {
			continue
		}
		sOff, eOff := tokenSpan(doc, t)
		// Use the start of the token as its "position"
		if bTok, ok := nearestBinding(doc, name, sOff); ok {
			if bTok.Range == bOrigin.Range {
				locs = append(locs, Location{
					URI:   doc.uri,
					Range: makeRange(doc.lines, sOff, eOff, doc.text),
				})
			}
		}
	}
	s.sendResponse(id, locs, nil)
}

////////////////////////////////////////////////////////////////////////////////
// Semantic tokens (full & range)
////////////////////////////////////////////////////////////////////////////////

func (s *server) onSemanticTokensFull(id json.RawMessage, paramsRaw json.RawMessage) {
	var params SemanticTokensParams
	_ = json.Unmarshal(paramsRaw, &params)
	doc := s.snapshotDoc(params.TextDocument.URI)
	data := s.semanticTokensData(doc, -1, -1) // full
	s.sendResponse(id, SemanticTokens{Data: data}, nil)
}

func (s *server) onSemanticTokensRange(id json.RawMessage, paramsRaw json.RawMessage) {
	var params SemanticTokensRangeParams
	_ = json.Unmarshal(paramsRaw, &params)
	doc := s.snapshotDoc(params.TextDocument.URI)
	if doc == nil {
		s.sendResponse(id, SemanticTokens{Data: nil}, nil)
		return
	}
	start := posToOffset(doc.lines, params.Range.Start, doc.text)
	end := posToOffset(doc.lines, params.Range.End, doc.text)
	data := s.semanticTokensData(doc, start, end)
	s.sendResponse(id, SemanticTokens{Data: data}, nil)
}

type semEntry struct {
	line, ch, lenU16, typ int
	mods                  uint32
}

// Keyword “brackets” for semantic coloring:
func isKeywordOpen(t mindscript.Token) bool {
	return t.Type == mindscript.DO || t.Type == mindscript.THEN
}
func isKeywordClose(t mindscript.Token) bool {
	return t.Type == mindscript.END || t.Type == mindscript.ELIF
}
func isKeywordCloseReopen(t mindscript.Token) bool {
	return t.Type == mindscript.ELSE
}

// semanticTokensData builds LSP-encoded semantic token data.
// If [selStart, selEnd) are >=0, only tokens overlapping that range are emitted.
func (s *server) semanticTokensData(doc *docState, selStart, selEnd int) []uint32 {
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

	// Emit code tokens, skipping anything inside comment spans.
	// Track brace depth to classify map-literal keys (`id`/`"str"`) as properties.
	braceDepth := 0

	for i := 0; i < len(doc.tokens); i++ {
		tk := doc.tokens[i]
		if tk.Type == mindscript.ANNOTATION {
			// the whole annotation region already colored as comment
			continue
		}

		sOff, eOff := tokenSpan(doc, tk)
		if eOff <= sOff {
			// still keep structural depth in sync
			if tk.Type == mindscript.LCURLY {
				braceDepth++
			} else if tk.Type == mindscript.RCURLY && braceDepth > 0 {
				braceDepth--
			}
			continue
		}

		if selStart >= 0 && selEnd >= 0 && !(eOff > selStart && sOff < selEnd) {
			// maintain depth even when skipping by range
			if tk.Type == mindscript.LCURLY {
				braceDepth++
			} else if tk.Type == mindscript.RCURLY && braceDepth > 0 {
				braceDepth--
			}
			continue
		}

		if isInComment(sOff, eOff) {
			// maintain depth across comments
			if tk.Type == mindscript.LCURLY {
				braceDepth++
			} else if tk.Type == mindscript.RCURLY && braceDepth > 0 {
				braceDepth--
			}
			continue
		}

		typIdx := -1
		switch {
		case isKeywordButNotType(tk.Type) || tk.Type == mindscript.BOOLEAN:
			// Let the bracket pass own do/then/elif/else/end so we don't duplicate tokens.
			if isKeywordOpen(tk) || isKeywordClose(tk) || isKeywordCloseReopen(tk) {
				// maintain depth for braces only (not relevant here), then skip emission
				if tk.Type == mindscript.LCURLY {
					braceDepth++
				} else if tk.Type == mindscript.RCURLY && braceDepth > 0 {
					braceDepth--
				}
				continue
			}
			typIdx = semTypes["keyword"]

		case tk.Type == mindscript.STRING:
			// Property after '.' OR map key before ':'
			if i-1 >= 0 && doc.tokens[i-1].Type == mindscript.PERIOD {
				typIdx = semTypes["property"]
			} else if braceDepth > 0 && i+1 < len(doc.tokens) && doc.tokens[i+1].Type == mindscript.COLON {
				typIdx = semTypes["property"]
			} else {
				typIdx = semTypes["string"]
			}

		case tk.Type == mindscript.INTEGER || tk.Type == mindscript.NUMBER:
			typIdx = semTypes["number"]

		case tk.Type == mindscript.ID:
			name := tokenName(tk)
			idx := i // avoid O(n) indexOfToken
			if idx >= 1 && doc.tokens[idx-1].Type == mindscript.PERIOD {
				typIdx = semTypes["property"]
			} else if braceDepth > 0 && i+1 < len(doc.tokens) && doc.tokens[i+1].Type == mindscript.COLON {
				// `{ key: ... }`
				typIdx = semTypes["property"]
			} else {
				kind := ""
				if sy, ok := findSymbol(doc, name); ok {
					kind = sy.Kind
				}
				if kind == "fun" || (idx+1 < len(doc.tokens) && doc.tokens[idx+1].Type == mindscript.CLROUND) {
					typIdx = semTypes["function"]
				} else if kind == "type" {
					typIdx = semTypes["type"]
				} else if v, err := s.ip.Global.Get(name); err == nil && v.Tag == mindscript.VTType {
					typIdx = semTypes["type"]
				} else {
					typIdx = semTypes["variable"]
				}
			}

		default:
			// keep depth updated, even if we don't emit anything
			if tk.Type == mindscript.LCURLY {
				braceDepth++
			} else if tk.Type == mindscript.RCURLY && braceDepth > 0 {
				braceDepth--
			}
			continue
		}

		// LSP requires single-line tokens. Split on newlines.
		segStart := sOff
		for cur := sOff; cur < eOff; cur++ {
			if doc.text[cur] == '\r' {
				continue
			}
			if doc.text[cur] == '\n' {
				if segStart < cur {
					st := offsetToPos(doc.lines, segStart, doc.text)
					entries = append(entries, semEntry{
						line:   st.Line,
						ch:     st.Character,
						lenU16: u16Len(doc.text[segStart:cur]),
						typ:    typIdx,
						mods:   0,
					})
				}
				segStart = cur + 1
			}
		}
		if segStart < eOff {
			st := offsetToPos(doc.lines, segStart, doc.text)
			entries = append(entries, semEntry{
				line:   st.Line,
				ch:     st.Character,
				lenU16: u16Len(doc.text[segStart:eOff]),
				typ:    typIdx,
				mods:   0,
			})
		}

		// update structural depth after processing a token
		if tk.Type == mindscript.LCURLY {
			braceDepth++
		} else if tk.Type == mindscript.RCURLY && braceDepth > 0 {
			braceDepth--
		}
	}

	// ---------- Keyword bracket pass (adds entries; no duplication with main pass) ----------
	const maxLevels = 6
	level := 0
	push := func() {
		if level < maxLevels-1 {
			level++
		}
	}
	pop := func() {
		if level > 0 {
			level--
		}
	}

	for i := 0; i < len(doc.tokens); i++ {
		tk := doc.tokens[i]

		// Compute token span; obey range filter & comment masking
		sOff, eOff := tokenSpan(doc, tk)
		if eOff <= sOff {
			continue
		}
		if selStart >= 0 && selEnd >= 0 && !(eOff > selStart && sOff < selEnd) {
			continue
		}
		if isInComment(sOff, eOff) {
			continue
		}
		// Strings are opaque; keywords inside strings are not structure.
		if tk.Type == mindscript.STRING {
			continue
		}
		// Only bracket-like keywords
		if !(isKeywordOpen(tk) || isKeywordClose(tk) || isKeywordCloseReopen(tk)) {
			continue
		}

		typIdx := semTypes["bracket"]
		// Use the *closing* level for close/close-reopen so matching pairs share the same color.
		// Example: after 'then' we push (depth=1). On 'end', we want the same b* as 'then' → level-1.
		lvl := level
		if isKeywordClose(tk) || isKeywordCloseReopen(tk) {
			if lvl > 0 {
				lvl--
			}
		}
		modBit := uint32(1 << lvl) // maps to b0..b5 in package.json

		emit := func(segStart, segEnd int) {
			st := offsetToPos(doc.lines, segStart, doc.text)
			entries = append(entries, semEntry{
				line:   st.Line,
				ch:     st.Character,
				lenU16: u16Len(doc.text[segStart:segEnd]),
				typ:    typIdx,
				mods:   modBit,
			})
		}

		// Split by line exactly like the main pass
		segStart := sOff
		for cur := sOff; cur < eOff; cur++ {
			if doc.text[cur] == '\r' {
				continue
			}
			if doc.text[cur] == '\n' {
				if segStart < cur {
					emit(segStart, cur)
				}
				segStart = cur + 1
			}
		}
		if segStart < eOff {
			emit(segStart, eOff)
		}

		// Update nesting after emission
		switch {
		case isKeywordOpen(tk):
			push()
		case isKeywordCloseReopen(tk):
			pop()
			push()
		case isKeywordClose(tk):
			pop()
		}
	}

	// Sort then delta-encode
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
		data = append(data, uint32(dl), uint32(dc), uint32(e.lenU16), uint32(e.typ), e.mods)
	}
	return data
}

////////////////////////////////////////////////////////////////////////////////
// Signature help
////////////////////////////////////////////////////////////////////////////////

func (s *server) onSignatureHelp(id json.RawMessage, paramsRaw json.RawMessage) {
	var params SignatureHelpParams
	_ = json.Unmarshal(paramsRaw, &params)

	doc := s.snapshotDoc(params.TextDocument.URI)
	if doc == nil {
		s.sendResponse(id, SignatureHelp{}, nil)
		return
	}

	off := posToOffset(doc.lines, params.Position, doc.text)
	tIdx, _, _, _, ok := tokenAtOffset(doc, off)
	if !ok && len(doc.tokens) > 0 {
		tIdx = len(doc.tokens) - 1
	}

	// Walk left to find the matching '(' (CLROUND) with proper nesting.
	depth := 0
	openIdx := -1
	for i := tIdx; i >= 0; i-- {
		switch doc.tokens[i].Type {
		case mindscript.RROUND:
			depth++
		case mindscript.CLROUND, mindscript.LROUND:
			if depth == 0 {
				openIdx = i
				goto found
			}
			depth--
		}
	}
found:
	if openIdx < 0 || doc.tokens[openIdx].Type != mindscript.CLROUND {
		s.sendResponse(id, SignatureHelp{}, nil)
		return
	}

	// Try to get the callee name (simple heuristic: previous ID)
	name := ""
	if openIdx-1 >= 0 && doc.tokens[openIdx-1].Type == mindscript.ID {
		name = tokenName(doc.tokens[openIdx-1])
	}

	// Count commas at top-level between '(' and cursor to find active parameter.
	paramIdx := 0
	depth = 0
	for i := openIdx + 1; i < len(doc.tokens); i++ {
		tk := doc.tokens[i]
		sOff, eOff := tokenSpan(doc, tk)
		if off < sOff {
			break
		}
		switch tk.Type {
		case mindscript.CLROUND, mindscript.LROUND:
			depth++
		case mindscript.RROUND:
			if depth == 0 {
				i = len(doc.tokens) // break two loops
				break
			}
			depth--
		case mindscript.COMMA:
			if depth == 0 && off >= sOff && off >= eOff {
				paramIdx++
			}
		}
	}

	// Build signature(s)
	resp := SignatureHelp{
		Signatures:      []SignatureInformation{},
		ActiveSignature: 0,
		ActiveParameter: paramIdx,
	}

	// Prefer a local binding signature if available
	if name != "" {
		for _, b := range doc.binds {
			if b.Name == name && (b.Kind == "fun" || b.Kind == "oracle") && b.Sig != "" {
				resp.Signatures = append(resp.Signatures, SignatureInformation{Label: b.Sig})
				s.sendResponse(id, resp, nil)
				return
			}
		}
		// Fall back to top-level symbol sig (if present)
		for _, sym := range doc.symbols {
			if sym.Name == name && sym.Kind == "fun" {
				label := sym.Sig
				if label == "" {
					label = name + "(...) -> Any"
				}
				resp.Signatures = append(resp.Signatures, SignatureInformation{Label: label})
				s.sendResponse(id, resp, nil)
				return
			}
		}
		// Try global meta (metadata only; no user code execution)
		if v, err := s.ip.Global.Get(name); err == nil && v.Tag == mindscript.VTFun {
			if meta, ok := s.ip.FunMeta(v); ok {
				ps := meta.ParamSpecs()
				parts := make([]string, 0, len(ps))
				paramsInfo := make([]ParameterInformation, 0, len(ps))
				for _, p := range ps {
					seg := fmt.Sprintf("%s: %s", p.Name, mindscript.FormatType(p.Type))
					parts = append(parts, seg)
					paramsInfo = append(paramsInfo, ParameterInformation{Label: seg})
				}
				label := fmt.Sprintf("%s(%s) -> %s", name, strings.Join(parts, ", "), mindscript.FormatType(meta.ReturnType()))
				docm := strings.TrimSpace(meta.Doc())
				var docPtr *MarkupContent
				if docm != "" {
					docPtr = &MarkupContent{Kind: "markdown", Value: docm}
				}
				resp.Signatures = append(resp.Signatures, SignatureInformation{
					Label:         label,
					Documentation: docPtr,
					Parameters:    paramsInfo,
				})
				s.sendResponse(id, resp, nil)
				return
			}
		}
	}

	// Unknown function — provide a minimal shell
	if name != "" {
		resp.Signatures = append(resp.Signatures, SignatureInformation{Label: name + "(…)"})
	}
	s.sendResponse(id, resp, nil)
}

////////////////////////////////////////////////////////////////////////////////
// Folding ranges
////////////////////////////////////////////////////////////////////////////////

func overlaps(a, b [2]int) bool { return a[0] < b[1] && b[0] < a[1] }

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

	var out []FoldingRange

	// Comment/annotation blocks
	for _, sp := range commentSpans(doc) {
		startLine := offsetToPos(doc.lines, sp[0], doc.text).Line
		endLine := offsetToPos(doc.lines, sp[1], doc.text).Line
		if endLine > startLine {
			kind := "comment"
			out = append(out, FoldingRange{StartLine: startLine, EndLine: endLine, Kind: &kind})
		}
	}

	// AST-based folding using spans (starts at headers, ends at closing)
	if doc.spans != nil && doc.ast != nil && len(doc.ast) > 0 {
		foldable := map[string]bool{
			"fun": true, "oracle": true, "if": true, "while": true, "for": true,
			// Optional: fold big literals too
			"array": true, "map": true,
		}
		var walk func(node []any, path mindscript.NodePath)
		walk = func(node []any, path mindscript.NodePath) {
			if len(node) == 0 {
				return
			}
			tag, _ := node[0].(string)
			if foldable[tag] {
				if sp, ok := doc.spans.Get(path); ok {
					startL := offsetToPos(doc.lines, sp.StartByte, doc.text).Line
					endL := offsetToPos(doc.lines, sp.EndByte, doc.text).Line
					if endL > startL {
						out = append(out, FoldingRange{StartLine: startL, EndLine: endL})
					}
				}
			}
			for i := 1; i < len(node); i++ {
				if ch, ok := node[i].([]any); ok {
					walk(ch, append(path, i-1))
				}
			}
		}
		walk(doc.ast, mindscript.NodePath{})
	}

	s.sendResponse(id, out, nil)
}
