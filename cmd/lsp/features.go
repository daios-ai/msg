// cmd/lsp/features.go
package main

import (
	"bytes"
	"encoding/json"
	"fmt"
	"sort"
	"strings"

	mindscript "github.com/DAIOS-AI/msg"
)

////////////////////////////////////////////////////////////////////////////////
// Initialize & text sync
////////////////////////////////////////////////////////////////////////////////

func (s *server) onInitialize(id json.RawMessage, _ json.RawMessage) {
	// Keep the token legend order in sync with semTypes in core.go
	legendTypes := []string{
		"keyword", "function", "type", "variable", "property", "string", "number", "comment",
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
	semProv.Legend.TokenModifiers = []string{}

	result := InitializeResult{
		Capabilities: ServerCapabilities{
			TextDocumentSync: TextDocumentSyncOptions{
				OpenClose:         true,
				Change:            2,
				WillSave:          false,
				WillSaveWaitUntil: true,
			},
			HoverProvider:      true,
			DefinitionProvider: true,
			CompletionProvider: &struct {
				TriggerCharacters []string `json:"triggerCharacters"`
			}{TriggerCharacters: []string{".", ":", "[", "(", ","}},
			DocumentSymbolProvider:          true,
			ReferencesProvider:              true,
			WorkspaceSymbolProvider:         false,
			DocumentFormattingProvider:      true,
			DocumentRangeFormattingProvider: true,
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

// WillSaveWaitUntil: offer formatting edits right before save.
func (s *server) onWillSaveWaitUntil(id json.RawMessage, paramsRaw json.RawMessage) {
	var params struct {
		TextDocument TextDocumentIdentifier `json:"textDocument"`
		Reason       int                    `json:"reason"` // unused
	}
	_ = json.Unmarshal(paramsRaw, &params)

	doc := s.snapshotDoc(params.TextDocument.URI)
	if doc == nil {
		s.sendResponse(id, []TextEdit{}, nil)
		return
	}
	out, err := mindscript.Standardize(doc.text)
	if err != nil || out == doc.text {
		// no edits (either failed to parse or already formatted)
		if err != nil {
			s.publishError(doc, err)
		}
		s.sendResponse(id, []TextEdit{}, nil)
		return
	}
	edit := TextEdit{
		Range:   makeRange(doc.lines, 0, len(doc.text), doc.text),
		NewText: out,
	}
	s.sendResponse(id, []TextEdit{edit}, nil)
}

////////////////////////////////////////////////////////////////////////////////
// Hover
////////////////////////////////////////////////////////////////////////////////

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

	// Local document symbol
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

	// Builtin types by ID
	if tokOK && (tk.Type == mindscript.ID || tk.Type == mindscript.TYPE) {
		if docTxt, ok := builtinTypeDocs[name]; ok {
			content := fmt.Sprintf("**type** `%s`\n\n%s", name, docTxt)
			s.sendResponse(id, Hover{Contents: MarkupContent{Kind: "markdown", Value: content}, Range: &rng}, nil)
			return
		}
	}

	// Globals from interpreter (functions/types)
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

	// Local param type from enclosing fun(s), using AST spans.
	if tokOK && tk.Type == mindscript.ID && doc.ast != nil && doc.spans != nil {
		sOff, eOff := tokenSpan(doc, tk)
		path, node, ok := findSmallestCovering(doc.ast, doc.spans, sOff, eOff)
		if ok && len(node) >= 2 && node[0] == "id" {
			if tS, ok := findParamTypeInEnclosingFuns(doc.ast, path, name); ok && tS != nil {
				label := fmt.Sprintf("**param** `%s: %s`", name, mindscript.FormatType(tS))
				s.sendResponse(id, Hover{Contents: MarkupContent{Kind: "markdown", Value: label}, Range: &rng}, nil)
				return
			}
		}
	}

	// Fallback classification for IDs
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

	// --- dot context: person. → suggest properties only ---
	if ok, base := isDotContext(doc, params.Position); ok && doc.ast != nil {
		seen := map[string]bool{}
		props := []CompletionItem{}

		// find the last top-level assignment to `base` that has a map literal
		ast := doc.ast
		for i := 1; i < len(ast); i++ {
			n, _ := ast[i].([]any)
			if len(n) >= 3 && n[0] == "assign" {
				lhs, _ := n[1].([]any)
				rhs := n[2]
				if len(lhs) >= 2 && (lhs[0] == "decl" || lhs[0] == "id") {
					if nm, ok := lhs[1].(string); ok && nm == base {
						for _, k := range mapKeysLiteral(rhs) {
							if !seen[k] {
								seen[k] = true
								props = append(props, CompletionItem{
									Label: k,
									Kind:  10, // Property
								})
							}
						}
					}
				}
			}
		}
		sort.Slice(props, func(i, j int) bool { return props[i].Label < props[j].Label })
		s.sendResponse(id, props, nil)
		return
	}

	seen := map[string]bool{}
	items := make([]CompletionItem, 0, 128)

	// Document symbols
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
		items = append(items, CompletionItem{
			Label:  sym.Name,
			Kind:   kind,
			Detail: detail,
		})
	}

	// Globals / core bindings
	if entries, order := s.listBindings(s.ip.Global); len(order) > 0 {
		for _, name := range order {
			if seen[name] {
				continue
			}
			seen[name] = true
			v := entries[name]
			kind := 6
			switch v.Tag {
			case mindscript.VTFun:
				kind = 3
			case mindscript.VTType:
				kind = 5
			}
			items = append(items, CompletionItem{Label: name, Kind: kind})
		}
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

// Ask the interpreter for a visible environment map via userland getEnv(), if present.
func (s *server) listBindings(env *mindscript.Env) (map[string]mindscript.Value, []string) {
	ast := mindscript.S{"call", mindscript.S{"id", "getEnv"}}
	v, err := s.ip.EvalAST(ast, env)
	if err != nil || v.Tag != mindscript.VTMap {
		return map[string]mindscript.Value{}, nil
	}
	mo := v.Data.(*mindscript.MapObject)
	return mo.Entries, append([]string(nil), mo.Keys...)
}

func isDotContext(doc *docState, pos Position) (bool, string) {
	off := posToOffset(doc.lines, pos, doc.text)
	idx, tk, _, _, ok := tokenAtOffset(doc, off-1) // look just before cursor
	if !ok || tk.Type != mindscript.PERIOD {
		return false, ""
	}
	// previous significant token should be an ID
	if idx-1 >= 0 && doc.tokens[idx-1].Type == mindscript.ID {
		return true, tokenName(doc.tokens[idx-1])
	}
	return false, ""
}

// extract keys from a ("map", ("pair"| "pair!", ("str",k), v) ...)
func mapKeysLiteral(n any) []string {
	s, ok := n.([]any)
	if !ok || len(s) == 0 || s[0] != "map" {
		return nil
	}
	out := []string{}
	for i := 1; i < len(s); i++ {
		p, _ := s[i].([]any)
		if len(p) >= 3 && (p[0] == "pair" || p[0] == "pair!") {
			// p[1] may be ("str", key) or wrapped by ("annot", ("str", text), ("str", key))
			k := p[1]
			for {
				if a, ok := k.([]any); ok && len(a) >= 3 && a[0] == "annot" {
					k = a[2]
					continue
				}
				break
			}
			if ks, ok := k.([]any); ok && len(ks) >= 2 && ks[0] == "str" {
				if name, ok := ks[1].(string); ok {
					out = append(out, name)
				}
			}
		}
	}
	return out
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

	locs := []Location{}
	for i, t := range doc.tokens {
		if t.Type != mindscript.ID || tokenName(t) != name {
			continue
		}
		// Exclude property names: immediately preceded by '.'
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
}

////////////////////////////////////////////////////////////////////////////////
// Document formatting (full + range)
////////////////////////////////////////////////////////////////////////////////

func (s *server) onDocumentFormatting(id json.RawMessage, paramsRaw json.RawMessage) {
	var params DocumentFormattingParams
	_ = json.Unmarshal(paramsRaw, &params)

	doc := s.snapshotDoc(params.TextDocument.URI)
	if doc == nil {
		s.sendResponse(id, []TextEdit{}, nil)
		return
	}

	out, err := mindscript.Standardize(doc.text)
	if err != nil {
		// surface as diagnostics and return no edits
		s.publishError(doc, err)
		s.sendResponse(id, []TextEdit{}, nil)
		return
	}
	// Full replacement edit
	full := TextEdit{
		Range:   makeRange(doc.lines, 0, len(doc.text), doc.text),
		NewText: out,
	}
	s.sendResponse(id, []TextEdit{full}, nil)
}

func (s *server) onDocumentRangeFormatting(id json.RawMessage, paramsRaw json.RawMessage) {
	var params DocumentRangeFormattingParams
	_ = json.Unmarshal(paramsRaw, &params)

	doc := s.snapshotDoc(params.TextDocument.URI)
	if doc == nil || doc.spans == nil || doc.ast == nil {
		s.sendResponse(id, []TextEdit{}, nil)
		return
	}

	start := posToOffset(doc.lines, params.Range.Start, doc.text)
	end := posToOffset(doc.lines, params.Range.End, doc.text)
	if start < 0 {
		start = 0
	}
	if end > len(doc.text) {
		end = len(doc.text)
	}
	if start >= end {
		s.sendResponse(id, []TextEdit{}, nil)
		return
	}

	_, node, ok := findSmallestCovering(doc.ast, doc.spans, start, end)
	if !ok {
		// Fallback: format whole file minimally (or no edits)
		s.sendResponse(id, []TextEdit{}, nil)
		return
	}

	pretty := mindscript.FormatSExpr(node)

	// Use the recorded span for the node to compute the replacement range
	// Re-find the path to retrieve the exact span we’re replacing.
	path, _, _ := findSmallestCovering(doc.ast, doc.spans, start, end) // same call; path reused
	sp, _ := doc.spans.Get(path)
	edit := TextEdit{
		Range:   makeRange(doc.lines, sp.StartByte, sp.EndByte, doc.text),
		NewText: pretty,
	}
	s.sendResponse(id, []TextEdit{edit}, nil)
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
}

// semanticTokensData builds LSP-encoded semantic token data.
// If [selStart, selEnd) are >=0, only tokens overlapping that range are emitted.
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

	// 1) Emit comment/annotation semantic tokens so they are visibly colored.
	emitCommentSeg := func(sOff, eOff int) {
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
						typ:    semTypes["comment"],
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
				typ:    semTypes["comment"],
			})
		}
	}
	for _, sp := range cspans {
		if selStart >= 0 && selEnd >= 0 && !(sp[1] > selStart && sp[0] < selEnd) {
			continue
		}
		emitCommentSeg(sp[0], sp[1])
	}

	// 2) Emit code tokens, skipping anything inside comment spans.
	// Track brace depth to classify map-literal keys (`id`/`"str"` before ':') as properties.
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
			typIdx = semTypes["keyword"]

		case tk.Type == mindscript.STRING:
			// Map key heuristic: inside {...} and followed by ':' → property
			if braceDepth > 0 && i+1 < len(doc.tokens) && doc.tokens[i+1].Type == mindscript.COLON {
				typIdx = semTypes["property"]
			} else {
				typIdx = semTypes["string"]
			}

		case tk.Type == mindscript.INTEGER || tk.Type == mindscript.NUMBER:
			typIdx = semTypes["number"]

		case tk.Type == mindscript.ID:
			name := tokenName(tk)
			idx := indexOfToken(doc.tokens, tk)
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

		case tk.Type == mindscript.TYPE:
			typIdx = semTypes["type"]

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
			})
		}

		// update structural depth after processing a token
		if tk.Type == mindscript.LCURLY {
			braceDepth++
		} else if tk.Type == mindscript.RCURLY && braceDepth > 0 {
			braceDepth--
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
		data = append(data, uint32(dl), uint32(dc), uint32(e.lenU16), uint32(e.typ), 0)
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

	// Prefer local symbol sig
	if name != "" {
		for _, sym := range doc.symbols {
			if sym.Name == name && sym.Kind == "fun" {
				label := sym.Sig
				if label == "" {
					label = name + "(...) -> Any"
				}
				si := SignatureInformation{Label: label}
				resp.Signatures = append(resp.Signatures, si)
				s.sendResponse(id, resp, nil)
				return
			}
		}
		// Try global meta
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
				doc := strings.TrimSpace(meta.Doc())
				var docPtr *MarkupContent
				if doc != "" {
					docPtr = &MarkupContent{Kind: "markdown", Value: doc}
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

	// Structured blocks: do/end, if/elif/else/end, while/end, for/end, fun…do…end, oracle…do…end
	type opener struct {
		typ  mindscript.TokenType
		line int
	}
	stack := []opener{}

	push := func(t mindscript.Token) {
		stack = append(stack, opener{typ: t.Type, line: t.Line - 1})
	}
	pop := func(matches ...mindscript.TokenType) (op opener, ok bool) {
		if len(stack) == 0 {
			return opener{}, false
		}
		top := stack[len(stack)-1]
		for _, m := range matches {
			if top.typ == m {
				stack = stack[:len(stack)-1]
				return top, true
			}
		}
		return opener{}, false
	}

	for _, t := range doc.tokens {
		switch t.Type {
		case mindscript.LCURLY, mindscript.LSQUARE, mindscript.CLSQUARE, mindscript.LROUND, mindscript.CLROUND:
			// Treat grouping opens as fold starters (LSQUARE also from spaced)
			push(t)
		case mindscript.DO, mindscript.IF, mindscript.WHILE, mindscript.FOR, mindscript.FUNCTION, mindscript.ORACLE:
			push(t)
		case mindscript.RCURLY:
			if op, ok := pop(mindscript.LCURLY); ok && (t.Line-1) > op.line {
				out = append(out, FoldingRange{StartLine: op.line, EndLine: t.Line - 1})
			}
		case mindscript.RSQUARE:
			if op, ok := pop(mindscript.LSQUARE, mindscript.CLSQUARE); ok && (t.Line-1) > op.line {
				out = append(out, FoldingRange{StartLine: op.line, EndLine: t.Line - 1})
			}
		case mindscript.RROUND:
			if op, ok := pop(mindscript.LROUND, mindscript.CLROUND); ok && (t.Line-1) > op.line {
				out = append(out, FoldingRange{StartLine: op.line, EndLine: t.Line - 1})
			}
		case mindscript.END:
			// Close any of the block starters above
			if op, ok := pop(mindscript.DO, mindscript.IF, mindscript.WHILE, mindscript.FOR, mindscript.FUNCTION, mindscript.ORACLE); ok && (t.Line-1) > op.line {
				out = append(out, FoldingRange{StartLine: op.line, EndLine: t.Line - 1})
			}
		}
	}

	s.sendResponse(id, out, nil)
}
