package main

import (
	"bufio"
	"bytes"
	"encoding/json"
	"strings"
	"testing"
	"unicode/utf8"

	mindscript "github.com/DAIOS-AI/msg/internal/mindscript"
)

/* ============================== Helpers (wire) ============================== */

type wireNotif struct {
	JSONRPC string          `json:"jsonrpc"`
	Method  string          `json:"method"`
	Params  json.RawMessage `json:"params,omitempty"`
}

type wireResp struct {
	JSONRPC string          `json:"jsonrpc"`
	ID      json.RawMessage `json:"id,omitempty"`
	Result  json.RawMessage `json:"result,omitempty"`
	Error   *ResponseError  `json:"error,omitempty"`
}

type semResp struct {
	JSONRPC string          `json:"jsonrpc"`
	ID      json.RawMessage `json:"id,omitempty"`
	Result  struct {
		Data []uint32 `json:"data"`
	} `json:"result"`
	Error *ResponseError `json:"error,omitempty"`
}

// readAllMsgs decodes all framed messages currently in buf into a slice of raw bodies.
func readAllMsgs(buf *bytes.Buffer) (bodies [][]byte, _ error) {
	r := bufio.NewReader(bytes.NewReader(buf.Bytes()))
	for {
		body, err := readMsg(r)
		if err != nil {
			break // io.EOF when buffer is exhausted
		}
		bodies = append(bodies, body)
	}
	return bodies, nil
}

func callInitialize(t *testing.T, s *server) {
	t.Helper()
	var buf bytes.Buffer
	old := stdoutSink
	stdoutSink = &buf
	defer func() { stdoutSink = old }()
	s.onInitialize(json.RawMessage("1"), nil)

	// Drain one response to keep reader in sync.
	r := bufio.NewReader(bytes.NewReader(buf.Bytes()))
	if _, err := readMsg(r); err != nil {
		t.Fatalf("initialize read: %v", err)
	}
}

func callSemanticTokensFull(t *testing.T, s *server, uri, src string) ([]uint32, *docState) {
	t.Helper()
	callInitialize(t, s)

	// Open the doc (analyze populates tokens and caches).
	openParams := struct {
		TextDocument TextDocumentItem `json:"textDocument"`
	}{TextDocument: TextDocumentItem{
		URI:        uri,
		LanguageID: "mindscript",
		Version:    1,
		Text:       src,
	}}
	openRaw, _ := json.Marshal(openParams)
	s.onDidOpen(openRaw)

	// Request semantic tokens/full.
	var buf bytes.Buffer
	old := stdoutSink
	stdoutSink = &buf
	defer func() { stdoutSink = old }()

	params := struct {
		TextDocument TextDocumentIdentifier `json:"textDocument"`
	}{TextDocument: TextDocumentIdentifier{URI: uri}}
	raw, _ := json.Marshal(params)
	s.onSemanticTokensFull(json.RawMessage("2"), raw)

	r := bufio.NewReader(bytes.NewReader(buf.Bytes()))
	body, err := readMsg(r)
	if err != nil {
		t.Fatalf("sem full read: %v", err)
	}
	var resp semResp
	if err := json.Unmarshal(body, &resp); err != nil {
		t.Fatalf("sem full unmarshal: %v", err)
	}
	if resp.Error != nil {
		t.Fatalf("sem full error: %+v", resp.Error)
	}
	s.mu.RLock()
	doc := s.docs[uri]
	s.mu.RUnlock()
	return resp.Result.Data, doc
}

// decode LSP semantic tokens (delta-encoded UTF-16 positions) to absolute ranges.
type semTok struct {
	Start Position
	End   Position
	Typ   int
}

func decodeSemTokens(data []uint32) []semTok {
	out := []semTok{}
	line, ch := 0, 0
	for i := 0; i+4 < len(data); i += 5 {
		dl := int(data[i+0])
		dc := int(data[i+1])
		lenU16 := int(data[i+2])
		typ := int(data[i+3])
		if dl == 0 {
			ch += dc
		} else {
			line += dl
			ch = dc
		}
		start := Position{Line: line, Character: ch}
		end := Position{Line: line, Character: ch + lenU16}
		out = append(out, semTok{Start: start, End: end, Typ: typ})
	}
	return out
}

// move forward by n UTF-16 code units without crossing a newline; returns end offset.
func offsetAfterU16(text string, start, n int) int {
	i := start
	for n > 0 && i < len(text) {
		r, sz := utf8.DecodeRuneInString(text[i:])
		if r == '\n' {
			break
		}
		if r < 0x10000 {
			n--
		} else {
			n -= 2
		}
		i += sz
	}
	return i
}

// gatherDiagnostics pulls all publishDiagnostics notifications from a buffer.
func gatherDiagnostics(buf *bytes.Buffer) ([]PublishDiagnosticsParams, error) {
	bodies, _ := readAllMsgs(buf)
	out := []PublishDiagnosticsParams{}
	for _, b := range bodies {
		var n wireNotif
		if err := json.Unmarshal(b, &n); err != nil {
			continue
		}
		if n.Method != "textDocument/publishDiagnostics" {
			continue
		}
		var pd PublishDiagnosticsParams
		if err := json.Unmarshal(n.Params, &pd); err != nil {
			continue
		}
		out = append(out, pd)
	}
	return out, nil
}

// Hover wire caller.
func hoverCall(t *testing.T, s *server, uri, src string, pos Position) (Hover, bool) {
	t.Helper()

	// Open document (will emit diagnostics; ignored here).
	openParams := struct {
		TextDocument TextDocumentItem `json:"textDocument"`
	}{TextDocument: TextDocumentItem{
		URI:        uri,
		LanguageID: "mindscript",
		Version:    1,
		Text:       src,
	}}
	openRaw, _ := json.Marshal(openParams)
	s.onDidOpen(openRaw)

	// Capture only the hover response.
	var buf bytes.Buffer
	oldSink := stdoutSink
	stdoutSink = &buf
	defer func() { stdoutSink = oldSink }()

	hoverParams := struct {
		TextDocument TextDocumentIdentifier `json:"textDocument"`
		Position     Position               `json:"position"`
	}{TextDocument: TextDocumentIdentifier{URI: uri}, Position: pos}
	paramsRaw, _ := json.Marshal(hoverParams)

	id := json.RawMessage([]byte("42"))
	s.onHover(id, paramsRaw)

	// Read back framed response and decode Hover.
	r := bufio.NewReader(bytes.NewReader(buf.Bytes()))
	body, err := readMsg(r)
	if err != nil {
		t.Fatalf("reading hover response: %v", err)
	}
	var wr wireResp
	if err := json.Unmarshal(body, &wr); err != nil {
		t.Fatalf("unmarshal wireResp: %v", err)
	}
	if len(wr.Result) == 0 {
		return Hover{}, false
	}
	var hv Hover
	if err := json.Unmarshal(wr.Result, &hv); err != nil {
		t.Fatalf("unmarshal Hover: %v", err)
	}
	return hv, true
}

/* ============================== LSP tests ============================== */

/* ---- Document Symbols & Definition ---- */

func Test_Main_DocumentSymbols_TopLevel(t *testing.T) {
	s := newServer()
	callInitialize(t, s)

	src := "let x = 1\nf = fun(x: Int) do return x end\n"
	uri := "file:///sym.ms"

	// open
	openParams := struct {
		TextDocument TextDocumentItem `json:"textDocument"`
	}{TextDocument: TextDocumentItem{
		URI:        uri,
		LanguageID: "mindscript",
		Version:    1,
		Text:       src,
	}}
	rawOpen, _ := json.Marshal(openParams)
	s.onDidOpen(rawOpen)

	// request document symbols
	var buf bytes.Buffer
	old := stdoutSink
	stdoutSink = &buf
	defer func() { stdoutSink = old }()

	req := struct {
		TextDocument TextDocumentIdentifier `json:"textDocument"`
	}{TextDocument: TextDocumentIdentifier{URI: uri}}
	rawReq, _ := json.Marshal(req)
	s.onDocumentSymbols(json.RawMessage("99"), rawReq)

	bodies, _ := readAllMsgs(&buf)
	if len(bodies) == 0 {
		t.Fatal("no response for document symbols")
	}
	var resp wireResp
	if err := json.Unmarshal(bodies[len(bodies)-1], &resp); err != nil {
		t.Fatalf("unmarshal wireResp: %v", err)
	}
	var syms []DocumentSymbol
	if err := json.Unmarshal(resp.Result, &syms); err != nil {
		t.Fatalf("unmarshal symbols: %v", err)
	}
	have := map[string]string{}
	for _, ds := range syms {
		have[ds.Name] = ds.Detail
	}
	if _, ok := have["x"]; !ok {
		t.Fatal("missing symbol x")
	}
	if d := have["f"]; !strings.Contains(d, "f(") {
		t.Fatalf("f symbol detail should include a signature, got %q", d)
	}
}

func Test_Main_Definition_ResolvesToDecl(t *testing.T) {
	s := newServer()
	callInitialize(t, s)

	src := "let x = 1\ny = x\n"
	uri := "file:///def.ms"
	openParams := struct {
		TextDocument TextDocumentItem `json:"textDocument"`
	}{TextDocument: TextDocumentItem{
		URI:        uri,
		LanguageID: "mindscript",
		Version:    1,
		Text:       src,
	}}
	rawOpen, _ := json.Marshal(openParams)
	s.onDidOpen(rawOpen)

	// position on the usage 'x'
	lns := lineOffsets(src)
	pos := offsetToPos(lns, strings.LastIndex(src, "x"), src)

	var buf bytes.Buffer
	old := stdoutSink
	stdoutSink = &buf
	defer func() { stdoutSink = old }()

	req := struct {
		TextDocument TextDocumentIdentifier `json:"textDocument"`
		Position     Position               `json:"position"`
	}{TextDocument: TextDocumentIdentifier{URI: uri}, Position: pos}
	raw, _ := json.Marshal(req)
	s.onDefinition(json.RawMessage("7"), raw)

	bodies, _ := readAllMsgs(&buf)
	if len(bodies) == 0 {
		t.Fatal("no response for definition")
	}
	var resp wireResp
	_ = json.Unmarshal(bodies[len(bodies)-1], &resp)
	var loc Location
	if err := json.Unmarshal(resp.Result, &loc); err != nil {
		t.Fatalf("unmarshal Location: %v", err)
	}
	// the returned range should cover the decl 'x' on line 0
	if loc.Range.Start.Line != 0 {
		t.Fatalf("def range line=%d want 0", loc.Range.Start.Line)
	}
}

/* ---- Diagnostics ---- */

func Test_Main_Diagnostics_OnDidOpen_LexError_Publishes(t *testing.T) {
	s := newServer()
	var buf bytes.Buffer
	old := stdoutSink
	stdoutSink = &buf
	defer func() { stdoutSink = old }()

	// A single '$' should be a hard lexical error.
	openParams := struct {
		TextDocument TextDocumentItem `json:"textDocument"`
	}{TextDocument: TextDocumentItem{
		URI:        "file:///lexerr.ms",
		LanguageID: "mindscript",
		Version:    1,
		Text:       "$",
	}}
	raw, _ := json.Marshal(openParams)
	s.onDidOpen(raw)

	diags, _ := gatherDiagnostics(&buf)
	if len(diags) == 0 {
		t.Fatal("expected a diagnostics notification for lex error")
	}
	last := diags[len(diags)-1]
	if got := len(last.Diagnostics); got != 1 {
		t.Fatalf("expected 1 diagnostic, got %d", got)
	}
	d := last.Diagnostics[0]
	if d.Code != "LEX" {
		t.Fatalf("expected Code=LEX, got %q", d.Code)
	}
	if d.Severity != 1 {
		t.Fatalf("expected Severity=1 (Error), got %d", d.Severity)
	}
	// Range should be non-empty.
	if d.Range.Start.Line == d.Range.End.Line && d.Range.Start.Character == d.Range.End.Character {
		t.Fatalf("expected non-empty range, got %#v", d.Range)
	}
}

func Test_Main_Diagnostics_OnDidOpen_ParseError_Publishes(t *testing.T) {
	s := newServer()
	var buf bytes.Buffer
	old := stdoutSink
	stdoutSink = &buf
	defer func() { stdoutSink = old }()

	// "end" alone is lex-valid but parse-invalid → ParseError
	openParams := struct {
		TextDocument TextDocumentItem `json:"textDocument"`
	}{TextDocument: TextDocumentItem{
		URI:        "file:///parseerr.ms",
		LanguageID: "mindscript",
		Version:    1,
		Text:       "end\n",
	}}
	raw, _ := json.Marshal(openParams)
	s.onDidOpen(raw)

	diags, _ := gatherDiagnostics(&buf)
	if len(diags) == 0 {
		t.Fatal("expected a diagnostics notification for parse error")
	}
	last := diags[len(diags)-1]
	if len(last.Diagnostics) != 1 {
		t.Fatalf("expected 1 diagnostic, got %d", len(last.Diagnostics))
	}
	if last.Diagnostics[0].Code != "PARSE" {
		t.Fatalf("expected Code=PARSE, got %q", last.Diagnostics[0].Code)
	}
}

func Test_Main_Diagnostics_ClearAfterFix_OnDidChange(t *testing.T) {
	s := newServer()
	var buf bytes.Buffer
	old := stdoutSink
	stdoutSink = &buf
	defer func() { stdoutSink = old }()

	// 1) Open with invalid text to produce a diagnostic.
	openParams := struct {
		TextDocument TextDocumentItem `json:"textDocument"`
	}{TextDocument: TextDocumentItem{
		URI:        "file:///fix.ms",
		LanguageID: "mindscript",
		Version:    1,
		Text:       "$",
	}}
	rawOpen, _ := json.Marshal(openParams)
	s.onDidOpen(rawOpen)

	// 2) Replace the whole content with valid code; server should publish a CLEAR (empty list).
	changeParams := struct {
		TextDocument struct {
			URI string `json:"uri"`
		} `json:"textDocument"`
		ContentChanges []TextDocumentContentChangeEvent `json:"contentChanges"`
	}{
		TextDocument: struct {
			URI string `json:"uri"`
		}{URI: "file:///fix.ms"},
		ContentChanges: []TextDocumentContentChangeEvent{
			{Text: "let x = 1\n"}, // full replace
		},
	}
	rawCh, _ := json.Marshal(changeParams)
	s.onDidChange(rawCh)

	diags, _ := gatherDiagnostics(&buf)
	if len(diags) == 0 {
		t.Fatal("expected diagnostics traffic")
	}

	foundAnyForURI := false
	foundClear := false
	lastURI := ""
	lastCount := -1

	for i := len(diags) - 1; i >= 0; i-- {
		if !strings.HasSuffix(diags[i].URI, "/fix.ms") {
			continue
		}
		if !foundAnyForURI {
			lastURI = diags[i].URI
			lastCount = len(diags[i].Diagnostics)
			foundAnyForURI = true
		}
		if len(diags[i].Diagnostics) == 0 {
			foundClear = true
			break
		}
	}

	if !foundAnyForURI {
		t.Fatalf("no diagnostics found for target URI; got %d publishes total", len(diags))
	}
	if !foundClear {
		t.Fatalf("expected diagnostics cleared after fix; got %d entries (uri=%q)", lastCount, lastURI)
	}
}

/* ---- Semantic tokens ---- */

func Test_Main_SemanticTokens_WholeIdentifiersNotSplit(t *testing.T) {
	s := newServer()
	src := "let state = 1\nstate = state\n" // 'state' appears thrice
	uri := "file:///sem_whole.ms"

	data, doc := callSemanticTokensFull(t, s, uri, src)
	if doc == nil {
		t.Fatal("doc nil")
	}
	if len(data) == 0 {
		t.Fatal("no semantic tokens returned")
	}
	toks := decodeSemTokens(data)

	// Collect slices of the source each token covers and ensure whole-identifier matches for "state"
	lines := doc.lines
	text := doc.text
	bad := []string{}
	found := 0
	for _, tk := range toks {
		startOff := posToOffset(lines, tk.Start, text)
		endOff := offsetAfterU16(text, startOff, tk.End.Character-tk.Start.Character)
		if startOff < 0 || endOff <= startOff || endOff > len(text) {
			continue
		}
		lex := text[startOff:endOff]
		if strings.Contains(lex, "state") {
			found++
			if lex != "state" {
				bad = append(bad, lex)
			}
		}
	}
	if found == 0 {
		t.Fatal("expected at least one token over 'state'")
	}
	if len(bad) > 0 {
		t.Fatalf("identifier 'state' was split/partially colored: %#v", bad)
	}
}

func Test_Main_SemanticTokens_IgnoreCommentsAndAnnotations(t *testing.T) {
	s := newServer()
	src := strings.TrimSpace(`
## whole line comment mentioning state and obj.x
# a block annotation
# another line
let x = 1
#( inline annotation with foo and bar )
x = x
`)
	uri := "file:///sem_comments.ms"
	data, doc := callSemanticTokensFull(t, s, uri, src)
	if doc == nil {
		t.Fatal("doc nil")
	}
	toks := decodeSemTokens(data)

	// Build "forbidden" spans: line comments (## ... \n), block '#' lines, inline "#(...)".
	text := doc.text
	var forbid [][2]int

	// 2a) "##" line comments
	start := 0
	for {
		i := strings.Index(text[start:], "##")
		if i < 0 {
			break
		}
		i += start
		j := strings.IndexByte(text[i:], '\n')
		if j < 0 {
			j = len(text) - i
		}
		forbid = append(forbid, [2]int{i, i + j})
		start = i + j
	}

	// 2b) block '#' lines (start of line -> optional spaces -> '#')
	for lineIdx := 0; lineIdx < len(doc.lines); lineIdx++ {
		lo := doc.lines[lineIdx]
		hi := len(text)
		if lineIdx+1 < len(doc.lines) {
			hi = doc.lines[lineIdx+1]
		}
		line := text[lo:hi]
		trim := strings.TrimLeft(line, " \t")
		if strings.HasPrefix(trim, "#") && !strings.HasPrefix(trim, "##") { // annotation-style
			forbid = append(forbid, [2]int{lo, hi})
		}
	}

	// 2c) inline "#(" ... ")"
	start = 0
	for {
		i := strings.Index(text[start:], "#(")
		if i < 0 {
			break
		}
		i += start
		j := strings.Index(text[i:], ")")
		if j < 0 {
			j = len(text) - i
		}
		forbid = append(forbid, [2]int{i, i + j + 1})
		start = i + j + 1
	}

	overlaps := func(a, b [2]int) bool { return a[0] < b[1] && b[0] < a[1] }

	// Assert no semantic token overlaps any forbidden span
	for _, tk := range toks {
		sOff := posToOffset(doc.lines, tk.Start, text)
		eOff := offsetAfterU16(text, sOff, tk.End.Character-tk.Start.Character)
		span := [2]int{sOff, eOff}
		for _, fb := range forbid {
			if overlaps(span, fb) {
				t.Fatalf("semantic token overlapped comment/annotation span: %v overlaps %v (%q)",
					span, fb, text[fb[0]:fb[1]])
			}
		}
	}
}

/* ---- Hover ---- */

func Test_Main_Hover_OnLocalLet_ShowsMarkdownAndRange(t *testing.T) {
	src := "let x = 1\ny = x\n"
	uri := "file:///hover_local.ms"
	s := newServer()

	// Place cursor at the usage 'x' on the second line
	lns := lineOffsets(src)
	useIdx := strings.LastIndex(src, "x")
	if useIdx < 0 {
		t.Fatal("no x usage")
	}
	pos := offsetToPos(lns, useIdx, src)

	hv, ok := hoverCall(t, s, uri, src, pos)
	if !ok {
		t.Fatal("expected a hover result, got nil")
	}
	if hv.Contents.Kind != "markdown" {
		t.Fatalf("hover kind=%q want markdown", hv.Contents.Kind)
	}
	// The server may show a synthesized type: "**variable** `x: Int`".
	if !(strings.Contains(hv.Contents.Value, "**variable** `x`") ||
		strings.Contains(hv.Contents.Value, "**variable** `x:")) {
		t.Fatalf("hover contents=%q want to contain **variable** `x` (optionally with : type)", hv.Contents.Value)
	}
	// Range should cover exactly the 'x'
	if hv.Range == nil {
		t.Fatal("hover range is nil; expected a range")
	}
	startOff := posToOffset(lns, hv.Range.Start, src)
	endOff := posToOffset(lns, hv.Range.End, src)
	if endOff-startOff != 1 || src[startOff:endOff] != "x" {
		t.Fatalf("hover range slice=%q want x", src[startOff:endOff])
	}
}

func Test_Main_Hover_OnIdentifierAtEndExclusiveBoundary(t *testing.T) {
	src := "foo(x)\n"
	uri := "file:///hover_end.ms"
	s := newServer()

	lns := lineOffsets(src)
	// Cursor right after "foo" (before '(') to exercise end-exclusive + fallback
	i := strings.Index(src, "foo")
	if i < 0 {
		t.Fatal("no foo")
	}
	pos := offsetToPos(lns, i+len("foo"), src)

	hv, ok := hoverCall(t, s, uri, src, pos)
	// There is no symbol info for foo in this doc, so hover may legitimately be nil.
	// However word targeting must succeed if a symbol existed. We assert that the
	// range, if present, actually covers "foo".
	if ok && hv.Range != nil {
		start := posToOffset(lns, hv.Range.Start, src)
		end := posToOffset(lns, hv.Range.End, src)
		got := src[start:end]
		if got != "foo" {
			t.Fatalf("hover boundary coverage=%q want foo", got)
		}
	}
}

func Test_Main_Hover_OnGlobalNative_ShowsSignature(t *testing.T) {
	uri := "file:///hover_native.ms"
	s := newServer()

	// Register a native and then reference it from the document.
	s.ip.RegisterNative(
		"add1",
		[]mindscript.ParamSpec{{Name: "x", Type: mindscript.S{"id", "Int"}}},
		mindscript.S{"id", "Int"},
		func(ip *mindscript.Interpreter, ctx mindscript.CallCtx) mindscript.Value {
			return mindscript.Int(41 + 1)
		},
	)

	src := "add1(1)\n"
	lns := lineOffsets(src)
	at := strings.Index(src, "add1")
	if at < 0 {
		t.Fatal("no add1")
	}
	pos := offsetToPos(lns, at, src)

	hv, ok := hoverCall(t, s, uri, src, pos)
	if !ok {
		t.Fatal("expected hover result for global native")
	}
	if hv.Contents.Kind != "markdown" {
		t.Fatalf("hover kind=%q want markdown", hv.Contents.Kind)
	}
	// Expect a formatted signature from FunMeta path.
	if !strings.Contains(hv.Contents.Value, "**fun** `add1(x: Int) -> Int`") {
		t.Fatalf("hover contents=%q want signature for add1", hv.Contents.Value)
	}
}
