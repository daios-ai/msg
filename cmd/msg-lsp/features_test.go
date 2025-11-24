package main

import (
	"bufio"
	"bytes"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"strings"
	"testing"
	"time"
)

//
// --------------------------- Test Fixtures -----------------------------------
//

const fixtureCoreHappy = `let Person = type { name!: Str, age: Int }
let inc = fun(n: Int) -> Int do n + 1 end
let countryCode = oracle(name: Str) -> Str

let xs = [1, 2]
let { name: n, age: a } = { name: "Ada", age: 36 }
let y = inc(xs[0])
let cc = countryCode(n)
if cc == null then "no" else cc end
`

const fixtureErrors = `let z = missing + 1
let d = 10 / 0
let m = 5 % 0

let add2 = fun(a: Int, b: Int) -> Int do a + b end
let tooMany = add2(1, 2, 3)

let arr = [10, 20]
let badIdx = arr["0"]

let p = { name: "Ada" }
let missingField = p.age

let f = fun(n: Int) -> Int do if n < 0 then "oops" else n + 1 end end
`

const fixtureRefsTokens = `let total = 0
let xs = [1, 2, 3]
for x in xs do
	total = total + x
end
total
`

const fixtureTypesFuncs = `let Pair = type { left!: Int, right!: Int }
let mk = fun(a: Int, b: Int) -> Pair do { left: a, right: b } end
let p = mk(1, 2)
let add = fun(a: Int, b: Int) -> Int do a + b end
let curried = add(1)(2)
p
`

//
// --------------------------- RPC Wire Helpers --------------------------------
//

// read one framed (Content-Length) JSON-RPC message from *Reader
func readFramed(r *bufio.Reader) ([]byte, error) {
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
		return nil, fmt.Errorf("bad content-length")
	}
	buf := make([]byte, contentLen)
	_, err := r.Read(buf)
	return buf, err
}

type rpcEnvelope struct {
	JSONRPC string          `json:"jsonrpc"`
	ID      json.RawMessage `json:"id,omitempty"`
	Method  string          `json:"method,omitempty"`
	Params  json.RawMessage `json:"params,omitempty"`
	Result  json.RawMessage `json:"result,omitempty"`
	Error   *ResponseError  `json:"error,omitempty"`
}

// replace decodeAll with:
func decodeAll(buf *bytes.Buffer) ([]rpcEnvelope, error) {
	out := []rpcEnvelope{}
	r := bufio.NewReader(bytes.NewReader(buf.Bytes()))
	for {
		b, err := readFramed(r)
		if err != nil {
			if errors.Is(err, io.EOF) {
				break
			}
			return out, err
		}
		var env rpcEnvelope
		if err := json.Unmarshal(b, &env); err != nil {
			return out, err
		}
		out = append(out, env)
	}
	return out, nil
}

func findResponse(envelopes []rpcEnvelope, id json.RawMessage) *rpcEnvelope {
	for i := range envelopes {
		if len(envelopes[i].ID) > 0 && bytes.Equal(envelopes[i].ID, id) {
			return &envelopes[i]
		}
	}
	return nil
}

func lastPublishDiagnostics(envelopes []rpcEnvelope, uri string) *PublishDiagnosticsParams {
	var out *PublishDiagnosticsParams
	for i := range envelopes {
		if envelopes[i].Method == "textDocument/publishDiagnostics" {
			var p PublishDiagnosticsParams
			_ = json.Unmarshal(envelopes[i].Params, &p)
			if p.URI == uri {
				tmp := p
				out = &tmp
			}
		}
	}
	return out
}

//
// ----------------------------- Client-ish Helpers ----------------------------
//

type testClient struct {
	s   *server
	out *bytes.Buffer
}

func newTestClient(t *testing.T) *testClient {
	t.Helper()
	var buf bytes.Buffer
	old := stdoutSink
	stdoutSink = &buf
	t.Cleanup(func() { stdoutSink = old })
	return &testClient{s: newServer(), out: &buf}
}

func (c *testClient) initialize(t *testing.T) InitializeResult {
	t.Helper()
	id := json.RawMessage(`1`)
	params, _ := json.Marshal(InitializeParams{})
	c.s.onInitialize(id, params)
	envs, err := decodeAll(c.out)
	if err != nil {
		t.Fatalf("decode initialize: %v", err)
	}
	resp := findResponse(envs, id)
	if resp == nil || len(resp.Result) == 0 {
		t.Fatalf("missing initialize result")
	}
	var res InitializeResult
	_ = json.Unmarshal(resp.Result, &res)
	return res
}

func (c *testClient) didOpen(t *testing.T, uri, text string) {
	t.Helper()
	type didOpenParams struct {
		TextDocument TextDocumentItem `json:"textDocument"`
	}
	params := didOpenParams{
		TextDocument: TextDocumentItem{
			URI:        uri,
			LanguageID: "mindscript",
			Version:    1,
			Text:       text,
		},
	}
	b, _ := json.Marshal(params)
	c.s.onDidOpen(b)
}

func (c *testClient) hoverAt(t *testing.T, id string, uri string, line, ch int) *Hover {
	t.Helper()
	type params struct {
		TextDocument TextDocumentIdentifier `json:"textDocument"`
		Position     Position               `json:"position"`
	}
	p := params{
		TextDocument: TextDocumentIdentifier{URI: uri},
		Position:     Position{Line: line, Character: ch},
	}
	b, _ := json.Marshal(p)
	jid := json.RawMessage(id)
	c.s.onHover(jid, b)
	envs, _ := decodeAll(c.out)
	resp := findResponse(envs, jid)
	if resp == nil {
		t.Fatalf("no hover response")
	}
	if len(resp.Result) == 0 || string(resp.Result) == "null" {
		return nil
	}
	var h Hover
	_ = json.Unmarshal(resp.Result, &h)
	return &h
}

func (c *testClient) definitionAt(t *testing.T, id string, uri string, line, ch int) []Location {
	t.Helper()
	type params struct {
		TextDocument TextDocumentIdentifier `json:"textDocument"`
		Position     Position               `json:"position"`
	}
	p := params{TextDocument: TextDocumentIdentifier{URI: uri}, Position: Position{Line: line, Character: ch}}
	b, _ := json.Marshal(p)
	jid := json.RawMessage(id)
	c.s.onDefinition(jid, b)
	envs, _ := decodeAll(c.out)
	resp := findResponse(envs, jid)
	if resp == nil {
		t.Fatalf("no definition response")
	}
	if len(resp.Result) == 0 || string(resp.Result) == "null" {
		return nil
	}
	var locs []Location
	_ = json.Unmarshal(resp.Result, &locs)
	return locs
}

func (c *testClient) referencesAt(t *testing.T, id string, uri string, line, ch int) []Location {
	t.Helper()
	type params struct {
		TextDocument TextDocumentIdentifier `json:"textDocument"`
		Position     Position               `json:"position"`
		Context      struct {
			IncludeDeclaration bool `json:"includeDeclaration"`
		} `json:"context"`
	}
	var p params
	p.TextDocument.URI = uri
	p.Position = Position{Line: line, Character: ch}
	p.Context.IncludeDeclaration = true
	b, _ := json.Marshal(p)
	jid := json.RawMessage(id)
	c.s.onReferences(jid, b)
	envs, _ := decodeAll(c.out)
	resp := findResponse(envs, jid)
	if resp == nil {
		t.Fatalf("no references response")
	}
	if len(resp.Result) == 0 || string(resp.Result) == "null" {
		return nil
	}
	var locs []Location
	_ = json.Unmarshal(resp.Result, &locs)
	return locs
}

func (c *testClient) signatureHelpAt(t *testing.T, id string, uri string, line, ch int) *SignatureHelp {
	t.Helper()
	p := SignatureHelpParams{
		TextDocument: TextDocumentIdentifier{URI: uri},
		Position:     Position{Line: line, Character: ch},
	}
	b, _ := json.Marshal(p)
	jid := json.RawMessage(id)
	c.s.onSignatureHelp(jid, b)
	envs, _ := decodeAll(c.out)
	resp := findResponse(envs, jid)
	if resp == nil {
		t.Fatalf("no signatureHelp response")
	}
	if len(resp.Result) == 0 || string(resp.Result) == "null" {
		return nil
	}
	var sh SignatureHelp
	_ = json.Unmarshal(resp.Result, &sh)
	return &sh
}

func (c *testClient) semanticTokensFull(t *testing.T, id string, uri string) *SemanticTokens {
	t.Helper()
	p := SemanticTokensParams{TextDocument: TextDocumentIdentifier{URI: uri}}
	b, _ := json.Marshal(p)
	jid := json.RawMessage(id)
	c.s.onSemanticTokensFull(jid, b)
	envs, _ := decodeAll(c.out)
	resp := findResponse(envs, jid)
	if resp == nil {
		t.Fatalf("no semanticTokens/full response")
	}
	if len(resp.Result) == 0 || string(resp.Result) == "null" {
		return nil
	}
	var st SemanticTokens
	_ = json.Unmarshal(resp.Result, &st)
	return &st
}

func (c *testClient) documentSymbols(t *testing.T, id string, uri string) []DocumentSymbol {
	t.Helper()
	type params struct {
		TextDocument TextDocumentIdentifier `json:"textDocument"`
	}
	p := params{TextDocument: TextDocumentIdentifier{URI: uri}}
	b, _ := json.Marshal(p)
	jid := json.RawMessage(id)
	c.s.onDocumentSymbols(jid, b)
	envs, _ := decodeAll(c.out)
	resp := findResponse(envs, jid)
	if resp == nil {
		t.Fatalf("no documentSymbol response")
	}
	var ds []DocumentSymbol
	_ = json.Unmarshal(resp.Result, &ds)
	return ds
}

//
// ------------------------------ Tests ----------------------------------------
//

func Test_Features_Initialize_Capabilities(t *testing.T) {
	c := newTestClient(t)
	res := c.initialize(t)

	caps := res.Capabilities
	if res.Capabilities.TextDocumentSync.Change != 1 {
		t.Fatalf("expected full sync (1), got %d", res.Capabilities.TextDocumentSync.Change)
	}
	// We expect these to be TRUE once wired.
	if !caps.HoverProvider {
		t.Fatalf("hoverProvider should be true")
	}
	if !caps.DefinitionProvider {
		t.Fatalf("definitionProvider should be true")
	}
	if !caps.DocumentSymbolProvider {
		t.Fatalf("documentSymbolProvider should be true")
	}
	if !caps.ReferencesProvider {
		t.Fatalf("referencesProvider should be true")
	}
	if caps.SignatureHelpProvider == nil {
		t.Fatalf("signatureHelpProvider should be present")
	}
	if caps.SemanticTokensProvider == nil {
		t.Fatalf("semanticTokensProvider should be present")
	}
}

func Test_Features_CoreHappy_All(t *testing.T) {
	c := newTestClient(t)
	_ = c.initialize(t)
	uri := "file:///core_happy.ms"
	c.didOpen(t, uri, fixtureCoreHappy)

	// Allow any async notifications to be flushed (in case implementation queues them)
	time.Sleep(5 * time.Millisecond)

	envs, err := decodeAll(c.out)
	if err != nil {
		t.Fatalf("decode: %v", err)
	}

	t.Run("DiagnosticsNone", func(t *testing.T) {
		di := lastPublishDiagnostics(envs, uri)
		if di == nil {
			t.Fatalf("expected diagnostics notification, got none")
		}
		if len(di.Diagnostics) != 0 {
			t.Fatalf("expected 0 diagnostics, got %d", len(di.Diagnostics))
		}
	})

	t.Run("Hover_Types", func(t *testing.T) {
		// Crude positions approximate (we only need non-smoky exact content)
		// line 1: "let inc = fun(n: Int) -> Int do n + 1 end"
		h := c.hoverAt(t, `2`, uri, 1, 5) // on "inc"
		if h == nil || !strings.Contains(h.Contents.Value, "inc") {
			t.Fatalf("hover on inc should show token 'inc', got %#v", h)
		}

		// line 2: oracle
		h = c.hoverAt(t, `3`, uri, 2, 5) // on "countryCode"
		if h == nil || !strings.Contains(h.Contents.Value, "countryCode") {
			t.Fatalf("hover on countryCode should show token 'countryCode', got %#v", h)
		}

		// line 4: "let xs = [1, 2]"
		h = c.hoverAt(t, `4`, uri, 4, 5) // on "xs"
		if h == nil || !strings.Contains(h.Contents.Value, "xs") {
			t.Fatalf("hover on xs should show token 'xs', got %#v", h)
		}

		// destructured names: line 5 "... name: n, age: a ..."
		h = c.hoverAt(t, `5`, uri, 5, 20) // near "n"
		if h == nil || !strings.Contains(h.Contents.Value, "n") {
			t.Fatalf("hover on n should show token 'n', got %#v", h)
		}
		h = c.hoverAt(t, `6`, uri, 5, 32) // near "a"
		if h == nil || !strings.Contains(h.Contents.Value, "a") {
			t.Fatalf("hover on a should show token 'a', got %#v", h)
		}

		// Person is a type value (expression type == Type)
		h = c.hoverAt(t, `7`, uri, 0, 4) // "Person"
		if h == nil || !strings.Contains(h.Contents.Value, "Person") {
			t.Fatalf("hover on Person should show token 'Person', got %#v", h)
		}
	})

	t.Run("Definition_Basics", func(t *testing.T) {
		// find def of inc from call site: line 7 "let y = inc(xs[0])"
		locs := c.definitionAt(t, `8`, uri, 7, 10)
		if len(locs) == 0 {
			t.Fatalf("expected definition for inc")
		}
	})

	t.Run("References_Counts", func(t *testing.T) {
		// last line uses cc twice (in cond and else)
		locs := c.referencesAt(t, `9`, uri, 9, 5)
		if got := len(locs); got < 3 {
			t.Fatalf("expected >=3 refs for cc (def + uses), got %d", got)
		}
		// xs: def + index use
		locs = c.referencesAt(t, `10`, uri, 7, 15)
		if got := len(locs); got < 2 {
			t.Fatalf("expected >=2 refs for xs, got %d", got)
		}
	})

	t.Run("SignatureHelp", func(t *testing.T) {
		sh := c.signatureHelpAt(t, `11`, uri, 7, 11) // inc(|xs[0])
		if sh == nil || len(sh.Signatures) == 0 || !strings.Contains(sh.Signatures[0].Label, "Int -> Int") {
			t.Fatalf("signature for inc should be present/int-typed, got %#v", sh)
		}
		sh = c.signatureHelpAt(t, `12`, uri, 8, 18) // countryCode(|n)
		if sh == nil || len(sh.Signatures) == 0 || !strings.Contains(sh.Signatures[0].Label, "Str -> Str?") {
			t.Fatalf("signature for countryCode should show nullable return, got %#v", sh)
		}
	})

	t.Run("SemanticTokens_Some", func(t *testing.T) {
		st := c.semanticTokensFull(t, `13`, uri)
		if st == nil || len(st.Data) == 0 {
			t.Fatalf("expected non-empty semantic tokens stream")
		}
	})

	t.Run("DocumentSymbols_TopLevel", func(t *testing.T) {
		ds := c.documentSymbols(t, `14`, uri)
		want := []string{"Person", "inc", "countryCode", "xs", "y", "cc"}
		gotSet := map[string]bool{}
		for _, s := range ds {
			gotSet[s.Name] = true
		}
		for _, w := range want {
			if !gotSet[w] {
				t.Fatalf("missing document symbol %q; got: %+v", w, gotSet)
			}
		}
	})
}

func Test_Features_Errors_DiagnosticsAndHovers(t *testing.T) {
	c := newTestClient(t)
	_ = c.initialize(t)
	uri := "file:///errors.ms"
	c.didOpen(t, uri, fixtureErrors)
	time.Sleep(5 * time.Millisecond)

	envs, err := decodeAll(c.out)
	if err != nil {
		t.Fatalf("decode: %v", err)
	}
	di := lastPublishDiagnostics(envs, uri)
	if di == nil {
		t.Fatalf("expected diagnostics notification")
	}
	// We assert a strict set of diagnostic *codes*.
	wantCodes := map[string]int{
		"MS-UNKNOWN-NAME":      1,
		"MS-DIV-BY-ZERO-CONST": 2,
		"MS-ARG-OVERFLOW":      1,
		"MS-ARG-TYPE-MISMATCH": 1, // array index must be Int
		"MS-MAP-MISSING-KEY":   1,
		"MS-RET-TYPE-MISMATCH": 1,
	}
	got := map[string]int{}
	for _, d := range di.Diagnostics {
		got[d.Code]++
	}
	for code, wantN := range wantCodes {
		if got[code] != wantN {
			t.Fatalf("diagnostic %s: want %d, got %d; got=%v", code, wantN, got[code], got)
		}
	}

	t.Run("Hover_QuickChecks", func(t *testing.T) {
		// On add2 symbol line 4
		h := c.hoverAt(t, `20`, uri, 4, 5)
		if h == nil || !strings.Contains(h.Contents.Value, "Int -> Int -> Int") {
			t.Fatalf("hover on add2 should show Int -> Int -> Int, got %#v", h)
		}
		// On arr: [Int]
		h = c.hoverAt(t, `21`, uri, 8, 5)
		if h == nil || !strings.Contains(h.Contents.Value, "arr") {
			t.Fatalf("hover on arr should show token 'arr', got %#v", h)
		}
	})
}

func Test_Features_RefsTokens_Loops(t *testing.T) {
	c := newTestClient(t)
	_ = c.initialize(t)
	uri := "file:///refs_tokens.ms"
	c.didOpen(t, uri, fixtureRefsTokens)
	time.Sleep(5 * time.Millisecond)

	// references counts
	locs := c.referencesAt(t, `30`, uri, 4, 0) // final 'total'
	if len(locs) < 3 {
		t.Fatalf("expected >=3 refs for total (def + write + read), got %d", len(locs))
	}
	locs = c.referencesAt(t, `31`, uri, 2, 12) // xs in "for x in xs"
	if len(locs) < 2 {
		t.Fatalf("expected >=2 refs for xs, got %d", len(locs))
	}
	locs = c.referencesAt(t, `32`, uri, 3, 19) // x in body
	if len(locs) < 2 {
		t.Fatalf("expected >=2 refs for x (loop binding), got %d", len(locs))
	}

	// hover type after loop (should be Int)
	h := c.hoverAt(t, `33`, uri, 4, 1)
	if h == nil || !strings.Contains(h.Contents.Value, "Int") {
		t.Fatalf("hover on final total should include Int, got %#v", h)
	}

	// some semantic tokens
	st := c.semanticTokensFull(t, `34`, uri)
	if st == nil || len(st.Data) == 0 {
		t.Fatalf("expected non-empty semantic tokens in loop example")
	}
}

func Test_Features_TypesFuncs_HoversAndSigs(t *testing.T) {
	c := newTestClient(t)
	_ = c.initialize(t)
	uri := "file:///types_funcs.ms"
	c.didOpen(t, uri, fixtureTypesFuncs)
	time.Sleep(5 * time.Millisecond)

	t.Run("Hover_TypeValueAndFuncs", func(t *testing.T) {
		// Pair is a type value => expression type "Type"
		h := c.hoverAt(t, `40`, uri, 0, 4)
		if h == nil || !strings.Contains(h.Contents.Value, "Pair") {
			t.Fatalf("hover on Pair should show token 'Pair', got %#v", h)
		}
		// mk: Int -> Int -> Pair
		h = c.hoverAt(t, `41`, uri, 1, 5)
		if h == nil || !strings.Contains(h.Contents.Value, "mk") {
			t.Fatalf("hover on mk should show token 'mk', got %#v", h)
		}
		// p should be Pair shape (either alias or structural)
		h = c.hoverAt(t, `42`, uri, 2, 5)
		if h == nil || !strings.Contains(h.Contents.Value, "p") {
			t.Fatalf("hover on p should show token 'p', got %#v", h)
		}
		// curried: Int
		h = c.hoverAt(t, `43`, uri, 4, 5)
		if h == nil || !strings.Contains(h.Contents.Value, "curried") {
			t.Fatalf("hover on curried should show token 'curried', got %#v", h)
		}
	})

	t.Run("SignatureHelp_mk", func(t *testing.T) {
		sh := c.signatureHelpAt(t, `44`, uri, 2, 10) // mk(|1, 2)
		if sh == nil || len(sh.Signatures) == 0 {
			t.Fatalf("expected sig help for mk")
		}
		if !strings.Contains(sh.Signatures[0].Label, "Int -> Int -> Pair") {
			t.Fatalf("mk signature label mismatch: %q", sh.Signatures[0].Label)
		}
	})
}

func Test_Features_CoreHappy_Definitions_More(t *testing.T) {
	c := newTestClient(t)
	_ = c.initialize(t)
	uri := "file:///core_happy.ms"
	c.didOpen(t, uri, fixtureCoreHappy)
	time.Sleep(5 * time.Millisecond)

	t.Run("Def_of_xs_from_index_use", func(t *testing.T) {
		// line 7: let y = inc(xs[0])
		locs := c.definitionAt(t, `50`, uri, 7, 14) // on "xs"
		if len(locs) == 0 {
			t.Fatalf("expected definition for xs from use at index")
		}
	})

	t.Run("Def_of_n_from_oracle_call", func(t *testing.T) {
		// line 8: let cc = countryCode(n)
		locs := c.definitionAt(t, `51`, uri, 8, 23) // on "n"
		if len(locs) == 0 {
			t.Fatalf("expected definition for n from call site")
		}
	})

	t.Run("Def_of_cc_from_if_use", func(t *testing.T) {
		// line 9: if cc == null then "no" else cc end
		locs := c.definitionAt(t, `52`, uri, 9, 4) // first "cc"
		if len(locs) == 0 {
			t.Fatalf("expected definition for cc from if condition")
		}
	})
}

func Test_Features_CoreHappy_Hover_Exprs(t *testing.T) {
	c := newTestClient(t)
	_ = c.initialize(t)
	uri := "file:///core_happy.ms"
	c.didOpen(t, uri, fixtureCoreHappy)
	time.Sleep(5 * time.Millisecond)

	t.Run("Hover_xs_index_elem", func(t *testing.T) {
		// line 7: let y = inc(xs[0])
		h := c.hoverAt(t, `60`, uri, 7, 16) // near "[0]"
		if h == nil {
			t.Fatalf("hover on xs[0] should return a token, got nil")
		}
	})

	t.Run("Hover_cc_var_nullable", func(t *testing.T) {
		// line 8: let cc = countryCode(n)
		h := c.hoverAt(t, `61`, uri, 8, 5) // on "cc"
		if h == nil || !strings.Contains(h.Contents.Value, "cc") {
			t.Fatalf("hover on cc should show token 'cc', got %#v", h)
		}
	})
}

func Test_Features_Errors_DiagnosticMessages(t *testing.T) {
	c := newTestClient(t)
	_ = c.initialize(t)
	uri := "file:///errors.ms"
	c.didOpen(t, uri, fixtureErrors)
	time.Sleep(5 * time.Millisecond)

	envs, err := decodeAll(c.out)
	if err != nil {
		t.Fatalf("decode: %v", err)
	}
	di := lastPublishDiagnostics(envs, uri)
	if di == nil {
		t.Fatalf("expected diagnostics notification")
	}

	var haveIdxTypeMsg bool
	for _, d := range di.Diagnostics {
		if d.Code == "MS-ARG-TYPE-MISMATCH" && strings.Contains(d.Message, "array index must be Int") {
			haveIdxTypeMsg = true
			break
		}
	}
	if !haveIdxTypeMsg {
		t.Fatalf("expected MS-ARG-TYPE-MISMATCH with 'array index must be Int' message")
	}
}

func Test_Features_RefsTokens_GoToDef_Total(t *testing.T) {
	c := newTestClient(t)
	_ = c.initialize(t)
	uri := "file:///refs_tokens.ms"
	c.didOpen(t, uri, fixtureRefsTokens)
	time.Sleep(5 * time.Millisecond)

	// From final 'total' usage to its definition at line 0
	locs := c.definitionAt(t, `70`, uri, 4, 1)
	if len(locs) == 0 {
		t.Fatalf("expected definition location for final total")
	}
}

func Test_Features_TypesFuncs_NoDiagnostics(t *testing.T) {
	c := newTestClient(t)
	_ = c.initialize(t)
	uri := "file:///types_funcs.ms"
	c.didOpen(t, uri, fixtureTypesFuncs)
	time.Sleep(5 * time.Millisecond)

	envs, err := decodeAll(c.out)
	if err != nil {
		t.Fatalf("decode: %v", err)
	}
	di := lastPublishDiagnostics(envs, uri)
	if di == nil {
		t.Fatalf("expected diagnostics notification")
	}
	if len(di.Diagnostics) != 0 {
		t.Fatalf("expected 0 diagnostics, got %d: %+v", len(di.Diagnostics), di.Diagnostics)
	}
}

func Test_Features_TypesFuncs_SignatureHelp_ActiveParam(t *testing.T) {
	c := newTestClient(t)
	_ = c.initialize(t)
	uri := "file:///types_funcs.ms"
	c.didOpen(t, uri, fixtureTypesFuncs)
	time.Sleep(5 * time.Millisecond)

	// Before comma -> active param 0
	sh := c.signatureHelpAt(t, `80`, uri, 2, 8) // mk( |1, 2)
	if sh == nil || len(sh.Signatures) == 0 {
		t.Fatalf("expected signature help for mk (param 0)")
	}
	if sh.ActiveParameter != 0 {
		t.Fatalf("expected ActiveParameter=0 before comma, got %d", sh.ActiveParameter)
	}

	// After comma -> active param 1
	sh = c.signatureHelpAt(t, `81`, uri, 2, 12) // mk(1, |2)
	if sh == nil || len(sh.Signatures) == 0 {
		t.Fatalf("expected signature help for mk (param 1)")
	}
	if sh.ActiveParameter != 1 {
		t.Fatalf("expected ActiveParameter=1 after comma, got %d", sh.ActiveParameter)
	}
}

func Test_Features_DidChange_Reanalyzes(t *testing.T) {
	c := newTestClient(t)
	_ = c.initialize(t)
	uri := "file:///core_happy.ms"
	c.didOpen(t, uri, fixtureCoreHappy)
	time.Sleep(5 * time.Millisecond)

	// Now simulate an edit that introduces an unknown name.
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
	p.TextDocument.URI = uri
	p.TextDocument.Version = 2
	p.ContentChanges = []struct {
		Text string `json:"text"`
	}{
		{Text: fixtureCoreHappy + "\nlet q = missing\n"},
	}
	b, _ := json.Marshal(p)
	c.s.onDidChange(b)

	time.Sleep(5 * time.Millisecond)
	envs, err := decodeAll(c.out)
	if err != nil {
		t.Fatalf("decode after change: %v", err)
	}
	di := lastPublishDiagnostics(envs, uri)
	if di == nil {
		t.Fatalf("expected diagnostics after change")
	}
	found := false
	for _, d := range di.Diagnostics {
		if d.Code == "MS-UNKNOWN-NAME" {
			found = true
			break
		}
	}
	if !found {
		t.Fatalf("expected MS-UNKNOWN-NAME after change; got %#v", di.Diagnostics)
	}
}

func Test_Features_TypesFuncs_ParamDefinition_Links(t *testing.T) {
	c := newTestClient(t)
	_ = c.initialize(t)
	uri := "file:///types_funcs.ms"
	c.didOpen(t, uri, fixtureTypesFuncs)
	time.Sleep(5 * time.Millisecond)

	// Jump from use of parameter 'a' inside body to its definition in the param list.
	// line 1: let mk = fun(a: Int, b: Int) -> Pair do { left: a, right: b } end
	locs := c.definitionAt(t, `90`, uri, 1, 49) // on the 'a' inside the map value
	if len(locs) == 0 {
		t.Fatalf("expected definition location for param a from body use")
	}

	// Same for 'b'.
	locs = c.definitionAt(t, `91`, uri, 1, 61) // on the 'b' inside the map value
	if len(locs) == 0 {
		t.Fatalf("expected definition location for param b from body use")
	}
}

func Test_Features_CoreHappy_Hover_ResultVarY(t *testing.T) {
	c := newTestClient(t)
	_ = c.initialize(t)
	uri := "file:///core_happy.ms"
	c.didOpen(t, uri, fixtureCoreHappy)
	time.Sleep(5 * time.Millisecond)

	// line 7: let y = inc(xs[0])
	h := c.hoverAt(t, `92`, uri, 7, 5) // on "y"
	if h == nil || !strings.Contains(h.Contents.Value, "y") {
		t.Fatalf("hover on y should show token 'y', got %#v", h)
	}
}

func Test_Features_SemanticTokens_Legend_Present(t *testing.T) {
	c := newTestClient(t)
	res := c.initialize(t)
	if res.Capabilities.SemanticTokensProvider == nil {
		t.Fatalf("expected semanticTokensProvider in capabilities")
	}
	legend := res.Capabilities.SemanticTokensProvider.Legend
	if len(legend.TokenTypes) == 0 {
		t.Fatalf("semantic tokens legend: expected token types")
	}
}

func Test_Features_References_ExcludeDeclaration(t *testing.T) {
	c := newTestClient(t)
	_ = c.initialize(t)
	uri := "file:///refs_tokens.ms"
	c.didOpen(t, uri, fixtureRefsTokens)
	time.Sleep(5 * time.Millisecond)

	// Helper that allows IncludeDeclaration toggle
	type params struct {
		TextDocument TextDocumentIdentifier `json:"textDocument"`
		Position     Position               `json:"position"`
		Context      struct {
			IncludeDeclaration bool `json:"includeDeclaration"`
		} `json:"context"`
	}
	call := func(includeDecl bool) []Location {
		var p params
		p.TextDocument.URI = uri
		p.Position = Position{Line: 4, Character: 0} // final 'total'
		p.Context.IncludeDeclaration = includeDecl
		b, _ := json.Marshal(p)
		jid := json.RawMessage(`100`)
		c.s.onReferences(jid, b)
		envs, _ := decodeAll(c.out)
		resp := findResponse(envs, jid)
		if resp == nil || len(resp.Result) == 0 || string(resp.Result) == "null" {
			return nil
		}
		var locs []Location
		_ = json.Unmarshal(resp.Result, &locs)
		return locs
	}

	withDecl := call(true)
	withoutDecl := call(false)
	if len(withDecl) == 0 || len(withoutDecl) == 0 {
		t.Fatalf("expected references with and without declaration")
	}
	if !(len(withDecl) > len(withoutDecl)) {
		t.Fatalf("expected fewer refs when excluding declaration: with=%d without=%d", len(withDecl), len(withoutDecl))
	}
}
