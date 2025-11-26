// cmd/msg-lsp/analysis.go
//
// MindScript Static Analyzer — Frontend (LSP-Facing Surface)
//
// PURPOSE
// -------
// This file exposes the *minimal surface* the LSP server and tests rely on to
// analyze one MindScript file and retrieve editor-facing artifacts. It acts as
// the thin public shim; all mechanics live below the separator.
//
// WHAT CALLERS GET HERE
// ---------------------
// • Diagnostics (`Diag`): code + message on byte spans in the current file.
// • Token sidecar (`TokenIndex`): dense, whole-file, sorted tokens with optional
//   semantic payloads for hover/def-use (symbol/type) and declaration flag.
// • File product (`FileIndex`): source text, AST, spans, root Env, tokens, and
//   diagnostics; reserved node-types map.
// • Single entrypoint (`Analyzer.Analyze`): parse + build root Env + delegate to
//   the private folder (abstract interpreter) to populate tokens/diags.
// • Tokenization glue (`TokenIndexFromLexer`): produce token sidecar from lexer.
//
// SHAPE & INVARIANTS (RELIED UPON BY LSP FEATURES)
// ------------------------------------------------
// TokenIndex:
//   - Entries cover all non-trivia tokens, sorted by half-open byte ranges.
//   - Find(pos) is a binary search over [Start, End).
//   - Payload is either: VTSymbol (analysis symbol), VTType (type value), or empty.
//   - IsDecl marks declaration token of a binding.
// FileIndex:
//   - RootEnv is a real Env chained to ambient prelude when available.
//   - Diags may be empty even on success.
//   - emitIdentAtStart(start, text, isDecl, payload) enriches the token that
//     *starts* at byte ‘start’; if no token starts there, an internal diag is
//     recorded (no synthetic tokens are created).
//
// SCOPE OF THIS FILE
// ------------------
// • Public (top): slim shims, stable types, and entrypoints required elsewhere.
// • Private (bottom): helpers, internal logic, implementation details.
//   The separator `//// END_OF_PUBIC` (intentional spelling) marks the split.

package main

import (
	mindscript "github.com/DAIOS-AI/msg/internal/mindscript"

	"fmt"
	"strings"
)

////////////////////////////////////////////////////////////////////////////////
// Public: minimal types & entrypoints (shims)
////////////////////////////////////////////////////////////////////////////////

// Diag is a single diagnostic attached to a byte range in the current file.
type Diag struct {
	StartByte int
	EndByte   int
	Code      string
	Message   string
}

// TokenKind is a coarse semantic category for tokens.
type TokenKind int

const (
	TokIdentifier TokenKind = iota
	TokKeyword
	TokLiteral
	TokOperator
	TokPunct
)

// TokenEntry is one indexed token with optional semantic payload.
type TokenEntry struct {
	Start   int
	End     int
	Kind    TokenKind
	Text    string
	IsDecl  bool
	Payload mindscript.Value // VTSymbol | VTType | zero
}

// TokenIndex is a dense, sorted token sidecar with binary-search lookup.
type TokenIndex struct {
	Entries []TokenEntry
}

// Find returns the token whose half-open byte span [Start, End) contains pos,
// or nil if pos falls in whitespace/gaps.
func (ti *TokenIndex) Find(pos int) *TokenEntry {
	n := len(ti.Entries)
	if n == 0 {
		return nil
	}
	lo, hi := 0, n
	for lo < hi {
		mid := (lo + hi) >> 1
		if ti.Entries[mid].End <= pos {
			lo = mid + 1
		} else {
			hi = mid
		}
	}
	if lo < n {
		e := &ti.Entries[lo]
		if e.Start <= pos && pos < e.End {
			return e
		}
	}
	return nil
}

// FileIndex is the per-file analysis product used by LSP features.
type FileIndex struct {
	URI       string
	Text      string
	AST       mindscript.S
	Spans     *mindscript.SpanIndex
	RootEnv   *mindscript.Env
	Tokens    TokenIndex
	NodeTypes map[any]mindscript.S
	Diags     []Diag
}

// Analyzer orchestrates ambient setup and one-file analysis.
type Analyzer struct {
	IP *mindscript.Interpreter
}

// Analyze parses, builds RootEnv (child of ambient), and delegates a single-pass
// abstract interpretation to populate tokens/types/diags.
func (a *Analyzer) Analyze(uri, text string) *FileIndex {
	idx := &FileIndex{URI: uri, Text: text}

	ip := a.ensureIP()

	ast, spans, err := mindscript.ParseSExprWithSpans(text)
	if err != nil {
		idx.addDiag(Diag{Code: "MS-PARSE", Message: err.Error()})
		return idx
	}
	idx.AST, idx.Spans = ast, spans

	// Build a file root env shadowing the ambient/global.
	// We deliberately do *not* modify ip.Global. Ambient seeding is performed lazily
	// by resolveName (analysis_engine.go) the first time a global is referenced,
	// ensuring a uniform VTSymbol/VTType surface without duplicate code paths.
	if ip != nil && ip.Global != nil {
		idx.RootEnv = mindscript.NewEnv(ip.Global)
	} else {
		idx.RootEnv = mindscript.NewEnv(nil)
	}

	// Build token sidecar with the file's real env so even synthesized payloads
	// (like literals) capture a non-nil defining environment.
	idx.Tokens = TokenIndexFromLexer(text, idx.RootEnv)

	fc := &foldCtx{idx: idx, ip: ip, env: idx.RootEnv, topLevel: true}
	fc.fold(idx.AST)
	return idx
}

// TokenIndexFromLexer builds a full-file TokenIndex from the lexer output.
// The env is required so any synthesized payloads get a proper defining env.
func TokenIndexFromLexer(text string, env *mindscript.Env) TokenIndex {
	ti := TokenIndex{}
	lex := mindscript.NewLexer(text)
	toks, err := lex.Scan()
	if err != nil {
		return ti
	}
	for _, tk := range toks {
		switch tk.Type {
		case mindscript.EOF, mindscript.NOOP, mindscript.ILLEGAL:
			continue
		}
		entry := TokenEntry{
			Start: tk.StartByte,
			End:   tk.EndByte,
			Kind:  tokenKindFromLexer(tk.Type),
			Text:  tk.Lexeme,
		}
		// Attach primitive literal static types to improve hover.
		switch tk.Type {
		case mindscript.STRING:
			entry.Payload = newSymbolValIn(env, mindscript.S{"id", "Str"}, "")
		case mindscript.INTEGER:
			entry.Payload = newSymbolValIn(env, mindscript.S{"id", "Int"}, "")
		case mindscript.NUMBER:
			entry.Payload = newSymbolValIn(env, mindscript.S{"id", "Num"}, "")
		case mindscript.BOOLEAN:
			entry.Payload = newSymbolValIn(env, mindscript.S{"id", "Bool"}, "")
		case mindscript.NULL:
			entry.Payload = newSymbolValIn(env, mindscript.S{"id", "Null"}, "")
		}
		ti.add(entry)
	}
	return ti
}

//// END_OF_PUBIC

////////////////////////////////////////////////////////////////////////////////
// Private: helpers, internal logic, implementation details
////////////////////////////////////////////////////////////////////////////////

// addDiag appends a diagnostic.
func (idx *FileIndex) addDiag(d Diag) { idx.Diags = append(idx.Diags, d) }

// add appends a token entry (kept in source order by the lexer).
func (ti *TokenIndex) add(e TokenEntry) { ti.Entries = append(ti.Entries, e) }

// emitIdentAtStart enriches the token that *starts* at byte `start`. On failure,
// records a single INTERNAL SPAN MISMATCH diagnostic with context.
func (idx *FileIndex) emitIdentAtStart(start int, text string, isDecl bool, payload mindscript.Value) {
	if te := idx.Tokens.Find(start); te != nil && te.Start == start {
		te.Kind, te.Text, te.IsDecl = TokIdentifier, text, isDecl
		if (payload != mindscript.Value{}) {
			te.Payload = payload
		}
		return
	}

	role := "use"
	if isDecl {
		role = "declaration"
	}
	var containingNote string
	if ct := idx.Tokens.Find(start); ct != nil {
		containingNote = fmt.Sprintf(` Containing token: %q [%d,%d).`, ct.Text, ct.Start, ct.End)
	}
	es := idx.Tokens.Entries
	anchor := len(es)
	for i := 0; i < len(es); i++ {
		if es[i].Start >= start {
			anchor = i
			break
		}
	}
	type tv struct {
		Text string
		S, E int
	}
	ctx := make([]tv, 0, 4)
	add := func(i int) {
		if i >= 0 && i < len(es) {
			ctx = append(ctx, tv{es[i].Text, es[i].Start, es[i].End})
		}
	}
	add(anchor - 2)
	add(anchor - 1)
	add(anchor)
	add(anchor + 1)

	var prevNext strings.Builder
	if len(ctx) > 0 {
		prevNext.WriteString(" Context tokens:")
		for i := range ctx {
			label := [...]string{" P2", " P1", " N1", " N2"}[i]
			fmt.Fprintf(&prevNext, `%s=%q [%d,%d)`, label, ctx[i].Text, ctx[i].S, ctx[i].E)
			if i != len(ctx)-1 {
				prevNext.WriteString(";")
			}
		}
	}

	idx.addDiag(Diag{
		StartByte: start,
		EndByte:   start,
		Code:      "MS-INTERNAL-SPAN-MISMATCH",
		Message: fmt.Sprintf(
			`INTERNAL SPAN MISMATCH: expected identifier %q (%s) to start at byte %d, but no token starts there.%s%s `+
				"This usually means the lexer token spans and parser spans disagree or stamping used a non-leaf span. Hover/definitions may be degraded. Please report this bug.",
			text, role, start, containingNote, prevNext.String()),
	})
}

// Analysis symbol summary (stored in Env).

// Symbol is the payload for Value{Tag: VTSymbol} (analysis-only).
type Symbol struct {
	Type mindscript.S
	Env  *mindscript.Env
}

// newSymbolValIn constructs a VTSymbol and records the defining Env for later
// type resolution (e.g., in hover). Always supply a non-nil env.
func newSymbolValIn(env *mindscript.Env, t mindscript.S, doc string) mindscript.Value {
	if env == nil {
		panic("newSymbolValIn: env must not be nil")
	}
	if t == nil {
		t = mindscript.S{"id", "Any"}
	}
	return mindscript.Value{Tag: mindscript.VTSymbol, Data: &Symbol{Type: t, Env: env}, Annot: doc}
}

// asSymbol views a Value as *Symbol when Tag==VTSymbol.
func asSymbol(v mindscript.Value) (*Symbol, bool) {
	s, ok := v.Data.(*Symbol)
	if v.Tag != mindscript.VTSymbol || !ok || s == nil {
		return nil, false
	}
	return s, true
}

// Analyzer internals.

func (a *Analyzer) ensureIP() *mindscript.Interpreter {
	if a == nil {
		return nil
	}
	if a.IP != nil {
		return a.IP
	}
	ip, err := mindscript.NewInterpreter()
	if err != nil {
		return nil
	}
	a.IP = ip
	return ip
}

// Tokenization glue.

func tokenKindFromLexer(tt mindscript.TokenType) TokenKind {
	switch tt {
	case mindscript.ID:
		return TokIdentifier
	case mindscript.STRING, mindscript.INTEGER, mindscript.NUMBER, mindscript.BOOLEAN, mindscript.NULL:
		return TokLiteral
	case mindscript.LET, mindscript.DO, mindscript.END, mindscript.RETURN,
		mindscript.BREAK, mindscript.CONTINUE, mindscript.IF, mindscript.THEN,
		mindscript.ELIF, mindscript.ELSE, mindscript.FUNCTION, mindscript.ORACLE,
		mindscript.MODULE, mindscript.FOR, mindscript.IN, mindscript.FROM, mindscript.WHILE,
		mindscript.TYPECONS, mindscript.TYPE, mindscript.ENUM:
		return TokKeyword
	case mindscript.PLUS, mindscript.MINUS, mindscript.MULT, mindscript.DIV, mindscript.MOD, mindscript.POW,
		mindscript.ASSIGN, mindscript.EQ, mindscript.NEQ, mindscript.LESS, mindscript.LESS_EQ,
		mindscript.GREATER, mindscript.GREATER_EQ, mindscript.LSHIFT, mindscript.RSHIFT,
		mindscript.BITAND, mindscript.BITXOR, mindscript.BITOR, mindscript.BITNOT,
		mindscript.AND, mindscript.OR, mindscript.NOT, mindscript.BANG, mindscript.QUESTION, mindscript.ARROW:
		return TokOperator
	case mindscript.LROUND, mindscript.CLROUND, mindscript.RROUND,
		mindscript.LSQUARE, mindscript.CLSQUARE, mindscript.RSQUARE,
		mindscript.LCURLY, mindscript.RCURLY, mindscript.NOOP,
		mindscript.COLON, mindscript.COMMA, mindscript.PERIOD:
		return TokPunct
	default:
		return TokPunct
	}
}
