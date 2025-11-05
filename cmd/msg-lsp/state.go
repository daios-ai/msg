// state.go
//
// ROLE: Server/document data structures and minimal lifecycle helpers.
//       This file defines the shared model (server, documents, symbol/binding
//       records) and provides a constructor plus a read-only snapshot method.
//
// What lives here
//   • server/doc structs, mutexes, interpreter handle.
//   • Lightweight records used across features (symbolDef, bindingDef).
//   • newServer() to construct the server.
//   • (*server).snapshotDoc() to take a safe, read-only copy of a document.
//
// What does NOT live here
//   • No transport/framing (readMsg/writeMsg), no send/notify helpers.
//   • No analysis logic (lex/parse, spans, diagnostics).
//   • No LSP feature handlers (hover/definition/etc.).
//
// Why this separation
//   • Keeps state/model stable and import-light.
//   • Analysis and features can depend on this without dragging in transport.

package main

import (
	"sync"

	mindscript "github.com/DAIOS-AI/msg/internal/mindscript"
)

// symbolDef: top-level symbols shown in document symbols (and used by features).
type symbolDef struct {
	Name  string
	Kind  string // "let" | "fun" | "type"
	Range Range  // where it's declared
	Doc   string // first line, if available
	Sig   string // pretty signature for fun/oracle
}

// bindingDef: any assignment to a name (decl/id) anywhere in the file.
// Docs come uniformly from the VALUE's annotation wrapper if present.
type bindingDef struct {
	Name     string
	Range    Range // span of the defining identifier token
	DocFull  string
	DocFirst string
	Kind     string // "let" | "fun" | "oracle" | "type" | "param" | "" (best-effort)
	// Enriched info for hover/completion
	TypeNode   []any  // synthesized/static type (vars/params) or declared return (fun/oracle/type)
	Sig        string // pretty signature for fun/oracle
	IsTopLevel bool
}

// docState: per-document caches (populated by analysis).
type docState struct {
	uri     string
	text    string
	lines   []int // line start offsets (byte indices)
	symbols []symbolDef
	tokens  []mindscript.Token
	ast     mindscript.S
	spans   *mindscript.SpanIndex
	binds   []bindingDef // all bindings collected uniformly
}

// server: global state for the LSP server.
type server struct {
	mu   sync.RWMutex
	docs map[string]*docState
	ip   *mindscript.Interpreter
}

// newServer constructs a server with an interpreter handle for metadata queries.
// (Core analysis never executes user programs; the interpreter is used only to
// read builtin/global metadata when formatting certain answers.)
func newServer() *server {
	ip, _ := mindscript.NewInterpreter()
	return &server{
		docs: make(map[string]*docState),
		ip:   ip,
	}
}

// snapshotDoc returns a consistent, read-only snapshot of a document.
// Slices are shallow-copied so readers can't race on mutation.
func (s *server) snapshotDoc(uri string) *docState {
	s.mu.RLock()
	defer s.mu.RUnlock()
	d := s.docs[uri]
	if d == nil {
		return nil
	}
	cp := *d // shallow copy of struct

	// Copy slices so callers can't mutate underlying arrays.
	if d.lines != nil {
		cp.lines = append([]int(nil), d.lines...)
	}
	if d.tokens != nil {
		cp.tokens = append([]mindscript.Token(nil), d.tokens...)
	}
	if d.symbols != nil {
		cp.symbols = append([]symbolDef(nil), d.symbols...)
	}
	if d.binds != nil {
		cp.binds = append([]bindingDef(nil), d.binds...)
	}

	// AST and SpanIndex are treated as immutable by readers.
	cp.ast = d.ast
	cp.spans = d.spans

	return &cp
}
