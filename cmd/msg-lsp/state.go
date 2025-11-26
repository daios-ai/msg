// state.go
//
// ROLE: Server/document data structures and minimal lifecycle helpers.
//       This file defines the shared model (server, documents, symbol/binding
//       records) and provides a constructor plus a read-only snapshot method.
//
// What lives here
//   • server/doc structs, mutexes, interpreter handle.
//   • Lightweight records used across features (bindingDef).
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
	"unicode/utf8"

	mindscript "github.com/daios-ai/msg/internal/mindscript"
)

// bindingDef: single source of truth for names.
//
// All bindings (ambient + file-local, params + lets + funs/types) use this
// shape. It is byte-span based and LSP-agnostic; features derive Ranges/views
// as needed.
type bindingDef struct {
	Name      string
	StartByte int  // defining span start (byte offset)
	EndByte   int  // defining span end (byte offset)
	HasRange  bool // false for ambient/builtins: never publish a fake definition range

	DocFull  string
	DocFirst string

	// Enriched info for hover/completion/signatures.
	TypeNode []any // synthesized/static type or declared type

	IsTopLevel bool // true for real top-level bindings in this file
}

// docState: per-document caches (populated by analysis).
type docState struct {
	uri   string
	text  string
	lines []int // line start offsets (byte indices)

	tokens []mindscript.Token
	ast    mindscript.S
	spans  *mindscript.SpanIndex

	// All bindings (ambient + locals). Single source of truth for names/types/docs.
	binds []bindingDef
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
	if d.binds != nil {
		cp.binds = append([]bindingDef(nil), d.binds...)
	}

	// AST and SpanIndex are treated as immutable by readers.
	cp.ast = d.ast
	cp.spans = d.spans

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

// posToOffset converts an LSP Position (UTF-16 code units) to a byte offset.
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

// offsetToPos converts a byte offset to an LSP Position (UTF-16 code units).
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
