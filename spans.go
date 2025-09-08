// spans.go — Sidecar spans for MindScript ASTs (S-expressions)
//
// WHAT THIS MODULE DOES
// =====================
// This module provides a tiny, non-invasive mechanism to associate **source-code
// byte spans** with nodes of a MindScript AST (encoded as the S-expression type
// `S` from parser.go) **without modifying the AST itself**.
//
// The spans are modeled as half-open byte intervals `[StartByte, EndByte)`
// relative to the original UTF-8 source. Line/column coordinates are intentionally
// omitted here to keep the structure minimal; callers can derive them on demand
// from the original source text.
//
// HOW SPANS ARE ASSOCIATED TO NODES
// =================================
// We use a *sidecar* structure (`SpanIndex`) keyed by a stable, structural
// address called a **NodePath**. A `NodePath` is a slice of child indexes
// into the AST tree: e.g. `[]int{0,2,1}` means “root’s 0th child → its 2nd
// child → its 1st child”. Paths are defined against the S-expression shape
// where a node is `[]any{tagString, child0, child1, ...}` — i.e. the first
// element is the string tag, and child index 0 refers to the element at
// S[1], child index 1 refers to S[2], etc.
//
// This file does **not** compute spans itself. Instead, the parser (or any
// external producer) records one `Span` per AST node in **post-order**
// (children before parent) while constructing or inspecting the tree, and
// then calls `BuildSpanIndexPostOrder(ast, spans)` to bind those spans to
// concrete paths via a deterministic walk of the AST in the same order.
//
// The result is a `SpanIndex` you can query with a `NodePath` to retrieve
// the associated byte interval in the original source.
//
// DEPENDENCIES ON OTHER FILES
// ===========================
// • parser.go
//   - Defines the S-expression node type alias `type S = []any`.
//   - Produces the AST that this module indexes.
//   - (Optional instrumentation) While parsing, record a `Span` per finished
//     node in **post-order** (children first, then the node) using the token
//     byte spans collected by the lexer.
//
// • lexer.go
//   - Tokens should carry precise byte offsets (`StartByte`/`EndByte`) so that
//     the parser can compute node spans as:
//     node.StartByte = firstToken.StartByte
//     node.EndByte   = lastToken.EndByte
//
// PERFORMANCE & CONCURRENCY
// =========================
// Building an index is O(n) in the number of AST nodes. `SpanIndex` is
// read-only after construction and safe to share for concurrent reads.
// Memory usage is one map entry per node (string key per `NodePath`).
//
// PUBLIC VS PRIVATE LAYOUT
// ========================
// The file is split into a PUBLIC API (types and functions you call) and a
// PRIVATE section (helpers and internal details). The PUBLIC API is fully
// documented so its behavior is understandable without reading the PRIVATE
// code.
//
// ─────────────────────────────────────────────────────────────────────────────
package mindscript

import (
	"strconv"
	"strings"
)

////////////////////////////////////////////////////////////////////////////////
//                                  PUBLIC API
////////////////////////////////////////////////////////////////////////////////

// Span represents a half-open byte interval [StartByte, EndByte) in the original
// source text. Offsets are counted in bytes from the start of the UTF-8 source.
// EndByte is exclusive.
//
// Line/column coordinates are not stored here to keep Span minimal; if you need
// them, compute (line, col) from the original source using your preferred mapping.
type Span struct {
	StartByte int // inclusive
	EndByte   int // exclusive
}

// NodePath is a stable structural address into an S-expression AST.
// Each integer selects a child in the node's children array:
//
//	path element k  → child at S[k+1] (since S[0] is the string tag).
//
// Example:
//
//	// ("call", callee, arg0, arg1)
//	//  tag   ^      ^ child0 ^ child1
//	//  S[0]        S[1]      S[2]     S[3]
//	path []int{0}   → callee
//	path []int{2}   → arg1
type NodePath []int

// SpanIndex stores a sidecar mapping from NodePath → Span for an AST.
// It is read-only after construction. Use Get to retrieve spans by path.
//
// Typical construction flow (performed by the parser or a post-pass):
//  1. Walk/construct the AST while recording one Span per node in post-order.
//  2. Call BuildSpanIndexPostOrder(ast, postorderSpans) to bind spans to paths.
//  3. Query with si.Get(path) wherever you need source intervals.
type SpanIndex struct {
	byPath map[string]Span
}

// Get returns the span associated with the given path, if present.
// The boolean is false if the path is unknown or the index is nil.
//
// A SpanIndex may be partial (e.g., producer skipped some nodes). In that case
// only the recorded nodes will resolve to spans.
func (si *SpanIndex) Get(p NodePath) (Span, bool) {
	if si == nil {
		return Span{}, false
	}
	sp, ok := si.byPath[pathKey(p)]
	return sp, ok
}

// BuildSpanIndexPostOrder constructs a SpanIndex by walking the AST in
// **post-order** (children before parent) and binding each visited node to
// the next span from the provided `postorder` slice.
//
// Contract:
//   - The `postorder` slice must list exactly one Span for each node in `root`
//     in post-order. If it is longer, extras are ignored; if shorter, remaining
//     nodes are left unindexed (Get will return (Span{}, false) for them).
//   - The resulting index is read-only and safe for concurrent reads.
//
// Complexity: O(n) time and O(n) space where n is the number of AST nodes.
//
// Example usage (parser instrumentation idea):
//
//	// During parse, for each finished node (after parsing children):
//	//   spans = append(spans, Span{StartByte:firstTok.StartByte, EndByte:lastTok.EndByte})
//	idx := BuildSpanIndexPostOrder(ast, spans)
//	sp, ok := idx.Get(NodePath{0,2}) // lookup "child 0's child 2"
func BuildSpanIndexPostOrder(root S, postorder []Span) *SpanIndex {
	si := &SpanIndex{byPath: make(map[string]Span, len(postorder))}
	bindPostOrder(si, root, postorder)
	return si
}

//// END_OF_PUBLIC

////////////////////////////////////////////////////////////////////////////////
//                                 PRIVATE
////////////////////////////////////////////////////////////////////////////////

// pathKey serializes a NodePath to a compact "a.b.c" string used as the map key.
func pathKey(p NodePath) string {
	if len(p) == 0 {
		return ""
	}
	var sb strings.Builder
	for i, x := range p {
		if i > 0 {
			sb.WriteByte('.')
		}
		sb.WriteString(strconv.Itoa(x))
	}
	return sb.String()
}

// bindPostOrder walks `root` in post-order, assigning spans from `postorder`
// to each visited node, in order.
func bindPostOrder(si *SpanIndex, root S, postorder []Span) {
	i := 0
	var walk func(n S, path NodePath)
	walk = func(n S, path NodePath) {
		// Visit children
		for ci := 1; ci < len(n); ci++ {
			if child, ok := n[ci].(S); ok {
				walk(child, append(path, ci-1))
			}
		}
		// Bind this node
		if i < len(postorder) {
			si.byPath[pathKey(path)] = postorder[i]
			i++
		}
	}
	walk(root, nil)
}

// wrapUnderModule adapts a body SpanIndex to the AST:
//
//	("module", ("str", canonName), body)
//
// Paths shift under child #1; we also add spans for "" (module) and "0" (name).
func wrapUnderModule(body *SpanIndex) *SpanIndex {
	if body == nil {
		return nil
	}
	out := &SpanIndex{byPath: make(map[string]Span, len(body.byPath)+2)}

	// Body root span (old path ""), reuse it for the module root as well.
	root, _ := body.Get(nil)
	out.byPath[""] = root // module node span

	// Name node at child 0: a zero-length span at the start of the file/body.
	out.byPath["0"] = Span{StartByte: root.StartByte, EndByte: root.StartByte}

	// Shift all body paths under child 1: ""→"1", "a.b"→"1.a.b".
	for k, sp := range body.byPath {
		if k == "" {
			out.byPath["1"] = sp
		} else {
			out.byPath["1."+k] = sp
		}
	}
	return out
}
