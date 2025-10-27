// spans_debug.go — Debugging utilities for SpanIndex/mark mapping
//
// WHAT THIS MODULE DOES
// =====================
// This module centralizes **debugging-only** helpers for source span / mark
// inspection and validation in MindScript. It provides:
//
//   • A single public toggle, `DebuggingMode`, which is picked up at process
//     start from the `MSGDEBUG` environment variable. Hosts may also set it
//     programmatically (tests, REPLs).
//
//   • A public verifier, `VerifySpanIndexPostOrder`, that checks the critical
//     invariant used by caret positioning: the parser must record **exactly
//     one span per AST node in post-order**. The function walks the AST in
//     post-order, ensures each node path exists in the SpanIndex, and can print
//     a compact preview of the first N bindings for inspection.
//
//   • Private, verbose dump routines (e.g., `dumpSourcePosDebug`) used by the
//     runtime solely when `DebuggingMode` is true, to explain how bytecode
//     program counters (PC) map back to AST paths and then to byte spans.
//
// DEPENDENCIES / INTEGRATION POINTS
// =================================
//   • parser.go
//       - Defines the S-expression node type `type S = []any` and constructs
//         ASTs consumed by this module.
//       - Produces post-order span instrumentation which is wrapped into a
//         `SpanIndex` (see spans.go).
//
//   • spans.go
//       - Defines `Span`, `NodePath`, `SpanIndex`, and the post-order binding
//         function `BuildSpanIndexPostOrder`. This module *reads* the SpanIndex
//         (no mutations) to verify integrity and to print diagnostics.
//
//   • vm.go / interpreter_exec.go
//       - VM marks (PC→NodePath) and caret mapping call into a single private
//         function `dumpSourcePosDebug` from `sourcePosFromChunk` **only when**
//         `DebuggingMode` is true. That keeps the hot path clean while still
//         allowing rich debugging output on demand.
//
// PUBLIC VS PRIVATE
// =================
// PUBLIC  : `DebuggingMode` and `VerifySpanIndexPostOrder`
//           (fully documented below; small surface; no heavy logic here)
// PRIVATE : Everything else (dumpers, walkers, stringifiers). These are helpers
//           invoked from the VM/caret path when debugging is enabled.
//
// Concurrency: All helpers are read-only over their inputs and print to an
// `io.Writer` or `os.Stderr`. They keep no global mutable state (aside from the
// public `DebuggingMode` flag which callers may set early during process init).

package mindscript

import (
	"fmt"
	"io"
	"os"
	"sort"
	"unicode/utf8"
)

////////////////////////////////////////////////////////////////////////////////
//                                  PUBLIC API
////////////////////////////////////////////////////////////////////////////////

// DebuggingMode controls whether verbose span/mark diagnostics are emitted
// by the runtime. It is initialized from the environment variable `MSGDEBUG`
// at process start. Hosts and tests may override it programmatically.
//
// Typical usage:
//
//	if DebuggingMode {
//	    // call lightweight verifiers or allow verbose dumps
//	}
//
// NOTE: Production code should *not* branch on environment variables directly;
// use this flag as the single source of truth.
var DebuggingMode = os.Getenv("MSGDEBUG") != ""

// VerifySpanIndexPostOrder walks the given AST (`ast`) in **post-order** and
// checks that the provided SourceRef (`sr`) has a `SpanIndex` which binds
// **exactly one span per AST node** in that same order.
//
// Behavior:
//   - Returns `nil` when every AST node has a corresponding span in `sr.Spans`.
//   - Returns an error of the form "span index missing X/Y nodes" when any
//     post-order node path is absent from the index.
//   - If `w` is non-nil, it prints a short report header and previews up to
//     `previewN` (path, span) examples (clamped to the available nodes).
//
// Printing is intended for debugging sessions and tests. Production callers
// typically pass `w=nil` (or gate calls on `DebuggingMode`) to avoid output.
//
// Parameters:
//   - ast       : Root S-expression AST to verify.
//   - sr        : SourceRef containing the source text and SpanIndex (required).
//   - previewN  : Number of (path, span) preview lines to print; 0 disables.
//   - w         : Optional writer for the preview; defaults to os.Stderr when nil.
//
// This function does not mutate inputs and does not depend on global state.
func VerifySpanIndexPostOrder(ast S, sr *SourceRef, previewN int, w io.Writer) error {
	if sr == nil || sr.Spans == nil {
		return fmt.Errorf("no spans on SourceRef")
	}
	if w == nil {
		w = os.Stderr
	}

	// Collect desired post-order paths from the AST.
	var want []NodePath
	var walk func(n S, path NodePath)
	walk = func(n S, path NodePath) {
		for ci := 1; ci < len(n); ci++ {
			if c, ok := n[ci].(S); ok {
				walk(c, append(path, ci-1))
			}
		}
		want = append(want, append(NodePath(nil), path...))
	}
	walk(ast, nil)

	// Probe spans for each desired path.
	got, missing := 0, 0
	for _, p := range want {
		if _, ok := sr.Spans.Get(p); ok {
			got++
		} else {
			missing++
		}
	}

	// Optional compact preview.
	if previewN > 0 {
		if previewN > len(want) {
			previewN = len(want)
		}
		fmt.Fprintln(w, "[spans] =====================")
		fmt.Fprintf(w, "[spans] name=%q nodes=%d spans=%d missing=%d\n",
			sr.Name, len(want), got, missing)
		for i := 0; i < previewN; i++ {
			p := want[i]
			if sp, ok := sr.Spans.Get(p); ok {
				fmt.Fprintf(w, "[spans]   %s  [%d,%d)  %q\n",
					dbgPath(p), sp.StartByte, sp.EndByte, safeSlice(sr.Src, sp))
			} else {
				fmt.Fprintf(w, "[spans]   %s  <missing>\n", dbgPath(p))
			}
		}
	}

	if missing > 0 {
		return fmt.Errorf("span index missing %d/%d nodes", missing, len(want))
	}
	return nil
}

//// END_OF_PUBLIC

////////////////////////////////////////////////////////////////////////////////
//                               PRIVATE IMPLEMENTATION
////////////////////////////////////////////////////////////////////////////////

// dumpSourcePosDebug prints detailed diagnostics about mark ↔ span mapping.
// Used by sourcePosFromChunk *only when* DebuggingMode == true.
//
// It does NOT influence control flow; it’s purely for inspection.
func dumpSourcePosDebug(ch *Chunk, sr *SourceRef, pc int) {
	fmt.Fprintf(os.Stderr, "\n[posmap] =====================\n")
	fmt.Fprintf(os.Stderr, "[posmap] pc=%d\n", pc)

	if sr != nil {
		fmt.Fprintf(os.Stderr, "[posmap] SourceRef ptr=%p name=%q spans=%t pathBase=%s\n",
			sr, sr.Name, sr.Spans != nil, dbgPath(sr.PathBase))
	} else {
		fmt.Fprintf(os.Stderr, "[posmap] SourceRef=<nil>\n")
	}
	if ch != nil {
		fmt.Fprintf(os.Stderr, "[posmap] Chunk: code=%d consts=%d marks=%d\n", len(ch.Code), len(ch.Consts), len(ch.Marks))
	} else {
		fmt.Fprintf(os.Stderr, "[posmap] Chunk=<nil>\n")
	}

	// Span dump
	if sr != nil && sr.Spans != nil {
		dbgDumpAllSpans(sr)
	} else {
		fmt.Fprintf(os.Stderr, "[posmap] NO SPANS to dump\n")
	}

	// If anything crucial is missing, stop here.
	if ch == nil || sr == nil || sr.Spans == nil || len(ch.Marks) == 0 || sr.Src == "" {
		fmt.Fprintf(os.Stderr, "[posmap] early fallback to (1,1)\n")
		return
	}

	// Compute best mark index (last with PC <= pc)
	i := -1
	for j := range ch.Marks {
		if ch.Marks[j].PC <= pc {
			i = j
		} else {
			break
		}
	}
	fmt.Fprintf(os.Stderr, "[posmap] bestMarkIndex=%d (total marks=%d)\n", i, len(ch.Marks))
	if i >= 0 {
		start := i - 3
		if start < 0 {
			start = 0
		}
		end := i + 3
		if end >= len(ch.Marks) {
			end = len(ch.Marks) - 1
		}
		for k := start; k <= end && k >= 0; k++ {
			m := ch.Marks[k]
			cur := ""
			if k == i {
				cur = "  <<"
			}
			fmt.Fprintf(os.Stderr, "[posmap]   mark[%d]: PC=%d path=%s%s\n", k, m.PC, dbgPath(m.Path), cur)
		}
	}

	// Try to resolve the best mark (and show hits/misses) without affecting the main logic.
	tryPathDebug := func(p NodePath, label string) bool {
		src := sr.Src
		for cut := len(p); cut >= 0; cut-- {
			sub := p[:cut]
			if sp, ok := sr.Spans.Get(sub); ok {
				line, col := offsetToLineCol(src, sp.StartByte)
				fmt.Fprintf(os.Stderr, "[posmap] HIT  %s path=%s  span=[%d,%d)  -> line=%d col=%d\n",
					label, dbgPath(sub), sp.StartByte, sp.EndByte, line, col)
				if sp.StartByte >= 0 && sp.EndByte <= len(src) && sp.StartByte <= sp.EndByte {
					snippet := src[sp.StartByte:sp.EndByte]
					fmt.Fprintf(os.Stderr, "[posmap] HIT  source[%d:%d]:\n-----8<-----\n%s\n----->8-----\n",
						sp.StartByte, sp.EndByte, snippet)
				} else {
					fmt.Fprintf(os.Stderr, "[posmap] HIT  source slice out of bounds!\n")
				}
				return true
			}
			fmt.Fprintf(os.Stderr, "[posmap] MISS %s path=%s\n", label, dbgPath(sub))
		}
		return false
	}

	if i >= 0 {
		if !tryPathDebug(ch.Marks[i].Path, "mark") {
			for k := i - 1; k >= 0; k-- {
				if tryPathDebug(ch.Marks[k].Path, fmt.Sprintf("earlier[%d]", k)) {
					return
				}
			}
			fmt.Fprintf(os.Stderr, "[posmap] all lookups failed; fallback to (1,1)\n")
		}
	}
}

// ---- Private helpers --------------------------------------------------------

func dbgPath(p NodePath) string {
	if len(p) == 0 {
		return "<root>"
	}
	out := make([]byte, 0, 32)
	for i, x := range p {
		if i > 0 {
			out = append(out, '.')
		}
		out = append(out, []byte(fmt.Sprintf("%d", x))...)
	}
	return string(out)
}

func dbgDumpAllSpans(sr *SourceRef) {
	fmt.Fprintf(os.Stderr, "[posmap] ---- SPAN TREE DUMP (name=%q) ----\n", sr.Name)
	if sr.Spans == nil {
		fmt.Fprintf(os.Stderr, "[posmap] (no spans)\n")
		return
	}
	keys := make([]string, 0, len(sr.Spans.byPath))
	for k := range sr.Spans.byPath {
		keys = append(keys, k)
	}
	sort.Slice(keys, func(i, j int) bool {
		di, dj := 0, 0
		for c := 0; c < len(keys[i]); c++ {
			if keys[i][c] == '.' {
				di++
			}
		}
		for c := 0; c < len(keys[j]); c++ {
			if keys[j][c] == '.' {
				dj++
			}
		}
		if di != dj {
			return di < dj
		}
		return keys[i] < keys[j]
	})
	for _, k := range keys {
		sp := sr.Spans.byPath[k]
		pathStr := k
		if pathStr == "" {
			pathStr = "<root>"
		}
		fmt.Fprintf(os.Stderr, "[posmap] span path=%s  [start=%d end=%d)\n", pathStr, sp.StartByte, sp.EndByte)
		if sp.StartByte >= 0 && sp.EndByte <= len(sr.Src) && sp.StartByte <= sp.EndByte {
			snippet := sr.Src[sp.StartByte:sp.EndByte]
			fmt.Fprintf(os.Stderr, "[posmap] source[%d:%d]:\n-----8<-----\n%s\n----->8-----\n", sp.StartByte, sp.EndByte, snippet)
		} else {
			fmt.Fprintf(os.Stderr, "[posmap] (invalid slice bounds for this span)\n")
		}
	}
	fmt.Fprintf(os.Stderr, "[posmap] ---- END SPAN TREE ----\n")
}

// safeSlice shows a compact, printable view of the span slice.
// It clamps to valid byte bounds and replaces newlines/tabs with symbols for readability.
func safeSlice(src string, sp Span) string {
	sb, eb := sp.StartByte, sp.EndByte
	if sb < 0 {
		sb = 0
	}
	if eb < sb {
		eb = sb
	}
	if eb > len(src) {
		eb = len(src)
	}
	s := src[sb:eb]
	// Ensure we don't cut through a rune (defensive)
	for !utf8.ValidString(s) && eb > sb {
		eb--
		s = src[sb:eb]
	}
	// Compact newlines/tabs
	out := make([]rune, 0, len(s))
	for _, r := range s {
		switch r {
		case '\n':
			out = append(out, '↵')
		case '\t':
			out = append(out, '⇥')
		default:
			out = append(out, r)
		}
	}
	return string(out)
}
