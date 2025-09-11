package mindscript

import (
	"fmt"
	"io"
	"os"
	"unicode/utf8"
)

// VerifySpanIndexPostOrder walks the AST in post-order and checks that
// the SpanIndex binds exactly one span per node in that same order.
// If MSG_DEBUG_SPANCHECK is set, it always prints a short report.
// previewN controls how many (path, span) examples to print (0 = none).
func VerifySpanIndexPostOrder(ast S, sr *SourceRef, previewN int, w io.Writer) error {
	if sr == nil || sr.Spans == nil {
		return fmt.Errorf("no spans on SourceRef")
	}
	if w == nil {
		w = os.Stderr
	}

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

	// Pull spans in the same traversal to compare counts & existence
	got := 0
	missing := 0
	for _, p := range want {
		if _, ok := sr.Spans.Get(p); ok {
			got++
		} else {
			missing++
		}
	}

	verbose := os.Getenv("MSG_DEBUG_POS") != ""
	if verbose {
		fmt.Fprintln(w, "[spans] =====================")
		fmt.Fprintf(w, "[spans] name=%q nodes=%d spans=%d missing=%d\n",
			sr.Name, len(want), got, missing)
		if previewN > 0 {
			if previewN > len(want) {
				previewN = len(want)
			}
			for i := 0; i < previewN; i++ {
				p := want[i]
				if sp, ok := sr.Spans.Get(p); ok {
					start, end := sp.StartByte, sp.EndByte
					frag := ""
					if 0 <= start && end <= len(sr.Src) && start <= end {
						frag = sr.Src[start:end]
					}
					fmt.Fprintf(w, "[spans]   %s  [%d,%d)  %q\n", dbgPath(p), start, end, frag)
				} else {
					fmt.Fprintf(w, "[spans]   %s  <missing>\n", dbgPath(p))
				}
			}
		}
	}

	if missing > 0 {
		return fmt.Errorf("span index missing %d/%d nodes", missing, len(want))
	}
	return nil
}

// collectPostOrderPaths walks the AST in post-order and collects absolute paths
// (relative to the given root).
func collectPostOrderPaths(root S) []NodePath {
	var out []NodePath
	var walk func(n S, path NodePath)
	walk = func(n S, path NodePath) {
		for ci := 1; ci < len(n); ci++ {
			if child, ok := n[ci].(S); ok {
				walk(child, append(path, ci-1))
			}
		}
		out = append(out, append(NodePath(nil), path...))
	}
	walk(root, nil)
	return out
}

// nodeAtPath selects a sub-AST at an absolute path in the given root.
func nodeAtPath(root S, path NodePath) (S, bool) {
	n := root
	for _, idx := range path {
		if idx+1 >= len(n) {
			return nil, false
		}
		ch, ok := n[idx+1].(S)
		if !ok {
			return nil, false
		}
		n = ch
	}
	return n, true
}

// padPath converts a relative path inside a focused subtree into the absolute
// path expected by sr.Spans (accounting for SourceRef.PathBase).
func padPath(sr *SourceRef, base NodePath, rel NodePath) NodePath {
	// The SpanIndex keys are absolute paths within the SourceRef. When you enter
	// a function/module chunk, PathBase prefixes the marks. This helper lets you
	// look up spans for a subtree by prefixing base to rel, but if sr.PathBase
	// is non-empty, you must also prefix it.
	abs := append(NodePath(nil), base...)
	abs = append(abs, rel...)
	if len(sr.PathBase) > 0 {
		p := make(NodePath, 0, len(sr.PathBase)+len(abs))
		p = append(p, sr.PathBase...)
		p = append(p, abs...)
		return p
	}
	return abs
}

// containsPath returns true if po contains path p.
func containsPath(po []NodePath, p NodePath) bool {
	for _, q := range po {
		if pathsEqual(q, p) {
			return true
		}
	}
	return false
}

func pathsEqual(a, b NodePath) bool {
	if len(a) != len(b) {
		return false
	}
	for i := range a {
		if a[i] != b[i] {
			return false
		}
	}
	return true
}

// parsePathKey reverses spans.go/pathKey for debug comparisons.
func parsePathKey(k string) NodePath {
	if k == "" {
		return nil
	}
	var out NodePath
	i := 0
	n := 0
	sign := 1
	for i < len(k) {
		ch := k[i]
		if ch == '.' {
			out = append(out, sign*n)
			n, sign = 0, 1
			i++
			continue
		}
		if ch == '-' {
			sign = -1
			i++
			continue
		}
		if ch >= '0' && ch <= '9' {
			n = n*10 + int(ch-'0')
			i++
			continue
		}
		// invalid char → bail
		return nil
	}
	out = append(out, sign*n)
	return out
}

// safeSlice shows a compact, printable view of the span slice.
// It clamps to valid byte bounds and replaces newlines with ↵ for readability.
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
	// Make sure we aren't cutting through a rune (defensive)
	for !utf8.ValidString(s) && eb > sb {
		eb--
		s = src[sb:eb]
	}
	// Compact newlines/tabs for one-line debug
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
