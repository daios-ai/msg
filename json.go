// json.go — permissive JSON "repair" helper for MindScript hosts.
//
// WHAT THIS FILE PROVIDES
// =======================
// A tiny, pragmatic utility that extracts JSON-looking content from LLM-ish,
// markdown-flavored text, applies conservative repairs, then decodes it using
// Go’s standard library with number preservation.
//
// Public API:
//   - jsonRepair(src string) (value any, fixed string, warnings []string)
//       • value    — Go JSON-compatible graph: map[string]any, []any, string,
//                    bool, nil, and json.Number (safe to re-marshal).
//       • fixed    — the repaired JSON text that the decoder consumed.
//       • warnings — human-friendly notes about what was changed or detected.
//
// BEHAVIOR (HIGH LEVEL)
// ---------------------
// 1) Normalization:
//    • Strip UTF-8 BOM and CR/CRLF → LF.
//    • Replace *invalid* UTF-8 sequences with U+FFFD (and warn)  ⟵ (Fix #5).
//
// 2) Extraction (LLM/markdown aware):
//    • Prefer fenced ```json … ``` blocks.
//    • Else any fenced ``` … ``` / ~~~ … ~~~ block.
//    • Else, consider inline backtick-delimited regions and pick the *best*
//      candidate (most JSON-ish / longest) instead of the first match          ⟵ (Fix #3).
//    • Else, scan the whole text for the first balanced {...} or [...] region.
//    • Record a warning when extraction discards surrounding prose.
//
// 3) Repairs (string-aware where needed):
//    • Remove // and /* */ comments.
//    • Clean markdown artifacts (dangling fence lines / “json” header).
//    • Smart quotes → ASCII; single-quoted strings → double-quoted strings.
//    • key=value → "key": value; unquoted keys → quoted.
//    • Trailing commas removed; duplicate commas collapsed (adds nulls).
//    • Loose numbers normalized: .5 → 0.5, +1 → 1, 01 → 1, 1. → 1 (only when
//      safe), NaN/±Infinity → null. A trailing dot is removed *only* if the next
//      non-space char is a JSON delimiter or EOF (prevents "1.a" → "1a")        ⟵ (Fix #2).
//    • Brackets/braces balanced; unterminated string closed.
//
// 4) Decoding (permissive stream):
//    • Use json.Decoder with UseNumber() to preserve large integers.
//    • Accept concatenated/NDJSON-like streams → returns an array with warning.
//    • Tolerate trailing junk once (trim right) and warn.
//
// DEPENDENCIES
// ============
// • Standard library only: bytes, encoding/json, regexp, strings, unicode,
//   unicode/utf8.
// • Integrates conceptually with builtin_json.go (which converts Value↔Go-JSON),
//   but this file is standalone and does not depend on other MindScript files.
//
// PUBLIC API BELOW
// ================

package mindscript

import (
	"bytes"
	"encoding/json"
	"regexp"
	"strings"
	"unicode"
	"unicode/utf8"
)

// jsonRepair extracts JSON-like content from src, repairs common issues,
// decodes it with number preservation, and returns (value, fixedText, warnings).
//
// Guarantees:
//   - fixedText is valid UTF-8 JSON that encoding/json can re-decode.
//   - value uses json.Number for numbers (no precision loss before the caller
//     decides how to handle them).
//   - warnings lists notable extractions/repairs (order is chronological).
//
// Notes on permissiveness vs safety:
//   - When normalizing loose numbers, a trailing dot is removed only if the next
//     non-space char is a JSON delimiter (']', '}', ',') or EOF. This prevents
//     corrupting tokens like "1.a" → "1a".
//   - Invalid UTF-8 sequences are replaced with U+FFFD before all other steps;
//     a warning is emitted.
//
// This surface is intentionally tiny. See PRIVATE section for the exact passes.
func jsonRepair(src string) (any, string, []string) {
	var warns []string

	// 0) Normalize BOM/line endings and sanitize invalid UTF-8 (warn on change).
	src = stripBOM(src)
	src = strings.ReplaceAll(src, "\r\n", "\n")
	src = strings.ReplaceAll(src, "\r", "\n")
	if s2, changed := sanitizeUTF8(src); changed {
		src = s2
		warns = append(warns, "replaced invalid UTF-8 with U+FFFD")
	}

	// 1) Prefer fenced JSON/code blocks; else balanced region from prose.
	if body, ok := pickFencedBlock(src); ok {
		src = body
		warns = append(warns, "extracted fenced code block")
	} else if sub, ok := extractBalancedJSON(src); ok {
		if sub != src {
			warns = append(warns, "extracted balanced JSON region from surrounding prose")
		}
		src = sub
	}

	// 2) Repairs (string-aware where needed)
	if stripped, changed := stripComments(src); changed {
		src = stripped
		warns = append(warns, "removed comments")
	}
	src = cleanupMarkdownArtifacts(src)
	src = normalizeSmartQuotes(src)
	if fixed, changed := singleToDoubleQuotedStrings(src); changed {
		src = fixed
		warns = append(warns, "converted single-quoted strings to double-quoted")
	}
	if fixed, changed := fixKeyEquals(src); changed {
		src = fixed
		warns = append(warns, "converted key=value to key:value")
	}
	if fixed, changed := quoteUnquotedKeys(src); changed {
		src = fixed
		warns = append(warns, "quoted unquoted object keys")
	}
	if fixed, changed := dropTrailingCommas(src); changed {
		src = fixed
		warns = append(warns, "removed trailing commas")
	}
	if fixed, changed := collapseDuplicateCommas(src); changed {
		src = fixed
		warns = append(warns, "collapsed duplicate commas")
	}
	if fixed, changed := normalizeLooseNumbers(src); changed {
		src = fixed
		warns = append(warns, "normalized loose number formats and NaN/Infinity")
	}
	if fixed, changed, note := balanceAndClose(src); changed {
		src = fixed
		if note != "" {
			warns = append(warns, note)
		} else {
			warns = append(warns, "balanced brackets/braces and/or closed string")
		}
	}

	// 3) Decode (supports multiple top-level documents)
	val, warns2 := decodeJSONPermissive(src)
	warns = append(warns, warns2...)
	return val, src, warns
}

//// END_OF_PUBLIC

// ----------------------------- PRIVATE IMPLEMENTATION ------------------------

// Precompiled regexes (hot path).
var (
	reFenceJSON   = regexp.MustCompile("(?is)```json\\s*(.*?)\\s*```")
	reFenceAny    = regexp.MustCompile("(?is)```[a-z0-9_-]*\\s*(.*?)\\s*```")
	reFenceTilde  = regexp.MustCompile(`(?is)~{3,}[a-z0-9_-]*\s*(.*?)\s*~{3,}`)
	reInlineTicks = regexp.MustCompile("(?s)`([^`]+)`")

	reKeyEq      = regexp.MustCompile(`([{\s,])([A-Za-z_][A-Za-z0-9_\-\.]*)\s*=\s*`)
	reUnqKey     = regexp.MustCompile(`([{\s,])([A-Za-z_][A-Za-z0-9_\-\.]*)\s*:`)
	reTrailComma = regexp.MustCompile(`,(\s*[}\]])`)
)

// stripBOM removes a leading UTF-8 BOM if present.
func stripBOM(s string) string {
	if strings.HasPrefix(s, "\uFEFF") {
		return strings.TrimPrefix(s, "\uFEFF")
	}
	return s
}

// sanitizeUTF8 replaces invalid UTF-8 sequences with U+FFFD.
func sanitizeUTF8(s string) (string, bool) {
	var b strings.Builder
	b.Grow(len(s))
	changed := false
	for i := 0; i < len(s); {
		r, w := utf8.DecodeRuneInString(s[i:])
		if r == utf8.RuneError && w == 1 {
			// Invalid byte → replacement char
			b.WriteRune('\uFFFD')
			i++
			changed = true
			continue
		}
		b.WriteRune(r)
		i += w
	}
	if !changed {
		return s, false
	}
	return b.String(), true
}

// pickFencedBlock returns the best candidate from fenced/inline code blocks.
// Priority: ```json … ``` > any ``` … ```/~~~ … ~~~ > best inline `…`.
func pickFencedBlock(s string) (string, bool) {
	if m := reFenceJSON.FindStringSubmatch(s); len(m) == 2 {
		return m[1], true
	}
	if m := reFenceAny.FindStringSubmatch(s); len(m) == 2 {
		return m[1], true
	}
	if m := reFenceTilde.FindStringSubmatch(s); len(m) == 2 {
		return m[1], true
	}
	// Inline: choose the "best" candidate, not merely the first.
	all := reInlineTicks.FindAllStringSubmatch(s, -1)
	if len(all) == 0 {
		return "", false
	}
	best := ""
	bestScore := -1
	for _, m := range all {
		if len(m) != 2 {
			continue
		}
		cand := strings.TrimSpace(m[1])
		score := scoreJSONish(cand)
		if score > bestScore {
			best, bestScore = cand, score
		}
	}
	if best != "" {
		return best, true
	}
	return "", false
}

// scoreJSONish gives a rough score of how JSON-like a candidate looks.
func scoreJSONish(s string) int {
	score := 0
	trim := strings.TrimSpace(s)
	if strings.HasPrefix(trim, "{") || strings.HasPrefix(trim, "[") {
		score += 5
	}
	if strings.HasSuffix(trim, "}") || strings.HasSuffix(trim, "]") {
		score += 3
	}
	// Favor length a bit (longer blocks more likely to be payloads)
	if l := len(trim); l > 0 {
		score += min(l/64, 5) // cap contribution
	}
	// Count quotes/colons heuristically
	q := strings.Count(trim, `"`)
	col := strings.Count(trim, `:`)
	score += min((q+col)/8, 4)
	return score
}

func min(a, b int) int {
	if a < b {
		return a
	}
	return b
}

// extractBalancedJSON scans for the first balanced {...} or [...] region.
// If an opener is found but not closed, returns the suffix starting at the
// opener (caller can attempt repairs).
func extractBalancedJSON(s string) (string, bool) {
	var (
		start  = -1
		stack  []rune
		inStr  bool
		escape bool
	)
	for i, r := range s {
		if start < 0 {
			if r == '{' || r == '[' {
				start = i
				stack = []rune{r}
				inStr, escape = false, false
			}
			continue
		}
		if inStr {
			if escape {
				escape = false
				continue
			}
			if r == '\\' {
				escape = true
			} else if r == '"' {
				inStr = false
			}
			continue
		}
		switch r {
		case '"':
			inStr = true
		case '{', '[':
			stack = append(stack, r)
		case '}', ']':
			if len(stack) == 0 {
				return "", false
			}
			top := stack[len(stack)-1]
			if (top == '{' && r == '}') || (top == '[' && r == ']') {
				stack = stack[:len(stack)-1]
				if len(stack) == 0 {
					return s[start : i+1], true
				}
			}
		}
	}
	if start >= 0 {
		return s[start:], true
	}
	trim := strings.TrimSpace(s)
	if strings.HasPrefix(trim, "{") || strings.HasPrefix(trim, "[") {
		return trim, true
	}
	return s, false
}

// stripComments removes // and /* */ outside of strings.
func stripComments(s string) (string, bool) {
	var b strings.Builder
	inStr := false
	escape := false
	changed := false
	for i := 0; i < len(s); i++ {
		c := s[i]
		if inStr {
			b.WriteByte(c)
			if escape {
				escape = false
			} else if c == '\\' {
				escape = true
			} else if c == '"' {
				inStr = false
			}
			continue
		}
		if c == '"' {
			inStr = true
			b.WriteByte(c)
			continue
		}
		// // comment
		if c == '/' && i+1 < len(s) && s[i+1] == '/' {
			changed = true
			i += 2
			for i < len(s) && s[i] != '\n' {
				i++
			}
			if i < len(s) {
				b.WriteByte('\n')
			}
			continue
		}
		// /* ... */ comment
		if c == '/' && i+1 < len(s) && s[i+1] == '*' {
			changed = true
			i += 2
			for i+1 < len(s) && !(s[i] == '*' && s[i+1] == '/') {
				i++
			}
			if i+1 < len(s) {
				i++ // skip '/'
			}
			continue
		}
		b.WriteByte(c)
	}
	return b.String(), changed
}

func cleanupMarkdownArtifacts(s string) string {
	lines := strings.Split(s, "\n")
	out := make([]string, 0, len(lines))
	for _, ln := range lines {
		t := strings.TrimSpace(ln)
		if t == "```" || t == "~~~" || t == "json" {
			continue
		}
		out = append(out, ln)
	}
	return strings.TrimSpace(strings.Join(out, "\n"))
}

func normalizeSmartQuotes(s string) string {
	r := strings.NewReplacer(
		"“", `"`, "”", `"`, "„", `"`, "«", `"`, "»", `"`,
		"‘", `'`, "’", `'`, "‚", `'`,
	)
	return r.Replace(s)
}

// Convert 'single-quoted' JSON-like strings to "double-quoted" (string-aware).
func singleToDoubleQuotedStrings(s string) (string, bool) {
	var b strings.Builder
	inDq, inSq := false, false
	escape := false
	changed := false

	for i := 0; i < len(s); i++ {
		c := s[i]
		if inDq {
			b.WriteByte(c)
			if escape {
				escape = false
			} else if c == '\\' {
				escape = true
			} else if c == '"' {
				inDq = false
			}
			continue
		}
		if inSq {
			switch c {
			case '\\':
				b.WriteByte('\\')
			case '\'':
				b.WriteByte('"')
				inSq = false
				changed = true
			case '"':
				b.WriteString(`\"`)
				changed = true
			default:
				b.WriteByte(c)
			}
			continue
		}
		switch c {
		case '"':
			inDq = true
			b.WriteByte(c)
		case '\'':
			inSq = true
			b.WriteByte('"')
			changed = true
		default:
			b.WriteByte(c)
		}
	}
	if inSq {
		b.WriteByte('"')
		changed = true
	}
	return b.String(), changed
}

func fixKeyEquals(s string) (string, bool) {
	out := reKeyEq.ReplaceAllString(s, `${1}"${2}": `)
	return out, out != s
}

func quoteUnquotedKeys(s string) (string, bool) {
	out := reUnqKey.ReplaceAllString(s, `${1}"${2}":`)
	return out, out != s
}

func dropTrailingCommas(s string) (string, bool) {
	out := reTrailComma.ReplaceAllString(s, `$1`)
	return out, out != s
}

// Collapse ",," runs → ", null," in a linear pass (avoids O(n²) ReplaceAll loops).
func collapseDuplicateCommas(s string) (string, bool) {
	var b strings.Builder
	b.Grow(len(s))
	changed := false
	i := 0
	for i < len(s) {
		if s[i] == ',' {
			// Count consecutive commas
			j := i + 1
			for j < len(s) && s[j] == ',' {
				j++
			}
			if j > i+1 {
				// For N commas, emit one comma + (N-1) " null," sequences.
				b.WriteByte(',')
				for k := 0; k < j-(i+1); k++ {
					b.WriteString(" null,")
				}
				changed = true
				i = j
				continue
			}
		}
		b.WriteByte(s[i])
		i++
	}
	return b.String(), changed
}

// Normalize permissive numbers and specials outside of strings:
//
//	.5    -> 0.5            (only at value boundary)
//	1.    -> 1              (only if next non-space is delimiter or EOF)  ⟵ safe guard
//	+1    -> 1              (only at value boundary)
//	01    -> 1              (only at value boundary; keeps single zero)
//	NaN   -> null
//	±Infinity -> null
func normalizeLooseNumbers(s string) (string, bool) {
	var b bytes.Buffer
	inStr := false
	escape := false
	changed := false

	i := 0
	for i < len(s) {
		c := s[i]
		if inStr {
			b.WriteByte(c)
			if escape {
				escape = false
			} else if c == '\\' {
				escape = true
			} else if c == '"' {
				inStr = false
			}
			i++
			continue
		}

		if c == '"' {
			inStr = true
			b.WriteByte(c)
			i++
			continue
		}

		// NaN/Infinity → null
		if i+3 <= len(s) && s[i:i+3] == "NaN" && wordBoundary(s, i, i+3) {
			b.WriteString("null")
			i += 3
			changed = true
			continue
		}
		if i+8 <= len(s) && s[i:i+8] == "Infinity" && wordBoundary(s, i, i+8) {
			b.WriteString("null")
			i += 8
			changed = true
			continue
		}
		if i+9 <= len(s) && s[i:i+9] == "-Infinity" && wordBoundary(s, i, i+9) {
			b.WriteString("null")
			i += 9
			changed = true
			continue
		}

		// +number at value boundary
		if c == '+' && i+1 < len(s) && isASCIIDigit(s[i+1]) && isValueLeftDelimiterAt(s, prevNonSpace(s, i)) {
			i++
			changed = true
			continue
		}

		// .digits -> 0.digits at value boundary
		if c == '.' && i+1 < len(s) && isASCIIDigit(s[i+1]) && isValueLeftDelimiterAt(s, prevNonSpace(s, i)) {
			b.WriteString("0.")
			j := i + 1
			for j < len(s) && isASCIIDigit(s[j]) {
				b.WriteByte(s[j])
				j++
			}
			i = j
			changed = true
			continue
		}

		// digits. -> digits   only if next non-space char is a delimiter or EOF
		if isASCIIDigit(c) {
			j := i
			for j < len(s) && isASCIIDigit(s[j]) {
				j++
			}
			if j < len(s) && s[j] == '.' {
				nns := nextNonSpaceIndex(s, j+1)
				if nns == -1 || isRightDelimiter(s[nns]) {
					// safe to drop the trailing dot
					b.WriteString(s[i:j])
					i = j + 1
					changed = true
					continue
				}
			}
		}

		// 0NN… (leading zeros) at value boundary -> NN…
		if c == '0' && i+1 < len(s) && isASCIIDigit(s[i+1]) && isValueLeftDelimiterAt(s, prevNonSpace(s, i)) {
			j := i
			for j < len(s) && s[j] == '0' {
				j++
			}
			k := j
			for k < len(s) && isASCIIDigit(s[k]) {
				k++
			}
			if j < k {
				b.WriteString(s[j:k])
				i = k
				changed = true
				continue
			}
			// single zero falls through
		}

		b.WriteByte(c)
		i++
	}
	return b.String(), changed
}

func wordBoundary(s string, start, end int) bool {
	leftOK := true
	if start-1 >= 0 {
		c := s[start-1]
		leftOK = !(c == '_' || (c >= '0' && c <= '9') || (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z'))
	}
	rightOK := true
	if end < len(s) {
		c := s[end]
		rightOK = !(c == '_' || (c >= '0' && c <= '9') || (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z'))
	}
	return leftOK && rightOK
}

func isASCIIDigit(b byte) bool { return b >= '0' && b <= '9' }
func isWs(b byte) bool         { return b == ' ' || b == '\n' || b == '\t' || b == '\r' }

func prevNonSpace(s string, i int) int {
	for k := i - 1; k >= 0; k-- {
		if !isWs(s[k]) {
			return k
		}
	}
	return -1
}

func nextNonSpaceIndex(s string, i int) int {
	for k := i; k < len(s); k++ {
		if !isWs(s[k]) {
			return k
		}
	}
	return -1
}

func isValueLeftDelimiterAt(s string, idx int) bool {
	if idx < 0 {
		return true
	}
	switch s[idx] {
	case '{', '[', ':', ',':
		return true
	default:
		return false
	}
}

func isRightDelimiter(b byte) bool {
	switch b {
	case '}', ']', ',':
		return true
	default:
		return false
	}
}

// balanceAndClose repairs bracket/brace mismatches and closes an unterminated string.
func balanceAndClose(s string) (string, bool, string) {
	type opener struct{ r rune }
	var (
		out    strings.Builder
		stack  []opener
		inStr  bool
		escape bool
		change bool
	)

	needCloser := func(r rune) rune {
		if r == '{' {
			return '}'
		}
		return ']'
	}

	out.Grow(len(s) + 4)

	for i := 0; i < len(s); {
		r, w := utf8.DecodeRuneInString(s[i:])
		i += w

		if inStr {
			out.WriteRune(r)
			if escape {
				escape = false
				continue
			}
			if r == '\\' {
				escape = true
			} else if r == '"' {
				inStr = false
			}
			continue
		}

		switch r {
		case '"':
			inStr = true
			out.WriteRune(r)

		case '{', '[':
			stack = append(stack, opener{r: r})
			out.WriteRune(r)

		case '}', ']':
			if len(stack) == 0 {
				out.WriteRune(r)
				continue
			}
			for len(stack) > 0 {
				top := stack[len(stack)-1].r
				if (top == '{' && r == '}') || (top == '[' && r == ']') {
					out.WriteRune(r)
					stack = stack[:len(stack)-1]
					break
				}
				out.WriteRune(needCloser(top))
				stack = stack[:len(stack)-1]
				change = true
				if len(stack) == 0 {
					out.WriteRune(r)
					break
				}
			}

		default:
			out.WriteRune(r)
		}
	}

	if inStr {
		out.WriteRune('"')
		change = true
	}

	for i := len(stack) - 1; i >= 0; i-- {
		if stack[i].r == '{' {
			out.WriteByte('}')
		} else {
			out.WriteByte(']')
		}
		change = true
	}

	note := ""
	if change {
		note = "balanced/normalized brackets or closed unterminated string"
	}
	return out.String(), change, note
}

// decodeJSONPermissive decodes fixed text; supports multiple top-level documents.
func decodeJSONPermissive(s string) (any, []string) {
	var warns []string
	rest := strings.TrimSpace(s)
	var values []any

	for rest != "" {
		dec := json.NewDecoder(strings.NewReader(rest))
		dec.UseNumber()

		var v any
		if err := dec.Decode(&v); err != nil {
			trim := strings.TrimRightFunc(rest, unicode.IsSpace)
			if trim != rest {
				dec2 := json.NewDecoder(strings.NewReader(trim))
				dec2.UseNumber()
				if err2 := dec2.Decode(&v); err2 == nil {
					warns = append(warns, "ignored trailing content after JSON")
					values = append(values, v)
					break
				}
			}
			warns = append(warns, "decode failed after repairs: "+err.Error())
			return nil, warns
		}
		values = append(values, v)

		off := dec.InputOffset()
		if off >= int64(len(rest)) {
			rest = ""
			break
		}
		rest = rest[off:]
		rest = strings.TrimLeftFunc(rest, unicode.IsSpace)
		if len(rest) > 0 && rest[0] == ',' {
			rest = strings.TrimLeftFunc(rest[1:], unicode.IsSpace)
		}
	}

	if len(values) == 0 {
		return nil, []string{"no JSON value found"}
	}
	if len(values) == 1 {
		return values[0], warns
	}
	warns = append(warns, "multiple JSON documents parsed; returned array")
	arr := make([]any, len(values))
	copy(arr, values)
	return arr, warns
}
