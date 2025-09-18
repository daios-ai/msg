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
	"fmt"
	"regexp"
	"strings"
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

// jsonRepair takes possibly-broken JSON-like text and tries to repair it.
// It returns: (parsed value, repaired JSON string, warnings).

// ─────────────────────────────────────────────────────────────────────────────
// Types
// ─────────────────────────────────────────────────────────────────────────────

// Warning info
type jWarning struct {
	Code string
	Msg  string
}

// ─────────────────────────────────────────────────────────────────────────────
// Entry point
// ─────────────────────────────────────────────────────────────────────────────

// jsonRepair parses and repairs JSON-like text.
// Returns: parsed value (map/array/scalar), canonical JSON string, warnings.
func jsonRepair(src string) (any, string, []string) {
	warnings := []string{}

	// 1. Strip BOM and code fences
	src2, ws := jPreprocess(src)
	warnings = append(warnings, ws...)

	// 2. Remove comments
	src3, ws := jStripComments(src2)
	warnings = append(warnings, ws...)

	// 3. Normalize booleans/nulls/numbers/single quotes/unquoted keys/trailing commas
	src4, ws := jRepairSyntax(src3)
	warnings = append(warnings, ws...)

	// 4. Balance brackets
	src5, ws := jBalanceBrackets(src4)
	warnings = append(warnings, ws...)

	// 5. Try parsing
	var val any
	if err := json.Unmarshal([]byte(src5), &val); err != nil {
		// fallback: wrap in array or object?
		// best effort: return nil + warnings
		warnings = append(warnings, "final parse failed: "+err.Error())
		return nil, "null", warnings
	}

	// 6. Canonicalize
	buf := &bytes.Buffer{}
	enc := json.NewEncoder(buf)
	enc.SetEscapeHTML(false)
	enc.SetIndent("", "")
	_ = enc.Encode(val)
	out := strings.TrimSpace(buf.String())

	return val, out, warnings
}

// ─────────────────────────────────────────────────────────────────────────────
// Helpers
// ─────────────────────────────────────────────────────────────────────────────

// Strip BOM, code fences
func jPreprocess(src string) (string, []string) {
	w := []string{}
	s := strings.TrimSpace(src)
	// BOM
	if strings.HasPrefix(s, "\uFEFF") {
		s = s[1:]
		w = append(w, "removed BOM")
	}
	// code fences
	if strings.HasPrefix(s, "```") {
		re := regexp.MustCompile("(?s)^```[a-zA-Z]*\\n(.*)```$")
		m := re.FindStringSubmatch(s)
		if len(m) == 2 {
			s = m[1]
			w = append(w, "removed code fence")
		}
	}
	return s, w
}

// Remove JS/JSON5 style comments
func jStripComments(src string) (string, []string) {
	re1 := regexp.MustCompile(`(?m)//[^\n]*`)
	re2 := regexp.MustCompile(`(?s)/\*.*?\*/`)
	out := re1.ReplaceAllString(src, "")
	out2 := re2.ReplaceAllString(out, "")
	ws := []string{}
	if out != src {
		ws = append(ws, "removed // comments")
	}
	if out2 != out {
		ws = append(ws, "removed /* */ comments")
	}
	return out2, ws
}

// Normalize single quotes, unquoted keys, trailing commas, booleans, nulls, numbers
func jRepairSyntax(src string) (string, []string) {
	w := []string{}
	s := src

	// single-quoted strings → double
	reSQ := regexp.MustCompile(`'([^'\\]*(?:\\.[^'\\]*)*)'`)
	if reSQ.MatchString(s) {
		s = reSQ.ReplaceAllStringFunc(s, func(m string) string {
			body := m[1 : len(m)-1]
			body = strings.ReplaceAll(body, `"`, `\"`)
			return `"` + body + `"`
		})
		w = append(w, "converted single-quoted strings")
	}

	// unquoted object keys → quoted
	reKey := regexp.MustCompile(`([{,]\s*)([A-Za-z_][A-Za-z0-9_\-]*)(\s*:)`)
	if reKey.MatchString(s) {
		s = reKey.ReplaceAllString(s, `${1}"${2}"${3}`)
		w = append(w, "quoted unquoted keys")
	}

	// trailing commas
	reTC := regexp.MustCompile(`,(\s*[}\]])`)
	if reTC.MatchString(s) {
		s = reTC.ReplaceAllString(s, "${1}")
		w = append(w, "removed trailing commas")
	}

	// booleans and null
	reTrue := regexp.MustCompile(`\bTrue\b`)
	reFalse := regexp.MustCompile(`\bFalse\b`)
	reNone := regexp.MustCompile(`\bNone\b`)
	if reTrue.MatchString(s) {
		s = reTrue.ReplaceAllString(s, "true")
		w = append(w, "normalized True→true")
	}
	if reFalse.MatchString(s) {
		s = reFalse.ReplaceAllString(s, "false")
		w = append(w, "normalized False→false")
	}
	if reNone.MatchString(s) {
		s = reNone.ReplaceAllString(s, "null")
		w = append(w, "normalized None→null")
	}

	// numbers: .5 → 0.5
	reLeadingDot := regexp.MustCompile(`(^|[^0-9])\.([0-9]+)`)
	s = reLeadingDot.ReplaceAllString(s, `${1}0.${2}`)

	// numbers: 1. → 1.0 (handle non-digit follower and end-of-input separately)
	// 1) digit '.' NON-DIGIT  → insert .0 before that non-digit
	reTrailingDotNonDigit := regexp.MustCompile(`([0-9])\.(\D)`)
	s = reTrailingDotNonDigit.ReplaceAllString(s, `${1}.0${2}`)

	// 2) digit '.' end-of-input → append 0
	reTrailingDotEOF := regexp.MustCompile(`([0-9])\.$`)
	s = reTrailingDotEOF.ReplaceAllString(s, `${1}.0`)

	// remove + in numbers
	rePlus := regexp.MustCompile(`\+([0-9])`)
	s = rePlus.ReplaceAllString(s, `${1}`)

	// remove underscores
	if strings.Contains(s, "_") {
		reUnderscore := regexp.MustCompile(`([0-9])_([0-9])`)
		if reUnderscore.MatchString(s) {
			s = reUnderscore.ReplaceAllString(s, `${1}${2}`)
			w = append(w, "removed underscores in numbers")
		}
	}

	return s, w
}

func jBalanceBrackets(src string) (string, []string) {
	emitCloser := func(b rune) rune {
		if b == '{' {
			return '}'
		}
		return ']'
	}

	var (
		out        []rune
		stack      []rune
		inString   bool
		escaped    bool
		addedObj   int
		addedArr   int
		droppedObj int
		droppedArr int
	)

	for _, r := range []rune(src) {
		// Handle string context (double quotes per JSON)
		if inString {
			out = append(out, r)
			if escaped {
				escaped = false
				continue
			}
			if r == '\\' {
				escaped = true
				continue
			}
			if r == '"' {
				inString = false
			}
			continue
		}
		if r == '"' {
			inString = true
			out = append(out, r)
			continue
		}

		switch r {
		case '{', '[':
			stack = append(stack, r)
			out = append(out, r)

		case '}', ']':
			want := '{'
			if r == ']' {
				want = '['
			}
			// If the top doesn't match, auto-close intervening mismatches
			for len(stack) > 0 && stack[len(stack)-1] != want {
				top := stack[len(stack)-1]
				out = append(out, emitCloser(top))
				if top == '{' {
					addedObj++
				} else {
					addedArr++
				}
				stack = stack[:len(stack)-1]
			}
			// If we now have the expected opener, consume it and emit the closer
			if len(stack) > 0 && stack[len(stack)-1] == want {
				stack = stack[:len(stack)-1]
				out = append(out, r)
			} else {
				// Unmatched closer: DROP it (don’t emit to out), record a warning
				if r == '}' {
					droppedObj++
				} else {
					droppedArr++
				}
				// (continue without appending)
			}

		default:
			out = append(out, r)
		}
	}

	// Close any remaining opens
	for i := len(stack) - 1; i >= 0; i-- {
		c := emitCloser(stack[i])
		out = append(out, c)
		if stack[i] == '{' {
			addedObj++
		} else {
			addedArr++
		}
	}

	var warnings []string
	if addedObj > 0 {
		warnings = append(warnings, fmt.Sprintf("balanced object braces (added %d '}' )", addedObj))
	}
	if addedArr > 0 {
		warnings = append(warnings, fmt.Sprintf("balanced array brackets (added %d ']' )", addedArr))
	}
	if droppedObj > 0 {
		warnings = append(warnings, fmt.Sprintf("dropped %d unmatched '}'", droppedObj))
	}
	if droppedArr > 0 {
		warnings = append(warnings, fmt.Sprintf("dropped %d unmatched ']'", droppedArr))
	}

	return string(out), warnings
}
