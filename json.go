package mindscript

import (
	"bytes"
	"encoding/json"
	"fmt"
	"regexp"
	"strconv"
	"strings"
	"unicode"
)

// Public entrypoint (unexported per request).
// jsonRepair takes messy JSON-like text produced by LLMs, attempts to repair it,
// parses it into a Go value, and returns (value, canonical JSON text, warnings).
func jsonRepair(src string) (any, string, []string) {
	j := &jRepairer{}
	v := j.repairAndParse(src)
	if j.err != nil {
		j.warn(jNOT_JSON_TOP, j.err.Error(), 1, 1)
		return nil, "null", j.renderWarnings()
	}

	// Marshal to strict JSON
	jb, mErr := json.Marshal(v)
	if mErr != nil {
		j.warn("marshal_error", mErr.Error(), 1, 1)
		return nil, "null", j.renderWarnings()
	}
	return v, string(jb), j.renderWarnings()
}

// --------------------------- Warning model ----------------------------

type jWarning struct {
	Code string
	Msg  string
	Line int
	Col  int
}

// Warning codes
const (
	jFENCE_STRIP     = "fence_strip"
	jCMT_STRIP       = "comment_strip"
	jSQ_STR          = "single_quote_string"
	jUNQ_KEY         = "unquoted_key"
	jTRAIL_COMMA     = "trailing_comma"
	jMISS_COMMA      = "missing_comma"
	jMISS_COLON      = "missing_colon"
	jBALANCE_OBJ     = "balance_object"
	jBALANCE_ARR     = "balance_array"
	jNUM_LEADING_DOT = "num_leading_dot"
	jNUM_PLUS        = "num_plus"
	jNUM_UNDERSCORE  = "num_underscore"
	jNUM_HEXLIKE     = "num_hexlike"
	jNUM_INF_NAN     = "num_inf_nan"
	jBOOL_CASE       = "bool_case"
	jNULL_ALIAS      = "null_alias"
	jTRAILING_JUNK   = "trailing_junk"
	jNOT_JSON_TOP    = "not_json_top"
)

// --------------------------- Tokens ----------------------------

type jTokenKind int

const (
	jEOF jTokenKind = iota
	jWS
	jCOMMENT
	jLBRACE
	jRBRACE
	jLBRACK
	jRBRACK
	jCOLON
	jCOMMA
	jSTRING_DQ
	jSTRING_SQ
	jNUMBER
	jIDENT
	jUNKNOWN
)

type jToken struct {
	Kind      jTokenKind
	Lexeme    string
	Line      int
	Col       int
	StartByte int
	EndByte   int
}

// --------------------------- Repairer ----------------------------

type jRepairer struct {
	ws  []jWarning
	err error
}

func (r *jRepairer) warn(code, msg string, line, col int) {
	r.ws = append(r.ws, jWarning{Code: code, Msg: msg, Line: line, Col: col})
}
func (r *jRepairer) renderWarnings() []string {
	out := make([]string, len(r.ws))
	for i, w := range r.ws {
		if w.Line > 0 {
			out[i] = fmt.Sprintf("%s: %s (line %d, col %d)", w.Code, w.Msg, w.Line, w.Col)
		} else {
			out[i] = fmt.Sprintf("%s: %s", w.Code, w.Msg)
		}
	}
	return out
}

// Main pipeline
func (r *jRepairer) repairAndParse(src string) any {
	clean := r.prepass(src)

	// Hard gate: if we cannot find a plausible JSON *start*, bail out.
	if !jLooksLikeJSON(clean) {
		r.err = fmt.Errorf("no JSON-like content found")
		return nil
	}

	toks := r.lex(clean)
	p := &jParser{r: r, toks: toks}
	return p.parseAny()
}

// --------------------------- Pre-pass ----------------------------

var jFenceRx = regexp.MustCompile("(?s)```([a-zA-Z0-9_-]+)?\\s*\\n(.*?)\\n\\s*```")

func (r *jRepairer) prepass(s string) string {
	// Strip BOM
	if strings.HasPrefix(s, "\uFEFF") {
		s = s[1:]
	}
	// Normalize newlines
	s = strings.ReplaceAll(s, "\r\n", "\n")
	s = strings.ReplaceAll(s, "\r", "\n")

	// Remove code fences if present — choose the largest fenced block
	var best string
	for _, m := range jFenceRx.FindAllStringSubmatch(s, -1) {
		inner := m[2]
		if len(inner) > len(best) {
			best = inner
		}
	}
	if best != "" {
		r.warn(jFENCE_STRIP, "removed code fence", 1, 1)
		s = best
	}

	// Trim to first plausible JSON value start (but do NOT anchor at arbitrary letters)
	i := jFindJSONStart(s)
	if i >= 0 {
		if i > 0 {
			r.warn(jNOT_JSON_TOP, "trimmed leading non-JSON text", 1, 1)
		}
		s = s[i:]
	}

	// Leave trailing junk for the parser; it will warn and ignore.
	return s
}

// Heuristic: find index of first plausible JSON value start, else -1.
func jFindJSONStart(s string) int {
	i := 0
	// skip leading whitespace
	for i < len(s) && isSpaceByte(s[i]) {
		i++
	}
	for i < len(s) {
		c := s[i]
		switch c {
		case '{', '[', '"', '\'':
			return i
		case '-':
			// number must follow: digit or dot
			if i+1 < len(s) && (isDigitByte(s[i+1]) || s[i+1] == '.') {
				return i
			}
		case '+':
			// we accept +number as a start, we'll normalize it away
			if i+1 < len(s) && (isDigitByte(s[i+1]) || s[i+1] == '.') {
				return i
			}
		case '.':
			if i+1 < len(s) && isDigitByte(s[i+1]) {
				return i
			}
		default:
			if isDigitByte(c) {
				return i
			}
			// true/false/null (case-insensitive) only
			if hasWordAt(s, i, "true") || hasWordAt(s, i, "false") || hasWordAt(s, i, "null") {
				return i
			}
		}
		// otherwise, advance to next char and keep scanning
		i++
	}
	return -1
}

func jLooksLikeJSON(s string) bool { return jFindJSONStart(s) >= 0 }

func hasWordAt(s string, i int, w string) bool {
	if i+len(w) > len(s) {
		return false
	}
	if !strings.EqualFold(s[i:i+len(w)], w) {
		return false
	}
	// Ensure token boundary
	end := i + len(w)
	prevOK := i == 0 || !isIdentByte(s[i-1])
	nextOK := end == len(s) || !isIdentByte(s[end])
	return prevOK && nextOK
}

func isSpaceByte(b byte) bool {
	return b == ' ' || b == '\t' || b == '\n' || b == '\f' || b == '\v' || b == '\r'
}
func isDigitByte(b byte) bool { return '0' <= b && b <= '9' }
func isIdentByte(b byte) bool {
	return unicode.IsLetter(rune(b)) || isDigitByte(b) || b == '_' || b == '-'
}

// --------------------------- Lexer ----------------------------

func (r *jRepairer) lex(s string) []jToken {
	var toks []jToken
	i, line, col := 0, 1, 1
	adv := func(n int) { i += n; col += n }
	emit := func(k jTokenKind, start int) {
		toks = append(toks, jToken{Kind: k, Lexeme: s[start:i], Line: line, Col: col - (i - start), StartByte: start, EndByte: i})
	}

	for i < len(s) {
		ch := s[i]

		// Whitespace
		if ch == ' ' || ch == '\t' || ch == '\f' || ch == '\v' || ch == '\r' {
			start := i
			for i < len(s) && (s[i] == ' ' || s[i] == '\t' || s[i] == '\f' || s[i] == '\v' || s[i] == '\r') {
				adv(1)
			}
			emit(jWS, start)
			continue
		}
		if ch == '\n' {
			toks = append(toks, jToken{Kind: jWS, Lexeme: "\n", Line: line, Col: col, StartByte: i, EndByte: i + 1})
			i++
			line++
			col = 1
			continue
		}

		// Comments
		if ch == '/' && i+1 < len(s) {
			if s[i+1] == '/' {
				start := i
				adv(2)
				for i < len(s) && s[i] != '\n' {
					adv(1)
				}
				emit(jCOMMENT, start)
				continue
			}
			if s[i+1] == '*' {
				start := i
				adv(2)
				for i+1 < len(s) && !(s[i] == '*' && s[i+1] == '/') {
					if s[i] == '\n' {
						line++
						col = 1
						i++
						continue
					}
					adv(1)
				}
				if i+1 < len(s) {
					adv(2)
				}
				emit(jCOMMENT, start)
				continue
			}
		}

		// Punct
		switch ch {
		case '{':
			toks = append(toks, jToken{Kind: jLBRACE, Lexeme: "{", Line: line, Col: col, StartByte: i, EndByte: i + 1})
			adv(1)
			continue
		case '}':
			toks = append(toks, jToken{Kind: jRBRACE, Lexeme: "}", Line: line, Col: col, StartByte: i, EndByte: i + 1})
			adv(1)
			continue
		case '[':
			toks = append(toks, jToken{Kind: jLBRACK, Lexeme: "[", Line: line, Col: col, StartByte: i, EndByte: i + 1})
			adv(1)
			continue
		case ']':
			toks = append(toks, jToken{Kind: jRBRACK, Lexeme: "]", Line: line, Col: col, StartByte: i, EndByte: i + 1})
			adv(1)
			continue
		case ':':
			toks = append(toks, jToken{Kind: jCOLON, Lexeme: ":", Line: line, Col: col, StartByte: i, EndByte: i + 1})
			adv(1)
			continue
		case ',':
			toks = append(toks, jToken{Kind: jCOMMA, Lexeme: ",", Line: line, Col: col, StartByte: i, EndByte: i + 1})
			adv(1)
			continue
		case '"':
			start := i
			adv(1)
			var buf bytes.Buffer
			for i < len(s) {
				c := s[i]
				if c == '\\' && i+1 < len(s) {
					buf.WriteByte(c)
					buf.WriteByte(s[i+1])
					adv(2)
					continue
				}
				if c == '"' {
					adv(1)
					break
				}
				if c == '\n' {
					line++
					col = 1
					i++
					continue
				}
				buf.WriteByte(c)
				adv(1)
			}
			emit(jSTRING_DQ, start)
			continue
		case '\'':
			start := i
			adv(1)
			for i < len(s) {
				c := s[i]
				if c == '\\' && i+1 < len(s) {
					adv(2)
					continue
				}
				if c == '\'' {
					adv(1)
					break
				}
				if c == '\n' {
					line++
					col = 1
					i++
					continue
				}
				adv(1)
			}
			emit(jSTRING_SQ, start)
			continue
		}

		// Number-ish
		if ch == '+' || ch == '-' || ch == '.' || unicode.IsDigit(rune(ch)) {
			start := i
			adv(1)
			for i < len(s) {
				c := s[i]
				if unicode.IsLetter(rune(c)) || unicode.IsDigit(rune(c)) || strings.ContainsRune("._xXoObB+-", rune(c)) {
					adv(1)
					continue
				}
				break
			}
			emit(jNUMBER, start)
			continue
		}

		// Identifier (keys or literals)
		if unicode.IsLetter(rune(ch)) || ch == '_' {
			start := i
			adv(1)
			for i < len(s) {
				c := s[i]
				if unicode.IsLetter(rune(c)) || unicode.IsDigit(rune(c)) || c == '_' || c == '-' {
					adv(1)
					continue
				}
				break
			}
			emit(jIDENT, start)
			continue
		}

		// Unknown single char
		toks = append(toks, jToken{Kind: jUNKNOWN, Lexeme: string(ch), Line: line, Col: col, StartByte: i, EndByte: i + 1})
		adv(1)
	}
	toks = append(toks, jToken{Kind: jEOF, Lexeme: "", Line: line, Col: col, StartByte: i, EndByte: i})
	return toks
}

// --------------------------- Parser ----------------------------

type jParser struct {
	r    *jRepairer
	toks []jToken
	pos  int
}

func (p *jParser) cur() jToken { return p.toks[p.pos] }
func (p *jParser) next() jToken {
	t := p.toks[p.pos]
	if p.pos < len(p.toks)-1 {
		p.pos++
	}
	return t
}
func (p *jParser) peek(k jTokenKind) bool { return p.cur().Kind == k }
func (p *jParser) atEnd() bool            { return p.peek(jEOF) }
func (p *jParser) skipIgnorable() {
	for {
		if p.peek(jWS) {
			p.next()
			continue
		}
		if p.peek(jCOMMENT) {
			p.r.warn(jCMT_STRIP, "removed comment", p.cur().Line, p.cur().Col)
			p.next()
			continue
		}
		break
	}
}

func (p *jParser) parseAny() any {
	p.skipIgnorable()
	if p.atEnd() {
		p.r.err = fmt.Errorf("empty input after preprocessing")
		return nil
	}
	v := p.parseValue()
	p.skipIgnorable()
	if !p.atEnd() {
		p.r.warn(jTRAILING_JUNK, "ignored trailing non-JSON content", p.cur().Line, p.cur().Col)
	}
	return v
}

func (p *jParser) parseValue() any {
	p.skipIgnorable()
	t := p.cur()
	switch t.Kind {
	case jLBRACE:
		return p.parseObject()
	case jLBRACK:
		return p.parseArray()
	case jSTRING_DQ:
		p.next()
		return p.decodeStringDQ(t)
	case jSTRING_SQ:
		p.r.warn(jSQ_STR, "converted single-quoted string", t.Line, t.Col)
		p.next()
		return p.decodeStringSQ(t)
	case jNUMBER:
		p.next()
		return p.parseNumber(t)
	case jIDENT:
		p.next()
		lex := t.Lexeme
		l := strings.ToLower(lex)
		switch l {
		case "true":
			if lex != "true" {
				p.r.warn(jBOOL_CASE, "normalized boolean", t.Line, t.Col)
			}
			return true
		case "false":
			if lex != "false" {
				p.r.warn(jBOOL_CASE, "normalized boolean", t.Line, t.Col)
			}
			return false
		case "null":
			if lex != "null" {
				p.r.warn(jNULL_ALIAS, "normalized null", t.Line, t.Col)
			}
			return nil
		case "none":
			p.r.warn(jNULL_ALIAS, "normalized None→null", t.Line, t.Col)
			return nil
		case "nan", "infinity", "+infinity", "-infinity":
			p.r.warn(jNUM_INF_NAN, "replaced NaN/Infinity with null", t.Line, t.Col)
			return nil
		default:
			// Bare identifier as a value is not JSON — make it null (strict).
			p.r.warn(jNOT_JSON_TOP, "bare identifier as value → null", t.Line, t.Col)
			return nil
		}
	case jUNKNOWN:
		// Drop and retry parsing a valid value
		p.next()
		return p.parseValue()
	default:
		// Unexpected token; strict fallback is null (no “coerce to string”).
		p.r.warn(jNOT_JSON_TOP, "unexpected token → null", t.Line, t.Col)
		p.next()
		return nil
	}
}

func (p *jParser) parseObject() any {
	start := p.next() // '{'
	obj := make(map[string]any)
	seen := map[string]struct{}{}
	for {
		p.skipIgnorable()

		// Normal close
		if p.peek(jRBRACE) {
			p.next()
			break
		}
		// Mismatched close: ']' ends object early — don't consume it; let parent handle.
		if p.peek(jRBRACK) {
			p.r.warn(jBALANCE_OBJ, "encountered ']' while parsing object — ended object early", p.cur().Line, p.cur().Col)
			break
		}
		// EOF: synthesize close
		if p.peek(jEOF) {
			p.r.warn(jBALANCE_OBJ, "inserted missing '}' at EOF", start.Line, start.Col)
			break
		}

		// key
		var key string
		kt := p.cur()
		switch kt.Kind {
		case jSTRING_DQ:
			p.next()
			key = p.decodeStringDQ(kt)
		case jSTRING_SQ:
			p.r.warn(jSQ_STR, "converted single-quoted key", kt.Line, kt.Col)
			p.next()
			key = p.decodeStringSQ(kt)
		case jIDENT:
			key = kt.Lexeme
			p.r.warn(jUNQ_KEY, fmt.Sprintf("quoted unquoted key %q", key), kt.Line, kt.Col)
			p.next()
		default:
			key = fmt.Sprintf("$key_%d", p.pos)
			p.r.warn(jMISS_COLON, "synthesized missing key", kt.Line, kt.Col)
		}

		p.skipIgnorable()
		if !p.peek(jCOLON) {
			p.r.warn(jMISS_COLON, fmt.Sprintf("inserted ':' after key %q", key), p.cur().Line, p.cur().Col)
		} else {
			p.next()
		}

		// value
		p.skipIgnorable()
		val := p.parseValue()
		if _, dup := seen[key]; dup {
			p.r.warn(jUNQ_KEY, fmt.Sprintf("duplicate key %q: kept last", key), p.cur().Line, p.cur().Col)
		}
		obj[key] = val
		seen[key] = struct{}{}

		p.skipIgnorable()

		// Comma or end
		if p.peek(jCOMMA) {
			p.next()
			p.skipIgnorable()
			if p.peek(jRBRACE) {
				p.r.warn(jTRAIL_COMMA, "removed trailing comma in object", p.cur().Line, p.cur().Col)
			}
			continue
		}
		if p.peek(jRBRACE) {
			p.next()
			break
		}
		// Mismatched ']' ends object early (do not consume).
		if p.peek(jRBRACK) {
			p.r.warn(jBALANCE_OBJ, "encountered ']' while parsing object — ended object early", p.cur().Line, p.cur().Col)
			break
		}
		if p.peek(jEOF) {
			p.r.warn(jBALANCE_OBJ, "inserted missing '}' at EOF", start.Line, start.Col)
			break
		}

		// Missing comma between pairs — insert
		p.r.warn(jMISS_COMMA, "inserted missing comma between object items", p.cur().Line, p.cur().Col)
	}
	return obj
}

func (p *jParser) parseArray() any {
	start := p.next() // '['
	var arr []any
	for {
		p.skipIgnorable()

		// Normal close
		if p.peek(jRBRACK) {
			p.next()
			break
		}
		// Mismatched close: '}' ends array early — don't consume it; let parent handle.
		if p.peek(jRBRACE) {
			p.r.warn(jBALANCE_ARR, "encountered '}' while parsing array — ended array early", p.cur().Line, p.cur().Col)
			break
		}
		// EOF: synthesize close
		if p.peek(jEOF) {
			p.r.warn(jBALANCE_ARR, "inserted missing ']' at EOF", start.Line, start.Col)
			break
		}

		val := p.parseValue()
		arr = append(arr, val)

		p.skipIgnorable()
		if p.peek(jCOMMA) {
			p.next()
			p.skipIgnorable()
			if p.peek(jRBRACK) {
				p.r.warn(jTRAIL_COMMA, "removed trailing comma in array", p.cur().Line, p.cur().Col)
			}
			continue
		}
		if p.peek(jRBRACK) {
			p.next()
			break
		}
		// Mismatched '}' ends array early (do not consume).
		if p.peek(jRBRACE) {
			p.r.warn(jBALANCE_ARR, "encountered '}' while parsing array — ended array early", p.cur().Line, p.cur().Col)
			break
		}
		if p.peek(jEOF) {
			p.r.warn(jBALANCE_ARR, "inserted missing ']' at EOF", start.Line, start.Col)
			break
		}

		// Missing comma between elements — insert
		p.r.warn(jMISS_COMMA, "inserted missing comma between array elements", p.cur().Line, p.cur().Col)
	}
	return arr
}

// --------------------------- Decoders / normalizers ----------------------------

func (p *jParser) decodeStringDQ(t jToken) string {
	lex := t.Lexeme
	if len(lex) >= 2 && lex[0] == '"' && lex[len(lex)-1] == '"' {
		return jUnescapeDoubleQuoted(lex[1 : len(lex)-1])
	}
	return jUnescapeDoubleQuoted(strings.Trim(lex, "\""))
}

func (p *jParser) decodeStringSQ(t jToken) string {
	lex := t.Lexeme
	if len(lex) >= 2 && lex[0] == '\'' && lex[len(lex)-1] == '\'' {
		inner := lex[1 : len(lex)-1]
		inner = strings.ReplaceAll(inner, "\\'", "'")
		inner = strings.ReplaceAll(inner, "\"", "\\\"")
		return inner
	}
	return lex
}

func jUnescapeDoubleQuoted(s string) string {
	// Permissive pass: keep escapes; escape control characters
	var b strings.Builder
	for i := 0; i < len(s); i++ {
		c := s[i]
		if c < 0x20 {
			b.WriteString(fmt.Sprintf("\\u%04X", c))
			continue
		}
		b.WriteByte(c)
	}
	return b.String()
}

func (p *jParser) parseNumber(t jToken) any {
	raw := strings.ToLower(strings.TrimSpace(t.Lexeme))

	// Remove underscores
	if strings.Contains(raw, "_") {
		p.r.warn(jNUM_UNDERSCORE, "removed '_' from number", t.Line, t.Col)
		raw = strings.ReplaceAll(raw, "_", "")
	}
	// Strip leading plus
	if strings.HasPrefix(raw, "+") {
		p.r.warn(jNUM_PLUS, "removed leading '+'", t.Line, t.Col)
		raw = raw[1:]
	}
	// Normalize leading/trailing dot
	if strings.HasPrefix(raw, ".") {
		p.r.warn(jNUM_LEADING_DOT, "normalized leading '.'", t.Line, t.Col)
		raw = "0" + raw
	}
	if strings.HasSuffix(raw, ".") {
		p.r.warn(jNUM_LEADING_DOT, "normalized trailing '.'", t.Line, t.Col)
		raw = raw + "0"
	}

	// Hex / bin / octal
	if strings.HasPrefix(raw, "0x") {
		p.r.warn(jNUM_HEXLIKE, "converted hex literal", t.Line, t.Col)
		if n, err := strconv.ParseUint(raw[2:], 16, 64); err == nil {
			return float64(n)
		}
	}
	if strings.HasPrefix(raw, "0b") {
		p.r.warn(jNUM_HEXLIKE, "converted binary literal", t.Line, t.Col)
		if n, err := strconv.ParseUint(raw[2:], 2, 64); err == nil {
			return float64(n)
		}
	}
	if strings.HasPrefix(raw, "0o") {
		p.r.warn(jNUM_HEXLIKE, "converted octal literal", t.Line, t.Col)
		if n, err := strconv.ParseUint(raw[2:], 8, 64); err == nil {
			return float64(n)
		}
	}

	// Infinity / NaN
	switch raw {
	case "nan", "infinity", "+infinity", "-infinity":
		p.r.warn(jNUM_INF_NAN, "replaced NaN/Infinity with null", t.Line, t.Col)
		return nil
	}

	// Try int first
	if i64, err := strconv.ParseInt(raw, 10, 64); err == nil {
		return float64(i64) // use float64 to align with generic JSON numbers
	}
	// Then float
	if f64, err := strconv.ParseFloat(raw, 64); err == nil {
		return f64
	}
	// Bad number → null (strict)
	p.r.warn(jNOT_JSON_TOP, "bad number literal → null", t.Line, t.Col)
	return nil
}
