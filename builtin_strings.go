// === FILE: std_sys.go ===
package mindscript

import (
	"regexp"
	"strings"
	"unicode"
)

func registerStringBuiltins(ip *Interpreter) {
	// substr(s, i, j)
	ip.RegisterNative(
		"substr",
		[]ParamSpec{{"s", S{"id", "Str"}}, {"i", S{"id", "Int"}}, {"j", S{"id", "Int"}}},
		S{"id", "Str"},
		func(_ *Interpreter, ctx CallCtx) Value {
			s := ctx.MustArg("s").Data.(string)
			i := int(ctx.MustArg("i").Data.(int64))
			j := int(ctx.MustArg("j").Data.(int64))
			r := []rune(s)
			if i < 0 {
				i = 0
			}
			if j < i {
				j = i
			}
			if i > len(r) {
				i = len(r)
			}
			if j > len(r) {
				j = len(r)
			}
			return Str(string(r[i:j]))
		},
	)
	setBuiltinDoc(ip, "substr", `Unicode-safe substring by rune index.

Takes the half-open slice [i, j). Indices are clamped to bounds and negative
values are treated as 0.

Params:
  s: Str — source string
  i: Int — start index (inclusive)
  j: Int — end index (exclusive)

Returns:
  Str`)

	ip.RegisterNative(
		"toLower",
		[]ParamSpec{{"s", S{"id", "Str"}}},
		S{"id", "Str"},
		func(_ *Interpreter, ctx CallCtx) Value { return Str(strings.ToLower(ctx.MustArg("s").Data.(string))) },
	)
	setBuiltinDoc(ip, "toLower", `Lowercase conversion (Unicode aware).

Params:
  s: Str

Returns:
  Str`)

	ip.RegisterNative(
		"toUpper",
		[]ParamSpec{{"s", S{"id", "Str"}}},
		S{"id", "Str"},
		func(_ *Interpreter, ctx CallCtx) Value { return Str(strings.ToUpper(ctx.MustArg("s").Data.(string))) },
	)
	setBuiltinDoc(ip, "toUpper", `Uppercase conversion (Unicode aware).

Params:
  s: Str

Returns:
  Str`)

	trimFunc := func(left, right bool) func(string) string {
		return func(s string) string {
			if left && right {
				return strings.TrimSpace(s)
			}
			if left {
				return strings.TrimLeftFunc(s, unicode.IsSpace)
			}
			return strings.TrimRightFunc(s, unicode.IsSpace)
		}
	}

	ip.RegisterNative("strip",
		[]ParamSpec{{"s", S{"id", "Str"}}}, S{"id", "Str"},
		func(_ *Interpreter, ctx CallCtx) Value {
			return Str(trimFunc(true, true)(ctx.MustArg("s").Data.(string)))
		},
	)
	setBuiltinDoc(ip, "strip", `Remove leading and trailing whitespace (Unicode).

Params:
  s: Str

Returns:
  Str`)

	ip.RegisterNative("lstrip",
		[]ParamSpec{{"s", S{"id", "Str"}}}, S{"id", "Str"},
		func(_ *Interpreter, ctx CallCtx) Value {
			return Str(trimFunc(true, false)(ctx.MustArg("s").Data.(string)))
		},
	)
	setBuiltinDoc(ip, "lstrip", `Remove leading whitespace (Unicode).

Params:
  s: Str

Returns:
  Str`)

	ip.RegisterNative("rstrip",
		[]ParamSpec{{"s", S{"id", "Str"}}}, S{"id", "Str"},
		func(_ *Interpreter, ctx CallCtx) Value {
			return Str(trimFunc(false, true)(ctx.MustArg("s").Data.(string)))
		},
	)
	setBuiltinDoc(ip, "rstrip", `Remove trailing whitespace (Unicode).

Params:
  s: Str

Returns:
  Str`)

	ip.RegisterNative(
		"split",
		[]ParamSpec{{"s", S{"id", "Str"}}, {"sep", S{"id", "Str"}}},
		S{"array", S{"id", "Str"}},
		func(_ *Interpreter, ctx CallCtx) Value {
			s := ctx.MustArg("s").Data.(string)
			sep := ctx.MustArg("sep").Data.(string)
			parts := strings.Split(s, sep)
			out := make([]Value, len(parts))
			for i := range parts {
				out[i] = Str(parts[i])
			}
			return Arr(out)
		},
	)
	setBuiltinDoc(ip, "split", `Split a string on a separator (no regex).

If sep is empty (""), splits between UTF-8 code points.

Params:
  s: Str   — source string
  sep: Str — separator

Returns:
  [Str]`)

	ip.RegisterNative(
		"join",
		[]ParamSpec{{"xs", S{"array", S{"id", "Str"}}}, {"sep", S{"id", "Str"}}},
		S{"id", "Str"},
		func(_ *Interpreter, ctx CallCtx) Value {
			xs := ctx.MustArg("xs").Data.([]Value)
			sep := ctx.MustArg("sep").Data.(string)
			strs := make([]string, len(xs))
			for i := range xs {
				strs[i] = xs[i].Data.(string)
			}
			return Str(strings.Join(strs, sep))
		},
	)
	setBuiltinDoc(ip, "join", `Join strings with a separator.

Params:
  xs: [Str] — pieces to join
  sep: Str  — separator

Returns:
  Str`)

	// match(pattern: Str, s: Str) -> [Str]
	ip.RegisterNative(
		"match",
		[]ParamSpec{{"pattern", S{"id", "Str"}}, {"string", S{"id", "Str"}}},
		S{"array", S{"id", "Str"}},
		func(_ *Interpreter, ctx CallCtx) Value {
			pat := ctx.MustArg("pattern").Data.(string)
			s := ctx.MustArg("string").Data.(string)
			re, err := regexp.Compile(pat)
			if err != nil {
				// Soft error: invalid regex
				return annotNull("invalid regex: " + err.Error())
			}
			ms := re.FindAllString(s, -1)
			out := make([]Value, len(ms))
			for i := range ms {
				out[i] = Str(ms[i])
			}
			return Arr(out)
		},
	)
	setBuiltinDoc(ip, "match", `Find all non-overlapping matches of a regex.

Params:
  pattern: Str — RE2-compatible regular expression
  string:  Str — input

Returns:
  [Str] — matched substrings (no capture groups)`)

	// replace(pattern: Str, repl: Str, s: Str) -> Str
	ip.RegisterNative(
		"replace",
		[]ParamSpec{{"pattern", S{"id", "Str"}}, {"replace", S{"id", "Str"}}, {"string", S{"id", "Str"}}},
		S{"id", "Str"},
		func(_ *Interpreter, ctx CallCtx) Value {
			pat := ctx.MustArg("pattern").Data.(string)
			rep := ctx.MustArg("replace").Data.(string)
			s := ctx.MustArg("string").Data.(string)
			re, err := regexp.Compile(pat)
			if err != nil {
				// Soft error: invalid regex
				return annotNull("invalid regex: " + err.Error())
			}
			return Str(re.ReplaceAllString(s, rep))
		},
	)
	setBuiltinDoc(ip, "replace", `Replace all non-overlapping regex matches.

Params:
  pattern: Str — RE2-compatible regular expression
  replace: Str — replacement (no backrefs)
  string:  Str — input

Returns:
  Str`)
}
