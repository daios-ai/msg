package mindscript

import (
	"fmt"
	"strings"
)

// execOracle: centralize parsing + type-checking; oracle returns are always nullable.
//
// Contract with userland:
//
//	__oracle_execute(prompt: Str, inType: Type, outType: Type, examples: [Any]) -> Str | Null
func (ip *Interpreter) execOracle(funVal Value, _ *Env) Value {
	if funVal.Tag != VTFun {
		return annotNull("oracle: not a function")
	}
	f := funVal.Data.(*Fun)

	// Build prompt (unchanged)
	prompt := ip.buildOraclePrompt(funVal, f)
	ip.oracleLastPrompt = prompt

	// Resolve types
	inT := S{"id", "Any"}
	if len(f.ParamTypes) == 1 {
		inT = ip.resolveType(f.ParamTypes[0], f.Env)
	}
	declOutT := ip.resolveType(f.ReturnType, f.Env) // declared success type T

	// Oracle returns are *always* nullable: T?  (but Any? == Any → don't wrap)
	outTNullable := ensureNullableUnlessAny(declOutT)
	baseOut := stripNullable(outTNullable) // base(T) after making nullable

	// Locate executor
	hook, err := ip.Global.Get("__oracle_execute")
	if err != nil || hook.Tag != VTFun {
		return annotNull("oracle backend not configured (define __oracle_execute)")
	}

	// Call executor with *nullable* out type so prompts/schemas are accurate
	res := ip.Apply(hook, []Value{
		Str(prompt),
		TypeVal(inT),
		TypeVal(outTNullable),
		Arr(append([]Value(nil), f.Examples...)),
	})

	// Null from executor = legitimate oracle failure (already annotated if any)
	if res.Tag == VTNull {
		return res
	}
	if res.Tag != VTStr {
		return annotNull("oracle executor must return Str (raw model output) or null")
	}

	raw := strings.TrimSpace(res.Data.(string))
	raw = unwrapFenced(raw) // remove ```…``` if present

	// If success base type is Str, accept raw string directly,
	// but literal "null" MUST be treated as an error (annotated).
	if isIdType(baseOut, "Str") {
		if raw == "null" {
			return annotNull("oracle returned null")
		}
		return Str(raw)
	}

	// Otherwise expect JSON; use userland jsonParse.
	parsed, perr := ip.callGlobal1("jsonParse", Str(raw))
	if perr != "" {
		// Any non-JSON is an error; literal "null" also errors but gets a clearer message.
		if raw == "null" {
			return annotNull("oracle returned null")
		}
		return annotNull("oracle output was not valid JSON")
	}

	// If jsonParse produced Null, that's still an error in MindScript.
	// Use the same uniform message unless the raw text was literally "null".
	if parsed.Tag == VTNull {
		if raw == "null" {
			return annotNull("oracle returned null")
		}
		return annotNull("oracle output was not valid JSON")
	}

	// Validate against T? (nullable)
	if !ip.isType(parsed, outTNullable, f.Env) {
		return annotNull("oracle output did not match the declared return type")
	}
	return parsed
}

// buildOraclePrompt assembles a readable prompt using the value’s annotation,
// resolved input/output types, and examples. If a userland hook
// __oracle_build_prompt exists, it is preferred.
func (ip *Interpreter) buildOraclePrompt(funVal Value, f *Fun) string {
	// 1) Gather ingredients
	instruction := strings.TrimSpace(funVal.Annot)
	if instruction == "" {
		instruction = "You are a helpful assistant. Follow the function signature and examples."
	}
	inT := S{"id", "Any"}
	if len(f.ParamTypes) == 1 {
		inT = ip.resolveType(f.ParamTypes[0], f.Env)
	}
	outT := ip.resolveType(f.ReturnType, f.Env)

	// 2) Prefer MindScript hook: __oracle_build_prompt
	if ip != nil && ip.Global != nil {
		if hook, err := ip.Global.Get("__oracle_build_prompt"); err == nil && hook.Tag == VTFun {
			ex := Arr(append([]Value(nil), f.Examples...))
			res := ip.Apply(hook, []Value{
				Str(instruction),
				TypeVal(inT),
				TypeVal(outT),
				ex,
			})
			if res.Tag == VTStr {
				return res.Data.(string)
			}
			// Fall through to Go fallback if hook misbehaves.
		}
	}

	// 3) Go fallback using the pretty printers already in the runtime
	var b strings.Builder
	fmt.Fprintf(&b, "%s\n\n", instruction)
	fmt.Fprintf(&b, "INPUT TYPE:\n%s\n\n", indentBlock(FormatType(inT), 2))
	fmt.Fprintf(&b, "OUTPUT TYPE:\n%s\n\n", indentBlock(FormatType(outT), 2))

	if len(f.Examples) > 0 {
		fmt.Fprintf(&b, "EXAMPLES:\n")
		for i, ex := range f.Examples {
			if ex.Tag == VTArray {
				xs := ex.Data.([]Value)
				if len(xs) == 2 {
					inStr := FormatValue(xs[0])
					outStr := FormatValue(xs[1])
					fmt.Fprintf(&b, "  #%d\n  INPUT:\n%s\n  OUTPUT:\n%s\n",
						i+1, indentBlock(inStr, 4), indentBlock(outStr, 4))
					continue
				}
			}
			fmt.Fprintf(&b, "  #%d\n  %s\n", i+1, indentBlock(FormatValue(ex), 4))
		}
		fmt.Fprintln(&b)
	}

	fmt.Fprintf(&b, "TASK:\n")
	fmt.Fprintf(&b, "Given the INPUT (which conforms to the INPUT TYPE), produce an OUTPUT that conforms to the OUTPUT TYPE.\n")
	fmt.Fprintf(&b, "Respond with only the content of the output (no code fences or extra prose).\n")
	return b.String()
}

// helper for formatting blocks
func indentBlock(s string, n int) string {
	pad := strings.Repeat(" ", n)
	lines := strings.Split(s, "\n")
	for i := range lines {
		lines[i] = pad + lines[i]
	}
	return strings.Join(lines, "\n")
}

// Expose the last prompt for testing / REPL demo.
func (ip *Interpreter) LastOraclePrompt() string { return ip.oracleLastPrompt }

// ---- small helpers (engine-side only) ----

// ---- helpers ----

// ensureNullableUnlessAny: wrap t as ("? t") unless already nullable or t == Any.
// (Any? == Any, so leave Any untouched.)
func ensureNullableUnlessAny(t S) S {
	if isAnyType(t) || isNullable(t) {
		return t
	}
	return S{"unop", "?", t}
}

func isAnyType(t S) bool {
	return len(t) == 2 && t[0] == "id" && t[1] == "Any"
}

func isNullable(t S) bool {
	return len(t) >= 3 && t[0] == "unop" && t[1] == "?" // ("unop","?", T)
}

func stripNullable(t S) S {
	for isNullable(t) {
		t = t[2].(S)
	}
	return t
}

func isIdType(t S, name string) bool {
	return len(t) == 2 && t[0] == "id" && t[1] == name
}

func unwrapFenced(s string) string {
	if strings.HasPrefix(s, "```") {
		if i := strings.IndexByte(s, '\n'); i >= 0 {
			s = s[i+1:]
		} else {
			return s
		}
		if j := strings.LastIndex(s, "```"); j >= 0 {
			s = s[:j]
		}
		s = strings.TrimSpace(s)
	}
	return s
}

// callGlobal1 calls a global builtin (like jsonParse) with one arg.
// Returns (value, errorAnnotation). errorAnnotation is "" on success.
func (ip *Interpreter) callGlobal1(name string, arg Value) (Value, string) {
	fn, err := ip.Global.Get(name)
	if err != nil || fn.Tag != VTFun {
		return annotNull(name + " is not available; load the standard library"), "missing"
	}
	// Safely apply; convert runtime panics into annotated null.
	defer func() {
		if r := recover(); r != nil {
			// vm wraps errors as annotated nulls already, but normalize here:
			switch e := r.(type) {
			case rtErr:
				arg = annotNull(e.msg) // reuse arg as a scratch to carry error back (ignored)
			default:
				arg = annotNull("runtime error")
			}
		}
	}()
	v := ip.Apply(fn, []Value{arg})
	// If we got here via panic recovery above, v might still be zero.
	// Treat annotated nulls as errors for our call site.
	if v.Tag == VTNull && v.Annot != "" {
		return v, v.Annot
	}
	return v, ""
}
