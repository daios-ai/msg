// oracles.go

package mindscript

import (
	"encoding/json"
	"strings"
)

// execOracle: centralize prompt construction, backend dispatch, parsing, type-checking.
// Oracles can fail; the function returns T? (success value T, or null to signal failure).
//
// Current backend contract used by tests:
//
//	__oracle_execute(prompt: Str, inType: Type, outType: Type, examples: [Any]) -> Str | Null
func (ip *Interpreter) execOracle(funVal Value, _ *Env) Value {
	if funVal.Tag != VTFun {
		return annotNull("oracle: not a function")
	}
	f := funVal.Data.(*Fun)

	// --- Build prompt ---
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

	// Validate examples
	validated := ip.validateExamples(f.Examples, inT, baseOut, f.Env)

	// Locate executor
	hook, err := ip.Global.Get("__oracle_execute")
	if err != nil || hook.Tag != VTFun {
		return annotNull("oracle backend not configured (define __oracle_execute)")
	}

	// Call executor with *nullable* out type so backends can constrain outputs.
	res := ip.Apply(hook, []Value{
		Str(prompt),
		TypeVal(inT),
		TypeVal(outTNullable),
		Arr(validated),
	})

	// Null from executor = oracle failure (already annotated if backend wants).
	if res.Tag == VTNull {
		return res
	}
	if res.Tag != VTStr {
		return annotNull("oracle executor must return Str (raw model output) or null")
	}

	raw := strings.TrimSpace(res.Data.(string))
	raw = unwrapFenced(raw) // remove ``` … ``` if present

	// Always expect JSON for the *boxed* output.
	parsed, perr := ip.callGlobal1("jsonParse", Str(raw))
	if perr != "" {
		if raw == "null" {
			return annotNull("oracle returned null")
		}
		return annotNull("oracle output was not valid JSON")
	}
	if parsed.Tag == VTNull {
		if raw == "null" {
			return annotNull("oracle returned null")
		}
		return annotNull("oracle output was not valid JSON")
	}

	// Unbox: expect {"output": <T>} then validate <T> against T?
	if parsed.Tag != VTMap {
		return annotNull("oracle output did not match the declared return type")
	}
	mo := parsed.Data.(*MapObject)
	outVal, ok := mo.Entries["output"]
	if !ok {
		return annotNull("oracle output did not match the declared return type")
	}
	if !ip.isType(outVal, outTNullable, f.Env) {
		return annotNull("oracle output did not match the declared return type")
	}
	return outVal
}

// Build prompt: PROMPT, SCHEMAS, then TASK/INPUT/OUTPUT blocks.
func (ip *Interpreter) buildOraclePrompt(funVal Value, f *Fun) string {
	// 1) Gather ingredients (keep declared aliases to preserve annotations)
	instruction := strings.TrimSpace(funVal.Annot) // may be empty
	inTDeclared := S{"id", "Any"}
	if len(f.ParamTypes) == 1 {
		inTDeclared = f.ParamTypes[0] // keep alias to retain its .Annot
	}
	outBaseDeclared := stripNullable(f.ReturnType) // success type; keep alias

	// 2) Prefer userland hook if present
	if ip != nil && ip.Global != nil {
		if hook, err := ip.Global.Get("__oracle_build_prompt"); err == nil && hook.Tag == VTFun {
			ex := Arr(append([]Value(nil), f.Examples...))
			res := ip.Apply(hook, []Value{
				Str(instruction),
				TypeVal(inTDeclared),
				TypeVal(outBaseDeclared),
				ex,
			})
			if res.Tag == VTStr {
				return res.Data.(string)
			}
		}
	}

	// 3) Go fallback — emit exactly what the manual/test expects.

	pretty := func(m map[string]any) string {
		bs, err := json.MarshalIndent(m, "", "  ")
		if err != nil {
			return "{}"
		}
		return string(bs)
	}

	// ---- INPUT JSON SCHEMA (inner) ----
	// Resolve shape for content…
	inResolved := ip.resolveType(inTDeclared, f.Env)
	inSchemaInner := ip.jsonSchemaForTypeS(inResolved, f.Env)
	// …but pull the alias’ annotation for the "description".
	inAliasVal := ip.typeValForPrompt(inTDeclared, f.Env)
	if desc := strings.TrimSpace(inAliasVal.Annot); desc != "" {
		inSchemaInner["description"] = desc
	}

	// ---- OUTPUT JSON SCHEMA ----
	outInner := ip.TypeValueToJSONSchema(ip.typeValForPrompt(outBaseDeclared, f.Env), f.Env)
	boxedOutSchema := map[string]any{
		"type": "object",
		"properties": map[string]any{
			"output": map[string]any{
				"$ref": "#/$defs/Out",
			},
		},
		"required": []any{"output"},
		"$defs": map[string]any{
			"Out": outInner,
		},
	}

	var b strings.Builder

	// Header exactly as expected by the test/manual
	b.WriteString("PROMPT:\n")
	b.WriteString("Please follow the instruction to the best of your ability:\n")
	b.WriteString("for every input, provide an output that solves the task and\n")
	b.WriteString("respects the format of the OUTPUT JSON SCHEMA. Never put code\n")
	b.WriteString("fences around the output (like ```json); only generate valid JSON.\n\n")

	// Schemas
	b.WriteString("INPUT JSON SCHEMA:\n\n")
	b.WriteString(indentBlock(pretty(inSchemaInner), 2))
	b.WriteString("\n\n")

	b.WriteString("OUTPUT JSON SCHEMA:\n\n")
	b.WriteString(indentBlock(pretty(boxedOutSchema), 2))
	b.WriteString("\n\n")

	// Example blocks (render raw input; boxed output).
	taskLine := instruction
	if taskLine == "" {
		taskLine = "Given the input, determine the output."
	}
	for _, ex := range f.Examples {
		if ex.Tag != VTArray {
			continue
		}
		xs := ex.Data.([]Value)
		if len(xs) != 2 {
			continue
		}
		inJSON := compactJSONWithSpaces(valueToGoJSONable(xs[0]))
		outJSON := compactJSONWithSpaces(map[string]any{
			"output": valueToGoJSONable(xs[1]),
		})

		b.WriteString("TASK:\n\n")
		b.WriteString(taskLine + "\n\n")
		b.WriteString("INPUT:\n\n")
		b.WriteString(inJSON + "\n\n")
		b.WriteString("OUTPUT:\n\n")
		b.WriteString(outJSON + "\n\n")
	}

	// Final TASK with the current call's input (actual bound arg if we can find it)
	var curIn any = nil
	if v, ok := findOnlyBoundArg(f); ok {
		curIn = valueToGoJSONable(v)
	}
	b.WriteString("TASK:\n\n")
	b.WriteString(taskLine + "\n\n")
	b.WriteString("INPUT:\n\n")
	b.WriteString(compactJSONWithSpaces(curIn) + "\n\n")
	b.WriteString("OUTPUT:\n")

	return b.String()
}

// jsonSchemaForTypeS builds a JSON Schema (as map[string]any) from a resolved type S.
// It covers common constructs (object, array, primitives, nullable, enum).
func (ip *Interpreter) jsonSchemaForTypeS(t S, env *Env) map[string]any {
	if len(t) == 0 {
		return map[string]any{}
	}
	switch t[0].(string) {
	case "id":
		switch t[1].(string) {
		case "Any":
			return map[string]any{} // unconstrained
		case "Null":
			return map[string]any{"type": "null"}
		case "Bool":
			return map[string]any{"type": "boolean"}
		case "Int":
			return map[string]any{"type": "integer"}
		case "Num":
			return map[string]any{"type": "number"}
		case "Str":
			return map[string]any{"type": "string"}
		default:
			// Resolve aliases and recurse.
			rt := ip.resolveType(t, env)
			if !equalS(rt, t) {
				return ip.jsonSchemaForTypeS(rt, env)
			}
			return map[string]any{}
		}

	case "unop":
		// Nullable: ("unop","?", T) → anyOf [schema(T), {"type":"null"}]
		if len(t) >= 3 && t[1].(string) == "?" {
			inner := ip.jsonSchemaForTypeS(t[2].(S), env)
			return map[string]any{
				"anyOf": []any{inner, map[string]any{"type": "null"}},
			}
		}
		return map[string]any{}

	case "array":
		// Homogeneous arrays
		items := map[string]any{}
		if len(t) == 2 {
			items = ip.jsonSchemaForTypeS(t[1].(S), env)
		}
		return map[string]any{
			"type":  "array",
			"items": items,
		}

	case "map":
		props := map[string]any{}
		req := []any{}
		fields := mapTypeFields(t)
		for name, f := range fields {
			props[name] = ip.jsonSchemaForTypeS(f.typ, env)
			if f.required {
				req = append(req, name)
			}
		}
		m := map[string]any{
			"type":       "object",
			"properties": props,
		}
		if len(req) > 0 {
			m["required"] = req
		}
		return m

	case "enum":
		// Best-effort: map enum literal S-exprs to Go JSON-able and emit "enum"
		values := []any{}
		for i := 1; i < len(t); i++ {
			if v, ok := ip.litToValue(t[i].(S)); ok {
				values = append(values, valueToGoJSONable(v))
			}
		}
		if len(values) > 0 {
			return map[string]any{"enum": values}
		}
		return map[string]any{}

	default:
		return map[string]any{}
	}
}

// Convert a MindScript Value to a JSON-marshallable Go value.
func valueToGoJSONable(v Value) any {
	switch v.Tag {
	case VTNull:
		return nil
	case VTBool:
		return v.Data.(bool)
	case VTInt:
		return v.Data.(int64)
	case VTNum:
		return v.Data.(float64)
	case VTStr:
		return v.Data.(string)
	case VTArray:
		xs := v.Data.([]Value)
		out := make([]any, len(xs))
		for i := range xs {
			out[i] = valueToGoJSONable(xs[i])
		}
		return out
	case VTMap:
		mo := v.Data.(*MapObject)
		m := make(map[string]any, len(mo.Entries))
		for k, vv := range mo.Entries {
			m[k] = valueToGoJSONable(vv)
		}
		return m
	default:
		return nil
	}
}

// Marshal compact JSON but add spaces after ":" and "," to match test strings
func compactJSONWithSpaces(x any) string {
	b, err := json.Marshal(x)
	if err != nil {
		return "null"
	}
	s := string(b)
	s = strings.ReplaceAll(s, ":", ": ")
	s = strings.ReplaceAll(s, ",", ", ")
	return s
}

// Find the only directly-bound argument in the saturated oracle closure, if any.
func findOnlyBoundArg(f *Fun) (Value, bool) {
	if f == nil || f.Env == nil || f.Env.table == nil {
		return Null, false
	}
	if len(f.Env.table) != 1 {
		return Null, false
	}
	for _, v := range f.Env.table {
		return v, true
	}
	return Null, false
}

// Try to preserve annotations in schema rendering by reusing the actual VTType
// value from the environment when possible (so .Annot is kept). Fall back to
// synthesizing a VTType from S if not found.
func (ip *Interpreter) typeValForPrompt(t S, env *Env) Value {
	if len(t) >= 2 && t[0].(string) == "id" {
		name := t[1].(string)
		if env != nil {
			if v, err := env.Get(name); err == nil && v.Tag == VTType {
				return v // carries .Annot from declaration site
			}
		}
	}
	return TypeValIn(t, env)
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

// ---- standard helpers (unchanged) ----

func ensureNullableUnlessAny(t S) S {
	if isAnyType(t) || isNullable(t) {
		return t
	}
	return S{"unop", "?", t}
}
func isAnyType(t S) bool { return len(t) == 2 && t[0] == "id" && t[1] == "Any" }
func isNullable(t S) bool {
	return len(t) >= 3 && t[0] == "unop" && t[1] == "?"
}
func stripNullable(t S) S {
	for isNullable(t) {
		t = t[2].(S)
	}
	return t
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
func (ip *Interpreter) validateExamples(exs []Value, inT S, outBase S, env *Env) []Value {
	ok := make([]Value, 0, len(exs))
	for _, ex := range exs {
		if ex.Tag != VTArray {
			continue
		}
		xs := ex.Data.([]Value)
		if len(xs) != 2 {
			continue
		}
		vin, vout := xs[0], xs[1]
		if !ip.isType(vin, inT, env) {
			continue
		}
		if vout.Tag == VTNull {
			continue
		}
		if !ip.isType(vout, outBase, env) {
			continue
		}
		ok = append(ok, ex)
	}
	return ok
}
func (ip *Interpreter) callGlobal1(name string, arg Value) (v Value, errStr string) {
	fn, err := ip.Global.Get(name)
	if err != nil || fn.Tag != VTFun {
		v = annotNull(name + " is not available; load the standard library")
		errStr = "missing"
		return
	}
	defer func() {
		if r := recover(); r != nil {
			switch e := r.(type) {
			case rtErr:
				v = annotNull(e.msg)
			default:
				v = annotNull("runtime error")
			}
			errStr = v.Annot
		}
	}()
	v = ip.Apply(fn, []Value{arg})
	if v.Tag == VTNull && v.Annot != "" {
		errStr = v.Annot
	}
	return
}
