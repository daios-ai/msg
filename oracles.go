// oracles.go: Oracle execution & prompt building for MindScript
//
// What this file does
// -------------------
// MindScript “oracles” are functions whose implementation is delegated to an
// external model (LLM or other backend). At runtime, calling an oracle builds
// a prompt from four ingredients:
//
//  1. The oracle’s *instruction* (the function value’s .Annot text).
//  2. The declared input type (parameter 0) and output success type
//     (return type, made nullable operationally).
//  3. Optional example pairs [input, output] for few-shot guidance.
//  4. The current call’s concrete input.
//
// The prompt is then dispatched to a pluggable backend called
// `__oracle_execute`. The backend returns either a raw string (model output)
// or `null` to signal failure. The raw string is expected to be **valid JSON**
// containing a single field:
//
//	{"output": <value>}
//
// The interpreter parses that JSON (via the standard library’s `jsonParse`),
// validates `<value>` against the oracle’s **declared** return type (made
// nullable at call-time), and returns the value or an annotated `null` to
// signal a typed runtime failure.
//
// Public API (this file)
// ----------------------
//   - `(*Interpreter) LastOraclePrompt() string`
//     Exposes the last prompt constructed for an oracle call. This is useful
//     for testing and REPL exploration. It is *read-only* state.
//
// Internal responsibilities (private here)
// ----------------------------------------
//
//   - `(*Interpreter) execOracle(...) Value`
//     The oracle call engine used by the interpreter when executing an oracle
//     function. It builds the prompt, calls `__oracle_execute`, parses and
//     type-checks the result, and returns a nullable value (success or failure).
//
//   - `(*Interpreter) buildOraclePrompt(...) string`
//     Constructs the portable prompt text. If a userland hook named
//     `__oracle_build_prompt` is defined (a native MindScript function), it is
//     given the opportunity to produce a custom prompt; otherwise a Go fallback
//     prompt (documented below) is emitted. The fallback layout contains:
//
//     PROMPT:
//     <generic guidance + instruction>
//
//     INPUT JSON SCHEMA:
//     <pretty JSON Schema for the input type, using the alias annotation
//     for "description" when available>
//
//     OUTPUT JSON SCHEMA:
//     <pretty JSON Schema for an *object* with a required "output" field
//     whose schema is the declared success type>
//
//     TASK / INPUT / OUTPUT blocks:
//
//   - One block per example, then one final block for the current input.
//
//   - Example OUTPUT blocks are the **boxed** object with the "output" key.
//
// - JSON Schema helpers for types & values used by the prompt builder.
//
// Dependencies on other files
// ---------------------------
// - interpreter.go
//   - `Interpreter` type and fields (`Global`, `oracleLastPrompt`).
//   - Value model: `Value`, `ValueTag`, constructors (e.g., `Str`, `Arr`),
//     `Fun`, `Env`, and helper `annotNull`.
//   - Engine hooks: `(*Interpreter) Apply` and `(*Interpreter) resolveType`,
//     as well as type checks `isType`.
//
// - types.go
//   - Type representation `S` (S-exprs) and helpers like `mapTypeFields`,
//     `equalS`, and literal conversions `litToValue`.
//
// - schema.go
//   - `(*Interpreter) TypeValueToJSONSchema(...)` to materialize JSON Schema
//     for a (boxed) output type when building prompts.
//
// - std/lib.ms (MindScript native standard library):
//   - A global function `jsonParse : Str -> Any` must be present for parsing
//     raw model JSON. This is looked up from the global environment.
//
// - External hook contracts (MindScript space):
//   - `__oracle_execute(prompt: Str, inType: Type, outType: Type, examples: [Any])
//     -> Str | Null`
//     Backends must register this function. Returning `null` signals oracle
//     failure; returning `Str` must be *raw JSON* without code fences.
//   - Optional: `__oracle_build_prompt(instruction: Str, inType: Type,
//     outType: Type, examples: [Any]) -> Str`
//     If present, it can fully control the prompt string.
//
// Error & type semantics
// ----------------------
//   - Oracles are *operationally nullable*: if an oracle’s declared return type
//     is `T`, the runtime treats it as `T?` (nullable) when validating results.
//   - `null` results indicate failure; an *annotated* null carries a reason.
//   - Output JSON must be an object with an `"output"` key whose value conforms
//     to `T?` (after the runtime widens). Any mismatch yields an annotated null.
//
// NOTE: Only `LastOraclePrompt` is exported from this file. The rest is used
// internally by the interpreter when an oracle function is called.
package mindscript

import (
	"encoding/json"
	"fmt"
	"strings"
)

/* ===========================
   PUBLIC
   =========================== */

// LastOraclePrompt returns the most recent prompt string that the interpreter
// constructed for an oracle call during this process. This is intended for
// testing, logging, and REPL inspection.
//
// Behavior
//   - If no oracle has been executed yet, it returns the empty string.
//   - Reading this value has no side effects and does not mutate any state.
//
// Notes
//   - The prompt format depends on whether your program defines the optional
//     `__oracle_build_prompt` hook. If present, that hook’s output becomes the
//     recorded prompt; otherwise the built-in fallback format is recorded.
//   - This function does not trigger any oracle execution by itself.
func (ip *Interpreter) LastOraclePrompt() string { return ip.oracleLastPrompt }

//// END_OF_PUBLIC

/* ===========================
   PRIVATE
   =========================== */

// execOracle is a CallCtx-based stub used during bring-up.
// It prints the parameter types, return type, arguments (in declared order),
// and examples. Then it returns a value that conforms to the oracle's
// operationally-nullable return type (T?).

// execOracle is a CallCtx-based stub used during bring-up.
// It prints declared parameter types (and their resolved forms), the declared
// and operational (nullable) return type, the current call's argument values,
// and the provided examples. Then it returns a value that conforms to the
// oracle's operationally-nullable return type (T?).
// execOracle builds the oracle prompt, calls the backend __exec_oracle(prompt),
// parses the JSON result, validates it against the operational return type (T?),
// and returns the unboxed value (or annotated null on failure).
func (ip *Interpreter) execOracle(funVal Value, ctx CallCtx) Value {
	if funVal.Tag != VTFun {
		return annotNull("oracle: not a function")
	}
	f := funVal.Data.(*Fun)

	// ---- Recover declared signature from hidden bindings in the closure env ----
	var paramNames []string
	var declTypes []S
	if f.Env != nil {
		if nv, err := f.Env.Get("$__sig_names"); err == nil && nv.Tag == VTArray {
			for _, v := range nv.Data.([]Value) {
				if v.Tag == VTStr {
					paramNames = append(paramNames, v.Data.(string))
				}
			}
		}
		if tv, err := f.Env.Get("$__sig_types"); err == nil && tv.Tag == VTArray {
			for _, v := range tv.Data.([]Value) {
				if v.Tag == VTType {
					if tvv, ok := v.Data.(*TypeValue); ok {
						declTypes = append(declTypes, tvv.Ast)
					}
				}
			}
		}
	}

	// ---- Build boxes and the prompt ----
	// Input map value from current arguments.
	inArgsVal := valueMapFromArgs(ctx, paramNames) // VTMap holding current call args

	// Vector of declared parameter types (as VTType values).
	inTypesVal := Arr(func() []Value {
		arr := make([]Value, len(declTypes))
		for i := range declTypes {
			arr[i] = TypeVal(declTypes[i])
		}
		return arr
	}())

	// Boxed output type: {"output": T}, where T is the *declared* success type.
	outBoxTypeS := S{"map", S{"pair!", S{"str", "output"}, stripNullable(f.ReturnType)}}

	// Normalize examples into [inputMap, outputMap] pairs.
	exPairs := boxExamplesAsMaps(f.Examples, paramNames)
	examplesVal := Arr(exPairs)

	// Instruction = function annotation (task description).
	instructionVal := Str(strings.TrimSpace(funVal.Annot))

	// Build prompt text and record it.
	prompt := ip.buildOraclePromptV(
		instructionVal,       // Str
		inArgsVal,            // Any (VTMap of current arguments)
		inTypesVal,           // [Type] (declared param types)
		TypeVal(outBoxTypeS), // Type  (boxed {"output": T})
		examplesVal,          // [Any] ([inputMap, outputMap] pairs)
		f.Env,
	)
	ip.oracleLastPrompt = prompt

	// ---- Call backend: __exec_oracle(prompt: Str) -> Str | Null ----
	hook, err := ip.Global.Get("__oracle_execute")
	if err != nil || hook.Tag != VTFun {
		return annotNull("oracle backend not configured (define __exec_oracle)")
	}
	res := ip.Apply(hook, []Value{Str(prompt)})

	// Backend can signal failure with null.
	if res.Tag == VTNull {
		return res
	}
	if res.Tag != VTStr {
		fmt.Print(FormatValue(res))
		return annotNull("oracle executor must return Str (raw model output) or null")
	}

	// ---- Parse returned JSON and validate it matches {"output": T?} ----
	raw := strings.TrimSpace(res.Data.(string))
	raw = unwrapFenced(raw) // tolerate accidental ```json fences

	parsed, perr := ip.callGlobal1("jsonParse", Str(raw))
	if perr != "" {
		if raw == "null" {
			return annotNull("oracle returned null")
		}
		return annotNull("oracle output was not valid JSON")
	}
	if parsed.Tag != VTMap {
		return annotNull("oracle output did not match the declared return type")
	}
	mo := parsed.Data.(*MapObject)
	outVal, ok := mo.Entries["output"]
	if !ok {
		return annotNull("oracle output did not match the declared return type")
	}

	// Validate against operationally nullable return type (T?).
	outTNullable := ensureNullableUnlessAny(f.ReturnType)
	if !ip.isType(outVal, outTNullable, f.Env) {
		return annotNull("oracle output did not match the declared return type")
	}
	return outVal
}

// buildOraclePromptV builds the oracle prompt from the pieces you specified.
// Signature (as requested):
//
//	instruction : Value(Str)
//	input       : Value(Any)   (boxed map of actual call arguments)
//	inTypes     : Value([Type])  (declared parameter types, order matches names)
//	outType     : Value(Type)    (declared *success* type; exec makes it nullable)
//	examples    : Value([Any])   (each item is a boxed map: {input: {..}, output: {...}} or [inputMap, outputMap])
func (ip *Interpreter) buildOraclePromptV(
	instruction Value, // Str
	input Value, // Any (actually a VTMap with param-name keys)
	inTypes Value, // [Type] (declared param types in declared order)
	outType Value, // Type   (boxed {"output": T} type)
	examples Value, // [Any]  (each item is [inputMap, outputMap] or {"input":..,"output":..})
	env *Env,
) string {
	// --- Derive the input MAP TYPE (names + types) from `input` and `inTypes` ---
	names := []string{}
	if input.Tag == VTMap {
		mo := input.Data.(*MapObject)
		// Preserve insertion/declared order
		for _, k := range mo.Keys {
			names = append(names, k)
		}
	}
	declTypes := []S{}
	if inTypes.Tag == VTArray {
		for _, v := range inTypes.Data.([]Value) {
			if v.Tag == VTType {
				if tv, ok := v.Data.(*TypeValue); ok {
					declTypes = append(declTypes, tv.Ast)
				}
			}
		}
	}
	// Build an S for the input map type
	inMapTypeS := mapTypeFromParams(names, declTypes)

	// Pretty-print a schema from a VTType
	toSchemaString := func(tv Value) string {
		s := ip.TypeValueToJSONSchema(tv, env)
		b, err := json.MarshalIndent(s, "", "  ")
		if err != nil {
			return "{}"
		}
		return string(b)
	}

	// INPUT schema from the derived input map TYPE
	inputSchema := toSchemaString(TypeVal(inMapTypeS))
	// OUTPUT schema from the boxed output TYPE ({"output": T})
	outputSchema := toSchemaString(outType)

	// Example/arg JSON stringifier
	toJSON := func(v Value) string {
		j, err := valueToGoJSON(v)
		if err != nil {
			return "null"
		}
		b, err := json.Marshal(j)
		if err != nil {
			return "null"
		}
		s := string(b)
		s = strings.ReplaceAll(s, ":", ": ")
		s = strings.ReplaceAll(s, ",", ", ")
		return s
	}

	var bld strings.Builder

	// Header & guidance
	bld.WriteString("PROMPT:\n")
	bld.WriteString("Please follow the instruction to the best of your ability:\n")
	bld.WriteString("for every input, provide an output that solves the task and\n")
	bld.WriteString("respects the format of the OUTPUT JSON SCHEMA. Never put code\n")
	bld.WriteString("fences around the output (like ```json); only generate valid JSON.\n\n")

	// Schemas
	bld.WriteString("INPUT JSON SCHEMA:\n\n")
	bld.WriteString(indentBlock(inputSchema, 2))
	bld.WriteString("\n\n")

	bld.WriteString("OUTPUT JSON SCHEMA:\n\n")
	bld.WriteString(indentBlock(outputSchema, 2))
	bld.WriteString("\n\n")

	// Instruction line
	taskLine := "Given the input, determine the output."
	if instruction.Tag == VTStr && strings.TrimSpace(instruction.Data.(string)) != "" {
		taskLine = strings.TrimSpace(instruction.Data.(string))
	}

	// Examples
	if examples.Tag == VTArray {
		for _, ex := range examples.Data.([]Value) {
			var inVal, outVal Value
			switch ex.Tag {
			case VTArray:
				pair := ex.Data.([]Value)
				if len(pair) != 2 {
					continue
				}
				inVal, outVal = pair[0], pair[1]
			case VTMap:
				mo := ex.Data.(*MapObject)
				inVal = mo.Entries["input"]
				outVal = mo.Entries["output"]
			default:
				continue
			}

			bld.WriteString("TASK:\n\n")
			bld.WriteString(taskLine + "\n\n")
			bld.WriteString("INPUT:\n\n")
			bld.WriteString(toJSON(inVal) + "\n\n")
			bld.WriteString("OUTPUT:\n\n")
			bld.WriteString(toJSON(outVal) + "\n\n")
		}
	}

	// Final TASK with the current call's input
	bld.WriteString("TASK:\n\n")
	bld.WriteString(taskLine + "\n\n")
	bld.WriteString("INPUT:\n\n")
	bld.WriteString(toJSON(input) + "\n\n")
	bld.WriteString("OUTPUT:\n")

	return bld.String()
}

// --------- helpers --------------------

// Build a map type S from parameter names and declared types.
func mapTypeFromParams(names []string, types []S) S {
	m := S{"map"}
	for i := 0; i < len(names) && i < len(types); i++ {
		m = append(m, S{"pair!", S{"str", names[i]}, types[i]})
	}
	return m
}

// Box current call arguments (from ctx) into a map Value conforming to the input map type.
func valueMapFromArgs(ctx CallCtx, names []string) Value {
	entries := make(map[string]Value, len(names))
	keys := make([]string, 0, len(names))
	for _, name := range names {
		v, _ := ctx.Arg(name) // if missing, leave zero value (omitted)
		entries[name] = v
		keys = append(keys, name)
	}
	return Value{Tag: VTMap, Data: &MapObject{Entries: entries, KeyAnn: map[string]string{}, Keys: keys}}
}

// Box examples: each example is [inVal, outVal] where inVal can be tuple/array or map.
// We normalize to maps: input → map[name]=..., output → map{"output": outVal}.
func boxExamplesAsMaps(exs []Value, names []string) []Value {
	out := make([]Value, 0, len(exs))
	for _, ex := range exs {
		if ex.Tag != VTArray {
			continue
		}
		pair := ex.Data.([]Value)
		if len(pair) != 2 {
			continue
		}
		inV, outV := pair[0], pair[1]

		// Normalize input to a map value using param order
		inMap := map[string]Value{}
		keys := []string{}
		if inV.Tag == VTArray {
			xs := inV.Data.([]Value)
			for i := 0; i < len(xs) && i < len(names); i++ {
				inMap[names[i]] = xs[i]
				keys = append(keys, names[i])
			}
		} else if inV.Tag == VTMap {
			mo := inV.Data.(*MapObject)
			for _, k := range mo.Keys {
				inMap[k] = mo.Entries[k]
				keys = append(keys, k)
			}
		} else {
			// Single-arg case: use first name
			if len(names) > 0 {
				inMap[names[0]] = inV
				keys = append(keys, names[0])
			}
		}
		inVal := Value{Tag: VTMap, Data: &MapObject{Entries: inMap, KeyAnn: map[string]string{}, Keys: keys}}

		outVal := Value{Tag: VTMap, Data: &MapObject{
			Entries: map[string]Value{"output": outV},
			KeyAnn:  map[string]string{},
			Keys:    []string{"output"},
		}}

		// Store as a 2-tuple [inputMap, outputMap]
		out = append(out, Arr([]Value{inVal, outVal}))
	}
	return out
}

// defaultValueForType returns a simple default Value that conforms to the given type S.
// It respects nullable types by stripping '?' and producing a value for the base type.
// For unknown/complex cases, it returns Null (which conforms to T? at oracle call sites).
func (ip *Interpreter) defaultValueForType(t S, env *Env) Value {
	base := stripNullable(t)

	// Built-in ids
	if len(base) >= 2 {
		if tag, _ := base[0].(string); tag == "id" {
			switch base[1].(string) {
			case "Any":
				return Null
			case "Null":
				return Null
			case "Bool":
				return Bool(false)
			case "Int":
				return Int(0)
			case "Num":
				return Num(0)
			case "Str":
				return Str("")
			default:
				// Unknown alias: resolve once and retry; if no change, return Null.
				rt := ip.resolveType(base, env)
				if !equalS(rt, base) {
					return ip.defaultValueForType(rt, env)
				}
				return Null
			}
		}
	}

	// Structural forms
	if len(base) >= 1 {
		switch base[0].(string) {
		case "array":
			return Arr([]Value{})
		case "map":
			// Empty map satisfies open-world objects; required keys are not enforced here.
			return Map(map[string]Value{})
		case "enum":
			// Try to pick the first literal if present.
			if len(base) > 1 {
				if v, ok := ip.litToValue(base[1].(S)); ok {
					return v
				}
			}
			return Null
		}
	}

	return Null
}

// execOracle centralizes prompt construction, backend dispatch, parsing,
// and type-checking. It returns a nullable value (success or failure).
//
// Current backend contract:
//
//	__oracle_execute(prompt: Str, inType: Type, outType: Type, examples: [Any]) -> Str | Null
func (ip *Interpreter) execOracle2(funVal Value, _ *Env) Value {
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
	// Oracle returns are always nullable: T?  (but Any? == Any → don't wrap)
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

// buildOraclePrompt constructs the prompt string for a given oracle function.
// If a userland `__oracle_build_prompt` exists, it is used; otherwise a
// deterministic fallback prompt is produced as documented in the file header.
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

	// Boxed output schema
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

// jsonSchemaForTypeS builds a JSON Schema (as map[string]any) from a **resolved**
// type S. It covers common constructs (object, array, primitives, nullable, enum).
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

// valueToGoJSONable converts a MindScript Value to a JSON-marshallable Go value.
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

// compactJSONWithSpaces marshals compact JSON but adds spaces after ":" and ","
// to match test strings (e.g., {"a": 1, "b": 2}).
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

// findOnlyBoundArg returns the only directly-bound argument in the saturated
// oracle closure, if any. This is used to render the final TASK/INPUT block.
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

// typeValForPrompt tries to preserve annotations in schema rendering by reusing
// the actual VTType value from the environment (so .Annot is kept). Falls back
// to synthesizing a VTType from S if not found.
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

// indentBlock prefixes each line in s with n spaces (used by prompt builder).
func indentBlock(s string, n int) string {
	pad := strings.Repeat(" ", n)
	lines := strings.Split(s, "\n")
	for i := range lines {
		lines[i] = pad + lines[i]
	}
	return strings.Join(lines, "\n")
}

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

// callGlobal1 invokes a global unary function by name and traps runtime errors,
// returning the value and a non-empty error string when a failure is produced.
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
