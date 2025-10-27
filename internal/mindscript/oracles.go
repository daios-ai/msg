// oracles.go — Oracle execution & prompt building (PUBLIC API + PRIVATE impl)
//
// WHAT THIS FILE DOES
// ===================
// MindScript “oracles” are functions whose execution is delegated to an external
// backend (e.g., an LLM). When an oracle is called at runtime, the interpreter:
//
//  1. Builds a **prompt** that captures:
//     • the oracle’s instruction (taken from the function Value’s .Annot),
//     • the declared input type (parameter 0) and declared *success* return type,
//     • optional example pairs [input, output] for few-shot guidance,
//     • the current call’s concrete input,
//     and records that prompt internally (for debugging and testing).
//
//  2. Calls a pluggable backend hook in user space:
//     __oracle_execute(prompt: Str, inType: Type, outType: Type, examples: [Any])
//     which must return either:
//     • Str — raw JSON (no fences) shaped like {"output": <value>}, or
//     • Null — to signal failure.
//     The interpreter parses the JSON via the host-provided global function
//     jsonParse : Str -> Any (from the standard library).
//
//  3. Validates the extracted value against the oracle’s **operational** return
//     type, which is the declared success type widened to **nullable** (`T?`).
//     On mismatch or parse failure, an *annotated null* Value is returned.
//
// Everything in this file is **private** implementation that the public
// interpreter uses internally when executing oracle functions (see Interpreter
// in interpreter.go). Public callers never need to invoke those directly.
//
// DEPENDENCIES (OTHER FILES)
// --------------------------
// • interpreter.go
//   - type Interpreter (fields: Global, etc.)
//   - Value model (Value, ValueTag, constructors like Str/Arr/Map, Fun, Env)
//   - Thin call/eval surface (*Interpreter.Apply, resolveType, isType, etc.)
//   - Annotated-null helpers (annotNull), SourceRef handling.
//
// • types.go
//   - Type S-expressions and helpers (mapTypeFields, equalS, litToValue).
//
// • schema.go
//   - (*Interpreter) TypeValueToJSONSchema(...) to render JSON Schema for types.
//
// • std/lib.ms (MindScript standard library in user space)
//   - jsonParse : Str -> Any  — required for parsing model JSON.
//
// • Runtime hooks (user space, optional/required):
//   - REQUIRED:  __oracle_execute(prompt: Str) -> Str | Null
//   - OPTIONAL:  __oracle_build_prompt(instruction: Str, inType: Type, outType: Type, examples: [Any]) -> Str
//
// NOTE
// ----
// Oracles are **operationally nullable**: a declared return type T is validated
// as T? at runtime. The backend must produce boxed JSON {"output": <value>}.
//
// ──────────────────────────────────────────────────────────────────────────────
// PUBLIC API
// ──────────────────────────────────────────────────────────────────────────────
package mindscript

import (
	"encoding/json"
	"strings"
)

/* ===========================
   PUBLIC
   =========================== */

//// END_OF_PUBLIC

/* ===========================
   PRIVATE IMPLEMENTATION
   =========================== */

// execOracle is the primary oracle call engine used by the interpreter when an
// oracle function is invoked. It builds the prompt, calls the backend hook,
// parses & validates, and returns a nullable Value (success or annotated failure).
func (ip *Interpreter) execOracle(funVal Value, ctx CallCtx) Value {
	if funVal.Tag != VTFun {
		return annotNull("oracle: not a function")
	}
	f := funVal.Data.(*Fun)

	// Recover original declaration signature from Fun.Sig (stable across currying).
	paramNames := f.Params
	declTypes := f.ParamTypes
	if f.Sig != nil {
		paramNames = f.Sig.Names
		declTypes = f.Sig.Types
	}

	// Build runtime boxes and the prompt.
	inArgsVal := valueMapFromArgs(ctx, paramNames) // VTMap holding current call args

	inTypesVal := Arr(func() []Value {
		arr := make([]Value, len(declTypes))
		for i := range declTypes {
			arr[i] = TypeValIn(declTypes[i], f.Env)
		}
		return arr
	}())

	// Boxed output type: {"output": T}, where T is the declared success type (non-nullable here).
	outBoxTypeS := S{"map", S{"pair!", S{"str", "output"}, stripNullable(f.ReturnType)}}

	exPairs := boxExamplesAsMaps(f.Examples, paramNames)
	examplesVal := Arr(exPairs)

	instructionVal := Str(strings.TrimSpace(funVal.Annot))

	// Build prompt text (prefers user hook when available) and record it.
	prompt := ip.buildOraclePrompt(
		instructionVal,
		inArgsVal,
		inTypesVal,
		TypeValIn(outBoxTypeS, f.Env),
		examplesVal,
		f.Env,
	)

	// Locate executor hook strictly via the oracle's lexical environment.
	hook, err := f.Env.Get("__oracle_execute")
	if err != nil || hook.Tag != VTFun {
		return annotNull("oracle backend not configured (define __oracle_execute)")
	}
	res := ip.Apply(hook, []Value{Str(prompt)})

	// Backend can signal failure with null.
	if res.Tag == VTNull {
		return res
	}
	if res.Tag != VTStr {
		return annotNull("oracle executor must return a string (raw model output) or null")
	}

	// Parse returned JSON and validate it matches {"output": T?}.
	raw := strings.TrimSpace(res.Data.(string))
	decoded, _, _ := jsonRepair(raw)
	if decoded == nil {
		return annotNull("oracle output was not valid JSON")
	}
	v := goJSONToValue(decoded)
	if v.Tag != VTMap {
		v = Value{Tag: VTMap, Data: &MapObject{
			Entries: map[string]Value{"output": v},
			Keys:    []string{"output"},
		}}
	}
	mo := v.Data.(*MapObject)
	out, ok := mo.Entries["output"]
	if !ok || !ip.isType(out, ensureNullableUnlessAny(f.ReturnType), f.Env) {
		return annotNull("oracle output did not match the declared return type")
	}
	return out
}

// buildOraclePrompt builds the prompt from Value-boxed parts.
// instruction: Str
// input:       Any (VTMap of call arguments)
// inTypes:     [Type] (declared param types, in order)
// outType:     Type   (boxed {"output": T})
// examples:    [Any]  (each item is [inputMap, outputMap] or {"input":..,"output":..})
func (ip *Interpreter) buildOraclePrompt(
	instruction Value,
	input Value,
	inTypes Value,
	outType Value,
	examples Value,
	env *Env,
) string {
	// Derive input map type S from (names, declared types).
	names := []string{}
	if input.Tag == VTMap {
		mo := input.Data.(*MapObject)
		names = append(names, mo.Keys...)
		// for _, k := range mo.Keys {
		// 	names = append(names, k)
		// }
	}
	declTypes := []S{}
	if inTypes.Tag == VTArray {
		for _, v := range inTypes.Data.(*ArrayObject).Elems {
			if v.Tag == VTType {
				if tv, ok := v.Data.(*TypeValue); ok {
					declTypes = append(declTypes, tv.Ast)
				}
			}
		}
	}
	inMapTypeS := mapTypeFromParams(names, declTypes)

	// Pretty-print schemas.
	toSchemaString := func(tv Value) string {
		s := ip.TypeValueToJSONSchema(tv, env)
		b, err := json.MarshalIndent(s, "", "  ")
		if err != nil {
			return "{}"
		}
		return string(b)
	}
	inputSchema := toSchemaString(TypeValIn(inMapTypeS, env))
	outputSchema := toSchemaString(outType)

	// JSON stringifier (with spaces after ":" and ",").
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

	// Header & guidance.
	bld.WriteString("PROMPT:\n")
	bld.WriteString("Please follow the instruction to the best of your ability:\n")
	bld.WriteString("for every input, provide an output that solves the task and\n")
	bld.WriteString("respects the format of the OUTPUT JSON SCHEMA. Never put code\n")
	bld.WriteString("fences around the output (like ```json); only generate valid JSON.\n\n")

	// Schemas.
	bld.WriteString("INPUT JSON SCHEMA:\n\n")
	bld.WriteString(indentBlock(inputSchema, 2))
	bld.WriteString("\n\n")

	bld.WriteString("OUTPUT JSON SCHEMA:\n\n")
	bld.WriteString(indentBlock(outputSchema, 2))
	bld.WriteString("\n\n")

	// Instruction / task line.
	taskLine := "Given the input, determine the output."
	if instruction.Tag == VTStr && strings.TrimSpace(instruction.Data.(string)) != "" {
		taskLine = strings.TrimSpace(instruction.Data.(string))
	}

	bld.WriteString("TASK:\n\n")
	bld.WriteString(taskLine + "\n\n")

	// Examples.
	if examples.Tag == VTArray {
		for _, ex := range examples.Data.(*ArrayObject).Elems {
			if ex.Tag != VTArray {
				continue
			}
			pair := ex.Data.(*ArrayObject).Elems
			if len(pair) != 2 {
				continue
			}
			inVal, outVal := pair[0], pair[1]

			bld.WriteString("INPUT:\n\n")
			bld.WriteString(toJSON(inVal) + "\n\n")
			bld.WriteString("OUTPUT:\n\n")
			bld.WriteString(toJSON(outVal) + "\n\n")
		}
	}

	// Final TASK with current call's input.
	bld.WriteString("INPUT:\n\n")
	bld.WriteString(toJSON(input) + "\n\n")
	bld.WriteString("OUTPUT:\n")

	return bld.String()
}

// ------------- Helpers -----------------------------------

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
		v := ctx.Arg(name) // if missing, omit/zero-value
		entries[name] = v
		keys = append(keys, name)
	}
	return Value{Tag: VTMap, Data: &MapObject{Entries: entries, Keys: keys}}
}

// Box examples: each example is [inVal, outVal]; normalize to maps for prompt rendering:
// input → map[name]=..., output → {"output": outVal}.
// Accepts a VTArray of pairs (or Null → returns empty).
func boxExamplesAsMaps(exs Value, names []string) []Value {
	out := []Value{}
	if exs.Tag != VTArray {
		return out
	}
	for _, ex := range exs.Data.(*ArrayObject).Elems {
		if ex.Tag != VTArray {
			continue
		}
		pair := ex.Data.(*ArrayObject).Elems
		if len(pair) != 2 {
			continue
		}
		inV, outV := pair[0], pair[1]

		// Normalize input to a map using param order.
		inMap := map[string]Value{}
		keys := []string{}
		if inV.Tag == VTArray {
			xs := inV.Data.(*ArrayObject).Elems
			for i := 0; i < len(xs) && i < len(names); i++ {
				inMap[names[i]] = xs[i]
				keys = append(keys, names[i])
			}
		} else {
			// Single-arg case: use first name if available.
			if len(names) > 0 {
				inMap[names[0]] = inV
				keys = append(keys, names[0])
			}
		}
		inVal := Value{Tag: VTMap, Data: &MapObject{Entries: inMap, Keys: keys}}

		outVal := Value{Tag: VTMap, Data: &MapObject{
			Entries: map[string]Value{"output": outV},
			Keys:    []string{"output"},
		}}

		out = append(out, Arr([]Value{inVal, outVal}))
	}
	return out
}

// indentBlock prefixes each line with n spaces (prompt formatting).
func indentBlock(s string, n int) string {
	pad := strings.Repeat(" ", n)
	lines := strings.Split(s, "\n")
	for i := range lines {
		lines[i] = pad + lines[i]
	}
	return strings.Join(lines, "\n")
}

// Nullable helpers and small utilities.

func ensureNullableUnlessAny(t S) S {
	if isAnyType(t) || isNullable(t) {
		return t
	}
	return S{"unop", "?", t}
}
func isAnyType(t S) bool  { return len(t) == 2 && t[0] == "id" && t[1] == "Any" }
func isNullable(t S) bool { return len(t) >= 3 && t[0] == "unop" && t[1] == "?" }
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
