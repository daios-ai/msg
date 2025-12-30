// oracles.go — Oracle execution & prompt building (PUBLIC API + PRIVATE impl)
//
// WHAT THIS FILE DOES
// ===================
// MindScript “oracles” are functions whose execution is delegated to an external
// backend (e.g., an LLM). When an oracle is called at runtime, the interpreter:
//
//  1. Builds a **prompt** that captures:
//     • the oracle’s instruction (taken from the function Value’s .Annot),
//     • the declared input type (parameter map built from the declaration),
//     • the declared *success* return type boxed as {"output": T},
//     • optional examples for few-shot guidance (see "EXAMPLES" below),
//     • the current call’s concrete input,
//     and records that prompt internally (for debugging and testing).
//
//     The prompt is currently constructed by this file’s internal builder
//     (see buildOraclePrompt). There is no user-space prompt hook.
//
//  2. Calls a pluggable backend hook resolved from the oracle’s lexical scope:
//     __oracle_execute(req: {}) -> Str | Null
//     which must return either:
//     • Str  — raw JSON (no code fences) representing either:
//     - {"output": <value>}  (preferred), or
//     - a bare JSON value     (will be boxed to {"output": <value>})
//     • Null — to signal failure.
//
//     The request map is intentionally minimal and contains ONLY:
//     - prompt (Str)                     pre-rendered standard prompt
//     - annotation (Str)                 oracle function annotation (raw, untrimmed)
//     - examples ([Any])                 normalized pairs: [inputMap, outputMap]
//     - input (Any)                      current call input map
//     - inTypes ([Type])                 declared param types in order
//     - outType (Type)                   boxed {"output": T} success type
//     - inputSchema/outputSchema (Any)   JSON Schema objects (as maps)
//     - inputSchemaString/outputSchemaString (Str)  pretty-printed schema JSON
//
//  3. Parses returned JSON using the host-side permissive repair/parser
//     (jsonRepair), extracts/boxes the {"output": ...} field, and validates the
//     extracted value against the oracle’s **operational** return type.
//
//     Oracles are **operationally nullable**: at runtime, outputs are validated
//     against T? (declared success type widened to nullable unless Any).
//     On parse failure or type mismatch, an *annotated null* Value is returned.
//
// EXAMPLES
// --------
// Oracles store examples in a canonical, signature-shaped form:
//
//	For an oracle with N parameters, each example is:
//	  [arg1, arg2, ..., argN, returnValue]
//
//	where each arg_i conforms to the declared i-th parameter type and
//	returnValue conforms to the oracle’s declared *success* return type
//	(non-nullable). At runtime, examples are normalized into prompt-friendly
//	pairs: [inputMap, {"output": returnValue}].
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
// • json.go
//   - jsonRepair(...) and Go↔︎Value JSON conversion helpers used for parsing and
//     boxing oracle results.
//
// • Runtime hooks (user space, optional/required):
//   - REQUIRED:  __oracle_execute(req: {}) -> Str | Null
//
// NOTE
// ----
// Oracles are **operationally nullable**: a declared return type T is validated
// as T? at runtime. The backend should produce boxed JSON {"output": <value>},
// but a bare JSON value is accepted and will be boxed automatically.
//
// ──────────────────────────────────────────────────────────────────────────────
// PUBLIC API
// ──────────────────────────────────────────────────────────────────────────────
package mindscript

import (
	"encoding/json"
	"fmt"
	"strings"
)

/* ===========================
   PUBLIC
   =========================== */

//// END_OF_PUBLIC

/* ===========================
   PRIVATE IMPLEMENTATION
   =========================== */

// oracleSetExamples validates and sets examples on an oracle function.
//
// Canonical example format (arity N):
//
//	[arg1, arg2, ..., argN, returnValue]
//
// where each arg_i conforms to the declared i-th param type, and returnValue
// conforms to the oracle's declared *success* return type (non-nullable).
//
// Passing Null clears examples.
func (ip *Interpreter) oracleSetExamples(funVal Value, examples Value) error {
	if funVal.Tag != VTFun {
		return fmt.Errorf("not a function")
	}
	f := funVal.Data.(*Fun)
	if f == nil || !f.IsOracle {
		return fmt.Errorf("not an oracle")
	}

	// Recover original declaration signature (stable across currying).
	paramNames := f.Params
	declTypes := f.ParamTypes
	if f.Sig != nil {
		paramNames = f.Sig.Names
		declTypes = f.Sig.Types
	}

	// Clear.
	if examples.Tag == VTNull {
		f.Examples = Null
		return nil
	}
	if examples.Tag != VTArray {
		return fmt.Errorf("examples must be an array or null")
	}

	n := len(declTypes)
	if len(paramNames) != n {
		return fmt.Errorf("oracle signature mismatch (names=%d types=%d)", len(paramNames), n)
	}

	// Success return type (examples represent successful outputs).
	successRet := stripNullable(f.ReturnType)

	exs := examples.Data.(*ArrayObject).Elems
	for ei, ex := range exs {
		if ex.Tag != VTArray {
			return fmt.Errorf("example %d: must be an array", ei)
		}
		xs := ex.Data.(*ArrayObject).Elems
		if len(xs) != n+1 {
			return fmt.Errorf("example %d: expected %d items ([%d args..., return])", ei, n+1, n)
		}

		// Args.
		for i := 0; i < n; i++ {
			if !ip.isType(xs[i], declTypes[i], f.Env) {
				return fmt.Errorf("example %d: arg %d does not match declared type", ei, i+1)
			}
		}

		// Return value (success type).
		if !ip.isType(xs[n], successRet, f.Env) {
			return fmt.Errorf("example %d: return value does not match declared return type", ei)
		}
	}

	f.Examples = examples
	return nil
}

// oracleSchemas renders JSON Schema for the given input/output Type values.
// Returns both the schema as a MindScript Value (maps/arrays) and the pretty JSON string.
func (ip *Interpreter) oracleSchemas(inType Value, outType Value, env *Env) (inSchema Value, outSchema Value, inStr string, outStr string) {
	toSchema := func(tv Value) any {
		return ip.TypeValueToJSONSchema(tv, env)
	}
	toSchemaString := func(schema any) string {
		b, err := json.MarshalIndent(schema, "", "  ")
		if err != nil {
			return "{}"
		}
		return string(b)
	}

	inAny := toSchema(inType)
	outAny := toSchema(outType)

	// Convert schema objects into MindScript values for the hook request.
	inSchema = goJSONToValue(inAny)
	outSchema = goJSONToValue(outAny)

	inStr = toSchemaString(inAny)
	outStr = toSchemaString(outAny)
	return
}

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

	// NOTE: do not trim; annotation is carried verbatim.
	instructionVal := Str(funVal.Annot)

	// Build prompt text (prefers user hook when available) and record it.
	outTypeVal := TypeValIn(outBoxTypeS, f.Env)
	prompt := ip.buildOraclePrompt(
		instructionVal,
		inArgsVal,
		inTypesVal,
		outTypeVal,
		examplesVal,
		f.Env,
	)

	// Locate executor hook strictly via the oracle's lexical environment.
	hook, err := f.Env.Get("__oracle_execute")
	if err != nil || hook.Tag != VTFun {
		return annotNull("oracle backend not configured (define __oracle_execute(req))")
	}

	// Precompute schemas and pretty strings for the request bundle.
	// Input schema is derived from the required-params input map type.
	inMapTypeS := mapTypeFromParams(paramNames, declTypes)
	inTypeVal := TypeValIn(inMapTypeS, f.Env)
	inSchemaVal, outSchemaVal, inSchemaStr, outSchemaStr := ip.oracleSchemas(inTypeVal, outTypeVal, f.Env)

	// New hook request bundle (and nothing else).
	req := Value{Tag: VTMap, Data: &MapObject{
		Entries: map[string]Value{
			"prompt":             Str(prompt),
			"annotation":         instructionVal,
			"examples":           examplesVal,
			"input":              inArgsVal,
			"inTypes":            inTypesVal,
			"outType":            outTypeVal,
			"inputSchema":        inSchemaVal,
			"outputSchema":       outSchemaVal,
			"inputSchemaString":  Str(inSchemaStr),
			"outputSchemaString": Str(outSchemaStr),
		},
		Keys: []string{
			"prompt",
			"annotation",
			"examples",
			"input",
			"inTypes",
			"outType",
			"inputSchema",
			"outputSchema",
			"inputSchemaString",
			"outputSchemaString",
		},
	}}

	res := ip.Apply(hook, []Value{req})

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

	// JSON stringifier.
	toJSON := func(v Value) string {
		j, err := valueToGoJSON(v)
		if err != nil {
			return "null"
		}
		b, err := json.MarshalIndent(j, "", "  ")
		if err != nil {
			return "null"
		}
		return string(b)
	}

	var bld strings.Builder

	// System prompt.
	bld.WriteString("PROMPT:\n")
	bld.WriteString("Please follow the instruction to the best of your ability:\n")
	bld.WriteString("for every input, provide an output that solves the task and\n")
	bld.WriteString("respects the OUTPUT JSON SCHEMA. Never put code fences around\n")
	bld.WriteString("the output. Only generate valid JSON.\n\n")

	// Schemas.
	bld.WriteString("INPUT JSON SCHEMA:\n\n")
	bld.WriteString(indentBlock(inputSchema, 2))
	bld.WriteString("\n\n")

	bld.WriteString("OUTPUT JSON SCHEMA:\n\n")
	bld.WriteString(indentBlock(outputSchema, 2))
	bld.WriteString("\n\n")

	// Instruction / task line.
	taskLine := "Given the input, determine the output."
	if instruction.Tag == VTStr && instruction.Data.(string) != "" {
		// NOTE: do not trim; keep annotation verbatim.
		taskLine = instruction.Data.(string)
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

// Box examples (canonical):
//
//	each example is [arg1, ..., argN, returnValue]
//
// Normalize to prompt pairs:
//
//	input  → {"x": arg1, "y": arg2, ...}
//	output → {"output": returnValue}
//
// Accepts a VTArray (or Null → returns empty).
func boxExamplesAsMaps(exs Value, names []string) []Value {
	out := []Value{}
	if exs.Tag != VTArray {
		return out
	}
	n := len(names)
	for _, ex := range exs.Data.(*ArrayObject).Elems {
		if ex.Tag != VTArray {
			continue
		}
		xs := ex.Data.(*ArrayObject).Elems
		if len(xs) != n+1 {
			continue
		}

		// Input map from args.
		inMap := make(map[string]Value, n)
		keys := make([]string, 0, n)
		for i := 0; i < n; i++ {
			inMap[names[i]] = xs[i]
			keys = append(keys, names[i])
		}
		inVal := Value{Tag: VTMap, Data: &MapObject{Entries: inMap, Keys: keys}}

		outVal := Value{Tag: VTMap, Data: &MapObject{
			Entries: map[string]Value{"output": xs[n]},
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
