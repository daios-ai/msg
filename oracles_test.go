package mindscript

import (
	"encoding/json"
	"strings"
	"testing"
	"unicode"
)

// --- helpers specific to these oracle tests ----------------------------------

// Installs a fake __oracle_execute that returns the provided raw string
// (or an annotated null when raw == "").
// Registered in Core and hoisted to Global (execOracle looks in Global).
func registerFakeOracle(ip *Interpreter, raw string) {
	ip.RegisterNative(
		"__oracle_execute",
		[]ParamSpec{
			{Name: "prompt", Type: S{"id", "Str"}},
			{Name: "inType", Type: S{"id", "Type"}},
			{Name: "outType", Type: S{"id", "Type"}}, // engine passes T? here
			{Name: "examples", Type: S{"array", S{"id", "Any"}}},
		},
		S{"unop", "?", S{"id", "Str"}}, // Str? (executor may return null on transport failure)
		func(_ *Interpreter, ctx CallCtx) Value {
			if raw == "" {
				return annotNull("fake backend: empty")
			}
			return Str(raw)
		},
	)
	if v, err := ip.Core.Get("__oracle_execute"); err == nil {
		ip.Global.Define("__oracle_execute", v)
	}
}

// Like registerFakeOracle, but the fake inspects outType to ensure the engine
// passes a *nullable* success type for non-Any, and leaves Any unwrapped.
// Returns BOXED JSON `{"output":{"ok":true}}` on success, otherwise annotated null with reason.
func registerAssertingFakeOracle(ip *Interpreter) {
	ip.RegisterNative(
		"__oracle_execute",
		[]ParamSpec{
			{Name: "prompt", Type: S{"id", "Str"}},
			{Name: "inType", Type: S{"id", "Type"}},
			{Name: "outType", Type: S{"id", "Type"}}, // engine passes T? for non-Any
			{Name: "examples", Type: S{"array", S{"id", "Any"}}},
		},
		S{"unop", "?", S{"id", "Str"}},
		func(_ *Interpreter, ctx CallCtx) Value {
			otV := ctx.MustArg("outType")
			if otV.Tag != VTType {
				return annotNull("outType is not a Type")
			}
			ot := strings.TrimSpace(FormatValue(otV)) // e.g. "Str?" or "{ok!: Bool}?" or "Any"

			// Case: Any must stay unwrapped by the engine; we still return boxed JSON.
			if ot == "Any" {
				return Str(`{"output":{"ok":true}}`)
			}

			// Non-Any must be nullable (end with '?')
			if !strings.HasSuffix(ot, "?") {
				return annotNull("outType was not nullable")
			}
			base := strings.TrimSpace(strings.TrimSuffix(ot, "?"))

			switch base {
			case "Str":
				return Str(`{"output":"ok"}`)
			default:
				// If the declared success type is the object {ok!: Bool}, return that shape.
				if strings.Contains(base, "ok!: Bool") {
					return Str(`{"output":{"ok":true}}`)
				}
				// Fallback for other types: still boxed & valid under T?
				return Str(`{"output":null}`)
			}
		},
	)
	if v, err := ip.Core.Get("__oracle_execute"); err == nil {
		ip.Global.Define("__oracle_execute", v)
	}
}

// Minimal jsonParse native (Str -> Any | annotated Null).
// Uses Go's encoding/json and converts into MindScript Values.
func registerJSONParse(ip *Interpreter) {
	ip.RegisterNative(
		"jsonParse",
		[]ParamSpec{{Name: "text", Type: S{"id", "Str"}}},
		S{"id", "Any"},
		func(_ *Interpreter, ctx CallCtx) Value {
			raw := ctx.MustArg("text").Data.(string)
			var x any
			if err := json.Unmarshal([]byte(raw), &x); err != nil {
				return annotNull("json parse error")
			}
			return goJSONToValue(x)
		},
	)
	if v, err := ip.Core.Get("jsonParse"); err == nil {
		ip.Global.Define("jsonParse", v)
	}
}

// Simple prompt hook that echoes the outType textual form into the prompt.
func registerBuildPromptEchoOutType(ip *Interpreter) {
	ip.RegisterNative(
		"__oracle_build_prompt",
		[]ParamSpec{
			{Name: "instruction", Type: S{"id", "Str"}},
			{Name: "inType", Type: S{"id", "Type"}},
			{Name: "outType", Type: S{"id", "Type"}},
			{Name: "examples", Type: S{"array", S{"id", "Any"}}},
		},
		S{"id", "Str"},
		func(_ *Interpreter, ctx CallCtx) Value {
			ot := ctx.MustArg("outType")
			return Str(strings.TrimSpace(FormatValue(ot)))
		},
	)
	if v, err := ip.Core.Get("__oracle_build_prompt"); err == nil {
		ip.Global.Define("__oracle_build_prompt", v)
	}
}

// Fake executor that returns count of *validated* examples it received.
// It returns BOXED JSON `{"output":"ok"}` on success.
func registerCountingExecutor(ip *Interpreter) {
	ip.RegisterNative(
		"__oracle_execute",
		[]ParamSpec{
			{Name: "prompt", Type: S{"id", "Str"}},
			{Name: "inType", Type: S{"id", "Type"}},
			{Name: "outType", Type: S{"id", "Type"}},
			{Name: "examples", Type: S{"array", S{"id", "Any"}}},
		},
		S{"unop", "?", S{"id", "Str"}},
		func(_ *Interpreter, ctx CallCtx) Value {
			ex := ctx.MustArg("examples")
			if ex.Tag != VTArray {
				return annotNull("examples not array")
			}
			n := len(ex.Data.([]Value))
			_ = n // we don’t surface it, the tests only validate length via separate helper
			return Str(`{"output":"ok"}`)
		},
	)
	if v, err := ip.Core.Get("__oracle_execute"); err == nil {
		ip.Global.Define("__oracle_execute", v)
	}
}

// --- tiny assertions ---------------------------------------------------------

func wantNullAnnotContains(t *testing.T, v Value, substr string) {
	t.Helper()
	if v.Tag != VTNull {
		t.Fatalf("got %v, want annotated Null", v)
	}
	if !strings.Contains(v.Annot, substr) {
		t.Fatalf("annot %q does not contain %q", v.Annot, substr)
	}
}

// --- tests -------------------------------------------------------------------

func Test_Oracle_StrSuccess(t *testing.T) {
	ip := NewRuntime()
	registerJSONParse(ip)
	registerFakeOracle(ip, `{"output":"Ada Lovelace"}`)

	v, err := ip.EvalSource(`
		# Say a scientist's name as plain text.
		let scientist = oracle() -> Str
		scientist()
	`)
	if err != nil {
		t.Fatalf("EvalSource error: %v", err)
	}
	wantStr(t, v, "Ada Lovelace")
}

func Test_Oracle_JSONSuccess_Object(t *testing.T) {
	ip := NewRuntime()
	registerJSONParse(ip)
	registerFakeOracle(ip, `{"output":{"name":"Marie Curie"}}`)

	// Pull the field via MindScript to keep the test simple & stable.
	v, err := ip.EvalSource(`
		let scientist = oracle() -> {name!: Str}
		scientist().name
	`)
	if err != nil {
		t.Fatalf("EvalSource error: %v", err)
	}
	wantStr(t, v, "Marie Curie")
}

func Test_Oracle_JSONInvalid_YieldsError(t *testing.T) {
	ip := NewRuntime()
	registerJSONParse(ip)
	registerFakeOracle(ip, `not-json`)

	v, err := ip.EvalSource(`
		let f = oracle() -> {name!: Str}
		f()
	`)
	if err != nil {
		t.Fatalf("EvalSource error: %v", err)
	}
	wantNullAnnotContains(t, v, "not valid JSON")
}

func Test_Oracle_JSONWrongShape_YieldsError(t *testing.T) {
	ip := NewRuntime()
	registerJSONParse(ip)
	registerFakeOracle(ip, `{"ok":true}`)

	v, err := ip.EvalSource(`
		let f = oracle() -> {name!: Str}
		f()
	`)
	if err != nil {
		t.Fatalf("EvalSource error: %v", err)
	}
	wantNullAnnotContains(t, v, "did not match the declared return type")
}

func Test_Oracle_PromptUsesNonNullSuccessType(t *testing.T) {
	ip := NewRuntime()
	registerBuildPromptEchoOutType(ip)
	registerJSONParse(ip)
	registerFakeOracle(ip, `{"output":{"name":"X"}}`)

	// outType echoed into prompt should be NON-null success type, i.e., "{name!: Str}"
	_, err := ip.EvalSource(`
		let g = oracle() -> {name!: Str}
		g()
	`)
	if err != nil {
		t.Fatalf("EvalSource error: %v", err)
	}
	got := ip.LastOraclePrompt()
	compact := func(s string) string {
		var b strings.Builder
		for _, r := range s {
			if !unicode.IsSpace(r) {
				b.WriteRune(r)
			}
		}
		return b.String()
	}
	if compact(got) != "{name!:Str}" {
		t.Fatalf("prompt outType = %q, want %q", got, "{name!: Str}")
	}
}

func Test_Oracle_ExecutorReceivesNullableOutType(t *testing.T) {
	ip := NewRuntime()
	registerJSONParse(ip)
	registerAssertingFakeOracle(ip)

	// Case 1: Any stays unwrapped (executor accepts "Any").
	v1, err := ip.EvalSource(`
		let a = oracle() -> Any
		a()
	`)
	if err != nil {
		t.Fatalf("EvalSource error: %v", err)
	}
	if v1.Tag == VTNull && v1.Annot != "" {
		t.Fatalf("executor rejected outType for Any: %v", v1.Annot)
	}

	// Case 2: Str is wrapped to Str? (executor requires nullable; returns boxed JSON).
	v2, err := ip.EvalSource(`
		let b = oracle() -> Str
		b()
	`)
	if err != nil {
		t.Fatalf("EvalSource error: %v", err)
	}
	// Just ensure not Null.
	if v2.Tag == VTNull {
		t.Fatalf("executor rejected outType for Str?: %v", v2.Annot)
	}
}

func Test_Oracle_JSONFailure_TypeMismatch_AnnotatedNull(t *testing.T) {
	ip := NewRuntime()
	registerJSONParse(ip)
	registerFakeOracle(ip, `{"output":{"wrong":42}}`)

	v, err := ip.EvalSource(`
		let scientist = oracle() -> {name!: Str}
		scientist()
	`)
	if err != nil {
		t.Fatalf("EvalSource error: %v", err)
	}
	wantAnnotatedNullContains(t, v, "type") // centralized type mismatch → annotated null
}

func Test_Oracle_AnyPassThrough_NoNullableWidening(t *testing.T) {
	ip := NewRuntime()
	registerJSONParse(ip)
	registerFakeOracle(ip, `{"output":{"foo":123}}`)

	v, err := ip.EvalSource(`
		let anything = oracle() -> Any
		anything().foo
	`)
	if err != nil {
		t.Fatalf("EvalSource error: %v", err)
	}
	wantInt(t, v, 123) // Any accepts whatever JSON parses to
}

func Test_Oracle_FencedJSON_Unwrapped(t *testing.T) {
	ip := NewRuntime()
	registerJSONParse(ip)
	registerFakeOracle(ip, "```json\n{\"output\":{\"name\":\"Rosalind Franklin\"}}\n```")

	v, err := ip.EvalSource(`
		let scientist = oracle() -> {name!: Str}
		scientist().name
	`)
	if err != nil {
		t.Fatalf("EvalSource error: %v", err)
	}
	wantStr(t, v, "Rosalind Franklin")
}

func Test_Oracle_ExecutorTransportError_PropagatesAnnotatedNull(t *testing.T) {
	ip := NewRuntime()
	// Simulate transport failure: executor returns annotated null
	ip.RegisterNative(
		"__oracle_execute",
		[]ParamSpec{
			{Name: "prompt", Type: S{"id", "Str"}},
			{Name: "inType", Type: S{"id", "Type"}},
			{Name: "outType", Type: S{"id", "Type"}},
			{Name: "examples", Type: S{"array", S{"id", "Any"}}},
		},
		S{"unop", "?", S{"id", "Str"}},
		func(_ *Interpreter, ctx CallCtx) Value { return annotNull("network down") },
	)
	if v, err := ip.Core.Get("__oracle_execute"); err == nil {
		ip.Global.Define("__oracle_execute", v)
	}

	v, err := ip.EvalSource(`
		let scientist = oracle() -> {name!: Str}
		scientist()
	`)
	if err != nil {
		t.Fatalf("EvalSource error: %v", err)
	}
	wantAnnotatedNullContains(t, v, "network down")
}

func Test_Oracle_OutType_Passed_Is_Nullable_For_NonAny(t *testing.T) {
	ip := NewRuntime()
	registerJSONParse(ip)
	registerAssertingFakeOracle(ip) // asserts outType is nullable when not Any

	v, err := ip.EvalSource(`
		## Backend checks that outType is nullable (T?)
		## Return BOXED JSON {"output":{"ok": true}} so we can validate value too.
		let s = oracle() -> {ok!: Bool}
		s().ok
	`)
	if err != nil {
		t.Fatalf("EvalSource error: %v", err)
	}
	wantBool(t, v, true)
}

func Test_Oracle_OutType_Any_Not_Wrapped(t *testing.T) {
	ip := NewRuntime()
	registerJSONParse(ip)
	// Fake checks Any is not wrapped; if fine, returns BOXED {"output":{"ok":true}}
	registerAssertingFakeOracle(ip)

	v, err := ip.EvalSource(`
		let a = oracle() -> Any
		## Backend returns {"output":{"ok": true}}; engine should parse because return type isn't Str.
		a().ok
	`)
	if err != nil {
		t.Fatalf("EvalSource error: %v", err)
	}
	wantBool(t, v, true)
}

// Fake that inspects the "examples" argument and returns "ok" if it looks right.
func registerExamplesAssertingOracle(ip *Interpreter, wantLen int) {
	ip.RegisterNative(
		"__oracle_execute",
		[]ParamSpec{
			{Name: "prompt", Type: S{"id", "Str"}},
			{Name: "inType", Type: S{"id", "Type"}},
			{Name: "outType", Type: S{"id", "Type"}},
			{Name: "examples", Type: S{"array", S{"id", "Any"}}},
		},
		S{"unop", "?", S{"id", "Str"}},
		func(_ *Interpreter, ctx CallCtx) Value {
			ex := ctx.MustArg("examples")
			if ex.Tag != VTArray {
				return annotNull("examples not array")
			}
			arr := ex.Data.([]Value)
			if len(arr) != wantLen {
				return annotNull("wrong examples length")
			}
			// Optional: verify each item is a 2-tuple [input, output]
			for _, it := range arr {
				if it.Tag != VTArray || len(it.Data.([]Value)) != 2 {
					return annotNull("example is not [input, output]")
				}
			}
			return Str(`{"output":"ok"}`)
		},
	)
	if v, err := ip.Core.Get("__oracle_execute"); err == nil {
		ip.Global.Define("__oracle_execute", v)
	}
}

// -----------------------------------------------------------------------------
// New tests
// -----------------------------------------------------------------------------

func Test_Oracle_MultiParam_Arity_And_TypeCheck(t *testing.T) {
	ip := NewRuntime()
	// Backend won't be reached if params fail type-checking (engine enforces)
	registerFakeOracle(ip, `{"output":{"ignored":true}}`)

	// Wrong type for first parameter (expects Int)
	v, err := ip.EvalSource(`
		let f = oracle(a: Int, b: Str) -> Str
		f("not-int", "ok")
	`)
	if err != nil {
		t.Fatalf("EvalSource error: %v", err)
	}
	wantAnnotatedNullContains(t, v, "type mismatch") // enforced before backend
}

func Test_Oracle_Fenced_NoLabel_Unwrapped_JSON(t *testing.T) {
	ip := NewRuntime()
	registerJSONParse(ip)
	registerFakeOracle(ip, "```\n{\"output\":{\"name\":\"Katherine Johnson\"}}\n```")

	v, err := ip.EvalSource(`
		let scientist = oracle() -> {name!: Str}
		scientist().name
	`)
	if err != nil {
		t.Fatalf("EvalSource error: %v", err)
	}
	wantStr(t, v, "Katherine Johnson")
}

func Test_Oracle_Str_Fenced_NoLabel_Unwrapped_Text(t *testing.T) {
	ip := NewRuntime()
	registerJSONParse(ip)
	registerFakeOracle(ip, "```\n{\"output\":\"Hello, world!\"}\n```")

	v, err := ip.EvalSource(`
		let greet = oracle() -> Str
		greet()
	`)
	if err != nil {
		t.Fatalf("EvalSource error: %v", err)
	}
	wantStr(t, v, "Hello, world!")
}

func Test_Oracle_StrNullLiteral_YieldsAnnotatedNull(t *testing.T) {
	ip := NewRuntime()
	registerFakeOracle(ip, "null") // executor returns the literal string "null"

	v, err := ip.EvalSource(`
		let scientist = oracle() -> Str
		scientist()
	`)
	if err != nil {
		t.Fatalf("EvalSource error: %v", err)
	}
	wantAnnotatedNullContains(t, v, "returned null")
}

func Test_Oracle_NonStr_NonJSON_Yields_AnnotatedNull(t *testing.T) {
	ip := NewRuntime()
	registerJSONParse(ip)
	registerFakeOracle(ip, "this is not json at all")

	v, err := ip.EvalSource(`
		let scientist = oracle() -> {name!: Str}
		scientist()
	`)
	if err != nil {
		t.Fatalf("EvalSource error: %v", err)
	}
	wantAnnotatedNullContains(t, v, "valid JSON")
}

func Test_Oracle_Object_LiteralNull_Yields_AnnotatedNull(t *testing.T) {
	ip := NewRuntime()
	registerFakeOracle(ip, "null")

	v, err := ip.EvalSource(`
		let scientist = oracle() -> {name!: Str}
		scientist()
	`)
	if err != nil {
		t.Fatalf("EvalSource error: %v", err)
	}
	wantAnnotatedNullContains(t, v, "returned null")
}

func Test_Oracle_Examples_Passed_To_Backend(t *testing.T) {
	ip := NewRuntime()
	registerJSONParse(ip)
	registerExamplesAssertingOracle(ip, 3)

	v, err := ip.EvalSource(`
		let ex = [
			[0, "zero"],
			[1, "one"],
			[2, "two"]
		]
		let number2word = oracle(n: Int) -> Str from ex
		number2word(5)   ## backend returns "ok" if examples arrived as [ [in,out], ... ]
	`)
	if err != nil {
		t.Fatalf("EvalSource error: %v", err)
	}
	wantStr(t, v, "ok")
}

func Test_Oracle_Examples_From_Variable_Expr(t *testing.T) {
	ip := NewRuntime()
	registerJSONParse(ip)
	registerExamplesAssertingOracle(ip, 3)

	v, err := ip.EvalSource(`
        let ex = [
            [0, "zero"],
            [1, "one"],
            [2, "two"]
        ]
        let number2word = oracle(n: Int) -> Str from ex
        number2word(5)
    `)
	if err != nil {
		t.Fatalf("EvalSource error: %v", err)
	}
	wantStr(t, v, "ok")
}

func Test_Oracle_Examples_From_Expression(t *testing.T) {
	ip := NewRuntime()
	registerJSONParse(ip)
	registerExamplesAssertingOracle(ip, 2)

	v, err := ip.EvalSource(`
        let a = [[0,"zero"]]
        let b = [[1,"one"]]
        let number2word = oracle(n: Int) -> Str from (a + b)
        number2word(7)
    `)
	if err != nil {
		t.Fatalf("EvalSource error: %v", err)
	}
	wantStr(t, v, "ok")
}

// Already had a transport error test; add one more variant: backend returns empty -> annotated null
func Test_Oracle_Executor_Returns_Empty_AnnotatedNull(t *testing.T) {
	ip := NewRuntime()
	registerFakeOracle(ip, "") // will return annotNull("fake backend: empty")

	v, err := ip.EvalSource(`
		let s = oracle() -> Str
		s()
	`)
	if err != nil {
		t.Fatalf("EvalSource error: %v", err)
	}
	wantAnnotatedNullContains(t, v, "fake backend")
}

func Test_Oracle_Prompt_BoxedSchemas_And_Examples(t *testing.T) {
	ip := NewRuntime()
	// We don't care about the result here; we only want the prompt.
	// Returning "null" avoids needing jsonParse in this test.
	registerFakeOracle(ip, "null")

	// Program modeled on the manual’s example.
	src := `
		# Example input output pairs.
		let examples = [
		  [{in: 9}, {out: 16}],
		  [{in: 25}, {out: 36}],
		  [{in: 1}, {out: 4}]
		]

		# The input number.
		let Input = type {in!: Int}

		# The output number.
		let Output = type {out!: Int}

		# Given a square number, provide the next square.
		let square = oracle(args: Input) -> Output from examples

		square({in: 144})
	`
	if _, err := ip.EvalSource(src); err != nil {
		t.Fatalf("EvalSource error: %v", err)
	}

	got := ip.LastOraclePrompt()
	if strings.TrimSpace(got) == "" {
		t.Fatalf("engine did not capture last oracle prompt")
	}

	// ---- Build the expected prompt exactly like the engine does ----

	// Helpers (mirror oracles.go)
	indentBlock := func(s string, n int) string {
		pad := strings.Repeat(" ", n)
		lines := strings.Split(s, "\n")
		for i := range lines {
			lines[i] = pad + lines[i]
		}
		return strings.Join(lines, "\n")
	}
	pretty := func(m map[string]any) string {
		bs, err := json.MarshalIndent(m, "", "  ")
		if err != nil {
			return "{}"
		}
		return string(bs)
	}

	// Recreate the schemas the prompt should contain.
	// Input inner schema: the alias itself.
	inSchemaInner := map[string]any{
		"type":        "object",
		"description": "The input number.",
		"properties": map[string]any{
			"in": map[string]any{
				"type": "integer",
			},
		},
		"required": []any{"in"},
	}

	// Boxed output schema: {output: Output}
	boxedOutSchema := map[string]any{
		"type": "object",
		"properties": map[string]any{
			"output": map[string]any{
				"$ref": "#/$defs/Out",
			},
		},
		"required": []any{"output"},
		"$defs": map[string]any{
			"Out": map[string]any{
				"type":        "object",
				"description": "The output number.",
				"properties": map[string]any{
					"out": map[string]any{
						"type": "integer",
					},
				},
				"required": []any{"out"},
			},
		},
	}

	// Examples and final call (render like the manual)
	exIn := []string{
		`{"in": 9}`,
		`{"in": 25}`,
		`{"in": 1}`,
	}
	exOut := []string{
		`{"output": {"out": 16}}`,
		`{"output": {"out": 36}}`,
		`{"output": {"out": 4}}`,
	}
	finalInput := `{"in": 144}`

	var b strings.Builder
	b.WriteString("PROMPT:\n")
	b.WriteString("Please follow the instruction to the best of your ability:\n")
	b.WriteString("for every input, provide an output that solves the task and\n")
	b.WriteString("respects the format of the OUTPUT JSON SCHEMA. Never put code\n")
	b.WriteString("fences around the output (like ```json); only generate valid JSON.\n\n")

	b.WriteString("INPUT JSON SCHEMA:\n\n")
	b.WriteString(indentBlock(pretty(inSchemaInner), 2))
	b.WriteString("\n\n")

	b.WriteString("OUTPUT JSON SCHEMA:\n\n")
	// For output, we show the *boxed* schema directly.
	b.WriteString(indentBlock(pretty(boxedOutSchema), 2))
	b.WriteString("\n\n")

	// Example blocks
	for i := range exIn {
		b.WriteString("TASK:\n\n")
		b.WriteString("Given a square number, provide the next square.\n\n")
		b.WriteString("INPUT:\n\n")
		b.WriteString(exIn[i])
		b.WriteString("\n\n")
		b.WriteString("OUTPUT:\n\n")
		b.WriteString(exOut[i])
		b.WriteString("\n\n")
	}

	// Final task with the oracle’s instruction (from the annotation)
	b.WriteString("TASK:\n\n")
	b.WriteString("Given a square number, provide the next square.\n\n")
	b.WriteString("INPUT:\n\n")
	b.WriteString(finalInput)
	b.WriteString("\n\n")
	b.WriteString("OUTPUT:\n")

	want := b.String()

	if got != want {
		// Make mismatches easy to inspect in CI logs.
		t.Fatalf("prompt mismatch:\n--- GOT ---\n%s\n--- WANT ---\n%s", got, want)
	}
}
