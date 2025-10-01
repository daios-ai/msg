package mindscript

import (
	"encoding/json"
	"strings"
	"testing"
)

// --- helpers specific to these oracle tests ----------------------------------

// Installs a fake __oracle_execute that returns the provided raw string
// (or an annotated null when raw == "").
// Registered in Core and hoisted to Global (execOracle looks in Global).
// NOTE: In the current engine, __oracle_execute takes only (prompt: Str) -> Str | Null.
func registerFakeOracle(ip *Interpreter, raw string) {
	ip.RegisterNative(
		"__oracle_execute",
		[]ParamSpec{
			{Name: "prompt", Type: S{"id", "Str"}},
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

// Variant that also captures the prompt string into *outPrompt.
func registerFakeOracleWithCapture(ip *Interpreter, raw string, outPrompt *string) {
	ip.RegisterNative(
		"__oracle_execute",
		[]ParamSpec{
			{Name: "prompt", Type: S{"id", "Str"}},
		},
		S{"unop", "?", S{"id", "Str"}},
		func(_ *Interpreter, ctx CallCtx) Value {
			// capture the prompt for inspection in tests
			if outPrompt != nil {
				if p := ctx.Arg("prompt"); p.Tag == VTStr {
					*outPrompt = p.Data.(string)
				}
			}
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

// Minimal jsonParse native (Str -> Any | annotated Null).
// Uses Go's encoding/json and converts into MindScript Values.
func registerJSONParse(ip *Interpreter) {
	ip.RegisterNative(
		"jsonParse",
		[]ParamSpec{{Name: "text", Type: S{"id", "Str"}}},
		S{"id", "Any"},
		func(_ *Interpreter, ctx CallCtx) Value {
			raw := ctx.Arg("text").Data.(string)
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

// --- tiny assertions ---------------------------------------------------------

// Count example INPUT blocks in a prompt (excludes the final call's INPUT).
// Heuristic: the prompt renders one "INPUT:" per example plus one final "INPUT:"
// for the current call. So (#INPUT occurrences - 1) == #examples.
func countExamplesInPrompt(prompt string) int {
	if prompt == "" {
		return 0
	}
	n := strings.Count(prompt, "INPUT:\n\n")
	if n > 0 {
		return n - 1
	}
	return 0
}

// --- tests -------------------------------------------------------------------

func Test_Oracle_StrSuccess(t *testing.T) {
	ip := NewInterpreter()
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
	ip := NewInterpreter()
	registerJSONParse(ip)
	registerFakeOracle(ip, `{"output":{"name":"Marie Curie"}}`)

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
	ip := NewInterpreter()
	registerJSONParse(ip)
	registerFakeOracle(ip, `not-json`)

	v, err := ip.EvalSource(`
		let f = oracle() -> {name!: Str}
		f()
	`)
	if err != nil {
		t.Fatalf("EvalSource error: %v", err)
	}
	wantAnnotatedNullContains(t, v, "not valid JSON")
}

func Test_Oracle_JSONWrongShape_YieldsError(t *testing.T) {
	ip := NewInterpreter()
	registerJSONParse(ip)
	registerFakeOracle(ip, `{"ok":true}`)

	v, err := ip.EvalSource(`
		let f = oracle() -> {name!: Str}
		f()
	`)
	if err != nil {
		t.Fatalf("EvalSource error: %v", err)
	}
	wantAnnotatedNullContains(t, v, "did not match the declared return type")
}

func Test_Oracle_NullableBehavior_For_NonAny(t *testing.T) {
	ip := NewInterpreter()
	registerJSONParse(ip)
	registerFakeOracle(ip, `{"output": null}`)

	v, err := ip.EvalSource(`
		let b = oracle() -> Str
		b()
	`)
	if err != nil {
		t.Fatalf("EvalSource error: %v", err)
	}
	if v.Tag != VTNull || v.Annot != "" {
		t.Fatalf("want plain null (accepted under Str?), got: %v (annot=%q)", v, v.Annot)
	}
}

func Test_Oracle_JSONFailure_TypeMismatch_AnnotatedNull(t *testing.T) {
	ip := NewInterpreter()
	registerJSONParse(ip)
	registerFakeOracle(ip, `{"output":{"wrong":42}}`)

	v, err := ip.EvalSource(`
		let scientist = oracle() -> {name!: Str}
		scientist()
	`)
	if err != nil {
		t.Fatalf("EvalSource error: %v", err)
	}
	wantAnnotatedNullContains(t, v, "declared return type")
}

func Test_Oracle_AnyPassThrough_NoNullableWidening(t *testing.T) {
	ip := NewInterpreter()
	registerJSONParse(ip)
	registerFakeOracle(ip, `{"output":{"foo":123}}`)

	v, err := ip.EvalSource(`
		let anything = oracle() -> Any
		anything().foo
	`)
	if err != nil {
		t.Fatalf("EvalSource error: %v", err)
	}
	wantInt(t, v, 123)
}

func Test_Oracle_FencedJSON_Unwrapped(t *testing.T) {
	ip := NewInterpreter()
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
	ip := NewInterpreter()
	ip.RegisterNative(
		"__oracle_execute",
		[]ParamSpec{
			{Name: "prompt", Type: S{"id", "Str"}},
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

func Test_Oracle_ObjectResult_Parsed_OK(t *testing.T) {
	ip := NewInterpreter()
	registerJSONParse(ip)
	registerFakeOracle(ip, `{"output":{"ok":true}}`)

	v, err := ip.EvalSource(`
		let s = oracle() -> {ok!: Bool}
		s().ok
	`)
	if err != nil {
		t.Fatalf("EvalSource error: %v", err)
	}
	wantBool(t, v, true)
}

func Test_Oracle_OutType_Any_Not_Wrapped(t *testing.T) {
	ip := NewInterpreter()
	registerJSONParse(ip)
	registerFakeOracle(ip, `{"output":{"ok":true}}`)

	v, err := ip.EvalSource(`
		let a = oracle() -> Any
		## Backend returns {"output":{"ok": true}}; Any accepts the parsed map.
		a().ok
	`)
	if err != nil {
		t.Fatalf("EvalSource error: %v", err)
	}
	wantBool(t, v, true)
}

// --- examples handling via captured prompt -----------------------------------

func Test_Oracle_Examples_Present_In_Prompt(t *testing.T) {
	ip := NewInterpreter()
	registerJSONParse(ip)
	var lastPrompt string
	registerFakeOracleWithCapture(ip, `{"output":"ok"}`, &lastPrompt)

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

	if got := countExamplesInPrompt(lastPrompt); got != 3 {
		t.Fatalf("want 3 examples in prompt, got %d\n\nPROMPT:\n%s", got, lastPrompt)
	}
}

func Test_Oracle_Examples_From_Variable_Expr_In_Prompt(t *testing.T) {
	ip := NewInterpreter()
	registerJSONParse(ip)
	var lastPrompt string
	registerFakeOracleWithCapture(ip, `{"output":"ok"}`, &lastPrompt)

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

	if got := countExamplesInPrompt(lastPrompt); got != 3 {
		t.Fatalf("want 3 examples in prompt, got %d\n\nPROMPT:\n%s", got, lastPrompt)
	}
}

func Test_Oracle_Examples_From_Expression_In_Prompt(t *testing.T) {
	ip := NewInterpreter()
	registerJSONParse(ip)
	var lastPrompt string
	registerFakeOracleWithCapture(ip, `{"output":"ok"}`, &lastPrompt)

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

	if got := countExamplesInPrompt(lastPrompt); got != 2 {
		t.Fatalf("want 2 examples in prompt, got %d\n\nPROMPT:\n%s", got, lastPrompt)
	}
}

// --- fenced / null literal edge cases ---------------------------------------

func Test_Oracle_Fenced_NoLabel_Unwrapped_JSON(t *testing.T) {
	ip := NewInterpreter()
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
	ip := NewInterpreter()
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
	ip := NewInterpreter()
	registerFakeOracle(ip, "null") // executor returns the literal string "null"

	v, err := ip.EvalSource(`
		let scientist = oracle() -> Str
		scientist()
	`)
	if err != nil {
		t.Fatalf("EvalSource error: %v", err)
	}
	wantAnnotatedNullContains(t, v, "not valid JSON")
}

func Test_Oracle_NonStr_NonJSON_Yields_AnnotatedNull(t *testing.T) {
	ip := NewInterpreter()
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
	ip := NewInterpreter()
	registerFakeOracle(ip, "null")

	v, err := ip.EvalSource(`
		let scientist = oracle() -> {name!: Str}
		scientist()
	`)
	if err != nil {
		t.Fatalf("EvalSource error: %v", err)
	}
	wantAnnotatedNullContains(t, v, "not valid JSON")
}

// Already had a transport error test; add one more variant: backend returns empty -> annotated null
func Test_Oracle_Executor_Returns_Empty_AnnotatedNull(t *testing.T) {
	ip := NewInterpreter()
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

// -----------------------------------------------------------------------------
// New tests focusing on arity/type errors at call sites (hard errors).
// -----------------------------------------------------------------------------

func Test_Oracle_MultiParam_Arity_And_TypeCheck(t *testing.T) {
	ip := NewInterpreter()
	// Backend won't be reached if params fail type-checking (engine enforces)
	registerFakeOracle(ip, `{"output":{"ignored":true}}`)

	// Wrong type for first parameter (expects Int) → CONTRACT VIOLATION ⇒ HARD ERROR
	_, err := ip.EvalSource(`
		let f = oracle(a: Int, b: Str) -> Str
		f("not-int", "ok")
	`)
	wantHardErrorContains(t, err, "type mismatch")
}

func wantHardErrorContains(t *testing.T, err error, substr string) {
	t.Helper()
	if err == nil {
		t.Fatalf("expected hard error containing %q, got nil error", substr)
	}
	msg := err.Error()
	if !strings.Contains(msg, substr) {
		t.Fatalf("hard error mismatch: want msg to contain %q, got: %s", substr, msg)
	}
	// Optional: assert caret header is present when runtimeErrorsAsGoError=true
	if !strings.Contains(msg, "RUNTIME ERROR") {
		t.Fatalf("expected caret-style RUNTIME ERROR, got: %s", msg)
	}
}

// --- multi-input + type aliases (with schema descriptions) -------------------

func Test_Oracle_MultiParam_With_Aliases_Success_And_Prompt(t *testing.T) {
	ip := NewInterpreter()
	registerJSONParse(ip)

	var lastPrompt string
	registerFakeOracleWithCapture(ip, `{"output":"ready"}`, &lastPrompt)

	v, err := ip.EvalSource(`
		# A user record.
		let User = type {name!: Str, age: Int}

		# Hobbies.
		let Hobbies = type [Str]

		# Status of execution.
		let Status = type Enum["ready", "running", "done"]

		let decide = oracle(u: User, h: Hobbies) -> Status
		decide({name: "Ada", age: 36}, ["math", "poetry"]) == "ready"
	`)
	if err != nil {
		t.Fatalf("EvalSource error: %v", err)
	}
	// Oracle returned "ready", which is valid Status → expression should be true.
	wantBool(t, v, true)

	// Prompt should include both inputs rendered and schema descriptions from annotations.
	if !strings.Contains(lastPrompt, `"name": "Ada"`) || !strings.Contains(lastPrompt, `"age": 36`) {
		t.Fatalf("prompt missing user input fields:\n%s", lastPrompt)
	}
	if !strings.Contains(lastPrompt, `"math"`) || !strings.Contains(lastPrompt, `"poetry"`) {
		t.Fatalf("prompt missing hobbies input:\n%s", lastPrompt)
	}
	// Descriptions must be present in the INPUT/OUTPUT JSON SCHEMA blocks.
	if !strings.Contains(lastPrompt, `"description": "A user record."`) {
		t.Fatalf("missing User description in schema:\n%s", lastPrompt)
	}
	if !strings.Contains(lastPrompt, `"description": "Hobbies."`) {
		t.Fatalf("missing Hobbies description in schema:\n%s", lastPrompt)
	}
	if !strings.Contains(lastPrompt, `"description": "Status of execution."`) {
		t.Fatalf("missing Status description in schema:\n%s", lastPrompt)
	}
}

func Test_Oracle_TypeAlias_In_ReturnType_NullableOperationally(t *testing.T) {
	ip := NewInterpreter()
	registerJSONParse(ip)

	var lastPrompt string
	// Backend returns null → allowed at runtime as T? (operationally nullable).
	registerFakeOracleWithCapture(ip, `{"output": null}`, &lastPrompt)

	v, err := ip.EvalSource(`
		# Status of execution.
		let Status = type Enum["ready", "running", "done"]

		let query = oracle() -> Status
		query()
	`)
	if err != nil {
		t.Fatalf("EvalSource error: %v", err)
	}
	// Should be plain (unannotated) null under Status?
	if v.Tag != VTNull || v.Annot != "" {
		t.Fatalf("want plain null (accepted under Status?), got %v (annot=%q)", v, v.Annot)
	}
	// Output schema should carry alias description.
	if !strings.Contains(lastPrompt, `"description": "Status of execution."`) {
		t.Fatalf("missing Status description in output schema:\n%s", lastPrompt)
	}
}

func Test_Oracle_Alias_ArrayParam_Success_And_Prompt(t *testing.T) {
	ip := NewInterpreter()
	registerJSONParse(ip)

	var lastPrompt string
	registerFakeOracleWithCapture(ip, `{"output":"ok"}`, &lastPrompt)

	v, err := ip.EvalSource(`
		# Hobbies.
		let Hobbies = type [Str]

		let summarize = oracle(h: Hobbies) -> Str
		summarize(["skiing", "reading"])
	`)
	if err != nil {
		t.Fatalf("EvalSource error: %v", err)
	}
	wantStr(t, v, "ok")

	// Prompt should show the array inputs and the alias description.
	if !strings.Contains(lastPrompt, `"skiing"`) || !strings.Contains(lastPrompt, `"reading"`) {
		t.Fatalf("prompt missing hobbies values:\n%s", lastPrompt)
	}
	if !strings.Contains(lastPrompt, `"description": "Hobbies."`) {
		t.Fatalf("missing Hobbies description in schema:\n%s", lastPrompt)
	}
}

func Test_Oracle_Alias_Param_WrongShape_HardError(t *testing.T) {
	ip := NewInterpreter()
	registerJSONParse(ip)
	registerFakeOracle(ip, `{"output":"ignored"}`) // should not be reached

	_, err := ip.EvalSource(`
		# A user record.
		let User = type {name!: Str, age: Int}

		let f = oracle(u: User) -> Str
		# Wrong shape: name is Int (should be Str).
		f({name: 123, age: 5})
	`)
	// Structural type check at call-site should fail.
	wantHardErrorContains(t, err, "type mismatch")
}

func Test_Oracles_TaskLine_UsesAnnotation_StraightCall(t *testing.T) {
	ip := NewInterpreter()
	registerJSONParse(ip)

	var gotPrompt string
	registerFakeOracleWithCapture(ip, `{"output": 6}`, &gotPrompt)

	v, err := ip.EvalSource(`
		# add two numbers
		let add = oracle(n: Int, m: Int) -> Int
		add(2, 4)
	`)
	if err != nil {
		t.Fatalf("EvalSource error: %v", err)
	}
	if v.Tag != VTInt || v.Data.(int64) != 6 {
		t.Fatalf("want Int(6), got: %v", v)
	}

	if gotPrompt == "" {
		t.Fatalf("did not capture prompt")
	}

	// TASK must reflect the annotation (not the fallback).
	if !strings.Contains(gotPrompt, "\nTASK:\n\nadd two numbers\n\n") {
		t.Fatalf("TASK line did not use annotation; prompt was:\n%s", gotPrompt)
	}
	if strings.Contains(gotPrompt, "Given the input, determine the output.") {
		t.Fatalf("fallback TASK line was used; prompt was:\n%s", gotPrompt)
	}

	// No examples provided → no example INPUT blocks.
	if n := countExamplesInPrompt(gotPrompt); n != 0 {
		t.Fatalf("expected 0 example INPUT blocks, got %d", n)
	}
}

func Test_Oracles_TaskLine_UsesAnnotation_CurriedCall(t *testing.T) {
	ip := NewInterpreter()
	registerJSONParse(ip)

	var gotPrompt string
	registerFakeOracleWithCapture(ip, `{"output": 6}`, &gotPrompt)

	v, err := ip.EvalSource(`
		# add two numbers
		let add = oracle(n: Int, m: Int) -> Int
		add(2)(4)
	`)
	if err != nil {
		t.Fatalf("EvalSource error: %v", err)
	}
	if v.Tag != VTInt || v.Data.(int64) != 6 {
		t.Fatalf("want Int(6), got: %v", v)
	}

	if gotPrompt == "" {
		t.Fatalf("did not capture prompt")
	}

	// Ensure annotation survives currying and appears in the TASK line.
	if !strings.Contains(gotPrompt, "\nTASK:\n\nadd two numbers\n\n") {
		t.Fatalf("TASK line did not use annotation after currying; prompt was:\n%s", gotPrompt)
	}
	if strings.Contains(gotPrompt, "Given the input, determine the output.") {
		t.Fatalf("fallback TASK line was used after currying; prompt was:\n%s", gotPrompt)
	}

	// No examples provided → no example INPUT blocks.
	if n := countExamplesInPrompt(gotPrompt); n != 0 {
		t.Fatalf("expected 0 example INPUT blocks, got %d", n)
	}
}

// --- extra tiny assertions for these tests ----------------------------------

func wantAnnotNull(t *testing.T, v Value, substr string) {
	t.Helper()
	if v.Tag != VTNull {
		t.Fatalf("want annotated null, got tag=%v val=%v", v.Tag, v)
	}
	if v.Annot == "" || !strings.Contains(v.Annot, substr) {
		t.Fatalf("want annotated null containing %q, got %q", substr, v.Annot)
	}
}

// --- lexical-resolution tests -----------------------------------------------

func Test_Oracle_Hook_StrToStr_Typecheck(t *testing.T) {
	ip, err := NewRuntime()
	if err != nil {
		t.Fatalf("NewRuntime error: %v", err)
	}

	// Evaluate in Global(ns): should see __oracle_execute via Global -> Base(ns) -> Core.
	v, err := ip.EvalSource(`isType(__oracle_execute, type Str -> Str?)`)
	if err != nil {
		t.Fatalf("EvalSource error: %v", err)
	}
	if v.Tag != VTBool || v.Data.(bool) != true {
		t.Fatalf("expected true for isType(__oracle_execute, type Str -> Str?), got: %#v", v)
	}
}

//  2. A user-space binding defined BEFORE the oracle is created is captured
//     lexically and used by the oracle.
func Test_Oracle_LexicalHook_UserSpaceBinding(t *testing.T) {
	ip := NewInterpreter()
	registerJSONParse(ip)

	v, err := ip.EvalSource(`
		let __oracle_execute = fun(prompt: Str) -> Str do
			"{\"output\":\"X-Ray Spex\"}"
		end
		let o = oracle() -> Str
		o()
	`)
	if err != nil {
		t.Fatalf("EvalSource error: %v", err)
	}
	wantStr(t, v, "X-Ray Spex")
}

//  3. A call-site shadow (let __oracle_execute = ...) does NOT override the
//     oracle's captured hook under pure lexical semantics.
func Test_Oracle_LexicalHook_IgnoresCallSiteShadow(t *testing.T) {
	ip := NewInterpreter()
	registerJSONParse(ip)

	// Install a fake backend in Core/Global *before* oracle definition so it’s
	// visible lexically when the oracle is created.
	registerFakeOracle(ip, `{"output":"Ada Lovelace"}`)

	v, err := ip.EvalSource(`
		let scientist = oracle() -> Str
		let g = fun() -> Str do
			# This local binding is at the call site and must NOT affect the oracle.
			let __oracle_execute = fun(prompt: Str) -> Str do
				"{\"output\":\"Hedy Lamarr\"}"
			end
			scientist()
		end
		g()
	`)
	if err != nil {
		t.Fatalf("EvalSource error: %v", err)
	}
	// Should still use the pre-installed hook, not the call-site shadow.
	wantStr(t, v, "Ada Lovelace")
}

//  4. The captured hook works inside a spawned process because the function's
//     closure chain is snapshotted into the child isolate.
func Test_Oracle_LexicalHook_SpawnedProcess(t *testing.T) {
	ip, _ := NewRuntime()
	registerJSONParse(ip)
	registerFakeOracle(ip, `{"output":"Grace Hopper"}`)

	v, err := ip.EvalSource(`
		let scientist = oracle() -> Str
		let worker = fun() -> Str do
			scientist()
		end
		let p = procSpawn(worker)
		procJoin(p)
	`)
	if err != nil {
		t.Fatalf("EvalSource error: %v", err)
	}
	wantStr(t, v, "Grace Hopper")
}
