package mindscript

import (
	"testing"
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
// Returns JSON `{"ok":true}` on success, otherwise annotated null with reason.
func registerAssertingFakeOracle(ip *Interpreter) {
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
			outType := ctx.MustArg("outType").Data.(S)

			// helpers
			isAny := func(t S) bool { return len(t) == 2 && t[0] == "id" && t[1] == "Any" }
			isNullable := func(t S) bool { return len(t) >= 3 && t[0] == "unop" && t[1] == "?" }

			switch {
			case isAny(outType):
				// OK: Any must NOT be wrapped (Any? == Any)
			case isNullable(outType):
				// OK: non-Any should be nullable
			default:
				return annotNull("outType was not nullable")
			}
			return Str(`{"ok":true}`)
		},
	)
	if v, err := ip.Core.Get("__oracle_execute"); err == nil {
		ip.Global.Define("__oracle_execute", v)
	}
}

// --- tests -------------------------------------------------------------------

func Test_Oracle_StrSuccess(t *testing.T) {
	ip := NewRuntime()
	registerFakeOracle(ip, "Ada Lovelace")

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

func Test_Oracle_StrNullLiteral_YieldsNull(t *testing.T) {
	ip := NewRuntime()
	registerFakeOracle(ip, "null") // backend returns the literal string "null"

	v, err := ip.EvalSource(`
		let scientist = oracle() -> Str
		scientist()
	`)
	if err != nil {
		t.Fatalf("EvalSource error: %v", err)
	}
	wantNull(t, v) // oracle returns are always nullable
}

func Test_Oracle_JSONSuccess_Object(t *testing.T) {
	ip := NewRuntime()
	registerFakeOracle(ip, `{"name":"Marie Curie"}`)

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

func Test_Oracle_JSONFailure_TypeMismatch_AnnotatedNull(t *testing.T) {
	ip := NewRuntime()
	registerFakeOracle(ip, `{"wrong":42}`)

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
	registerFakeOracle(ip, `{"foo":123}`)

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
	registerFakeOracle(ip, "```json\n{\"name\":\"Rosalind Franklin\"}\n```")

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
	registerAssertingFakeOracle(ip) // asserts outType is nullable when not Any

	v, err := ip.EvalSource(`
		## Backend checks that outType is nullable (T?)
		## Return JSON {"ok": true} so we can validate value too.
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
	// Fake checks Any is not wrapped; if fine, returns {"ok":true}
	registerAssertingFakeOracle(ip)

	v, err := ip.EvalSource(`
		let a = oracle() -> Any
		## Backend returns {"ok": true}; engine should parse because return type isn't Str.
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
			return Str("ok")
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
	registerFakeOracle(ip, `{"ignored":true}`)

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
	registerFakeOracle(ip, "```\n{\"name\":\"Katherine Johnson\"}\n```")

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
	registerFakeOracle(ip, "```\nHello, world!\n```")

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
	registerFakeOracle(ip, "this is not json at all")

	v, err := ip.EvalSource(`
		let scientist = oracle() -> {name!: Str}
		scientist()
	`)
	if err != nil {
		t.Fatalf("EvalSource error: %v", err)
	}
	wantAnnotatedNullContains(t, v, "valid json")
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
