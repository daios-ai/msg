
# Oracles

We have finally reached the chapter where we explain the central distinguishing feature of MindScript: **oracles**. Oracles are LLM-backed “black box” functions whose inputs/outputs are constrained by runtime-checked type schemas. Intuitively, calling an oracle is like “asking a friend for an answer”, and they are useful for semantic processing.

An oracle call is still a normal function call in MindScript: it has parameters, a return type, and it produces a value. The difference is that its implementation lives outside the language runtime and is invoked through an oracle backend.

---

## Oracle Declaration

An oracle is declared like a function signature, but without a body:

```mindscript
# Write the name of an important researcher in the given field.
let researcher = oracle(field: Str) -> {name: Str}
```

The oracle’s *formal contract* is the declared parameter and return types. The runtime checks:

* argument values match the parameter types,
* the returned value matches the declared return type.

The oracle’s *instruction* comes from the oracle value’s annotation (the `# ...` text attached to it).

Because every oracle call can fail at the boundary (backend unavailable, malformed model output, schema mismatch), the runtime widens the return type to a nullable type when appropriate. In practice: if the declared return type is `T`, the operational return type is `T?` unless it is already nullable (or is `Any`, where widening is not useful).

```mindscript-repl
==> researcher
# Write the name of an important researcher in the given field.
<oracle: field:Str -> {name: Str}?>
```

Once declared, calling an oracle is just a normal call:

```mindscript-repl
==> researcher("physics")
{"name": "Albert Einstein"}

==> researcher("biology")
{"name": "Charles Darwin"}
```

### Using examples

Sometimes you want to help the oracle produce outputs by supplying input/output examples. This is especially useful when the intended function is difficult to describe with an instruction alone. This is done with the keyword `from` followed by an array containing examples.

A valid collection of examples is an array containing input-output pairs that must conform to the type constraints. The general format of a single example is:

```mindscript
[in1, in2, ..., inN, out]
```

where `in1` to `inN` are the input values and `out` is the output value, totalling *N*+1 values for an oracle with *N* inputs.

For instance, let's say we want to create an oracle that takes a number as input (`Int`) and outputs that number spelled in English (`Str`), such as `5` and `five`. Then this is how we could declare an oracle based on a handful of examples:

```mindscript
let examples = [
    [5, "five"],
    [10, "ten"],
    [21, "twenty-one"],
    [42, "forty-two"]
]

let numberToEnglish = oracle(number: Int) -> Str from examples
```

Now calls can generalize beyond the examples:

```mindscript-repl
==> numberToEnglish(-5)
"minus five"

==> numberToEnglish(125)
"one hundred twenty-five"

==> numberToEnglish(1024)
"one thousand twenty-four"
```

You can combine an instruction and examples:

```mindscript
# Convert integers to English words.
let numberToEnglish = oracle(number: Int) -> Str from examples
```

There are two helper functions to get and set an oracle `o`’s examples after it has been declared:

* `oracleGetExamples(o)` returns the examples. In our previous example, `oracleGetExamples(numberToEnglish)` would return the `examples` array.
* `oracleSetExamples(o, examples)` sets the examples for the oracle.

---

## Backend management

Oracles depend on a backend executor. This can be managed using the `llm` module loaded upon startup, which allows you to select a backend and a model, and configure it.

Alternatively, if you wish to use a backend not provided in the `llm` module, or if you prefer to use a custom backend manager, you can do that too. Refer to the section on [oracle execution technical details](#oracle-execution-technical-details).

### Querying the status

The runtime and the `llm` module provide builtin helper functions for querying the status of the oracle backend:

* `oracleStatus()` returns a short status string.
* `oracleHealth()` performs a tiny real oracle call and returns `{ok: true, ms: Int}` on success, or an error on failure.
* `llm.status()` returns information about the configuration of the backend.

You can test them in the REPL:

```mindscript-repl
==> oracleStatus()
"oracle: installed"

==> oracleHealth()
{ok: true, ms: 843}

==> llm.status()
{backend: "ollama", model: "llama3.2", authed: true, options: {}}
```

### Choosing backend and model

To check the available backends, use the `llm.backends()` to display them. To obtain a list of the names of all available models for the chosen backend use `llm.models()`.

```mindscript-repl
==> llm.backends()
["ollama", "openai", "openai-responses", "anthropic", "cohere"]

==> llm.models()
["llama3.2:latest", "llama3.1:latest", "Phi3:latest"]
```

Once you have decided which backend and model to use, use `llm.useBackend(backendName)` and `llm.useModel(modelName)`. For instance,

```mindscript-repl
==> llm.useBackend("openai-responses")
{backend: "openai-responses", model: "", authed: true, options: {}}

==> llm.useModel("gpt-4o-mini")
{backend: "openai-responses", model: "gpt-4o-mini", authed: true, options: {}}
```

The backends that require an API key to function will consult environmental variables at startup. At the time of writing, these are

```
OPENAI_API_KEY
ANTHROPIC_API_KEY
COHERE_API_KEY
```

for OpenAI, Anthropic, and Cohere, respectively. You can also set them at runtime using the `llm.auth` function:

```mindscript-repl
llm.auth({apiKey: "MY-SECRET-KEY"})
```

However, for security reasons, make sure you don't hardcode the API keys in the source code. It is better to e.g. read them from an environmental variable (e.g. using `osEnv`).

### Configuring to the Backend

You can further configure the backend, e.g. if you wish you change the URL of the endpoint, of adjust the options (e.g. temperature, sampling parameters) of a model.

To get the current configuration, use `llm.getConfig()`:

```mindscript-repl
==> let conf = llm.getConfig()
{
	backend: "openai-responses",
	baseUrl: "https://api.openai.com/v1",
	model: "gpt-4o-mini",
	options: {}
}
```

This returns a map holding the current configuration. To set a new configuration, you can override its fields and call `llm.setConfig(conf)`. For instance, assume we have another provider who uses an OpenAI-compatible API for requests:

```mindscript-repl
==> conf.baseUrl = "https://api.my-provider.com/"
"https://api.my-provider.com/"

==> conf.model = "custom-model"
"custom-model"

==> llm.setConfig(conf)
{backend: "openai-responses", model: "custom-model", authed: true, options: {}}
```

Backend-model combinations also support execution options. The most typical ones are the sampling parameters (e.g. `temperature`, `top_k`, `top_p`, `max_tokens`). But which ones are available/supported depends on the specific API of the backend provider.

### Diagnostics (prompt tap)

!!! note
    This section is incomplete.

For debugging, the prelude provides a “tap” installer and log accessors:

* `oracleInstallWithTap(exec)`
* `oracleLastPrompt(null)`
* `oracleLog(null)`

These let you capture the most recent request and keep a rolling log of requests/outputs.

---

## Practical patterns

### Constrain output aggressively

Oracles are most reliable when you make the contract *small and checkable*. Prefer **structured return types** over free-form text, and keep fields as simple as you can.

```mindscript
let Person = type {
    name!: Str,
    role: Str?,
    confidence: Num?
}

# Extract one person mentioned in the text.
# Use null for missing optional fields.
let extractPerson = oracle(text: Str) -> Person
```

A structured schema pays off in three ways:

* **Type-checking catches junk early.** If the model returns the wrong shape, the oracle call fails as a value (`null` with a reason) instead of silently producing a misleading string.
* **Post-processing becomes deterministic.** You can normalize casing, trim whitespace, and clamp numeric ranges without guessing where the data lives.
* **Backends can enforce structure.** Some backends can use the derived JSON Schema to constrain outputs (for example “structured output” modes). Capabilities vary, so don’t assume every backend supports every schema feature equally. When portability matters, stick to straightforward JSON shapes: objects, arrays, strings, numbers, booleans, and `null`, plus enums and nullable fields.

After the oracle returns, do deterministic cleanup and validation in normal code:

```mindscript
let p = extractPerson(inputText)
if p == null then
    p
else
    if p.role != null then
        p.role = strip(toLower(p.role))
    end
    if p.confidence != null then
        # Keep confidence in [0, 1] if that’s your convention.
        if p.confidence < 0 then p.confidence = 0 end
        if p.confidence > 1 then p.confidence = 1 end
    end
    p
end
```

A good rule of thumb: let the oracle do the *semantic* work (interpretation), and let deterministic code do the *mechanical* work (normalization, validation, formatting).

### Secrets and untrusted text

Assume prompts and oracle requests may end up in logs (especially if you enable prompt tapping). Don’t embed secrets directly in instructions or input text; pass sensitive data through safer channels when available, or keep it out of the oracle call entirely.

For untrusted text, treat the oracle as a boundary:

* **Narrow the schema** to only what you need.
* **Validate before acting** (types, allowed values, lengths/ranges).
* **Avoid “prompt-injection as control flow.”** Never let the model decide what code to run, what URL to fetch, or what command to execute without strict, deterministic checks in between.

If you want “safe automation,” the pattern is: oracle proposes → code verifies → code acts.

---

## Oracle execution technical details

!!! danger
    This section describes the executor hook and request/response contract. Most users should not need it.

This section explains how oracle calls are implemented, what the execution pipeline looks like, and how to use that knowledge to build a backend or customize LLM execution.

At a high level, an oracle call is a three-stage pipeline:

* **Context collection.** The runtime gathers everything needed to execute the oracle call:

    * the global system prompt,
    * the oracle annotation (instruction),
    * examples (if any),
    * the current call input values,
    * input/output JSON Schemas derived from the parameter and return types.

    The runtime then normalizes examples, renders schema strings, and builds the final prompt. All of these derived artifacts are packaged into a single request value `req`.

* **Executor hook call.** The runtime invokes the executor hook `__oracle_execute(req)`. Whatever code is currently installed there is responsible for turning the request into a model response.

* **Result analysis.** The runtime parses the returned JSON string (repairing common “JSON-ish” mistakes when possible) and checks the resulting value against the oracle’s declared output type.

### The executor hook

Oracles execute by calling a single hook function that lives in the oracle’s lexical environment:

```mindscript
__oracle_execute(req: OracleRequest) -> Str?
```

The hook is prelude-owned and initialized to a placeholder implementation, so the name always exists even before any backend is configured. In `std.ms` it starts life as:

* a global binding named `__oracle_execute`,
* initially set to a function that reports the lack of configuration,
* plus an installation helper that replaces it.

Backends install their executor by calling:

```mindscript
oracleInstall(exec: OracleRequest -> Str?) -> Bool
```

The default prelude behavior is to import the `llm` module, select a backend/model, and then install `llm.exec` via `oracleInstall(llm.exec)`. In other words, “installing a backend” is simply rebinding the global `__oracle_execute` name to a function with the right signature.

Two practical consequences follow from this design:

1. **Backends are just functions.** Anything that can implement `OracleRequest -> Str?` can serve as an oracle backend (OpenAI, Ollama, a mock backend for tests, a logger wrapper, etc.).
2. **The hook is explicit and inspectable.** Because `__oracle_execute` is an ordinary binding, you can swap it, wrap it (tap/log), or replace it in a controlled way at program startup.

### What the backend receives: `OracleRequest`

The value passed to `__oracle_execute` is an object of type `OracleRequest`. It contains both the “human” intent and the “machine” constraints of the call:

* `prompt: Str` — the fully rendered prompt string to send to the model.
* `annotation: Str` — the oracle’s annotation text (instruction) verbatim.
* `examples: [Any]` — normalized examples.
* `input: Any` — the current call’s inputs boxed as a map `{paramName: value}`.
* `inTypes: [Type]` — parameter types in declaration order.
* `outType: Type` — the *boxed* output type, equivalent to `type {output!: T}`.
* `inputSchema: Any`, `outputSchema: Any` — JSON Schema values for inputs/outputs.
* `inputSchemaString: Str`, `outputSchemaString: Str` — pretty schema strings.

Examples are normalized before being passed to the backend: each surface example `[in1, ..., inN, out]` becomes a pair `[inputMap, outputMap]` where `inputMap = {param1: in1, ...}` and `outputMap = {output: out}`.

The intent here is that a backend does not need to reconstruct anything: the runtime hands it the exact prompt (and the structural metadata used to produce it) in a single request object.

### What the backend must return

The executor returns a string containing JSON (no code fences). The oracle engine accepts:

* a boxed object: `{"output": <value>}` (preferred), or
* a bare JSON value **only when it is not an object** (number/string/bool/null/array), which will be treated as if it were boxed.

The returned value is then checked against the oracle’s declared return type (via the boxed schema).

### Scope note: when `__oracle_execute` is resolved

`__oracle_execute` is resolved from the oracle’s **lexical environment at declaration time**. This is a deliberate design choice: it makes oracle values self-contained and predictable, but it also means backend switching is not retroactive.

If you plan to install or switch backends/models, do it before declaring oracles (or redeclare them) so the oracle captures the intended executor.

