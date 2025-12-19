
<style>
  .my-code span.prompt { color: black; }
  .my-code span.note { color: green; }
  .my-code span.value { color: blue; }
</style>

# Oracles and Annotations

This chapter covers two features that make MindScript different from typical scripting languages:

1. **Annotations**: `# ...` text attached to values, used for documentation and (especially) for guiding oracles.
2. **Oracles**: LLM-backed “black box” functions whose inputs/outputs are constrained by runtime-checked type schemas.

An oracle call is still a normal function call in MindScript: it has parameters, a return type, and it produces a value. The difference is that its implementation lives outside the language runtime and is invoked through an oracle backend.

---

## Annotations

An annotation is introduced with `#` and attaches to the **value of the next expression**.

<div class="my-code" markdown="0">
<pre><code><span class="prompt">==> # The speed of light in meters per second.</span>
<span class="prompt">... let c = 299792458</span>
<span class="note">The speed of light in meters per second.</span>
<span class="value">299792458</span>
</code></pre>
</div>

Annotations are metadata:

* They help humans (documentation).
* They help oracles (prompt instructions).
* They do **not** change equality, arithmetic, indexing, etc.

Annotations don’t automatically “flow” through computations:

<div class="my-code" markdown="0">
<pre><code><span class="prompt">==> # The golden ratio.</span>
<span class="prompt">... let phi = (1 + sqrt(5)) / 2</span>
<span class="note">The golden ratio.</span>
<span class="value">1.618033988749895</span>

<span class="prompt">==> 2 * phi</span> <span class="value">3.23606797749979</span> </code></pre>

</div>

### Programmatic access to annotations

You can read and write annotations using:

* `noteGet(x) -> Str?`
* `noteSet(text: Str, value) -> Any`

<div class="my-code" markdown="0">
<pre><code><span class="prompt">==> let c = 299792458</span>
<span class="value">299792458</span>

<span class="prompt">==> noteSet("The speed of light in meters per second.", c)</span> <span class="note">The speed of light in meters per second.</span> <span class="value">299792458</span>

<span class="prompt">==> noteGet(c)</span> <span class="value">"The speed of light in meters per second."</span> </code></pre>

</div>

### Multi-line annotations

Consecutive lines that start with `#` form a single multi-line annotation, attached to the following value:

```mindscript
# Processes a batch of raw sales records:
#   1. Filters out returned items.
#   2. Groups the remaining sales by region.
#   3. Sums revenue per region.
let processQuarterlySales = fun(records: [Any]) -> {} do
    ...
end
```

---

## Oracles

An **oracle** is declared like a function signature, but without a body:

```mindscript
# Write the name of an important researcher in the given field.
let researcher = oracle(field: Str) -> {name: Str}
```

The oracle’s **formal contract** is the declared parameter and return types. The runtime checks:

* argument values match the parameter types,
* the returned value matches the declared return type (with oracle-specific nullability rules described below).

The oracle’s **instruction** comes from the oracle value’s annotation (the `# ...` text attached to it).

Calling an oracle is just a normal call:

<div class="my-code" markdown="0">
<pre><code><span class="prompt">==> researcher("physics")</span>
<span class="value">{"name": "Albert Einstein"}</span>

<span class="prompt">==> researcher("biology")</span> <span class="value">{"name": "Charles Darwin"}</span> </code></pre>

</div>

### Oracles can fail (nullable return in practice)

Oracles are allowed to “not produce an answer.” Operationally, an oracle return is treated as **nullable** unless the declared return type is `Any`. This is how failures are represented:

* a failure returns `null` with an annotation explaining what went wrong (invalid JSON, type mismatch, backend error, etc.).

This makes it idiomatic to handle failures explicitly:

```mindscript
let r = researcher("physics")
if r == null then
    println("oracle failed: " + noteGet(r))
    null
else
    r.name
end
```

(When `r` is `null`, `noteGet(r)` may carry the failure reason.)

---

## Using examples

You can provide example input/output pairs using `from`.

Examples are most clear when written as:

* **input**: an object mapping parameter names to values
* **output**: a value matching the oracle return type

```mindscript
let examples = [
    [{number: 0}, "zero"],
    [{number: 1}, "one"],
    [{number: 2}, "two"],
    [{number: 3}, "three"]
]

let numberToEnglish = oracle(number: Int) -> Str from examples
```

Now calls can generalize beyond the examples:

```mindscript
numberToEnglish(2)    ## "two"
numberToEnglish(42)   ## e.g. "forty-two"
```

You can combine an annotation (instruction) with examples:

```mindscript
# Convert integers to English words.
let numberToEnglish = oracle(number: Int) -> Str from examples
```

---

## How the prompt is constructed

When you call an oracle, MindScript builds a prompt that includes:

* the instruction (the oracle annotation),
* an **input JSON Schema** derived from parameter names and parameter types,
* an **output JSON Schema** derived from the return type, **boxed** as:

  * `{"output": T}` where `T` is the oracle’s declared return type (success type),
* optional examples (normalized as input maps and boxed outputs),
* the current call’s concrete input values.

The backend hook is:

```mindscript
__oracle_execute(prompt: Str) -> Str?
```

It must return a JSON string. The runtime then parses it, extracts `output`, and type-checks it.

### What the model must return

The model output must be valid JSON, without code fences. It may return either:

* a boxed object: `{"output": <value>}`, or
* a bare value `<value>` (which MindScript will treat as if it were `{"output": <value>}`).

If parsing fails or the value doesn’t match the declared type, the oracle call returns an **annotated null**.

MindScript also attempts to repair common “JSON-ish” mistakes (fences, trailing commas, single quotes, etc.), but you should still aim for strict JSON.

---

## Operating oracles (installing and checking the backend)

The standard library exposes a simple workflow:

* `oracleStatus()` reports whether an oracle backend is installed.
* `oracleHealth()` performs a small end-to-end oracle call and reports success/failure.
* `oracleInstall(exec)` installs a backend function `exec: Str -> Str?` as `__oracle_execute`.

By default, the prelude imports `llm` and installs `llm.exec` as the oracle backend. You can inspect and change the backend/model through the `llm` module:

```mindscript
let llm = import("llm")

llm.useBackend("openai-responses")
llm.useModel("gpt-4.1-mini")
```

If you want to capture prompts for debugging, use the “tap” installer:

* `oracleInstallWithTap(exec)`
* `oracleLastPrompt()`
* `oracleLog()`

---

## Practical patterns

### Constrain output aggressively

Prefer structured output with explicit schemas:

```mindscript
let Person = type {
    name!: Str,
    role: Str?,
    confidence: Num?
}

# Extract a person from the text. Use null for missing fields.
let extractPerson = oracle(text: Str) -> Person
```

Then do deterministic cleanup after the oracle returns:

```mindscript
let p = extractPerson(inputText)
if p != null and p.role != null then
    p.role = toLower(strip(p.role))
end
p
```

### Use `try` for hard failures, and `null` for soft failures

If you call code that may panic (including import failures surfaced as hard errors), wrap it:

```mindscript
let r = try(fun() do
    extractPerson("Ada Lovelace wrote notes on the Analytical Engine.")
end)

if r.ok then
    r.value
else
    println("failed: " + str(r.value))
    null
end
```

### Secrets and untrusted text

Treat oracle prompts as data you may log. Avoid embedding secrets directly in prompts. When consuming untrusted text, keep the oracle output schema narrow and validate the result before using it.

