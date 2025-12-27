# Oracles

We have finally reached the chapter where we explain the central distinguishing feature of MindScript: **oracles**. Oracles are LLM-backed "black box" functions whose inputs/outputs are constrained by runtime-checked type schemas. Intuitively, calling an oracle is like "asking a friend for an answer", and are thus useful for semantic processing.

An oracle call is still a normal function call in MindScript: it has parameters, a return type, and it produces a value. The difference is that its implementation lives outside the language runtime and is invoked through an oracle backend.


---

## Oracle Declaration

An oracle is declared like a function signature, but without a body:
intal
```mindscript
# Write the name of an important researcher in the given field.
let researcher = oracle(field: Str) -> {name: Str}
```

The oracle’s *formal contract* is the declared parameter and return types. The runtime checks:

* argument values match the parameter types,
* the returned value matches the declared return type.

The oracle’s *instruction* comes from the oracle value’s annotation (the `# ...` text attached to it). Because every oracle can fail to produce an output, the runtime automatically expands the return type to a nullable type.

```mindscript-reply
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

Sometimes you want to help the oracle to produce outputs by supplying input/output examples. This is especially useful when the intended function is difficult to describe with an instruction alone. This is done with the keyword `from` followed by an array containing examples.

A valid collection of examples is an array containing input-output pairs that must conform to the type constraints. The general format of a single example is
```mindscript
[in1, in2, ..., inN, out]
```
where `in1` to `inN` are the input values and `out` is the output value, totalling *N*+1 values for an oracle with *N* inputs. For instance, let's say we want to create an oracle that takes a number as input (`Int`) and outputs that number spelled in English (`Str`), such as `5` and `five`. Then, this is how we could declare an oracle based on a handful of examples:

```mindscript
let examples = [
    [5, "five"],
    [5, "ten"],
    [5, "twenty-one"],
    [5, "fourty-two"]
]

let numberToEnglish = oracle(number: Int) -> Str from examples
```

Now calls can generalize beyond the examples:

```mindscript-repl
==> numberToEnglish(-5)
"minus five"

==> numberToEnglish(125)
"one hundred twenty-five"

==> numberToEnglish(-8)
"negative eight"

==> numberToEnglish(1024)
"one thousand twenty-four"
```

This examples demonstrates how to drive the behavior of an oracle *only* based on  few-shot examples, with no instructions. Of course, you can combine instruction and examples like so:

```mindscript
# Convert integers to English words.
let numberToEnglish = oracle(number: Int) -> Str from examples
```

The performance of oracles can improve significantly with a handful of examples, especially when it is hard to describe the task in words.

There are two helper functions to get and set an oracle `o`'s examples after it has been declared:

* `oracleGetExamples(o)` returns the examples;
* and `oracleSetExamples(o, examples)` sets them.

---

## Oracle execution details

!!! note
    This sections contains technical information for advanced usage.

Let's have a peek under the hood to see how oracles are implemented. When an oracle is called, the runtime executes the following sequence:

- *Prompt construction*: it builds a prompt using all the available hints.
- *Backend call*: it calls a oracle execution callback function.
- *Result analysis*: it parses and type-checks the result.

The details follow.

### Prompt construction 

During this phase construction MindScript builds a prompt from:

* a global *system prompt*, asking the model to follow the instruction and return a valid JSON object;
* an **instruction*, which is the oracle's annotation;
* an *input JSON Schema* derived from parameter names and types;
* an *output JSON Schema* derived from the return type `T`, **boxed** as `{"output": T}`;
* *examples*, normalized (inputs transformed into a map, outputs boxed),
* the current call's *input values*.

This data is then inserted into a prompt template, ready to be delivered. 

### Backend call and `__oracle_execute`

The MindScript runtime assumes there is an *LLM callback function* with signature

```mindscript
__oracle_execute(prompt: Str) -> Str?
```

defined within the oracle's lexical environment. The oracle will call this function with the prompt, expecting a string in return. In other words, the oracle will execute whichever function is bound to the variable `__oracle_execute` within the lexical scope *at declaration time*.

This design allows installing/changing backends at runtime by assigning a new backend `exec` to `__oracle_execute`, or equivalently, by calling the standard helper function to install a new backend is
```mindscript
oracleInstall(exec: Str -> Str?) -> Bool
```


### Result Analysis 

The prompt asks the model to output a valid JSON string, without code fences. This could be either:

* a boxed object: `{"output": <value>}`, or
* a bare value `<value>` (which MindScript will treat as if it were `{"output": <value>}`).

If parsing fails or the value doesn’t match the declared type, the oracle call returns an error. MindScript also attempts to repair common “JSON-ish” mistakes (fences, trailing commas, single quotes, etc.), but you should still aim for strict JSON.

---

## Backend management

At startup, MindScript automatically loads LLM helper functions and the LLM management module `llm`. This module exposes the following utility functions:

* `oracleStatus()` reports whether an oracle backend is installed.
* `oracleHealth()` performs a small end-to-end oracle call and reports success/failure.
* `oracleInstall(exec)` installs a backend function `exec: Str -> Str?` as `__oracle_execute`.
* `llm.status()`, to report the current backend status;
* `llm.backends()`, to list the registered backend names;
* `llm.useBackend(name: Str)`, to switch to another backend by name;
* `llm.models()`, to list the available models for the chosen backend;
* `llm.useModel(name: Str)`, to set the active model for the backend.

The typical workflow is as follows: at the beginning of a program, we call `llm.useBackend` and `llm.useModel` with the chosen LLM backend and model, respectively. For instance,

```mindscript
llm.useBackend("openai-responses")
llm.useModel("gpt-4.1-mini")

# oracle definitions
...
```

Internally, 

In addition, there are the following helper functions:



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

