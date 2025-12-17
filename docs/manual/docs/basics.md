# Quick Tour

MindScript is a small language for a very specific kind of work: taking JSON-shaped input, transforming it into JSON-shaped output, and (when it helps) inserting a *semantic* step powered by an LLM. The language stays compact by treating “data work” as the default and making everything expression-oriented, so scripts naturally read as “build a value, return it”.

This tour uses two styles of examples. Some are **script snippets** you’d put in a `.ms` file. Others are **REPL transcripts** where the prompt is `==>`, followed by what you type, and the lines after are what the runtime prints.

## A tiny example that mixes semantic and deterministic work

Here’s a complete mini-program that does both types of computation: one oracle call to generate something semantic, and then deterministic code that computes a number and builds an output object.

```mindscript
let ticket = { id: 17, body: "Cannot log in after password reset; shows error 403." }

# Write a short, helpful title for this support ticket.
let makeTitle = oracle(text: Str) -> Str

let words = len(split(ticket.body, " "))
{ id: ticket.id, title: makeTitle(ticket.body), wordCount: words }
```

The last line is the “result” of the script: an object with three fields. If you’re used to Python or JavaScript, think of it like ending a script with a final expression rather than printing manually.


## Evaluation in the REPL

Basic values print in a JSON-like way:

```text
==> 42
42

==> {name: "Ada", age: 36}
{name: "Ada", age: 36}

==> [10, 20, 30]
[10, 20, 30]
```

`print` (and `println`) prints a value but also returns it, which fits MindScript’s “expressions everywhere” style:

```text
==> print("hello")
hello
"hello"
```

If you attach a note/comment to a value it will stick to it
```text
==> # The name of the person
... let name = "John"
"John" # The name of the person
```

## Values are JSON-shaped

MindScript’s value model is intentionally familiar:

* `null`, `true`, `false`
* integers (`Int`) and floating numbers (`Num`)
* strings (`Str`)
* arrays: `[a, b, c]`
* objects/maps: `{key: value}`

Arrays are 0-based, and negative indices count from the end:

```mindscript
let xs = [10, 20, 30]
xs[0]   # 10
xs[-1]  # 30
```

Objects use either bare identifier keys or quoted keys:

```mindscript
let obj = { name: "Ada", "not-ident": 123 }
obj.name
obj["not-ident"]
```

Accessing a missing key or going out of bounds is a **panic**.
```text
==> obj.age
RUNTIME ERROR in <repl> at 1:5: unknown property "age"

   1 | obj.age
     |     ^

==> xs[3]
RUNTIME ERROR in <repl> at 1:4: array index out of range

   1 | xs[3]
     |    ^

```

## Names, assignment, and destructuring

You bind names with `let`:

```mindscript
let greeting = "Hello"
```

You update existing names with `=`:

```mindscript
let x = 1
x = x + 1
```

Updating a name that was never declared panics.

Destructuring lets you unpack arrays and objects in one step:

```mindscript
let [a, b] = [1, 2, 3]                     # a=1,     b=2
let { name: n, age: a } = { name: "Ada" }  # n="Ada", a=null
```

If a key is missing during object destructuring, the binding becomes `null`.

## Everything is an expression

MindScript is expression-oriented. Blocks, conditionals, loops, declarations, assignments—these all evaluate to values. This is why scripts often end with an object or array literal: the program “returns” that value.

A `do ... end` block evaluates to the last expression inside:

```mindscript
let answer =
  do
    let a = 40
    let b = 2
    a + b
  end

answer  # Should be 42
```

This style becomes especially pleasant when you build up a result in steps and want the last line to be the output value.

## Control flow that still produces values

Conditionals use `if / elif / else / end` and evaluate to the chosen branch:

```mindscript
let label =
  if 2 < 3 then
    "yes"
  else
    "no"
  end
```

There is a `while` loop:

```mindscript
let i = 0
while i < 3 do
  println(i)
  i = i + 1
end
```

And a `for` loop that iterates arrays and objects. Iterating an object yields `[key, value]` pairs, so destructuring works naturally:

```mindscript
let sum = 0
for x in [1, 2, 3] do
  sum = sum + x
end
sum
```

```mindscript
for [k, v] in { a: 1, b: 2 } do
  printf("%s=%d\n", [k, v])
end
```

Early exits carry values. `return`, `break`, and `continue` can all include a payload (and if you omit it, it defaults to `null`). This makes it easy to write loops that *produce* a meaningful result rather than only causing side effects.

## Functions: familiar syntax, curried calls

Functions are values created with `fun`:

```mindscript
let add = fun(a: Int, b: Int) -> Int do
  a + b
end
```

MindScript treats multi-argument functions as curried under the hood. In practice that means:

* `add(1, 2)` is sugar for `add(1)(2)`
* you can partially apply:

```mindscript
let add3 = add(3)
add3(4)  # 7
```

Parameter types and return types are checked at runtime. If a function returns something that doesn’t match its declared return type, that’s a panic. If you omit types, they default to `Any`.

## Types are first-class, structural schemas

Types describe the shape of values and can be named and inspected.

```mindscript
let Person = type { name!: Str, age: Int }
```

A field marked with `!` is required. Fields without `!` are optional. Nullable types are written with `?`:

```mindscript
let Person2 = type { name!: Str, email: Str? }
```

You can introspect and validate at runtime:

```mindscript
typeOf(42)                          # Int
isType({ name: "Ada", age: 36 }, Person)
```

Enums are literal sets:

```mindscript
let Label = type Enum["bug", "question", "feature"]
```

These types become especially valuable at oracle boundaries, where you want strict, machine-checkable constraints.

## Annotations: comments that attach to values

MindScript has comments that can act as metadata. A `#` annotation can attach to the next expression (header style) or to the expression on the same line (inline style). This metadata is crucial for oracles and still useful as documentation elsewhere.

Header annotation:

```mindscript
# The speed of light in meters per second.
let c = 299_792_458
```

Inline annotation:

```mindscript
let c = 299792458 # The speed of light.
```

Annotations don’t change equality or typing, but you can read them (for example with `noteGet`), and the oracle system can use them as part of its prompt construction.

## Oracles: type-gated, LLM processing 

An oracle is declared like a function signature, but it has no body:

```mindscript
# Write a short, helpful title for this support ticket.
let makeTitle = oracle(text: Str) -> Str
```

When you call an oracle, arguments are type-checked. The output type is automatically made nullable if it isn't already. In the example, `Str` is changed to `Str?`. If validation fails—or the backend is unavailable—the result becomes `null`. That means you write oracle code defensively:

```mindscript
let title = makeTitle(ticket.body)
if title == null then
  "Untitled"
else
  title
end
```

You can also guide an oracle using examples:

```mindscript
let examples = [
  [0, "zero"],
  [1, "one"],
  [2, "two"]
]

# Say the English word for a number.
let number2word = oracle(n: Int) -> Str from examples
```

Examples steer behavior but don’t eliminate uncertainty; schema validation is what keeps the output within bounds.

## Oracle setup

At startup, MindScript automatically imports the LLM backend configuration library `llm`, containing discovery, initialization, and health functions:

```text
==> llm.backends()
["ollama", "openai", "openai-responses", "anthropic", "cohere"]

==> llm.models()
["qwen2.5-coder:1.5b", "qwen2.5:1.5b"]

==> llm.status()
{backend: "ollama", model: "qwen2.5:1.5b", authed: true, options: {}}

==> llm.getConfig()
{
	backend: "ollama",
	baseUrl: "http://localhost:11434",
	model: "qwen2.5:1.5b",
	options: {}
}

==> oracleStatus()
"oracle: installed"

==> oracleHealth()
{ok: true, ms: 12993}
```

## Errors, panics, and `try`

MindScript uses two different failure styles.

The first is **errors as ordinary values**. If a failure is expected to happen, —e.g. bad input, missing fields, oracles failing validation—it's idiomatic to return `null` and make that possibility explicit in the type using a nullable return such as `T?` (or, `Any`). You typically attach a short error message to the returned `null` value. The caller can thus check for `null` and decide what to do next. 

The second is a **panic**, caused by missing keys, out-of-bounds indexing, type mismatches, parser/runtime faults, hitting a parser/runtime fault, that is, any programming or fatal error that aborts evaluation. 

If you need to recover from a panic, use `try`, which runs a zero-argument function and captures the panic into a normal `{ok, value}` result:

```mindscript
let r = try(fun() do
  { x: 1 }.missing  # panics
end)

r
```

## Modules and imports

Modules are how you scale from a single script to a project with reusable files. A module is just a `.ms` file whose top-level `let` bindings become that module’s “exports”.

Let's say you have a `util.ms` with the following content:
```mindscript
# Normalize a ticket.
let normalize = fun(t: { body!: Str }) -> { body!: Str } do
  { body: strip(t.body) }
end
```

Then, you can call `normalize` from `main.ms` as follows:
```mindscript
let util = import("util.ms")
let ticket = { body: "  hello  " }
util.normalize(ticket)
```

The imported module evaluates once and returns a **module value** that you can use like an object/map. Each module runs in its own environment (namespace), and names defined inside a module do not leak into the caller.

You can inspect the content of a module using the `dir` function:
```text
===> let m = import("util.ms")
<module: util>

==> dir(util)
[
	"normalize" # Normalize a ticket.
]
```

Imports are **per-process singletons** and every subsequent `import(...)` call will refer to the exact same instance. Also, `import` can load from the local filesystem or from the web (same interface), so the same namespace rules apply either way.


## Standard library

MindScript fires up with a solid set of builtins right out of the box:

* **Data + types**: ways to inspect values at runtime and validate that unknown input has the shape you expect, so you can write small “adapters” instead of fragile code.
* **JSON + schema bridge**: parse and emit JSON, and translate between MindScript’s type descriptions and JSON Schema so your contracts can travel with your data (and especially across oracle boundaries).
* **Text + formatting**: everyday string operations, lightweight pattern matching, and formatting helpers for both values and MindScript source.
* **IO + integration**: work with files and streams, read environment variables, talk to the network (including HTTP), and run external programs—everything you need for practical glue code.
* **Time + URLs + encodings**: timestamps and RFC3339 time strings, URL/query parsing and construction, and common encodings like hex/base64 for moving data between systems.
* **Binary utilities**: compression and cryptographic primitives, with strings doubling as a convenient byte container when you need to handle non-text payloads.
* **Concurrency**: run isolated background work, pass messages between tasks, use timers/tickers, and keep mutable state safe by confining it to a single-threaded “actor”.
* **Introspection**: attach/read notes on values, and build tools that parse, validate, format, or evaluate MindScript code programmatically.
* **Foreign function interface (advanced)**: a bridge to existing C libraries. Use it to reuse battle-tested native code or reach for performance-critical pieces without leaving MindScript.

In addition, MindScript ships with **standard modules** you can import, such as:

* `llm`: a small manager for language-model providers. It gives you one place to choose a backend, select a model, and more.
* `testing`: a lightweight test framework plus a test runner. It supports named tests, table-driven cases, assertions, and more. It is what `msg test` uses.
* `canon`: the formatter module used by `msg fmt`, including whole-tree formatting and “check only” workflows for CI.
* `nethttp`: an HTTP server library for building small services with a predictable API: routing, request/response helpers, and streaming-friendly handling.
* `mtml`: a small HTML templating engine in the Jinja family, focused on safe defaults and a short surface area; good for generating emails and simple pages from structured data.
