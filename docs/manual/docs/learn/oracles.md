<style>
  .my-code span.prompt { color: black; }
  .my-code span.note { color: green; }
  .my-code span.value { color: blue; }
</style>

# Oracles and Informal Types

In this chapter, we explore two powerful features of MindScript that blur the line between code and documentation, and increase its expressiveness beyond traditional programming languages:

1. *Informal Annotations*: a lightweight, comment-based type hinting system;
2. *Oracles* LLM-backed computational “black boxes” that boost the language’s capabilities.

In classical computation theory, an **oracle machine** is a Turing machine augmented with access to a “black-box” that instantly solves a particular problem, even if it is undecidable by the machine alone. Similarly, 

* *Semantic Queries*: Oracles can answer questions about meaning or world knowledge that no static algorithm could compute (e.g., “Who invented the World Wide Web?”).
* *On-the-fly Reasoning*: They enable operations like free-form text generation, pattern recognition, or domain-specific inference, all at runtime.
* *Extensible Capability*: By changing the LLM prompt (informal type) or examples, you effectively “upgrade” the oracle’s power without altering the language core.

This mechanism sets MindScript apart from a purely syntactic language to one with **semantic computation** capabilities, akin to dynamically consulting an expert whenever you need an intelligent answer.


## Informal Annotations

In MindScript, you can attach human-readable comments directly to values or definitions. These comments become **informal types**, guiding a Large Language Model (LLM) when it interprets or generates code. They are *not* checked at runtime, but they enrich the "intent" behind your code.

Comments are denoted by the `#` operator following by an explanation. Place a comment on its own line immediately before an expression. The annotation attaches to the *value* of that expression.
<div class="my-code" markdown="0">
<pre><code><span class="prompt">> # The speed of light in meters per second.</span>
<span class="prompt">| let c = 299792458</span>
<span class="note">The speed of light in meters per second.</span>
<span class="value">299792458</span>
</code></pre>
</div>

Here, the value `299792458` gains the informal type "The speed of light in meters per second."

If we type a variable name in the REPL, it returns the annotation and the value:
<div class="my-code" markdown="0">
<pre><code><span class="prompt">> c</span>
<span class="note">The speed of light in meters per second.</span>
<span class="value">299792458</span>
</code></pre>
</div>

We have already seen annotations with double-hashes `##`. Double-hash annotations are ignored by the interpreter, i.e. they behave just like code comments in other programming languages.
<div class="my-code" markdown="0">
<pre><code><span class="prompt">> ## The speed of light in meters per second.</span>
<span class="prompt">> let c = 299792458    ## This will be ignored too.</span>
<span class="value">299792458</span>
</code></pre>
</div>

Setting and retrieving notes can also be achieved programmatically using the `setNote([val], [note])` and `getNote([val])` functions respectively.
<div class="my-code" markdown="0">
<pre><code><span class="prompt">> let c = 299792458</span>
<span class="value">299792458</span>

<span class="prompt">> setNote(c, "The speed of light in meters per second.")</span>
<span class="note">The speed of light in meters per second.</span>
<span class="value">299792458</span>

<span class="prompt">> getNote(c)</span>
<span class="value">"The speed of light in meters per second."</span>
</code></pre>
</div>

You can stack multiple `#`-comments directly above a definition to form a single, multi-line annotation. All of those lines will be treated as one coherent "hint". For example, imagine you want to annotate a function that processes quarterly sales data:

```mindscript
# Processes a batch of raw sales records.
#   1. Filters out any returned items.
#   2. Groups the remaining sales by region code.
#   3. Sums the total revenue per region.
# Returns list of revenues, one per region.
let processQuarterlySales = fun(records: [SaleRecord]) -> [Revenue] do

    ... the code here ...

end
```
Here, the four consecutive `#` lines form one multi-line annotation describing *what* `processQuarterlySales` does and *how*. Under the hood, MindScript (and any connected LLM) will treat that block as a single "informal type" or hint attached to the function's value.


Annotations are *meta-data*. Unlike values, which represent the actual information your code processes, annotations only convey intent, documentation, or hints for another human reader or an oracle. In other words, formal operations won't be impacted by an annotation.

Annotations don't propagate in operations. If you use an annotated value in an expression, the result won't have any annotation unless you attach one explicitly.
<div class="my-code" markdown="0">
<pre><code><span class="prompt">> # The Golden ratio.</span>
<span class="prompt">| let phi = (1 + sqrt(5)) / 2</span>
<span class="note">The Golden ratio.</span>
<span class="value">1.618033988749895</span>

<span class="prompt">> 2 * phi</span>
<span class="value">3.23606797749979</span>
</code></pre>
</div>


### Documenting Functions

As is the case in other programming languages, annotations are especially helpful for documenting the purpose of functions:

```mindscript
# Computes the sum of two integers.
let sum = fun(n: Int, m: Int) -> Int do
    n + m
end
```
This declaration specifies that the function `sum` has a type with two parts:

* *Formal part*: `fun(n: Int, m: Int) -> Int` declares the signature which restricts the formal type of the inputs and outputs and is runtime checked.
* *Informal part*: The preceding comment helps a human reader and an oracle to understand that `sum` "computes the sum of two integers." 


### Annotating Data Structures

You can sprinkle annotations inside object literals to explain individual fields:

```mindscript
let Person = {
    # The person’s full name.
    name!: Str,

    # Age in completed years.
    age: Int
}
```

This approach turns your data model into self-documenting code. It enhances readability and it serves as an explanation of the *semantic meaning* of the data passed around.

## Oracles

An **oracle** in MindScript is like a function powered by an LLM: it can perform inductive inference based on either:

* A **natural-language description** (an informal type), or/and
* A **set of examples**.

Calling an oracle is like sending a message with a question to another person who replies with the best answer they can come up with. Under the hood, invoking an oracle sends a prompt—including formal and informal type hints or examples—to the LLM, which returns a result conforming to the declared type.

### Basic Syntax

To declare an oracle, use the `oracle` keyword similarly to using `fun` for defining a function but without specifying a body.
```mindscript
# Write the name of an important researcher in the given field.
let researcher = oracle(field: Str) -> {name: Str}
```
In this declaration, the formal type of the oracle is `field:Str -> {name: Str}`: it expects a `Str` and returns an object containing a `name` property. The informal type of the oracle, "Write the name of an important researcher in the given field.", is interpreted as an instruction.

You can then call it as you would any function:
<div class="my-code" markdown="0">
<pre><code><span class="prompt">> researcher("physics")</span>
<span class="value">{ "name": "Albert Einstein" }</span>

<span class="prompt">> researcher("biology")</span>
<span class="value">{ "name": "Charles Darwin" }</span>
</code></pre>
</div>


### Using Examples

Sometimes it is better to demonstrate the desired behavior using examples rather than describing it. If you already have input/output pairs, you can add them using the `from` keyword followed by a list of examples:

```mindscript
let examples = [
    [0, "zero"],
    [1, "one"],
    [2, "two"],
    [3, "three"]
]

let number2lang = oracle(number: Int) -> Str from examples
```
Here, `number2lang(2)` will induce `"two"`, and even produce new mappings like `"forty-two"` for `42`. An array of examples must always contain `[input, output]` pairs. 

Of course, you can always combine descriptions and examples to improve the hints for the oracle.


## How is the LLM prompt constructed?

<style>
  .oracle span.purple { color: purple; }
  .oracle span.blue   { color: blue; }
  .oracle span.green  { color: green; }
  .oracle span.red    { color: red; }
  .oracle span.cyan   { color: darkcyan; }
</style>

As mentioned before, informal types are hints for guiding the behavior of the LLM. But how does this happen, precisely? Under the hood, MindScript uses the hints during the oracle invocation to draft a prompt for the LLM. The informal type of the function will be interpreted as the instruction, the input and output types are constraints on the JSON Schemas, etc.

To illustrate, consider the following creation of an oracle.

<div class="oracle" markdown="0">
<pre><code><span class="purple"># Example input output pairs.
let examples = [
  [{input: 9}, {output: 16}],
  [{input: 25}, {output: 36}], 
  [{input: 1}, {output: 4}]
]</span>

<span class="blue"># The input number.
let Input = type {input!: Int}</span>

<span class="green"># The output number.
let Output = type {output!: Int}</span>

<span class="red"># Given a square number, provide the next square.</span>
let main = oracle(<span class="blue">args: Input</span>) -> <span class="green">Output</span> from <span class="purple">examples</span>

main(<span class="cyan">{input: 144}</span>)
</code></pre>
</div>

Then a call to the oracle will generate the following prompt below, which has been colored to highlight the correspondences to the code above.
<div class="oracle" markdown="0">
<pre><code>
PROMPT: 
You are a helpful assistant, and your task is to provide answers
respecting the format of the OUTPUT JSON SCHEMA. Do not put code
fences around the output (like ```json), only generate valid JSON.

<span class="blue">INPUT JSON SCHEMA:

{
    "type": "object",
    "required": [
        "input"
    ],
    "properties": {
        "args": {
            "type": "object",
            "description": "The input number.",
            "required": [
                "input"
            ],
            "properties": {
                "input": {
                    "type": "integer"
                }
            }
        }
    }
}</span>

<span class="green">OUTPUT JSON SCHEMA:

{
    "type": "object", 
    "properties": {
        "result": {
            "type": "object", 
            "description": "The output number.", 
            "required": ["output"], 
            "properties": {"output": {"type": "integer"}}}
    }, 
    "required": ["result"]
}</span>

<span class="red">TASK:

Given the input, determine the output.</span>

<span class="purple">INPUT:

{"input": {"input": 9}}

OUTPUT:

{"output": 16}</span>

<span class="red">TASK:

Given the input, determine the output.</span>

<span class="purple">INPUT:

{"input": {"input": 25}}

OUTPUT:

{"output": 36}</span>

<span class="red">TASK:

Given the input, determine the output.</span>

<span class="purple">INPUT:

{"input": {"input": 1}}

OUTPUT:

{"output": 4}</span>

<span class="red">TASK:

Given a square number, provide the next square.</span>

<span class="cyan">INPUT:

{"input": {"input": 144}}</span>

OUTPUT:

</code></pre>
</div>






