# Metaprogramming and Introspection

!!! warning
    This page is under construction.

MindScript also exposes program structure itself as data. *Metaprogramming* means treating code as a value that you can parse, validate, transform, format, and evaluate. Introspection means observing or manipulating metadata attached to values (notably annotations) and inspecting runtime structure.

This chapter focuses on the practical tooling layer that MindScript ships with. The goal is not to turn every script into a compiler, but to make common tasks—formatting, light linting, safe evaluation of small snippets, reproducible code generation, and data-to-code roundtrips—straightforward and reliable.

## Code as data: the runtime-S representation

MindScript exposes a canonical representation of programs as ordinary values. This representation is called runtime-S. A runtime-S node is an array whose first element is a string “tag” identifying the node kind, followed by child nodes and scalar fields. Because it is made from arrays, strings, numbers, booleans, and objects, you can store it in files, send it over the network, and manipulate it with the same tools you use for JSON.

A key design choice is that runtime-S is sugar-free. Many surface syntactic forms (such as property access, indexing, or operator precedence) are lowered into a small set of canonical node tags. That makes transformation simpler: you don’t need to preserve trivia like parentheses or spacing in order to produce correct output. The formatter can recover readable source from the canonical form.

Runtime-S also preserves annotations. In source code, `# ...` attaches to values. In runtime-S, annotations are represented explicitly as an `annot` wrapper node. This matters for tooling, because documentation and “failure reasons” are part of the data model and should survive roundtrips.

## Parsing source into runtime-S

The entry point for metaprogramming is `astParse`, which turns a source string into a runtime-S AST.

```mindscript
let src = "let x = 1 + 2\nx"
let ast = astParse(src)

if ast == null then
    println("parse failed: " + (noteGet(ast) or "<no details>"))
    null
else
    ast
end
```

`astParse` returns `null` on parse failure, usually annotated with a readable error message. This is intentionally a “soft” boundary, because tooling often wants to report problems rather than stop the whole process.

Even when parsing succeeds, you should not assume the AST has a valid canonical shape. A runtime-S value may have been constructed by hand, produced by a buggy tool, or received from an untrusted source. That is why MindScript separates parsing from validation.

## Validating runtime-S before you trust it

Canonical validation checks that a runtime-S tree is structurally well-formed: tags are known, arities are correct, and node layouts match what the evaluator and formatter expect. The validator reports problems as an array of error objects rather than panicking, which makes it suitable for editors, CI checks, and pipelines.

```mindscript
let errors = astValidate(ast)

if len(errors) != 0 then
    println("AST is invalid.")
    println(errors)
    null
else
    println("AST is valid.")
    ast
end
```

Each error contains at least a message and a path describing where the problem occurred. When the runtime-S value is not even shaped like an AST (for example, it is not an array whose first element is a tag string), the validator still returns a standardized error entry instead of crashing. This “always returns errors as data” behavior is a deliberate choice: tooling needs totality and predictable failure modes.

Validation is a structural guarantee, not a semantic guarantee. A structurally valid program can still be “bad” at runtime (for example, it can index the wrong kind of value, call a missing command, or attempt I/O). Validation answers “is this a well-formed program?”, not “is this a safe program?” or “will it succeed?”.

## Formatting runtime-S back to source

Once you have runtime-S, `astFormat` can produce canonical, formatted MindScript source. This is useful when you are building programmatic refactors, code generators, or format-preserving pipelines.

```mindscript
let formatted = astFormat(ast)

if formatted == null then
    println("format failed: " + (noteGet(formatted) or "<no details>"))
    null
else
    formatted
end
```

Formatting is defined on the canonical form, which is why validation is the right step before formatting. If you skip validation and feed malformed runtime-S into the formatter, you should expect a `null` result with an explanation rather than a hard crash.

A good way to think about this pipeline is: `astParse` gives you structure, `astValidate` gives you confidence, and `astFormat` gives you stable output.

## Evaluating runtime-S: `astEval` versus `reify`

MindScript provides two evaluation entry points for runtime-S. They exist because metaprogramming is used in two very different modes.

The first mode is “evaluate something in the current scope”, which is what you want for REPL helpers, small expression evaluators, and code that should not leak definitions into the global environment. That is what `astEval` does.

```mindscript
let evalSnippet = fun(src: Str) -> Any do
    let ast = astParse(src)
    if ast == null then
        return ast
    end

    let errs = astValidate(ast)
    if len(errs) != 0 then
        return (null  # <invalid AST>)
    end

    astEval(ast)
end

evalSnippet("1 + 2")
```

`astEval` decodes runtime-S, validates it, and then evaluates it in the caller’s scope. If decoding or validation fails, it returns `null` with an explanation. If evaluation fails due to a runtime error, it returns `null` with an explanation rather than terminating the process. This “tooling-friendly” behavior is intentional: metaprogramming often wants errors as values.

The second mode is “turn code into persistent definitions”, which is what you want for generators that create helpers, synthesize modules, or build a small DSL on top of MindScript. That is what `reify` does.

```mindscript
let defineHelper = fun() -> Any do
    let ast = astParse('let add1 = fun(x: Int) -> Int do x + 1 end')
    if ast == null then
        return ast
    end

    let errs = astValidate(ast)
    if len(errs) != 0 then
        return (null  # <invalid AST>)
    end

    reify(ast)
end

defineHelper()
add1(41)   # now exists persistently in the current session
```

The semantic difference is persistence: `reify` evaluates in the persistent environment, so definitions survive beyond the call (and, in the REPL, beyond the current prompt). This makes `reify` a powerful mechanism, and it is also why you should treat it with care. Reifying untrusted code is equivalent to executing untrusted code.

If you need “evaluate, but don’t permanently define”, use `astEval`. If you need “evaluate as if it were typed into the REPL and keep the result”, use `reify`.

## Reflecting values into constructor code

Metaprogramming is not only about transforming source code; it is also about turning data back into code. MindScript exposes a `reflect` function that produces runtime-S constructor code for a value. In other words, `reflect(v)` returns an AST that, when evaluated, reconstructs `v`.

This is useful in two common situations. The first is serialization for reproducibility: instead of writing ad-hoc JSON for a complex value, you can emit MindScript code that re-creates the value precisely (including annotations where applicable). The second is snapshot-style debugging: you can capture a value as code and paste it into a test or a REPL session.

```mindscript
let v = {
    # human-facing label
    name: "Ada",
    tags: ["math", "programming"]
}

let ast = reflect(v)
if ast == null then
    println("cannot reflect: " + (noteGet(ast) or "<no details>"))
    null
else
    let src = astFormat(ast)
    if src == null then
        println("cannot format reflected AST: " + (noteGet(src) or "<no details>"))
        null
    else
        src
    end
end
```

Not every value is reflectable. Pure data values (null/bools/numbers/strings/arrays/objects) are reflectable. Types are reflectable as type constructors. MindScript functions and oracles can be reflected as code when they are defined in MindScript. Opaque runtime objects such as handles generally cannot be reflected, because they represent external resources (open files, sockets, processes) that have no meaningful source-level constructor.

When reflection cannot represent a value, it returns `null` with a reason. This is a correctness property: pretending a handle is reconstructible would produce misleading code.

## A practical refactoring example: normalize “free-floating” annotations

Because annotations are explicit nodes in runtime-S, you can write tools that enforce style. As a simple example, suppose you want to ensure that “section header” comment blocks do not accidentally attach to a following definition. In source, a blank line prevents attachment, but in AST form you can detect standalone annotations reliably.

The following script parses a file, validates it, formats it, and writes it back. It does not implement a sophisticated transformation; instead it demonstrates the canonical tool pipeline you should follow whenever you manipulate code.

```mindscript
let rewriteFile = fun(path: Str) -> Bool? do
    let src = readFile(path)
    if src == null then
        return (null  # <cannot read file>)
    end

    let ast = astParse(src)
    if ast == null then
        return (null  # <parse failed>)
    end

    let errs = astValidate(ast)
    if len(errs) != 0 then
        write(STDERR, "invalid AST in " + path + "\n")
        write(STDERR, formatValue(errs) + "\n")
        flush(STDERR)
        return (null  # <invalid AST>)
    end

    let out = astFormat(ast)
    if out == null then
        return (null  # <format failed>)
    end

    let n = writeFile(path, out)
    if n == null then
        return (null  # <cannot write file>)
    end
    true
end
```

This pattern matters more than it looks. The parse step gives you structure, the validate step gives you a contract that the rest of your tooling can rely on, and the format step produces canonical output that is stable across machines and editor settings. Once you build transformation passes that rewrite `ast` (for example, renaming identifiers or inserting timing calls), this outer scaffolding stays the same.

## A safe “expression evaluator” for configuration

A common temptation is to accept “little snippets of code” as configuration. If you do that, you need a predictable boundary: parsing and evaluation failures should be data, not a process crash; code should not accidentally define names permanently; and you should be able to control what context the code sees.

A pragmatic approach is to evaluate in a fresh scope by calling a helper function, and to wrap evaluation in `try` so that hard failures become values.

```mindscript
let evalExpr = fun(expr: Str, env: {}) -> Any do
    let ast = astParse(expr)
    if ast == null then
        return ast
    end

    let errs = astValidate(ast)
    if len(errs) != 0 then
        return (null  # <invalid AST>)
    end

    let r = try(fun() do
        do
            for kv in env do
                let k = kv[0]
                let v = kv[1]
                # store values in locals for the expression to use
                # (this is a simple approach; serious sandboxes should not run untrusted code)
                (let tmp = 0)  # placeholder expression to keep structure obvious
            end
            astEval(ast)
        end
    end)

    if r.ok then
        r.value
    else
        null  # <eval failed>
    end
end
```

This example is intentionally conservative about promises: it is not a sandbox. MindScript does not try to make untrusted code safe to execute. Validation ensures structural correctness, not safety. If you execute arbitrary code, it can call `open`, `exec`, `http`, and other boundary functions. The right security boundary for untrusted input is “do not evaluate it”.

What metaprogramming gives you is control over *tooling failure modes*: you can accept or reject code based on structural rules, present good diagnostics, and keep evaluation from polluting persistent scope.

## Programmatic annotations as a documentation channel

Annotations are first-class metadata. In tooling, that is valuable because it lets you attach explanations to intermediate values without changing their “shape”. For example, a linter can return `null` on failure and annotate it with the reason, or it can annotate a successful value with a short “how it was computed” note.

At the value level, `noteGet` and `noteSet` let you build this channel explicitly. When you combine this with `reflect`, you get a reproducible audit trail: a value can carry its own documentation, and `reflect` can emit code that preserves that documentation.

## Summary

Metaprogramming in MindScript is built on a small, explicit contract. Code can be represented as runtime-S, which is just JSON-shaped data. You can parse source into runtime-S, validate it structurally, format it back into canonical source, and evaluate it either in the current scope (`astEval`) or persistently (`reify`). You can also reflect values into constructor code, which supports reproducibility and tooling workflows.

The power here is not cleverness; it is predictability. Every step has a total interface, and every failure can be represented as a value with an explanation. That makes it practical to build formatters, linters, small code generators, and debugging tools directly in MindScript, using the same error-handling idioms you already use at other boundaries.
