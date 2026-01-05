# Errors and Panics

MindScript treats errors as **values**. If a function doesn't succeed in performing its operation, for instance, because there was

* an invalid input;
* missing data;
* a network failure;
* failed authentication;

and so forth, then the convention is to signal this by returning `null` carrying a short explanation as an [annotation](/learn/annotations). Because this forces a nullable return type (e.g. `Str?`, `[Int]?`, `{...}?`, `Any`, etc.), functions that can fail can be spotted at a glance.

This design was deliberately chosen to avoid hidden control flow that can come with exception-based mechanisms in languages such as Java or Python. The drawback is that it requires programmers to check for errors upfront everytime a nullable function is called.

!!! warning
    A key rule: it’s okay to display the annotation text for humans in logs and UI, but avoid basing control flow on it, as annotations are metadata, and also, error messages might not be stable across different versions of the code. If you need to distinguish error kinds programmatically, return a formal value (for example an `Enum[...]` or a tagged object).

In addition, MindScript has **panics**. Panics happen when there's

* a syntax error;
* a missing module;
* a call with wrong argument types;
* a fatal invariant violation;
* or other runtime errors.

When MindScript encounters a panic, *it considers it fatal and halts execution*. As we'll see later, there is a recovery mechanism (the `try` function), but it should be used sparingly.


---

## Returning errors and checking boundaries

The basic pattern is: *validate, and return* `null` *(with a reason) on failure*.

```mindscript
let parsePort = fun(s: Str) -> Int? do
    let n = int(s)
    if n == null then
        return null  # invalid port
    end
    if n < 1 or n > 65535 then
        return null  # port out of range
    end
    n
end
```

The `# ...` text attaches to the returned `null`. It’s meant for humans (logs/UI). If you need machine-readable error kinds, return a formal value (enum/tagged object), not a message string.

The standard library defines `error(msg: Str) -> Null`, a helper function which returns `null` annotated with `msg`. It can provide a cosmetic touch to signal intent in the code:

```mindscript
let parsePort = fun(s: Str) -> Int? do
    let n = int(s)
    if n == null then
        return error("invalid port")
    end
    if n < 1 or n > 65535 then
        return error("port out of range")
    end
    n
end
```

Remember, as a rule of thumb, *check for `null` immediately after any `T?`-returning call* before indexing, property access, or passing it to code that expects a non-null shape. This is a common beginner mistake.

---

## Panics: fatal stops

A **panic** stops evaluation and prints a diagnostic. Panics are for “the program is wrong” situations: contract violations, missing required properties/modules, or invariants that should never be violated.

You can trigger a panic explicitly:

```mindscript
panic("this should never happen")
```

### Catching panics with `try(...)`

Sometimes you want to treat a panic as “just another result” so you can keep going or report a nicer message. The standard tool is:

```mindscript
try(f: (Null -> Any)) -> { ok: Bool, value: Any }
```

which is used to wrap a function that can panic. If the call succeeds, it returns `{ok: true, value: V}` where `V` is the return value; otherwise it return `{ok: false, value: null}`.

The following example illustrates how to use it for sanitizing a potentially unsafe field access:

```mindscript
let r = try(fun() do
    let x = jsonParse("{ not json }")
    x["user"]   # may panic
end)

if r.ok then
    r.value
else
    println("no 'user' field")
    null
end
```

### Legitimate uses of `panic(...)` and `try(...)`

While panics should be avoided in favor of errors, there are legitimate use cases:

* *internal invariants*: e.g. "this branch should be unreachable";
* *programmer errors*: your own config/code is invalid and you want to stop;
* *hard contract enforcement* in libraries where callers must not proceed.

Similarly, use `try` sparingly, typically for:

* *tooling code* that must keep running (formatters, linters, AST tooling);
* *wrapping a hard failure at a boundary* so you can return a clean `null`;
* *best-effort batch processing* where one bad record should be skipped rather than killing the whole run.
