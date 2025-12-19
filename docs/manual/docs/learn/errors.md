STUB# Errors and Inspection

MindScript treats errors as **values**.

The idiom is simple:

* When something fails in a normal, expected way (bad input, missing data, invalid JSON, network failure, “can’t parse”), the operation usually produces **`null`**.
* When something fails, it is common for that `null` to carry a short explanation as an **annotation** (a note attached to the value).
* When something truly cannot continue (a fatal invariant violation, or a misuse of a built-in’s contract), the program **panics** and stops.

This chapter shows how to write MindScript that:

* returns useful `null` values on failure,
* inspects failure reasons,
* and uses panic only when you genuinely want to stop execution.

> Important type note: `Any` is the supertype, so it includes `Null`. In practice, **`Any` is nullable**. If you want “not null”, use a narrower type than `Any` and check it.

---

## The basic shape: returning `null` (with a reason)

Here is the most common pattern in MindScript: validate input, and return `null` on failure.

```mindscript
let parsePort = fun(s: Str) -> Int? do
    let n = int(s)
    if n == null then
        null  # <invalid port>
    elif n < 1 or n > 65535 then
        null  # <port out of range>
    else
        n
    end
end
```

A few things are happening:

* The return type is `Int?`, meaning “an integer or `null`”.
* The function returns `null` in multiple places.
* The `# <...>` text attaches to the `null` value. That note becomes your error message.

### Reading the reason later

You can retrieve the note from the returned value:

```mindscript
let p = parsePort("eighty")
if p == null then
    println("port error: " + (noteGet(p) or "<no details>"))
    null
else
    p
end
```

Why the `(noteGet(p) or "<no details>")`?

* `noteGet(x)` returns `Str?`.
* If there is no note, it returns `null`.
* `+` only concatenates strings with strings, so you want a real `Str` in both cases.

---

## `error(...)` is the standard “return null with a message”

The standard library defines:

```mindscript
error(msg: Str) -> Null
```

It returns `null` annotated with `msg`. It’s a convenience for “return a failure value”.

```mindscript
let requireKey = fun(obj: {}, key: Str) -> Any? do
    if not mapHas(obj, key) then
        error("<missing key: " + key + ">")
    else
        obj[key]
    end
end
```

This is a good fit when:

* you want a one-line failure value,
* and you expect callers to check `== null` and then inspect `noteGet(...)`.

---

## Checking results without making the code noisy

A common beginner mistake is to forget that an operation can return `null`, and then immediately use the result as if it were a non-null value.

Example: parsing JSON.

```mindscript
let x = jsonParse("{ not json }")
# x is null on failure
```

If you immediately index it:

```mindscript
let y = x["field"]  # this will panic (x isn't a map)
```

…you’ve turned a normal “invalid JSON” situation into a fatal stop.

Instead, branch right away:

```mindscript
let x = jsonParse("{ not json }")
if x == null then
    println("bad input: " + (noteGet(x) or "<no details>"))
    null
else
    x
end
```

If you are doing this often, wrap it in a helper:

```mindscript
let require = fun(v: Any, msg: Str) -> Any do
    if v == null then
        error(msg)
    else
        v
    end
end
```

Remember the type note: `Any` includes `null`, so `require` is not “making it non-null at the type level”; it’s making the check explicit and giving you a message at the failure site.

---

## Panics: fatal stops

A **panic** stops evaluation and prints a diagnostic. It is used for:

* contract violations (calling a built-in with the wrong kinds of values),
* invariants (“this should never happen”),
* uncaught fatal failures.

You can trigger one explicitly:

```mindscript
panic("this should never happen")
```

You’ll also see panics from common mistakes, such as:

* indexing a non-array / non-map,
* using an unknown property on an object/module,
* passing the wrong type into a built-in that requires a specific type,
* using a non-`Bool` in an `if`/`while` condition.

### The key guideline

Use `null` (with a note) for *expected* failure paths.

Use `panic(...)` only when continuing would be wrong.

---

## Catching panics with `try(...)`

Sometimes you want to treat a panic as “just another result” so you can keep going or report a nicer message. The standard tool is:

```mindscript
try(f: (Null -> Any)) -> { ok: Bool, value: Any }
```

You pass a zero-argument function (a common MindScript idiom is “callable with a `Null` parameter”), and `try` returns:

* `{ ok: true, value: <result> }` when it ran successfully
* `{ ok: false, value: <error value> }` when it panicked

Example:

```mindscript
let r = try(fun() do
    # This might panic if the input isn't an object.
    let x = jsonParse("{ not json }")
    x["user"]
end)

if r.ok then
    println("user: " + str(r.value))
    r.value
else
    println("failed: " + str(r.value))
    null
end
```

Notes:

* The `value` on failure is commonly `null` with a readable message attached.
* `str(...)` is useful here because it produces a printable representation for common data values.

---

## Inspecting values while debugging

When you’re unsure what you have, print it.

Recommended tools:

* `println(x)` prints a readable representation and returns `x` (handy inside expressions).
* `formatValue(x)` produces a stable, formatted string representation.
* `noteGet(x)` shows the annotation message, if any.

Example: instrumenting a pipeline:

```mindscript
let x = jsonParse(input)
println(x)

if x == null then
    println("parse failed: " + (noteGet(x) or "<no details>"))
    null
else
    # continue with x
    x
end
```

---

## Making your own “error contracts” explicit

MindScript code becomes easier to use when functions clearly document:

* what success looks like,
* what failure looks like (`null`),
* and what the note means.

A common style is to use short, bracketed slugs:

```mindscript
let loadConfig = fun(path: Str) -> {}? do
    let s = readFile(path)
    if s == null then
        null  # <cannot read config>
    else
        let cfg = jsonParse(s)
        if cfg == null then
            null  # <config is not valid JSON>
        else
            cfg
        end
    end
end
```

Callers don’t need to know which sub-step failed; they just need a stable message to display or log.

---

## Reading diagnostics when a panic happens

When MindScript stops due to a panic (or a parse error), it prints a diagnostic that points at a source location. In practice you use it like this:

1. Look at the **file name** and the **line/column**.
2. Look at the **caret (`^`)**: that’s the expression that triggered the panic.
3. Fix the immediate contract violation first (wrong type, missing key, bad property), then rerun.

In the REPL, incomplete input is handled by continuing the prompt until the parser considers the expression complete. If you see the continuation prompt, you haven’t finished a block or expression yet.

---

## Summary patterns

Use these as defaults:

* For expected failure: `null  # <reason>` (or `error("<reason>")`).
* Immediately check results that can be `null` before indexing, calling, or assuming shape.
* Use `noteGet(x)` to retrieve the reason from a `null` value.
* Use `try(fun() do ... end)` when you need to handle a panic as data.
* Use `panic(...)` only for truly fatal situations.
