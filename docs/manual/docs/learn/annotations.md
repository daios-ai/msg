<style>
  .my-code span.prompt { color: black; }
  .my-code span.note { color: green; }
  .my-code span.value { color: blue; }
</style>

# Annotations

MindScript uses `#` to write **annotations**.

An annotation is text that the language preserves and attaches to what follows. Annotations are used for:

* documentation that travels with data,
* carrying a failure reason on a returned `null`,
* and (later) providing instructions to oracles.

Most of the time, an annotation attaches to a **value**. But you can also write comment-only lines that act like **true comments**: they are kept as standalone notes in the source without attaching to any value.

---

## The attachment rule

MindScript decides whether a `# ...` attaches based on what comes next.

### 1) Attached annotations

A `# ...` annotation attaches to the value produced by the **next expression**.

<div class="my-code" markdown="0">
<pre><code><span class="prompt">==> # The speed of light in meters per second.</span>
<span class="prompt">... let c = 299792458</span>
<span class="note">The speed of light in meters per second.</span>
<span class="value">299792458</span>
</code></pre>
</div>

This is the key mental model: annotations attach to **values**, not to variable names or to lines.

A more explicit example:

```mindscript
# This annotation attaches to the value 1.
let x = 1

# This annotation attaches to the value (x + 1).
x + 1
```

### 2) Free-floating comments

If an annotation line is separated from the next expression by a **blank line**, it does **not** attach to anything. It acts as a free-floating comment.

```mindscript
# This is a standalone comment.
# It is not attached to any value.

let x = 1
```

This lets you write ordinary comments for sectioning and narration without accidentally annotating the next value.

### 3) Multi-line blocks

Consecutive lines whose first non-space character is `#` form a single multi-line annotation block. Whether that block attaches depends on the same rule as above:

* If the block is immediately followed by an expression, it attaches to that expression’s value.
* If there is a blank line after the block, it is free-floating.

Attached block:

```mindscript
# Processes raw sales records:
#   1. Filters out returns.
#   2. Groups by region.
#   3. Sums revenue.
let process = fun(records: [Any]) -> {} do
    ...
end
```

Free-floating block:

```mindscript
# --- Data cleaning helpers ---
# This section is about normalization.

let normalize = fun(s: Str) -> Str do
    strip(toLower(s))
end
```

---

## Annotations are metadata

Annotations do not change computation. They do not affect equality, arithmetic, indexing, or control flow.

```mindscript
# The golden ratio.
let phi = (1 + sqrt(5)) / 2

phi == phi          # true
2 * phi             # a number
noteGet(2 * phi)    # usually null (annotations do not propagate automatically)
```

Annotations are not automatically “carried through” derived values. You attach notes where you want them.

---

## Reading and writing annotations

You can access annotations at runtime:

* `noteGet(x) -> Str?` returns the annotation text, or `null` if none.
* `noteSet(text: Str, value: Any) -> Any` attaches an annotation and returns the value.

```mindscript
let c = 299792458
noteGet(c)  # null

noteSet("The speed of light in meters per second.", c)
noteGet(c)  # "The speed of light in meters per second."
```

`noteSet` updates the annotation on the value you pass in.

---

## Annotating `null` to explain failure

Many functions in the standard library return `null` to signal failure, and attach a reason as an annotation. You can do the same.

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

Then the caller can inspect the reason:

```mindscript
let p = parsePort("eighty")
if p == null then
    println(noteGet(p) or "<no details>")
end
```

A related helper from the prelude is:

```mindscript
error(msg: Str) -> Null
```

It returns `null` annotated with `msg`, which is convenient when you want a one-line failure result.

---

## Annotations inside objects and arrays

Annotations can attach to values inside composites, which is useful for documenting individual fields.

```mindscript
let user = {
    # Unique identifier from the upstream system.
    id: "u_123",

    # Display name shown in the UI.
    name: "Ada"
}

noteGet(user.id)
noteGet(user.name)
```

And inside arrays:

```mindscript
let limits = [
    # Minimum supported value.
    1,

    # Maximum supported value.
    10
]

noteGet(limits[0])
noteGet(limits[1])
```

---

## Annotating returned values explicitly

Sometimes you want to return a `null` with a note from inside a function. Write the annotation on the value you are returning:

```mindscript
let findUser = fun(id: Str) -> {}? do
    let u = { }  # pretend lookup
    if u == null then
        return (null  # <user not found>)
    end
    u
end
```

This style makes it obvious that the note belongs to the returned value.

---

## Common surprises

### Blank lines control whether a comment attaches

If you intend a comment to be free-floating, put a blank line after it.

If you intend it to annotate the next value, do not put a blank line in between.

This is the most important practical rule to internalize.

### Dot-chains cannot be interrupted

Property access must be contiguous (`obj.name`). You cannot insert an annotation between `obj` and `.name`. Annotate the full expression instead:

```mindscript
# User's display name.
let n = obj.name
```

### Calls and indexing are whitespace-sensitive

Calls and indexing require no space before `(` or `[`.

* `f(x)` is a call; `f (x)` is not.
* `arr[i]` is indexing; `arr [i]` is not.

This matters because it changes what the parser considers “the next expression”, which in turn changes what an annotation attaches to. When in doubt, annotate a whole line (or use parentheses) to make intent clear.

---

## Recommended style

* Use line-leading `# ...` blocks for documentation.
* Use short, stable tags when annotating `null` for failure (`<invalid input>`, `<missing key>`, …).
* Use a blank line after a comment block when you mean it as narration rather than as an annotation.

---

## Summary

* `# ...` attaches to the value of the **next expression** if there is no blank line separating them.
* A `# ...` block followed by a blank line is a standalone comment.
* Annotations are metadata: they do not change evaluation.
* Use `noteGet` / `noteSet` to work with annotations programmatically.
* Annotated `null` is the standard way to explain failure results.
