# Annotations

In the previous chapters, we've been using annotations (`# ...`) as if they were comments.

Most programming languages let you add comments to explain your code. Comments help humans understand intent, but they typically do not affect how the program runs. For example, in many languages you'd write comments like this:

```mindscript
# Computes the first n numbers in the Fibonacci sequence.
let fibonacci = fun(n: Int) -> [Int] do
    ...
    SOME IMPLEMENTATION
    ...
end
```

If the comment and signature are clear, a reader can understand what fibonacci is for without reading the body.

MindScript takes this idea one step further. For some computations, we want to treat the implementation as a black box (an oracle) and rely on surrounding intent and type constraints:

```mindscript
# Summarize the text as 3 bullet points.
let summarize = oracle(text: Str) -> [Str]
```

To support this, MindScript elevates commenting to a first-class language feature called **annotations**. Annotations are preserved by the language and attached to values, so they can be inspected at runtime.

MindScript uses `#` as an annotation marker. It takes the following text and attaches it to a value. It is used for:

* documentation that travels with data,
* carrying a failure reason on a returned `null`,
* and providing instructions to [oracles](/learn/oracles).

Annotations are metadata: they do not change evaluation (they don’t affect equality, arithmetic, indexing, or control flow). It’s fine to read annotations for diagnostics, but deterministic program logic should rely on ordinary values and types rather than annotation text.

---

## The attachment rule

All annotations start with a `#` followed by a text up to the end of the line, and then attach to the "nearest" value. The rule for attachment is:

**Trailing-annotations**: If a *single-line* annotation is on the right of an expression, it attaches to the value of that expression. For instance, in

```mindscript
let x = 1  # This attaches to the value 1.
let y = -1 # This attaches to the value -1.
```

**Pre-annotation**: Otherwise, it attaches to the value of the expression on the very next line. For instance,

```mindscript
# This attaches to the value 1.
let x = 1

# This attaches to the value (x + 1).
x + 1
```

While this simple attachment rule was designed trying to mimic what feels natural in a programming language, it has a few special cases worth discussing.

### Multi-line blocks

Consecutive lines whose first non-space character is `#` form a single multi-line annotation block. For instance,

```mindscript
# Processes raw sales records:
#   1. Filters out returns.
#   2. Groups by region.
#   3. Sums revenue.
let process = fun(records: [Any]) -> {} do
    ...
end
```

attaches the four-line annotation block to the function assigned to the `process` variable. Because a multi-line block fails the single line condition for trailing annotations they always attach to the next expression.


### Free-floating comments

If an annotation block is followed by a *blank line*, it does *not* attach to the next expression. Instead, it attaches to a special *no-op* expression represented by the blank line, so it behaves like a free-floating comment.

```mindscript
# This is a standalone comment.
# It is not attached to any value.

let x = 1
```

This lets you write ordinary comments for sectioning and narration without accidentally annotating the next value.

!!! info
    Technically, a blank line is parsed as a no-op expression whose result is ignored. The formatter prints multiple blank lines as a single blank line because they reduce to the same no-op.


### Annotations inside objects and arrays

Annotations can attach to values inside composites, which is useful for documenting individual fields. For instance, we can document the fields of a specific user record:

```mindscript
let user = {
    # Unique identifier from the upstream system.
    id: "u_123",
    # Display name shown in the UI.
    name: "Ada"
}
```

The same works with annotations inside arrays:

```mindscript
let limits = [
    1, # Minimum supported value.
    10 # Maximum supported value.
]
```

Furthermore, we'll see later that annotations can also be used to annotate fields in type schemas.

!!! warning
    The formatter may move annotations when it rewrites code. It chooses a canonical placement on a case-to-case basis, so an annotation may end up in a different location than in the original source. In particular, single-line annotations are often formatted as trailing annotations when they fit on the same line.

---

## Annotations are metadata

As already mentioned, annotations are metadata attached to values and they do not affect computation, as illustrated in the next code:

```mindscript-repl
==> # The golden ratio.
... let phi = (1 + sqrt(5.)) / 2
1.618033988749895 # the golden ratio.

==> let phiCopy = phi
1.618033988749895 # the golden ratio.

==> phi == phi
true

==> 2 * phi
3.23606797749979
```

This also shows that annotations are not automatically "carried through" derived values; you have to attach them where you want them. Only assignment preserves annotations.

### Reading and writing annotations

You can read and write annotations at runtime:

* `noteGet(x) -> Str?` returns the annotation text, or `null` if none.
* `noteSet(text: Str, value: Any) -> Any` attaches an annotation and returns the value.

In the next example we programmatically set and retrieve an annotation:

```mindscript
==> let c = 299_792_458
299792458

==> noteSet("The speed of light in meters per second.", c)
299_792_458 # The speed of light in meters per second.

==> noteGet(c)
"The speed of light in meters per second."
```

These functions should be used with care under limited circumstances such as printing human-readable messages. Avoid using annotation content to drive logic.

---

## Summary

* Single-line `# ...` annotations on the r.h.s. of an expression attach to its value; otherwise they attach to the value of the next expression if there is no blank line separating them.
* A `# ...` block followed by a blank line is a standalone comment.
* Annotations are metadata: they do not change evaluation.
* Use `noteGet` / `noteSet` to work with annotations programmatically.
* Annotated `null` is the standard way to explain failure results.
