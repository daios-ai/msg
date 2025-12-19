# Testing

MindScript scripts often start life as one-off utilities. The moment a script becomes part of a data pipeline, a CI job, or a production integration, you need a way to prevent “small refactors” from silently changing behavior. MindScript’s testing story is designed around that reality: tests are ordinary MindScript code, they run under the same runtime type checks as everything else, and failures produce readable diagnostics that point back to the source location.

The `msg` runtime ships with a test runner (`msg test`) and a standard module named `testing`. The `testing` module provides a registry for tests and a small set of assertion helpers. The runner discovers test files, loads them, executes the registered tests, prints a compact report, and exits non-zero when any test fails. That last property is the whole point: it makes tests composable with CI and shell automation.

This chapter shows how to write tests that are readable, deterministic, and useful when they fail, including strategies for code that calls oracles.

---

## The basic model: tests are registered functions

A MindScript test is a function of type `Null -> Any`. You register tests by importing `testing` and calling `testing.test(name, fn)`. The runner will load your test files and execute these registered functions.

Create a file named `math_test.ms`:

```mindscript
let testing = import("testing")

let add = fun(a: Int, b: Int) -> Int do
    a + b
end

testing.test("add adds integers", fun() do
    testing.assertEq(5, add(2, 3))
end)
```

Run tests from the current directory:

```bash
msg test
```

The runner discovers files whose names end in `_test.ms` under the path you pass to `msg test` (default is the current directory). Each discovered test file is loaded, it registers tests with `testing.test(...)`, and then the runner executes the registry.

This “register then run” design is intentional. It keeps tests declarative and makes it easy for the runner to support different execution strategies later (sequential, parallel, with per-test timeouts). It also keeps the model close to what many engineers already know from Go, Jest/Mocha-style registration, and Python’s collection mechanisms.

---

## Assertions and how failures behave

Assertions are the workhorse of tests. MindScript’s `testing` module follows a simple rule: an assertion either returns normally (the test continues), or it panics with a readable message (the test fails). The runner catches these panics, reports them as failures, and continues running other tests unless the process itself is fatally compromised.

Here is the most common assertion pair:

```mindscript
testing.assert(ok: Bool, msg: Str?)
testing.assertEq(want: Any, got: Any)
```

Use `assertEq` for structural equality across numbers, strings, arrays, and objects:

```mindscript
let testing = import("testing")

testing.test("deep equality works for JSON-shaped data", fun() do
    let got = {a: 1, b: [2, 3], c: {d: "x"}}
    let want = {a: 1, b: [2, 3], c: {d: "x"}}
    testing.assertEq(want, got)
end)
```

When `assertEq` fails, it prints both values using a stable representation, which matters because you want diffs to be meaningful and reproducible. This is especially valuable for nested objects where “expected vs actual” needs to be obvious at a glance.

MindScript also supports type-oriented assertions:

```mindscript
testing.assertType(v: Any, T: Type)
```

This is most useful at boundaries, such as JSON parsing and oracle outputs, where “shape” is the core contract:

```mindscript
let testing = import("testing")

let User = type { id!: Str, name!: Str, email: Str? }

testing.test("parsed user conforms to schema", fun() do
    let x = jsonParse("{\"id\":\"u1\",\"name\":\"Ada\"}")
    testing.assert(x != null, "jsonParse failed: " + (noteGet(x) or "<no details>"))
    testing.assertType(x, User)
end)
```

If you need to verify that code fails (panics) when misused, use `assertThrows`. It runs a function and expects a panic; optionally it checks that the first line of the error contains a substring.

```mindscript
let testing = import("testing")

testing.test("indexing a non-object panics", fun() do
    testing.assertThrows(fun() do
        let x = 123
        x["field"]
    end, "index")
end)
```

This style is preferable to “catching errors yourself” because it tests the observable behavior of the runtime contract. In MindScript, contract violations should be loud, and `assertThrows` lets you encode that expectation precisely.

---

## Table-driven tests with `cases`

Many functions are best tested by running the same logic over a list of inputs. The `testing.cases(name, xs, fn)` helper is designed for this. It generates a family of tests named `name/0`, `name/1`, and so on, which makes failures easy to locate without manually naming each case.

Here is a realistic example: a port parser that returns `Int?` and annotates failure reasons.

```mindscript
let testing = import("testing")

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

testing.cases("parsePort", [
    ["80", 80, null],
    ["0", null, "<port out of range>"],
    ["70000", null, "<port out of range>"],
    ["eighty", null, "<invalid port>"],
], fun(case) do
    let input = case[0]
    let want = case[1]
    let wantNote = case[2]

    let got = parsePort(input)

    testing.assertEq(want, got)

    if want == null then
        testing.assertEq(wantNote, noteGet(got))
    end
end)
```

This test does two things that are worth calling out. It verifies the returned value and it verifies the error message contract (the annotation on `null`). In MindScript, annotated `null` is an API surface. If you intend callers to branch on reasons, that reason should be stable enough to test.

---

## Snapshot tests for large structured output

When the output is large—pretty-printed objects, normalized JSON, formatted code, generated schemas—asserting equality inline is unpleasant and fragile. Snapshot tests solve this by storing an “expected output” in a file and comparing the current result to that stored value.

The `testing` module provides:

```mindscript
snapshot(path: Str, value: Any, update?: Bool)
snapshotUpdate(path: Str, value: Any)
```

A snapshot test reads the snapshot file, compares it to a stable representation of `value`, and fails if they differ. The stable representation is designed to be deterministic and diff-friendly.

Here is a snapshot test for a normalization function:

```mindscript
let testing = import("testing")

let normalizeUser = fun(u: {}) -> {} do
    if mapHas(u, "name") and u.name != null then if true then
        u.name = strip(toLower(u.name))
    end end
    u
end

testing.test("normalizeUser snapshot", fun() do
    let input = {name: "  ADA  ", extra: [1, 2, 3]}
    let out = normalizeUser(clone(input))

    testing.snapshot("testdata/normalizeUser.snap", out)
end)
```

The snapshot file lives alongside the test data (`testdata/` is a common convention because it keeps fixtures organized and makes repository layout predictable).

When you intentionally change behavior, update the snapshot. One disciplined workflow is to keep snapshot updates explicit so you never accidentally accept changes:

```mindscript
let testing = import("testing")

testing.test("normalizeUser snapshot update (manual)", fun() do
    let out = normalizeUser({name: "  ADA  ", extra: [1, 2, 3]})
    testing.snapshotUpdate("testdata/normalizeUser.snap", out)
end)
```

You would run this test locally only when you want to refresh the golden file, then revert the update test or keep it commented out, depending on your project style. The important idea is that snapshot updates should be deliberate, because snapshots encode behavior.

---

## Running tests with `msg test`

The test runner is invoked through `msg test`. With no arguments it runs tests under the current directory. If you pass a path, it uses that path as a prefix for discovery.

```bash
msg test
msg test .
msg test src
```

The runner prints progress and a summary. When any test fails, `msg test` exits with a non-zero status code so CI can fail fast.

Two flags change how execution happens.

Verbose mode (`-v`) prints each test name and its result, which is useful when diagnosing hangs or ordering-sensitive issues:

```bash
msg test . -v
```

Parallel mode (`-p`) executes tests concurrently:

```bash
msg test . -p
```

Parallel mode is valuable when tests involve I/O, network calls (preferably mocked or stubbed), or oracle calls that spend time waiting. It also raises the standard concurrency question: are your tests isolated? If two tests mutate global state, write the same snapshot file, or share a resource without coordination, parallel execution can produce flaky outcomes. The simplest discipline is to make each test self-contained and avoid shared mutable state. When that is not possible, keep those tests sequential or isolate state per test.

The timeout flag (`-t`) sets a per-test timeout in milliseconds:

```bash
msg test . -t 5000
```

A timeout is not a correctness mechanism; it is a guardrail. It protects you against tests that hang due to deadlocks, unbounded retries, or a stalled external dependency. When a test times out, the runner reports a timeout failure and continues with the rest.

---

## Determinism, randomness, and reproducibility

Good tests are reproducible. MindScript makes deterministic code easy, but scripts often involve randomness or time. The testing module provides `testing.seed(n)` to seed randomness used by test helpers (and you can also use `seedRand` from the standard library). If you use randomness in tests, seed it at the beginning of the test so failures can be replayed.

```mindscript
let testing = import("testing")

testing.test("randomized input is reproducible with a seed", fun() do
    testing.seed(12345)

    let xs = []
    for _ in range(0, 5) do
        push(xs, randInt(10))
    end

    testing.assertEq([6, 3, 3, 0, 5], xs)
end)
```

The exact sequence depends on the RNG implementation, so snapshotting or hardcoding sequences is only appropriate if you intend to lock it down. A more robust pattern is to test invariants (“all values are between 0 and 9”, “no duplicates after dedupe”, “sorted output is nondecreasing”) while still seeding for repeatability when debugging.

---

## Testing code that uses oracles

Oracles introduce nondeterminism. A testing strategy must either control that nondeterminism or avoid it.

The first line of defense is schema discipline. If an oracle returns a narrow structured type, then a large portion of “randomness” becomes irrelevant because the output must conform to a shape. Tests can then assert type conformance and validate deterministic post-processing:

```mindscript
let testing = import("testing")

let Label = type { label!: Str, confidence: Num? }

# Classify a short message.
let classify = oracle(text: Str) -> Label

let normalizeLabel = fun(x: Label) -> Label do
    x.label = strip(toLower(x.label))
    x
end

testing.test("oracle output conforms and normalization is deterministic", fun() do
    let r = classify("Shipping was delayed by weather.")
    testing.assert(r != null, "oracle failed: " + (noteGet(r) or "<no details>"))
    testing.assertType(r, Label)

    let n = normalizeLabel(r)
    testing.assert(n.label == toLower(n.label), "label was not normalized")
end)
```

When you need full determinism, treat the oracle as an interface and stub it. Since oracles are values, you can assign a non-oracle function with the same call shape in tests and exercise the rest of your pipeline deterministically:

```mindscript
let testing = import("testing")

let Label = type { label!: Str, confidence: Num? }

let normalizeLabel = fun(x: Label) -> Label do
    x.label = strip(toLower(x.label))
    x
end

testing.test("pipeline can be tested with a stubbed oracle", fun() do
    let classify = fun(text: Str) -> Label do
        {label: "Delay", confidence: 0.9}
    end

    let out = normalizeLabel(classify("anything"))
    testing.assertEq("delay", out.label)
end)
```

This approach is technically justified because it tests the deterministic part of your program—your control flow, data transformations, and boundary checks—without relying on an external service. It also keeps your test suite fast and stable.

If you do want end-to-end oracle tests, keep them clearly separated (for example in a different directory or with naming conventions) and expect them to be slower and potentially flaky. In CI, you can gate them on environment availability, but the core project health should not depend on a third-party API being reachable.

---

## Summary

Testing in MindScript is built around the idea that tests are ordinary functions registered in `_test.ms` files and executed by `msg test`. Assertions fail by panicking; the runner catches those failures and reports them with readable diagnostics. `cases` makes table-driven testing ergonomic, snapshots make large structured outputs testable, and timeouts/parallel execution make large suites practical. For oracle-based code, schema constraints and deterministic post-processing reduce nondeterminism, and stubbing is the reliable way to keep tests reproducible.
