
# `testing` — a tiny test helper for MindScript

The `testing` module is a small library for writing and running tests in MindScript. It does not try to be clever or feature-heavy. Instead, it gives you a few simple tools: a way to register tests, a handful of assertion helpers, a basic runner, and a couple of extras for table-driven tests, snapshots, and reproducible randomness.

You write ordinary MindScript functions to describe your tests. The library keeps track of them by name and runs them for you, either from your own script or from the `msg test` command-line tool.

---

## 1. First test, end to end

Suppose you have a function that adds two integers:

```mindscript
let add = fun(a: Int, b: Int) -> Int do
	a + b
end
```

Create a file next to it called `math_test.ms`:

```mindscript
let testing = import("testing")

testing.test("math/add/basic", fun(_: Null) do
	let got = add(2, 3)
	testing.assertEq(5, got)
end)
```

This file imports the `testing` module once and registers a single test. The call to `testing.test` takes a string name and a function with the shape `(Null) -> Any`. Inside that function you write the test body. If the body finishes without panicking, the test passes. If it panics (for example, because an assertion fails), the test fails.

To run tests for a project from the command line you normally use the `msg` tool:

```bash
$ msg test ./myproject -v
```

This discovers all files ending in `_test.ms` under `./myproject`, imports them, runs every registered test, and prints output such as:

```text
=== RUN   math/add/basic
--- PASS: math/add/basic (3ms)
ok: 1  fail: 0  total: 1  durationMs: 3
```

The `-v` flag turns on verbose mode, which prints a line for each test. You can also pass `-p` to run tests in parallel and `-t <ms>` to enforce a per-test timeout in milliseconds.

In most cases this is all you need: a few `_test.ms` files, some `testing.test` calls, and `msg test` on the command line.

If you want more control—for example, to integrate with another tool—you can also call the runner directly:

```mindscript
let testing = import("testing")

let summary = testing.run({
	pathPrefix: ".",   # where to look for *_test.ms files
	verbose: true,
	parallel: false,
	timeoutMs: 0
})

if summary.failed > 0 then
	exit(1)
end
```

This is roughly what `msg test` does internally.

---

## 2. Registering tests and basic assertions

A “test” in this library is just a function with a fixed shape. You register it under a string name and let the runner call it later.

The core registration function looks like this:

```mindscript
testing.test(name: Str, fn: Null -> Any) -> Bool
```

A common naming style is to write a path-like string such as `"math/add/basic"` or `"parser/empty-input"`. The library does not interpret the name; it just displays it in output and uses it to track where failures come from. Names must be unique. If you register the same name twice, `test` panics with a duplicate-name message.

Inside the test function you normally use one of the assertion helpers to check results.

### 2.1 Boolean and equality checks

The simplest assertion is `assert`:

```mindscript
testing.assert(ok: Bool, msg: Str?) -> Bool
```

It expects a boolean condition. If the condition is `true`, it returns `true` and the test continues. If it is `false`, it panics. When `msg` is `null` it uses a generic `"assertion failed"` message; otherwise it uses your message:

```mindscript
testing.test("parser/accepts-42", fun(_: Null) do
	let result = parse("42")
	let ok = result != null
	testing.assert(ok, "parse should succeed for '42'")
end)
```

For checking that a value matches an expected result, `assertEq` is usually more convenient:

```mindscript
testing.assertEq(want: Any, got: Any) -> Bool
```

It uses MindScript’s deep equality: numbers compare by value (`2 == 2.0` is true), arrays and maps compare by content, and map key order does not matter. If `want` and `got` are equal it returns `true`. If they differ it panics with a short message showing both values:

```mindscript
testing.test("math/add/returns-sum", fun(_: Null) do
	let got = add(10, 5)
	testing.assertEq(15, got)
end)
```

When this fails you see a message like:

```text
not equal:
want: 15
got:  14
```

### 2.2 Type checks and expected failures

Sometimes you want to check that a result fits a particular type rather than a single value. For that you can use `assertType`:

```mindscript
testing.assertType(v: Any, T: Type) -> Bool
```

A common pattern is to use `typeOf` on an example value:

```mindscript
testing.test("http/response-shape", fun(_: Null) do
	let res = fetch("/status")
	let Response = typeOf({code: 200, message: "OK"})
	testing.assertType(res, Response)
end)
```

If `res` does not conform to `Response`, `assertType` panics with a small message that shows the value and the type description.

There are also tests where you expect code to panic. Instead of wiring `try` yourself, you can use `assertThrows`:

```mindscript
testing.assertThrows(fn: Null -> Any, sub: Str?) -> Bool
```

It runs `fn(null)` and expects a panic. If `fn` succeeds, `assertThrows` panics itself. If `sub` is a string, it also checks that the first line of the panic message contains that string. When `sub` is `null`, any panic message is acceptable.

A small example:

```mindscript
testing.test("config/rejects-invalid-json", fun(_: Null) do
	testing.assertThrows(fun(_: Null) do
		loadConfig("not-json")
	end, "invalid JSON")
end)
```

Here the test passes only if `loadConfig("not-json")` panics and its message contains `"invalid JSON"`.

---

## 3. A few extras: table-driven tests, snapshots, and seeding

The core of the library is `test`, `assert`, and `assertEq`. On top of that there are a few helpers that solve common patterns without adding much complexity.

### 3.1 Table-driven tests with `cases`

When the same test logic should run on many inputs, you can collect them in an array and use `cases`:

```mindscript
testing.cases(name: Str, xs: [Any], fn: Any -> Any) -> Int
```

It registers one subtest per element of `xs`, named `name + "/" + index`. Each subtest calls `fn` with the corresponding element.

For example, to test `add` over several pairs:

```mindscript
let data = [
	{a: 1, b: 2, want: 3},
	{a: 2, b: 5, want: 7},
	{a: 10, b: 0, want: 10}
]

testing.cases("math/add/table", data, fun(c) do
	let got = add(c.a, c.b)
	testing.assertEq(c.want, got)
end)
```

When you run the suite, the tests appear as `math/add/table/0`, `math/add/table/1`, and so on, which makes it easy to see which case failed.

### 3.2 Snapshot tests with `snapshot` and `snapshotUpdate`

For generated output such as JSON or HTML, it can be convenient to compare against a “golden” file on disk. The snapshot helpers do this by rendering a value to a string and comparing it to a file.

The main function is:

```mindscript
testing.snapshot(path: Str, value: Any, update: Bool?) -> Bool
```

It converts `value` to text (preferring JSON when possible), reads the file at `path`, and compares. If the file does not exist, or if the contents differ, passing `update = true` causes it to write the new contents instead of failing.

In a test this might look like:

```mindscript
testing.test("report/output-matches-snapshot", fun(_: Null) do
	let report = generateReport()
	let ok = testing.snapshot("testdata/report.json", report, null)
	testing.assert(ok, null)
end)
```

On the first run you can pass `true` for `update` (or call `snapshotUpdate`) to create the file. Later runs can use `null` so that the test fails if the output changes unexpectedly.

`snapshotUpdate(path, value)` is just a small wrapper that always overwrites the file:

```mindscript
testing.snapshotUpdate(path: Str, value: Any) -> Bool
```

### 3.3 Reproducible randomness with `seed`

Tests that rely on random numbers are easier to debug when you can reproduce failures. The `seed` helper sets the pseudo-random seed and returns it:

```mindscript
testing.seed(n: Int) -> Int
```

A typical pattern is:

```mindscript
testing.test("rand/reproducible", fun(_: Null) do
	testing.seed(12345)
	let a = randInt(1000000)

	testing.seed(12345)
	let b = randInt(1000000)

	testing.assertEq(a, b)
end)
```

This confirms that, given the same seed, the sequence of random values is the same.

---

## 4. Running tests and understanding failures

The `msg test` command is the easiest way to run tests. It:

* walks the directory tree starting at the given path (or at `.` when omitted),
* finds every file whose name ends in `_test.ms`,
* imports those files, which register tests via `testing.test` and `testing.cases`,
* runs all registered tests, possibly in parallel, and
* prints a summary at the end.

The `testing.run` function is the programmatic equivalent:

```mindscript
testing.run({
	pathPrefix: Str,
	parallel: Bool,
	timeoutMs: Int,
	verbose: Bool
}) -> {
	passed: Int,
	failed: Int,
	total: Int,
	durationMs: Int
}
```

The meaning of the options is simple:

* `pathPrefix` tells the runner where to look for `_test.ms` files.
* `verbose` controls whether you see a “RUN/PASS/FAIL” line for every test or only for failing tests.
* `parallel` decides whether tests run one after another or in separate processes.
* `timeoutMs` sets a per-test timeout; non-positive values or `null` mean “no timeout”.

When a test fails, the runner prints the test name, the panic message, and, when possible, the file and line where the `testing.test` or `testing.cases` call was written. This “AT path:line” hint is collected by scanning the source of your test files before importing them, so you can usually jump straight to the right place.

Most failures come from assertions. In those cases you see the messages produced by `assert`, `assertEq`, `assertType`, or `assertThrows`. When your own code panics directly, its message appears in the same way.

---

## 5. Short reference

This section summarizes the main functions and types provided by the `testing` module. All functions follow normal MindScript conventions: return `true` on success, and panic loudly when there is a programmer error in the test code.

* Register one test:

  ```mindscript
  testing.test(name: Str, fn: Null -> Any) -> Bool
  ```

* Inspect and clear the registry:

  ```mindscript
  testing.tests(_: Null) -> {Str: Any}   # shallow clone of name -> fn
  testing.reset(_: Null) -> Bool         # remove all registered tests
  ```

* Assertions:

  ```mindscript
  testing.assert(ok: Bool, msg: Str?) -> Bool
  testing.assertEq(want: Any, got: Any) -> Bool
  testing.assertType(v: Any, T: Type) -> Bool
  testing.assertThrows(fn: Null -> Any, sub: Str?) -> Bool
  ```

* Table-driven tests:

  ```mindscript
  testing.cases(name: Str, xs: [Any], fn: Any -> Any) -> Int
  ```

* Snapshot helpers:

  ```mindscript
  testing.snapshot(path: Str, value: Any, update: Bool?) -> Bool
  testing.snapshotUpdate(path: Str, value: Any) -> Bool
  ```

* Random seed helper:

  ```mindscript
  testing.seed(n: Int) -> Int
  ```

* Runner options and summary:

  ```mindscript
  let RunnerOpts = type {
  	parallel: Bool,
  	pathPrefix: Str,
  	timeoutMs: Int,
  	verbose: Bool
  }

  let Summary = type {
  	durationMs: Int,
  	failed: Int,
  	passed: Int,
  	total: Int
  }

  testing.run(opts: RunnerOpts) -> Summary
  ```

With these pieces you can write small, clear test functions, run them with `msg test`, and read failures that point you back to the right file, line, and message.
