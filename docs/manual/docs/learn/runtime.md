# The `msg` Runtime

When you install MindScript you get a small command-line program called `msg`. It reads MindScript `.ms` files, parses them, evaluates them, and prints results. It also gives you a REPL for interactive work, a standard code formatter, and a test runner for.

This chapter is a tour of `msg` itself, so that you know what commands exist, what they do, and what a normal workflow looks like.

---

## Finding your way around

When you forget a command, ask `msg`:

```bash
msg help
```

This command prints a message listing all the commands:

* `run` runs a file
* `repl` starts an interactive prompt
* `fmt` formats files
* `test` runs tests
* `version` prints the version string

You may see `get` listed too, but this is not implemented yet.

---

## Running a file: `msg run`

The workhorse command is `msg run`. To execute a file named `hello.ms`, enter

```bash
msg run hello.ms
```

During execution your program may also write to stdout/stderr (for example via `println`). That output appears as the program runs; the final formatted value is printed at the end.

### A small stdin/stdout template

A common pattern is “data in, data out”: read from stdin, transform, print to stdout.

Many operations report failure by returning `null` (often with an attached message). In scripts, it’s normal to check for `null` and print a short diagnostic.

Create `echo.ms`:

```mindscript
let input = readAll(STDIN)

if input == null then
    println("failed to read stdin")
    null
else
    println(input)
end
```

Run it as a pipeline:

```bash
echo "hello" | msg run echo.ms
```

### Script arguments

The additional inputs to `msg run` are interpreted as command-line arguments for the MindScript program. The runtime exposes information through a special object named `runtime` within the program. The most important field is `runtime.argv`, which is an array of strings containing the arguments.

Create `args.ms`:

```mindscript
println(runtime.argv)
```

Run it:

```bash
msg run args.ms one two three
```

You will see:

```mindscript
["one", "two", "three"]
```

Alternatively you can add `--` to separate the arguments from `msg`’s own parsing:

```bash
msg run args.ms -- one two three
```

### Finding the entry file from inside the program

Programs often need to locate files relative to the script itself (a `config.json`, a `data/` folder, fixtures). The field `runtime.path` is the path of the entry script, which is absolute when possible. The field `runtime.isEntry` is `true` for that entry script.


---

## Exploring interactively: `msg repl`

You can run `msg` interactively using the `msg repl` command. This starts an interactive session where you can type expressions and see their results immediately (a read-eval-print loop).

```
$ msg repl
MindScript 0.1.4 REPL
Ctrl+C cancels input, Ctrl+D exits. Type :quit to exit.
==> 1 + 1
2
==> let obj = {name: "John", age: 25}
{name: "John", age: 25}
==> 
```

This works as expected: if the input is an incomplete expression, then REPL will keep reading until it is complete.

To exit the REPL, type `:quit` or press `Ctrl+D`. If you press `Ctrl+C`, the REPL cancels the current input without exiting.

The REPL also writes a history file at `~/.mindscript_history`.

---

## Keeping code tidy: `msg fmt`

MindScript ships with a formatter which will rewrite your source code following the standard conventions. The `msg fmt` takes a path prefix as an input and it will format all the files that match it in place.

To format everything under the current directory, enter:

```bash
msg fmt .
```

This formats all the files inside the current and the nested directories.

In continuous integration you usually do not want to rewrite files. You want to check whether they *would* change. That is what `--check` is for:

```bash
msg fmt --check .
```

In check mode, `msg fmt` prints the files that are not formatted and exits with a failing status if any would change.

---

## Running tests: `msg test`

The runtime ships with a built-in unit tester and a standard module `testing` to write tests. The command `msg test` then runs your project’s automated checks and reports pass/fail with a summary. It discovers test files, loads them, runs the registered tests, prints progress, and exits non-zero if anything failed.

By convention, test files end in `_test.ms`. To run all tests under the current directory:

```bash
msg test
```

To run tests under a specific path prefix `xyz`, enter:

```bash
msg test xyz
```

This will print the all test failures if there were any.

### Modes and flags

You can run your tests in verbose mode with the `-v` flag:

```bash
msg test . -v
```

In addition, you can run your tests concurrently using the parallel mode flag `-p`. This is useful when tests are independent and you want speed:

```bash
msg test . -p
```

You can also set per-test time limit in milliseconds. This is mainly a safety net for tests that might hang:

```bash
msg test . -t 5000
```

Flags can be combined:

```bash
msg test . -p -v -t 5000
```

To learn how to write tests, please refer to the chapter on [testing](learn/testing.md).

---

## Version information: `msg version`

When you file a bug report or compare behaviors between machines, the first thing you want is the exact runtime version.

```bash
msg version
```

It prints the compiled version string and nothing else.

## Install packages using `msg get`

This feature is not implemented yet.

---

## A normal workflow

A typical small MindScript project looks like this in practice:

* start in the REPL when you are unsure
* move into a file when the idea solidifies
* run it with `msg run`, often as a pipeline component
* format with `msg fmt` before committing
* run `msg test` when the project grows beyond a single script
* compare `msg version` when behaviors differ across machines

That is the entire `msg` story: run, explore, format, test, and know what version you’re on.
