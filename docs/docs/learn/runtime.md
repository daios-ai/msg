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

The process then ends with exit status 0 if successful or non-zero otherwise.

### A small stdin/stdout example

A common pattern is to read data from the standard input, process it, and print to the standard output (or standard error if there's an error). To read/write from these, use the predefined handles named `STDIN`, `STDOUT`, and `STDERR` respectively. 

Create `echo.ms`:

```mindscript
let input = readAll(STDIN)
if input == null then
    write(STDERR, "failed to read stdin")
    flush(STDOUT)
else
    write(STDOUT, input)
    flush(STDOUT)
end
```

Run it as a pipeline:

```bash
echo "hello" | msg run echo.ms
```

This will print `hello` to the standard output.

### Script arguments

Additional inputs to `msg run` are interpreted as command-line arguments for the script. The runtime exposes execution context information through a special object named `runtime` within the program. It also contains the field `runtime.argv`, which is an array of strings containing the arguments.

Create `args.ms`:

```mindscript
println(runtime.argv)
```

To test it, run it as follows:

```bash
msg run args.ms one two three
```

This will print

```mindscript
["one", "two", "three"]
```

Two additional useful fields of the `runtime` object are:

* `runtime.path`, which contains the path of the entry script;
* `runtime.isEntry`, which is `true` if it is the entry script.

---

## Interactive sessions: `msg repl`

You can run `msg` interactively using the `msg repl` command. This starts an interactive session where you can type expressions and see their results immediately (a Read-Eval-Print Loop).

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

MindScript ships with a formatter which will rewrite your source code following the standard conventions. The `msg fmt` takes a *path prefix* as an input and it will format all the matching files *in place*.

For instance, to format all the MindScripts `.ms` files in the current directory and all its subdirectories, enter:

```bash
msg fmt .
```

Sometimes you only want to check whether a given source code is formatted correctly. For this, you can run the formatting command with the `--check` flag.

```bash
msg fmt --check .
```

In this mode the formatter will print a list of the files that are not well-formatted.

---

## Running tests: `msg test`

The runtime ships with a built-in unit tester and a standard module called `testing` to write tests. The command `msg test` then runs your project’s automated checks and reports pass/fail with a summary. It discovers test files, loads them, runs the registered tests, prints progress, and exits non-zero if anything failed.

By convention, test files end in `_test.ms`, and they are usually located alongside the code they test. Like in formatting, the testing command takes a *path prefix* as argument. So, to run all tests under the current directory, enter:

```bash
msg test .
```

This will print the all test failures if there were any.

### Modes and flags

You can run your tests in verbose mode with the `-v` flag:

```bash
msg test . -v
```

This will print all the tests that are being executed with their success/failure status.

In addition, you can run your tests concurrently using the parallel mode flag `-p`. This is useful when tests are independent and you want speed:

```bash
msg test . -p
```

As a safety net for tests that might hang, you can also set a per-test time limit, in milliseconds. This is done using the `-t` flag:

```bash
msg test . -t 5000
```

Obviously, flags can be combined:

```bash
msg test . -p -v -t 5000
```

Writing tests is beyond the scope of this chapter. We will learn how to do this in chapter on [testing](learn/testing.md).

---

## Version information: `msg version`

When you file a bug report or compare behaviors between machines, the first thing you want is the exact runtime version. The `msg version` command prints the version string.

```bash
msg version
```

## Install packages using `msg get`

This feature is not implemented yet.

## Final words

This concludes the chapter on the `msg` command line interface. As we have seen, this tool packs (and thereby standardizes) a number of utilities typically used during the lifecycle of a project.
