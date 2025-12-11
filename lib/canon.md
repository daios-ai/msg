# **canon** — canonical formatting for MindScript

`canon` is the library behind `msg fmt`, the standard formatter for MindScript. It rewrites your `.ms` files into a stable style: indentation, spacing, comments, and layout are normalized so the whole project looks consistent. It does not change program behavior, only how the code is written.

Most people use it via the CLI. You can also import it directly when you want to build tools, editor integrations, or format generated code.

---

## 1. Using `canon` from the CLI (`msg fmt`)

The usual entry point is the `fmt` command of the `msg` tool:

```bash
$ msg fmt [--check] [path ...]
```

When you pass one or more paths, `msg fmt` walks each of them (or `.` when you omit paths), finds all `*.ms` files underneath, and rewrites any file that is not already in canonical form.

Examples:

```bash
# Format the current directory tree in place
$ msg fmt

# Format a single file
$ msg fmt app.ms

# Format everything under src/ and tests/
$ msg fmt src tests
```

For CI and pre-commit hooks you normally use check mode:

```bash
# Check formatting without rewriting files
$ msg fmt --check

# Check only src/
$ msg fmt --check src
```

In check mode the formatter still runs and discovers the same files, but it does not write anything back. If any file would change, `msg fmt --check` exits with a non-zero status, which is enough for a CI job or hook to fail and ask you to run the formatter locally.

In day-to-day work you run `msg fmt` regularly, or wire it into your editor on save, and keep a `msg fmt --check` step in CI to make sure everything stays canonical.

---

## 2. Importing `canon` as a library

When you want to drive formatting from MindScript itself, import the module:

```mindscript
let canon = import("canon")
```

This is useful for project-specific tools, for formatting generated code before writing it to disk, or for building editor helpers.

The following sections describe the main functions. Return values that may fail use the usual nullable pattern: on error you get `null` instead of a panic, so callers can decide how to handle I/O or syntax problems.

---

## 3. Formatting a source string with `format`

The smallest piece of the API is `format`, which takes a single source string and returns the canonical version:

```mindscript
let canon = import("canon")

let src = "   let    x=1   \n"
let fmtd = canon.format(src)

if fmtd == null then
	println("unformattable source")
else
	println(fmtd)  # prints: "let x = 1\n"
end
```

Signature:

```mindscript
canon.format(src: Str) -> Str?
```

On success `format` returns the canonical version of `src`. Once a string is canonical, calling `format` again returns exactly the same string. The function is pure and works entirely in memory; it does not touch the filesystem.

---

## 4. Discovering `.ms` files with `files`

To work on a whole tree you first need to know which files to visit. The `files` function walks a directory and returns all MindScript source files under it:

```mindscript
let canon = import("canon")

let paths = canon.files(".")
if paths == null then
	println("file discovery failed")
else
	for p in paths do
		println(p)
	end
end
```

Shape:

```mindscript
canon.files(root: Str) -> [Str]?
```

The `root` may be a relative or absolute path. Internally it is normalized to an absolute, cleaned directory path and then walked recursively. On success you get an array of absolute, cleaned paths for every `.ms` file below `root`, including subdirectories.

If `root` is not a directory, or if discovery fails (for example due to a permission error), `files` returns `null`.

This is the same discovery step that `msg fmt` uses internally.

---

## 5. Checking whether one file is canonical with `checkFile`

If you want to know whether a particular file is already formatted, use `checkFile`. It reads the file, formats the contents, and compares the result to the original text:

```mindscript
let canon = import("canon")

let ok = canon.checkFile("app.ms")
if ok == null then
	println("could not read or format app.ms")
elif ok then
	println("app.ms is already canonical")
else
	println("app.ms would change if formatted")
end
```

Signature:

```mindscript
canon.checkFile(path: Str) -> Bool?
```

This is the library equivalent of a check-only mode for a single file, and it is useful in custom scripts or tools that want to warn about formatting without changing files.

---

## 6. Formatting one file in place with `formatFile`

To actually rewrite a file on disk, call `formatFile`. It formats the contents and writes them back only if the canonical form differs:

```mindscript
let canon = import("canon")

let res = canon.formatFile("app.ms")
if res == null then
	println("formatFile failed")
elif res.changed then
	println("app.ms was reformatted")
else
	println("app.ms was already canonical")
end
```

Shape:

```mindscript
canon.formatFile(path: Str) -> { changed: Bool }?
```

Internally `formatFile` normalizes the path, reads the file, runs `canon.format`, and compares the result with the original. It returns a small map when it succeeds, with `changed` set to `true` if the file was rewritten and `false` if it did not need any changes.

While it runs, it also prints a line of the form:

```text
Formatting: path
```

so you can see which files are being processed.

---

## 7. Formatting a tree and getting a summary with `formatTree`

For project-level work, `formatTree` combines discovery and formatting into a single call and returns a compact summary:

```mindscript
let canon = import("canon")

let rep = canon.formatTree(".")
if rep == null then
	println("discovery failed")
else
	printf("total=%d changed=%d errors=%d\n", [
		rep.total,
		rep.changed,
		rep.errors
	])
end
```

Signature:

```mindscript
canon.formatTree(root: Str)
	-> { total: Int, changed: Int, errors: Int }?
```

`formatTree` first calls `canon.files(root)` to find all `.ms` files. For each file, it calls `formatFile` and updates the counters:

* `total`   — number of MindScript files seen.
* `changed` — files that were rewritten.
* `errors`  — files that could not be processed (read/format/write failures).

If discovery fails (for example, `root` is not a directory), `formatTree` returns `null`.

This is the closest library-level analogue of `msg fmt` and is a good starting point for custom formatting scripts.

---

## 8. Working with `canon` in everyday use

In normal development you let `canon` handle style and layout. Run `msg fmt` often so changes stay small and predictable, and add `msg fmt --check` to your CI pipeline so that all code that lands in the repository is already canonical.

When you need more control, reach for the library functions:

* Use `format` for in-memory strings.
* Use `files` and `checkFile` for custom checks.
* Use `formatFile` when you want to touch one file.
* Use `formatTree` when you want a single summary for a whole project.

---

## 9. Short reference

This section summarizes the main functions provided by the `canon` module. All functions follow the MindScript conventions used elsewhere: they return soft errors as `null` instead of panicking on I/O or syntax failures.

```mindscript
let canon = import("canon")
```

**Format a source string**

```mindscript
canon.format(src: Str) -> Str?
```

Format a MindScript source string in memory.

* Returns the canonical source on success.
* Returns `null` when the source cannot be formatted (for example, because it is syntactically invalid).

---

**Discover `.ms` files**

```mindscript
canon.files(root: Str) -> [Str]?
```

Walk `root` recursively and return an array of absolute, cleaned paths for all files ending in `.ms`.

* `root` may be relative or absolute; it is normalized internally.
* Returns `null` if `root` is not a directory or if discovery fails.

---

**Check one file**

```mindscript
canon.checkFile(path: Str) -> Bool?
```

Read and format a single file, then compare the formatted text to the original.

* `true`  — file is already canonical.
* `false` — file would change if formatted.
* `null`  — read or format error.

---

**Format one file in place**

```mindscript
canon.formatFile(path: Str) -> { changed: Bool }?
```

Format and rewrite a single file only when needed.

* `{changed: true}`  — file was rewritten.
* `{changed: false}` — file was already canonical.
* `null`             — read/format/write error.

Also prints `Formatting: path` while running.

---

**Format a tree and summarize**

```mindscript
canon.formatTree(root: Str)
  -> { total: Int, changed: Int, errors: Int }?
```

Combine discovery and formatting over `root`.

* `total`   — number of `.ms` files seen.
* `changed` — files that were rewritten.
* `errors`  — files that had a read/format/write error.
* Returns `null` if discovery fails (for example, when `root` is not a directory).
