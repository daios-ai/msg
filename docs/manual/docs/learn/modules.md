# Modules

Once a script grows beyond a few dozen lines, you start wanting a bit of structure: a place to put helpers, a way to group related definitions, and a way to avoid name collisions as your codebase grows.

In MindScript, the tool for that is the **module**.

A module is a **namespace** you access with dot notation (`m.name`). It is also a **capsule**: a single value that bundles a set of bindings so you can pass them around as one thing. In practice, modules are how you build libraries—whether they live in files, directories, URLs, or are created in memory.

Most of the time you won’t "think in modules"; you’ll just import one and use its names. But understanding the module model pays off quickly, because it explains why imports are predictable and why reorganizing files doesn’t have to break everything.

### What a module is

A module is a single runtime value that packages a set of named bindings. You access those bindings with dot notation (`m.name`).

More precisely, a module gives you:

* *A scope boundary.* The module’s code runs in its own top-level lexical scope. It does not capture locals from the importing file. Its meaning depends on its own source (and whatever it imports), not on where it was imported from.

* *A singleton by identity.* Importing the same module identity more than once returns the same module instance. The runtime caches loaded modules so repeated imports are consistent and cheap, and so import cycles can be detected reliably.

* *A self-contained unit you can treat as data.* Because modules don’t depend on importer locals, you can safely move them around as a unit (for example by referring to their identity, or by shipping their source and loading it with `importCode`). This is different from functions that close over ambient variables, where behavior can depend on captured lexical state that isn’t present at the use site.

If you’ve used other languages, the feel is familiar:

* Python: closest to a Python module object — you import it, then access exported names.
* Node: like requiring a file that returns an object-like namespace.
* Go: like a package initialized once and reused by name.

MindScript’s twist is that modules are just values: you can import them from files/URLs, construct them directly in memory, and (in tooling contexts) represent them in a form suitable for transmission and re-loading.

By convention, module names are short `snake_case` identifiers, like `testing`, `util`, or `http_client`.


### Modules are a lexical boundary

A module is not "an inner block" of the file that imports it. It executes in its own top-level scope, which means it cannot see your locals:

```mindscript
let secret = 123

let mymod = module "my_module" do
    let get = fun() -> Int do
        secret
    end
end
```

Here the reference to `secret` is not valid. This restriction prevents unintended changes to a module's meaning depending on where it was imported, or which variables happened to be in scope at the import site.

---

## Importing a module

Most modules are loaded from files, directory packages, or URLs. To import one, call:

```mindscript
import(spec: Str) -> Any
```

The result is a module value (a namespace). You access its exported bindings with dot notation:

```mindscript
let util = import("util")
util.slugify("Hello World")
```

Imports are extensionless: you name the module, not a particular file. For a module named `X`, the runtime recognizes exactly two possible entry points:

* `X.ms` (a single-file module)
* `X/init.ms` (a directory package whose entry point is `init.ms`)

If exactly one of these exists at the chosen location, it is loaded. If both exist, the import fails as ambiguous. If neither exists, the runtime tries the next base location (if any), otherwise the import fails as “module not found”.

Under the hood, importing resolves `spec` to a concrete location (on disk or over the network), loads the source code, evaluates it in a fresh module environment, and returns the resulting module value.

### Name resolution

Resolution is deliberately small and predictable. It has two steps: choose where to look, then choose which entry point to load.

First, the runtime decides whether `spec` is absolute or relative.

If `spec` is absolute (an absolute filesystem path, or a full `http://` / `https://` URL), it is treated as an exact address. The runtime tries only that location; there is no searching and no fallback. For example:

```mindscript
let math = import("https://mydomain.com/project/math_module")
let crypt = import("/home/user/myproject/crypt")
```

If `spec` is relative, the runtime searches two bases, in order:

1. the directory of the importing module (or the current working directory in the REPL)
2. `<installationdir>/lib/` (the standard library root)

For example:

```mindscript
let math = import("my_project/math_module")
```

Second, for each base the runtime checks the two entry points described above (`X.ms` and `X/init.ms`) and requires an unambiguous match. The same rule applies whether the base is a filesystem location or a URL: `spec` names a module, and the runtime resolves it by checking those two entry points and rejecting ambiguity.


### File vs package

For each location tried, the runtime checks exactly two entry points:

* `fullspec.ms`
* `fullspec/init.ms`

Rules:

* if exactly one exists: load it
* if both exist: fail (“ambiguous module”)
* if neither exists: try the next location (or fail “module not found”)


---

## Writing your own module

A module on disk is just a normal `.ms` file containing top-level bindings. There’s no `module` keyword at the top of the file.

Example `util.ms`:

```mindscript
let slugify = fun(s: Str) -> Str do
    replace("\\s+", "-", toLower(strip(s)))
end
```

Use it:

```mindscript
let util = import("./util")
util.slugify("  Hello World  ")  # "hello-world"
```

That’s it: importing the file turns those top-level bindings into a module namespace.

### What is “exported”?

The simplest rule is: **top-level bindings become module fields**. If you define `let slugify = ...` at top level, it becomes `util.slugify`.

In practice you’ll often keep “helper helpers” private by convention (for example names that start with `_`), but remember: that’s a convention, not a security boundary. The module is still a value with fields.

---

## Structuring a project

Start simple. A single script is fine when you’re experimenting:

```
tool.ms
```

As the code grows, split it into a small `src/` tree and keep tests **colocated** with the code they cover (the test runner discovers `*_test.ms` files automatically):

```
src/
  main.ms
  main_test.ms
  util.ms
  util_test.ms
  mylib/
    init.ms
    parsing.ms
    parsing_test.ms
```

A few practical notes:

* Put the entry point in `src/main.ms`.
* Keep reusable helpers in nearby modules (`src/util.ms`, `src/parse.ms`).
* Colocate tests next to their module (`util_test.ms`, `parse_test.ms`, etc.). This keeps maintenance simple and mirrors the “tests live with code” style used by Go.
* Use a directory module (`src/mylib/init.ms`) when you want a stable library surface that can grow. Callers import the directory name:

```mindscript
let mylib = import("./mylib")
```

Inside `src/mylib/init.ms`, you can import submodules like `./parsing` as the library grows.

Finally, keep imports unambiguous: don’t create both `mylib.ms` and `mylib/init.ms` for the same module name—`import("mylib")` should resolve to exactly one entry point.


---

## `importCode`: modules from strings

Sometimes you don’t have a file or URL. You have source code in a string: generated code, embedded test fixtures, or tooling output. That’s what `importCode` is for:

```mindscript
importCode(name: Str, src: Str) -> Any
```

Example:

```mindscript
let m = importCode("demo", "let answer = 42")
m.answer  # 42
```

This is handy in a few places:

* *tests*, where you want a tiny throwaway module without creating a file,
* *tools*, where you generate small modules on the fly,
* *metaprogramming*, where you load code that you just produced.

Two details matter:

1. The module identity is *exactly* the `name` you provide (so choose a name that won’t collide accidentally).
2. The source code executes, so treat it as *trusted-code-only*.

If you need to run untrusted text, don’t import it as code. Parse it as data instead.

---

## In-memory modules with `module ... do ... end`

When you want a small namespace without creating a file, you can construct a module value directly:

```mindscript
let text = module "text_tools" do
    let clean = fun(s: Str) -> Str do strip(toLower(s)) end
    let nonEmpty = fun(s: Str) -> Bool do s != "" end
end

text.clean("  Ada  ")   # "ada"
```

This is a nice way to package “a couple of helpers” without exporting them globally. It also makes the “module as value” idea concrete: you can store it in an object, return it from a function, or pass it to another module.

Just remember: the body is a module scope, not a closure. It won’t see locals from where it was written.

---

## How `module`, `import`, and `importCode` fit together

`module "name" do ... end` is the primitive that creates a module value.

`import(spec)` and `importCode(name, src)` are the ergonomic front doors:

* `import` resolves a location, loads source, and evaluates it as a module.
* `importCode` takes source directly and evaluates it as a module.
* Both end up producing the same kind of module value with the same caching and cycle detection behavior.

You usually don’t need to care about this layering. It becomes useful later, when you start doing introspection and tooling: modules stop being “just how you organize files” and start being first-class values you can generate, inspect, and export deliberately.

