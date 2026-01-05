# Modules

Once a script grows beyond a few dozen lines, you start wanting a bit of structure: a place to put helpers, a way to group related definitions, and a way to avoid name collisions as your codebase grows.

In MindScript, the tool for that is the **module**.

### What a module is

A module can be thought of as a map with its own **namespace**. Modules are how you build **libraries** (a bundle of state and functionality) and **capsules** (self-containd objects that can be transmitted). They can live in files/directories you import, or in built during runtime.

More precisely, a module gives you:

* *A scope boundary.* The module’s code runs in its own top-level lexical scope. It can't capture locals from the importing file.

* *A singleton by identity.* Importing the same module identity more than once returns the same module instance. 

* *A self-contained unit you can treat as data.* When importing/creating a module, you can access its properties as if they were a map.


By convention, module names are short `snake_case` identifiers, like `testing`, `util`, or `http_client`. You can create them:

* directly inside a script using the `module NAME do BLOCK end` syntax;
* writing the source code in a file, and then importing it using the `import(NAME)` function from the local filesystem or from an URL.

### Modules are a lexical boundary

A module is not "an inner block" of the file that imports it. It executes in its own top-level scope, which means it cannot see your locals:

```mindscript
let secret = 123

let mymod = module "my_module" do
    let get = fun() -> Int do
        secret # runtime error!
    end
end
```

Here the reference to `secret` is not valid. This restriction prevents unintended changes to a module's meaning depending on where it was imported, or which variables happened to be in scope at the import site.

---

## Importing a module from a file

Most modules are loaded from files, directory packages, or URLs. To import one, call:

```mindscript
import(spec: Str) -> Any
```

The result is a module value (a namespace). You access its exported bindings with dot notation:

```mindscript
let util = import("util")
util.slugify("Hello World")
```

You don't need to provide a file name. When importing a module named `X` by doing `import("X")`, then runtime will load exactly one of the two possible files:

* `X.ms`, a single-file module; or
* `X/init.ms` a directory package whose entry point is `init.ms`. It won't load any other file within the same directory.

If exactly one of these exists at the chosen location, it is loaded. If both exist, the import fails as ambiguous. If neither exists, the runtime tries the next base location (if any, see *name resolution* below), otherwise the import fails as “module not found”.

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

1. the directory of the importing module or the current working directory in the REPL;
2. the standard library root directory `<installation-directory>/lib/`.

For example:

```mindscript
let math = import("project/math")
```

will first try `<current-directory>/project/math` and then `<installation-directory>/lib/project/math`.

Second, for each base the runtime checks the two entry points described above (`X.ms` and `X/init.ms`) and requires an unambiguous match. The same rule applies whether the base is a filesystem location or a URL: `spec` names a module, and the runtime resolves it by checking those two entry points and rejecting ambiguity.

### Writing your own module

A module on disk is just a normal MindScript program file with the extension `.ms`. There’s no `module` keyword at the top of the file.

Example `util.ms`:

```mindscript
let slugify = fun(s: Str) -> Str do
    replace("\\s+", "-", toLower(strip(s)))
end
```

All the top-level bindings become module fields. If you define `let slugify = ...` at top level, it becomes `util.slugify` if you import it as `let util = import("util")`.

```mindscript
let util = import("./util")
util.slugify("  Hello World  ")  # "hello-world"
```

By convention, names that start with `_` such as `_name` or `_idNumber` are considered private, although this is not enforced by the runtime and is thus not a security boundary.

### Structuring a project

Start simple. A single script is fine when you’re experimenting:

```text
tool.ms
```

As the code grows, split it into a small `src/` tree and keep tests **colocated** with the code they cover (the test runner discovers `*_test.ms` files automatically):

```text
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
* Colocate tests next to their module (`util_test.ms`, `parse_test.ms`, etc.). 
* Use a directory module (`src/mylib/init.ms`) when you want a stable library surface that can grow. Callers import the directory name:

```mindscript
let mylib = import("mylib")
```

Inside `src/mylib/init.ms`, you can import submodules like `parsing` as the library grows.

Finally, keep imports unambiguous: don't create both `mylib.ms` and `mylib/init.ms` for the same module name; `import("mylib")` should resolve to exactly one entry point.


---

## Modules from strings

Sometimes you have source code in a string: generated code, embedded test fixtures, or tooling output. That’s what `importCode` is for:

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

## Embedded Modules

When you want a small namespace without creating a file, you can construct a module value directly:

```mindscript
let text = module "text_tools" do
    let clean = fun(s: Str) -> Str do strip(toLower(s)) end
    let nonEmpty = fun(s: Str) -> Bool do s != "" end
end

text.clean("  Ada  ")   # "ada"
```

This is a nice way to package "a couple of helpers" without exporting them globally. It also makes the "module as value" idea concrete: you can store it in an object, return it from a function, or pass it to another module.

