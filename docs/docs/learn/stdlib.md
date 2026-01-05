# Standard Library

!!! warning
    This page is under construction.

MindScript’s standard library is built for the kind of work scripts actually do: read some data, turn it into values, transform it, then write something useful back out—often in a way that composes in shell pipelines and CI.

You will see the same design pattern across the library:

* **At boundaries**, functions usually return `null` on failure, often with an annotation explaining why.
* **Inside your program**, you keep values JSON-shaped (arrays, objects, strings, numbers) and use small, predictable primitives to transform them.
* **When you truly cannot continue**, something panics (a hard stop). You can catch hard failures with `try(...)` when you want to keep going.

This chapter is not a reference list. It shows how the standard tools fit together in real scripts.

---

## Seeing what you have

When you’re building a transformation, the fastest way to debug is to print intermediate values without restructuring your code.

`println(x)` prints a readable representation **and returns `x`**, so you can insert it into an expression pipeline.

```mindscript
let s = readAll(STDIN)
if s == null then
    s
else
    let x = jsonRepair(s)
    if x == null then
        x
    else
        println(x)    # inspect, then keep going
        x
    end
end
```

When you need a guaranteed representation (for logs, snapshots, or error reports), use formatting explicitly:

* `formatValue(x)` produces a stable formatted string for any value.
* `str(x)` converts only “data values” (`null/bool/numbers/strings/arrays/maps`) to a string, returning `null` for opaque runtime objects (handles, types, functions).

A common pattern for human-facing error messages:

```mindscript
let x = jsonParse(input)
if x == null then
    println("bad input: " + (noteGet(x) or "<no details>"))
    null
else
    x
end
```

---

## Working with arrays and objects

Most scripts transform JSON-like structures:

* arrays: `[ {...}, {...} ]`
* objects: `{ user: {...}, items: [...] }`

### Arrays: indexing, mutation, copying

Arrays are mutable. Indexing supports negatives (`xs[-1]` is the last element). If you need to avoid shared mutation, deep-copy with `clone`.

```mindscript
let xs = [1, 2, 3]
push(xs, 4)      # [1, 2, 3, 4]
pop(xs)          # 4
xs[-1]           # 3

let ys = clone(xs)
ys[0] = 999
xs[0]            # still 1
```

`len(xs)` gives the element count.

### Objects: property access, computed keys, safe edits

Objects are ordered maps. Use dot access when the key is a normal identifier, and computed access when it isn’t (or when the key is a runtime value).

```mindscript
let user = {name: "Ada", "x-coordinate": 10}

user.name                 # "Ada"
user."x-coordinate"       # 10

let k = "name"
user.(k)                  # "Ada"
```

Accessing an unknown property is a hard failure. If missing keys are normal in your data, check first:

```mindscript
let stripDebug = fun(obj: {}) -> {} do
    if mapHas(obj, "debug") then
        mapDelete(obj, "debug")   # in-place
    end
    obj
end
```

---

## Iteration: arrays, objects, and iterator functions

`for` loops can iterate over:

* an array (yields elements),
* an object (yields `[key, value]` pairs),
* an iterator function of type `Null -> Any?` (returns next item or `null` to stop).

The prelude builds on this with `iter`, `range`, `map`, `filter`, `reduce`, and `list`, which lets you write “pipeline-style” data processing without inventing custom loop patterns every time.

### Turning values into iterators

`iter(v)` returns an iterator function:

```mindscript
let it1 = iter([10, 20, 30])
it1()   # 10
it1()   # 20

let it2 = iter({a: 1, b: 2})
it2()   # ["a", 1]
it2()   # ["b", 2]
```

### A small pipeline example

```mindscript
let clean = fun(s: Str) -> Str do strip(toLower(s)) end
let nonEmpty = fun(s: Str) -> Bool do s != "" end

let out = list(
    filter(nonEmpty,
        map(clean, iter(["  Ada  ", "", "  BOB "]))
    )
)

out   # ["ada", "bob"]
```

### Counting with `range`

`range(start, stop)` is stop-exclusive; use `null` for an open-ended range.

```mindscript
for i in range(0, 3) do
    println(i)
end
```

---

## Strings and regular expressions

Most boundary work is text cleanup: trimming, splitting, normalizing, and light pattern replacement.

Strings are treated as Unicode text for operations like `len` and `substr` (they use rune indices, not bytes).

```mindscript
let s = "  Hello   World \n"
strip(s)                    # "Hello   World"
replace("\\s+", " ", strip(s))   # "Hello World"
```

For delimited text:

```mindscript
let parts = split("a,b,c", ",")     # ["a", "b", "c"]
join(parts, "|")                   # "a|b|c"
```

Regex matching returns a list of matched substrings (non-overlapping matches). Replacement substitutes matches (no capture-group backrefs).

---

## JSON: strict parsing, permissive parsing, and printing

JSON is the default interchange format, so the standard library gives you both:

* strict parsing for clean inputs, and
* a permissive “repair” path for real-world messy inputs.

```mindscript
let x = jsonParse(input)
if x == null then
    x = jsonRepair(input)
end
x
```

To print JSON, use `jsonStringify(x)`. It returns `null` if `x` contains non-JSON kinds (functions, handles, types, modules).

A canonical stdin → stdout JSON transformer looks like this:

```mindscript
let s = readAll(STDIN)
if s == null then
    s
else
    let x = jsonRepair(s)
    if x == null then
        x
    else
        x.processedAt = nowMillis()
        jsonStringify(x)
    end
end
```

---

## Schemas at boundaries

In MindScript, types are runtime values (`Type`) used mainly as **schemas**: validate incoming data before you index it, and validate outgoing data before you ship it.

```mindscript
let User = type { id!: Str, name!: Str, email: Str? }

let x = jsonRepair(input)
if x == null then
    x
elif not isType(x, User) then
    null  # <not a User>
else
    x.name
end
```

When you need to understand an unfamiliar value, start with `typeOf(x)`.

---

## Files, streams, and OS basics

Scripts often read stdin, write stdout, and touch the filesystem. MindScript uses handles (`STDIN`, `STDOUT`, `STDERR`, file handles, network handles) so the same I/O functions work across domains.

### A line-based filter

`readLine(h)` returns a line (without the newline) or `null` at EOF.

```mindscript
while true do
    let line = readLine(STDIN)
    if line == null then
        break(null)
    end

    line = strip(line)
    if line != "" then
        write(STDOUT, line + "\n")
    end
end

flush(STDOUT)
```

`write(...)` is buffered; call `flush(...)` when you need output to appear immediately.

### Whole-file convenience

For small scripts, whole-file helpers keep code short:

```mindscript
let cfg = readFile("config.json")
if cfg == null then
    cfg
else
    jsonRepair(cfg)
end
```

There are also basic OS utilities (stat/mkdir/rename/remove/cwd/chdir, env get/set, tempDir) for glue work.

---

## HTTP and TCP networking

For typical API calls, use `http(...)`, which returns a response object (status, headers, body). For large downloads, use `httpStream(...)`, which gives you a readable handle `bodyH`.

### GET JSON → transform

```mindscript
let r = http({url: "https://api.example.com/items"})
if r == null then
    r
else
    jsonRepair(r.body)
end
```

### Streaming download skeleton

```mindscript
let r = httpStream({url: url})
if r == null then
    r
else
    let out = open("download.bin", "w")
    if out == null then
        out
    else
        while true do
            let chunk = readN(r.bodyH, 64_000)
            if chunk == null then break(null) end
            if len(chunk) == 0 then break(null) end
            write(out, chunk)
        end
        close(r.bodyH)
        close(out)
    end
end
```

TCP primitives (`netConnect/netListen/netAccept`) exist for cases where you need raw connections; they integrate with the same read/write functions.

---

## Running external commands

Sometimes the right tool already exists. `exec([cmd...], opts?)` runs a process without relying on shell quoting and returns `{status, stdout, stderr}` (non-zero `status` is not an error by itself).

```mindscript
let r = exec(["git", "rev-parse", "HEAD"], {cwd: pathDir(runtime.path)})
if r == null then
    r
elif r.status != 0 then
    null  # <git failed>
else
    strip(r.stdout)
end
```

---

## Time: timestamps, parsing, retry loops

Time utilities are for timestamps, RFC3339 formatting/parsing, and retries.

```mindscript
let attempt = 0
let r = null

while attempt < 5 and r == null do
    r = http({url: url, timeoutMs: 5000})
    attempt = attempt + 1
    if r == null then
        sleep(200 * (attempt * attempt))
    end
end

r
```

`dateNow()` returns local time fields. `timeFormatRFC3339(millis)` formats UTC.

---

## Bytes in `Str`: encoding, URLs, crypto, compression

Several APIs treat `Str` as a byte container (HTTP bodies, gzip, crypto). Use encoding helpers to render bytes safely and move them through text channels.

```mindscript
let b = randBytes(32)
if b == null then
    b
else
    base64Encode(b)
end
```

URL helpers let you parse and modify queries structurally:

```mindscript
let u = urlParse("https://example.com/search?q=ada")
if u == null then
    u
else
    u.query.q = ["ada", "lovelace"]
    urlBuild(u)
end
```

Crypto primitives (sha256, hmacSha256, ctEqual) and gzip helpers exist for common integration needs (integrity checks, signatures, payload compression).

---

## Modules: organizing real scripts

As soon as a script grows beyond a single file, use modules. Imports are extensionless:

```mindscript
let testing = import("testing")
let llm = import("llm")
```

For tooling and tests, `importCode(name, src)` loads a module from a string.

---

## Advanced tools (not day one)

MindScript also ships with powerful capabilities that are best learned after you’re comfortable with data + boundaries:

* concurrency (procs, channels, actors),
* AST tooling (`astParse`, `astValidate`, `reflect`, `reify`),
* FFI (`ffiOpen`) where supported.

They exist for advanced workflows, but most scripts never need them.
