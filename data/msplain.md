# MindScript Language Manual (Human-Oriented)

MindScript is a compact, expression-oriented language for two kinds of computation: deductive (ordinary, deterministic functions) and inductive (first-class oracles backed by models or external sources). It pairs formal, structural types with informal annotations (human text that actually guides oracles) while keeping syntax clean and readable.

---

## 1) Core Ideas

* **Everything is an expression.** Blocks, conditionals, loops, and declarations evaluate to a value (often the last subexpression).
* **Strict left-to-right evaluation.** Receivers, operands, and arguments are evaluated left to right.
* **Structural types are first-class.** Types can be named, passed around, and introspected.
* **Two kinds of failure.**

  * **Hard errors** abort execution (`fail`), catchable via `try`.
  * **Soft errors** are ordinary values annotated with a reason, typically using `null` of a nullable type (`T?`), with an attached trailing comment.

---

## 2) Lexical Conventions

### 2.1 Whitespace & indentation

* Indentation uses **tabs** (not spaces) for canonical formatting.
* Canonicalized output ends with exactly **one** trailing newline.

### 2.2 Comments & annotations

MindScript uses a single, unified comment system that also carries metadata:

* **PRE annotations (header comments)**: one or more lines starting with `#` immediately **above** the construct they describe.
* **POST annotations (inline trailing)**: ` # …` placed at the **end** of the line for a construct.
  POST **forces a newline** after it.

**After-separator rule (important):**

* In comma/colon separated lists (arrays, enums, destructuring arrays, value arrays), a POST attached to an element is printed **after the comma that follows that element** (or after the element itself, if it’s the last one).
* In **maps and object patterns/types**:

  * A **key POST** appears on the key line **after the colon**: `key: # post`
  * A **value POST** in a non-last entry appears **after the following comma**: `key: value, # post`

**Examples**

```mindscript
# PRE describes the next value
[ 1, 2, 3 ] # POST explains this array

[ 1, # POST belongs to 1 and goes after the comma
  2 ]

{
	key: # POST on key after colon
	42
}

{
	a: 1, # POST for 'a' after the comma
	b: 2
}
```

---

## 3) Values

Primitive scalars:

* `null`, `true`, `false`
* Integers (unbounded in spec; implementations may use 64-bit): `0`, `-7`
* Numbers (float64): `2.0`, `1e-3`
* Strings: `"hello\nworld"`

Composite:

* Arrays: `[a, b, c]`
* Maps: `{ key: value, "not-ident": 1 }`
  Map keys that look like identifiers are bare; otherwise they are quoted.

---

## 4) Types (Structural and First-Class)

Base types: `Any | Null | Bool | Int | Num | Str | Type`
Nullable: `T?`
Arrays: `[T]`
Open maps (string keys): `{ field!: T, other: U }` — `!` marks **required** fields; others are optional.
Enums (literal sets): `Enum[ lit1, lit2, ... ]` where literals can be scalars, arrays, or maps.
Functions & oracles: `(A, B) -> R` (right-associative; `(A) -> (B) -> R` flattens to `(A, B) -> R`).

**Subtyping (informal):**

* `Int <: Num`
* `T?` matches `null` or `T`
* Arrays covariant by element type
* Maps fieldwise; requiredness can only become **more** required (not less)
* Enums by member subset
* Functions/oracles: parameters **contravariant**, return **covariant**
* `Any` is top

**Examples**

```mindscript
type Ints = [Int]
type Person = { name!: Str, age: Int }
type Method = (Str, Num) -> Bool
type MaybeText = Str?
type Verb = Enum[ "GET", "POST" ]
```

---

## 5) Expressions & Operators

### 5.1 Precedence (high → low)

* Calls / indexing / property: `f(x)`, `a[i]`, `obj.field`
* Unary: `-x`, `not x`, type-nullable constructor prints as `T?`
* Binary arithmetic: `* / %`, then `+ -`
* Comparisons: `< <= > >=`
* Equality: `== !=` (deep for arrays/maps; number equality by value; `2 == 2.0` is true)
* Boolean: `and`, then `or`
* Assignment (`=`) is an expression with low precedence (rarely used in nested exprs; prefer statements).

**Rules**

* **Relational ops** only compare **number–number** (mixing `Int`/`Num` allowed) or **string–string**; other mixes are **hard errors**.
* Arithmetic promotes as needed; `/` yields a floating result.
* `%` follows the dividend sign.

**Examples**

```mindscript
2 + 2.5
not (a < 3 and b == 0)
x = y + 1
obj.field[0].sub(42)
```

---

## 6) Variables, Patterns & Assignment

Identifiers: `x`, `_tmp`, `Name_2`

**Declaration** uses `let` with a **pattern**:

* Single name: `let x = 10`
* Array destructuring (missing entries bind to `null`; extra items ignored unless matched):

  ```mindscript
  let [a, b] = [1, 2, 3]   # a=1, b=2
  ```
* Object destructuring (missing keys bind to `null`):

  ```mindscript
  let { name: n, age: a } = { name: "Ada" }  # n="Ada", a=null
  ```

**Update** assigns to l-values (`name`, `obj.field`, `arr[i]`). Updating an unbound name is a **hard error**:

```mindscript
let x = 1
x = 2
```

There are no named declarations. Functions, types, modules, etc. are all
anonymous and need to be bound to a variable for later reference.
```
let f = fun() do true end
let T = type {name: Str}
let m = module "m" do ... end
```

---

## 7) Control Flow

### 7.1 Blocks

A block returns the last subexpression (or `null` if empty):

```mindscript
do
	1
	2
end   # => 2
```

### 7.2 If / elif / else

```mindscript
if cond then
	doSomething()
elif other then
	handleOther()
else
	default()
end
```

### 7.3 While

```mindscript
while i < 10 do
	i = i + 1
end
```

### 7.4 For (arrays, maps, modules, host iterables)

```mindscript
let sum = 0
for x in [1, 2, 3] do
	sum = sum + x
end
sum
```

When iterating a map, the loop yields `[key, value]` pairs:

```mindscript
for [k, v] in { a: 1, b: 2 } do
	write(STDOUT, sprintf("%s=%d\n", [k, v]))
end
```

### 7.5 Early exits

`return expr`, `break expr`, `continue [expr]` — each carries a value; `null` payload prints as the bare keyword:

```mindscript
while true do
	break "done"
end
```

---

## 8) Functions & Oracles

### 8.1 Functions

```mindscript
# Increment an Int by 1.
# Args: n:Int
# Return: Int
fun(n: Int) -> Int do
	n + 1
end
```

* Parameter types and return types should be explicit in public APIs.
* Omissions default to `Any` in the surface grammar but are **not** printed by canonical formatters:

  * `fun(x) do ... end` means `x: Any`
  * `-> Any` may be omitted when returning `Any`
* Calls are **curried** semantically, but the surface form is standard calls: `f(a)(b)` is allowed; `f(a, b)` is conventional.
* The **actual** returned value must conform to the declared return type; otherwise **hard error**.

### 8.2 Oracles (typed generative calls)

An oracle behaves like a function with **typed inputs** and an **uncertain** output: calls never hard-fail; instead they produce a value of `R?`.

```mindscript
# Pick a primary color.
# Return: Enum[ "red", "green", "blue" ]?
oracle() -> Enum[ "red", "green", "blue" ]
```

**Calling rules**

* Argument arity/types are checked **strictly** (hard errors on mismatch).
* Output is treated as `R?`:

  * If the generated candidate conforms to `R`, it’s returned.
  * Otherwise or if unavailable, you get a **soft** `null` with a trailing POST reason.

---

## 9) Modules

Modules encapsulate a namespace and evaluate to a **module value** (iterable like a map of exports).

```mindscript
module "mathx" do
	let PI2 = 2.0 * PI

	fun area(r: Num) -> Num do
		PI * r * r
	end
end
```

* Outer bindings aren’t visible unless passed in explicitly.
* Values are visible via property access: `m.area(3.0)`.
* Modules print as `<module: name>` in value formatting when available.

---

## 10) Annotations & Soft Errors

Annotations are your single, unified mechanism for documentation and soft signaling.

* **PRE**: lines beginning with `#` immediately above the construct/value.
* **POST**: trailing ` # …` that forces a newline and obeys the **after-separator** rule.

Soft failures are represented as **nullable types** returning `null` with a POST:

```mindscript
# Factorial with input check.
# Args: n:Int
# Return: Int? (annot-null if n < 0)
fun(n: Int) -> Int? do
	if n < 0 then
		return null # <negative input>
	end
	let res = 1
	let i = 2
	while i <= n do
		res = res * i
		i = i + 1
	end
	res
end
```

Guidelines:

* Prefer `T?` + annotated `null` over throwing for expected/uncertain conditions.
* Keep POST short and actionable. PRE is for human docs.

---

## 11) Equality, Comparison, Truthiness

* `==` / `!=` are **deep** for arrays and maps (map key order ignored).
  Numbers compare by value (`2 == 2.0` is true).
* `< <= > >=` only allow **number–number** or **string–string**; other mixes are **hard errors**.
* Booleans are **not** implicitly coerced from numbers/strings in these operators; use explicit conversion helpers if needed.

---

## 12) Standard Library (Practical Reference)

### Constants & Handles

* `E: 2.718281828459045` — Euler's number e.
* `PI: 3.141592653589793` — Mathematical constant π.
* `STDERR: <handle: file>` — Writable handle for the process standard error.
* `STDIN: <handle: file>` — Readable handle for the process standard input.
* `STDOUT: <handle: file>` — Writable handle for the process standard output.

### Introspection, Types & Runtime

* `snapshot: <fun: _:Null -> {}>` — Return a map snapshot of the visible environment (including built-ins).
* `typeOf: <fun: x:Any -> Type>` — Return the dynamic Type of a value.
* `isType: <fun: x:Any -> T:Type -> Bool>` — Check whether a value conforms to a Type.
* `isSubtype: <fun: A:Type -> B:Type -> Bool>` — Structural subtype test: A <: B.

### AST & Code Reflection

* `astParse: <fun: src:Str -> [Any]?>` — Parse source code into runtime-S (VTArray).
* `astValidate: <fun: ast:[Any] -> [Any]>` — Validate a runtime-S AST.
* `astEval: <fun: ast:[Any] -> Any>` — Evaluate a runtime-S AST in the caller's scope.
* `astFormat: <fun: ast:[Any] -> Str?>` — Format a runtime-S AST into stable source.
* `reflect: <fun: val:Any -> [Any]?>` — Reflect a value into constructor code (runtime-S).
* `reify: <fun: rt:[Any] -> Any>` — Decode and evaluate constructor code (runtime-S).
* `formatCode: <fun: src:Str -> Str?>` — Format source code.
* `formatValue: <fun: x:Any -> Str>` — Render a runtime value (with annotations).

### Serialization, Schemas & Encoding

* `jsonParse: <fun: s:Str -> Any>` — Parse a JSON string into MindScript values.
* `jsonStringify: <fun: x:Any -> Str?>` — Serialize a value to a compact JSON string.
* `jsonSchemaToType: <fun: schema:Any -> Type?>` — Convert a JSON Schema object to a MindScript Type.
* `jsonSchemaStringToType: <fun: src:Str -> Type>` — Parse a JSON Schema string and convert it to a MindScript Type.
* `typeToJSONSchema: <fun: t:Type -> Any>` — Convert a MindScript Type to a JSON Schema object.
* `typeStringToJSONSchema: <fun: src:Str -> Any>` — Parse a MindScript type string and convert it to JSON Schema.
* `base64Encode: <fun: x:Str -> Str>` — Base64-encode bytes from a string.
* `base64Decode: <fun: s:Str -> Str?>` — Decode a standard Base64 string.
* `hexEncode: <fun: x:Str -> Str>` — Hex-encode bytes to a lowercase hexadecimal string.
* `hexDecode: <fun: s:Str -> Str?>` — Decode a hexadecimal string.
* `gzipCompress: <fun: data:Str -> Str>` — Compress data using gzip (default level).
* `gzipDecompress: <fun: data:Str -> Str?>` — Decompress a gzip payload.

### Conversion & Length

* `bool: <fun: x:Any -> Bool>` — Convert to Bool using common "truthiness" rules.
* `int: <fun: x:Any -> Int?>` — Convert to Int when possible; otherwise return null.
* `num: <fun: x:Any -> Num?>` — Convert to Num when possible; otherwise return null.
* `str: <fun: x:Any -> Str?>` — Stringify a value.
* `len: <fun: x:Any -> Int?>` — Length of a value.

### Strings & Text

* `join: <fun: xs:[Str] -> sep:Str -> Str>` — Join strings with a separator.
* `split: <fun: s:Str -> sep:Str -> [Str]>` — Split a string on a separator (no regex).
* `match: <fun: pattern:Str -> string:Str -> [Str]>` — Find all non-overlapping matches of a regex.
* `replace: <fun: pattern:Str -> replace:Str -> string:Str -> Str>` — Replace all non-overlapping regex matches.
* `sprintf: <fun: fmt:Str -> args:[Any] -> Str?>` — Format a string with printf-style verbs.
* `printf: <fun: fmt:Str -> args:[Any] -> Str?>` — Print a formatted string to standard output.
* `substr: <fun: s:Str -> i:Int -> j:Int -> Str>` — Unicode-safe substring by rune index.
* `toLower: <fun: s:Str -> Str>` — Lowercase conversion (Unicode aware).
* `toUpper: <fun: s:Str -> Str>` — Uppercase conversion (Unicode aware).
* `strip: <fun: s:Str -> Str>` — Remove leading and trailing whitespace (Unicode).
* `lstrip: <fun: s:Str -> Str>` — Remove leading whitespace (Unicode).
* `rstrip: <fun: s:Str -> Str>` — Remove trailing whitespace (Unicode).
* `noteGet: <fun: x:Any -> Str?>` — Get the annotation attached to a value, if any.
* `noteSet: <fun: text:Str -> value:Any -> Any>` — Attach or replace an annotation on a value.

### Math

* `sin: <fun: x:Num -> Num>` — Sine of an angle in radians.
* `cos: <fun: x:Num -> Num>` — Cosine of an angle in radians.
* `tan: <fun: x:Num -> Num>` — Tangent of an angle in radians.
* `sqrt: <fun: x:Num -> Num>` — Square root.
* `pow: <fun: base:Num -> exp:Num -> Num>` — Power: base^exp.
* `exp: <fun: x:Num -> Num>` — Exponential function e^x.
* `log: <fun: x:Num -> Num>` — Natural logarithm (base e).

### Randomness & Cryptography

* `seedRand: <fun: n:Int -> Null>` — Seed the pseudo-random number generator.
* `randFloat: <fun: _:Null -> Num>` — Uniform random number in \[0.0, 1.0).
* `randInt: <fun: n:Int -> Int>` — Uniform random integer in \[0, n).
* `randBytes: <fun: n:Int -> Str?>` — Uniform cryptographically secure random bytes.
* `sha256: <fun: x:Str -> Str>` — SHA-256 digest (raw bytes).
* `hmacSha256: <fun: key:Str -> msg:Str -> Str>` — HMAC-SHA256 authentication tag (raw bytes).
* `ctEqual: <fun: a:Str -> b:Str -> Bool>` — Constant-time equality for byte strings.

### Time & Scheduling

* `dateNow: <fun: _:Null -> {}>` — Current local date/time components.
* `nowMillis: <fun: _:Null -> Int>` — Current wall-clock time in milliseconds since the Unix epoch.
* `nowNanos: <fun: _:Null -> Int>` — Current wall-clock time in nanoseconds since the Unix epoch.
* `sleep: <fun: ms:Int -> Null>` — Pause execution for a number of milliseconds.
* `ticker: <fun: ms:Int -> Any>` — Emit periodic ticks on a channel until closed.
* `timerAfter: <fun: ms:Int -> Any>` — Emit one tick after a delay, then close.
* `timeFormatRFC3339: <fun: millis:Int -> Str>` — Format a Unix-epoch timestamp (milliseconds) as RFC 3339 (UTC).
* `timeParseRFC3339: <fun: s:Str -> Int?>` — Parse an RFC 3339 timestamp into milliseconds since the Unix epoch.

### Channels (CSP-style)

* `chanOpen: <fun: cap:Int? -> Any>` — Create a new channel (buffered when cap>0).
* `chanClose: <fun: c:Any -> Null>` — Close a channel (idempotent).
* `chanSend: <fun: c:Any -> x:Any -> Null>` — Send a value on a channel (blocking).
* `chanRecv: <fun: c:Any -> Any>` — Receive a value from a channel (blocking).
* `chanTrySend: <fun: c:Any -> x:Any -> Bool>` — Attempt a non-blocking send on a channel.
* `chanTryRecv: <fun: c:Any -> {}>` — Attempt a non-blocking receive from a channel.

### Processes & Concurrency

* `procSpawn: <fun: f:Any -> Any>` — Run a function concurrently in an isolated process. Use procJoin/procCancel/procJoinAll/procJoinAny.
* `procCancel: <fun: p:Any -> Null>` — Request cooperative cancellation of a process (best effort).
* `procJoin: <fun: p:Any -> Any>` — Wait for a process to finish and return its result.
* `procJoinAll: <fun: ps:[Any] -> [Any]>` — Wait for all processes to finish and return their results in order.
* `procJoinAny: <fun: ps:[Any] -> {}>` — Wait for any process to finish; return its index and value.
* `try: <fun: f:Any -> {}>` — Run a function and capture hard failures.

### Processes & System Exec

* `exec: }?>` — Run an external program.
* `exit: <fun: code:Int? -> Null>` — Terminate the current process with an optional status code.

### Files, Paths & OS

* `cwd: <fun: _:Null -> Str?>` — Get the current working directory.
* `chdir: <fun: path:Str -> Bool>` — Change the current working directory.
* `dirList: <fun: path:Str -> [Str]?>` — List directory entries as an array of names.
* `mkdir: <fun: path:Str -> Bool>` — Create a directory (creating parents as needed).
* `open: <fun: path:Str -> mode:Enum["r", "w", "a", "rw"] -> Any>` — Open a file and return a handle.
* `readAll: <fun: h:Any -> Str?>` — Read all remaining bytes from a handle.
* `readN: <fun: h:Any -> n:Int -> Str?>` — Read up to n bytes from a handle.
* `readLine: <fun: h:Any -> Str?>` — Read one line from a handle (without the trailing newline).
* `write: <fun: h:Any -> s:Str -> Int?>` — Write a string to a file or network handle.
* `flush: <fun: h:Any -> Bool>` — Flush buffered output for a handle.
* `close: <fun: h:Any -> Bool>` — Close a file, network connection, or listener handle.
* `readFile: <fun: path:Str -> Str?>` — Read an entire file into a string.
* `writeFile: <fun: path:Str -> data:Str -> Int?>` — Write a string to a file (overwriting if it exists).
* `remove: <fun: path:Str -> Bool>` — Delete a file or an empty directory.
* `rename: <fun: old:Str -> new:Str -> Bool>` — Rename (move) a file or directory.
* `stat: <fun: path:Str -> {isDir!: Bool, modTimeMillis!: Int, mode!: Int, size!: Int}?>` — File status (like ls -l metadata).
* `pathBase: <fun: path:Str -> Str>` — Return the last element of a path (OS-specific).
* `pathDir: <fun: path:Str -> Str>` — Return all but the last element of a path.
* `pathExt: <fun: path:Str -> Str>` — Return the file extension (including the leading dot), or "" if none.
* `pathClean: <fun: path:Str -> Str>` — Clean a path by applying lexical simplifications.
* `pathJoin: <fun: parts:[Str] -> Str>` — Join path elements using the OS-specific separator.
* `tempDir: <fun: _:Null -> Str>` — Return the system temporary directory.

### Environment & Modules

* `osEnv: <fun: name:Str -> Str?>` — Read an environment variable.
* `osSetEnv: <fun: name:Str -> value:Str? -> Bool>` — Set or unset an environment variable.
* `import: <fun: path:Str -> Any>` — Load a module from filesystem or HTTP(S).
* `importCode: <fun: name:Str -> src:Str -> Any>` — Evaluate source text as a module in memory.

### Networking & HTTP

* `netListen: <fun: addr:Str -> Any>` — Listen on a TCP address "host\:port".
* `netAccept: <fun: l:Any -> Any>` — Accept one TCP connection from a listener.
* `netConnect: <fun: addr:Str -> Any>` — Open a TCP connection to "host\:port".
* `http: }?>` — Make an HTTP request (buffered).
* `httpStream: }?>` — Make an HTTP request (streaming).
* `urlParse: <fun: s:Str -> {}?>` — Parse a URL into components.
* `urlBuild: <fun: u:{} -> Str>` — Build a URL string from components.
* `urlQueryParse: <fun: s:Str -> {}?>` — Parse a URL query string into a map.
* `urlQueryString: <fun: q:{} -> Str>` — Serialize a query map to 'application/x-www-form-urlencoded'.

### Data Structures & Utilities

* `clone: <fun: x:Any -> Any>` — Clone a value (deep-copy).
* `mapHas: <fun: obj:{} -> key:Str -> Bool>` — Return true if a key exists in a map.
* `mapDelete: <fun: obj:{} -> key:Str -> {}>` — Delete a property from a map (in place).

---

If you want, I can splice this into the full manual (replacing the old §12), or generate a one-page cheatsheet of just the built-ins by category.


---

## 13) Collections & Iteration Details

* Arrays are 0-based; negative indices count from the end:

  ```mindscript
  let xs = [10, 20]
  xs[-1]     # => 20
  ```
* Maps are open-world; unknown keys are allowed and simply absent.

**Access**

```mindscript
obj.field        # property (quoted if needed)
arr[i]           # 0-based, negative from end
map["k"]         # explicit string key
```

Missing map keys or array out-of-bounds yield **soft** annotated null if the implementation chooses to surface it; idiomatic code uses `T?` and checks.

---

## 14) Formatting Policy (Pretty & Standard)

* Minimal parentheses based on fixed precedence.
* No space before `(` in calls and parameter lists (`f(x)`, `fun(x)`).
* Control keywords render without parens:

  ```
  return expr
  break
  continue
  ```
* Arrays/maps prefer single-line if they fit a width budget; otherwise multi-line with indentation.
* When a POST ends the **last** element/entry line, the closing `]`/`}` appears on the **next line** and no extra blank line is inserted.

---

## 15) Error Handling

* Use **hard errors** (`fail`) for programmer mistakes or non-recoverable conditions. Catch with `try`.
* Use **soft errors** (nullable types + annotations) when uncertainty or absence is expected.

```mindscript
# Extract host from URL; soft-null if invalid.
fun host(url: Str) -> Str? do
	let u = urlParse(url)
	if u == null then
		return null # <invalid url>
	end
	u.host
end
```

---

## 16) Worked Examples

### 16.1 Sum array

```mindscript
# Sum an array of Ints.
fun sum(xs: [Int]) -> Int do
	let acc = 0
	for x in xs do
		acc = acc + x
	end
	acc
end
```

### 16.2 Map iteration with destructuring

```mindscript
for [k, v] in { a: 1, b: 2 } do
	printf("%s=%d\n", [k, v])
end
```

### 16.3 Destructuring patterns

```mindscript
let [a, b] = [1, 2, 3]   # a=1, b=2
let { name: n, age: a } = { name: "Ada" }  # n="Ada", a=null
```

### 16.4 If / elif / else as an expression

```mindscript
let msg =
	if ok then
		"yes"
	elif maybe then
		"maybe"
	else
		"no"
	end
```

### 16.5 Oracle with typed output

```mindscript
# Choose a color (may soft-fail).
oracle() -> Enum[ "red", "green", "blue" ]

let c = chooseColor()
if c == null then
	"no color" # <oracle unavailable>
else
	c
end
```

---

## 17) Design Guidelines

* **Public APIs**: declare explicit parameter and return types. Use `T?` only when soft errors are possible. Avoid exposing raw `null` in public contracts.
* **Annotations**: use PRE for docs; use concise POST for machine-readable signals and pipeline logs.
* **Canonicalization**: omit default `: Any` parameters and `-> Any` returns in source; formatters remove redundant parens and trailing commas, normalize quoting, and sort map keys in type/value printers for stability.

---

## 18) Quick Reference (Cheat Sheet)

```mindscript
# Declaration & assignment
let x = 1
x = x + 1

# Arrays & maps
let xs = [1, 2, 3]
let m  = { name: "Ada", age: 36 }

# Access
xs[0]; xs[-1]
m.name; m["not-ident"]

# Functions
fun inc(n: Int) -> Int do n + 1 end
inc(41)

# Control
if cond then A() elif other then B() else C() end
while test() do step() end
for x in xs do use(x) end

# Return/break/continue
return
break "done"
continue

# Types
let Pair = type { left!: Int, right!: Int }
let F = type Int -> Num -> Str

# Annotations
# PRE above
value # POST here forces newline
```

---

**End of Manual.**
