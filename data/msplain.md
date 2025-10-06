Below is the **updated & improved** manual. I kept the exact section structure and folded in all the language pitfalls you highlighted, with focused “good/bad” examples where they help most (no new sections added).

---

# MindScript Language Manual (Human-Oriented)

MindScript is a compact, expression-oriented language for two kinds of computation: deductive (ordinary, deterministic functions) and inductive (first-class oracles backed by models or external sources). It pairs formal, structural types with informal annotations (human text that guides oracles) while keeping syntax clean and readable.

---

## 1) Core Ideas

* **Everything is an expression.** Blocks, conditionals, loops, and declarations evaluate to a value (often the last subexpression).
* **Strict left-to-right evaluation.** Receivers, operands, and arguments are evaluated left to right.
* **Structural types are first-class.** Types can be named, passed around, and introspected.
* **Two kinds of failure.**

  * **Errors** are ordinary values: idiomatically return `null` of a nullable type (`T?`) with a **POST** annotation that explains the reason.
  * **Panics** abort evaluation (lexer/parser/runtime error). Use `try` to recover. Panics are rare and should be avoided.

---

## 2) Lexical Conventions

### 2.1 Whitespace & indentation

* Indentation uses **tabs** (not spaces) for canonical formatting.
* Canonicalized output ends with exactly **one** trailing newline.
* **No semicolons.** MindScript never uses `;` as a statement separator.

  ```mindscript
  # Good
  let x = 1
  x = x + 1

  # Bad (syntax error)
  let x = 1; x = x + 1
  ```

### 2.2 Comments & annotations

MindScript uses a single, unified comment system that also carries metadata:

* **PRE annotations (header comments)**: one or more lines starting with `#` immediately **above** the construct they describe.
* **POST annotations (inline trailing)**: ` # …` placed at the **end** of the line for a construct. POST **forces a newline** after it and is part of the value’s metadata.

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

**POST swallows to end-of-line (common pitfall):**

```mindscript
# Good
if x then "ok" else "bad" end

# Pitfall: POST ends the current line, so you still need 'end' on its own line.
if x then "ok" else "bad" # note: POST here forces newline
end
```

Annotations are **first-class metadata**: they are attached to values and **do not affect logic** (ignored by equality/subtyping), but can be read with `noteGet`.

---

## 3) Values

Primitive scalars:

* `null`, `true`, `false`
* Integers (**int64**): `0`, `-7`
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
Arrays: `[T]` ( **monomorphic**; not heterogeneous)
Open maps (string keys): `{ field!: T, other: U }` — `!` marks **required** fields; others are optional.
Enums (literal sets): `Enum[ lit1, lit2, ... ]` where literals can be scalars, arrays, or maps.
Functions & oracles: `(A, B) -> R` (right-associative; semantically curried).

**No general sum types.** Use `Any` (top type) or nullable `T?`. When the type is omitted, it is `Any`.

**Only values have a type.** Variables are untyped containers; **types apply to values** and to function/oracle signatures (checked at runtime).

**Types themselves are values** of type `Type`.

**Function values are curried.** A surface type `(A, B) -> R` is equivalent to `A -> B -> R`.

**Internal structure can be annotated** (use PRE/POST around fields to document semantics).

**Duck typing encouraged.** Constrain exactly what a function needs in its signature; the runtime checks conformance.

**Nullable & optional fields (guidance):**

* `{age: Int}` → optional field; when present must be `Int`.
* `{age: Int?}` → optional **or** `null` when present; use sparingly (omissible + nullable can be semantically muddy).
* Prefer either **optional** or **nullable**, not both at once, unless truly needed.

**Examples (with pitfalls)**

```mindscript
# Arrays are monomorphic
let ok: [Str] = ["a", "b"]
# Bad (heterogeneous): ["a", 1]

# Open maps; '!' means required
let Person = type { name!: Str, age: Int }

# Optional vs nullable
let A = type { note: Str }     # optional string (when present, must be Str)
let B = type { note: Str? }    # optional; when present can be Str or null  # use cautiously

# Function is curried (semantics)
let Add = type (Int, Int) -> Int   # == Int -> Int -> Int
```

**Subtyping (informal):**

* `Int <: Num`
* `T?` matches `null` or `T`
* Arrays covariant by element type
* Maps fieldwise; requiredness can only become **more** required (not less)
* Enums by member subset
* Functions/oracles: parameters **contravariant**, return **covariant**
* `Any` is top

---

## 5) Expressions & Operators

### 5.1 Precedence (high → low)

* Calls / indexing / property: `f(x)`, `a[i]`, `obj.field`
* Unary: `-x`, `not x`
* Binary arithmetic: `* / %`, then `+ -`
* Comparisons: `< <= > >=`
* Equality: `== !=` (deep for arrays/maps; number equality by value; `2 == 2.0` is true)
* Boolean: `and`, then `or`
* Assignment (`=`) is an expression with low precedence (rarely used in nested exprs; prefer statements).

**Rules**

* **Relational ops** only compare **number–number** (mixing `Int`/`Num` allowed) or **string–string**; other mixes **panic**.
* Arithmetic promotion happens **only when at least one operand is `Num`**; `/` yields a floating result (`Num`).
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

**Update** assigns to l-values (`name`, `obj.field`, `arr[i]`). Updating an unbound name **panics**:

```mindscript
let x = 1
x = 2
```

There are no named declarations. **Functions and types are anonymous**; to name them you **bind to a variable**:

```mindscript
let f = fun(n: Int) -> Int do n + 1 end
let T = type {name!: Str}
let m = module "m" do end
```

**Sugar (surface syntax only):**

* Omitted parameter types and return types default to `Any`.
* Calling with no arguments `f()` lowers to `f(null)`.
* Omitted payloads for `return` / `break` / `continue` lower to `null`.

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
# Good (always close with 'end')
if cond then
	doSomething()
elif other then
	handleOther()
else
	default()
end

# Bad (missing 'end' → syntax error)
if cond then
	doSomething()
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

* **Prefer explicit parameter and return types** in public APIs.

  ```mindscript
  # Good
  fun echo(x: Str) -> Str do x end

  # Discouraged (defaults to Any)
  fun echo(x) do x end
  ```
* Omissions default to `Any` in the surface grammar but are **not** printed by canonical formatters:

  * `fun(x) do ... end` means `x: Any`
  * `-> Any` may be omitted when returning `Any`
* Calls are **curried** semantically (right-associative):

  ```mindscript
  let add = fun(a: Int, b: Int) -> Int do a + b end
  let inc = add(1)   # inc : Int -> Int
  inc(41)            # => 42
  ```
* The **actual** returned value must conform to the declared return type; otherwise the runtime **panics**.

### 8.2 Oracles (typed generative calls)

An oracle behaves like a function with **typed inputs** and an **uncertain** output: calls never panic; instead they produce a value of `R?`.

```mindscript
# Pick a primary color.
# Return: Enum[ "red", "green", "blue" ]?
oracle() -> Enum[ "red", "green", "blue" ]
```

**Calling rules**

* Argument arity/types are checked **strictly** (panic on mismatch).
* Output is treated as `R?`:

  * If the generated candidate conforms to `R`, it’s returned.
  * Otherwise or if unavailable, you get `null` with a trailing POST reason.

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
* After load, a module behaves like a map of exports **and is writable**: property sets overwrite exports.
* Modules print as `<module: name>` in value formatting when available.

---

## 10) Annotations & Errors

Annotations are your single, unified mechanism for documentation and error signaling.

* **PRE**: lines beginning with `#` immediately above the construct/value.
* **POST**: trailing ` # …` that forces a newline and obeys the **after-separator** rule.

**Idiomatic errors** are represented as **nullable types** returning `null` with a POST:

```mindscript
# Factorial with input check.
# Args: n:Int
# Return: Int?  # returns null # <negative input> on bad input
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

**Panics** are exceptional (lexer/parser/runtime) and abort execution. Use `try` to recover (see §12/§15 helpers).

Guidelines:

* Prefer `T?` + annotated `null` over panicking for expected/uncertain conditions.
* Keep POST short and actionable. PRE is for human docs.
* **Annotations are ignored** for equality and subtyping.

---

## 11) Equality, Comparison, Truthiness

* `==` / `!=` are **deep** for arrays and maps (map key order ignored). Numbers compare by value (`2 == 2.0` is true). **Annotations are ignored**.
* `< <= > >=` only allow **number–number** or **string–string**; other mixes **panic**.
* Boolean operators `and`/`or` require booleans (no implicit coercion). Use `bool(x)` to convert with truthiness rules (see §12).

---

## 12) Standard Library (Practical Reference)

> Conversions like `int/num/bool/str` **return `null` on failure**; attach a POST note if needed.

```mindscript
int("42")     # 42
int("oops")   # null # <invalid int>
```

Types below use MindScript notation; `?` indicates a nullable (error-capable) result.

```
E: 2.718281828459045 — Euler's number e.
PI: 3.141592653589793 — Mathematical constant π.
STDERR: <handle: file> — Writable handle for the process standard error.
STDIN: <handle: file> — Readable handle for the process standard input.
STDOUT: <handle: file> — Writable handle for the process standard output.
assert: <fun: cond:Bool -> Bool> — Assert that a condition holds.
astEval: <fun: ast:[Any] -> Any> — Evaluate a runtime-S AST in the caller's scope.
astFormat: <fun: ast:[Any] -> Str?> — Format a runtime-S AST into stable source.
astParse: <fun: src:Str -> [Any]?> — Parse source code into runtime-S (VTArray).
astValidate: <fun: ast:[Any] -> [Any]> — Validate a runtime-S AST.
base64Decode: <fun: s:Str -> Str?> — Decode a standard Base64 string.
base64Encode: <fun: x:Str -> Str> — Base64-encode bytes from a string.
bool: <fun: x:Any -> Bool?> — Convert to Bool using common "truthiness" rules; otherwise errs.
chanClose: <fun: c:Any -> Bool?> — Close a channel (idempotent).
chanOpen: <fun: cap:Int? -> Any> — Create a new channel for Values.
chanRecv: <fun: c:Any -> Any> — Receive a value from a channel (blocking).
chanSend: <fun: c:Any -> x:Any -> Bool?> — Send a value on a channel (blocking).
chanTryRecv: <fun: c:Any -> {}> — Attempt a non-blocking receive from a channel.
chanTrySend: <fun: c:Any -> x:Any -> Bool> — Attempt a non-blocking send on a channel.
chdir: <fun: path:Str -> Bool?> — Change the current working directory.
clone: <fun: x:Any -> Any> — Clone a value (deep-copy).
close: <fun: h:Any -> Bool?> — Close a file, network connection, or listener handle.
codeImport: <fun: code:Str -> Str -> {}> — Create an importer from a code string.
cos: <fun: x:Num -> Num> — Cosine of an angle in radians.
ctEqual: <fun: a:Str -> b:Str -> Bool> — Constant-time equality for byte strings.
cwd: <fun: _:Null -> Str?> — Get the current working directory.
dateNow: <fun: _:Null -> {}> — Current local date/time components.
dir: <fun: x:{} -> [Str]> — Directory listing of visible fields/functions.
dirList: <fun: path:Str -> [Str]?> — List directory entries as an array of names.
error: <fun: msg:Str -> Null> — Produce an annotated null (soft error).
exec: }?> — Run an external program.
exit: <fun: code:Int? -> Null> — Terminate the current process with an optional status code.
exp: <fun: x:Num -> Num> — Exponential function e^x.
filter: <fun: cond:(Any -> Bool) -> it:(Null -> Any?) -> Null -> Any?> — Filter an iterator (lazy predicate).
flush: <fun: h:Any -> Bool?> — Flush buffered output for a handle.
formatCode: <fun: src:Str -> Str?> — Format source code.
formatValue: <fun: x:Any -> Str> — Render a runtime value (with annotations).
gzipCompress: <fun: data:Str -> Str> — Compress data using gzip (default level).
gzipDecompress: <fun: data:Str -> Str?> — Decompress a gzip payload.
hexDecode: <fun: s:Str -> Str?> — Decode a hexadecimal string.
hexEncode: <fun: x:Str -> Str> — Hex-encode bytes to a lowercase hexadecimal string.
hmacSha256: <fun: key:Str -> msg:Str -> Str> — HMAC-SHA256 authentication tag (raw bytes).
http: }?> — Make an HTTP request (buffered).
httpStream: }?> — Make an HTTP request (streaming).
import: <fun: path:Str -> Any> — Load a module from filesystem or HTTP(S).
importCode: <fun: name:Str -> src:Str -> Any> — Evaluate source text as a module in memory.
importUrl: <fun: url:Str -> {}?> — Import a module from a URL.
int: <fun: x:Any -> Int?> — Convert to Int when possible; otherwise errs.
isSubtype: <fun: A:Type -> B:Type -> Bool> — Structural subtype test: A <: B.
isType: <fun: x:Any -> T:Type -> Bool> — Check whether a value conforms to a Type.
iter: <fun: v:Any -> Null -> Any?> — Turn arrays or maps into (Null -> Any?) iterators; pass iterators through unchanged.
join: <fun: xs:[Str] -> sep:Str -> Str> — Join strings with a separator.
jsonParse: <fun: s:Str -> Any> — Parse a JSON string into MindScript values.
jsonRepair: <fun: s:Str -> Any> — Repair and parse messy JSON into MindScript values.
jsonSchemaStringToType: <fun: src:Str -> Type> — Parse a JSON Schema string and convert it to a MindScript Type.
jsonSchemaToType: <fun: schema:Any -> Type?> — Convert a JSON Schema object to a MindScript Type.
jsonStringify: <fun: x:Any -> Str?> — Serialize a value to a compact JSON string.
keys: <fun: obj:{} -> Null -> Str?> — Iterator over keys of an object.
len: <fun: x:Any -> Int?> — Length of a value.
list: <fun: it:(Null -> Any?) -> [Any]> — Collect an iterator into an array.
log: <fun: x:Num -> Num> — Natural logarithm (base e).
lstrip: <fun: s:Str -> Str> — Remove leading whitespace (Unicode).
map: <fun: f:(Any -> Any) -> it:(Null -> Any?) -> Null -> Any?> — Map over an iterator (lazy transform).
mapDelete: <fun: obj:{} -> key:Str -> {}> — Delete a property from a map (in place).
mapHas: <fun: obj:{} -> key:Str -> Bool> — Return true if a key exists in a map.
match: <fun: pattern:Str -> string:Str -> [Str]> — Find all non-overlapping matches of a regex.
mkdir: <fun: path:Str -> Bool?> — Create a directory (creating parents as needed).
mute: <fun: _:Any -> Null> — Mute (sink) a value.
naturals: <fun: _:Null -> Null -> Int> — Infinite iterator: 1, 2, 3, ...
naturals0: <fun: _:Null -> Null -> Int> — Infinite iterator: 0, 1, 2, ...
netAccept: <fun: l:Any -> Any> — Accept one TCP connection from a listener.
netConnect: <fun: addr:Str -> Any> — Open a TCP connection to "host:port".
netListen: <fun: addr:Str -> Any> — Listen on a TCP address "host:port".
noteGet: <fun: x:Any -> Str?> — Get the annotation attached to a value, if any.
noteSet: <fun: text:Str -> value:Any -> Any> — Attach or replace an annotation on a value.
nowMillis: <fun: _:Null -> Int> — Current wall-clock time in milliseconds since the Unix epoch.
nowNanos: <fun: _:Null -> Int> — Current wall-clock time in nanoseconds since the Unix epoch.
num: <fun: x:Any -> Num?> — Convert to Num when possible; otherwise errs.
open: <fun: path:Str -> mode:Enum["r", "w", "a", "rw"] -> Any?> — Open a file and return a handle.
oracleHealth: <fun: _:Null -> {}?> — Quick oracle health check (real call, tiny).
oracleInstall: <fun: exec:(Str -> Str?) -> Bool> — Install a new global oracle executor.
oracleInstallWithTap: <fun: exec:(Str -> Str?) -> Bool> — Install an oracle executor that records each prompt before forwarding.
oracleLastPrompt: <fun: _:Null -> Str?> — Return the most recently recorded oracle prompt (if any).
oracleLog: <fun: _:Null -> [[Str]]> — Return the full recorded oracle prompt/output log.
oracleStatus: <fun: _:Null -> Str> — Show oracle installation status.
osEnv: <fun: name:Str -> Str?> — Read an environment variable.
osSetEnv: <fun: name:Str -> value:Str? -> Bool?> — Set or unset an environment variable.
panic: <fun: message:Str? -> Null> — Fail: throw a runtime error (hard fault).
pathBase: <fun: path:Str -> Str> — Return the last element of a path (OS-specific).
pathClean: <fun: path:Str -> Str> — Clean a path by applying lexical simplifications.
pathDir: <fun: path:Str -> Str> — Return all but the last element of a path.
pathExt: <fun: path:Str -> Str> — Return the file extension (including the leading dot), or "" if none.
pathJoin: <fun: parts:[Str] -> Str> — Join path elements using the OS-specific separator.
pop: <fun: arr:[Any] -> Any> — Remove and return the last element of an array.
pow: <fun: base:Num -> exp:Num -> Num> — Power: base^exp.
print: <fun: x:Any -> Any> — Print a value and return it.
printf: <fun: fmt:Str -> args:[Any] -> Str?> — Print a formatted string to standard output.
println: <fun: x:Any -> Any> — Print a value with newline and return it.
procCancel: <fun: p:Any -> Bool?> — Request cooperative cancellation of a process (best effort).
procCancelled: <fun: p:Any -> Bool> — Check whether cancellation was requested for a process.
procJoin: <fun: p:Any -> Any> — Wait for a process to finish and return its result.
procJoinAll: <fun: ps:[Any] -> [Any]> — Wait for all processes to finish and return their results in order.
procJoinAny: <fun: ps:[Any] -> {}?> — Wait for any process to finish; return its index and value.
procSpawn: <fun: f:Any -> Any> — Run a function concurrently in an isolated process (clone of the current interpreter).
push: <fun: arr:[Any] -> v:Any -> [Any]> — Append an element to an array (in place).
randBytes: <fun: n:Int -> Str?> — Uniform cryptographically secure random bytes.
randFloat: <fun: _:Null -> Num> — Uniform random number in [0.0, 1.0).
randInt: <fun: n:Int -> Int> — Uniform random integer in [0, n).
range: <fun: start:Int -> stop:Int? -> Null -> Int?> — Integer range iterator (stop exclusive; infinite if stop is null).
readAll: <fun: h:Any -> Str?> — Read all remaining bytes from a handle.
readFile: <fun: path:Str -> Str?> — Read an entire file into a string.
readLine: <fun: h:Any -> Str?> — Read one line from a handle (without the trailing newline).
readN: <fun: h:Any -> n:Int -> Str?> — Read up to n bytes from a handle.
reduce: <fun: f:((Any, Any) -> Any) -> it:(Null -> Any?) -> Any?> — Fold an iterator with a binary function.
reflect: <fun: val:Any -> [Any]?> — Reflect a value into constructor code (runtime-S).
reify: <fun: rt:[Any] -> Any> — Decode and evaluate constructor code (runtime-S).
remove: <fun: path:Str -> Bool?> — Delete a file or an empty directory.
rename: <fun: old:Str -> new:Str -> Bool?> — Rename (move) a file or directory.
replace: <fun: pattern:Str -> replace:Str -> string:Str -> Str> — Replace all non-overlapping regex matches.
rstrip: <fun: s:Str -> Str> — Remove trailing whitespace (Unicode).
runtime: } — 
seedRand: <fun: n:Int -> Null> — Seed the pseudo-random number generator.
sha256: <fun: x:Str -> Str> — SHA-256 digest (raw bytes).
shift: <fun: arr:[Any] -> Any> — Remove and return the first element of an array.
sin: <fun: x:Num -> Num> — Sine of an angle in radians.
sleep: <fun: ms:Int -> Null> — Pause execution for a number of milliseconds.
slice: <fun: xs:[Any] -> s:Int -> e:Int -> [Any]> — Slice an array [s, e).
snapshot: <fun: _:Null -> {}> — Return a map snapshot of the visible environment (including built-ins).
sort: <fun: arr:[Any] -> cmp:((Any, Any) -> Int) -> [Any]> — Sort an array in-place using a comparison function.
split: <fun: s:Str -> sep:Str -> [Str]> — Split a string on a separator (no regex).
sprintf: <fun: fmt:Str -> args:[Any] -> Str?> — Format a string with printf-style verbs.
sqrt: <fun: x:Num -> Num> — Square root.
stat: <fun: path:Str -> {isDir!: Bool, modTimeMillis!: Int, mode!: Int, size!: Int}?> — File status (like ls -l metadata).
str: <fun: x:Any -> Str?> — Convert to string if possible; otherwise err.
strip: <fun: s:Str -> Str> — Remove leading and trailing whitespace (Unicode).
substr: <fun: s:Str -> i:Int -> j:Int -> Str> — Unicode-safe substring by rune index.
tan: <fun: x:Num -> Num> — Tangent of an angle in radians.
tempDir: <fun: _:Null -> Str> — Return the system temporary directory.
ticker: <fun: ms:Int -> Any> — Create a channel that emits periodic ticks until closed by the consumer.
timeFormatRFC3339: <fun: millis:Int -> Str> — Format a Unix-epoch timestamp (milliseconds) as RFC 3339 (UTC).
timeParseRFC3339: <fun: s:Str -> Int?> — Parse an RFC 3339 timestamp into milliseconds since the Unix epoch.
timerAfter: <fun: ms:Int -> Any> — Create a channel that emits one tick after a delay, then closes.
toLower: <fun: s:Str -> Str> — Lowercase conversion (Unicode aware).
toUpper: <fun: s:Str -> Str> — Uppercase conversion (Unicode aware).
try: <fun: f:(Null -> Any) -> {ok!: Bool, value!: Any}> — Run a zero-arg function and capture panics.
typeOf: <fun: x:Any -> Type> — Return the dynamic Type of a value.
typeStringToJSONSchema: <fun: src:Str -> Any> — Parse a MindScript type string and convert it to JSON Schema.
typeToJSONSchema: <fun: t:Type -> Any> — Convert a MindScript Type to a JSON Schema object.
uid: <fun: value:Any -> Int> — Stable-ish integer id for a value’s content.
unshift: <fun: arr:[Any] -> v:Any -> [Any]> — Prepend an element to an array (in place).
urlBuild: <fun: u:{} -> Str> — Build a URL string from components.
urlParse: <fun: s:Str -> {}?> — Parse a URL into components.
urlQueryParse: <fun: s:Str -> {}?> — Parse a URL query string into a map.
urlQueryString: <fun: q:{} -> Str> — Serialize a query map to 'application/x-www-form-urlencoded'.
values: <fun: obj:{} -> Null -> Any?> — Iterator over values of an object.
write: <fun: h:Any -> s:Str -> Int?> — Write a string to a file or network handle.
writeFile: <fun: path:Str -> data:Str -> Int?> — Write a string to a file (overwriting if it exists).
```

---

## 13) Collections & Access Semantics

* Arrays are 0-based; negative indices count from the end:

  ```mindscript
  let xs = [10, 20]
  xs[-1]     # => 20
  ```
* Maps are open-world; unknown keys are allowed and simply absent from the value until set. Map addition merges: `{a: 0, b: 1} + {a: 1, c: 2}` => `{a: 1, b: 1, c: 2}`

**Access**

```mindscript
obj.field        # property (quoted if needed)
arr[i]           # 0-based, negative from end
map["k"]         # explicit string key
```

**Missing key / out-of-bounds:** **panic**. Use `mapHas`, bounds checks, or safe wrappers to avoid panics when absence is expected.

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

## 15) Failure Handling (Errors vs Panics)

* Use **errors** (`T?` + POST-annotated `null`) for expected/uncertain conditions.
* Reserve **panics** for programmer mistakes or unrecoverable runtime conditions (type/arity violations, out-of-bounds, missing key without check, invalid comparisons, use of unbound names, etc.). Panics can be caught with `try`.

```mindscript
# Extract host from URL; returns error (null) if invalid.
fun host(url: Str) -> Str? do
	let u = urlParse(url)
	if u == null then
		return null # <invalid url>
	end
	u.host
end

# Recovering from a panic:
let r = try(fun() do
	fail("boom")
end)
# r == { ok: false, value: null, error: "boom" }
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
# Choose a color (may return error/null).
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

* **Public APIs**: declare explicit parameter and return types. Use `T?` only when an error/null is a legitimate outcome. Avoid exposing raw `null` without a type contract.
* **Annotations**: use PRE for docs; use concise POST for machine-readable signals and pipeline logs.
* **Canonicalization**: omit default `: Any` parameters and `-> Any` returns in source; formatters remove redundant parens and trailing commas, normalize quoting, and sort map keys in type/value printers for stability.
* **Avoid panics** in library code; prefer returning `null` with a clear POST reason.
* **Favor duck typing**: function signatures should state only what’s required; rely on structural checks at call time.

---

## 18) Quick Reference (Cheat Sheet)

```mindscript
# Declaration & assignment (no semicolons)
let x = 1
x = x + 1

# Arrays & maps (arrays are monomorphic)
let xs = [1, 2, 3]
let m  = { name: "Ada", age: 36 }

# Access (panics on missing key / OOB)
xs[0]; xs[-1]
m.name; m["not-ident"]

# Functions (prefer explicit types; semantically curried)
fun inc(n: Int) -> Int do n + 1 end
let add = fun(a: Int, b: Int) -> Int do a + b end
add(1)(41)  # 42

# Control (always close with 'end')
if cond then A() elif other then B() else C() end

# Return/break/continue
return          # lowers to return null
break "done"
continue        # lowers to continue null

# Types
let Pair = type { left!: Int, right!: Int }
let F = type Int -> Num -> Str
let Person = type { name!: Str, age: Int? }  # avoid omissible+nullable unless needed

# Oracles
oracle getColor() -> Enum["red","green","blue"]

# Annotations
# PRE above
value # POST here forces newline

# Errors vs Panics
return null # <reason>     # error (preferred for expected absence)
fail("boom")               # panic (rare; recoverable via try)
```

---

**End of Manual.**
