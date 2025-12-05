# MindScript Language Manual (Human-Oriented) — Revised

  

MindScript is a compact, expression-oriented language for two kinds of computation: **deductive** (deterministic functions) and **inductive** (first-class oracles powered by models or external sources). It pairs **formal, structural types** with **informal annotations** (human text that guides oracles) while keeping syntax clean and readable.

Canonical output ends with exactly **one** trailing newline.
 

## 1) Core Ideas

*  **Everything is an expression.** Blocks, conditionals, loops, and declarations evaluate to a value (usually the last sub-expression).
*  **Strict left-to-right evaluation.** Receivers, operands, and arguments evaluate left→right.
*  **Structural types are first-class.** Types can be named, passed around, and introspected.
*  **Two kinds of failure:**
	*  **Errors** are values: idiomatically return `null` of a nullable type (`T?`) usually with an **annotation** explaining the reason.
	*  **Panics** abort evaluation (lexer/parser/runtime). Use `try` to recover; panics should be rare.

## 2) Lexical Conventions

### 2.1 Whitespace & indentation

* Indentation uses **tabs**, not spaces. The **canonical formatter converts spaces → tabs**.
* Semicolons `;` are lowered to newlines `\n`.

```mindscript

# Good
let x = 1
x = x + 1

# Or
let x = 1; x = x + 1
```

### 2.2 Comments & annotations

The values in MindScript can be annotated with a comment. These annotations are metadata, and they can influence the execution of **oracles** (see below). Annotation always attach to values.

An **inline annotation** is a trailing single-line comment `EXPR # text\n`, on the same line as the expression it annotates.

```mindscript
let c = 300_000_000 # The speed of light.

let p = {
	age: 17 # The age of the person.
}
``` 

An **annotation** is a single- or multi-line header comment block, annotating the next value expression. 

```mindscript
# sum of natural numbers
# N: largest number
let nPairs = fun(N: Int) -> Int do N*(N+1)/2 end
```

A **free-floating annotation** is a single- or multi-line comment block following by one or more blank lines. (Blank lines create a special **noop** value ignored by the interpreter.)
```mindscript
# We set the last value.

let last = 1
```
**Annotation propagation:** Annotations are attached to the specific value and do not propagate, except in reference assignments (ordinary assignment), cloning, and structural updates (mutating part of a structure). Annotations are first-class metadata and do not affect logic (ignored by equality/subtyping), but can influence oracle execution; access with `noteGet`.

## 3) Values
Primitives:
* Null: `null`
* Boolean: `true`, `false`
* Integers (int64): `0`, `-7`
* Numbers (float64): `2.0`, `1e-3`
* Strings: `"hello\nworld"`

Composite:
* Arrays: `[a, b, c]`
* Maps: `{ key: value, "not-ident": 1 }` (identifier-like keys are bare; others quoted)

## 4) Types (Structural & First-Class)

Base: `Any | Null | Bool | Int | Num | Str | Handle | Type`
Nullable: `T?`
Arrays: `[T]` (monomorphic)
Open maps (string keys): `{ field!: T, other: U }` — `!` marks **required**; others optional
Enums (literal sets): `Enum[ lit1, lit2, ... ]` (literals may be scalars, arrays, or maps)
Functions/oracles: `(A, B) -> R` (right-associative; semantically curried)

**Handles (overview):**
* **What:** Opaque, capability-carrying values for external resources (files, sockets, etc.).
* **Type form:** `Handle.<kind>` (e.g., `Handle.file`, `Handle.netConn`). Avoid using `Handle`. Every handle kind is a distinct nominal type, e.g. `Handle.file` and `Handle.netConn` never subtype or unify with each other; both only subtype `Any`.
* **Creation & lifecycle:** created by APIs like `open` / `netConnect` / `netListen`; then use `read*/write/flush/...`; finally `close`.
* **Discovery:** printed as `<handle: kind>`.

**No general sum types.** Use `Any` or nullable `T?`. Omitted types default to `Any`.
**Only values have a type.** Variables are untyped containers; types apply to **values** and to function/oracle **signatures** (checked at runtime).
**Types themselves are values** of type `Type`.
**Function values are curried.**  `(A, B) -> R` ≡ `A -> B -> R`.
**Internal structure can be annotated** (use annotations around fields for docs).
**Duck typing encouraged.** Constrain only what your function needs; runtime checks conformance.
**Nullable & optional guidance**
* `{age!: Int}` → mandatory field of type `Int`.
*  `{age: Int}` → optional field; when present must be `Int`.
*  `{age: Int?}` → optional **or**  `null` when present; use sparingly.

**Enums & mixed literal types**
* Enum members are **just literals**. If members differ in type, the member type is `Any` (unless they are all of type `T` except some `null`s → then members are `T?`).

**Subtyping (informal)**
*  `Int <: Num`
*  `T?` matches `null` or `T`
*  **Handles are nominal by kind.** `Handle.file` only matches file handles; kinds do **not** subtype/unify structurally.
* Arrays are covariant by element type
* Maps fieldwise; **requiredness can only become more required** in **type–type comparison** (e.g., supertype `x: T` → subtype may demand `x!: T`). This rule is **not** used by `isType` on values (values don’t carry requiredness/nullability flags).
* Enums by member subset, .e.g `Enum["a"] <: Enum["a","b"] <: Str <: Any`.
* Functions/oracles: parameters **contravariant**, return **covariant**
*  `Any` is top

**Examples**
```mindscript

# Arrays are monomorphic, so this is type [Str]
let ok = ["a", "b"]

# Unifies to [Any]: 
["a", 1]

# Open maps; '!' means required
let Person = type { name!: Str, age: Int }

# Optional vs nullable

let A = type { note: Str } # optional string (when present, must be Str)
let B = type { note: Str? } # optional; when present can be Str or null

# Function is curried (semantics)

let Add = type Int -> Int -> Int # e.g. fun(a: Int, b: Int) -> Int do ... end
```

## 5) Expressions & Operators

### 5.1 Precedence (high → low)

* Calls / indexing / property: `f(x)`, `a[i]`, `obj.field`
* Exponentiation: `**`  _(right-associative)_
* Unary: `-x`, `not x`, `~x`
* Binary arithmetic: `* / %`, then `+ -`
* Shifts: `<< >>`
* Bitwise: `&`, then `^`, then `|`
* Comparisons: `< <= > >=`
* Equality: `== !=` (deep for arrays/maps; number equality by value; `2 == 2.0` true)
* Boolean: `and`, then `or`
* Declaration (`let`) to initialize variables
* Assignment (`=`) is an expression with low precedence

**Rules & details**
*  **Relational ops** only compare **number–number** (mixing `Int`/`Num` allowed) or **string–string**; other mixes **panic**.
*  **Arithmetic promotion**: only when at least one operand is `Num`.
*  `/` with **Int/Int yields Int** (e.g., `1/2 == 0`), otherwise yields `Num`.
*  `%` follows the **dividend** sign. Examples: `3%3==0; 3%2==1; 3%1==0; 3%0` is error; `3%-1==0; 3%-2==1; 3%-3==0`.
*  **Assignment is an expression**; `let x = 0` evaluates to `0`.
*  **Exponentiation `**`**:
   * Right-associative: `2 ** 3 ** 2 == 2 ** (3 ** 2)`.
   * Numeric only. `Int ** Int` returns `Int` when the exact result fits in 64-bit; otherwise returns `Num`. If either operand is `Num`, result is `Num`.
   * Domain errors (e.g., negative base with non-integer exponent) **panic**.
*  **Bitwise ops** (`& | ^ ~ << >>`):
   * Operands must be `Int`; otherwise **panic**.
   * `~` is unary bitwise NOT (two’s-complement on 64-bit).
   * `& | ^` are standard bitwise and/or/xor.
   * `<< >>` are shifts on 64-bit signed integers; right shift is arithmetic.
   * Shift count must be `0..63`; negative or ≥64 **panic**.

**Examples**
```mindscript
2 + 2.5
not (a < 3 and b == 0)
x = y + 1
obj.field[0].sub(42)
```

## 6) Variables, Patterns & Assignment

Identifiers: `x`, `_tmp`, `Name_2`

**Declaration** uses `let` with a **pattern**:
* Single name: `let x = 10`
* Array destructuring (missing entries bind to `null`; extra items ignored unless matched):
```mindscript
let [a, b] = [1, 2, 3] # a=1, b=2
```
* Object destructuring (missing keys bind to `null`):
```mindscript
let { name: n, age: a } = { name: "Ada" } # n="Ada", a=null
```

**Update** assigns to l-values (`name`, `obj.field`, `arr[i]`). Updating an unbound name **panics**.
```mindscript
let x = 1
x = 2
```

There are **no named declarations** like `fun myfunction() do ... end`. **Functions and types are anonymous**; to name them, **bind** with `let`:

```mindscript
let f = fun(n: Int) -> Int do n + 1 end
let T = type {name!: Str}
let m = module "m" do end
```

**Sugar (surface syntax):**
* Omitted parameter/return types default to `Any`.
* Calling with no arguments `f()` lowers to `f(null)`.
* Omitted payloads for `return` / `break` / `continue` lower to `null`.

## 7) Control Flow

### 7.1 Blocks

A block returns the last subexpression (or `null` if empty):
```mindscript
do
  1
  2
end # => 2
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
```

Iterating a map yields `[key, value]` pairs:
```mindscript
for [k, v] in { a: 1, b: 2 } do
	printf("%s=%d\n", [k, v])
end
```

Iterators are closures that return `null` to stop iteration:
```mindscript
let val = 0
let upTo10 = fun() do
	val = val + 1
	if val == 11 then null else val end
end

for n in upTo10 do println(n) end
```


### 7.5 Early exits

`return EXPR`, `break EXPR`, `continue EXPR`, each carries a value. If omitted,  `null` payload is assumed.
```mindscript
while true do
	break "done"
end
```

## 8) Functions & Oracles

### 8.1 Functions
```mindscript
# Increment an Int by 1.
# Args: n:Int
# Return: Int
let inc = fun(n: Int) -> Int do
	n + 1
end
```

* Prefer **explicit parameter and return types** in public APIs.
* Semantically **curried** (right-associative). **Call sugar**: `add(1, 2)` ≡ `add(1)(2)`.

**Overflow arity** panics: `add(1, 2, 3)` → panic.
* Return value must conform to declared return type or the runtime **panics**.

### 8.2 Oracles (typed inductive calls)

**What oracles are:** Oracles are **first-class, LLM-backed “black-box” functions** with **typed inputs** and **uncertain outputs**. You declare their shape (types), provide **annotations** as task guidance, and optionally supply **examples**; the runtime constructs a prompt that includes input/output schemas and your hints, asking the LLM to return a value conforming to the output type.

**Basic declaration:** oracles don't have a body.
```mindscript
# Write the name of an important researcher in the given field.
let researcher = oracle(field: Str) -> { name!: Str }
```

**Use examples to guide execution (optional):**
```mindscript
let pairs = [
	[0, "zero"],
	[1, "one"]
]

# Say the English word for a number.
let number2word = oracle(n: Int) -> Str from pairs
```

**Calling rules (language semantics):**
*  **Arguments:** strict type-check; mismatch **panics**.
*  **Result type:** automatically made nullable by the interpreter (`R` -> `R?`).
* If the candidate conforms to `R`, you get that value.
* Otherwise (or unavailable), you get `null`, optionally with an annotated reason (plain text).

**Example with nullable result:**
```mindscript
# Return a primary color (may fail; returns null with reason).
let pickColor = oracle() -> Enum["red","green","blue"]

let c = pickColor()
if c == null then
	"no color" # oracle unavailable
else
	c
end
```

**Example with human-readable error reason (no angle brackets):**
```mindscript
# Return a valid ISO 3166-1 alpha-2 country code for a given name.
# Return: Str?, returns null when unrecognized country.
let countryCode = oracle(name: Str) -> Str

let cc = countryCode("Wakanda")
if cc == null then
	"no code" # unrecognized country
else
	cc
end
```

> Implementation note (non-normative): The runtime’s oracle prompt building uses your type signatures (as schemas), PRE annotations, and optional examples to steer the model, aiming for schema-conformant outputs. ([mindscript.daios.ai][1])


## 9) Modules

Modules encapsulate a namespace and evaluate to a **module value**. The module has **its own environment**, and that environment is **visible as a map** from outside. A binding like `let x = 0` inside module `m` becomes visible as `m.x`. Modules are a **per-import copy**. Writes are confined to the **current runtime** only. `dir(m)` iterates the module (viewed as a map) to build a convenient listing.

**Inline module (rare)**
```mindscript
let m = module "mathx" do
	let PI2 = 2.0 * PI
	let area = fun(r: Num) -> Num do
		PI * r * r
	end
end

m.area(3.0)
```

**Normal module**: Python-like, define in another file, and import, without wrapping into a `module` block.
Eg. `mathx.ms`
```
let PI2 = 2.0 * PI
let area = fun(r: Num) -> Num do
	PI * r * r
end
```
Then, import as
```
let m = import("mathx")
m.PI2
```

## 10) Annotations & Errors

Errors are values. Idiomatically we indicate that a function can fail by making the output nullable, e.g. `Int?`, `{name: Str}?` or `Any` (the supertype). To provide a textual reason, annotate the `null` value.

```mindscript
# Returns input if between 0 and 5 (inclusive).
# Args: n:Int
# Return: Int?
let between0and5 = fun(n: Int) -> Int? do
	if n < 0 then
		return null # number is negative
	elif n > 5 then
		return null # number is larger than 5
	end
	n
end
```

**Panics** are exceptional (lexer/parser/runtime) and abort execution. Use `try` to recover (see §12/§15).

Guidelines:
* Prefer `T?` + annotated `null` over panicking for expected/uncertain conditions.
* Never use the content of the annotation for the logic - it's unidiomatic.

## 11) Equality, Comparison, Truthiness

*  `==` / `!=` are **deep** for arrays/maps (map key order ignored). Numbers compare by value (`2 == 2.0` is true). * Annotations are ignored.
* Handles compare by identity (two distinct handles are not equal, even if they refer to the same OS resource).
*  `< <= > >=` allow only **number–number** or **string–string**; other mixes **panic**.
* Boolean operators `and` / `or` require booleans (no implicit coercion).
* Only `false` is false.
*  `bool(x)` converts using truthiness rules; `bool(function)` or `bool(type)` returns `null`.

## 12) Standard Library (Practical Reference)

> Conversions like `int/num/bool/str`  **return `null` on failure**; attach a POST reason if helpful.

(Shown here in MindScript notation; `?` indicates nullable/soft-error results.)
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
exec: <fun: cmd:[Str] -> opts:{cwd: Str, env: {}, stdin: Str}? -> {status!: Int, stderr!: Str, stdout!: Str}?> — Run an external program.
exit: <fun: code:Int? -> Null> — Terminate the current process with an optional status code.
exp: <fun: x:Num -> Num> — Exponential function e^x.
ffiOpen: <fun: spec:Any -> Any> — Open a shared library (ELF/SysV) and return an FFI module.
filter: <fun: cond:(Any -> Bool) -> it:(Null -> Any?) -> Null -> Any?> — Filter an iterator (lazy predicate).
flush: <fun: h:Any -> Bool?> — Flush buffered output for a handle.
formatCode: <fun: src:Str -> Str?> — Format source code.
formatValue: <fun: x:Any -> Str> — Render a runtime value (with annotations).
gzipCompress: <fun: data:Str -> Str> — Compress data using gzip (default level).
gzipDecompress: <fun: data:Str -> Str?> — Decompress a gzip payload.
hexDecode: <fun: s:Str -> Str?> — Decode a hexadecimal string.
hexEncode: <fun: x:Str -> Str> — Hex-encode bytes to a lowercase hexadecimal string.
hmacSha256: <fun: key:Str -> msg:Str -> Str> — HMAC-SHA256 authentication tag (raw bytes).
http: <fun: req:{body: Str, bodyH: Any, headers: {}, method: Str, timeoutMs: Int, url!: Str} -> { body: Str, bodyH: Any, durationMs: Int, headers!: {}, proto: Str, status!: Int, statusText: Str, url: Str }?> — Make an HTTP request (buffered).
httpStream: <fun: req:{body: Str, bodyH: Any, headers: {}, method: Str, timeoutMs: Int, url!: Str} -> { body: Str, bodyH: Any, durationMs: Int, headers!: {}, proto: Str, status!: Int, statusText: Str, url: Str }?> — Make an HTTP request (streaming).
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
llm: <module: llm> — The LLM module.
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
ownerCall: <fun: o:Any -> f:Any -> args:[Any] -> Any> — Call f(...args) inside the owner's single thread.
ownerClose: <fun: o:Any -> Bool?> — Close the owner's request queue (idempotent).
ownerGet: <fun: o:Any -> key:Str -> Any> — Get a property from the wrapped value (map/module) inside the owner.
ownerRun: <fun: o:Any -> f:Any -> Any> — Run function f(m) inside the owner's single thread.
ownerSet: <fun: o:Any -> key:Str -> v:Any -> Any> — Set a property on the wrapped value (map/module) inside the owner.
ownerWrap: <fun: m:Any -> pinOSThread:Bool? -> Any> — Create a single-thread owner wrapper for value 'm'.
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
reduce: <fun: f:(Any -> Any -> Any) -> it:(Null -> Any?) -> Any?> — Fold an iterator with a binary function.
reflect: <fun: val:Any -> [Any]?> — Reflect a value into constructor code (runtime-S).
reify: <fun: rt:[Any] -> Any> — Decode and evaluate constructor code (runtime-S).
remove: <fun: path:Str -> Bool?> — Delete a file or an empty directory.
rename: <fun: old:Str -> new:Str -> Bool?> — Rename (move) a file or directory.
replace: <fun: pattern:Str -> replace:Str -> string:Str -> Str> — Replace all non-overlapping regex matches.
rstrip: <fun: s:Str -> Str> — Remove trailing whitespace (Unicode).
runtime: { isEntry: true, path: "/home/pedro/Documents/Projects/msg/builtins.ms", argv: [] } — 
seedRand: <fun: n:Int -> Null> — Seed the pseudo-random number generator.
sha256: <fun: x:Str -> Str> — SHA-256 digest (raw bytes).
shift: <fun: arr:[Any] -> Any> — Remove and return the first element of an array.
sin: <fun: x:Num -> Num> — Sine of an angle in radians.
sleep: <fun: ms:Int -> Null> — Pause execution for a number of milliseconds.
slice: <fun: xs:[Any] -> s:Int -> e:Int -> [Any]> — Slice an array [s, e).
snapshot: <fun: _:Null -> {}> — Return a map snapshot of the visible environment (including built-ins).
sort: <fun: arr:[Any] -> cmp:(Any -> Any -> Int) -> [Any]> — Sort an array in-place using a comparison function.
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

## 13) Collections & Access Semantics

* Arrays are **0-based**; negative indices count from the end:
```mindscript
let xs = [10, 20, 30]
xs[1] # => 20
xs[-1] # => 30
```

* Maps are open-world; unknown keys are allowed until set. Map addition merges:
`{a: 0, b: 1} + {a: 1, c: 2}` ⇒ `{a: 1, b: 1, c: 2}`

**Access**
```mindscript
obj.field  # property (quoted if needed)
arr[i]     # 0-based, negatives from end
map["k"]   # explicit string key
```

**Missing key/out-of-bounds:**  **panic**. Use `mapHas`, checks, or safe wrappers in your code.
 
## 14) Worked Examples

### 14.1 Sum array

```mindscript
# Sum an array of Ints.
let sum = fun(xs: [Int]) -> Int do
	let acc = 0
	for x in xs do
		acc = acc + x
	end
	acc
end
```

### 14.2 Map iteration with destructuring

```mindscript
for [k, v] in { a: 1, b: 2 } do
	printf("%s=%d\n", [k, v])
end
```

### 14.3 Destructuring patterns

```mindscript
let [a, b] = [1, 2, 3] # a=1, b=2
let { name: n, age: a } = { name: "Ada" } # n="Ada", a=null
```

### 14.4 If / elif / else as an expression

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

### 14.5 Oracle with typed output & nullable result

```mindscript
# Choose a color (may return error/null).
let chooseColor = oracle() -> Enum["red","green","blue"]

let c = chooseColor()
if c == null then
	"no color" # oracle unavailable
else
	c
end
```

## 15) Design Guidelines

*  **Public APIs:** declare explicit parameter and return types. Use `T?` only when `null` is a legitimate outcome; avoid exposing raw `null` without a type contract.
*  **Annotations:** use them for docs.
*  **Canonicalization:** formatter removes redundant parens/trailing commas, normalizes quoting, sorts map keys in printers for stability, and converts spaces → tabs.
*  **Avoid panics** in library code; favor returning `null` with a clear reason (in the annotation).
*  **Favor duck typing:** declare only what’s required; rely on structural checks at call time.

## 16) Quick Reference (Cheat Sheet)

```mindscript
# Declarations & assignment (no semicolons)
let x = 1 # value of this expression is 1
x = x + 1

# Arrays & maps (arrays are monomorphic)

let xs = [1, 2, 3]
let m = { name: "Ada", age: 36 }

# Access (panics on missing key / OOB)

xs[0]; xs[-1]
m.name; m["not-ident"]

# Functions (prefer explicit types; semantically curried)

let inc = fun(n: Int) -> Int do n + 1 end
let add = fun(a: Int, b: Int) -> Int do a + b end

add(1, 2) # sugar for add(1)(2)

# add(1,2,3) # panic: too many arguments

# Control (always close with 'end')

if cond then A() elif other then B() else C() end

# Return/break/continue (payload defaults to null)

return
break "done"
continue

# Types

let Pair = type { left!: Int, right!: Int }
let F = type Int -> Num -> Str
let Person = type { name!: Str, age: Int? }

# Oracles

# Choose a color!
let getColor = oracle() -> Enum["red","green","blue"]

# Annotations

# header
# comment
value # trailing comment

# Errors vs Panics

return null # reason (preferred for expected absence)
panic("boom") # hard fault (recoverable via try; avoid unless the error is fatal)

# Truthiness & bool()

bool(0) == false
bool(fun(x) -> x end) == null
```

**End of Manual.**