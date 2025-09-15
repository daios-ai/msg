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

* `E: 2.718281828459045` — Euler’s number.
* `PI: 3.141592653589793` — π.
* `STDERR: <handle: file>` — Writable standard error.
* `STDIN: <handle: file>` — Readable standard input.
* `STDOUT: <handle: file>` — Writable standard output.

### Core runtime & assertions

* `assert: <fun: cond:Bool -> Bool>` — Hard-fail if `cond` is false; returns true otherwise.
* `fail: <fun: message:Str? -> Null>` — Hard error.
* `error: <fun: msg:Str -> Null>` — **Soft** error (returns `null` with annotation).
* `print: <fun: x:Any -> Any>` / `println: <fun: x:Any -> Any>` — Print (with/without newline) and return `x`.
* `try: <fun: f:Any -> {}>` — Capture hard failures. **Shape:** `{ ok!: Bool, value: Any, error: Str }`.

### Introspection, Types & Reflection

* `snapshot: <fun: _:Null -> {}>` — Map snapshot of visible env.
* `typeOf: <fun: x:Any -> Type>`
* `isType: <fun: x:Any -> T:Type -> Bool>`
* `isSubtype: <fun: A:Type -> B:Type -> Bool>`
* `formatValue: <fun: x:Any -> Str>`
* `formatCode: <fun: src:Str -> Str?>`
* `reflect: <fun: val:Any -> [Any]?>`
* `reify: <fun: rt:[Any] -> Any>`
* `astParse: <fun: src:Str -> [Any]?>`
* `astValidate: <fun: ast:[Any] -> [Any]>`
* `astEval: <fun: ast:[Any] -> Any>`
* `astFormat: <fun: ast:[Any] -> Str?>`
* `dir: <fun: x:{} -> [Str]>` — Field/function names on an object.
* `uid: <fun: value:Any -> Int>` — Stable-ish content id.

### Serialization, Schemas & Encoding

* `jsonParse: <fun: s:Str -> Any>`
* `jsonStringify: <fun: x:Any -> Str?>`
* `jsonSchemaToType: <fun: schema:Any -> Type?>`
* `jsonSchemaStringToType: <fun: src:Str -> Type>`
* `typeToJSONSchema: <fun: t:Type -> Any>`
* `typeStringToJSONSchema: <fun: src:Str -> Any>`
* `base64Encode: <fun: x:Str -> Str>` / `base64Decode: <fun: s:Str -> Str?>`
* `hexEncode: <fun: x:Str -> Str>` / `hexDecode: <fun: s:Str -> Str?>`
* `gzipCompress: <fun: data:Str -> Str>` / `gzipDecompress: <fun: data:Str -> Str?>`

### Conversion & Length

* `bool: <fun: x:Any -> Bool>`
* `int: <fun: x:Any -> Int?>`
* `num: <fun: x:Any -> Num?>`
* `str: <fun: x:Any -> Str?>`
* `len: <fun: x:Any -> Int?>`

### Strings & Text

* `join: <fun: xs:[Str] -> sep:Str -> Str>`
* `split: <fun: s:Str -> sep:Str -> [Str]>`
* `match: <fun: pattern:Str -> string:Str -> [Str]>`
* `replace: <fun: pattern:Str -> replace:Str -> string:Str -> Str>`
* `sprintf: <fun: fmt:Str -> args:[Any] -> Str?>`
* `printf: <fun: fmt:Str -> args:[Any] -> Str?>`
* `substr: <fun: s:Str -> i:Int -> j:Int -> Str>`
* `toLower: <fun: s:Str -> Str>` / `toUpper: <fun: s:Str -> Str>`
* `strip: <fun: s:Str -> Str>` / `lstrip: <fun: s:Str -> Str>` / `rstrip: <fun: s:Str -> Str>`
* `noteGet: <fun: x:Any -> Str?>` / `noteSet: <fun: text:Str -> value:Any -> Any>`

### Math

* `sin | cos | tan | sqrt | exp | log | pow` — Standard float math.

### Randomness & Crypto

* `seedRand: <fun: n:Int -> Null>`
* `randFloat: <fun: _:Null -> Num>`
* `randInt: <fun: n:Int -> Int>`
* `randBytes: <fun: n:Int -> Str?>`
* `sha256: <fun: x:Str -> Str>`
* `hmacSha256: <fun: key:Str -> msg:Str -> Str>`
* `ctEqual: <fun: a:Str -> b:Str -> Bool>`

### Time & Scheduling

* `dateNow: <fun: _:Null -> {}>`
* `nowMillis: <fun: _:Null -> Int>` / `nowNanos: <fun: _:Null -> Int>`
* `sleep: <fun: ms:Int -> Null>`
* `ticker: <fun: ms:Int -> Any>` — Periodic ticks on a channel.
* `timerAfter: <fun: ms:Int -> Any>` — One tick after delay.
* `timeFormatRFC3339: <fun: millis:Int -> Str>`
* `timeParseRFC3339: <fun: s:Str -> Int?>`

### Channels (CSP-style)

* `chanOpen: <fun: cap:Int? -> Any>`
* `chanClose: <fun: c:Any -> Null>`
* `chanSend: <fun: c:Any -> x:Any -> Null>`
* `chanRecv: <fun: c:Any -> Any>`
* `chanTrySend: <fun: c:Any -> x:Any -> Bool>`
* `chanTryRecv: <fun: c:Any -> {}>` — Non-blocking receive result.

### Iterators & Sequences

* `iter: <fun: v:Any -> Null -> Any?>` — Arrays/maps → iterator; iterators pass through.
* `map: <fun: f:(Any -> Any) -> it:(Null -> Any?) -> Null -> Any?>`
* `filter: <fun: cond:(Any -> Bool) -> it:(Null -> Any?) -> Null -> Any?>`
* `reduce: <fun: f:((Any, Any) -> Any) -> it:(Null -> Any?) -> Any?>`
* `list: <fun: it:(Null -> Any?) -> [Any]>` — Collect.
* `range: <fun: start:Int -> stop:Int? -> Null -> Int?>` — `[start, stop)`; infinite if `stop=null`.
* `naturals: <fun: _:Null -> Null -> Int>` — 1, 2, 3, …
* `naturals0: <fun: _:Null -> Null -> Int>` — 0, 1, 2, …
* `keys: <fun: obj:{} -> Null -> Str?>` — Iterate keys.
* `values: <fun: obj:{} -> Null -> Any?>` — Iterate values.

### Arrays (helpers)

* `push: <fun: xs:[Any] -> v:Any -> [Any]>` — Returns new array.
* `pop: <fun: xs:[Any] -> Any?>` — Last element (no mutation).
* `shift: <fun: xs:[Any] -> v:Any -> [Any]>` — Prepend (returns new array).
* `unshift: <fun: xs:[Any] -> Any?>` — First element (peek).
* `slice: <fun: xs:[Any] -> s:Int -> e:Int -> [Any]>`

### Maps (helpers)

* `mapDelete: <fun: obj:{} -> key:Str -> {}>` — Delete a property from a map (in place).
* `mapHas: <fun: obj:{} -> key:Str -> Bool>` — Return true if a key exists in a map.

### Processes & Concurrency

* `procSpawn: <fun: f:Any -> Any>` — Run in isolated process.
* `procCancel: <fun: p:Any -> Null>`
* `procCancelled: <fun: p:Any -> Bool>`
* `procJoin: <fun: p:Any -> Any>`
* `procJoinAll: <fun: ps:[Any] -> [Any]>`
* `procJoinAny: <fun: ps:[Any] -> {}?>` — `{ index!: Int, value: Any }` when available.

### Files, Paths & OS

* `cwd: <fun: _:Null -> Str?>`
* `chdir: <fun: path:Str -> Bool?>`
* `dirList: <fun: path:Str -> [Str]?>`
* `mkdir: <fun: path:Str -> Bool?>`
* `open: <fun: path:Str -> mode:Enum["r","w","a","rw"] -> Any?>`
* `readAll: <fun: h:Any -> Str?>`
* `readN: <fun: h:Any -> n:Int -> Str?>`
* `readLine: <fun: h:Any -> Str?>`
* `write: <fun: h:Any -> s:Str -> Int?>`
* `flush: <fun: h:Any -> Bool?>`
* `close: <fun: h:Any -> Bool?>`
* `readFile: <fun: path:Str -> Str?>`
* `writeFile: <fun: path:Str -> data:Str -> Int?>`
* `remove: <fun: path:Str -> Bool?>`
* `rename: <fun: old:Str -> new:Str -> Bool?>`
* `stat: <fun: path:Str -> {isDir!: Bool, modTimeMillis!: Int, mode!: Int, size!: Int}?>`
* `pathBase | pathDir | pathExt | pathClean | pathJoin`
* `tempDir: <fun: _:Null -> Str>`

### Environment & Modules

* `osEnv: <fun: name:Str -> Str?>`
* `osSetEnv: <fun: name:Str -> value:Str? -> Bool?>`
* `import: <fun: path:Str -> Any>`
* `importCode: <fun: name:Str -> src:Str -> Any>`
* `codeImport: <fun: code:Str -> Str -> {}>` — Create importer from code string.
* `importUrl: <fun: url:Str -> {}?>` — Import module from URL.

### Networking & HTTP

* `netListen: <fun: addr:Str -> Any>`
* `netAccept: <fun: l:Any -> Any>`
* `netConnect: <fun: addr:Str -> Any>`
* `urlParse: <fun: s:Str -> {}?>`
* `urlBuild: <fun: u:{} -> Str>`
* `urlQueryParse: <fun: s:Str -> {}?>`
* `urlQueryString: <fun: q:{} -> Str>`
* `http: <?>` / `httpStream: <?>` / `exec: <?>` — **Implementation-defined**; wire types left unspecified in this spec.

### Oracles (installation)

* `oracleInstall: <fun: exec:(Str -> Str?) -> Null>` — Install global oracle executor.
* `oracleHealth: <fun: _:Null -> {}?>` — Probe executor.
* `oracleStatus: <fun: _:Null -> Str>` — Status string.
* `llm: <module: llm>` — LLM module entrypoint (executor + default backend selection).

### Misc

* `mute: <fun: _:Any -> Null>` — Sink a value (ignore).
* `clone: <fun: x:Any -> Any>` — Clone a value (deep-copy).


---

## 13) Collections & Iteration Details

* Arrays are 0-based; negative indices count from the end:

  ```mindscript
  let xs = [10, 20]
  xs[-1]     # => 20
  ```
* Maps are open-world; unknown keys are allowed and simply absent.
  Map addition merges: `{a: 0, b: 1} + {a: 1, c: 2}` => `{a: 1, b: 1, c: 2}`

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
