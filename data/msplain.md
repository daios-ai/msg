# MindScript Language Manual (Human-Oriented)

MindScript is a concise, expression-oriented language with a clean surface syntax, structural types, first-class functions, and rich annotations. This manual presents the **human** (plain) MindScript syntax and semantics—free of machine-only encodings.

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
	write(STDOUT, sprintf("%s=%d\n", k, v))
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
* Exports are visible via property access: `m.area(3.0)`.
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

Below are common operations. Types use MindScript notation; `?` = nullable (soft possible).

### Constants & Handles

* `PI: Num`, `E: Num`
* `STDIN / STDOUT / STDERR`

```mindscript
write(STDOUT, "Hello\n")
```

### Introspection & Environment

* `bindings(currentOnly?: Bool) -> { Str: Any }`
* `snapshot() -> { Str: Any }` (deep)
* `typeOf(x: Any) -> Type`
* `isType(x: Any) -> (Type -> Bool)`
* `isSubtype(A: Type) -> (B: Type -> Bool)`

```mindscript
typeOf(7)      # => Int
```

### AST, Reflection, Codecs

* `astParse(src: Str) -> [Any]?` (annot-null on parse error)
* `astEval(ast: [Any]) -> Any`
* `reflect(x: Any) -> [Any]`
* `reify(program: [Any]) -> Any`
* `jsonParse(s: Str) -> Any`
* `jsonStringify(x: Any) -> Str?` (annot-null if not encodable)
* `base64Encode/Decode : Str -> Str / Str -> Str?`
* `hexEncode/Decode : Str -> Str / Str -> Str?`
* `gzipCompress/Decompress : Str -> Str / Str -> Str?`

```mindscript
jsonStringify({ k: 1 })  # => "{\"k\":1}"
```

### URL & HTTP

* `urlParse(s: Str) -> { scheme!:Str, host!:Str, port: Int, path!:Str, query!:Str, fragment: Str }?`
* `urlBuild(components: { ... }) -> Str`
* `urlQueryParse(s: Str) -> { Str: [Str] }?`
* `urlQueryString(m: { Str: [Str] }) -> Str`
* `http(req: { method:Str, url:Str, headers:{Str:Str}, body:Str, ... }) -> { status!:Int, statusText:Str, headers!:{Str:Str}, body!:Str, url:Str, proto:Str, durationMs: Int }?`
* `httpStream(req: { ... }) -> { bodyH!: Any }?`

```mindscript
let u = urlParse("https://example.com/a?b=1")
if u == null then return null # <invalid url> end
u.host
```

### Filesystem & OS

* `cwd() -> Str?`
* `chdir(path: Str) -> Bool`
* `open(path: Str)("r"|"w"|"a"|"rw") -> Any?`
* `readFile(path: Str) -> Str?`
* `writeFile(path: Str)(content: Str) -> Int?`
* `remove/rename/mkdir : Str -> Bool / Str -> (Str -> Bool) / Str -> Bool`
* `dirList(path: Str) -> [Str]?`
* `stat(path: Str) -> { ... }?`
* `pathBase/Dir/Ext/Clean : Str -> Str`
* `pathJoin(parts: [Str]) -> Str`
* `osEnv(name: Str) -> Str?`
* `osSetEnv(name: Str)(value: Str?) -> Bool`

### Handles I/O

* `readAll(h: Any) -> Str?`
* `readN(h: Any)(n: Int) -> Str?`
* `readLine(h: Any) -> Str?`
* `write(h: Any)(s: Str) -> Int?`
* `flush/close : Any -> Bool / Any -> Bool`

### Networking

* `netListen(addr: Str) -> Any?`
* `netAccept(listener: Any) -> Any?`
* `netConnect(addr: Str) -> Any?`

### Processes & Execution

* `exec(argv: [Str])({ env: {Str:Str}, cwd: Str }) -> { status!:Int, stdout!:Str, stderr!:Str }?`
* `exit(code: Int?) -> Null`
* `fail(msg: Str?) -> Null`  # **hard error**
* `try(x: Any) -> { ok: Bool, value: Any, error: Str? }`

```mindscript
try(fail("boom"))  # => { ok: false, value: null, error: "boom" }
```

### Concurrency & Timers

* `chanOpen/chanClose : () -> Any / Any -> Bool`
* `chanSend/chanRecv : Any -> (Any -> Bool) / Any -> Any?`
* `chanTrySend/chanTryRecv : Any -> (Any -> Bool) / Any -> Any?`
* `ticker(ms: Int) -> Any`
* `timerAfter(ms: Int) -> Any`
* `sleep(ms: Int) -> Null`
* `nowMillis/nowNanos : () -> Int`

### Math

* `sin/cos/tan/sqrt/pow/log/exp : Num -> Num`
* `randFloat() -> Num`   # in \[0,1)
* `randInt(n: Int) -> Int`   # in \[0,n)
* `seedRand(seed: Int) -> Null`
* `randBytes(n: Int) -> Str?`

### Strings & Text

* `str(x: Any) -> Str?`
* `toLower/toUpper/strip/lstrip/rstrip : Str -> Str`
* `substr(s: Str)(start: Int)(len: Int) -> Str`
* `split(s: Str)(sep: Str) -> [Str]`   # empty sep splits by code point
* `join(xs: [Str])(sep: Str) -> Str`
* `match(s: Str)(pattern: Str) -> [Str]`   # RE2
* `replace(s: Str)(pattern: Str)(with: Str) -> Str`
* `sprintf/printf/formatCode/formatValue`

### Conversion, Length, Maps

* `int/num/bool : Any -> Int / Num / Bool`
* `len(x: Any) -> Int`   # arrays (# elems), maps (# keys), strings (runes)
* `clone(x: Any) -> Any` # deep for arrays/maps
* `mapHas(m: {Str:Any})(k: Str) -> Bool`
* `mapDelete(m: {Str:Any})(k: Str) -> Bool`

### Importing & Schemas

* `import(path: Str) -> Any?`
* `importCode(path: Str) -> Any?`
* `jsonSchemaToType(schema: Any) -> Type?`
* `jsonSchemaStringToType(s: Str) -> Type?`
* `typeToJSONSchema(t: Type) -> Any?`
* `typeStringToJSONSchema(s: Str) -> Any?`

### Cryptography

* `sha256(s: Str) -> Str`      # 32 raw bytes in string container
* `hmacSha256(key: Str)(msg: Str) -> Str`
* `ctEqual(a: Str)(b: Str) -> Bool`

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
	printf("%s=%d\n", k, v)
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
type Pair = { left!: Int, right!: Int }
type F = (Int, Num) -> Str

# Annotations
# PRE above
value # POST here forces newline
```

---

**End of Manual.**
