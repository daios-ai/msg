# JSON-MindScript Manual (Revised, MindScript-aligned)

**JSON-MindScript** is a strict-JSON, machine-readable S-expressions language. Every construct is an expression, every file is **pure JSON**, and evaluation is **strict left-to-right**. It supports **deductive** (deterministic functions) and **inductive** (first-class **oracles**) computation. Structural **types are first-class**. Documentation and machine metadata travel via **annotations**.

Canonical output ends with exactly **one** trailing newline.

---

## 1) Parsing & File Rules

* Source files are **pure JSON**: no comments, trailing commas, JSON5/JSONC, etc.
* Implementations **must** reject invalid JSON **before** evaluation.

**Self-test:** the file should pass a vanilla JSON parser (e.g., `jq -e .`).

**Invalid (do not copy):**

```json
["array",
  /* nope */ ["int", 1],
  ["int", 2]
]
```

```json
["map",
  ["pair", ["str","name"], ["str","Ada"]],
]
```

---

## 2) Evaluation Model (Strict, Expression-Oriented)

* **Everything is an expression.** Blocks return the last subexpression (or `["null"]` if empty).
* **Strict L→R evaluation** for receivers, operands, and arguments.
* **Assignment** evaluates to the assigned value.

---

## 3) Core Values

**Tagged scalars**

* `["null"]`
* `["bool", true|false]`
* `["int", <int64>]`
* `["num", <float64>]`
* `["str", "<utf8>"]`

**Arrays**

* `["array", v1, v2, ...]` — 0-based; negative indices count from the end.
* **Monomorphic**: all elements share one type; violations panic at construction/update.

**Maps (string-keyed)**

* `["map", ["pair", ["str","k"], v], ...]`
* In **type** positions, `["pair!", ["str","k"], T]` marks **required**.
* Later duplicate keys **overwrite** earlier ones.
* Insertion order is preserved for display/iteration; ignored by equality/subtyping.

**Functions**

* `["fun", paramsArray, retTypeOrAny, bodyBlock]`
* **No zero-arity functions**: every function/oracle has **≥1 parameter**.

**Oracles**

* `["oracle", paramsArray, retType, optsMap?]` (§12)

**Modules**

* `["module", nameExpr, bodyBlock]` — yields a writable export map (§11)

**Types (first-class)**

* `["type", typeExpr]` — see §9

**Annotations**

* `["annot", ["str", textOrAngleText], value]` — docs/metadata (§10)

---

## 4) Operators, Calls & Access

**Unary**

* Arithmetic / logic: `["unop","-", x]`, `["unop","not", x]`
* Nullable **type constructor**: `["unop","?", T]` → `T?` (see §9)

**Binary**

* Arithmetic: `"+" "-" "*" "/" "%"`

  * **Numeric tower**: `Int` (int64), `Num` (float64).
  * **Promotion**: only when **one operand is `Num`**.
  * **Division**: `Int/Int` → **Int** (e.g., `1/2 == 0`); otherwise → **Num**.
  * **Modulo**: sign follows the **dividend**.
  * **Divide-by-zero**: **panic**.
* Comparisons: `"<" "<=" ">" ">="` — only **number–number** (Int & Num mix OK) or **string–string**; other mixes **panic**.
* Equality: `"==" "!="` — deep for arrays/maps; numbers by value (`2 == 2.0` true); annotations ignored.
* Boolean: `"and" "or"` — short-circuit; operands must be `Bool` (no implicit truthiness here).

**Access & call**

* Property: `["get", obj, ["str","name"]]`
* Index array: `["idx", arr, i]` (0-based; negatives from end)
* Index map: `["idx", map, ["str","k"]]`
* **Call**: `["call", callee, a1, ...]` (L→R evaluation) — **must include ≥1 argument** (see “No zero-arity” below).

**No zero-arity calls**

* `["call", f]` is **invalid**.
* To “run” a callable with no meaningful input, declare **one parameter** (e.g., `Any` or `Null`) and **pass an argument** (often `["null"]`).

**Panics** for invalid operations: invalid receiver/target, missing key, out-of-bounds index, illegal comparison, divide-by-zero, arity overflow.

**Examples**

```json
["binop","+", ["int",2], ["num",2.5]]
```

```json
["binop","/", ["int",1], ["int",2]]      // => Int 0
```

```json
["binop","/", ["int",3], ["num",2.0]]    // => Num 1.5
```

```json
["idx", ["array", ["int",10], ["int",20]], ["int",-1]]
```

```json
["binop","and", ["bool", false], ["call", ["id","expensive"], ["null"]]]
```

---

## 5) Collections

* **Arrays**: 0-based; `-1` is the last element; negatives count from end; **monomorphic**.
* **Maps**: string keys; open-world; later duplicates overwrite.

**Examples**

```json
["idx", ["array", ["int",1], ["int",2], ["int",3]], ["int",-1]]
```

```json
["idx",
  ["map",
    ["pair", ["str","x"], ["int",1]],
    ["pair", ["str","x"], ["int",2]]
  ],
  ["str","x"]
]
```

---

## 6) Variables, Patterns & Assignment

**Identifiers**

* `["id","name"]`

**Patterns**

* Name declaration: `["decl","x"]`
* Array destructure: `["darr", p1, p2, ...]`
* Object destructure: `["dobj", ["pair", ["str","k"], subPattern], ...]`

**Binding vs update**

* **Bind** when target is a *pattern* → introduces names.
* **Update** when target is an l-value (`"id"|"get"|"idx"`) → updates existing.
  Updating an unbound name/invalid property/index **panics**.

**Destructuring semantics**

* Arrays: missing entries bind `["null"]`; extra items ignored unless matched.
* Objects: missing keys bind `["null"]`.

**Assignment result**

* `["assign", target, value]` evaluates to the **assigned value**.

**Examples**

```json
["assign", ["decl","x"], ["int",10]]
```

```json
["block",
  ["assign", ["decl","x"], ["int",1]],
  ["assign", ["id","x"], ["int",2]],
  ["id","x"]
]
```

```json
["assign",
  ["darr", ["decl","a"], ["decl","b"]],
  ["array", ["int",1], ["int",2], ["int",3]]
]
```

```json
["assign",
  ["dobj",
    ["pair", ["str","name"], ["decl","n"]],
    ["pair", ["str","age"],  ["decl","a"]]
  ],
  ["map", ["pair", ["str","name"], ["str","Ada"]]]
]
```

---

## 7) Control Flow

**Block**

* `["block", e1, e2, ...]` → value of last expression (or `["null"]` if empty).

**If / elif / else**

* `["if", ["pair", cond1, thenBlock1], ["pair", cond2, thenBlock2], ..., elseBlock]`
  `elseBlock` required (use `["block"]` for empty).

**While**

* `["while", cond, bodyBlock]`

**For** (arrays, maps, modules, host iterables)

* `["for", patOrLval, iterable, bodyBlock]`

  * Arrays yield elements.
  * Maps yield `["array", ["str",key], value]` pairs.
  * Modules yield exported bindings (name/value).
  * Host iterables may end by panicking or naturally.

**Early exits** (exactly one value; use `["null"]` for “empty”)

* `["return", value]`
* `["break", value]`
* `["continue", value]`

**Examples**

```json
["if",
  ["pair", ["bool", false], ["block", ["str","no"]]],
  ["block", ["str","yes"]]
]
```

```json
["block",
  ["assign", ["decl","i"], ["int",0]],
  ["while", ["binop","<", ["id","i"], ["int",3]],
    ["block",
      ["assign", ["id","i"], ["binop","+", ["id","i"], ["int",1]]]
    ]
  ],
  ["id","i"]
]
```

```json
["block",
  ["assign", ["decl","sum"], ["int",0]],
  ["for", ["decl","x"], ["array", ["int",1], ["int",2], ["int",3]],
    ["block", ["assign", ["id","sum"], ["binop","+", ["id","sum"], ["id","x"]]]]
  ],
  ["id","sum"]
]
```

---

## 8) Errors vs Panics

* **Error**: an intentional result using a **nullable type** `T?` with `["annot", ["str","<msg>"], ["null"]]`.
* **Panic**: exceptional runtime fault (e.g., invalid index, missing key, type/arity **overflow** violation, illegal comparison, divide-by-zero). Panics abort evaluation unless captured.

**Capturing panics — `try`**

* `["call", ["id","try"], expr]` returns **exactly** `{ ok!: Bool, value!: Any }`.
  When `ok=false`, `value` carries implementation-defined panic info (there is **no** `error` field).

**Quick table**

| Situation                       | Behavior                                    |
| ------------------------------- | ------------------------------------------- |
| Oracle output doesn’t match `R` | Return `["annot","<reason>",["null"]]`      |
| Oracle unavailable              | Return `["annot","<unavailable>",["null"]]` |
| **Arity underflow**             | Returns a **function** awaiting the rest    |
| **Arity overflow**              | **Panic** (capture via `try`)               |
| Missing map key / OOB index     | **Panic**                                   |
| Divide by zero                  | **Panic**                                   |

**Example**

```json
["block",
  ["assign", ["decl","r1"],
    ["call", ["id","try"],
      ["block", ["binop","/", ["int",1], ["int",0]]]
    ]
  ],
  ["assign", ["decl","r2"],
    ["call", ["id","try"],
      ["block", ["int",42]]
    ]
  ],
  ["array", ["id","r1"], ["id","r2"]]
]
```

---

## 9) Types (Structural, First-Class)

**Type IDs**

* `Any | Null | Bool | Int | Num | Str | Type`

**Nullable**

* `["unop","?", T]` renders as `T?`.
* **Special case:** **`Any? = Any`**. That is, `["unop","?", ["id","Any"]]` ≡ `["id","Any"]`.

**Arrays**

* `["array", T]` (monomorphic element type)

**Maps (open)**

* `["map", ["pair",  ["str","k"], T], ...]` — optional field
* `["map", ["pair!", ["str","k"], T], ...]` — **required** field

**Enums**

* `["enum", lit1, lit2, ...]` — literals may be scalars, arrays, or maps
* (If empty enums are disallowed in your implementation, treat `["enum"]` as invalid.)

**Functions & Oracles**

* Type arrow: `["binop","->", A, B]` (right-associative).
  `(A -> (B -> R))` ≡ `(A, B) -> R`.

**Subtyping (informal)**

* `Int <: Num`
* `T?` matches `Null` or `T`
* Arrays covariant by element type
* Maps fieldwise; **requiredness may only become more required** in **type–type** comparison
* Enums by member subset
* Functions/oracles: params **contravariant**, return **covariant**
* `Any` is top

**Value conformance vs requiredness**

* `isType(x, T)` ignores requiredness/optionality bits (they apply to **type–type** subtyping, not value checks).

**Examples**

```json
["type", ["id","Int"]]
```

````json
["type", ["array", ["unop","?", ["id","Str"]]]]
``]
```json
["type",
  ["map",
    ["pair!", ["str","name"], ["id","Str"]],
    ["pair",  ["str","age"],  ["id","Int"]]
  ]
]
````

```json
["type", ["enum", ["str","GET"], ["str","POST"]]]
```

---

## 10) Annotations (Docs & Machine Metadata)

**Form**

* `["annot", ["str","text"], value]` — human-oriented docs
* `["annot", ["str","<text>"], value]` — machine metadata (e.g., error reason)

**Semantics**

* Evaluates `value` and returns the same result and type.
* Ignored for equality and subtyping.

**Placement patterns**

```json
["assign", ["decl","x"], ["annot", ["str","counter"], ["int",1]]]
```

```json
["annot", ["str","Greet.\n\nArgs: name:Str\nReturn: Str"],
  ["fun",
    ["array", ["pair", ["id","name"], ["id","Str"]]],
    ["id","Str"],
    ["block", ["call", ["id","sprintf"], ["str","Hello, %s"], ["id","name"]]]
  ]
]
```

```json
["return", ["annot", ["str","fast path"], ["id","result"]]]
```

```json
["annot", ["str","<not found>"], ["null"]]
```

**Pretty-printing guidance (surface syntax)**

* An annotation that logically *follows* a value on a line should render as a **trailing annotation** on that value. When following separators (`,` or `:`), it attaches to the last value before the separator for display.

---

## 11) Modules

`["module", nameExpr, bodyBlock]` yields a **module value**:

* Behaves like a **map** for typing/iteration.
* **Namespace encapsulation:** the body has its own scope.
* **Writability:** `get`/`set` reflect **exports**; writes update the export table (overwrites allowed).
* **Import semantics:** multiple imports of the **same module** get the **same instance**.
* **Write locality:** writes affect **only the current runtime**.

**Example**

```json
["block",
  ["assign", ["decl","M"],
    ["module", ["str","M"],
      ["block",
        ["assign", ["decl","x"], ["int",12]],
        ["assign", ["decl","id"],
          ["fun", ["array", ["pair", ["id","y"], ["id","Any"]]], ["id","Any"], ["block", ["id","y"]]]
        ]
      ]
    ]
  ],
  ["assign", ["get", ["id","M"], ["str","x"]], ["int",99]],
  ["get", ["id","M"], ["str","x"]]
]
```

---

## 12) Functions, Oracles & Calls

**Function form**

```json
["fun",
  ["array", ["pair", ["id","name"], typeOrAny], ...],
  retTypeOrAny,
  bodyBlock]
```

**Rules**

* Each argument must be a **subtype** of its parameter type.
* The actual returned value must be a subtype of the declared return type.
* Violations **panic**.
* Calls are **curried** semantically; JSON call form is `["call", f, a1, ...]`.
* **No zero-arity**: declarations have **≥1 parameter**.

**Arity behavior**

* **Underflow** (fewer args): returns a **function** awaiting remaining arguments.
* **Overflow** (too many args): **panic**.

**Underflow example**

```json
["block",
  ["assign", ["decl","add2"],
    ["fun",
      ["array",
        ["pair", ["id","a"], ["id","Int"]],
        ["pair", ["id","b"], ["id","Int"]]
      ],
      ["id","Int"],
      ["block", ["binop","+", ["id","a"], ["id","b"]]]
    ]
  ],
  ["assign", ["decl","add2_10"], ["call", ["id","add2"], ["int",10]]],
  ["call", ["id","add2_10"], ["int",5]]
]
```

**Overflow example (captured)**

```json
["call", ["id","try"],
  ["call", ["id","add2"], ["int",1], ["int",2], ["int",3]]
]
```

**Zero-arg sugar note (surface vs JSON)**
In surface MindScript, `f()` lowers to `f(null)` (implying the function has **one** parameter). In JSON, you **must** pass the argument explicitly: `["call", f, ["null"]]`.

### Oracles (typed inductive calls)

**Declaration**

```json
["oracle",
  ["array", ["pair", ["id","name"], type], ...],
  retType,
  ["map",
    ["pair", ["str","doc"], ["str","..."]],
    ["pair", ["str","examples"], ["array",
      ["array", inputArray1, output1]
    ]]
  ]?]
```

**Calling rules**

* Parameter arity/types are **checked**; mismatch **panics**.
* **Automatic nullable boundary:** calls **never panic for result** — treat output as `R?`. If the candidate conforms to `R`, return it; else return `["annot", "<reason>", ["null"]]`.

**Examples**

*A. Oracle with typed output & doc (one param that accepts `null`)*

```json
["assign", ["decl","chooseColor"],
  ["oracle",
    ["array", ["pair", ["id","_"], ["id","Any"]]],    // one parameter
    ["type", ["enum", ["str","red"], ["str","green"], ["str","blue"]]],
    ["map", ["pair", ["str","doc"], ["str","Pick a primary color."]]]
  ]
]
```

Call:

```json
["call", ["id","chooseColor"], ["null"]]
```

*B. Oracle with examples*

```json
["assign", ["decl","numberToWord"],
  ["oracle",
    ["array", ["pair", ["id","n"], ["id","Int"]]],
    ["id","Str"],
    ["map",
      ["pair", ["str","doc"], ["str","Say the English word for a small integer."]],
      ["pair", ["str","examples"],
        ["array",
          ["array", ["array", ["int",0]], ["str","zero"]],
          ["array", ["array", ["int",1]], ["str","one"]],
          ["array", ["array", ["int",2]], ["str","two"]]
        ]
      ]
    ]
  ]
]
```

*C. Failure path → annotated null (`R?`)*

```json
["assign", ["decl","cc"],
  ["oracle",
    ["array", ["pair", ["id","name"], ["id","Str"]]],
    ["id","Str"],
    ["map", ["pair", ["str","doc"], ["str","ISO 3166-1 alpha-2 code."]]]
  ]
],
["call", ["id","cc"], ["str","Wakanda"]]
// => ["annot", ["str","<unrecognized country>"], ["null"]]
```

---

## 13) Equality, Comparison & Truthiness

* `==` / `!=` deep for arrays/maps (map key order ignored). Numbers by value; `2 == 2.0` is true.
* `< <= > >=` only allow number–number or string–string; otherwise **panic**.
* **Truthiness** for `bool` coercion: **falsey** = `null`, `0`, `0.0`, `""`, `[]`, `{}`; everything else truthy.
* **Non-convertibles**: `bool(function)` and `bool(type)` return **`Bool?`** as an **annotated null** (not `false`).

**Example**

```json
["call", ["id","bool"],
  ["fun", ["array", ["pair", ["id","_"], ["id","Any"]]], ["id","Any"], ["block", ["null"]]]
]
// => ["annot", ["str","<non-convertible to Bool>"], ["null"]]
```

---

## 14) Code Conventions

* Names: modules `snake_case`; funcs/vars `camelCase`; types `PascalCase`; consts `SCREAMING_SNAKE_CASE`; private `_prefix`.
* Docs: public functions carry a concise `annot` docstring (one-line summary; blank line; Args/Return; error notes if any).
* Types: public APIs use explicit param & return types; use `T?` only when error/absence is possible.
* No “void”: every expression yields a value; avoid exposing raw `["null"]` unless type is `T?`.
* **Indentation:** tabs.
* **Formatters** should end files with **exactly one trailing newline**.

---

## 15) Simplifying Assumptions (Runtime)

* **Int** is **int64**; **Num** is **float64**.
* **Promotion** only when one operand is `Num`.
* **Division**: `Int/Int` → **Int**; otherwise → **Num**.
* **Modulo** sign follows dividend.
* **Maps** are open-world; requiredness is a type property.
* **Missing key** / **array OOB** **panic**.
* **Annotations** never affect equality or subtyping.
* **Modules** act like maps; multiple imports share the **same instance**; writes are **local to the current runtime**.

---

## 16) Built-in Functions (Practical Reference)

Types below use MindScript type notation; `?` = nullable (soft error). When a function may **panic**, it is noted. **`try` is corrected** per §8.

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
exec: <fun: cmd:[Str] -> opts:{cwd: Str, env: {}, stdin: Str}? -> { status!: Int, stderr!: Str, stdout!: Str }?> — Run an external program.
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
http: <fun: req:{ body: Str, bodyH: Any, headers: {}, method: Str, timeoutMs: Int, url!: Str } -> { body: Str, bodyH: Any, durationMs: Int, headers!: {}, proto: Str, status!: Int, statusText: Str, url: Str }?> — Make an HTTP request (buffered).
httpStream: <fun: req:{ body: Str, bodyH: Any, headers: {}, method: Str, timeoutMs: Int, url!: Str } -> { body: Str, bodyH: Any, durationMs: Int, headers!: {}, proto: Str, status!: Int, statusText: Str, url: Str }?> — Make an HTTP request (streaming).
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
runtime: { argv: [], isEntry: true, path: "/home/pedro/Documents/Projects/msg/builtins.ms" } — 
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

---

## 17) Conformant Illustrations

**Strict function checks (panic on mismatch)**

```json
["call",
  ["fun",
    ["array", ["pair", ["id","n"], ["id","Int"]]],
    ["id","Int"],
    ["block", ["binop","+", ["id","n"], ["int",1]]]
  ],
  ["num", 2.5]
]
```

**Short-circuit boolean**

```json
["binop","and", ["bool", false], ["call", ["id","f"], ["null"]]]
```

**Encapsulated, writable module (shared import; local write)**

```json
["block",
  ["assign", ["decl","M"],
    ["module", ["str","M"],
      ["block", ["assign", ["decl","x"], ["int",12]]]
    ]
  ],
  ["assign", ["get", ["id","M"], ["str","x"]], ["int", 99]],
  ["get", ["id","M"], ["str","x"]]
]
```

**Error vs panic**

```json
["block",
  ["assign", ["decl","m"], ["map", ["pair", ["str","a"], ["int",1]]]],
  ["assign", ["decl","v1"], ["idx", ["id","m"], ["str","a"]]],

  // Panic example (missing key)
  ["call", ["id","try"],
    ["idx", ["id","m"], ["str","b"]]
  ],

  // Error (nullable) example
  ["assign", ["decl","u"],
    ["call", ["id","urlParse"], ["str","not a url"]]
  ]
]
```

**Division and modulo**

```json
["binop","/", ["int",7], ["int",3]]   // => Int 2
```

```json
["binop","%", ["int",3], ["int",-2]]  // => Int 1 (dividend sign)
```

---

## 18) Quick Start Examples

**Increment**

```json
["assign", ["decl","inc"],
  ["annot", ["str","Increment an Int by 1.\n\nArgs: n:Int\nReturn: Int"],
    ["fun",
      ["array", ["pair", ["id","n"], ["id","Int"]]],
      ["id","Int"],
      ["block", ["binop","+", ["id","n"], ["int",1]]]
    ]
  ]
]
```

**Sum an array**

```json
["assign", ["decl","sum"],
  ["fun",
    ["array", ["pair", ["id","xs"], ["array", ["id","Int"]]]],
    ["id","Int"],
    ["block",
      ["assign", ["decl","acc"], ["int",0]],
      ["for", ["decl","x"], ["id","xs"],
        ["block", ["assign", ["id","acc"], ["binop","+", ["id","acc"], ["id","x"]]]]
      ],
      ["id","acc"]
    ]
  ]
]
```

**Oracle with typed output & examples**

```json
["assign", ["decl","numberToWord"],
  ["oracle",
    ["array", ["pair", ["id","n"], ["id","Int"]]],
    ["id","Str"],
    ["map",
      ["pair", ["str","doc"], ["str","Say the English word for a small integer."]],
      ["pair", ["str","examples"],
        ["array",
          ["array", ["array", ["int",0]], ["str","zero"]],
          ["array", ["array", ["int",1]], ["str","one"]],
          ["array", ["array", ["int",2]], ["str","two"]]
        ]
      ]
    ]
  ]
]
```

**Oracle (nullable boundary)**

```json
["assign", ["decl","chooseColor"],
  ["oracle",
    ["array", ["pair", ["id","_"], ["id","Any"]]],
    ["type", ["enum", ["str","red"], ["str","green"], ["str","blue"]]],
    ["map", ["pair", ["str","doc"], ["str","Pick a primary color."]]]
  ]
],
["call", ["id","chooseColor"], ["null"]]
```

**Arity underflow (currying)**

```json
["assign", ["decl","add2_10"], ["call", ["id","sum"], ["int",10]]]
```

**Arity overflow (panic, captured)**

```json
["call", ["id","try"],
  ["call", ["id","sum"], ["int",1], ["int",2], ["int",3]]
]
```

**End of Manual.**
