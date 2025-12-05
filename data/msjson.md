
# JSON-MindScript Manual (Updated)

JSON-MindScript is a strict-JSON, machine-readable encoding of MindScript’s canonical S-expressions. Every construct is an expression, every file is **pure JSON**, and evaluation is **strict left-to-right**. It supports **deductive** (deterministic functions) and **inductive** (first-class oracles) computation. Structural **types are first-class**. Documentation and machine metadata travel via **annotations**.

Canonical output ends with exactly **one** trailing newline.

This manual describes the canonical runtime S-grammar used by the introspection layer (`IxToS`, `IxFromS`, `IxReflect`, `IxValidateS`, `IxReify`).

---

## 1) Core Ideas

* **Everything is an expression.** Blocks, conditionals, loops, declarations, and assignments evaluate to a value (usually the last subexpression).
* **Strict left-to-right evaluation.** Receivers, operands, and arguments evaluate left→right.
* **Structural types are first-class.** Types can be constructed as values (`["type", typeNode]`), passed around, and introspected.
* **Two kinds of failure:**

  * **Errors** are values: idiomatically a **nullable type** (`T?`) whose error case is an (optionally annotated) `["null"]`.
  * **Panics** abort evaluation (lexer/parser/runtime). Use `try` to capture them.

---

## 2) Files, JSON & Node Shape

### 2.1 Pure JSON

* Source files are **pure JSON**: no comments, trailing commas, JSON5/JSONC features, etc.
* Implementations **must** reject invalid JSON **before** evaluation.

Self-test: the file must parse under a vanilla JSON parser.

Invalid:

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

### 2.2 Runtime S node shape

* Every node is a JSON array:

  * First element: a **string tag** (e.g. `"int"`, `"fun"`, `"map"`).
  * Remaining elements: tag-specific **children** (subnodes or scalars).
* Example:

```json
["binop", "+", ["int", 1], ["int", 2]]
```

`IxFromS` accepts only:

* arrays (`VTArray`) whose first element is `VTStr` tag;
* children that are arrays (recursively S), `VTStr`, `VTInt`, `VTNum`, or `VTBool`.
  Any other scalar tag is rejected as a **hard error**.

---

## 3) Core Values

### 3.1 Tagged scalars

All scalars are tagged:

* `["null"]`
* `["bool", true | false]`
* `["int", <int64>]`
* `["num", <float64>]`
* `["str", "<utf8>"]`

Negative numbers encode directly as `["int",-2]`, `["num",-1.5]`.

### 3.2 Arrays (value context)

Value arrays:

```json
["array", v1, v2, ...]
```

* Elements are expression nodes.
* Semantically **monomorphic** at the language level (all elements conceptually share one element type).
* Indices:

  * 0-based.
  * Negative indices count from the end: `-1` is last, `-2` is second-last.
* Out-of-bounds access **panics**.

### 3.3 Maps (value context)

String-keyed maps:

```json
["map",
  ["pair", ["str","k1"], v1],
  ["pair", ["str","k2"], v2]
]
```

* Keys:

  * Key node is `["str","k"]` or an annotated string (see §10).
* Value node is any expression.
* Later duplicate keys **overwrite** earlier ones.
* Insertion order is preserved for display/iteration but ignored by equality/subtyping.
* **Value maps** use only `"pair"` (not `"pair!"`; `"pair!"` is type-only, §4.3).

### 3.4 Special values

* **No handle literals:** the `"handle"` tag is forbidden in canonical runtime S. Handles exist only as opaque runtime values obtained from built-ins (e.g. `open`, `netConnect`).
* **Noop:** `["noop"]` is a special “do nothing” value used to represent blanks between annotations in surface code. It is rarely needed in JSON directly.

---

## 4) Types (Structural & First-Class)

Type ASTs appear inside `["type", typeNode]` values and in function/oracle signatures.

### 4.1 Type IDs

Primitive type identifiers (as `["id", "..."]`):

* `Any | Null | Bool | Int | Num | Str | Handle | Type`

Handle hierarchy:

* `Handle` is a base type identifier.
* Handle kinds are qualified forms, e.g. `Handle.file`, encoded as:

```json
["get", ["id","Handle"], ["str","file"]]
```

Each `Handle.<kind>` is a distinct nominal type; kinds do not unify structurally and only subtype `Any`.

### 4.2 Nullable types

Nullable type:

```json
["unop", "?", T]
```

Rendered as `T?`.

Special case:

* **`Any? = Any`**: `["unop","?", ["id","Any"]]` is semantically equivalent to `["id","Any"]`.

### 4.3 Arrays (type context)

Array type:

```json
["array", ElemType]
```

* Exactly one child type; `IxValidateS` enforces arity 2 for this node.
* At the value level, arrays are monomorphic by this element type.

### 4.4 Maps (type context, open maps)

Open maps (string keys, optional/required fields):

```json
["map",
  ["pair",  KeyNode, TypeNode],   // optional
  ["pair!", KeyNode, TypeNode]    // required
]
```

* Key node: `["str","fieldName"]` or an annotated string (see §10).
* `"pair"` → optional field; when present must conform to `TypeNode`.
* `"pair!"` → **required** field.
* Requiredness is only meaningful in **type–type** comparison; value conformance (`isType`) ignores requiredness flags.

### 4.5 Enums

Enum of members:

```json
["enum", member1, member2, ...]
```

Current semantics:

* **Members are arbitrary expressions**, not just literals.
* `IxValidateS` validates each member as an expression (not a `block`).
* At **evaluation time** (e.g. via `IxReify`), members must evaluate to **literal values**:

  * `null`, `bool`, `int`, `num`, `str`, array of literals, or map of literals.
* The runtime type system operates on the resulting **literal member set**.

### 4.6 Function & oracle types

Function/oracle arrow type:

```json
["binop", "->", A, B]
```

* Right-associative: `A -> B -> R` ≡ `A -> (B -> R)` ≡ `(A, B) -> R`.
* Used for function/oracle type declarations, not for value-level calls.

### 4.7 Type nodes vs value nodes

* Type ASTs appear:

  * In `["type", typeNode]` values.
  * In function/oracle signatures directly.
* Inside a `["fun", ...]` or `["oracle", ...]`:

  * Parameter types and return/output types are **type ASTs**, e.g.:

    ```json
    ["id","Int"]
    ["array", ["id","Str"]]
    ["enum", ["str","GET"], ["str","POST"]]
    ```

  * They are **not** wrapped in `["type", ...]`.

`["type", typeNode]` itself is a **value node** of type `Type`.

---

## 5) Expressions, Operators, Calls & Access

### 5.1 Unary operators

Value-level unary ops:

```json
["unop", "-", x]        // numeric negation
["unop", "not", x]      // boolean not
```

Type-level unary operator:

```json
["unop", "?", T]        // nullable type, §4.2
```

### 5.2 Binary operators

Arithmetic (`["binop", op, lhs, rhs]`):

* `op ∈ { "+", "-", "*", "/", "%" }`
* Numeric tower:

  * `Int` = 64-bit signed integer
  * `Num` = 64-bit float
* Promotion:

  * Only when **at least one operand is `Num`**.
* Division:

  * `Int / Int` → `Int` (truncating toward zero), e.g. `1/2 == 0`.
  * Otherwise `Num`.
* Modulo:

  * Sign follows the **dividend**.
  * Divide-by-zero **panics**.

Comparisons:

* `op ∈ { "<", "<=", ">", ">=" }`
* Only **number–number** (mixing `Int`/`Num` allowed) or **string–string`.
* Other mixes **panic**.

Equality:

* `op ∈ { "==", "!=" }`
* Deep comparison for arrays/maps (map key order ignored).
* Numbers compare by value (`2 == 2.0` is true).
* Annotations are ignored.

Boolean operators:

```json
["binop", "and", a, b]
["binop", "or",  a, b]
```

* Short-circuit semantics.
* Operands must be `Bool` (no implicit truthiness here).

### 5.3 Access & call

Property access:

```json
["get", objExpr, ["str","field"]]
```

Index array:

```json
["idx", arrExpr, indexExpr]   // indexExpr is an expression producing an Int
```

Index map:

```json
["idx", mapExpr, ["str","key"]]
```

Call:

```json
["call", calleeExpr, arg1Expr, arg2Expr, ...]
```

* **No zero-arg calls:** `["call", f]` is invalid.
* Argument list must have ≥ 1 expression.

Panics:

* Invalid receiver/target (e.g. calling non-callables).
* Missing key / out-of-bounds index.
* Illegal comparison (wrong operand kinds).
* Divide-by-zero.
* Arity overflow (too many arguments) when calling functions.

---

## 6) Variables, Patterns & Assignment

### 6.1 Identifiers

Identifier node:

```json
["id","name"]
```

### 6.2 Patterns (canonical form)

Bindings are introduced via `["let", pattern]`:

* Simple binding:

  ```json
  ["let", ["id","x"]]
  ```

* Array destructuring pattern:

  ```json
  ["let",
    ["array",
      ["id","a"],
      ["id","b"]
    ]
  ]
  ```

* Map destructuring pattern:

  ```json
  ["let",
    ["map",
      ["pair", ["str","name"], ["id","n"]],
      ["pair", ["str","age"],  ["id","a"]]
    ]
  ]
  ```

Patterns are ordinary S-nodes (`"id"`, `"array"`, `"map"`) interpreted in **pattern position**.

### 6.3 Assignable targets

Left-hand side (LHS) of `["assign", ...]` may be:

* `["let", pattern]` — binding (declaration).
* `["id", ...]` — variable update.
* `["get", obj, keyStrNode]` — property update.
* `["idx", arrOrMap, indexExprOrKeyStrNode]` — index update.
* `["array", ...patternElements...]` — destructuring assignment.
* `["map", ...pair(key, pattern)...]` — destructuring assignment.

The helper in the validator treats tags `let`, `id`, `get`, `idx`, `array`, `map` as assignable.

### 6.4 Binding vs update

Assignment:

```json
["assign", targetNode, valueExpr]
```

* If `targetNode` is `["let", pattern]`:

  * Introduces new bindings per the pattern.
* Otherwise:

  * Updates existing variables/fields/indices.
  * Updating an unbound name or invalid property/index **panics**.

Destructuring semantics (same as surface MindScript):

* Arrays:

  * Missing entries bind to `["null"]`.
  * Extra items are ignored unless matched by the pattern shape.
* Maps:

  * Missing keys bind to `["null"]`.

The result of `["assign", ...]` is the **assigned value** (the RHS result).

### 6.5 Examples

Simple bind + update:

```json
["block",
  ["assign", ["let", ["id","x"]], ["int",1]],
  ["assign", ["id","x"], ["int",2]],
  ["id","x"]
]
```

Array destructuring bind:

```json
["assign",
  ["let",
    ["array",
      ["id","a"],
      ["id","b"]
    ]
  ],
  ["array", ["int",1], ["int",2], ["int",3]]
]
```

Map destructuring bind:

```json
["assign",
  ["let",
    ["map",
      ["pair", ["str","name"], ["id","n"]],
      ["pair", ["str","age"],  ["id","a"]]
    ]
  ],
  ["map",
    ["pair", ["str","name"], ["str","Ada"]]
  ]
]
```

---

## 7) Control Flow

### 7.1 Block

Block:

```json
["block", e1, e2, ...]
```

* Evaluates each child expression in order.
* Returns the value of the **last** child, or `["null"]` if empty.

### 7.2 If / elif / else

Canonical shape:

```json
["if",
  ["pair", cond1Expr, thenBlock1],
  ["pair", cond2Expr, thenBlock2],
  ...,
  elseBlock
]
```

* Each non-final arm is a `["pair", condExpr, thenBlock]` where `thenBlock` is a `"block"`.
* The final element is an **else block**:

  * Must be a `["block", ...]`.
  * JSON-MindScript requires an explicit `else` (use `["block"]` for empty).

### 7.3 While

```json
["while", condExpr, bodyBlock]
```

* `bodyBlock` must be a `"block"` node.

### 7.4 For (arrays, maps, modules, iterables)

```json
["for", patternOrLval, iterableExpr, bodyBlock]
```

* Arrays yield elements.
* Maps yield `["array", keyNode, valueExpr]` pairs where `keyNode` is `["str","k"]`.
* Modules yield exported bindings as `[name,value]` pairs.
* `bodyBlock` is a `"block"`.

### 7.5 Early exits

Each carries exactly one value (use `["null"]` if “empty”):

```json
["return", valueExpr]
["break",  valueExpr]
["continue", valueExpr]
```

---

## 8) Functions & Oracles

### 8.1 Functions

Function value:

```json
["fun",
  ["array",
    ["pair", ["id","paramName1"], ParamType1],
    ["pair", ["id","paramName2"], ParamType2]
  ],
  ReturnType,
  bodyBlock
]
```

Rules:

* **No zero-arity functions:** the params array must have **at least one** `"pair"`.
* Parameter and return types are **type ASTs** (not wrapped in `["type", ...]`).
* The body must be a `"block"`.
* At call time:

  * Each argument must conform to the parameter type; otherwise **panic**.
  * The returned value must conform to the declared return type; otherwise **panic**.

Arity behavior:

* **Underflow** (fewer arguments than parameters):

  * Returns a **function** waiting for the remaining parameters (currying).
* **Overflow** (too many arguments):

  * **Panic**.

Call form (JSON):

```json
["call", funExpr, arg1Expr, arg2Expr]
```

* **No zero-arg calls**: there must be ≥1 argument.
* To emulate surface MindScript’s `f()`, you explicitly pass `["null"]` to a one-parameter function.

### 8.2 Oracles (typed inductive calls)

Oracle value:

```json
["oracle",
  ["array",
    ["pair", ["id","paramName1"], ParamType1],
    ...
  ],
  OutputType,
  sourceExpr
]
```

* Exactly **4 children**:

  1. Params array (same format as `fun`).
  2. Output type (type AST, not `["type", ...]`).
  3. `sourceExpr`: an **expression** that describes how to call the oracle (e.g. a doc string, examples map, etc.). It is not a `block`.
* **No zero-arity oracles:** params array must contain ≥1 `"pair"`.

Calling rules:

* Arguments are strictly type-checked; mismatch **panics**.
* **Automatic nullable boundary**:

  * Static output type: `R`.
  * Dynamic behavior: calls return `R?`.
  * If the oracle candidate value conforms to `R`, you get that value.
  * Otherwise (or if unavailable), you get an `["annot", ["str", reason], ["null"]]` or an unannotated `["null"]`.

Example (one “don’t care” parameter to allow zero-meaningful-input calls):

```json
["assign",
  ["let", ["id","chooseColor"]],
  ["oracle",
    ["array",
      ["pair", ["id","_"], ["id","Any"]]
    ],
    ["enum", ["str","red"], ["str","green"], ["str","blue"]],
    ["str","Pick a primary color."]
  ]
]
```

Call:

```json
["call", ["id","chooseColor"], ["null"]]
```

---

## 9) Modules

Module value:

```json
["module", nameExpr, bodyBlock]
```

* `nameExpr` is usually `["str","ModuleName"]`, but any expression is allowed.
* `bodyBlock` is a `"block"` of assignments; by convention, exports are `["assign", ["let", ["id","x"]], value]` forms.

Semantics:

* Modules evaluate to **module values** that behave like maps:

  * Visible fields correspond to exported bindings.
  * `dir` / iteration see the module as a map.
* **Namespace encapsulation:** the body runs in its own environment.
* **Exports & writability:**

  * `["get", moduleValue, ["str","name"]]` reads exported fields.
  * `["assign", ["get", moduleValue, ["str","name"]], v]` writes them.
* **Import semantics:**

  * Multiple imports of the same module path yield the **same module instance** (shared).
* **Write locality:**

  * Writes affect only the current runtime (process), but they are visible to all importers within that runtime.

Example:

```json
["block",
  ["assign",
    ["let", ["id","M"]],
    ["module", ["str","M"],
      ["block",
        ["assign", ["let", ["id","x"]], ["int",12]],
        ["assign",
          ["let", ["id","id"]],
          ["fun",
            ["array", ["pair", ["id","y"], ["id","Any"]]],
            ["id","Any"],
            ["block", ["id","y"]]
          ]
        ]
      ]
    ]
  ],
  ["assign", ["get", ["id","M"], ["str","x"]], ["int",99]],
  ["get", ["id","M"], ["str","x"]]
]
```

---

## 10) Annotations & Errors

### 10.1 Annotation form

Annotation node:

```json
["annot",
  ["str","text"],
  valueNode
]
```

* Exactly 3 elements.
* Second element must be a `"str"` node.
* **No nested annotations**:

  * `valueNode` must not itself be an `"annot"` node in canonical runtime S.
  * `IxValidateS` rejects nested `annot` trees.

Machine-oriented text (e.g. error reasons) can use a convention like `"<...>"`, but angle brackets have no special semantics in this layer.

### 10.2 Semantics

* Evaluating `["annot", textNode, valueNode]`:

  * Evaluates `valueNode` and returns that value.
  * The annotation is attached as metadata to the resulting value (accessible via `noteGet`).
* Annotations are ignored by:

  * Equality (`==` / `!=`).
  * Subtyping / type checks.

### 10.3 Errors as annotated null

Idiomatic “soft errors” (nullable types):

* Use a nullable type `T?` and return an (optionally annotated) `["null"]` on failure:

  * Simple null: `["null"]`.
  * Annotated null:

    ```json
    ["annot", ["str","<reason>"], ["null"]]
    ```

* `IxToS` uses this shape for soft errors during encoding:

  * `["annot", ["str", msg], ["null"]]`.

### 10.4 Panics & try

* Panics are hard runtime faults (e.g. bad index, type/arity violation, divide-by-zero).
* `try` has type:

  ```text
  try: (Null -> Any) -> { ok!: Bool, value!: Any }
  ```

Call form in JSON:

```json
["call",
  ["id","try"],
  ["fun",
    ["array", ["pair", ["id","_"], ["id","Null"]]],
    ["id","Any"],
    ["block", exprToRun]
  ]
]
```

* On success: returns `{ ok: true, value: result }`.
* On panic: returns `{ ok: false, value: panicInfo }` (panic info is implementation-defined).

---

## 11) Equality, Comparison & Truthiness

* `["binop","==", a, b]`, `["binop","!=", a, b]`:

  * Deep for arrays/maps (map key order ignored).
  * Numbers by value (`2 == 2.0` → `true`).
  * Annotations ignored.
* `["binop","<", ...]`, `["binop","<=", ...]`, `["binop",">", ...]`, `["binop",">=", ...]`:

  * Number–number or string–string only; else **panic**.

Truthiness for `bool` conversion (library function):

* Falsey: `null`, `0`, `0.0`, `""`, `[]`, `{}`.
* Everything else truthy.
* `bool(x)` has type `Bool?`:

  * Returns `["bool", true]` or `["bool", false]` on success.
  * Returns `["null"]` (optionally annotated) when `x` is not convertible (e.g. functions, types).

Example (non-convertible):

```json
["call",
  ["id","bool"],
  ["fun",
    ["array", ["pair", ["id","_"], ["id","Any"]]],
    ["id","Any"],
    ["block", ["null"]]
  ]
]
```

Result is a `Bool?` (`["null"]` or annotated null).

---


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


---

## 13) Collections & Access Semantics

* Arrays:

  * 0-based; negative indices count from the end.
  * `["idx", arrExpr, indexExpr]`:

    * Index must evaluate to `Int`.
    * OOB indices **panic**.

* Maps:

  * String keys.

  * Open-world: unknown keys may be added at any time.

  * Duplicate keys in literals: later wins.

  * Access via:

    ```json
    ["get", moduleOrMapExpr, ["str","field"]]   // for module/map as property
    ["idx", mapExpr, ["str","key"]]             // map index
    ```

  * Missing key **panics**.

* Map addition (at language level, as in surface MindScript):

  ```json
  ["binop", "+",
    mapExpr1,
    mapExpr2
  ]
  ```

  merges maps like:

  `{a:0, b:1} + {a:1, c:2} ⇒ {a:1, b:1, c:2}`.

---

## 14) Worked Examples

### 14.1 Sum array

```json
["assign",
  ["let", ["id","sum"]],
  ["fun",
    ["array",
      ["pair", ["id","xs"], ["array", ["id","Int"]]]
    ],
    ["id","Int"],
    ["block",
      ["assign", ["let", ["id","acc"]], ["int",0]],
      ["for", ["let", ["id","x"]], ["id","xs"],
        ["block",
          ["assign",
            ["id","acc"],
            ["binop", "+", ["id","acc"], ["id","x"]]
          ]
        ]
      ],
      ["id","acc"]
    ]
  ]
]
```

### 14.2 Map iteration with destructuring

```json
["block",
  ["for",
    ["let",
      ["array",
        ["id","k"],
        ["id","v"]
      ]
    ],
    ["map",
      ["pair", ["str","a"], ["int",1]],
      ["pair", ["str","b"], ["int",2]]
    ],
    ["block",
      ["call",
        ["id","printf"],
        ["str","%s=%d\n"],
        ["array", ["id","k"], ["id","v"]]
      ]
    ]
  ]
]
```

### 14.3 If / elif / else

```json
["assign",
  ["let", ["id","msg"]],
  ["if",
    ["pair",
      ["id","ok"],
      ["block", ["str","yes"]]
    ],
    ["pair",
      ["id","maybe"],
      ["block", ["str","maybe"]]
    ],
    ["block", ["str","no"]]
  ]
]
```

### 14.4 Oracle with typed output & nullable result

```json
["assign",
  ["let", ["id","chooseColor"]],
  ["oracle",
    ["array",
      ["pair", ["id","_"], ["id","Any"]]
    ],
    ["enum", ["str","red"], ["str","green"], ["str","blue"]],
    ["str","Pick a primary color."]
  ]
],
["call", ["id","chooseColor"], ["null"]]
```

---

## 15) Design Guidelines

* **Public APIs:** use explicit parameter and return types. Use `T?` only when `null` is a legitimate outcome.
* **Annotations:** use `annot` for documentation and machine-readable metadata. Do not base control flow on annotation text.
* **Canonicalization:**

  * Formatter removes redundant parens/trailing commas in surface code.
  * JSON runtime S uses stable tagging; printers should normalize quoting and map key order for reproducibility.
* **Avoid panics** in library-level code; prefer `T?` with a clear annotated `null` where failure is expected.
* **Favor duck typing:** in type signatures, declare only what is required; rely on structural checks (`isType`, runtime checks) at call time.

---

## 16) Quick Reference (JSON Node Cheat Sheet)

```json
// Values
["null"]
["bool", true]
["int", 42]
["num", 3.14]
["str", "hello"]

["array", v1, v2, ...]
["map", ["pair", ["str","k"], v], ...]

["annot", ["str","text"], value]

// Types
["type", TypeNode]

["id","Int"]
["unop","?", ["id","Str"]]          // Str?
["array", ElemType]                 // [ElemType]
["map",
  ["pair!", ["str","name"], ["id","Str"]],
  ["pair",  ["str","age"],  ["id","Int"]]
]
["enum", memberExpr1, memberExpr2]

["binop","->", A, B]                // function/oracle type

// Functions & oracles
["fun",
  ["array", ["pair", ["id","n"], ["id","Int"]]],
  ["id","Int"],
  ["block",
    ["binop", "+", ["id","n"], ["int",1]]
  ]
]

["oracle",
  ["array", ["pair", ["id","n"], ["id","Int"]]],
  ["id","Str"],
  ["str","Say the English word for a small integer."]
]

// Calls (≥1 arg)
["call", funOrOracleExpr, arg1, arg2]

// Variables, patterns, assignment
["id","x"]
["assign", ["let", ["id","x"]], ["int",1]]
["assign", ["id","x"], ["int",2]]

["assign",
  ["let",
    ["array", ["id","a"], ["id","b"]]
  ],
  ["array", ["int",1], ["int",2]]
]

// Control flow
["block", e1, e2, ...]
["if",
  ["pair", cond1, thenBlock1],
  ["pair", cond2, thenBlock2],
  elseBlock
]
["while", condExpr, bodyBlock]
["for", patternOrLval, iterableExpr, bodyBlock]

["return", value]
["break",  value]
["continue", value]

// Access
["get", objExpr, ["str","field"]]
["idx", arrExpr, indexExpr]
["idx", mapExpr, ["str","k"]]

// Modules
["module", ["str","M"],
  ["block",
    ["assign", ["let", ["id","x"]], ["int",12]]
  ]
]

// Errors vs panics
// nullable result (soft error):
["annot", ["str","<reason>"], ["null"]]

// panic capture:
["call",
  ["id","try"],
  ["fun",
    ["array", ["pair", ["id","_"], ["id","Null"]]],
    ["id","Any"],
    ["block",
      exprThatMayPanic
    ]
  ]
]
```

**End of JSON-MindScript Manual.**
