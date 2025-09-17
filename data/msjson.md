# JSON-MindScript Manual

**JSON-MindScript** is a machine-readable, JSON-formatted, S-expressions language. Every construct is an expression and every source file is **strict JSON**.

It supports two kinds of computation: **deductive** (deterministic functions) and **inductive** (first-class **oracles** backed by models or external sources). Structural types are first-class. Documentation and machine metadata are carried via **annotations**.

---

## 1) Parsing & File Rules

* Sources must be **pure JSON**. No comments, trailing commas, or JSON5/JSONC features.
* Implementations must reject invalid JSON *before* evaluation.

**Self-test:** the file should pass a vanilla JSON parser (e.g., `jq -e .`).

**Invalid examples (do not copy):**

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

* **Everything is an expression.** A block returns the last subexpression (or `["null"]` if empty).
* **Strict left-to-right evaluation** for receivers, operands, and arguments.
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

* `["array", v1, v2, ...]` — 0-based; negative indices count from end.

**Maps (string-keyed)**

* `["map", ["pair", ["str","k"], v], ...]`
* `["pair!", ["str","k"], v]` is used only in **type** positions to mark “required”.
* Later duplicate keys overwrite earlier ones.
* Order of keys is preserved for display/iteration; ignored for equality/subtyping.

**Functions**

* `["fun", paramsArray, retTypeOrAny, bodyBlock]`

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
* Nullable type constructor (type-level): `["unop","?", T]` → `T?` (see §9)

**Binary**

* Arithmetic: `"+" "-" "*" "/" "%"`.

  * **Numeric tower:** `Int` (int64) and `Num` (float64).
  * **Promotion:** only when **one operand is `Num`**.
  * `/` returns `Num`.
  * `%` follows dividend sign.
  * **Division by zero panics.**
* Comparisons: `"<" "<=" ">" ">="` — only **number–number** (Int and Num mix OK) or **string–string**; other mixes **panic**.
* Equality: `"==" "!="` — deep for arrays/maps; numbers by value (`2 == 2.0` is true); annotations are ignored.
* Boolean: `"and" "or"` — short-circuit; operands must be `Bool`.

**Access & call**

* Property: `["get", obj, ["str","name"]]`
* Index array: `["idx", arr, i]` (0-based; negatives from end)
* Index map: `["idx", map, ["str","k"]]`
* Call: `["call", callee, a1, ...]` (L→R evaluation)

**Panics for invalid operations**: invalid receiver/target, missing key, out-of-bounds index, illegal comparison, division by zero.

**Examples**

```json
["binop","+", ["int",2], ["num",2.5]]
```

```json
["binop","and", ["bool", false], ["call", ["id","expensive"]]]
```

```json
["idx", ["array", ["int",10], ["int",20]], ["int",-1]]
```

```json
["get", ["id","obj"], ["str","field"]]
```

```json
["call", ["id","sqrt"], ["num",9.0]]
```

---

## 5) Collections

* **Arrays:** 0-based; `-1` refers to the last element; negatives count from end.
* **Maps:** string keys; open-world (unknown keys may exist and be ignored by types); later duplicates overwrite.

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

* Decl name: `["decl","x"]`
* Array destructure: `["darr", p1, p2, ...]`
* Object destructure: `["dobj", ["pair", ["str","k"], subPattern], ...]`

**Binding vs update**

* **Bind:** target is a *pattern* → introduces names.
* **Update:** target is an l-value (`"id"|"get"|"idx"`) → updates existing.
  Updating an unbound name, invalid property, or invalid index **panics**.

**Destructuring semantics**

* Arrays: missing entries bind `["null"]`; extra items ignored unless matched.
* Objects: missing keys bind `["null"]`.

**Assignment result**

* `["assign", target, value]` evaluates to the assigned **value**.

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

* `["block", e1, e2, ...]` → value of the last expression, or `["null"]` if empty.

**If / elif / else**

* Form: `["if", ["pair", cond1, thenBlock1], ["pair", cond2, thenBlock2], ..., elseBlock]`
  The `elseBlock` is required (use `["block"]` for empty).

**While**

* `["while", cond, bodyBlock]`

**For** (arrays, maps, modules, host iterables)

* `["for", patOrLval, iterable, bodyBlock]`

  * Arrays yield elements.
  * Maps yield `["array", ["str",key], value]` pairs.
  * Modules yield exported bindings (name/value).
  * Host iterables may end by panicking or naturally.

**Early exits**

* `["return", value]`
* `["break", value]`
* `["continue", value]`
  Exactly **one** value is required; use `["null"]` for an “empty” payload.

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

* **Error**: a *regular* result that uses a nullable type `T?` with `["annot", ["str","<msg>"], ["null"]]`. This is the idiomatic way to signal problems.
* **Panic**: an exceptional condition in the lexer/parser/runtime (e.g., invalid index, missing key, out-of-range, type violation, **division by zero**, illegal comparison). Panics abort evaluation unless captured.

**Capturing panics**

* `["call", ["id","try"], expr]` catches panics and returns a map:
  `{ ok!: Bool, value: Any, error: Str? }`
  When `ok=false`, `error` contains a message; otherwise `error` is `null` or absent.

---

## 9) Types (Structural, First-Class)

**Type IDs**

* `Any | Null | Bool | Int | Num | Str | Type`

**Nullable**

* `["unop","?", T]` renders as `T?`.

**Arrays**

* `["array", T]`

**Maps (open)**

* `["map", ["pair",  ["str","k"], T], ...]` — optional field
* `["map", ["pair!", ["str","k"], T], ...]` — **required** field

**Enums**

* `["enum", lit1, lit2, ...]` — literals may be scalars, arrays, or maps

**Functions & Oracles**

* Type arrow: `["binop","->", A, B]` (right-associative).
  `(A -> (B -> R))` ≡ `(A, B) -> R`.

**Subtyping (informal)**

* `Int <: Num`
* `T?` matches `Null` or `T`
* Arrays covariant by element type
* Maps fieldwise; requiredness may only become **more required**, not less
* Enums by member subset
* Functions/oracles: parameters **contravariant**, return **covariant**
* `Any` is top

**Type operations (inference, guidance)**

* **LUB (⊔):** `Any` absorbs; `Int ⊔ Num = Num`; arrays elementwise; maps fieldwise (required OR); enums union; functions/oracles pointwise (param GLB, return LUB), else `Any`.

**Examples**

```json
["type", ["id","Int"]]
```

```json
["type", ["array", ["unop","?", ["id","Str"]]]]
```

```json
["type",
  ["map",
    ["pair!", ["str","name"], ["id","Str"]],
    ["pair",  ["str","age"],  ["id","Int"]]
  ]
]
```

```json
["type", ["enum", ["str","GET"], ["str","POST"]]]
```

---

## 10) Annotations (Docs & Machine Metadata)

**Form**

* `["annot", ["str","text"], value]` — human-oriented documentation
* `["annot", ["str","<text>"], value]` — machine metadata (e.g., error reason)

**Semantics**

* Evaluates `value` and returns its exact result and type.
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

**Pretty-printing guidance (to surface syntax)**

* An annotation that conceptually *follows* a value on the same logical line should be rendered as a **trailing annotation** on that value. When following separators (`,` or `:`), it attaches to the last value before the separator for display.

---

## 11) Modules

`["module", nameExpr, bodyBlock]` yields a **module value**:

* Behaves like a **map** for typing/iteration.
* **Namespace encapsulation:** module body has its own scope; outer bindings are not visible unless passed in.
* **Writability:** `get`/`set` reflect **exports**; writes update the export table (overwrites allowed).

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
* The **actual returned value** must be a subtype of the declared return type.
* Violations **panic**.
* Calls are **curried** semantically; JSON call form is `["call", f, a1, ...]`.

**Oracles (typed generative calls)**

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

Calling rules:

* Parameter arity/types are **checked**; mismatch **panics**.
* Output is treated as `R?` at the boundary: if the generated candidate conforms to `R`, return it; otherwise return `["annot", ["str","<reason>"], ["null"]]`. Unavailability or mismatch of the model output does not cause a panic.

---

## 13) Equality, Comparison & Truthiness

* `==` / `!=` are deep for arrays and maps (map key order ignored). Numbers compare by value; `2 == 2.0` is true.
* `< <= > >=` only allow number–number or string–string comparisons; anything else **panics**.
* **Truthiness** (for `bool` coercion): **falsey** = `null`, `0`, `0.0`, `""`, `[]`, `{}`; everything else is truthy for ints, nums, strings, arrays, maps. Non-convertible values cause `bool(x)` to return an **error** (`Bool?`).

---

## 14) Code Conventions

* Names: modules `snake_case`; funcs/vars `camelCase`; types `PascalCase`; consts `SCREAMING_SNAKE_CASE`; private `_prefix`.
* Docs: public functions carry a concise `annot` docstring (one-line summary; blank line; Args/Return; error notes if any).
* Types: public APIs declare explicit param & return types; use `T?` only when an error/absence is possible.
* No “void”: every expression yields a value; avoid exposing raw `["null"]` in public APIs unless type is `T?`.
* **Indentation:** tabs.
* **Canonical pretty-printers** should end files with **exactly one trailing newline**.

---

## 15) Simplifying Assumptions (Runtime)

* **Int** is **int64**; **Num** is **float64**.
* Numeric promotion only when one operand is `Num`. `/` returns `Num`. `%` follows dividend sign.
* Maps are open-world; requiredness is a type property.
* **Missing map key** / **array out-of-bounds** **panic**.
* Annotations never affect equality or subtyping.
* Modules act like maps; `get`/`set` reflect exports; namespaces are encapsulated.

---

## 16) Built-in Functions (Practical Reference)

Types below use MindScript type notation; `?` = nullable (error possible). When a function may **panic**, it is noted.

### Constants & Handles

* **PI, E : Num**
* **STDIN / STDOUT / STDERR : Any**

```json
["call", ["id","write"], ["id","STDOUT"], ["str","Hello\n"]]
```

### Introspection, AST & Types

* **snapshot : Null → {}**
* **typeOf : Any → Type**
* **isType : Any → (Type → Bool)**
* **isSubtype : Type → (Type → Bool)**
* **uid : Any → Int**
* **formatValue : Any → Str**
* **formatCode : Str → Str?**
* **reflect : Any → \[Any]?**
* **reify : \[Any] → Any**
* **astParse : Str → \[Any]?**
* **astValidate : \[Any] → \[Any]**
* **astEval : \[Any] → Any**
* **astFormat : \[Any] → Str?**
* **dir : {} → \[Str]**

### Errors & Control

* **try : Any → { ok!: Bool, value: Any, error: Str? }**
* **assert : Bool → Bool** — **panics** if argument is false.
* **fail : Str? → Null** — **panic** with a message.
* **error : Str → Null** — Produce an error value by returning `["annot", ["str","<msg>"], ["null"]]`.

### Conversion & Length

* **bool : Any → Bool?** — Falsey: `null`, `0`, `0.0`, `""`, `[]`, `{}`; otherwise truthy; error if not convertible.
* **int : Any → Int?**
* **num : Any → Num?**
* **str : Any → Str?**
* **len : Any → Int?**

### Strings & Text

* **join : \[Str] → (Str → Str)**
* **split : Str → (Str → \[Str])**
* **match : Str → (Str → \[Str])**
* **replace : Str → (Str → (Str → Str))**
* **sprintf / printf : Str → (\[Any] → Str?)**
* **substr : Str → (Int → (Int → Str))**
* **toLower / toUpper / strip / lstrip / rstrip : Str → Str**
* **noteGet : Any → Str?**
* **noteSet : Str → (Any → Any)**

### Math

* **sin | cos | tan | sqrt | exp | log | pow : Num → Num** (curried supported)

### Randomness & Crypto

* **seedRand : Int → Null**
* **randFloat : Null → Num**
* **randInt : Int → Int**
* **randBytes : Int → Str?**
* **sha256 : Str → Str**
* **hmacSha256 : Str → (Str → Str)**
* **ctEqual : Str → (Str → Bool)**

### Time & Scheduling

* **dateNow : Null → {}**
* **nowMillis / nowNanos : Null → Int**
* **sleep : Int → Null**
* **ticker : Int → Any**
* **timerAfter : Int → Any**
* **timeFormatRFC3339 : Int → Str**
* **timeParseRFC3339 : Str → Int?**

### Channels (CSP-style)

* **chanOpen : Null → Any**
* **chanClose : Any → Bool**
* **chanSend : Any → (Any → Null)**
* **chanRecv : Any → Any**
* **chanTrySend : Any → (Any → Bool)**
* **chanTryRecv : Any → {}?**

### Iterators & Sequences

* **iter : Any → (Null → Any?)** — Arrays/maps → iterator; iterators pass through.
* **map : (Any → Any) → ((Null → Any?) → (Null → Any?))**
* **filter : (Any → Bool) → ((Null → Any?) → (Null → Any?))**
* **reduce : ((Any, Any) → Any) → ((Null → Any?) → Any?)**
* **list : (Null → Any?) → \[Any]**
* **range : Int → (Int? → (Null → Int?))** — `[start, stop)`; infinite if `stop=null`.
* **naturals : Null → (Null → Int)**
* **naturals0 : Null → (Null → Int)**
* **keys : {} → (Null → Str?)**
* **values : {} → (Null → Any?)**

### Arrays (in-place helpers)

* **push : \[Any] → (Any → \[Any])** — Append **in place**; returns the same array.
* **unshift : \[Any] → (Any → \[Any])** — Prepend **in place**; returns the same array.
* **pop : \[Any] → Any** — Remove & return last element; **panics if empty**.
* **shift : \[Any] → Any** — Remove & return first element; **panics if empty**.
* **slice : \[Any] → (Int → (Int → \[Any]))** — Non-mutating.

### Maps (helpers)

* **mapDelete : {} → (Str → {})** — Delete key **in place**; preserves order and per-key annotations; returns the **same** map.
* **mapHas : {} → (Str → Bool)**

### Files, Paths & OS

* **cwd : Null → Str?**
* **chdir : Str → Bool**
* **dirList : Str → \[Str]?**
* **mkdir : Str → Bool**
* **open : Str → (Enum("r","w","a","rw") → Any?)**
* **readAll : Any → Str?**
* **readN : Any → (Int → Str?)**
* **readLine : Any → Str?**
* **write : Any → (Str → Int?)**
* **flush : Any → Bool**
* **close : Any → Bool**
* **readFile : Str → Str?**
* **writeFile : Str → (Str → Int?)**
* **remove : Str → Bool**
* **rename : Str → (Str → Bool)**
* **stat : Str → {isDir!: Bool, modTimeMillis!: Int, mode!: Int, size!: Int}?**
* **pathBase / pathDir / pathExt / pathClean : Str → Str**
* **pathJoin : \[Str] → Str**
* **tempDir : Null → Str**

### Environment & Importing

* **osEnv : Str → Str?**
* **osSetEnv : Str → (Str? → Bool)**
* **import : Str → Any?**
* **importCode : Str → (Str → Any)**
* **codeImport : Str → (Str → {})**
* **importUrl : Str → {}?**

### URL & HTTP

* **urlParse : Str → {}?** — `{ scheme!, host!, port?, path!, query!, fragment? }` or error.
* **urlBuild : {} → Str**
* **urlQueryParse : Str → {}?** — `"a=1&b=2&b=3"` → `{ "a":["1"], "b":["2","3"] }`.
* **urlQueryString : {} → Str**
* **http : {} → {}?** — Buffered fetch. Returns `{ status!, statusText, headers!, body!, url, proto, durationMs }` or error.
* **httpStream : {} → {}?** — Streaming variant; returns `{ bodyH! }` handle on success.

### Processes, Exec & Exit

* **procSpawn : Any → Any**
* **procCancel : Any → Null**
* **procCancelled : Any → Bool**
* **procJoin : Any → Any**
* **procJoinAll : \[Any] → \[Any]**
* **procJoinAny : \[Any] → { index!: Int, value: Any }?**
* **exec : \[Str] → ({}? → {}?)** — Run external command with optional `{ env, cwd }`; returns `{ status!, stdout!, stderr! }` or error.
* **exit : Int? → Null**

### Networking (basic)

* **netListen : Str → Any?** — Start listener (e.g., `"tcp:127.0.0.1:8080"`).
* **netAccept : Any → Any?** — Accept a connection.
* **netConnect : Str → Any?** — Connect to address.

### Serialization, Schemas & Encoding

* **jsonParse : Str → Any**
* **jsonStringify : Any → Str?**
* **jsonSchemaToType : Any → Type?**
* **jsonSchemaStringToType : Str → Type?**
* **typeToJSONSchema : Type → Any**
* **typeStringToJSONSchema : Str → Any**
* **base64Encode : Str → Str** / **base64Decode : Str → Str?**
* **hexEncode : Str → Str** / **hexDecode : Str → Str?**
* **gzipCompress : Str → Str** / **gzipDecompress : Str → Str?**

### Oracles (installation & health)

* **oracleInstall : (Str → Str?) → Null** — Install global oracle executor (string in → string out or error).
* **oracleHealth : Null → {}?** — Probe executor.
* **oracleStatus : Null → Str** — Status string.
* **llm : <module>** — LLM module entrypoint.

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
["binop","and", ["bool", false], ["call", ["id","f"]]]
```

**Encapsulated, writable module**

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

---

## 18) Code Examples (Quick Start)

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

**Oracle with typed output**

```json
["assign", ["decl","chooseColor"],
  ["oracle",
    ["array"],
    ["type", ["enum", ["str","red"], ["str","green"], ["str","blue"]]],
    ["map", ["pair", ["str","doc"], ["str","Pick a primary color."]]]
  ]
]
```

---
