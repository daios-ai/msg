let _genExamples = [
    ["Add two numbers (Int/Num) and return their sum as a Num.",
    [
	"annot",
	["str","Add two numbers.\n\nArgs: a:Num, b:Num\nReturn: Num"],
	[
		"fun",
		[
			"array",
			["pair", ["id","a"], ["id","Num"]],
			["pair", ["id","b"], ["id","Num"]]
		],
		["id","Num"],
		[
			"block",
			["binop","+", ["id","a"], ["id","b"]]
		]
	]
]
    ],
    [
        "Compute factorial(n) for n ≥ 0 using a while loop; return soft-null if n < 0.",
        [
	"annot",
	["str","Factorial with input check.\n\nArgs: n:Int\nReturn: Int? (annot-null if n < 0)"],
	[
		"fun",
		[
			"array",
			["pair", ["id","n"], ["id","Int"]]
		],
		["unop","?", ["id","Int"]],
		[
			"block",
			["if",
				[
					"pair",
					["binop","<", ["id","n"], ["int",0]],
					[
						"block",
						["return", ["annot", ["str","<negative input>"], ["null"]]]
					]
				],
				["block",
					["assign", ["decl","res"], ["int",1]],
					["assign", ["decl","i"], ["int",2]],
					["while",
						["binop","<=", ["id","i"], ["id","n"]],
						[
							"block",
							["assign", ["id","res"], ["binop","*", ["id","res"], ["id","i"]]],
							["assign", ["id","i"], ["binop","+", ["id","i"], ["int",1]]]
						]
					],
					["id","res"]
				]
			]
		]
	]
]

    ],
    [
        "Pick a primary color.",
[
	"annot",
	["str","Pick a primary color.\n\nArgs: none\nReturn: Enum(red|green|blue)? (soft-null if unavailable)"],
	[
		"oracle",
		["array"],
		["enum", ["str","red"], ["str","green"], ["str","blue"]],
		[
			"map",
			["pair", ["str","doc"], ["str","Pick one of the allowed colors."]],
			["pair", ["str","examples"],
				[
					"array",
					["array", ["array"], ["str","red"]],
					["array", ["array"], ["str","green"]],
					["array", ["array"], ["str","blue"]]
				]
			]
		]
	]
]

    ],
    [
        "Suggest a concise git commit message from a diff.",
        [
	"annot",
	["str","Generate a short, imperative git commit message.\n\nArgs: diff:Str\nReturn: Str? (annot-null if cannot summarize)"],
	[
		"oracle",
		[
			"array",
			["pair", ["id","diff"], ["id","Str"]]
		],
		["id","Str"],
		[
			"map",
			["pair", ["str","doc"], ["str","Summarize the diff into a concise commit subject (<= 72 chars)."]],
			["pair", ["str","examples"],
				[
					"array",
					[
						"array",
						["array", ["str","+ add input validation to /login"]],
						["str","add input validation for /login"]
					],
					[
						"array",
						["array", ["str","- remove dead code in UserService"]],
						["str","remove dead code in UserService"]
					]
				]
			]
		]
	]
]

    ],
    [
        "Parse a URL string and return its host; soft-null if invalid URL.",
        [
	"annot",
	["str","Extract host from URL.\n\nArgs: url:Str\nReturn: Str? (annot-null if invalid)"],
	[
		"fun",
		[
			"array",
			["pair", ["id","url"], ["id","Str"]]
		],
		["unop","?", ["id","Str"]],
		[
			"block",
			["assign", ["decl","u"], ["call", ["id","urlParse"], ["id","url"]]],
			["if",
				[
					"pair",
					["binop","==", ["id","u"], ["null"]],
					[
						"block",
						["return", ["annot", ["str","<invalid url>"], ["null"]]]
					]
				],
				[
					"block",
					["idx", ["id","u"], ["str","host"]]
				]
			]
		]
	]
]

    ],
]

# Write a JSON-MindScript anonymous function or oracle for the requested behavior.
#
# The JSON-MindScript programming language is described below.
#
# # JSON-MindScript Manual
#
# **MindScript** is a machine-readable, JSON-formatted, S-expressions language. Every construct is an expression. Every source file is **strict JSON**.
#
# ---
#
# ## Parsing Rule (Non-negotiable)
#
# * Sources must be **pure JSON**. No `//…`, `/*…*/`, `#…`, trailing commas, or JSON5/JSONC features.
# * Implementations must reject invalid JSON **before** evaluation.
#
# **Self-test:** your file should pass a vanilla JSON parser (e.g., `jq -e .`).
#
# **Negative (invalid) examples — do not copy:**
#
# **N-1**
#
# ```json
# ["array",
#   /* first guess */ ["int", 1],
#   ["int", 2]
# ]
# ```
#
# **N-2**
#
# ```json
# ["map",
#   ["pair", ["str","name"], ["str","Ada"]], 
# ]
# ```
#
# ---
#
# ## Core Values & Evaluation
#
# **Tagged values:**
# `["null"]`, `["bool", true|false]`, `["int", <int64>]`, `["num", <float64>]`, `["str", <string>]`
# Arrays: `["array", v1, v2, ...]`
# Maps: `["map", ["pair", ["str","k"], v], ...]`
# Functions: `["fun", paramsArray, retTypeOrAny, bodyBlock]`
# Oracles: `["oracle", paramsArray, retType, optsMap?]` (§12)
# Modules: `["module", nameExpr, bodyBlock]`
# Types (first-class): `["type", typeExpr]`
# Annotations: `["annot", ["str", textOr<text>], value]` (§10)
#
# **Evaluation order:** strict left→right for receivers, operands, and arguments.
# **Equality & comparison:** deep for arrays/maps (map key order ignored). Numbers compare by value (`2 == 2.0`). Relational `< <= > >=` only **number–number** (Int and Num mix OK) or **string–string**; other mixes are **hard errors**. Annotations are ignored.
#
# **Errors:**
# Hard: `fail(msg)` aborts; catch with `["call","try", expr]`.
# Soft: annotated null → `["annot", ["str","<reason>"], ["null"]]`.
#
# **Examples**
#
# ```json
# ["int", 4]
# ```
#
# ```json
# ["array", ["int",1], ["int",2], ["int",3]]
# ```
#
# ```json
# ["map", ["pair", ["str","name"], ["str","Ada"]]]
# ```
#
# ```json
# ["binop","==", ["int",2], ["num",2.0]]
# ```
#
# ---
#
# ## Operators & Access
#
# **Unary:** `["unop","-", x]`, `["unop","not", x]`, `["unop","?", T]` (nullable type constructor).
# **Binary:** arithmetic `+ - * / %` (numeric promotion; `%` follows dividend sign), comparisons, `== !=`, boolean `and`/`or` (short-circuit; booleans only).
#
# **Access & call:**
# Property: `["get", obj, ["str","name"]]`
# Index array: `["idx", arr, i]` (0-based; negative counts from end)
# Index map: `["idx", map, ["str","k"]]` (missing ⇒ soft annotated null)
# Call: `["call", callee, a1, ...]` (L→R evaluation)
#
# **Examples**
#
# ```json
# ["binop","+", ["int",2], ["num",2.5]]
# ```
#
# ```json
# ["binop","and", ["bool", false], ["call", ["id","expensive"]]]
# ```
#
# ```json
# ["idx", ["array", ["int",10], ["int",20]], ["int",1]]
# ```
#
# ```json
# ["idx", ["map", ["pair", ["str","a"], ["int",1]]], ["str","b"]]
# ```
#
# ```json
# ["call", ["id","sqrt"], ["num",9.0]]
# ```
#
# ---
#
# ## Collections
#
# Arrays: 0-based; negative indices count backward.
# Maps: string keys; later duplicates overwrite; **open-world** (unknown keys allowed).
#
# **Examples**
#
# ```json
# ["idx", ["array", ["int",1], ["int",2], ["int",3]], ["int",-1]]
# ```
#
# ```json
# ["idx",
#   ["map",
#     ["pair", ["str","x"], ["int",1]],
#     ["pair", ["str","x"], ["int",2]]
#   ],
#   ["str","x"]
# ]
# ```
#
# ---
#
# ## Variables & Assignment (Patterns)
#
# Identifiers: `["id","name"]`.
#
# Patterns:
# `["decl","x"]` | `["darr", p1, p2, ...]` | `["dobj", ["pair", ["str","k"], subPattern], ...]`
#
# * **Bind**: target is a pattern → introduces names.
# * **Update**: target is l-value (`"id"|"get"|"idx"`) → updates existing; missing binding ⇒ **hard**.
# * **Destructuring**: missing entries bind `["null"]`; extra array items ignored unless matched.
#
# **Examples**
#
# ```json
# ["assign", ["decl","x"], ["int",10]]
# ```
#
# ```json
# ["block",
#   ["assign", ["decl","x"], ["int",1]],
#   ["assign", ["id","x"], ["int",2]],
#   ["id","x"]
# ]
# ```
#
# ```json
# ["assign", ["darr", ["decl","a"], ["decl","b"]], ["array", ["int",1], ["int",2], ["int",3]]]
# ```
#
# ```json
# ["assign",
#   ["dobj",
#     ["pair", ["str","name"], ["decl","n"]],
#     ["pair", ["str","age"],  ["decl","a"]]
#   ],
#   ["map", ["pair", ["str","name"], ["str","Ada"]]]
# ]
# ```
#
# ---
#
# ## Control Flow
#
# Block returns last value (or `["null"]` if empty):
#
# ```json
# ["block", ["int",1], ["int",2]]
# ```
#
# If / elif / else:
#
# ```json
# ["if",
#   ["pair", ["bool", false], ["block", ["str","no"]]],
#   ["block", ["str","yes"]]
# ]
# ```
#
# While:
#
# ```json
# ["block",
#   ["assign", ["decl","i"], ["int",0]],
#   ["while", ["binop","<", ["id","i"], ["int",3]],
#     ["block", ["assign", ["id","i"], ["binop","+", ["id","i"], ["int",1]]]]
#   ],
#   ["id","i"]
# ]
# ```
#
# For (arrays, maps, modules, host iterables):
#
# ```json
# ["block",
#   ["assign", ["decl","sum"], ["int",0]],
#   ["for", ["decl","x"], ["array", ["int",1], ["int",2], ["int",3]],
#     ["block", ["assign", ["id","sum"], ["binop","+", ["id","sum"], ["id","x"]]]]
#   ],
#   ["id","sum"]
# ]
# ```
#
# `return` / `break` / `continue` carry values (default `["null"]`):
#
# ```json
# ["block",
#   ["while", ["bool", true],
#     ["block",
#       ["break", ["str","done"]]
#     ]
#   ]
# ]
# ```
#
# ---
#
# ## Types (Structural, First-Class)
#
# Base IDs: `Any | Null | Bool | Int | Num | Str | Type`
# Nullable: `["unop","?", T]` (`T?`)
# Arrays: `["array", T]`
# Maps (open): `["map", ["pair" | "pair!", ["str","k"], T] ...]` — `pair!` = required
# Enums: `["enum", expr1, expr2, ...]`
# Functions/Oracles: `["binop","->", A, B]` (right-assoc)
#
# **Typing & subtyping:**
# `Any` top; `Int <: Num`; `T?` matches `["null"]` or `T`.
# Arrays covariant; maps fieldwise (requiredness only becomes **more** required, not less); enums by member subset.
# Functions/oracles: **params contravariant**, **returns covariant**.
# **LUB (⊔):** `Any` absorbs; `Int ⊔ Num = Num`; arrays elementwise; maps fieldwise (required OR); enums union; functions/oracles pointwise (param GLB, return LUB), else `Any`.
#
# **Examples**
#
# ```json
# ["type", ["id","Int"]]
# ```
#
# ```json
# ["type", ["array", ["unop","?", ["id","Str"]]]]
# ```
#
# ```json
# ["type",
#   ["map",
#     ["pair!", ["str","name"], ["id","Str"]],
#     ["pair",  ["str","age"],  ["id","Int"]]
#   ]
# ]
# ```
#
# ```json
# ["type", ["enum", ["str","GET"], ["str","POST"]]]
# ```
#
# ---
#
# ## Functions & Calls
#
# Form:
#
# ```json
# ["fun",
#   ["array", ["pair", ["id","name"], typeOrAny], ...],
#   retTypeOrAny,
#   bodyBlock]
# ```
#
# Rules:
#
# * Each argument must be a **subtype** of its parameter.
# * The **actual returned value** must be a subtype of the declared return type.
# * Violations are **hard** errors.
# * Calls are **curried**.
#
# Docstrings: wrap public functions with `["annot", "...", fun]`.
#
# **Examples**
#
# ```json
# ["assign", ["decl","inc"],
#   ["annot", ["str","Increment an Int by 1.\n\nArgs: n:Int\nReturn: Int"],
#     ["fun",
#       ["array", ["pair", ["id","n"], ["id","Int"]]],
#       ["id","Int"],
#       ["block", ["binop","+", ["id","n"], ["int",1]]]
#     ]
#   ]
# ]
# ```
#
# ```json
# ["call", ["id","inc"], ["int",41]]
# ```
#
# ```json
# ["call", ["id","inc"], ["num",2.5]]
# ```
#
# ---
#
# ## Modules
#
# `["module", nameExpr, bodyBlock]` yields a module value.
#
# * Behaves like a **map** for typing/iteration; `get`/assignment reflect exports.
# * **Encapsulation:** module body has its own namespace; outer bindings aren’t visible unless passed in.
# * Module types normalize to map types.
#
# **Example**
#
# ```json
# ["block",
#   ["assign", ["decl","M"],
#     ["module", ["str","M"],
#       ["block",
#         ["assign", ["decl","x"], ["int",12]],
#         ["assign", ["decl","id"],
#           ["fun", ["array", ["pair", ["id","y"], ["id","Any"]]], ["id","Any"], ["block", ["id","y"]]]
#         ]
#       ]
#     ]
#   ],
#   ["get", ["id","M"], ["str","x"]]
# ]
# ```
#
# ---
#
# ## Iteration
#
# * `["for", pOrLval, iterable, body]` over arrays (elements), maps (yields `["array", ["str",k], v]`), modules (exported bindings), host iterables (may soft-end via annotated null).
# * `["while", cond, body]`. Loop value is last evaluated expression; `break`/`continue` carry values.
#
# **Example (map iteration)**
#
# ```json
# ["for", ["darr", ["decl","k"], ["decl","v"]],
#   ["map", ["pair", ["str","a"], ["str","1"]]],
#   ["block", ["id","k"]]
# ]
# ```
#
# ---
#
# ## Annotations & Comments
#
# Only legal way to add comments/metadata and to encode **soft errors**.
#
# Form: `["annot", ["str","text" or "<text>"], value]`
#
# * `"text"` = human docs (pre)
# * `"<text>"` = machine-generated metadata (post), e.g., soft error reason
#
# Semantics: evaluates `value`, returns its exact result and type; annotations ignored for equality/subtyping.
#
# **Soft error example**
#
# ```json
# ["annot", ["str","<not found>"], ["null"]]
# ```
#
# **Placement cookbook**
#
# * Assignment RHS:
#
# ```json
# ["assign", ["decl","x"], ["annot", ["str","counter"], ["int",1]]]
# ```
#
# * Function docstring:
#
# ```json
# ["annot", ["str","Greet.\n\nArgs: name:Str\nReturn: Str"],
#   ["fun",
#     ["array", ["pair", ["id","name"], ["id","Str"]]],
#     ["id","Str"],
#     ["block", ["call", ["id","sprintf"], ["str","Hello, %s"], ["id","name"]]]
#   ]
# ]
# ```
#
# * Return value:
#
# ```json
# ["return", ["annot", ["str","fast path"], ["id","result"]]]
# ```
#
# * Call (side effect):
#
# ```json
# ["annot", ["str","log greeting"], ["call", ["id","write"], ["id","STDOUT"], ["str","Hello"]]]
# ```
#
# ---
#
# ## Standard Library Convention
#
# * Prefer **soft failures** (`T?` + annotated nulls) when uncertainty/availability is expected.
# * Use `["call","try", expr]` to capture **hard** failures as `{ ok: Bool, value: Any, error: Str? }`.
# * Public APIs should avoid exposing raw `["null"]`.
#
# **Example**
#
# ```json
# ["call", ["id","try"], ["call", ["id","fail"], ["str","boom"]]]
# ```
#
# ---
#
# ## Oracles (Typed Generative Calls)
#
# Value form:
#
# ```json
# ["oracle",
#   ["array", ["pair", ["id","name"], type], ...],
#   retType,
#   ["map",
#     ["pair", ["str","doc"], ["str","..."]],
#     ["pair", ["str","examples"], ["array",
#       ["array", inputArray1, output1]
#     ]]
#   ]?]
# ```
#
# Calling rules:
#
# * Parameter arity/types are **hard-checked**.
# * Output is treated as `R?` at the boundary:
#
#   * If generated candidate conforms to `R`, return it.
#   * Otherwise/unavailable ⇒ annotated null (soft). Output never hard-fails.
#
# **Example**
#
# ```json
# ["block",
#   ["assign", ["decl","chooseColor"],
#     ["oracle",
#       ["array"],
#       ["type", ["enum", ["str","red"], ["str","green"], ["str","blue"]]],
#       ["map", ["pair", ["str","doc"], ["str","Pick a primary color."]]]
#     ]
#   ],
#   ["call", ["id","chooseColor"]]
# ]
# ```
#
# ---
#
# ## Conformant Illustrations
#
# **Missing key ⇒ soft**
#
# ```json
# ["idx", ["map", ["pair", ["str","a"], ["int",1]]], ["str","b"]]
# ```
#
# **Required/optional type**
#
# ```json
# ["type",
#   ["map",
#     ["pair!", ["str","id"], ["id","Int"]],
#     ["pair",  ["str","name"], ["id","Str"]]
#   ]
# ]
# ```
#
# **Strict function check**
#
# ```json
# ["call",
#   ["fun",
#     ["array", ["pair", ["id","n"], ["id","Int"]]],
#     ["id","Int"],
#     ["block", ["binop","+", ["id","n"], ["int",1]]]
#   ],
#   ["num", 2.5]
# ]
# ```
#
# **Short-circuit boolean**
#
# ```json
# ["binop","and", ["bool", false], ["call", ["id","f"]]]
# ```
#
# **Encapsulated module**
#
# ```json
# ["block",
#   ["assign", ["decl","x"], ["int",10]],
#   ["module", ["str","M"],
#     ["block",
#       ["assign", ["decl","x"], ["int",12]]
#     ]
#   ]
# ]
# ```
#
# ---
#
# ## Simplifying Assumptions
#
# * Omitted types default to `Any` (discouraged for public APIs).
# * Numeric promotion; `/` may yield `Num`; `%` follows dividend sign.
# * Maps are open-world; requiredness is **type-only**.
# * Missing map key / array OOB ⇒ **soft** annotated null.
# * Cross-type comparisons (except Int/Num numeric) ⇒ **hard**.
# * Annotations never affect typing/equality.
# * Modules act like maps for typing/iteration; get/set reflect exports; namespaces are encapsulated.
# * Oracles: inputs hard-checked; outputs treated as `R?`.
#
# **Example**
#
# ```json
# ["binop","/", ["int",1], ["int",2]]
# ```
#
# ---
#
# ## Code Conventions
#
# * Naming: modules `snake_case`; funcs/vars `camelCase`; types `PascalCase`; consts `SCREAMING_SNAKE_CASE`; private `_prefix`.
# * Docs: each public function has a concise docstring `annot` (one-line summary; blank line; args/return; soft-error notes).
# * Types: public functions declare explicit param & return types; use `T?` only where soft errors are possible.
# * Indentation: tabs (style).
# * No “void”: every expression yields a value; avoid exposing raw `["null"]`.
# * JSON hygiene: no comments, no trailing commas, no JSONC/JSON5.
#
# ---
#
# ## 16) Built-in Functions (Practical Reference)
#
# Below are brief, task-oriented descriptions you can code against. Return types follow MindScript type notation; `?` = nullable (soft failure possible).
#
# ### Constants & Handles
#
# * **PI, E : Num** — Math constants.
# * **STDIN / STDOUT / STDERR : Any** — Standard stream handles (read/write with I/O funcs).
#
# ```json
# ["call", ["id","write"], ["id","STDOUT"], ["str","Hello\n"]]
# ```
#
# ### Introspection & Environment
#
# * **bindings : (Bool?) → Map** — Visible name→value snapshot; pass `true` for current frame only; otherwise merged view. Read-only, shallow.
# * **snapshot : Null → Map** — Deep, shadow-aware map of all visible bindings (incl. Core). Per-key annotations mirror source values.
# * **typeOf : Any → Type** — Dynamic type tag of a value.
# * **isType : Any → (Type → Bool)** — Predicate: does value conform to a type?
# * **isSubtype : Type → (Type → Bool)** — Predicate on types.
#
# ```json
# ["call", ["id","typeOf"], ["int",7]]
# ```
#
# ### AST, Reflection, Codecs
#
# * **astParse : Str → Array** — Parse MindScript source to runtime-S AST; annot-null on lex/parse error.
# * **astEval : Array → Any** — Evaluate runtime-S AST in caller scope (effects occur there).
# * **reflect : Any → Array** — Constructor program that rebuilds the value.
# * **reify : Array → Any** — Validate & evaluate constructor program in persistent env.
# * **jsonParse : Str → Any** — JSON text → values (Null/Bool/Int/Num/Str/\[Any]/{Str\:Any}).
# * **jsonStringify : Any → Str?** — Value → compact JSON; annot-null if not encodable (e.g., cyclic or non-JSON types).
# * **base64Encode/Decode : Str → Str / Str → Str?** — Raw bytes in `Str`.
# * **hexEncode/Decode : Str → Str / Str → Str?**
# * **gzipCompress/Decompress : Str → Str / Str → Str?**
#
# ```json
# ["call", ["id","jsonStringify"], ["map", ["pair", ["str","k"], ["int",1]]]]
# ```
#
# ### URL & HTTP
#
# * **urlParse : Str → Map?** — Returns `{ scheme!, host!, port?, path!, query!, fragment? }` or annot-null if invalid.
# * **urlBuild : Map → Str** — Components → URL.
# * **urlQueryParse : Str → Map?** — `"a=1&b=2&b=3"` → `{ "a":["1"], "b":["2","3"] }`.
# * **urlQueryString : Map → Str** — Map → percent-encoded query (no leading `?`).
# * **http : Map → Map?** — Buffered fetch. Input keys: `method`, `url`, `headers`, `body`, timeouts, etc. Returns `{ status!, statusText, headers!, body!, url, proto, durationMs }` or annot-null on network/IO error.
# * **httpStream : Map → Map?** — Streaming variant; returns `{ bodyH! }` handle on success.
#
# ```json
# ["call", ["id","urlParse"], ["str","https://example.com/a?b=1"]]
# ```
#
# ### Filesystem & OS
#
# * **cwd : Null → Str?** — Current directory; annot-null if unavailable.
# * **chdir : Str → Bool** — Change current directory.
# * **open : Str → (Enum("r","w","a","rw") → Any)** — Open file; yields handle or annot-null on failure.
# * **readFile : Str → Str?** — Entire file content.
# * **writeFile : Str → (Str → Int?)** — Write content; returns bytes written or annot-null.
# * **remove / rename / mkdir : Str → Bool / Str→(Str→Bool) / Str→Bool** — Basic FS ops.
# * **dirList : Str → \[Str]?** — Directory entries.
# * **stat : Str → Map?** — Common metadata.
# * **pathBase / pathDir / pathExt / pathClean : Str → Str** — Path utilities.
# * **pathJoin : \[Str] → Str** — Join with platform separator.
# * **osEnv / osSetEnv : Str → Str? / Str → (Str? → Bool)** — Get/set env vars.
#
# ```json
# ["call", ["id","pathJoin"], ["array", ["str","/tmp"], ["str","file.txt"]]]
# ```
#
# ### Handles I/O
#
# * **readAll : Any → Str?** — Read remaining bytes from a readable handle.
# * **readN : Any → (Int → Str?)** — Read N bytes.
# * **readLine : Any → Str?** — Read a line; annot-null at EOF.
# * **write : Any → (Str → Int?)** — Write bytes; returns bytes written or annot-null.
# * **flush / close : Any → Bool / Bool** — Flush or close (never closes stdio).
#
# ```json
# ["call", ["id","readLine"], ["id","STDIN"]]
# ```
#
# ### Networking (basic)
#
# * **netListen : Str → Any?** — Start listener (e.g., `"tcp:127.0.0.1:8080"`).
# * **netAccept : Any → Any?** — Accept a connection.
# * **netConnect : Str → Any?** — Connect to address.
#
# ### Processes & Execution
#
# * **exec : \[Str] → (Map? → Map?)** — Run external command with optional `{ env, cwd }`; returns `{ status!, stdout!, stderr! }` or annot-null if spawn fails.
# * **exit : Int? → Null** — Terminate process with optional code.
# * **fail : Str? → Null** — Hard error (throws).
# * **try : Any → Map** — Capture hard fail: `{ ok: Bool, value: Any, error: Str? }`.
# * **procSpawn / procJoin / procCancel / procJoinAll / procJoinAny** — Concurrent subprocess control (as originally specified).
#
# ```json
# ["call", ["id","try"], ["call", ["id","fail"], ["str","nope"]]]
# ```
#
# ### Concurrency & Timers
#
# * **chanOpen / chanClose : Null→Any / Any→Bool** — Open/close channel.
# * **chanSend / chanRecv : Any→(Any→Bool) / Any→Any?** — Send/receive (non-blocking variants `chanTrySend/chanTryRecv` return Bool/Any?).
# * **ticker : Int → Any** — Periodic tick handle (ms).
# * **timerAfter : Int → Any** — One-shot tick handle (ms).
# * **sleep : Int → Null** — Block for milliseconds.
# * **nowMillis / nowNanos : Null → Int** — Time since epoch.
#
# ```json
# ["call", ["id","nowMillis"], ["null"]]
# ```
#
# ### Math
#
# * **sin / cos / tan / sqrt / pow / log / exp : Num → Num (or curried)** — Standard math.
# * **randFloat : Null → Num** — Uniform in \[0,1).
# * **randInt : Int → Int** — Uniform integer in \[0,n).
# * **seedRand : Int → Null** — Seed PRNG.
# * **randBytes : Int → Str?** — Random bytes.
#
# ```json
# ["call", ["id","randInt"], ["int",10]]
# ```
#
# ### Strings & Text
#
# * **str : Any → Str?** — Convert to string or annot-null if not encodable.
# * **toLower / toUpper / strip / lstrip / rstrip : Str → Str** — Case/whitespace utilities.
# * **substr : Str → (Int → (Int → Str))** — Rune-indexed, clamped.
# * **split : Str → (Str → \[Str])** — Empty separator splits by code point.
# * **join : \[Str] → (Str → Str)** — Join with delimiter.
# * **match : Str → (Str → \[Str])** — RE2 full-string or find-matches; returns matches (no groups).
# * **replace : Str → (Str → (Str → Str))** — Regex or literal replace depending on pattern.
# * **sprintf / printf / formatCode / formatValue** — Formatting helpers.
#
# ```json
# ["call", ["id","sprintf"], ["str","%s %d"], ["str","v"], ["int",3]]
# ```
#
# ### Conversion, Length, Maps
#
# * **int / num / bool : Any → Int/Num/Bool** — Coercions (bool uses common truthiness).
# * **len : Any → Int** — Length: arrays (# elems), maps (# keys), strings (runes).
# * **clone : Any → Any** — Deep copy arrays/maps; scalars by identity.
# * **mapHas : Map → (Str → Bool)** — Key presence.
# * **mapDelete : Map → (Str → Bool)** — Remove key; returns success.
#
# ```json
# ["call", ["id","len"], ["str","🙂"]]
# ```
#
# ### Importing & Schemas
#
# * **import / importCode : Str → Any?** — Load value or code; annot-null on failure (e.g., not found).
# * **jsonSchemaToType / jsonSchemaStringToType : Any/Str → Type?** — Convert JSON Schema → MindScript Type.
# * **typeToJSONSchema / typeStringToJSONSchema : Type/Str → Any?** — Convert Type → JSON Schema.
#
# ```json
# ["call", ["id","import"], ["str","./lib.ms.json"]]
# ```
#
# ### Cryptography
#
# * **sha256 : Str → Str** — 32-byte digest (raw bytes in `Str`).
# * **hmacSha256 : Str → (Str → Str)** — MAC (raw bytes).
# * **ctEqual : Str → (Str → Bool)** — Constant-time equality.
#
# ```json
# ["call", ["id","ctEqual"], ["call", ["id","sha256"], ["str","a"]], ["call", ["id","sha256"], ["str","a"]]]
# ```
#
# ---
#
# ### Appendix — Commenting Cheatsheet (Bad → Good)
#
# Bad:
#
# ```json
# ["assign", ["decl","x"], ["int",1]]
# ```
#
# Good:
#
# ```json
# ["assign", ["decl","x"], ["annot", ["str","counter"], ["int",1]]]
# ```
#
# Bad:
#
# ```json
# ["return", ["id","result"]]
# ```
#
# Good:
#
# ```json
# ["return", ["annot", ["str","fast path"], ["id","result"]]]
# ```
let _genOracle = oracle(spec: Str) -> []? from _genExamples


# Generate a function/oracle given a specification.
#
# Args: 
# - spec:Str A description of the desired behavior. 
#
# Return:
# - A function or oracle.
let gen = fun(spec: Str) -> Any do
    let g = _genOracle(spec)
    let validation = astValidate(g)
    print(validation)
    if validation != [] then
        return noteSet(validation[0].message, null)  
    end
    return astEval(g)
end

