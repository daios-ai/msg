# ===============================
# Pragmatic help with guide + env
# ===============================

# MindScript — a quick, practical guide (enough to start coding)
# ==============================================================
# 
# MindScript is a small, expression-oriented language with:
# • whitespace-sensitive delimiters for calls and indexing,
# • a JSON-friendly structural type system,
# • ordered maps/arrays, enums, functions/closures,
# • annotations (# …) that double as documentation,
# • optional “oracles” (functions whose body is fulfilled by an external model).
# 
# This document summarizes the essentials so you can read & write MindScript.
# 
# 
# 1) LEXING & LAYOUT BASICS
# -------------------------
# • Source is UTF-8; strings support JSON-style escapes and \uXXXX (with surrogate pairs).
# • Identifiers: [A-Za-z_][A-Za-z0-9_]*; keywords are reserved.
# • Annotations: any consecutive lines that (after indentation) start with ‘#’
# form one logical annotation block. The leading ‘#’ (and at most one space)
# is removed; lines are joined with “\n”. An annotation attaches to the next
# expression (PRE) or, if placed after an expression on the same line,
# attaches as POST. Most commonly you’ll use annotations as docstrings:
# 
# # Adds two numbers.
# let add = fun(a: Int, b: Int) -> Int do
# a + b
# end
# 
# • IMPORTANT whitespace rule (before '(' and '['):
# - If there is NO space before '(' then it's a call opener (CLROUND).
# Example: f(x) is a CALL. But f (x) is just grouping after f, not a call.
# - If there is NO space before '[' then it's indexing (CLSQUARE).
# Example: arr[i] indexes. But arr [i] is just grouping, not indexing.
# Practically: For function calls and indexing, do not put a space before
# the opening delimiter. For grouping, either form works.
# 
# • Period ‘.’ is context-sensitive:
# - If followed by digits (or part of a numeric literal), it's a number (.5, 1.0).
# - Otherwise it’s property access: obj.name or obj."str key".
# 
# • Tokens always end with an explicit EOF internally; you don’t write it.
# 
# 
# 2) LITERALS, VALUES, AND TYPES
# ------------------------------
# Values are JSON-like: null, bool, int, number, string, arrays, maps (ordered),
# functions, modules, and types. The type system is *structural* and JSON-friendly.
# 
# LITERALS:
# null, true/false, integer (64-bit), number (64-bit float), string ('"…"' or "'…'")
# arrays:   [1, 2, 3]
# maps:     {name: "Ada", age: 36}
# 
# MAP FIELDS & REQUIRED BANG:
# • In types, a field with ‘!’ is required. Example: {name!: Str, age: Int}
# means name is required, age is optional.
# • In values (runtime maps), you just write the key/value normally.
# 
# TYPES:
# • Any, Null, Bool, Int, Num, Str, Type are built-ins.
# • Arrays: [T]         (array of T)
# • Maps:   {k: T}      (optional key)      {k!: T} (required key)
# • Nullable: T?        (maybe-null)
# • Enums:  Enum["red", "green", "blue"] (literal set; values are those strings)
# • Function types: A -> B (used in signatures and type-level work)
# 
# TYPE ALIASES (with doc):
# # A user record.
# let User = type {name!: Str, age: Int}
# 
# # Hobbies.
# let Hobbies = type [Str]
# 
# # Status of execution.
# let Status = type Enum["ready", "running", "done"]
# 
# The leading # annotation becomes the alias’ description in generated schemas.
# 
# TYPE ANNOTATIONS inline:
# • You can annotate keys: { #(Age in years.) age: Int }
# • You can annotate any expression:  # helpful note  (expr)
# 
# STRUCTURAL SUBTYPING (informal intuition):
# • Int <: Num.  Arrays and maps check element/field shapes structurally.
# • Objects are open-world: additional fields are allowed unless restricted
# by your own logic. Required vs optional is enforced where relevant
# (e.g., type checks, assignment helpers, oracles).
# 
# 
# 3) EXPRESSIONS, OPERATORS, AND PRECEDENCE
# -----------------------------------------
# Expressions compose: literals, identifiers, calls, indexing, property get, arrays,
# maps, if/while/for blocks, function definitions, etc.
# 
# Postfix ‘?’ is a runtime "presence check" in some contexts (engine-level), but you’ll
# primarily use ‘?’ in types (nullable T?). Common arithmetic and comparison operators:
# 
# Unary:    -x, not x
# Binary:   +, -, *, /, %, <, <=, >, >=, ==, !=, and, or, =
# Precedence (high → low): unary > * / % > + - > comparisons > == != > and > or > -> > =
# Assignment '=' is right-associative and targets ids/get/idx and destructures.
# 
# PROPERTY ACCESS & INDEX:
# obj.name
# obj."str key"
# arr[i]          # (no space before '[')
# (expr)          # grouping
# 
# CALLS:
# f(x, y)         # (no space before '('); with a space ‘f (x)’ is grouping, not a call.
# 
# ARRAYS & MAPS:
# ["a", "b", "c"]
# {name: "Ada", age: 36}
# 
# IF / ELIF / ELSE:
# if cond then
# ...
# elif other then
# ...
# else
# ...
# end
# 
# WHILE:
# while cond do
# ...
# end
# 
# FOR (iterator style; element target can be a destructure or declaration):
# for let x in xs do
# ...
# end
# 
# DO/END blocks (as expressions):
# do
# expr1
# expr2
# end
# The value of a block is the last non-blank expression inside it.
# 
# RETURN / BREAK / CONTINUE:
# return expr
# break value
# continue value
# 
# DECLARATIONS:
# let x                 # defines x = null
# let x = expr          # define with value
# let {a, b!: c} = v    # object destructure patterns (with required)
# let [x, y] = arr      # array destructure
# 
# FUNCTIONS:
# fun(params) -> RetType do
# body
# end
# 
# Example:
# let add = fun(a: Int, b: Int) -> Int do
# a + b
# end
# add(2, 3)
# 
# Parameter types are enforced at call; return values are checked against RetType.
# Functions are first-class and support currying if partially applied.
# 
# MODULES:
# module MyStuff do
# let greet = fun(name: Str) -> Str do "hi " + name end
# end
# 
# Modules evaluate to a value you can import/use (host specifics may vary).
# 
# ENUMS (value-level):
# • Enum types are type-level literals; values are typically strings matching the set.
# Example value: "ready" : Status   (type check ensures it’s one of the allowed tags).
# 
# ANNOTATIONS IN CODE:
# • As docstrings above declarations.
# • As field/key docs inside type maps.
# • As inline wrapping: #(note) expr     (useful for tooling/introspection).
# 
# BLANK LINES:
# • Inside delimiters ((), [], {}, calls/params), blank lines are ignored as whitespace.
# • At top level, blank lines are treated as no-ops.
# 
# 
# 4) ORACLES (MODEL-BACKED FUNCTIONS)
# -----------------------------------
# An oracle is declared like a function but with the ‘oracle’ keyword:
# 
# # Given a number, produce its English name.
# let numberToWord = oracle(n: Int) -> Str
# 
# Call it like a function:
# numberToWord(5)
# 
# Semantics at runtime (host + engine):
# • The engine builds a PROMPT that includes:
# - the oracle’s instruction (from the function’s annotation text),
# - parameter types and the declared *success* return type,
# - any example pairs you attach (few-shot),
# - the actual input for the call.
# • It calls a host-provided hook __oracle_execute(prompt: Str) -> Str | Null,
# expecting raw JSON (no fences) of the form {"output": <value>}.
# • The engine parses via jsonParse (provided by your std lib).
# • The output is validated against the oracle’s return type, but *operationally
# nullable*: a return type T is treated as T? at runtime, so returning
# {"output": null} is accepted (unless the type is Any, where null is just null).
# 
# EXAMPLES FOR ORACLES:
# • Attach training pairs with ‘from’:
# 
# let ex = [
# [0, "zero"],
# [1, "one"],
# [2, "two"]
# ]
# 
# # Convert a small integer to an English word.
# let numberToWord = oracle(n: Int) -> Str from ex
# 
# numberToWord(7)   # model-backed call
# 
# DOCS & SCHEMAS:
# • The # annotation on the oracle becomes its instruction text.
# • If your types are declared with annotations (e.g., let Status = type Enum[…]),
# those comments are carried into the JSON Schema as "description".
# 
# ERROR SHAPES:
# • If the backend returns non-JSON, wrong shape, or the wrong type,
# oracles return *annotated null* (a special null value with an error note).
# • If the transport fails (backend returns Null), the oracle result is Null
# (usually with an explanatory annotation).
# 
# PRACTICAL TIPS:
# • Always make return types precise to keep models constrained.
# • Prefer aliasing types with doc comments; it improves schemas and prompts.
# • Use examples sparingly but helpfully; the engine embeds them in PROMPTs.
# 
# 
# 5) PUTTING IT TOGETHER — EXAMPLES
# ---------------------------------
# Simple math:
# let sq = fun(n: Int) -> Int do n * n end
# sq(9)   # 81
# 
# Arrays & maps:
# let xs = [1, 2, 3]
# xs[0]            # 1      (no space before '[')
# 
# let person = {name: "Ada", age: 36}
# person.name      # "Ada"
# 
# Control flow:
# let sign = fun(n: Num) -> Str do
# if n < 0 then
# "neg"
# elif n == 0 then
# "zero"
# else
# "pos"
# end
# end
# 
# Destructuring:
# let {name, age} = person
# name   # "Ada"
# 
# Types:
# # Measurement in meters.
# let Meters = type Num
# 
# let dist = fun(a: Meters, b: Meters) -> Meters do
# b - a
# end
# 
# Enums & nullable:
# # Status of execution.
# let Status = type Enum["ready", "running", "done"]
# 
# let maybeStatus = fun(b: Bool) -> Status do
# if b then "ready" else "done" end
# end
# 
# let last: Status? = null
# 
# Oracle:
# # Produce a random haiku about a topic.
# let haiku = oracle(topic: Str) -> {lines!: [Str], author: Str}
# haiku("autumn").lines[0]
# 
# Module:
# module MathX do
# let mean = fun(xs: [Num]) -> Num do
# # naive mean
# let n = 0
# let s = 0.0
# for x in xs do
# n = n + 1
# s = s + x
# end
# if n == 0 then 0.0 else s / n end
# end
# end
# 
# MathX.mean([1,2,3,4])   # 2.5
# 
# 
# 6) COMMON PITFALLS & STYLE NOTES
# --------------------------------
# • Don’t put a space before '(' when calling: f(x) not f (x).
# • Don’t put a space before '[' when indexing: a[i] not a [i].
# • After '.', identifiers and quoted strings are treated as property names,
# even if they are keywords: obj."then" and obj.then are both valid.
# • Blank lines inside delimiters are ignored; at top level they’re harmless.
# • Prefer documenting with leading # blocks; they carry into schemas and tooling.
# • Prefer type aliases (let Alias = type …) for reusability and better prompts.
# 
# 
# 7) RUNTIME/ERROR MODEL (WHAT YOU’LL SEE)
# ----------------------------------------
# • Most programming errors surface as formatted “hard errors” with source carets.
# • Some operations (esp. oracles) can return an *annotated null*: a null value
# whose annotation text explains the soft failure. Handle those as needed.
# • Functions are first-class and can be partially applied (currying).
# 
# 
# 8) QUICK REFERENCE
# ------------------
# Keywords: and or not let do end return break continue
# if then elif else
# function / fun, oracle, module
# for in from while
# typecons type enum   (type-level constructs surface as ‘type’ + literals)
# 
# Operators: + - * / %  == !=  < <= > >=  and or  ->  =
# 
# Types: Any Null Bool Int Num Str Type
# [T]  { k: T, k!: T }  T?  Enum["a", "b", ...]  A -> B
# 
# Call and index: f(x)   a[i]   obj.name   obj."str key"
# 
# Declaration: let x = expr     let Alias = type { … }
# 
# Blocks: do … end   if … then … elif … else … end   while … end   for … in … do … end
# 
# That’s it — you can code productively with this. Happy hacking!
let HELP_GUIDE = true

# Oracle used to synthesize concise answers from (guide + tool index + question).
let _helpOracle = oracle(prompt: Str) -> Str from [
	[
		"TOOLS: { \"open\": {\"type\":\"fun\", \"doc\":\"Open a file.\"} }\nGUIDE:\n(…)\nQUESTION:\nHow do I open a file?\nREPLY RULES:\n- brief",
		"Use `open(path, \"r\")` for a readable handle; for whole-file reads, `readFile(path)`."
	]
]

# Build a compact index and ask the oracle with the guide.
# 
# Summarizes visible names with their dynamic type and first doc line,
# then asks the oracle to answer q using that index plus the GUIDE.
# Args: q: Str
# Returns: Str? — short, pragmatic answer (annotated null on oracle failure)
let help = fun(q: Str) -> Str? do
	let snap = snapshot(null)
	let index = {}
	for [name, val] in snap do
		if substr(name, 0, 1) == "_" then
			continue
		end
		let t = str(typeOf(val))
		let doc = noteGet(val)
		let first = if doc == null then
			null
		else
			let parts = split(doc, "\n")
			if len(parts) == 0 then
				null
			else
				parts[0]
			end
		end
		index[name] = {type: t, doc: first}
	end

	let prompt = join([
		"Using ONLY the following MindScript context, answer the user's question briefly and pragmatically.",
		"",
		"TOOLS (name → {type, doc}):",
		jsonStringify(index),
		"",
		"GUIDE:",
		noteGet(HELP_GUIDE),
		"",
		"QUESTION:",
		q,
		"",
		"REPLY RULES:",
		"- 1–3 sentences, concrete.",
		"- Mention exact function names + minimal usage.",
		"- If multiple fit, give the best 1–3 options.",
		"- If nothing is relevant, say so briefly."
	], "\n")

	_helpOracle(prompt)
end