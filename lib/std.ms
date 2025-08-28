## stdlib.ms
## Minimal standard library implemented in MindScript on top of builtins.
##

## === MINDSCRIPT CODE CONVENTION
##
## 1) Naming conventions are like Go/JS:
##    - Functions and variables: camelCase
##    - Types: PascalCase
##    - Constants: SCREAMING_SNAKE_CASE
##    - Modules: (prefer single-word) snake_case
##    - Private names start with _underscore.
## 2) Errors come in two levels, Go-like:
##    - fail(msg) for contractual errors (hard fails) that cause a runtime
##      error: e.g. type mismatch, too many arguments, etc.
##      The try function can be used to catch errors.
##    - null with error annotations denote soft errors, e.g. file not found,
##      connection not open, etc. Function returning errors have a nullable
##      type.
## 3) Functions should have annotations like Python docstrings. First line
##   is a brief summary, then blank line, then detailed description with
##   explanation of arguments and return type.
## 4) Tabs for indentation.


## -----------------------------
## Error/Assert
## -----------------------------

# Produce an annotated null (soft error).
#
# Returns `null` annotated with `msg`. Use for recoverable failures.
# Args: msg: Str
# Returns: Null
let error = fun(msg: Str) -> Null do
	__annotate(msg)(null)
end

# Assert that a condition holds.
#
# Returns true if `cond` is true; otherwise throws a hard failure.
# Args: cond: Bool
# Returns: Bool
let assert = fun(cond: Bool) -> Bool do
	if cond then true else fail("assertion failed") end
end


## -----------------------------
## Console I/O
## -----------------------------

# Print a value and return it.
#
# Prints the string form of `x` without a trailing newline, and returns `x`.
# Args: x: Any
# Returns: Any
let print = fun(x: Any) -> Any do
	printf("%s", [str(x)])
	x
end

# Print a value with newline and return it.
#
# Prints the string form of `x` followed by a newline, and returns `x`.
# Args: x: Any
# Returns: Any
let println = fun(x: Any) -> Any do
	printf("%s\n", [str(x)])
	x
end


## -----------------------------
## Time & randomness
## -----------------------------

# Current timestamp in milliseconds since the Unix epoch.
#
# Thin wrapper over native `nowMillis()`.
# Args: _: Null (ignored)
# Returns: Int
let tsNow = fun(_: Null) -> Int do
	nowMillis()
end

# Random float uniformly in [0, 1).
#
# Thin wrapper over native `randFloat()`.
# Args: _: Null (ignored)
# Returns: Num
let random = fun(_: Null) -> Num do
	randFloat()
end


## -----------------------------
## Tiny internal helpers
## -----------------------------

# Build a JSON array incrementally (string-backed buffer).
#
# Returns an object with:
#   - push(x: Any)  -> Null
#   - build(_: Null)-> [Any]
# Useful to avoid repeated array reallocations.
# Args: (none)
# Returns: { push:(Any -> Null), build:(Null -> [Any]) }
let _arrayBuilder = fun() -> {push:(Any -> Null), build:(Null -> [Any])} do
	let buf = "["
	let first = true
	let push = fun(x: Any) -> Null do
		if first then
			buf = buf + jsonStringify(x)
			first = false
			null
		else
			buf = buf + "," + jsonStringify(x)
			null
		end
	end
	let build = fun(_: Null) -> [Any] do
		jsonParse(buf + "]")
	end
	{ push: push, build: build }
end


## -----------------------------
## Iterators
## -----------------------------

# Infinite iterator: 0, 1, 2, ...
#
# Produces a thunk `(Null -> Int)` that increments on each call.
# Args: _: Null (ignored)
# Returns: (Null -> Int)
let natural0 = fun(_: Null) -> (Null -> Int) do
	let i = 0
	fun(_: Null) -> Int do
		let v = i
		i = i + 1
		v
	end
end

# Infinite iterator: 1, 2, 3, ...
#
# Produces a thunk `(Null -> Int)` that starts at 1.
# Args: _: Null (ignored)
# Returns: (Null -> Int)
let natural = fun(_: Null) -> (Null -> Int) do
	let i = 1
	fun(_: Null) -> Int do
		let v = i
		i = i + 1
		v
	end
end

# Integer range iterator (stop exclusive; infinite if stop is null).
#
# Returns a thunk that yields start, start+1, ... until `stop-1`, or forever if `stop` is null.
# Args: start: Int, stop: Int?
# Returns: (Null -> Int?)
let range = fun(start: Int, stop: Int?) -> (Null -> Int?) do
	let i = start
	fun(_: Null) -> Int? do
		if stop == null then
			let v = i
			i = i + 1
			v
		elif i >= stop then
			null
		else
			let v = i
			i = i + 1
			v
		end
	end
end

# Turn arrays into (Null -> Any?) iterators; pass iterators through unchanged.
#
# If `v` is `[Any]`, returns a thunk yielding items then null; otherwise assumes `v` is already an iterator.
# Args: v: Any
# Returns: (Null -> Any?)
let iter = fun(v: Any) -> (Null -> Any?) do
	if isType(v, type [Any]) then
		let xs = v
		let i = 0
		fun(_: Null) -> Any? do
			if i < len(xs) then
				let y = xs[i]
				i = i + 1
				y
			else
				null
			end
		end
	else
		v
	end
end

# Collect an iterator into an array.
#
# Consumes `it` until it returns null.
# Args: it: (Null -> Any?)
# Returns: [Any]
let list = fun(it: (Null -> Any?)) -> [Any] do
	let b = _arrayBuilder()
	for _ in natural0(null) do
		let x = it(null)
		if x == null then break(null) end
		b.push(x)
	end
	b.build(null)
end

# Map over an iterator (lazy transform).
#
# Returns a thunk that yields `f(x)` for each `x` in `it`.
# Args: f: (Any -> Any), it: (Null -> Any?)
# Returns: (Null -> Any?)
let map = fun(f: (Any -> Any), it: (Null -> Any?)) -> (Null -> Any?) do
	fun(_: Null) -> Any? do
		let x = it(null)
		if x == null then null else f(x) end
	end
end

# Filter an iterator (lazy predicate).
#
# Returns a thunk that yields only values where `cond(x)` is true.
# Args: cond: (Any -> Bool), it: (Null -> Any?)
# Returns: (Null -> Any?)
let filter = fun(cond: (Any -> Bool), it: (Null -> Any?)) -> (Null -> Any?) do
	fun(_: Null) -> Any? do
		for _ in natural0(null) do
			let x = it(null)
			if x == null then break(null) end
			if cond(x) then return(x) end
		end
		null
	end
end

# Fold an iterator with a binary function.
#
# Reduces `it` by applying `f` left-to-right. Returns null for empty iterator.
# Args: f: (Any -> Any -> Any), it: (Null -> Any?)
# Returns: Any?
let reduce = fun(f: (Any -> Any -> Any), it: (Null -> Any?)) -> Any? do
	let first = it(null)
	if first == null then
		null
	else
		let acc = first
		for _ in natural0(null) do
			let x = it(null)
			if x == null then break(null) end
			acc = f(acc, x)
		end
		acc
	end
end


## -----------------------------
## Arrays
## -----------------------------

# Slice an array [s, e).
#
# Returns a shallow copy of `xs` from index `s` (inclusive) to `e` (exclusive).
# Args: xs: [Any], s: Int, e: Int
# Returns: [Any]
let slice = fun(xs: [Any], s: Int, e: Int) -> [Any] do
	let b = _arrayBuilder()
	for i in range(s, e) do
		b.push(xs[i])
	end
	b.build(null)
end

# Push a value to the end (returns a new array).
#
# Args: xs: [Any], v: Any
# Returns: [Any]
let push = fun(xs: [Any], v: Any) -> [Any] do
	let b = _arrayBuilder()
	for x in iter(xs) do b.push(x) end
	b.push(v)
	b.build(null)
end

# Pop the last element without modifying the input array.
#
# Args: xs: [Any]
# Returns: Any?
let pop = fun(xs: [Any]) -> Any? do
	let n = len(xs)
	if n == 0 then null else xs[n - 1] end
end

# Shift a value to the front (returns a new array).
#
# Places `v` at the head and appends all items of `xs`.
# Args: xs: [Any], v: Any
# Returns: [Any]
let shift = fun(xs: [Any], v: Any) -> [Any] do
	let b = _arrayBuilder()
	b.push(v)
	for x in iter(xs) do b.push(x) end
	b.build(null)
end

# Unshift (peek) the first element.
#
# Args: xs: [Any]
# Returns: Any?
let unshift = fun(xs: [Any]) -> Any? do
	if len(xs) == 0 then null else xs[0] end
end


## -----------------------------
## Objects (maps)
## -----------------------------

# Delete a property from a map.
#
# Returns the updated object.
# Args: obj: {}, prop: Str
# Returns: {}
let delProp = fun(obj: {}, prop: Str) -> {} do
	mapDelete(obj, prop)
end

# Whether a key exists in a map.
#
# Args: obj: {}, key: Str
# Returns: Bool
let hasKey = fun(obj: {}, key: Str) -> Bool do
	mapHas(obj, key)
end

# Get a property value.
#
# Args: obj: {}, key: Str
# Returns: Any
let getProp = fun(obj: {}, key: Str) -> Any do
	obj[key]
end

# Set a property value (returns the value).
#
# Args: obj: {}, key: Str, value: Any
# Returns: Any
let setProp = fun(obj: {}, key: Str, value: Any) -> Any do
	obj[key] = value
	value
end

# Iterator over keys of an object.
#
# Args: obj: {}
# Returns: (Null -> Str?)
let keys = fun(obj: {}) -> (Null -> Str?) do
	let it = iter(obj)
	fun(_: Null) -> Str? do
		let pair = it(null)
		if pair == null then null else pair[0] end
	end
end

# Iterator over values of an object.
#
# Args: obj: {}
# Returns: (Null -> Any?)
let values = fun(obj: {}) -> (Null -> Any?) do
	let it = iter(obj)
	fun(_: Null) -> Any? do
		let pair = it(null)
		if pair == null then null else pair[1] end
	end
end

# Directory listing of visible fields/functions.
#
# For maps: returns keys not starting with "_".
# For modules: returns exported names (not starting with "_"), or [] if unavailable.
# Args: x: Any
# Returns: [Str]
let dir = fun(x: {}) -> [Str] do
	let out = []
	for let [k, _] in x do
		if not (substr(k, 0, 1) == "_") then
			out = out + [k]
		end
	end
	out
end

# List function names of an object (if supported).
#
# Placeholder: requires native function detection support.
# Args: _obj: {}
# Returns: [Str]?
let dirFun = fun(_obj: {}) -> [Str]? do
	error("dirFun is not implemented in stdlib; requires native function detection")
end


## -----------------------------
## Importing / modules
## -----------------------------

# Create an importer from a code string.
#
# Returns a function that imports the code under a given module name.
# Args: code: Str
# Returns: (Str -> {})
let codeImport = fun(code: Str) -> (Str -> {}) do
	fun(name: Str) -> {} do
		importCode(name, code)
	end
end

# Import a module from a URL.
#
# Returns the module object, or annotated null on failure.
# Args: url: Str
# Returns: {}?
let importUrl = fun(url: Str) -> {}? do
	let r = try(fun() do import(url) end)
	if r.ok then r.value else error("import failed: " + str(r.error)) end
end


## -----------------------------
## Networking
## -----------------------------

# Simple HTTP GET helper.
#
# Performs a GET and returns the response body on HTTP 200; otherwise an annotated null.
# Args: url: Str
# Returns: Str?
let httpGet = fun(url: Str) -> Str? do
	let r = http({ "url": url, "method": "GET" })
	if r.status == 200 then
		r.body
	else
		error("http: GET " + url + " → HTTP " + str(r.status))
	end
end


## -----------------------------
## Schemas / grammars
## -----------------------------

# Render a JSON Schema for a Type.
#
# Uses native `typeToJSONSchema` and serializes to a string.
# Args: t: Type
# Returns: Str?
let jsonSchema = fun(t: Type) -> Str? do
	let r = try(fun() do typeToJSONSchema(t) end)
	if r.ok then jsonStringify(r.value) else error("jsonSchema: missing native bridge") end
end

# Produce a BNF grammar for a Type (placeholder).
#
# Requires a native bridge to be useful.
# Args: _t: Type
# Returns: Str?
let bnf = fun(_t: Type) -> Str? do
	error("bnf(Type) not implemented in stdlib; provide a native bridge")
end


## -----------------------------
## Annotations
## -----------------------------

# Get the annotation/note attached to a value, if any.
#
# Thin wrapper over native `help(x)` (if provided).
# Args: x: Any
# Returns: Str?
let getNote = fun(x: Any) -> Str? do
	help(x)
end

# Set the annotation/note on a value (placeholder).
#
# Requires native support to attach annotations at runtime.
# Args: _x: Any, _note: Str?
# Returns: Any?
let setNote = fun(_x: Any, _note: Str?) -> Any? do
	error("setNote requires native support to attach annotations at runtime")
end


## -----------------------------
## Misc
## -----------------------------

# Bind a method to a value (like partial application of receiver).
#
# Returns a unary function that calls `f(value, x)`.
# Args: value: Any, f: (Any -> Any)
# Returns: (Any -> Any)
let bindMethod = fun(value: Any, f: (Any -> Any)) -> (Any -> Any) do
	fun(x: Any) -> Any do f(value, x) end
end

# Stable-ish integer id for a value’s string form.
#
# Implements a simple DJB2-like hash over `str(value)`.
# Args: value: Any
# Returns: Int
let uid = fun(value: Any) -> Int do
	let s = str(value)
	let h = 5381
	for ch in split(s, "") do
		h = ((h * 33) + len(ch))
	end
	h
end

# Mute (sink) a value.
#
# Accepts any value and returns null.
# Args: _: Any
# Returns: Null
let mute = fun(_: Any) -> Null do
	null
end


## -----------------------------
## Oracle hook (owned by prelude/Core)
## -----------------------------

# Global oracle execution hook.
#
# Backends install their executor here via `oracleInstall`. Until installed,
# calling an oracle returns an annotated null.
# Args: _prompt: Str, _inType: Type, _outTypeNullable: Type, _examples: [Any]
# Returns: Str?
let __oracle_execute = fun(_prompt: Str, _inType: Type, _outTypeNullable: Type, _examples: [Any]) -> Str? do
	error("oracle backend not configured")
end

# Internal flag for diagnostics.
#
# Tracks whether a backend was installed via `oracleInstall`.
# Args: (none)
# Returns: (stateful)
let _oracleInstalled = false

# Install a new global oracle executor.
#
# Rebinds `__oracle_execute` to the provided function.
# Args: exec: (Str -> Type -> Type -> [Any] -> Str?)
# Returns: Null
let oracleInstall = fun(exec: (Str -> Type -> Type -> [Any] -> Str?)) -> Null do
	__oracle_execute = exec
	_oracleInstalled = true
	null
end

# Show oracle installation status.
#
# Returns a short string indicating whether a backend is installed.
# Args: _: Null (ignored)
# Returns: Str
let oracleStatus = fun(_: Null) -> Str do
	if _oracleInstalled then "oracle: installed" else "oracle: not installed" end
end


## -----------------------------
## LLM backend bootstrap
## -----------------------------

# Load the llm module (namespaced) and install its executor into the global hook.
#
# The `llm` module manages backends/models/options; this merely wires it to oracles.
# Args: (module import side effects)
# Returns: (installs __oracle_execute)
let llm = import("llm")
oracleInstall(llm.exec)


## -----------------------------
## Standard oracle sanity check (real call)
## -----------------------------

# Say hello in a surprising, playful one-liner.
#
# Produces a short, delightful greeting that directly addresses the given name.
# Single sentence, 6–14 words, plain text (no code fences). Emoji allowed.
# Runs the full oracle path: prompt build → backend exec → JSON boxing → type-check.
# Args: name: Str
# Returns: Str
let oracleHello = oracle(name: Str) -> Str from [
	["Ada", "Surprise, Ada — you just unlocked bonus sunshine today! ☀️"],
	["Sam", "Plot twist, Sam: today you’re the main character! 🎬"]
]

# Run oracleHello and report status + timing.
#
# On success, returns { ok:true, backend, model, ms, value }.
# On failure, returns an annotated null describing the problem.
# Args: name: Str
# Returns: { ok: Bool, backend: Str, model: Str?, ms: Int, value: Str }?
let oracleHelloCheck = fun(name: Str) -> {}? do
	let st = llm.status(null)
	let t0 = tsNow(null)
	let r = try(fun() do oracleHello(name) end)
	let t1 = tsNow(null)

	if r.ok then
		{ ok: true, backend: st.backend, model: st.model, ms: t1 - t0, value: r.value }
	else
		error("oracleHello failed: " + str(r.error))
	end
end
