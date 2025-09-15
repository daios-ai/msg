## std.ms
## Minimal standard library implemented in MindScript on top of builtins.
##
## === MINDSCRIPT CODE CONVENTION
##
## 1) Naming
##	Functions/vars: camelCase; Types: PascalCase; Consts: SCREAMING_SNAKE_CASE;
##	Modules: snake_case; Private: _prefix.
##
## 2) Errors
##	Soft errors: use nullable returns (T?); return `null  # <reason>`.
##	Any? = Any → do NOT use Any for maybes; pick a concrete T.
##	Hard errors: `fail(msg)` only for invariants/programmer bugs or unrecoverable state.
##	Catch hard errors with `try`; prefer soft errors for expected conditions.
##	Caller check idiom:
##	```
##		let v = parseX(s)
##		if v == null then return null  # <invalid input> end
##		use(v)
##	```
##	Add brief, grep-able slugs: `<not found>`, `<timeout: 200ms>`.
##
## 3) Function docs (PRE)
##	First line: short summary. Then blank line. Then Args/Returns/Soft-fail notes.
##	Example:
##	```
##		# Parse URL string.
##		#
##		# Args: s: Str
##		# Returns: {…}? (null on <invalid url>)
##		fun parseUrl(s: Str) -> { scheme!:Str, host!:Str, port:Int, path!:Str, query!:Str, fragment:Str }? do
##			let u = urlParse(s); if u == null then return null  # <invalid url> end; u
##		end
##	```
##
## 4) Formatting
##	Tabs for indentation. POST forces newline and follows after-separator rule.
##	Canonical output ends with exactly one trailing newline.

## -----------------------------
## Error/Assert
## -----------------------------

# Produce an annotated null (soft error).
#
# Returns `null` annotated with `msg`. Use for recoverable failures.
# Args: msg: Str
# Returns: Null
let error = fun(msg: Str) -> Null do
	__annotate(msg, null)
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
## Iterators
## -----------------------------

# Infinite iterator: 0, 1, 2, ...
#
# Produces a thunk `(Null -> Int)` that increments on each call.
# Args: _: Null (ignored)
# Returns: (Null -> Int)
let naturals0 = fun(_: Null) -> (Null -> Int) do
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
let naturals = fun(_: Null) -> (Null -> Int) do
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

# Turn arrays or maps into (Null -> Any?) iterators; pass iterators through unchanged.
#
# - [Any]      → yields elements
# - {}         → yields [key, value] pairs
# - (Null->?)  → returned as-is
# Else: hard error.
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
	elif isType(v, type {}) then
		let kvs = []
		for let [k, val] in v do
			kvs = kvs + [[k, val]]
		end
		let i = 0
		fun(_: Null) -> Any? do
			if i < len(kvs) then
				let p = kvs[i]
				i = i + 1
				p
			else
				null
			end
		end
	elif isType(v, type (Null) -> Any?) then
		v
	else
		fail("iter: expected array, map, or iterator")
	end
end

# Collect an iterator into an array.
#
# Consumes `it` until it returns null.
# Args: it: (Null -> Any?)
# Returns: [Any]
let list = fun(it: (Null -> Any?)) -> [Any] do
	let out = []
	for _ in naturals0(null) do
		let x = it(null)
		if x == null then break(null) end
		out = out + [x]
	end
	out
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
		for _ in naturals0(null) do
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
		for _ in naturals0(null) do
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
	let out = []
	for i in range(s, e) do
		out = out + [xs[i]]
	end
	out
end

# Push a value to the end (returns a new array).
#
# Args: xs: [Any], v: Any
# Returns: [Any]
let push = fun(xs: [Any], v: Any) -> [Any] do
	xs + [v]
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
	[v] + xs
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

# Iterator over keys of an object.
#
# Args: obj: {}
# Returns: (Null -> Str?)
let keys = fun(obj: {}) -> (Null -> Str?) do
	let it = iter(obj)  # yields [k, v]
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
# If a value has a PRE note, the first line is attached as an annotation to the key.
# Args: x: {}
# Returns: [Str]
let dir = fun(x: {}) -> [Str] do
	let out = []
	for let [k, v] in x do
		if not (substr(k, 0, 1) == "_") then
			let ann = noteGet(v)
			if ann != null then
				ann = split(ann, "\n").0
				k = noteSet(ann, k)
			end
			out = out + [k]
		end
	end
	out
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
## Misc
## -----------------------------

# Stable-ish integer id for a value’s content.
#
# Hashes the UTF-8 bytes of `str(value)` via sha256, then folds over the hex string
# with a DJB2-like accumulator for a compact Int.
# Args: value: Any
# Returns: Int
let uid = fun(value: Any) -> Int do
	let hx = hexEncode(sha256(str(value)))
	let h = 5381
	let n = len(hx)
	let i = 0

	let hexVal = fun(c: Str) -> Int do
		if c == "0" then 0
		elif c == "1" then 1
		elif c == "2" then 2
		elif c == "3" then 3
		elif c == "4" then 4
		elif c == "5" then 5
		elif c == "6" then 6
		elif c == "7" then 7
		elif c == "8" then 8
		elif c == "9" then 9
		elif c == "a" or c == "A" then 10
		elif c == "b" or c == "B" then 11
		elif c == "c" or c == "C" then 12
		elif c == "d" or c == "D" then 13
		elif c == "e" or c == "E" then 14
		elif c == "f" or c == "F" then 15
		else 0
		end
	end

	while i < n do
		let nib = substr(hx, i, i + 1)
		h = (h * 33) + hexVal(nib)
		i = i + 1
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
## Oracle hook (prelude-owned, minimal)
## -----------------------------

# Global oracle execution hook.
#
# Backends install their executor here via `oracleInstall`.
# Until installed, oracle calls should return an annotated null via this hook.
# Once installed, this function is supposed to return a JSON string containing
# a value `x` boxed as `{output: x}`.
# Args: prompt: Str
# Returns: Str?
let __oracle_execute = fun(prompt: Str) -> Str? do
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
# Args: exec: (Str -> Str?)
# Returns: Null
let oracleInstall = fun(exec: Str -> Str?) -> Bool do
	__oracle_execute = exec
	_oracleInstalled = true
	true
end

# Show oracle installation status.
#
# Returns a short string indicating whether a backend is installed.
# Args: _: Null (ignored)
# Returns: Str
let oracleStatus = fun(_: Null) -> Str do
	if _oracleInstalled then "oracle: installed" else "oracle: not installed" end
end

# Quick oracle health check (real call, tiny).
# Success: { ok:true, ms }; failure/not installed: annotated null.
let oracleHealth = fun(_: Null) -> {}? do
	if not _oracleInstalled then return error("oracle not installed") end

	let ping = oracle(msg: Str) -> Str
	let t0 = nowMillis()
	let out = ping("ping")
	let dt = nowMillis() - t0
	if out == null then return out end
	if out == "pong" then return { ok: true, ms: dt } end

	let j = jsonParse(out)
	if j != null and mapHas(j, "output") and j.output == "pong" then
		return { ok: true, ms: dt }
	end

	error("pong mismatch")
end

# ---- Prompt tap (diagnostics) ----

# Most recent oracle prompt captured for diagnostics.
# Type: Str?
let _oracleLastPrompt = null

# Rolling log of oracle prompts captured for diagnostics.
# Type: [Str]
let _oraclePromptLog = []

# Install an oracle executor that records each prompt before forwarding.
# Args: exec: (Str -> Str?)  # underlying executor
# Return: Bool               # true on install
let oracleInstallWithTap = fun(exec: Str -> Str?) -> Bool do
	oracleInstall(fun(p: Str) -> Str? do
		_oracleLastPrompt = p
		_oraclePromptLog = _oraclePromptLog + [p]
		exec(p)
	end)
	true
end

# Return the most recently recorded oracle prompt (if any).
# Args: _: Null (ignored)
# Return: Str?
let oracleLastPrompt = fun(_: Null) -> Str? do
	_oracleLastPrompt
end

# Return the full recorded oracle prompt log.
# Args: _: Null (ignored)
# Return: [Str]
let oraclePromptLog = fun(_: Null) -> [Str] do
	_oraclePromptLog
end


## -----------------------------
## LLM backend bootstrap (installed in prelude)
## -----------------------------

# Load the llm module, install its executor, and select a default backend/model.
#
# Adjust these defaults to match your environment; they can be overridden later.
let llm = import("llm")
llm.init()
oracleInstall(llm.exec)
llm.useBackend("openai")
llm.useModel("chatgpt-4o-latest")
