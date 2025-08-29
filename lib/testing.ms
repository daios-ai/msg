## test.ms — tiny test registry & runner for MindScript
##
## Features:
##  - Register tests with `suite(name){ test(name){ ... } }`
##  - Hooks: beforeAll/afterAll, beforeEach/afterEach
##  - Run all or by substring match: runAll(), runMatch("math")
##  - `expect(...)` matchers + `assertEq(...)`
##  - `withOracle(exec, body)` to fake oracles within a test
##
## Conventions:
##  - Contract failures use fail(msg); runner catches with try(...)
##  - Returning annotated null (soft error) from a test also counts as failure
##  - Tabs for indentation

## -----------------------------
## State
## -----------------------------

let _tests = []				# [{name:Str, fn:(Null->Any?)}]
let _suiteStack = []		# ["parent","child",...]
let _beforeAll = []			# [(Null->Any?)]
let _afterAll = []			# [(Null->Any?)]
let _beforeEach = []		# [(Null->Any?)]
let _afterEach = []			# [(Null->Any?)]

## -----------------------------
## Registry API
## -----------------------------

# Register a test case.
#
# Adds a test named by the current suite path + `name`. Body throws on failure
# (via fail) or may return annotated null to signal a soft failure.
# Args: name: Str, fn: (Null -> Any?)
# Returns: Null
let test = fun(name: Str, fn: (Null -> Any?)) -> Null do
	let full = (fun(_: Null) -> Str do
		if len(_suiteStack) == 0 then name
		else
			let path = ""
			for p in iter(_suiteStack) do
				path = (if path == "" then p else path + "/" + p end)
			end
			path + "/" + name
		end
	end)(null)
	_tests = _tests + [{ name: full, fn: fn }]
	null
end

# Define a suite (group) of tests.
#
# Pushes `name` onto a suite stack while executing `body`, so inner tests
# receive a prefixed path. Errors thrown while defining a suite propagate.
# Args: name: Str, body: (Null -> Any?)
# Returns: Null
let suite = fun(name: Str, body: (Null -> Any?)) -> Null do
	_suiteStack = _suiteStack + [name]
	let r = try(body)
	# pop even if body failed
	_suiteStack = slice(_suiteStack, 0, len(_suiteStack) - 1)
	if not r.ok then fail("suite '" + name + "' init failed: " + str(r.error)) end
	null
end

# Register a function to run once before all tests.
#
# Args: fn: (Null -> Any?)
# Returns: Null
let beforeAll = fun(fn: (Null -> Any?)) -> Null do
	_beforeAll = _beforeAll + [fn]
	null
end

# Register a function to run once after all tests.
#
# Args: fn: (Null -> Any?)
# Returns: Null
let afterAll = fun(fn: (Null -> Any?)) -> Null do
	_afterAll = _afterAll + [fn]
	null
end

# Register a function to run before each test.
#
# Args: fn: (Null -> Any?)
# Returns: Null
let beforeEach = fun(fn: (Null -> Any?)) -> Null do
	_beforeEach = _beforeEach + [fn]
	null
end

# Register a function to run after each test.
#
# Args: fn: (Null -> Any?)
# Returns: Null
let afterEach = fun(fn: (Null -> Any?)) -> Null do
	_afterEach = _afterEach + [fn]
	null
end


## -----------------------------
## Runner
## -----------------------------

# Run all registered tests.
#
# Executes hooks and tests in registration order. Returns a structured summary.
# Args: _: Null (ignored)
# Returns: { ok: Bool, total: Int, passed: Int, failed: Int, ms: Int, results: [ {} ] }
let runAll = fun(_: Null) -> {} do
	_runSelected(_tests)
end

# Run tests whose names contain `pattern` (substring match).
#
# Args: pattern: Str
# Returns: { ok: Bool, total: Int, passed: Int, failed: Int, ms: Int, results: [ {} ] }
let runMatch = fun(pattern: Str) -> {} do
	let selected = []
	for t in iter(_tests) do
		if _contains(t.name, pattern) then selected = selected + [t] end
	end
	_runSelected(selected)
end


## -----------------------------
## Matchers / assertions
## -----------------------------

# Create an expectation object with basic matchers.
#
# All mismatch cases call fail(msg). Successful matches return null.
# Args: actual: Any
# Returns: { toBe:(Any->Null), toEqual:(Any->Null), toBeClose:(Num->Num->Null), toBeType:(Type->Null), toBeNull:(Null->Null), toContain:(Any->Null) }
let expect = fun(actual: Any) -> {} do
	{
		toBe: fun(expected: Any) -> Null do
			if actual == expected then null else fail("expected == " + str(expected) + ", got " + str(actual)) end
		end,

		toEqual: fun(expected: Any) -> Null do
			# stringified deep-ish equality for JSON-friendly values
			let a = jsonStringify(actual)
			let b = jsonStringify(expected)
			if a == b then null else fail("expected equal\nwant: " + b + "\n got: " + a) end
		end,

		toBeClose: fun(expected: Num, eps: Num) -> Null do
			let diff = (if actual > expected then actual - expected else expected - actual end)
			if diff <= eps then null else fail("expected close to " + str(expected) + " ± " + str(eps) + ", got " + str(actual)) end
		end,

		toBeType: fun(t: Type) -> Null do
			if isType(actual, t) then null else fail("expected type " + str(t) + ", got " + str(actual)) end
		end,

		toBeNull: fun(_: Null) -> Null do
			if actual == null then null else fail("expected null, got " + str(actual)) end
		end,

		toContain: fun(elem: Any) -> Null do
			if isType(actual, type [Any]) then
				let ok = false
				for x in iter(actual) do
					if jsonStringify(x) == jsonStringify(elem) then ok = true end
				end
				if ok then null else fail("expected array to contain " + str(elem)) end
			elif isType(actual, type Str) then
				if _contains(actual, elem) then null else fail("expected substring: " + str(elem)) end
			elif isType(actual, type {}) then
				if hasKey(actual, elem) then null else fail("expected object to have key " + str(elem)) end
			else
				fail("toContain not supported for value: " + str(actual))
			end
		end
	}
end

# Assert equality (shortcut).
#
# Throws a hard failure if a != b (via stringify comparison).
# Args: a: Any, b: Any, note: Str?
# Returns: Null
let assertEq = fun(a: Any, b: Any, note: Str?) -> Null do
	let sa = jsonStringify(a)
	let sb = jsonStringify(b)
	if sa == sb then null else
		let msg = (if note == null then "" else note + ": " end) + "want " + sb + ", got " + sa
		fail(msg)
	end
end


## -----------------------------
## Oracle test helper
## -----------------------------

# Run a block with a temporary oracle executor.
#
# Installs `exec` via oracleInstall, runs `body`, then restores the previous executor.
# Any failure inside `body` is rethrown after restoration.
# Args: exec: (Str -> Type -> Type -> [Any] -> Str?), body: (Null -> Any?)
# Returns: Any?
let withOracle = fun(exec: (Str -> Type -> Type -> [Any] -> Str?), body: (Null -> Any?)) -> Any? do
	let prev = __oracle_execute
	oracleInstall(exec)
	let r = try(body)
	oracleInstall(prev)
	if r.ok then r.value else fail(r.error) end
end


## -----------------------------
## Internals
## -----------------------------

# Run a list of tests with hooks; return summary map.
#
# Args: selected: [ {name:Str, fn:(Null->Any?)} ]
# Returns: { ok: Bool, total: Int, passed: Int, failed: Int, ms: Int, results: [ {} ] }
let _runSelected = fun(selected: [Any]) -> {} do
	let t0 = tsNow(null)

	# beforeAll
	let ba = _runHooks("beforeAll", _beforeAll)
	if not ba.ok then
		let ms = tsNow(null) - t0
		return {
			ok: false, total: len(selected), passed: 0, failed: len(selected),
			ms: ms,
			results: [{ name: "beforeAll", ok: false, ms: ba.ms, error: ba.error }]
		}
	end

	let results = []
	let passed = 0
	let failed = 0

	for t in iter(selected) do
		let tStart = tsNow(null)

		# beforeEach
		let be = _runHooks("beforeEach", _beforeEach)
		let testOk = be.ok
		let errMsg = (if be.ok then "" else be.error)

		# run test if be.ok
		if testOk then
			let r = try(t.fn)
			if r.ok then
				# treat annotated null as soft failure
				if r.value == null then
					let note = getNote(r.value)
					if note != null then
						testOk = false
						errMsg = "soft failure: " + str(note)
					end
				end
			else
				testOk = false
				errMsg = r.error
			end
		end

		# afterEach (run regardless; append error if any)
		let ae = _runHooks("afterEach", _afterEach)
		if testOk and not ae.ok then
			testOk = false
			errMsg = "afterEach: " + ae.error
		end

		let tMs = tsNow(null) - tStart
		if testOk then
			results = results + [{ name: t.name, ok: true, ms: tMs }]
			passed = passed + 1
		else
			results = results + [{ name: t.name, ok: false, ms: tMs, error: errMsg }]
			failed = failed + 1
		end
	end

	# afterAll
	let aa = _runHooks("afterAll", _afterAll)
	if not aa.ok then
		results = results + [{ name: "afterAll", ok: false, ms: aa.ms, error: aa.error }]
		failed = failed + 1
	end

	let total = len(selected)
	let ms = tsNow(null) - t0
	{ ok: failed == 0, total: total, passed: passed, failed: failed, ms: ms, results: results }
end

# Run a list of hook functions; accumulate first error only.
#
# Args: label: Str, fns: [(Null->Any?)]
# Returns: { ok: Bool, ms: Int, error: Str }
let _runHooks = fun(label: Str, fns: [Any]) -> {} do
	let h0 = tsNow(null)
	for fn in iter(fns) do
		let r = try(fn)
		if not r.ok then
			let ms = tsNow(null) - h0
			return { ok: false, ms: ms, error: label + ": " + str(r.error) }
		else
			# soft error from a hook counts as failure
			if r.value == null then
				let note = getNote(r.value)
				if note != null then
					let ms2 = tsNow(null) - h0
					return { ok: false, ms: ms2, error: label + " (soft): " + str(note) }
				end
			end
		end
	end
	let ms = tsNow(null) - h0
	{ ok: true, ms: ms, error: "" }
end

# Substring check for strings (needle may be Any; coerced to Str).
#
# Args: hay: Str, needle: Any
# Returns: Bool
let _contains = fun(hay: Str, needle: Any) -> Bool do
	let n = str(needle)
	if n == "" then true else len(split(hay, n)) >= 2 end
end
