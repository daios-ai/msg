# MindScript testing toolkit (minimal, Go-like ergonomics).
#
# Exports:
# 	test(name, fn)        — register a test function
# 	tests()               — return the registered tests map (name → fn)
# 	assert(ok, msg?)      — boolean assertion
# 	assertEq(want, got)   — deep-ish equality with readable diff
# 	assertType(v, T)      — runtime type assertion
# 	assertThrows(fn, sub?)— expect a failure; optional substring match
# 	cases(name, xs, fn)   — table tests: register one subtest per element
# 	snapshot(path, v)     — golden/snapshot compare (update with MS_UPDATE_SNAPSHOTS=1)
# 	seed(n)               — seed the RNG for reproducible tests

# Name of the opt-in environment flag for updating snapshots.
let MS_UPDATE_SNAPSHOTS = "MS_UPDATE_SNAPSHOTS"

# Internal registry (insertion order preserved by map semantics).
let _tests = {}

# --- helpers ---------------------------------------------------------------

# Render a value to a stable string (JSON when possible).
let _repr = fun(x: Any) -> Str do
	# jsonStringify is stable and compact; fall back to str(x) only if ever needed.
	return jsonStringify(x)
end

# Simple substring test using split (no regex surprises).
let _contains = fun(hay: Str, needle: Str) -> Bool do
	return len(split(hay, needle)) > 1
end

# Trim long blobs for diffs.
let _trim = fun(s: Str, limit: Int) -> Str do
	if len(s) <= limit then
		return s
	end
	return sprintf("%s…(%d bytes total)", [substr(s, 0, limit), len(s)])
end

# Fail with a formatted message (internal).
let _failf = fun(fmt: Str, args: [Any]) -> Null do
	fail(sprintf(fmt, args))
end

# --- API -------------------------------------------------------------------

# Register a test function under a unique name.
#
# Registers a test that will be discovered by a runner. Names must be unique.
#
# Args:
# 	name: Str — test name
# 	fn:   Any — zero-arg function to execute for this test
#
# Returns:
# 	Bool — true on success
let test = fun(name: Str, fn: Any) -> Bool do
	if not __is_fun(fn) then
		_failf("test(%s): second argument must be a function", [_repr(name)])
	end
	if mapHas(_tests, name) then
		_failf("duplicate test name: %s", [_repr(name)])
	end
	_tests.(name) = fn
	return true
end

# Return the registered tests (name → function).
#
# Returns:
# 	{} — shallow map view of the registry (modifying it does not affect internal state)
let tests = fun(_: Null) -> {} do
	return clone(_tests)
end

# Boolean assertion.
#
# Fails with msg (or a default) when ok is false.
#
# Args:
# 	ok:  Bool
# 	msg: Str? — optional message
#
# Returns:
# 	Bool — true when assertion holds
let assert = fun(ok: Bool, msg: Str?) -> Bool do
	if not ok then
		if msg == null then
			fail("assertion failed")
		end
		fail(msg)
	end
	return true
end

# Equality assertion with a readable diff.
#
# Uses structural equality (==). On mismatch, shows JSONified want/got.
#
# Args:
# 	want: Any
# 	got:  Any
#
# Returns:
# 	Bool — true when equal
let assertEq = fun(want: Any, got: Any) -> Bool do
	if want == got then
		return true
	end
	let w = _repr(want)
	let g = _repr(got)
	_failf("not equal:\nwant: %s\ngot:  %s", [_trim(w, 4000), _trim(g, 4000)])
	# unreachable
	return true
end

# Runtime type assertion.
#
# Asserts that value v conforms to the declared Type T.
#
# Args:
# 	v: Any
# 	T: Type
#
# Returns:
# 	Bool — true when v : T
let assertType = fun(v: Any, T: Type) -> Bool do
	if isType(v, T) then
		return true
	end
	_failf("type mismatch:\nvalue: %s\ndoes not satisfy: %s",
		[_trim(_repr(v), 4000), str(T)])
	# unreachable
	return true
end

# Expect a failure (hard error), optionally matching a substring.
#
# Runs fn and asserts that it fails. If sub is provided, the error message
# must contain sub.
#
# Args:
# 	fn:  Any   — zero-arg function expected to fail
# 	sub: Str?  — optional substring that must appear in the error
#
# Returns:
# 	Bool — true when a failure occurred (and matched, if sub provided)
let assertThrows = fun(fn: Any, sub: Str?) -> Bool do
	if not __is_fun(fn) then
		_failf("assertThrows: argument must be a function", [])
	end
	let r = try(fn)
	if r.ok then
		_failf("expected failure, got success with value: %s", [_trim(_repr(r.value), 4000)])
	end
	if sub != null and not _contains(r.error, sub) then
		_failf("failure did not contain substring.\nwant in: %s\ngot:     %s",
			[_repr(sub), _trim(r.error, 4000)])
	end
	return true
end

# Table-driven tests (register subtests).
#
# Registers one subtest per element in xs, named "name/i" (0-based).
# The function fn is called as fn(x) for each element.
#
# Args:
# 	name: Str
# 	xs:   [Any]
# 	fn:   Any   — function accepting one argument (each element)
#
# Returns:
# 	Int — number of subtests registered
let cases = fun(name: Str, xs: [Any], fn: Any) -> Int do
	if not __is_fun(fn) then
		_failf("cases(%s): fn must be a function taking one argument", [_repr(name)])
	end
	let n = len(xs)
	let i = 0
	for _ in xs do
		let subName = sprintf("%s/%d", [name, i])
		let x = xs[i]
		test(subName, fun(_: Null) -> Any do
			# Delegate to user-supplied function; they perform assertions inside.
			return fn(x)
		end)
		i = i + 1
	end
	return n
end

# Snapshot (golden) testing for values.
#
# Compares the stable serialization of value to the file at path.
# If MS_UPDATE_SNAPSHOTS=1, the file is (over)written with the new value.
#
# Args:
# 	path:  Str — snapshot file path
# 	value: Any — value to serialize and compare
#
# Returns:
# 	Bool — true when snapshot matches or was updated
let snapshot = fun(path: Str, value: Any) -> Bool do
	let want = _repr(value)
	let have = readFile(path)
	let update = osEnv(MS_UPDATE_SNAPSHOTS) == "1"

	if have == null then
		if update then
			writeFile(path, want)
			return true
		end
		_failf("snapshot missing (set %s=1 to write): %s", [MS_UPDATE_SNAPSHOTS, path])
	end

	if have != want then
		if update then
			writeFile(path, want)
			return true
		end
		_failf("snapshot mismatch:\nfile: %s\n--- have ---\n%s\n--- want ---\n%s\n(set %s=1 to update)",
			[path, _trim(have, 4000), _trim(want, 4000), MS_UPDATE_SNAPSHOTS])
	end

	return true
end

# Seed the RNG for reproducible tests.
#
# Args:
# 	n: Int — seed value
#
# Returns:
# 	Int — the same seed (convenient for logging)
let seed = fun(n: Int) -> Int do
	seedRand(n)
	return n
end
