# testing.ms — minimal testing toolkit + simple test runner
#
# Usage in *_test.ms:
#	let T = import("testing")
#	T.test("adds", fun(_: Null) -> Any do
#		T.assertEq(2 + 2, 4)
#	end)
#
# Notes:
#	- MindScript has no true zero-arg functions; f() is sugar for f(null).
#	- Tests are functions of type (Null) -> Any and are invoked with null.
#
# Running:
#	let T = import("testing")
#	T.run({ root: ".", quiet: false, parallel: false, timeoutMs: 0, verbose: false })

# =========================
# Internal state
# =========================

# Test registry (name -> (Null)->Any)
let _tests = {}

# =========================
# Helpers
# =========================

# Stable value rendering (JSON when possible).
let _repr = fun(x: Any) -> Str do
	let j = jsonStringify(x)
	if j != null then
		return j
	end
	return str(x)
end

# Simple substring check via split.
let _contains = fun(hay: Str, needle: Str) -> Bool do
	if len(needle) == 0 then
		return true
	end
	return len(split(hay, needle)) > 1
end

# Trim long blobs for error messages.
let _trim = fun(s: Str, limit: Int) -> Str do
	if len(s) <= limit then
		return s
	end
	return sprintf("%s…(%d bytes total)", [substr(s, 0, limit), len(s)])
end

# Fail with formatted message.
let _failf = fun(fmt: Str, args: [Any]) -> Null do
	let m = sprintf(fmt, args)
	if m == null then
		fail("internal formatting error")
	end
	fail(m)
end

# Function type predicates.
let _isNullArgFun = fun(f: Any) -> Bool do
	return isType(f, type (Null) -> Any)
end

let _isOneArgFun = fun(f: Any) -> Bool do
	return isType(f, type (Any) -> Any)
end

# =========================
# Public API — assertions & registry
# =========================

# Register a test.
let test = fun(name: Str, fn: Any) -> Bool do
	if not _isNullArgFun(fn) then
		_failf("test(%s): second argument must be a function of type (Null)->Any", [_repr(name)])
	end
	if mapHas(_tests, name) then
		_failf("duplicate test name: %s", [_repr(name)])
	end
	_tests[name] = fn
	return true
end

# Return a shallow copy of the registry.
let tests = fun(_: Null) -> { Str: Any } do
	return clone(_tests)
end

# Reset the registry.
let reset = fun(_: Null) -> Bool do
	let keys = []
	for [k, _] in _tests do
		keys = keys + [k]
	end
	for k in keys do
		mapDelete(_tests, k)
	end
	return true
end

# Boolean assertion.
let assert = fun(ok: Bool, msg: Str?) -> Bool do
	if not ok then
		if msg == null then
			fail("assertion failed")
		end
		fail(msg)
	end
	return true
end

# Equality assertion with readable diff.
let assertEq = fun(want: Any, got: Any) -> Bool do
	if want == got then
		return true
	end
	let w = _repr(want)
	let g = _repr(got)
	_failf("not equal:\nwant: %s\ngot:  %s", [_trim(w, 4000), _trim(g, 4000)])
	return true
end

# Runtime type assertion.
let assertType = fun(v: Any, T: Type) -> Bool do
	if isType(v, T) then
		return true
	end
	_failf("type mismatch:\nvalue: %s\ndoes not satisfy: %s",
		[_trim(_repr(v), 4000), str(T)])
	return true
end

# Expect a hard failure, optionally matching a substring.
let assertThrows = fun(fn: Any, sub: Str?) -> Bool do
	if not _isNullArgFun(fn) then
		_failf("assertThrows: argument must be a function of type (Null)->Any", [])
	end
	let r = try(fun(_: Null) -> Any do fn(null) end)
	if r.ok then
		_failf("expected failure, got success with value: %s", [_trim(_repr(r.value), 4000)])
	end
	if sub != null and not _contains(r.error, sub) then
		_failf("failure did not contain substring.\nwant in: %s\ngot:     %s",
			[_repr(sub), _trim(r.error, 4000)])
	end
	return true
end

# Table-driven subtests: registers name/0, name/1, ...
let cases = fun(name: Str, xs: [Any], fn: Any) -> Int do
	if not _isOneArgFun(fn) then
		_failf("cases(%s): fn must be a function taking one argument", [_repr(name)])
	end
	let n = len(xs)
	let i = 0
	for _ in xs do
		let subName = sprintf("%s/%d", [name, i])
		let x = xs[i]
		test(subName, fun(_: Null) -> Any do
			return fn(x)
		end)
		i = i + 1
	end
	return n
end

# Snapshot (golden) testing for values.
let snapshot = fun(path: Str, value: Any, update: Bool?) -> Bool do
	let want = _repr(value)
	let have = readFile(path)
	let doUpdate = (update != null) and bool(update)

	if have == null then
		if doUpdate then
			writeFile(path, want)
			return true
		end
		_failf("snapshot missing (pass update=true to write): %s", [path])
	end

	if have != want then
		if doUpdate then
			writeFile(path, want)
			return true
		end
		_failf("snapshot mismatch:\nfile: %s\n--- have ---\n%s\n--- want ---\n%s\n(pass update=true to overwrite)",
			[path, _trim(have, 4000), _trim(want, 4000)])
	end

	return true
end

let snapshotUpdate = fun(path: Str, value: Any) -> Bool do
	return snapshot(path, value, true)
end

# Seed RNG for reproducible tests.
let seed = fun(n: Int) -> Int do
	seedRand(n)
	return n
end

# =========================
# Runner types
# =========================

# Runner options (all keys optional).
let RunnerOpts = type {
	root: Str,        # directory to search (default ".")
	parallel: Bool,   # run tests concurrently
	quiet: Bool,      # suppress discovery logs
	verbose: Bool,    # Go-style per-test RUN/PASS/FAIL lines (default false)
	timeoutMs: Int    # per-test timeout in ms (<=0 or null disables)
}

# Summary result.
let Summary = type {
	passed: Int,
	failed: Int,
	total: Int,
	durationMs: Int
}

# =========================
# Runner helpers
# =========================

# Print a line unless quiet.
let _log = fun(quiet: Bool, msg: Str) -> Str do
	if not quiet then
		printf("%s\n", [msg])
	end
	return msg
end

# s ends with suffix.
let _endsWith = fun(s: Str, suffix: Str) -> Bool do
	let n = len(s)
	let m = len(suffix)
	if m == 0 then
		return true
	end
	if n < m then
		return false
	end
	return substr(s, n - m, n) == suffix
end

# Recognize *_test.ms by basename.
let _isTestFile = fun(path: Str) -> Bool do
	let base = pathBase(path)
	return _endsWith(base, "_test.ms")
end

# Cross-platform abs path.
let _isAbs = fun(p: Str) -> Bool do
	if len(p) > 0 and substr(p, 0, 1) == "/" then
		return true
	end
	if len(p) >= 2 and substr(p, 0, 2) == "\\\\" then
		return true
	end
	return len(match("^[A-Za-z]:[\\\\/]", p)) > 0
end

# Recursive walk (robust via stat()).
let _walk = fun(dir: Str) -> [Str] do
	let entries = dirList(dir)
	if entries == null then
		return []
	end
	let out = []
	for entry in entries do
		let p = pathJoin([dir, entry])
		let st = stat(p)
		if st == null then
			continue
		elif st.isDir then
			let rec = _walk(p)
			for q in rec do
				out = out + [q]
			end
		else
			out = out + [p]
		end
	end
	return out
end

# Discover test files (sorted).
let _discover = fun(root: Str) -> [Str] do
	let base = if cwd() == null then "." else cwd() end
	let rootPath =
		if _isAbs(root) then
			pathClean(root)
		else
			pathClean(pathJoin([base, root]))
		end

	let st = stat(rootPath)
	if st == null or not st.isDir then
		return []
	end

	let all = _walk(rootPath)
	let ts = []
	for f in all do
		if _isTestFile(f) then
			ts = ts + [f]
		end
	end

	# insertion sort
	let n = len(ts)
	let i = 0
	while i < n do
		let j = i
		while j > 0 and ts[j - 1] > ts[j] do
			let tmp = ts[j - 1]
			ts[j - 1] = ts[j]
			ts[j] = tmp
			j = j - 1
		end
		i = i + 1
	end
	return ts
end

# Import discovered tests so they register.
let _loadTests = fun(files: [Str], quiet: Bool) -> Int do
	let loaded = 0
	for f in files do
		_log(quiet, sprintf("discover: %s", [f]))
		let r = try(fun(_: Null) -> Any do import(f) end)
		if r.ok then
			loaded = loaded + 1
		else
			_log(quiet, sprintf("FAIL import %s\n%s", [f, r.error]))
		end
	end
	return loaded
end

# --- run one test; optional timeout ---
# Returns: { name, ok, durationMs, error?, timeout? }
let _runOne = fun(name: Str, fn: Any, timeoutMs: Int?, verbose: Bool, quiet: Bool) -> {} do
	if verbose then
		printf("=== RUN   %s\n", [name])
	end
	let t0 = nowMillis()

	# no timeout
	if timeoutMs == null or int(timeoutMs) <= 0 then
		let r = try(fun(_: Null) -> Any do fn(null) end)
		let dur = nowMillis() - t0
		let res =
			if r.ok then { name: name, ok: true, durationMs: dur }
			else { name: name, ok: false, error: r.error, durationMs: dur }
			end
		if verbose then
			let tag = if res.ok then "PASS" else "FAIL" end
			printf("--- %s: %s (%dms)\n", [tag, name, int(dur)])
			if not res.ok and mapHas(res, "error") and res.error != null then
				printf("%s\n", [res.error])
			end
		elif not quiet then
			if res.ok then
				printf("ok   %s\n", [name])
			else
				let err = if mapHas(res, "error") and res.error != null then res.error else "" end
				printf("FAIL %s\n%s\n", [name, err])
			end
		else
			if not res.ok then
				let err = if mapHas(res, "error") and res.error != null then res.error else "" end
				printf("FAIL %s\n%s\n", [name, err])
			end
		end
		return res
	end

	# race test vs timer (simple)
	let done = chanOpen(1)

	let testP = procSpawn(fun(_: Null) -> Null do
		let r = try(fun(_: Null) -> Any do fn(null) end)
		let res =
			if r.ok then { name: name, ok: true }
			else { name: name, ok: false, error: r.error }
			end
		chanSend(done, res)
	end)

	let timerP = procSpawn(fun(_: Null) -> Null do
		sleep(int(timeoutMs))
		chanSend(done, { name: name, ok: false, timeout: true })
	end)

	let base = chanRecv(done)

	# cancel the loser (best-effort, cooperative)
	if mapHas(base, "timeout") and bool(base.timeout) then
		procCancel(testP)
	else
		procCancel(timerP)
	end

	let dur = nowMillis() - t0
	let res =
		if mapHas(base, "timeout") and bool(base.timeout) then
			{ name: name, ok: false, error: sprintf("timeout after %dms", [int(timeoutMs)]), timeout: true, durationMs: dur }
		elif base.ok then
			{ name: name, ok: true, durationMs: dur }
		else
			{ name: name, ok: false, error: if mapHas(base, "error") and base.error != null then base.error else "" end, durationMs: dur }
		end

	if verbose then
		let tag =
			if mapHas(res, "timeout") and bool(res.timeout) then "TIMEOUT"
			elif res.ok then "PASS"
			else "FAIL"
			end
		printf("--- %s: %s (%dms)\n", [tag, name, int(dur)])
		if not res.ok and mapHas(res, "error") and res.error != null then
			printf("%s\n", [res.error])
		end
	elif not quiet then
		if res.ok then
			printf("ok   %s\n", [name])
		else
			let isTimeout = mapHas(res, "timeout") and bool(res.timeout)
			let tag = if isTimeout then "TIMEOUT" else "FAIL" end
			let err = if mapHas(res, "error") and res.error != null then res.error else "" end
			printf("%s %s\n%s\n", [tag, name, err])
		end
	else
		if not res.ok then
			let isTimeout = mapHas(res, "timeout") and bool(res.timeout)
			let tag = if isTimeout then "TIMEOUT" else "FAIL" end
			let err = if mapHas(res, "error") and res.error != null then res.error else "" end
			printf("%s %s\n%s\n", [tag, name, err])
		end
	end

	return res
end

# Execute all registered tests and summarize.
let _runAll = fun(quiet: Bool, parallel: Bool, timeoutMs: Int?, verbose: Bool) -> Summary do
	let start = nowMillis()
	let passed = 0
	let failed = 0
	let total = 0

	let reg = tests(null)

	# collect & sort names
	let names = []
	for [k, _] in reg do
		names = names + [k]
	end
	let n = len(names)
	let i = 0
	while i < n do
		let j = i
		while j > 0 and names[j - 1] > names[j] do
			let tmp = names[j - 1]
			names[j - 1] = names[j]
			names[j] = tmp
			j = j - 1
		end
		i = i + 1
	end

	if parallel then
		let ps = []
		for name in names do
			let fn = reg[name]
			ps = ps + [procSpawn(fun(_: Null) -> {} do
				return _runOne(name, fn, timeoutMs, verbose, quiet)
			end)]
			total = total + 1
		end
		let results = procJoinAll(ps)
		let i = 0
		while i < len(results) do
			let res = results[i]
			if res.ok then
				passed = passed + 1
			else
				failed = failed + 1
			end
			i = i + 1
		end
	else
		for name in names do
			let fn = reg[name]
			total = total + 1
			let res = _runOne(name, fn, timeoutMs, verbose, quiet)
			if res.ok then
				passed = passed + 1
			else
				failed = failed + 1
			end
		end
	end

	let dur = nowMillis() - start
	return { passed: passed, failed: failed, total: total, durationMs: dur }
end

# =========================
# Public API — runner
# =========================

# Run all tests discovered under a directory.
let run = fun(opts: RunnerOpts) -> Summary do
	let root =
		if mapHas(opts, "root") and opts.root != null then opts.root else "." end
	let quiet =
		if mapHas(opts, "quiet") and opts.quiet != null then bool(opts.quiet) else false end
	let verbose =
		if mapHas(opts, "verbose") and opts.verbose != null then bool(opts.verbose) else false end
	let parallel =
		if mapHas(opts, "parallel") and opts.parallel != null then bool(opts.parallel) else false end
	let timeoutMs =
		if mapHas(opts, "timeoutMs") and opts.timeoutMs != null then int(opts.timeoutMs) else null end

	let files = _discover(root)
	_log(quiet, sprintf("found %d test file(s)", [len(files)]))

	let _ = _loadTests(files, quiet)

	let summary = _runAll(quiet, parallel, timeoutMs, verbose)

	if not quiet then
		printf("ok: %d  fail: %d  total: %d  durationMs: %d\n",
			[summary.passed, summary.failed, summary.total, summary.durationMs])
	end

	return summary
end
