# testing.ms — minimal testing toolkit + simple test runner
#
# Usage in *_test.ms:
# let testing = import("testing")
# testing.test("adds", fun(_: Null) -> Any do
# 	testing.assert(true, null)
# end)
#
# Running:
# let testing = import("testing")
# testing.run({ pathPrefix: ".", verbose: false, parallel: false, timeoutMs: 0 })

# =========================
# Internal state
# =========================

let _tests = {} # name -> (Null)->Any
let _origins = {} # concrete tests: test("name", ...)
let _caseOrigins = {} # case groups: cases("prefix", ...)

# Return the recorded origin for a test name, with cases("x") fallback for "x/NN".
# 
# Find the source file and line number where a test or its case-group was declared.
# 
# Args:
#	name: Str — test name, possibly "prefix/NN" for subtests
# 
# Return:
#	{}? — map {file!: Str, line!: Int} or null if unknown
let _originOf = fun(name: Str) -> {}? do
	if mapHas(_origins, name) then
		return _origins[name]
	end
	let parts = split(name, "/")
	let n = len(parts)
	if n >= 2 then
		let last = parts[n - 1]
		if len(match("^[0-9]+$", last)) > 0 then
			let parent = join(slice(parts, 0, n - 1), "/")
			if mapHas(_caseOrigins, parent) then
				return _caseOrigins[parent]
			end
		end
	end
	return null
end

# =========================
# Small helpers
# =========================

# Stable value rendering: JSON if possible, else str, else formatValue.
#
# Produce a stable textual representation of any value for diffs and messages.
#
# Args:
#	x: Any — value to render
#
# Return:
#	Str — string form (never null)
let _repr = fun(x: Any) -> Str do
	let j = jsonStringify(x)
	if j != null then
		return j
	end
	let s = str(x)
	if s != null then
		return s
	end
	return formatValue(x)
end

# Minimal substring check.
#
# Test whether 'needle' occurs in 'hay'.
#
# Args:
#	hay: Str
#	needle: Str
#
# Return:
#	Bool — true if found (empty needle always true)
let _contains = fun(hay: Str, needle: Str) -> Bool do
	if len(needle) == 0 then
		return true
	end
	return len(split(hay, needle)) > 1
end

# In-place insertion sort for strings (determinism).
#
# Sort an array of strings lexicographically, mutating the array.
#
# Args:
#	xs: [Str]
#
# Return:
#	[Str] — the same array, sorted
let _sortStrs = fun(xs: [Str]) -> [Str] do
	let n = len(xs)
	let i = 0
	while i < n do
		let j = i
		while j > 0 and xs[j - 1] > xs[j] do
			let tmp = xs[j - 1]
			xs[j - 1] = xs[j]
			xs[j] = tmp
			j = j - 1
		end
		i = i + 1
	end
	return xs
end

# =========================
# Public API — assertions & registry
# =========================

# Register a test.
#
# Register a unique test function under a string name. The function must have type (Null)->Any.
#
# Args:
#	name: Str — unique test name
#	fn: (Null)->Any — test body
#
# Return:
#	Bool — true on success; panics on duplicate name or bad type
let test = fun(name: Str, fn: Null -> Any) -> Bool do
	if not isType(fn, type Null -> Any) then
		panic(sprintf("test(%s): second argument must be a function of type (Null)->Any", [
			_repr(name)
		]))
	end
	if mapHas(_tests, name) then
		panic(sprintf("duplicate test name: %s", [_repr(name)]))
	end
	_tests[name] = fn
	return true
end

# View registered tests.
#
# Return a shallow clone of the current test registry.
#
# Args:
#	_: Null
#
# Return:
#	{Str: Any} — map name -> function
let tests = fun(_: Null) -> {Str: Any} do
	return clone(_tests)
end

# Reset registry.
#
# Remove all registered tests.
#
# Args:
#	_: Null
#
# Return:
#	Bool — true when done
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
#
# Assert that a condition is true; panic with message if false.
#
# Args:
#	ok: Bool — condition
#	msg: Str? — optional message
#
# Return:
#	Bool — true if ok, otherwise panics
let assert = fun(ok: Bool, msg: Str?) -> Bool do
	if not ok then
		if msg == null then
			panic("assertion failed")
		end
		panic(msg)
	end
	return true
end

# Equality assertion.
#
# Assert deep equality; panic with pretty diff on mismatch.
#
# Args:
#	want: Any
#	got: Any
#
# Return:
#	Bool — true if equal, otherwise panics
let assertEq = fun(want: Any, got: Any) -> Bool do
	if want == got then
		return true
	end
	panic(sprintf("not equal:\nwant: %s\ngot:  %s", [_repr(want), _repr(got)]))
end

# Runtime type assertion.
#
# Assert that value satisfies a Type; panic with readable message on mismatch.
#
# Args:
#	v: Any — value
#	T: Type — required type
#
# Return:
#	Bool — true if satisfied, otherwise panics
let assertType = fun(v: Any, T: Type) -> Bool do
	if isType(v, T) then
		return true
	end
	panic(sprintf("type mismatch:\nvalue: %s\ndoes not satisfy: %s", [
		_repr(v),
		str(T)
	]))
end

# Panic expectation.
#
# Run a zero-arg function and expect it to panic; optionally require a substring in the first line.
#
# Args:
#	fn: (Null)->Any — function expected to panic
#	sub: Str? — required substring in first line (empty means any message)
#
# Return:
#	Bool — true if a panic occurred and matched; otherwise panics
let assertThrows = fun(fn: Null -> Any, sub: Str?) -> Bool do
	if not isType(fn, type Null -> Any) then
		panic("assertThrows: argument must be a function of type (Null)->Any")
	end
	let r = try(fun(_: Null) do fn(null) end)
	if r.ok then
		panic(sprintf("expected failure, got success with value: %s", [_repr(r.value)]))
	end
	if sub != null then
		let e = noteGet(r.value)
		let parts = split(e, "\n")
		let first = if len(parts) == 0 then e else parts[0] end
		if not _contains(first, sub) then
			panic(sprintf("failure did not contain substring.\nwant in: %s\ngot:\n%s", [
				_repr(sub),
				e
			]))
		end
	end
	return true
end

# Table-driven subtests.
#
# Register subtests as name/0, name/1, ... by applying fn to each element.
#
# Args:
#	name: Str — prefix
#	xs: [Any] — cases
#	fn: Any -> Any — single-argument function
#
# Return:
#	Int — number of registered cases; panics if fn has wrong type
let cases = fun(name: Str, xs: [Any], fn: Any -> Any) -> Int do
	if not isType(fn, type Any -> Any) then
		panic(sprintf("cases(%s): fn must be a function taking one argument", [_repr(name)]))
	end
	let n = len(xs)
	let i = 0
	for _ in xs do
		let sub = sprintf("%s/%d", [name, i])
		let x = xs[i]
		test(sub, fun(_: Null) -> Any do fn(x) end)
		i = i + 1
	end
	return n
end

# Snapshot (golden) testing for values.
#
# Compare 'value' against file contents; when update=true, overwrite the file.
#
# Args:
#	path: Str — file path
#	value: Any — expected value
#	update: Bool? — overwrite when true
#
# Return:
#	Bool — true on success; panics on mismatch or IO failure
let snapshot = fun(path: Str, value: Any, update: Bool?) -> Bool do
	let want = _repr(value)
	let have = readFile(path)
	let doUpdate = update != null and bool(update)
	if have == null then
		if doUpdate then
			let n = writeFile(path, want)
			if n == null then
				panic(sprintf("snapshot write failed: %s", [path]))
			end
			return true
		end
		panic(sprintf("snapshot missing (pass update=true to write): %s", [path]))
	end
	if have != want then
		if doUpdate then
			let n2 = writeFile(path, want)
			if n2 == null then
				panic(sprintf("snapshot write failed: %s", [path]))
			end
			return true
		end
		panic(sprintf("snapshot mismatch:\nfile: %s\n--- have ---\n%s\n--- want ---\n%s\n(pass update=true to overwrite)", [
			path,
			have,
			want
		]))
	end
	return true
end

# Snapshot overwrite helper.
#
# Write the golden snapshot unconditionally.
#
# Args:
#	path: Str
#	value: Any
#
# Return:
#	Bool — true after write
let snapshotUpdate = fun(path: Str, value: Any) -> Bool do
	return snapshot(path, value, true)
end

# Seed RNG.
#
# Seed the pseudo-random generator and echo the seed for chaining.
#
# Args:
#	n: Int — seed
#
# Return:
#	Int — the same seed
let seed = fun(n: Int) -> Int do
	seedRand(n)
	return n
end

# =========================
# Runner types
# =========================

let RunnerOpts = type {
	pathPrefix: Str, # run tests whose paths start with this prefix (dir or file)
	parallel: Bool, # run tests concurrently
	verbose: Bool, # Go-style per-test RUN/PASS/FAIL lines
	timeoutMs: Int # per-test timeout in ms (<=0 or null disables)
}

let Summary = type {passed: Int, failed: Int, total: Int, durationMs: Int}

# =========================
# Discovery & loading
# =========================

# Index tests in source text.
#
# Scan source lines to record locations of test("...") and cases("...").
#
# Args:
#	file: Str — filename
#	src: Str — file contents
#
# Return:
#	Int — number of registrations indexed
let _indexTests = fun(file: Str, src: Str) -> Int do
	let lines = split(src, "\n")
	let i = 0
	let found = 0
	for _ in lines do
		let line = lines[i]
		let p1 = split(line, "test(\"")
		if len(p1) > 1 then
			let tail = p1[1]
			let name = split(tail, "\"")[0]
			if len(name) > 0 then
				_origins[name] = {file: file, line: i + 1}
				found = found + 1
			end
		end
		let p2 = split(line, "cases(\"")
		if len(p2) > 1 then
			let tail2 = p2[1]
			let prefix = split(tail2, "\"")[0]
			if len(prefix) > 0 then
				_caseOrigins[prefix] = {file: file, line: i + 1}
				found = found + 1
			end
		end
		i = i + 1
	end
	return found
end

# EndsWith helper.
#
# Args:
#	s: Str
#	suffix: Str
#
# Return:
#	Bool — true if s ends with suffix
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

# StartsWith helper.
#
# Args:
#	s: Str
#	prefix: Str
#
# Return:
#	Bool — true if s starts with prefix
let _startsWith = fun(s: Str, prefix: Str) -> Bool do
	let n = len(prefix)
	if n == 0 then
		return true
	end
	if len(s) < n then
		return false
	end
	return substr(s, 0, n) == prefix
end

# Test file predicate.
#
# Args:
#	path: Str
#
# Return:
#	Bool — true if basename ends with "_test.ms"
let _isTestFile = fun(path: Str) -> Bool do
	return _endsWith(pathBase(path), "_test.ms")
end

# Absolute path check.
#
# Args:
#	p: Str
#
# Return:
#	Bool — true if absolute (POSIX/Windows)
let _isAbs = fun(p: Str) -> Bool do
	if len(p) > 0 and substr(p, 0, 1) == "/" then
		return true
	end
	if len(p) >= 2 and substr(p, 0, 2) == "\\\\" then
		return true
	end
	return len(match("^[A-Za-z]:[\\\\/]", p)) > 0
end

# Clean absolute path.
#
# Args:
#	p: Str
#
# Return:
#	Str — absolute, cleaned path
let _cleanAbs = fun(p: Str) -> Str do
	let base = if cwd() == null then "." else cwd() end
	let abs = if _isAbs(p) then p else pathJoin([base, p]) end
	return pathClean(abs)
end

# Recursive directory walk.
#
# Collect all paths within a directory tree.
#
# Args:
#	dir: Str
#
# Return:
#	[Str] — list of paths
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

# Discover test files by prefix.
#
# Return sorted absolute paths for files whose absolute path starts with prefix and end with "_test.ms".
#
# Args:
#	prefix: Str — directory or file path (absolute or relative)
#
# Return:
#	[Str] — sorted test file paths
let _discoverByPrefix = fun(prefix: Str) -> [Str] do
	let pref = _cleanAbs(prefix)
	let st = stat(pref)
	let root = if st != null and st.isDir then pref else pathDir(pref) end
	let all = _walk(root)
	let out = []
	for f in all do
		let abs = pathClean(f)
		if _startsWith(abs, pref) and _isTestFile(abs) then
			out = out + [abs]
		end
	end
	return _sortStrs(out)
end

# Load test files fresh each run.
#
# Import test modules with unique names to avoid cache; index test origins; log import failures.
#
# Args:
#	files: [Str] — list of absolute paths
#	verbose: Bool — emit discovery logs
#
# Return:
#	Int — number of successfully loaded files
let _loadTests = fun(files: [Str], verbose: Bool) -> Int do
	let loaded = 0
	for f in files do
		if verbose then
			printf("discover: %s\n", [f])
		end
		let src = readFile(f)
		if src == null then
			printf("FAIL read %s\n", [f])
			continue
		end
		_indexTests(f, src)
		let name = sprintf("testing:%s:%d", [f, nowNanos()])
		let r = try(fun(_: Null) do
			importCode(name, src)
		end)
		if r.ok then
			loaded = loaded + 1
		else
			let errorMessage = noteGet(r.value)
			printf("FAIL import %s\n%s\n", [f, errorMessage])
		end
	end
	return loaded
end

# =========================
# Execution
# =========================

# Run a single test.
#
# Execute one test function with optional timeout; print per-test lines based on verbosity.
#
# Args:
#	name: Str — test name
#	fn: Any — function (Null)->Any
#	timeoutMs: Int? — per-test timeout in ms (<=0 or null disables)
#	verbose: Bool — print RUN/PASS/FAIL lines
#	emit: Bool — whether this function prints status lines
#
# Return:
#	{} — { name: Str, ok: Bool, durationMs: Int, error: Str?, timeout: Bool }
let _runOne = fun(name: Str, fn: Any, timeoutMs: Int?, verbose: Bool, emit: Bool) -> {} do
	if emit and verbose then
		printf("=== RUN   %s\n", [name])
	end
	let t0 = nowMillis()
	let _exec = fun(_: Null) -> {} do
		let r = try(fun(_: Null) do fn(null) end)
		if r.ok then
			return {ok: true}
		end
		return {ok: false, error: noteGet(r.value)}
	end
	let base = if timeoutMs == null or int(timeoutMs) <= 0 then
		_exec(null)
	else
		let tp = procSpawn(fun(_: Null) -> {} do _exec(null) end)
		let tt = procSpawn(fun(_: Null) -> {} do
			let t = timerAfter(int(timeoutMs))
			chanRecv(t)
			{ok: false, timeout: true, error: sprintf("timeout after %dms", [int(timeoutMs)])}
		end)
		let win = procJoinAny([tp, tt])
		if win.index == 0 then
			procCancel(tt)
			procJoin(tt)
		else
			procCancel(tp)
			procJoin(tp)
		end
		win.value
	end
	let dur = nowMillis() - t0
	let ok = base.ok
	let isTimeout = mapHas(base, "timeout") and bool(base.timeout)
	let err = if mapHas(base, "error") and base.error != null then base.error else "" end
	let _printOriginIfAny = fun(_: Null) -> Null do
		let o = _originOf(name)
		if o != null then
			printf("    AT    %s:%d (test)\n", [o.file, int(o.line)])
		end
		null
	end
	if emit then
		if verbose then
			if ok then
				printf("--- PASS: %s (%dms)\n", [name, int(dur)])
			else
				_printOriginIfAny(null)
				printf("%s\n", [err])
				let tag = if isTimeout then "TIMEOUT" else "FAIL" end
				printf("--- %s: %s (%dms)\n", [tag, name, int(dur)])
			end
		else
			if not ok then
				printf("=== RUN   %s\n", [name])
				_printOriginIfAny(null)
				printf("%s\n", [err])
				let tag2 = if isTimeout then "TIMEOUT" else "FAIL" end
				printf("--- %s: %s (%dms)\n", [tag2, name, int(dur)])
			end
		end
	end
	return {
		name: name,
		ok: ok,
		durationMs: dur,
		error: if not ok then err else null end,
		timeout: isTimeout
	}
end

# Run all registered tests.
#
# Execute tests sequentially or in parallel; aggregate a summary.
#
# Args:
#	parallel: Bool — run concurrently
#	timeoutMs: Int? — per-test timeout (<=0 or null disables)
#	verbose: Bool — print per-test lines even on pass
#
# Return:
#	Summary — {passed, failed, total, durationMs}
let _runAll = fun(parallel: Bool, timeoutMs: Int?, verbose: Bool) -> Summary do
	let start = nowMillis()
	let passed = 0
	let failed = 0
	let total = 0
	let reg = tests(null)
	let names = []
	for [k, _] in reg do
		names = names + [k]
	end
	names = _sortStrs(names)
	if parallel then
		if verbose then
			for name in names do
				printf("=== RUN   %s\n", [name])
			end
		end
		let ps = []
		for name in names do
			let fn = reg[name]
			ps = ps + [
				procSpawn(fun(_: Null) -> {} do
					_runOne(name, fn, timeoutMs, verbose, false)
				end)
			]
			total = total + 1
		end
		let results = procJoinAll(ps)
		let i = 0
		while i < len(results) do
			let res = results[i]
			if res.ok then
				passed = passed + 1
				if verbose then
					printf("--- PASS: %s (%dms)\n", [res.name, int(res.durationMs)])
				end
			else
				failed = failed + 1
				let o = _originOf(res.name)
				if o != null then
					printf("    AT    %s:%d (test)\n", [o.file, int(o.line)])
				end
				if res.error != null then
					printf("%s\n", [res.error])
				end
				let tag = if res.timeout then "TIMEOUT" else "FAIL" end
				printf("--- %s: %s (%dms)\n", [tag, res.name, int(res.durationMs)])
			end
			i = i + 1
		end
	else
		for name in names do
			let fn = reg[name]
			total = total + 1
			let res = _runOne(name, fn, timeoutMs, verbose, true)
			if res.ok then
				passed = passed + 1
			else
				failed = failed + 1
			end
		end
	end
	let dur = nowMillis() - start
	return {passed: passed, failed: failed, total: total, durationMs: dur}
end

# =========================
# Public API — runner
# =========================

# Run the test suite.
#
# Reset registry, discover tests by pathPrefix, import fresh modules, run tests, print a summary line.
#
# Args:
#	opts: RunnerOpts — {pathPrefix, parallel, verbose, timeoutMs}
#
# Return:
#	Summary — execution summary
let run = fun(opts: RunnerOpts) -> Summary do
	let prefix = if mapHas(opts, "pathPrefix") and opts.pathPrefix != null then opts.pathPrefix else "." end
	let verbose = if mapHas(opts, "verbose") and opts.verbose != null then bool(opts.verbose) else false end
	let parallel = if mapHas(opts, "parallel") and opts.parallel != null then bool(opts.parallel) else false end
	let timeoutMs = if mapHas(opts, "timeoutMs") and opts.timeoutMs != null then int(opts.timeoutMs) else null end
	reset(null)
	let files = _discoverByPrefix(prefix)
	if verbose then
		printf("found %d test file(s)\n", [len(files)])
	end
	let _ = _loadTests(files, verbose)
	let summary = _runAll(parallel, timeoutMs, verbose)
	printf("ok: %d  fail: %d  total: %d  durationMs: %d\n", [
		summary.passed,
		summary.failed,
		summary.total,
		summary.durationMs
	])
	return summary
end
