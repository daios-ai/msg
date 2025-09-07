# testrunner.ms — test discovery & execution helper for MindScript
#
# Usage:
#	let r = import("testrunner")
#	r.run({root: "lib", quiet: false, parallel: false})

# =========================
# Types
# =========================

# Runner options (all optional keys).
let RunnerOpts = type {
	root: Str,        # directory to search (default ".")
	parallel: Bool,   # reserved; currently ignored
	quiet: Bool       # suppress discovery logs
}

# Summary shape: whatever `testing.run*` returns; passed through.
let Summary = type {}

# =========================
# Helpers (private)
# =========================

# Print a line unless quiet.
#
# Summary:
#	Prints `msg` to STDOUT only when `quiet` is false.
#
# Args:
#	quiet: Bool — suppress output when true
#	msg:   Str  — message to print
#
# Returns:
#	Str — the same message
let _log = fun(quiet: Bool, msg: Str) -> Str do
	if not quiet then println(msg) end
	msg
end

# Return true iff s ends with suffix.
let _endsWith = fun(s: Str, suffix: Str) -> Bool do
	let n = len(s)
	let m = len(suffix)
	if m == 0 then
		true
	else
		if n < m then false else substr(s, n - m, n) == suffix end
	end
end

# Recognize *_test.ms files.
let _isTestFile = fun(name: Str) -> Bool do
	_endsWith(name, "_test.ms")
end

# Recursively list files under a directory (relative to input paths).
#
# Args:
#	dir: Str
#
# Returns:
#	[Str]
let _walk = fun(dir: Str) -> [Str] do
	let entries = dirList(dir)
	if entries == null then
		[]
	else
		let out = []
		for entry in iter(entries) do
			let p = pathJoin([dir, entry])
			let st = stat(p)
			if st != null and st.isDir then
				out = out + _walk(p)
			else
				out = out + [p]
			end
		end
		out
	end
end

# Discover test files (sorted for stability).
let _discover = fun(root: Str) -> [Str] do
	let all = _walk(root)
	let tests = []
	for f in iter(all) do
		if _isTestFile(f) then tests = tests + [f] end
	end
	# Insertion sort (inputs are usually small).
	let n = len(tests)
	let i = 0
	while i < n do
		let j = i
		while j > 0 and tests[j - 1] > tests[j] do
			let tmp = tests[j - 1]
			tests[j - 1] = tests[j]
			tests[j] = tmp
			j = j - 1
		end
		i = i + 1
	end
	tests
end

# Import discovered tests so they register with `testing`.
#
# Returns number of files successfully loaded.
let _loadTests = fun(files: [Str], quiet: Bool) -> Int do
	let loaded = 0
	for f in iter(files) do
		_log(quiet, "discover: " + f)
		let m = import(f)
		if m == null then
			let why = noteGet(m)
			let suffix = if why == null then "" else " — " + why end
			_log(false, "warning: could not import " + f + suffix)
		else
			loaded = loaded + 1
		end
	end
	loaded
end

# Try calling testing.runAll(null), else testing.run(null).
#
# Returns:
#	Summary? — null (annotated) on error
# Try testing.runAll({}) if present; else testing.run({}).
#
# Returns:
# {}? — testing summary (annotated null only if the testing module returns one)
let _invokeTesting = fun(testing: {}) -> {}? do
	if mapHas(testing, "runAll") then
		let r1 = try(fun() do testing.runAll({}) end)
		if r1.ok then
			return r1.value
		else
			fail("testing.runAll failed: " + str(r1.error))
		end
	end
	if mapHas(testing, "run") then
		let r2 = try(fun() do testing.run({}) end)
		if r2.ok then
			return r2.value
		else
			fail("testing.run failed: " + str(r2.error))
		end
	end
	fail("testing module missing run/runAll")
end


# =========================
# Public API
# =========================

# Run all tests discovered under a directory.
#
# Summary:
#	Walks `root` (default ".") recursively, imports every `*_test.ms` file
#	so they can register tests with the `testing` module, then calls
#	`testing.runAll(null)` if available, otherwise `testing.run(null)`,
#	and returns its summary map.
#
# Args:
#	opts: RunnerOpts — {root, parallel, quiet} (all optional)
#
# Returns:
#	Summary — pass-through of testing’s result
let run = fun(opts: RunnerOpts) -> Summary do
	let root =
		if mapHas(opts, "root") and opts.root != null then opts.root else "." end
	let quiet =
		if mapHas(opts, "quiet") and opts.quiet != null then bool(opts.quiet) else false end
	# Reserved (not implemented yet).
	let _parallel =
		if mapHas(opts, "parallel") and opts.parallel != null then bool(opts.parallel) else false end

	let testing = import("testing")
	if testing == null then
		let m = noteGet(testing)
		let suffix = if m == null then "" else ": " + m end
		fail("runner: could not import testing module" + suffix)
	end

	let files = _discover(root)
	_log(quiet, sprintf("found %d test file(s)", [len(files)]))

	let _ = _loadTests(files, quiet)

	let summary = _invokeTesting(testing)
	if summary == null then
		let m2 = noteGet(summary)
		let suffix2 = if m2 == null then "" else ": " + m2 end
		fail("runner: testing.run failed" + suffix2)
	end

	# Compact summary line (best-effort) unless quiet.
	if not quiet and mapHas(summary, "total") then
		let line = sprintf(
			"ok: %d  fail: %d  skip: %d  durationMs: %d",
			[
				(if mapHas(summary, "passed") then summary.passed else 0 end),
				(if mapHas(summary, "failed") then summary.failed else 0 end),
				(if mapHas(summary, "skipped") then summary.skipped else 0 end),
				(if mapHas(summary, "durationMs") then summary.durationMs else 0 end)
			]
		)
		println(line)
	end
	summary
end
