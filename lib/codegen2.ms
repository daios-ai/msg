# Codegen core: co-evolve program and tests from a single Spec.
#
# Minimal surface:
# - Oracles: specToTests(spec, seed) -> [Test]? ; specToProgram(spec, seed) -> Str?
# - Runner: run(code, tests) -> RunResult
# - Loop: bootstrap(...) -> State? ; evolve(state, budget) -> State
#
# Conventions:
# - Soft errors return null with an annotation; hard errors are avoided.
# - Functions are anonymous, bound via let; tabs for indentation.

# ---------- Types ----------

type Spec = { text!: Str }  # Single source of truth (types + PRE text)

type Seeds = { tests!: Int, program!: Int }  # Deterministic seeds

type Test = { name!: Str, src!: Str }  # Each test is a Bool-returning expression

type FailureClass = Enum["lex", "parse", "runtime"]

type TestOutcome = {
	name!: Str,
	ok!: Bool,
	error: Str,
	class!: FailureClass
}

type Trace = {
	testOutcomes!: [TestOutcome],
	elapsedMillis!: Int
}

type RunResult = { pass!: Bool, trace!: Trace }

type Program = { code!: Str, hash!: Str }

type State = {
	spec!: Spec,
	seeds!: Seeds,
	T_test!: [Test],
	T_train!: [Test],
	program: Program,
	last!: RunResult
}

type Budget = {
	maxRounds!: Int,
	maxAddsPerRound!: Int,
	maxRepairsPerRound!: Int
}

# ---------- Oracles (public) ----------

# Generate tests from a Spec (seeded, role-separated).
#
# Produce a non-empty, deduplicated set of Bool-returning test expressions.
#
# Args:
#	spec: Spec — types + PRE notes (single source of truth).
#	seed: Int — deterministic seed; same (spec, seed) → same tests.
# Return:
#	[Test]? — tests or null on error.
let specToTests = oracle(spec: Spec, seed: Int) -> [Test]?

# Generate a program candidate that attempts to satisfy the Spec (seeded, role-separated).
#
# Produce compilable MindScript source code for the program.
#
# Args:
#	spec: Spec — single source of truth (types + PRE).
#	seed: Int — deterministic seed; separate from tests’ seed.
# Return:
#	Str? — compilable source or null on error.
let specToProgram = oracle(spec: Spec, seed: Int) -> Str?

# ---------- Runner (public) ----------

# Run a program against tests; classify failures; aggregate a trace.
#
# Each test is an expression expected to evaluate to true.
#
# Args:
#	code: Str — MindScript program (module/function code).
#	tests: [Test] — Bool-returning expressions.
# Return:
#	RunResult — pass=true iff all tests pass.
let run = fun(code: Str, tests: [Test]) -> RunResult do
	let t0 = nowMillis(null)
	let imported = try(fun() do importCode("prog", code) end)
	if not imported.ok then
		let cls = _guessClass(imported.error)
		let ts = list(map(fun(tt) do { name: tt.name, ok: false, error: imported.error, class: cls } end, iter(tests)))
		{ pass: false, trace: { testOutcomes: ts, elapsedMillis: nowMillis(null) - t0 } }
	else
		let outcomes = []
		for tt in tests do
			let oc = _runOne(tt)
			outcomes = push(outcomes, oc)
		end
		let allOk = _allOk(outcomes)
		{ pass: allOk, trace: { testOutcomes: outcomes, elapsedMillis: nowMillis(null) - t0 } }
	end
end

# ---------- Loop (public) ----------

# Bootstrap from Spec: freeze tiny golden shard, generate first program, evaluate.
#
# Generate tests, split into T_test/T_train, synthesize program, and run once.
#
# Args:
#	spec: Spec
#	seeds: Seeds
#	goldenSize: Int — size of frozen T_test (tiny; e.g., 3)
# Return:
#	State? — initialized state or null on error.
let bootstrap = fun(spec: Spec, seeds: Seeds, goldenSize: Int) -> State? do
	let allTests = specToTests(spec, seeds.tests)
	if allTests == null then
		return null # <tests generation failed>
	end
	let tests = _dedupeTests(allTests)
	if len(tests) == 0 then
		return null # <no tests>
	end

	let T_test = _pickGoldenShard(tests, goldenSize)
	let T_train = _minus(tests, T_test)

	let code = specToProgram(spec, seeds.program)
	if code == null then
		return null # <program generation failed>
	end
	let program = { code: code, hash: _hash(code) }

	let gate = run(program.code, T_test)
	let work = run(program.code, T_train)
	let merged = _mergeTraces(gate.trace, work.trace)
	{ spec: spec, seeds: seeds, T_test: T_test, T_train: T_train, program: program, last: { pass: gate.pass and work.pass, trace: merged } }
end

# Bounded evolution loop: add ≤1 canonical test per distinct failure; ≤1 minimal repair.
#
# Iterate within strict budgets. Stops on gate failure, all-pass, or budget exhaustion.
#
# Args:
#	state: State — from bootstrap or prior step.
#	budget: Budget — strict caps.
# Return:
#	State — final state after 0..maxRounds steps.
let evolve = fun(state: State, budget: Budget) -> State do
	let round = 0
	let s = state
	while round < budget.maxRounds do
		let gate = run(s.program.code, s.T_test)
		if not gate.pass then
			let mergedEarly = _mergeTraces(gate.trace, run(s.program.code, s.T_train).trace)
			return { spec: s.spec, seeds: s.seeds, T_test: s.T_test, T_train: s.T_train, program: s.program, last: { pass: false, trace: mergedEarly } }
		end

		let work = run(s.program.code, s.T_train)
		if work.pass then
			let mergedDone = _mergeTraces(gate.trace, work.trace)
			return { spec: s.spec, seeds: s.seeds, T_test: s.T_test, T_train: s.T_train, program: s.program, last: { pass: true, trace: mergedDone } }
		end

		let adds = _canonicalAdds(s.spec, s.program.code, work.trace, s, budget.maxAddsPerRound)
		let newTrain = _appendNew(s.T_train, adds)
		let repaired = _maybeRepair(s.spec, s.program.code, work.trace, budget.maxRepairsPerRound, s.seeds.program)
		let nextCode = if repaired == null then s.program.code else repaired end
		let nextProg = { code: nextCode, hash: _hash(nextCode) }

		let nextGate = run(nextCode, s.T_test)
		let nextWork = run(nextCode, newTrain)
		let nextMerged = _mergeTraces(nextGate.trace, nextWork.trace)
		let next = { spec: s.spec, seeds: s.seeds, T_test: s.T_test, T_train: newTrain, program: nextProg, last: { pass: nextGate.pass and nextWork.pass, trace: nextMerged } }

		s = next
		round = round + 1
	end
	s
end

# ---------- Helpers (private) ----------

# Evaluate a single test; classify failure vs success.
#
# Args:
#	tt: Test
# Return:
#	TestOutcome
let _runOne = fun(tt: Test) -> TestOutcome do
	let parsed = astParse(tt.src)
	if parsed == null then
		{ name: tt.name, ok: false, error: "parse error", class: "parse" }
	else
		let ev = try(fun() do astEval(parsed) end)
		if not ev.ok then
			let cls = _guessClass(ev.error)
			{ name: tt.name, ok: false, error: ev.error, class: cls }
		else
			let okv = bool(ev.value)
			if not okv then
				{ name: tt.name, ok: false, error: "assertion failed", class: "runtime" }
			else
				{ name: tt.name, ok: true, error: "", class: "runtime" }
			end
		end
	end
end

# Heuristic failure-class guess from error text.
#
# Args:
#	msg: Str
# Return:
#	FailureClass
let _guessClass = fun(msg: Str) -> FailureClass do
	let m = toLower(msg)
	let hasLex = len(match("lex|token", m)) > 0
	let hasParse = len(match("parse|syntax", m)) > 0
	if hasLex then
		"lex"
	elif hasParse then
		"parse"
	else
		"runtime"
	end
end

# All outcomes ok?
#
# Args:
#	outs: [TestOutcome]
# Return:
#	Bool
let _allOk = fun(outs: [TestOutcome]) -> Bool do
	let ok = true
	for o in outs do
		if not o.ok then ok = false end
	end
	ok
end

# Pick a tiny golden shard (first k after dedupe for stability).
#
# Args:
#	ts: [Test]
#	k: Int
# Return:
#	[Test]
let _pickGoldenShard = fun(ts: [Test], k: Int) -> [Test] do
	let out = []
	let i = 0
	while i < len(ts) and len(out) < k do
		out = push(out, ts[i])
		i = i + 1
	end
	out
end

# Deduplicate tests by (name, src).
#
# Args:
#	ts: [Test]
# Return:
#	[Test]
let _dedupeTests = fun(ts: [Test]) -> [Test] do
	let seen = {}
	let out = []
	for t in ts do
		let key = _hash(t.name + ":" + t.src)
		if not mapHas(seen, key) then
			seen[key] = true
			out = push(out, t)
		end
	end
	out
end

# Set minus by (name, src).
#
# Args:
#	all: [Test]
#	part: [Test]
# Return:
#	[Test]
let _minus = fun(all: [Test], part: [Test]) -> [Test] do
	let out = []
	for t in all do
		let found = false
		for p in part do
			if t.name == p.name and t.src == p.src then
				found = true
			end
		end
		if not found then out = push(out, t) end
	end
	out
end

# Merge two traces (gate first, work second).
#
# Args:
#	a: Trace
#	b: Trace
# Return:
#	Trace
let _mergeTraces = fun(a: Trace, b: Trace) -> Trace do
	{ testOutcomes: a.testOutcomes + b.testOutcomes, elapsedMillis: a.elapsedMillis + b.elapsedMillis }
end

# Append new tests avoiding duplicates.
#
# Args:
#	base: [Test]
#	adds: [Test]
# Return:
#	[Test]
let _appendNew = fun(base: [Test], adds: [Test]) -> [Test] do
	_dedupeTests(base + adds)
end

# Determine ≤cap canonical additions: ≤1 per distinct failure class.
#
# Args:
#	spec: Spec
#	code: Str
#	tr: Trace
#	st: State
#	cap: Int
# Return:
#	[Test]
let _canonicalAdds = fun(spec: Spec, code: Str, tr: Trace, st: State, cap: Int) -> [Test] do
	let classes = _distinctClasses(tr)
	let out = []
	for c in classes do
		if len(out) >= cap then
			break
		end
		let seed = _deriveSeed(st.seeds.tests, code, c)
		let cand = specToTests(spec, seed)
		if cand == null then
			continue
		end
		let pool = _dedupeTests(cand)
		let pick = _firstReproducing(pool, code, c, st)
		if pick != null then
			out = push(out, pick)
		end
	end
	out
end

# Distinct failure classes from a trace (stable order).
#
# Args:
#	tr: Trace
# Return:
#	[FailureClass]
let _distinctClasses = fun(tr: Trace) -> [FailureClass] do
	let seen = {}
	let out = []
	for o in tr.testOutcomes do
		if not mapHas(seen, o.class) then
			seen[o.class] = true
			out = push(out, o.class)
		end
	end
	out
end

# First candidate that reproduces the target class against current code.
#
# Args:
#	pool: [Test]
#	code: Str
#	target: FailureClass
#	st: State
# Return:
#	Test?
let _firstReproducing = fun(pool: [Test], code: Str, target: FailureClass, st: State) -> Test? do
	for t in pool do
		# skip duplicates
		let dup = false
		for e in st.T_train do
			if e.name == t.name and e.src == t.src then
				dup = true
			end
		end
		if dup then
			continue
		end
		let outcome = run(code, [t]).trace.testOutcomes[0]
		if (not outcome.ok) and outcome.class == target then
			return t
		end
	end
	return null # <no canonical candidate>
end

# One minimal "repair": regenerate program with derived seed from trace key.
#
# Args:
#	spec: Spec
#	code: Str
#	tr: Trace
#	cap: Int
#	baseSeed: Int
# Return:
#	Str?
let _maybeRepair = fun(spec: Spec, code: Str, tr: Trace, cap: Int, baseSeed: Int) -> Str? do
	if cap <= 0 then
		return null # <repairs disabled>
	end
	let seed = _mix(baseSeed, _traceKey(tr))
	let next = specToProgram(spec, seed)
	if next == null then
		return null # <repair failed>
	end
	next
end

# Content hash as hex (stable-ish).
#
# Args:
#	s: Str
# Return:
#	Str
let _hash = fun(s: Str) -> Str do
	sha256(s)
end

# Derive a seed from base + code + class (uses uid for Int).
#
# Args:
#	base: Int
#	code: Str
#	cls: FailureClass
# Return:
#	Int
let _deriveSeed = fun(base: Int, code: Str, cls: FailureClass) -> Int do
	base + uid(code + ":" + cls)
end

# Mix an Int seed with a string key (uses uid).
#
# Args:
#	seed: Int
#	key: Str
# Return:
#	Int
let _mix = fun(seed: Int, key: Str) -> Int do
	seed + uid(key)
end

# Compact trace key (class:error pairs).
#
# Args:
#	tr: Trace
# Return:
#	Str
let _traceKey = fun(tr: Trace) -> Str do
	join(list(map(fun(o) do o.class + ":" + o.error end, iter(tr.testOutcomes))), "|")
end

# Deterministically derive Seeds from the Spec text.
#
# Args:
#	spec: Spec
# Return:
#	Seeds
let deriveSeeds = fun(spec: Spec) -> Seeds do
	let base = uid(spec.text)
	{ tests: base, program: base + 1 }
end

# One-shot: freeze tiny shard, generate code, and evolve under small defaults.
#
# Args:
#	spec: Spec
# Return:
#	State? — final state or null on error.
let satisfy = fun(spec: { text!: Str }) -> State? do
	# Small, sane defaults (minimal surface).
	let DEFAULT_GOLDEN_SIZE = 3
	let DEFAULT_BUDGET = { maxRounds: 3, maxAddsPerRound: 1, maxRepairsPerRound: 1 }

	let seeds = deriveSeeds(spec)
	let s0 = bootstrap(spec, seeds, DEFAULT_GOLDEN_SIZE)
	if s0 == null then
		return null # <bootstrap failed>
	end
	evolve(s0, DEFAULT_BUDGET)
end

