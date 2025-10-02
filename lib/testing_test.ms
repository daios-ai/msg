# Tests for testing.ms (orthogonal, minimal).
# Imports and uses the testing module explicitly.
let T = import("testing")

# --- assert(ok, msg?) ------------------------------------------------------

T.test("testing/assert/true", fun(_: Null) do
	T.assert(true, null)
end)

T.test("testing/assert/false-default-msg", fun(_: Null) do
	T.assertThrows(fun(_: Null) do
		T.assert(false, null)
	end, "assertion failed")
end)

T.test("testing/assert/false-custom-msg", fun(_: Null) do
	T.assertThrows(fun(_: Null) do
		T.assert(false, "nope")
	end, "nope")
end)

# --- assertEq(want, got) ---------------------------------------------------

T.test("testing/assertEq/equal-scalars", fun(_: Null) do
	T.assertEq(42, 42)
end)

T.test("testing/assertEq/equal-deep-map", fun(_: Null) do
	T.assertEq({a: 1, b: [2, 3]}, {b: [2, 3], a: 1})
end)

T.test("testing/assertEq/equal-deep-array", fun(_: Null) do
	T.assertEq([1, 2, {x: 3}], [1, 2, {x: 3}])
end)

T.test("testing/assertEq/mismatch-diffs", fun(_: Null) do
	T.assertThrows(fun(_: Null) do
		T.assertEq(1, 2)
	end, "not equal")
end)

# --- assertType(v, T) ------------------------------------------------------
# Use typeOf(...) to avoid referencing bare names like Int/Any.

T.test("testing/assertType/match", fun(_: Null) do
	# Expect an array-of-Int — infer its Type from a value.
	T.assertType([1, 2], typeOf([1]))
end)

T.test("testing/assertType/mismatch", fun(_: Null) do
	T.assertThrows(fun(_: Null) do
		# Expect Int (infer from 0), but pass a Num (3.14)
		T.assertType(3.14, typeOf(0))
	end, "type mismatch")
end)

# --- assertThrows(fn, sub?) ------------------------------------------------

T.test("testing/assertThrows/catches-fail", fun(_: Null) do
	T.assertThrows(fun(_: Null) do
		panic("boom")
	end, "boom")
end)

T.test("testing/assertThrows/requires-function", fun(_: Null) do
	T.assertThrows(fun(_: Null) do
		T.assertThrows(123, null)
	end, "type mismatch in parameter 'fn'")
end)

T.test("testing/assertThrows/fails-when-no-throw", fun(_: Null) do
	T.assertThrows(fun(_: Null) do
		T.assertThrows(fun(_: Null) do
			42
		end, null)
	end, "expected failure, got success")
end)

T.test("testing/assertThrows/substring-mismatch", fun(_: Null) do
	T.assertThrows(fun(_: Null) do
		T.assertThrows(fun(_: Null) do
			panic("actual message")
		end, "different substring")
	end, "did not contain substring")
end)

# --- test(name, fn) API ----------------------------------------------------

T.test("testing/test/register-returns-true", fun(_: Null) do
	let ok = T.test("testing/register/ok", fun(_: Null) do
		T.assert(true, null)
	end)
	T.assertEq(true, ok)
end)

T.test("testing/test/duplicate-name-fails", fun(_: Null) do
	T.test("testing/register/dup", fun(_: Null) do
		null
	end)
	T.assertThrows(fun(_: Null) do
		T.test("testing/register/dup", fun(_: Null) do
			null
		end)
	end, "duplicate test name")
end)

T.test("testing/test/second-arg-must-be-fn", fun(_: Null) do
	T.assertThrows(fun(_: Null) do
		T.test("testing/register/not-fn", 7)
	end, "type mismatch in parameter 'fn'")
end)

# --- cases(name, xs, fn) ---------------------------------------------------

T.test("testing/cases/registers-n-subtests", fun(_: Null) do
	let n = T.cases("testing/table/smoke", [10, 20], fun(x) do
		T.assert(x != null, null)
	end)
	T.assertEq(2, n)
end)

T.test("testing/cases/empty", fun(_: Null) do
	let n = T.cases("testing/table/empty", [], fun(_) do
		null
	end)
	T.assertEq(0, n)
end)

T.test("testing/cases/requires-one-arg-fn", fun(_: Null) do
	T.assertThrows(fun(_: Null) do
		T.cases("testing/table/bad-fn", [1], 123)
	end, "type mismatch in parameter 'fn'")
end)

# --- snapshot(path, value[, update]) ---------------------------------------

# Helper to build a unique temp filepath (in the OS temp dir).
let _tmpPath = fun(prefix: Str, suffix: Str) -> Str do
	let base = tempDir()
	let stamp = sprintf("%d", [nowMillis()])
	pathJoin([base, sprintf("%s.%s%s", [prefix, stamp, suffix])])
end

T.test("testing/snapshot/write-and-read", fun(_: Null) do
	let path = _tmpPath("snapshot.write-read", ".json")
	T.snapshotUpdate(path, {hello: "world", n: 7})
	T.assert(T.snapshot(path, {hello: "world", n: 7}, null), null)
	remove(path) # cleanup

end)

T.test("testing/snapshot/mismatch-detected", fun(_: Null) do
	let path = _tmpPath("snapshot.mismatch", ".json")
	T.snapshotUpdate(path, [1, 2, 3])
	T.assertThrows(fun(_: Null) do
		T.snapshot(path, [1, 2, 4], null)
	end, "snapshot mismatch")
	remove(path) # cleanup

end)

# --- seed(n) & RNG reproducibility -----------------------------------------

T.test("testing/seed/reproducible-int", fun(_: Null) do
	T.seed(12345)
	let a = randInt(1000000)
	T.seed(12345)
	let b = randInt(1000000)
	T.assertEq(a, b)
end)

T.test("testing/seed/reproducible-float", fun(_: Null) do
	T.seed(424242)
	let a = randFloat(null)
	T.seed(424242)
	let b = randFloat(null)
	T.assertEq(a, b)
end)

# --- tests() view & registry isolation -------------------------------------

T.test("testing/tests/returns-map-clone", fun(_: Null) do
	let m = T.tests(null)
	T.assertType(m, type {})
	m["__probe__"] = 1
	let m2 = T.tests(null)
	T.assertEq(false, mapHas(m2, "__probe__"))
end)

# --- assert: return value ---------------------------------------------------

T.test("testing/assert/returns-true", fun(_: Null) do
	let ok = T.assert(true, null)
	T.assertEq(true, ok)
end)

# --- assertEq: int vs float equality ---------------------------------------

T.test("testing/assertEq/int-vs-float-equal", fun(_: Null) do
	T.assertEq(2, 2.0)
end)

# --- assertThrows: empty substring means "any message" ----------------------

T.test("testing/assertThrows/empty-substring-ok", fun(_: Null) do
	T.assertThrows(fun(_: Null) do
		panic("some failure message")
	end, "")
end)

# --- assertType: subtyping & function values --------------------------------

T.test("testing/assertType/subtyping-Int-to-Num", fun(_: Null) do
	# 42:Int should satisfy Num.
	T.assertType(42, typeOf(0.0))
end)

T.test("testing/assertType/function-value", fun(_: Null) do
	let f = fun(_: Null) do
		null
	end
	T.assertType(f, typeOf(f))
end)

# --- cases: ensure subtest names are registered -----------------------------

T.test("testing/cases/registrations-visible", fun(_: Null) do
	let base = "testing/table/vis"
	let _ = T.cases(base, [7, 8], fun(_) do
		null
	end)
	let m = T.tests(null)
	T.assertEq(true, mapHas(m, sprintf("%s/0", [base])))
	T.assertEq(true, mapHas(m, sprintf("%s/1", [base])))
end)

# --- snapshot: overwrite on update -----------------------------------------

T.test("testing/snapshot/update-overwrites", fun(_: Null) do
	let path = _tmpPath("snapshot.overwrite", ".json")
	T.snapshotUpdate(path, {v: 1})
	T.assertThrows(fun(_: Null) do
		T.snapshot(path, {v: 2}, null)
	end, "snapshot mismatch")
	T.snapshotUpdate(path, {v: 2})
	T.assert(T.snapshot(path, {v: 2}, null), null)
	remove(path) # cleanup

end)

# --- reset: clears registry -------------------------------------------------

T.test("testing/reset/clears", fun(_: Null) do
	let before = T.tests(null)
	# Should be >= existing tests in this suite; just assert it's a map with some size.
	T.assert(len(before) != null and len(before) >= 0, null)
	T.reset(null)
	let after = T.tests(null)
	T.assertEq(0, len(after))
end)

# --- seed: returns echo value ----------------------------------------------

T.test("testing/seed/echo", fun(_: Null) do
	T.assertEq(123, T.seed(123))
end)