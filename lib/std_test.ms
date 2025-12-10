let T = import("testing")

# -----------------------------
# Errors & Console I/O
# -----------------------------

T.test("std/error/annotated-null", fun(_: Null) do
	let e = error("boom")
	T.assertEq(e, null)
	T.assertEq(noteGet(e), "boom")
end)

T.test("std/assert/success", fun(_: Null) do
	T.assertEq(assert(true), true)
end)

T.test("std/assert/failure-throws", fun(_: Null) do
	T.assertThrows(fun(_: Null) do
		assert(false)
	end, "assertion failed")
end)

T.test("std/print/returns-value", fun(_: Null) do
	T.assertEq(print(123), 123)
end)

T.test("std/println/returns-value", fun(_: Null) do
	T.assertEq(println("x"), "x")
end)

# -----------------------------
# Iterators
# -----------------------------

T.test("std/naturals0/three-steps", fun(_: Null) do
	let it = naturals0(null)
	T.assertEq(it(null), 0)
	T.assertEq(it(null), 1)
	T.assertEq(it(null), 2)
end)

T.test("std/naturals/three-steps", fun(_: Null) do
	let it = naturals(null)
	T.assertEq(it(null), 1)
	T.assertEq(it(null), 2)
	T.assertEq(it(null), 3)
end)

T.test("std/range/bounded", fun(_: Null) do
	let it = range(3, 6)
	T.assertEq(it(null), 3)
	T.assertEq(it(null), 4)
	T.assertEq(it(null), 5)
	T.assertEq(it(null), null)
end)

T.test("std/range/unbounded-head", fun(_: Null) do
	let it = range(7, null)
	T.assertEq(it(null), 7)
	T.assertEq(it(null), 8)
	T.assertEq(it(null), 9)
end)

T.test("std/iter/array", fun(_: Null) do
	let it = iter([10, 20])
	T.assertEq(it(null), 10)
	T.assertEq(it(null), 20)
	T.assertEq(it(null), null)
end)

T.test("std/iter/map", fun(_: Null) do
	let it = iter({a: 1, b: 2})
	let p1 = it(null)
	let p2 = it(null)
	let p3 = it(null)
	T.assertType(p1, type [Any])
	T.assertType(p2, type [Any])
	T.assertEq(p3, null)
	let k1 = if p1 == null then
		""
	else
		p1[0]
	end
	let k2 = if p2 == null then
		""
	else
		p2[0]
	end
	T.assert(k1 == "a" or k2 == "a", null)
	T.assert(k1 == "b" or k2 == "b", null)
end)

T.test("std/iter/passthrough-iterator", fun(_: Null) do
	let base = naturals0(null) # (Null->Int) should subtype (Null->Any?)

	let it = iter(base)
	T.assertEq(it(null), 0)
	T.assertEq(it(null), 1)
end)

T.test("std/list/collect", fun(_: Null) do
	let it = range(1, 4) # 1,2,3

	T.assertEq(list(it), [1, 2, 3])
end)

T.test("std/map/lazy", fun(_: Null) do
	let it = map(fun(x) do
		int(x) + 1
	end, range(0, 3))
	T.assertEq(it(null), 1)
	T.assertEq(it(null), 2)
	T.assertEq(it(null), 3)
	T.assertEq(it(null), null)
end)

T.test("std/filter/lazy", fun(_: Null) do
	let it = filter(fun(x) -> Bool do
		int(x) % 2 == 0
	end, range(0, 5))
	T.assertEq(it(null), 0)
	T.assertEq(it(null), 2)
	T.assertEq(it(null), 4)
	T.assertEq(it(null), null)
end)

T.test("std/reduce/non-empty", fun(_: Null) do
	# 1+2+3
	let sum = reduce(fun(a, b) do
		int(a) + int(b)
	end, range(1, 4))

	T.assertEq(sum, 6)
end)

T.test("std/reduce/empty", fun(_: Null) do
	let empty = reduce(fun(a, b) do
		a
	end, range(0, 0))
	T.assertEq(empty, null)
end)

# -----------------------------
# Arrays
# -----------------------------

T.test("std/slice/basic", fun(_: Null) do
	let xs = [0, 1, 2, 3, 4]
	T.assertEq(slice(xs, 1, 4), [1, 2, 3])
end)

T.test("std/push/appends", fun(_: Null) do
	T.assertEq(push([1, 2], 3), [1, 2, 3])
end)

T.test("std/pop/last", fun(_: Null) do
	T.assertEq(pop([9, 8]), 8)
end)

T.test("std/pop/empty-panics", fun(_: Null) do
	T.assertThrows(fun(_: Null) do
		pop([])
	end, null)
end)

T.test("std/shift/first", fun(_: Null) do
	T.assertEq(shift([1, 2, 3]), 1)
end)

T.test("std/shift/empty-panics", fun(_: Null) do
	T.assertThrows(fun(_: Null) do
		shift([])
	end, null)
end)

T.test("std/unshift/prepend", fun(_: Null) do
	T.assertEq(unshift([2, 3], 1), [1, 2, 3])
	T.assertEq(unshift([], 5), [5])
end)

# -----------------------------
# Objects (maps)
# -----------------------------

T.test("std/keys/iterator", fun(_: Null) do
	let obj = {a: 1, b: 2}
	let it = keys(obj)
	let k1 = it(null)
	let k2 = it(null)
	let k3 = it(null)
	T.assertType(k1, type Str?)
	T.assertType(k2, type Str?)
	T.assertEq(k3, null)
	T.assert(k1 == "a" or k2 == "a", null)
	T.assert(k1 == "b" or k2 == "b", null)
end)

T.test("std/values/iterator", fun(_: Null) do
	let obj = {a: 7, b: 9}
	let it = values(obj)
	let v1 = it(null)
	let v2 = it(null)
	let v3 = it(null)
	T.assert(v1 == 7 or v2 == 7, null)
	T.assert(v1 == 9 or v2 == 9, null)
	T.assertEq(v3, null)
end)

T.test("std/dir/filters-and-annotates", fun(_: Null) do
	let obj = {
		a: 1, # alpha
		_hidden: 2,
		b: 3 # bravo
	}
	let ds = dir(obj)
	T.assertEq(len(ds), 2)
	let s0 = ds[0]
	let s1 = ds[1]
	T.assert(s0 == "a" or s1 == "a", null)
	T.assert(s0 == "b" or s1 == "b", null)
	let ann0 = noteGet(s0)
	let ann1 = noteGet(s1)
	T.assert(ann0 == "alpha" or ann0 == "bravo", null)
	T.assert(ann1 == "alpha" or ann1 == "bravo", null)
	T.assert(ann0 != ann1, null)
end)

# -----------------------------
# Import helpers
# -----------------------------

T.test("std/codeImport/basic", fun(_: Null) do
	let src = "let foo = 7\n"
	let imp = codeImport(src)
	let m = imp("m_std_code_import")
	T.assertEq(m.foo, 7)
end)

T.test("std/importUrl/failure-soft-null", fun(_: Null) do
	let r = importUrl("http://invalid.localhost/does-not-exist.ms")
	T.assertEq(r, null)
	T.assert(not (noteGet(r) == null), "expected annotated null")
end)

# -----------------------------
# Misc
# -----------------------------

T.test("std/uid/deterministic-and-different", fun(_: Null) do
	let a1 = uid("x")
	let a2 = uid("x")
	let b = uid("y")
	T.assertEq(a1, a2)
	T.assert(a1 != b, null)
end)

T.test("std/mute/always-null", fun(_: Null) do
	T.assertEq(mute(123), null)
end)

# -----------------------------
# Oracle hook & health
# -----------------------------

T.test("std/oracleStatus/prints-state", fun(_: Null) do
	let s = oracleStatus(null)
	# Accept either state without coupling to environment
	T.assert(println(s) != null, null)
	T.assert(s == "oracle: installed" or s == "oracle: not installed", null)
end)

T.test("std/oracleHealth/installed-or-soft-null", fun(_: Null) do
	let r = oracleHealth(null)
	if r == null then
		let ann = noteGet(r)
		T.assert(ann != null, "<missing annotation>")
		T.assert(str(ann) == "oracle not installed" or len(str(ann)) > 0, null)
	else
		T.assertType(r, type {ms: Int, ok: Bool})
		T.assertEq(r.ok, true)
		T.assert(r.ms >= 0, null)
	end
end)