# canon_test.ms — tiny, orthogonal tests for canon

let T = import("testing")
let C = import("canon")

# =========================
# Test helpers (private)
# =========================

# Compat: tempDir wrapper that supports both tempDir(null) and tempDir().
let _tempDir = fun(_: Null) -> Str do
	# Try 1-arg form first (per spec); fall back to 0-arg if needed.
	let r = try(fun(_: Null) do
		tempDir(null)
	end)
	if r.ok then
		return r.value
	end
	# Assume zero-arg in this runtime.
	tempDir()
end

# Create an empty temporary directory and return its absolute path.
let _mkTempDir = fun(_: Null) -> Str do
	let base = _tempDir(null)
	let ts = sprintf("canon-test-%d", [nowNanos()])
	let dir = pathJoin([base, ts])
	let ok = mkdir(dir)
	if not ok then
		return pathClean(dir) # <mkdir failed>

	end
	pathClean(dir)
end

# Write a file with given content in dir; return absolute path.
let _write = fun(dir: Str, name: Str, data: Str) -> Str do
	let p = pathJoin([dir, name])
	let _ = writeFile(p, data)
	pathClean(p)
end

# Read file contents (hard-fail in tests on error).
let _mustRead = fun(path: Str) -> Str do
	let s = readFile(path)
	if s == null then
		panic(sprintf("mustRead failed: %s", [path]))
	end
	s
end

# Minimal valid MindScript snippet (canonical form should be idempotent).
let _validSrc = fun(_: Null) -> Str do
	"let x = 1\n"
end

# Messy but valid snippet that should format to _validSrc().
let _messySrc = fun(_: Null) -> Str do
	"   let    x=1   \n"
end

# Syntactically invalid snippet (formatter should soft-error).
let _badSrc = fun(_: Null) -> Str do
	"let\n"
end

# =========================
# Tests
# =========================

T.test("canon/format_valid", fun(_: Null) do
	let src = _validSrc(null)
	let fmtd = C.format(src)
	T.assert(fmtd != null, "format returned null on valid input")
	T.assertEq(fmtd, C.format(fmtd))
end)

T.test("canon/format_unformattable", fun(_: Null) do
	let fmtd = C.format(_badSrc(null))
	T.assertEq(null, fmtd)
end)

T.test("canon/files_discovery", fun(_: Null) do
	let dir = _mkTempDir(null)
	let a = _write(dir, "a.ms", _validSrc(null))
	let _ = _write(dir, "b.txt", "not mindscript\n")
	let sub = pathJoin([dir, "sub"])
	let _mk = mkdir(sub)
	let c = _write(sub, "c.ms", _validSrc(null))

	let got = C.files(dir)
	T.assert(got != null, "files returned null")
	T.assertEq(len(got), 2)

	# Order not guaranteed; check presence.
	let joined = join(got, "|")
	T.assert(bool(len(match(pathClean(a), joined)) > 0), "missing a.ms")
	T.assert(bool(len(match(pathClean(c), joined)) > 0), "missing c.ms")
end)

T.test("canon/files_no_dir", fun(_: Null) do
	let dir = _mkTempDir(null)
	let p = _write(dir, "just_a_file.ms", _validSrc(null))
	let got = C.files(p)
	T.assertEq(null, got)
end)

T.test("canon/checkFile_true", fun(_: Null) do
	let dir = _mkTempDir(null)
	let canon = C.format(_validSrc(null))
	let p = _write(dir, "ok.ms", canon)
	let ok = C.checkFile(p)
	T.assertEq(true, ok)
end)

T.test("canon/checkFile_false", fun(_: Null) do
	let dir = _mkTempDir(null)
	let p = _write(dir, "messy.ms", _messySrc(null))
	let ok = C.checkFile(p)
	T.assertEq(false, ok)
end)

T.test("canon/checkFile_read_fail", fun(_: Null) do
	let dir = _mkTempDir(null)
	let missing = pathJoin([dir, "nope.ms"])
	let ok = C.checkFile(missing)
	T.assertEq(null, ok)
end)

T.test("canon/formatFile_change", fun(_: Null) do
	let dir = _mkTempDir(null)
	let p = _write(dir, "needs_format.ms", _messySrc(null))
	let res = C.formatFile(p)
	T.assert(res != null, "formatFile returned null unexpectedly")
	T.assertEq(true, res.changed)

	# File now equals canonical
	let now = _mustRead(p)
	let canon = C.format(_validSrc(null))
	T.assertEq(canon, now)
end)

T.test("canon/formatFile_noop", fun(_: Null) do
	let dir = _mkTempDir(null)
	let canon = C.format(_validSrc(null))
	let p = _write(dir, "already_ok.ms", canon)
	let res = C.formatFile(p)
	T.assert(res != null, "formatFile returned null unexpectedly")
	T.assertEq(false, res.changed)
	T.assertEq(canon, _mustRead(p))
end)

T.test("canon/formatFile_fail", fun(_: Null) do
	let dir = _mkTempDir(null)
	let p = _write(dir, "bad.ms", _badSrc(null))
	let res = C.formatFile(p)
	T.assertEq(null, res)
end)

T.test("canon/formatTree_happy", fun(_: Null) do
	let dir = _mkTempDir(null)
	let canon = C.format(_validSrc(null))
	let _ = _write(dir, "a_ok.ms", canon)
	let _ = _write(dir, "b_messy.ms", _messySrc(null))
	let _ = _write(dir, "c_bad.ms", _badSrc(null))

	let rep = C.formatTree(dir)
	T.assert(rep != null, "formatTree returned null unexpectedly")
	T.assertEq(3, rep.total)
	T.assertEq(1, rep.changed)
	T.assertEq(1, rep.errors)
end)

T.test("canon/formatTree_discovery_fail", fun(_: Null) do
	let dir = _mkTempDir(null)
	let file = _write(dir, "lonely.ms", _validSrc(null))
	let rep = C.formatTree(file) # not a directory

	T.assertEq(null, rep)
end)