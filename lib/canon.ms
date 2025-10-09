# File extension for MindScript sources.
let EXT_MS = ".ms"

# -------------------------
# Internal helpers (private)
# -------------------------

# Return true if string s ends with suffix.
# 
# Args:
# s: Str — input string.
# suffix: Str — suffix to check.
# Return:
# Bool — true iff s ends with suffix.
let _endsWith = fun(s: Str, suffix: Str) -> Bool do
	let n = len(s)
	let m = len(suffix)
	if m == 0 then
		return true
	end
	if n < m then
		return false
	end
	substr(s, n - m, n) == suffix
end

# Return true if path p is absolute (POSIX/Windows).
# 
# Args:
# p: Str — path to test.
# Return:
# Bool — true if absolute.
let _isAbs = fun(p: Str) -> Bool do
	if len(p) > 0 and substr(p, 0, 1) == "/" then
		return true
	end
	if len(p) >= 2 and substr(p, 0, 2) == "\\\\" then
		return true
	end
	len(match("^[A-Za-z]:[\\\\/]", p)) > 0
end

# Recursively walk a directory and return all file paths.
# 
# Args:
# dir: Str — absolute, cleaned directory path.
# Return:
# [Str]? — list of absolute file paths; null on error.
let _walk = fun(dir: Str) -> [Str]? do
	let entries = dirList(dir)
	if entries == null then
		return null # <cannot list directory>

	end
	let out = []
	for entry in entries do
		let p = pathJoin([dir, entry])
		let st = stat(p)
		if st == null then
			return null # <stat failed>

		end
		if st.isDir then
			let rec = _walk(p)
			if rec == null then
				return null # <walk failed>

			end
			for q in rec do
				out = out + [q]
			end
		else
			out = out + [p]
		end
	end
	out
end

# Return true if file path looks like a MindScript source.
# 
# Args:
# path: Str — file path.
# Return:
# Bool — true if *.ms file.
let _isMsFile = fun(path: Str) -> Bool do
	# Prefer extension check; fall back to suffix as a guard.
	let ext = pathExt(path)
	if ext != null and ext == EXT_MS then
		return true
	end
	_endsWith(path, EXT_MS)
end

# Compute an absolute, cleaned path from root (cwd-aware).
# 
# Args:
# root: Str — input path (relative or absolute).
# Return:
# Str — absolute, cleaned path (best-effort).
let _toAbsClean = fun(root: Str) -> Str do
	let base = if cwd() == null then
		"."
	else
		cwd()
	end
	if _isAbs(root) then
		pathClean(root)
	else
		pathClean(pathJoin([base, root]))
	end
end

# -------------------------
# Public API
# -------------------------

# Format a source string using the canonical formatter.
# 
# Summary:
# Thin wrapper around formatCode. Pure; no I/O.
# 
# Args:
# src: Str — MindScript source code.
# Return:
# Str? — canonical source on success; null on failure (e.g., syntax error).
let format = fun(src: Str) -> Str? do
	let out = formatCode(src)
	if out == null then
		return null # <unformattable>

	end
	out
end

# Discover MindScript files from a root directory.
# 
# Summary:
# Recursively walks from root (default runner should pass "."),
# and returns absolute, cleaned paths for files ending with ".ms".
# 
# Args:
# root: Str — directory to scan (relative or absolute).
# Return:
# [Str]? — list of files; null if root is not a directory or on I/O errors.
let files = fun(root: Str) -> [Str]? do
	let rootPath = _toAbsClean(root)
	let st = stat(rootPath)
	if st == null or not st.isDir then
		return null # <root not a directory>

	end
	let all = _walk(rootPath)
	if all == null then
		return null # <walk error>

	end
	let out = []
	for f in all do
		if _isMsFile(f) then
			out = out + [f]
		end
	end
	out
end

# Check if a file is already canonically formatted.
# 
# Summary:
# Reads file, formats its contents, and compares with original.
# 
# Args:
# path: Str — file path (relative or absolute).
# Return:
# Bool? — true if canonical, false if differs, null on read/format error.
let checkFile = fun(path: Str) -> Bool? do
	let abs = _toAbsClean(path)
	let src = readFile(abs)
	if src == null then
		return null # <cannot read file>

	end
	let fmtd = format(src)
	if fmtd == null then
		return null # <format failed>

	end
	fmtd == src
end

# Format a single file in place iff it differs.
# 
# Summary:
# Reads file, runs canonical formatter, and writes back only when changed.
# No hard failures; returns null on any read/format/write error.
# 
# Args:
# path: Str — file path (relative or absolute).
# Return:
# { changed!: Bool }? — change flag; null on error.
let formatFile = fun(path: Str) -> {changed: Bool}? do
	let abs = _toAbsClean(path)
	let src = readFile(abs)
	if src == null then
		return null # <cannot read file>

	end
	let fmtd = format(src)
	if fmtd == null then
		return null # <format failed>

	end
	if fmtd == src then
		return {changed: false}
	end
	let n = writeFile(abs, fmtd)
	if n == null then
		return null # <cannot write file>

	end
	{changed: true}
end

# Format all MindScript files under a directory tree.
# 
# Summary:
# Walks files(root), formats each in place, and aggregates a concise report.
# 
# Args:
# root: Str — directory to scan (relative or absolute).
# Return:
# { total!: Int, changed!: Int, errors!: Int }? — totals; null if discovery fails.
let formatTree = fun(root: Str) -> {changed: Int, errors: Int, total: Int}? do
	let fs = files(root)
	if fs == null then
		return null # <discovery failed>

	end
	let total = 0
	let changed = 0
	let errors = 0
	for f in fs do
		total = total + 1
		let r = formatFile(f)
		if r == null then
			errors = errors + 1
		elif r.changed then
			changed = changed + 1
		end
	end
	{total: total, changed: changed, errors: errors}
end