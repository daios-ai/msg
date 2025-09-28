# mtml — Minimal, ergonomic HTML templating for MindScript
#
# "Less is more": a compact engine with the essentials:
# - {{ expr | filter(arg) }}  (autoescape ON by default)
# - {% if %} / {% elif %} / {% else %} / {% end %}
# - {% for x in xs %} … {% end %}  (loop meta: index0, index, first, last, length)
# - {% include "partial.html" [with expr] %}
# - {% extends "base.html" %} + {% block name %} … {% end %} (single inheritance)
# - Whitespace trim: {%- … -%}, {{- … -}}
#
# MVP constraints to keep the core tiny (updated):
# - Expressions: identifiers, dotted access, string/number/bool/null, calls to registered helpers,
#   filter pipelines (expr | filter(a, b)), and ternary A if cond else B
# - Autoescape: HTML-text & attribute-safe character escaping (no full JS/CSS contextuality).
#   Use safeHTML()/|safe to bypass for HTML; use safeURL() for href/src.
# - Includes/extends: confined to Env.baseDir via a loader jail; relative paths resolve from the current template.
# - strictUndefined defaults to true: unknown names are render errors.
# - Context guards (minimal, to close biggest XSS traps without new syntax):
#   * Interpolation inside <script> or <style> is disallowed (annotated error).
#   * Interpolation inside event handler attributes (on*) is disallowed.
#   * Interpolation in href/src requires SafeURL (or annotated error).
#
# Public surface (unchanged):
# env(), registerFilter(), registerHelper(), registerGlobal(),
# fromString(), load(), render(), renderTo(),
# safeHTML(), safeURL(), diagnostics(), installBuiltins()

let SafeHTML = type {__safe_html__!: Bool, html!: Str}
let SafeURL  = type {__safe_url__!:  Bool, url!:  Str}

let safeHTML = fun(s: Str) -> SafeHTML do
	{ __safe_html__: true, html: s }
end

# --- URL allowlist (hardened baseline; allows relatives) -------------------

let _urlAllowed = fun(u: Str) -> Bool do
	if u == null then return false end
	let s = strip(toLower(u))
	# reject control chars and backslashes
	if len(match("[\\r\\n\\x00\\\\]", s)) > 0 then return false end
	# allow relative and protocol-relative
	if substr(s, 0, 2) == "//" then return true end
	if substr(s, 0, 1) == "/"  then return true end
	if substr(s, 0, 2) == "./" then return true end
	if substr(s, 0, 3) == "../" then return true end
	# absolute schemes
	if substr(s, 0, 8) == "https://" then return true end
	if substr(s, 0, 7) == "http://"  then return true end
	if substr(s, 0, 7) == "mailto:"  then return true end
	if substr(s, 0, 4)  == "tel:"     then return true end
	false
end

let safeURL = fun(u: Str) -> SafeURL? do
	if not _urlAllowed(u) then
		return error("render: disallowed url scheme")
	end
	{ __safe_url__: true, url: u }
end

# --- Types ------------------------------------------------------------------

let RenderOpts = type {
	baseDir: Str,
	autoescape: Bool,
	strictUndefined: Bool,
	bytecodeCache: {}
}

let Span = type {line: Int, col: Int}

let TemplateError = type {
	kind: Enum["parse", "load", "render", "type"],
	message: Str,
	where: Span?
}

let Template = type {__mtml__: Bool, name: Str, ast: [Any]}

let Writer = type {write: Str -> Int?, flush: Null -> Bool?}

let Env = type {
	__mtml_env__: Bool,
	opts: RenderOpts,
	_filters: {},
	_helpers: {},
	_globals: {},
	_loader: Str -> {src: Str, name: Str}?
}

# =========================
# Utilities (escape, strings, errors, paths)
# =========================

let _defaultOpts = fun(o: RenderOpts) -> RenderOpts do
	let out = {
		baseDir: "",
		autoescape: true,
		strictUndefined: true,
		bytecodeCache: {}
	}
	if o != null then
		for [k, v] in o do
			out[k] = v
		end
	end
	out
end

let _err = fun(kind: Str, msg: Str, line: Int, col: Int) -> TemplateError do
	{ kind: kind, message: msg, where: {line: line, col: col} }
end

let diagnostics = fun(e: TemplateError) -> Str do
	if e == null then return "" end
	if e.where == null then
		return sprintf("[%s] %s", [e.kind, e.message])
	end
	sprintf("[%s] %s at %d:%d", [
		e.kind, e.message,
		int(e.where.line), int(e.where.col)
	])
end

let _htmlEscape = fun(s: Str) -> Str do
	# quick check: nothing to escape
	if match("[&<>\"']", s) == null then return s end
	let t  = replace("&",  "&amp;",  s)
	let t2 = replace("<",  "&lt;",   t)
	let t3 = replace(">",  "&gt;",   t2)
	let t4 = replace("\"", "&quot;", t3)
	replace("'", "&#39;", t4)
end

# Trim a single trailing newline (and surrounding whitespace) from the RIGHT of a chunk.
let _trimRightNL = fun(s: Str) -> Str do
	let i = len(s) - 1
	while i >= 0 do
		let ch = substr(s, i, i + 1)
		if ch == " " or ch == "\t" then
			i = i - 1
			continue
		end
		if ch == "\n" then
			return substr(s, 0, i)
		end
		break
	end
	s
end

# Remove trailing spaces/tabs only (keep trailing newline if any).
let _trimRightWS = fun(s: Str) -> Str do
	let i = len(s) - 1
	while i >= 0 do
		let ch = substr(s, i, i + 1)
		if ch == " " or ch == "\t" then
			i = i - 1
			continue
		end
		break
	end
	substr(s, 0, i + 1)
end

# Find the index of the FIRST '(' at top-level (depth 0) and not in quotes.
let _findTopLevelParen = fun(s: Str) -> Int do
	let i = 0
	let inQ = ""
	let depth = 0
	while i < len(s) do
		let ch = substr(s, i, i + 1)
		if inQ != "" then
			if ch == inQ then inQ = "" end
			i = i + 1
			continue
		end
		if ch == "\"" or ch == "'" then
			inQ = ch
			i = i + 1
			continue
		end
		if ch == "(" then
			if depth == 0 then return i end
			depth = depth + 1
			i = i + 1
			continue
		end
		if ch == ")" and depth > 0 then
			depth = depth - 1
		end
		i = i + 1
	end
	-1
end

# Trim a single leading newline (and surrounding whitespace) from the LEFT of a chunk.
let _trimLeadingWSNL = fun(s: Str) -> Str do
	let i = 0
	# eat spaces/tabs
	while i < len(s) do
		let ch = substr(s, i, i + 1)
		if ch == " " or ch == "\t" then
			i = i + 1
			continue
		end
		break
	end
	# if next is a newline, drop it too
	if i < len(s) and substr(s, i, i + 1) == "\n" then
		return substr(s, i + 1, len(s))
	end
	substr(s, i, len(s))
end

# Path jail helpers: resolve req relative to fromName under baseDir; return clean full path or error.
let _resolvePath = fun(baseDir: Str, fromName: Str, req: Str) -> Str? do
	let base = if len(fromName) == 0 then baseDir else pathDir(pathJoin([baseDir, fromName])) end
	let clean = pathClean(pathJoin([base, req]))
	let bd = pathClean(baseDir)
	if len(bd) > 0 and substr(clean, 0, len(bd)) != bd then
		return error("load: path escapes baseDir")
	end
	clean
end

let _relName = fun(baseDir: Str, full: Str) -> Str do
	let bd = pathClean(baseDir)
	if len(bd) == 0 then return full end
	if substr(full, 0, len(bd)) == bd and len(full) > len(bd) and substr(full, len(bd), len(bd) + 1) == "/" then
		return substr(full, len(bd) + 1, len(full))
	end
	full
end

# =========================
# Environment & registry
# =========================

let env = fun(opts: RenderOpts) -> Env do
	let o = _defaultOpts(opts)
	let e = {
		__mtml_env__: true,
		opts: o,
		_filters: {},
		_helpers: {},
		_globals: {},
		_loader: fun(path: Str) -> {src: Str, name: Str}? do
			let full = _resolvePath(o.baseDir, "", path)
			if full == null then
				return error("load: unable to resolve path")
			end
			let s = readFile(full)
			if s == null then
				return error("load: unable to read file")
			end
			{src: s, name: _relName(o.baseDir, full)}
		end
	}
	e
end

let registerFilter = fun(e: Env, name: Str, f) -> Bool do
	e._filters[name] = f
	true
end

let registerHelper = fun(e: Env, name: Str, f) -> Bool do
	e._helpers[name] = f
	true
end

let registerGlobal = fun(e: Env, name: Str, value) -> Bool do
	e._globals[name] = value
	true
end

# =========================
# Tokenizer ({{ }}, {% %}, with trim variants) + attr context hints
# =========================

# Token: { kind!: Enum["text","expr","stmt"], value!: Str, trimL!: Bool, trimR!: Bool, line!: Int, col!: Int, ctx?: {} }
# ctx for expr: {kind:"text"|"attr", attrName?:Str}
let _attrCtx = fun(src: Str, idx: Int) -> {kind: Str, attrName: Str?} do
	# find nearest '<' or '>' to the left
	let j = idx - 1
	let seenLt = -1
	let seenGt = -1
	while j >= 0 do
		let c = substr(src, j, j + 1)
		if c == "<" then seenLt = j
			break
		end
		if c == ">" then seenGt = j
			break
		end
		j = j - 1
	end
	if seenGt > seenLt then return {kind: "text"} end
	if seenLt == -1 then return {kind: "text"} end

	# inside a tag; are we inside a quoted value?
	let q = ""
	j = idx - 1
	while j > seenLt do
		let c = substr(src, j, j + 1)
		if c == "\"" or c == "'" then
			q = c
			break
		end
		j = j - 1
	end
	if q == "" then return {kind: "text"} end

	# ensure '=' before the quote
	let k = j - 1
	while k > seenLt and substr(src, k, k + 1) == " " do
		k = k - 1
	end
	if k <= seenLt or substr(src, k, k + 1) != "=" then
		return {kind: "text"}
	end

	# extract attribute name
	let aEnd = k - 1
	while aEnd > seenLt and match("[\\s/]", substr(src, aEnd, aEnd + 1)) == null do
		aEnd = aEnd - 1
	end
	let name = strip(substr(src, aEnd + 1, k - 0))
	{kind: "attr", attrName: toLower(name)}
end

let _lex = fun(src: Str) -> [Any]? do
	let tokens = []
	let i = 0
	let line = 1
	let col = 1
	let pendingTrimNext = false

	let pushText = fun(s: Str, tl: Bool, tr: Bool, ln: Int, co: Int) -> Null do
		if len(s) == 0 then return end
		if pendingTrimNext then
			s = _trimLeadingWSNL(s)
			pendingTrimNext = false
		end
		tokens = tokens + [
			{ kind: "text", value: s, trimL: tl, trimR: tr, line: ln, col: co }
		]
		null
	end

	while i < len(src) do
		let startLine = line
		let startCol = col
		let j = i
		let found = -1
		let tag = ""
		while j < len(src) do
			let ch = substr(src, j, j + 1)
			let nxt = if j + 1 < len(src) then substr(src, j + 1, j + 2) else "" end
			if ch == "{" and (nxt == "{" or nxt == "%") then
				found = j
				tag = ch + nxt
				break
			end
			if ch == "\n" then
				line = line + 1
				col = 1
			else
				col = col + 1
			end
			j = j + 1
		end

		if found == -1 then
			let text = substr(src, i, len(src))
			pushText(text, false, false, startLine, startCol)
			break
		end

		let text = substr(src, i, found)
		pushText(text, false, false, startLine, startCol)

		let trimL = false
		if found + 2 < len(src) and substr(src, found + 2, found + 3) == "-" then
			trimL = true
			tag = tag + "-"
		end

		let close = if tag == "{{" or tag == "{{-" then "}}" else "%}" end
		let k = found + if tag == "{{-" or tag == "{%-" then 3 else 2 end
		let innerStart = k
		let trimR = false
		let closed = -1
		while k < len(src) - 1 do
			let c2 = substr(src, k, k + 1)
			let c3 = substr(src, k + 1, k + 2)
			if c2 == "-" and c3 + substr(src, k + 2, k + 3) == close then
				trimR = true
				closed = k
				break
			end
			if c2 + c3 == close then
				closed = k
				break
			end
			k = k + 1
		end
		if closed == -1 then
			return error("parse: unclosed tag")
		end
		let inner = strip(substr(src, innerStart, closed))
		let endSpan = if trimR then closed + 3 else closed + 2 end
		let kind = if substr(tag, 0, 2) == "{{" then "expr" else "stmt" end
		let ctx = if kind == "expr" then _attrCtx(src, found) else null end
		tokens = tokens + [
			{ kind: kind, value: inner, trimL: trimL, trimR: trimR, line: startLine, col: startCol, ctx: ctx }
		]

		if trimR then
			pendingTrimNext = true
		end
		if trimL and len(tokens) >= 2 and tokens[len(tokens)-2].kind == "text" then
			let prev = tokens[len(tokens)-2]
			prev.value = _trimRightWS(prev.value)
			tokens[len(tokens)-2] = prev
		end

		let chunk = substr(src, found, endSpan)
		let p = 0
		while p < len(chunk) do
			let ch2 = substr(chunk, p, p + 1)
			if ch2 == "\n" then
				line = line + 1
				col = 1
			else
				col = col + 1
			end
			p = p + 1
		end
		i = endSpan
	end

	tokens
end

# =========================
# Parser to AST
# Node kinds:
# text(s, trimL, trimR)
# expr(code, span, ctx?)
# if [ [cond, body], [elifCond, body], ... , elseBody? ]
# for (name, iterExpr, body)
# include (pathExpr, ctxExpr?)
# block (name, body)
# extends (pathExpr)
# seq [nodes...]
# =========================

let _node = fun(tag: Str, a, b, c, d) -> {} do
	{tag: tag, a: a, b: b, c: c, d: d}
end

let _splitTopLevel = fun(s: Str, sep: Str) -> [Str] do
	let out = []
	let i = 0
	let last = 0
	let inQ = ""
	let esc = false
	let depth = 0
	let bcurly = 0
	let bsquare = 0
	while i < len(s) do
		let ch = substr(s, i, i + 1)
		if inQ != "" then
			if esc then
				esc = false
			else
				if ch == "\\" then
					esc = true
				elif ch == inQ then
					inQ = ""
				end
			end
		else
			if ch == "\"" or ch == "'" then
				inQ = ch
			elif ch == "(" then
				depth = depth + 1
			elif ch == ")" and depth > 0 then
				depth = depth - 1
			elif ch == "{" then
				bcurly = bcurly + 1
			elif ch == "}" and bcurly > 0 then
				bcurly = bcurly - 1
			elif ch == "[" then
				bsquare = bsquare + 1
			elif ch == "]" and bsquare > 0 then
				bsquare = bsquare - 1
			else
				if depth == 0 and bcurly == 0 and bsquare == 0 and substr(s, i, i + len(sep)) == sep then
					out = out + [strip(substr(s, last, i))]
					i = i + len(sep)
					last = i
					continue
				end
			end
		end
		i = i + 1
	end
	out = out + [strip(substr(s, last, len(s)))]
	out
end

let _parse = fun(tokens: [Any]) -> [Any]? do
	let pos = 0

	let parseSeq = fun(stopTags: [Str]) -> [Any]? do
		let out = []
		while pos < len(tokens) do
			let t = tokens[pos]
			if t.kind == "text" then
				pos = pos + 1
				out = out + [
					_node("text", {
						s: t.value, tl: t.trimL, tr: t.trimR, line: t.line, col: t.col
					}, null, null, null)
				]
				continue
			end
			if t.kind == "expr" then
				pos = pos + 1
				out = out + [
					_node("expr", {code: t.value, tl: t.trimL, tr: t.trimR, line: t.line, col: t.col, ctx: t.ctx}, null, null, null)
				]
				continue
			end

			let body = t.value
			let parts = split(body, " ")
			let head = parts[0]

			for tag in stopTags do
				if head == tag then
					return out
				end
			end

			if head == "if" then
				pos = pos + 1
				let branches = []
				let cond = strip(substr(body, 3, len(body)))
				let thenBody = parseSeq(["elif", "else", "end"])
				if thenBody == null then
					return error("parse: unterminated if")
				end
				branches = branches + [[cond, thenBody]]

				while pos < len(tokens) and tokens[pos].kind == "stmt" do
					let st = tokens[pos].value
					let p2 = split(st, " ")
					if p2[0] == "elif" then
						pos = pos + 1
						let cond2 = strip(substr(st, 5, len(st)))
						let b2 = parseSeq(["elif", "else", "end"])
						if b2 == null then
							return error("parse: unterminated elif")
						end
						branches = branches + [[cond2, b2]]
						continue
					end
					if p2[0] == "else" then
						pos = pos + 1
						let b3 = parseSeq(["end"])
						if b3 == null then
							return error("parse: unterminated else")
						end
						let endTL = if pos < len(tokens) then tokens[pos].trimL else false end
						let endTR = if pos < len(tokens) then tokens[pos].trimR else false end
						out = out + [_node("if", branches, b3, null, {tlOpen: t.trimL, trOpen: t.trimR, tlEnd: endTL, trEnd: endTR})]
						if pos < len(tokens) and tokens[pos].kind == "stmt" and tokens[pos].value == "end" then
							pos = pos + 1
							break
						end
						return error("parse: expected end after else")
					end
					break
				end

				let endTL2 = if pos < len(tokens) then tokens[pos].trimL else false end
				let endTR2 = if pos < len(tokens) then tokens[pos].trimR else false end
				out = out + [_node("if", branches, null, null, {tlOpen: t.trimL, trOpen: t.trimR, tlEnd: endTL2, trEnd: endTR2})]
				if pos < len(tokens) and tokens[pos].kind == "stmt" and tokens[pos].value == "end" then
					pos = pos + 1
				else
					return error("parse: expected end")
				end
				continue
			end

			if head == "for" then
				pos = pos + 1
				let rest = strip(substr(body, 4, len(body)))
				let bits = _splitTopLevel(rest, " in ")
				if len(bits) != 2 then
					return error("parse: for syntax")
				end
				let name = strip(bits[0])
				let iter = strip(bits[1])
				let b = parseSeq(["end"])
				if b == null then
					return error("parse: unterminated for")
				end
				let endTL = if pos < len(tokens) then tokens[pos].trimL else false end
				let endTR = if pos < len(tokens) then tokens[pos].trimR else false end
				out = out + [_node("for", name, iter, b, {tlOpen: t.trimL, trOpen: t.trimR, tlEnd: endTL, trEnd: endTR})]
				if pos < len(tokens) and tokens[pos].kind == "stmt" and tokens[pos].value == "end" then
					pos = pos + 1
				else
					return error("parse: expected end")
				end
				continue
			end

			if head == "include" then
				pos = pos + 1
				let rest = strip(substr(body, 8, len(body)))
				let parts2 = _splitTopLevel(rest, " with ")
				let pth = parts2[0]
				let ctxExpr = if len(parts2) >= 2 then parts2[1] else null end
				out = out + [_node("include", pth, ctxExpr, null, {tlOpen: t.trimL, trOpen: t.trimR})]
				continue
			end

			if head == "block" then
				pos = pos + 1
				let nm = strip(substr(body, 6, len(body)))
				let b = parseSeq(["end"])
				if b == null then
					return error("parse: unterminated block")
				end
				let endTL = if pos < len(tokens) then tokens[pos].trimL else false end
				let endTR = if pos < len(tokens) then tokens[pos].trimR else false end
				out = out + [_node("block", nm, b, null, {tlOpen: t.trimL, trOpen: t.trimR, tlEnd: endTL, trEnd: endTR})]
				if pos < len(tokens) and tokens[pos].kind == "stmt" and tokens[pos].value == "end" then
					pos = pos + 1
				else
					return error("parse: expected end")
				end
				continue
			end

			if head == "extends" then
				pos = pos + 1
				let p = strip(substr(body, 8, len(body)))
				out = out + [_node("extends", p, null, null, {tlOpen: t.trimL, trOpen: t.trimR})]
				continue
			end

			if head == "end" then
				return out
			end

			return error("parse: unknown tag")
		end
		out
	end

	let ast = parseSeq([])
	if ast == null then
		return error("parse: invalid structure")
	end
	ast
end

# =========================
# Expression evaluator (small)
# =========================

let _parseStringLit = fun(s: Str) -> Str? do
	if len(s) < 2 then return end
	let q = substr(s, 0, 1)
	if q != "\"" and q != "'" then return end
	if substr(s, len(s) - 1, len(s)) != q then return end
	substr(s, 1, len(s) - 1)
end

let _numOrBoolOrNull = fun(s: Str) -> Any? do
	if s == "true" then return true end
	if s == "false" then return false end
	if s == "null" then return end
	let i = int(s)
	if i != null and str(i) == s then return i end
	let n = num(s)
	if n != null then return n end
	null
end

let _argsParse = fun(s: Str) -> [Str] do
	if len(strip(s)) == 0 then return [] end
	_splitTopLevel(s, ",")
end

# Split "A if cond else B" at top-level (quote/paren aware)
let _splitTernary = fun(s: Str) -> {A: Str, cond: Str, B: Str}? do
	let partsIf = _splitTopLevel(s, " if ")
	if len(partsIf) != 2 then return null end
	let partsElse = _splitTopLevel(partsIf[1], " else ")
	if len(partsElse) != 2 then return null end
	{A: partsIf[0], cond: partsElse[0], B: partsElse[1]}
end

let _evalNamePath = fun(scope: {}, name: Str, strict: Bool) -> Any? do
	let parts = _splitTopLevel(name, ".")
	let cur = scope
	let j = 0
	while j < len(parts) do
		let p = parts[j]
		if j == 0 then
			if mapHas(cur, p) then
				cur = cur[p]
			else
				if strict then
					return error("render: missing name " + p)
				else
					return null
				end
			end
		else
			if cur == null then
				if strict then return error("render: access null") else return null end
			end
			if isType(cur, type {}) and mapHas(cur, p) then
				cur = cur[p]
			else
				if strict then return error("render: missing field " + p) else return null end
			end
		end
		j = j + 1
	end
	cur
end

let _evalExpr = fun(e: Env, scope: {}, code: Str) -> Any? do
	let pipes = _splitTopLevel(code, "|")
	if len(pipes) == 0 then return end

	let base = strip(pipes[0])
	let val = null

	# Base may be a ternary; resolve first so filters can apply to the result.
	let ter = _splitTernary(base)
	if ter != null then
		let cv = _evalExpr(e, scope, ter.cond)
		if cv == null and noteGet(cv) != null then return cv end
		if cv == null then return error("render: bad condition") end
		let truth = bool(cv)
		if truth == null then return error("render: non-bool condition") end
		val = if truth then _evalExpr(e, scope, ter.A) else _evalExpr(e, scope, ter.B) end
		if val == null and noteGet(val) != null then return val end
	else
		let q = _parseStringLit(base)
		if q != null then
			val = q
		else
			let prim = _numOrBoolOrNull(base)
			if prim != null or base == "null" then
				val = prim
			else
				let p = _findTopLevelParen(base)
				if p >= 0 and substr(base, len(base) - 1, len(base)) == ")" then
					let nm = strip(substr(base, 0, p))
					# Parentheses used for grouping, not a call
					if len(nm) == 0 then
						val = _evalExpr(e, scope, strip(substr(base, p + 1, len(base) - 1)))
					else
						if not mapHas(e._helpers, nm) then
							return error("render: unknown helper " + nm)
						end
						let argsStr = strip(substr(base, p + 1, len(base) - 1))
						let argExprs = _argsParse(argsStr)
						let argv = []
						let i = 0
						while i < len(argExprs) do
							let v = _evalExpr(e, scope, argExprs[i])
							if v == null and noteGet(v) != null then return v end
							if v == null and strip(argExprs[i]) != "null" then
								return error("render: bad arg")
							end
							argv = argv + [v]
							i = i + 1
						end
						val = e._helpers[nm]
						let k = 0
						while k < len(argv) do
							val = val(argv[k])
							k = k + 1
						end
					end
				else
					let merged = {}
					for [k, v] in e._globals do merged[k] = v end
					for [k2, v2] in scope do merged[k2] = v2 end
					let got = _evalNamePath(merged, base, e.opts.strictUndefined)
					val = got
				end
			end
		end
	end

	# Apply filters in the pipeline.
	let i = 1
	while i < len(pipes) do
		let spec = strip(pipes[i])
		if len(spec) == 0 then 
			i = i + 1
			continue 
		end
		let p = _findTopLevelParen(spec)
		let fname = ""
		let argList = []
		if p >= 0 and substr(spec, len(spec) - 1, len(spec)) == ")" then
			fname = strip(substr(spec, 0, p))
			let as = strip(substr(spec, p + 1, len(spec) - 1))
			argList = _argsParse(as)
		else
			fname = spec
		end
		if fname == "safe" then
			# Only allow strings or already safe wrappers
			if isType(val, type Str) then
				val = safeHTML(val)
			elif isType(val, type {}) and (mapHas(val, "__safe_html__") or mapHas(val, "__safe_url__")) then
				val = val
			else
				return error("render: |safe expects Str or already safe value")
			end
			i = i + 1
			continue
		end
		if not mapHas(e._filters, fname) then
			return error("render: unknown filter " + fname)
		end
		let f = e._filters[fname]
		let argv = [val]
		let j = 0
		while j < len(argList) do
			let vj = _evalExpr(e, scope, argList[j])
			if vj == null and noteGet(vj) != null then return vj end
			argv = argv + [vj]
			j = j + 1
		end
		let out = f
		let k = 0
		while k < len(argv) do
			out = out(argv[k])
			k = k + 1
		end
		val = out
		i = i + 1
	end

	val
end

# =========================
# Renderer (streaming with 1-NL holdback) + minimal context guards
# =========================

let _trimLeftNL = fun(bufs: Str) -> Str do
	let i = len(bufs) - 1
	while i >= 0 do
		let ch = substr(bufs, i, i + 1)
		if ch == " " or ch == "\t" then
			i = i - 1
			continue
		end
		if ch == "\n" then
			return substr(bufs, 0, i)
		end
		break
	end
	bufs
end

# Track <script>/<style> via scanning text chunks
let _scanTagState = fun(buf: {inScript: Bool, inStyle: Bool}, s: Str) -> Null do
	let low = toLower(s)
	if match("<script\\b", low) != null then buf.inScript = true end
	if match("</script>",  low) != null then buf.inScript = false end
	if match("<style\\b",  low) != null then buf.inStyle = true end
	if match("</style>",   low) != null then buf.inStyle = false end
	null
end

# Helper to flush a pending deferred newline.
let _flushPendingNL = fun(w: Writer, buf: {s: Str, trimNext: Bool, pendingNL: Bool}) -> Bool? do
	if buf.pendingNL then
		let n = w.write("\n")
		if n == null then return error("render: writer failed") end
		buf.s = buf.s + "\n"
		buf.pendingNL = false
	end
	true
end

# Write with: (1) optional left-trim for *next* chunk, (2) hold back a trailing '\n'
let _write = fun(w: Writer, buf: {s: Str, trimNext: Bool, pendingNL: Bool}, s: Str) -> Bool? do
	if buf.trimNext then
		s = _trimLeadingWSNL(s)
		buf.trimNext = false
	end

	if buf.pendingNL and len(s) > 0 then
		let n1 = w.write("\n")
		if n1 == null then return error("render: writer failed") end
		buf.s = buf.s + "\n"
		buf.pendingNL = false
	end

	if len(s) > 0 and substr(s, len(s)-1, len(s)) == "\n" then
		let body = substr(s, 0, len(s)-1)
		if len(body) > 0 then
			let n2 = w.write(body)
			if n2 == null then return error("render: writer failed") end
			buf.s = buf.s + body
		end
		buf.pendingNL = true
		return true
	end

	if len(s) > 0 then
		let n = w.write(s)
		if n == null then return error("render: writer failed") end
		buf.s = buf.s + s
	end
	true
end

let _renderSeq = fun(e: Env, ast: [Any], scope: {}, w: Writer, buf: {s: Str, trimNext: Bool, pendingNL: Bool, inScript: Bool, inStyle: Bool}, curName: Str) -> Bool? do
	let autoesc = bool(e.opts.autoescape)
	let i = 0
	while i < len(ast) do
		let n = ast[i]

		if n.tag == "text" then
			let s = n.a.s
			if n.a.tl and buf.pendingNL then
				buf.pendingNL = false
			end
			if n.a.tr then
				buf.trimNext = true
			end
			_scanTagState(buf, s)
			let ok = _write(w, buf, s)
			if ok == null then return ok end

		elif n.tag == "expr" then
			if mapHas(n.a, "tl") and n.a.tl and buf.pendingNL then
				buf.pendingNL = false
			end

			# Context guards (fail closed)
			if buf.inScript then
				return error("render: interpolation inside <script> is not allowed")
			end
			if buf.inStyle then
				return error("render: interpolation inside <style> is not allowed")
			end

			let v = _evalExpr(e, scope, n.a.code)
			if v == null and noteGet(v) != null then return v end

			# Attribute-level restrictions if lexer hinted attr context
			if mapHas(n.a, "ctx") and n.a.ctx != null and n.a.ctx.kind == "attr" then
				let an = n.a.ctx.attrName
				if an != null then
					if substr(an, 0, 2) == "on" then
						return error("render: event handler attributes disallow interpolation")
					end
					if an == "href" or an == "src" then
						if not (isType(v, type {}) and mapHas(v, "__safe_url__")) then
							return error("render: href/src require SafeURL")
						end
					end
				end
			end

			let out = ""
			if v == null then
				let src = strip(n.a.code)
				let hasPipe  = len(match("\\|", src)) > 0
				let hasCall  = len(match("\\(", src)) > 0
				let hasQuote = len(match("[\"']", src)) > 0
				let hasSpace = len(match("\\s", src)) > 0
				let badChar  = len(match("[^A-Za-z0-9_\\.]", src)) > 0
				let isBare   = (not hasPipe) and (not hasCall) and (not hasQuote) and (not hasSpace) and (not badChar) and len(src) > 0
				if e.opts.strictUndefined and isBare then
					return error("render: missing name " + src)
				end
				out = ""
			elif isType(v, type {}) and mapHas(v, "__safe_url__") then
				out = v.url
			elif isType(v, type {}) and mapHas(v, "__safe_html__") then
				out = v.html
			else
				let s2 = if isType(v, type Str) then v else str(v) end
				if s2 == null then return error("render: stringify failed") end
				out = if autoesc then _htmlEscape(s2) else s2 end
			end

			if mapHas(n.a, "tr") and n.a.tr then
				buf.trimNext = true
			end
			let ok2 = _write(w, buf, out)
			if ok2 == null then return ok2 end

		elif n.tag == "if" then
			if n.d != null and n.d.trOpen then
				buf.trimNext = true
			end

			let done = false
			let bi = 0
			while bi < len(n.a) do
				let pair = n.a[bi]
				let cond = _evalExpr(e, scope, pair[0])
				if cond == null and noteGet(cond) != null then return cond end
				if cond == null then return error("render: bad if cond") end
				let b = bool(cond)
				if b == null then return error("render: non-bool if cond") end
				if b then
					let ok = _renderSeq(e, pair[1], scope, w, buf, curName)
					if ok == null then return ok end
					done = true
					break
				end
				bi = bi + 1
			end
			if not done and n.b != null then
				let ok2 = _renderSeq(e, n.b, scope, w, buf, curName)
				if ok2 == null then return ok2 end
			end

			if n.d != null and n.d.tlEnd then
				if buf.pendingNL then buf.pendingNL = false end
			end
			if n.d != null and n.d.trEnd then
				buf.trimNext = true
			end

		elif n.tag == "for" then
			if n.d != null and n.d.trOpen then
				buf.trimNext = true
			end

			let it = _evalExpr(e, scope, n.b)
			if it == null and noteGet(it) != null then return it end

			if it == null then
				null
			elif isType(it, type [Any]) then
				let L = len(it)
				let idx = 0
				while idx < L do
					let loop = { index0: idx, index: idx + 1, first: idx == 0, last: idx == L - 1, length: L }
					let sc = {}
					for [k, v] in scope do sc[k] = v end
					sc[n.a] = it[idx]
					sc["loop"] = loop
					let ok = _renderSeq(e, n.c, sc, w, buf, curName)
					if ok == null then return ok end
					idx = idx + 1
				end
			elif isType(it, type {}) then
				let keysArr = []
				for [k, _] in it do keysArr = keysArr + [k] end
				let L2 = len(keysArr)
				let j = 0
				let pat = strip(n.a)
				let dk = ""
				let dv = ""
				if len(pat) > 0 and substr(pat, 0, 1) == "[" and substr(pat, len(pat) - 1, len(pat)) == "]" then
					let inner = strip(substr(pat, 1, len(pat) - 1))
					let parts = _splitTopLevel(inner, ",")
					if len(parts) == 2 then
						dk = strip(parts[0])
						dv = strip(parts[1])
					end
				end
				while j < L2 do
					let k = keysArr[j]
					let loop2 = { index0: j, index: j + 1, first: j == 0, last: j == L2 - 1, length: L2 }
					let sc2 = {}
					for [kk, vv] in scope do sc2[kk] = vv end
					if dk != "" and dv != "" then
						sc2[dk] = k
						sc2[dv] = it[k]
					else
						sc2[n.a] = [k, it[k]]
					end
					sc2["loop"] = loop2
					let ok3 = _renderSeq(e, n.c, sc2, w, buf, curName)
					if ok3 == null then return ok3 end
					j = j + 1
				end
			else
				null
			end

			if n.d != null and n.d.tlEnd then
				if buf.pendingNL then buf.pendingNL = false end
			end
			if n.d != null and n.d.trEnd then
				buf.trimNext = true
			end

		elif n.tag == "include" then
			if n.d != null and n.d.trOpen then
				buf.trimNext = true
			end

			let pathExpr = n.a
			let pth = _parseStringLit(pathExpr)
			let pathVal = if pth != null then pth else _evalExpr(e, scope, pathExpr) end
			if pathVal == null and noteGet(pathVal) != null then return pathVal end
			if pathVal == null then return error("render: include path") end

			let full = _resolvePath(e.opts.baseDir, curName, str(pathVal))
			if full == null then return full end
			let src = readFile(full)
			if src == null then return error("load: include failed") end
			let childT = fromString(e, src, _relName(e.opts.baseDir, full))
			if childT == null then return childT end

			let ctx = scope
			if n.b != null then
				let evalRes = _evalExpr(e, scope, n.b)
				let cx = evalRes
				let s = strip(n.b)
				let braceLiteral = len(s) >= 2 and substr(s, 0, 1) == "{" and substr(s, len(s) - 1, len(s)) == "}"
				if braceLiteral and (cx == null or noteGet(cx) != null) then
					let inner = strip(substr(s, 1, len(s) - 1))
					let pairs = _splitTopLevel(inner, ",")
					let m = {}
					let pi = 0
					let okMap = true
					while pi < len(pairs) do
						let kv = _splitTopLevel(pairs[pi], ":")
						if len(kv) != 2 then okMap = false
							break 
						end
						let kraw = strip(kv[0])
						let ks = _parseStringLit(kraw)
						let key = if ks != null then ks else kraw end
						let vexpr = strip(kv[1])
						let vv = _evalExpr(e, scope, vexpr)
						if vv == null and noteGet(vv) != null then okMap = false
							break 
						end
						if vv == null and vexpr != "null" then okMap = false
							break 
						end
						m[key] = vv
						pi = pi + 1
					end
					if okMap then cx = m end
				end
				if cx == null then
					if noteGet(evalRes) != null and not braceLiteral then return evalRes end
					return error("render: include ctx")
				end
				if isType(cx, type {}) then
					let merged = {}
					for [k, v] in scope do merged[k] = v end
					for [k2, v2] in cx do merged[k2] = v2 end
					ctx = merged
				else
					return error("render: include ctx must be map")
				end
			end

			let ok = _renderTemplate(e, childT, ctx, w)
			if ok == null then return ok end

		elif n.tag == "block" then
			if n.d != null and n.d.trOpen then
				buf.trimNext = true
			end
			let okb = _renderSeq(e, n.b, scope, w, buf, curName)
			if okb == null then return okb end
			if n.d != null and n.d.tlEnd then
				if buf.pendingNL then buf.pendingNL = false end
			end
			if n.d != null and n.d.trEnd then
				buf.trimNext = true
			end

		elif n.tag == "extends" then
			if n.d != null and n.d.trOpen then
				buf.trimNext = true
			end
			null

		else
			return error("render: unknown node")
		end

		i = i + 1
	end
	true
end

let _collectBlocks = fun(ast: [Any]) -> {} do
	let m = {}
	let i = 0
	while i < len(ast) do
		let n = ast[i]
		if n.tag == "block" then
			m[n.a] = n.b
		end
		i = i + 1
	end
	m
end

let _applyBlocks = fun(baseAst: [Any], slots: {}) -> [Any] do
	let out = []
	let i = 0
	while i < len(baseAst) do
		let n = baseAst[i]
		if n.tag == "block" then
			if mapHas(slots, n.a) then
				out = out + slots[n.a]
			else
				out = out + [_node("seq", n.b, null, null, null)]
			end
		else
			out = out + [n]
		end
		i = i + 1
	end
	out
end

let _renderTemplate = fun(e: Env, t: Template, data: {}, w: Writer) -> Bool? do
	let hasExt = false
	let parentPath = ""
	let i = 0
	while i < len(t.ast) do
		let n = t.ast[i]
		if n.tag == "extends" then
			hasExt = true
			let pth = _parseStringLit(n.a)
			let pv = if pth != null then pth else str(_evalExpr(e, data, n.a)) end
			if pv == null then return error("render: extends path") end
			parentPath = pv
			break
		end
		i = i + 1
	end
	if not hasExt then
		let buf = {s: "", trimNext: false, pendingNL: false, inScript: false, inStyle: false}
		let ok = _renderSeq(e, t.ast, data, w, buf, t.name)
		if ok == null then return ok end
		if buf.pendingNL then
			let n = w.write("\n")
			if n == null then return error("render: writer failed") end
			buf.pendingNL = false
		end
		true
	else
		let full = _resolvePath(e.opts.baseDir, t.name, parentPath)
		if full == null then return full end
		let src = readFile(full)
		if src == null then return error("load: extends failed") end
		let baseT = fromString(e, src, _relName(e.opts.baseDir, full))
		if baseT == null then return baseT end

		let slots = _collectBlocks(t.ast)
		let combined = _applyBlocks(baseT.ast, slots)
		let buf2 = {s: "", trimNext: false, pendingNL: false, inScript: false, inStyle: false}
		let ok2 = _renderSeq(e, combined, data, w, buf2, baseT.name)
		if ok2 == null then return ok2 end
		if buf2.pendingNL then
			let n2 = w.write("\n")
			if n2 == null then return error("render: writer failed") end
			buf2.pendingNL = false
		end
		true
	end
end

# =========================
# Compilation & I/O
# =========================

let fromString = fun(e: Env, src: Str, name: Str?) -> Template? do
	let toks = _lex(src)
	if toks == null then return toks end
	let ast = _parse(toks)
	if ast == null then return ast end
	{ __mtml__: true, name: if name == null then "<string>" else name end, ast: ast }
end

let load = fun(e: Env, path: Str) -> Template? do
	let info = e._loader(path)
	if info == null then return error("load: not found") end
	fromString(e, info.src, info.name)
end

let _stringWriter = fun() -> {w: Writer, buf: {s: Str}} do
	let state = {s: ""}
	let w = {
		write: fun(s: Str) -> Int? do
			state.s = state.s + s
			len(s)
		end,
		flush: fun(_: Null) -> Bool? do
			true
		end
	}
	{w: w, buf: state}
end

let render = fun(e: Env, t: Template, data: {}) -> Str? do
	if t == null or not isType(t, Template) then
		return error("render: not a template")
	end
	let pack = _stringWriter()
	let ok = _renderTemplate(e, t, data, pack.w)
	if ok == null then return ok end
	pack.buf.s
end

let renderTo = fun(e: Env, t: Template, w: Writer, data: {}) -> Int? do
	let counter = {n: 0}
	let ww = {
		write: fun(s: Str) -> Int? do
			let n = w.write(s)
			if n == null then return end
			counter.n = counter.n + int(n)
			n
		end,
		flush: fun(_: Null) -> Bool? do
			w.flush(null)
		end
	}
	let ok = _renderTemplate(e, t, data, ww)
	if ok == null then return ok end
	let _ = ww.flush(null)
	counter.n
end

# =========================
# Built-in filters (tiny but useful; unchanged)
# =========================

let _installBuiltins = fun(e: Env) -> Null do
	let upper = fun(s: Str) -> Str do toUpper(s) end
	let lower = fun(s: Str) -> Str do toLower(s) end

	let default = fun(x, fallback) do
		if x == null then fallback
		elif isType(x, type Str) and x == "" then fallback
		else x end
	end

	let joinF = fun(xs: [Any], sep: Str) -> Str do
		let ss = []
		let i = 0
		while i < len(xs) do
			let si = str(xs[i])
			if si == null then si = "" end
			ss = ss + [si]
			i = i + 1
		end
		join(ss, sep)
	end

	let _ = registerFilter(e, "upper", upper)
	let _ = registerFilter(e, "lower", lower)
	let _ = registerFilter(e, "default", default)
	let _ = registerFilter(e, "join", joinF)
	null
end

let installBuiltins = fun(e: Env) -> Bool do
	_installBuiltins(e)
	true
end
