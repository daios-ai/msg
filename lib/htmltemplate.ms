# html_template — Minimal, ergonomic HTML templating for MindScript
# 
# "Less is more": a compact engine with the essentials:
# - {{ expr | filter(arg) }}  (autoescape ON by default)
# - {% if %} / {% elif %} / {% else %} / {% end %}
# - {% for x in xs %} … {% end %}  (loop meta: index0, index, first, last, length)
# - {% include "partial.html" [with expr] %}
# - {% extends "base.html" %} + {% block name %} … {% end %} (single inheritance)
# - Whitespace trim: {%- … -%}, {{- … -}}
# 
# MVP notes (intentional constraints to keep the core tiny):
# - Expressions: identifiers, dotted access, string/number/bool/null, calls to registered helpers,
# filter pipelines (expr | filter(a, b))
# - Autoescape: HTML-text & attribute-safe character escaping (no JS/CSS contextuality yet).
# Use safeHTML()/|safe to bypass. Use safeURL() for href/src data;
# both wrap values with a safety tag and render unescaped.
# - Includes use Env.baseDir; extends supports a single parent chain.
# - strictUndefined defaults to true: unknown names are render errors.
# 
# Public surface (documented with python-style docstrings):
# env(), registerFilter(), registerHelper(), registerGlobal(),
# fromString(), load(), render(), renderTo(),
# safeHTML(), safeURL(), diagnostics()
# 
# Errors:
# Public functions return annotated null on failure with a brief machine POST,
# e.g. null # render: missing name foo
# 
# Internals avoid returning raw null on success.

let SafeHTML = type {__safe_html__: Bool, html: Str}
let SafeURL = type {__safe_url__: Bool, url: Str}

let RenderOpts = type {
	autoescape: Bool,
	baseDir: Str,
	bytecodeCache: {},
	strictUndefined: Bool
}

let Span = type {col: Int, line: Int}

let TemplateError = type {
	kind: Enum["parse", "load", "render", "type"],
	message: Str,
	where: Span?
}

let Template = type {__mtml__: Bool, ast: [Any], name: Str}

let Writer = type {flush: Null -> Bool?, write: Str -> Int?}

let Env = type {
	__mtml_env__: Bool,
	_filters: {},
	_globals: {},
	_helpers: {},
	_loader: Str -> {name: Str, src: Str}?,
	opts: RenderOpts
}

# =========================
# Utilities (escape, strings, errors)
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
	{
		kind: kind,
		message: msg,
		where: {line: line, col: col}
	}
end

let diagnostics = fun(e: TemplateError) -> Str do
	""
	"\n\tHuman-friendly formatting of a TemplateError.\n\n\tArgs:\n\t\te: TemplateError\n\n\tReturn:\n\t\tStr\n\t"
	""
	if e == null then
		return ""
	end
	if e.where == null then
		return sprintf("[%s] %s", [e.kind, e.message])
	end
	sprintf("[%s] %s at %d:%d", [
		e.kind,
		e.message,
		int(e.where.line),
		int(e.where.col)
	])
end

let _htmlEscape = fun(s: Str) -> Str do
	# minimal HTML/text + attribute escaping
	let t = replace("&", "&amp;", s)
	t = replace("<", "&lt;", t)
	t = replace(">", "&gt;", t)
	t = replace("\"", "&quot;", t)
	replace("'", "&#39;", t)
end

let safeHTML = fun(html: Str) -> SafeHTML do
	""
	"\n\tMark a string as trusted HTML (bypasses autoescape in HTML text contexts).\n\n\tArgs:\n\t\thtml: Str\n\n\tReturn:\n\t\tSafeHTML\n\t"
	""
	{__safe_html__: true, html: html}
end

let safeURL = fun(url: Str) -> SafeURL do
	""
	"\n\tMark a string as a trusted URL (for href/src contexts).\n\n\tArgs:\n\t\turl: Str\n\n\tReturn:\n\t\tSafeURL\n\t"
	""
	{__safe_url__: true, url: url}
end

# =========================
# Environment & registry
# =========================

let env = fun(opts: RenderOpts) -> Env do
	""
	"\n\tCreate a new templating environment.\n\n\tArgs:\n\t\topts: { baseDir?: Str, autoescape?: Bool=true, strictUndefined?: Bool=true, bytecodeCache?: {} }\n\n\tReturn:\n\t\tEnv\n\t"
	""
	let o = _defaultOpts(opts)
	let e = {
		__mtml_env__: true,
		opts: o,
		_filters: {},
		_helpers: {},
		_globals: {},
		_loader: fun(path: Str) -> {name: Str, src: Str}? do
			let full = if len(o.baseDir) == 0 then
				path
			else
				pathJoin([o.baseDir, path])
			end
			let s = readFile(full)
			if s == null then
				return null # load: unable to read file

			end
			{src: s, name: path}
		end
	}
	e
end

let registerFilter = fun(e: Env, name: Str, f) -> Bool do
	""
	"\n\tRegister a typed filter callable.\n\n\tArgs:\n\t\te: Env\n\t\tname: Str  # filter name used in templates\n\t\tf: Any     # callable; types enforced by caller\n\n\tReturn:\n\t\tBool\n\t"
	""
	e._filters[name] = f
	true
end

let registerHelper = fun(e: Env, name: Str, f) -> Bool do
	""
	"\n\tRegister a typed helper callable (usable inside expressions).\n\n\tArgs:\n\t\te: Env\n\t\tname: Str\n\t\tf: Any\n\n\tReturn:\n\t\tBool\n\t"
	""
	e._helpers[name] = f
	true
end

let registerGlobal = fun(e: Env, name: Str, value) -> Bool do
	""
	"\n\tExpose a global value in templates.\n\n\tArgs:\n\t\te: Env\n\t\tname: Str\n\t\tvalue: Any\n\n\tReturn:\n\t\tBool\n\t"
	""
	e._globals[name] = value
	true
end

# =========================
# Tokenizer ({{ }}, {% %}, with trim variants)
# =========================

# Token: { kind!: Enum["text","expr","stmt"], value!: Str, trimL!: Bool, trimR!: Bool, line!: Int, col!: Int }
let _lex = fun(src: Str) -> [Any]? do
	let tokens = []
	let i = 0
	let line = 1
	let col = 1

	let pushText = fun(s: Str, tl: Bool, tr: Bool, ln: Int, co: Int) -> Null do
		if len(s) == 0 then
			return
		end
		tokens = tokens + [
			{
				kind: "text",
				value: s,
				trimL: tl,
				trimR: tr,
				line: ln,
				col: co
			}
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
			let nxt = if j + 1 < len(src) then
				substr(src, j + 1, j + 2)
			else
				""
			end
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

		let close = if tag == "{{" or tag == "{{-" then
			"}}"
		else
			"%}"
		end
		let k = found + if tag == "{{-" or tag == "{%-" then
			3
		else
			2
		end
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
			return null # parse: unclosed tag

		end
		let inner = strip(substr(src, innerStart, closed))
		let endSpan = if trimR then
			closed + 3
		else
			closed + 2
		end
		let kind = if tag[1] == "{" and tag[2] == "{" then
			"expr"
		else
			"stmt"
		end
		tokens = tokens + [
			{
				kind: kind,
				value: inner,
				trimL: trimL,
				trimR: trimR,
				line: startLine,
				col: startCol
			}
		]

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
# expr(code, span)
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
						s: t.value,
						tl: t.trimL,
						tr: t.trimR,
						line: t.line,
						col: t.col
					}, null, null, null)
				]
				continue
			end
			if t.kind == "expr" then
				pos = pos + 1
				out = out + [
					_node("expr", {code: t.value, line: t.line, col: t.col}, null, null, null)
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
					return null # parse: unterminated if

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
							return null # parse: unterminated elif

						end
						branches = branches + [[cond2, b2]]
						continue
					end
					if p2[0] == "else" then
						pos = pos + 1
						let b3 = parseSeq(["end"])
						if b3 == null then
							return null # parse: unterminated else

						end
						out = out + [_node("if", branches, b3, null, null)]
						if pos < len(tokens) and tokens[pos].kind == "stmt" and tokens[pos].value == "end" then
							pos = pos + 1
							break
						end
						return null # parse: expected end after else

					end
					break
				end

				out = out + [_node("if", branches, null, null, null)]
				if pos < len(tokens) and tokens[pos].kind == "stmt" and tokens[pos].value == "end" then
					pos = pos + 1
				else
					return null # parse: expected end

				end
				continue
			end

			if head == "for" then
				pos = pos + 1
				let rest = strip(substr(body, 4, len(body)))
				let bits = split(rest, " in ")
				if len(bits) != 2 then
					return null # parse: for syntax

				end
				let name = strip(bits[0])
				let iter = strip(bits[1])
				let b = parseSeq(["end"])
				if b == null then
					return null # parse: unterminated for

				end
				out = out + [_node("for", name, iter, b, null)]
				if pos < len(tokens) and tokens[pos].kind == "stmt" and tokens[pos].value == "end" then
					pos = pos + 1
				else
					return null # parse: expected end

				end
				continue
			end

			if head == "include" then
				pos = pos + 1
				let rest = strip(substr(body, 8, len(body)))
				let withIdx = match("\\swith\\s", rest)
				let pth = rest
				let ctxExpr = null
				if len(withIdx) >= 2 then
					let m = withIdx[0]
					let s = int(m[0])
					let e = int(m[1])
					pth = strip(substr(rest, 0, s))
					ctxExpr = strip(substr(rest, e, len(rest)))
				end
				out = out + [_node("include", pth, ctxExpr, null, null)]
				continue
			end

			if head == "block" then
				pos = pos + 1
				let nm = strip(substr(body, 6, len(body)))
				let b = parseSeq(["end"])
				if b == null then
					return null # parse: unterminated block

				end
				out = out + [_node("block", nm, b, null, null)]
				if pos < len(tokens) and tokens[pos].kind == "stmt" and tokens[pos].value == "end" then
					pos = pos + 1
				else
					return null # parse: expected end

				end
				continue
			end

			if head == "extends" then
				pos = pos + 1
				let p = strip(substr(body, 8, len(body)))
				out = out + [_node("extends", p, null, null, null)]
				continue
			end

			if head == "end" then
				return out
			end

			return null # parse: unknown tag

		end
		out
	end

	let ast = parseSeq([])
	if ast == null then
		return null # parse: invalid structure

	end
	ast
end

# =========================
# Expression evaluator (very small)
# Supports:
# - literals: "str", 123, 1.2, true, false, null
# - names & dotted access: a, user.name, config.items
# - helper calls: helper(a, b)
# - filter pipelines: expr | filter(a, b) | other()
# - ternary sugar: A if cond else B
# =========================

let _parseStringLit = fun(s: Str) -> Str? do
	if len(s) < 2 then
		return
	end
	let q = substr(s, 0, 1)
	if q != "\"" and q != "'" then
		return
	end
	if substr(s, len(s) - 1, len(s)) != q then
		return
	end
	substr(s, 1, len(s) - 1)
end

let _numOrBoolOrNull = fun(s: Str) -> Any? do
	if s == "true" then
		return true
	end
	if s == "false" then
		return false
	end
	if s == "null" then
		return
	end
	let i = int(s)
	if i != null and str(i) == s then
		return i
	end
	let n = num(s)
	if n != null then
		return n
	end
	null
end

let _splitTopLevel = fun(s: Str, sep: Str) -> [Str] do
	let out = []
	let i = 0
	let last = 0
	let inQ = ""
	let depth = 0
	while i < len(s) do
		let ch = substr(s, i, i + 1)
		if inQ != "" then
			if ch == inQ then
				inQ = ""
			end
		else
			if ch == "\"" or ch == "'" then
				inQ = ch
			elif ch == "(" then
				depth = depth + 1
			elif ch == ")" and depth > 0 then
				depth = depth - 1
			else
				if depth == 0 and substr(s, i, i + len(sep)) == sep then
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
				return null # render: missing name

			end
		else
			if cur == null then
				return null # render: access null

			end
			if isType(cur, type {}) and mapHas(cur, p) then
				cur = cur[p]
			else
				return null # render: missing field

			end
		end
		j = j + 1
	end
	cur
end

let _argsParse = fun(s: Str) -> [Str] do
	if len(strip(s)) == 0 then
		return []
	end
	_splitTopLevel(s, ",")
end

let _evalExpr = fun(e: Env, scope: {}, code: Str) -> Any? do
	# ternary rewrite: A if cond else B
	let tern = match("\\sif\\s", code)
	if len(tern) >= 2 then
		let m = tern[0]
		let s = int(m[0])
		let epos = int(m[1])
		let A = strip(substr(code, 0, s))
		let rem = substr(code, epos, len(code))
		let els = match("\\selse\\s", rem)
		if len(els) >= 2 then
			let m2 = els[0]
			let s2 = int(m2[0])
			let e2 = int(m2[1])
			let cond = strip(substr(rem, 0, s2))
			let B = strip(substr(rem, e2, len(rem)))
			let cv = _evalExpr(e, scope, cond)
			if cv == null then
				return null # render: bad condition

			end
			let truth = bool(cv)
			if truth == null then
				return null # render: non-bool condition

			end
			return if truth then
				_evalExpr(e, scope, A)
			else
				_evalExpr(e, scope, B)
			end
		end
	end

	let pipes = _splitTopLevel(code, "|")
	if len(pipes) == 0 then
		return
	end

	let base = strip(pipes[0])
	let val = null

	let q = _parseStringLit(base)
	if q != null then
		val = q
	else
		let prim = _numOrBoolOrNull(base)
		if prim != null or base == "null" then
			val = prim
		else
			let openP = match("\\(", base)
			if len(openP) >= 2 then
				let p = int(openP[0][0])
				let nm = strip(substr(base, 0, p))
				let argsStr = strip(substr(base, p + 1, len(base) - 1))
				if not mapHas(e._helpers, nm) then
					return null # render: unknown helper

				end
				let argExprs = _argsParse(argsStr)
				let argv = []
				let i = 0
				while i < len(argExprs) do
					let v = _evalExpr(e, scope, argExprs[i])
					if v == null and noteGet(v) == null then
						if strip(argExprs[i]) != "null" then
							return null # render: bad arg

						end
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
			else
				let merged = {}
				for [k, v] in e._globals do
					merged[k] = v
				end
				for [k2, v2] in scope do
					merged[k2] = v2
				end
				let got = _evalNamePath(merged, base, e.opts.strictUndefined)
				if got == null then
					if e.opts.strictUndefined then
						return null # render: missing name

					end
				end
				val = got
			end
		end
	end

	let i = 1
	while i < len(pipes) do
		let spec = strip(pipes[i])
		if len(spec) == 0 then
			i = i + 1
			continue
		end
		let par = match("\\(", spec)
		let fname = ""
		let argList = []
		if len(par) >= 2 then
			let p = int(par[0][0])
			fname = strip(substr(spec, 0, p))
			let as = strip(substr(spec, p + 1, len(spec) - 1))
			argList = _argsParse(as)
		else
			fname = spec
		end
		if fname == "safe" then
			if not isType(val, SafeHTML) and not isType(val, SafeURL) then
				val = safeHTML(str(val))
			end
			i = i + 1
			continue
		end
		if not mapHas(e._filters, fname) then
			return null # render: unknown filter

		end
		let f = e._filters[fname]
		let argv = [val]
		let j = 0
		while j < len(argList) do
			let vj = _evalExpr(e, scope, argList[j])
			if vj == null and strip(argList[j]) != "null" then
				return null # render: bad filter arg

			end
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
# Renderer
# =========================

let _trimLeftNL = fun(buf: Str) -> Str do
	let i = len(buf) - 1
	while i >= 0 do
		let ch = substr(buf, i, i + 1)
		if ch == " " or ch == "\t" then
			i = i - 1
			continue
		end
		if ch == "\n" then
			return substr(buf, 0, i)
		end
		break
	end
	buf
end

let _write = fun(w: Writer, s: Str) -> Bool do
	let n = w.write(s)
	if n == null then
		return false
	end
	true
end

let _renderSeq = fun(e: Env, ast: [Any], scope: {}, w: Writer, buf: {s: Str}) -> Bool? do
	let autoesc = bool(e.opts.autoescape)
	let i = 0
	while i < len(ast) do
		let n = ast[i]
		if n.tag == "text" then
			let s = n.a.s
			if n.a.tl then
				if buf != null then
					buf.s = _trimLeftNL(buf.s)
				end
			end
			if not _write(w, s) then
				return
			end
		elif n.tag == "expr" then
			let v = _evalExpr(e, scope, n.a.code)
			if v == null and strip(n.a.code) != "null" then
				return null # render: bad expression

			end
			let out = ""
			if v == null then
				out = ""
			elif isType(v, SafeHTML) then
				out = v.html
			elif isType(v, SafeURL) then
				out = v.url
			else
				let s2 = str(v)
				if s2 == null then
					return null # render: stringify failed

				end
				out = if autoesc then
					_htmlEscape(s2)
				else
					s2
				end
			end
			if not _write(w, out) then
				return
			end
		elif n.tag == "if" then
			let done = false
			let bi = 0
			while bi < len(n.a) do
				let pair = n.a[bi]
				let cond = _evalExpr(e, scope, pair[0])
				if cond == null then
					return null # render: bad if cond

				end
				let b = bool(cond)
				if b == null then
					return null # render: non-bool if cond

				end
				if b then
					let ok = _renderSeq(e, pair[1], scope, w, buf)
					if ok == null then
						return
					end
					done = true
					break
				end
				bi = bi + 1
			end
			if not done and n.b != null then
				let ok2 = _renderSeq(e, n.b, scope, w, buf)
				if ok2 == null then
					return
				end
			end
		elif n.tag == "for" then
			let it = _evalExpr(e, scope, n.b)
			if it == null then
				null # iterate nothing

			elif isType(it, type [Any]) then
				let L = len(it)
				let idx = 0
				while idx < L do
					let loop = {
						index0: idx,
						index: idx + 1,
						first: idx == 0,
						last: idx == L - 1,
						length: L
					}
					let sc = {}
					for [k, v] in scope do
						sc[k] = v
					end
					sc[n.a] = it[idx]
					sc["loop"] = loop
					let ok = _renderSeq(e, n.c, sc, w, buf)
					if ok == null then
						return
					end
					idx = idx + 1
				end
			elif isType(it, type {}) then
				let keysArr = []
				for [k, _] in it do
					keysArr = keysArr + [k]
				end
				let L2 = len(keysArr)
				let j = 0
				while j < L2 do
					let k = keysArr[j]
					let loop2 = {
						index0: j,
						index: j + 1,
						first: j == 0,
						last: j == L2 - 1,
						length: L2
					}
					let sc2 = {}
					for [kk, vv] in scope do
						sc2[kk] = vv
					end
					sc2[n.a] = {"0": k, "1": it[k]}
					sc2["loop"] = loop2
					let ok3 = _renderSeq(e, n.c, sc2, w, buf)
					if ok3 == null then
						return
					end
					j = j + 1
				end
			else
				null # not iterable: skip

			end
		elif n.tag == "include" then
			let pathExpr = n.a
			let pth = _parseStringLit(pathExpr)
			let pathVal = if pth != null then
				pth
			else
				_evalExpr(e, scope, pathExpr)
			end
			if pathVal == null then
				return null # render: include path

			end
			let info = e._loader(str(pathVal))
			if info == null then
				return null # load: include failed

			end
			let childT = fromString(e, info.src, info.name)
			if childT == null then
				return null # parse: include

			end
			let ctx = scope
			if n.b != null then
				let cx = _evalExpr(e, scope, n.b)
				if cx == null then
					return null # render: include ctx

				end
				if isType(cx, type {}) then
					let merged = {}
					for [k, v] in scope do
						merged[k] = v
					end
					for [k2, v2] in cx do
						merged[k2] = v2
					end
					ctx = merged
				else
					return null # render: include ctx must be map

				end
			end
			let ok = _renderTemplate(e, childT, ctx, w)
			if ok == null then
				return
			end
		elif n.tag == "block" then
			let okb = _renderSeq(e, n.b, scope, w, buf)
			if okb == null then
				return
			end
		elif n.tag == "extends" then
			null # handled in _renderTemplate

		else
			return null # render: unknown node

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
			let pv = if pth != null then
				pth
			else
				str(_evalExpr(e, data, n.a))
			end
			if pv == null then
				return null # render: extends path

			end
			parentPath = pv
			break
		end
		i = i + 1
	end
	if not hasExt then
		let buf = {s: ""}
		return _renderSeq(e, t.ast, data, w, buf)
	end

	let info = e._loader(parentPath)
	if info == null then
		return null # load: extends failed

	end
	let baseT = fromString(e, info.src, info.name)
	if baseT == null then
		return null # parse: extends

	end

	let slots = _collectBlocks(t.ast)
	let combined = _applyBlocks(baseT.ast, slots)
	let buf2 = {s: ""}
	_renderSeq(e, combined, data, w, buf2)
end

# =========================
# Compilation & I/O
# =========================

let fromString = fun(e: Env, src: Str, name: Str?) -> Template? do
	""
	"\n\tCompile a template from a string.\n\n\tArgs:\n\t\te: Env\n\t\tsrc: Str\n\t\tname: Str?  # display name for diagnostics\n\n\tReturn:\n\t\tTemplate?  # annotated null on parse error\n\t"
	""
	let toks = _lex(src)
	if toks == null then
		return null # parse: tokenize failed

	end
	let ast = _parse(toks)
	if ast == null then
		return null # parse: build failed

	end
	{
		__mtml__: true,
		name: if name == null then
			"<string>"
		else
			name
		end,
		ast: ast
	}
end

let load = fun(e: Env, path: Str) -> Template? do
	""
	"\n\tLoad and compile a template from Env.baseDir/path.\n\n\tArgs:\n\t\te: Env\n\t\tpath: Str\n\n\tReturn:\n\t\tTemplate?\n\t"
	""
	let info = e._loader(path)
	if info == null then
		return null # load: not found

	end
	fromString(e, info.src, info.name)
end

let _stringWriter = fun() -> {buf: {s: Str}, w: Writer} do
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
	""
	"\n\tRender into a string.\n\n\tArgs:\n\t\te: Env\n\t\tt: Template\n\t\tdata: {}\n\n\tReturn:\n\t\tStr?  # annotated null on error\n\t"
	""
	if t == null or not isType(t, Template) then
		return null # render: not a template

	end
	let pack = _stringWriter()
	let ok = _renderTemplate(e, t, data, pack.w)
	if ok == null then
		return null # render: failed

	end
	pack.buf.s
end

let renderTo = fun(e: Env, t: Template, w: Writer, data: {}) -> Int? do
	""
	"\n\tRender to a streaming Writer.\n\n\tArgs:\n\t\te: Env\n\t\tt: Template\n\t\tw: Writer\n\t\tdata: {}\n\n\tReturn:\n\t\tInt?  # total bytes written, null on error\n\t"
	""
	let counter = {n: 0}
	let ww = {
		write: fun(s: Str) -> Int? do
			let n = w.write(s)
			if n == null then
				return
			end
			counter.n = counter.n + int(n)
			n
		end,
		flush: fun(_: Null) -> Bool? do
			w.flush(null)
		end
	}
	let ok = _renderTemplate(e, t, data, ww)
	if ok == null then
		return null # render: failed

	end
	counter.n
end

# =========================
# Built-in filters (tiny but useful)
# =========================

let _installBuiltins = fun(e: Env) -> Null do
	let upper = fun(s: Str) -> Str do
		toUpper(s)
	end
	let lower = fun(s: Str) -> Str do
		toLower(s)
	end

	let default = fun(x, fallback) do
		if x == null then
			fallback
		elif isType(x, type Str) and x == "" then
			fallback
		else
			x
		end
	end

	let joinF = fun(xs: [Any], sep: Str) -> Str do
		let ss = []
		let i = 0
		while i < len(xs) do
			let si = str(xs[i])
			if si == null then
				si = ""
			end
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
	""
	"\n\tInstall a minimal set of built-in filters: upper, lower, default, join.\n\n\tArgs:\n\t\te: Env\n\n\tReturn:\n\t\tBool\n\t"
	""
	_installBuiltins(e)
	true
end