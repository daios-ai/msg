# mtml — Minimal, context-aware HTML templating for MindScript
# Surface: {{ expr | filter(a,b) }}, {% if/elif/else %}, {% for x in xs %},
# {% include "p" [with expr] %}, {% extends "b" %}+{% block n %}...{% end %}, trims {%- -%}, {{- -}}
# Policies: no unquoted attribute interpolation, no |safe in templates
# URL attributes: plain strings allowed (HTML-escaped); SafeURL passes verbatim
# style/event attrs forbidden
# <script> requires final | tojson

# ---------------- Types & constructors ----------------

let SafeHTML = type {__safe_html__!: Bool, __brand__!: Str, html!: Str}
let SafeURL  = type {__safe_url__!:  Bool, __brand__!: Str, url!:  Str}

# module-private brand (unforgeable token)
# Generate a per-process random brand used to authenticate Safe* wrappers.
#
# Args:
# 	_: Null
#
# Return:
# 	Str  # random brand token
let _BRAND = fun(_: Null) -> Str do
	let b = randBytes(16)
	if b != null then
		return b
	end
	str(nowNanos())
end()

# Wrap a raw HTML string as SafeHTML (host-only, unforgeable).
#
# Args:
# 	s: Str  # trusted HTML string
#
# Return:
# 	SafeHTML
let safeHTML = fun(s: Str) -> SafeHTML do
	{ __safe_html__: true, __brand__: _BRAND, html: s }
end

# Check whether a URL string is allowed by policy (scheme/path allow-list).
#
# Args:
# 	u: Str
#
# Return:
# 	Bool
let _urlAllowed = fun(u: Str) -> Bool do
	if u == null then
		return false
	end
	let s = strip(toLower(u))
	if len(match("[\\r\\n\\x00\\\\]", s)) > 0 then
		return false
	end
	if substr(s, 0, 2) == "//" then
		return true
	end
	if substr(s, 0, 1) == "/" then
		return true
	end
	if substr(s, 0, 2) == "./" then
		return true
	end
	if substr(s, 0, 3) == "../" then
		return true
	end
	if substr(s, 0, 8) == "https://" then
		return true
	end
	if substr(s, 0, 7) == "http://" then
		return true
	end
	if substr(s, 0, 4) == "tel:" then
		return true
	end
	false
end

# Wrap a URL string as SafeURL if it passes policy; otherwise annotated null.
#
# Args:
# 	u: Str
#
# Return:
# 	SafeURL?  # null # <render: disallowed url scheme> when not allowed
let safeURL = fun(u: Str) -> SafeURL? do
	if not _urlAllowed(u) then
		return error("render: disallowed url scheme")
	end
	{ __safe_url__: true, __brand__: _BRAND, url: u }
end

let RenderOpts = type { baseDir: Str, autoescape: Bool, strictUndefined: Bool, bytecodeCache: {} }
let Span = type {line: Int, col: Int}
let TemplateError = type {kind: Enum["parse","load","render","type"], message: Str, where: Span?}
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

# ---------------- Utilities ----------------

# Merge user options with defaults, preferring user keys.
#
# Args:
# 	o: RenderOpts
#
# Return:
# 	RenderOpts
let _defaultOpts = fun(o: RenderOpts) -> RenderOpts do
	let out = { baseDir: "", autoescape: true, strictUndefined: true, bytecodeCache: {} }
	if o != null then
		for [k, v] in o do
			out[k] = v
		end
	end
	out
end

# Construct a TemplateError record (helper).
#
# Args:
# 	kind: Str
# 	msg: Str
# 	line: Int
# 	col: Int
#
# Return:
# 	TemplateError
let _err = fun(kind: Str, msg: Str, line: Int, col: Int) -> TemplateError do
	{ kind: kind, message: msg, where: {line: line, col: col} }
end

# Render a human-readable diagnostic string for a TemplateError.
#
# Args:
# 	e: TemplateError
#
# Return:
# 	Str
let diagnostics = fun(e: TemplateError) -> Str do
	if e == null then
		return ""
	end
	if e.where == null then
		return sprintf("[%s] %s", [e.kind, e.message])
	end
	sprintf("[%s] %s at %d:%d", [e.kind, e.message, int(e.where.line), int(e.where.col)])
end

# Produce an annotated null for a failure at a source location.
#
# Args:
# 	kind: Str
# 	msg: Str
# 	at: {}  # expects {line:Int?, col:Int?}
#
# Return:
# 	Null  # annotated with "[kind] msg at line:col"
let _fail = fun(kind: Str, msg: Str, at: {}) -> Null do
	let ln = if at != null and mapHas(at, "line") then int(at.line) else 0 end
	let co = if at != null and mapHas(at, "col")  then int(at.col)  else 0 end
	error(diagnostics(_err(kind, msg, ln, co)))
end

# HTML-escape a string (&, <, >, ", ').
#
# Args:
# 	s: Str
#
# Return:
# 	Str
let _htmlEscape = fun(s: Str) -> Str do
	if len(match("[&<>\"']", s)) == 0 then
		return s
	end
	let t  = replace("&",  "&amp;",  s)
	let t2 = replace("<",  "&lt;",   t)
	let t3 = replace(">",  "&gt;",   t2)
	let t4 = replace("\"", "&quot;", t3)
	replace("'", "&#39;", t4)
end

# Check SafeHTML brand authenticity.
#
# Args:
# 	v: Any
#
# Return:
# 	Bool
let _isSafeHTML = fun(v: Any) -> Bool do
	isType(v, type {}) and mapHas(v, "__safe_html__") and mapHas(v, "__brand__") and v.__brand__ == _BRAND
end

# Check SafeURL brand authenticity.
#
# Args:
# 	v: Any
#
# Return:
# 	Bool
let _isSafeURL = fun(v: Any) -> Bool do
	isType(v, type {}) and mapHas(v, "__safe_url__") and mapHas(v, "__brand__") and v.__brand__ == _BRAND
end

# Trim leading indentation and a single following newline (for {% -%} trims).
#
# Args:
# 	s: Str
#
# Return:
# 	Str
let _trimLeadingWSNL = fun(s: Str) -> Str do
	let i = 0
	while i < len(s) do
		let ch = substr(s, i, i + 1)
		if ch == " " or ch == "\t" then
			i = i + 1
		else
			break
		end
	end
	if i < len(s) and substr(s, i, i + 1) == "\n" then
		return substr(s, i + 1, len(s))
	end
	substr(s, i, len(s))
end

# Trim trailing spaces/tabs (not newline) from the right.
#
# Args:
# 	s: Str
#
# Return:
# 	Str
let _trimRightWS = fun(s: Str) -> Str do
	let i = len(s) - 1
	while i >= 0 do
		let ch = substr(s, i, i + 1)
		if ch == " " or ch == "\t" then
			i = i - 1
		else
			break
		end
	end
	substr(s, 0, i + 1)
end

# Resolve a template-relative path within baseDir jail.
#
# Args:
# 	baseDir: Str
# 	fromName: Str  # current template logical name
# 	req: Str       # requested include/extends path
#
# Return:
# 	Str?  # absolute normalized path, or null # <load: path escapes baseDir>
let _resolvePath = fun(baseDir: Str, fromName: Str, req: Str) -> Str? do
	let normalized = replace("\\\\", "/", req)
	let base = if len(fromName) == 0 then baseDir else pathDir(pathJoin([baseDir, fromName])) end
	let clean = pathClean(pathJoin([base, normalized]))
	let bd = pathClean(baseDir)
	if len(bd) > 0 and substr(clean, 0, len(bd)) != bd then
		return error("load: path escapes baseDir")
	end
	clean
end

# Convert an absolute path back to a jailed logical name.
#
# Args:
# 	baseDir: Str
# 	full: Str
#
# Return:
# 	Str
let _relName = fun(baseDir: Str, full: Str) -> Str do
	let bd = pathClean(baseDir)
	if len(bd) == 0 then
		return full
	end
	if substr(full, 0, len(bd)) == bd and len(full) > len(bd) and substr(full, len(bd), len(bd) + 1) == "/" then
		return substr(full, len(bd) + 1, len(full))
	end
	full
end

# ---------------- Environment ----------------

# Create a rendering environment with defaults and a jailed loader.
#
# Args:
# 	opts: RenderOpts  # baseDir, autoescape, strictUndefined, bytecodeCache (unused)
#
# Return:
# 	Env
let env = fun(opts: RenderOpts) -> Env do
	let o = _defaultOpts(opts)
	{
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
end

# Register a filter (curried function of Any arity).
#
# Args:
# 	e: Env
# 	name: Str
# 	f: Any  # curried function
#
# Return:
# 	Bool
let registerFilter = fun(e: Env, name: Str, f: Any) -> Bool do
	e._filters[name] = f
	true
end

# Register a helper (callable from expressions like helper(a,b)).
#
# Args:
# 	e: Env
# 	name: Str
# 	f: Any  # curried function
#
# Return:
# 	Bool
let registerHelper = fun(e: Env, name: Str, f: Any) -> Bool do
	e._helpers[name] = f
	true
end

# Register a global variable visible to templates.
#
# Args:
# 	e: Env
# 	name: Str
# 	value: Any
#
# Return:
# 	Bool
let registerGlobal = fun(e: Env, name: Str, value: Any) -> Bool do
	e._globals[name] = value
	true
end

# ---------------- Tokenizer helpers ----------------

# Whether an attribute name is treated as a URL context.
#
# Args:
# 	n: Str
#
# Return:
# 	Bool
let _isURLAttr = fun(n: Str) -> Bool do
	let x = toLower(n)
	if x == "href" then return true end
	if x == "src" then return true end
	if x == "action" then return true end
	if x == "formaction" then return true end
	if x == "poster" then return true end
	if x == "xlink:href" then return true end
	if x == "data-src" then return true end
	if x == "srcset" then return true end
	if x == "imagesrcset" then return true end
	false
end

# Whether an attribute name is an inline event handler (on*).
#
# Args:
# 	n: Str
#
# Return:
# 	Bool
let _isEventAttr = fun(n: Str) -> Bool do
	len(n) >= 2 and toLower(substr(n, 0, 2)) == "on"
end

# Classify a quote character.
#
# Args:
# 	ch: Str  # one-character string
#
# Return:
# 	Str  # "double" | "single" | "unquoted"
let _quoteOf = fun(ch: Str) -> Str do
	if ch == "\"" then
		"double"
	elif ch == "'" then
		"single"
	else
		"unquoted"
	end
end

# Case-insensitive literal check at offset i.
#
# Args:
# 	s: Str
# 	i: Int
# 	lit: Str
#
# Return:
# 	Bool
let _hasInsensitiveAt = fun(s: Str, i: Int, lit: Str) -> Bool do
	if i < 0 then
		return false
	end
	let n = len(lit)
	if i + n > len(s) then
		return false
	end
	let seg = toLower(substr(s, i, i + n))
	toLower(lit) == seg
end

# ---------------- Tokenizer (HTML-aware, minimal) ----------------
# Emits [{kind:"text"|"expr"|"stmt", value, trimL, trimR, line, col, ctx?, quote?, attrName?}]

# Lex a template into text/expr/stmt tokens with HTML context awareness.
#
# Args:
# 	src: Str
#
# Return:
# 	[Any]?  # null # <parse: ...> on failure (e.g., unclosed tag)
let _lex = fun(src: Str) -> [Any]? do
	let out = []
	let i = 0
	let line = 1
	let col = 1
	let trimNext = false

	let ctx = "HtmlText"
	let inTag = false
	let curAttr = ""
	let inAttr = false
	let q = ""
	let pendingCtx = ""

	let pushText = fun(s: Str, ln: Int, co: Int, tl: Bool, tr: Bool) -> Null do
		if len(s) == 0 then
			return
		end
		let vv = if trimNext then _trimLeadingWSNL(s) else s end
		trimNext = false
		out = out + [{kind: "text", value: vv, trimL: tl, trimR: tr, line: ln, col: co}]
		null
	end

	let bump = fun(ch: Str) -> Null do
		if ch == "\n" then
			line = line + 1
			col = 1
		else
			col = col + 1
		end
		null
	end

	while i < len(src) do
		let startL = line
		let startC = col

		let j = i
		let tag = ""
		let found = -1
		while j < len(src) do
			let a = substr(src, j, j + 1)
			let b = ""
			if j + 1 < len(src) then
				b = substr(src, j + 1, j + 2)
			end

			if a == "<" then
				inTag = true
				curAttr = ""
				inAttr = false
				q = ""
			elif a == ">" then
				inTag = false
				curAttr = ""
				inAttr = false
				q = ""
				if pendingCtx != "" then
					ctx = pendingCtx
					pendingCtx = ""
				end
			elif inTag then
				if a == "=" then
					inAttr = true
				elif a == "\"" or a == "'" then
					if inAttr and q == "" then
						q = a
					elif inAttr and q == a then
						q = ""
					end
				elif a == " " or a == "\t" or a == "\n" then
					if q == "" then
						inAttr = false
						curAttr = ""
					end
				else
					if not inAttr and q == "" then
						if curAttr == "" then
							curAttr = a
						else
							curAttr = curAttr + a
						end
					end
				end
			end

			if a == "<" and _hasInsensitiveAt(src, j, "<script") then
				pendingCtx = "RawJS"
			end
			if _hasInsensitiveAt(src, j - 2, "</script>") then
				ctx = "HtmlText"
			end
			if a == "<" and _hasInsensitiveAt(src, j, "<style") then
				pendingCtx = "RawCSS"
			end
			if _hasInsensitiveAt(src, j - 1, "</style>") then
				ctx = "HtmlText"
			end

			if a == "{" and (b == "{" or b == "%") then
				found = j
				tag = a + b
				break
			end
			bump(a)
			j = j + 1
		end

		if found == -1 then
			pushText(substr(src, i, len(src)), startL, startC, false, false)
			break
		end

		pushText(substr(src, i, found), startL, startC, false, false)

		let trimL = false
		if found + 2 < len(src) and substr(src, found + 2, found + 3) == "-" then
			trimL = true
			tag = tag + "-"
		end

		let close = if substr(tag, 0, 2) == "{{" then "}}" else "%}" end
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
			return _fail("parse", "unclosed tag", {line: startL, col: startC})
		end
		let inner = strip(substr(src, innerStart, closed))
		let endSpan = if trimR then closed + 3 else closed + 2 end
		let kind = if substr(tag, 0, 2) == "{{" then "expr" else "stmt" end

		let tokCtx = null
		if kind == "expr" then
			if ctx == "RawJS" then
				tokCtx = "RawJS"
			elif ctx == "RawCSS" then
				tokCtx = "RawCSS"
			elif inTag and inAttr then
				let an = toLower(strip(curAttr))
				if _isEventAttr(an) then
					tokCtx = "EventAttr"
				elif an == "style" then
					tokCtx = "StyleAttr"
				elif _isURLAttr(an) then
					tokCtx = "URLAttr"
				else
					tokCtx = "GenericAttr"
				end
			else
				tokCtx = "HtmlText"
			end
		end

		let qk = ""
		if kind == "expr" and inTag and inAttr then
			qk = _quoteOf(q)
		end

		out = out + [{
			kind: kind,
			value: inner,
			trimL: trimL,
			trimR: trimR,
			line: startL,
			col: startC,
			ctx: tokCtx,
			quote: qk,
			attrName: curAttr
		}]
		if trimR then
			trimNext = true
		end

		let chunk = substr(src, found, endSpan)
		let p = 0
		while p < len(chunk) do
			bump(substr(chunk, p, p + 1))
			p = p + 1
		end
		i = endSpan
	end

	out
end

# ---------------- Parser (AST) ----------------

# Construct a tiny node record with optional payloads.
#
# Args:
# 	tag: Str
# 	a: Any
# 	b: Any
# 	c: Any
# 	d: Any
#
# Return:
# 	{}
let _node = fun(tag: Str, a: Any, b: Any, c: Any, d: Any) -> {} do
	{tag: tag, a: a, b: b, c: c, d: d}
end

# Split a string by a separator at top level (respects quotes/brackets/parens).
#
# Args:
# 	s: Str
# 	sep: Str
#
# Return:
# 	[Str]
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

# Parse tokens into an AST (sequence of nodes). Returns annotated null on errors.
#
# Args:
# 	tokens: [Any]
#
# Return:
# 	[Any]?  # null # <parse: ...> on structural errors
let _parse = fun(tokens: [Any]) -> [Any]? do
	let pos = 0

	let parseSeq = fun(stopTags: [Str]) -> [Any]? do
		let out = []
		while pos < len(tokens) do
			let t = tokens[pos]
			if t.kind == "text" then
				pos = pos + 1
				out = out + [
					_node("text", {s: t.value, tl: t.trimL, tr: t.trimR, line: t.line, col: t.col}, null, null, null)
				]
				continue
			end
			if t.kind == "expr" then
				pos = pos + 1
				out = out + [
					_node("expr", {code: t.value, tl: t.trimL, tr: t.trimR, line: t.line, col: t.col, ctx: t.ctx, quote: t.quote, attrName: t.attrName}, null, null, null)
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
				let thenBody = parseSeq(["elif","else","end"])
				if thenBody == null then
					return _fail("parse", "unterminated if", t)
				end
				branches = branches + [[cond, thenBody]]

				while pos < len(tokens) and tokens[pos].kind == "stmt" do
					let st = tokens[pos].value
					let p2 = split(st, " ")
					if p2[0] == "elif" then
						let tElif = tokens[pos]
						pos = pos + 1
						let cond2 = strip(substr(st, 5, len(st)))
						let b2 = parseSeq(["elif","else","end"])
						if b2 == null then
							return _fail("parse", "unterminated elif", tElif)
						end
						branches = branches + [[cond2, b2]]
						continue
					end
					if p2[0] == "else" then
						let tElse = tokens[pos]
						pos = pos + 1
						let b3 = parseSeq(["end"])
						if b3 == null then
							return _fail("parse", "unterminated else", tElse)
						end
						let endTL = tokens[pos].trimL
						let endTR = tokens[pos].trimR
						out = out + [_node("if", branches, b3, null, {tlOpen: t.trimL, trOpen: t.trimR, tlEnd: endTL, trEnd: endTR, line: t.line, col: t.col})]
						if pos < len(tokens) and tokens[pos].kind == "stmt" and tokens[pos].value == "end" then
							pos = pos + 1
							break
						end
						return _fail("parse", "expected end after else", tElse)
					end
					break
				end

				if len(out) == 0 or out[len(out)-1].tag != "if" then
					let endTL2 = tokens[pos].trimL
					let endTR2 = tokens[pos].trimR
					out = out + [_node("if", branches, null, null, {tlOpen: t.trimL, trOpen: t.trimR, tlEnd: endTL2, trEnd: endTR2, line: t.line, col: t.col})]
					if pos < len(tokens) and tokens[pos].kind == "stmt" and tokens[pos].value == "end" then
						pos = pos + 1
					else
						return _fail("parse", "expected end", t)
					end
				end
				continue
			end

			if head == "for" then
				pos = pos + 1
				let rest = strip(substr(body, 4, len(body)))
				let bits = _splitTopLevel(rest, " in ")
				if len(bits) != 2 then
					return _fail("parse", "for syntax", t)
				end
				let name = strip(bits[0])
				let iter = strip(bits[1])
				let b = parseSeq(["end"])
				if b == null then
					return _fail("parse", "unterminated for", t)
				end
				let endTL = tokens[pos].trimL
				let endTR = tokens[pos].trimR
				out = out + [_node("for", name, iter, b, {tlOpen: t.trimL, trOpen: t.trimR, tlEnd: endTL, trEnd: endTR, line: t.line, col: t.col})]
				if pos < len(tokens) and tokens[pos].kind == "stmt" and tokens[pos].value == "end" then
					pos = pos + 1
				else
					return _fail("parse", "expected end", t)
				end
				continue
			end

			if head == "include" then
				pos = pos + 1
				let rest = strip(substr(body, 8, len(body)))
				let parts2 = _splitTopLevel(rest, " with ")
				let pth = parts2[0]
				let ctxExpr = null
				if len(parts2) >= 2 then
					ctxExpr = parts2[1]
				end
				out = out + [_node("include", pth, ctxExpr, null, {tlOpen: t.trimL, trOpen: t.trimR, line: t.line, col: t.col})]
				continue
			end

			if head == "block" then
				pos = pos + 1
				let nm = strip(substr(body, 6, len(body)))
				let b = parseSeq(["end"])
				if b == null then
					return _fail("parse", "unterminated block", t)
				end
				let endTL = tokens[pos].trimL
				let endTR = tokens[pos].trimR
				out = out + [_node("block", nm, b, null, {tlOpen: t.trimL, trOpen: t.trimR, tlEnd: endTL, trEnd: endTR, line: t.line, col: t.col})]
				if pos < len(tokens) and tokens[pos].kind == "stmt" and tokens[pos].value == "end" then
					pos = pos + 1
				else
					return _fail("parse", "expected end", t)
				end
				continue
			end

			if head == "extends" then
				pos = pos + 1
				let p = strip(substr(body, 8, len(body)))
				out = out + [_node("extends", p, null, null, {tlOpen: t.trimL, trOpen: t.trimR, line: t.line, col: t.col})]
				continue
			end

			if head == "end" then
				return out
			end

			return _fail("parse", "unknown tag", tokens[pos])
		end
		out
	end

	let ast = parseSeq([])
	if ast == null then
		return _fail("parse", "invalid structure", {line: 0, col: 0})
	end
	ast
end

# ---------------- Expression evaluation ----------------

# Parse a quoted string literal and return its contents.
#
# Args:
# 	s: Str
#
# Return:
# 	Str?  # null when not a quoted literal
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

# Parse number/bool/null primitives from a bare token.
#
# Args:
# 	s: Str
#
# Return:
# 	Any?  # Int|Num|Bool|null, or null when not a primitive literal
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

# Split a comma-separated argument string at top level.
#
# Args:
# 	s: Str
#
# Return:
# 	[Str]
let _argsParse = fun(s: Str) -> [Str] do
	if len(strip(s)) == 0 then
		return []
	end
	_splitTopLevel(s, ",")
end

# Parse Jinja-style ternary: A if cond else B (top-level only).
#
# Args:
# 	s: Str
#
# Return:
# 	{A: Str, cond: Str, B: Str}?  # null if not a ternary form
let _splitTernary = fun(s: Str) -> {A: Str, cond: Str, B: Str}? do
	let partsIf = _splitTopLevel(s, " if ")
	if len(partsIf) != 2 then
		return null
	end
	let partsElse = _splitTopLevel(partsIf[1], " else ")
	if len(partsElse) != 2 then
		return null
	end
	{A: partsIf[0], cond: partsElse[0], B: partsElse[1]}
end

# Find the index of the first top-level '('; -1 if none.
#
# Args:
# 	s: Str
#
# Return:
# 	Int
let _findTopLevelParen = fun(s: Str) -> Int do
	let i = 0
	let inQ = ""
	let depth = 0
	while i < len(s) do
		let ch = substr(s, i, i + 1)
		if inQ != "" then
			if ch == inQ then
				inQ = ""
			end
			i = i + 1
			continue
		end
		if ch == "\"" or ch == "'" then
			inQ = ch
			i = i + 1
			continue
		end
		if ch == "(" then
			if depth == 0 then
				return i
			end
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

# Evaluate a dotted name path in scope (globals override by scope merge).
#
# Args:
# 	scope: {}
# 	name: Str
# 	strict: Bool  # strictUndefined behavior
#
# Return:
# 	Any?  # null # <render: ...> on missing per strict mode
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
				if strict then
					return error("render: access null")
				else
					return null
				end
			end
			if isType(cur, type {}) and mapHas(cur, p) then
				cur = cur[p]
			else
				if strict then
					return error("render: missing field " + p)
				else
					return null
				end
			end
		end
		j = j + 1
	end
	cur
end

# Evaluate an expression string within scope using filters/helpers/globals.
#
# Args:
# 	e: Env
# 	scope: {}
# 	code: Str
# 	at: {}  # location {line:Int, col:Int}
#
# Return:
# 	Any?  # annotated null on render errors
let _evalExpr = fun(e: Env, scope: {}, code: Str, at: {}) -> Any? do
	let pipes = _splitTopLevel(code, "|")
	if len(pipes) == 0 then
		return
	end

	let base = strip(pipes[0])
	let val = null

	let ter = _splitTernary(base)
	if ter != null then
		let cv = _evalExpr(e, scope, ter.cond, at)
		if cv == null and noteGet(cv) != null then
			return cv
		end
		if cv == null then
			return _fail("render", "bad condition", at)
		end
		let truth = bool(cv)
		if truth == null then
			return _fail("render", "non-bool condition", at)
		end
		if truth then
			val = _evalExpr(e, scope, ter.A, at)
		else
			val = _evalExpr(e, scope, ter.B, at)
		end
		if val == null and noteGet(val) != null then
			return val
		end
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
					if len(nm) == 0 then
						val = _evalExpr(e, scope, strip(substr(base, p + 1, len(base) - 1)), at)
					else
						if not mapHas(e._helpers, nm) then
							return _fail("render", "unknown helper " + nm, at)
						end
						let argsStr = strip(substr(base, p + 1, len(base) - 1))
						let argExprs = _argsParse(argsStr)
						let argv = []
						let i = 0
						while i < len(argExprs) do
							let v = _evalExpr(e, scope, argExprs[i], at)
							if v == null and noteGet(v) != null then
								return v
							end
							if v == null and strip(argExprs[i]) != "null" then
								return _fail("render", "bad arg", at)
							end
							argv = argv + [v]
							i = i + 1
						end
						val = e._helpers[nm]
						let k = 0
						while k < len(argv) do
							let r = try(fun() do
								val(argv[k])
							end)
							if not r.ok then
								return _fail("render", "arity: too many arguments", at)
							end
							val = r.value
							k = k + 1
						end
						let rf = reflect(val)
						if rf != null and isType(rf, type [Any]) and len(rf) > 0 and rf[0] == "fun" then
							return _fail("render", "arity: too few arguments", at)
						end
					end
				else
					let merged = {}
					for [k, v] in e._globals do
						merged[k] = v
					end
					for [k2, v2] in scope do
						merged[k2] = v2
					end
					val = _evalNamePath(merged, base, e.opts.strictUndefined)
				end
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
			return _fail("render", "|safe disabled; pass SafeHTML from host", at)
		end
		if not mapHas(e._filters, fname) then
			return _fail("render", "unknown filter " + fname, at)
		end
		let f = e._filters[fname]
		let argv = [val]
		let j = 0
		while j < len(argList) do
			let vj = _evalExpr(e, scope, argList[j], at)
			if vj == null and noteGet(vj) != null then
				return vj
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

# ---------------- Rendering ----------------

# Whether an expression’s final pipe is tojson (required in <script>).
#
# Args:
# 	code: Str
#
# Return:
# 	Bool
let _endsWithToJSON = fun(code: Str) -> Bool do
	let pipes = _splitTopLevel(code, "|")
	if len(pipes) == 0 then
		return false
	end
	toLower(strip(pipes[len(pipes) - 1])) == "tojson"
end

# Trim a single newline to the left of the current position (for trims).
#
# Args:
# 	s: Str
#
# Return:
# 	Str
let _trimLeftNL = fun(s: Str) -> Str do
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

# Flush a pending newline to the writer if present.
#
# Args:
# 	w: Writer
# 	buf: {s: Str, trimNext: Bool, pendingNL: Bool}
# 	at: {}
#
# Return:
# 	Bool?  # null # <render: writer failed>
let _flushPendingNL = fun(w: Writer, buf: {s: Str, trimNext: Bool, pendingNL: Bool}, at: {}) -> Bool? do
	if buf.pendingNL then
		let n = w.write("\n")
		if n == null then
			return _fail("render", "writer failed", at)
		end
		buf.s = buf.s + "\n"
		buf.pendingNL = false
	end
	true
end

# Write a string to the writer honoring trimming rules.
#
# Args:
# 	w: Writer
# 	buf: {s: Str, trimNext: Bool, pendingNL: Bool}
# 	s: Str
# 	at: {}
#
# Return:
# 	Bool?  # null # <render: writer failed>
let _write = fun(w: Writer, buf: {s: Str, trimNext: Bool, pendingNL: Bool}, s: Str, at: {}) -> Bool? do
	if buf.trimNext then
		s = _trimLeadingWSNL(s)
		buf.trimNext = false
	end

	if buf.pendingNL and len(s) > 0 then
		let n1 = w.write("\n")
		if n1 == null then
			return _fail("render", "writer failed", at)
		end
		buf.s = buf.s + "\n"
		buf.pendingNL = false
	end

	if len(s) > 0 and substr(s, len(s)-1, len(s)) == "\n" then
		let body = substr(s, 0, len(s)-1)
		if len(body) > 0 then
			let n2 = w.write(body)
			if n2 == null then
				return _fail("render", "writer failed", at)
			end
			buf.s = buf.s + body
		end
		buf.pendingNL = true
		return true
	end

	if len(s) > 0 then
		let n = w.write(s)
		if n == null then
			return _fail("render", "writer failed", at)
		end
		buf.s = buf.s + s
	end
	true
end

# Render a sequence of AST nodes into a Writer with a scope.
#
# Args:
# 	e: Env
# 	ast: [Any]
# 	scope: {}
# 	w: Writer
# 	buf: {s: Str, trimNext: Bool, pendingNL: Bool}
# 	curName: Str
# 	stack: [Str]  # include/extends cycle detection
#
# Return:
# 	Bool?  # annotated null on render errors
let _renderSeq = fun(e: Env, ast: [Any], scope: {}, w: Writer, buf: {s: Str, trimNext: Bool, pendingNL: Bool}, curName: Str, stack: [Str]) -> Bool? do
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
			let ok = _write(w, buf, s, n.a)
			if ok == null then
				return ok
			end

		elif n.tag == "expr" then
			if mapHas(n.a, "tl") and n.a.tl and buf.pendingNL then
				buf.pendingNL = false
			end

			let c = n.a.ctx
			if c == "RawCSS" or c == "StyleAttr" then
				return _fail("render", "interpolation in CSS/style is forbidden", n.a)
			end
			if c == "EventAttr" then
				return _fail("render", "event handler attributes disallow interpolation", n.a)
			end
			if c == "RawJS" and not _endsWithToJSON(n.a.code) then
				return _fail("render", "interpolation inside <script> requires | tojson", n.a)
			end
			if (c == "GenericAttr" or c == "URLAttr") and n.a.quote == "unquoted" then
				return _fail("render", "unquoted attribute interpolation is forbidden", n.a)
			end

			let v = _evalExpr(e, scope, n.a.code, n.a)
			if v == null and noteGet(v) != null then
				return v
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
					return _fail("render", "missing name " + src, n.a)
				end
				out = ""
			elif _isSafeHTML(v) then
				out = v.html
			elif _isSafeURL(v) then
				# defense-in-depth: always re-validate policy
				if not _urlAllowed(v.url) then
					return _fail("render", "disallowed url scheme", n.a)
				end
				if c == "URLAttr" then
					out = v.url
				else
					let s2 = v.url
					out = if autoesc then _htmlEscape(s2) else s2 end
				end
			else
				# Block forged Safe* shapes (structurally match but wrong brand)
				if isType(v, type {__safe_html__!: Bool, html!: Str}) and not _isSafeHTML(v) then
					return _fail("render", "forged SafeHTML", n.a)
				end
				if isType(v, type {__safe_url__!: Bool, url!: Str}) and not _isSafeURL(v) then
					return _fail("render", "forged SafeURL", n.a)
				end
				let s2 = if isType(v, type Str) then v else str(v) end
				if s2 == null then
					return _fail("render", "stringify failed", n.a)
				end
				if c == "URLAttr" then
					let an = toLower(n.a.attrName)
					if an == "srcset" or an == "imagesrcset" then
						for item in split(s2, ",") do
							let t = strip(item)
							if len(t) > 0 then
								let p = 0
								while p < len(t) and len(match("\\s", substr(t, p, p + 1))) == 0 do
									p = p + 1
								end
								if not _urlAllowed(strip(substr(t, 0, p))) then
									return _fail("render", "disallowed url scheme", n.a)
								end
							end
						end
					elif not _urlAllowed(s2) then
						return _fail("render", "disallowed url scheme", n.a)
					end
				end
				if c == "RawJS" then
					out = s2
				else
					if autoesc then
						out = _htmlEscape(s2)
					else
						out = s2
					end
				end
			end

			if mapHas(n.a, "tr") and n.a.tr then
				buf.trimNext = true
			end
			let ok2 = _write(w, buf, out, n.a)
			if ok2 == null then
				return ok2
			end

		elif n.tag == "if" then
			if n.d != null and n.d.trOpen then
				buf.trimNext = true
			end

			let done = false
			let bi = 0
			while bi < len(n.a) do
				let pair = n.a[bi]
				let cond = _evalExpr(e, scope, pair[0], n.d)
				if cond == null and noteGet(cond) != null then
					return cond
				end
				if cond == null then
					return _fail("render", "bad if cond", n.d)
				end
				let b = bool(cond)
				if b == null then
					return _fail("render", "non-bool if cond", n.d)
				end
				if b then
					let ok = _renderSeq(e, pair[1], scope, w, buf, curName, stack)
					if ok == null then
						return ok
					end
					done = true
					break
				end
				bi = bi + 1
			end
			if not done and n.b != null then
				let ok3 = _renderSeq(e, n.b, scope, w, buf, curName, stack)
				if ok3 == null then
					return ok3
				end
			end

			if n.d != null and n.d.tlEnd and buf.pendingNL then
				buf.pendingNL = false
			end
			if n.d != null and n.d.trEnd then
				buf.trimNext = true
			end

		elif n.tag == "for" then
			if n.d != null and n.d.trOpen then
				buf.trimNext = true
			end

			let it = _evalExpr(e, scope, n.b, n.d)
			if it == null and noteGet(it) != null then
				return it
			end

			if it == null then
				null
			elif isType(it, type [Any]) then
				let L = len(it)
				let idx = 0
				while idx < L do
					let loop = { index0: idx, index: idx + 1, first: idx == 0, last: idx == L - 1, length: L }
					let sc = {}
					for [k, v] in scope do
						sc[k] = v
					end
					sc[n.a] = it[idx]
					sc["loop"] = loop
					let ok4 = _renderSeq(e, n.c, sc, w, buf, curName, stack)
					if ok4 == null then
						return ok4
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
					for [kk, vv] in scope do
						sc2[kk] = vv
					end
					if dk != "" and dv != "" then
						sc2[dk] = k
						sc2[dv] = it[k]
					else
						sc2[n.a] = [k, it[k]]
					end
					sc2["loop"] = loop2
					let ok5 = _renderSeq(e, n.c, sc2, w, buf, curName, stack)
					if ok5 == null then
						return ok5
					end
					j = j + 1
				end
			else
				null
			end

			if n.d != null and n.d.tlEnd and buf.pendingNL then
				buf.pendingNL = false
			end
			if n.d != null and n.d.trEnd then
				buf.trimNext = true
			end

		elif n.tag == "include" then
			if n.d != null and n.d.trOpen then
				buf.trimNext = true
			end

			let pthLit = _parseStringLit(n.a)
			let pathVal = if pthLit != null then pthLit else _evalExpr(e, scope, n.a, n.d) end
			if pathVal == null and noteGet(pathVal) != null then
				return pathVal
			end
			if pathVal == null then
				return _fail("render", "include path", n.d)
			end

			let full = _resolvePath(e.opts.baseDir, curName, str(pathVal))
			if full == null then
				return full
			end
			let src = readFile(full)
			if src == null then
				return error("load: include failed")
			end
			let childT = fromString(e, src, _relName(e.opts.baseDir, full))
			if childT == null then
				return childT
			end

			let ctx2 = scope
			if n.b != null then
				let evalRes = _evalExpr(e, scope, n.b, n.d)
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
						if len(kv) != 2 then
							okMap = false
							break
						end
						let kraw = strip(kv[0])
						let ks = _parseStringLit(kraw)
						let key = if ks != null then ks else kraw end
						let vexpr = strip(kv[1])
						let vv = _evalExpr(e, scope, vexpr, n.d)
						if vv == null and noteGet(vv) != null then
							okMap = false
							break
						end
						if vv == null and vexpr != "null" then
							okMap = false
							break
						end
						m[key] = vv
						pi = pi + 1
					end
					if okMap then
						cx = m
					end
				end
				if cx == null then
					if noteGet(evalRes) != null and not braceLiteral then
						return evalRes
					end
					return _fail("render", "include ctx", n.d)
				end
				if isType(cx, type {}) then
					let merged = {}
					for [k, v] in scope do
						merged[k] = v
					end
					for [k2, v2] in cx do
						merged[k2] = v2
					end
					ctx2 = merged
				else
					return _fail("render", "include ctx must be map", n.d)
				end
			end

			let z = 0
			while z < len(stack) do
				if stack[z] == childT.name then
					return _fail("render", "include/extends cycle", n.d)
				end
				z = z + 1
			end

			let ok6 = _renderTemplate(e, childT, ctx2, w, stack + [childT.name])
			if ok6 == null then
				return ok6
			end

		elif n.tag == "block" then
			if n.d != null and n.d.trOpen then
				buf.trimNext = true
			end
			let okb = _renderSeq(e, n.b, scope, w, buf, curName, stack)
			if okb == null then
				return okb
			end
			if n.d != null and n.d.tlEnd and buf.pendingNL then
				buf.pendingNL = false
			end
			if n.d != null and n.d.trEnd then
				buf.trimNext = true
			end

		elif n.tag == "extends" then
			null

		else
			return _fail("render", "unknown node", n.a)
		end

		i = i + 1
	end
	true
end

# Collect block bodies from an AST by block name.
#
# Args:
# 	ast: [Any]
#
# Return:
# 	{}  # map blockName -> [Any]
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

# Apply block overrides from child onto base AST (full replacement; no super()).
#
# Args:
# 	baseAst: [Any]
# 	slots: {}  # name -> [Any]
#
# Return:
# 	[Any]
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

# Render a compiled template, handling extends/blocks/include.
#
# Args:
# 	e: Env
# 	t: Template
# 	data: {}
# 	w: Writer
# 	stack: [Str]
#
# Return:
# 	Bool?  # annotated null on errors
let _renderTemplate = fun(e: Env, t: Template, data: {}, w: Writer, stack: [Str]) -> Bool? do
	let hasExt = false
	let parentPath = ""
	let i = 0
	while i < len(t.ast) do
		let n = t.ast[i]
		if n.tag == "extends" then
			hasExt = true
			let pth = _parseStringLit(n.a)
			let pv = if pth != null then pth else str(_evalExpr(e, data, n.a, n.d)) end
			if pv == null then
				return _fail("render", "extends path", n.d)
			end
			parentPath = pv
			break
		end
		i = i + 1
	end
	if not hasExt then
		let buf = {s: "", trimNext: false, pendingNL: false}
		let ok = _renderSeq(e, t.ast, data, w, buf, t.name, stack)
		if ok == null then
			return ok
		end
		if buf.pendingNL then
			let n1 = w.write("\n")
			if n1 == null then
				return _fail("render", "writer failed", {})
			end
			buf.pendingNL = false
		end
		true
	else
		let full = _resolvePath(e.opts.baseDir, t.name, parentPath)
		if full == null then
			return full
		end
		let src = readFile(full)
		if src == null then
			return error("load: extends failed")
		end
		let baseT = fromString(e, src, _relName(e.opts.baseDir, full))
		if baseT == null then
			return baseT
		end
		let cyc = 0
		while cyc < len(stack) do
			if stack[cyc] == baseT.name then
				return _fail("render", "include/extends cycle", {})
			end
			cyc = cyc + 1
		end

		let slots = _collectBlocks(t.ast)
		let combined = _applyBlocks(baseT.ast, slots)
		let buf2 = {s: "", trimNext: false, pendingNL: false}
		let ok2 = _renderSeq(e, combined, data, w, buf2, baseT.name, stack + [baseT.name])
		if ok2 == null then
			return ok2
		end
		if buf2.pendingNL then
			let n2 = w.write("\n")
			if n2 == null then
				return _fail("render", "writer failed", {})
			end
			buf2.pendingNL = false
		end
		true
	end
end

# ---------------- Compilation & I/O ----------------

# Compile a template from a string into a Template value.
#
# Args:
# 	e: Env
# 	src: Str
# 	name: Str?  # logical template name; defaults to "<string>"
#
# Return:
# 	Template?  # annotated null on parse errors
let fromString = fun(e: Env, src: Str, name: Str?) -> Template? do
	let toks = _lex(src)
	if toks == null then
		return toks
	end
	let ast = _parse(toks)
	if ast == null then
		return ast
	end
	{ __mtml__: true, name: if name == null then "<string>" else name end, ast: ast }
end

# Load and compile a template via the environment loader (jailed).
#
# Args:
# 	e: Env
# 	path: Str
#
# Return:
# 	Template?  # annotated null on load/parse errors
let load = fun(e: Env, path: Str) -> Template? do
	let info = e._loader(path)
	if info == null then
		return error("load: not found")
	end
	fromString(e, info.src, info.name)
end

# Create a string-backed writer for internal rendering.
#
# Args:
# 	_: Null
#
# Return:
# 	{w: Writer, buf: {s: Str}}
let _stringWriter = fun(_: Null) -> {w: Writer, buf: {s: Str}} do
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

# Render a template to a string (convenience).
#
# Args:
# 	e: Env
# 	t: Template
# 	data: {}
#
# Return:
# 	Str?  # annotated null on render errors
let render = fun(e: Env, t: Template, data: {}) -> Str? do
	if t == null or not isType(t, Template) then
		return error("render: not a template")
	end
	let pack = _stringWriter(null)
	let ok = _renderTemplate(e, t, data, pack.w, [t.name])
	if ok == null then
		return ok
	end
	pack.buf.s
end

# Render a template into a Writer; returns total bytes written.
#
# Args:
# 	e: Env
# 	t: Template
# 	w: Writer
# 	data: {}
#
# Return:
# 	Int?  # annotated null on writer/render errors
let renderTo = fun(e: Env, t: Template, w: Writer, data: {}) -> Int? do
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
			w.flush()
		end
	}
	let ok = _renderTemplate(e, t, data, ww, [t.name])
	if ok == null then
		return ok
	end
	let _ = ww.flush(null)
	counter.n
end

# ---------------- Built-ins ----------------

# Install a minimal set of built-in filters (upper, lower, default, join, tojson, url).
#
# Args:
# 	e: Env
#
# Return:
# 	Null
let _installBuiltins = fun(e: Env) -> Null do
	let upper = fun(s: Str) -> Str do
		toUpper(s)
	end
	let lower = fun(s: Str) -> Str do
		toLower(s)
	end

	let default = fun(x: Any, fallback: Any) -> Any do
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

	let tojson = fun(x: Any) -> SafeHTML do
		let s = jsonStringify(x)
		if s == null then
			s = "null"
		end
		let s2 = replace("</script>", "<\\/script>", s)
		let s3 = replace("&", "\\u0026", s2)
		let s4 = replace("<", "\\u003c", s3)
		let s5 = replace(">", "\\u003e", s4)
		let s6 = replace("\u2028", "\\u2028", s5)
		let s7 = replace("\u2029", "\\u2029", s6)
		safeHTML(s7)
	end

	let url = fun(u: Str) -> SafeURL? do
		safeURL(u)
	end

	let _ = registerFilter(e, "upper", upper)
	let _ = registerFilter(e, "lower", lower)
	let _ = registerFilter(e, "default", default)
	let _ = registerFilter(e, "join", joinF)
	let _ = registerFilter(e, "tojson", tojson)
	let _ = registerFilter(e, "url", url)
	null
end

# Public entry to install built-ins (fluent).
#
# Args:
# 	e: Env
#
# Return:
# 	Bool
let installBuiltins = fun(e: Env) -> Bool do
	_installBuiltins(e)
	true
end
