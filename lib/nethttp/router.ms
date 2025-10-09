# nethttp/router — routing, contracts, binding, responders, OpenAPI hook
# 
# This module implements a path router for value and streaming handlers.
# It provides request/response helpers, automatic binding/validation
# for path/query/body via contracts, response validation (optional),
# test utilities, and OpenAPI export support.

let openapi = import("nethttp/openapi")

# =========================
# Public types (data-only)
# =========================

# Request — immutable request value passed to handlers.
let Request = type {
	body!: Any,
	headers!: {},
	method!: Str,
	pathParams: {}?,
	url!: {host: Str?, path!: Str, query: Str?, scheme: Str?}
}

# Responder — streaming response interface for handlers.
let Responder = type {
	end!: Null -> Bool?,
	flush!: Null -> Bool?,
	setHeader!: Str -> Str -> Responder,
	status!: Int -> Responder,
	write!: Str -> Int?
}

# _Response — buffered response shape used by helpers and value routes.
let _Response = type {body!: Str, headers!: {}, status!: Int}

# _HttpError — structured error returned by helpers like raise().
let _HttpError = type {
	__http_error__!: Bool,
	body!: Str,
	headers!: {},
	status!: Int
}

# Contract — marker wrapper for route contracts (path/query/body/responses).
let Contract = type {}

# _Ctx — per-request context passed to handlers (bound values live here).
let _Ctx = type {
	body: Any,
	deadlineMs: Int?,
	path: {},
	query: {},
	req!: Request
}

let _StreamFn = type Request -> Responder -> {} -> Null
let _Mw = type _StreamFn -> _StreamFn

# =========================
# Utilities (private)
# =========================

let _normMethod = fun(m: Str) -> Str do
	toUpper(m)
end

let _splitPath = fun(p: Str) -> [Str] do
	let s = if len(p) == 0 or substr(p, 0, 1) != "/" then
		"/" + p
	else
		p
	end
	let raw = split(s, "/")
	let out = []
	let i = 0
	while i < len(raw) do
		let seg = raw[i]
		if len(seg) > 0 and seg != "." then
			out = out + [seg]
		end
		i = i + 1
	end
	out
end

let _isParamSeg = fun(seg: Str) -> Bool do
	let n = len(seg)
	n >= 2 and substr(seg, 0, 1) == "{" and substr(seg, n - 1, n) == "}"
end

let _isCatchSeg = fun(seg: Str) -> Bool do
	let n = len(seg)
	n >= 3 and substr(seg, 0, 1) == "{" and substr(seg, 1, 2) == "*" and substr(seg, n - 1, n) == "}"
end

let _paramName = fun(seg: Str) -> Str do
	let name = substr(seg, 1, len(seg) - 1)
	if len(name) > 0 and substr(name, 0, 1) == "*" then
		return substr(name, 1, len(name))
	end
	name
end

let _makeURL = fun(path: Str, query: Str?) -> {} do
	{
		scheme: null,
		host: null,
		path: path,
		query: query
	}
end

let _json = fun(v) -> Str? do
	let s = jsonStringify(v)
	if s != null then
		return s
	end
	null
end

let _setErr = fun(ctx: {}, r: {}) -> Null do
	if ctx == null then
		return
	end
	if mapHas(r, "error") and r.error != null then
		ctx._errorDump = r.error
	else
		ctx._errorDump = "panic"
	end
	null
end

# contract — Wrap a route contract (marker).
# 
# Summary:
# Return the given contract marker. This is a no-op wrapper that
# just signals intent at call sites.
# 
# Args:
# c: Contract — Route contract object.
# Return:
# Contract — The same contract marker.
let contract = fun(c: Contract) -> Contract do
	c
end

# =========================
# Error helpers (public)
# =========================

# raise — Build a structured HTTP error result.
# 
# Summary:
# Create a typed error object to be returned by value handlers.
# The router will convert it into an HTTP response with the given
# status and body, plus optional headers (empty here).
# 
# Args:
# status: Int — HTTP status code (e.g., 401).
# message: Str — Response body as plain text.
# Return:
# {} — Structured error object (HttpError-like).
let raise = fun(status: Int, message: Str) -> {} do
	{
		__http_error__: true,
		status: status,
		body: message,
		headers: {}
	}
end

# raiseWithHeaders — Build a structured HTTP error with headers.
# 
# Args:
# status: Int — HTTP status code.
# message: Str — Response body as plain text.
# headers: {} — Headers to set on the response.
# Return:
# {} — Structured error object.
let raiseWithHeaders = fun(status: Int, message: Str, headers: {}) -> {} do
	{
		__http_error__: true,
		status: status,
		body: message,
		headers: headers
	}
end

# =========================
# Binding helpers (private)
# =========================

let _rawBody = fun(req: Request) -> Str? do
	if isType(req.body, type Str) then
		return req.body
	end
	let s = readAll(req.body)
	if s == null then
		return
	end
	s
end

let _coerceScalar = fun(s: Str, T: Type) -> Any? do
	if isSubtype(T, type Str) then
		let pj = jsonParse(s)
		return if pj != null and isType(pj, type Str) then
			pj
		else
			s
		end
	end
	if isSubtype(T, type Int) then
		let i = int(s)
		if i != null then
			return i
		end
		let pj = jsonParse(s)
		if pj != null and isType(pj, type Int) then
			return pj
		end
		return
	end
	if isSubtype(T, type Num) then
		let n = num(s)
		if n != null then
			return n
		end
		let pj = jsonParse(s)
		if pj != null and (isType(pj, type Num) or isType(pj, type Int)) then
			return pj
		end
		return
	end
	if isSubtype(T, type Bool) then
		let sl = toLower(s)
		if sl == "true" then
			return true
		end
		if sl == "false" then
			return false
		end
		let pj = jsonParse(s)
		if pj != null and isType(pj, type Bool) then
			return pj
		end
		return
	end
	null
end

let _bindQueryMap = fun(m: {}, T: Type) -> Any? do
	let rt = reflect(T)
	if rt == null then
		return
	end
	let node = rt[1]
	if node[0] != "map" then
		return
	end

	let out = {}
	let entries = slice(node, 1, len(node))
	let i = 0
	while i < len(entries) do
		let e = entries[i]
		let tag = e[0]
		let k = e[1][1]
		let TkNode = e[2]
		let Tk = reify(["type", TkNode])
		let vs = if mapHas(m, k) then
			m[k]
		else
			null
		end

		if vs == null or len(vs) == 0 then
			if tag == "pair!" then
				return
			end
			i = i + 1
			continue
		end

		if TkNode[0] == "array" then
			let U = reify(["type", TkNode[1]])
			if len(vs) == 1 then
				let pj = jsonParse(vs[0])
				if pj != null and isType(pj, type [Any]) then
					let arr = []
					let j = 0
					while j < len(pj) do
						let s = str(pj[j])
						let u = _coerceScalar(s, U)
						if u == null then
							return
						end
						arr = arr + [u]
						j = j + 1
					end
					out[k] = arr
					i = i + 1
					continue
				end
			end
			let arr2 = []
			let j2 = 0
			while j2 < len(vs) do
				let u2 = _coerceScalar(vs[j2], U)
				if u2 == null then
					return
				end
				arr2 = arr2 + [u2]
				j2 = j2 + 1
			end
			out[k] = arr2
			i = i + 1
			continue
		end

		let s = vs[len(vs) - 1]
		let v = _coerceScalar(s, Tk)
		if v == null then
			return
		end
		out[k] = v
		i = i + 1
	end

	if isType(out, T) then
		return out
	end
	null
end

let _bindQueryFromReq = fun(req: Request, TQ: Type) -> Any? do
	let qm = urlQueryParse(if req.url.query == null then
		""
	else
		req.url.query
	end)
	if qm == null then
		return
	end
	_bindQueryMap(qm, TQ)
end

let _paramsFromPath = fun(parts: [Str], segs: [Str]) -> {} do
	let ps = {}
	let i = 0
	while i < len(parts) do
		let p = parts[i]
		if _isParamSeg(p) then
			ps[_paramName(p)] = segs[i]
		end
		i = i + 1
	end
	ps
end

let _bindPath = fun(ps: {}, TP: Type) -> Any? do
	let rt = reflect(TP)
	if rt == null then
		return
	end
	let node = rt[1]
	if node[0] != "map" then
		return
	end
	let out = {}
	let entries = slice(node, 1, len(node))
	let i = 0
	while i < len(entries) do
		let e = entries[i]
		let tag = e[0]
		let k = e[1][1]
		let Tk = reify(["type", e[2]])
		let raw = if mapHas(ps, k) then
			ps[k]
		else
			null
		end
		if raw == null then
			if tag == "pair!" then
				return
			end
			i = i + 1
			continue
		end
		let v = _coerceScalar(raw, Tk)
		if v == null then
			return
		end
		out[k] = v
		i = i + 1
	end
	if isType(out, TP) then
		return out
	end
	null
end

let _bindBody = fun(req: Request, TB: Type) -> Any? do
	let raw = _rawBody(req)
	if raw == null then
		return
	end
	let v = jsonParse(raw)
	if v == null then
		return
	end
	if not isType(v, TB) then
		return
	end
	v
end

# Build a compact JSON validation payload.
# reason may be absent (null) when a binder failed without a note.
let _validationJSON = fun(where: Str, reason: Str?) -> Str do
	let msg = if reason == null then
		"validation failed"
	else
		reason
	end
	let obj = {error: "validation", detail: [{in: where, reason: msg}]}
	let s = _json(obj)
	if s == null then
		"{\"error\":\"validation\"}"
	else
		s
	end
end

# =========================
# Response helpers (value handlers)
# =========================

# json — Build an application/json response with status code.
# 
# Args:
# status: Int — HTTP status code.
# value: Any — JSON-serializable value.
# Return:
# {} — Buffered response (status/headers/body).
let json = fun(status: Int, value) -> {} do
	let s = _json(value)
	if s == null then
		return {
			status: 500,
			headers: {"Content-Type": "text/plain"},
			body: "json encode error"
		}
	end
	{
		status: status,
		headers: {"Content-Type": "application/json; charset=utf-8"},
		body: s
	}
end

# text — Build a text/plain response with status code.
# 
# Args:
# status: Int — HTTP status code.
# s: Str — Response body.
# Return:
# {} — Buffered response.
let text = fun(status: Int, s: Str) -> {} do
	{
		status: status,
		headers: {"Content-Type": "text/plain"},
		body: s
	}
end

# noContent — Build a 204 No Content response.
# 
# Args:
# _: Null — Unused.
# Return:
# {} — Buffered response with empty body.
let noContent = fun(_: Null) -> {} do
	{status: 204, headers: {}, body: ""}
end

# redirect — Build a redirect response with Location header.
# 
# Args:
# status: Int — Redirect status (e.g., 302).
# location: Str — Absolute path starting with "/".
# Return:
# {} — Buffered response; 400 if invalid location.
let redirect = fun(status: Int, location: Str) -> {} do
	if len(location) == 0 or substr(location, 0, 1) != "/" then
		return {
			status: 400,
			headers: {"Content-Type": "text/plain"},
			body: "bad redirect"
		}
	end
	{status: status, headers: {Location: location}, body: ""}
end

# =========================
# Buffered responder (for value routes, tests)
# =========================

let _newResponder = fun(_: Null) -> {api!: Responder, res!: _Response} do
	let state = {status: 200, headers: {}, body: ""}
	let api = {}
	api.status = fun(code: Int) -> Responder do
		state.status = code
		api
	end
	api.setHeader = fun(k: Str, v: Str) -> Responder do
		state.headers[k] = v
		api
	end
	api.write = fun(chunk: Str) -> Int? do
		state.body = state.body + chunk
		len(chunk)
	end
	api.flush = fun(_: Null) -> Bool? do
		true
	end
	api.end = fun(_: Null) -> Bool? do
		true
	end
	{res: state, api: api}
end

# =========================
# Socket-backed responder (true streaming)
# =========================

let _reason = fun(code: Int) -> Str do
	if code == 200 then
		return "OK"
	end
	if code == 204 then
		return "No Content"
	end
	if code == 302 then
		return "Found"
	end
	if code == 400 then
		return "Bad Request"
	end
	if code == 401 then
		return "Unauthorized"
	end
	if code == 404 then
		return "Not Found"
	end
	if code == 405 then
		return "Method Not Allowed"
	end
	if code == 413 then
		return "Payload Too Large"
	end
	if code == 414 then
		return "URI Too Long"
	end
	if code == 422 then
		return "Unprocessable Entity"
	end
	if code == 431 then
		return "Request Header Fields Too Large"
	end
	if code == 500 then
		return "Internal Server Error"
	end
	if code == 504 then
		return "Gateway Timeout"
	end
	"OK"
end

let _newSockResponder = fun(conn, opts: {}) -> {api!: Responder, state!: {}} do
	let st = {
		status: 200,
		headers: {},
		wroteHead: false,
		chunked: false,
		ended: false
	}
	let _emitHead = fun(_: Null) -> Bool do
		if st.wroteHead then
			return true
		end
		if not mapHas(st.headers, "Content-Type") then
			st.headers["Content-Type"] = "text/plain"
		end
		let useCL = mapHas(st.headers, "Content-Length")
		if not useCL then
			st.headers["Transfer-Encoding"] = "chunked"
			st.chunked = true
		end
		let head = sprintf("HTTP/1.1 %d %s\r\n", [
			int(st.status),
			_reason(int(st.status))
		])
		for [k, v] in st.headers do
			head = head + sprintf("%s: %s\r\n", [k, v])
		end
		head = head + "\r\n"
		let n = write(conn, head)
		if n == null then
			return false
		end
		let _ = flush(conn)
		st.wroteHead = true
		true
	end

	let api = {}
	api.status = fun(code: Int) -> Responder do
		if st.wroteHead then
			return api
		end
		st.status = code
		api
	end
	api.setHeader = fun(k: Str, v: Str) -> Responder do
		if st.wroteHead then
			return api
		end
		st.headers[k] = v
		api
	end
	api.write = fun(chunk: Str) -> Int? do
		if st.ended then
			return 0
		end
		if not _emitHead(null) then
			return
		end
		if st.chunked then
			let pre = sprintf("%x\r\n", [len(chunk)])
			if write(conn, pre) == null then
				return
			end
			if write(conn, chunk) == null then
				return
			end
			if write(conn, "\r\n") == null then
				return
			end
			let _ = flush(conn)
			len(chunk)
		else
			let n = write(conn, chunk)
			if n == null then
				return
			end
			let _ = flush(conn)
			n
		end
	end
	api.flush = fun(_: Null) -> Bool? do
		flush(conn)
	end
	api.end = fun(_: Null) -> Bool? do
		if st.ended then
			return true
		end
		if not st.wroteHead then
			if not _emitHead(null) then
				return true
			end
		end
		if st.chunked then
			if write(conn, "0\r\n\r\n") == null then
				return
			end
		end
		let _ = flush(conn)
		st.ended = true
		true
	end

	{api: api, state: st}
end

# =========================
# Router (closure-backed)
# =========================

let _makeRoute = fun(method: Str, pattern: Str, call: _StreamFn, style: Str, c: Contract) -> {} do
	let m = _normMethod(method)
	let isPrefix = len(pattern) > 1 and substr(pattern, len(pattern) - 1, len(pattern)) == "/"
	{
		method: m,
		pattern: pattern,
		parts: _splitPath(pattern),
		call: call,
		style: style,
		contract: c,
		prefix: isPrefix
	}
end

let _toRouteSpec = fun(method: Str, pattern: Str, style: Str, c: Contract) -> {} do
	let out = {method: _normMethod(method), pattern: pattern, style: style}

	if mapHas(c, "summary") and c.summary != null and isType(c.summary, type Str) then
		out.summary = c.summary
	end
	if mapHas(c, "description") and c.description != null and isType(c.description, type Str) then
		out.description = c.description
	end
	if mapHas(c, "tags") and c.tags != null and isType(c.tags, type [Str]) then
		out.tags = c.tags
	end
	if mapHas(c, "deprecated") and c.deprecated != null then
		out.deprecated = bool(c.deprecated)
	end
	if mapHas(c, "operationId") and c.operationId != null and isType(c.operationId, type Str) then
		out.operationId = c.operationId
	end

	if mapHas(c, "security") and c.security != null then
		if isType(c.security, type [{}]) then
			out.security = c.security
		elif isType(c.security, type {}) then
			out.security = [c.security]
		end
	end

	if mapHas(c, "x") and c.x != null and isType(c.x, type {}) then
		out.x = c.x
	end

	if mapHas(c, "path") or mapHas(c, "query") or mapHas(c, "headers") or mapHas(c, "cookies") or mapHas(c, "body") or mapHas(c, "responses") then
		out.contract = c
	end

	out
end

let _pathMatch = fun(parts: [Str], reqSegs: [Str]) -> {}? do
	let nParts = len(parts)
	let nReq = len(reqSegs)
	if nParts == 0 then
		return if nReq == 0 then
			{params: {}, matched: 0}
		else
			null
		end
	end
	let last = nParts - 1
	let hasCatch = _isCatchSeg(parts[last])
	if not hasCatch and nReq != nParts then
		return
	end
	if hasCatch and nReq < last then
		return
	end
	let params = {}
	let limit = if hasCatch then
		last
	else
		nParts
	end
	let i = 0
	while i < limit do
		let pat = parts[i]
		let seg = reqSegs[i]
		if _isParamSeg(pat) then
			params[_paramName(pat)] = seg
		else
			if pat != seg then
				return
			end
		end
		i = i + 1
	end
	if hasCatch then
		let name = _paramName(parts[last])
		let rest = join(slice(reqSegs, last, nReq), "/")
		params[name] = rest
	end
	{
		params: params,
		matched: if hasCatch then
			last
		else
			nParts
		end
	}
end

let _getHeader = fun(h: {}, name: Str) -> Str? do
	let want = toLower(name)
	for [k, v] in h do
		if toLower(k) == want then
			return v
		end
	end
	null
end

# router — Create a new router.
# 
# Summary:
# Construct a router instance supporting value and streaming routes,
# middleware composition, nested mounting, and OpenAPI export.
# 
# Args:
# opts: {}? — Optional:
# docs?: {
# enabled?: Bool (default true),
# path?: Str (default "/openapi.json"),
# info?: openapi.Info (default {title:"API", version:"0.1.0"}),
# servers?: [Str] (default []),
# opts?: openapi.SpecOpts (default {})
# }
# Return:
# {} — Router instance with route/routeStream/mount/use/test helpers.
let router = fun(opts: {}?) -> {} do
	let o = if opts == null then
		{}
	else
		opts
	end
	let dIn = if mapHas(o, "docs") and o.docs != null then
		o.docs
	else
		{}
	end
	let docs = {
		enabled: if mapHas(dIn, "enabled") and dIn.enabled != null then
			bool(dIn.enabled)
		else
			true
		end,
		path: if mapHas(dIn, "path") and dIn.path != null then
			dIn.path
		else
			"/openapi.json"
		end,
		info: if mapHas(dIn, "info") and dIn.info != null then
			dIn.info
		else
			{title: "API", version: "0.1.0"}
		end,
		servers: if mapHas(dIn, "servers") and dIn.servers != null then
			dIn.servers
		else
			[]
		end,
		opts: if mapHas(dIn, "opts") and dIn.opts != null then
			dIn.opts
		else
			{}
		end
	}

	let _routes = []
	let _mws = []
	let _mounts = []
	let _specRoutes = []

	let _buildSpec = fun(_: Null) -> {} do
		openapi.spec(_specRoutes, docs.info, docs.servers, docs.opts)
	end

	let _startsWith = fun(a: [Str], b: [Str]) -> Bool do
		if len(a) < len(b) then
			return false
		end
		let i = 0
		while i < len(b) do
			if a[i] != b[i] then
				return false
			end
			i = i + 1
		end
		true
	end

	let _allowedMethodsLocal = fun(path: Str) -> [Str] do
		let segs = _splitPath(path)
		let out = []
		for r in _routes do
			let hit = _pathMatch(r.parts, segs) != null or mapHas(r, "prefix") and r.prefix == true and _startsWith(segs, r.parts)
			if hit then
				let seen = false
				let i = 0
				while i < len(out) do
					if out[i] == r.method then
						seen = true
						break
					end
					i = i + 1
				end
				if not seen then
					out = out + [r.method]
				end
			end
		end
		out
	end

	let _matchLocal = fun(method: Str, path: Str) -> {}? do
		let want = _normMethod(method)
		let segs = _splitPath(path)
		let best = null
		let score = -1
		for r in _routes do
			if r.method != want then
				if not (want == "HEAD" and r.method == "GET" and r.style == "value") then
					continue
				end
			end
			let p = _pathMatch(r.parts, segs)
			if p != null then
				let s = int(p.matched)
				if s > score then
					best = {route: r, segs: segs, params: p.params}
					score = s
				end
			end
		end
		if best != null then
			return best
		end
		null
	end

	let _pathExistsLocal = fun(path: Str) -> Bool do
		let segs = _splitPath(path)
		for r in _routes do
			if _pathMatch(r.parts, segs) != null then
				return true
			end
		end
		false
	end

	let _compose = fun(fn: _StreamFn) -> _StreamFn do
		let h = fn
		let i = len(_mws) - 1
		while i >= 0 do
			h = _mws[i](h)
			i = i - 1
		end
		h
	end

	let _emitViaMw = fun(req: Request, ctx: {}, status: Int, headers: {}, body: Str) -> _Response do
		let pack = _newResponder(null)
		let h = fun(_: Request, res: Responder, _: {}) -> Null do
			for [k, v] in headers do
				res.setHeader(k, v)
			end
			res.status(status)
			let _ = res.write(body)
			let _ = res.end(null)
			null
		end
		let call = _compose(h)
		let _ = call(req, pack.api, ctx)
		{
			status: pack.res.status,
			headers: pack.res.headers,
			body: pack.res.body
		}
	end

	let _emitStreamViaMw = fun(req: Request, ctx: {}, conn, opts: {}, status: Int, headers: {}, body: Str) -> Null do
		let sock = _newSockResponder(conn, opts)
		let h = fun(_: Request, res: Responder, _: {}) -> Null do
			for [k, v] in headers do
				res.setHeader(k, v)
			end
			res.status(status)
			let _ = res.write(body)
			let _ = res.end(null)
			null
		end
		let call = _compose(h)
		let _ = call(req, sock.api, ctx)
		null
	end

	let _delegateToMount = fun(req: Request) -> _Response? do
		let segs = _splitPath(req.url.path)
		for m in _mounts do
			let pfx = m.prefixParts
			if len(segs) < len(pfx) then
				continue
			end
			let ok = true
			let i = 0
			while i < len(pfx) do
				if pfx[i] != segs[i] then
					ok = false
					break
				end
				i = i + 1
			end
			if not ok then
				continue
			end
			let rest = slice(segs, len(pfx), len(segs))
			let subPath = "/" + join(rest, "/")
			let subReq = {
				method: req.method,
				url: {
					scheme: req.url.scheme,
					host: req.url.host,
					path: subPath,
					query: req.url.query
				},
				headers: req.headers,
				body: req.body,
				pathParams: {}
			}
			return m.child._entryCall(subReq)
		end
		null
	end

	let _entryCore = fun(req: Request) -> _Response do
		let m = _matchLocal(req.method, req.url.path)
		if m == null then
			let delegated = _delegateToMount(req)
			if delegated != null then
				return delegated
			end

			let ctx = {req: req}
			if _normMethod(req.method) == "OPTIONS" then
				let allowed = _allowedMethodsLocal(req.url.path)
				if len(allowed) > 0 then
					return _emitViaMw(req, ctx, 204, {
						Allow: join(allowed, ", ")
					}, "")
				end
				if len(_mws) > 0 then
					return _emitViaMw(req, ctx, 204, {}, "")
				end
				ctx._errorDump = "not found"
				return _emitViaMw(req, ctx, 404, {"Content-Type": "text/plain"}, "not found")
			end
			if _pathExistsLocal(req.url.path) then
				ctx._errorDump = "method not allowed"
				return _emitViaMw(req, ctx, 405, {
					"Content-Type": "text/plain",
					Allow: join(_allowedMethodsLocal(req.url.path), ", ")
				}, "method not allowed")
			end
			ctx._errorDump = "not found"
			return _emitViaMw(req, ctx, 404, {"Content-Type": "text/plain"}, "not found")
		end

		let r = m.route
		let params = if mapHas(m, "params") and m.params != null then
			m.params
		else
			{}
		end
		let C = if mapHas(r, "contract") then
			r.contract
		else
			{}
		end

		let rq = {
			method: req.method,
			url: req.url,
			headers: req.headers,
			body: req.body,
			pathParams: params
		}
		let ctx = {req: rq}

		if mapHas(C, "path") and C.path != null then
			let P = _bindPath(rq.pathParams, C.path)
			if P == null then
				ctx._errorDump = "validation error in path: " + str(noteGet(P))
				return _emitViaMw(rq, ctx, 422, {
					"Content-Type": "application/json; charset=utf-8"
				}, _validationJSON("path", noteGet(P)))
			end
			ctx.path = P
		end
		if mapHas(C, "query") and C.query != null then
			let Q = _bindQueryFromReq(rq, C.query)
			if Q == null then
				ctx._errorDump = "validation error in query: " + str(noteGet(Q))
				return _emitViaMw(rq, ctx, 422, {
					"Content-Type": "application/json; charset=utf-8"
				}, _validationJSON("query", noteGet(Q)))
			end
			ctx.query = Q
		end
		if mapHas(C, "body") and C.body != null then
			let B = _bindBody(rq, C.body)
			if B == null then
				ctx._errorDump = "validation error in body: " + str(noteGet(B))
				return _emitViaMw(rq, ctx, 422, {
					"Content-Type": "application/json; charset=utf-8"
				}, _validationJSON("body", noteGet(B)))
			end
			ctx.body = B
		end

		let pack = _newResponder(null)
		let call = _compose(r.call)

		let t = try(fun(_: Null) do
			call(rq, pack.api, ctx)
		end)
		if not t.ok then
			ctx._errorDump = "panic"
			return {
				status: 500,
				headers: {"Content-Type": "text/plain"},
				body: "internal server error"
			}
		end

		{
			status: pack.res.status,
			headers: pack.res.headers,
			body: pack.res.body
		}
	end

	let _preflight = fun(req: Request) -> {ok!: Bool} do
		let m = _matchLocal(req.method, req.url.path)
		if m == null then
			return {ok: false}
		end
		let r = m.route
		let params = if mapHas(m, "params") and m.params != null then
			m.params
		else
			{}
		end
		let C = if mapHas(r, "contract") then
			r.contract
		else
			{}
		end
		let rq = {
			method: req.method,
			url: req.url,
			headers: req.headers,
			body: "",
			pathParams: params
		}
		if mapHas(C, "path") and C.path != null then
			let P = _bindPath(rq.pathParams, C.path)
			if P == null then
				return {ok: false}
			end
		end
		if mapHas(C, "query") and C.query != null then
			let Q = _bindQueryFromReq(rq, C.query)
			if Q == null then
				return {ok: false}
			end
		end
		{ok: true}
	end

	let api = {
		__kind: "nethttp.router",
		# route — Register a value route that returns a buffered result.
		# 
		# Args:
		# method: Str — HTTP method (GET/POST/...).
		# pattern: Str — Path pattern (supports {param} and {*catch}).
		# c: Contract — Optional contract (binding/validation).
		# handler: Request -> {} -> Any — Returns Any or _Response/_HttpError.
		# Return:
		# Bool — true on success.
		route: fun(method: Str, pattern: Str, c: Contract, handler: Request -> {} -> Any) -> Bool do
			let parts = _splitPath(pattern)
			let stream = fun(req: Request, res: Responder, ctx: {}) -> Null do
				let r = try(fun(_: Null) do
					handler(req, ctx)
				end)
				if not r.ok then
					res.status(500).setHeader("Content-Type", "text/plain")
					let _ = res.write("internal server error")
					let _ = res.end(null)
					return
				end
				let out = r.value
				if out == null then
					res.status(500).setHeader("Content-Type", "text/plain")
					let _ = res.write("handler returned null")
					let _ = res.end(null)
					return
				end
				if isType(out, _HttpError) then
					let hdrs = if mapHas(out, "headers") and out.headers != null then
						out.headers
					else
						{}
					end
					let rt = if mapHas(out, "body") and out.body != null then
						out.body
					else
						""
					end
					for [k, v] in hdrs do
						res.setHeader(k, v)
					end
					res.status(out.status)
					let _ = res.write(rt)
					let _ = res.end(null)
					return
				end
				if isType(out, _Response) then
					if mapHas(c, "responses") and c.responses != null then
						let payload = if mapHas(out, "body") then
							jsonParse(out.body)
						else
							out
						end
						if not _checkResponse(out.status, payload, c.responses) then
							res.status(500).setHeader("Content-Type", "application/json")
							let _ = res.write(_validationJSON("response", "response-validation failed"))
							let _ = res.end(null)
							return
						end
					end
					for [k, v] in out.headers do
						res.setHeader(k, v)
					end
					res.status(out.status)
					let _ = res.write(out.body)
					let _ = res.end(null)
					return
				end
				let js = _json(out)
				if js == null then
					res.status(500).setHeader("Content-Type", "text/plain")
					let _ = res.write("json encode error")
					let _ = res.end(null)
					return
				end
				if mapHas(c, "responses") and c.responses != null then
					if not _checkResponse(200, out, c.responses) then
						res.status(500).setHeader("Content-Type", "application/json")
						let _ = res.write(_validationJSON("response", "response-validation failed"))
						let _ = res.end(null)
						return
					end
				end
				res.setHeader("Content-Type", "application/json; charset=utf-8").status(200)
				let _ = res.write(js)
				let _ = res.end(null)
				null
			end
			_specRoutes = _specRoutes + [
				_toRouteSpec(method, pattern, "value", c)
			]
			_routes = _routes + [
				_makeRoute(method, pattern, stream, "value", c)
			]
			true
		end,
		# routeStream — Register a streaming route.
		# 
		# Args:
		# method: Str — HTTP method.
		# pattern: Str — Path pattern.
		# c: Contract — Optional contract.
		# handler: Request -> Responder -> {} -> Null — Performs streaming writes.
		# Return:
		# Bool — true on success.
		routeStream: fun(method: Str, pattern: Str, c: Contract, handler: Request -> Responder -> {} -> Null) -> Bool do
			let stream = fun(req: Request, res: Responder, ctx: {}) -> Null do
				let r = try(fun(_: Null) do
					handler(req, res, ctx)
				end)
				if not r.ok then
					_setErr(ctx, r)
					res.status(500).setHeader("Content-Type", "text/plain")
					let _ = res.write("internal server error")
					let _ = res.end(null)
				end
				null
			end
			_specRoutes = _specRoutes + [
				_toRouteSpec(method, pattern, "stream", c)
			]
			_routes = _routes + [
				_makeRoute(method, pattern, stream, "stream", c)
			]
			true
		end,
		# mount — Mount a child router at a fixed prefix.
		# 
		# Args:
		# prefix: Str — Path prefix (e.g., "/v1").
		# child: {} — Router returned by router().
		# Return:
		# Bool — true on success.
		mount: fun(prefix: Str, child: {}) -> Bool do
			_mounts = _mounts + [
				{prefixParts: _splitPath(prefix), child: child}
			]
			true
		end,
		# use — Register a streaming-friendly middleware.
		# 
		# Args:
		# mw: _Mw — Middleware function.
		# Return:
		# Bool — true on success.
		use: fun(mw: _Mw) -> Bool do
			_mws = _mws + [mw]
			true
		end,
		# _entryCall — Buffered entrypoint used by the server and tests.
		_entryCall: fun(req: Request) -> _Response do
			_entryCore(req)
		end,
		# _entryCallStream — Streaming entrypoint for server-side use.
		_entryCallStream: fun(req: Request, conn, opts: {}) -> {}? do
			let m = _matchLocal(req.method, req.url.path)
			if m == null then
				return
			end
			let r = m.route
			if r.style != "stream" then
				return
			end

			let params = if mapHas(m, "params") and m.params != null then
				m.params
			else
				{}
			end
			let C = if mapHas(r, "contract") then
				r.contract
			else
				{}
			end
			let rq = {
				method: req.method,
				url: req.url,
				headers: req.headers,
				body: req.body,
				pathParams: params
			}
			let ctx = {req: rq}

			if mapHas(C, "path") and C.path != null then
				let P = _bindPath(rq.pathParams, C.path)
				if P == null then
					ctx._errorDump = "validation error in path: " + str(noteGet(P))
					_emitStreamViaMw(rq, ctx, conn, opts, 422, {
						"Content-Type": "application/json; charset=utf-8"
					}, _validationJSON("path", noteGet(P)))
					return {streamed: true}
				end
				ctx.path = P
			end
			if mapHas(C, "query") and C.query != null then
				let Q = _bindQueryFromReq(rq, C.query)
				if Q == null then
					ctx._errorDump = "validation error in query: " + str(noteGet(Q))
					_emitStreamViaMw(rq, ctx, conn, opts, 422, {
						"Content-Type": "application/json; charset=utf-8"
					}, _validationJSON("query", noteGet(Q)))
					return {streamed: true}
				end
				ctx.query = Q
			end
			if mapHas(C, "body") and C.body != null then
				let B = _bindBody(rq, C.body)
				if B == null then
					ctx._errorDump = "validation error in body: " + str(noteGet(B))
					_emitStreamViaMw(rq, ctx, conn, opts, 422, {
						"Content-Type": "application/json; charset=utf-8"
					}, _validationJSON("body", noteGet(B)))
					return {streamed: true}
				end
				ctx.body = B
			end

			let sock = _newSockResponder(conn, opts)
			let call = _compose(r.call)
			let t = try(fun(_: Null) do
				call(rq, sock.api, ctx)
			end)
			if not t.ok then
				_setErr(ctx, t)
				let _ = sock.api.status(500).setHeader("Content-Type", "text/plain").write("internal server error")
				let _ = sock.api.end(null)
				return {streamed: true}
			end
			let _ = sock.api.end(null)
			{streamed: true}
		end,
		# _dump — Internal debugging: returns routes and middleware stacks.
		_dump: fun(_: Null) -> {} do
			{routes: _routes, mws: _mws}
		end,
		# _preflight — Lightweight contract-only check used for 100-continue.
		_preflight: fun(req: Request) -> {ok!: Bool} do
			_preflight(req)
		end
	}
	if docs.enabled then
		api.route("GET", docs.path, contract({responses: {"200": type Any}}), fun(req: Request, ctx: {}) do
			json(200, _buildSpec(null))
		end)
	end

	api
end

# =========================
# Response validation (contract responses)
# =========================

let _checkResponse = fun(status: Int, out, respMap: {}) -> Bool do
	if respMap == null then
		return true
	end
	if not mapHas(respMap, str(status)) and not mapHas(respMap, status) then
		return true
	end
	let T = if mapHas(respMap, str(status)) then
		respMap[str(status)]
	else
		respMap[status]
	end
	if not isType(T, type Type) then
		return true
	end
	isType(out, T)
end

# =========================
# Test client (buffered)
# =========================

# testClient — Build a buffered test client for a router.
# 
# Summary:
# Allows issuing synthetic requests against a router without sockets.
# 
# Args:
# r: {} — Router returned by router().
# Return:
# {} — {call!: (req:{method?:Str, path?:Str, headers?:{}, body?:Str}) -> {status:Int, headers:{}, body:Str}}
let testClient = fun(r: {}) -> {} do
	let call = fun(req: {}) -> {} do
		let method = if mapHas(req, "method") and req.method != null then
			req.method
		else
			"GET"
		end
		let pathStr = if mapHas(req, "path") and req.path != null then
			req.path
		else
			"/"
		end
		let headers = if mapHas(req, "headers") and req.headers != null then
			req.headers
		else
			{}
		end
		let body = if mapHas(req, "body") and req.body != null then
			req.body
		else
			""
		end

		let q = ""
		let p = pathStr
		let parts = split(pathStr, "?")
		if len(parts) >= 2 then
			p = parts[0]
			q = join(slice(parts, 1, len(parts)), "?")
		end

		let RQ = {
			method: _normMethod(method),
			url: _makeURL(p, q),
			headers: headers,
			body: body,
			pathParams: {}
		}
		let res = r._entryCall(RQ)
		{
			status: res.status,
			headers: res.headers,
			body: res.body
		}
	end
	{call: call}
end