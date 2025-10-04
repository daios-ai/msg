# nethttp — streaming-first HTTP server for MindScript
#
# Design goals (LESS IS MORE):
# - Single streaming core (Request -> Responder -> Null), middleware-friendly
# - Route-time contracts: path / query / body / responses
# - Pre-bind path+query+body before handler (422 on failure)
# - Value routes are sugar over streaming core
# - Auto OpenAPI from the same contracts (minimal)
# - Internals never return null on success; boundary helpers may return annotated null
#
# Public surface:
#   router(opts?), route(), routeStream(), mount(), contract(), testClient()
#   serve(), shutdown()
#   mwRecover(), mwRequestID(), mwTimeout(), mwCors(), mwAccessLog()
#
# Docs (FastAPI-style):
# - Full OpenAPI 3.1 is generated automatically and exposed at /openapi.json by default.
# - Disable or change the path via router({docs:{enabled:false|true, path, info, servers, opts}}).
#
# Streaming & backpressure:
# - True streaming responses for stream routes via socket-backed Responder
# - Chunked transfer when length unknown; Content-Length when known
# - Backpressure honored via socket writes (no in-memory growth)
# - Request: Transfer-Encoding: chunked still rejected (documented)
# - Expect: 100-continue preflight (route shape / limits) before reading body


let oapi = import("openapi")



# =========================
# Public types (data-only)
# =========================

# HTTP request value passed to handlers.
#
# Fields:
#   method: Str — Uppercase HTTP method, e.g. "GET".
#   url: {scheme: Str?, host: Str?, path: Str, query: Str?} — Parsed request-target parts.
#   headers: {} — Map of request headers (case-preserving on ingest).
#   body: Any — Raw request body as Str (server) or readable handle (test/client).
#   pathParams: {}? — Raw `{name}` path parameters (strings), prior to contract binding.
# Return: type
let Request = type {
	method: Str,
	url: {scheme: Str?, host: Str?, path: Str, query: Str?},
	headers: {},
	body: Any,
	pathParams: {}?
}

# Streaming responder interface for handlers.
#
# Methods:
#   status(code: Int) -> Responder — Set status (before head write); chainable.
#   setHeader(k: Str, v: Str) -> Responder — Set header (before head write); chainable.
#   write(chunk: Str) -> Int? — Write a body chunk; returns bytes written or null # <write failed>.
#   flush(_: Null) -> Bool? — Flush downstream buffers; null # <write failed> on error.
#   end(_: Null) -> Bool? — Finish response; null # <write failed> on error.
# Behavior:
#   - For streaming routes, head is emitted lazily on first write/status/header.
#   - If Content-Length is absent, Transfer-Encoding: chunked is used.
# Return: type
let Responder = type {
	status: Int -> Responder,
	setHeader: Str -> Str -> Responder,
	write: Str -> Int?,
	flush: Null -> Bool?,
	end: Null -> Bool?
}

let _Response = type {status!: Int, headers!: {}, body!: Str}

let _HttpError = type {
	__http_error__!: Bool,
	status!: Int,
	body!: Str,
	headers!: {}
}

# A marker wrapper for route contracts (path/query/body/responses).
#
# Purpose:
#   Use contract({...}) to declare schemas for binding/validation at route time.
# Return: type (opaque)
let Contract = type {}

let _Ctx = type {req: Request, path: {}, query: {}, body: Any, deadlineMs: Int?}

let _StreamFn = type Request -> Responder -> {} -> Null
let _Mw = type _StreamFn -> _StreamFn

# =========================
# Utilities
# =========================

let _normMethod = fun(m: Str) -> Str do
	toUpper(m)
end

let _hasPrefix = fun(s: Str, prefix: Str) -> Bool do
	let n = len(prefix)
	if len(s) < n then
		return false
	end
	substr(s, 0, n) == prefix
end

let _splitPath = fun(p: Str) -> [Str] do
	let s = if len(p) == 0 or substr(p, 0, 1) != "/" then "/" + p else p end
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
	{scheme: null, host: null, path: path, query: query}
end

let _json = fun(v) -> Str? do
	let s = jsonStringify(v)
	if s != null then
		return s
	end
	null # <json stringify failed>
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

# Wrap a route contract (marker).
#
# Summary:
#   Returns the same object, labeled as a Contract for readability at callsites.
# Args:
#   c: Contract — A map describing path/query/body/responses (types).
# Return: Contract
let contract = fun(c: Contract) -> Contract do
	c
end

# =========================
# Error helpers (public)
# =========================

# Create a structured HTTP error response for value routes.
#
# Summary:
#   Returns a special object recognized by router value handlers to emit the given status and body.
# Args:
#   status: Int — HTTP status code.
#   message: Str — Plaintext/error message body.
# Return: {} — Structured error marker (not a panic).
let raise = fun(status: Int, message: Str) -> {} do
	{__http_error__: true, status: status, body: message, headers: {}}
end

# Create a structured HTTP error with custom headers.
#
# Args:
#   status: Int — HTTP status code.
#   message: Str — Error message body.
#   headers: {} — Additional headers to include in the response.
# Return: {} — Structured error marker.
let raiseWithHeaders = fun(status: Int, message: Str, headers: {}) -> {} do
	{__http_error__: true, status: status, body: message, headers: headers}
end

# =========================
# Binding helpers (internal)
# =========================

let _rawBody = fun(req: Request) -> Str? do
	if isType(req.body, type Str) then
		return req.body
	end
	let s = readAll(req.body)
	if s == null then
		return null # <read body failed>
	end
	s
end

let _coerceScalar = fun(s: Str, T: Type) -> Any? do
	if isSubtype(T, type Str) then
		let pj = jsonParse(s)
		return if pj != null and isType(pj, type Str) then pj else s end
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
		return null # <bad int>
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
		return null # <bad num>
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
		return null # <bad bool>
	end
	null # <unsupported scalar type>
end

let _bindQueryMap = fun(m: {}, T: Type) -> Any? do
	let rt = reflect(T)
	if rt == null then
		return null # <reflect failed>
	end
	let node = rt[1]
	if node[0] != "map" then
		return null # <only object types supported for query>
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
		let vs = if mapHas(m, k) then m[k] else null end

		if vs == null or len(vs) == 0 then
			if tag == "pair!" then
				return null # <missing key: {k}>
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
							return null # <bad element for {k}>
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
					return null # <bad element for {k}>
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
			return null # <coercion failed: {k}>
		end
		out[k] = v
		i = i + 1
	end

	if isType(out, T) then
		return out
	end
	null # <query validation failed>
end

let _bindQueryFromReq = fun(req: Request, TQ: Type) -> Any? do
	let qm = urlQueryParse(if req.url.query == null then "" else req.url.query end)
	if qm == null then
		return null # <invalid query string>
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
		return null # <reflect failed>
	end
	let node = rt[1]
	if node[0] != "map" then
		return null # <path must be object type>
	end
	let out = {}
	let entries = slice(node, 1, len(node))
	let i = 0
	while i < len(entries) do
		let e = entries[i]
		let tag = e[0]
		let k = e[1][1]
		let Tk = reify(["type", e[2]])
		let raw = if mapHas(ps, k) then ps[k] else null end
		if raw == null then
			if tag == "pair!" then
				return null # <missing key: {k}>
			end
			i = i + 1
			continue
		end
		let v = _coerceScalar(raw, Tk)
		if v == null then
			return null # <coercion failed: {k}>
		end
		out[k] = v
		i = i + 1
	end
	if isType(out, TP) then
		return out
	end
	null # <path validation failed>
end

let _bindBody = fun(req: Request, TB: Type) -> Any? do
	let raw = _rawBody(req)
	if raw == null then
		return null # <read body failed>
	end
	let v = jsonParse(raw)
	if v == null then
		return null # <invalid json>
	end
	if not isType(v, TB) then
		return null # <validation failed: expected body to satisfy {str(TB)}>
	end
	v
end

let _validationJSON = fun(where: Str, reason: Str) -> Str do
	let obj = {error: "validation", detail: [{in: where, reason: reason}]}
	let s = _json(obj)
	if s == null then
		return "{\"error\":\"validation\"}"
	end
	s
end

# =========================
# Response helpers (for value handlers)
# =========================

# Send JSON from a value route.
#
# Summary:
#   Serializes the value to JSON and returns a buffered response with Content-Type set.
# Args:
#   status: Int — HTTP status code.
#   value: Any — Value to encode as JSON.
# Return: {} — Buffered response or {status:500,"json encode error"} on encode failure.
let json = fun(status: Int, value) -> {} do
	let s = _json(value)
	if s == null then
		return {status: 500, headers: {"Content-Type": "text/plain"}, body: "json encode error"}
	end
	{status: status, headers: {"Content-Type": "application/json; charset=utf-8"}, body: s}
end

# Send plaintext from a value route.
#
# Args:
#   status: Int — HTTP status code.
#   s: Str — Body.
# Return: {} — Buffered response with text/plain Content-Type.
let text = fun(status: Int, s: Str) -> {} do
	{status: status, headers: {"Content-Type": "text/plain"}, body: s}
end

# Send 204 No Content from a value route.
#
# Args:
#   _: Null — Unused.
# Return: {} — Buffered response with empty body.
let noContent = fun(_: Null) -> {} do
	{status: 204, headers: {}, body: ""}
end

# Send an internal redirect (path-only) from a value route.
#
# Summary:
#   Only absolute-path locations are allowed (e.g., "/new"). External absolute URLs are rejected with 400.
# Args:
#   status: Int — Redirect code (e.g., 302).
#   location: Str — Absolute path starting with "/".
# Return: {} — Buffered response or {status:400,"bad redirect"}.
let redirect = fun(status: Int, location: Str) -> {} do
	if len(location) == 0 or substr(location, 0, 1) != "/" then
		return {status: 400, headers: {"Content-Type": "text/plain"}, body: "bad redirect"}
	end
	{status: status, headers: {Location: location}, body: ""}
end

# =========================
# Buffered responder (for value routes, tests)
# =========================

let _newResponder = fun() -> {res: _Response, api: Responder} do
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

let _newSockResponder = fun(conn, opts: {}) -> {api: Responder, state: {}} do
	let st = {status: 200, headers: {}, wroteHead: false, chunked: false, ended: false}
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
		let head = sprintf("HTTP/1.1 %d %s\r\n", [int(st.status), _reason(int(st.status))])
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
		if not _emitHead() then
			return null # <write head failed>
		end
		if st.chunked then
			let pre = sprintf("%x\r\n", [len(chunk)])
			if write(conn, pre) == null then
				return null # <write failed>
			end
			if write(conn, chunk) == null then
				return null # <write failed>
			end
			if write(conn, "\r\n") == null then
				return null # <write failed>
			end
			let _ = flush(conn)
			len(chunk)
		else
			let n = write(conn, chunk)
			if n == null then
				return null # <write failed>
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
			if not _emitHead() then
				return true
			end
		end
		if st.chunked then
			if write(conn, "0\r\n\r\n") == null then
				return null # <write failed>
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

let _mkRoute = fun(method: Str, pattern: Str, call: _StreamFn, style: Str, c: Contract) -> {} do
	let m = _normMethod(method)
	let isPrefix = len(pattern) > 1 and substr(pattern, len(pattern) - 1, len(pattern)) == "/"
	{method: m, pattern: pattern, parts: _splitPath(pattern), call: call, style: style, contract: c, prefix: isPrefix}
end

let _toRouteSpec = fun(method: Str, pattern: Str, style: Str, c: Contract) -> {} do
	let out = {
		method: _normMethod(method),
		pattern: pattern,
		style: style,
		summary: if mapHas(c, "summary") and c.summary != null then c.summary else null end,
		description: if mapHas(c, "description") and c.description != null then c.description else null end,
		tags: if mapHas(c, "tags") and c.tags != null then c.tags else null end,
		deprecated: if mapHas(c, "deprecated") and c.deprecated != null then bool(c.deprecated) else null end,
		operationId: if mapHas(c, "operationId") and c.operationId != null then c.operationId else null end,
		security: if mapHas(c, "security") and c.security != null then c.security else null end,
		x: if mapHas(c, "x") and c.x != null then c.x else null end	
	}
	# Only attach 'contract' when it has meaningful content; empty {} would fail RouteSpec type.
	if mapHas(c, "path") or mapHas(c, "query") or mapHas(c, "headers") or mapHas(c, "cookies") or mapHas(c, "body") or mapHas(c, "responses") then
		out.contract = c
	end
	out
end


let _pathMatch = fun(parts: [Str], reqSegs: [Str]) -> {}? do
	let nParts = len(parts)
	let nReq = len(reqSegs)
	if nParts == 0 then
		return if nReq == 0 then {params: {}, matched: 0} else null end
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
	let limit = if hasCatch then last else nParts end
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
	{params: params, matched: if hasCatch then last else nParts end}
end



# Create a new router.
#
# Summary:
#   Builds a closure-backed router with registration for value/stream routes, middleware, and mounts.
# Methods on returned object:
#   route(method: Str, pattern: Str, c: Contract, handler) -> Bool
#   routeStream(method: Str, pattern: Str, c: Contract, handler) -> Bool
#   mount(prefix: Str, childRouter) -> Bool
#   use(mw: _Mw) -> Bool
#   _entryCall(req: Request) -> {status!: Int, headers!: {}, body!: Str}
#   _entryCallStream(req: Request, conn, opts: {}) -> {}?
# Args:
#   _: Null — Unused.
# Return: {} — Router instance.
let router = fun(opts: {}?) -> {} do
	let o = if opts == null then {} else opts end
	let dIn = if mapHas(o, "docs") and o.docs != null then o.docs else {} end
	let docs = {
		enabled: if mapHas(dIn, "enabled") and dIn.enabled != null then bool(dIn.enabled) else true end,
		path: if mapHas(dIn, "path") and dIn.path != null then dIn.path else "/openapi.json" end,
		info: if mapHas(dIn, "info") and dIn.info != null then dIn.info else {title: "API", version: "0.1.0"} end,
		servers: if mapHas(dIn, "servers") and dIn.servers != null then dIn.servers else [] end,
		opts: if mapHas(dIn, "opts") and dIn.opts != null then dIn.opts else {} end
	}

	let _routes = []
	let _mws = []
	let _mounts = []
	let _specRoutes = []

	let _buildSpec = fun(_: Null) -> {} do
		oapi.spec(_specRoutes, docs.info, docs.servers, docs.opts)
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
			let hit = _pathMatch(r.parts, segs) != null or (mapHas(r, "prefix") and r.prefix == true and _startsWith(segs, r.parts))
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
				url: {scheme: req.url.scheme, host: req.url.host, path: subPath, query: req.url.query},
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
			if _normMethod(req.method) == "OPTIONS" then
				let allowed = _allowedMethodsLocal(req.url.path)
				if len(allowed) > 0 then
					return {status: 204, headers: {"Allow": join(allowed, ", ")}, body: ""}
				end
				# If any middleware is installed (e.g., CORS), honor generic preflight.
				if len(_mws) > 0 then
					return {status: 204, headers: {}, body: ""}
				end
				return {status: 404, headers: {"Content-Type": "text/plain"}, body: "not found"}
			end
			if _pathExistsLocal(req.url.path) then
				let allowed405 = _allowedMethodsLocal(req.url.path)
				return {status: 405, headers: {"Content-Type": "text/plain", "Allow": join(allowed405, ", ")}, body: "method not allowed"}
			end
			return {status: 404, headers: {"Content-Type": "text/plain"}, body: "not found"}
		end

		let r = m.route
		let params = if mapHas(m, "params") and m.params != null then m.params else {} end
		let C = if mapHas(r, "contract") then r.contract else {} end

		let rq = {method: req.method, url: req.url, headers: req.headers, body: req.body, pathParams: params}
		let ctx = {req: rq}

		if mapHas(C, "path") and C.path != null then
			let P = _bindPath(rq.pathParams, C.path)
			if P == null then
				return {status: 422, headers: {"Content-Type": "application/json; charset=utf-8"}, body: _validationJSON("path", noteGet(P))}
			end
			ctx.path = P
		end
		if mapHas(C, "query") and C.query != null then
			let Q = _bindQueryFromReq(rq, C.query)
			if Q == null then
				return {status: 422, headers: {"Content-Type": "application/json; charset=utf-8"}, body: _validationJSON("query", noteGet(Q))}
			end
			ctx.query = Q
		end
		if mapHas(C, "body") and C.body != null then
			let B = _bindBody(rq, C.body)
			if B == null then
				return {status: 422, headers: {"Content-Type": "application/json; charset=utf-8"}, body: _validationJSON("body", noteGet(B))}
			end
			ctx.body = B
		end

		let pack = _newResponder()
		let call = _compose(r.call)

		let t = try(fun(_: Null) do call(rq, pack.api, ctx) end)
		if not t.ok then
			return {status: 500, headers: {"Content-Type": "text/plain"}, body: "internal server error"}
		end

		{status: pack.res.status, headers: pack.res.headers, body: pack.res.body}
	end

	# Preflight used by server for Expect: 100-continue (no body binding here)
	let _preflight = fun(req: Request) -> {ok!: Bool} do
		let m = _matchLocal(req.method, req.url.path)
		if m == null then
			return {ok: false}
		end
		let r = m.route
		let params = if mapHas(m, "params") and m.params != null then m.params else {} end
		let C = if mapHas(r, "contract") then r.contract else {} end
		let rq = {method: req.method, url: req.url, headers: req.headers, body: "", pathParams: params}
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

		# Register a buffered (value) route with contract binding.
		#
		# Summary:
		#   Handler returns either a buffered {_Response}, a structured http error (raise*),
		#   any JSON-encodable value (auto 200 + application/json), or null (treated as 500).
		#   If 'responses' is present in contract, the response payload is type-checked.
		# Args:
		#   method: Str — "GET", "POST", ...
		#   pattern: Str — e.g., "/users/{id}", "/v1/", "{*tail}" at end for catch-all.
		#   c: Contract — {path?:Type, query?:Type, body?:Type, responses?:{code->Type}}
		#   handler: (req: Request, ctx: {}) -> Any — Value-returning handler.
		# Return: Bool — true when registered.
		route: fun(method: Str, pattern: Str, c: Contract, handler) -> Bool do
			let parts = _splitPath(pattern)
			let stream = fun(req: Request, res: Responder, ctx: {}) -> Null do
				let r = try(fun(_: Null) do handler(req, ctx) end)
				if not r.ok then
					res.status(500).setHeader("Content-Type", "text/plain")
					let _ = res.write("internal server error")
					let _ = res.end()
					return
				end
				let out = r.value
				if out == null then
					res.status(500).setHeader("Content-Type", "text/plain")
					let _ = res.write("handler returned null")
					let _ = res.end()
					return
				end
				if isType(out, _HttpError) then
					let hdrs = if mapHas(out, "headers") and out.headers != null then out.headers else {} end
					let rt = if mapHas(out, "body") and out.body != null then out.body else "" end
					for [k, v] in hdrs do
						res.setHeader(k, v)
					end
					res.status(out.status)
					let _ = res.write(rt)
					let _ = res.end()
					return
				end
				if isType(out, _Response) then
					if mapHas(c, "responses") and c.responses != null then
						let payload = if mapHas(out, "body") then jsonParse(out.body) else out end
						if not _checkResponse(out.status, payload, c.responses) then
							res.status(500).setHeader("Content-Type", "application/json")
							let _ = res.write(_validationJSON("response", "response-validation failed"))
							let _ = res.end()
							return
						end
					end
					for [k, v] in out.headers do
						res.setHeader(k, v)
					end
					res.status(out.status)
					let _ = res.write(out.body)
					let _ = res.end()
					return
				end
				let js = _json(out)
				if js == null then
					res.status(500).setHeader("Content-Type", "text/plain")
					let _ = res.write("json encode error")
					let _ = res.end()
					return
				end
				if mapHas(c, "responses") and c.responses != null then
					if not _checkResponse(200, out, c.responses) then
						res.status(500).setHeader("Content-Type", "application/json")
						let _ = res.write(_validationJSON("response", "response-validation failed"))
						let _ = res.end()
						return
					end
				end
				res.setHeader("Content-Type", "application/json; charset=utf-8").status(200)
				let _ = res.write(js)
				let _ = res.end()
				null
			end
			_specRoutes = _specRoutes + [_toRouteSpec(method, pattern, "value", c)]
			_routes = _routes + [_mkRoute(method, pattern, stream, "value", c)]
			true
		end,

		# Register a streaming route with contract binding.
		#
		# Summary:
		#   Handler receives a streaming Responder; must write/end explicitly.
		#   Note: HEAD is NOT auto-mapped to GET for streaming routes (405 unless registered).
		# Args:
		#   method: Str — HTTP method.
		#   pattern: Str — Route pattern (may include {name}, {*tail}).
		#   c: Contract — Path/query/body types and optional responses (ignored for streaming).
		#   handler: (req: Request, res: Responder, ctx: {}) -> Null
		# Return: Bool — true when registered.
		routeStream: fun(method: Str, pattern: Str, c: Contract, handler) -> Bool do
			let stream = fun(req: Request, res: Responder, ctx: {}) -> Null do
				let r = try(fun(_: Null) do handler(req, res, ctx) end)
				if not r.ok then
					res.status(500).setHeader("Content-Type", "text/plain")
					let _ = res.write("internal server error")
					let _ = res.end()
				end
				null
			end
			_specRoutes = _specRoutes + [_toRouteSpec(method, pattern, "stream", c)]
			_routes = _routes + [_mkRoute(method, pattern, stream, "stream", c)]
			true
		end,

		# Mount a child router at a path prefix.
		#
		# Args:
		#   prefix: Str — e.g., "/api", "/v1/users".
		#   child: {} — Another router() instance.
		# Return: Bool — true when mounted.
		mount: fun(prefix: Str, child) -> Bool do
			_mounts = _mounts + [{prefixParts: _splitPath(prefix), child: child}]
			true
		end,

		# Install middleware (applied outer-to-inner in registration order).
		#
		# Args:
		#   mw: _Mw — Middleware transformer: next:_StreamFn -> _StreamFn.
		# Return: Bool — true when installed.
		use: fun(mw: _Mw) -> Bool do
			_mws = _mws + [mw]
			true
		end,

		_entryCall: fun(req: Request) -> _Response do
			_entryCore(req)
		end,

		_entryCallStream: fun(req: Request, conn, opts: {}) -> {}? do
			let m = _matchLocal(req.method, req.url.path)
			if m == null then
				return null
			end
			let r = m.route
			let params = if mapHas(m, "params") and m.params != null then m.params else {} end
			let C = if mapHas(r, "contract") then r.contract else {} end
			let rq = {method: req.method, url: req.url, headers: req.headers, body: req.body, pathParams: params}
			let ctx = {req: rq}
			if mapHas(C, "path") and C.path != null then
				let P = _bindPath(rq.pathParams, C.path)
				if P == null then
					let s = _validationJSON("path", noteGet(P))
					let _ = write(conn, sprintf("HTTP/1.1 422 %s\r\nContent-Length: %d\r\nContent-Type: application/json; charset=utf-8\r\n\r\n%s", [_reason(422), len(s), s]))
					let _ = flush(conn)
					return {streamed: true}
				end
				ctx.path = P
			end
			if mapHas(C, "query") and C.query != null then
				let Q = _bindQueryFromReq(rq, C.query)
				if Q == null then
					let s = _validationJSON("query", noteGet(Q))
					let _ = write(conn, sprintf("HTTP/1.1 422 %s\r\nContent-Length: %d\r\nContent-Type: application/json; charset=utf-8\r\n\r\n%s", [_reason(422), len(s), s]))
					let _ = flush(conn)
					return {streamed: true}
				end
				ctx.query = Q
			end
			if mapHas(C, "body") and C.body != null then
				let B = _bindBody(rq, C.body)
				if B == null then
					let s = _validationJSON("body", noteGet(B))
					let _ = write(conn, sprintf("HTTP/1.1 422 %s\r\nContent-Length: %d\r\nContent-Type: application/json; charset=utf-8\r\n\r\n%s", [_reason(422), len(s), s]))
					let _ = flush(conn)
					return {streamed: true}
				end
				ctx.body = B
			end
			if r.style == "stream" then
				let sock = _newSockResponder(conn, opts)
				let call = _compose(r.call)
				let t = try(fun(_: Null) do call(rq, sock.api, ctx) end)
				if not t.ok then
					let _ = write(conn, sprintf("HTTP/1.1 500 %s\r\nContent-Length: 21\r\nContent-Type: text/plain\r\n\r\ninternal server error", [_reason(500)]))
					let _ = flush(conn)
					return {streamed: true}
				end
				let _ = sock.api.end()
				return {streamed: true}
			end
			_entryCore(req)
		end,

		_dump: fun(_: Null) -> {} do
			{routes: _routes, mws: _mws}
		end,

		_preflight: fun(req: Request) -> {ok!: Bool} do
			_preflight(req)
		end
	}
	if docs.enabled then
	api.route("GET", docs.path, contract({responses: {"200": type Any}}), fun(req, ctx) do
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
	let T = if mapHas(respMap, str(status)) then respMap[str(status)] else respMap[status] end
	if not isType(T, type Type) then
		return true
	end
	isType(out, T)
end

# =========================
# Test client (buffered)
# =========================

# Create a buffered in-memory test client for a router.
#
# Summary:
#   Simulates HTTP calls without sockets; routes through the router’s entry points.
# Methods on return:
#   call(req: {method?:Str, path?:Str, headers?:{}, body?:Str}) -> {status:Int, headers:{}, body:Str}
# Args:
#   r: {} — Router instance created by router().
# Return: {} — {call: fun} test client.
let testClient = fun(r) -> {} do
	let call = fun(req: {}) -> {} do
		let method = if mapHas(req, "method") and req.method != null then req.method else "GET" end
		let pathStr = if mapHas(req, "path") and req.path != null then req.path else "/" end
		let headers = if mapHas(req, "headers") and req.headers != null then req.headers else {} end
		let body = if mapHas(req, "body") and req.body != null then req.body else "" end

		let q = ""
		let p = pathStr
		let parts = split(pathStr, "?")
		if len(parts) >= 2 then
			p = parts[0]
			q = join(slice(parts, 1, len(parts)), "?")
		end

		let R = {method: _normMethod(method), url: _makeURL(p, q), headers: headers, body: body, pathParams: {}}
		let res = r._entryCall(R)
		{status: res.status, headers: res.headers, body: res.body}
	end
	{call: call}
end

# =========================
# Middleware (streaming-friendly)
# =========================

# Recover middleware — converts handler panics to 500 responses.
#
# Summary:
#   Wraps next and catches panics (via try), returning "internal server error".
# Args:
#   _: Null — Unused.
# Return: _Mw — Middleware function.
let mwRecover = fun(_: Null) -> _Mw do
	return fun(next: _StreamFn) -> _StreamFn do
		return fun(req: Request, res: Responder, ctx: {}) -> Null do
			let r = try(fun(_: Null) do next(req, res, ctx) end)
			if not r.ok then
				res.status(500).setHeader("Content-Type", "text/plain")
				let _ = res.write("internal server error")
				let _ = res.end()
			end
			null
		end
	end
end

# Request ID middleware — injects/propagates a request identifier.
#
# Summary:
#   If the incoming header is present (case-insensitive), reuses it.
#   Otherwise generates one and echoes it on the response.
# Args:
#   headerName: Str — e.g., "X-Request-ID".
# Return: _Mw — Middleware function.
let mwRequestID = fun(headerName: Str) -> _Mw do
	let name = headerName
	return fun(next: _StreamFn) -> _StreamFn do
		return fun(req: Request, res: Responder, ctx: {}) -> Null do
			let seen = _getHeader(req.headers, name)
			let id = if seen != null then seen else sprintf("r-%d", [nowNanos()]) end
			req.headers[name] = id
			res.setHeader(name, id)
			next(req, res, ctx)
			null
		end
	end
end

# Timeout middleware — emits 504 if no bytes were written by the deadline.
#
# Summary:
#   Wraps the responder to defer first write; if time passes and nothing was written,
#   sends 504 timeout and prevents further writes.
# Args:
#   ms: Int — Deadline in milliseconds from request start.
# Return: _Mw — Middleware function.
let mwTimeout = fun(ms: Int) -> _Mw do
	let dur = ms
	return fun(next: _StreamFn) -> _StreamFn do
		return fun(req: Request, res: Responder, ctx: {}) -> Null do
			let deadline = nowMillis() + dur
			let closed = {v: false}
			let wrote = {v: false}
			ctx.deadlineMs = deadline

			let closeTimeout = fun(_: Null) -> Null do
				if closed.v then
					return
				end
				res.status(504).setHeader("Content-Type", "text/plain")
				let _ = res.write("timeout")
				let _ = res.end()
				closed.v = true
				null
			end

			let timedOut = fun(_: Null) -> Bool do
				nowMillis() > deadline
			end

			let proxy = {}
			proxy.status = fun(code: Int) -> Responder do
				if closed.v then
					return proxy
				end
				wrote.v = true
				res.status(code)
				proxy
			end
			proxy.setHeader = fun(k: Str, v: Str) -> Responder do
				if closed.v then
					return proxy
				end
				if timedOut() and not wrote.v then
					closeTimeout()
					return proxy
				end
				wrote.v = true
				res.setHeader(k, v)
				proxy
			end
			proxy.write = fun(chunk: Str) -> Int? do
				if closed.v then
					return 0
				end
				if timedOut() and not wrote.v then
					closeTimeout()
					return 0
				end
				wrote.v = true
				res.write(chunk)
			end
			proxy.flush = fun(_: Null) -> Bool? do
				if closed.v then
					return true
				end
				if timedOut() and not wrote.v then
					closeTimeout()
					return true
				end
				res.flush()
			end
			proxy.end = fun(_: Null) -> Bool? do
				if closed.v then
					return true
				end
				if timedOut() and not wrote.v then
					closeTimeout()
					return true
				end
				res.end()
			end

			next(req, proxy, ctx)
			if not closed.v and not wrote.v and timedOut() then
				closeTimeout()
			end
			null
		end
	end
end

# CORS middleware — adds Access-Control-* headers and handles OPTIONS.
#
# Summary:
#   Supports wildcard origins or credentials mode that reflects the Origin and sets Vary: Origin.
# Args:
#   opts: {} — {
#     origin?: Str|"*",
#     methods?: [Str],    # default allows common verbs
#     headers?: [Str],    # default "Content-Type, Authorization"
#     credentials?: Bool  # reflect Origin + Vary when true
#   }
# Return: _Mw — Middleware function.
let mwCors = fun(opts: {}) -> _Mw do
	let originOpt = if mapHas(opts, "origin") and opts.origin != null then opts.origin else "*" end
	let methods = if mapHas(opts, "methods") and opts.methods != null then join(opts.methods, ", ") else "GET, POST, PUT, PATCH, DELETE, OPTIONS" end
	let allowH = if mapHas(opts, "headers") and opts.headers != null then join(opts.headers, ", ") else "Content-Type, Authorization" end
	let credsOn = mapHas(opts, "credentials") and opts.credentials != null and bool(opts.credentials)

	return fun(next: _StreamFn) -> _StreamFn do
		return fun(req: Request, res: Responder, ctx: {}) -> Null do
			let reqOrigin = _getHeader(req.headers, "Origin")
			let allowOrigin =
				if credsOn then
					if originOpt == "*" then
						if reqOrigin != null then reqOrigin else "*" end
					else
						originOpt
					end
				else
					originOpt
				end
			res.setHeader("Access-Control-Allow-Origin", allowOrigin)
			res.setHeader("Access-Control-Allow-Methods", methods)
			res.setHeader("Access-Control-Allow-Headers", allowH)
			res.setHeader("Access-Control-Allow-Credentials", if credsOn then "true" else "false" end)
			if credsOn then
				res.setHeader("Vary", "Origin")
			end
			if req.method == "OPTIONS" then
				res.status(204)
				let _ = res.end()
				return
			end
			next(req, res, ctx)
			null
		end
	end
end

# Access log middleware — logs after response with status/bytes/duration.
#
# Summary:
#   Proxies status/write to measure bytes; writes a single line via printf.
# Args:
#   _: Null — Unused.
# Return: _Mw — Middleware function.
let mwAccessLog = fun(_: Null) -> _Mw do
	return fun(next: _StreamFn) -> _StreamFn do
		return fun(req: Request, res: Responder, ctx: {}) -> Null do
			let t0 = nowMillis()
			let status = {v: 200}
			let bytes = {v: 0}

			let orig = res
			let proxy = {}
			proxy.status = fun(code: Int) -> Responder do
				status.v = code
				orig.status(code)
				proxy
			end
			proxy.setHeader = fun(k: Str, v: Str) -> Responder do
				orig.setHeader(k, v)
				proxy
			end
			proxy.write = fun(chunk: Str) -> Int? do
				let n = orig.write(chunk)
				if n != null then
					bytes.v = bytes.v + int(n)
				end
				n
			end
			proxy.flush = fun(_: Null) -> Bool? do
				orig.flush()
			end
			proxy.end = fun(_: Null) -> Bool? do
				orig.end()
			end

			next(req, proxy, ctx)

			let dur = nowMillis() - t0
			printf("%s \"%s %s\" %d %d %dms\n", ["-", req.method, req.url.path, int(status.v), int(bytes.v), int(dur)])
			null
		end
	end
end

# =========================
# Server lifecycle (HTTP/1.1 MVP)
# =========================

let _ServerHandle = type {listener: Any, stop: {closed: Bool}, conns: [Any], opts: {}}

# Start an HTTP/1.1 server with a router.
#
# Summary:
#   Accepts TCP connections from 'listener', parses requests with limits, and dispatches
#   to the router. Supports Expect: 100-continue preflight and keep-alive.
# Args:
#   listener: Any — TCP listener handle (from netListen).
#   r: {} — Router instance built by router().
#   opts: {} — {
#     readHeaderTimeoutMs?: Int,
#     writeTimeoutMs?: Int,
#     idleTimeoutMs?: Int,
#     maxHeaderBytes?: Int = 8192,
#     maxHeaders?: Int?,
#     maxBodyBytes?: Int = 1048576,
#     maxStartLineBytes?: Int?,
#     addDateHeader?: Bool?
#   }
# Return: {}? — Server handle or null # <serve: ...> on configuration errors.
let serve = fun(listener, r, opts: {}) -> {}? do
	if listener == null then
		return null # <serve: missing listener>
	end
	if not mapHas(r, "_entryCall") then
		return null # <serve: not a router>
	end

	if not mapHas(opts, "readHeaderTimeoutMs") or opts.readHeaderTimeoutMs == null then opts.readHeaderTimeoutMs = 0 end
	if not mapHas(opts, "writeTimeoutMs") or opts.writeTimeoutMs == null then opts.writeTimeoutMs = 0 end
	if not mapHas(opts, "idleTimeoutMs") or opts.idleTimeoutMs == null then opts.idleTimeoutMs = 0 end
	if not mapHas(opts, "maxHeaderBytes") or opts.maxHeaderBytes == null then opts.maxHeaderBytes = 8192 end
	if not mapHas(opts, "maxBodyBytes") or opts.maxBodyBytes == null then opts.maxBodyBytes = 1048576 end

	let server = {listener: listener, stop: {closed: false}, conns: [], opts: opts}

	let _ = procSpawn(fun(_: Null) -> Null do
		while not server.stop.closed do
			let c = netAccept(listener)
			if c == null then
				continue
			end
			server.conns = server.conns + [c]
			procSpawn(fun(_: Null) -> Null do
				_connLoop(c, r, server)
			end)
		end
		null
	end)

	server
end

# Gracefully shut down a server, closing the listener and active connections.
#
# Args:
#   server: {}? — Handle returned by serve(); null is a no-op.
#   timeoutMs: Int? — Currently unused; reserved for future graceful drains.
# Return: Bool — true after resources are closed/cleared.
let shutdown = fun(server, timeoutMs: Int?) -> Bool do
	if server == null then
		return true
	end
	server.stop.closed = true
	close(server.listener)
	let i = 0
	while i < len(server.conns) do
		let c = server.conns[i]
		close(c)
		i = i + 1
	end
	server.conns = []
	true
end

# =========================
# HTTP/1.1 helpers (MVP)
# =========================

let _reason = fun(code: Int) -> Str do
	if code == 200 then return "OK" end
	if code == 204 then return "No Content" end
	if code == 302 then return "Found" end
	if code == 400 then return "Bad Request" end
	if code == 401 then return "Unauthorized" end
	if code == 404 then return "Not Found" end
	if code == 405 then return "Method Not Allowed" end
	if code == 413 then return "Payload Too Large" end
	if code == 414 then return "URI Too Long" end
	if code == 422 then return "Unprocessable Entity" end
	if code == 431 then return "Request Header Fields Too Large" end
	if code == 500 then return "Internal Server Error" end
	if code == 504 then return "Gateway Timeout" end
	"OK"
end

let _trimCR = fun(s: Str) -> Str do
	let n = len(s)
	if n > 0 and substr(s, n - 1, n) == "\r" then
		return substr(s, 0, n - 1)
	end
	s
end

let _validHeaderValue = fun(v: Str) -> Bool do
	match("\r|\n", v) == []
end

let _readLineLimited = fun(c, remaining: Int?) -> Str? do
	let line = readLine(c)
	if line == null then
		return
	end
	let s = _trimCR(line)
	if remaining != null then
		let left = int(remaining) - len(s) - 2
		if left < 0 then
			return
		end
	end
	s
end

let _readHeaders = fun(c, maxBytes: Int, maxCount: Int?) -> {}? do
	let headers = {}
	let budget = maxBytes
	let count = 0
	while true do
		let line = _readLineLimited(c, budget)
		if line == null then
			return
		end
		if len(line) == 0 then
			break
		end
		count = count + 1
		if maxCount != null and count > int(maxCount) then
			return {"__too_many__": true}
		end
		let parts = split(line, ":")
		if len(parts) < 2 then
			return
		end
		let key = parts[0]
		let value = strip(join(slice(parts, 1, len(parts)), ":"))
		if mapHas(headers, key) and toLower(key) == "content-length" and headers[key] != value then
			headers["__cl_conflict__"] = true
		end
		headers[key] = value
		budget = budget - len(line) - 2
	end
	headers
end

let _parseTarget = fun(t: Str) -> {} do
	if match("^[a-zA-Z][a-zA-Z0-9+.-]*://", t) != [] then
		return {absolute: true, path: "/", query: null}
	end
	let path = t
	let query = null
	let parts = split(t, "?")
	if len(parts) >= 2 then
		path = parts[0]
		query = join(slice(parts, 1, len(parts)), "?")
	end
	{path: path, query: query}
end

let _readBodyContentLength = fun(c, n: Int, cap: Int) -> Str? do
	let want = if n > cap then cap else n end
	let buf = ""
	let remaining = want
	while remaining > 0 do
		let chunk = readN(c, remaining)
		if chunk == null then
			return
		end
		buf = buf + chunk
		remaining = remaining - len(chunk)
		if len(chunk) == 0 then
			return
		end
	end
	buf
end

# Headers-only parse; body is read later (after 100-continue if any)
let _readRequest = fun(conn, opts: {}) -> {}? do
	let line = readLine(conn)
	if line == null then
		return
	end
	let reqLine = _trimCR(line)
	if len(reqLine) > 0 and mapHas(opts, "maxStartLineBytes") and opts.maxStartLineBytes != null and len(reqLine) > int(opts.maxStartLineBytes) then
		return {tooLong: true}
	end
	let parts = split(reqLine, " ")
	if len(parts) != 3 then
		return
	end
	let method = toUpper(parts[0])
	let target = parts[1]
	let proto = parts[2]
	if proto != "HTTP/1.1" then
		return
	end

	let maxHdr = int(opts.maxHeaderBytes)
	let maxCnt = if mapHas(opts, "maxHeaders") then opts.maxHeaders else null end
	let hdrs = _readHeaders(conn, maxHdr, maxCnt)
	if hdrs == null then
		return
	end
	if mapHas(hdrs, "__too_many__") then
		return {tooManyHeaders: true}
	end

	if mapHas(hdrs, "Transfer-Encoding") then
		return {badRequest: true}
	end
	if mapHas(hdrs, "__cl_conflict__") and bool(hdrs["__cl_conflict__"]) then
		return {badRequest: true}
	end

	let keepAlive = true
	let connV = _getHeader(hdrs, "Connection")
	if connV != null and toLower(connV) == "close" then
		keepAlive = false
	end

	let t = _parseTarget(target)
	if mapHas(t, "absolute") and t.absolute == true then
		return {badRequest: true}
	end

	let wantCL = -1
	if mapHas(hdrs, "Content-Length") then
		let n = int(hdrs["Content-Length"])
		if n == null or n < 0 then
			return
		end
		wantCL = n
	end

	{
		req: {method: method, url: {scheme: null, host: null, path: t.path, query: t.query}, headers: hdrs, body: "", pathParams: {}},
		wantCL: wantCL,
		keepAlive: keepAlive
	}
end

let _writeResponse = fun(conn, res: _Response, opts: {}) -> Bool do
	let status = int(res.status)
	let headers = clone(res.headers)
	let body = if res.body == null then "" else res.body end
	if mapHas(opts, "addDateHeader") and opts.addDateHeader != null and opts.addDateHeader and not mapHas(headers, "Date") then
		headers["Date"] = timeFormatRFC3339(nowMillis())
	end
	for [k, v] in headers do
		if not _validHeaderValue(v) then
			let _ = write(conn, sprintf("HTTP/1.1 400 %s\r\nContent-Length: 11\r\nContent-Type: text/plain\r\n\r\nbad request", [_reason(400)]))
			flush(conn)
			return false
		end
	end
	if not mapHas(headers, "Content-Length") then
		headers["Content-Length"] = str(len(body))
	end
	if not mapHas(headers, "Content-Type") then
		headers["Content-Type"] = "text/plain"
	end
	let head = sprintf("HTTP/1.1 %d %s\r\n", [status, _reason(status)])
	for [k, v] in headers do
		head = head + sprintf("%s: %s\r\n", [k, v])
	end
	head = head + "\r\n"
	let n1 = write(conn, head)
	if n1 == null then
		return false
	end
	let n2 = write(conn, body)
	if n2 == null then
		return false
	end
	flush(conn)
	true
end

let _removeConn = fun(arr: [Any], c) -> [Any] do
	let out = []
	let i = 0
	while i < len(arr) do
		let it = arr[i]
		if uid(it) != uid(c) then
			out = out + [it]
		end
		i = i + 1
	end
	out
end

let _connLoop = fun(conn, router, server) -> Null do
	while true do
		let rr = _readRequest(conn, server.opts)
		if rr == null then
			let _ = write(conn, sprintf("HTTP/1.1 400 %s\r\nContent-Length: 11\r\nContent-Type: text/plain\r\n\r\nbad request", [_reason(400)]))
			break
		end
		if mapHas(rr, "tooLong") and bool(rr.tooLong) then
			let _ = write(conn, sprintf("HTTP/1.1 414 %s\r\nContent-Length: 8\r\nContent-Type: text/plain\r\n\r\ntoo long", [_reason(414)]))
			break
		end
		if mapHas(rr, "badRequest") and bool(rr.badRequest) then
			let _ = write(conn, sprintf("HTTP/1.1 400 %s\r\nContent-Length: 11\r\nContent-Type: text/plain\r\n\r\nbad request", [_reason(400)]))
			break
		end
		if mapHas(rr, "tooManyHeaders") and bool(rr.tooManyHeaders) then
			let _ = write(conn, sprintf("HTTP/1.1 431 %s\r\nContent-Length: 27\r\nContent-Type: text/plain\r\n\r\nrequest header fields too large", [_reason(431)]))
			break
		end

		let req = rr.req

		# Expect: 100-continue preflight (path+query only)
		let expect = _getHeader(req.headers, "Expect")
		if expect != null and toLower(expect) == "100-continue" then
			let pf = router._preflight(req)
			if pf == null or not pf.ok then
				let _ = write(conn, sprintf("HTTP/1.1 404 %s\r\nContent-Length: 9\r\nContent-Type: text/plain\r\n\r\nnot found", [_reason(404)]))
				let _ = flush(conn)
				break
			end
			if rr.wantCL > 0 then
				let _ = write(conn, "HTTP/1.1 100 Continue\r\n\r\n")
				let _ = flush(conn)
			end
		end

		# Read body (Content-Length only)
		if rr.wantCL >= 0 then
			let cap = int(server.opts.maxBodyBytes)
			if rr.wantCL > cap then
				let _ = write(conn, sprintf("HTTP/1.1 413 %s\r\nContent-Length: 15\r\nContent-Type: text/plain\r\n\r\npayload too large", [_reason(413)]))
				break
			end
			let s = _readBodyContentLength(conn, rr.wantCL, cap)
			if s == null then
				let _ = write(conn, sprintf("HTTP/1.1 400 %s\r\nContent-Length: 11\r\nContent-Type: text/plain\r\n\r\nbad request", [_reason(400)]))
				break
			end
			req.body = s
		end

		# Try streaming path first (for stream routes)
		let sres = router._entryCallStream(req, conn, server.opts)
		if sres != null and mapHas(sres, "streamed") and sres.streamed == true then
			if not rr.keepAlive then
				break
			end
			continue
		end

		# Buffered value route
		let out = router._entryCall(req)
		# Auto-HEAD for value routes: same headers (with accurate Content-Length), no body
		if req.method == "HEAD" then
			let hdr = clone(out.headers)
			if not mapHas(hdr, "Content-Length") then
				hdr["Content-Length"] = str(len(out.body))
			end
			let ok = _writeResponse(conn, {status: out.status, headers: hdr, body: ""}, server.opts)
			if not ok or not rr.keepAlive then
				break
			end
			continue
		end
		let ok = _writeResponse(conn, out, server.opts)
		if not ok or not rr.keepAlive then
			break
		end
	end

	close(conn)
	server.conns = _removeConn(server.conns, conn)
	null
end
