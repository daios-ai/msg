# nethttp — streaming-first HTTP server for MindScript
# 
# Design goals:
# - Single streaming core (Request -> Responder -> Null), middleware-friendly
# - Route-time contracts (FastAPI-like): path / query / body / responses
# - Pre-binding & validation before handler runs (422 on failure)
# - Value routes as sugar over the streaming core
# - Auto OpenAPI from the same route contracts (stub for now)
# - Internals never return null on success; boundary helpers may return annotated null
# 
# Less is more:
# - Public surface: router(), route(), routeStream(), mount(), contract(), openapiDoc(), testClient()
# 
# Errors in handlers:
# - Use a structured _HttpError object to signal HTTP errors from user code.
# 
# Breaking change (value route handler shape):
# - Value handlers are now fun(req, ctx) -> {} (previously fun(ctx) -> {})

# =========================
# Public types (data-only)
# =========================

let Request = type {
	method: Str,
	url: {scheme: Str?, host: Str?, path: Str, query: Str?},
	headers: {},
	body: Any,
	pathParams: {}?
}

# Fluent responder; boundary write/flush/end surface errors (Int?/Bool?).
let Responder = type {
	status: Int -> Responder,
	setHeader: Str -> Str -> Responder,
	write: Str -> Int?,
	flush: Null -> Bool?,
	end: Null -> Bool?
}

# Internal accumulator for test client / server writer
let _Response = type {status: Int, headers: {}, body: Str}

# Structured error for value handlers
let _HttpError = type {
	__http_error__: Bool,
	status: Int,
	body: Str,
	headers: {}
}

# Contract (open map; optional fields)
# path: Type, query: Type, body: Type, responses: {"200": Type, "404": Type, ...}, summary: Str, tags: [Str]
# NOTE: MindScript maps are JSON-like; use STRING keys for status codes in responses
let Contract = type {}

# Handler context (open)
let _Ctx = type {req: Request, path: {}, query: {}, body: Any, deadlineMs: Int?}

# Streaming handler / middleware (3-arg)
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
		if len(seg) > 0 then
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
	# "{*rest}" ⇒ first char after '{' is '*'
	let n = len(seg)
	n >= 3 and substr(seg, 0, 1) == "{" and substr(seg, 1, 2) == "*" and substr(seg, n - 1, n) == "}"
end

let _paramName = fun(seg: Str) -> Str do
	# strip braces, then optional leading '*'
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
	null # <json stringify failed>

end

let contract = fun(c: Contract) -> Contract do
	c
end

# =========================
# Error helpers (public)
# =========================

let raise = fun(status: Int, message: Str) -> {} do
	{
		__http_error__: true,
		status: status,
		body: message,
		headers: {}
	}
end

let raiseWithHeaders = fun(status: Int, message: Str, headers: {}) -> {} do
	{
		__http_error__: true,
		status: status,
		body: message,
		headers: headers
	}
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

# Scalars + arrays; minimal coercions
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
		let vs = if mapHas(m, k) then
			m[k]
		else
			null
		end

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
	let qm = urlQueryParse(if req.url.query == null then
		""
	else
		req.url.query
	end)
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
		let raw = if mapHas(ps, k) then
			ps[k]
		else
			null
		end
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

let text = fun(status: Int, s: Str) -> {} do
	{
		status: status,
		headers: {"Content-Type": "text/plain"},
		body: s
	}
end

let noContent = fun(_: Null) -> {} do
	{status: 204, headers: {}, body: ""}
end

let redirect = fun(status: Int, location: Str) -> {} do
	{
		status: status,
		headers: {Location: location},
		body: ""
	}
end

# =========================
# Responder (opaque; per-request)
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
# Router (closure-backed): exact match only + mount(prefix, child)
# =========================

let _mkRoute = fun(method: Str, pattern: Str, call: _StreamFn, style: Str, c: Contract) -> {} do
	let m = _normMethod(method)
	{
		method: m,
		pattern: pattern,
		parts: _splitPath(pattern),
		call: call,
		style: style,
		contract: c
	}
end

let _pathMatch = fun(parts: [Str], reqSegs: [Str]) -> {}? do
	# Determine if the last segment is a catch-all "{*name}"
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

	# Shape checks: exact length unless trailing catch-all, which allows extra segs
	if not hasCatch and nReq != nParts then
		return
	end
	if hasCatch and nReq < last then
		return
	end

	# Match fixed/param prefix and record single-segment params
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

	# Trailing catch-all captures the remainder (can be empty if request ends there)
	if hasCatch then
		let name = _paramName(parts[last])
		let rest = join(slice(reqSegs, last, nReq), "/")
		params[name] = rest
	end

	# matched score prefers longer (more specific) patterns
	{
		params: params,
		matched: if hasCatch then
			last
		else
			nParts
		end
	}
end

let router = fun(_: Null) -> {} do
	let _routes = []
	let _mws = []
	let _mounts = [] # entries: { prefixParts: [Str], child: Router }

	let _matchLocal = fun(method: Str, path: Str) -> {}? do
		let want = _normMethod(method)
		let segs = _splitPath(path)
		let best = null
		let score = -1
		for r in _routes do
			if r.method != want then
				continue
			end
			let p = _pathMatch(r.parts, segs)
			if p != null then
				# prefer the *actual* matched depth (handles catch-alls cleanly)
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
		# Try mounts in registration order; first match wins
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
				pathParams: {} # child recomputes its own path params
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
				return {status: 204, headers: {}, body: ""}
			end
			if _pathExistsLocal(req.url.path) then
				return {
					status: 405,
					headers: {"Content-Type": "text/plain"},
					body: "method not allowed"
				}
			end
			return {
				status: 404,
				headers: {"Content-Type": "text/plain"},
				body: "not found"
			}
		end

		let r = m.route
		let segs = m.segs
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
				return {
					status: 422,
					headers: {"Content-Type": "application/json; charset=utf-8"},
					body: _validationJSON("path", noteGet(P))
				}
			end
			ctx.path = P
		end

		if mapHas(C, "query") and C.query != null then
			let Q = _bindQueryFromReq(rq, C.query)
			if Q == null then
				return {
					status: 422,
					headers: {"Content-Type": "application/json; charset=utf-8"},
					body: _validationJSON("query", noteGet(Q))
				}
			end
			ctx.query = Q
		end

		if mapHas(C, "body") and C.body != null then
			let B = _bindBody(rq, C.body)
			if B == null then
				return {
					status: 422,
					headers: {"Content-Type": "application/json; charset=utf-8"},
					body: _validationJSON("body", noteGet(B))
				}
			end
			ctx.body = B
		end

		let pack = _newResponder()
		let call = _compose(r.call)

		let t = try(fun(_: Null) do
			call(rq, pack.api, ctx)
		end)
		if not t.ok then
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

	let api = {
		__kind: "nethttp.router",
		# Value route: handler(req, ctx) -> {}   (BREAKING: req added)
		route: fun(method: Str, pattern: Str, c: Contract, handler) -> Bool do
			let parts = _splitPath(pattern)

			let stream = fun(req: Request, res: Responder, ctx: {}) -> Null do
				let r = try(fun(_: Null) do
					handler(req, ctx)
				end)
				if not r.ok then
					res.status(500)
					res.setHeader("Content-Type", "text/plain")
					let _ = res.write("internal server error")
					let _ = res.end(null)
					return
				end
				let out = r.value

				if out == null then
					res.status(500)
					res.setHeader("Content-Type", "text/plain")
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

				if isType(out, type {status: Int, headers: {}, body: Str}) then
					if mapHas(c, "responses") and c.responses != null then
						let payload = if mapHas(out, "body") then
							jsonParse(out.body)
						else
							out
						end
						if not _checkResponse(out.status, payload, c.responses) then
							res.status(500)
							res.setHeader("Content-Type", "application/json")
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
					res.status(500)
					res.setHeader("Content-Type", "text/plain")
					let _ = res.write("json encode error")
					let _ = res.end(null)
					return
				end
				if mapHas(c, "responses") and c.responses != null then
					if not _checkResponse(200, out, c.responses) then
						res.status(500)
						res.setHeader("Content-Type", "application/json")
						let _ = res.write(_validationJSON("response", "response-validation failed"))
						let _ = res.end(null)
						return
					end
				end
				res.setHeader("Content-Type", "application/json; charset=utf-8")
				res.status(200)
				let _ = res.write(js)
				let _ = res.end(null)
				null
			end

			_routes = _routes + [_mkRoute(method, pattern, stream, "value", c)]
			true
		end,
		# Streaming route: handler(req, res, ctx) -> Null
		routeStream: fun(method: Str, pattern: Str, c: Contract, handler) -> Bool do
			let stream = fun(req: Request, res: Responder, ctx: {}) -> Null do
				let r = try(fun(_: Null) do
					handler(req, res, ctx)
				end)
				if not r.ok then
					res.status(500)
					res.setHeader("Content-Type", "text/plain")
					let _ = res.write("internal server error")
					let _ = res.end(null)
				end
				null
			end
			_routes = _routes + [_mkRoute(method, pattern, stream, "stream", c)]
			true
		end,
		# Mount a child router at a fixed prefix (exact segment prefix)
		mount: fun(prefix: Str, child) -> Bool do
			_mounts = _mounts + [
				{prefixParts: _splitPath(prefix), child: child}
			]
			true
		end,
		use: fun(mw: _Mw) -> Bool do
			_mws = _mws + [mw]
			true
		end,
		_entryCall: fun(req: Request) -> _Response do
			_entryCore(req)
		end,
		_dump: fun(_: Null) -> {} do
			{routes: _routes, mws: _mws}
		end
	}
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
# Dispatch (internal) and test client (public)
# =========================

let testClient = fun(r) -> {} do
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

		let R = {
			method: _normMethod(method),
			url: _makeURL(p, q),
			headers: headers,
			body: body,
			pathParams: {}
		}
		let res = r._entryCall(R)
		{
			status: res.status,
			headers: res.headers,
			body: res.body
		}
	end
	{call: call}
end

# =========================
# Middleware (built-ins) — streaming only
# =========================

let mwRecover = fun(_: Null) -> _Mw do
	return fun(next: _StreamFn) -> _StreamFn do
		return fun(req: Request, res: Responder, ctx: {}) -> Null do
			let r = try(fun(_: Null) do
				next(req, res, ctx)
			end)
			if not r.ok then
				printf("panic: %s\n", [r.error])
				res.status(500)
				res.setHeader("Content-Type", "text/plain")
				let _ = res.write("internal server error")
				let _ = res.end(null)
			end
			null
		end
	end
end

let mwRequestID = fun(headerName: Str) -> _Mw do
	let name = headerName
	return fun(next: _StreamFn) -> _StreamFn do
		return fun(req: Request, res: Responder, ctx: {}) -> Null do
			let id = if mapHas(req.headers, name) and req.headers[name] != null then
				req.headers[name]
			else
				sprintf("r-%d", [nowNanos()])
			end
			req.headers[name] = id
			res.setHeader(name, id)
			next(req, res, ctx)
			null
		end
	end
end

let mwTimeout = fun(ms: Int) -> _Mw do
	let dur = ms
	return fun(next: _StreamFn) -> _StreamFn do
		return fun(req: Request, res: Responder, ctx: {}) -> Null do
			let deadline = nowMillis() + dur
			let closed = {v: false}
			let wrote = {v: false}

			# expose deadline to handlers for cooperative cancellation
			ctx.deadlineMs = deadline

			let closeTimeout = fun(_: Null) -> Null do
				if closed.v then
					return
				end
				res.status(504)
				res.setHeader("Content-Type", "text/plain")
				let _ = res.write("timeout")
				let _ = res.end(null)
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
				if timedOut(null) and not wrote.v then
					closeTimeout(null)
					return proxy
				end
				res.status(code)
				proxy
			end
			proxy.setHeader = fun(k: Str, v: Str) -> Responder do
				if closed.v then
					return proxy
				end
				if timedOut(null) and not wrote.v then
					closeTimeout(null)
					return proxy
				end
				res.setHeader(k, v)
				proxy
			end
			proxy.write = fun(chunk: Str) -> Int? do
				if closed.v then
					return 0
				end
				if timedOut(null) and not wrote.v then
					closeTimeout(null)
					return 0
				end
				wrote.v = true
				res.write(chunk)
			end
			proxy.flush = fun(_: Null) -> Bool? do
				if closed.v then
					return true
				end
				if timedOut(null) and not wrote.v then
					closeTimeout(null)
					return true
				end
				res.flush(null)
			end
			proxy.end = fun(_: Null) -> Bool? do
				if closed.v then
					return true
				end
				if timedOut(null) and not wrote.v then
					closeTimeout(null)
					return true
				end
				res.end(null)
			end

			# run downstream in the SAME process (cooperative)
			next(req, proxy, ctx)

			# If handler returned without writing and we missed the deadline, finalize 504 now.
			if not closed.v and not wrote.v and timedOut(null) then
				closeTimeout(null)
			end
			null
		end
	end
end

let mwCors = fun(opts: {}) -> _Mw do
	let origin = if mapHas(opts, "origin") and opts.origin != null then
		opts.origin
	else
		"*"
	end
	let methods = if mapHas(opts, "methods") and opts.methods != null then
		join(opts.methods, ", ")
	else
		"GET, POST, PUT, PATCH, DELETE, OPTIONS"
	end
	let allowH = if mapHas(opts, "headers") and opts.headers != null then
		join(opts.headers, ", ")
	else
		"Content-Type, Authorization"
	end
	let creds = if mapHas(opts, "credentials") and opts.credentials != null and bool(opts.credentials) then
		"true"
	else
		"false"
	end

	return fun(next: _StreamFn) -> _StreamFn do
		return fun(req: Request, res: Responder, ctx: {}) -> Null do
			res.setHeader("Access-Control-Allow-Origin", origin)
			res.setHeader("Access-Control-Allow-Methods", methods)
			res.setHeader("Access-Control-Allow-Headers", allowH)
			res.setHeader("Access-Control-Allow-Credentials", creds)
			if req.method == "OPTIONS" then
				res.status(204)
				let _ = res.end(null)
				return
			end
			next(req, res, ctx)
			null
		end
	end
end

let mwAccessLog = fun(_: Null) -> _Mw do
	return fun(next: _StreamFn) -> _StreamFn do
		return fun(req: Request, res: Responder, ctx: {}) -> Null do
			let t0 = nowMillis()
			let status = 200
			let bytes = 0

			let orig = res
			let proxy = {}
			proxy.status = fun(code: Int) -> Responder do
				status = code
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
					bytes = bytes + int(n)
				end
				n
			end
			proxy.flush = fun(_: Null) -> Bool? do
				orig.flush(null)
			end
			proxy.end = fun(_: Null) -> Bool? do
				orig.end(null)
			end

			next(req, proxy, ctx)

			let dur = nowMillis() - t0
			printf("%s \"%s %s\" %d %d %dms\n", [
				"-",
				req.method,
				req.url.path,
				int(status),
				int(bytes),
				int(dur)
			])
			null
		end
	end
end

# =========================
# Server lifecycle (HTTP/1.1 MVP)
# =========================

let _ServerHandle = type {
	listener: Any,
	stop: {closed: Bool},
	conns: [Any],
	opts: {}
}

let serve = fun(listener, r, opts: {}) -> {}? do
	if listener == null then
		return null # <serve: missing listener>

	end
	if not mapHas(r, "_entryCall") then
		return null # <serve: not a router>

	end

	if not mapHas(opts, "readHeaderTimeoutMs") or opts.readHeaderTimeoutMs == null then
		opts.readHeaderTimeoutMs = 0
	end
	if not mapHas(opts, "writeTimeoutMs") or opts.writeTimeoutMs == null then
		opts.writeTimeoutMs = 0
	end
	if not mapHas(opts, "idleTimeoutMs") or opts.idleTimeoutMs == null then
		opts.idleTimeoutMs = 0
	end
	if not mapHas(opts, "maxHeaderBytes") or opts.maxHeaderBytes == null then
		opts.maxHeaderBytes = 8192
	end
	if not mapHas(opts, "maxBodyBytes") or opts.maxBodyBytes == null then
		opts.maxBodyBytes = 1048576
	end

	let server = {
		listener: listener,
		stop: {closed: false},
		conns: [],
		opts: opts
	}

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
	if code == 200 then
		return "OK"
	end
	if code == 204 then
		return "No Content"
	end
	if code == 400 then
		return "Bad Request"
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

let _trimCR = fun(s: Str) -> Str do
	let n = len(s)
	if n > 0 and substr(s, n - 1, n) == "\r" then
		return substr(s, 0, n - 1)
	end
	s
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

let _readHeaders = fun(c, maxBytes: Int) -> {}? do
	let headers = {}
	let budget = maxBytes
	while true do
		let line = _readLineLimited(c, budget)
		if line == null then
			return
		end
		if len(line) == 0 then
			break
		end
		let parts = split(line, ":")
		if len(parts) < 2 then
			return
		end
		let key = parts[0]
		let value = strip(join(slice(parts, 1, len(parts)), ":"))
		headers[key] = value
		budget = budget - len(line) - 2
	end
	headers
end

let _parseTarget = fun(t: Str) -> {} do
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
	let want = if n > cap then
		cap
	else
		n
	end
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

let _readRequest = fun(conn, opts: {}) -> {}? do
	let line = readLine(conn)
	if line == null then
		return
	end
	let reqLine = _trimCR(line)
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
	let hdrs = _readHeaders(conn, maxHdr)
	if hdrs == null then
		return
	end

	let keepAlive = true
	if mapHas(hdrs, "Connection") and toLower(hdrs["Connection"]) == "close" then
		keepAlive = false
	end

	let t = _parseTarget(target)

	let bodyStr = ""
	if mapHas(hdrs, "Content-Length") then
		let n = int(hdrs["Content-Length"])
		if n == null or n < 0 then
			return
		end
		let cap = int(opts.maxBodyBytes)
		let s = _readBodyContentLength(conn, n, cap)
		if s == null then
			return
		end
		if n > cap then
			let toDrop = n - cap
			while toDrop > 0 do
				let d = readN(conn, if toDrop > 4096 then
					4096
				else
					toDrop
				end)
				if d == null or len(d) == 0 then
					break
				end
				toDrop = toDrop - len(d)
			end
			return {tooLarge: true}
		end
		bodyStr = s
	end

	{
		req: {
			method: method,
			url: {scheme: null, host: null, path: t.path, query: t.query},
			headers: hdrs,
			body: bodyStr,
			pathParams: {}
		},
		keepAlive: keepAlive
	}
end

let _writeResponse = fun(conn, res: _Response) -> Bool do
	let status = int(res.status)
	let headers = clone(res.headers)
	let body = if res.body == null then
		""
	else
		res.body
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
			let _ = write(conn, sprintf("HTTP/1.1 400 %s\r\nContent-Length: 11\r\nContent-Type: text/plain\r\n\r\nbad request", [
				_reason(400)
			]))
			break
		end
		if mapHas(rr, "tooLarge") and bool(rr.tooLarge) then
			let _ = write(conn, sprintf("HTTP/1.1 413 %s\r\nContent-Length: 15\r\nContent-Type: text/plain\r\n\r\npayload too large", [
				_reason(413)
			]))
			break
		end

		let req = rr.req
		let out = router._entryCall(req)

		let ok = _writeResponse(conn, out)
		if not ok or not rr.keepAlive then
			break
		end
	end

	close(conn)
	server.conns = _removeConn(server.conns, conn)
	null
end

# =========================
# OpenAPI (contract-based)
# =========================

let openapiDoc = fun(r, info: {}, servers: [Str]?) -> {}? do
	if not mapHas(r, "_dump") then
		return null # <not a router>

	end
	let dump = r._dump()
	let rs = []
	for rr in dump.routes do
		rs = rs + [{method: rr.method, pattern: rr.pattern, style: rr.style}]
	end
	{
		info: info,
		servers: if servers == null then
			[]
		else
			servers
		end,
		routes: rs
	}
end