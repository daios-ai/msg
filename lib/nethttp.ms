# nethttp — streaming-first HTTP server for MindScript
#
# Design goals:
# - Single streaming core (Request -> Responder -> Null)
# - Route-time contracts (FastAPI-like): path / query / body / responses
# - Pre-binding & validation before handler runs (422 on failure)
# - Value routes as sugar; streaming routes unchanged
# - Auto OpenAPI from the same route contracts
# - Internals never return null on success; boundary helpers may return annotated null
#
# Less is more:
# - Tiny surface: route(), routeStream(), contract(), router(), openapiDoc()
# - Optional legacy shims remain (handle, handleValue, bindJson, bindQuery, path)

# =========================
# Public types (data-only)
# =========================

let Request = type {
	method!: Str,
	url!: { scheme: Str?, host: Str?, path!: Str, query: Str? },
	headers!: {},
	body!: Any,
	pathParams: {}?
}

# Fluent responder
let Responder = type {
	status: Int -> Responder,
	setHeader: Str -> Str -> Responder,
	write: Str -> Responder,
	flush: Null -> Responder,
	end: Null -> Null
}

# Internal accumulator for test client / server writer
let _Response = type { status!: Int, headers!: {}, body!: Str }

# Structured error for value handlers (adapter consumes)
let _HttpError = type {
	__http_error__!: Bool,
	status!: Int,
	body: Str,
	headers: {}
}

# Contract object (open map; all fields optional)
# path: Type, query: Type, body: Type, responses: {Int: Type}, summary: Str, tags: [Str]
let Contract = type {}

# Handler contexts (open)
let _Ctx = type { req!: Request, path: {}, query: {}, body: Any }

# Streaming handler / middleware
let _StreamFn = type Request -> Responder -> Null
let _Mw = type _StreamFn -> _StreamFn

# =========================
# Utilities
# =========================

let _normMethod = fun(m: Str) -> Str do toUpper(m) end

let _hasPrefix = fun(s: Str, prefix: Str) -> Bool do
	let n = len(prefix)
	if len(s) < n then return false end
	substr(s, 0, n) == prefix
end

let _splitPath = fun(p: Str) -> [Str] do
	let s = if len(p) == 0 or substr(p, 0, 1) != "/" then "/" + p else p end
	let raw = split(s, "/")
	let out = []
	let i = 0
	while i < len(raw) do
		let seg = raw[i]
		if len(seg) > 0 then out = out + [seg] end
		i = i + 1
	end
	out
end

let _isParamSeg = fun(seg: Str) -> Bool do
	let n = len(seg)
	n >= 2 and substr(seg, 0, 1) == "{" and substr(seg, n - 1, n) == "}"
end

let _paramName = fun(seg: Str) -> Str do substr(seg, 1, len(seg) - 1) end

let _makeURL = fun(path: Str, query: Str?) -> {} do
	{ scheme: null, host: null, path: path, query: query }
end

let _json = fun(v) -> Str? do
	let s = jsonStringify(v)
	if s != null then return s end
	return null # <json stringify failed>
end

let contract = fun(c: Contract) -> Contract do c end

# =========================
# Error helpers (public)
# =========================

let raise = fun(status: Int, message: Str) -> {} do
	{ __http_error__: true, status: status, body: message, headers: {} }
end

let raiseWithHeaders = fun(status: Int, message: Str, headers: {}) -> {} do
	{ __http_error__: true, status: status, body: message, headers: headers }
end

# =========================
# Binding helpers (boundary-level)
# (kept for legacy routes; route/routeStream handle this automatically)
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

let bindJson = fun(req: Request, T: Type) -> Any? do
	let raw = _rawBody(req)
	if raw == null then 
		return null # <read body failed> 
	end
	let v = jsonParse(raw)
	if v == null then 
		return null # <invalid json> 
	end
	if not isType(v, T) then 
		return null # <validation failed> 
	end
	v
end

let query = fun(req: Request) -> {} do
	let raw = if req.url.query == null then "" else req.url.query end
	let m = urlQueryParse(raw)
	if m == null then return {} end
	m
end

# Improved query binding (scalars + arrays; minimal coercions)
let _coerceScalar = fun(s: Str, T: Type) -> Any? do
	if isSubtype(T, type Str) then
		let pj = jsonParse(s)
		return if pj != null and isType(pj, type Str) then pj else s end
	end
	if isSubtype(T, type Int) then
		let i = int(s)
		if i != null then return i end
		let pj = jsonParse(s)
		if pj != null and isType(pj, type Int) then return pj end
		return null # <bad int>
	end
	if isSubtype(T, type Num) then
		let n = num(s)
		if n != null then return n end
		let pj = jsonParse(s)
		if pj != null and (isType(pj, type Num) or isType(pj, type Int)) then return pj end
		return null # <bad num>
	end
	if isSubtype(T, type Bool) then
		let sl = toLower(s)
		if sl == "true" then return true end
		if sl == "false" then return false end
		let pj = jsonParse(s)
		if pj != null and isType(pj, type Bool) then return pj end
		return null # <bad bool>
	end
	return null # <unsupported scalar type>
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
		let tag = e[0]         # "pair" | "pair!"
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

	if isType(out, T) then return out end
	return null # <query validation failed>
end

let bindQuery = fun(req: Request, T: Type) -> Any? do
	let qm = urlQueryParse(if req.url.query == null then "" else req.url.query end)
	if qm == null then 
		return null # <invalid query string> 
	end
	_bindQueryMap(qm, T)
end

let path = fun(req: Request) -> {} do
	if req.pathParams == null then return {} end
	req.pathParams
end

# =========================
# Response helpers (public, for value handlers)
# =========================

let json = fun(status: Int, value) -> {} do
	let s = _json(value)
	if s == null then
		return { status: 500, headers: {"Content-Type": "text/plain"}, body: "json encode error" }
	end
	{ status: status, headers: {"Content-Type": "application/json; charset=utf-8"}, body: s }
end

let text = fun(status: Int, s: Str) -> {} do
	{ status: status, headers: {"Content-Type": "text/plain"}, body: s }
end

let noContent = fun(_: Null) -> {} do
	{ status: 204, headers: {}, body: "" }
end

let redirect = fun(status: Int, location: Str) -> {} do
	{ status: status, headers: {"Location": location}, body: "" }
end

# =========================
# Responder (opaque; per-request)
# =========================

let _newResponder = fun() -> { res: _Response, api: Responder } do
	let state = { status: 200, headers: {}, body: "" }
	let api = {}

	api.status = fun(code: Int) -> Responder do
		state.status = code
		api
	end
	api.setHeader = fun(k: Str, v: Str) -> Responder do
		state.headers[k] = v
		api
	end
	api.write = fun(chunk: Str) -> Responder do
		state.body = state.body + chunk
		api
	end
	api.flush = fun(_: Null) -> Responder do
		api
	end
	api.end = fun(_: Null) -> Null do
		null
	end

	{ res: state, api: api }
end

# =========================
# Value -> Stream adapter (legacy public)
# =========================

let value = fun(fn: Request -> Any?) -> _StreamFn do
	return fun(req: Request, res: Responder) -> Null do
		let r = try(fun(_: Null) do fn(req) end)
		if not r.ok then
			res.status(500).setHeader("Content-Type","text/plain").write("internal server error").end(null)
			return null
		end
		let out = r.value

		if out == null then
			let nt = noteGet(out)
			let msg = if nt != null then nt else "error" end
			res.status(500).setHeader("Content-Type","text/plain").write(msg).end(null)
			return null
		end

		if isType(out, _HttpError) then
			let hdrs = if mapHas(out, "headers") and out.headers != null then out.headers else {} end
			let rt = if mapHas(out, "body") and out.body != null then out.body else "" end
			for [k, v] in hdrs do res.setHeader(k, v) end
			res.status(out.status).write(rt).end(null)
			return null
		end

		if isType(out, type {status!: Int, headers!: {}, body!: Str}) then
			for [k, v] in out.headers do res.setHeader(k, v) end
			res.status(out.status).write(out.body).end(null)
			return null
		end

		let js = _json(out)
		if js == null then
			res.status(500).setHeader("Content-Type","text/plain").write("json encode error").end(null)
			return null
		end
		res.setHeader("Content-Type","application/json; charset=utf-8").status(200).write(js).end(null)
		null
	end
end

# =========================
# Validation helpers for contract routes
# =========================

let _validationJSON = fun(where: Str, reason: Str) -> Str do
	let obj = { error: "validation", detail: [{ in: where, reason: reason }] }
	let s = _json(obj)
	if s == null then return "{\"error\":\"validation\"}" end
	s
end

# Build a "params dict" from path template + request path segments
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

# Bind & validate PATH according to Type TP (object type)
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
		# path params treated as scalars
		let v = _coerceScalar(raw, Tk)
		if v == null then 
			return null # <coercion failed: {k}> 
		end
		out[k] = v
		i = i + 1
	end
	if isType(out, TP) then return out end
	return null # <path validation failed>
end

# Bind & validate QUERY according to Type TQ
let _bindQueryFromReq = fun(req: Request, TQ: Type) -> Any? do
	let qm = urlQueryParse(if req.url.query == null then "" else req.url.query end)
	if qm == null then 
		return null # <invalid query string> 
	end
	_bindQueryMap(qm, TQ)
end

# Bind & validate BODY according to Type TB
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
		return null # <validation failed> 
	end
	v
end

# Validate an outgoing value against declared response type (if present)
let _checkResponse = fun(status: Int, out, respMap: {}) -> Bool do
	if respMap == null then return true end
	if not mapHas(respMap, str(status)) and not mapHas(respMap, status) then
		return true
	end
	let T = if mapHas(respMap, str(status)) then respMap[str(status)] else respMap[status] end
	if not isType(T, type Type) then return true end
	return isType(out, T)
end

# =========================
# Router (closure-backed)
# =========================

let _mkRoute = fun(method: Str, pattern: Str, call: _StreamFn, style: Str, c: Contract) -> {} do
	let m = _normMethod(method)
	let isPrefix = _hasPrefix(pattern, "/") and len(pattern) > 1 and substr(pattern, len(pattern) - 1, len(pattern)) == "/"
	{
		method: m,
		pattern: pattern,
		isPrefix: isPrefix,
		parts: _splitPath(pattern),
		call: call,
		style: style,
		contract: c
	}
end

let _pathMatch = fun(parts: [Str], reqSegs: [Str], isPrefix: Bool) -> {}? do
	if isPrefix and len(reqSegs) < len(parts) then return end
	if not isPrefix and len(reqSegs) != len(parts) then return end
	let n = len(parts)
	let i = 0
	while i < n do
		let rp = parts[i]
		let qp = reqSegs[i]
		if not _isParamSeg(rp) and rp != qp then
			return
		end
		i = i + 1
	end
	{} # non-null match
end

let router = fun(_: Null) -> {} do
	let _routes = []
	let _mws = []   # [_Mw]

	let _match = fun(method: Str, path: Str) -> {}? do
		let want = _normMethod(method)
		let segs = _splitPath(path)
		let best = null
		let score = -1
		for r in _routes do
			if r.method != want then continue end
			let p = _pathMatch(r.parts, segs, r.isPrefix)
			if p != null then
				let s = len(r.parts)
				if s > score then
					best = { route: r, segs: segs }
					score = s
				end
			end
		end
		if best != null then return best end
		null
	end

	let _pathExists = fun(path: Str) -> Bool do
		let segs = _splitPath(path)
		for r in _routes do
			if _pathMatch(r.parts, segs, r.isPrefix) != null then return true end
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

	let _entryCore = fun(req: Request) -> _Response do
		# routing
		let m = _match(req.method, req.url.path)
		if m == null then
			if _normMethod(req.method) == "OPTIONS" then
				return { status: 204, headers: {}, body: "" }
			end
			if _pathExists(req.url.path) then
				return { status: 405, headers: {"Content-Type":"text/plain"}, body: "method not allowed" }
			end
			return { status: 404, headers: {"Content-Type":"text/plain"}, body: "not found" }
		end

		let r = m.route
		let segs = m.segs

		# pre-bind according to contract (if present)
		let C = if mapHas(r, "contract") then r.contract else {} end

		# Start with base request (pathParams filled for legacy)
		let rq = { method: req.method, url: req.url, headers: req.headers, body: req.body, pathParams: _paramsFromPath(r.parts, segs) }

		# Build ctx (only declared sections present)
		let ctx = { req: rq }

		# PATH
		if mapHas(C, "path") and C.path != null then
			let P = _bindPath(rq.pathParams, C.path)
			if P == null then
				return { status: 422, headers: {"Content-Type":"application/json"}, body: _validationJSON("path", noteGet(P)) }
			end
			ctx.path = P
		end

		# QUERY
		if mapHas(C, "query") and C.query != null then
			let Q = _bindQueryFromReq(rq, C.query)
			if Q == null then
				return { status: 422, headers: {"Content-Type":"application/json"}, body: _validationJSON("query", noteGet(Q)) }
			end
			ctx.query = Q
		end

		# BODY
		if mapHas(C, "body") and C.body != null then
			let B = _bindBody(rq, C.body)
			if B == null then
				return { status: 422, headers: {"Content-Type":"application/json"}, body: _validationJSON("body", noteGet(B)) }
			end
			ctx.body = B
		end

		# execute streaming handler into an in-memory responder
		let pack = _newResponder()
		let call = _compose(r.call)

		# For handlers that expect only (req,res), we still pass (req,res) (legacy).
		# Contract routes wrap into a streaming fn that closes over ctx.

		let t = try(fun(_: Null) do call(rq, pack.api) end)
		if not t.ok then
			return { status: 500, headers: {"Content-Type":"text/plain"}, body: "internal server error" }
		end
		{ status: pack.res.status, headers: pack.res.headers, body: pack.res.body }
	end

	let _fullKey = fun(m: Str, p: Str) -> Str do m + " " + p end

	let api = {
		__kind: "nethttp.router",

		# -------- Contract-first: value route --------
		route: fun(method: Str, pattern: Str, c: Contract, handler) -> Bool do
			# Wrap into streaming fn that runs the handler with ctx, then serializes
			let parts = _splitPath(pattern)
			let stream = fun(req: Request, res: Responder) -> Null do
				# Pre-bind via outer _entryCore: we only need to rebuild ctx here
				let pathParams = _paramsFromPath(parts, _splitPath(req.url.path))
				let rq2 = { method: req.method, url: req.url, headers: req.headers, body: req.body, pathParams: pathParams }
				let ctx = { req: rq2 }
				if mapHas(c, "path") and c.path != null then
					let P = _bindPath(pathParams, c.path)
					if P == null then
						res.status(422).setHeader("Content-Type","application/json").write(_validationJSON("path", noteGet(P))).end(null)
						return null
					end
					ctx.path = P
				end
				if mapHas(c, "query") and c.query != null then
					let Q = _bindQueryFromReq(rq2, c.query)
					if Q == null then
						res.status(422).setHeader("Content-Type","application/json").write(_validationJSON("query", noteGet(Q))).end(null)
						return null
					end
					ctx.query = Q
				end
				if mapHas(c, "body") and c.body != null then
					let B = _bindBody(rq2, c.body)
					if B == null then
						res.status(422).setHeader("Content-Type","application/json").write(_validationJSON("body", noteGet(B))).end(null)
						return null
					end
					ctx.body = B
				end

				let r = try(fun(_: Null) do handler(ctx) end)
				if not r.ok then
					res.status(500).setHeader("Content-Type","text/plain").write("internal server error").end(null)
					return null
				end
				let out = r.value

				if out == null then
					let nt = noteGet(out)
					let msg = if nt != null then nt else "error" end
					res.status(500).setHeader("Content-Type","text/plain").write(msg).end(null)
					return null
				end

				# If user returned explicit response or HttpError, pass through
				if isType(out, _HttpError) then
					let hdrs = if mapHas(out, "headers") and out.headers != null then out.headers else {} end
					let rt = if mapHas(out, "body") and out.body != null then out.body else "" end
					for [k, v] in hdrs do res.setHeader(k, v) end
					res.status(out.status).write(rt).end(null)
					return null
				end
				if isType(out, type {status!: Int, headers!: {}, body!: Str}) then
					let st = out.status
					# optional response validation
					if mapHas(c, "responses") and c.responses != null then
						if not _checkResponse(st, if mapHas(out,"body") then jsonParse(out.body) else out end, c.responses) then
							res.status(500).setHeader("Content-Type","application/json").write(_validationJSON("response","response-validation failed")).end(null)
							return null
						end
					end
					for [k, v] in out.headers do res.setHeader(k, v) end
					res.status(st).write(out.body).end(null)
					return null
				end

				# Plain value → JSON 200 by default; validate if response type declared
				let js = _json(out)
				if js == null then
					res.status(500).setHeader("Content-Type","text/plain").write("json encode error").end(null)
					return null
				end
				if mapHas(c, "responses") and c.responses != null then
					let ok = _checkResponse(200, out, c.responses)
					if not ok then
						res.status(500).setHeader("Content-Type","application/json").write(_validationJSON("response","response-validation failed")).end(null)
						return null
					end
				end
				res.setHeader("Content-Type","application/json; charset=utf-8").status(200).write(js).end(null)
				null
			end

			_routes = _routes + [_mkRoute(method, pattern, stream, "value", c)]
			true
		end,

		# -------- Contract-first: streaming route --------
		routeStream: fun(method: Str, pattern: Str, c: Contract, handler) -> Bool do
			# We wrap to pre-bind and then call handler(req,res,ctx)
			let parts = _splitPath(pattern)
			let stream = fun(req: Request, res: Responder) -> Null do
				let pathParams = _paramsFromPath(parts, _splitPath(req.url.path))
				let rq2 = { method: req.method, url: req.url, headers: req.headers, body: req.body, pathParams: pathParams }
				let ctx = { req: rq2 }

				if mapHas(c, "path") and c.path != null then
					let P = _bindPath(pathParams, c.path)
					if P == null then
						res.status(422).setHeader("Content-Type","application/json").write(_validationJSON("path", noteGet(P))).end(null)
						return null
					end
					ctx.path = P
				end
				if mapHas(c, "query") and c.query != null then
					let Q = _bindQueryFromReq(rq2, c.query)
					if Q == null then
						res.status(422).setHeader("Content-Type","application/json").write(_validationJSON("query", noteGet(Q))).end(null)
						return null
					end
					ctx.query = Q
				end
				if mapHas(c, "body") and c.body != null then
					let B = _bindBody(rq2, c.body)
					if B == null then
						res.status(422).setHeader("Content-Type","application/json").write(_validationJSON("body", noteGet(B))).end(null)
						return null
					end
					ctx.body = B
				end

				let r = try(fun(_: Null) do handler(rq2, res, ctx) end)
				if not r.ok then
					res.status(500).setHeader("Content-Type","text/plain").write("internal server error").end(null)
				end
				null
			end
			_routes = _routes + [_mkRoute(method, pattern, stream, "stream", c)]
			true
		end,

		# -------- Legacy shims (kept; not contract-enforced) --------
		handle: fun(method: Str, pattern: Str, handler: _StreamFn) -> Bool do
			_routes = _routes + [_mkRoute(method, pattern, handler, "stream", {})]
			true
		end,

		handleValue: fun(method: Str, pattern: Str, handler: Request -> Any?) -> Bool do
			let s = value(handler)
			_routes = _routes + [_mkRoute(method, pattern, s, "value", {})]
			true
		end,

		# Mount child router under a prefix
		mount: fun(prefix: Str, child) -> Bool do
			if not (mapHas(child, "__kind") and child.__kind == "nethttp.router") then
				panic("mount: child must be a Router")
			end
			let dump = child._dump()
			let pre = if _hasPrefix(prefix, "/") then prefix else "/" + prefix end
			for rr in dump.routes do
				let p = if _hasPrefix(rr.pattern, "/") then rr.pattern else "/" + rr.pattern end
				# preserve original style and contract
				_routes = _routes + [_mkRoute(rr.method, pre + p, rr.call, rr.style, rr.contract)]
			end
			true
		end,

		# Middleware
		use: fun(mw: _Mw) -> Bool do
			_mws = _mws + [mw]
			true
		end,

		_entryCall: fun(req: Request) -> _Response do
			_entryCore(req)
		end,

		_dump: fun(_: Null) -> {} do
			{ routes: _routes, mws: _mws }
		end
	}
	api
end

# =========================
# Dispatch (internal) and test client (public)
# =========================

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

		let R = {
			method: _normMethod(method),
			url: _makeURL(p, q),
			headers: headers,
			body: body,
			pathParams: {}
		}
		let res = r._entryCall(R)
		{ status: res.status, headers: res.headers, body: res.body }
	end
	{ call: call }
end

# =========================
# Middleware (built-ins) — streaming only
# =========================

let mwRecover = fun(_: Null) -> _Mw do
	return fun(next: _StreamFn) -> _StreamFn do
		return fun(req: Request, res: Responder) -> Null do
			let r = try(fun(_: Null) do next(req, res) end)
			if not r.ok then
				printf("panic: %s\n", [r.error])
				res.status(500).setHeader("Content-Type", "text/plain").write("internal server error").end(null)
			end
			null
		end
	end
end

let mwRequestID = fun(headerName: Str) -> _Mw do
	let name = headerName
	return fun(next: _StreamFn) -> _StreamFn do
		return fun(req: Request, res: Responder) -> Null do
			let id = if mapHas(req.headers, name) and req.headers[name] != null then
				req.headers[name]
			else
				sprintf("r-%d", [nowNanos()])
			end
			req.headers[name] = id
			res.setHeader(name, id)
			next(req, res)
			null
		end
	end
end

let mwTimeout = fun(ms: Int) -> _Mw do
	let dur = ms
	return fun(next: _StreamFn) -> _StreamFn do
		return fun(req: Request, res: Responder) -> Null do
			let pWorker = procSpawn(fun(_: Null) -> {} do next(req, res) end)
			let pTimer = procSpawn(fun(_: Null) -> {} do
				let t = timerAfter(dur)
				chanRecv(t)
				{ timeout: true }
			end)
			let r = procJoinAny([pWorker, pTimer])
			let timedOut = isType(r.value, type {timeout!: Bool}) and bool(r.value.timeout)
			if timedOut then
				res.status(504).setHeader("Content-Type", "text/plain").write("timeout").end(null)
				procCancel(pWorker)
			else
				procCancel(pTimer)
			end
			null
		end
	end
end

let mwCors = fun(opts: {}) -> _Mw do
	let origin = if mapHas(opts, "origin") and opts.origin != null then opts.origin else "*" end
	let methods = if mapHas(opts, "methods") and opts.methods != null then join(opts.methods, ", ") else "GET, POST, PUT, PATCH, DELETE, OPTIONS" end
	let allowH = if mapHas(opts, "headers") and opts.headers != null then join(opts.headers, ", ") else "Content-Type, Authorization" end
	let creds = if mapHas(opts, "credentials") and opts.credentials != null and bool(opts.credentials) then "true" else "false" end

	return fun(next: _StreamFn) -> _StreamFn do
		return fun(req: Request, res: Responder) -> Null do
			res.setHeader("Access-Control-Allow-Origin", origin)
			res.setHeader("Access-Control-Allow-Methods", methods)
			res.setHeader("Access-Control-Allow-Headers", allowH)
			res.setHeader("Access-Control-Allow-Credentials", creds)
			if req.method == "OPTIONS" then
				res.status(204).end(null)
				return null
			end
			next(req, res)
			null
		end
	end
end

let mwAccessLog = fun(_: Null) -> _Mw do
	return fun(next: _StreamFn) -> _StreamFn do
		return fun(req: Request, res: Responder) -> Null do
			let t0 = nowMillis()
			let status = 200
			let bytes = 0

			# proxy that preserves chaining
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
			proxy.write = fun(chunk: Str) -> Responder do
				bytes = bytes + len(chunk)
				let _ = orig.write(chunk)
				proxy
			end
			proxy.flush = fun(_: Null) -> Responder do
				let _ = orig.flush(null)
				proxy
			end
			proxy.end = fun(_: Null) -> Null do
				let _ = orig.end(null)
				null
			end

			next(req, proxy)

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
	listener!: Any,
	stop!: {closed!: Bool},
	conns!: [Any],
	opts!: {}
}

let serve = fun(listener, r, opts: {}) -> {}? do
	if listener == null then
		return null # <serve: missing listener>
	end
	if not (mapHas(r, "_entryCall")) then
		return null # <serve: not a router>
	end

	# Defaults
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

	let server = { listener: listener, stop: {closed: false}, conns: [], opts: opts }

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

	return server
end

let shutdown = fun(server, timeoutMs: Int?) -> Bool do
	if server == null then return true end
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
	if code == 400 then return "Bad Request" end
	if code == 404 then return "Not Found" end
	if code == 405 then return "Method Not Allowed" end
	if code == 413 then return "Payload Too Large" end
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

let _readLineLimited = fun(c, remaining: Int?) -> Str? do
	let line = readLine(c)
	if line == null then return null end
	let s = _trimCR(line)
	if remaining != null then
		let left = int(remaining) - len(s) - 2
		if left < 0 then return null end
	end
	s
end

let _readHeaders = fun(c, maxBytes: Int) -> {}? do
	let headers = {}
	let budget = maxBytes
	while true do
		let line = _readLineLimited(c, budget)
		if line == null then return null end
		if len(line) == 0 then break end
		let parts = split(line, ":")
		if len(parts) < 2 then return null end
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
	{ path: path, query: query }
end

let _readBodyContentLength = fun(c, n: Int, cap: Int) -> Str? do
	let want = if n > cap then cap else n end
	let buf = ""
	let remaining = want
	while remaining > 0 do
		let chunk = readN(c, remaining)
		if chunk == null then return null end
		buf = buf + chunk
		remaining = remaining - len(chunk)
		if len(chunk) == 0 then return null end
	end
	buf
end

let _readRequest = fun(conn, opts: {}) -> {}? do
	let line = readLine(conn)
	if line == null then return null end
	let reqLine = _trimCR(line)
	let parts = split(reqLine, " ")
	if len(parts) != 3 then return null end
	let method = toUpper(parts[0])
	let target = parts[1]
	let proto = parts[2]
	if proto != "HTTP/1.1" then return null end

	let maxHdr = int(opts.maxHeaderBytes)
	let hdrs = _readHeaders(conn, maxHdr)
	if hdrs == null then return null end

	let keepAlive = true
	if mapHas(hdrs, "Connection") and toLower(hdrs["Connection"]) == "close" then
		keepAlive = false
	end

	let t = _parseTarget(target)

	let bodyStr = ""
	if mapHas(hdrs, "Content-Length") then
		let n = int(hdrs["Content-Length"])
		if n == null or n < 0 then return null end
		let cap = int(opts.maxBodyBytes)
		let s = _readBodyContentLength(conn, n, cap)
		if s == null then return null end
		if n > cap then
			let toDrop = n - cap
			while toDrop > 0 do
				let d = readN(conn, if toDrop > 4096 then 4096 else toDrop end)
				if d == null or len(d) == 0 then break end
				toDrop = toDrop - len(d)
			end
			return { tooLarge: true }
		end
		bodyStr = s
	end

	{
		req: {
			method: method,
			url: { scheme: null, host: null, path: t.path, query: t.query },
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
	let body = if res.body == null then "" else res.body end

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
	if n1 == null then return false end
	let n2 = write(conn, body)
	if n2 == null then return false end
	flush(conn)
	true
end

let _removeConn = fun(arr: [Any], c) -> [Any] do
	let out = []
	let i = 0
	while i < len(arr) do
		let it = arr[i]
		if uid(it) != uid(c) then out = out + [it] end
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
		if mapHas(rr, "tooLarge") and bool(rr.tooLarge) then
			let _ = write(conn, sprintf("HTTP/1.1 413 %s\r\nContent-Length: 15\r\nContent-Type: text/plain\r\n\r\npayload too large", [_reason(413)]))
			break
		end

		let req = rr.req
		let out = router._entryCall(req)  # _Response

		let ok = _writeResponse(conn, out)
		if not ok or not rr.keepAlive then break end
	end

	close(conn)
	server.conns = _removeConn(server.conns, conn)
	null
end

# =========================
# OpenAPI (contract-based)
# =========================

let openapi = fun(r) -> {}? do
	# Legacy: list routes & style only (kept for compatibility)
	if not (mapHas(r, "_dump")) then
		return null # <not a router>
	end
	let dump = r._dump()
	let rs = []
	for rr in dump.routes do
		rs = rs + [{ method: rr.method, pattern: rr.pattern, style: rr.style }]
	end
	{ routes: rs }
end

let openapiDoc = fun(r, info: {}, servers: [Str]?) -> {}? do
	["stub"]
end
