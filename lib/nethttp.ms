# nethttp.ms — first-draft stubs (swept for arrow types in record type literals)
#
# Philosophy:
# - Small surface, explicit ops at edges.
# - Internal helpers never return null on success.
# - Edge ops may return null with a POST reason.
#
# Status:
# - Router + matching + path params: implemented (exact, {param}, and prefix "/...").
# - bindJson / bindQuery / query / path: implemented (bindQuery = light coercions).
# - Value-returning and streaming handlers supported in testClient.
# - Responses helpers implemented.
# - Middleware: mwRecover, mwRequestID, mwTimeout, mwCors (minimal behaviors).
# - serve/shutdown/openapi: stubs (return null # <unimplemented>).

# =========================
# Public types
# =========================

let Request = type {
	method!: Str,
	url!: { scheme: Str?, host: Str?, path!: Str, query: Str? },
	headers!: {Str: Str},
	body!: Any,                # readable handle OR raw Str (handled by helpers)
	pathParams: {Str: Str}?
}

# NOTE: arrow types (right-associative), not fun(...) in type fields.
let Responder = type {
	status: Int -> Responder,
	setHeader: Str -> Str -> Responder,
	write: Str -> Int?,      # may return null # <io>
	flush: Null -> Bool?,    # may return null # <io>
	end: Null -> Bool?       # may return null # <io>
}

let Response = type { status!: Int, headers!: {Str: Str}, body!: Str }

let Router = type {
	handle: Str -> Str -> Any -> Any,   # method -> path -> handler -> handler
	mount: Str -> Any -> Bool,          # prefix -> childRouter -> ok
	use: Any -> Bool                    # mw -> ok
}

# =========================
# Internal router state
# =========================

let _mkRouter = fun(_: Null) -> {} do
	return {
		_routes: [],       # [{method, path, parts, isPrefix, handler, kind}]
		_mw: []            # [mwFun]
	}
end

let _splitPath = fun(p: Str) -> [Str] do
	if len(p) == 0 then return [] end
	let clean = if substr(p, 0, 1) == "/" then substr(p, 1, len(p)) else p end
	if len(clean) == 0 then return [] end
	return split(clean, "/")
end

let _isPrefixRoute = fun(path: Str) -> Bool do
	# Convention: path ending with "/" matches as subtree prefix (e.g., "/static/")
	if len(path) == 0 then return false end
	return substr(path, len(path) - 1, len(path)) == "/"
end

let _compileRoute = fun(method: Str, path: Str) -> {} do
	let parts = _splitPath(path)
	let params = []
	let i = 0
	for s in parts do
		if len(s) >= 2 and substr(s, 0, 1) == "{" and substr(s, len(s) - 1, len(s)) == "}" then
			params = params + [substr(s, 1, len(s) - 1)]
		end
	end
	return {
		method: toUpper(method),
		path: path,
		parts: parts,
		params: params,
		isPrefix: _isPrefixRoute(path)
	}
end

# Match rules:
# - Longest path wins.
# - Exact or "{param}" segment matches.
# - If route.isPrefix, it matches any request whose path starts with route.path.
let _match = fun(R: {}, method: Str, reqPath: Str) -> {}? do
	let m = toUpper(method)
	let rpath = reqPath
	let rparts = _splitPath(rpath)

	let best = null
	let bestLen = -1

	for rt in R._routes do
		if rt.method != m then continue end

		if rt.isPrefix then
			# prefix match by raw string (normalized)
			if len(rt.path) == 0 or substr(rpath, 0, len(rt.path)) == rt.path then
				# params for prefix routes are empty (no "{x}" support after wildcard in this draft)
				if len(rt.path) > bestLen then
					best = { route: rt, params: {} }
					bestLen = len(rt.path)
				end
			end
			continue
		end

		# exact-length match with param extraction
		let ok = true
		if len(rt.parts) != len(rparts) then ok = false end
		let params = {}
		if ok then
			let i = 0
			while i < len(rt.parts) do
				let a = rt.parts[i]
				let b = rparts[i]
				if len(a) >= 2 and substr(a, 0, 1) == "{" and substr(a, len(a) - 1, len(a)) == "}" then
					let name = substr(a, 1, len(a) - 1)
					params[name] = b
				elif a != b then
					ok = false
					break null
				end
				i = i + 1
			end
		end
		if ok and len(rt.path) > bestLen then
			best = { route: rt, params: params }
			bestLen = len(rt.path)
		end
	end

	return best
end

# Compose value-returning middleware left-to-right: mw(next)->handler
let _chainValue = fun(mws: [Any], leaf) -> Any do
	let out = leaf
	let i = len(mws) - 1
	while i >= 0 do
		out = mws[i](out)
		i = i - 1
	end
	return out
end

# =========================
# Public constructors
# =========================

# Router factory
let router = fun(_: Null) -> Router do
	let R = { _routes: [], _mw: [] }  # private mutable state
	let out = { _routes: [], _mw: [] } # exported view (kept in sync)

	let _sync = fun(_: Null) -> Null do
		out._routes = R._routes
		out._mw = R._mw
		null
	end

	let handle = fun(method: Str, path: Str, handler) -> Any do
		let c = _compileRoute(method, path)
		let kind = if isType(handler, type Request -> Any?) then
			"value"
		elif isType(handler, type Request -> Responder -> Null) then
			"stream"
		else
			fail("handle: handler must be Request->Any? or Request->Responder->Null")
		end
		R._routes = R._routes + [{
			method: toUpper(c.method),
			path: c.path,
			parts: c.parts,
			params: c.params,
			isPrefix: c.isPrefix,
			handler: handler,
			kind: kind
		}]
		_sync(null)
		return handler
	end

	let use = fun(mw) -> Bool do
		R._mw = R._mw + [mw]
		_sync(null)
		return true
	end

	let mount = fun(prefix: Str, child) -> Bool do
		if not mapHas(child, "_routes") then
			fail("mount: child is not a router")
		end
		for rt in child._routes do
			let newPath = if prefix == "/" then
				rt.path
			else
				let pfx = if substr(prefix, len(prefix) - 1, len(prefix)) == "/" then
					substr(prefix, 0, len(prefix) - 1)
				else prefix end
				let suf = if substr(rt.path, 0, 1) == "/" then substr(rt.path, 1, len(rt.path)) else rt.path end
				sprintf("%s/%s", [pfx, suf])
			end
			handle(rt.method, newPath, rt.handler)
		end
		_sync(null)
		return true
	end

	out.handle = handle
	out.mount = mount
	out.use = use
	_sync(null)
	return out
end


# =========================
# Request helpers (edges)
# =========================

let query = fun(req: Request) -> {Str: [Str]} do
	if mapHas(req, "_queryCache") and req._queryCache != null then
		return req._queryCache
	end
	let q = if req.url.query == null then "" else req.url.query end
	let m = urlQueryParse(q)
	if m == null then
		return {}
	end
	let out = {}
	for [k, v] in m do
		if isType(v, type [Str]) then
			out[k] = v
		else
			out[k] = [str(v)]
		end
	end
	req._queryCache = out
	return out
end

let path = fun(req: Request) -> {Str: Str} do
	if mapHas(req, "pathParams") and req.pathParams != null then
		return req.pathParams
	end
	return {}
end

let bindJson = fun(req: Request, T: Type) -> Any? do
	let raw = if isType(req.body, type Str) then
		req.body
	else
		let s = readAll(req.body)
		if s == null then
			return noteSet("<read error>", null)
		end
		s
	end
	let v = jsonParse(raw)
	if v == null then
		return noteSet("<invalid json>", null)
	end
	if not isType(v, T) then
		return noteSet("<validation>", null)
	end
	return v
end

let bindQuery = fun(req: Request, T: Type) -> Any? do
	let qm = query(req)

	let _coerceOne = fun(s: Str, TT: Type) -> Any? do
		if TT == type Str then
			return s
		elif TT == type Int then
			let i = int(s)
			if i == null then return noteSet("<int parse>", null) end
			return i
		elif TT == type Num then
			let n = num(s)
			if n == null then return noteSet("<num parse>", null) end
			return n
		elif TT == type Bool then
			let ls = toLower(s)
			if ls == "true" or ls == "1" then return true end
			if ls == "false" or ls == "0" then return false end
			return noteSet("<bool parse>", null)
		else
			let x = jsonParse(s)
			if x == null then return noteSet("<json parse>", null) end
			if not isType(x, TT) then return noteSet("<validation>", null) end
			return x
		end
	end

	let obj = {}
	for [k, vs] in qm do
		if len(vs) == 0 then
			obj[k] = ""
		elif len(vs) == 1 then
			obj[k] = vs[0]
		else
			obj[k] = vs
		end
	end

	let schema = typeToJSONSchema(T)
	if schema == null or not mapHas(schema, "properties") then
		if isType(obj, T) then return obj end
		return noteSet("<validation>", null)
	end

	let props = schema.properties
	let res = {}
	let okAll = true

	for [k, v] in props do
		if mapHas(obj, k) then
			let val = obj[k]
			if isType(val, type [Str]) then
				let arr = []
				for s in val do
					let c = _coerceOne(s, type Str)
					if c == null then okAll = false end
					arr = arr + [if c == null then "" else c end]
				end
				res[k] = arr
			elif isType(val, type Str) then
				let c2 = _coerceOne(val, type Str)
				if c2 == null then okAll = false end
				res[k] = if c2 == null then "" else c2 end
			else
				res[k] = val
			end
		end
	end

	if not isType(res, T) then
		if not isType(obj, T) then
			return noteSet("<validation>", null)
		end
		return obj
	end
	return res
end

# =========================
# Response builders
# =========================

let json = fun(status: Int, v) -> Response do
	let body = jsonStringify(v)
	let s = if body == null then formatValue(v) else body end
	return { status: status, headers: {"Content-Type": "application/json"}, body: s }
end

let text = fun(status: Int, s: Str) -> Response do
	return { status: status, headers: {"Content-Type": "text/plain; charset=utf-8"}, body: s }
end

let noContent = fun(_: Null) -> Response do
	return { status: 204, headers: {}, body: "" }
end

let redirect = fun(status: Int, location: Str) -> Response do
	return { status: status, headers: {"Location": location}, body: "" }
end

let raise = fun(status: Int, message: Str) -> Null do
	return noteSet(sprintf("<status:%d message:%s>", [status, message]), null)
end

# =========================
# Middleware (minimal)
# =========================

let mwRecover = fun(_: Null) -> Any do
	return fun(next) -> Any do
		return fun(req: Request) -> Any? do
			let r = try(fun(_: Null) do next(req) end)
			if r.ok then
				return r.value
			end
			return json(500, { error: "internal error" })
		end
	end
end

let mwRequestID = fun(headerName: Str) -> Any do
	let H = if len(headerName) == 0 then "X-Request-ID" else headerName end
	return fun(next) -> Any do
		return fun(req: Request) -> Any? do
			let idv = sprintf("%d", [nowNanos()])
			req.ctx = if mapHas(req, "ctx") and req.ctx != null then req.ctx else {} end
			req.ctx.id = idv
			let out = next(req)
			if out != null and mapHas(out, "status") and mapHas(out, "headers") and mapHas(out, "body") then
				out.headers[H] = idv
				return out
			end
			let r = json(200, out)
			r.headers[H] = idv
			return r
		end
	end
end

let mwTimeout = fun(ms: Int) -> Any do
	let dur = if ms <= 0 then 0 else ms end
	return fun(next) -> Any do
		return fun(req: Request) -> Any? do
			if dur == 0 then return next(req) end
			let done = chanOpen(1)
			let p = procSpawn(fun(_: Null) -> Null do
				let v = next(req)
				chanSend(done, v)
			end)
			let tt = timerAfter(dur)
			chanRecv(tt)   # tick -> timeout (stub simplification)
			procCancel(p)
			return json(504, { error: "timeout" })
		end
	end
end

let mwCors = fun(opts: {}) -> Any do
	let allowOrigin = if mapHas(opts, "allowOrigin") and opts.allowOrigin != null then opts.allowOrigin else "*" end
	let allowMethods = if mapHas(opts, "allowMethods") and opts.allowMethods != null then opts.allowMethods else "GET,POST,PUT,PATCH,DELETE,OPTIONS" end
	let allowHeaders = if mapHas(opts, "allowHeaders") and opts.allowHeaders != null then opts.allowHeaders else "Content-Type,Authorization" end
	return fun(next) -> Any do
		return fun(req: Request) -> Any? do
			if toUpper(req.method) == "OPTIONS" then
				let r = noContent(null)
				r.headers["Access-Control-Allow-Origin"] = allowOrigin
				r.headers["Access-Control-Allow-Methods"] = allowMethods
				r.headers["Access-Control-Allow-Headers"] = allowHeaders
				return r
			end
			let out = next(req)
			let r2 = if out != null and isType(out, type {status: Int, headers: {Str: Str}, body: Str}) then
				out
			else
				json(200, out)
			end
			r2.headers["Access-Control-Allow-Origin"] = allowOrigin
			return r2
		end
	end
end

# =========================
# Server lifecycle (stubs)
# =========================

let serve = fun(listener, r: Router, opts: {}) -> {}? do
	return noteSet("<unimplemented: listener accept loop>", null)
end

let shutdown = fun(server, timeoutMs: Int?) -> Bool do
	return true
end

# =========================
# Docs (stub)
# =========================

let openapi = fun(r: Router) -> {}? do
	let items = []
	for rt in r._routes do
		items = items + [{ method: rt.method, path: rt.path, kind: rt.kind }]
	end
	return { routes: items }
end

# =========================
# In-process test client (usable today)
# =========================

let testClient = fun(r: Router) -> {} do
	let call = fun(req: {}) -> { status: Int, headers: {}, body: Str } do
		let method = toUpper(req.method)
		let pathStr = req.path
		let m = _match(r, method, pathStr)
		if m == null then
			let urlObj = {
				scheme: "http",
				host: "test",
				path: pathStr,
				query: if mapHas(req, "query") then req.query else ""
			}
			let headers = if mapHas(req, "headers") and req.headers != null then req.headers else {} end
			let body = if mapHas(req, "body") and req.body != null then req.body else "" end
			let request = { method: method, url: urlObj, headers: headers, body: body, pathParams: {} }

			# Tiny 404 leaf so middleware can wrap/change it (e.g., CORS preflight to 204).
			let leaf = fun(_: Request) -> Any? do
				return json(404, { error: "not found" })
			end
			let chain = _chainValue(r._mw, leaf)
			let out = chain(request)

			# Normalize
			if out != null and mapHas(out, "status") and mapHas(out, "headers") and mapHas(out, "body") then
				return out
			end
			return json(200, out)
		end
		let rt = m.route
		let params = m.params

		let urlObj = {
			scheme: "http",
			host: "test",
			path: pathStr,
			query: if mapHas(req, "query") then req.query else "" end
		}
		let headers = if mapHas(req, "headers") and req.headers != null then req.headers else {} end
		let body = if mapHas(req, "body") and req.body != null then req.body else "" end

		let request = {
			method: method,
			url: urlObj,
			headers: headers,
			body: body,
			pathParams: params
		}

		if rt.kind == "value" then
			let chain = _chainValue(r._mw, rt.handler)
			let out = chain(request)

			if out == null then
				let note = noteGet(out)
				let status = 500
				let msg = "error"
				if note != null then
					let s = note
					let n = int(replace("^.*status:([0-9]+).*$", "$1", s))
					if n != null then status = n end
					msg = s
				end
				return json(status, { error: msg })
			end

			if out != null and mapHas(out, "status") and mapHas(out, "headers") and mapHas(out, "body") then
				return out
			end
			return json(200, out)
		end

		# Streaming: provide a fake Responder that buffers writes.
		let statusRef = { v: 200 }
		let hdrs = {}
		let bodyBuf = ""

		let res = {
			status: fun(code: Int) -> Responder do
				statusRef.v = code
				return res
			end,
			setHeader: fun(k: Str, v: Str) -> Responder do
				hdrs[k] = v
				return res
			end,
			write: fun(s: Str) -> Int? do
				bodyBuf = bodyBuf + s
				return len(s)
			end,
			flush: fun(_: Null) -> Bool? do
				return true
			end,
			end: fun(_: Null) -> Bool? do
				return true
			end
		}

		rt.handler(request, res)

		return { status: statusRef.v, headers: hdrs, body: bodyBuf }
	end

	return { call: call }
end
