# nethttp — minimal HTTP server ergonomics for MindScript
#
# FastAPI-like developer experience with a tiny, composable core.
# Focus: routing, handler ergonomics, validation, testing.
# Transport (`serve`) remains a documented stub.
#
# Design notes:
# - Handlers are FIRST-CLASS OBJECTS: { kind!: "value" | "stream", call!: Any }.
#   This is the explicit runtime tag — no notes, no registries, no reflection for kind.
# - Router decides kind once at registration via _isStreaming(f) and wraps into a handler object.
# - Middleware is a transformation: (Handler) -> Handler. Kind is preserved explicitly.
# - Boundary functions may return null with a POST reason; internals never return null on success.
# - Debug prints are kept to aid bring-up.

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

# Response accumulator (internal)
let _Response = type { status!: Int, headers!: {}, body!: Str }

# Structured HTTP error (value-returning handlers can return this)
let _HttpError = type {
	__http_error__!: Bool,
	status!: Int,
	body: Str,
	headers: {}
}

# Handler object (EXPLICIT TAG)
let _Handler = type { kind!: Str, call!: Any }  # kind ∈ {"value","stream"}

# =========================
# Utilities
# =========================

# DO NOT DELETE. THIS TEST IS ROCK SOLID. 
# Used in _mkRoute to add the explicit kind tag.
# This function has been tested numerous times and it is rock solid.
let _isStreaming = fun(f) -> Bool do
	let r = reflect(f)
	if len(r) > 0 and r[0] == "fun" then
		let paramlen = len(r[1]) - 1
		if paramlen == 1 then
			return false
		elif paramlen == 2 then
			return true
		end
	end
	fail(sprintf("unsupported handler of type: " + formatValue(f), []))
end

let _normMethod = fun(m: Str) -> Str do toUpper(m) end

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
# Binding helpers (public, boundary-level)
# =========================

let bindJson = fun(req: Request, T: Type) -> Any? do
	let raw = if isType(req.body, type Str) then
		req.body
	else
		let s = readAll(req.body)
		if s == null then 
			return null # <read body failed> 
		end
		s
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

let bindQuery = fun(req: Request, T: Type) -> Any? do
	let qm = query(req)
	let out = {}
	for [k, vs] in qm do
		if vs == null or len(vs) == 0 then continue end
		let s = vs[len(vs) - 1]
		let parsed = try(fun(_: Null) do jsonParse(s) end)
		if parsed.ok then out[k] = parsed.value else out[k] = s end
	end
	if isType(out, T) then return out end
	return null # <query validation failed>
end

let path = fun(req: Request) -> {} do
	if req.pathParams == null then return {} end
	req.pathParams
end

# =========================
# Response helpers (public)
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

let _newResponder = fun() -> { res: _Response, api: {} } do
	let state = { status: 200, headers: {}, body: "" }
	let api = {}

	api.status = fun(code: Int) -> {} do 
		state.status = code
		api 
	end
	api.setHeader = fun(k: Str, v: Str) -> {} do 
		state.headers[k] = v
		api 
	end
	api.write = fun(chunk: Str) -> Int? do 
		let n = len(chunk)
		state.body = state.body + chunk
		n 
	end
	api.flush = fun(_: Null) -> Bool? do true end
	api.end = fun(_: Null) -> Bool? do true end

	{ res: state, api: api }
end

# =========================
# Router (closure-backed; no module globals)
# =========================

# Route record (private)
let _mkRoute = fun(method: Str, pattern: Str, rawHandler) -> {} do
	let m = _normMethod(method)
	let isPrefix = _hasPrefix(pattern, "/") and len(pattern) > 1 and substr(pattern, len(pattern) - 1, len(pattern)) == "/"

	# Decide once (explicit tag) and wrap into a handler object
	let isStream = _isStreaming(rawHandler)
	let h = if isStream then
		{ kind: "stream", call: rawHandler }
	else
		{ kind: "value", call: rawHandler }
	end

	{
		method: m,
		pattern: pattern,
		isPrefix: isPrefix,
		parts: _splitPath(pattern),
		handler: h,
		kind: h.kind
	}
end

# Path matcher → params or null
let _pathMatch = fun(parts: [Str], reqSegs: [Str], isPrefix: Bool) -> {}? do
	if isPrefix and len(reqSegs) < len(parts) then return end
	if not isPrefix and len(reqSegs) != len(parts) then return end
	let n = len(parts)
	let params = {}
	let i = 0
	while i < n do
		let rp = parts[i]
		let qp = reqSegs[i]
		if _isParamSeg(rp) then
			params[_paramName(rp)] = qp
		elif rp != qp then
			return
		end
		i = i + 1
	end
	params
end




let router = fun(_: Null) -> {} do
	let _routes = []
	let _mws = []   # array of (Handler)->Handler transformers

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
					best = { route: r, params: p }
					score = s
				end
			end
		end
		if best != null then return { route: best.route, params: best.params } end
		null
	end

	let _pathExists = fun(path: Str) -> Bool do
		let segs = _splitPath(path)
		for r in _routes do
			if _pathMatch(r.parts, segs, r.isPrefix) != null then return true end
		end
		false
	end

	let _core = fun(req: Request) -> Any? do
		let m = _match(req.method, req.url.path)
		if m == null then
			if _normMethod(req.method) == "OPTIONS" then return noContent(null) end
			if _pathExists(req.url.path) then return text(405, "method not allowed") end
			return text(404, "not found")
		end

		let rq = { method: req.method, url: req.url, headers: req.headers, body: req.body, pathParams: m.params }
		let rt = m.route

		# Compose middlewares (outermost last applies first) over the handler OBJECT
		let h = rt.handler
		let i = len(_mws) - 1
		while i >= 0 do
			h = _mws[i](h)
			i = i - 1
		end

		# Enforce handler kind
		if h.kind != "stream" and h.kind != "value" then
			fail("invalid handler kind: expected value or stream")
		end

		if h.kind == "stream" then
			let pack = _newResponder()
			let r1 = try(fun(_: Null) do 
				h.call(rq, pack.api) 
			end)
			if not r1.ok then
				return text(500, "internal error")
			end
			return { status: pack.res.status, headers: pack.res.headers, body: pack.res.body }
		end

		# value
		let r2 = try(fun(_: Null) do h.call(rq) end)
		if not r2.ok then
			return text(500, "internal error")
		end
		let out = r2.value

		if isType(out, _HttpError) then
			let hdrs = if mapHas(out, "headers") and out.headers != null then out.headers else {} end
			return { status: out.status, headers: hdrs, body: out.body }
		end

		if isType(out, type {status!: Int, headers!: {}, body!: Str}) then
			return out
		end

		let js = _json(out)
		if js == null then return text(500, "json encode error") end
		{ status: 200, headers: {"Content-Type": "application/json; charset=utf-8"}, body: js }
	end

	let _toInternal = fun(out) -> _Response do
		if out == null then
			let nt = noteGet(out)
			let msg = if nt != null then nt else "error" end
			return { status: 500, headers: {"Content-Type": "text/plain"}, body: msg }
		end
		if isType(out, type {status!: Int, headers!: {}, body!: Str}) then
			return { status: out.status, headers: out.headers, body: out.body }
		end
		let js = _json(out)
		if js == null then
			return { status: 500, headers: {"Content-Type": "text/plain"}, body: "json encode error" }
		end
		{ status: 200, headers: {"Content-Type": "application/json; charset=utf-8"}, body: js }
	end

	let api = {
		__kind: "nethttp.router",

		handle: fun(method: Str, pattern: Str, handler) -> Any do
			_routes = _routes + [_mkRoute(method, pattern, handler)]
			true
		end,

		mount: fun(prefix: Str, child) -> Bool do
			if not (mapHas(child, "__kind") and child.__kind == "nethttp.router") then
				fail("mount: child must be a Router")
			end
			let dump = child._dump()
			let pre = if _hasPrefix(prefix, "/") then prefix else "/" + prefix end
			for rr in dump.routes do
				let p = if _hasPrefix(rr.pattern, "/") then rr.pattern else "/" + rr.pattern end
				api.handle(rr.method, pre + p, rr.handler.call)  # re-register using raw call; kind will be recomputed
			end
			true
		end,

		# Register a middleware transformer: (Handler) -> Handler
		use: fun(mw) -> Bool do
			_mws = _mws + [mw]
			true
		end,

		_entryCall: fun(req: Request) -> _Response do
			_toInternal(_core(req))
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
# Middleware (built-ins)
# =========================
# IMPORTANT: middleware is now (Handler) -> Handler.
# Each built-in inspects h.kind and returns a new handler with the same kind.

let mwRecover = fun(_: Null) -> Any do
	return fun(h) do
		if h.kind == "stream" then
			return {
				kind: "stream",
				call: fun(req, res) -> Null do
					let r = try(fun(_: Null) do h.call(req, res) end)
					if not r.ok then
						printf("panic: %s\n", [r.error])
						res.status(500).setHeader("Content-Type", "text/plain").write("internal error")
						res.end(null)
					end
					null
				end
			}
		elif h.kind == "value" then
			return {
				kind: "value",
				call: fun(req: Request) -> Any? do
					let r2 = try(fun(_: Null) do h.call(req) end)
					if not r2.ok then
						printf("panic: %s\n", [r2.error])
						return text(500, "internal error")
					end
					r2.value
				end
			}
		end
		fail("invalid handler kind in mwRecover")
	end
end

let mwRequestID = fun(headerName: Str) -> Any do
	let name = headerName
	return fun(h) do
		if h.kind == "stream" then
			return {
				kind: "stream",
				call: fun(req, res) -> Null do
					let id = if mapHas(req.headers, name) and req.headers[name] != null then 
						req.headers[name] 
					else 
						sprintf("r-%d", [nowNanos()]) 
					end
					req.headers[name] = id
					res.setHeader(name, id)
					h.call(req, res)
					null
				end
			}
		elif h.kind == "value" then
			return {
				kind: "value",
				call: fun(req: Request) -> Any? do
					let id2 = if mapHas(req.headers, name) and req.headers[name] != null then
						req.headers[name]
					else
						sprintf("r-%d", [nowNanos()])
					end
					req.headers[name] = id2

					let out = h.call(req)
					if out == null then
						return out
					end

					# If it's already a response map, inject the header and pass through.
					if isType(out, type {status!: Int, headers!: {}, body!: Str}) then
						out.headers[name] = id2
						return out
					end

					# Implicit JSON case: wrap into a response so we can add the header.
					let resp = json(200, out)
					resp.headers[name] = id2
					return resp
				end
			}
		end
		fail("invalid handler kind in mwRequestID")
	end
end

let mwTimeout = fun(ms: Int) -> Any do
	let dur = ms
	return fun(h) do
		if h.kind == "stream" then
			return {
				kind: "stream",
				call: fun(req, res) -> Null do
					let pWorker = procSpawn(fun(_: Null) -> {} do h.call(req, res) end)
					let pTimer = procSpawn(fun(_: Null) -> {} do
						let t = timerAfter(dur)
						chanRecv(t)
						{ timeout: true }
					end)
					let r = procJoinAny([pWorker, pTimer])
					let timedOut = isType(r.value, type {timeout!: Bool}) and bool(r.value.timeout)
					if timedOut then
						res.status(504).setHeader("Content-Type", "text/plain").write("timeout")
						res.end(null)
						procCancel(pWorker)
					else
						procCancel(pTimer)
					end
					null
				end
			}
		elif h.kind == "value" then
			return {
				kind: "value",
				call: fun(req: Request) -> Any? do
					let pWorker = procSpawn(fun(_: Null) -> {} do h.call(req) end)
					let pTimer = procSpawn(fun(_: Null) -> {} do
						let t = timerAfter(dur)
						chanRecv(t)
						{ timeout: true }
					end)
					let r = procJoinAny([pWorker, pTimer])
					let timedOut = isType(r.value, type {timeout!: Bool}) and bool(r.value.timeout)
					if timedOut then
						procCancel(pWorker)
						return text(504, "timeout")
					end
					procCancel(pTimer)
					r.value
				end
			}
		end
		fail("invalid handler kind in mwTimeout")
	end
end

let mwCors = fun(opts: {}) -> Any do
	let origin = if mapHas(opts, "origin") and opts.origin != null then opts.origin else "*" end
	let methods = if mapHas(opts, "methods") and opts.methods != null then join(opts.methods, ", ") else "GET, POST, PUT, PATCH, DELETE, OPTIONS" end
	let allowH = if mapHas(opts, "headers") and opts.headers != null then join(opts.headers, ", ") else "Content-Type, Authorization" end
	let creds = if mapHas(opts, "credentials") and opts.credentials != null and bool(opts.credentials) then "true" else "false" end

	return fun(h) do
		if h.kind == "stream" then
			return {
				kind: "stream",
				call: fun(req, res) -> Null do
					res.setHeader("Access-Control-Allow-Origin", origin)
					res.setHeader("Access-Control-Allow-Methods", methods)
					res.setHeader("Access-Control-Allow-Headers", allowH)
					res.setHeader("Access-Control-Allow-Credentials", creds)
					if req.method == "OPTIONS" then
						res.status(204).end(null)
						return null
					end
					h.call(req, res)
					null
				end
			}
		elif h.kind == "value" then
			return {
				kind: "value",
				call: fun(req: Request) -> Any? do
					if req.method == "OPTIONS" then
						return noContent(null)
					end
					let v = h.call(req)
					if v == null then return v end
					if isType(v, type {status!: Int, headers!: {}, body!: Str}) then
						v.headers["Access-Control-Allow-Origin"] = origin
						v.headers["Access-Control-Allow-Methods"] = methods
						v.headers["Access-Control-Allow-Headers"] = allowH
						v.headers["Access-Control-Allow-Credentials"] = creds
					end
					v
				end
			}
		end
		fail("invalid handler kind in mwCors")
	end
end

let mwAccessLog = fun(_: Null) -> Any do
	return fun(h) do
		if h.kind == "value" then
			return {
				kind: "value",
				call: fun(req: Request) -> Any? do
					let t0 = nowMillis()
					let out = h.call(req)
					let dur = nowMillis() - t0

					# Derive status & size without mutating framework internals.
					let status = 200
					let size = 0

					if out == null then
						# annotated null => treat as error; framework will map to 500
						status = 500
					elif isType(out, type {status!: Int, headers!: {}, body!: Str}) then
						status = out.status
						size = len(out.body)
					elif isType(out, _HttpError) then
						status = out.status
						size = if out.body == null then 0 else len(out.body) end
					else
						# Implicit JSON success path; approximate size with encoded form
						let js = _json(out)
						if js == null then
							status = 500
							size = 0
						else
							status = 200
							size = len(js)
						end
					end

					printf("%s \"%s %s\" %d %d %dms\n", [
						"-",           # remote (unknown in Request for now)
						req.method,
						req.url.path,
						int(status),
						int(size),
						int(dur)
					])

					out
				end
			}
		elif h.kind == "stream" then
			return {
				kind: "stream",
				call: fun(req, res) -> Null do
					let t0 = nowMillis()
					let status = 200
					let bytes = 0

					# Wrap the responder to observe status/bytes.
					let orig = res
					let proxy = {}
					proxy.status = fun(code: Int) -> {} do
						status = code
						orig.status(code)
						proxy
					end
					proxy.setHeader = fun(k: Str, v: Str) -> {} do
						orig.setHeader(k, v)
						proxy
					end
					proxy.write = fun(chunk: Str) -> Int? do
						let n = len(chunk)
						let r = orig.write(chunk)
						bytes = bytes + n
						r
					end
					proxy.flush = fun(_: Null) -> Bool? do
						orig.flush(null)
					end
					proxy.end = fun(_: Null) -> Bool? do
						orig.end(null)
					end

					h.call(req, proxy)

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
			}
		end
		fail("invalid handler kind in mwAccessLog")
	end
end

# =========================
# Server lifecycle (HTTP/1.1 MVP)
# =========================

# Minimal server handle for shutdown.
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

	# Defaults (small + sensible)
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
		opts.maxBodyBytes = 1048576  # 1 MiB
	end

	let server = { listener: listener, stop: {closed: false}, conns: [], opts: opts }

	# Accept loop in a background process.
	let _ = procSpawn(fun(_: Null) -> Null do
		while not server.stop.closed do
			let c = netAccept(listener)
			if c == null then
				# transient accept error; keep going unless closing
				continue
			end
			# Track connection
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
	if server == null then
		return true
	end
	server.stop.closed = true
	# Best effort: stop listening and close all open connections.
	close(server.listener)

	# Close tracked connections (best effort).
	let i = 0
	while i < len(server.conns) do
		let c = server.conns[i]
		# Ignore errors on close.
		close(c)
		i = i + 1
	end
	server.conns = []
	true
end

# =========================
# Internal HTTP/1.1 helpers (MVP)
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
	# Use readLine (non-CRLF-aware), then trim a trailing '\r' if present.
	# Enforce header size budget by subtracting consumed bytes (approximate).
	let line = readLine(c)
	if line == null then
		return null
	end
	let s = _trimCR(line)
	if remaining != null then
		let left = int(remaining) - len(s) - 2  # crude account for CRLF
		if left < 0 then
			return null # <headers too large>
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
			return null # <bad headers or too large>
		end
		if len(line) == 0 then
			break
		end
		# Split on first colon
		let parts = split(line, ":")
		if len(parts) < 2 then
			return null # <malformed header>
		end
		let key = parts[0]
		let value = strip(join(slice(parts, 1, len(parts)), ":"))
		headers[key] = value
		# shrink budget roughly
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
		if chunk == null then
			return null # <read body failed>
		end
		buf = buf + chunk
		remaining = remaining - len(chunk)
		if len(chunk) == 0 then
			# EOF before Content-Length — treat as error
			return null # <early eof>
		end
	end
	# If original n > cap, we signal payload too large by caller using length comparison.
	buf
end

let _lower = fun(s: Str) -> Str do toLower(s) end
let _upper = fun(s: Str) -> Str do toUpper(s) end

let _readRequest = fun(conn, opts: {}) -> {}? do
	# Request line
	let line = readLine(conn)
	if line == null then
		return null
	end
	let reqLine = _trimCR(line)
	let parts = split(reqLine, " ")
	if len(parts) != 3 then
		return null # <bad request line>
	end
	let method = _upper(parts[0])
	let target = parts[1]
	let proto = parts[2]
	if proto != "HTTP/1.1" then
		return null # <unsupported proto>
	end

	# Headers (budgeted)
	let maxHdr = int(opts.maxHeaderBytes)
	let hdrs = _readHeaders(conn, maxHdr)
	if hdrs == null then
		return null
	end

	# Keep-Alive
	let keepAlive = true
	if mapHas(hdrs, "Connection") and _lower(hdrs["Connection"]) == "close" then
		keepAlive = false
	end

	# Target parse
	let t = _parseTarget(target)

	# Body (Content-Length only in MVP)
	let bodyStr = ""
	if mapHas(hdrs, "Content-Length") then
		let n = int(hdrs["Content-Length"])
		if n == null or n < 0 then
			return null # <bad content-length>
		end
		let cap = int(opts.maxBodyBytes)
		let s = _readBodyContentLength(conn, n, cap)
		if s == null then
			return null # <read body failed>
		end
		if n > cap then
			# Drain the rest of the declared body to keep the connection sane.
			let toDrop = n - cap
			while toDrop > 0 do
				let d = readN(conn, if toDrop > 4096 then 4096 else toDrop end)
				if d == null or len(d) == 0 then
					break
				end
				toDrop = toDrop - len(d)
			end
			# Mark as too large (caller maps to 413)
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
		if uid(it) != uid(c) then
			out = out + [it]
		end
		i = i + 1
	end
	out
end

let _connLoop = fun(conn, router, server) -> Null do
	# Simple per-connection loop (keep-alive). No idle/read/write deadlines in MVP.
	while true do
		let rr = _readRequest(conn, server.opts)
		if rr == null then
			# Could not parse request line/headers
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
		if not ok or not rr.keepAlive then
			break
		end
	end

	close(conn)
	server.conns = _removeConn(server.conns, conn)
	null
end

# =========================
# Tiny docs (optional)
# =========================

let openapi = fun(r) -> {}? do
	if not (mapHas(r, "_dump")) then
		return null # <not a router>
	end
	let dump = r._dump()
	let rs = []
	for rr in dump.routes do
		rs = rs + [{ method: rr.method, pattern: rr.pattern, streaming: rr.kind == "stream" }]
	end
	{ routes: rs }
end
