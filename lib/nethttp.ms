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
	method: Str,
	url: {scheme: Str?, host: Str?, path: Str, query: Str?},
	headers: {Str: Str},
	body: Any, # readable handle OR raw Str (handled by helpers)
	pathParams: {Str: Str}?
}

# NOTE: arrow types (right-associative), not fun(...) in type fields.
let Responder = type {
	status: Int -> Responder,
	setHeader: Str -> Str -> Responder,
	write: Str -> Int?, # may return null # <io>
	flush: Null -> Bool?, # may return null # <io>
	end: Null -> Bool? # may return null # <io>
}

let Response = type {status: Int, headers: {Str: Str}, body: Str}

let Router = type {
	handle: Str -> Str -> Any -> Any, # method -> path -> handler -> handler
	mount: Str -> Any -> Bool, # prefix -> childRouter -> ok
	use: Any -> Bool # mw -> ok
}

# =========================
# Internal router state
# =========================

let _mkRouter = fun(_: Null) -> {} do
	return {
		_routes: [], # [{method, path, parts, isPrefix, handler, kind}]
		_mw: [] # [mwFun]
	}
end

let _splitPath = fun(p: Str) -> [Str] do
	if len(p) == 0 then
		return []
	end
	let clean = if substr(p, 0, 1) == "/" then
		substr(p, 1, len(p))
	else
		p
	end
	if len(clean) == 0 then
		return []
	end
	return split(clean, "/")
end

let _isPrefixRoute = fun(path: Str) -> Bool do
	# Convention: path ending with "/" matches as subtree prefix (e.g., "/static/")
	if len(path) == 0 then
		return false
	end
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
	let rparts = _splitPath(reqPath)

	let best = null
	let bestLen = -1
	let pathHit = false

	for rt in R._routes do
		let segmentsMatch = fun(_: Null) -> Bool do
			let i = 0
			while i < len(rt.parts) do
				let a = rt.parts[i]
				let b = rparts[i]
				let isParam = len(a) >= 2 and substr(a, 0, 1) == "{" and substr(a, len(a) - 1, len(a)) == "}"
				if not (isParam or a == b) then
					return false
				end
				i = i + 1
			end
			return true
		end

		# pathOk: prefix or same-length with {param} allowed
		let pathOk = if rt.isPrefix then
			len(rt.path) == 0 or substr(reqPath, 0, len(rt.path)) == rt.path
		else
			let sameLen = len(rt.parts) == len(rparts)
			if not sameLen then
				false
			else
				segmentsMatch(null)
			end
		end

		if pathOk then
			pathHit = true
		end
		if not pathOk or rt.method != m then
			continue
		end

		# prefer longest; extract params only when selecting
		if len(rt.path) > bestLen then
			let params = if rt.isPrefix then
				{}
			else
				let ps = {}
				let i = 0
				while i < len(rt.parts) do
					let a = rt.parts[i]
					let b = rparts[i]
					let isParam = len(a) >= 2 and substr(a, 0, 1) == "{" and substr(a, len(a) - 1, len(a)) == "}"
					if isParam then
						ps[substr(a, 1, len(a) - 1)] = b
					end
					i = i + 1
				end
				ps
			end
			best = {route: rt, params: params}
			bestLen = len(rt.path)
		end
	end

	if best != null then
		return best
	end
	if pathHit then
		return {methodNotAllowed: true}
	end
	return
end

# Compose value-returning middleware left-to-right: mw(next)->handler
let _chainValue = fun(mws: [Any], leaf) do
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
	let R = {_routes: [], _mw: []} # private mutable state

	let out = {_routes: [], _mw: []} # exported view (kept in sync)

	let _sync = fun(_: Null) -> Null do
		out._routes = R._routes
		out._mw = R._mw
		null
	end

	let handle = fun(method: Str, path: Str, handler) -> Any do
		let c = _compileRoute(method, path)

		# Non-recursive ResponderShape for classification only
		let ResponderShape = type {
			status: Int -> Any,
			setHeader: Str -> Str -> Any,
			write: Str -> Any?, flush: Null -> Any?, end: Null -> Any?
		}
		let isStream = isType(handler, type Request -> ResponderShape -> Null) or
					isType(handler, type Request -> (ResponderShape -> Null))
		let returnsFunction = isType(handler, type Request -> Any -> Any)
		let isValue  = (not isStream) and (not returnsFunction) and
					isType(handler, type Request -> Any?)

		if not isStream and not isValue then
			fail("handle: handler must be Request->Any? or Request->Responder->Null")
		end

		let kind = if isStream then "stream" else "value" end

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
				else
					prefix
				end
				let suf = if substr(rt.path, 0, 1) == "/" then
					substr(rt.path, 1, len(rt.path))
				else
					rt.path
				end
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
	let q = if req.url.query == null then
		""
	else
		req.url.query
	end
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
	let obj = {}

	let auto = fun(s: Str) do
		let j = jsonParse(s)
		if j != null then
			return j
		end # handles numbers, true/false, arrays, objects

		return s # bare strings remain strings

	end

	for [k, vs] in qm do
		if len(vs) == 1 then
			obj[k] = auto(vs[0])
		else
			let out = []
			for s in vs do
				out = out + [auto(s)]
			end
			obj[k] = out
		end
	end

	if isType(obj, T) then
		return obj
	end
	return noteSet("<validation>", null)
end

# =========================
# Response builders
# =========================

let json = fun(status: Int, v) -> Response do
	let body = jsonStringify(v)
	let s = if body == null then
		formatValue(v)
	else
		body
	end
	return {
		status: status,
		headers: {"Content-Type": "application/json"},
		body: s
	}
end

let text = fun(status: Int, s: Str) -> Response do
	return {
		status: status,
		headers: {"Content-Type": "text/plain; charset=utf-8"},
		body: s
	}
end

let noContent = fun(_: Null) -> Response do
	return {status: 204, headers: {}, body: ""}
end

let redirect = fun(status: Int, location: Str) -> Response do
	return {status: status, headers: {Location: location}, body: ""}
end

let raise = fun(status: Int, message: Str) -> Null do
	return noteSet(sprintf("<status:%d message:%s>", [status, message]), null)
end

# =========================
# Middleware (minimal)
# =========================

let mwRecover = fun(_: Null) do
	return fun(next) do
		return fun(req: Request) -> Any? do
			let r = try(fun(_: Null) do
				next(req)
			end)
			if r.ok then
				return r.value
			end
			return json(500, {error: "internal error"})
		end
	end
end

let mwRequestID = fun(headerName: Str) do
	let H = if len(headerName) == 0 then
		"X-Request-ID"
	else
		headerName
	end
	return fun(next) do
		return fun(req: Request) -> Any? do
			let idv = sprintf("%d", [nowNanos()])
			req.ctx = if mapHas(req, "ctx") and req.ctx != null then
				req.ctx
			else
				{}
			end
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

let mwTimeout = fun(ms: Int) do
	let dur = if ms <= 0 then
		0
	else
		ms
	end
	return fun(next) do
		return fun(req: Request) -> Any? do
			if dur == 0 then
				return next(req)
			end

			let worker = procSpawn(fun(_: Null) -> Any? do
				return next(req)
			end)
			let timerP = procSpawn(fun(_: Null) -> {} do
				sleep(dur)
				return {timeout: true}
			end)

			let first = procJoinAny([worker, timerP])
			if first.index == 0 then
				# handler won
				procCancel(timerP)
				let v = first.value
				return v
			else
				# timeout won
				procCancel(worker)
				return json(504, {error: "timeout"})
			end
		end
	end
end

let mwCors = fun(opts: {}) do
	let allowOrigin = if mapHas(opts, "allowOrigin") and opts.allowOrigin != null then
		opts.allowOrigin
	else
		"*"
	end
	let allowMethods = if mapHas(opts, "allowMethods") and opts.allowMethods != null then
		opts.allowMethods
	else
		"GET,POST,PUT,PATCH,DELETE,OPTIONS"
	end
	let allowHeaders = if mapHas(opts, "allowHeaders") and opts.allowHeaders != null then
		opts.allowHeaders
	else
		"Content-Type,Authorization"
	end
	return fun(next) do
		return fun(req: Request) -> Any? do
			if toUpper(req.method) == "OPTIONS" then
				let r = noContent(null)
				r.headers["Access-Control-Allow-Origin"] = allowOrigin
				r.headers["Access-Control-Allow-Methods"] = allowMethods
				r.headers["Access-Control-Allow-Headers"] = allowHeaders
				return r
			end
			let out = next(req)
			let r2 = if out != null and isType(out, type {
				status: Int,
				headers: {Str: Str},
				body: Str
			}) then
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
# Server internals (compact helpers)
# =========================

# Options (kept tiny)
# readHeaderTimeoutMs?: Int
# readTimeoutMs?: Int
# writeTimeoutMs?: Int
# idleTimeoutMs?: Int
# maxHeaderBytes?: Int
# maxBodyBytes?: Int
let _normOpts = fun(opts: {}) -> {} do
	let rh = if mapHas(opts, "readHeaderTimeoutMs") then
		int(opts.readHeaderTimeoutMs)
	else
		0
	end
	let rd = if mapHas(opts, "readTimeoutMs") then
		int(opts.readTimeoutMs)
	else
		0
	end
	let wr = if mapHas(opts, "writeTimeoutMs") then
		int(opts.writeTimeoutMs)
	else
		0
	end
	let id = if mapHas(opts, "idleTimeoutMs") then
		int(opts.idleTimeoutMs)
	else
		0
	end
	let mh = if mapHas(opts, "maxHeaderBytes") then
		int(opts.maxHeaderBytes)
	else
		8192
	end
	let mb = if mapHas(opts, "maxBodyBytes") then
		int(opts.maxBodyBytes)
	else
		1048576
	end
	return {
		readHeaderTimeoutMs: rh,
		readTimeoutMs: rd,
		writeTimeoutMs: wr,
		idleTimeoutMs: id,
		maxHeaderBytes: mh,
		maxBodyBytes: mb
	}
end

# Blocking race between work() and a timeout.
let _raceTimeout = fun(ms: Int, work) -> {} do
	if ms <= 0 then
		return {index: 0, value: work(null)}
	end
	let w = procSpawn(fun(_: Null) do
		return work(null)
	end)
	let t = procSpawn(fun(_: Null) -> {} do
		sleep(ms)
		return {timeout: true}
	end)
	let r = procJoinAny([w, t])
	if r.index == 0 then
		procCancel(t)
	else
		procCancel(w)
	end
	return r
end

# Write full string with a single timeout budget.
let _writeAll = fun(conn, s: Str, timeoutMs: Int) -> Bool do
	let send = fun(_: Null) -> Bool do
		let off = 0
		let total = len(s)
		while off < total do
			let n = write(conn, substr(s, off, total))
			if n == null or int(n) <= 0 then
				return false
			end
			off = off + int(n)
		end
		return true
	end
	if timeoutMs <= 0 then
		return send(null)
	end
	let r = _raceTimeout(timeoutMs, send)
	return r.index == 0 and bool(r.value)
end

# Minimal status text mapper.
let _statusText = fun(code: Int) -> Str do
	if code == 200 then
		return "OK"
	end
	if code == 201 then
		return "Created"
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
	if code == 500 then
		return "Internal Server Error"
	end
	if code == 504 then
		return "Gateway Timeout"
	end
	return "OK"
end

# Read one CRLF-terminated line with timeout; returns Str? (annotated null on timeout/IO).
let _readLine = fun(conn, timeoutMs: Int) -> Str? do
	let r = _raceTimeout(timeoutMs, fun(_: Null) -> Str? do
		return readLine(conn)
	end)
	if r.index != 0 then
		return noteSet("<timeout>", null)
	end
	return r.value
end

# Parse request start-line + headers; enforces readHeaderTimeoutMs and maxHeaderBytes.
# Returns { method, path, query, headers }? (annotated null on error).
let _readHead = fun(conn, opts: {}) -> {}? do
	let t = _normOpts(opts)
	let total = 0

	# DEBUG
	println("[nethttp] _readHead: start")

	let first = _readLine(conn, t.readHeaderTimeoutMs)
	if first == null then
		return noteSet("<read header>", null)
	end
	total = total + len(first) + 2

	# DEBUG
	println(sprintf("[nethttp] _readHead: first=%q", [first]))

	let parts = split(strip(first), " ")
	if len(parts) < 3 then
		return noteSet("<bad request line>", null)
	end
	let method = toUpper(parts[0])
	let full = parts[1]
	let pq = split(full, "?")
	let path = if len(pq) >= 1 then
		pq[0]
	else
		"/"
	end
	let query = if len(pq) == 2 then
		pq[1]
	else
		""
	end

	let H = {}
	while true do
		if total > t.maxHeaderBytes then
			return noteSet("<headers too large>", null)
		end
		let line = _readLine(conn, t.readHeaderTimeoutMs)
		if line == null then
			return noteSet("<read header>", null)
		end
		if len(strip(line)) == 0 then
			break
		end
		total = total + len(line) + 2
		let kv = split(line, ":")
		if len(kv) >= 2 then
			let k = strip(kv[0])
			let v = strip(join(slice(kv, 1, len(kv)), ":"))
			H[k] = v
		end
	end

	return {method: method, path: path, query: query, headers: H}
end

# Buffered body read using Content-Length; enforces maxBodyBytes and readTimeoutMs.
let _readBody = fun(conn, headers: {}, opts: {}) -> Str? do
	let t = _normOpts(opts)
	let rawLen = if mapHas(headers, "Content-Length") then
		headers["Content-Length"]
	else
		"0"
	end
	let want = int(rawLen)
	if want == null or want <= 0 then
		return ""
	end
	if want > t.maxBodyBytes then
		return noteSet("<payload too large>", null)
	end

	let r = _raceTimeout(t.readTimeoutMs, fun(_: Null) -> Str? do
		let s = readN(conn, want)
		if s == null or len(s) != want then
			return noteSet("<read body>", null)
		end
		return s
	end)
	if r.index != 0 then
		return noteSet("<timeout>", null)
	end
	return r.value
end

# Hex helper for chunk sizes.
let _hex = fun(n: Int) -> Str do
	if n == 0 then
		return "0"
	end
	let digits = "0123456789abcdef"
	let out = ""
	let x = n
	while x > 0 do
		let d = x % 16
		out = substr(digits, d, d + 1) + out
		x = int(x / 16)   # keep integer division; avoid Num promotion
	end
	return out
end

# Value response writer (adds Content-Length, Date, Connection).
let _writeResponse = fun(conn, resp: Response, opts: {}) -> Bool do
	let t = _normOpts(opts)
	let body = if resp.body == null then
		""
	else
		resp.body
	end
	let headers = clone(resp.headers)

	# honor explicit Connection from caller (e.g., "close")
	if not mapHas(headers, "Connection") then
		headers["Connection"] = "keep-alive"
	end

	if not mapHas(headers, "Content-Length") then
		headers["Content-Length"] = sprintf("%d", [len(body)])
	end
	if not mapHas(headers, "Date") then
		headers["Date"] = timeFormatRFC3339(nowMillis())
	end

	let head = sprintf("HTTP/1.1 %d %s\r\n", [
		resp.status,
		_statusText(resp.status)
	])
	for [k, v] in headers do
		head = head + sprintf("%s: %s\r\n", [k, v])
	end
	head = head + "\r\n" + body
	return _writeAll(conn, head, t.writeTimeoutMs)
end

# Streaming responder (chunked). setHeader is ignored after first write/status.
let _makeChunkedResponder = fun(conn, opts: {}) -> Responder do
	let t = _normOpts(opts)
	let wrote = {v: false}
	let status = {v: 200}
	let H = {"Transfer-Encoding": "chunked", Connection: "keep-alive"}

	let _flushHead = fun(_: Null) -> Bool do
		if wrote.v then
			return true
		end
		wrote.v = true
		if not mapHas(H, "Date") then
			H["Date"] = timeFormatRFC3339(nowMillis())
		end
		let s = sprintf("HTTP/1.1 %d %s\r\n", [status.v, _statusText(status.v)])
		for [k, v] in H do
			s = s + sprintf("%s: %s\r\n", [k, v])
		end
		s = s + "\r\n"
		return _writeAll(conn, s, t.writeTimeoutMs)
	end

	let _chunk = fun(s: Str) -> Bool do
		let c = sprintf("%s\r\n%s\r\n", [_hex(len(s)), s])
		return _writeAll(conn, c, t.writeTimeoutMs)
	end

	let res = {
		status: fun(code: Int) -> Responder do
			status.v = code
			return res
		end,
		setHeader: fun(k: Str, v: Str) -> Responder do
			if not wrote.v then
				H[k] = v
			end
			return res
		end,
		write: fun(s: Str) -> Int? do
			if not _flushHead(null) then
				return noteSet("<write head>", null)
			end
			if not _chunk(s) then
				return noteSet("<write body>", null)
			end
			return len(s)
		end,
		flush: fun(_: Null) -> Bool? do
			return true
		end,
		end: fun(_: Null) -> Bool? do
			if not wrote.v then
				if not _flushHead(null) then
					return noteSet("<write head>", null)
				end
			end
			return _writeAll(conn, "0\r\n\r\n", t.writeTimeoutMs)
		end
	}
	return res
end

# Coerce handler output to Response (mirrors testClient).
let _normalizeOut = fun(out) -> Response do
	if out != null and mapHas(out, "status") and mapHas(out, "headers") and mapHas(out, "body") then
		return out
	end
	return json(200, out)
end

# =========================
# Server lifecycle
# =========================

# Serve on an existing listener; returns handle { listener, opts, closing!: Bool, conns!: {Str:Any}, acpt!: Any }
# Bodies buffered (Content-Length). Keep-alive; idle timeout between requests.
let serve = fun(listener, r: Router, opts: {}) -> {}? do
	if listener == null then
		return noteSet("<accept>", null)
	end
	let S = {
		listener: listener,
		opts: _normOpts(opts),
		closing: false,
		conns: {},
		acpt: null
	}

	let _serveConn = fun(conn, id: Int) -> Null do
		let t = S.opts
		while true do
			# Apply idle timeout as a budget for the next head read.
			let head = fun(_: Null) -> {}? do
				if t.idleTimeoutMs <= 0 then
					return _readHead(conn, t)
				end
				let rto = _raceTimeout(t.idleTimeoutMs, fun(_: Null) -> {}? do
					return _readHead(conn, t)
				end)
				if rto.index != 0 then
					return
				end
				return rto.value
			end()

			# DEBUG
			println(sprintf("[nethttp] head=%s", [
				if head == null then
					"<null>"
				else
					sprintf("%s %s?%s", [head.method, head.path, head.query])
				end
			]))

			if head == null then
				break
			end

			# Honor client “Connection: close”
			let wantClose = fun(_: Null) -> Bool do
				if mapHas(head.headers, "Connection") then
					let v = toLower(head.headers["Connection"])
					return v == "close"
				end
				return false
			end()

			let m = _match(r, head.method, head.path)
			if m == null then
				let leaf = fun(_: Request) -> Any? do
					return json(404, {error: "not found"})
				end
				let req404 = {
					method: head.method,
					url: {
						scheme: "http",
						host: "",
						path: head.path,
						query: head.query
					},
					headers: head.headers,
					body: ""
				}
				let out404 = _chainValue(r._mw, leaf)(req404)
				let resp404 = _normalizeOut(out404)
				if wantClose then
					resp404.headers["Connection"] = "close"
				end
				if not _writeResponse(conn, resp404, t) then
					break
				end
				if wantClose then
					break
				end
				continue
			end
			if mapHas(m, "methodNotAllowed") and bool(m.methodNotAllowed) then
				let resp405 = json(405, {error: "method not allowed"})
				if wantClose then
					resp405.headers["Connection"] = "close"
				end
				if not _writeResponse(conn, resp405, t) then
					break
				end
				if wantClose then
					break
				end
				continue
			end

			let body = _readBody(conn, head.headers, t)
			if body == null then
				let resp413 = json(413, {
					error: "payload too large or read error"
				})
				if wantClose then
					resp413.headers["Connection"] = "close"
				end
				_writeResponse(conn, resp413, t)
				break
			end

			let req = {
				method: head.method,
				url: {
					scheme: "http",
					host: "",
					path: head.path,
					query: head.query
				},
				headers: head.headers,
				body: body,
				pathParams: m.params
			}

			let rt = m.route
			if rt.kind == "value" then
				let out = _chainValue(r._mw, rt.handler)(req)
				if out == null then
					let note = noteGet(out)
					let status = 500
					if note != null then
						let n = int(replace("^.*status:([0-9]+).*$", "$1", note))
						if n != null then
							status = n
						end
					end
					let errResp = json(status, {
						error: if note != null then
							note
						else
							"error"
						end
					})
					if wantClose then
						errResp.headers["Connection"] = "close"
					end
					if not _writeResponse(conn, errResp, t) then
						break
					end
					if wantClose then
						break
					end
				else
					let resp = _normalizeOut(out)
					if wantClose then
						resp.headers["Connection"] = "close"
					end
					if not _writeResponse(conn, resp, t) then
						break
					end
					if wantClose then
						break
					end
				end
			else
				# streaming: adapt to value-middleware shape so mws still run
				let wc = wantClose
				let chain = _chainValue(r._mw, fun(next) do
					return fun(req2: Request) -> Any? do
						let res = _makeChunkedResponder(conn, t)
						if wc then
							# must set before first write/head flush
							res.setHeader("Connection", "close")
						end
						rt.handler(req2, res)
						return noContent(null) # not written; streaming already wrote

					end
				end)
				let _ = chain(req)
				if wantClose then
					break
				end
			end
		end
		close(conn)
		mapDelete(S.conns, sprintf("%d", [id]))
		return
	end

	let acpt = procSpawn(fun(_: Null) -> Null do
		let i = 1
		while not S.closing do
			let c = netAccept(S.listener)

			# DEBUG
			if c != null then
				println("[nethttp] accept ok")
			end

			if c == null then
				break
			end
			S.conns[sprintf("%d", [i])] = c
			let id = i
			i = i + 1
			procSpawn(fun(_: Null) -> Null do
				_serveConn(c, id)
			end)
		end
		return
	end)
	S.acpt = acpt
	return S
end

# Graceful shutdown: close listener, wait up to timeout for conns, then force-close.
let shutdown = fun(server, timeoutMs: Int?) -> Bool do
	if server == null then
		return false
	end
	server.closing = true
	close(server.listener)

	let deadline = if timeoutMs == null or int(timeoutMs) <= 0 then
		0
	else
		nowMillis() + int(timeoutMs)
	end
	while len(server.conns) > 0 and (deadline == 0 or nowMillis() < deadline) do
		sleep(10)
	end
	if len(server.conns) > 0 then
		for [_, c] in server.conns do
			close(c)
		end
	end
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

# In-process test client (value + stream; mirrors server normalization).
let testClient = fun(r: Router) -> {} do
	let call = fun(req: {}) -> {status: Int, headers: {}, body: Str} do
		let method = toUpper(req.method)
		let p = req.path
		let m = _match(r, method, p)

		# Construct Request
		let urlObj = {
			scheme: "http",
			host: "test",
			path: p,
			query: if mapHas(req, "query") then req.query else "" end
		}
		let headers = if mapHas(req, "headers") and req.headers != null then req.headers else {} end
		let body = if mapHas(req, "body") and req.body != null then req.body else "" end

		# 404 / 405 handling with middleware
		if m == null then
			let leaf = fun(_: Request) -> Any? do
				return json(404, {error: "not found"})
			end
			let out = _chainValue(r._mw, leaf)({method: method, url: urlObj, headers: headers, body: body, pathParams: {}})
			if out != null and mapHas(out, "status") and mapHas(out, "headers") and mapHas(out, "body") then
				return out
			end
			return json(200, out)
		end
		if mapHas(m, "methodNotAllowed") and bool(m.methodNotAllowed) then
			return json(405, {error: "method not allowed"})
		end

		let rt = m.route
		let request = {method: method, url: urlObj, headers: headers, body: body, pathParams: m.params}

		if rt.kind == "value" then
			let out = _chainValue(r._mw, rt.handler)(request)
			if out == null then
				let note = noteGet(out)
				let code = if note != null then
					let n = int(replace("^.*status:([0-9]+).*$", "$1", note))
					if n != null then n else 500 end
				else 500 end
				return json(code, {error: if note != null then note else "error" end})
			end
			if out != null and mapHas(out, "status") and mapHas(out, "headers") and mapHas(out, "body") then
				return out
			end
			return json(200, out)
		end

		# Streaming branch: simulate responder that buffers writes
		let statusRef = {v: 200}
		let hdrs = {}
		let buf = ""
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
				buf = buf + s
				return len(s)
			end,
			flush: fun(_: Null) -> Bool? do
				return true
			end,
			end: fun(_: Null) -> Bool? do
				return true
			end
		}
		let _ = _chainValue(r._mw, fun(next) -> Any do
			return fun(req2: Request) -> Any? do
				rt.handler(req2, res)
				return noContent(null)
			end
		end)(request)
		return {status: statusRef.v, headers: hdrs, body: buf}
	end
	return {call: call}
end
