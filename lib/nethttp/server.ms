# nethttp/server — HTTP/1.1 server lifecycle (minimal viable implementation)
# 
# This module wires a router to a plain TCP listener and serves HTTP/1.1.
# Behavior mirrors the original implementation; only naming, docs, and comments
# were polished to follow conventions.

let router = import("nethttp/router")
let _getHeader = router._getHeader

# Private server handle state.
let _ServerHandle = type {
	conns!: [Any],
	listener!: Any,
	opts!: {},
	stop!: {closed!: Bool}
}

# Map status code to reason phrase (polished list; logic unchanged).
let _reason = fun(code: Int) -> Str do
	if code == 200 then
		"OK"
	elif code == 204 then
		"No Content"
	elif code == 302 then
		"Found"
	elif code == 400 then
		"Bad Request"
	elif code == 401 then
		"Unauthorized"
	elif code == 404 then
		"Not Found"
	elif code == 405 then
		"Method Not Allowed"
	elif code == 413 then
		"Payload Too Large"
	elif code == 414 then
		"URI Too Long"
	elif code == 422 then
		"Unprocessable Entity"
	elif code == 431 then
		"Request Header Fields Too Large"
	elif code == 500 then
		"Internal Server Error"
	elif code == 504 then
		"Gateway Timeout"
	else
		"OK"
	end
end

# Trim a trailing CR from a line (HTTP/1.x CRLF handling).
let _trimCR = fun(s: Str) -> Str do
	let n = len(s)
	if n > 0 and substr(s, n - 1, n) == "\r" then
		substr(s, 0, n - 1)
	else
		s
	end
end

# Header value must not contain CR/LF.
let _validHeaderValue = fun(v: Str) -> Bool do
	match("\r|\n", v) == []
end

# Read a single line and enforce a remaining-byte budget (if provided).
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

# Read request headers up to byte and count limits.
# Returns map of headers, or special keys "__too_many__" / "__cl_conflict__".
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
			return {__too_many__: true}
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

# Parse the request-target into path/query. Absolute-form is rejected higher up.
let _parseTarget = fun(t: Str) -> {absolute: Bool?, path!: Str, query: Str?} do
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

# Read request body using Content-Length with a hard cap.
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

# Headers-only request parse; body is handled later (after 100-continue if any).
# On success returns {req, wantCL, keepAlive}; on structured errors returns a map
# with flags handled by the caller.
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
	let maxCnt = if mapHas(opts, "maxHeaders") then
		opts.maxHeaders
	else
		null
	end
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
		req: {
			method: method,
			url: {scheme: null, host: null, path: t.path, query: t.query},
			headers: hdrs,
			body: "",
			pathParams: {}
		},
		wantCL: wantCL,
		keepAlive: keepAlive
	}
end

# Write a complete response (status line, headers, body). Validates header values.
let _writeResponse = fun(conn, res: {body!: Str, headers!: {}, status!: Int}, opts: {}) -> Bool do
	let status = int(res.status)
	let headers = clone(res.headers)
	let body = if res.body == null then
		""
	else
		res.body
	end
	if mapHas(opts, "addDateHeader") and opts.addDateHeader != null and opts.addDateHeader and not mapHas(headers, "Date") then
		headers["Date"] = timeFormatRFC3339(nowMillis())
	end
	for [k, v] in headers do
		if not _validHeaderValue(v) then
			let _ = write(conn, sprintf("HTTP/1.1 400 %s\r\nContent-Length: 11\r\nContent-Type: text/plain\r\n\r\nbad request", [
				_reason(400)
			]))
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

# Remove a connection from the active list (by identity).
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

# Handle a single TCP connection: parse->route->respond loop.
let _connLoop = fun(conn, rtr: {}, server: _ServerHandle) -> Null do
	while true do
		let rr = _readRequest(conn, server.opts)
		if rr == null then
			let _ = write(conn, sprintf("HTTP/1.1 400 %s\r\nContent-Length: 11\r\nContent-Type: text/plain\r\n\r\nbad request", [
				_reason(400)
			]))
			break
		end
		if mapHas(rr, "tooLong") and bool(rr.tooLong) then
			let _ = write(conn, sprintf("HTTP/1.1 414 %s\r\nContent-Length: 8\r\nContent-Type: text/plain\r\n\r\ntoo long", [
				_reason(414)
			]))
			break
		end
		if mapHas(rr, "badRequest") and bool(rr.badRequest) then
			let _ = write(conn, sprintf("HTTP/1.1 400 %s\r\nContent-Length: 11\r\nContent-Type: text/plain\r\n\r\nbad request", [
				_reason(400)
			]))
			break
		end
		if mapHas(rr, "tooManyHeaders") and bool(rr.tooManyHeaders) then
			let _ = write(conn, sprintf("HTTP/1.1 431 %s\r\nContent-Length: 27\r\nContent-Type: text/plain\r\n\r\nrequest header fields too large", [
				_reason(431)
			]))
			break
		end

		let req = rr.req

		# Expect: 100-continue preflight (path+query only)
		let expect = _getHeader(req.headers, "Expect")
		if expect != null and toLower(expect) == "100-continue" then
			let pf = rtr._preflight(req)
			if pf == null or not pf.ok then
				let _ = write(conn, sprintf("HTTP/1.1 404 %s\r\nContent-Length: 9\r\nContent-Type: text/plain\r\n\r\nnot found", [
					_reason(404)
				]))
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
				let _ = write(conn, sprintf("HTTP/1.1 413 %s\r\nContent-Length: 15\r\nContent-Type: text/plain\r\n\r\npayload too large", [
					_reason(413)
				]))
				break
			end
			let s = _readBodyContentLength(conn, rr.wantCL, cap)
			if s == null then
				let _ = write(conn, sprintf("HTTP/1.1 400 %s\r\nContent-Length: 11\r\nContent-Type: text/plain\r\n\r\nbad request", [
					_reason(400)
				]))
				break
			end
			req.body = s
		end

		# Try streaming path first (for stream routes)
		let sres = rtr._entryCallStream(req, conn, server.opts)
		if sres != null and mapHas(sres, "streamed") and sres.streamed == true then
			if not rr.keepAlive then
				break
			end
			continue
		end

		# Buffered value route
		let out = rtr._entryCall(req)
		# Auto-HEAD for value routes: same headers (accurate Content-Length), no body
		if req.method == "HEAD" then
			let hdr = clone(out.headers)
			if not mapHas(hdr, "Content-Length") then
				hdr["Content-Length"] = str(len(out.body))
			end
			let ok = _writeResponse(conn, {
				status: out.status,
				headers: hdr,
				body: ""
			}, server.opts)
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

# serve — Start an HTTP/1.1 server with a router on a TCP listener.
# 
# Summary:
# Spawns an accept loop and per-connection handler loops. Supports basic
# limits (headers/body/start-line), 100-continue preflight, value and
# stream routes, keep-alive, and optional Date header injection.
# 
# Args:
# listener: Any — TCP listener handle returned by netListen.
# r: {} — A router value created by nethttp/router.
# opts: {} — {
# readHeaderTimeoutMs?: Int,
# writeTimeoutMs?: Int,
# idleTimeoutMs?: Int,
# maxHeaderBytes?: Int (default 8192),
# maxBodyBytes?: Int (default 1048576),
# maxStartLineBytes?: Int?,
# maxHeaders?: Int?,
# addDateHeader?: Bool?  # when true, adds Date header to responses
# }
# Return:
# _ServerHandle? — Handle used for shutdown; null on invalid inputs.
let serve = fun(listener, r: {}, opts: {}) -> _ServerHandle? do
	if listener == null then
		return
	end
	if not mapHas(r, "_entryCall") then
		return
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

# shutdown — Gracefully stop the server and close active connections.
# 
# Summary:
# Stops new accepts, closes the listener, and best-effort closes all
# active connections. No wait on in-flight work is enforced here.
# 
# Args:
# server: _ServerHandle? — Handle returned by serve.
# timeoutMs: Int? — Unused placeholder for future coordination.
# Return:
# Bool — true when the shutdown sequence ran (idempotent).
let shutdown = fun(server: _ServerHandle?, timeoutMs: Int?) -> Bool do
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