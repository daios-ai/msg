# nethttp/middleware — streaming-friendly middleware built on router core

let core = import("nethttp/router")

# Reuse core types and helpers (first-class Types and functions)
let Request   = core.Request
let Responder = core.Responder
let _StreamFn = core._StreamFn
let _Mw       = core._Mw

let _getHeader = core._getHeader
let _setErr    = core._setErr


let _getHeader = fun(h: {}, name: Str) -> Str? do
	let want = toLower(name)
	for [k, v] in h do
		if toLower(k) == want then
			return v
		end
	end
	null
end


# Recover middleware — converts handler panics to 500 responses.
#
# Summary:
#   Wraps next and catches panics (via try), returning "internal server error".
#   Also records a short panic reason on ctx._errorDump (for mwAccessLog).
# Args:
#   _: Null — Unused.
# Return: _Mw — Middleware function.
let mwRecover = fun(_: Null) -> _Mw do
	return fun(next: _StreamFn) -> _StreamFn do
		return fun(req: Request, res: Responder, ctx: {}) -> Null do
			let r = try(fun(_: Null) do next(req, res, ctx) end)
			if not r.ok then
				_setErr(ctx, r)    # NEW
				res.status(500).setHeader("Content-Type", "text/plain")
				let _ = res.write("internal server error")
				let _ = res.end()
				return
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
# Format:
#   "METHOD /path?query" <status> <n>bytes <dur>ms OK|FAIL
#   On FAIL, an error dump line follows.
# Color:
#   Set to enable ANSI colors (green OK, red FAIL, dim dump).
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
			let pathq = req.url.path + if req.url.query == null then "" else "?" + req.url.query end
			let ok = status.v < 500
			let tag = if ok then "OK" else "FAIL" end

			let colorOn = true
			let GRN = if colorOn then "\u001b[32m" else "" end
			let RED = if colorOn then "\u001b[31m" else "" end
			let DIM = if colorOn then "\u001b[2m"  else "" end
			let RST = if colorOn then "\u001b[0m"  else "" end
			let tagC = if ok then GRN + tag + RST else RED + tag + RST end

			printf("\"%s %s\" %d %dbytes %dms %s\n", [
				req.method, pathq, int(status.v), int(bytes.v), int(dur), tagC
			])

			if not ok and mapHas(ctx, "_errorDump") and ctx._errorDump != null then
				printf("%s%s%s\n", [DIM, str(ctx._errorDump), RST])
			end
			null
		end
	end
end
