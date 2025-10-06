# nethttp/middleware — streaming-friendly middleware built on router core
#
# This module provides common streaming-compatible middleware:
#   - mwRecover: convert handler panics to 500 responses
#   - mwRequestID: inject/propagate a request identifier header
#   - mwTimeout: emit 504 if nothing was written before a deadline
#   - mwCors: add CORS headers and handle OPTIONS preflights
#   - mwAccessLog: log method/route/status/bytes/duration after responses

let router = import("nethttp/router")

# Re-exported core types and internal helpers (private to this module).
let Request   = router.Request
let Responder = router.Responder
let _StreamFn = router._StreamFn
let _Mw       = router._Mw

let _getHeader = router._getHeader
let _setErr    = router._setErr

# mwRecover — Recover from panics and return 500.
#
# Summary:
# 	Wraps a streaming handler to catch panics and convert them to
# 	"500 internal server error" responses. Intended as the first middleware.
#
# Args:
# 	_: Null — Unused marker.
# Return:
# 	_Mw — Middleware function: (_StreamFn) -> _StreamFn.
let mwRecover = fun(_: Null) -> _Mw do
	fun(next: _StreamFn) -> _StreamFn do
		fun(req: Request, res: Responder, ctx: {}) -> Null do
			let r = try(fun(_: Null) do next(req, res, ctx) end)
			if not r.ok then
				_setErr(ctx, r)
				res.status(500).setHeader("Content-Type", "text/plain")
				let _ = res.write("internal server error")
				let _ = res.end(null)
				return
			end
			null
		end
	end
end

# mwRequestID — Inject or propagate a request identifier.
#
# Summary:
# 	Sets a request/response header with a request id. If the incoming request
# 	already has the header, it is propagated; otherwise a new id is generated.
#
# Args:
# 	headerName: Str — Header key to use (e.g., "X-Request-ID").
# Return:
# 	_Mw — Middleware function.
let mwRequestID = fun(headerName: Str) -> _Mw do
	let name = headerName
	fun(next: _StreamFn) -> _StreamFn do
		fun(req: Request, res: Responder, ctx: {}) -> Null do
			let seen = _getHeader(req.headers, name)
			let id = if seen != null then seen else sprintf("r-%d", [nowNanos()]) end
			req.headers[name] = id
			res.setHeader(name, id)
			next(req, res, ctx)
			null
		end
	end
end

# mwTimeout — Emit 504 if no bytes were written before the deadline.
#
# Summary:
# 	Tracks writes on the response; if the deadline passes and no bytes
# 	were sent, it emits a "504 timeout" response automatically.
#
# Args:
# 	ms: Int — Timeout in milliseconds.
# Return:
# 	_Mw — Middleware function.
let mwTimeout = fun(ms: Int) -> _Mw do
	let dur = ms
	fun(next: _StreamFn) -> _StreamFn do
		fun(req: Request, res: Responder, ctx: {}) -> Null do
			let deadline = nowMillis() + dur
			let closed = {v: false}
			let wrote  = {v: false}
			ctx.deadlineMs = deadline

			let closeTimeout = fun(_: Null) -> Null do
				if closed.v then
					return
				end
				res.status(504).setHeader("Content-Type", "text/plain")
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
				wrote.v = true
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
				wrote.v = true
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

			next(req, proxy, ctx)
			if not closed.v and not wrote.v and timedOut(null) then
				closeTimeout(null)
			end
			null
		end
	end
end

# mwCors — Add CORS headers and handle OPTIONS preflights.
#
# Summary:
# 	Sets Access-Control-* headers on all responses and short-circuits
# 	OPTIONS requests with 204 No Content. When credentials are enabled,
# 	the Origin is reflected and Vary: Origin is set.
#
# Args:
# 	opts: {} — {
# 		origin?: Str (default "*"),
# 		methods?: [Str] (default "GET, POST, PUT, PATCH, DELETE, OPTIONS"),
# 		headers?: [Str] (default "Content-Type, Authorization"),
# 		credentials?: Bool (default false)
# 	}
# Return:
# 	_Mw — Middleware function.
let mwCors = fun(opts: {}) -> _Mw do
	let originOpt = if mapHas(opts, "origin") and opts.origin != null then opts.origin else "*" end
	let methods = if mapHas(opts, "methods") and opts.methods != null then join(opts.methods, ", ") else "GET, POST, PUT, PATCH, DELETE, OPTIONS" end
	let allowH = if mapHas(opts, "headers") and opts.headers != null then join(opts.headers, ", ") else "Content-Type, Authorization" end
	let credsOn = mapHas(opts, "credentials") and opts.credentials != null and bool(opts.credentials)

	fun(next: _StreamFn) -> _StreamFn do
		fun(req: Request, res: Responder, ctx: {}) -> Null do
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
				let _ = res.end(null)
				return
			end
			next(req, res, ctx)
			null
		end
	end
end

# mwAccessLog — Log status, bytes, and duration after responses.
#
# Summary:
# 	Wraps a handler to record method/route/bytes/status/duration and prints
# 	a colored single-line entry after the response completes. Non-2xx
# 	responses also print an error context if available.
#
# Args:
# 	_: Null — Unused marker.
# Return:
# 	_Mw — Middleware function.
let mwAccessLog = fun(_: Null) -> _Mw do
	fun(next: _StreamFn) -> _StreamFn do
		fun(req: Request, res: Responder, ctx: {}) -> Null do
			let t0 = nowMillis()
			let status = {v: 200}
			let bytes  = {v: 0}

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
				orig.flush(null)
			end
			proxy.end = fun(_: Null) -> Bool? do
				orig.end(null)
			end

			next(req, proxy, ctx)

			let dur = nowMillis() - t0
			let route = req.url.path + if req.url.query == null then "" else "?" + req.url.query end
			let ok = status.v < 400
			let tag = if ok then "OK" else "FAIL" end

			let colorOn = true
			let GRN = if colorOn then "\u001b[32m" else "" end
			let RED = if colorOn then "\u001b[31m" else "" end
			let DIM = if colorOn then "\u001b[2m"  else "" end
			let RST = if colorOn then "\u001b[0m"  else "" end

			let tagC = if ok then GRN + tag + RST else RED + tag + RST end
			let statusC = if ok then GRN + str(int(status.v)) + RST else RED + str(int(status.v)) + RST end

			printf("\"%s %s\" %dbytes %dms %s %s\n", [
				req.method, route, int(bytes.v), int(dur), statusC, tagC
			])

			if not ok and mapHas(ctx, "_errorDump") and ctx._errorDump != null then
				printf("%s%s%s\n", [DIM, str(ctx._errorDump), RST])
			end
			null
		end
	end
end
