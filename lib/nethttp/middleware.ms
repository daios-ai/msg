# nethttp/middleware — streaming-friendly middleware built on router core

let core = import("nethttp/router")

# Reuse core types and helpers
let Request   = core.Request
let Responder = core.Responder
let _StreamFn = core._StreamFn
let _Mw       = core._Mw

let _getHeader = core._getHeader
let _setErr    = core._setErr

# Recover middleware — converts handler panics to 500 responses.
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

# Request ID middleware — injects/propagates a request identifier.
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

# Timeout middleware — emits 504 if no bytes were written by the deadline.
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

# CORS middleware — adds Access-Control-* headers and handles OPTIONS.
# opts: {origin: Str?, methods: [Str]?, headers: [Str]?, credentials: Bool?}
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

# Access log middleware — logs after response with status/bytes/duration.
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
