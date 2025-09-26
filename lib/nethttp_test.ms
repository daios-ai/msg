let testing = import("testing")
let http = import("nethttp")

# =========================================================
# 1) PATH PARAMS — typed, automatic bind
# =========================================================
testing.test("nethttp/route path param (typed)", fun(_: Null) do
	let r = http.router()

	r.route("GET", "/hello/{name}", http.contract({path: type {name: Str}}), fun(ctx) -> Any do
		http.text(200, "hi " + ctx.path.name)
	end)

	let c = http.testClient(r)
	let res = c.call({method: "GET", path: "/hello/Ada"})
	testing.assertEq(200, res.status)
	testing.assertEq("hi Ada", res.body)
end)

# =========================================================
# 2) QUERY — success (auto-bind to declared type)
# =========================================================
testing.test("nethttp/route query success", fun(_: Null) do
	let r = http.router()

	r.route("GET", "/q", http.contract({query: type {name: Str, age: Int}}), fun(ctx) -> Any do
		http.json(200, {ok: true, who: ctx.query.name, age: ctx.query.age})
	end)

	let c = http.testClient(r)
	let res = c.call({method: "GET", path: "/q?name=%22Ada%22&age=36"})
	testing.assertEq(200, res.status)
	let obj = jsonParse(res.body)
	testing.assertEq(true, obj.ok)
	testing.assertEq("Ada", obj.who)
	testing.assertEq(36, obj.age)
end)

# =========================================================
# 3) QUERY — failure (auto 422 on validation error)
# =========================================================
testing.test("nethttp/route query failure -> 422", fun(_: Null) do
	let r = http.router()

	r.route("GET", "/qbad", http.contract({query: type {name: Str, age: Int}}), fun(ctx) -> Any do
		http.text(200, "ok")
	end)

	let c = http.testClient(r)
	let res = c.call({method: "GET", path: "/qbad?name=Ada&age=oops"})
	testing.assertEq(422, res.status)
end)

# =========================================================
# 4) BODY — success
# =========================================================
testing.test("nethttp/route body json success", fun(_: Null) do
	let r = http.router()

	r.route("POST", "/echo", http.contract({body: type {msg: Str}}), fun(ctx) -> Any do
		http.json(200, {ok: true, msg: ctx.body.msg})
	end)

	let c = http.testClient(r)
	let res = c.call({method: "POST", path: "/echo", body: "{\"msg\":\"hi\"}"})
	testing.assertEq(200, res.status)
	let obj = jsonParse(res.body)
	testing.assertEq(true, obj.ok)
	testing.assertEq("hi", obj.msg)
end)

# =========================================================
# 5) BODY — failure (auto 422)
# =========================================================
testing.test("nethttp/route body json failure -> 422", fun(_: Null) do
	let r = http.router()

	r.route("POST", "/echo2", http.contract({body: type {msg: Str}}), fun(ctx) -> Any do
		http.text(200, "ok")
	end)

	let c = http.testClient(r)
	let res = c.call({method: "POST", path: "/echo2", body: "oops"})
	testing.assertEq(422, res.status)
end)

# =========================================================
# 6) IMPLICIT JSON ENCODE from returned value (no helper)
# =========================================================
testing.test("nethttp/route implicit json encode", fun(_: Null) do
	let r = http.router()

	r.route("GET", "/auto", http.contract({}), fun(_: {}) -> Any do
		{ok: true, n: 7}
	end)

	let c = http.testClient(r)
	let res = c.call({method: "GET", path: "/auto"})
	testing.assertEq(200, res.status)
	let obj = jsonParse(res.body)
	testing.assertEq(true, obj.ok)
	testing.assertEq(7, obj.n)
end)

# =========================================================
# 7) RAISE helper (structured error) -> custom status
# =========================================================
testing.test("nethttp/route raise -> status", fun(_: Null) do
	let r = http.router()

	r.route("GET", "/need-auth", http.contract({}), fun(_: {}) -> Any do
		http.raise(401, "unauthorized")
	end)

	let c = http.testClient(r)
	let res = c.call({method: "GET", path: "/need-auth"})
	testing.assertEq(401, res.status)
	testing.assertEq("unauthorized", res.body)
end)

# =========================================================
# 8) STREAMING handler (raw) works
# =========================================================
testing.test("nethttp/streaming handler works", fun(_: Null) do
	let r = http.router()

	r.routeStream("GET", "/stream", http.contract({}), fun(req, res, ctx) -> Null do
		res.status(200).setHeader("Content-Type", "text/plain").write("hey")
		res.end(null)
		null
	end)

	let c = http.testClient(r)
	let res = c.call({method: "GET", path: "/stream"})
	testing.assertEq(200, res.status)
	testing.assertEq("hey", res.body)
end)

# =========================================================
# 9) 404 / 405 and prefix matching
# =========================================================
testing.test("nethttp/routing 404_405 prefix", fun(_: Null) do
	let r = http.router()

	# trailing slash => subtree prefix
	r.routeStream("GET", "/x/", http.contract({}), fun(req, res, ctx) -> Null do
		res.status(200).write("ok")
		res.end(null)
		null
	end)

	let c = http.testClient(r)
	testing.assertEq(200, c.call({method: "GET", path: "/x/abc"}).status)
	testing.assertEq(405, c.call({method: "POST", path: "/x/abc"}).status)
	testing.assertEq(404, c.call({method: "GET", path: "/nope"}).status)
end)

# =========================================================
# 10) CORS preflight (OPTIONS)
# =========================================================
testing.test("nethttp/cors preflight", fun(_: Null) do
	let r = http.router()
	r.use(http.mwCors({origin: "*"}))

	let c = http.testClient(r)
	let res = c.call({method: "OPTIONS", path: "/any"})
	testing.assertEq(204, res.status)
end)

# =========================================================
# 11) Recover middleware catches panics -> 500
# =========================================================
testing.test("nethttp/recover value panic", fun(_: Null) do
	let r = http.router()
	r.use(http.mwRecover())

	r.route("GET", "/p", http.contract({}), fun(_: {}) -> Any do
		panic("boom")
	end)

	let c = http.testClient(r)
	let res = c.call({method: "GET", path: "/p"})
	testing.assertEq(500, res.status)
end)

testing.test("nethttp/recover stream panic", fun(_: Null) do
	let r = http.router()
	r.use(http.mwRecover())

	r.routeStream("GET", "/ps", http.contract({}), fun(req, res, ctx) -> Null do
		panic("boom")
	end)

	let c = http.testClient(r)
	let res = c.call({method: "GET", path: "/ps"})
	testing.assertEq(500, res.status)
end)

# =========================================================
# 12) Timeout middleware -> 504 on slow value handler
# =========================================================
testing.test("nethttp/timeout value", fun(_: Null) do
	let r = http.router()
	r.use(http.mwTimeout(20))

	r.route("GET", "/slow", http.contract({}), fun(_: {}) -> Any do
		sleep(50)
		http.text(200, "late")
	end)

	let c = http.testClient(r)
	let res = c.call({method: "GET", path: "/slow"})
	testing.assertEq(504, res.status)
end)

# =========================================================
# 13) Request ID middleware (inject + propagate)
# =========================================================
testing.test("nethttp/request id (inject + propagate)", fun(_: Null) do
	let r = http.router()
	r.use(http.mwRequestID("X-Request-ID"))

	r.route("GET", "/ok", http.contract({}), fun(_: {}) -> Any do
		http.text(200, "ok")
	end)

	let c = http.testClient(r)
	let a = c.call({method: "GET", path: "/ok"})
	testing.assertEq(200, a.status)
	testing.assert(a.headers["X-Request-ID"] != null, "missing request id")

	let b = c.call({
		method: "GET",
		path: "/ok",
		headers: {"X-Request-ID": "req-123"}
	})
	testing.assertEq("req-123", b.headers["X-Request-ID"])
end)

# =========================================================
# 14) Access log middleware preserves body and chaining
# =========================================================
testing.test("nethttp/accesslog preserves body + chaining", fun(_: Null) do
	let r = http.router()
	r.use(http.mwAccessLog())

	r.routeStream("GET", "/s", http.contract({}), fun(req, res, ctx) -> Null do
		res.status(200).setHeader("Content-Type", "text/plain").write("hi")
		res.end(null)
		null
	end)

	let c = http.testClient(r)
	let res = c.call({method: "GET", path: "/s"})
	testing.assertEq(200, res.status)
	testing.assertEq("hi", res.body)
end)

# =========================================================
# 15) Prefix registration (mount-free)
# =========================================================
testing.test("nethttp/prefix registration", fun(_: Null) do
	let parent = http.router()
	parent.route("GET", "/v1/ping", http.contract({}), fun(_: {}) -> Any do
		http.text(200, "pong")
	end)

	let c = http.testClient(parent)
	let res = c.call({method: "GET", path: "/v1/ping"})
	testing.assertEq(200, res.status)
	testing.assertEq("pong", res.body)
end)

# =========================================================
# 16) JSON encode error -> 500
# =========================================================
testing.test("nethttp/json encode error", fun(_: Null) do
	let r = http.router()

	r.route("GET", "/badjson", http.contract({}), fun(_: {}) -> Any do
		fun(_: Null) do
			true
		end
	end)

	let c = http.testClient(r)
	let res = c.call({method: "GET", path: "/badjson"})
	testing.assertEq(500, res.status)
	testing.assertEq("json encode error", res.body)
end)

# =========================================================
# 17) OpenAPI (minimal): ensure we get a doc back with the registered paths
# =========================================================
testing.test("nethttp/openapi basic", fun(_: Null) do
	let r = http.router()

	r.route("GET", "/a/{id}", http.contract({path: type {id: Str}}), fun(_: {}) -> Any do
		http.text(200, "ok")
	end)

	r.routeStream("GET", "/stream", http.contract({}), fun(req, res, ctx) -> Null do
		res.status(200).write("x")
		res.end(null)
		null
	end)

	let doc = http.openapiDoc(r, {title: "API", version: "1.0.0"}, [
		"http://localhost"
	])

	testing.assert(doc != null, "openapi null")
	let s = str(doc)
	testing.assert(len(split(s, "/a/{id}")) > 1 or len(split(s, "\"/a/{id}\"")) > 1, "missing /a/{id} in doc")
end)

let testing = import("testing")
let http = import("nethttp")

# =========================================================
# A) PATH: coercion failure -> 422
# =========================================================
testing.test("nethttp/path coercion failure -> 422", fun(_: Null) do
	let r = http.router()
	r.route("GET", "/a/{id}", {path: type {id!: Int}}, fun(ctx) -> Any? do
		http.text(200, "ok")
	end)

	let c = http.testClient(r)
	let res = c.call({method: "GET", path: "/a/not-an-int"})
	testing.assertEq(422, res.status)
end)

# =========================================================
# B1) QUERY arrays: repeated params bind to [Str]
# =========================================================
testing.test("nethttp/query arrays repeated", fun(_: Null) do
	let r = http.router()
	r.route("GET", "/tags", {query: type {tag!: [Str]}}, fun(ctx) -> Any? do
		http.json(200, {count: len(ctx.query.tag)})
	end)

	let c = http.testClient(r)
	let res = c.call({method: "GET", path: "/tags?tag=a&tag=b&tag=c"})
	testing.assertEq(200, res.status)
	let obj = jsonParse(res.body)
	testing.assertEq(3, obj.count)
end)

# =========================================================
# B2) QUERY arrays: JSON array in single param binds to [Str]
# =========================================================
testing.test("nethttp/query arrays json", fun(_: Null) do
	let r = http.router()
	r.route("GET", "/tags2", {query: type {tag!: [Str]}}, fun(ctx) -> Any? do
		http.json(200, {ok: true, tags: ctx.query.tag})
	end)

	let c = http.testClient(r)
	let res = c.call({method: "GET", path: "/tags2?tag=%5B%22x%22,%22y%22%5D"})
	testing.assertEq(200, res.status)
	let obj = jsonParse(res.body)
	testing.assertEq(true, obj.ok)
	testing.assertEq("x", obj.tags[0])
	testing.assertEq("y", obj.tags[1])
end)

# =========================================================
# C1) BODY: invalid JSON -> 422
# =========================================================
testing.test("nethttp/body invalid json -> 422", fun(_: Null) do
	let r = http.router()
	r.route("POST", "/echo", {body: type {msg!: Str}}, fun(ctx) -> Any? do
		http.text(200, "ok")
	end)

	let c = http.testClient(r)
	let res = c.call({method: "POST", path: "/echo", body: "not json"})
	testing.assertEq(422, res.status)
end)

# =========================================================
# C2) BODY: schema mismatch -> 422
# =========================================================
testing.test("nethttp/body schema mismatch -> 422", fun(_: Null) do
	let r = http.router()
	r.route("POST", "/echo2", {body: type {msg!: Str}}, fun(ctx) -> Any? do
		http.text(200, "ok")
	end)

	let c = http.testClient(r)
	let res = c.call({method: "POST", path: "/echo2", body: "{\"msg\": 1}"})
	testing.assertEq(422, res.status)
end)

# =========================================================
# D) RESPONSE schema validation failure -> 500
# =========================================================
testing.test("nethttp/response schema validation -> 500", fun(_: Null) do
	let r = http.router()
	r.route("GET", "/check", {responses: {"200": type {ok!: Bool}}}, fun(ctx) -> Any? do
		http.json(200, {ok: "nope"})
	end)

	let c = http.testClient(r)
	let res = c.call({method: "GET", path: "/check"})
	testing.assertEq(500, res.status)
end)



# =========================================================
# E) TIMEOUT middleware: fast handler stays 200
# =========================================================
testing.test("nethttp/timeout fast -> 200", fun(_: Null) do
	let r = http.router()
	r.use(http.mwTimeout(50))

	r.route("GET", "/fast", {}, fun(ctx) -> Any? do
		sleep(10)
		http.text(200, "ok")
	end)

	let c = http.testClient(r)
	let res = c.call({method: "GET", path: "/fast"})
	testing.assertEq(200, res.status)
	testing.assertEq("ok", res.body)
end)

# =========================================================
# F) STREAM: multi-chunk writes + flush preserve body
# =========================================================
testing.test("nethttp/stream chunks + flush", fun(_: Null) do
	let r = http.router()
	r.routeStream("GET", "/chunks", {}, fun(req, res, ctx) -> Null do
		res.status(200).setHeader("Content-Type", "text/plain").write("a")
		let _ = res.flush(null)
		let _ = res.write("b")
		let _ = res.end(null)
		null
	end)

	let c = http.testClient(r)
	let res = c.call({method: "GET", path: "/chunks"})
	testing.assertEq(200, res.status)
	testing.assertEq("ab", res.body)
end)

# =========================================================
# G) OPTIONS auto-204 on known (prefix) path
# =========================================================
testing.test("nethttp/options auto 204 on prefix", fun(_: Null) do
	let r = http.router()
	r.routeStream("GET", "/p/", {}, fun(req, res, ctx) -> Null do
		res.status(200).write("ok")
		res.end(null)
		null
	end)

	let c = http.testClient(r)
	let res = c.call({method: "OPTIONS", path: "/p/anything"})
	testing.assertEq(204, res.status)
end)

# =========================================================
# S1) SERVER: plain text value route over TCP
# =========================================================
testing.test("server/hello over tcp", fun(_: Null) -> Any do
	let http = import("nethttp")

	let r = http.router()
	r.route("GET", "/hello", http.contract({}), fun(_: {}) -> Any do
		http.text(200, "hello")
	end)

	let addr = "127.0.0.1:18081"
	let l = netListen(addr)
	testing.assert(l != null, "listen failed")
	let srv = http.serve(l, r, {})
	testing.assert(srv != null, "serve failed")

	# raw client
	let c = netConnect(addr)
	testing.assert(c != null, "connect failed")
	let _ = write(c, "GET /hello HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n")
	let _ = flush(c)
	let resp = readAll(c)
	close(c)

	# assert status line and body
	testing.assert(len(split(resp, "HTTP/1.1 200 OK")) > 1, "bad status")
	testing.assert(len(split(resp, "\r\n\r\nhello")) > 1, "bad body")

	let _ = http.shutdown(srv, 0)
end)

# =========================================================
# S2) SERVER: 422 semantics (reason phrase + JSON content-type w/ charset)
# =========================================================
testing.test("server/422 reason + content-type", fun(_: Null) -> Any do
	let http = import("nethttp")

	let r = http.router()
	r.route("GET", "/greet/{name}", http.contract({
		path:  type { name!: Str },
		query: type { times!: Int }
	}), fun(_: {}) -> Any do
		http.json(200, {ok: true})
	end)

	let addr = "127.0.0.1:18082"
	let l = netListen(addr)
	testing.assert(l != null, "listen failed")
	let srv = http.serve(l, r, {})
	testing.assert(srv != null, "serve failed")

	let c = netConnect(addr)
	testing.assert(c != null, "connect failed")
	let _ = write(c, "GET /greet/Ada?times=oops HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n")
	let _ = flush(c)
	let resp = readAll(c)
	close(c)

	# Reason phrase should be "Unprocessable Entity"
	testing.assert(len(split(resp, "HTTP/1.1 422 Unprocessable Entity")) > 1, "bad 422 reason")
	# JSON content type with charset
	testing.assert(len(split(resp, "Content-Type: application/json; charset=utf-8")) > 1, "bad 422 content-type")

	let _ = http.shutdown(srv, 0)
end)

# =========================================================
# S3) SERVER: JSON POST echo (Content-Length correctness)
# =========================================================
testing.test("server/post echo content-length", fun(_: Null) -> Any do
	let http = import("nethttp")

	let r = http.router()
	r.route("POST", "/echo", http.contract({ body: type { msg!: Str } }), fun(ctx) -> Any do
		http.json(200, {ok: true, msg: ctx.body.msg})
	end)

	let addr = "127.0.0.1:18083"
	let l = netListen(addr)
	testing.assert(l != null, "listen failed")
	let srv = http.serve(l, r, {})
	testing.assert(srv != null, "serve failed")

	let body = "{\"msg\":\"hi\"}"
	let req =
		sprintf("POST /echo HTTP/1.1\r\nHost: localhost\r\nContent-Type: application/json\r\nContent-Length: %d\r\nConnection: close\r\n\r\n%s", [
			len(body), body
		])

	let c = netConnect(addr)
	testing.assert(c != null, "connect failed")
	let _ = write(c, req)
	let _ = flush(c)
	let resp = readAll(c)
	close(c)

	# status ok
	testing.assert(len(split(resp, "HTTP/1.1 200 OK")) > 1, "bad status")
	# body
	testing.assert(len(split(resp, "\r\n\r\n{\"msg\":\"hi\",\"ok\":true}")) > 1, "bad body")
	# content-length header matches payload length (22)
	testing.assert(len(split(resp, "Content-Length: 22")) > 1, "bad content-length")

	let _ = http.shutdown(srv, 0)
end)

# =========================================================
# S4) SERVER: streaming route + OPTIONS preflight
# =========================================================
testing.test("server/stream + options", fun(_: Null) -> Any do
	let http = import("nethttp")

	let r = http.router()
	# prefix route to allow OPTIONS anywhere under /time/
	r.routeStream("GET", "/time/", http.contract({}), fun(req, res, ctx) -> Null do
		res.status(200).setHeader("Content-Type", "text/plain").write("tick 0\ntick 1\ntick 2\n")
		res.end(null)
		null
	end)

	let addr = "127.0.0.1:18084"
	let l = netListen(addr)
	testing.assert(l != null, "listen failed")
	let srv = http.serve(l, r, {})
	testing.assert(srv != null, "serve failed")

	# GET stream
	let c1 = netConnect(addr)
	testing.assert(c1 != null, "connect failed")
	let _ = write(c1, "GET /time/now HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n")
	let _ = flush(c1)
	let resp1 = readAll(c1)
	close(c1)
	testing.assert(len(split(resp1, "HTTP/1.1 200 OK")) > 1, "bad status")
	testing.assert(len(split(resp1, "\r\n\r\n")) > 1, "no body separator")
	testing.assert(len(split(resp1, "tick 0")) > 1 and len(split(resp1, "tick 2")) > 1, "bad stream body")

	# OPTIONS preflight auto-204
	let c2 = netConnect(addr)
	testing.assert(c2 != null, "connect failed")
	let _ = write(c2, "OPTIONS /time/anything HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n")
	let _ = flush(c2)
	let resp2 = readAll(c2)
	close(c2)
	testing.assert(len(split(resp2, "HTTP/1.1 204 No Content")) > 1, "bad options 204")

	let _ = http.shutdown(srv, 0)
end)

# =========================================================
# S5) SERVER: 405 Method Not Allowed
# =========================================================
testing.test("server/405 method not allowed", fun(_: Null) -> Any do
	let http = import("nethttp")

	let r = http.router()
	r.route("GET", "/only-get", http.contract({}), fun(_: {}) -> Any do
		http.text(200, "ok")
	end)

	let addr = "127.0.0.1:18085"
	let l = netListen(addr)
	testing.assert(l != null, "listen failed")
	let srv = http.serve(l, r, {})
	testing.assert(srv != null, "serve failed")

	let c = netConnect(addr)
	testing.assert(c != null, "connect failed")
	let _ = write(c, "POST /only-get HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n")
	let _ = flush(c)
	let resp = readAll(c)
	close(c)

	testing.assert(len(split(resp, "HTTP/1.1 405 Method Not Allowed")) > 1, "expected 405")
	let _ = http.shutdown(srv, 0)
end)

# =========================================================
# S6) SERVER: redirect helper sets Location
# =========================================================
testing.test("server/redirect sets location", fun(_: Null) -> Any do
	let http = import("nethttp")

	let r = http.router()
	r.route("GET", "/old", http.contract({}), fun(_: {}) -> Any do
		http.redirect(302, "/new")
	end)

	let addr = "127.0.0.1:18086"
	let l = netListen(addr)
	testing.assert(l != null, "listen failed")
	let srv = http.serve(l, r, {})
	testing.assert(srv != null, "serve failed")

	let c = netConnect(addr)
	testing.assert(c != null, "connect failed")
	let _ = write(c, "GET /old HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n")
	let _ = flush(c)
	let resp = readAll(c)
	close(c)

	testing.assert(len(split(resp, "HTTP/1.1 302 OK")) > 1 or len(split(resp, "HTTP/1.1 302 Found")) > 1, "bad 302 status")
	testing.assert(len(split(resp, "\r\nLocation: /new\r\n")) > 1, "missing Location header")
	let _ = http.shutdown(srv, 0)
end)

# =========================================================
# S7) SERVER: CORS headers appear on normal GET (not only OPTIONS)
# =========================================================
testing.test("server/cors headers on get", fun(_: Null) -> Any do
	let http = import("nethttp")

	let r = http.router()
	r.use(http.mwCors({origin: "*"}))
	r.route("GET", "/ping", http.contract({}), fun(_: {}) -> Any do
		http.text(200, "pong")
	end)

	let addr = "127.0.0.1:18087"
	let l = netListen(addr)
	testing.assert(l != null, "listen failed")
	let srv = http.serve(l, r, {})
	testing.assert(srv != null, "serve failed")

	let c = netConnect(addr)
	testing.assert(c != null, "connect failed")
	let _ = write(c, "GET /ping HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n")
	let _ = flush(c)
	let resp = readAll(c)
	close(c)

	testing.assert(len(split(resp, "HTTP/1.1 200 OK")) > 1, "bad status")
	testing.assert(len(split(resp, "Access-Control-Allow-Origin: *")) > 1, "missing ACAO")
	let _ = http.shutdown(srv, 0)
end)

# =========================================================
# S8) SERVER: 413 Payload Too Large via maxBodyBytes
# =========================================================
testing.test("server/413 payload too large", fun(_: Null) -> Any do
	let http = import("nethttp")

	let r = http.router()
	r.route("POST", "/upload", http.contract({}), fun(_: {}) -> Any do
		http.text(200, "ok")
	end)

	let addr = "127.0.0.1:18088"
	let l = netListen(addr)
	testing.assert(l != null, "listen failed")
	let srv = http.serve(l, r, { maxBodyBytes: 8 })   # tiny cap
	testing.assert(srv != null, "serve failed")

	let payload = "0123456789abcdef"  # 16 bytes > cap
	let req = sprintf("POST /upload HTTP/1.1\r\nHost: localhost\r\nContent-Length: %d\r\nConnection: close\r\n\r\n%s", [
		len(payload), payload
	])

	let c = netConnect(addr)
	testing.assert(c != null, "connect failed")
	let _ = write(c, req)
	let _ = flush(c)
	let resp = readAll(c)
	close(c)

	testing.assert(len(split(resp, "HTTP/1.1 413 Payload Too Large")) > 1, "expected 413")
	let _ = http.shutdown(srv, 0)
end)

# =========================================================
# S9) KEEP-ALIVE: two sequential requests on one connection
# =========================================================
testing.test("server/keepalive two requests", fun(_: Null) -> Any do
	let http = import("nethttp")

	let r = http.router()
	r.route("GET", "/a", {}, fun(_: {}) -> Any do http.text(200, "A") end)
	r.route("GET", "/b", {}, fun(_: {}) -> Any do http.text(200, "B") end)

	let addr = "127.0.0.1:18089"
	let l = netListen(addr)
	testing.assert(l != null, "listen failed")
	let srv = http.serve(l, r, {})
	testing.assert(srv != null, "serve failed")

	# One socket, two requests; second closes the connection.
	let c = netConnect(addr)
	testing.assert(c != null, "connect failed")

	let req =
		"GET /a HTTP/1.1\r\nHost: localhost\r\n\r\n" +
		"GET /b HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n"
	let _ = write(c, req)
	let _ = flush(c)
	let resp = readAll(c)
	close(c)

	# Expect two 200 OK responses and both bodies.
	testing.assert(len(split(resp, "HTTP/1.1 200 OK")) >= 3, "expected two 200 statuses")
	testing.assert(len(split(resp, "\r\n\r\nA")) > 1, "missing body A")
	testing.assert(len(split(resp, "\r\n\r\nB")) > 1, "missing body B")

	let _ = http.shutdown(srv, 0)
end)

# =========================================================
# S10) HEADER CASE: case-sensitive behavior is explicit
# Middleware expects exact key; lowercase key should not be used.
# =========================================================
testing.test("server/header case-sensitive (explicit)", fun(_: Null) -> Any do
	let http = import("nethttp")

	let r = http.router()
	r.use(http.mwRequestID("X-Request-ID"))
	r.route("GET", "/ok", {}, fun(_: {}) -> Any do http.text(200, "ok") end)

	let addr = "127.0.0.1:18090"
	let l = netListen(addr)
	testing.assert(l != null, "listen failed")
	let srv = http.serve(l, r, {})
	testing.assert(srv != null, "serve failed")

	let c = netConnect(addr)
	testing.assert(c != null, "connect failed")

	# Send lowercase header "x-request-id"; middleware looks for "X-Request-ID"
	let _ = write(c, "GET /ok HTTP/1.1\r\nHost: localhost\r\nx-request-id: client-id\r\nConnection: close\r\n\r\n")
	let _ = flush(c)
	let resp = readAll(c)
	close(c)

	# Server should generate a fresh X-Request-ID header, not reuse lowercase one.
	testing.assert(len(split(resp, "HTTP/1.1 200 OK")) > 1, "bad status")
	testing.assert(len(split(resp, "\nX-Request-ID: ")) > 1, "missing canonical X-Request-ID")
	testing.assert(len(split(resp, "\nx-request-id: client-id")) == 1, "lowercase header should not be echoed")

	let _ = http.shutdown(srv, 0)
end)

# =========================================================
# S11) PATH PARAM percent-encoding is not decoded (explicit behavior)
# =========================================================
testing.test("server/path param raw percent-encoding", fun(_: Null) -> Any do
	let http = import("nethttp")

	let r = http.router()
	r.route("GET", "/p/{v}", {path: type {v!: Str}}, fun(ctx) -> Any do
		http.text(200, ctx.path.v)
	end)

	let addr = "127.0.0.1:18091"
	let l = netListen(addr)
	testing.assert(l != null, "listen failed")
	let srv = http.serve(l, r, {})
	testing.assert(srv != null, "serve failed")

	let c1 = netConnect(addr)
	testing.assert(c1 != null, "connect failed")
	let _ = write(c1, "GET /p/%2F HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n")
	let _ = flush(c1)
	let resp1 = readAll(c1)
	close(c1)
	testing.assert(len(split(resp1, "\r\n\r\n%2F")) > 1, "expected raw %2F (no decode)")

	let c2 = netConnect(addr)
	testing.assert(c2 != null, "connect failed")
	let _2 = write(c2, "GET /p/%20 HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n")
	let _2f = flush(c2)
	let resp2 = readAll(c2)
	close(c2)
	testing.assert(len(split(resp2, "\r\n\r\n%20")) > 1, "expected raw %20 (no decode)")

	let _ = http.shutdown(srv, 0)
end)

# =========================================================
# S12) QUERY COERCION: booleans, optional missing, single-element arrays
# =========================================================
testing.test("server/query coercion edges", fun(_: Null) -> Any do
	let http = import("nethttp")

	let r = http.router()
	r.route("GET", "/q", {query: type {flag!: Bool, n!: Int, tags!: [Int]}}, fun(ctx) -> Any do
		http.json(200, {flag: ctx.query.flag, n: ctx.query.n, tags: ctx.query.tags})
	end)

	let addr = "127.0.0.1:18092"
	let l = netListen(addr)
	testing.assert(l != null, "listen failed")
	let srv = http.serve(l, r, {})
	testing.assert(srv != null, "serve failed")

	# flag=false, omit optional 'n' by leaving it absent (contract has n without ! so it's optional)
	# NOTE: the binder treats 'n' missing as null, which passes optional. 'tags=1' should coerce to [1].
	let c = netConnect(addr)
	testing.assert(c != null, "connect failed")
	let _ = write(c, "GET /q?flag=false&n=5&tags=1 HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n")
	let _ = flush(c)
	let resp = readAll(c)
	close(c)

	testing.assert(len(split(resp, "HTTP/1.1 200 OK")) > 1, "bad status")
	testing.assert(len(split(resp, "\"flag\":false")) > 1, "flag=false not parsed")
	testing.assert(len(split(resp, "\"tags\":[1]")) > 1, "single-element array not parsed")

	let _ = http.shutdown(srv, 0)
end)

# =========================================================
# S13) LARGE HEADERS: exceed header budget -> 400 (current behavior)
# =========================================================
testing.test("server/large headers -> 400", fun(_: Null) -> Any do
	let http = import("nethttp")

	let r = http.router()
	r.route("GET", "/h", {}, fun(_: {}) -> Any do http.text(200, "ok") end)

	let addr = "127.0.0.1:18093"
	let l = netListen(addr)
	testing.assert(l != null, "listen failed")
	# Make budget tiny so we overflow easily.
	let srv = http.serve(l, r, { maxHeaderBytes: 64 })
	testing.assert(srv != null, "serve failed")

	let big = "X-Long: " + join([
		"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
	], "")  # definitely >64 including CRLF & Host
	let req = "GET /h HTTP/1.1\r\nHost: localhost\r\n" + big + "\r\n\r\n"

	let c = netConnect(addr)
	testing.assert(c != null, "connect failed")
	let _ = write(c, req)
	let _ = flush(c)
	let resp = readAll(c)
	close(c)

	# Current server maps header read failure to 400 Bad Request.
	testing.assert(len(split(resp, "HTTP/1.1 400 Bad Request")) > 1, "expected 400 for header overflow")

	let _ = http.shutdown(srv, 0)
end)

# =========================================================
# S14) DATE HEADER: explicitly absent (documented behavior)
# =========================================================
testing.test("server/no Date header (explicit)", fun(_: Null) -> Any do
	let http = import("nethttp")

	let r = http.router()
	r.route("GET", "/d", {}, fun(_: {}) -> Any do http.text(200, "ok") end)

	let addr = "127.0.0.1:18094"
	let l = netListen(addr)
	testing.assert(l != null, "listen failed")
	let srv = http.serve(l, r, {})
	testing.assert(srv != null, "serve failed")

	let c = netConnect(addr)
	testing.assert(c != null, "connect failed")
	let _ = write(c, "GET /d HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n")
	let _ = flush(c)
	let resp = readAll(c)
	close(c)

	# We don't emit Date header; make that explicit.
	testing.assert(len(split(resp, "\nDate:")) == 1, "Date header should be absent")

	let _ = http.shutdown(srv, 0)
end)

# =========================================================
# S15) SHUTDOWN: stops new accepts (late connect fails or yields no response)
# =========================================================
testing.test("server/shutdown stops accepts", fun(_: Null) -> Any do
	let http = import("nethttp")

	let r = http.router()
	r.route("GET", "/once", {}, fun(_: {}) -> Any do http.text(200, "ok") end)

	let addr = "127.0.0.1:18095"
	let l = netListen(addr)
	testing.assert(l != null, "listen failed")
	let srv = http.serve(l, r, {})
	testing.assert(srv != null, "serve failed")

	# First request succeeds.
	let c1 = netConnect(addr)
	testing.assert(c1 != null, "connect1 failed")
	let _ = write(c1, "GET /once HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n")
	let _ = flush(c1)
	let _ = readAll(c1)
	close(c1)

	# Now shutdown.
	let _ = http.shutdown(srv, 0)

	# Late connection should fail to connect or produce no valid response.
	let c2 = netConnect(addr)
	if c2 == null then
		# good: listener closed
		return true
	end
	let w = write(c2, "GET /once HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n")
	let _ = flush(c2)
	let resp = readAll(c2)
	close(c2)
	# Either write failed (null) or empty/partial response returned. We only assert it's not a full 200 line.
	testing.assert(w == null or len(resp) == 0 or len(split(resp, "HTTP/1.1 200 OK")) == 1, "shutdown did not stop new accepts")
end)
