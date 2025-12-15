let testing = import("testing")
let http = import("nethttp")

# =========================================================
# 1) PATH PARAMS — typed, automatic bind
# =========================================================
testing.test("nethttp/route path param (typed)", fun(_: Null) do
	let r = http.router()

	r.route("GET", "/hello/{name}", http.contract({path: type {name: Str}}), fun(req, ctx) do
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

	r.route("GET", "/q", http.contract({query: type {age: Int, name: Str}}), fun(req, ctx) do
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

	r.route("GET", "/qbad", http.contract({query: type {age: Int, name: Str}}), fun(req, ctx) do
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

	r.route("POST", "/echo", http.contract({body: type {msg: Str}}), fun(req, ctx) do
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

	r.route("POST", "/echo2", http.contract({body: type {msg: Str}}), fun(req, ctx) do
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

	r.route("GET", "/auto", http.contract({}), fun(req, ctx) do
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

	r.route("GET", "/need-auth", http.contract({}), fun(req, ctx) do
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

	let child = http.router()
	child.routeStream("GET", "/{tail}", http.contract({}), fun(req, res, ctx) -> Null do
		res.status(200).write("ok")
		res.end(null)
		null
	end)

	r.mount("/x", child)

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

	r.route("GET", "/p", http.contract({}), fun(req, ctx) do
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

	r.route("GET", "/slow", http.contract({}), fun(req, ctx) do
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

	r.route("GET", "/ok", http.contract({}), fun(req, ctx) do
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
	parent.route("GET", "/v1/ping", http.contract({}), fun(req, ctx) do
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

	r.route("GET", "/badjson", http.contract({}), fun(req, ctx) do
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

	r.route("GET", "/a/{id}", http.contract({path: type {id: Str}}), fun(req, ctx) do
		http.text(200, "ok")
	end)

	r.routeStream("GET", "/stream", http.contract({}), fun(req, res, ctx) -> Null do
		res.status(200).write("x")
		res.end(null)
		null
	end)

	# auto-generated docs must be exposed at /openapi.json
	let c = http.testClient(r)
	let res = c.call({method: "GET", path: "/openapi.json"})
	testing.assert(res.status == 200, "openapi status != 200")
	testing.assert(len(res.body) > 0, "openapi body empty")
	testing.assert(len(split(res.body, "/a/{id}")) > 1 or len(split(res.body, "\"/a/{id}\"")) > 1, "missing /a/{id} in openapi.json")
end)

# =========================================================
# A) PATH: coercion failure -> 422
# =========================================================
testing.test("nethttp/path coercion failure -> 422", fun(_: Null) do
	let r = http.router()
	r.route("GET", "/a/{id}", {path: type {id: Int}}, fun(req, ctx) -> Any? do
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
	r.route("GET", "/tags", {query: type {tag: [Str]}}, fun(req, ctx) -> Any? do
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
	r.route("GET", "/tags2", {query: type {tag: [Str]}}, fun(req, ctx) -> Any? do
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
	r.route("POST", "/echo", {body: type {msg: Str}}, fun(req, ctx) -> Any? do
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
	r.route("POST", "/echo2", {body: type {msg: Str}}, fun(req, ctx) -> Any? do
		http.text(200, "ok")
	end)

	let c = http.testClient(r)
	let res = c.call({method: "POST", path: "/echo2", body: "{\"msg\": 1}"})
	testing.assertEq(422, res.status)
end)

# =========================================================
# D) RESPONSE schema validation failure -> 500
# (kept as-is; response checking still optional feature-flag)
# =========================================================
testing.test("nethttp/response schema validation -> 500", fun(_: Null) do
	let r = http.router()
	r.route("GET", "/check", {responses: {"200": type {ok: Bool}}}, fun(req, ctx) -> Any? do
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

	r.route("GET", "/fast", {}, fun(req, ctx) -> Any? do
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
testing.test("nethttp/hello over tcp", fun(_: Null) do
	let http = import("nethttp")

	let r = http.router()
	r.route("GET", "/hello", http.contract({}), fun(req, ctx) do
		http.text(200, "hello")
	end)

	let addr = "127.0.0.1:18081"
	let l = netListen(addr)
	testing.assert(l != null, "listen failed")
	let srv = http.serve(l, r, {})
	testing.assert(srv != null, "serve failed")

	let c = netConnect(addr) # raw client
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
testing.test("nethttp/422 reason + content-type", fun(_: Null) do
	let http = import("nethttp")

	let r = http.router()
	r.route("GET", "/greet/{name}", http.contract({
		path: type {name: Str},
		query: type {times: Int}
	}), fun(req, ctx) do
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
testing.test("nethttp/post echo content-length", fun(_: Null) do
	let http = import("nethttp")

	let r = http.router()
	r.route("POST", "/echo", http.contract({body: type {msg: Str}}), fun(req, ctx) do
		http.json(200, {ok: true, msg: ctx.body.msg})
	end)

	let addr = "127.0.0.1:18083"
	let l = netListen(addr)
	testing.assert(l != null, "listen failed")
	let srv = http.serve(l, r, {})
	testing.assert(srv != null, "serve failed")

	let body = "{\"msg\":\"hi\"}"
	let req = sprintf("POST /echo HTTP/1.1\r\nHost: localhost\r\nContent-Type: application/json\r\nContent-Length: %d\r\nConnection: close\r\n\r\n%s", [
		len(body),
		body
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
testing.test("nethttp/stream + options", fun(_: Null) do
	let http = import("nethttp")

	let root = http.router()
	let time = http.router()
	time.routeStream("GET", "/{tail}", http.contract({}), fun(req, res, ctx) -> Null do
		res.status(200).setHeader("Content-Type", "text/plain").write("tick 0\ntick 1\ntick 2\n")
		res.end(null)
		null
	end)
	root.mount("/time", time)

	let addr = "127.0.0.1:18084"
	let l = netListen(addr)
	let srv = http.serve(l, root, {})

	let c1 = netConnect(addr) # GET stream
	let _ = write(c1, "GET /time/now HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n")
	let _ = flush(c1)
	let resp1 = readAll(c1)
	close(c1)
	testing.assert(len(split(resp1, "HTTP/1.1 200 OK")) > 1, "bad status")
	testing.assert(len(split(resp1, "\r\n\r\n")) > 1, "no body separator")
	testing.assert(len(split(resp1, "tick 0")) > 1 and len(split(resp1, "tick 2")) > 1, "bad stream body")

	# OPTIONS preflight (still 204 with your current logic)
	let c2 = netConnect(addr)
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
testing.test("nethttp/405 method not allowed", fun(_: Null) do
	let http = import("nethttp")

	let r = http.router()
	r.route("GET", "/only-get", http.contract({}), fun(req, ctx) do
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
testing.test("nethttp/redirect sets location", fun(_: Null) do
	let http = import("nethttp")

	let r = http.router()
	r.route("GET", "/old", http.contract({}), fun(req, ctx) do
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
testing.test("nethttp/cors headers on get", fun(_: Null) do
	let http = import("nethttp")

	let r = http.router()
	r.use(http.mwCors({origin: "*"}))
	r.route("GET", "/ping", http.contract({}), fun(req, ctx) do
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
testing.test("nethttp/413 payload too large", fun(_: Null) do
	let http = import("nethttp")

	let r = http.router()
	r.route("POST", "/upload", http.contract({}), fun(req, ctx) do
		http.text(200, "ok")
	end)

	let addr = "127.0.0.1:18088"
	let l = netListen(addr)
	testing.assert(l != null, "listen failed")
	let srv = http.serve(l, r, {maxBodyBytes: 8})
	testing.assert(srv != null, "serve failed")

	let payload = "0123456789abcdef" # 16 bytes > cap

	let req = sprintf("POST /upload HTTP/1.1\r\nHost: localhost\r\nContent-Length: %d\r\nConnection: close\r\n\r\n%s", [
		len(payload),
		payload
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
testing.test("nethttp/keepalive two requests", fun(_: Null) do
	let http = import("nethttp")

	let r = http.router()
	r.route("GET", "/a", {}, fun(req, ctx) do
		http.text(200, "A")
	end)
	r.route("GET", "/b", {}, fun(req, ctx) do
		http.text(200, "B")
	end)

	let addr = "127.0.0.1:18089"
	let l = netListen(addr)
	testing.assert(l != null, "listen failed")
	let srv = http.serve(l, r, {})
	testing.assert(srv != null, "serve failed")

	# One socket, two requests; second closes the connection.
	let c = netConnect(addr)
	testing.assert(c != null, "connect failed")

	let req = "GET /a HTTP/1.1\r\nHost: localhost\r\n\r\n" + "GET /b HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n"
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
# S11) PATH PARAM percent-encoding is not decoded (explicit behavior)
# =========================================================
testing.test("nethttp/path param raw percent-encoding", fun(_: Null) do
	let http = import("nethttp")

	let r = http.router()
	r.route("GET", "/p/{v}", {path: type {v: Str}}, fun(req, ctx) do
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
testing.test("nethttp/query coercion edges", fun(_: Null) do
	let http = import("nethttp")

	let r = http.router()
	r.route("GET", "/q", {query: type {flag: Bool, n: Int, tags: [Int]}}, fun(req, ctx) do
		http.json(200, {
			flag: ctx.query.flag,
			n: ctx.query.n,
			tags: ctx.query.tags
		})
	end)

	let addr = "127.0.0.1:18092"
	let l = netListen(addr)
	testing.assert(l != null, "listen failed")
	let srv = http.serve(l, r, {})

	testing.assert(srv != null, "serve failed")

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
testing.test("nethttp/large headers -> 400", fun(_: Null) do
	let http = import("nethttp")

	let r = http.router()
	r.route("GET", "/h", {}, fun(req, ctx) do
		http.text(200, "ok")
	end)

	let addr = "127.0.0.1:18093"
	let l = netListen(addr)
	testing.assert(l != null, "listen failed")
	let srv = http.serve(l, r, {maxHeaderBytes: 64})
	testing.assert(srv != null, "serve failed")

	let big = "X-Long: " + join([
		"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
	], "")
	let req = "GET /h HTTP/1.1\r\nHost: localhost\r\n" + big + "\r\n\r\n"

	let c = netConnect(addr)
	testing.assert(c != null, "connect failed")
	let _ = write(c, req)
	let _ = flush(c)
	let resp = readAll(c)
	close(c)

	testing.assert(len(split(resp, "HTTP/1.1 400 Bad Request")) > 1, "expected 400 for header overflow")

	let _ = http.shutdown(srv, 0)
end)

# =========================================================
# S14) DATE HEADER: explicitly absent (documented behavior)
# =========================================================
testing.test("nethttp/no Date header (explicit)", fun(_: Null) do
	let http = import("nethttp")

	let r = http.router()
	r.route("GET", "/d", {}, fun(req, ctx) do
		http.text(200, "ok")
	end)

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

	testing.assert(len(split(resp, "\nDate:")) == 1, "Date header should be absent")

	let _ = http.shutdown(srv, 0)
end)

# =========================================================
# S15) SHUTDOWN: stops new accepts (late connect fails or yields no response)
# =========================================================
testing.test("nethttp/shutdown stops accepts", fun(_: Null) do
	let http = import("nethttp")

	let r = http.router()
	r.route("GET", "/once", {}, fun(req, ctx) do
		http.text(200, "ok")
	end)

	let addr = "127.0.0.1:18095"
	let l = netListen(addr)
	testing.assert(l != null, "listen failed")
	let srv = http.serve(l, r, {})
	testing.assert(srv != null, "serve failed")

	let c1 = netConnect(addr) # First request succeeds.
	testing.assert(c1 != null, "connect1 failed")
	let _ = write(c1, "GET /once HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n")
	let _ = flush(c1)
	let _ = readAll(c1)
	close(c1)

	let _ = http.shutdown(srv, 0) # Now shutdown.

	# Late connection should fail to connect or produce no valid response.
	let c2 = netConnect(addr)
	if c2 == null then
		return true
	end
	let w = write(c2, "GET /once HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n")
	let _ = flush(c2)
	let resp = readAll(c2)
	close(c2)
	testing.assert(w == null or len(resp) == 0 or len(split(resp, "HTTP/1.1 200 OK")) == 1, "shutdown did not stop new accepts")
end)

# =========================================================
# nethttp/headers case-insensitive (ingest + echo)
# Expect: lowercase request header is accepted; response echoes canonical key with same value.
# =========================================================
testing.test("nethttp/headers case-insensitive", fun(_: Null) do
	let r = http.router()
	r.use(http.mwRequestID("X-Request-ID"))
	r.route("GET", "/ok", {}, fun(req, ctx) do
		http.text(200, "ok")
	end)

	let addr = "127.0.0.1:18101"
	let l = netListen(addr)
	let srv = http.serve(l, r, {})
	let c = netConnect(addr)
	let _ = write(c, "GET /ok HTTP/1.1\r\nHost: localhost\r\nx-request-id: client-id\r\nConnection: close\r\n\r\n")
	let _ = flush(c)
	let resp = readAll(c)
	close(c)
	let _ = http.shutdown(srv, 0)

	testing.assert(len(split(resp, "HTTP/1.1 200 OK")) > 1, "bad status")
	testing.assert(len(split(resp, "\nX-Request-ID: client-id")) > 1, "missing echoed canonical request id")
end)

# =========================================================
# nethttp/options known vs unknown path
# Expect: 204 for known shape; 404 for unknown
# =========================================================
testing.test("nethttp/options known-vs-unknown", fun(_: Null) do
	let r = http.router()
	r.route("GET", "/users/{id}", {}, fun(req, ctx) do
		http.text(200, "ok")
	end)

	let addr = "127.0.0.1:18102"
	let l = netListen(addr)
	let srv = http.serve(l, r, {})

	let c1 = netConnect(addr) # known shape
	let _ = write(c1, "OPTIONS /users/42 HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n")
	let _ = flush(c1)
	let resp1 = readAll(c1)
	close(c1)
	testing.assert(len(split(resp1, "HTTP/1.1 204 No Content")) > 1, "expected 204 on known path")

	let c2 = netConnect(addr) # unknown
	let _2 = write(c2, "OPTIONS /nope HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n")
	let _2f = flush(c2)
	let resp2 = readAll(c2)
	close(c2)
	testing.assert(len(split(resp2, "HTTP/1.1 404 Not Found")) > 1, "expected 404 on unknown path")

	let _ = http.shutdown(srv, 0)
end)

# =========================================================
# nethttp/405 Allow header
# Expect: 405 with Allow listing permitted methods for the path shape
# =========================================================
testing.test("nethttp/405 with Allow", fun(_: Null) do
	let r = http.router()
	r.route("GET", "/only-get/{x}", {}, fun(req, ctx) do
		http.text(200, "ok")
	end)

	let addr = "127.0.0.1:18103"
	let l = netListen(addr)
	let srv = http.serve(l, r, {})

	let c = netConnect(addr)
	let _ = write(c, "POST /only-get/1 HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n")
	let _ = flush(c)
	let resp = readAll(c)
	close(c)
	let _ = http.shutdown(srv, 0)

	testing.assert(len(split(resp, "HTTP/1.1 405 Method Not Allowed")) > 1, "expected 405")
	testing.assert(len(split(resp, "\nAllow: GET")) > 1, "missing Allow header")
end)

# =========================================================
# nethttp/reason phrases polish (302 -> Found)
# =========================================================
testing.test("nethttp/reason phrase 302 Found", fun(_: Null) do
	let r = http.router()
	r.route("GET", "/old", {}, fun(req, ctx) do
		http.redirect(302, "/new")
	end)

	let addr = "127.0.0.1:18104"
	let l = netListen(addr)
	let srv = http.serve(l, r, {})

	let c = netConnect(addr)
	let _ = write(c, "GET /old HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n")
	let _ = flush(c)
	let resp = readAll(c)
	close(c)
	let _ = http.shutdown(srv, 0)

	testing.assert(len(split(resp, "HTTP/1.1 302 Found")) > 1, "expected 302 Found")
end)

# =========================================================
# nethttp/TE chunked rejected (unsupported)
# Expect: 400 on Transfer-Encoding: chunked request
# =========================================================
testing.test("nethttp/te chunked -> 400", fun(_: Null) do
	let r = http.router()
	r.route("POST", "/u", {}, fun(req, ctx) do
		http.text(200, "ok")
	end)

	let addr = "127.0.0.1:18105"
	let l = netListen(addr)
	let srv = http.serve(l, r, {})

	let c = netConnect(addr)
	let _ = write(c, "POST /u HTTP/1.1\r\nHost: localhost\r\nTransfer-Encoding: chunked\r\nConnection: close\r\n\r\n" + "4\r\nTest\r\n0\r\n\r\n")
	let _ = flush(c)
	let resp = readAll(c)
	close(c)
	let _ = http.shutdown(srv, 0)

	testing.assert(len(split(resp, "HTTP/1.1 400 Bad Request")) > 1, "expected 400 for TE: chunked")
end)

# =========================================================
# nethttp/duplicate Content-Length mismatch -> 400
# =========================================================
testing.test("nethttp/duplicate content-length mismatch", fun(_: Null) do
	let r = http.router()
	r.route("POST", "/echo", {}, fun(req, ctx) do
		http.text(200, "ok")
	end)

	let addr = "127.0.0.1:18106"
	let l = netListen(addr)
	let srv = http.serve(l, r, {})

	let c = netConnect(addr)
	let _ = write(c, "POST /echo HTTP/1.1\r\nHost: localhost\r\nContent-Length: 3\r\nContent-Length: 5\r\nConnection: close\r\n\r\nabc")
	let _ = flush(c)
	let resp = readAll(c)
	close(c)
	let _ = http.shutdown(srv, 0)

	testing.assert(len(split(resp, "HTTP/1.1 400 Bad Request")) > 1, "expected 400 on conflicting Content-Length")
end)

# =========================================================
# nethttp/absolute-form target -> 400
# =========================================================
testing.test("nethttp/absolute-form -> 400", fun(_: Null) do
	let r = http.router()
	r.route("GET", "/p", {}, fun(req, ctx) do
		http.text(200, "ok")
	end)

	let addr = "127.0.0.1:18107"
	let l = netListen(addr)
	let srv = http.serve(l, r, {})

	let c = netConnect(addr)
	let _ = write(c, "GET http://localhost/p HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n")
	let _ = flush(c)
	let resp = readAll(c)
	close(c)
	let _ = http.shutdown(srv, 0)

	testing.assert(len(split(resp, "HTTP/1.1 400 Bad Request")) > 1, "expected 400 for absolute-form target")
end)

# =========================================================
# nethttp/start-line limit -> 414 URI Too Long
# =========================================================
testing.test("nethttp/start-line too long -> 414", fun(_: Null) do
	let r = http.router()
	r.route("GET", "/ok", {}, fun(req, ctx) do
		http.text(200, "ok")
	end)

	let addr = "127.0.0.1:18108"
	let l = netListen(addr)
	let srv = http.serve(l, r, {maxStartLineBytes: 64})

	let big = "/" + join([
		"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
	], "")
	let c = netConnect(addr)
	let _ = write(c, "GET " + big + " HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n")
	let _ = flush(c)
	let resp = readAll(c)
	close(c)
	let _ = http.shutdown(srv, 0)

	testing.assert(len(split(resp, "HTTP/1.1 414 URI Too Long")) > 1, "expected 414")
end)

# =========================================================
# nethttp/header count limit -> 431
# =========================================================
testing.test("nethttp/header count -> 431", fun(_: Null) do
	let r = http.router()
	r.route("GET", "/h", {}, fun(req, ctx) do
		http.text(200, "ok")
	end)

	let addr = "127.0.0.1:18109"
	let l = netListen(addr)
	let srv = http.serve(l, r, {maxHeaders: 2})

	let req = "GET /h HTTP/1.1\r\nHost: localhost\r\nX-A: 1\r\nX-B: 2\r\nX-C: 3\r\nConnection: close\r\n\r\n"
	let c = netConnect(addr)
	let _ = write(c, req)
	let _ = flush(c)
	let resp = readAll(c)
	close(c)
	let _ = http.shutdown(srv, 0)

	testing.assert(len(split(resp, "HTTP/1.1 431 Request Header Fields Too Large")) > 1, "expected 431")
end)

# =========================================================
# nethttp/streaming chunked response (no Content-Length)
# Expect: Transfer-Encoding: chunked; body has chunk terminator; contains ticks
# =========================================================
testing.test("nethttp/streaming chunked", fun(_: Null) do
	let r = http.router()
	r.routeStream("GET", "/ticks", {}, fun(req, res, ctx) -> Null do
		res.status(200).setHeader("Content-Type", "text/plain")
		let _ = res.write("tick 0\n")
		sleep(10)
		let _ = res.write("tick 1\n")
		sleep(10)
		let _ = res.write("tick 2\n")
		let _ = res.end(null)
		null
	end)

	let addr = "127.0.0.1:18110"
	let l = netListen(addr)
	let srv = http.serve(l, r, {})

	let c = netConnect(addr)
	let _ = write(c, "GET /ticks HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n")
	let _ = flush(c)
	let resp = readAll(c)
	close(c)
	let _ = http.shutdown(srv, 0)

	testing.assert(len(split(resp, "HTTP/1.1 200 OK")) > 1, "bad status")
	testing.assert(len(split(resp, "Transfer-Encoding: chunked")) > 1, "missing chunked header")
	testing.assert(len(split(resp, "\r\n0\r\n\r\n")) > 1, "missing chunk terminator")
	testing.assert(len(split(resp, "tick 0")) > 1 and len(split(resp, "tick 2")) > 1, "missing chunks")
end)

# =========================================================
# nethttp/header CRLF injection blocked (redirect Location)
# Expect: 400 on CRLF in Location
# =========================================================
testing.test("nethttp/header crlf blocked", fun(_: Null) do
	let r = http.router()
	r.route("GET", "/bad", {}, fun(req, ctx) do
		# Attempt to inject newline in Location
		http.redirect(302, "/good\r\nInjected: yes")
	end)

	let addr = "127.0.0.1:18111"
	let l = netListen(addr)
	let srv = http.serve(l, r, {})

	let c = netConnect(addr)
	let _ = write(c, "GET /bad HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n")
	let _ = flush(c)
	let resp = readAll(c)
	close(c)
	let _ = http.shutdown(srv, 0)

	testing.assert(len(split(resp, "HTTP/1.1 400 Bad Request")) > 1, "expected 400 on CRLF header value")
end)

# =========================================================
# nethttp/redirect path-only policy
# Expect: external absolute URL is rejected with 400
# =========================================================
testing.test("nethttp/redirect external blocked", fun(_: Null) do
	let r = http.router()
	r.route("GET", "/go", {}, fun(req, ctx) do
		http.redirect(302, "http://evil.example/boom")
	end)

	let addr = "127.0.0.1:18112"
	let l = netListen(addr)
	let srv = http.serve(l, r, {})

	let c = netConnect(addr)
	let _ = write(c, "GET /go HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n")
	let _ = flush(c)
	let resp = readAll(c)
	close(c)
	let _ = http.shutdown(srv, 0)

	testing.assert(len(split(resp, "HTTP/1.1 400 Bad Request")) > 1, "expected 400 for external redirect")
end)

# =========================================================
# nethttp/path normalization for matching (handlers still see raw)
# Expect: /a//b and /a/./b route same as /a/b
# =========================================================
testing.test("nethttp/path normalization (match only)", fun(_: Null) do
	let r = http.router()
	r.route("GET", "/a/b", {}, fun(req, ctx) do
		http.text(200, "ok")
	end)

	let addr = "127.0.0.1:18113"
	let l = netListen(addr)
	let srv = http.serve(l, r, {})

	let req1 = "GET /a//b HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n"
	let req2 = "GET /a/./b HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n"

	let c1 = netConnect(addr)
	let _ = write(c1, req1)
	let _ = flush(c1)
	let resp1 = readAll(c1)
	close(c1)
	testing.assert(len(split(resp1, "HTTP/1.1 200 OK")) > 1, "expected 200 for // path")

	let c2 = netConnect(addr)
	let _2 = write(c2, req2)
	let _2f = flush(c2)
	let resp2 = readAll(c2)
	close(c2)
	testing.assert(len(split(resp2, "HTTP/1.1 200 OK")) > 1, "expected 200 for ./ path")

	let _ = http.shutdown(srv, 0)
end)

# =========================================================
# nethttp/cors credentials: echo Origin + Vary: Origin
# =========================================================
testing.test("nethttp/cors credentials echo origin", fun(_: Null) do
	let r = http.router()
	r.use(http.mwCors({origin: "*", credentials: true}))
	r.route("GET", "/ping", {}, fun(req, ctx) do
		http.text(200, "pong")
	end)

	let addr = "127.0.0.1:18114"
	let l = netListen(addr)
	let srv = http.serve(l, r, {})

	let c = netConnect(addr)
	let _ = write(c, "GET /ping HTTP/1.1\r\nHost: localhost\r\nOrigin: https://example.com\r\nConnection: close\r\n\r\n")
	let _ = flush(c)
	let resp = readAll(c)
	close(c)
	let _ = http.shutdown(srv, 0)

	testing.assert(len(split(resp, "HTTP/1.1 200 OK")) > 1, "bad status")
	testing.assert(len(split(resp, "Access-Control-Allow-Origin: https://example.com")) > 1, "expected reflected origin")
	testing.assert(len(split(resp, "\nVary: Origin")) > 1, "missing Vary: Origin")
end)

# =========================================================
# nethttp/date header default ON; toggle OFF
# =========================================================
testing.test("nethttp/date header on/off", fun(_: Null) do
	let r1 = http.router()
	r1.route("GET", "/d", {}, fun(req, ctx) do
		http.text(200, "ok")
	end)

	let addr1 = "127.0.0.1:18115"
	let l1 = netListen(addr1)
	let srv1 = http.serve(l1, r1, {addDateHeader: true})

	let c1 = netConnect(addr1)
	let _ = write(c1, "GET /d HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n")
	let _ = flush(c1)
	let resp1 = readAll(c1)
	close(c1)
	let _ = http.shutdown(srv1, 0)
	testing.assert(len(split(resp1, "\nDate: ")) > 1, "expected Date header present")

	let r2 = http.router()
	r2.route("GET", "/d", {}, fun(req, ctx) do
		http.text(200, "ok")
	end)

	let addr2 = "127.0.0.1:18116"
	let l2 = netListen(addr2)
	let srv2 = http.serve(l2, r2, {addDateHeader: false})

	let c2 = netConnect(addr2)
	let _2 = write(c2, "GET /d HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n")
	let _2f = flush(c2)
	let resp2 = readAll(c2)
	close(c2)
	let _ = http.shutdown(srv2, 0)
	testing.assert(len(split(resp2, "\nDate: ")) == 1, "Date header should be absent when disabled")
end)

# =========================================================
# nethttp/HEAD auto for value routes
# Expect: 200; Content-Length equals GET body length; no body bytes written.
# =========================================================
testing.test("nethttp/HEAD auto value route", fun(_: Null) do
	let http = import("nethttp")

	let r = http.router()
	r.route("GET", "/h", {}, fun(req, ctx) do
		http.text(200, "hello")
	end)

	let addr = "127.0.0.1:18120"
	let l = netListen(addr)
	let srv = http.serve(l, r, {}) # default: no Date header (fine)

	let c = netConnect(addr) # Send HEAD (no body expected)
	let _ = write(c, "HEAD /h HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n")
	let _ = flush(c)
	let resp = readAll(c)
	close(c)
	let _ = http.shutdown(srv, 0)

	# Status OK
	testing.assert(len(split(resp, "HTTP/1.1 200 OK")) > 1, "bad status")
	# Correct Content-Length (len("hello") == 5)
	testing.assert(len(split(resp, "\nContent-Length: 5")) > 1, "wrong Content-Length for HEAD")
	# No body bytes after header terminator
	testing.assert(len(split(resp, "\r\n\r\nhello")) == 1, "HEAD must not include body")
end)

# =========================================================
# nethttp/HEAD on streaming route -> 405 (unless explicitly registered)
# Expect: 405 and Allow header lists GET
# =========================================================
testing.test("nethttp/HEAD streaming -> 405", fun(_: Null) do
	let http = import("nethttp")

	let r = http.router()
	r.routeStream("GET", "/s", {}, fun(req, res, ctx) -> Null do
		res.status(200).setHeader("Content-Type", "text/plain").write("stream")
		res.end(null)
		null
	end)

	let addr = "127.0.0.1:18121"
	let l = netListen(addr)
	let srv = http.serve(l, r, {})

	let c = netConnect(addr)
	let _ = write(c, "HEAD /s HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n")
	let _ = flush(c)
	let resp = readAll(c)
	close(c)
	let _ = http.shutdown(srv, 0)

	testing.assert(len(split(resp, "HTTP/1.1 405 Method Not Allowed")) > 1, "expected 405 for HEAD on streaming route")
	testing.assert(len(split(resp, "\nAllow: GET")) > 1, "Allow header should list GET")
end)

# =========================================================
# nethttp/accesslog 404 catch-all does not alter response
# Ensures middleware path executes for unknown routes.
# =========================================================
testing.test("nethttp/accesslog 404 catch-all", fun(_: Null) do
	let http = import("nethttp")

	let r = http.router()
	r.use(http.mwAccessLog())
	r.use(http.mwRecover())

	let c = http.testClient(r)
	let res = c.call({method: "GET", path: "/no-such"})
	testing.assertEq(404, res.status)
end)

# =========================================================
# nethttp/accesslog 422 catch-all for validation errors
# Router + access log must still emit proper 422 response.
# =========================================================
testing.test("nethttp/accesslog 422 catch-all", fun(_: Null) do
	let http = import("nethttp")

	let r = http.router()
	r.use(http.mwAccessLog())
	r.route("GET", "/q", http.contract({query: type {n: Int}}), fun(req, ctx) do
		http.text(200, "ok")
	end)

	let c = http.testClient(r)
	let res = c.call({method: "GET", path: "/q?n=oops"})
	testing.assertEq(422, res.status)
end)