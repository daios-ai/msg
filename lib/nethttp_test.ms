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
