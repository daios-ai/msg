let testing = import("testing")
let http = import("nethttp")

# =========================================================
# 1) PATH PARAMS — typed, automatic bind
# =========================================================
testing.test("nethttp/route path param (typed)", fun(_: Null) -> Any do
	let r = http.router()

	r.route("GET", "/hello/{name}", {
		path: type { name!: Str }
	}, fun(ctx) -> Any? do
		http.text(200, "hi " + ctx.path.name)
	end)

	let c = http.testClient(r)
	let res = c.call({ method: "GET", path: "/hello/Ada" })
	testing.assertEq(200, res.status)
	testing.assertEq("hi Ada", res.body)
end)

# =========================================================
# 2) QUERY — success (auto-bind to declared type)
# =========================================================
testing.test("nethttp/route query success", fun(_: Null) -> Any do
	let r = http.router()

	r.route("GET", "/q", {
		query: type { name!: Str, age!: Int }
	}, fun(ctx) -> Any? do
		http.json(200, { ok: true, who: ctx.query.name, age: ctx.query.age })
	end)

	let c = http.testClient(r)
	let res = c.call({ method: "GET", path: "/q?name=%22Ada%22&age=36" })
	testing.assertEq(200, res.status)
	let obj = jsonParse(res.body)
	testing.assertEq(true, obj.ok)
	testing.assertEq("Ada", obj.who)
	testing.assertEq(36, obj.age)
end)

# =========================================================
# 3) QUERY — failure (auto 400 on validation error)
# =========================================================
testing.test("nethttp/route query failure -> 422", fun(_: Null) -> Any do
	let r = http.router()

	r.route("GET", "/qbad", {
		query: type { name!: Str, age!: Int }
	}, fun(ctx) -> Any? do
		http.text(200, "ok")
	end)

	let c = http.testClient(r)
	let res = c.call({ method: "GET", path: "/qbad?name=Ada&age=oops" })
	testing.assertEq(422, res.status)
end)

# =========================================================
# 4) BODY — success
# =========================================================
testing.test("nethttp/route body json success", fun(_: Null) -> Any do
	let r = http.router()

	r.route("POST", "/echo", {
		body: type { msg!: Str }
	}, fun(ctx) -> Any? do
		http.json(200, { ok: true, msg: ctx.body.msg })
	end)

	let c = http.testClient(r)
	let res = c.call({ method: "POST", path: "/echo", body: "{\"msg\":\"hi\"}" })
	testing.assertEq(200, res.status)
	let obj = jsonParse(res.body)
	testing.assertEq(true, obj.ok)
	testing.assertEq("hi", obj.msg)
end)

# =========================================================
# 5) BODY — failure (auto 400)
# =========================================================
testing.test("nethttp/route body json failure -> 422", fun(_: Null) -> Any do
	let r = http.router()

	r.route("POST", "/echo2", {
		body: type { msg!: Str }
	}, fun(ctx) -> Any? do
		http.text(200, "ok")
	end)

	let c = http.testClient(r)
	let res = c.call({ method: "POST", path: "/echo2", body: "oops" })
	testing.assertEq(422, res.status)
end)


# =========================================================
# 6) IMPLICIT JSON ENCODE from returned value (no helper)
# =========================================================
testing.test("nethttp/route implicit json encode", fun(_: Null) -> Any do
	let r = http.router()

	r.route("GET", "/auto", {}, fun(ctx) -> Any? do
		{ ok: true, n: 7 }
	end)

	let c = http.testClient(r)
	let res = c.call({ method: "GET", path: "/auto" })
	testing.assertEq(200, res.status)
	let obj = jsonParse(res.body)
	testing.assertEq(true, obj.ok)
	testing.assertEq(7, obj.n)
end)

# =========================================================
# 7) RAISE helper (structured error) -> custom status
# =========================================================
testing.test("nethttp/route raise -> status", fun(_: Null) -> Any do
	let r = http.router()

	r.route("GET", "/need-auth", {}, fun(ctx) -> Any? do
		http.raise(401, "unauthorized")
	end)

	let c = http.testClient(r)
	let res = c.call({ method: "GET", path: "/need-auth" })
	testing.assertEq(401, res.status)
	testing.assertEq("unauthorized", res.body)
end)

# =========================================================
# 8) STREAMING handler (raw) still works
# =========================================================
testing.test("nethttp/streaming handler works", fun(_: Null) -> Any do
	let r = http.router()

	r.handle("GET", "/stream", fun(req, res) -> Null do
		res.status(200).setHeader("Content-Type","text/plain").write("hey")
		res.end(null)
		null
	end)

	let c = http.testClient(r)
	let res = c.call({ method: "GET", path: "/stream" })
	testing.assertEq(200, res.status)
	testing.assertEq("hey", res.body)
end)

# =========================================================
# 9) 404 / 405 and prefix matching
# =========================================================
testing.test("nethttp/routing 404_405 prefix", fun(_: Null) -> Any do
	let r = http.router()

	# prefix style (keep existing semantics)
	r.handle("GET", "/x/", fun(req, res) -> Null do
		res.status(200).write("ok")
		res.end(null)
		null
	end)

	let c = http.testClient(r)
	testing.assertEq(200, c.call({ method: "GET", path: "/x/abc" }).status)
	testing.assertEq(405, c.call({ method: "POST", path: "/x/abc" }).status)
	testing.assertEq(404, c.call({ method: "GET", path: "/nope" }).status)
end)

# =========================================================
# 10) CORS preflight (OPTIONS)
# =========================================================
testing.test("nethttp/cors preflight", fun(_: Null) -> Any do
	let r = http.router()
	r.use(http.mwCors({ origin: "*" }))

	let c = http.testClient(r)
	let res = c.call({ method: "OPTIONS", path: "/any" })
	testing.assertEq(204, res.status)
end)

# =========================================================
# 11) Recover middleware catches panics -> 500
# =========================================================
testing.test("nethttp/recover value panic", fun(_: Null) -> Any do
	let r = http.router()
	r.use(http.mwRecover())

	r.route("GET", "/p", {}, fun(ctx) -> Any? do
		panic("boom")
	end)

	let c = http.testClient(r)
	let res = c.call({ method: "GET", path: "/p" })
	testing.assertEq(500, res.status)
end)

testing.test("nethttp/recover stream panic", fun(_: Null) -> Any do
	let r = http.router()
	r.use(http.mwRecover())
	r.handle("GET", "/ps", fun(req, res) -> Null do
		panic("boom")
	end)

	let c = http.testClient(r)
	let res = c.call({ method: "GET", path: "/ps" })
	testing.assertEq(500, res.status)
end)

# =========================================================
# 12) Timeout middleware -> 504 on slow value handler
# =========================================================
testing.test("nethttp/timeout value", fun(_: Null) -> Any do
	let r = http.router()
	r.use(http.mwTimeout(20))

	r.route("GET", "/slow", {}, fun(ctx) -> Any? do
		sleep(50)
		http.text(200, "late")
	end)

	let c = http.testClient(r)
	let res = c.call({ method: "GET", path: "/slow" })
	testing.assertEq(504, res.status)
end)

# =========================================================
# 13) Request ID middleware (inject + propagate)
# =========================================================
testing.test("nethttp/request id (inject + propagate)", fun(_: Null) -> Any do
	let r = http.router()
	r.use(http.mwRequestID("X-Request-ID"))

	r.route("GET", "/ok", {}, fun(ctx) -> Any? do
		http.text(200, "ok")
	end)

	let c = http.testClient(r)
	let a = c.call({ method: "GET", path: "/ok" })
	testing.assertEq(200, a.status)
	testing.assert(a.headers["X-Request-ID"] != null, "missing request id")

	let b = c.call({ method: "GET", path: "/ok", headers: {"X-Request-ID": "req-123"} })
	testing.assertEq("req-123", b.headers["X-Request-ID"])
end)

# =========================================================
# 14) Access log middleware preserves body and chaining
# =========================================================
testing.test("nethttp/accesslog preserves body + chaining", fun(_: Null) -> Any do
	let r = http.router()
	r.use(http.mwAccessLog())

	r.handle("GET", "/s", fun(req, res) -> Null do
		res.status(200).setHeader("Content-Type","text/plain").write("hi")
		res.end(null)
		null
	end)

	let c = http.testClient(r)
	let res = c.call({ method: "GET", path: "/s" })
	testing.assertEq(200, res.status)
	testing.assertEq("hi", res.body)
end)

# =========================================================
# 15) Mount child router under prefix
# =========================================================
testing.test("nethttp/mount prefix", fun(_: Null) -> Any do
	let child = http.router()
	child.route("GET", "/ping", {}, fun(ctx) -> Any? do
		http.text(200, "pong")
	end)

	let parent = http.router()
	parent.mount("/v1", child)

	let c = http.testClient(parent)
	let res = c.call({ method: "GET", path: "/v1/ping" })
	testing.assertEq(200, res.status)
	testing.assertEq("pong", res.body)
end)

# =========================================================
# 16) JSON encode error -> 500
# =========================================================
testing.test("nethttp/json encode error", fun(_: Null) -> Any do
	let r = http.router()

	r.route("GET", "/badjson", {}, fun(ctx) -> Any? do
		fun(_: Null) -> Any do true end
	end)

	let c = http.testClient(r)
	let res = c.call({ method: "GET", path: "/badjson" })
	testing.assertEq(500, res.status)
	testing.assertEq("json encode error", res.body)
end)

# =========================================================
# 17) OpenAPI (minimal): ensure we get a doc back with at least the registered paths
#     Accept either openapiDoc(...) (new) or openapi(...) (legacy).
# =========================================================
testing.test("nethttp/openapi basic", fun(_: Null) -> Any do
	let r = http.router()

	r.route("GET", "/a/{id}", { path: type { id!: Str } }, fun(ctx) -> Any? do
		http.text(200, "ok")
	end)

	r.handle("GET", "/stream", fun(req, res) -> Null do
		res.status(200).write("x")
		res.end(null)
		null
	end)

	# Prefer openapiDoc if present; fallback to openapi
	let doc = if mapHas(http, "openapiDoc") then
		http.openapiDoc(r, { title: "API", version: "1.0.0" }, ["http://localhost"])
	else
		http.openapi(r)
	end
	testing.assert(doc != null, "openapi null")

	# Very light assertion so it works with both shapes
	let s = str(doc)
	testing.assert((len(split(s, "/a/{id}")) > 1) or (len(split(s, "\"/a/{id}\"")) > 1), "missing /a/{id} in doc")
end)
