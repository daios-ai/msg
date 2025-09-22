let testing = import("testing")
let http = import("nethttp")

# ---- bindQuery: success ----
testing.test("nethttp/bindQuery success", fun(_: Null) -> Any do
	let req = {
		method: "GET",
		url: { scheme: null, host: null, path: "/q", query: "name=Ada&age=36" },
		headers: {},
		body: "",
		pathParams: {}
	}
	let Q = type { name!: Str, age: Int }
	let v = http.bindQuery(req, Q)
	testing.assertEq({ name: "Ada", age: 36 }, v)
end)

# ---- bindQuery: failure (null + note) ----
testing.test("nethttp/bindQuery failure", fun(_: Null) -> Any do
	let req = {
		method: "GET",
		url: { scheme: null, host: null, path: "/q", query: "name=Ada&age=oops" },
		headers: {},
		body: "",
		pathParams: {}
	}
	let Q = type { name!: Str, age: Int }
	let v = http.bindQuery(req, Q)

	# must be null
	testing.assertEq(null, v)

	# note should contain the reason, regardless of angle-bracket framing
	let note = noteGet(v)
	let parts = split(note, "query validation failed")
	testing.assertEq(true, len(parts) > 1)
end)

# ---- tiny router: hello/{name} ----
testing.test("nethttp/router hello", fun(_: Null) -> Any do
	let Request = http.Request
	let r = http.router()

	r.handle("GET", "/hello/{name}", fun(req: Request) -> Any do
		let p = http.path(req)
		return http.text(200, "hi " + p.name)
	end)

	let c = http.testClient(r)
	let res = c.call({ method: "GET", path: "/hello/Ada" })
	testing.assertEq(200, res.status)
	testing.assertEq("hi Ada", res.body)
end)

# ---- tiny router: streaming ----
testing.test("nethttp/streaming tiny", fun(_: Null) -> Any do
	let r = http.router()

	r.handle("GET", "/stream", fun(req, res) -> Null do
		res.status(200).setHeader("Content-Type", "text/plain")
		let _ = res.write("hey")
		let _2 = res.end(null)
		return
	end)

	let c = http.testClient(r)
	let res = c.call({ method: "GET", path: "/stream" })
	testing.assertEq(200, res.status)
	testing.assertEq("hey", res.body)
end)

# ---- tiny router: value echo ----
testing.test("nethttp/value echo", fun(_: Null) -> Any do
	let r = http.router()

	r.handle("POST", "/echo", fun(req) -> Any? do
		let T = type { msg!: Str }
		let v = http.bindJson(req, T)
		if v == null then
			return http.raise(400, "<bad json>")
		end
		return http.json(200, { ok: true, msg: v.msg })
	end)

	let c = http.testClient(r)
	let res = c.call({ method: "POST", path: "/echo", body: "{\"msg\":\"hi\"}" })
	testing.assertEq(200, res.status)

	# Body is JSON; key order may vary — parse then assert fields.
	let obj = jsonParse(res.body)
	testing.assertEq(true, obj.ok)
	testing.assertEq("hi", obj.msg)
end)

# 404/405 behavior (prefix route + missing path)
testing.test("nethttp/routing 404_405", fun(_: Null) -> Any do
	let r = http.router()
	r.handle("GET", "/x/", fun(req, res) -> Null do
		res.status(200).write("ok")
		res.end(null)
		return
	end)
	let c = http.testClient(r)
	let a = c.call({ method: "GET", path: "/x/abc" })
	testing.assertEq(200, a.status)
	let b = c.call({ method: "POST", path: "/x/abc" })
	testing.assertEq(405, b.status)
	let c2 = c.call({ method: "GET", path: "/nope" })
	testing.assertEq(404, c2.status)
end)

# CORS preflight
testing.test("nethttp/cors preflight", fun(_: Null) -> Any do
	let r = http.router()
	r.use(http.mwCors({origin: "*"}))
	let c = http.testClient(r)
	let res = c.call({ method: "OPTIONS", path: "/any" })
	testing.assertEq(204, res.status)
end)

# Recover middleware on value handler that panics
testing.test("nethttp/recover value panic", fun(_: Null) -> Any do
	let r = http.router()
	r.use(http.mwRecover(null))
	r.handle("GET", "/p", fun(req) -> Any? do
		fail("boom")
	end)
	let c = http.testClient(r)
	let res = c.call({ method: "GET", path: "/p" })
	testing.assertEq(500, res.status)
end)

# Recover middleware on streaming handler that panics
testing.test("nethttp/recover stream panic", fun(_: Null) -> Any do
	let r = http.router()
	r.use(http.mwRecover(null))
	r.handle("GET", "/ps", fun(req, res) -> Null do
		fail("boom")
	end)
	let c = http.testClient(r)
	let res = c.call({ method: "GET", path: "/ps" })
	testing.assertEq(500, res.status)
	testing.assertEq("internal error", res.body)
end)

# Timeout middleware on value handler
testing.test("nethttp/timeout value", fun(_: Null) -> Any do
	let r = http.router()
	r.use(http.mwTimeout(20))
	r.handle("GET", "/slow", fun(req) -> Any? do
		sleep(50)
		return http.text(200, "late")
	end)
	let c = http.testClient(r)
	let res = c.call({ method: "GET", path: "/slow" })
	testing.assertEq(504, res.status)
end)

# Mount child router under a prefix
testing.test("nethttp/mount prefix", fun(_: Null) -> Any do
	let child = http.router()
	child.handle("GET", "/ping", fun(req) -> Any? do
		return http.text(200, "pong")
	end)

	let parent = http.router()
	parent.mount("/v1", child)

	let c = http.testClient(parent)
	let res = c.call({ method: "GET", path: "/v1/ping" })
	testing.assertEq(200, res.status)
	testing.assertEq("pong", res.body)
end)

# Value handler: implicit JSON encoding
testing.test("nethttp/implicit json", fun(_: Null) -> Any do
	let r = http.router()
	r.handle("GET", "/j", fun(req) -> Any? do
		return { ok: true, n: 7 }
	end)
	let c = http.testClient(r)
	let res = c.call({ method: "GET", path: "/j" })
	testing.assertEq(200, res.status)
	let obj = jsonParse(res.body)
	testing.assertEq(true, obj.ok)
	testing.assertEq(7, obj.n)
end)

# Value handler: JSON encode error (non-serializable)
testing.test("nethttp/json encode error", fun(_: Null) -> Any do
	let r = http.router()
	r.handle("GET", "/badjson", fun(req) -> Any? do
		return fun(_: Null) -> Any do true end
	end)
	let c = http.testClient(r)
	let res = c.call({ method: "GET", path: "/badjson" })
	testing.assertEq(500, res.status)
	testing.assertEq("json encode error", res.body)
end)

# Request ID middleware (value path)
testing.test("nethttp/request id header", fun(_: Null) -> Any do
	let r = http.router()
	r.use(http.mwRequestID("X-Request-ID"))
	r.handle("GET", "/ok", fun(req) -> Any? do
		return http.text(200, "ok")
	end)
	let c = http.testClient(r)
	let res = c.call({ method: "GET", path: "/ok" })
	testing.assertEq(200, res.status)
	testing.assert(res.headers["X-Request-ID"] != null, "missing request id")
end)

# Tiny openapi listing returns routes
testing.test("nethttp/openapi routes", fun(_: Null) -> Any do
	let r = http.router()
	r.handle("GET", "/a/{id}", fun(req) -> Any? do
		return http.text(200, "ok")
	end)
	r.handle("GET", "/stream", fun(req, res) -> Null do
		res.status(200).write("x")
		res.end(null)
		return
	end)
	let doc = http.openapi(r)
	testing.assert(doc != null, "openapi null")
	let rs = doc.routes
	testing.assertEq(2, len(rs))
	# find the streaming entry
	let found = false
	let i = 0
	while i < len(rs) do
		let it = rs[i]
		if it.pattern == "/stream" and it.streaming then
			found = true
		end
		i = i + 1
	end
	testing.assert(found, "missing streaming route in openapi")
end)

# Implicit-JSON response should include X-Request-ID
testing.test("nethttp/request id implicit json", fun(_: Null) -> Any do
	let r = http.router()
	r.use(http.mwRequestID("X-Request-ID"))
	r.handle("GET", "/j", fun(req) -> Any? do
		return { ok: true }   # implicit JSON
	end)

	let c = http.testClient(r)
	let res = c.call({ method: "GET", path: "/j" })
	testing.assertEq(200, res.status)
	testing.assert(res.headers["X-Request-ID"] != null, "missing X-Request-ID")
end)

# Streaming response should include X-Request-ID
testing.test("nethttp/request id streaming", fun(_: Null) -> Any do
	let r = http.router()
	r.use(http.mwRequestID("X-Request-ID"))
	r.handle("GET", "/s", fun(req, res) -> Null do
		res.status(200).setHeader("Content-Type", "text/plain").write("ok")
		res.end(null)
		return
	end)

	let c = http.testClient(r)
	let res = c.call({ method: "GET", path: "/s" })
	testing.assertEq(200, res.status)
	testing.assert(res.headers["X-Request-ID"] != null, "missing X-Request-ID")
end)

# Incoming X-Request-ID should be propagated (not regenerated)
testing.test("nethttp/request id propagate incoming", fun(_: Null) -> Any do
	let r = http.router()
	r.use(http.mwRequestID("X-Request-ID"))
	r.handle("GET", "/p", fun(req) -> Any? do
		return http.text(200, "ok")
	end)

	let c = http.testClient(r)
	let res = c.call({
		method: "GET",
		path: "/p",
		headers: {"X-Request-ID": "req-123"}
	})
	testing.assertEq(200, res.status)
	testing.assertEq("req-123", res.headers["X-Request-ID"])
end)
