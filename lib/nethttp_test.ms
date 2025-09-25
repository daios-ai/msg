let testing = import("testing")
let http = import("lib/nethttp.ms")

# -------------------------------
# bindQuery: success
# -------------------------------
testing.test("nethttp/bindQuery success", fun(_: Null) -> Any do
	let req = {
		method: "GET",
		url: { scheme: null, host: null, path: "/q", query: "name=Ada&age=36" },
		headers: {},
		body: "",
		pathParams: {}
	}
	let Q = type { name!: Str, age: Num }
	let v = http.bindQuery(req, Q)
	testing.assertEq({ name: "Ada", age: 36 }, v)
end)

# -------------------------------
# bindQuery: failure (null + note contains reason)
# -------------------------------
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

	testing.assertEq(null, v)

	let note = noteGet(v)
	let parts = split(note, "query validation failed")
	testing.assertEq(true, len(parts) > 1)
end)

# -------------------------------
# tiny router (value): hello/{name}
# -------------------------------
testing.test("nethttp/router hello (value)", fun(_: Null) -> Any do
	let Request = http.Request
	let r = http.router()

	r.handleValue("GET", "/hello/{name}", fun(req: Request) -> Any? do
		let p = http.path(req)
		http.text(200, "hi " + p.name)
	end)

	let c = http.testClient(r)
	let res = c.call({ method: "GET", path: "/hello/Ada" })
	testing.assertEq(200, res.status)
	testing.assertEq("hi Ada", res.body)
end)

# -------------------------------
# tiny router (stream): write + end
# -------------------------------
testing.test("nethttp/streaming tiny", fun(_: Null) -> Any do
	let r = http.router()

	r.handle("GET", "/stream", fun(req, res) -> Null do
		res.status(200).setHeader("Content-Type", "text/plain")
		let _ = res.write("hey")
		let _2 = res.end(null)
		null
	end)

	let c = http.testClient(r)
	let res = c.call({ method: "GET", path: "/stream" })
	testing.assertEq(200, res.status)
	testing.assertEq("hey", res.body)
end)

# -------------------------------
# value echo: bindJson good and bad (no HttpError helper)
# -------------------------------
testing.test("nethttp/value echo", fun(_: Null) -> Any do
	let r = http.router()

	r.handleValue("POST", "/echo", fun(req) -> Any? do
		let T = type { msg!: Str }
		let v = http.bindJson(req, T)
		if v == null then
			return http.text(400, "bad json")
		end
		http.json(200, { ok: true, msg: v.msg })
	end)

	let c = http.testClient(r)
	let res = c.call({ method: "POST", path: "/echo", body: "{\"msg\":\"hi\"}" })
	testing.assertEq(200, res.status)

	let obj = jsonParse(res.body)
	testing.assertEq(true, obj.ok)
	testing.assertEq("hi", obj.msg)

	let resBad = c.call({ method: "POST", path: "/echo", body: "oops" })
	testing.assertEq(400, resBad.status)
	testing.assertEq("bad json", resBad.body)
end)

# -------------------------------
# 404 / 405 / prefix match
# -------------------------------
testing.test("nethttp/routing 404_405", fun(_: Null) -> Any do
	let r = http.router()
	r.handle("GET", "/x/", fun(req, res) -> Null do
		res.status(200).write("ok")
		res.end(null)
		null
	end)
	let c = http.testClient(r)
	let a = c.call({ method: "GET", path: "/x/abc" })
	testing.assertEq(200, a.status)
	let b = c.call({ method: "POST", path: "/x/abc" })
	testing.assertEq(405, b.status)
	let c2 = c.call({ method: "GET", path: "/nope" })
	testing.assertEq(404, c2.status)
end)

# -------------------------------
# CORS preflight (OPTIONS)
# -------------------------------
testing.test("nethttp/cors preflight", fun(_: Null) -> Any do
	let r = http.router()
	r.use(http.mwCors({origin: "*"}))
	let c = http.testClient(r)
	let res = c.call({ method: "OPTIONS", path: "/any" })
	testing.assertEq(204, res.status)
end)

# -------------------------------
# recover middleware: value panic -> 500
# -------------------------------
testing.test("nethttp/recover value panic", fun(_: Null) -> Any do
	let r = http.router()
	r.use(http.mwRecover())
	r.handleValue("GET", "/p", fun(req) -> Any? do
		panic("boom")
	end)
	let c = http.testClient(r)
	let res = c.call({ method: "GET", path: "/p" })
	testing.assertEq(500, res.status)
end)

# -------------------------------
# recover middleware: stream panic -> 500 + body
# -------------------------------
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

# -------------------------------
# timeout middleware (value) — intent: slow -> 504
# -------------------------------
testing.test("nethttp/timeout value", fun(_: Null) -> Any do
	let r = http.router()
	r.use(http.mwTimeout(20))
	r.handleValue("GET", "/slow", fun(req) -> Any? do
		sleep(50)
		http.text(200, "late")
	end)
	let c = http.testClient(r)
	let res = c.call({ method: "GET", path: "/slow" })
	testing.assertEq(504, res.status)
end)

# -------------------------------
# mount child router under prefix
# -------------------------------
testing.test("nethttp/mount prefix", fun(_: Null) -> Any do
	let child = http.router()
	child.handleValue("GET", "/ping", fun(req) -> Any? do
		http.text(200, "pong")
	end)

	let parent = http.router()
	parent.mount("/v1", child)

	let c = http.testClient(parent)
	let res = c.call({ method: "GET", path: "/v1/ping" })
	testing.assertEq(200, res.status)
	testing.assertEq("pong", res.body)
end)

# -------------------------------
# implicit JSON encode from value handler
# -------------------------------
testing.test("nethttp/implicit json", fun(_: Null) -> Any do
	let r = http.router()
	r.handleValue("GET", "/j", fun(req) -> Any? do
		{ ok: true, n: 7 }
	end)
	let c = http.testClient(r)
	let res = c.call({ method: "GET", path: "/j" })
	testing.assertEq(200, res.status)
	let obj = jsonParse(res.body)
	testing.assertEq(true, obj.ok)
	testing.assertEq(7, obj.n)
end)

# -------------------------------
# JSON encode error (non-serializable) -> 500
# -------------------------------
testing.test("nethttp/json encode error", fun(_: Null) -> Any do
	let r = http.router()
	r.handleValue("GET", "/badjson", fun(req) -> Any? do
		fun(_: Null) -> Any do true end
	end)
	let c = http.testClient(r)
	let res = c.call({ method: "GET", path: "/badjson" })
	testing.assertEq(500, res.status)
	testing.assertEq("json encode error", res.body)
end)

# -------------------------------
# request ID: added on value path
# -------------------------------
testing.test("nethttp/request id header (value)", fun(_: Null) -> Any do
	let r = http.router()
	r.use(http.mwRequestID("X-Request-ID"))
	r.handleValue("GET", "/ok", fun(req) -> Any? do
		http.text(200, "ok")
	end)
	let c = http.testClient(r)
	let res = c.call({ method: "GET", path: "/ok" })
	testing.assertEq(200, res.status)
	testing.assert(res.headers["X-Request-ID"] != null, "missing request id")
end)

# -------------------------------
# request ID: propagate incoming
# -------------------------------
testing.test("nethttp/request id propagate incoming", fun(_: Null) -> Any do
	let r = http.router()
	r.use(http.mwRequestID("X-Request-ID"))
	r.handleValue("GET", "/p", fun(req) -> Any? do
		http.text(200, "ok")
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

# -------------------------------
# request ID: present on streaming path
# -------------------------------
testing.test("nethttp/request id streaming", fun(_: Null) -> Any do
	let r = http.router()
	r.use(http.mwRequestID("X-Request-ID"))
	r.handle("GET", "/s", fun(req, res) -> Null do
		res.status(200).setHeader("Content-Type", "text/plain").write("ok")
		res.end(null)
		null
	end)

	let c = http.testClient(r)
	let res = c.call({ method: "GET", path: "/s" })
	testing.assertEq(200, res.status)
	testing.assert(res.headers["X-Request-ID"] != null, "missing X-Request-ID")
end)

# -------------------------------
# access log: body preserved and chaining intact
# -------------------------------
testing.test("nethttp/accesslog counts streaming bytes", fun(_: Null) -> Any do
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

testing.test("nethttp/mwAccessLog keeps chaining on proxy", fun(_: Null) -> Any do
	let r = http.router()
	r.use(http.mwAccessLog())
	r.handle("GET", "/chain", fun(req, res) -> Null do
		res.status(201).setHeader("X", "1").write("ok")
		res.end(null)
		null
	end)
	let c = http.testClient(r)
	let res = c.call({ method: "GET", path: "/chain" })
	testing.assertEq(201, res.status)
	testing.assertEq("ok", res.body)
	testing.assertEq("1", res.headers["X"])
end)

# -------------------------------
# openapi: routes list with style tags
# -------------------------------
testing.test("nethttp/openapi routes (style)", fun(_: Null) -> Any do
	let r = http.router()
	r.handleValue("GET", "/a/{id}", fun(req) -> Any? do
		http.text(200, "ok")
	end)
	r.handle("GET", "/stream", fun(req, res) -> Null do
		res.status(200).write("x")
		res.end(null)
		null
	end)

	let doc = http.openapi(r)
	testing.assert(doc != null, "openapi null")
	let rs = doc.routes
	testing.assertEq(2, len(rs))

	let foundStream = false
	let foundValue = false
	let i = 0
	while i < len(rs) do
		let it = rs[i]
		if it.pattern == "/stream" and it.style == "stream" then
			foundStream = true
		end
		if it.pattern == "/a/{id}" and it.style == "value" then
			foundValue = true
		end
		i = i + 1
	end

	testing.assert(foundStream, "missing streaming route in openapi")
	testing.assert(foundValue, "missing value route in openapi")
end)

# -------------------------------
# NEW: streaming handler completes with annotated null
# -------------------------------
testing.test("nethttp/streaming annotated null completes", fun(_: Null) -> Any do
	let r = http.router()
	r.handle("GET", "/ann", fun(req, res) -> Null do
		res.status(200).setHeader("Content-Type","text/plain")
		res.write("ok")
		res.end(null)
		return null # <done>
	end)
	let c = http.testClient(r)
	let res = c.call({ method: "GET", path: "/ann" })
	testing.assertEq(200, res.status)
	testing.assertEq("ok", res.body)
end)
