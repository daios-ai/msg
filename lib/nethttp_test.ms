# nethttp_test.ms — tiny & orthogonal tests

let testing = import("testing")
let nethttp = import("nethttp")
let Request   = nethttp.Request
let Responder = nethttp.Responder

# --- Router: exact match
testing.test("nethttp/router/exact", fun(_: Null) -> Any do
	let r = nethttp.router()
	r.handle("GET", "/ping", fun(req: Request) -> Any? do
		return nethttp.text(200, "pong")
	end)

	let tc = nethttp.testClient(r)
	let res = tc.call({ method: "GET", path: "/ping" })
	testing.assertEq(200, res.status)
	testing.assertEq("pong", res.body)

	let miss = tc.call({ method: "GET", path: "/nope" })
	testing.assertEq(404, miss.status)
end)

# --- Router: path param
testing.test("nethttp/router/param", fun(_: Null) -> Any do
	let r = nethttp.router()
	r.handle("GET", "/users/{id}", fun(req: Request) -> Any? do
		let p = nethttp.path(req)
		return nethttp.json(200, { id: p.id })
	end)

	let tc = nethttp.testClient(r)
	let res = tc.call({ method: "GET", path: "/users/123" })
	testing.assertEq(200, res.status)
	testing.assertEq({ id: "123" }, jsonParse(res.body))
end)

# --- Router: prefix mount
testing.test("nethttp/router/prefix", fun(_: Null) -> Any do
	let api = nethttp.router()
	api.handle("GET", "/x", fun(req: Request) -> Any? do
		return { ok: true }
	end)

	let r = nethttp.router()
	r.mount("/api", api)

	let tc = nethttp.testClient(r)
	let res = tc.call({ method: "GET", path: "/api/x" })
	testing.assertEq(200, res.status)
	testing.assertEq({ ok: true }, jsonParse(res.body))
end)

# --- bindJson: ok and bad
testing.test("nethttp/bindJson", fun(_: Null) -> Any do
	let r = nethttp.router()
	let T = type { name!: Str, age: Int }

	r.handle("POST", "/users", fun(req: Request) -> Any? do
		let payload = nethttp.bindJson(req, T)
		if payload == null then
			return nethttp.raise(400, "<invalid body>")
		end
		return nethttp.json(201, payload)
	end)

	let tc = nethttp.testClient(r)

	let ok = tc.call({
		method: "POST",
		path: "/users",
		headers: {"Content-Type":"application/json"},
		body: "{\"name\":\"Ada\",\"age\":36}"
	})
	testing.assertEq(201, ok.status)
	testing.assertEq({ name: "Ada", age: 36 }, jsonParse(ok.body))

	let bad = tc.call({
		method: "POST",
		path: "/users",
		headers: {"Content-Type":"application/json"},
		body: "{oops}"
	})
	testing.assertEq(400, bad.status)
end)

# --- bindQuery: simple shape
testing.test("nethttp/bindQuery", fun(_: Null) -> Any do
	let r = nethttp.router()
	let Q = type { q!: Str }

	r.handle("GET", "/search", fun(req: Request) -> Any? do
		let v = nethttp.bindQuery(req, Q)
		if v == null then
			return nethttp.raise(400, "<invalid query>")
		end
		return nethttp.text(200, v.q)
	end)

	let tc = nethttp.testClient(r)
	let res = tc.call({ method: "GET", path: "/search", query: "q=hello" })
	testing.assertEq(200, res.status)
	testing.assertEq("hello", res.body)
end)

# --- Response shaping: raw value vs explicit text/json
testing.test("nethttp/response/helpers", fun(_: Null) -> Any do
	let r = nethttp.router()

	r.handle("GET", "/raw", fun(req: Request) -> Any? do
		return { a: 1 }     # auto 200 JSON
	end)

	r.handle("GET", "/text", fun(req: Request) -> Any? do
		return nethttp.text(200, "hi")
	end)

	let tc = nethttp.testClient(r)

	let raw = tc.call({ method: "GET", path: "/raw" })
	testing.assertEq(200, raw.status)
	testing.assertEq({ a: 1 }, jsonParse(raw.body))

	let txt = tc.call({ method: "GET", path: "/text" })
	testing.assertEq(200, txt.status)
	testing.assertEq("hi", txt.body)
end)

# --- Streaming handler
testing.test("nethttp/streaming/basic", fun(_: Null) -> Any do
	let r = nethttp.router()
	r.handle("GET", "/s", fun(req: Request, res: Responder) -> Null do
		res.status(200).setHeader("Content-Type", "text/plain")
		let _ = res.write("a\n")
		res.flush(null)
		let _2 = res.write("b\n")
		res.end(null)
	end)

	let tc = nethttp.testClient(r)
	let res = tc.call({ method: "GET", path: "/s" })
	testing.assertEq(200, res.status)
	testing.assertEq("text/plain", res.headers["Content-Type"])
	testing.assertEq("a\nb\n", res.body)
end)

# --- Middleware: recover (panic -> 500 JSON)
testing.test("nethttp/mw/recover", fun(_: Null) -> Any do
	let r = nethttp.router()
	r.use(nethttp.mwRecover())

	r.handle("GET", "/boom", fun(req: Request) -> Any? do
		fail("boom")
	end)

	let tc = nethttp.testClient(r)
	let res = tc.call({ method: "GET", path: "/boom" })
	testing.assertEq(500, res.status)
	testing.assertEq({ error: "internal error" }, jsonParse(res.body))
end)

# --- Middleware: request id header
testing.test("nethttp/mw/request_id", fun(_: Null) -> Any do
	let r = nethttp.router()
	r.use(nethttp.mwRecover())
	r.use(nethttp.mwRequestID("X-Req"))

	r.handle("GET", "/ok", fun(req: Request) -> Any? do
		return { ok: true }
	end)

	let tc = nethttp.testClient(r)
	let res = tc.call({ method: "GET", path: "/ok" })
	testing.assertEq(200, res.status)
	testing.assert(res.headers["X-Req"] != null, "missing X-Req")
	testing.assertEq({ ok: true }, jsonParse(res.body))
end)

# --- CORS preflight (OPTIONS should be 204 via middleware)
testing.test("nethttp/mw/cors", fun(_: Null) -> Any do
	let r = nethttp.router()
	r.use(nethttp.mwRecover())
	r.use(nethttp.mwCors({}))

	r.handle("POST", "/echo", fun(req: Request) -> Any? do
		let T = type { msg!: Str }
		let v = nethttp.bindJson(req, T)
		if v == null then return nethttp.raise(400, "<bad>") end
		return nethttp.json(200, { ok: true, msg: v.msg })
	end)

	let tc = nethttp.testClient(r)

	let pre = tc.call({ method: "OPTIONS", path: "/echo" })
	testing.assertEq(204, pre.status)
	testing.assertEq("*", pre.headers["Access-Control-Allow-Origin"])

	let resp = tc.call({
		method: "POST",
		path: "/echo",
		headers: {"Content-Type":"application/json"},
		body: "{\"msg\":\"yo\"}"
	})
	testing.assertEq(200, resp.status)
	testing.assertEq({ ok: true, msg: "yo" }, jsonParse(resp.body))
	testing.assertEq("*", resp.headers["Access-Control-Allow-Origin"])
end)

# --- Docs: routes listed
testing.test("nethttp/docs/openapi_list", fun(_: Null) -> Any do
	let api = nethttp.router()
	api.handle("GET", "/users/{id}", fun(req: Request) -> Any? do
		return { ok: true }
	end)

	let r = nethttp.router()
	r.mount("/api", api)

	r.handle("GET", "/docs.json", fun(req: Request) -> Any? do
		let spec = nethttp.openapi(r)
		if spec == null then return nethttp.raise(500, "<docs>") end
		return nethttp.json(200, spec)
	end)

	let tc = nethttp.testClient(r)
	let docs = tc.call({ method: "GET", path: "/docs.json" })
	testing.assertEq(200, docs.status)
	testing.assert(
		len(split(docs.body, "\"/api/users/{id}\"")) > 1,
		"docs missing route"
	)
end)
