# nethttp_test.ms — tiny & orthogonal tests

let testing = import("testing")
let nethttp = import("nethttp")
let Request = nethttp.Request
let Responder = nethttp.Responder

# --- Router: exact match
testing.test("nethttp/router/exact", fun(_: Null) do
	let r = nethttp.router()
	r.handle("GET", "/ping", fun(req: Request) -> Any? do
		return nethttp.text(200, "pong")
	end)

	let tc = nethttp.testClient(r)
	let res = tc.call({method: "GET", path: "/ping"})
	testing.assertEq(200, res.status)
	testing.assertEq("pong", res.body)

	let miss = tc.call({method: "GET", path: "/nope"})
	testing.assertEq(404, miss.status)
end)

# --- Router: path param
testing.test("nethttp/router/param", fun(_: Null) do
	let r = nethttp.router()
	r.handle("GET", "/users/{id}", fun(req: Request) -> Any? do
		let p = nethttp.path(req)
		return nethttp.json(200, {id: p.id})
	end)

	let tc = nethttp.testClient(r)
	let res = tc.call({method: "GET", path: "/users/123"})
	testing.assertEq(200, res.status)
	testing.assertEq({id: "123"}, jsonParse(res.body))
end)

# --- Router: prefix mount
testing.test("nethttp/router/prefix", fun(_: Null) do
	let api = nethttp.router()
	api.handle("GET", "/x", fun(req: Request) -> Any? do
		return {ok: true}
	end)

	let r = nethttp.router()
	r.mount("/api", api)

	let tc = nethttp.testClient(r)
	let res = tc.call({method: "GET", path: "/api/x"})
	testing.assertEq(200, res.status)
	testing.assertEq({ok: true}, jsonParse(res.body))
end)

# --- bindJson: ok and bad
testing.test("nethttp/bindJson", fun(_: Null) do
	let r = nethttp.router()
	let T = type {name: Str, age: Int}

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
		headers: {"Content-Type": "application/json"},
		body: "{\"name\":\"Ada\",\"age\":36}"
	})
	testing.assertEq(201, ok.status)
	testing.assertEq({name: "Ada", age: 36}, jsonParse(ok.body))

	let bad = tc.call({
		method: "POST",
		path: "/users",
		headers: {"Content-Type": "application/json"},
		body: "{oops}"
	})
	testing.assertEq(400, bad.status)
end)

# --- bindQuery: simple shape
testing.test("nethttp/bindQuery", fun(_: Null) do
	let r = nethttp.router()
	let Q = type {q: Str}

	r.handle("GET", "/search", fun(req: Request) -> Any? do
		let v = nethttp.bindQuery(req, Q)
		if v == null then
			return nethttp.raise(400, "<invalid query>")
		end
		return nethttp.text(200, v.q)
	end)

	let tc = nethttp.testClient(r)
	let res = tc.call({method: "GET", path: "/search", query: "q=hello"})
	testing.assertEq(200, res.status)
	testing.assertEq("hello", res.body)
end)

# --- Response shaping: raw value vs explicit text/json
testing.test("nethttp/response/helpers", fun(_: Null) do
	let r = nethttp.router()

	r.handle("GET", "/raw", fun(req: Request) -> Any? do
		return {a: 1} # auto 200 JSON

	end)

	r.handle("GET", "/text", fun(req: Request) -> Any? do
		return nethttp.text(200, "hi")
	end)

	let tc = nethttp.testClient(r)

	let raw = tc.call({method: "GET", path: "/raw"})
	testing.assertEq(200, raw.status)
	testing.assertEq({a: 1}, jsonParse(raw.body))

	let txt = tc.call({method: "GET", path: "/text"})
	testing.assertEq(200, txt.status)
	testing.assertEq("hi", txt.body)
end)

# --- Streaming handler
testing.test("nethttp/streaming/basic", fun(_: Null) do
	let r = nethttp.router()
	r.handle("GET", "/s", fun(req: Request, res: Responder) -> Null do
		res.status(200).setHeader("Content-Type", "text/plain")
		let _ = res.write("a\n")
		res.flush(null)
		let _2 = res.write("b\n")
		res.end(null)
	end)

	let tc = nethttp.testClient(r)
	let res = tc.call({method: "GET", path: "/s"})
	testing.assertEq(200, res.status)
	testing.assertEq("text/plain", res.headers["Content-Type"])
	testing.assertEq("a\nb\n", res.body)
end)

# --- Middleware: recover (panic -> 500 JSON)
testing.test("nethttp/mw/recover", fun(_: Null) do
	let r = nethttp.router()
	r.use(nethttp.mwRecover())

	r.handle("GET", "/boom", fun(req: Request) -> Any? do
		fail("boom")
	end)

	let tc = nethttp.testClient(r)
	let res = tc.call({method: "GET", path: "/boom"})
	testing.assertEq(500, res.status)
	testing.assertEq({error: "internal error"}, jsonParse(res.body))
end)

# --- Middleware: request id header
testing.test("nethttp/mw/request_id", fun(_: Null) do
	let r = nethttp.router()
	r.use(nethttp.mwRecover())
	r.use(nethttp.mwRequestID("X-Req"))

	r.handle("GET", "/ok", fun(req: Request) -> Any? do
		return {ok: true}
	end)

	let tc = nethttp.testClient(r)
	let res = tc.call({method: "GET", path: "/ok"})
	testing.assertEq(200, res.status)
	testing.assert(res.headers["X-Req"] != null, "missing X-Req")
	testing.assertEq({ok: true}, jsonParse(res.body))
end)

# --- CORS preflight (OPTIONS should be 204 via middleware)
testing.test("nethttp/mw/cors", fun(_: Null) do
	let r = nethttp.router()
	r.use(nethttp.mwRecover())
	r.use(nethttp.mwCors({}))

	r.handle("POST", "/echo", fun(req: Request) -> Any? do
		let T = type {msg: Str}
		let v = nethttp.bindJson(req, T)
		if v == null then
			return nethttp.raise(400, "<bad>")
		end
		return nethttp.json(200, {ok: true, msg: v.msg})
	end)

	let tc = nethttp.testClient(r)

	let pre = tc.call({method: "OPTIONS", path: "/echo"})
	testing.assertEq(204, pre.status)
	testing.assertEq("*", pre.headers["Access-Control-Allow-Origin"])

	let resp = tc.call({
		method: "POST",
		path: "/echo",
		headers: {"Content-Type": "application/json"},
		body: "{\"msg\":\"yo\"}"
	})
	testing.assertEq(200, resp.status)
	testing.assertEq({ok: true, msg: "yo"}, jsonParse(resp.body))
	testing.assertEq("*", resp.headers["Access-Control-Allow-Origin"])
end)

# --- Docs: routes listed
testing.test("nethttp/docs/openapi_list", fun(_: Null) do
	let api = nethttp.router()
	api.handle("GET", "/users/{id}", fun(req: Request) -> Any? do
		return {ok: true}
	end)

	let r = nethttp.router()
	r.mount("/api", api)

	r.handle("GET", "/docs.json", fun(req: Request) -> Any? do
		let spec = nethttp.openapi(r)
		if spec == null then
			return nethttp.raise(500, "<docs>")
		end
		return nethttp.json(200, spec)
	end)

	let tc = nethttp.testClient(r)
	let docs = tc.call({method: "GET", path: "/docs.json"})
	testing.assertEq(200, docs.status)
	testing.assert(len(split(docs.body, "\"/api/users/{id}\"")) > 1, "docs missing route")
end)

# --- 405 on method mismatch (exact + prefix)
testing.test("nethttp/router/405", fun(_: Null) do
	let r = nethttp.router()
	r.handle("GET", "/x", fun(req: Request) -> Any? do
		{ok: true}
	end)
	r.handle("GET", "/api/", fun(req: Request) -> Any? do
		{ok: true}
	end) # prefix

	let tc = nethttp.testClient(r)
	let exact = tc.call({method: "POST", path: "/x"})
	testing.assertEq(405, exact.status)

	let pref = tc.call({method: "POST", path: "/api/anything"})
	testing.assertEq(405, pref.status)
end)

# --- Middleware applies to 404 leaf (CORS preflight on unknown path)
testing.test("nethttp/mw/cors_on_404", fun(_: Null) do
	let r = nethttp.router()
	r.use(nethttp.mwCors({}))

	let tc = nethttp.testClient(r)
	let pre = tc.call({method: "OPTIONS", path: "/no/route"})
	testing.assertEq(204, pre.status)
	testing.assertEq("*", pre.headers["Access-Control-Allow-Origin"])
end)

# --- Request ID is injected even when handler returns an explicit Response
testing.test("nethttp/mw/request_id_on_response", fun(_: Null) do
	let r = nethttp.router()
	r.use(nethttp.mwRequestID("X-Req"))

	r.handle("GET", "/resp", fun(req: Request) -> Any? do
		return nethttp.text(200, "ok")
	end)

	let tc = nethttp.testClient(r)
	let res = tc.call({method: "GET", path: "/resp"})
	testing.assertEq(200, res.status)
	testing.assert(res.headers["X-Req"] != null, "missing X-Req on Response-returning handler")
	testing.assertEq("ok", res.body)
end)

# --- Timeout middleware returns 504 when handler is too slow
testing.test("nethttp/mw/timeout_504", fun(_: Null) do
	let r = nethttp.router()
	r.use(nethttp.mwTimeout(10)) # 10ms

	r.handle("GET", "/slow", fun(req: Request) -> Any? do
		sleep(50)
		return {ok: true}
	end)

	let tc = nethttp.testClient(r)
	let res = tc.call({method: "GET", path: "/slow"})
	testing.assertEq(504, res.status)
end)

# --- Response helpers: noContent + redirect
testing.test("nethttp/response/noContent_redirect", fun(_: Null) do
	let r = nethttp.router()
	r.handle("POST", "/done", fun(req: Request) -> Any? do
		nethttp.noContent(null)
	end)
	r.handle("GET", "/go", fun(req: Request) -> Any? do
		nethttp.redirect(302, "/there")
	end)

	let tc = nethttp.testClient(r)
	let n = tc.call({method: "POST", path: "/done"})
	testing.assertEq(204, n.status)
	testing.assertEq("", n.body)

	let rd = tc.call({method: "GET", path: "/go"})
	testing.assertEq(302, rd.status)
	testing.assertEq("/there", rd.headers["Location"])
end)

# --- bindQuery coercions (numbers & bools)
testing.test("nethttp/bindQuery/coercions", fun(_: Null) do
	let r = nethttp.router()
	let Q = type {n: Int, ok: Bool}

	r.handle("GET", "/coerce", fun(req: Request) -> Any? do
		let v = nethttp.bindQuery(req, Q)
		if v == null then
			return nethttp.raise(400, "<bad>")
		end
		return nethttp.json(200, v)
	end)

	let tc = nethttp.testClient(r)
	let res = tc.call({method: "GET", path: "/coerce", query: "n=42&ok=true"})
	testing.assertEq(200, res.status)
	testing.assertEq({n: 42, ok: true}, jsonParse(res.body))
end)

# --- Longest match wins over prefix
testing.test("nethttp/router/longest_match", fun(_: Null) do
	let r = nethttp.router()
	r.handle("GET", "/a/", fun(req: Request) -> Any? do
		nethttp.text(200, "prefix")
	end)
	r.handle("GET", "/a/b", fun(req: Request) -> Any? do
		nethttp.text(200, "exact")
	end)

	let tc = nethttp.testClient(r)
	let res = tc.call({method: "GET", path: "/a/b"})
	testing.assertEq(200, res.status)
	testing.assertEq("exact", res.body)
end)

# --- bindQuery: multi-value → array with light coercion
testing.test("nethttp/bindQuery/array_values", fun(_: Null) do
	let r = nethttp.router()
	let Q = type {tags: [Int]}
	r.handle("GET", "/q", fun(req: Request) -> Any? do
		let v = nethttp.bindQuery(req, Q)
		if v == null then
			return nethttp.raise(400, "<bad>")
		end
		return nethttp.json(200, v)
	end)
	let tc = nethttp.testClient(r)
	let res = tc.call({method: "GET", path: "/q", query: "tags=1&tags=2"})
	testing.assertEq(200, res.status)
	testing.assertEq({tags: [1, 2]}, jsonParse(res.body))
end)

# --- Method matching is case-insensitive
testing.test("nethttp/router/method_case_insensitive", fun(_: Null) do
	let r = nethttp.router()
	r.handle("GET", "/ok", fun(req: Request) -> Any? do
		nethttp.text(200, "ok")
	end)
	let tc = nethttp.testClient(r)
	let res = tc.call({method: "get", path: "/ok"}) # lower-case

	testing.assertEq(200, res.status)
	testing.assertEq("ok", res.body)
end)

# --- Value handler returning plain null → 500 (no annotation)
testing.test("nethttp/handler/null_defaults_500", fun(_: Null) do
	let r = nethttp.router()
	r.handle("GET", "/nil", fun(req: Request) -> Any? do
		return
	end)
	let tc = nethttp.testClient(r)
	let res = tc.call({method: "GET", path: "/nil"})
	testing.assertEq(500, res.status)
	testing.assertEq(true, len(split(res.body, "\"error\"")) > 1)
end)

# --- Request ID middleware also decorates the 404 leaf
testing.test("nethttp/mw/request_id_on_404_leaf", fun(_: Null) do
	let r = nethttp.router()
	r.use(nethttp.mwRequestID("X-Req"))
	let tc = nethttp.testClient(r)
	let res = tc.call({method: "GET", path: "/missing"})
	testing.assertEq(404, res.status)
	testing.assert(res.headers["X-Req"] != null, "missing X-Req on 404 leaf")
end)

# --- OpenAPI: stream handler is listed with kind="stream"
testing.test("nethttp/docs/openapi_stream_kind", fun(_: Null) do
	let r = nethttp.router()
	r.handle("GET", "/s", fun(req: Request, res: Responder) -> Null do
		res.status(200)
		res.end(null)
	end)
	let spec = nethttp.openapi(r)
	testing.assert(spec != null, "openapi should not be null")
	# find route entry
	let found = false
	for it in spec.routes do
		if it.path == "/s" and it.kind == "stream" then
			found = true
		end
	end
	testing.assert(found, "stream route missing or wrong kind")
end)

# --- path(req) is empty when no params present
testing.test("nethttp/path/no_params_empty", fun(_: Null) do
	let r = nethttp.router()
	r.handle("GET", "/p", fun(req: Request) -> Any? do
		return nethttp.json(200, nethttp.path(req))
	end)
	let tc = nethttp.testClient(r)
	let res = tc.call({method: "GET", path: "/p"})
	testing.assertEq(200, res.status)
	testing.assertEq({}, jsonParse(res.body))
end)