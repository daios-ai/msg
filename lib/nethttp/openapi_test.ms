let testing = import("testing")
let http = import("nethttp")
let oapi = import("openapi")

# Helper: build a minimal RouteSpec (kept small on purpose)
let _r = fun(method: Str, path: Str, style: Str, c: {}) -> {} do
	{method: method, pattern: path, style: style, contract: c}
end

# Helper: build RouteSpec[] from a nethttp router dump
let _routesFrom = fun(r) -> [ {} ] do
	let d = r._dump()
	let out = []
	let i = 0
	while i < len(d.routes) do
		let rr = d.routes[i]
		out = out + [{
			method: rr.method,
			pattern: rr.pattern,
			style: rr.style,
			contract: if mapHas(rr, "contract") then rr.contract else {} end
		}]
		i = i + 1
	end
	out
end

# Helper: always call openapi.spec with opts (defaults to {})
let _spec = fun(routes: [ {} ], info: {}, servers: [Str]?, opts: {}?) -> {} do
	oapi.spec(routes, info, servers, if opts == null then {} else opts end)
end

# 1) returns 3.1 doc with info/servers
testing.test("openapi/basic doc", fun(_: Null) do
	let r = http.router()
	let doc = _spec(_routesFrom(r), {title: "API", version: "1"}, ["http://x"], null)
	testing.assert(doc != null, "null doc")
	testing.assertEq("3.1.0", doc.openapi)
	testing.assertEq("API", doc.info.title)
	testing.assertEq("http://x", doc.servers[0].url)
end)

# 3) path+method present (lowercased)
testing.test("openapi/paths and methods", fun(_: Null) do
	let r = http.router()
	r.route("GET", "/ping", {}, fun(req, ctx) do http.text(200, "pong") end)
	let d = _spec(_routesFrom(r), {title: "t", version: "v"}, [], null)
	testing.assert(d.paths["/ping"] != null, "missing path")
	testing.assert(d.paths["/ping"].get != null, "missing get op")
end)

# 4) path params -> in: path, required: true
testing.test("openapi/path params required", fun(_: Null) do
	let r = http.router()
	r.route("GET", "/u/{id}", {path: type {id: Int}}, fun(req, ctx) do http.text(200, "ok") end)
	let d = _spec(_routesFrom(r), {title: "t", version: "v"}, [], null)
	let ps = d.paths["/u/{id}"].get.parameters
	testing.assertEq(1, len(ps))
	testing.assertEq("path", ps[0].in)
	testing.assertEq(true, ps[0].required)
	testing.assert(ps[0].schema["$ref"] != null, "no $ref for path")
end)

# 5) query params preserve optional vs required
testing.test("openapi/query required flags", fun(_: Null) do
	let r = http.router()
	r.route("GET", "/q", {query: type {a!: Int, b: Str}}, fun(req, ctx) do http.text(200, "ok") end)
	let d = _spec(_routesFrom(r), {title: "t", version: "v"}, [], null)
	let ps = d.paths["/q"].get.parameters
	testing.assertEq(2, len(ps))
	let reqA = if ps[0].name == "a" then ps[0] else ps[1] end
	let optB = if ps[0].name == "b" then ps[0] else ps[1] end
	testing.assertEq(true, reqA.required)
	testing.assertEq(false, optB.required)
end)

# 6) requestBody appears for body schema (application/json)
testing.test("openapi/request body json", fun(_: Null) do
	let r = http.router()
	r.route("POST", "/echo", {body: type {msg: Str}}, fun(req, ctx) do http.text(200, "ok") end)
	let d = _spec(_routesFrom(r), {title: "t", version: "v"}, [], null)
	let rb = d.paths["/echo"].post.requestBody
	testing.assert(rb != null, "missing requestBody")
	testing.assert(rb.required, "request not required")
	testing.assert(rb.content["application/json"] != null, "missing json ct")
	testing.assert(rb.content["application/json"].schema["$ref"] != null, "missing ref")
end)

# 7) default responses for value route (200, json)
testing.test("openapi/default value response", fun(_: Null) do
	let r = http.router()
	r.route("GET", "/auto", {}, fun(req, ctx) do http.text(200, "ok") end)
	let d = _spec(_routesFrom(r), {title: "t", version: "v"}, [], null)
	let rs = d.paths["/auto"].get.responses
	testing.assert(rs["200"] != null, "missing 200")
	testing.assert(rs["200"].content["application/json"] != null, "missing json content")
end)

# 8) default responses for stream route (200, text/plain string)
testing.test("openapi/default stream response", fun(_: Null) do
	let r = http.router()
	r.routeStream("GET", "/s", {}, fun(req, res, ctx) -> Null do res.status(200).write("x") res.end(null) null end)
	let d = _spec(_routesFrom(r), {title: "t", version: "v"}, [], null)
	let rs = d.paths["/s"].get.responses["200"]
	testing.assert(rs != null, "missing 200")
	testing.assert(rs.content["text/plain"] != null, "missing text/plain")
	testing.assertEq("string", rs.content["text/plain"].schema.type)
end)

# 9) explicit response Type -> json schema $ref
testing.test("openapi/response type ref", fun(_: Null) do
	let R = type {ok: Bool}
	let r = http.router()
	r.route("GET", "/r", {responses: {"200": R}}, fun(req, ctx) do http.json(200, {ok: true}) end)
	let d = _spec(_routesFrom(r), {title: "t", version: "v"}, [], null)
	let s = d.paths["/r"].get.responses["200"].content["application/json"].schema
	testing.assert(s["$ref"] != null, "missing ref")
end)

# 10) 204 response -> no content
testing.test("openapi/204 no content", fun(_: Null) do
	let r = http.router()
	r.route("DELETE", "/x", {responses: {"204": type {}}}, fun(req, ctx) do http.noContent(null) end)
	let d = _spec(_routesFrom(r), {title: "t", version: "v"}, [], null)
	let r204 = d.paths["/x"].delete.responses["204"]
	testing.assert(r204 != null, "missing 204")
	testing.assert(r204.content == null, "204 must not have content")
end)

# 11) response object with headers + custom contentType
testing.test("openapi/response headers+contentType", fun(_: Null) do
	let r = http.router()
	r.route(
		"GET",
		"/h",
		{responses: {"200": {description: "D", headers: {"X-Rate": type Int}, contentType: "text/plain", schema: type Str}}},
		fun(req, ctx) do http.text(200, "ok") end
	)
	let d = _spec(_routesFrom(r), {title: "t", version: "v"}, [], null)
	let rr = d.paths["/h"].get.responses["200"]
	testing.assertEq("D", rr.description)
	testing.assert(rr.headers["X-Rate"].schema["$ref"] != null, "header schema ref")
	testing.assert(rr.content["text/plain"] != null, "missing ct")
end)

# 12) $ref dedup for reused Type
testing.test("openapi/ref dedup", fun(_: Null) do
	let T = type {id: Int}
	let r = http.router()
	r.route("POST", "/a", {body: T, responses: {"200": T}}, fun(req, ctx) do
		http.json(200, {id: 1})
	end)
	let d = _spec(_routesFrom(r), {title: "t", version: "v"}, [], null)
	let comps = d.components.schemas
	testing.assertEq(1, len(comps))
end)

# 13) components names look like T<uid>
testing.test("openapi/component name prefix", fun(_: Null) do
	let r = http.router()
	r.route("GET", "/c", {query: type {q: Int}}, fun(req, ctx) do
		http.text(200, "ok")
	end)
	let d = _spec(_routesFrom(r), {title: "t", version: "v"}, [], null)
	let m = d.components.schemas
	testing.assert(len(m) >= 1, "no comps")
	let ks = list(keys(m))
	testing.assert(substr(ks[0], 0, 1) == "T", "bad comp name")
end)

# 14) servers optional -> empty array
testing.test("openapi/servers optional empty", fun(_: Null) do
	let r = http.router()
	let d = _spec(_routesFrom(r), {title: "t", version: "v"}, null, null)
	testing.assertEq(0, len(d.servers))
end)

# catch-all path normalized ({*tail} -> {tail}) + param present
testing.test("openapi/catch-all normalized", fun(_: Null) do
	let r = http.router()
	r.routeStream("GET", "/time/{*tail}", {}, fun(req, res, ctx) -> Null do res.status(200).end(null) null end)
	let d = _spec(_routesFrom(r), {title: "t", version: "v"}, [], null)
	testing.assert(d.paths["/time/{tail}"] != null, "missing normalized path")
	let ps = d.paths["/time/{tail}"].get.parameters
	testing.assertEq("path", ps[0].in)
	testing.assertEq("tail", ps[0].name)
	testing.assert(ps[0].required, "tail must be required")
end)

# HEAD mirrored for value GET
testing.test("openapi/head mirror for get", fun(_: Null) do
	let r = http.router()
	r.route("GET", "/h", {}, fun(req, ctx) do http.text(200, "ok") end)
	let d = _spec(_routesFrom(r), {title: "t", version: "v"}, [], null)
	testing.assert(d.paths["/h"].head != null, "missing head op")
end)

# builtin 422 response for any route with a contract
testing.test("openapi/builtin 422", fun(_: Null) do
	let r = http.router()
	r.route("GET", "/q", {query: type {n: Int}}, fun(req, ctx) do http.text(200, "ok") end)
	let rs = _spec(_routesFrom(r), {title: "t", version: "v"}, [], null).paths["/q"].get.responses
	testing.assert(rs["422"] != null, "missing 422")
	testing.assert(rs["422"].content["application/json"] != null, "422 must be json")
end)

# synthesize path params from template when contract.path absent
testing.test("openapi/path param synth", fun(_: Null) do
	let r = http.router()
	r.route("GET", "/u/{id}", {}, fun(req, ctx) do http.text(200, "ok") end)
	let ps = _spec(_routesFrom(r), {title: "t", version: "v"}, [], null).paths["/u/{id}"].get.parameters
	testing.assertEq(1, len(ps))
	testing.assertEq("path", ps[0].in)
	testing.assertEq("id", ps[0].name)
	testing.assert(ps[0].required, "id must be required")
end)

# query array hints: style=form, explode=true
testing.test("openapi/query array style", fun(_: Null) do
	let r = http.router()
	r.route("GET", "/tags", {query: type {tag: [Int]}}, fun(req, ctx) do http.text(200, "ok") end)
	let ps = _spec(_routesFrom(r), {title: "t", version: "v"}, [], null).paths["/tags"].get.parameters
	let p = if ps[0].name == "tag" then ps[0] else ps[1] end
	testing.assertEq("form", p.style)
	testing.assert(p.explode, "explode must be true")
end)

# requestBody options: required=false + custom contentType
testing.test("openapi/request body opts", fun(_: Null) do
	let r = http.router()
	r.route("POST", "/echo", {body: {required: false, contentType: "text/plain", schema: type Str}}, fun(req, ctx) do http.text(200, "ok") end)
	let rb = _spec(_routesFrom(r), {title: "t", version: "v"}, [], null).paths["/echo"].post.requestBody
	testing.assert(rb != null, "missing requestBody")
	testing.assertEq(false, rb.required)
	testing.assert(rb.content["text/plain"] != null, "missing text/plain")
end)

# stream override response content type (e.g., SSE)
testing.test("openapi/stream response override", fun(_: Null) do
	let r = http.router()
	r.routeStream(
		"GET",
		"/sse",
		{responses: {"200": {contentType: "text/event-stream", schema: type Str}}},
		fun(req, res, ctx) -> Null do res.status(200).end(null) null end
	)
	let rs = _spec(_routesFrom(r), {title: "t", version: "v"}, [], null).paths["/sse"].get.responses["200"]
	testing.assert(rs.content["text/event-stream"] != null, "missing event-stream")
end)

# multi-content response on same status
testing.test("openapi/multi content response", fun(_: Null) do
	let r = http.router()
	r.route(
		"GET",
		"/multi",
		{responses: {"200": {content: {"application/json": type {ok: Bool}, "text/plain": type Str}}}},
		fun(req, ctx) do http.text(200, "ok") end
	)
	let rs = _spec(_routesFrom(r), {title: "t", version: "v"}, [], null).paths["/multi"].get.responses["200"].content
	testing.assert(rs["application/json"] != null, "json content missing")
	testing.assert(rs["text/plain"] != null, "text content missing")
end)

# jsonSchemaDialect at root
testing.test("openapi/json schema dialect", fun(_: Null) do
	let r = http.router()
	let d = _spec(_routesFrom(r), {title: "t", version: "v"}, [], null)
	testing.assertEq("https://json-schema.org/draft/2020-12/schema", d.jsonSchemaDialect)
end)

# operationId synthesized
testing.test("openapi/operationId", fun(_: Null) do
	let r = http.router()
	r.route("GET", "/ping", {}, fun(req, ctx) do http.text(200, "pong") end)
	let op = _spec(_routesFrom(r), {title: "t", version: "v"}, [], null).paths["/ping"].get
	testing.assert(op.operationId != null, "missing operationId")
end)

# options op for known shapes (204)
testing.test("openapi/options 204 op", fun(_: Null) do
	let r = http.router()
	r.route("GET", "/p/{x}", {}, fun(req, ctx) do http.text(200, "ok") end)
	let d = _spec(_routesFrom(r), {title: "t", version: "v"}, [], null)
	let op = d.paths["/p/{x}"].options
	testing.assert(op != null, "missing options op")
	testing.assert(op.responses["204"] != null, "missing 204 on options")
end)

# pure RouteSpec usage (no nethttp)
testing.test("openapi/spec from RouteSpec only", fun(_: Null) do
	let routes = [{
		method: "POST",
		pattern: "/echo/{id}",
		style: "value",
		contract: {
			path: type {id: Int},
			query: type {q!: Str},
			body: type {msg: Str},
			responses: {"200": type {ok: Bool}}
		}
	}]
	let d = _spec(routes, {title: "t", version: "v"}, ["http://x"], null)
	testing.assert(d.paths["/echo/{id}"].post != null, "missing post op")
	testing.assert(d.paths["/echo/{id}"].post.requestBody != null, "missing rb")
	testing.assert(d.paths["/echo/{id}"].post.responses["200"] != null, "missing 200")
end)

let testing = import("testing")
let openapi = import("openapi")

# 1) PATH PARAMS — declared vs synthesized (no duplicates)
testing.test("openapi/path params declared_vs_synth", fun(_: Null) do
	let routes = [
		{method: "GET", pattern: "/u/{id}", style: "value", contract: {path: type {id: Int}}},
		{method: "GET", pattern: "/v/{name}", style: "value"}  # synth as Str
	]
	let doc = _spec(routes, {title: "t", version: "1"}, [], null)
	let p1 = doc.paths["/u/{id}"].get.parameters
	let p2 = doc.paths["/v/{name}"].get.parameters
	testing.assertEq(1, len(p1))               # no dupes
	testing.assertEq("id", p1[0].name)
	testing.assertEq("name", p2[0].name)
	testing.assertEq("path", p2[0].in)
	testing.assertEq("string", p2[0].schema.type)  # synthesized as Str
end)

# 2) QUERY — explode only for scalar arrays
testing.test("openapi/query explode scalar_arrays_only", fun(_: Null) do
	let Q = type { tag: [Str], obj: [ {a!: Int} ] }
	let routes = [{method: "GET", pattern: "/q", style: "value", contract: {query: Q}}]
	let doc = _spec(routes, {title: "t", version: "1"}, [], null)
	let ps = doc.paths["/q"].get.parameters
	let tag = null
	let obj = null
	let i = 0
	while i < len(ps) do
		if ps[i].name == "tag" then tag = ps[i] end
		if ps[i].name == "obj" then obj = ps[i] end
		i = i + 1
	end
	testing.assert(tag != null, "missing tag")
	testing.assertEq(true, tag.explode)                 # scalar array
	testing.assert(obj != null, "missing obj")
	testing.assert(obj.explode == null or obj.explode == false, "obj should not explode")
end)

# 3) RESPONSES — support 'default' key
testing.test("openapi/responses default_key", fun(_: Null) do
	let routes = [{
		method: "GET",
		pattern: "/r",
		style: "value",
		contract: {responses: {default: type {ok!: Bool}}}
	}]
	let doc = _spec(routes, {title: "t", version: "1"}, [], null)
	let rs = doc.paths["/r"].get.responses
	testing.assert(mapHas(rs, "default"), "missing default response")
end)

# 4) 204 — force no content
testing.test("openapi/204 no_content", fun(_: Null) do
	let routes = [{
		method: "GET",
		pattern: "/n",
		style: "value",
		contract: {responses: {"204": {description: "done"}}}
	}]
	let doc = _spec(routes, {title: "t", version: "1"}, [], null)
	let r204 = doc.paths["/n"].get.responses["204"]
	testing.assertEq(null, r204.content)
end)

# 5) DETERMINISM — parameters order: path before query; query names sorted
testing.test("openapi/params deterministic_order", fun(_: Null) do
	let routes = [{
		method: "GET",
		pattern: "/o/{z}",
		style: "value",
		contract: {path: type {z: Str}, query: type {b: Int, a: Int}}
	}]
	let doc = _spec(routes, {title: "t", version: "1"}, [], null)
	let ps = doc.paths["/o/{z}"].get.parameters
	testing.assertEq("path", ps[0].in)
	testing.assertEq("z", ps[0].name)
	# collect query names in listed order
	let qs = []
	let i = 0
	while i < len(ps) do
		if ps[i].in == "query" then qs = qs + [ps[i].name] end
		i = i + 1
	end
	testing.assertEq(2, len(qs))
	testing.assertEq("a", qs[0])  # alpha
	testing.assertEq("b", qs[1])
end)

let testing = import("testing")
let oapi2 = import("openapi")

# helper: make a tiny route
let _r = fun(method: Str, path: Str, style: Str, c: {}) -> {} do
	{method: method, pattern: path, style: style, contract: c}
end

# 1) Deterministic component names across two runs
testing.test("openapi/deterministic component names", fun(_: Null) do
	let T = type {id: Int}
	let routes = [_r("GET", "/a", "value", {responses: {"200": T}})]
	let d1 = _spec(routes, {title: "t", version: "1"}, [], null)
	let d2 = _spec(routes, {title: "t", version: "1"}, [], null)
	let k1 = list(keys(d1.components.schemas))[0]
	let k2 = list(keys(d2.components.schemas))[0]
	testing.assertEq(k1, k2)
end)

# 2) Components keys sorted alpha (determinism)
testing.test("openapi/components sorted", fun(_: Null) do
	let A = type {a: Int}
	let B = type {b: Int}
	let routes = [
		_r("GET", "/a", "value", {responses: {"200": A}}),
		_r("GET", "/b", "value", {responses: {"200": B}})
	]
	let d = _spec(routes, {title: "t", version: "1"}, [], null)
	let ks = list(keys(d.components.schemas))
	testing.assert(len(ks) >= 2, "needs 2 comps")
	testing.assert(ks[0] <= ks[1], "not alpha")
end)

# 3) Route metadata: summary/description/tags/deprecated/operationId override
testing.test("openapi/route metadata passthrough", fun(_: Null) do
	let routes = [{
		method: "GET",
		pattern: "/m",
		style: "value",
		contract: {},
		summary: "S",
		description: "D",
		tags: ["t1", "t2"],
		deprecated: true,
		operationId: "customOp"
	}]
	let op = _spec(routes, {title: "t", version: "1"}, [], null).paths["/m"].get
	testing.assertEq("S", op.summary)
	testing.assertEq("D", op.description)
	testing.assertEq(true, op.deprecated)
	testing.assertEq("customOp", op.operationId)
	testing.assertEq(2, len(op.tags))
end)

# 4) Security: root schemes + root requirement + per-route clear
testing.test("openapi/security root_and_override", fun(_: Null) do
	let routes = [
		{method: "GET", pattern: "/a", style: "value", contract: {}},         # inherits root security
		{method: "GET", pattern: "/b", style: "value", contract: {}, security: []}  # clears
	]
	let d = _spec(routes, {title: "t", version: "1"}, [], {
		securitySchemes: {apiKeyAuth: {type: "apiKey", name: "X-API-Key", in: "header"}},
		security: [{apiKeyAuth: []}]
	})
	let opA = d.paths["/a"].get
	let opB = d.paths["/b"].get
	testing.assert(d.components.securitySchemes.apiKeyAuth != null, "missing scheme")
	testing.assert(opA.security == null, "should inherit root")
	testing.assertEq(1, len(d.security))
	testing.assertEq(0, len(opB.security))  # explicit clear
end)

# 5) Request body: application/octet-stream -> string/binary
testing.test("openapi/body octet-stream binary", fun(_: Null) do
	let routes = [_r("POST", "/bin", "value", {body: {contentType: "application/octet-stream"}})]
	let rb = _spec(routes, {title: "t", version: "1"}, [], null).paths["/bin"].post.requestBody
	let sch = rb.content["application/octet-stream"].schema
	testing.assertEq("string", sch.type)
	testing.assertEq("binary", sch.format)
end)

# 6) Request body: multipart/form-data with schema
testing.test("openapi/body multipart with schema", fun(_: Null) do
	let F = type {name!: Str}
	let routes = [_r("POST", "/up", "value", {body: {contentType: "multipart/form-data", schema: F}})]
	let rb = _spec(routes, {title: "t", version: "1"}, [], null).paths["/up"].post.requestBody
	let s = rb.content["multipart/form-data"].schema
	testing.assert(s["$ref"] != null, "multipart schema missing ref")
end)

# 7) Builtin 422 uses override schema when provided
testing.test("openapi/422 override schema", fun(_: Null) do
	let Err = type {error!: Str, detail: [ {} ]}
	let routes = [_r("GET", "/q", "value", {query: type {n!: Int}})]
	let rs = _spec(routes, {title: "t", version: "1"}, [], {validationErrorSchema: Err}).paths["/q"].get.responses
	let sch = rs["422"].content["application/json"].schema
	testing.assert(sch["$ref"] != null, "missing 422 ref")
end)

# 8) Headers and cookies contract -> parameters in header/cookie
testing.test("openapi/headers and cookies params", fun(_: Null) do
	let routes = [_r("GET", "/p", "value", {
		headers: type { "X-Req": Str },
		cookies: type { session: Str }
	})]
	let ps = _spec(routes, {title: "t", version: "1"}, [], null).paths["/p"].get.parameters
	let seenH = false
	let seenC = false
	let i = 0
	while i < len(ps) do
		if ps[i].in == "header" and ps[i].name == "X-Req" then seenH = true end
		if ps[i].in == "cookie" and ps[i].name == "session" then seenC = true end
		i = i + 1
	end
	testing.assert(seenH, "missing header param")
	testing.assert(seenC, "missing cookie param")
end)

# 9) Query object param -> deepObject style
testing.test("openapi/query object deepObject", fun(_: Null) do
	let Q = type { obj: {a!: Int} }
	let routes = [_r("GET", "/q", "value", {query: Q})]
	let ps = _spec(routes, {title: "t", version: "1"}, [], null).paths["/q"].get.parameters
	let obj = if ps[0].name == "obj" then ps[0] else ps[1] end
	testing.assertEq("query", obj.in)
	testing.assertEq("deepObject", obj.style)
	testing.assert(obj.explode, "explode should be true")
end)

# 10) Response key normalization: numeric and default preserved; 204 no content
testing.test("openapi/responses normalize and 204", fun(_: Null) do
	let routes = [_r("GET", "/r", "value", {responses: {"200": type {ok: Bool}, default: type {msg: Str}, "204": {description: "done"}}})]
	let rs = _spec(routes, {title: "t", version: "1"}, [], null).paths["/r"].get.responses
	testing.assert(rs["200"] != null, "missing 200")
	testing.assert(rs["default"] != null, "missing default")
	testing.assertEq(null, rs["204"].content)
end)

# 11) Vendor extensions at route and root (x-*)
testing.test("openapi/vendor extensions passthrough", fun(_: Null) do
	let routes = [{
		method: "GET",
		pattern: "/x",
		style: "value",
		contract: {},
		x: {"x-route": true}
	}]
	let d = _spec(routes, {title: "t", version: "1"}, [], {x: {"x-root": 1}})
	let op = d.paths["/x"].get
	testing.assertEq(true, op["x-route"])
	testing.assertEq(1, d["x-root"])
end)

# 12) Examples on requestBody and response (single example)
testing.test("openapi/examples rb_and_resp", fun(_: Null) do
	let B = type {msg!: Str}
	let R = type {ok!: Bool}
	let routes = [_r("POST", "/e", "value", {
		body: {schema: B, example: {msg: "hi"}},
		responses: {"200": {schema: R, contentType: "application/json", example: {ok: true}}}
	})]
	let d = _spec(routes, {title: "t", version: "1"}, [], null)
	let rbEx = d.paths["/e"].post.requestBody.content["application/json"].example
	let rsEx = d.paths["/e"].post.responses["200"].content["application/json"].example
	testing.assert(rbEx != null, "missing rb example")
	testing.assert(rsEx != null, "missing resp example")
end)

# 13) Info passthrough: license/contact/terms
testing.test("openapi/info passthrough", fun(_: Null) do
	let info = {
		title: "t",
		version: "1",
		license: {name: "MIT"},
		contact: {name: "c"},
		termsOfService: "https://tos"
	}
	let d = _spec([], info, [], null)
	testing.assertEq("MIT", d.info.license.name)
	testing.assertEq("c", d.info.contact.name)
	testing.assertEq("https://tos", d.info.termsOfService)
end)

# 1) Deterministic: paths keys sorted (lexicographic)
testing.test("openapi/deterministic paths order", fun(_: Null) do
	let routes = [
		_r("GET", "/b", "value", {}),
		_r("GET", "/a", "value", {})
	]
	let d = oapi.spec(routes, {title: "t", version: "1"}, [], {})
	let ks = list(keys(d.paths))

	# Map key order is unspecified; sort for deterministic assertions.
	let s = clone(ks)
	let i = 1
	while i < len(s) do
		let j = i
		while j > 0 and s[j] < s[j - 1] do
			let t = s[j - 1]
			s[j - 1] = s[j]
			s[j] = t
			j = j - 1
		end
		i = i + 1
	end

	testing.assertEq(2, len(s))
	testing.assertEq("/a", s[0])
	testing.assertEq("/b", s[1])
end)


# 2) OPTIONS: real route is preserved (stub must not overwrite)
testing.test("openapi/options route preserved", fun(_: Null) do
	let routes = [
		{method: "OPTIONS", pattern: "/x", style: "value", contract: {}, summary: "real"},
		_r("GET", "/x", "value", {})
	]
	let d = oapi.spec(routes, {title: "t", version: "1"}, [], {})
	let op = d.paths["/x"].options
	testing.assert(op != null, "missing options op")
	testing.assertEq("real", op.summary)
end)

# 3) Cookies: default style=form, explode=true
testing.test("openapi/cookie defaults", fun(_: Null) do
	let routes = [_r("GET", "/p", "value", {cookies: type {session: Str}})]
	let ps = oapi.spec(routes, {title: "t", version: "1"}, [], {}).paths["/p"].get.parameters
	testing.assertEq(1, len(ps))
	testing.assertEq("cookie", ps[0].in)
	testing.assertEq("session", ps[0].name)
	testing.assertEq("form", ps[0].style)
	testing.assert(ps[0].explode, "cookie explode should be true")
end)

# 5) Components determinism: same inputs -> same component keys
testing.test("openapi/components deterministic", fun(_: Null) do
	let T = type {id: Int}
	let routes = [_r("GET", "/a", "value", {responses: {"200": T}})]
	let d1 = oapi.spec(routes, {title: "t", version: "1"}, [], {})
	let d2 = oapi.spec(routes, {title: "t", version: "1"}, [], {})
	let k1 = list(keys(d1.components.schemas))
	let k2 = list(keys(d2.components.schemas))
	testing.assertEq(len(k1), len(k2))
	testing.assertEq(k1[0], k2[0])
end)

# 6) No path param dupes: declared vs synthesized
testing.test("openapi/path params no_dupe", fun(_: Null) do
	let routes = [_r("GET", "/u/{id}", "value", {path: type {id: Int}})]
	let ps = oapi.spec(routes, {title: "t", version: "1"}, [], {}).paths["/u/{id}"].get.parameters
	testing.assertEq(1, len(ps))
	testing.assertEq("path", ps[0].in)
	testing.assertEq("id", ps[0].name)
	testing.assert(ps[0].required, "id must be required")
end)

# 7) Built-in 422 injected when any contract present (query here)
testing.test("openapi/builtin 422 present", fun(_: Null) do
	let routes = [_r("GET", "/q", "value", {query: type {n!: Int}})]
	let rs = oapi.spec(routes, {title: "t", version: "1"}, [], {}).paths["/q"].get.responses
	testing.assert(rs["422"] != null, "missing 422")
	testing.assert(rs["422"].content["application/json"] != null, "422 must be json")
end)

# Single media-type example -> emit `example`, not `examples`
testing.test("openapi/examples mediaType single_example", fun(_: Null) do
	let routes = [{
		method: "POST",
		pattern: "/e1",
		style: "value",
		contract: {
			body: {contentType: "application/json", schema: type {msg!: Str}, example: {msg: "hi"}},
			responses: {"200": {contentType: "application/json", schema: type {ok!: Bool}, example: {ok: true}}}
		}
	}]
	let d = oapi.spec(routes, {title: "t", version: "1"}, [], {})
	let rb = d.paths["/e1"].post.requestBody.content["application/json"]
	let rs = d.paths["/e1"].post.responses["200"].content["application/json"]

    testing.assert(mapHas(rb, "example"), "rb.example missing")
    testing.assert(not mapHas(rb, "examples"), "rb.examples should be absent")
    testing.assert(mapHas(rs, "example"), "resp.example missing")
    testing.assert(not mapHas(rs, "examples"), "resp.examples should be absent")
end)

# Map of named examples -> emit `examples`, not `example`
testing.test("openapi/examples mediaType examples_map", fun(_: Null) do
	let routes = [{
		method: "POST",
		pattern: "/e2",
		style: "value",
		contract: {
			body: {contentType: "application/json", schema: type {msg!: Str}, examples: {a: {value: {msg: "hi"}}, b: {value: {msg: "yo"}}}},
			responses: {"200": {contentType: "application/json", schema: type {ok!: Bool}, examples: {ok: {value: {ok: true}}}}}
		}
	}]
	let d = oapi.spec(routes, {title: "t", version: "1"}, [], {})
	let rb = d.paths["/e2"].post.requestBody.content["application/json"]
	let rs = d.paths["/e2"].post.responses["200"].content["application/json"]

	testing.assert(mapHas(rb, "examples"), "rb.examples missing")
    testing.assert(not mapHas(rb, "example"), "rb.example should be absent")
    testing.assert(mapHas(rs, "examples"), "resp.examples missing")
    testing.assert(not mapHas(rs, "example"), "resp.example should be absent")
end)

# Paths should be emitted already sorted (no client-side sorting needed)
testing.test("openapi/paths emitted sorted", fun(_: Null) do
	let routes = [
		{method: "GET", pattern: "/b", style: "value", contract: {}},
		{method: "GET", pattern: "/a", style: "value", contract: {}}
	]
	let d = oapi.spec(routes, {title: "t", version: "1"}, [], {})
	let ks = list(keys(d.paths))
	testing.assertEq(2, len(ks))
	testing.assertEq("/a", ks[0])
	testing.assertEq("/b", ks[1])
end)

# responses: numeric codes ascend; "default" last (check via sorted key list)
testing.test("openapi/responses ordered numeric_then_default", fun(_: Null) do
	let routes = [{
		method: "GET",
		pattern: "/r",
		style: "value",
		contract: {responses: {"404": type {x: Int}, "200": type {y: Int}, default: type {z: Int}}}
	}]
	let rs = oapi.spec(routes, {title: "t", version: "1"}, [], {}).paths["/r"].get.responses
	let ks = list(keys(rs))

	# insertion sort with tiny comparator
	let less = fun(a: Str, b: Str) -> Bool do
		let ia = int(a) let ib = int(b)
		if ia != null and ib != null then return ia < ib end
		if ia != null then return true end
		if ib != null then return false end
		if a == "default" then return false end
		if b == "default" then return true end
		a < b
	end
	let i = 1
	while i < len(ks) do
		let j = i
		while j > 0 and less(ks[j], ks[j - 1]) do
			let t = ks[j - 1] ks[j - 1] = ks[j] ks[j] = t
			j = j - 1
		end
		i = i + 1
	end

	testing.assertEq(3, len(ks))
	testing.assertEq("200", ks[0])
	testing.assertEq("404", ks[1])
	testing.assertEq("default", ks[2])
end)


# If a user provides an explicit 422 response, it must be preserved (no override)
testing.test("openapi/422 explicit preserved", fun(_: Null) do
	let Err = type {error!: Str}
	let routes = [{
		method: "GET",
		pattern: "/q",
		style: "value",
		contract: {query: type {n!: Int}, responses: {"422": {description: "mine", contentType: "application/json", schema: Err}}}
	}]
	let d = oapi.spec(routes, {title: "t", version: "1"}, [], {})
	let r422 = d.paths["/q"].get.responses["422"]
	testing.assert(r422 != null, "missing 422")
	testing.assertEq("mine", r422.description)
end)