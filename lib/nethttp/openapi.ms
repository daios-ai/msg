# openapi — build an OpenAPI 3.1 document from RouteSpec[]
#
# This module converts a router’s structured route metadata into an
# OpenAPI 3.1 document. It does not inspect handlers; it only uses the
# provided route specifications and contracts.

# -------------------------------------------------
# Public types (data-only)
# -------------------------------------------------

# RouteContract — optional bind/validation contract per route.
# Fields are Types or open objects as noted. Missing fields mean “no constraint”.
let RouteContract = type {
	path: Type?,
	query: Type?,
	headers: Type?,
	cookies: Type?,
	body: Any?,           # Router accepts Type or object; normalized here.
	responses: {}?        # code -> Any (Type or object)
}

# RouteSpec — a single route’s public metadata used to generate OpenAPI.
let RouteSpec = type {
	method!: Str,
	pattern!: Str,
	style!: Enum["value", "stream"],
	contract: RouteContract?,
	summary: Str?,
	description: Str?,
	tags: [Str]?,
	deprecated: Bool?,
	operationId: Str?,
	security: [ {} ]?,
	x: {}?
}

# Info — top-level OpenAPI info block.
let Info = type {
	title!: Str,
	version!: Str,
	description: Str?
}

# SpecOpts — optional module-wide OpenAPI options.
let SpecOpts = type {
	validationErrorSchema: Type?,
	securitySchemes: {}?,
	security: [ {} ]?,
	x: {}?
}

# -------------------------------------------------
# Internal helpers
# -------------------------------------------------

let _segs = fun(p: Str) -> [Str] do
	let s = if len(p) == 0 or substr(p, 0, 1) != "/" then "/" + p else p end
	let xs = split(s, "/")
	let out = []
	let i = 0
	while i < len(xs) do
		let g = xs[i]
		if len(g) > 0 and g != "." then
			out = out + [g]
		end
		i = i + 1
	end
	out
end

let _norm = fun(p: Str) -> Str do
	let xs = _segs(p)
	let out = []
	let i = 0
	while i < len(xs) do
		let g = xs[i]
		if len(g) >= 3 and substr(g, 0, 2) == "{*" and substr(g, len(g) - 1, len(g)) == "}" then
			out = out + ["{" + substr(g, 2, len(g))]
		else
			out = out + [g]
		end
		i = i + 1
	end
	"/" + join(out, "/")
end

let _paramNames = fun(p: Str) -> [Str] do
	let xs = _segs(p)
	let out = []
	let i = 0
	while i < len(xs) do
		let g = xs[i]
		if len(g) >= 2 and substr(g, 0, 1) == "{" and substr(g, len(g) - 1, len(g)) == "}" then
			let n = substr(g, 1, len(g) - 1)
			out = out + [if len(n) > 0 and substr(n, 0, 1) == "*" then substr(n, 1, len(n)) else n end]
		end
		i = i + 1
	end
	out
end

let _mapFields = fun(T: Type) -> [ {name!: Str, req!: Bool, T!: Type} ]? do
	let rt = reflect(T)
	if rt == null or rt[1][0] != "map" then
		return null
	end
	let es = slice(rt[1], 1, len(rt[1]))
	let out = []
	let i = 0
	while i < len(es) do
		let e = es[i]
		out = out + [{name: e[1][1], req: e[0] == "pair!", T: reify(["type", e[2]])}]
		i = i + 1
	end
	out
end

let _ref = fun(reg: {}, comps: {}, T: Type) -> {} do
	let k = str(uid(reflect(T)))
	if mapHas(reg, k) then
		return {"$ref": "#/components/schemas/" + reg[k]}
	end
	let name = "T" + replace("-", "0", k)
	reg[k] = name
	let js = typeToJSONSchema(T)
	comps[name] = if js == null then {} else js end
	{"$ref": "#/components/schemas/" + name}
end

let _sortParams = fun(ps: [ {} ]) -> [ {} ] do
	let rank = fun(where: Str) -> Int do
		if where == "path" then 0 elif where == "query" then 1 elif where == "header" then 2 elif where == "cookie" then 3 else 9 end
	end
	sort(clone(ps), fun(a: Any) -> Any -> Int do
		fun(b: Any) -> Int do
			let ra = rank(a.in)
			let rb = rank(b.in)
			if ra != rb then
				if ra < rb then -1 else 1 end
			else
				if a.name < b.name then -1 elif a.name > b.name then 1 else 0 end
			end
		end
	end)
end

let _sortKeys = fun(m: {}) -> {} do
	let ks = sort(list(keys(m)), fun(a: Any) -> Any -> Int do
		fun(b: Any) -> Int do
			if a < b then -1 elif a > b then 1 else 0 end
		end
	end)
	let out = {}
	let i = 0
	while i < len(ks) do
		let k = ks[i]
		out[k] = m[k]
		i = i + 1
	end
	out
end

let _addParams = fun(ps: [ {} ], where: Str, T: Type, reg: {}, comps: {}) -> [ {} ] do
	let fs = _mapFields(T)
	if fs == null then
		return ps
	end
	let out = ps
	let i = 0
	while i < len(fs) do
		let f = fs[i]
		let p = {
			name: f.name,
			in: where,
			required: if where == "path" then true else f.req end,
			schema: _ref(reg, comps, f.T)
		}
		if where == "cookie" then
			p.style = "form"
			p.explode = true
		elif where == "query" then
			let rt = reflect(f.T)
			if rt != null and rt[1][0] == "array" then
				let U = reify(["type", rt[1][1]])
				let ur = reflect(U)
				if ur != null and ur[1][0] == "id" then
					let n = ur[1][1]
					if n == "Str" or n == "Int" or n == "Num" or n == "Bool" then
						p.style = "form"
						p.explode = true
					else
						# Non-scalar array → do not explode (make key present to avoid panic)
						p.explode = false
					end
				else
					# Unknown/compound element type → do not explode
					p.explode = false
				end
			elif rt != null and rt[1][0] == "map" then
				p.style = "deepObject"
				p.explode = true
			end
		end
		out = out + [p]
		i = i + 1
	end
	out
end

let _synthPath = fun(ps: [ {} ], pat: Str) -> [ {} ] do
	let ns = _paramNames(pat)
	let out = ps
	let i = 0
	while i < len(ns) do
		out = out + [{name: ns[i], in: "path", required: true, schema: {type: "string"}}]
		i = i + 1
	end
	out
end

# Build the OpenAPI requestBody entry from a declared body definition.
let _reqBody = fun(def: Any, reg: {}, comps: {}) -> {} do
	if isType(def, type Type) then
		return {required: true, content: {"application/json": {schema: _ref(reg, comps, def)}}}
	end
	let req = if mapHas(def, "required") and def.required != null then bool(def.required) else true end
	let ct = if mapHas(def, "contentType") and def.contentType != null then def.contentType else "application/json" end
	if mapHas(def, "content") and def.content != null then
		let m = {}
		for [k, v] in def.content do
			m[k] = {schema: _ref(reg, comps, v)}
		end
		return {required: req, content: m}
	end
	let sc =
		if mapHas(def, "schema") and def.schema != null then _ref(reg, comps, def.schema)
		elif ct == "application/octet-stream" then {type: "string", format: "binary"}
		else {} end
	let entry = {schema: sc}
	if mapHas(def, "examples") and def.examples != null then
		entry.examples = def.examples
	elif mapHas(def, "example") and def.example != null then
		entry.example = def.example
	end
	let m2 = {}
	m2[ct] = entry
	{required: req, content: m2}
end

# Build one OpenAPI response entry for a status code and value.
let _resp = fun(code: Str, v: Any, reg: {}, comps: {}) -> {} do
	if code == "204" then
		return {description: "No Content", content: null}
	end
	if isType(v, type Type) then
		return {description: "OK", content: {"application/json": {schema: _ref(reg, comps, v)}}}
	end
	if not isType(v, type {}) then
		return {description: "OK"}
	end
	let d = if mapHas(v, "description") and v.description != null then v.description else "OK" end
	let out = {description: d}
	if mapHas(v, "headers") and v.headers != null then
		let hs = {}
		for [hk, hv] in v.headers do
			hs[hk] = {schema: _ref(reg, comps, hv)}
		end
		out.headers = hs
	end
	if mapHas(v, "content") and v.content != null then
		let ct = {}
		for [ck, cv] in v.content do
			ct[ck] = {schema: _ref(reg, comps, cv)}
		end
		out.content = ct
		return out
	end
	if mapHas(v, "contentType") and v.contentType != null then
		let sc = if mapHas(v, "schema") and v.schema != null then _ref(reg, comps, v.schema) else {} end
		let entry = {schema: sc}
		if mapHas(v, "examples") and v.examples != null then
			entry.examples = v.examples
		elif mapHas(v, "example") and v.example != null then
			entry.example = v.example
		end
		let ct2 = {}
		ct2[v.contentType] = entry
		out.content = ct2
	end
	out
end

let _defRes = fun(style: Str) -> {} do
	if style == "stream" then
		return {"200": {description: "OK", content: {"text/plain": {schema: {type: "string"}}}}}
	end
	{"200": {description: "OK", content: {"application/json": {schema: {}}}}}
end

let _opId = fun(m: Str, p: Str) -> Str do
	let s = toLower(m) + "_" + join(_segs(p), "_")
	let out = ""
	let i = 0
	while i < len(s) do
		let ch = substr(s, i, i + 1)
		let ok = (ch >= "a" and ch <= "z") or (ch >= "A" and ch <= "Z") or (ch >= "0" and ch <= "9") or ch == "_"
		out = out + if ch == "{" or ch == "}" or ch == "*" or not ok then "_" else ch end
		i = i + 1
	end
	out
end

# -------------------------------------------------
# Public API
# -------------------------------------------------

# spec — Build an OpenAPI 3.1 document for a set of routes.
#
# Args:
# 	routes: [RouteSpec] — Collected route specs (value/stream).
# 	info: Info — Top-level API info (title/version/description).
# 	servers: [Str]? — Optional server URLs (e.g., "https://api.example.com").
# 	opts: SpecOpts? — Optional OpenAPI generation options:
# 		- validationErrorSchema?: Type — schema for 422 responses.
# 		- securitySchemes?: {} — OpenAPI components.securitySchemes.
# 		- security?: [ {} ] — Global security requirement objects.
# 		- x?: {} — Vendor extensions added to top-level doc.
# Return:
# 	{} — Fully formed OpenAPI 3.1 document.
let spec = fun(routes: [RouteSpec], info: Info, servers: [Str]?, opts: SpecOpts?) -> {} do
	let paths = {}
	let comps = {}
	let reg = {}
	let o = if opts == null then {} else opts end
	let i = 0
	while i < len(routes) do
		let r = routes[i]
		let pat = _norm(r.pattern)
		if not mapHas(paths, pat) then
			paths[pat] = {}
		end
		let c = if mapHas(r, "contract") and r.contract != null then r.contract else {} end

		let ps = []
		let hasC = false
		if mapHas(c, "path") and c.path != null then
			ps = _addParams(ps, "path", c.path, reg, comps)
			hasC = true
		end
		if mapHas(c, "query") and c.query != null then
			ps = _addParams(ps, "query", c.query, reg, comps)
			hasC = true
		end
		if mapHas(c, "headers") and c.headers != null then
			ps = _addParams(ps, "header", c.headers, reg, comps)
			hasC = true
		end
		if mapHas(c, "cookies") and c.cookies != null then
			ps = _addParams(ps, "cookie", c.cookies, reg, comps)
			hasC = true
		end
		if not mapHas(c, "path") or c.path == null then
			ps = _synthPath(ps, pat)
		end
		ps = _sortParams(ps)

		let rb = null
		if mapHas(c, "body") and c.body != null then
			rb = _reqBody(c.body, reg, comps)
			hasC = true
		end

		let rs = if mapHas(c, "responses") and c.responses != null then {} else _defRes(r.style) end
		if mapHas(c, "responses") and c.responses != null then
			for [k, v] in c.responses do
				rs[str(k)] = _resp(str(k), v, reg, comps)
			end
		end
		if hasC and not mapHas(rs, "422") then
			let sch =
				if mapHas(o, "validationErrorSchema") and o.validationErrorSchema != null then _ref(reg, comps, o.validationErrorSchema)
				else {type: "object"} end
			rs["422"] = {description: "Validation Error", content: {"application/json": {schema: sch}}}
		end

		let op = {parameters: ps, responses: rs, operationId: _opId(r.method, pat)}
		if mapHas(r, "security") and r.security != null then
			# Route explicitly specifies security (including [] to clear).
			op.security = r.security
		elif mapHas(o, "security") and o.security != null then
			# Route inherits root security; expose explicit null so clients/tests can read it safely.
			op.security = null
		end
		if mapHas(r, "summary") and r.summary != null then op.summary = r.summary end
		if mapHas(r, "description") and r.description != null then op.description = r.description end
		if mapHas(r, "tags") and r.tags != null then op.tags = r.tags end
		if mapHas(r, "deprecated") and r.deprecated != null then op.deprecated = bool(r.deprecated) end
		if mapHas(r, "operationId") and r.operationId != null then op.operationId = r.operationId end
		if mapHas(r, "x") and r.x != null then
			for [kx, vx] in r.x do
				op[kx] = vx
			end
		end
		if rb != null then
			op.requestBody = rb
		end

		let m = toLower(r.method)
		paths[pat][m] = op
		if r.style == "value" and m == "get" then
			paths[pat]["head"] = {parameters: ps, responses: rs, operationId: _opId("HEAD", pat)}
		end
		if not mapHas(paths[pat], "options") then
			paths[pat]["options"] = {parameters: ps, responses: {"204": {description: "No Content"}}}
		end
		i = i + 1
	end

	let sv = []
	if servers != null then
		let j = 0
		while j < len(servers) do
			sv = sv + [{url: servers[j]}]
			j = j + 1
		end
	end

	let compsOut = {schemas: comps}
	if mapHas(o, "securitySchemes") and o.securitySchemes != null then
		compsOut.securitySchemes = o.securitySchemes
	end

	let doc = {
		openapi: "3.1.0",
		jsonSchemaDialect: "https://json-schema.org/draft/2020-12/schema",
		info: info,
		servers: sv,
		paths: _sortKeys(paths),
		components: compsOut
	}
	if mapHas(o, "security") and o.security != null then
		doc.security = o.security
	end
	if mapHas(o, "x") and o.x != null then
		for [kx, vx] in o.x do
			doc[kx] = vx
		end
	end
	doc
end
