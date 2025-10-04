# openapi — build OpenAPI 3.1 from generic RouteSpec[]

# -------------------------------------------------
# Types
# -------------------------------------------------

let RouteContract = type {
	path: Type,
	query: Type,
	headers: Type,
	cookies: Type,
	body: Any,
	responses: {}
}

let RouteSpec = type {
	method!: Str,
	pattern!: Str,
	style!: Str,
	contract: RouteContract,
	summary: Str,
	description: Str,
	tags: [Str],
	deprecated: Bool,
	operationId: Str,
	security: [ {} ]?,
	x: {}?
}

let SpecOpts = type {
	validationErrorSchema: Type,
	securitySchemes: {},
	security: [ {} ]?,
	x: {}?
}

# -------------------------------------------------
# Helpers
# -------------------------------------------------

# Parse path segments, skipping empty and "."
#
# Returns an array of non-empty path segments. Ensures the path starts
# with a slash and excludes "." entries.
#
# Args:
# 	p: Str — raw path string (may be relative or absolute).
#
# Return:
# 	[Str] — list of path segments.
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

# Normalize path: convert {*name} to {name}
#
# Ensures consistent catch-all representation by stripping the leading
# asterisk inside path parameter braces.
#
# Args:
# 	p: Str — path template.
#
# Return:
# 	Str — normalized path template.
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

# Extract parameter names from path template
#
# Scans a path template and returns the list of parameter names,
# converting catch-all parameters like `{*tail}` to `tail`.
#
# Args:
# 	p: Str — path template.
#
# Return:
# 	[Str] — parameter names in appearance order.
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

# Extract map fields from Type via reflection
#
# Reflects a structural map Type and returns its fields with their
# names, requiredness, and field Types. Returns null if T is not a map.
#
# Args:
# 	T: Type — a structural map Type.
#
# Return:
# 	[{name!: Str, req!: Bool, T!: Type}]? — field metadata array or null.
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

# Register Type in components and return $ref
#
# Deduplicates component schemas by a stable uid derived from reflection.
# Registers (or reuses) a name in `components.schemas` and returns a
# $ref object that points to the registered schema.
#
# Args:
# 	reg: {} — registry map: uid -> component name.
# 	comps: {} — components.schemas map to populate.
# 	T: Type — Type to register.
#
# Return:
# 	{} — a JSON Reference object: { "$ref": "#/components/schemas/Name" }.
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

# Sort parameters: path < query < header < cookie, then by name
#
# Produces a stable, deterministic ordering for parameter objects.
#
# Args:
# 	ps: [ {} ] — unsorted parameter list.
#
# Return:
# 	[ {} ] — sorted copy of the parameter list.
let _sortParams = fun(ps: [ {} ]) -> [ {} ] do
	let rank = fun(where: Str) -> Int do
		if where == "path" then return 0 end
		if where == "query" then return 1 end
		if where == "header" then return 2 end
		if where == "cookie" then return 3 end
		9
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

# Sort object keys lexicographically
#
# Returns a new object with keys inserted in ascending lexical order.
# This yields deterministic key order for emission/inspection.
#
# Args:
# 	m: {} — input map.
#
# Return:
# 	{} — new map with keys in sorted order.
let _sortKeys = fun(m: {}) -> {} do
	let ks = sort(list(keys(m)), fun(a: Any) -> Any -> Int do
		fun(b: Any) -> Int do
			if a < b then -1 elif a > b then 1 else 0 end
		end
	end)
	let out = {}
	let k = 0
	while k < len(ks) do
		let key = ks[k]
		out[key] = m[key]
		k = k + 1
	end
	out
end

# Add parameters from map Type to parameter list
#
# Expands a structural map Type into OpenAPI Parameter Objects at the
# given location (path/query/header/cookie). Applies style/explode hints
# for arrays of scalars and map-like query objects.
#
# Args:
# 	ps: [ {} ] — accumulated parameter list.
# 	where: Str — location: "path" | "query" | "header" | "cookie".
# 	T: Type — structural map Type to project.
# 	reg: {} — schema registry.
# 	comps: {} — components.schemas destination.
#
# Return:
# 	[ {} ] — extended parameter list.
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
		end
		if where == "query" then
			p.explode = null
			let rt = reflect(f.T)
			if rt != null and rt[1][0] == "array" then
				let elemRt = rt[1][1]
				let elem = reify(["type", elemRt])
				let elemRefl = reflect(elem)
				if elemRefl != null and elemRefl[1][0] == "id" then
					let n = elemRefl[1][1]
					if n == "Str" or n == "Int" or n == "Num" or n == "Bool" then
						p.style = "form"
						p.explode = true
					end
				end
			end
			if rt != null and rt[1][0] == "map" then
				p.style = "deepObject"
				p.explode = true
			end
		end
		out = out + [p]
		i = i + 1
	end
	out
end

# Synthesize path params from template when contract.path is absent
#
# Generates required path parameters using the path template itself,
# defaulting each schema to string.
#
# Args:
# 	ps: [ {} ] — accumulated parameter list.
# 	pat: Str — normalized path template.
#
# Return:
# 	[ {} ] — extended parameter list with synthesized path params.
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

# Build requestBody from contract.body
#
# Accepts either a Type (treated as required application/json) or a
# structured descriptor supporting `required`, `contentType`, `schema`,
# single `example` or map `examples`, or a direct `content` map.
#
# Args:
# 	def: Any — body definition (Type or object form).
# 	reg: {} — schema registry.
# 	comps: {} — components.schemas destination.
#
# Return:
# 	{} — OpenAPI Request Body Object.
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
	let sc = if mapHas(def, "schema") and def.schema != null then
		_ref(reg, comps, def.schema)
	else
		if ct == "application/octet-stream" then {type: "string", format: "binary"} else {} end
	end
	let entry = {schema: sc}
	if mapHas(def, "examples") and def.examples != null then
		entry.examples = def.examples
	else
		if mapHas(def, "example") and def.example != null then
			entry.example = def.example
		end
	end
	let m2 = {}
	m2[ct] = entry
	{required: req, content: m2}
end

# Build response object
#
# Supports Type shorthand, object forms with `description`, `headers`,
# single `example` / map `examples` paired with `contentType`, or a
# full `content` map. Special-cases 204 to force `content: null`.
#
# Args:
# 	code: Str — HTTP status code string.
# 	v: Any — response descriptor or Type.
# 	reg: {} — schema registry.
# 	comps: {} — components.schemas destination.
#
# Return:
# 	{} — OpenAPI Response Object.
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
		else
			if mapHas(v, "example") and v.example != null then
				entry.example = v.example
			end
		end
		let ct2 = {}
		ct2[v.contentType] = entry
		out.content = ct2
	end
	out
end

# Default responses based on route style
#
# Provides fallback response shapes when a route specifies no explicit
# responses: JSON for value routes, text for stream routes.
#
# Args:
# 	style: Str — route style ("value" or "stream").
#
# Return:
# 	{} — map of status code to Response Object.
let _defRes = fun(style: Str) -> {} do
	if style == "stream" then
		return {"200": {description: "OK", content: {"text/plain": {schema: {type: "string"}}}}}
	end
	{"200": {description: "OK", content: {"application/json": {schema: {}}}}}
end

# Generate operationId from method and path
#
# Synthesizes a stable operationId in the form `method_segment_segment`,
# converting invalid characters to underscores.
#
# Args:
# 	m: Str — HTTP method.
# 	p: Str — path template.
#
# Return:
# 	Str — synthesized operationId.
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
# Public: build OpenAPI doc from RouteSpec[]
# -------------------------------------------------

# Build an OpenAPI 3.1 document from RouteSpec definitions.
#
# Constructs a deterministic OpenAPI 3.1.0 document from a list of
# RouteSpec entries. It normalizes paths, derives parameters from
# structural Types (including synthesis from path templates), builds
# request bodies and responses (with sensible defaults), deduplicates
# component schemas, mirrors HEAD for value GETs, emits OPTIONS, and
# applies route/root security and vendor extensions.
#
# Args:
# 	routes: [RouteSpec] — list of route specifications.
# 	info: {} — OpenAPI `info` object (title, version, etc.).
# 	servers: [Str]? — optional server URLs (omitted -> empty list).
# 	opts: SpecOpts? — optional spec options (validation error schema,
# 		security schemes/requirements, vendor extensions).
#
# Return:
# 	{} — OpenAPI 3.1 document object.
let spec = fun(routes: [RouteSpec], info: {}, servers: [Str]?, opts: SpecOpts?) -> {} do
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
			let sch = if mapHas(o, "validationErrorSchema") and o.validationErrorSchema != null then
				_ref(reg, comps, o.validationErrorSchema)
			else
				{type: "object"}
			end
			rs["422"] = {description: "Validation Error", content: {"application/json": {schema: sch}}}
		end
		let op = {parameters: ps, responses: rs, operationId: _opId(r.method, pat), security: null}
		if mapHas(r, "security") and r.security != null then
			op.security = r.security
		end
		if mapHas(r, "summary") and r.summary != null then
			op.summary = r.summary
		end
		if mapHas(r, "description") and r.description != null then
			op.description = r.description
		end
		if mapHas(r, "tags") and r.tags != null then
			op.tags = r.tags
		end
		if mapHas(r, "deprecated") and r.deprecated != null then
			op.deprecated = bool(r.deprecated)
		end
		if mapHas(r, "operationId") and r.operationId != null then
			op.operationId = r.operationId
		end
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
