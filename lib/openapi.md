# openapi.ms — A Gentle Guide

This guide shows how to use the **`openapi`** module to build an **OpenAPI 3.1** document from a simple, strongly-typed list of routes. You write small **RouteSpec** objects (paths, methods, and contracts), then call `openapi.spec(...)` to get a deterministic OpenAPI document (with `$ref` components, request bodies, responses, security, etc.).

---

## What you get

* **Deterministic OpenAPI 3.1.0** document (stable paths order, component names, and parameter order).
* **Path normalization**: `/{*tail}` → `/{tail}`.
* **Path parameters**: derived from `contract.path` or synthesized from `{param}` in the path.
* **Query/header/cookie parameters** from **structural Types**, with correct **required** flags and styles:

  * Arrays of scalars: `style=form`, `explode=true`
  * Objects in query: `style=deepObject`, `explode=true`
  * Cookies: `style=form`, `explode=true`
* **Request bodies** from a Type or a descriptor (supports `contentType`, `schema`, `example(s)`, `content` map, and binary).
* **Responses**: default per style, or explicit; special case **204** (no content); supports `default:` key.
* **HEAD mirror** for value GET routes, automatic **OPTIONS** stub (unless you define a real OPTIONS).
* **Components/$ref**: deduped and named like `T<stableId>`.
* **422 Validation Error** injected if a contract is present (unless you provide your own 422; overridable schema).

---

## Installation / Import

```mindscript
let openapi = import("openapi")
```

> The module name is single-word **snake_case** per the library’s conventions.

---

## Key types

```mindscript
let RouteContract = type {
	path: Type,        # structural Type for path params
	query: Type,       # structural Type for query params
	headers: Type,     # structural Type for request headers
	cookies: Type,     # structural Type for cookies
	body: Any,         # Type or descriptor (see below)
	responses: {}      # map of status -> Type or descriptor
}

let RouteSpec = type {
	method!: Str,      # "GET" | "POST" | ...
	pattern!: Str,     # "/users/{id}"
	style!: Str,       # "value" or "stream"
	contract: RouteContract,
	summary: Str,
	description: Str,
	tags: [Str],
	deprecated: Bool,
	operationId: Str,  # override
	security: [ {} ]?, # per-route requirement; [] clears root
	x: {}?             # vendor extensions (x-*)
}

let SpecOpts = type {
	validationErrorSchema: Type,  # override 422 schema
	securitySchemes: {},          # components.securitySchemes
	security: [ {} ]?,            # root security requirement
	x: {}?                        # root vendor extensions (x-*)
}
```

---

## Quick start

### 1) Define some structural Types

```mindscript
let IdParam = type { id!: Int }
let Paging = type { page: Int, perPage: Int }
let User = type { id!: Int, name!: Str }
let NewUser = type { name!: Str }
let ApiError = type { error!: Str, detail: [ {} ] }
```

### 2) Describe routes with contracts

```mindscript
let routes = [
	{
		method: "GET",
		pattern: "/users/{id}",
		style: "value",
		contract: {
			path: IdParam,
			query: Paging,                 # optional query params
			responses: { "200": User }     # success schema
		},
		summary: "Get a user by id",
		tags: ["users"]
	},

	{
		method: "POST",
		pattern: "/users",
		style: "value",
		contract: {
			body: NewUser,                 # requestBody application/json
			responses: { "200": User }
		},
		summary: "Create a new user",
		tags: ["users"]
	}
]
```

### 3) Choose info, servers, and options (security, 422, vendor x-*)

```mindscript
let info = { title: "User API", version: "1.0.0" }
let servers = ["https://api.example.com"]

let opts = {
	securitySchemes: {
		ApiKeyAuth: { type: "apiKey", in: "header", name: "X-API-Key" }
	},
	security: [ { ApiKeyAuth: [] } ],
	validationErrorSchema: ApiError,        # used for auto 422 responses
	x: { "x-api-family": "users" }          # root vendor extension
}
```

### 4) Build the document

```mindscript
let doc = openapi.spec(routes, info, servers, opts)
```

Now `doc` is a complete OpenAPI 3.1.0 object you can serialize to JSON/YAML.

---

## Contracts in detail

### Path parameters

* If you **declare** `contract.path = type { id!: Int }` and your pattern contains `/{id}`, `id` becomes a **required** path parameter using `$ref` to the schema of `Int`.
* If you **omit** `contract.path`, the library **synthesizes** path params from `{name}` segments with schema `{type:"string"}`.

```mindscript
# Declared path params
{ method: "GET", pattern: "/u/{id}", style: "value", contract: { path: type { id!: Int } } }

# Synthesized path params (string)
{ method: "GET", pattern: "/v/{name}", style: "value", contract: {} }
```

Catch-alls like `/{*tail}` normalize to `/{tail}` and become required path params named `tail`.

### Query parameters

Provide a structural map `Type`. Required fields use `!`:

```mindscript
let SearchQ = type {
	q!: Str,            # required
	tag: [Str],         # arrays of scalars -> style=form, explode=true
	obj: { a!: Int }    # objects -> deepObject, explode=true
}

{ method: "GET", pattern: "/search", style: "value", contract: { query: SearchQ } }
```

Parameter ordering is **deterministic**: path, then query, then header, then cookie; and names alphabetically within each group.

### Headers & cookies

```mindscript
let H = type { "X-Req": Str }
let C = type { session: Str }

{ method: "GET", pattern: "/info", style: "value", contract: { headers: H, cookies: C } }
```

Cookies default to `style=form`, `explode=true`.

### Request body

You can supply:

1. A **Type** (implies `required: true`, `application/json`):

```mindscript
{ method: "POST", pattern: "/users", style: "value", contract: { body: NewUser } }
```

2. A **descriptor**:

```mindscript
{ method: "POST", pattern: "/upload", style: "value",
  contract: {
	body: {
		contentType: "application/octet-stream",
		required: false
	}
  }
}
```

> `application/octet-stream` maps to `{type:"string", format:"binary"}`.

You can also provide `example` or a map of `examples`, or set `content` directly:

```mindscript
{ method: "POST", pattern: "/echo", style: "value",
  contract: {
	body: {
		contentType: "application/json",
		schema: type { msg!: Str },
		example: { msg: "hi" }
	}
  }
}
```

### Responses

You can give a Type or a descriptor per status code; `default` is supported.

```mindscript
let Ok = type { ok!: Bool }

# Type shorthand
responses: { "200": Ok }

# Descriptor with headers and contentType
responses: {
	"200": {
		description: "plain reply",
		contentType: "text/plain",
		schema: type Str,
		example: "pong",
		headers: { "X-Rate": type Int }
	}
}

# Multiple media types
responses: {
	"200": {
		content: {
			"application/json": Ok,
			"text/plain": type Str
		}
	}
}

# default response
responses: { default: type { message!: Str } }
```

Special cases:

* **204** always emits `content: null` (even if you provided a schema).
* If a route has **any** contract piece (`path/query/headers/cookies/body/responses`), the library injects a **422** Validation Error response with JSON content, unless you already defined one. Use `SpecOpts.validationErrorSchema` to control its schema.

---

## Styles and route “shape”

* `style: "value"` routes default to `200` → `application/json` with `{}` schema.
* `style: "stream"` routes default to `200` → `text/plain` with `string` schema.
  Useful for SSE if you override `contentType: "text/event-stream"`.

```mindscript
{ method: "GET", pattern: "/sse", style: "stream",
  contract: { responses: { "200": { contentType: "text/event-stream", schema: type Str } } } }
```

---

## HEAD & OPTIONS

* For a **value GET** route, a **HEAD** operation is mirrored automatically with the same params/responses (unless you also define HEAD yourself).
* The library adds an **OPTIONS** operation with a `204` stub **unless** you provide a real OPTIONS route; real ones are preserved and not overwritten.

---

## Security

Add schemes and a root requirement via `SpecOpts`. Clear or override at the route level:

```mindscript
let opts = {
	securitySchemes: { ApiKeyAuth: { type: "apiKey", in: "header", name: "X-API-Key" } },
	security: [ { ApiKeyAuth: [] } ]
}

# Inherit root:
{ method: "GET", pattern: "/a", style: "value", contract: {} }

# Clear per-route:
{ method: "GET", pattern: "/b", style: "value", contract: {}, security: [] }
```

---

## Vendor extensions (`x-*`)

* **Root**: `SpecOpts.x` → added to the OpenAPI root.
* **Per route**: `RouteSpec.x` → merged into the operation.

```mindscript
let opts = { x: { "x-root": 1 } }
let routes = [
	{ method: "GET", pattern: "/x", style: "value", contract: {}, x: { "x-route": true } }
]
```

---

## Operation IDs

By default, `operationId` is synthesized as:
`<lowercase method>_<path_segments_joined_by_underscores>` with all invalid chars replaced by `_`.

You can override with `RouteSpec.operationId`.

---

## Components & `$ref`

* Any `Type` used in parameters/bodies/responses becomes a **component schema** with a deterministic name like `T<stableId>`.
* The same `Type` used multiple times dedupes to the same `$ref`.

---

## Sorting & determinism

* **Paths** keys are emitted in **lexicographic order**.
* **Parameters** are sorted by place (`path`, `query`, `header`, `cookie`), then by name.
* **Components** are deterministically named and their key order is lexicographic.

This makes diffs clean and repeatable.

---

## End-to-end example

```mindscript
let User = type { id!: Int, name!: Str }
let NewUser = type { name!: Str }
let Err = type { error!: Str, detail: [ {} ] }
let Q = type { tag: [Str], obj: { a!: Int } }

let routes = [
	{
		method: "GET",
		pattern: "/users/{id}",
		style: "value",
		contract: {
			path: type { id!: Int },
			query: Q,
			responses: { "200": User, default: Err }
		},
		summary: "Fetch a user",
		description: "Return a user by id. Supports tag filters and an object query.",
		tags: ["users"]
	},
	{
		method: "POST",
		pattern: "/users",
		style: "value",
		contract: {
			body: { contentType: "application/json", schema: NewUser, example: { name: "Ada" } },
			responses: { "200": User }
		},
		summary: "Create a user",
		tags: ["users"]
	},
	{
		method: "GET",
		pattern: "/events",
		style: "stream",
		contract: {
			responses: { "200": { contentType: "text/event-stream", schema: type Str } }
		},
		summary: "Stream events",
		tags: ["events"]
	}
]

let info = { title: "Example API", version: "1.0.0" }
let servers = ["https://api.example.com"]
let opts = {
	validationErrorSchema: Err,
	securitySchemes: { ApiKeyAuth: { type: "apiKey", in: "header", name: "X-API-Key" } },
	security: [ { ApiKeyAuth: [] } ]
}

let doc = openapi.spec(routes, info, servers, opts)
```

**Highlights you’ll see in `doc`:**

* `paths` contains `/events` (GET + HEAD for value routes only; stream won’t get HEAD), `/users/{id}` (GET + HEAD), and `/users` (POST).
* `parameters` for `/users/{id}` include:

  * `path` param `id` (required)
  * `query` param `tag` (`style=form`, `explode=true`)
  * `query` param `obj` (`style=deepObject`, `explode=true`)
* `requestBody` on POST `/users` under `application/json` with `example`.
* `responses` include default fallbacks where omitted; **204** routes (if present) have `content: null`.
* Automatic **422** on routes with contracts (schema from `Err`).
* `components.schemas` with deduped `$ref` names like `T<...>`.

---

## Exporting / serializing

Once you have `doc`, you can serialize it:

```mindscript
let json = jsonStringify(doc)     # compact JSON string
writeFile("openapi.json", json)
```

Or pretty-format with your own tooling.

---

## Troubleshooting

* **Missing parameters?** Ensure your `contract.path/query/headers/cookies` is a **map Type** (not a value instance), e.g. `type { id!: Int }`.
* **Style/explode not what you expect?**

  * Arrays only get `explode=true` when the element type is a **scalar** (`Str|Int|Num|Bool`).
  * Objects in query get `style=deepObject`, `explode=true`.
* **No 422 injected?** It’s only auto-added if there’s **any** contract; it’s also skipped if you provide your own `"422"` response.
* **Binary uploads?** Use `contentType: "application/octet-stream"`.
* **SSE?** Use `style: "stream"` and set response `contentType: "text/event-stream"`.

---

## Reference: `spec(...)`

```mindscript
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
#   routes: [RouteSpec] — route specifications.
#   info: {} — OpenAPI 'info' object.
#   servers: [Str]? — optional server URLs (omitted -> empty).
#   opts: SpecOpts? — options (422 schema, security, x-*).
#
# Return:
#   {} — OpenAPI 3.1 document object.
fun spec(routes: [RouteSpec], info: {}, servers: [Str]?, opts: SpecOpts?) -> {}
```
