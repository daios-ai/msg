# nethttp.ms — A Practical Guide

This tutorial walks you through building HTTP services with **nethttp.ms** — a streaming‑first web toolkit for MindScript. You’ll learn how to define routes, validate inputs with contracts, stream responses, plug in middleware, spin up the TCP server, and test everything in‑memory.

> TL;DR: `router()` gives you value routes (`route`) and streaming routes (`routeStream`). Attach **contracts** to bind/validate `path`, `query`, and `body` before your handler runs (422 on failure). Use helpers like `json(...)`, `text(...)`, `redirect(...)`, or `raise(...)`. Add middleware (`mwRecover`, `mwRequestID`, `mwTimeout`, `mwCors`, `mwAccessLog`). Start a server with `serve(...)`, and shut it down with `shutdown(...)`.

---

## 1) Quick Start

```mindscript
let http = import("nethttp")

let r = http.router()

# Value route: buffers output and returns a response or JSON from a value
r.route("GET", "/hello/{name}", http.contract({
	path: type { name: Str }
}), fun(req, ctx) do
	http.text(200, "hi " + ctx.path.name)
end)

# Run a tiny server
let addr = "127.0.0.1:8080"
let l = netListen(addr)
let srv = http.serve(l, r, { addDateHeader: true })

# ... later
let _ = http.shutdown(srv, 0)
```

Open `http://127.0.0.1:8080/hello/Ada` → `hi Ada`.

---

## 2) Core Concepts

### 2.1 Router

`router()` returns an object that lets you register routes, mount child routers, and install middleware:

* `route(method, pattern, contract, handler)` → **value** (buffered) routes
* `routeStream(method, pattern, contract, handler)` → **streaming** routes
* `mount(prefix, childRouter)` → mount a sub‑router under a path
* `use(mw)` → add middleware (outer‑to‑inner)

### 2.2 Contracts (Input Schemas)

A **contract** declares the shape of the inputs to bind **before** your handler runs:

```mindscript
http.contract({
	path:  type { id!: Int },
	query: type { q: Str, page: Int },
	body:  type { name!: Str, age: Int },
	responses: { "200": type { ok!: Bool } } # optional response validation
})
```

* If binding or validation fails, the router returns **422** with a JSON error body.
* Bound values appear in `ctx.path`, `ctx.query`, and `ctx.body`.

### 2.3 Value vs Streaming Routes

* **Value routes** compute a result and the framework **buffers** it into an HTTP response.
* **Streaming routes** receive a `Responder` and **must** write and end explicitly.

**Responder API** (streaming):

```
status(code:Int) -> Responder
setHeader(k:Str, v:Str) -> Responder
write(chunk:Str) -> Int?
flush(null) -> Bool?
end(null) -> Bool?
```

Headers are sent lazily on the first `status/setHeader/write`. If `Content-Length` is unknown, `Transfer-Encoding: chunked` is used automatically.

---

## 3) Defining Routes

### 3.1 Path Parameters & Catch‑All

```mindscript
r.route("GET", "/users/{id}", http.contract({ path: type { id: Int } }), fun(req, ctx) do
	http.json(200, { ok: true, id: ctx.path.id })
end)

r.routeStream("GET", "/files/{*tail}", http.contract({}), fun(req, res, ctx) -> Null do
	res.status(200).setHeader("Content-Type", "text/plain")
	let _ = res.write("tail= " + ctx.req.pathParams.tail)
	res.end(null)
	null
end)
```

Notes:

* Matching uses a normalized path (`/a//b`, `/a/./b` match `/a/b`).
* **Percent‑encoding is not auto‑decoded** for path params; you receive raw segments.

### 3.2 Query Binding (including arrays)

```mindscript
r.route("GET", "/search", http.contract({
	query: type { q: Str, tag: [Str], limit: Int }
}), fun(req, ctx) do
	http.json(200, { q: ctx.query.q, tags: ctx.query.tag, limit: ctx.query.limit })
end)
```

Accepts both repeated params (`?tag=a&tag=b`) and a JSON array in a single param (`?tag=["a","b"]`). Scalars coerce from strings or JSON scalars when possible.

### 3.3 JSON Body Binding

```mindscript
r.route("POST", "/echo", http.contract({ body: type { msg: Str } }), fun(req, ctx) do
	http.json(200, { ok: true, msg: ctx.body.msg })
end)
```

Invalid JSON or schema mismatches yield **422** automatically.

### 3.4 Response Validation (optional)

```mindscript
r.route("GET", "/health", http.contract({
	responses: { "200": type { ok!: Bool } }
}), fun(req, ctx) do
	http.json(200, { ok: true }) # if wrong shape -> 500
end)
```

---

## 4) Producing Responses (Value Routes)

Choose one of the following:

* Return a **buffered response** directly:

  ```mindscript
  http.text(200, "ok")
  http.json(200, { ok: true })
  http.noContent(null)            # 204
  http.redirect(302, "/new")    # path‑only, external URLs rejected
  ```
* Return **any JSON‑encodable value** → auto `200` with `application/json`.
* Return a **structured error** (no panic):

  ```mindscript
  http.raise(401, "unauthorized")
  http.raiseWithHeaders(429, "slow down", { "Retry-After": "5" })
  ```

If your handler returns `null` or JSON encoding fails, nethttp sends **500**.

> **HEAD**: value routes automatically support `HEAD` (same headers, empty body). Streaming routes return **405** for `HEAD` unless you register a specific `HEAD` route.

---

## 5) Streaming Responses

```mindscript
r.routeStream("GET", "/ticks", http.contract({}), fun(req, res, ctx) -> Null do
	res.status(200).setHeader("Content-Type", "text/plain")
	let _ = res.write("tick 0\n")
	sleep(10)
	let _ = res.write("tick 1\n")
	sleep(10)
	let _ = res.write("tick 2\n")
	res.end(null)
	null
end)
```

* If you don’t set `Content-Length`, the server uses **chunked** encoding and appends the proper terminator.
* Backpressure is honored because writes go directly to the socket.

---

## 6) Middleware

Install middleware with `router.use(mw)`; they wrap handlers **outer‑to‑inner**.

* **Recover**: convert panics to 500

  ```mindscript
  r.use(http.mwRecover())
  ```
* **Request ID**: propagate or inject an ID

  ```mindscript
  r.use(http.mwRequestID("X-Request-ID"))
  ```
* **Timeout**: if no bytes are written by the deadline → 504

  ```mindscript
  r.use(http.mwTimeout(250)) # ms from request start
  ```
* **CORS**: adds ACA* headers; handles `OPTIONS`

  ```mindscript
  r.use(http.mwCors({ origin: "*", credentials: true }))
  ```
* **Access Log**: prints one line per request

  ```mindscript
  r.use(http.mwAccessLog())
  ```

**Note:** For `OPTIONS` requests, the router will:

* Return **204** for known path shapes (or when any middleware is installed, to support generic preflight).
* Return **404** for unknown paths (when no middleware suggests otherwise).

---

## 7) Mounting Routers

Compose APIs by mounting:

```mindscript
let root = http.router()
let v1 = http.router()

v1.route("GET", "/ping", http.contract({}), fun(req, ctx) do
	http.text(200, "pong")
end)

root.mount("/v1", v1)
```

`GET /v1/ping` now resolves to the child router.

---

## 8) Server Lifecycle

### 8.1 Starting

```mindscript
let l = netListen("127.0.0.1:8080")
let srv = http.serve(l, r, {
	maxHeaderBytes: 8192,
	maxBodyBytes: 1048576,
	maxHeaders: null,
	addDateHeader: false,
	readHeaderTimeoutMs: 0,
	writeTimeoutMs: 0,
	idleTimeoutMs: 0
})
```

* Request‑side `Transfer-Encoding: chunked` is **rejected** (400).
* Conflicting `Content-Length` values → **400**.
* Over‑budget headers → **400**; header count limit → **431**.
* Overly long start line → **414**.
* Absolute‑form targets (`GET http://host/path HTTP/1.1`) → **400**.
* `Expect: 100-continue`: nethttp preflights **path/query** against the route before reading the body.

### 8.2 Shutting Down

```mindscript
let _ = http.shutdown(srv, 0)
```

Closes the listener and active connections; further connects will fail or produce no valid response.

---

## 9) Testing Without Sockets

Use the in‑memory **test client** to exercise your routes deterministically:

```mindscript
let c = http.testClient(r)

let res1 = c.call({ method: "GET", path: "/hello/Ada" })
# -> { status, headers, body }

let res2 = c.call({
	method: "POST",
	path: "/echo",
	headers: { "Content-Type": "application/json" },
	body: "{\"msg\":\"hi\"}"
})
```

---

## 10) Recipes

### 10.1 JSON CRUD Skeleton

```mindscript
let http = import("nethttp")
let r = http.router()

let ID = type { id!: Int }
let UserIn = type { name!: Str, age: Int }
let UserOut = type { id!: Int, name!: Str, age: Int }

# Create
r.route("POST", "/users", http.contract({ body: UserIn, responses: { "200": UserOut } }), fun(req, ctx) do
	# pretend persistence
	http.json(200, { id: 1, name: ctx.body.name, age: ctx.body.age })
end)

# Read
r.route("GET", "/users/{id}", http.contract({ path: ID, responses: { "200": UserOut } }), fun(req, ctx) do
	http.json(200, { id: ctx.path.id, name: "Ada", age: 36 })
end)

# Update
r.route("PUT", "/users/{id}", http.contract({ path: ID, body: UserIn, responses: { "200": UserOut } }), fun(req, ctx) do
	http.json(200, { id: ctx.path.id, name: ctx.body.name, age: ctx.body.age })
end)

# Delete
r.route("DELETE", "/users/{id}", http.contract({ path: ID }), fun(req, ctx) do
	http.noContent(null)
end)
```

### 10.2 Streaming “SSE‑like” Text

```mindscript
r.routeStream("GET", "/events", http.contract({}), fun(req, res, ctx) -> Null do
	res.status(200).setHeader("Content-Type", "text/plain")
	let _ = res.write("event: ready\n\n")
	let _ = res.flush(null)
	# emit a few ticks
	let i = 0
	while i < 3 do
		let _ = res.write(sprintf("data: tick %d\n\n", [i]))
		sleep(100)
		i = i + 1
	end
	res.end(null)
	null
end)
```

### 10.3 CORS With Credentials

```mindscript
r.use(http.mwCors({ origin: "*", credentials: true }))
```

Reflects the request `Origin` in `Access-Control-Allow-Origin` and sets `Vary: Origin`.

### 10.4 Timeouts (Return 504 if Nothing Is Written)

```mindscript
r.use(http.mwTimeout(200))

r.route("GET", "/slow", http.contract({}), fun(req, ctx) do
	sleep(500)
	http.text(200, "(too late)")
end)
```

### 10.5 Request ID

```mindscript
r.use(http.mwRequestID("X-Request-ID"))
```

If a client sends `x-request-id: foo` (any case), the same value is echoed with canonical casing.

---

## 11) OpenAPI‑like Introspection (Minimal)

```mindscript
let doc = http.openapiDoc(r, { title: "API", version: "1.0.0" }, ["http://localhost"])
# -> { info, servers, routes: [{ method, pattern, style }] }
```

This is a **lightweight** document intended for quick inspection/testing, not a full OpenAPI 3 spec.

---

## 12) Operational Notes & Gotchas

* **HEAD** auto‑mapping applies **only** to value routes; streaming routes require explicit `HEAD` registration (or else 405).
* **Request bodies**: only `Content-Length` is supported; `Transfer-Encoding: chunked` requests are rejected (400).
* **Redirects**: `http.redirect(302, "/somewhere")` only allows **absolute paths**. External URLs are rejected (400).
* **Header safety**: CRLF in header values → 400; invalid values are blocked.
* **Reason phrases**: common statuses return polished phrases (e.g., 302 **Found**, 422 **Unprocessable Entity**).
* **CORS**: installing `mwCors` also makes generic `OPTIONS` preflights succeed (204) even if there’s no matching method handler.
* **Error semantics**: input validation issues → 422; panics become 500 if `mwRecover` is installed, otherwise the framework still returns a 500 for unhandled panics.

---

## 13) Troubleshooting Checklist

* **422 on requests?** Check your contract types and that inputs are JSON or coercible strings (for scalars) and arrays (for repeated params or JSON arrays).
* **405 with `Allow`**? You hit the path shape but not the method; register the method or inspect the `Allow` header to see what’s supported.
* **HEAD returns body**? Make sure you’re not using a streaming route; value routes auto‑suppress bodies for `HEAD`.
* **Timeouts firing (504)**? If you install `mwTimeout`, ensure your handler writes **something** before the deadline (even headers) to mark progress.
* **Chunked client uploads failing**? chunked requests are not supported; send a `Content-Length`.

---

## 14) API Reference (Public Surface)

* **Construction & Composition**

  * `router()`, `mount(prefix, child)`, `use(mw)`
* **Routing**

  * `route(method, pattern, contract, handler)`
  * `routeStream(method, pattern, contract, handler)`
  * `contract(map)` (marker wrapper)
* **Response Helpers (value routes)**

  * `json(status, value)`, `text(status, s)`, `noContent(null)`, `redirect(status, path)`
  * `raise(status, message)`, `raiseWithHeaders(status, message, headers)`
* **Server**

  * `serve(listener, router, opts)`, `shutdown(server, timeoutMs)`
* **Introspection & Testing**

  * `openapiDoc(router, info, servers?)`
  * `testClient(router)` → `{ call: fun(req) -> {status, headers, body} }`

---

## 15) Closing Thoughts

**nethttp.ms** aims to be small, predictable, and streaming‑friendly. Lean on **contracts** for correctness, **middleware** for cross‑cutting concerns, and **streaming routes** when you need backpressure and low latency. Use the **test client** for quick, deterministic unit tests, and turn on the **access log** when you’re debugging in development.


