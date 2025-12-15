# **nethttp** for building HTTP services

Build fast, streaming-friendly HTTP services in MindScript with a tiny, predictable API.

You’ll start from “hello world,” then add validation, streaming, middleware, OpenAPI docs, a TCP server, and tests. Along the way you’ll learn how routing works, what contracts do, how the server behaves under edge cases, and how to structure an app you can ship.

## 0. What is `nethttp`?

`nethttp` is three pieces that fit together:

1. A **router** that matches paths, binds & validates inputs (“contracts”), and can run either buffered value handlers or streaming handlers.

2. **Middleware** designed for streaming—so you can still time out, recover, inject request IDs, add CORS headers, and log, even when a route is writing gradually.

3. A minimal **HTTP/1.1 server** that wires sockets to the router. It favors clarity and safety: no request chunked uploads, strict header limits, and clear error codes.

There’s also an **OpenAPI exporter** automatically exposed at `/openapi.json`, so your routes can be introspected by tools.

Import everything from one façade:

```mindscript
let http = import("nethttp")
```

Public surface (you’ll use these all manual long):
`router(opts?)`, `contract(obj)`, `testClient(router)`
`json(status, v)`, `text(status, s)`, `noContent(null)`, `redirect(status, "/path")`, `raise(status, msg)`
`mwRecover()`, `mwRequestID(name)`, `mwTimeout(ms)`, `mwCors(opts)`, `mwAccessLog()`
`serve(listener, router, opts)`, `shutdown(server, timeoutMs?)`


## 1. Your First Route

Let’s return a greeting and see the router’s basics.

```mindscript
let http = import("nethttp")

let r = http.router()  # also serves GET /openapi.json by default

r.route("GET", "/hello/{name}", http.contract({
  path: type { name: Str }   # declare we expect a string path segment
}), fun(req, ctx) do
  http.text(200, "hi " + ctx.path.name)
end)
```

**How it works**

* `route` registers a **value** handler (buffered). You return either:

  * a response built with `http.text/json/noContent/redirect`, or
  * *any* JSON-encodable value (which becomes `200` + JSON).
* The **contract** runs *before* your code. Here it binds the `{name}` segment into `ctx.path.name`. If it can’t coerce the type, the router answers **422** automatically.

Try it live:

```mindscript
let addr = "127.0.0.1:8080"
let l = netListen(addr)
let srv = http.serve(l, r, { addDateHeader: true })

# ... visit http://127.0.0.1:8080/hello/Ada

let _ = http.shutdown(srv, 0)
```

> Tip: The router mounts **`GET /openapi.json`** automatically—use it to see what it inferred from your contracts. We’ll customize it later.


## 2. Contracts: Binding and Validation, Explained

A **contract** is a plain object that tells the router how to bind & validate **path**, **query**, **body**, and (optionally) what your **responses** should look like.

```mindscript
http.contract({
  path:  type { id!: Int },                         # /items/{id}
  query: type { filter: Str, page: Int },           # ?filter=...&page=...
  body:  type { name!: Str, price: Num },           # JSON request body
  responses: { "200": type { ok!: Bool } }          # optional response schema check
})
```

* If path/query/body binding fails, the router answers **422** with a compact JSON error.
* If you declare `responses` for a status and your handler returns the wrong shape, the router answers **500** (it’s a server bug, not a client error).
* Bound values live in `ctx.path`, `ctx.query`, `ctx.body`. The raw request object is `req` (includes `method`, `url`, `headers`, `body`, `pathParams`).

**Query arrays** are convenient: `?tag=a&tag=b` or `?tag=["a","b"]` both bind to `type { tag: [Str] }`. Scalars coerce from string or JSON scalar where sensible (`Int`, `Num`, `Bool`, `Str`).


## 3. Value vs Streaming: Pick the Right Tool

### Value route (buffered)

Use this when you’re returning a JSON document or short text:

```mindscript
r.route("POST", "/echo", http.contract({ body: type { msg: Str } }), fun(req, ctx) do
  http.json(200, { ok: true, msg: ctx.body.msg })
end)
```

If your handler returns *any* JSON-encodable value, the router will serialize it as `application/json; charset=utf-8`.

**HEAD support:** all value `GET` routes get `HEAD` automatically (same headers, empty body). You don’t need to register a separate route.

### Streaming route

Use streaming when you want to start sending data early, keep latency low, or produce large/long-lived responses.

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

`res.write` pushes bytes to the socket. If you didn’t set a `Content-Length`, the server switches to **chunked** encoding automatically and appends the proper terminator.

**HEAD on streaming:** not auto-supported. If a client sends `HEAD /ticks`, they’ll get **405** with `Allow: GET` unless you add an explicit HEAD route.


## 4. Middleware: Cross-cutting, Streaming-Safe

Middleware wraps handlers (outermost → innermost). All built-ins are streaming-aware.

```mindscript
r.use(http.mwRecover())                # panics → 500
r.use(http.mwRequestID("X-Request-ID"))# propagate/inject request id header
r.use(http.mwTimeout(250))             # 504 if no bytes written by 250ms
r.use(http.mwCors({ origin: "*", credentials: true }))  # CORS headers + OPTIONS
r.use(http.mwAccessLog())              # status/bytes/duration after response
```

**Timeout semantics:** if *nothing* is written before the deadline (not even headers), the middleware emits **504**. Any write “marks progress” and disables the auto-timeout.

**CORS DETAILS**

* With `credentials:true`, the middleware reflects the request `Origin` and sets `Vary: Origin`.
* `OPTIONS` handling: for known path *shapes* the router returns **204**. If **any** middleware is installed, unknown shapes also get **204** (useful for generic preflights). With no middleware, unknown shapes return **404**.


## 5. Mounting: Compose Routers

Build modular APIs by mounting sub-routers:

```mindscript
let root = http.router()
let v1 = http.router()

v1.route("GET", "/ping", http.contract({}), fun(req, ctx) do
  http.text(200, "pong")
end)

root.mount("/v1", v1)   # GET /v1/ping
```

The child router sees paths without the parent prefix; the outer middleware stack still applies.


## 6. Server: What the Wire Looks Like

Start a server by binding a listener and passing your router:

```mindscript
let l = netListen("127.0.0.1:8080")
let srv = http.serve(l, r, {
  maxHeaderBytes: 8192,        # defaults shown here
  maxBodyBytes: 1048576,
  maxStartLineBytes: null,
  maxHeaders: null,
  addDateHeader: false,
  readHeaderTimeoutMs: 0,
  writeTimeoutMs: 0,
  idleTimeoutMs: 0
})
```

Shut down:

```mindscript
let _ = http.shutdown(srv, 0)
```

**Intentional constraints (so your service is predictable):**

* Request **Transfer-Encoding: chunked** is rejected: **400**. Only `Content-Length` uploads are accepted (up to `maxBodyBytes`).
* Conflicting duplicate `Content-Length` → **400**.
* Header size overflow → **400**; header **count** overflow → **431**.
* Start line too long → **414**.
* Absolute-form targets (`GET http://host/path HTTP/1.1`) → **400**.
* `Expect: 100-continue`: the router **preflights** path/query against the matching route before reading the body.

**Reason phrases** are clean (e.g., `422 Unprocessable Entity`, `302 Found`).
**Date header** is off by default—toggle with `addDateHeader: true`.


## 7. OpenAPI: First-class, Auto-generated

By default, a router serves **`GET /openapi.json`**. You can customize or disable it when creating the router:

```mindscript
let r = http.router({
  docs: {
    enabled: true,
    path: "/openapi.json",
    info: { title: "Shop API", version: "1.0.0", description: "Demo" },
    servers: ["http://127.0.0.1:8080"],
    opts: {
      # validationErrorSchema: type {...},   # set a custom 422 schema
      # securitySchemes: { ... },            # components.securitySchemes
      # security: [ { ... } ],               # global security requirement
      # x: { team: "payments" }              # vendor extensions
    }
  }
})
```

What ends up in the document?

* Paths and methods from your registrations (`route`, `routeStream`).
* Parameters synthesized from your **contracts** (and from the pattern itself for `{id}` segments).
* Request bodies built from `body` contracts (defaulting to `application/json` unless you specify others).
* Responses from your explicit `responses`, or a sensible default when omitted. If a contract is present but no `422` is listed, a validation error response is auto-added.
* `HEAD` is added for value `GET` routes.
* Component schemas are `$ref`’d and de-duplicated.

This is a **full OpenAPI 3.1** doc—connect it to UI tooling, client generation, or validation pipelines.


## 8. Testing Without Sockets

The test client calls the router as if it were behind the server, without opening ports:

```mindscript
let c = http.testClient(r)

let res1 = c.call({ method: "GET", path: "/hello/Ada" })
# -> { status: 200, headers: {...}, body: "hi Ada" }

let res2 = c.call({
  method: "POST",
  path: "/echo",
  headers: { "Content-Type": "application/json" },
  body: "{\"msg\":\"hi\"}"
})
# -> body: {"ok":true,"msg":"hi"}
```

Use this for unit tests: deterministic, fast, and validates your contracts and middleware logic.


## 9. Building a Small Service (End-to-End)

Let’s assemble CRUD, streaming updates, CORS, timeouts, logging, and docs.

```mindscript
let http = import("nethttp")

# ---------- Router with docs ----------
let r = http.router({
  docs: {
    enabled: true,
    info: { title: "Users API", version: "1.0.0" },
    servers: ["http://127.0.0.1:8080"]
  }
})

# ---------- Middleware ----------
r.use(http.mwRecover())
r.use(http.mwRequestID("X-Request-ID"))
r.use(http.mwTimeout(500))
r.use(http.mwCors({ origin: "*", credentials: true }))
r.use(http.mwAccessLog())

# ---------- Types ----------
let ID      = type { id!: Int }
let UserIn  = type { name!: Str, age: Int }
let UserOut = type { id!: Int, name!: Str, age: Int }

# ---------- Routes ----------
# Create
r.route("POST", "/users", http.contract({
  body: UserIn,
  responses: {"200": UserOut}
}), fun(req, ctx) do
  http.json(200, { id: 1, name: ctx.body.name, age: ctx.body.age })
end)

# Read
r.route("GET", "/users/{id}", http.contract({
  path: ID,
  responses: {"200": UserOut}
}), fun(req, ctx) do
  http.json(200, { id: ctx.path.id, name: "Ada", age: 36 })
end)

# Update
r.route("PUT", "/users/{id}", http.contract({
  path: ID,
  body: UserIn,
  responses: {"200": UserOut}
}), fun(req, ctx) do
  http.json(200, { id: ctx.path.id, name: ctx.body.name, age: ctx.body.age })
end)

# Delete
r.route("DELETE", "/users/{id}", http.contract({ path: ID }), fun(req, ctx) do
  http.noContent(null)
end)

# Streaming progress
r.routeStream("GET", "/users/{id}/events", http.contract({ path: ID }), fun(req, res, ctx) -> Null do
  res.status(200).setHeader("Content-Type", "text/plain")
  let _ = res.write("start\n")
  sleep(50)
  let _ = res.write("working\n")
  sleep(50)
  let _ = res.write("done\n")
  res.end(null)
  null
end)

# ---------- Server ----------
let l = netListen("127.0.0.1:8080")
let srv = http.serve(l, r, { addDateHeader: true })

# visit:
#   GET  /openapi.json
#   POST /users         {"name":"Grace","age":35}
#   GET  /users/1
#   GET  /users/1/events

# ...later
let _ = http.shutdown(srv, 0)
```

What you get:

* Input validation with helpful **422** when clients send bad data.
* Safe streaming with a deadline (504 if the handler stays silent).
* CORS that reflects the `Origin` when credentials are enabled.
* Request IDs in and out (case-insensitive header handling).
* Access logs that summarize each request with duration and status.
* A live **OpenAPI** endpoint your tools can consume.


## 10. Operational Notes (Deep Cuts)

* **Path matching**: `{*tail}` is a catch-all; otherwise patterns must match segment-by-segment. For matching, repeated slashes and `.` segments are normalized. The **raw** segments are still passed to your handler.
* **Percent-encoding** in path params is **not** decoded—you get exactly what was on the wire (e.g., `%2F` remains `%2F`).
* **Redirects** are path-only: `http.redirect(302, "/new")`. External URLs are rejected with **400** (guards against header injection and open redirects).
* **Header safety**: CR/LF in header values → **400**. The server will bail out early instead of sending a poisoned response.
* **Allow header**: when a path shape exists but the method doesn’t, the server replies **405** and lists permitted methods in `Allow`.


## 11. Troubleshooting

* *I keep getting 422s.* Re-check the contract. Are you coercing `Int` from a non-numeric string? For arrays, either repeat the param or send a JSON array in one param.
* *My streaming route 504s with the timeout middleware.* Write something early—headers or a small chunk—to mark progress.
* *Why is my HEAD returning the whole body?* Ensure the route is a **value** route. Streaming routes don’t auto-support HEAD.
* *Client uploads via TE: chunked fail.* Correct—unsupported by design. Send `Content-Length` and keep under `maxBodyBytes`.


## 12. Reference (for when you forget names)

* **Router**: `router(opts?)`, `.route`, `.routeStream`, `.mount`, `.use`
* **Contracts**: `contract({ path?, query?, body?, responses? })`
* **Responder** (streaming): `status`, `setHeader`, `write`, `flush`, `end`
* **Helpers**: `json`, `text`, `noContent`, `redirect`, `raise`
* **Server**: `serve(listener, router, opts)`, `shutdown(server, timeoutMs?)`
* **Testing**: `testClient(router) -> { call(req) -> {status, headers, body} }`
* **Docs**: `GET /openapi.json` (configurable via `router({docs:{...}})`)


