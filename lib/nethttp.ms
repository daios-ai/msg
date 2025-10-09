# nethttp — public façade for router, middleware, and server
# 
# Provides a stable, minimal API by re-exporting selected symbols from:
# - nethttp/router
# - nethttp/middleware
# - nethttp/server
# 
# Public surface:
# router(opts?), contract(), testClient()
# json(), text(), noContent(), redirect(), raise()
# serve(), shutdown()
# mwRecover(), mwRequestID(), mwTimeout(), mwCors(), mwAccessLog()

let router = import("nethttp/router")
let mw = import("nethttp/middleware")
let server = import("nethttp/server")

# =========================
# Types (re-export)
# =========================

# HTTP request shape passed to handlers (re-exported).
let Request = router.Request

# Streaming responder interface presented to handlers (re-exported).
let Responder = router.Responder

# Marker type for route contracts (re-exported).
let Contract = router.Contract

# =========================
# Router / contracts / helpers / tests (re-export)
# =========================

# Create a new router.
# 
# Args:
# opts: {}? — Optional router options:
# - docs.enabled?: Bool (default true)
# - docs.path?: Str (default "/openapi.json")
# - docs.info?: {title!: Str, version!: Str, description?: Str}
# - docs.servers?: [Str]
# - docs.opts?: {}
# Return:
# {} — Router instance with route/routeStream/mount/use methods.
let makeRouter = router.router

# Wrap a route contract to mark path/query/headers/cookies/body/responses.
# 
# Args:
# c: Contract — Contract object describing bindings and response types.
# Return:
# Contract — Same contract (marker wrapper).
let contract = router.contract

# Build a simple, in-memory test client for a router.
# 
# Args:
# r: {} — Router instance.
# Return:
# {call!: {} -> {status!: Int, headers!: {}, body!: Str}} — Test client.
let testClient = router.testClient

# Create a JSON response with status code.
# 
# Args:
# status: Int — HTTP status.
# value: Any — Value to JSON-encode as body.
# Return:
# {status!: Int, headers!: {}, body!: Str} — Response object.
let json = router.json

# Create a plain-text response with status code.
# 
# Args:
# status: Int — HTTP status.
# s: Str — Response body as text.
# Return:
# {status!: Int, headers!: {}, body!: Str} — Response object.
let text = router.text

# Create a 204 No Content response.
# 
# Args:
# _: Null — Placeholder (unused).
# Return:
# {status!: Int, headers!: {}, body!: Str} — Response object.
let noContent = router.noContent

# Create a redirect response (path-only policy).
# 
# Args:
# status: Int — Redirect status (e.g., 302).
# location: Str — Absolute path starting with '/'.
# Return:
# {status!: Int, headers!: {}, body!: Str} — Response object.
let redirect = router.redirect

# Create a structured HTTP error to be returned by handlers.
# 
# Args:
# status: Int — HTTP status code.
# message: Str — Text body.
# Return:
# {__http_error__!: Bool, status!: Int, body!: Str, headers!: {}} — Error object.
let raise = router.raise

# =========================
# Middleware (re-export)
# =========================

# Recover from handler panics and return 500.
# 
# Args:
# _: Null — Placeholder (unused).
# Return:
# (_StreamFn -> _StreamFn) — Middleware function.
let mwRecover = mw.mwRecover

# Inject or propagate a request ID via a header.
# 
# Args:
# headerName: Str — Header name to use (e.g., "X-Request-ID").
# Return:
# (_StreamFn -> _StreamFn) — Middleware function.
let mwRequestID = mw.mwRequestID

# Emit 504 if no bytes are written by the deadline.
# 
# Args:
# ms: Int — Timeout in milliseconds.
# Return:
# (_StreamFn -> _StreamFn) — Middleware function.
let mwTimeout = mw.mwTimeout

# Add CORS headers and handle OPTIONS.
# 
# Args:
# opts: {} — {origin?: Str, methods?: [Str], headers?: [Str], credentials?: Bool}
# Return:
# (_StreamFn -> _StreamFn) — Middleware function.
let mwCors = mw.mwCors

# Access log after response with status/bytes/duration.
# 
# Args:
# _: Null — Placeholder (unused).
# Return:
# (_StreamFn -> _StreamFn) — Middleware function.
let mwAccessLog = mw.mwAccessLog

# =========================
# Server lifecycle (re-export)
# =========================

# Serve HTTP/1.1 with the given listener and router.
# 
# Args:
# listener: Any — TCP listener handle from netListen("host:port").
# r: {} — Router instance produced by makeRouter().
# opts: {} — Server options:
# - readHeaderTimeoutMs?: Int
# - writeTimeoutMs?: Int
# - idleTimeoutMs?: Int
# - maxHeaderBytes?: Int (default 8192)
# - maxBodyBytes?: Int (default 1048576)
# - maxStartLineBytes?: Int?
# - maxHeaders?: Int?
# - addDateHeader?: Bool?
# Return:
# {listener!: Any, stop!: {closed!: Bool}, conns!: [Any], opts!: {}}? — Server handle, or null on error.
let serve = server.serve

# Gracefully shut down a server, closing the listener and active connections.
# 
# Args:
# srv: {listener!: Any, stop!: {closed!: Bool}, conns!: [Any], opts!: {}}? — Server handle or null.
# timeoutMs: Int? — Optional timeout (currently best-effort).
# Return:
# Bool — True when shutdown is complete or server was null.
let shutdown = server.shutdown

# Back-compat exported name (router constructor) — maintained for users of the old surface.
# Prefer makeRouter() for clarity; both reference the same function.
let router = makeRouter