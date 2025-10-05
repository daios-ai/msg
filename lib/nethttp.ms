# nethttp — façade module
#
# LESS IS MORE: this file only wires the split modules together and
# preserves the original public surface without changing behavior.
#
# Public surface:
#   router(opts?), contract(), testClient()
#   json(), text(), noContent(), redirect(), raise()
#   serve(), shutdown()
#   mwRecover(), mwRequestID(), mwTimeout(), mwCors(), mwAccessLog()

let R = import("nethttp/router")
let M = import("nethttp/middleware")
let S = import("nethttp/server")

# Types (re-export)
let Request   = R.Request
let Responder = R.Responder
let Contract  = R.Contract

# Router / contracts / helpers / tests (re-export)
let router     = R.router
let contract   = R.contract
let testClient = R.testClient
let json       = R.json
let text       = R.text
let noContent  = R.noContent
let redirect   = R.redirect
let raise      = R.raise

# Middleware (re-export)
let mwRecover    = M.mwRecover
let mwRequestID  = M.mwRequestID
let mwTimeout    = M.mwTimeout
let mwCors       = M.mwCors
let mwAccessLog  = M.mwAccessLog

# Server lifecycle (re-export)
let serve    = S.serve
let shutdown = S.shutdown
