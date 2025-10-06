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

# =========================
# Types (re-export)
# =========================

let Request   = R.Request         # type alias
let Responder = R.Responder       # type alias
let Contract  = R.Contract        # type alias

# =========================
# Router / contracts / helpers / tests (re-export)
# =========================

let router     = R.router         # ({}?) -> {}
let contract   = R.contract       # Contract -> Contract
let testClient = R.testClient     # ({}) -> {call!: {} -> {}}

let json       = R.json           # Int -> Any -> {}
let text       = R.text           # Int -> Str -> {}
let noContent  = R.noContent      # Null -> {}
let redirect   = R.redirect       # Int -> Str -> {}
let raise      = R.raise          # Int -> Str -> {}

# =========================
# Middleware (re-export)
# =========================

let mwRecover   = M.mwRecover     # Null -> (R._StreamFn -> R._StreamFn)
let mwRequestID = M.mwRequestID   # Str -> (R._StreamFn -> R._StreamFn)
let mwTimeout   = M.mwTimeout     # Int -> (R._StreamFn -> R._StreamFn)
let mwCors      = M.mwCors        # {} -> (R._StreamFn -> R._StreamFn)
let mwAccessLog = M.mwAccessLog   # Null -> (R._StreamFn -> R._StreamFn)

# =========================
# Server lifecycle (re-export)
# =========================

let serve    = S.serve            # Any -> {} -> {} -> {}?
let shutdown = S.shutdown         # {}? -> Int? -> Bool
