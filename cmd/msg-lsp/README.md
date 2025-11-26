
# 1) What are the parts of an LSP?

1. **Transport & Protocol**

   * JSON-RPC 2.0 framing (stdio/pipe/socket), request/response ids, notifications.
   * Protocol types (Position/Range/Diagnostic/etc.), capability negotiation on `initialize`.

2. **Server Core**

   * Router that maps `"textDocument/…"` & `"workspace/…"` to handlers.
   * Concurrency model (per-request goroutines, mutexes, cancellation).
   * Telemetry/logging, error handling.

3. **Document Store**

   * Open buffers, versions, **UTF-8↔UTF-16** conversions, CRLF handling.
   * Incremental edits, immutable **snapshots** for analysis.

4. **Language Frontend (Engine Adapter)**

   * Thin wrapper over your **MindScript** toolchain:

     * Lex/Parse (interactive), Parse+Spans, Pretty/Standardize.
     * Token→byte spans, AST + sidecar spans, error types.
   * Isolated behind an interface so LSP doesn’t depend on concrete engine structs.

5. **Analyzer**

   * Turns a snapshot into reusable artifacts:

     * tokens, AST, spans, **symbol tables**, scope graph, basic type facts.
   * Caching & invalidation keyed by document version; optional incremental.

6. **Feature Providers**

   * Pure functions over analyzer results:

     * Hover, Definition, References, Completion, Signature Help,
       Semantic Tokens, Folding, Document Symbols, Formatting.
   * Each one reads snapshot+analysis; none talk to transport directly.

7. **Workspace Index (optional, but useful)**

   * Cross-file symbols, references, project graph, module imports.

8. **Configuration**

   * Per-workspace settings (formatting options, diagnostics levels),
     file associations, feature toggles.

9. **Utilities**

   * Timeouts/cancellation (`context.Context`), scheduling/debouncing,
     small algorithms (range math, token search, node selection).

---

# 2) How to organize into cohesive, low-coupling units

Below is a Go-ish package layout that matches your current code and MindScript parts.

```
/cmd/mindscript-lsp        // main: wires stdio <-> server
/internal/jsonrpc          // framing + message IO (no language deps)
/internal/protocol         // LSP wire types & capability tables

/internal/server           // router + lifecycle; depends on jsonrpc, protocol
/internal/server/handlers  // thin glue: each handler invokes a Feature

/internal/docstore         // documents, versions, line index, UTF-16 math
/internal/engine           // adapter over mindscript.{lexer,parser,spans,printer}
/internal/analysis         // Analyze(snapshot) -> Result{tokens, ast, spans, symbols, scopes}
/internal/features         // hover, completion, definition, ... (one file per feature)
/internal/workspace        // optional: multi-file index & search
/internal/config           // settings, flags, dynamic registration
/internal/logging          // structured logs, telemetry (opt)
```

**Cohesion & boundaries**

* `jsonrpc`/`protocol` are **transport-only** (no MindScript import).
* `server` knows only about `Feature` interfaces and `docstore`. It does **not** import MindScript.
* `engine` is the **only** package that imports `github.com/daios-ai/msg` (lexer/parser/printer/spans/interpreter). If the engine changes, upper layers don’t.
* `analysis` depends on `engine` and `docstore` and returns a **stable, LSP-friendly result**:

  ```go
  type Result struct {
    Tokens []TokenView
    AST    SView
    Spans  *SpanIndex
    Syms   *Symbols   // defs & uses with ranges
    Scopes *Scopes    // lexical scope graph
    Err    error      // *LexError|*ParseError|*IncompleteError
  }
  ```
* Each `features/*.go` depends only on `analysis.Result` (+ `docstore.Snapshot`) and produces LSP wire types. This keeps features pluggable and testable.

**Key interfaces (to keep coupling low)**

```go
// engine adapter (mindscript facade)
type Engine interface {
  Lex(src string) ([]Token, error)
  ParseInteractive(src string) (S, error)
  ParseWithSpans(src string) (S, *SpanIndex, error)
  Pretty(src string) (string, error)
  FunMeta(Value) (Callable, bool) // if you expose builtins
}

// analyzer contracts
type Analyzer interface {
  Analyze(snap Snapshot) Result
}

// feature contract
type Feature interface {
  Method() string
  Handle(ctx context.Context, snap Snapshot, res Result, params json.RawMessage) (any, *ResponseError)
}
```

This setup gives you:

* **High cohesion**: each unit does one thing (transport, storage, analysis, a single feature).
* **Low coupling**: a feature never needs to know *how* parsing happened; it just reads `Result`. The server doesn’t know MindScript exists.

---

# 3) Dependency tree (who can import whom)

```
cmd/mindscript-lsp
└─ internal/server
   ├─ internal/jsonrpc
   ├─ internal/protocol
   ├─ internal/docstore
   ├─ internal/features
   │  └─ internal/analysis
   │     ├─ internal/engine
   │     │  └─ github.com/daios-ai/msg  (mindscript: lexer, parser, spans, printer, interpreter)
   │     └─ internal/docstore
   ├─ internal/config
   └─ internal/logging

internal/workspace (optional, used by features needing cross-file data)
└─ internal/analysis   (reads per-file Result)
   └─ internal/docstore
```

Or, as edges (acyclic):

* `jsonrpc` → (no deps)
* `protocol` → (no deps)
* `docstore` → (no deps)
* `engine` → **mindscript** only
* `analysis` → `engine`, `docstore`
* `features/*` → `analysis`, `docstore`, `protocol` (for reply types)
* `server` → `jsonrpc`, `protocol`, `docstore`, `features`, `config`, `logging`
* `workspace` → `analysis`, `docstore` (never `engine` directly)

**MindScript internals** remain self-contained:

```
github.com/daios-ai/msg (MindScript)
├─ lexer.go
├─ parser.go
├─ spans.go
├─ printer.go
└─ interpreter.go
```

The LSP should see them **only** through `internal/engine`.

---

## Extra practical tips

* **Snapshots everywhere:** pass immutable snapshots to analysis & features; updates replace the snapshot in the docstore.
* **Error flow:** analyzer returns `Err`; server translates to diagnostics. Features should run even with partial results (tokens only).
* **Config/Capabilities:** keep providers feature-flagged so you can turn them on/off without changing imports.
* **Testing:** unit-test each feature against synthetic `Result` fixtures; integration tests exercise the router + stdio framing.

If you adopt this shape, your project stays flat (few arrows), features remain swap-able, and engine changes won’t ripple through the server.
