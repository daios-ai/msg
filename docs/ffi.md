
# MindScript FFI (`ffiOpen`) — Design Specification

## 0. Overview

This document specifies **`ffiOpen`**, a minimal but practical foreign-function interface for MindScript targeting POSIX/ELF systems and the SysV ABI. The goal is to let MindScript programs _declare_ a compact, JSON-compatible description of the C symbols, data layouts, and calling conventions they need, and then call them as if they were native MindScript functions—safely, predictably, and without compromising the language’s value and type model.

The design is strongly inspired by LuaJIT’s FFI—both in _capability_ and _ergonomics_—but it is tailored to MindScript’s choices:

-   **Structural types** (S-expressions) and runtime `Type` values,
    
-   A minimal value lattice (`Null`, `Bool`, `Int`=64-bit, `Num`=64-bit float, `Str` UTF-8, arrays, maps),
    
-   **Opaque handles only** (`VTHandle`) instead of raw pointers **or inline aggregate literals**,
    
-   A strict error model (annotated null vs. hard errors),
    
-   **Isolates** by default (interpreter clones).
    

The FFI surface is small: a single entry point `ffiOpen(spec: Map) -> Module`, plus a standard toolbox submodule `__mem` for size/alignment/alloc/cast/field access/GC helpers. Everything else is data: the `spec` shape describes types, functions, and variables.

**Key cross-boundary rule (VERY CLEAR):**  
**All aggregates cross the FFI boundary as handles. Always.**

-   If C **returns an aggregate by value** (e.g., `struct`, `union`, fixed-size `array`), the bridge **allocates heap storage, copies the bytes, and returns a `VTHandle`** to that storage.
    
-   If C **expects an aggregate by value** (a by-value parameter), the caller **must supply a `VTHandle`** to previously allocated, compatible storage; the bridge **copies its bytes into the ABI call frame**.
    
-   If C **expects a pointer to an aggregate**, the caller **must supply a `VTHandle`**; the pointer is passed as-is.
    
-   **Inline map/array literals are not accepted at call sites** for aggregate parameters.
    

## 1. Goals and Motivation

### 1.1 What we want

1.  **Direct interop** with C libraries (POSIX world): call functions, access globals, work with structs/unions/enums, pass callbacks.
    
2.  **Predictable safety**: no “mystery memory”. Every pointer crossing the boundary is tagged and tracked as a **handle**, so MindScript stays high-level and safe by default.
    
3.  **Portable ABI choice**: pin to a single target (ELF + SysV ABI) for determinism; avoid half-supported platforms and surprises.
    
4.  **Ergonomic enough** for everyday use: concise specs, reasonable defaults, escape hatches where needed.
    
5.  **Honest about costs and lifetimes**: explicit allocation and finalization; no hidden frees or magical ownership transfers.
    

### 1.2 Why not “just pointers”?

MindScript intentionally avoids raw pointers in its value model. Instead, MindScript uses **`VTHandle`**—an opaque, tagged carrier. The tag documents the _intended_ pointee type (“what this pointer means”) and lets users attach finalizers via `__mem.gc`. This preserves safety and debuggability while remaining explicit about ownership and lifetimes.

### 1.3 Why a declarative `spec`?

-   **Determinism & reviewability.** The spec is a _manifest_ of what you import—auditable, diffable, reproducible.
    
-   **Static preparation.** From a spec, the FFI can precompute libffi call interfaces (CIFs), struct layouts, and symbol lookups before first call, so hot paths do not re-build metadata.
    

## 2. Scope and Non-Goals

-   **Supported**: POSIX/ELF platforms using the SysV ABI; dynamic link via `dlopen(3)/dlsym(3)`.
    
-   **Out of scope**: Windows ABI, COM/WinAPI; platform-specific vector ABIs; C++ name mangling and overloads; automatic header parsing; automatic ref-counting or ownership inference.
    

This focus minimizes surprises and allows tight, simple implementations.

## 3. Core Abstraction: `ffiOpen`

### 3.1 Signature and contract

```mindscript
ffiOpen(spec: Map) -> Module

```

-   `spec` **must** be a JSON-compatible MindScript map (no functions inside).
    
-   Returns a **Module** constructed from the specification:
    
    -   Functions become **callables** (`C.name(...)`),
        
    -   Variables become records with `.get()/.set()/.addr()`,
        
    -   Declared types appear as runtime `Type` values (`C.NameType`),
        
    -   A memory toolbox appears as `C.__mem`.
        
    -   **Aggregate by-value returns** are exposed as **pointer handles** to heap storage; the ABI is honored under the hood.
        

Hard errors (e.g., bad conversion, symbol not found, overflow) follow MindScript’s error discipline.

### 3.2 Why “one shot”?

A single function keeps **isolate semantics** clean (per-interpreter caches, no global registries) and avoids scattering stateful operations.

## 4. The Specification Format

The spec is a single map with three blocks—`types`, `functions`, `variables`—and a default library. No symbols are exported unless explicitly declared.

```json
{
  "version": "1",
  "lib": "libc.so.6",

  "types": { "Name": Type },
  "functions": [ FunctionDecl ],
  "variables": [ VarDecl ]
}

```

**Fields**

-   `version` — `"1"` (string). **Required.** Unknown versions must be rejected.
    
-   `lib` — default soname/path. **Required.**
    
-   `types` — map of type declarations. Optional.
    
-   `functions` — list of function imports. Optional.
    
-   `variables` — list of global imports. Optional.
    

### 4.1 Types: expressing C shapes without headers

Types are structural objects. They can be referenced by name (`TypeRef`) or inlined. The full set:

```json
{ "kind":"void" }

{ "kind":"int",   "bits": 8|16|32|64, "signed": true|false }

{ "kind":"float", "bits": 32|64 }

{ "kind":"pointer", "to": TypeRef, "tag": "optional.handle.tag" }

{ "kind":"array", "of": TypeRef, "len": N }
{ "kind":"array", "of": TypeRef }     // VLA/flexible (special placement rules)

{ "kind":"struct",
  "fields":[ { "name":"x", "type": TypeRef }, ... ] }

{ "kind":"union",
  "fields":[ { "name":"u", "type": TypeRef }, ... ] }

{ "kind":"enum", "base": IntTypeRef, "values": { "NAME": integer, ... } }

{ "kind":"funcptr", "ret": TypeRef, "params":[ TypeRef, ... ], "variadic": false }

{ "kind":"alias", "to": TypeRef }

{ "kind":"handle", "tag":"vendor.kind", "rep": TypeRef }

```

We do not support bitfields directly (although we allow for reading/writing bits using helpers; see below).

**Why structural ints/floats?**  
We need **portable intent** (“32-bit signed int”) rather than platform typedef names. The tuple `(kind, bits, signed)` maps cleanly to libffi and ABI rules.

**Pointers → handles**  
All pointers in MindScript appear as `VTHandle`. The pointer type allows an optional **tag** to set a friendly name (e.g., `"tag":"sqlite.conn*"`).

**Flexible arrays**  
Supported in the spec; layout engine computes sizes/alignments once per spec. `__mem.sizeof/alignof/offsetof` reflect the computed layout.

### 4.2 Reference (selected kinds)

-   `void`, `int`, `float`, `pointer`, `array`, `struct`, `union`, `enum`, `funcptr`, `alias`, `handle` — as shown above (unchanged).
    

### 4.3 Functions: imports as first-class values

```json
{
  "name": "qsort",
  "ret": TypeRef,
  "params": [ TypeRef, ... ],
  "variadic": false,
  "lib": "libc.so.6",
  "doc": "optional",
  "ret_as_str": true
}

```

**Additional validation/behavior notes**

-   **TypeRef**: string key from `spec.types` or inline type object.
    
-   **Callbacks**: For `{ "kind":"funcptr", ... }` params, pass a MindScript function; the FFI creates a closure and trampoline.
    
-   **Aggregates (STRUCT/UNION/ARRAY): handles only — no literals.**
    
    -   **Pointer parameters to aggregates**: pass a `VTHandle` pointing to compatible storage.
        
    -   **By-value parameters**: pass a `VTHandle` to compatible storage; the bridge copies bytes from that storage into the ABI call frame.
        
    -   **By-value returns**: the bridge allocates heap storage, copies returned bytes, and returns a `VTHandle` to that storage (not auto-freed).
        
-   **Strings**: If a parameter is `pointer` to 8-bit int (signed or unsigned), a MindScript `Str` is accepted and passed as a temporary NUL-terminated UTF-8 buffer.
    
-   **Nullability**: MindScript `Null` maps to C `NULL` for any `pointer`/`funcptr` parameter; otherwise error.
    
-   **Variadics**: When `variadic:true`, MindScript passes an array for the varargs bundle; C default promotions apply. Aggregate by-value varargs are **not supported** (pass a pointer handle when the C API allows).
    

### 4.4 Variables: controlled global state

```json
{ "name":"errno", "type": TypeRef, "lib":"libc.so.6" }

```

Exposed as `C.errno.get()/set()/.addr()`; `.addr()` returns a pointer handle with the correct tag.

## 5. Conversion and Marshalling Rules

### 5.1 MindScript → C (parameters)

-   **Int (64-bit)** → C integer: must fit. Signedness enforced; negatives rejected for unsigned.
    
-   **Num (64-bit float)** → C float; to C integer only if integral and in range.
    
-   **Bool** → 0/1, then integer rule.
    
-   **Null** → `NULL` for pointer/funcptr; otherwise error.
    
-   **Str (UTF-8)** → accepted only for params typed as `char*`/`unsigned char*`; copied to a temporary NUL-terminated buffer valid for the call.
    
-   **VTHandle** → accepted. **Required for all aggregate parameters** (pointer or by-value).
    
-   **Funcptr parameters (callbacks)** → pass a MindScript function; the FFI creates a libffi closure. Keep the function reachable for as long as C may call it.
    
-   **No literal auto-boxing.** Struct/union/array parameters **must** be supplied as `VTHandle` values referencing caller-allocated storage (e.g., via `__mem.new`/`__mem.box`). **Map/array literals are rejected.**
    

### 5.2 C → MindScript (returns / loads)

-   **Integers/floats/bools** → `Int`/`Num`/`Bool`.
    
-   **Pointers** → `VTHandle` with tag.
    
-   **`char*`** → pointer handle by default; if `"ret_as_str": true`, convert to MindScript `Str` by reading until NUL.
    
-   **Aggregates by pointer** → pointer handles.
    
-   **Aggregates by value (STRUCT/UNION/ARRAY)** → **always boxed**: the bridge allocates heap storage, copies the returned bytes, and returns a pointer handle tagged for the aggregate type. Returned storage is **not auto-freed**; use `__mem.gc(ptr,"free")` or a custom destructor.
    

### 5.3 Variadics

-   Only scalars and pointer/handle values are supported as varargs. **Aggregate by-value varargs are not supported**; pass a pointer handle when the C API allows.
    
-   C default promotions are applied (`float→double`, small integers→`int`).
    

### 5.4 Annotations

MindScript annotations are stripped when going MindScript→C and added C→MindScript based on the `doc` field.

**No aggregate literals.** Passing map/array literals for aggregate parameters is **not permitted**. Errors state that a `VTHandle` is required and indicate the expected aggregate type.

## 6. Memory & Utilities (`C.__mem`)

`C.__mem` is the small, opt-in toolbox for allocating buffers/structs, reading/writing raw memory, field accessors, and attaching finalizers. Answers for size/alignment/offsets come from the **spec’s** computed layout.

**When you need it**

-   Passing/receiving **handles** for pointers or by-value aggregates.
    
-   Computing **`sizeof/alignof/offsetof`** for your spec types.
    
-   **Casting** a pointer to another tag intentionally.
    
-   Attaching a **finalizer** (`free`, `close`, custom) to a handle.
    

**API (selected)**

-   `sizeof(T) -> Int` — ABI size (bytes).
    
-   `alignof(T) -> Int` — ABI alignment (bytes).
    
-   `offsetof(struct, field) -> Int` — byte offset per computed layout.
    
-   `typeof(T) -> Type` — normalize a spec name/inline to a runtime `Type`.
    
-   `new(T, count?) -> PtrHandle` — allocate one or `count` elements; flexible members use `count` for the trailing area. Use this to create storage for **all** aggregate parameters and to persist **by-value aggregate returns**.
    
-   `cast(T, valOrPtr) -> PtrHandle|Scalar` — explicit, checked cast (retags).
    
-   `gc(ptr, finalizer) -> PtrHandle` — attach destructor.
    
-   `string(ptr, len?) -> Str` — read UTF-8 (NUL-terminated if `len` omitted).
    
-   `copy(dstPtr, srcPtr_or_Str, n)` / `fill(dstPtr, byte, n)` — byte ops with checks.
    
-   `errno(value?) -> Int` — get/set `errno`.
    
-   `malloc/calloc/realloc/free` — raw allocators returning/consuming **tagged** handles.
    
-   `getf(T, ptr, fieldName) -> Any` / `setf(T, ptr, fieldName, value)` — field IO.
    
-   `box(T, initMap?) -> PtrHandle` — allocate storage for aggregate type `T`, optionally initialize from a field/index map, and return a tagged handle. **Note:** `box` initializes storage; calls still **pass the handle**, not a literal.
    

## 7. Error Model and Diagnostics

-   **Hard errors** for conversion/ABI mismatches: wrong arity, overflow, unknown symbol, closure creation failure, aggregate literal passed where a handle is required, etc.
    
-   **Handles required for aggregates.** If a struct/union/array parameter is supplied as anything other than a `VTHandle`, the call fails with a hard error: _“aggregate parameter requires handle to compatible storage (got …)”_.
    
-   For by-value aggregate returns, diagnostics may note that boxed storage was allocated and how to attach a destructor.
    

## 8. Concurrency and Isolates

Each `Interpreter` instance is an **isolate**. `ffiOpen` stores per-interpreter state (dlopen handles, symbol tables, computed layouts) and is **not** thread-safe across goroutines on the same interpreter. For parallelism, use `Clone()` and open the FFI in the clone. The `__mem.gc` finalizers run in the host’s GC context but must not reach back into other interpreters.

## 9. Security and Safety Considerations

-   **No automatic ownership transfers.** Users decide when to free.
    
-   **Tagged pointers only.** All pointer traffic is wrapped in `VTHandle` with a tag.
    
-   **Explicit casts.** `__mem.cast` exists, intentional and auditable.
    
-   **No header parsing.** Specs are explicit and small.
    
-   **Libraries are local.** `dlopen(..., RTLD_LOCAL)` avoids symbol leakage.
    

## 10. Example-driven Justification

### 10.1 libm hypot

```mindscript
let C = ffiOpen({
  "version": "1",
  "lib": "libm.so.6",
  "types": { "double": { "kind":"float", "bits":64 } },
  "functions": [
    { "name":"hypot", "ret":"double", "params":["double","double"] }
  ]
})
C.hypot(3.0, 4.0)  // 5.0

```

### 10.2 qsort with callback

```mindscript
let C = ffiOpen({
  "version": "1",
  "lib": "libc.so.6",
  "types": {
    "size_t": { "kind":"int","bits":64,"signed":false },
    "Cmp": { "kind":"funcptr",
      "ret": { "kind":"int","bits":32,"signed":true },
      "params": [
        { "kind":"pointer","to":{ "kind":"void" } },
        { "kind":"pointer","to":{ "kind":"void" } }
      ]
    }
  },
  "functions": [
    { "name":"qsort", "ret": { "kind":"void" },
      "params":[
        { "kind":"pointer","to":{ "kind":"void" } }, "size_t", "size_t", "Cmp"
      ]
    }
  ]
})

```

### 10.3 Pointers as handles; custom tags

```mindscript
let C = ffiOpen({
  "version": "1",
  "lib": "libSDL2-2.0.so.0",
  "types": {
    "voidp": { "kind":"pointer", "to": { "kind":"void" }, "tag":"sdl.ptr.void" },
    "SDL_Vector": { "kind":"handle", "tag":"sdl.vector", "rep":"voidp" }
  },
  "functions": [
    { "name":"SDL_CreateVector", "ret":"SDL_Vector",
      "params":[ { "kind":"int","bits":32,"signed":true } ] },
    { "name":"SDL_Add", "ret":{ "kind":"void" },
      "params":[ "SDL_Vector", "SDL_Vector", "SDL_Vector" ] }
  ]
})
let v = C.SDL_CreateVector(3)
C.SDL_Add(v, v, v)

```

### 10.4 Strings with explicit bridge

```mindscript
let C = ffiOpen({
  "version": "1",
  "lib": "libc.so.6",
  "types": {
    "charp": { "kind":"pointer", "to": { "kind":"int","bits":8,"signed":true } }
  },
  "functions": [
    { "name":"puts", "ret": { "kind":"int","bits":32,"signed":true },
      "params":[ "charp" ] },
    { "name":"strdup", "ret":"charp", "ret_as_str":true,
      "params":[ "charp" ] }
  ]
})
C.puts("hello")
let s = C.strdup("world")

```

### 10.5 Aggregates: **handles only** (parameters and returns)

```mindscript
let C = ffiOpen({
  "version":"1", "lib":"libc.so.6",
  "types":{
    "I32": { "kind":"int","bits":32,"signed":true },
    "P":   { "kind":"pointer","to":{ "kind":"void" } },
    "S":   { "kind":"struct", "fields":[
      {"name":"a","type":"I32"},
      {"name":"p","type":"P"}
    ] },
    "Sp":  { "kind":"pointer","to":"S" }
  },
  "functions":[
    { "name":"some_api_in_ptr",  "ret":{ "kind":"void" }, "params":["Sp"] },  // pointer param
    { "name":"some_api_in_val",  "ret":{ "kind":"void" }, "params":["S"] },   // by-value param
    { "name":"some_api_ret_val", "ret":"S", "params":[] }                     // by-value return
  ]
})

// Create storage for S and initialize it; pass HANDLE only.
let hs = C.__mem.box("S", { a: 7, p: null })
C.some_api_in_ptr(hs)               // passes pointer as-is
C.some_api_in_val(hs)               // copies bytes from hs into ABI by-value slot

// By-value return is boxed into heap storage and returned as a handle.
let out = C.some_api_ret_val()
// Attach a destructor when appropriate:
C.__mem.gc(out, "free")

```

## 11. Implementation Notes (for engineers)

-   **Loader:** `dlopen(lib, RTLD_LAZY|RTLD_LOCAL)`; per-symbol lib overrides; close when the module is GC’d or interpreter is dropped.
    
-   **Calls:** **libffi** for both calls and closures. Build CIFs at `ffiOpen` time; cache per interpreter.
    
    -   **Aggregates:** Provide real `ffi_type` for structs/unions so by-value params/returns follow the ABI; for by-value **returns**, copy to heap and wrap as handle; for by-value **params**, require a handle to caller-allocated storage and copy from it into the call frame.
        
-   **Layout:** compute struct/union layout per SysV rules (field alignment, padding, flexible member). All `__mem.sizeof/alignof/offsetof` refer to this computed model.
    
-   **Marshalling:**
    
    -   Temporary UTF-8 buffers for `Str` inputs to `char*`.
        
    -   Return pointers are **never auto-freed**.
        
    -   Expose `__mem.gc` for custom destruction.
        
-   **Lifetimes:** keep strong refs to callback trampolines while callable; allow explicit unpinning.
    
-   **Diagnostics:** include param name and any user annotations; show handle tags; include library/symbol on `dlsym` failures; **reject aggregate literals** with a clear message.
    

## 12. Summary

This FFI is **small**, **explicit**, and **safe by construction**:

-   Pointers are **handles** with **tags**, not integers.
    
-   Conversions are **checked** and **predictable**.
    
-   Specs are **declarative** and **portable**.
    
-   **Aggregates are uniform: handles only.** No inline literals at call sites. Pointer params take handles; by-value params copy from handles; by-value returns are boxed and returned as handles.
    
-   Ergonomics (like string bridging) exist where they are overwhelmingly common; lifetimes/ownership are explicit and visible.

# Appendix A — Standard Pattern for Publishing FFI Wrappers

This appendix standardizes how to expose C libraries to MindScript via `ffiOpen` while keeping a clean `import("libname")` experience. It also codifies naming, helper design, and lifetime rules consistent with the **“aggregates = handles only”** policy.



## A.1 Goals

-   **Single public entry:** `let M = import("libname")` (loads `libname.ms`).
    
-   **Ergonomic names:** Avoid raw `LIB_FooBar` at call sites; present idiomatic MindScript names.
    
-   **Safe helpers:** Add value on top of the raw FFI (allocation, GC hooks, invariants) without violating handle rules.
    



## A.2 File Shape & Import Bridge

Create `libname.ms`. Anything bound at top level is exported (Python-like).

```mindscript
# libname.ms  — public wrapper module

# 1) Load raw FFI privately
let _C = ffiOpen({
  version: "1",
  lib: "libNAME.so",
  types: { /* … */ },
  functions: [ /* … */ ],
  variables: [ /* optional … */ ]
})

# 2) Public surface (exports)
let version = "name@wrap-0.1.0"        # wrapper semver
let close = _C.close                    # release dlopen/libffi resources

# Constants you commit to supporting (either flat or grouped)
let SOME_FLAG = 0x100

# Aliases: strip common uppercase prefix and camelCase
let init = _C.NAME_Init
let quit = _C.NAME_Quit

# Helpers: use _C.__mem for allocation/GC/field IO; pass handles only
let makeThing = fun(x, y) {
  _C.__mem.box("Thing", { x:x, y:y })   # aggregate handle, not a literal at callsite
}
let createWindow = fun(title, w, h, flags) {
  let win = _C.NAME_CreateWindow(title, 100, 100, w, h, flags)
  if win == null then fail("createWindow: native returned null") end
  _C.__mem.gc(win, { sym:"NAME_DestroyWindow" })
  win
}

```

**Import usage**

```mindscript
let M = import("libname")
M.init(M.SOME_FLAG)
let t = M.makeThing(1, 2)
M.close()

```



## A.3 Naming Rules (Raw → Idiomatic)

1.  **Strip a shared uppercase prefix** from C symbols (e.g., `SDL_`, `PQ`, `EV`, `BZ2_`).
    
2.  **camelCase** the remainder:
    
    -   `SDL_DestroyWindow` → `destroyWindow`
        
    -   `PQexec` → `exec`
        
3.  **Constants:** keep SCREAMING_SNAKE_CASE, or group under a `const` record.
    
4.  **Types/handles:** export names only if useful; operations must use **handles**, never inline aggregates.
    



## A.4 Helper Functions & “Handles Only”

-   Never accept or return aggregate literals across your wrapper API. Use:
    
    -   `_C.__mem.box("Struct", { … })` to allocate/initialize,
        
    -   `_C.__mem.getf/setf` for field access,
        
    -   `_C.__mem.gc(ptr, "free" | {sym, lib?})` for lifetimes.
        
-   If the C function **returns an aggregate by value**, your user receives a **handle** (as mandated by the FFI). Document cleanup if needed.
    

----------

## A.5 SDL Example (Sketch)

```mindscript
# sdl.ms — wrapper exposing SDL2

let _C = ffiOpen({
  version: "1",
  lib: "libSDL2-2.0.so.0",
  types: {
    i32:{kind:"int",bits:32,signed:true},
    u32:{kind:"int",bits:32,signed:false},
    u8:{kind:"int",bits:8,signed:false},
    SDL_Window:{kind:"pointer",to:{kind:"void"},tag:"sdl.window*"},
    SDL_Renderer:{kind:"pointer",to:{kind:"void"},tag:"sdl.renderer*"},
    charp:{kind:"pointer",to:{kind:"int",bits:8,signed:true}},
    Rect:{kind:"struct",fields:[
      {name:"x",type:"i32"},{name:"y",type:"i32"},
      {name:"w",type:"i32"},{name:"h",type:"i32"}
    ]},
    Event:{kind:"struct",fields:[
      {name:"type",type:"u32"},
      {name:"_pad",type:{kind:"array",of:"u8",len:252}}
    ]}
  },
  functions: [
    {name:"SDL_Init",ret:"i32",params:["u32"]},
    {name:"SDL_Quit",ret:{kind:"void"},params:[]},
    {name:"SDL_CreateWindow",ret:"SDL_Window",
     params:["charp","i32","i32","i32","i32","u32"]},
    {name:"SDL_DestroyWindow",ret:{kind:"void"},params:["SDL_Window"]},
    {name:"SDL_CreateRenderer",ret:"SDL_Renderer",params:["SDL_Window","i32","u32"]},
    {name:"SDL_DestroyRenderer",ret:{kind:"void"},params:["SDL_Renderer"]},
    {name:"SDL_SetRenderDrawColor",ret:"i32",params:["SDL_Renderer","u8","u8","u8","u8"]},
    {name:"SDL_RenderClear",ret:{kind:"void"},params:["SDL_Renderer"]},
    {name:"SDL_RenderFillRect",ret:"i32",params:["SDL_Renderer",{kind:"pointer",to:"Rect"}]},
    {name:"SDL_RenderPresent",ret:{kind:"void"},params:["SDL_Renderer"]},
    {name:"SDL_GetTicks",ret:"u32",params:[]},
    {name:"SDL_PumpEvents",ret:{kind:"void"},params:[]},
    {name:"SDL_PollEvent",ret:"i32",params:[{kind:"pointer",to:"Event"}]}
  ]
})

# Public API
let version = "sdl2@wrap-0.1.0"
let INIT_VIDEO = 32
let WINDOW_SHOWN = 4
let RENDERER_ACCELERATED = 2
let RENDERER_PRESENTVSYNC = 4
let QUIT = 256

# Aliases (strip SDL_ → camelCase)
let init            = _C.SDL_Init
let quit            = _C.SDL_Quit
let getTicks        = _C.SDL_GetTicks
let pumpEvents      = _C.SDL_PumpEvents
let pollEvent       = _C.SDL_PollEvent
let setDrawColor    = _C.SDL_SetRenderDrawColor
let renderClear     = _C.SDL_RenderClear
let renderFillRect  = _C.SDL_RenderFillRect
let renderPresent   = _C.SDL_RenderPresent

# Helpers (attach destructors; aggregates via handles)
let createWindow = fun(title, x, y, w, h, flags) {
  let win = _C.SDL_CreateWindow(title, x, y, w, h, flags)
  if win == null then fail("createWindow: SDL_CreateWindow returned null") end
  _C.__mem.gc(win, { sym:"SDL_DestroyWindow" })
  win
}
let createRenderer = fun(win, index, flags) {
  let ren = _C.SDL_CreateRenderer(win, index, flags)
  if ren == null then fail("createRenderer: SDL_CreateRenderer returned null") end
  _C.__mem.gc(ren, { sym:"SDL_DestroyRenderer" })
  ren
}
let makeRect = fun(x, y, w, h) { _C.__mem.box("Rect", { x:x, y:y, w:w, h:h }) }
let makeEvent = fun() { _C.__mem.box("Event", null) }

let close = _C.close

```

**Consumer**

```mindscript
let SDL = import("sdl")
if SDL.init(SDL.INIT_VIDEO) != 0 then fail("SDL init failed") end
let win = SDL.createWindow("Demo", 100, 100, 800, 600, SDL.WINDOW_SHOWN)
let ren = SDL.createRenderer(win, -1, SDL.RENDERER_ACCELERATED + SDL.RENDERER_PRESENTVSYNC)
# …
SDL.close()

```



## A.6 Error, Lifetime, and Concurrency Guidance

-   **Null returns:** immediately `fail("context: message")` or return `Null` with documented semantics.
    
-   **Ownership:** when your wrapper _owns_ a pointer, attach a destructor via `_C.__mem.gc`.
    
-   **Aggregate discipline:** wrapper APIs must **only** pass/return **handles** for aggregates.
    
-   **Concurrency:** `ffiOpen` state is per-interpreter; for parallelism, `Clone()` and open in the clone.
    


## A.7 Publishing Checklist

-   Strip common prefix; camelCase aliases.
    
-   Export stable constants/types/helpers; include `version`.
    
-   Enforce **handles-only** for aggregates at your wrapper boundary.
    
-   Attach destructors for owned resources.
    
-   Check nulls/errno; provide clear errors.
    
-   Export `close`.
    
-   Pin `spec.lib` (soname) and document supported platforms.
    
-   Smoke tests: load, call, aggregate param, aggregate return, close.
    

----------

**Rationale:** This mirrors industry practice (e.g., Rust `-sys` + safe layer, Python `ctypes` + wrapper, LuaJIT FFI + Lua module, Node N-API + JS façade): a thin raw binding wrapped by a **curated, idiomatic** module, preserving safety and predictable lifetimes.