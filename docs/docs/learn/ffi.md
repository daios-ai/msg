# Foreign Function Interface

MindScript is happiest when most of your program lives in ordinary values—strings, numbers, arrays, and objects—because that keeps scripts portable and easy to reason about. Sometimes, though, the thing you want already exists as a mature C library: a codec, a database client, a numerical routine, a device API, or a legacy SDK. The foreign function interface (FFI) is the bridge for that situation.

In MindScript, the entry point is `ffiOpen(spec)`. You give it a *specification*—a plain MindScript object that describes the C library surface you want to call—and you get back a MindScript module. Functions become callable values. Global variables become small objects with `get()`, `set(value)`, and `addr()` accessors. Native pointers and aggregate storage become opaque `Handle` values so you can’t accidentally treat a pointer like an integer and corrupt memory.

The FFI is powerful, but it is also where the usual safety guarantees end. The runtime can type-check and range-check conversions, but it cannot prove your spec matches the real C header, nor can it prevent you from freeing memory too early if you design the wrapper incorrectly. This chapter focuses on writing small, correct bindings, and on building a wrapper layer so the rest of your code never needs to touch raw FFI details.

---

## Availability and platform assumptions

The FFI is not a pure-language feature: it depends on the host platform and the availability of `libffi` and dynamic loading (`dlopen`/`dlsym`). In practice it is intended for systems where that stack is available (commonly macOS and Linux builds with cgo enabled). If your build does not include FFI support, `ffiOpen` will not be present or will fail early.

Even when it is available, you should assume the FFI follows the platform ABI rules. That matters for “exotic” types (packed bitfields, large structs passed by value, variadic functions, and callbacks), because ABI mismatches produce the worst kind of bug: code that sometimes appears to work. The safest workflow is to begin with a trivial scalar function binding and only expand once you have a passing sanity test.

---

## The mental model: a spec becomes a module

A spec is a MindScript object that declares three things:

1. which shared library to load,
2. which C types to name and reuse, and
3. which exported symbols to bind (functions and variables).

`ffiOpen(spec)` returns a module with a predictable shape. Besides your declared symbols, it exposes bookkeeping fields (such as the loaded library name and a handle to the dynamic library) and a memory toolbox under `__mem`. The toolbox is where allocation, casting, struct field access, and string decoding live. Keeping these operations behind `__mem` is not cosmetic—it enforces a boundary between “ordinary MindScript values” and “native memory objects,” which makes it harder to accidentally treat a pointer like data.

The single most important design choice in this FFI is that native pointers and aggregate storage are *not* numbers. They are handles. You pass them back into FFI calls and into `__mem` helpers, but you do not inspect or compute with them.

---

## Your first binding: calling a scalar function

A good first binding is one with only integers or floating-point values. You want the simplest possible end-to-end path: open the library, call one function, compare to a known result.

Here is a complete binding for `hypot`, which on most Unix-like systems is provided by the math library:

```mindscript
let C = ffiOpen({
    version: "1",
    lib: "libm.so.6",

    types: {
        double: { kind: "float", bits: 64 }
    },

    functions: [
        { name: "hypot", ret: "double", params: ["double", "double"] }
    ]
})

let r = C.hypot(3.0, 4.0)
println(r)   # about 5.0
```

The technical justification for starting here is that scalar calls have the least surface area for ABI mistakes. If you declare a 64-bit float and pass a MindScript `Num`, the runtime can marshal that into the exact representation the C function expects. If you get a surprising result, it is usually because the symbol name is wrong, the library is not the one you think it is, or the type declaration does not match the C signature.

Once this works, you have proven that dynamic loading works, symbol lookup works, and call marshalling works on your machine. That baseline makes later debugging dramatically easier.

---

## The spec format and why it is explicit

The top-level object must include `version: "1"` and a default `lib`. The version is not decoration: it exists to prevent silent reinterpretation of specs as the format evolves. If a future runtime changes how type descriptions are interpreted, requiring an explicit version forces you to opt in instead of inheriting a subtle mismatch.

The `types` map is optional, but in practice you should use it. Naming types is not just for readability; it reduces the risk of copying a slightly different definition into multiple places. A single inconsistent `int` width is enough to turn a correct-looking spec into an incorrect call boundary.

The `functions` list binds C function symbols. Each entry is a declarative restatement of the C prototype: return type and parameter types. The runtime uses that declaration to marshal MindScript values into C arguments and to marshal the C return value back into MindScript.

The `variables` list binds global data symbols. Those bindings do not behave like plain values, because C globals live in native memory and can be mutated externally. Instead, each global becomes an object that you can query and set via explicit methods. That explicitness is important: it makes it clear you are crossing an FFI boundary every time you touch the value.

---

## Integers, floats, and range safety

When you bind an integer type, you must specify its width and signedness. MindScript integers are conceptually unbounded at the language level, but the FFI boundary is not. C wants a fixed-width value in a particular register or stack slot. The runtime therefore must check whether your `Int` fits into the declared width before performing the conversion.

This is not pedantry; it is memory safety. If you pass `1_000_000_000_000` as a 32-bit signed integer without range checks, the value will wrap, and the C function will see a different number than your program believes it passed. With range checks, the call fails loudly instead of producing a wrong computation or corrupting memory.

Floating-point bindings work similarly, except the “range” question is mostly about representability. A 64-bit float is the usual safe choice for interoperability, because most C libraries expose `double` in their public APIs.

---

## Strings: `Str` as bytes and `char*` boundaries

C “strings” are almost always pointers to bytes with a terminating NUL character (`'\0'`). MindScript strings (`Str`) are also used as byte containers throughout the runtime (for file data, HTTP bodies, gzip buffers, digests), which makes them a natural bridge type.

When a C function expects a `char*` (or `unsigned char*`) parameter, the FFI can accept a MindScript `Str` and produce a temporary NUL-terminated buffer for the duration of the call. The key phrase is “for the duration of the call.” If the C library stores the pointer and uses it later, passing a temporary buffer is incorrect. In that case you must allocate stable storage yourself (typically with `__mem`) and pass a pointer handle whose lifetime you control.

A minimal example with `puts` looks like this:

```mindscript
let C = ffiOpen({
    version: "1",
    lib: "libc.so.6",

    types: {
        int32: { kind: "int", bits: 32, signed: true },
        charp: { kind: "pointer", to: { kind: "int", bits: 8, signed: true } }
    },

    functions: [
        { name: "puts", ret: "int32", params: ["charp"] }
    ]
})

C.puts("Hello from MindScript")
```

You should read this spec the way a C compiler would: the function takes a pointer to 8-bit integer data. The only “string” semantics are the convention that the memory is NUL-terminated, which is why the temporary buffer is needed.

Return values are trickier. Some C functions return `char*` that you must free, some return `char*` that points into static storage and must not be freed, and some return `char*` that is only valid until the next call. The FFI can optionally convert certain `char*` returns into MindScript `Str` directly (when you mark the binding accordingly), but you still need to understand the ownership rule from the C library documentation. Converting to `Str` does not solve the ownership problem; it only chooses when the bytes are copied.

---

## Handles: pointers and aggregates are not maps and arrays

A C struct is not a MindScript object. A pointer is not a number. If you try to model native memory with ordinary MindScript composites, you lose the ability to enforce correct layout and lifetime, and you invite ABI mismatches.

The FFI therefore uses `Handle` values to represent native pointers and aggregate storage. A handle is opaque: you can pass it back into FFI calls, and you can hand it to `__mem` operations, but you cannot iterate it, index it, or inspect its address. This is a deliberate safety constraint.

The most practical consequence is this rule:

If a function’s parameter is declared as a struct, union, or fixed-size array (either by value or behind a pointer), you must pass a handle that refers to properly allocated storage. You cannot pass `{x: 1, y: 2}` where C expects `struct Point*`. The only safe place for a map literal is as an initializer *when allocating native storage*.

---

## `__mem`: allocating memory, reading fields, and casting

Every module produced by `ffiOpen` contains a `__mem` submodule. Think of it as “the small, sharp tools” you use to operate on native memory.

For everyday bindings, four operations matter first: allocation, struct field access, conversion of pointer-to-bytes into `Str`, and lifetime management.

### Working with a struct through a handle

Suppose a C library defines:

```c
struct Point { int32_t x; int32_t y; };
void move_point(struct Point* p, int32_t dx, int32_t dy);
```

You bind the type and the function, then allocate a `Point` with `__mem.box`, pass the resulting handle, and read back the fields:

```mindscript
let C = ffiOpen({
    version: "1",
    lib: "libpoints.so",

    types: {
        int32: { kind: "int", bits: 32, signed: true },

        Point: {
            kind: "struct",
            fields: [
                { name: "x", type: "int32" },
                { name: "y", type: "int32" }
            ]
        },

        PointPtr: { kind: "pointer", to: "Point" }
    },

    functions: [
        { name: "move_point", ret: { kind: "void" }, params: ["PointPtr", "int32", "int32"] }
    ]
})

let p = C.__mem.box("Point", { x: 10, y: 20 })
C.move_point(p, 5, -3)

let x = C.__mem.getf("Point", p, "x")
let y = C.__mem.getf("Point", p, "y")
println(sprintf("(%d,%d)", [x, y]))
```

The justification for this pattern is layout correctness. `__mem.box("Point", ...)` allocates a correctly-sized, correctly-aligned chunk of native memory that matches the struct declaration in your spec. `getf` and `setf` then compute field offsets from that same declaration, so you don’t have to manually calculate or hard-code byte offsets.

If you reach for raw `copy` and pointer arithmetic too early, you reintroduce the problems the FFI is trying to prevent.

### The difference between “pointer to bytes” and “text”

`__mem.string(ptr, len?)` exists because a pointer handle is not usable by itself. You often need to interpret the bytes it points to as a MindScript `Str`. When `len` is `null`, it typically reads until a NUL byte. When you know a length, passing it avoids scanning and avoids accidentally reading past the end.

Because MindScript `Str` can represent raw bytes, you can then render it safely using `hexEncode` or `base64Encode` when the bytes are not guaranteed to be printable.

---

## Ownership and lifetimes: the part you must design

C APIs rarely manage memory the way a garbage-collected language does. Some functions allocate; others return borrowed pointers; others require you to pass in buffers they fill. If you do not encode the ownership rule into your wrapper, you will eventually free too early or leak forever.

The FFI provides two complementary mechanisms: explicit allocation/free through `__mem` (such as `malloc`, `free`, `new`, and `box`) and an attachment mechanism `gc(ptr, finalizer)` that arranges for cleanup when the handle becomes unreachable.

Here is the simplest correct pattern for “malloc + free” ownership: allocate, attach a finalizer, and only then return the handle to the rest of your program.

```mindscript
let C = ffiOpen({
    version: "1",
    lib: "libc.so.6",
    types: {
        size: { kind: "int", bits: 64, signed: false }
    },
    functions: []
})

let allocBytes = fun(n: Int) -> Any do
    let p = C.__mem.malloc(n)
    if p == null then
        panic("malloc failed")
    end

    # Arrange to free it later. The string "free" is a common shorthand finalizer.
    C.__mem.gc(p, "free")
    p
end
```

This is not just convenience; it is correctness. The moment you return a raw pointer handle without a clear ownership rule, every caller must guess whether to free it, and different callers will guess differently. Centralizing the rule in one allocation function makes the rest of the code safe by default.

Some libraries require a custom destructor, not `free`. In that case the finalizer must call the correct symbol, and your wrapper should export a `close()` or `destroy()` function for eager cleanup when that matters.

The `ffiOpen` module itself also owns resources: dynamic library handles, libffi call interfaces, and callback closures. That is why the module exposes a `close()` function. Closing twice is treated as a misuse, because after close the module cannot safely service calls.

---

## Variadic functions and why they are special

Variadic C functions (such as `printf`) are dangerous to bind directly because C applies default promotions to variadic arguments (for example, smaller integer types may be promoted to `int`, and floats are promoted to `double`). The FFI supports variadic calls, but it needs a disciplined way to separate “fixed parameters” from “variadic tail.”

The rule enforced by this FFI is that the final argument for a variadic binding must be an array holding the variadic arguments. This makes the boundary explicit and keeps the call marshaling deterministic. You still need to ensure your types match the promoted forms that the C function expects, because C will not tell you you got it wrong—it will just read the wrong bytes.

If you are new to FFI, avoid variadic functions until you have ordinary fixed-signature bindings working and tested.

---

## Callbacks and function pointers: powerful, but not day one

Some C APIs accept function pointers as callbacks. The FFI can build a native closure that calls back into a MindScript function. That feature is real—and extremely useful—but it is also where lifetimes become subtle, because C might call your callback long after the original MindScript call has returned.

The safe pattern is the same as for other ownership problems: encapsulate callback creation inside a wrapper that pins the callback handle for at least as long as C might call it, and provide an explicit `close` operation that unregisters the callback before releasing memory. The FFI’s closure machinery is capable, but it cannot guess the lifetime rules of the library you bind.

Unless you have a pressing need, treat callbacks as an advanced topic and keep your first bindings to “call in, get result out” APIs.

---

## Global variables

When you bind a global variable symbol, the module exports an object with methods rather than exporting the value directly. That design prevents a common mistake: reading a global once and then assuming it stays in sync.

A global wrapper typically supports `get()` to read the current value, `set(value)` to write it, and `addr()` to obtain its address as a pointer handle when an API expects a pointer to that global.

In wrapper code, you should usually hide these behind named functions so callers never directly manipulate global state unless they truly have to.

---

## Wrapping the raw bindings into a MindScript-friendly module

The best FFI code is the code most of your project never sees. A wrapper module serves three purposes.

First, it stabilizes naming. C APIs often use prefixes and underscores; MindScript projects typically use `camelCase`. Your wrapper can adapt names without changing the underlying binding.

Second, it stabilizes types. The wrapper can accept and return ordinary MindScript values, allocating native storage and copying bytes only where required.

Third, it stabilizes ownership. The wrapper is where you decide who frees what, and where you attach finalizers so callers don’t have to.

Here is a small template that illustrates the idea:

```mindscript
# mylib.ms

let _C = ffiOpen({
    version: "1",
    lib: "libmylib.so",
    types: {
        int32: { kind: "int", bits: 32, signed: true }
    },
    functions: [
        { name: "mylib_add", ret: "int32", params: ["int32", "int32"] }
    ]
})

let add = fun(a: Int, b: Int) -> Int do
    _C.mylib_add(a, b)
end

let close = fun() -> Null do
    _C.close()
end
```

Your application code imports `mylib` and never touches `_C` or the spec. If you later discover that the real C type was `int64_t` on one platform, you change the spec in one place, rerun your sanity tests, and the application remains unchanged.

---

## Debugging and sanity testing

With FFI, most failures are spec mismatches. A wrong integer width, a wrong signedness, or a wrong calling convention will not always fail immediately. The discipline that keeps you safe is to add a tiny test per binding that exercises one function with known inputs and checks a known output. When you move to structs, add a test that writes fields, calls a function, and then reads fields back. When you bind memory ownership, add a test that allocates and frees in a loop to shake out leaks early.

When something fails, prefer small, local proofs over global debugging. Confirm the library loads. Confirm the symbol resolves. Confirm the simplest call works. Only then add the next layer of complexity.

---

## Summary

`ffiOpen` is a spec-driven bridge from MindScript to C. Its central safety idea is that native pointers and aggregate storage are represented as opaque handles, and all native memory manipulation is routed through the `__mem` toolbox. That design does not remove the need to understand C ownership and ABI rules, but it gives you a place to encode those rules once, inside a wrapper module, so the rest of your program can remain ordinary MindScript.

If you keep two habits—write a small sanity test for each new binding, and wrap raw FFI surfaces behind a MindScript-style module—you can call native libraries productively without letting FFI complexity leak into your whole codebase.
