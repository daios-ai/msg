
Here is a revised beginner-friendly manual that incorporates your points: jargon is kept but briefly explained, there is a clear “golden rule,” a few concrete `__mem` helpers are referenced, and the reference section is more complete and uses a small table.

---

# `ffiOpen` — Calling C Libraries from MindScript

`ffiOpen` lets MindScript talk to existing C libraries. You describe the parts of a C API you need, and `ffiOpen` turns that description into a MindScript module with callable functions and helper tools. That way you can reuse battle-tested native code instead of rewriting it, while keeping most of your work in MindScript.

This manual focuses on practical usage. It assumes you already know roughly what a C library is (a shared `.so` or `.dylib` or `.dll` with exported functions), but not how to call it safely from MindScript. You will see how to write a small “spec” (a description of types and functions), how to call those functions, how to work with C structs through handles, and how to wrap everything into a friendly module that hides the FFI plumbing.

“FFI” stands for “foreign function interface”: a way for MindScript to call functions written in another language (here, C), without rewriting them.

---

## 1. What `ffiOpen` Is and When to Use It

At its core, `ffiOpen` does one thing: it takes a MindScript map called a “spec,” which describes a native library, and returns a module that you can call from MindScript. The spec says which shared library file to open, which C types you care about, and which functions and globals you want to use. Once you have that module, you can write ordinary MindScript code that calls into native code.

You reach for `ffiOpen` when you want to call functions from an existing C library. That might be the standard C library, a math library, a database client, a graphics engine, or any other shared library present on your system. In small scripts you can call `ffiOpen` directly. In larger projects you usually wrap it in a dedicated module so most of your code sees a clean, MindScript-style API.

In FFI terms, MindScript is the “host” language and C is the “foreign” language. `ffiOpen` is the bridge between them.

---

## 2. First Steps: Your Very First `ffiOpen` Call

The simplest way to learn `ffiOpen` is to call a function you already know. Here is a complete example that calls `hypot` from the C math library to compute the hypotenuse of a right triangle:

```mindscript
let C = ffiOpen({
	version: "1",
	lib: "libm.so.6",
	types: {
		double: { kind: "float", bits: 64 }
	},
	functions: [
		{
			name: "hypot",
			ret: "double",
			params: ["double", "double"]
		}
	]
})

let r = C.hypot(3.0, 4.0)
println(r) # about 5.0
```

The C declaration for `hypot` in a header file looks like:

```c
double hypot(double x, double y);
```

In the spec you mirror that:

* You define a `double` type as a 64-bit floating-point number.
* You define a `hypot` function that returns a `double` and takes two `double` parameters.

After `ffiOpen` runs, `C.hypot` behaves like a normal MindScript function: you pass numbers in and you get a number out.

If you pass arguments that cannot be converted to the declared types, the call fails with a clear error instead of silently corrupting memory. As long as you keep the spec aligned with the C library’s header files, you can treat these calls like any other function in your code.

---

## 3. The Shape of a Spec

A spec is just a MindScript map with a few conventional keys. The outermost shape usually looks like this:

```mindscript
let spec = {
	version: "1",
	lib: "libm.so.6",

	types: {
		double: { kind: "float", bits: 64 }
	},

	functions: [
		{
			name: "hypot",
			ret: "double",
			params: ["double", "double"]
		}
	],

	variables: [
		# optional; often empty at first
	]
}
```

The `version` field says which spec format you are using. For now you always write `"1"`, and any other value is rejected. This allows the FFI to evolve without silently misinterpreting old specs.

The `lib` field gives the default shared library to open. On many Linux systems `libm.so.6` is the system math library, `libc.so.6` is the C standard library, and so on. Individual functions and variables can override the library if needed, but most small specs keep a single `lib` at the top.

The `types` map introduces names like `"double"` or `"int32"` and gives each a description. These names are later used in `ret`, `params`, and variable declarations. You do not have to describe every type in a library, only the ones you care about. If a type is used only once, you can inline it directly in the `functions` entry, but naming it in `types` usually makes the spec easier to read.

The `functions` array lists the C functions you want. Each entry gives the C symbol name (`name`), its return type (`ret`), and the types of each parameter (`params`). For simple scalar functions, this is all you need.

The `variables` array describes global variables you want to access, such as `errno` or other global state. Many first-time users leave this empty and come back to it when they need it.

---

## 4. Calling C Functions from MindScript

Once you have a spec, you call `ffiOpen(spec)` and get back a module. Each function entry in the spec becomes a field on that module, and you call it like any other MindScript function.

Here is a slightly richer example that mixes integers and floats:

```mindscript
let C = ffiOpen({
	version: "1",
	lib: "libm.so.6",
	types: {
		double: { kind: "float", bits: 64 },
		int32:  { kind: "int",   bits: 32, signed: true }
	},
	functions: [
		{ name: "hypot", ret: "double", params: ["double", "double"] },
		{ name: "abs",   ret: "int32",  params: ["int32"] }
	]
})

let h = C.hypot(5.0, 12.0)
let a = C.abs(-42)

println(h)
println(a)
```

`C.hypot` expects two floating-point values and returns a floating-point number. `C.abs` expects a 32-bit signed integer and returns a 32-bit signed integer. On the MindScript side both functions receive and return normal MindScript values: `Num` for floating-point values and `Int` for integers, just like any other MindScript function.

If you try to pass an argument that does not fit the declared type (for example, something too large to fit in 32 bits), the call fails with a clear error. This gives you a safety net: as long as you are faithful to the C declarations, the bridge enforces the boundary for you.

---

## 5. Strings and Other Simple Values

Most C libraries use three simple kinds of scalar types at their boundaries:

* integers,
* floating-point numbers,
* pointers.

Integers and floats are described with `"int"` and `"float"` kinds in your spec, as you have seen.

Strings in C are usually represented as a pointer to `char`, which is an 8-bit integer type. To pass a MindScript string into such a function, you model the parameter as a pointer to an 8-bit integer, and `ffiOpen` takes care of turning your MindScript `Str` into a temporary NUL-terminated buffer.

Here is an example that calls `puts` from the C standard library:

```mindscript
let C = ffiOpen({
	version: "1",
	lib: "libc.so.6",
	types: {
		charp: {
			kind: "pointer",
			to: { kind: "int", bits: 8, signed: true }
		}
	},
	functions: [
		{
			name: "puts",
			ret: { kind: "int", bits: 32, signed: true },
			params: ["charp"]
		}
	]
})

let _ = C.puts("Hello from MindScript")
```

When a function parameter has a pointer type like `"charp"` above, you can normally pass a MindScript string. The bridge builds a temporary native buffer that exists just for that single call; you do not need to free it and you should not try to keep a pointer to it afterwards.

Some C functions return `char*`. Depending on how a particular binding is written, you may choose to:

* receive that as a pointer handle, and then use a helper like `C.__mem.string(ptr, null)` to turn it into a MindScript `Str`, or
* arrange for the binding to convert it for you and return `Str` directly.

Wrapper modules often make that choice on your behalf and expose a clean MindScript function that simply returns `Str`.

---

## 6. Pointers and Aggregates as Handles

C code uses pointers and aggregate types such as structs, unions, and fixed-size arrays to represent more complex data. MindScript does not expose raw pointer values; instead, `ffiOpen` wraps native pointers and aggregate storage in opaque values called handles.

A handle is just an ordinary MindScript value, but its contents are managed by the FFI. You cannot add to it or inspect its numeric address; instead, you pass it back to C functions that expect pointers or by-value aggregate parameters, and you use helper functions to work with its fields or bytes when needed.

The important mental shift is this:

> **Golden rule:** whenever a function in your spec refers to a struct, union, or fixed-size array, that parameter or return value appears in MindScript as a handle, not as a map or array.

In other words:

* You allocate storage for aggregates once (for example with `C.__mem.box`).
* You pass the handle to that storage whenever a C function needs to read or write it.
* You never pass a struct or array literal directly at the call site.

Later sections show how to allocate and use such storage through the `__mem` helpers.

---

## 7. Allocating and Using Structs with `__mem`

The module returned by `ffiOpen` has a field named `__mem`. This is a small toolbox for working with native memory: allocating and initializing aggregates, reading and writing fields, turning pointers into strings, and attaching destructors. For beginners, two of the most approachable helpers are:

* `box`, which allocates storage for an aggregate type and optionally initialises it from a MindScript map, and
* `getf` / `setf`, which read and write struct fields via a handle.

Imagine the following C struct:

```c
struct Point {
    int x;
    int y;
};
```

You can describe it in your spec and write a function that moves a point by an offset:

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
		PointPtr: {
			kind: "pointer",
			to: "Point"
		}
	},
	functions: [
		{
			name: "move_point",
			ret: { kind: "void" },
			params: ["PointPtr", "int32", "int32"]
		}
	]
})
```

To use it from MindScript, you allocate storage for a `Point`, get back a handle, and then pass that handle to `move_point`:

```mindscript
# Allocate storage for one Point and initialize it.
let p = C.__mem.box("Point", { x: 10, y: 20 })

# Move it by (5, -3).
let _ = C.move_point(p, 5, -3)

# Read the fields back.
let x = C.__mem.getf("Point", p, "x")
let y = C.__mem.getf("Point", p, "y")

println(sprintf("Point is now at (%d,%d)", [x, y]))
```

Here `C.__mem.box("Point", {...})` allocates native memory that fits a `Point`, sets its `x` and `y` fields from the map you give it, and returns a handle named `p`. You then pass `p` into `C.move_point`, which treats it as a pointer argument. After the call, you use `C.__mem.getf` to peek at the updated fields.

There is also a symmetric `C.__mem.setf("Point", p, "x", 42)` if you want to update a field directly from MindScript. In all cases, you never pass `{ x: 10, y: 20 }` directly into `move_point`. The literal is only used at allocation time. Once you have a handle, that handle is the “ticket” you hand to the FFI whenever a C function wants to touch that storage.

---

## 8. Managing Lifetimes Safely

Native memory does not automatically go away when a handle becomes unreachable. Some libraries allocate with one function and require you to free with another; others expect you to keep a pointer valid for as long as C might use it. To keep this manageable, it is common to centralise allocation and cleanup in a few helper functions.

Suppose a C library offers a pair of functions like these:

```c
Widget* widget_new(void);
void widget_free(Widget* w);
```

Your spec might look like this:

```mindscript
let C = ffiOpen({
	version: "1",
	lib: "libwidget.so",
	types: {
		Widget: {
			kind: "pointer",
			to: { kind: "void" },
			tag: "widget*"
		}
	},
	functions: [
		{ name: "widget_new",  ret: "Widget", params: [] },
		{ name: "widget_free", ret: { kind: "void" }, params: ["Widget"] }
	]
})
```

You can write a small helper that creates a widget, checks the result, and arranges for it to be freed later:

```mindscript
let newWidget = fun() -> Any do
	let w = C.widget_new()
	if w == null then
		panic("widget_new failed")
	end

	# Attach a destructor so widget_free runs automatically when w is no longer used.
	let _ = C.__mem.gc(w, { sym: "widget_free" })

	w
end
```

Your application code simply calls `newWidget()` and passes the handle around. The details of how and when `widget_free` runs stay in one place, and you can change them later without affecting the rest of your code.

For long-running programs it is still wise to think explicitly about lifetimes. When you know you are done with a resource, you can offer a `close` or `destroy` helper that calls the underlying C function eagerly and clears any MindScript references. The important part is that all of this logic lives in your wrapper layer, not scattered across the application.

---

## 9. Building a Wrapper Module on Top of `ffiOpen`

In medium and large projects it quickly becomes uncomfortable to call `ffiOpen` everywhere and work directly with raw C names. A much nicer approach is to hide the spec and the FFI inside a dedicated module and export a handful of friendly functions.

Here is a very small example that wraps a hypothetical `mylib_add` function:

```mindscript
# mylib.ms — wrapper around a native C library

let _C = ffiOpen({
	version: "1",
	lib: "libmylib.so",
	types: {
		int32: { kind: "int", bits: 32, signed: true }
	},
	functions: [
		{
			name: "mylib_add",
			ret: "int32",
			params: ["int32", "int32"]
		}
	]
})

# Public function with a clean name and signature.
let add = fun(a: Int, b: Int) -> Int do
	_C.mylib_add(a, b)
end
```

From application code, this is now just:

```mindscript
let mylib = import("mylib")

println(mylib.add(2, 3))
```

You can apply the same pattern to more complex libraries. The wrapper file is where you define convenience functions, attach destructors, choose naming conventions (for example, stripping a common prefix like `mylib_` and using `camelCase`), and decide which details to expose. Most of your project can then pretend it is just using another MindScript module.

---

## 10. Common Pitfalls and How to Avoid Them

A few issues come up often when people start using `ffiOpen` for the first time.

One common mistake is to pass a map or array literal where the spec expects an aggregate type. For example, this is wrong:

```mindscript
# WRONG: passing a literal where a PointPtr is expected
let _ = C.move_point({ x: 10, y: 20 }, 5, -3)
```

If a parameter type in your spec is a struct, union, or fixed-size array, either directly or through a pointer type, you must pass a handle that points to allocated storage. The literal is used only when you create that storage, usually through a helper like:

```mindscript
let p = C.__mem.box("Point", { x: 10, y: 20 })
let _ = C.move_point(p, 5, -3)
```

Another source of trouble is a mismatch between the C header and your spec. If the C declaration says `double` and your spec says 32-bit float, you may get bizarre results or a hard error. It is worth keeping the header file open while writing the spec, and updating both together when the C library changes. A small sanity test that calls a simple function (like `hypot` or an `add`) and checks the result can catch spec mistakes early.

Lifetimes can also trip you up. When a function returns a pointer, think about who owns the memory it points to. Some functions return pointers into internal, static storage; those should never be freed. Others return newly allocated buffers that you are expected to free. The safest habit is to wrap such functions with helpers that immediately attach the correct destructor, and to avoid exposing the raw FFI to the rest of your code.

Finally, resist the urge to tackle complex callbacks, variadic functions, and large nested structs on day one. Start with a small spec for a handful of simple scalar functions. Once those work and you are comfortable with specs and handles, extend your spec gradually.

---

## 11. Short Reference

This section summarises the pieces you work with most often when using `ffiOpen`. It is not a full design specification, but it should be enough to remind you of shapes and names while coding.

### 11.1 Top-level usage

```mindscript
let C = ffiOpen(spec)
```

`spec` is a MindScript map. At minimum it should have:

* `version: Str` — currently `"1"`.
* `lib: Str` — default shared library name or path, e.g. `"libm.so.6"`.

It usually also has `types`, `functions`, and sometimes `variables`.

### 11.2 Spec fields at a glance

|       Field |   Type | Required | Description                             |
| ----------: | -----: | :------: | --------------------------------------- |
|   `version` |  `Str` |    yes   | Spec format version, currently `"1"`.   |
|       `lib` |  `Str` |    yes   | Default shared library to open.         |
|     `types` |   `{}` |    no    | Map from type name to type description. |
| `functions` | `[{}]` |    no    | Array of function declarations.         |
| `variables` | `[{}]` |    no    | Array of global variable declarations.  |

A minimal spec might be:

```mindscript
let spec = {
	version: "1",
	lib: "libm.so.6",
	types: {},
	functions: []
}
```

In practice, `types` and `functions` are almost always non-empty.

### 11.3 Common type descriptions

Types appear inside `spec.types`, inside `functions[i].ret`, and inside `functions[i].params`.

* 32-bit signed integer:

  ```mindscript
  { kind: "int", bits: 32, signed: true }
  ```

* 64-bit floating-point number:

  ```mindscript
  { kind: "float", bits: 64 }
  ```

* Pointer to a named type (for example, `Point`):

  ```mindscript
  { kind: "pointer", to: "Point" }
  ```

* Pointer to 8-bit signed integer (often used for C strings):

  ```mindscript
  {
  	kind: "pointer",
  	to: { kind: "int", bits: 8, signed: true }
  }
  ```

* Simple struct with named fields:

  ```mindscript
  {
  	kind: "struct",
  	fields: [
  		{ name: "x", type: "int32" },
  		{ name: "y", type: "int32" }
  	]
  }
  ```

You can either refer to another named type (like `"int32"`) or inline the full description, as in the `charp` example earlier.

### 11.4 Function and variable entries

A function entry in `functions` has at least:

```mindscript
{
	name: "hypot",
	ret: "double",
	params: ["double", "double"]
}
```

* `name: Str` — C symbol name to look up.
* `ret: TypeRef` — return type, either a name from `types` or an inline type description.
* `params: [TypeRef]` — array of parameter types.

Each entry becomes a callable field on the module returned by `ffiOpen`:

```mindscript
let C = ffiOpen(spec)
let r = C.hypot(3.0, 4.0)
```

Variable entries (for globals) look similar but typically expose `.get()`, `.set()`, and `.addr()` helpers on the returned module. Those are less commonly used in beginner code and are usually wrapped inside a module that hides the raw global.

### 11.5 The `__mem` toolbox (selected helpers)

Every FFI module returned by `ffiOpen(spec)` has a `__mem` field:

```mindscript
let C = ffiOpen(spec)
let M = C.__mem
```

This is a small helper module focused on pointers and aggregates. A few helpers you are likely to use early on:

* Allocate and optionally initialise an aggregate, return a handle:

  ```mindscript
  C.__mem.box(T: Str, init: {}?)
    -> Any  # handle
  ```

  Example:

  ```mindscript
  let p = C.__mem.box("Point", { x: 10, y: 20 })
  ```

* Read and write struct fields via a handle:

  ```mindscript
  C.__mem.getf(T: Str, h: Any, field: Str) -> Any
  C.__mem.setf(T: Str, h: Any, field: Str, v: Any) -> Any
  ```

  Example:

  ```mindscript
  let x = C.__mem.getf("Point", p, "x")
  let _ = C.__mem.setf("Point", p, "x", x + 1)
  ```

* Attach a destructor (finalizer) to a handle:

  ```mindscript
  C.__mem.gc(h: Any, finalizer: Any) -> Any
  ```

  The `finalizer` is usually a small map describing which C function to call, for example `{ sym: "widget_free" }`.

* Turn a C string pointer into a MindScript `Str`:

  ```mindscript
  C.__mem.string(ptr: Any, len: Int?) -> Str
  ```

  If `len` is `null`, `string` reads bytes until the first NUL character.

The full memory toolbox also includes helpers for computing sizes and offsets of types (`sizeof`, `alignof`, `offsetof`) and for low-level byte operations (`copy`, `fill`). Those become important when you write deeper bindings, but for many beginner use cases `box`, `getf`, `setf`, `gc`, and `string` are enough.

With these pieces in hand, you can gradually bring existing C libraries into your MindScript projects: begin with a tiny spec and a scalar function, then add pointers and structs via handles, and finally wrap everything in a module that hides the FFI details behind a clean, MindScript-style API.
