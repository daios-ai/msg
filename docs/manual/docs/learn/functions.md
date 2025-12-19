
<style>
  .my-code span.prompt { color: black; }
  .my-code span.note { color: green; }
  .my-code span.value { color: blue; }
</style>

# Functions

This chapter introduces **functions** in MindScript. Along the way, it introduces **type schemas**, because function calls and returns are **runtime-checked** against their declared types. If a value does not match a declared type, execution fails with a runtime error.

Type schemas are written using the `type` keyword. They are values of type `Type`, and you can pass them around like any other value.

---

## Type Schemas

A **type schema** describes the shape of a value. You can use schemas:

* on function parameters,
* on function return types,
* as named aliases for structured data.

Some basic examples:

```mindscript
type Str
type Int
type [Int]
type {name: Str, age: Int, hobbies: [Str]}
```

There are three core helpers:

* `typeOf(val) -> Type` returns the structural type of a runtime value.
* `isType(val, T: Type) -> Bool` checks whether `val` conforms to schema `T`.
* `isSubtype(A: Type, B: Type) -> Bool` checks whether `A` is a subtype of `B`.

<div class="my-code" markdown="0">
<pre><code><span class="prompt">==> isType(42, type Int)</span>
<span class="value">true</span>

<span class="prompt">==> isType({name: "John"}, type {name: Str})</span>
<span class="value">true</span>

<span class="prompt">==> isSubtype(type Int, type Num)</span>
<span class="value">true</span> </code></pre>

</div>

### Enumerated types

If a value must be one of a fixed set of literals, use an enum type:

```mindscript
type Enum["pending", "processing", "shipped", "delivered", "cancelled"]
```

Enum members must be **JSON literals** (null, booleans, numbers, strings, arrays/maps of literals).

Subtype behaves like “subset”:

```mindscript
let a = type Enum[1, 2, 3]
let b = type Enum[1, 2]

isSubtype(a, b)    ## false
isSubtype(b, a)    ## true
```

### Nullable types

A nullable type allows either a value of type `T` or `null`. Write this as `T?`.

```mindscript
let User = type {
    name!: Str,
    age!: Int,
    bio: Str?
}
```

Any type can be nullable:

```mindscript
let Tags = type [Str?]
```

This allows values like:

```mindscript
["sports", null, "international news", "movies"]
```

### Required fields in object schemas

In object schemas, **fields are optional by default**. This means:

```mindscript
let Person = type {
    name: Str
    age: Int
}

isType({name: "John", age: 45}, Person)  ## true
isType({}, Person)                        ## true
```

To require a field, add `!` after the field name:

```mindscript
let MusicRecord = type {
    title!: Str,            ## required
    artist!: Str,           ## required
    releaseYear: Int,       ## optional
    genre!: Str?,           ## required, may be null
}
```

```mindscript
let song1 = {
    title: "Carousel",
    artist: "Mr. Bungle",
    album: "Mr. Bungle",
    genre: null
}

let song2 = {
    title: "Yesterday",
    artist: "The Beatles",
    releaseYear: 1965
}

isType(song1, MusicRecord)  ## true
isType(song2, MusicRecord)  ## false (missing genre)
```

Note: object schemas are **open-world**: extra fields are allowed. The schema describes what must be present (and typed), not the complete set of keys.

### The universal type `Any`

`Any` is the universal schema: every value conforms to it.

<div class="my-code" markdown="0">
<pre><code><span class="prompt">==> isType(123, type Any)</span>
<span class="value">true</span>

<span class="prompt">==> isType({x: 1}, type Any)</span>
<span class="value">true</span> </code></pre>

</div>

Use `Any` when you genuinely don’t know the shape yet (or when building generic helpers), but prefer concrete schemas when you can.

### Type aliases

If you reuse a schema, bind it to a name:

```mindscript
let GeoPoint = type {
    lat!: Num,
    lng!: Num,
    label: Str?
}

let Territory = type {
    name!: Str,
    track!: [GeoPoint]
}
```

Type aliases are real values (of type `Type`), so you can pass them into functions, store them in modules, and convert them to JSON Schema when working with oracles.

---

## Declaring Functions

Functions are created with `fun`. The general form is:

```mindscript
fun(arg1: Type1, arg2: Type2, ...) -> ReturnType do
    ... body ...
end
```

Type annotations are optional. If you omit a parameter type or a return type, it defaults to `Any`.

A function with no parameters is written with an empty parameter list:

```mindscript
let hello = fun() -> Str do
    "hello"
end
```

To exit early, use `return(value)`. If you don’t use `return`, the value of the function body is the value of the last expression evaluated.

Example: factorial

```mindscript
let factorial = fun(n: Int) -> Int do
    if n == 0 then
        1
    else
        n * factorial(n - 1)
    end
end
```

### Calling functions

Calls must use parentheses with **no space** before `(`: `f(x)` is correct, but `f (x)` is the expression `f` followed by `(x)`.

<div class="my-code" markdown="0">
<pre><code><span class="prompt">==> factorial(4)</span>
<span class="value">24</span>

<span class="prompt">==> (fun(n, m) do n + m end)(1, 2)</span> 
<span class="value">3</span>

<span class="prompt">==> (fun(a, b) do a + b end)("Hello ", "Jack")</span>
<span class="value">"Hello Jack"</span></code></pre>

</div>

The two latter examples also show how you can evaluate a function without binding it to a variable (i.e. an evaluation of a lambda expression).

---

## Currying

MindScript functions support **currying**: if you call a function with fewer arguments than it expects, you get back a new function waiting for the remaining arguments.

```mindscript
let sum = fun(x: Int, y: Int) -> Int do
    x + y
end
```

<div class="my-code" markdown="0">
<pre><code><span class="prompt">==> typeOf(sum)</span>
<span class="value">type Int -> Int -> Int</span>

<span class="prompt">==> sum(3, 4)</span> <span class="value">7</span>

<span class="prompt">==> sum(3)</span> <span class="value">y:Int -> Int</span>

<span class="prompt">==> let add3 = sum(3)</span> <span class="value">y:Int -> Int</span>

<span class="prompt">==> add3(4)</span> <span class="value">7</span>

<span class="prompt">==> sum(3)(4)</span> <span class="value">7</span> </code></pre>

</div>

A practical pattern: apply a function to a list of arguments one by one.

```mindscript
let applyAll = fun(f, args: [Any]) do
    for x in iter(args) do
        f = f(x)
    end
end
```

<div class="my-code" markdown="0">
<pre><code><span class="prompt">==> applyAll(sum, [3, 4])</span>
<span class="value">7</span>
</code></pre>
</div>

---

## Structural typing (duck typing)

MindScript uses **structural typing** for object shapes: if a function expects `{name!: Str}`, it accepts any object that has a `name` field of type `Str` (extra fields are fine).

```mindscript
# Greets any object with a required 'name' field
let greet = fun(person: {name!: Str}) -> Str do
    "Hello, " + person.name + "!"
end

let foo = { name: "Alice", hobby: "chess" }
let bar = { name: "Bob", age: 30, city: "London" }

greet(foo)
greet(bar)
```

---

## Closures

Functions capture variables from their defining scope. Those variables remain alive as long as the function value is alive.

A common use is building iterators. An iterator is a function of type `Null -> Any?` that returns the next value, or `null` to stop. (MindScript also allows calling a `Null`-parameter function with no arguments.)

```mindscript
let makeCounter = fun() -> (Null -> Int) do
    let count = 0
    fun(_: Null) -> Int do
        count = count + 1
    end
end

let c1 = makeCounter()
c1()   ## 1
c1()   ## 2

let c2 = makeCounter()
c2()   ## 1 (independent count)
```

Each call to `makeCounter()` creates a fresh closure with its own `count`.


