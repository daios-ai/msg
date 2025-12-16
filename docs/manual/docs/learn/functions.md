<style>
  .my-code span.prompt { color: black; }
  .my-code span.note { color: green; }
  .my-code span.value { color: blue; }
</style>

# Functions

In this chapter we'll see how to create functions in MindScript. But before we dive into functions, we need to introduce type schemas. Recall that types in MindScript are runtime-checked. This means that whenever we invoke a function (or return from it), the interpreter will check whether the values conform to the type constraints. If they don't, then we get a runtime error. Type schemas are the type annotations that MindScript uses to perform these checks.


## Type Schemas

MindScript allows you to specify the *template* or *shape* of values&mdash;**type schemas**&mdash;using type annotations. These annotations can appear on function parameters, return types, or when defining custom types.

Type schemas are created using the `type` keyword, followed by a type specification. Below are some basic examples:
```
type Str
type Int
type [Int]
type {name: Str, age: Int, hobbies: [Str]}
```

There are three handy functions to perform type checking: `typeOf(val)`, which we have already met; `isType(val, templ)`, which returns `true` if the value `val` conforms to the type schema `templ`; and `isSubtype(sub, super)`, which returns `true` is the type schema `sub` conforms to the type schema `super`. For instance, the followig examples show how to check whether a given value conforms to a schema:

<div class="my-code" markdown="0">
<pre><code><span class="prompt">> isType(42, type Int)</span>
<span class="value">true</span>

<span class="prompt">> isType({name: "John"}, type {name: Str})</span>
<span class="value">true</span>
</code></pre>
</div>

We will see example of comparing two schemas using `subType` later.

In addition, there are ways of constraining and relaxing the allowed values.

### Enumerated types

Sometimes we only want to allow values within a fixed list. These correspond to *enumerated types* in MindScript, also known as *enums*. Enums are declared using the `enum` keyword, followed by an array of permitted values. For instance, if there are only handful of order statuses we can declare this as:
```
type Enum ["pending", "processing", "shipped", "delivered", "cancelled"]
```
This says that the allowed values are only those in the list.

Note an enum `a` that contains another enum `b` as a subset is a supertype:
```
let a = type Enum [1, 2, 3]
let b = type Enum [1, 2]

subType(a, b)    ## false
subType(b, a)    ## true
```

### Nullable types

Nullable types let you express that a value may either be of a given type or null. You indicate this with the `?` operator after the base type. For instance, consider a situation where we want users to provide a name and age, but the bio is optional (i.e. a string, but potentially missing). Then this would be expressed as
```mindscript
let User = type {
    name: Str,
    age: Int,
    bio: Str?
}
```
Of course, any type can be nullable, not just object properties. For instance,
```mindscript
let Tags = type [Str?]
```
This schema allows for list like
```mindscript
["sports", null, "international news", "movies"]
```


### Mandatory types in objects

When specifying a type schema for objects, the default is to consider the properties as optional. So, for instance:
```mindscript
let Person = type {
    name: Str
    age: Int
}

isType({name: "John", age: 45})  ## true
isType({}, Person)               ## true
```
While this is slightly confusing, it was deliberately designed like this so as to conform to the [JSON schema](https://json-schema.org) standard, because REST API often have named yet optional arguments.

To indicate that a property is required/mandatory, use `!` after the name of the property. For instance, 
```
let MusicRecord = type {
    title!: Str,            ## required
    artist!: Str,           ## required
    releaseYear: Int,       ## optional
    genre!: Str?,           ## required, can be null
}
```
In the `MusicRecord` type schema, the title, the artist's name, and the genre are all mandatory, while the release year is optional. Note that the genre, while required, can be equal to `null`.
```
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

isType(song1, MusicRecord)  ## true, in spite of missing release year
isType(song2, MusicRecord)  ## false, missing genre
```


### Universal Type

MindScript also has the type `Any`, which serves as the universal fallback type representing any possible type. Hence
```
isType(a, type Any)
```
will evaluate to `true` no matter what `a` is.

While it is recommended to be specific whenever possible, sometimes the univesal type is useful or even necessary. For instance,

- it allows you to write quick, un-typed helper functions without boilerplate, while still preserving the ability to add types later;
- core library functions like `map`, `filter`, and `reduce` operate over collections of arbitrary elements;
- sometimes you don't know the schema in advance, especially in interoperation with unstructured data.


### Type Aliases

When you have a complex type expression that you reuse in multiple places, giving it a name makes your code clearer and easier to maintain. In MindScript, you create a *type alias* by binding a `type` expression to a name:
```mindscript
let [AliasName] = type [TypeExpression]
```
From then on, you can refer to `[AliasName]` anywhere a type is expected.

Creating descriptive aliases like `GeoPoint` or `Track` immediately tells you what the shape represents, instead of forcing you to parse a long inline `{…}` or `[…]` every time. If multiple functions accept the same structured argument, you avoid copying-and-pasting the shape. If the shape changes, you update the alias in one place.

Let's have a look at the following `GeoPoint` definition for latitude/longitude coordinates:
```mindscript
let GeoPoint = type {
  lat!: Num,      ## required floating-point
  lng!: Num,      ## required floating-point
  label: Str?     ## optional descriptive label
}
```
Now any object matching `{lat: Num, lng: Num, label: Str?}` can be annotated as a `GeoPoint`. They can also be nested. For instance, we can define a type schema for representing the boundaries of territories:
```mindscript 
let Territory = type {
  name!: Str,
  track!: [GeoPoint]
}
```


## Declaring Functions

Functions in MindScript are created with the `fun` keyword. This yields anonymous functions (called *lambdas*) that can be bound to variables. The general syntax is:

```mindscript
fun([arg1]: [Type1], [arg2]: [Type2], ...) -> [ReturnType] do
  ...    ## body of the function
end
```

There are a few rules. Type annotations on arguments and return type are optional; omitting them defaults to the universal type `Any`. If you omit all arguments, a hidden `null: Null` parameter is added automatically. Hence, all functions have arguments. To exit early you can use `return([value])`; otherwise the last expression's value is returned.

As an example, consider the *factorial* function:

```mindscript
let factorial = fun(n: Int) -> Int do
  if n == 0 then
    1
  else
    n * factorial(n - 1)
  end
end
```

To evaluate a function, use parenthesis and the arguments right after the function expression, with no space in between:
<div class="my-code" markdown="0">
<pre><code><span class="prompt">> factorial(4)</span>
<span class="value">24</span>

<span class="prompt">> (fun(n, m) do n + m end)(1, 2)</span>
<span class="value">3</span>

<span class="prompt">> (fun(n, m) do n + m end)("Hello ", "Jack")</span>
<span class="value">"Hello Jack"</span>
</code></pre>
</div>
The last two examples also show how to evaluate (and immediately discard) a function.

In the case of the factorial function, notice that the declaration uses a recursive call
```{.mindscript hl_lines="5"}
let factorial = fun(n: Int) -> Int do
  if n == 0 then
    1
  else
    n * factorial(n - 1)
  end
end
```
even though the name `factorial` is only bound after the function has been created. This works because the variable `factorial` is only evaluated at runtime, when we call it.


## Currying

All MindScript functions are *curried*, that is, every multi-argument function is translated into a sequence of single-argument functions. Consider the following:
```mindscript
let sum = fun(x: Int, y: Int) -> Int do
  return(x + y)
end
```
If we check its type we get
<div class="my-code" markdown="0">
<pre><code><span class="prompt">> typeOf(sum)</span>
<span class="value">type Int -> Int -> Int</span>
</code></pre>
</div>
which says that `sum` is a function that takes `x:Int` and returns a function of type `Int -> Int`, which in turn takes an argument `y:Int` and returns a result of type `Int`.

Because of this, multi-argument functions can be called in stages. For instance:
<div class="my-code" markdown="0">
<pre><code><span class="prompt">> sum(3, 4)</span>
<span class="value">7</span>

<span class="prompt">> sum(3)</span>
<span class="value">y:Int -> Int</span>

<span class="prompt">> let add3 = sum(3)</span>
<span class="value">y:Int -> Int</span>

<span class="prompt">> add3(4)</span>
<span class="value">7</span>

<span class="prompt">> sum(3)(4)</span>
<span class="value">7</span>
</code></pre>
</div>
The expression `sum(3, 4)` returns `7` as expected. But then, `sum(3)` binds the argument `x=3`, returning the new function of type `y:Int -> Int` which adds 3 to its argument `y`. This is the reason why `sum(3, 4)` gives the same result as evaluating it in stages like in `sum(3)(4)`.

Currying promotes concise higher-order patterns: you can pass partially applied functions to `map`, `filter`, etc. For instance, consider the following function `eval` which takes an arbitrary function, its arguments, and then evaluates it:
```mindscript
let eval = fun(f: Any -> Any, args: [Any]) do
  for x in iter(args) do
    f = f(x)
  end
end
```
In the body we iterate over the arguments, and in each step, we evaluate `f = f(x)`, which evaluates the function with a single argument, obtains the partially evaluated function, and assigns it to `f` itself. If we apply this to `sum` and  arguments `[3, 4]`, we get
<div class="my-code" markdown="0">
<pre><code><span class="prompt">> eval(sum, [3, 4])</span>
<span class="value">7</span>
</code></pre>
</div>
as expected.

## Duck Typing

MindScript embraces *structural typing*, sometimes colloquially called "duck typing", for object shapes. (The expression comes from the saying "if it walks like a duck and quacks like a duck, then it is a duck".) A function expecting a `{name: Str, age: Int}` will accept *any* object containing those properties (and possibly more), regardless of its declared type.

```mindscript
# Greets any object with a 'name' field
let greet = fun(person: {name!: Str}) -> Str do
  "Hello, " + person.name + "!"
end

let foo = { name: "Alice", hobby: "chess" }
let bar = { name: "Bob", age: 30, city: "London" }

greet(foo)  ## prints "Hello, Alice!"
greet(bar)  ## prints "Hello, Bob!"
```

Even though `foo` and `bar` lack a shared named type, they both "quack" like `{name!: Str}`, so the function `greet` accepts them.


## Closures

MindScript functions form *closures*, that is, they capture variables from their defining scope. These closed-over variables remain alive even after the outer function returns. This is best explained using an example. Let's say you want to create an iterator (for a [loop](/manual/blocks#loop-expressions)) that counts from 1 onward:

```mindscript
let makeCounter = fun() -> (Null -> Int) do
  let count = 0
  fun() -> Int do
    count = count + 1
  end
end

let c1 = makeCounter()
c1()   ## 1
c1()   ## 2

let c2 = makeCounter()
c2()   ## 1 (independent count)
```
Here, each call to `makeCounter()` produces a fresh closure over its own `count` variable. The inner function can read and mutate `count`, and successive calls remember the updated value. Lexical scoping ensures these captured variables behave predictably, enabling patterns like generators, memoization, and private state.

