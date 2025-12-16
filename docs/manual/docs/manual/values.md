<style>
  .my-code span.prompt { color: black; }
  .my-code span.note { color: green; }
  .my-code span.value { color: blue; }
</style>

# Values and Formal Types

In this chapter we will explore MindScript's *formal type system*. Formal types are those that can be programmatically declared and manipulated, and they have strict semantics, just as you would expect from any programming language. This is contrast to *informal types*, which have a vague, context-dependent meaning. Informal types are explained [here](/manual/informal.md).

MindScript's built-in types have been deliberately chosen to mirror JSON types. In fact, the type system implements a simple subset of the [JSON schema](https://json-schema.org/) standard, rendering MindScript a scripting language that is specially suited for processing JSON objects and Web applications.

## Expressions and Everything-as-a-Value

Before diving into types, remember one core principle: *every MindScript construct is an expression*. That means whether you write a standalone literal or invoke a function, you always get back a value. For example:
<div class="my-code" markdown="0">
<pre><code><span class="prompt">> 42</span>
<span class="value">42</span>

<span class="prompt">> (40 + 2)</span>
<span class="value">42</span>

<span class="prompt">> print(42)</span>
<span class="value">42</span>

<span class="prompt">> let x = 42</span>
<span class="value">42</span>
</code></pre>
</div>
Because all constructs, including assignments, loops, and conditionals yield values, you can chain and nest them. If you know programming languages like LISP or any of its dialects (e.g. Scheme) this should be familiar.

## Dynamic Typing: Variables vs. Values

MindScript is dynamically typed, just like JavaScript or Python:
```mindscript
let greeting = "Hello, world!"
```
Here, `greeting` is a *variable* bound to the *value* `"Hello, world!"`, which is of type `Str` (string). Variables themselves don't have a type; only values do. You can check the type of any value using the `typeOf(...)` function:
<div class="my-code" markdown="0">
<pre><code><span class="prompt">> typeOf(greeting)</span>
<span class="value">type Str</span>

<span class="prompt">> typeOf(42)</span>
<span class="value">type Int</span>
</code></pre>
</div>
Unlike Python and JavaScript however, in MindScript types are *runtime checked*. Calling a function with incompatible argument types will lead to a runtime error.


## Primitive Types

MindScript ships with the following primitive types:

| Type     | Example Literals       | Description                                   |
| -------- | ---------------------- | --------------------------------------------- |
| `Null` | `null`                 | The type of the `null` value. It is idiomatic to use `null` values to mark the absence of a value or to indicate an error. |
| `Bool` | `true`<br>`false`        | The type of the two logical truth values.     |
| `Int`  | `42`<br>`-7`             | Integer numbers without fractional parts.     |
| `Num`  | `3.14`<br>`-1e3`         | Floating-point numbers for real values.       |
| `Str`  | `'Hi'`<br>`"🚀Launch!"` | Sequences of characters, delimited by double- or single-quotes. |
| `Type` | `type Str`<br>`type [Int]` | The type of types. Note that type literals always feature the type constructor keyword `type`. |

Remember, only the values/literals have a type, never the variables.

Again, you can check the types at runtime using the `typeOf(...)` function.
<div class="my-code" markdown="0">
<pre><code><span class="prompt">> typeOf(null)</span>
<span class="value">type Null</span>

<span class="prompt">> typeOf(true)</span>
<span class="value">type Bool</span>

<span class="prompt">> typeOf(42)</span>
<span class="value">type Int</span>

<span class="prompt">> typeOf(42.0)</span>
<span class="value">type Num</span>

<span class="prompt">> typeOf("foo")</span>
<span class="value">type Str</span>
</code></pre>
</div>

### Operators

MindScript provides familiar operators:

| Level        | Operators              |
| ------------ | ---------------------- |
| Arithmetic 1 | `-` (unary, e.g. -7)   |
| Arithmetic 2 | `*  /  %`              |
| Arithmetic 3 | `+  -`                 |
| Comparison   | `==  !=  <  >  <=  >=` |
| Logical 1    | `not`                  |
| Logical 2    | `and`                  |
| Logical 3    | `or`                   |
| Assignment   | `=`                    |

The operators higher in this list have precendence over those below (e.g. `*` before `not`).


### No automatic casting

In addition, there is no automatic type casting. Invoking a function with wrong types or applying an operator on incompatible values yields a runtime error (e.g., `"text" + 5`). The mathematical operators are the exception: applying a mathematical operator on an `Int` and a `Num` promotes the `Int` operand to a `Num`.
<div class="my-code" markdown="0">
<pre><code><span class="prompt">> true and false</span>
<span class="value">false</span>

<span class="prompt">> "Hello, " + 'world!'</span>
<span class="value">"Hello, world!"</span>

<span class="prompt">> 42 * 3</span>
<span class="value">126</span>

<span class="prompt">> 42 * 3.</span>
<span class="value">126.</span>
</code></pre>
</div>

## Container Types

MindScript has two container types: **arrays** and **objects**. Unlike other programming languages, there are no tuples (i.e. arrays of fixed size).

### Arrays

Arrays hold a sequence of values all of the same type. They are instantiated using square brackets:
<div class="my-code" markdown="0">
<pre><code><span class="prompt">> let path = ["start", "middle", "end"]  ## of type [Str]</span>
<span class="value">["start", "middle", "end"]</span>

<span class="prompt">> path[1]</span>
<span class="value">"middle"</span>

<span class="prompt">> path[-1]</span>
<span class="value">"end"</span>

<span class="prompt">> path[1] = "begin"</span>
<span class="value">"begin"</span>
</code></pre>
</div>
As shown above, individual elements are accessed using the index notation. Negative indices count from the end. Note the double-hash `##`: these are comments that are truly ignored by the interpreter.

You can mutate arrays in place by using functions such as `push`, `pop`, `shift`, and `unshift`. The `slice` function allows extracting slices.
<div class="my-code" markdown="0">
<pre><code><span class="prompt">> push(path, "bonus")</span>
<span class="value">["begin", "middle", "end", "bonus"]</span>

<span class="prompt">> pop(path) </span>
<span class="value">"bonus"</span>

<span class="prompt">> slice(path, 0, 2)</span>
<span class="value">["start", "middle"]</span>
</code></pre>
</div>


### Objects

Objects are key–value maps. They are instantiated by enclosing a list of key-value pairs within curly brackets:
<div class="my-code" markdown="0">
<pre><code><span class="prompt">> let user1 = {"name": "Alice", "age": 30}</span>
<span class="value">{"name": "Alice", "age": 30}</span>

<span class="prompt">> let user2 = {name: Sarah, age: 28}</span>
<span class="value">{"name": "Sarah", "age": 28}</span>

<span class="prompt">> let point = {"x-coordinate": -1, "y-coordinate": 12}</span>
<span class="value">{"x-coordinate": -1, "y-coordinate": 12}</span>
</code></pre>
</div>
Note that you can omit the quotes delimiting key names (e.g. `name` instead of `"name"`) if they follow the same naming convention as variable names. If they don't, you must use quotes.

You can access the properties using the dot `.` notation as shown below:
<div class="my-code" markdown="0">
<pre><code><span class="prompt">> user1.age</span>
<span class="value">30</span>

<span class="prompt">> user1.age = 31</span>
<span class="value">31</span>

<span class="prompt">> point."x-coordinate"</span>
<span class="value">-1</span>

<span class="prompt">> point."z-coordinate" = 3</span>
<span class="value">3</span>
</code></pre>
</div>
As shown before, it is valid to assign a value to a new property. However, attempting to access a non-existent property will yield a runtime error.

Alternatively, you can also use the `get` and `set` functions to retrieve or set the value of a property:
<div class="my-code" markdown="0">
<pre><code><span class="prompt">> let property = "email"</span>
<span class="value">"email"</span>

<span class="prompt">> set(user, "email", "alice@example.com")</span>
<span class="value">"alice@example.com"</span>

<span class="prompt">> get(user, property)</span>
<span class="value">"alice@example.com"</span>
</code></pre>
</div>
This is especially useful when the property name is only known at runtime.

To check whether a property exists, use the `exists` function:
<div class="my-code" markdown="0">
<pre><code><span class="prompt">> exists(user, "hobbies")</span>
<span class="value">false</span>

<span class="prompt">> exists(user, "age")</span>
<span class="value">true</span>
</code></pre>
</div>


## Function Types

Functions have types too. These are indicated with an arrow (`->`). For instance, a function that takes and integer and produces a string from has type `Int -> Str`. Function types will be discussed later in the [chapter about functions](manual/functions.md).


