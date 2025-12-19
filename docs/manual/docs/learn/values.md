<style>
  .my-code span.prompt { color: black; }
  .my-code span.note { color: green; }
  .my-code span.value { color: blue; }
</style>

# Values and Formal Types

In this chapter we will explore MindScript's values and their types. MindScript's built-in types have been deliberately chosen to mirror JSON types. In fact, the type system implements a subset of JSON Schema, rendering MindScript a scripting language that is specially suited for processing JSON objects and Web applications.

## Expressions and Everything-as-a-Value

Before diving into types, remember one core principle: *every MindScript construct is an expression*. That means whether you write a standalone literal or invoke a function, you always get back a value. For example:

<div class="my-code" markdown="0">
<pre><code><span class="prompt">==> 42</span>
<span class="value">42</span>

<span class="prompt">==> (40 + 2)</span>
<span class="value">42</span>

<span class="prompt">==> print(42)</span>
<span class="value">42</span>

<span class="prompt">==> let x = 42</span>
<span class="value">42</span> </code></pre>

</div>
Because all constructs, including assignments, loops, and conditionals yield values, you can chain and nest them. If you know programming languages like LISP or any of its dialects (e.g. Scheme) this should be familiar.

## Dynamic Typing: Variables vs. Values

MindScript is dynamically typed, just like JavaScript or Python:

```mindscript
let greeting = "Hello, world!"
```

Here, `greeting` is a *variable* bound to the *value* `"Hello, world!"`, which is of type `Str` (string). Variables themselves don't have a type; only values do. You can check the type of any value using the `typeOf(...)` function:

<div class="my-code" markdown="0">
<pre><code><span class="prompt">==> typeOf(greeting)</span>
<span class="value">type Str</span>

<span class="prompt">==> typeOf(42)</span> <span class="value">type Int</span> </code></pre>

</div>
Unlike Python and JavaScript however, in MindScript types are *runtime checked*. Calling a function with incompatible argument types will lead to a runtime error.

## Primitive Types

MindScript ships with the following primitive types:

| Type     | Example Literals           | Description                                                                                                                                                                                            |
| -------- | -------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------ |
| `Null`   | `null`                     | The type of the `null` value. It is idiomatic to use `null` values to mark the absence of a value or to indicate a failure. When `null` is annotated (via `# ...`), it often carries an error message. |
| `Bool`   | `true`<br>`false`          | The type of the two logical truth values.                                                                                                                                                              |
| `Int`    | `42`<br>`-7`<br>`1_000_000`               | Integer numbers without fractional parts.                                                                                                                                                              |
| `Num`    | `3.14`<br>`-1e3`<br>`.5`   | Floating-point numbers for real values.                                                                                                                                                                |
| `Str`    | `'Hi'`<br>`"🚀Launch!"`    | Strings. In practice `Str` is also used as a byte container for IO (HTTP bodies, gzip, crypto digests).                                                                                                |
| `Type`   | `type Str`<br>`type [Int]` | The type of types. Note that type literals always use the type constructor keyword `type`.                                                                                                             |
| `Any`    | *(no literal)*             | The universal type: any value conforms to `type Any`.                                                                                                                                                  |
| `Handle` | *(no literal)*             | Opaque host values (files, network connections, processes, channels, actors, FFI pointers, etc.).                                                                                                      |

Remember, only the values/literals have a type, never the variables.

Again, you can check the types at runtime using the `typeOf(...)` function.

<div class="my-code" markdown="0">
<pre><code><span class="prompt">==> typeOf(null)</span>
<span class="value">type Null</span>

<span class="prompt">==> typeOf(true)</span> 
<span class="value">type Bool</span>

<span class="prompt">==> typeOf(42)</span> 
<span class="value">type Int</span>

<span class="prompt">==> typeOf(42.0)</span> 
<span class="value">type Num</span>

<span class="prompt">==> typeOf("foo")</span> 
<span class="value">type Str</span> </code></pre>
</div>

### Operators

MindScript provides familiar operators. This list is not exhaustive, but covers what you will use day-to-day:

| Level        | Operators              |
| ------------ | ---------------------- |
| Power        | `**`                   |
| Arithmetic 1 | `-` (unary, e.g. `-7`) |
| Arithmetic 2 | `*  /  %`              |
| Arithmetic 3 | `+  -`                 |
| Shifts       | `<<  >>`               |
| Comparison   | `==  !=  <  >  <=  >=` |
| Bitwise 1    | `~`                    |
| Bitwise 2    | <code>& &vert;</code>  |
| Logical 1    | `not`                  |
| Logical 2    | `and`                  |
| Logical 3    | `or`                   |
| Assignment   | `=`                    |

A few important notes:

* `and` / `or` short-circuit, and both sides must be `Bool` values.
* `==` compares numbers by value across `Int` and `Num` (for example, `1 == 1.0` is true).
* `+` is overloaded:

  * numbers add (preserving `Int` when both sides are `Int`),
  * strings concatenate,
  * arrays concatenate,
  * maps merge (right-hand side overwrites on conflicts; new keys are appended in insertion order).

### No automatic casting

There is no automatic type casting. Invoking a function with wrong types or applying an operator on incompatible values yields a runtime error (e.g., `"text" + 5`). The numeric operators are the only “widening” case: applying a numeric operator to an `Int` and a `Num` promotes the `Int` operand to a `Num`.

<div class="my-code" markdown="0">
<pre><code><span class="prompt">==> true and false</span>
<span class="value">false</span>

<span class="prompt">==> "Hello, " + 'world!'</span> 
<span class="value">"Hello, world!"</span>

<span class="prompt">==> 42 * 3</span> 
<span class="value">126</span>

<span class="prompt">==> 42 * 3.0</span> 
<span class="value">126.0</span> </code></pre>
</div>

## Container Types

MindScript has two container types: **arrays** and **objects** (maps). Unlike some languages, there are no tuples (fixed-size arrays) as a separate type.

Both arrays and objects are **mutable**. If you assign the same array/object to two variables and mutate it through one, the other will observe the mutation.

### Arrays

Arrays hold a sequence of values. They are instantiated using square brackets:

<div class="my-code" markdown="0">
<pre><code><span class="prompt">==> let path = ["start", "middle", "end"]</span>
<span class="value">["start", "middle", "end"]</span>

<span class="prompt">==> path[1]</span> 
<span class="value">"middle"</span>

<span class="prompt">==> path[-1]</span> 
<span class="value">"end"</span>

<span class="prompt">==> path[1] = "begin"</span> 
<span class="value">"begin"</span> </code></pre>
</div>

As shown above, individual elements are accessed using the index notation. Negative indices count from the end.

You can mutate arrays in place by using functions such as `push`, `pop`, `shift`, and `unshift`. The `slice` function allows extracting slices.

<div class="my-code" markdown="0">
<pre><code><span class="prompt">==> push(path, "bonus")</span>
<span class="value">["start", "begin", "end", "bonus"]</span>

<span class="prompt">==> pop(path)</span> 
<span class="value">"bonus"</span>

<span class="prompt">==> slice(path, 0, 2)</span> 
<span class="value">["start", "begin"]</span> </code></pre>
</div>

### Objects

Objects are key–value maps. They are instantiated by enclosing a list of key-value pairs within curly brackets:

<div class="my-code" markdown="0">
<pre><code><span class="prompt">==> let user1 = {"name": "Alice", "age": 30}</span>
<span class="value">{"name": "Alice", "age": 30}</span>

<span class="prompt">==> let user2 = {name: "Sarah", age: 28}</span>
<span class="value">{"name": "Sarah", "age": 28}</span>

<span class="prompt">==> let point = {"x-coordinate": -1, "y-coordinate": 12}</span> 
<span class="value">{"x-coordinate": -1, "y-coordinate": 12}</span> </code></pre>
</div>
Note that you can omit the quotes delimiting key names (e.g. `name` instead of `"name"`) if they follow the same naming convention as variable names. If they don't, you must use quotes.

Objects preserve insertion order of keys. This matters when you iterate or when you print values: keys appear in the order they were inserted.

You can access properties using the dot `.` notation:

<div class="my-code" markdown="0">
<pre><code><span class="prompt">==> user1.age</span>
<span class="value">30</span>

<span class="prompt">==> user1.age = 31</span>
<span class="value">31</span>

<span class="prompt">==> point."x-coordinate"</span>
<span class="value">-1</span>

<span class="prompt">==> point."z-coordinate" = 3</span>
<span class="value">3</span> </code></pre>

</div>
As shown before, it is valid to assign a value to a new property. However, attempting to access a non-existent property yields a runtime error.

When the property name is only known at runtime, use a computed property access. If `property` is a string:

<div class="my-code" markdown="0">
<pre><code><span class="prompt">==> let property = "email"</span>
<span class="value">"email"</span>

<span class="prompt">==> user1.(property) = "[alice@example.com](mailto:alice@example.com)"</span> 
<span class="value">"[alice@example.com](mailto:alice@example.com)"</span>

<span class="prompt">==> user1.(property)</span> 
<span class="value">"[alice@example.com](mailto:alice@example.com)"</span> </code></pre>
</div>

To check whether a key exists, use `mapHas(obj, key)`:

<div class="my-code" markdown="0">
<pre><code><span class="prompt">==> mapHas(user1, "hobbies")</span>
<span class="value">false</span>

<span class="prompt">==> mapHas(user1, "age")</span>
<span class="value">true</span> </code></pre>
</div>

## Function Types

Functions have types too. These are indicated with an arrow (`->`). For instance, a function that takes an integer and produces a string has type `Int -> Str`. Function types will be discussed later in the [chapter about functions](manual/functions.md).
