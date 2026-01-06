
# Values and Types

In this chapter we will explore MindScript's values and their types. The built-in types have been deliberately chosen to mirror JSON types. In fact, the type system implements a subset of JSON Schema, making MindScript a scripting language that is specially suited for processing JSON objects and writing web applications.

## Expressions and Everything-as-a-Value

Before diving into types, remember one core principle: *every MindScript construct is an expression*. That means whether you write a standalone literal or invoke a function, you always get back a value. For example:

```mindscript-repl
==> 42
42

==> (40 + 2)
42

==> print(42)
42

==> let x = 42
42
```

Because all constructs, including assignments, loops, and conditionals yield values, you can chain and nest them. This should be a familiar concept if you know programming languages like LISP or any of its dialects (e.g. Scheme).

## Dynamic Typing: Variables vs. Values

MindScript is dynamically typed, just like JavaScript or Python:

```mindscript
let greeting = "Hello, world!"
```

Here, `greeting` is a *variable* bound to the *value* `"Hello, world!"`, which is of type `Str` (string). Variables themselves don't have a type; only values do. You can check the type of any value using the `typeOf(...)` function:

```mindscript-repl
==> typeOf(greeting)
type Str

==> typeOf(42)
type Int
```

Unlike Python and JavaScript however, in MindScript types are *runtime checked* at call boundaries: calling a function or oracle with incompatible argument types will lead to a runtime error.

## Primitive Types

MindScript has JSON-like primitive types, listed in the following table:

| Type | Example Literals | Description |
| ---- | ---------------- | ----------- |
| `Null` | `null` | The type of the `null` value. It is idiomatic to use `null` values to mark the absence of a value or to indicate a failure. When `null` is annotated (via `# ...`), it often carries an error message. |
| `Bool` | `true`<br>`false` | The type of the two logical truth values. |
| `Int` | `42`<br>`-7`<br>`1_000_000` | Integer numbers without fractional parts. |
| `Num` | `3.14`<br>`-1e3`<br>`.5` | Floating-point numbers for real values. |
| `Str` | `'Hi'`<br>`"🚀Launch!"` | Strings. In practice `Str` is also used as a byte container for IO (HTTP bodies, gzip, crypto digests). |
| `Type` | `type Str`<br>`type [Int]` | The type of types. Note that type literals always use the type constructor keyword `type`. |
| `Any` | *(no literal)* | The universal type: any value conforms to `type Any`. In practice, type checking is omitted. |
| `Handle` | *(no literal)* | Opaque host values (files, network connections, processes, channels, actors, FFI pointers, etc.). |

### Operators

Operators are used to perform operations on values. The following table lists them in order of precedence, from highest to lowest.

| Level        | Operators |
| ------------ | --------- |
| Power        | `**` |
| Unary negation | `-` (e.g. `-7`) |
| Product, division, and modulo | `*  /  %` |
| Addition*, and subtraction | `+  -` |
| Bitwise left and right shifts | `<<  >>` |
| Comparisons | `==  !=  <  >  <=  >=` |
| Bitwise NOT | `~` |
| Bitwise AND and OR | <code>& &vert;</code> |
| Logical negatiion | `not` |
| Logical conjunction | `and` |
| Logical disjunction | `or` |
| Assignment | `=` |

The logical operators `and` and `or` require `Bool` operands and they short-circuit: they evaluate their operands only up to the point when the truth value is established. 

The equality `==` operators compares numbers by value across `Int` and `Num`. So, for instance, `1 == 1.0` is true.

Finally, `+` is overloaded: numbers add, preserving `Int` when both sides are `Int`; strings and arrays concatenate; and maps merge, where the right-hand operand overwrites on conflicts and new keys are appended in insertion order.


### No automatic casting

There is no automatic type casting. Invoking a function with wrong types or applying an operator on incompatible values yields a runtime error (e.g., `"text" + 5`). The numeric operators are the only “widening” case: applying a numeric operator to an `Int` and a `Num` promotes the `Int` operand to a `Num`.

```mindscript-repl
==> true and false
false

==> "Hello, " + 'world!'
"Hello, world!"

==> 42 * 3
126

==> 42 * 3.0
126.0
```

## Container Types

MindScript has two container types: **arrays** and **objects** (maps). Unlike some languages, there are no tuples (fixed-size arrays) as a separate type.

Both arrays and objects are **mutable**. If you assign the same array/object to two variables and mutate it through one, the other will observe the mutation.

### Arrays

Arrays hold a sequence of values. They are instantiated using square brackets:

```mindscript-repl
==> let path = ["start", "middle", "end"]
["start", "middle", "end"]

==> path[1] 
"middle"

==> path[-1] 
"end"

==> path[1] = "begin" 
"begin"
```

As shown above, individual elements are accessed using the index notation. Negative indices count from the end.

You can mutate arrays in place by using functions such as `push`, `pop`, `shift`, and `unshift`. The `slice` function allows extracting slices.

```mindscript-repl
==> push(path, "bonus")
["start", "begin", "end", "bonus"]

==> pop(path) 
"bonus"

==> slice(path, 0, 2) 
["start", "begin"]
```

### Objects

Objects are key-value maps. They are instantiated by enclosing a list of key-value pairs within curly brackets:

```mindscript-repl
==> let user1 = {"name": "Alice", "age": 30}
{"name": "Alice", "age": 30}

==> let user2 = {name: "Sarah", age: 28}
{"name": "Sarah", "age": 28}

==> let point = {"x-coordinate": -1, "y-coordinate": 12} 
{"x-coordinate": -1, "y-coordinate": 12}
```

Notice above that you can omit the quotes delimiting key names (e.g. `name` instead of `"name"`) if they follow the same naming convention as variable names. If they don't, you must use quotes.

Objects preserve insertion order of keys. This matters when you iterate or when you print values: keys appear in the order they were inserted.

You can access properties using the dot `.` or index `[..]` notation:

```mindscript-repl
==> user1.age
30

==> user1.age = 31
31

==> user1["age"]
31

==> point."x-coordinate"
-1

==> point."z-coordinate" = 3
3
```

As shown before, it is valid to assign a value to a new property. However, attempting to access a non-existent property yields a runtime error.

When the property name is only known at runtime, use a computed property access. If `property` is a string:

```mindscript-repl
==> let property = "email"
"email"

==> user1.(property) = "[alice@example.com](mailto:alice@example.com)" 
"[alice@example.com](mailto:alice@example.com)"

==> user1.(property) 
"[alice@example.com](mailto:alice@example.com)"
```

To check whether a key exists, use `mapHas(obj, key)`:

```mindscript-repl
==> mapHas(user1, "hobbies")
false

==> mapHas(user1, "age")
true
```

## Function Types

Functions have types too. These are indicated with an arrow (`->`). For instance, a function that takes an integer and produces a string has type `Int -> Str`. Function types will be discussed later in the [chapter about functions](manual/functions.md).
