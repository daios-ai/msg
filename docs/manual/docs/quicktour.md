
# Quick Tour

This quick tour will provide you with a brief overview of the basics of MindScript. 

At its core, MindScript is a very simple programming language for processing JSON objects using a combination of traditional programming constructs and LLM processing. It has only few language constructs that can be easily picked up in a single session.


## Oracles and Annotations

The main novelty of MindScript is its seamless integration with LLMs. Two essential elements of MindScript are [oracles](basics.md#oracles) and [annotations](basics.md#annotations). Oracles are functions performed by the LLM, while annotations serve as hints for the LLM to do its job. They can be used in tandem to combine natural language with formal expressions. 

Take for example the following annotation (which can be seen as a comment in the code), and the subsequent declaration of an oracle (which can be thought of as a regular [function](basics.md#functions) carried out by the LLM):

```mindscript
# Write the name of an important researcher in the given field.
let researcher = oracle(field: Str) -> {name: Str}
```
That's it!

This creates an anonymous oracle based on the information provided by the annotation, which guides the generation of the output. As can be seen in the code, MindScript is typed: the input to the oracle is the value named `field` of type `Str` (a string), and the output is an object of type `{name: Str}`, which implies that the oracle is of type `Str -> {name: Str}`.

The annotation `Write the name ... in the given field` is the informal type of the function, which conveys its semantics by a natural-language specification (e.g. a comment) describing its intended behavior. (Under the hood, informal types get added as an instruction to the LLM prompt.)

We can now use the oracle as if it were a [function](basics.md#functions):
```
> researcher("physics")

{"name" : "Albert Einstein"}

> researcher("biology")

{"name": "Charles Darwin"}
```
where the inputs and outputs should conform to the respective formal types provided in the declaration of the oracle.

Let's take a quick dive into these and other nice features of this language.

## Expressions

Another important aspect of MindScript is that everything is an expression, that is, every construct produces a value. For instance, all of the following expressions evaluate to `42`:

```
42
(40 + 2)
print(42)
let my_variable = 42
```

This is particularly relevant in the context of [functions](basics.md#functions) and [control structures](basics.md#constrol-structures), where any expression (including `break` and `continue` "statements") return values (for an example, [see below](basics.md#pseudo-while-loops)).

## Types

Just like JavaScript or Python, MindScript is dynamically typed: only the values have a type, not the variables.

```
let greeting = "Hello, world!"
```

This defines a variable named `greeting` containing a value `Hello, world!` of type `Str`.

### Builtin types

The **primitive types** are:

- `Null`, with a single value `null`. Null values often indicate a missing value or an error.
- `Bool`: a boolean value, either `true` or `false`.
- `Int`: an integer number, for instance `42` or `-100`.
- `Num`: a floating-point number, such as `42.0` or `-1e2`.
- `Str`: a string, delimited by either double or single quotes, as in `"Hello, world!"` or `'Hello, world!'`.
- `Type`: the type of a type.

In addition, there are two **container types**:

- [Arrays](builtin.md#arrays), as in `[1, 2, 3]` of type `[Int]`. These are containers for a single type. Tuples do not exist in MindScript.
- [Objects](builtin.md#objects), as in `{name: "Albert Einstein", age: 76}` of type `{name: Str, age: Int}`.

There are also **enumerated types**, which can be created using `Enum` and an exhaustive list of permitted values: 
```
let TwoOutOfThree = type Enum [
  [1, 2], [1, 3], [2, 3]
] 
```
See another example [here](examples.md#sentiment-analysis).

**Function types** are indicated using the `->` constructor. For instance `cos(x: Num) -> Num` is of type `Num -> Num`;

Finally, there are **special types**:

- The `Any` type acts as a wildcard for any type.
- The nullabe types, indicated with a type followed by a question mark. For instance, `Str?` is either a string or `null`, whereas `Str` can only be a string. 
- (Only for object fields) The mandatory object properties are indicated with `!` following the name of the key. For instance, in `{name!: Str, age: Int}`, only the `name` property is required, whereas `age` can be ommitted. 

### Type aliases

New types are declared using the `type` keyword followed by a *type expression*:
```
let Speed = type Num

let Hobbies = type [Str]

let Person = type {
    name!: Str,
    email!: Str?,
    age: Int,
    hobbies: [Str]
}
```
This defines the type aliases `Speed`, `Hobbies`, and `Person`. Note that custom types are only aliases of the underlying structure, not separate types. Hence two types with different names are equal if their structures match.

Once created, they can be used as a normal MindScript values of type `Type`:
```
> typeOf(42)
Int

> typeOf(type Int)
Type

> typeOf(Person)
Type

> isSubtype({name: "Albert", email: null}, Person)
true
```

## Arrays and objects

Arrays are created by listing its members
```
> let a = ["This", "is", "good"]

["This", "is", "good"]
```

The elements of an array can be retrieved through indexing
```
> a[0]

"This"

> a[-1]

"good"
```

You can `push`, `pop`, `shift`, and `unshift` elements into an array:
```
> let a = ["a", "b", "c"]

["a", "b", "c"]

> push(a, "new")

["a", "b", "c", "new"]

> pop(a)

"new"

> a

["a", "b", "c"]

> shift(a, "new")

["new", "a", "b", "c"]

> unshift(a)

"new"

> a

["a", "b", "c"]
```

You can extract slices from an array using `slice`:
```
> let a = [1, 2, 3, 4, 5]

[1, 2, 3]

> slice(a, 1, 4)

[2, 3, 4]
```

To create an object, just write key-value pairs enclosed by curly brackets:
```
> let person = {"name": "John", "age": 25}

{"name": "John", "age": 25}
```
The double-quotes for keys can be ommitted. The code below is equivalent to the previous one.
```
> let person = {name: "John", age: 25}

{"name": "John", "age": 25}
```

Setting and retrieving properties can be done using the syntax the dot notation or by invoking the `set` and `get` functions:
```
> let person = {}

{}

> person.name = "Sarah"

"Sarah"

> person.name

"Sarah"

> person

{"name": "Sarah"}

> set(person, "name", "Jessica")

"Jessica"

> get(person, "name")

"Jessica"

> person

{"name": "Jessica"}
```


## Annotations

As mentioned in the beginning, annotations are an essential component of MindScript: they constitute the informal type used for providing interpretation hints for an [oracle](basics.md#oracles).

Any value can be annotated with an explanatory comment using the `#` operator, which attaches a string to the value of the next expression. For instance:
```
# The speed of light in meters per second.
let c = 299792458
```
If we now evaluate `c` in the REPL, we get both its value and annotation:
```
>c
The speed of light
299792458
```

Likewise, it is possible to annotate type expressions:
```
let Person = {
    # The name of the person.
    name!: Str,
    
    # The age of the person.
    age: Int
}
```
This is particularly useful to provide an oracle with semantic hints about inputs and outputs. 

Consider for example the annotations within the type `Person` above. If an oracle is declared as receiving an argument of type `Person`, the annotations help clarifying the meaning of the object's properties ([see the example in the section on oracles below](basics.md#oracles)). 


!!! important

    Outside the context of oracles, annotations work as regular comments, i.e. they are relevant only for the human programmer.


<!---

Variables are lexically scoped. The following code
```
let a = "outer a"
let b = "outer b"

do
    let a = "inner a"
    print(a)
    print(b)
end

print(a)
print(b)
```
outputs
```
inner a
outer b
outer a
outer b
```
as the declaration of the variable `a` inside the block
shadows the outer variable named `a`.
--->

## Operators

Most of the usual operators are available and they have the expected precedence rules:

```
* / % + - == != < <= > >= not and or =
```

Types aren't cast automatically, and applying an operator to values having incompatible
types will lead to runtime errors. 

## Functions

Functions are defined with the `fun` keyword (see the `oracle` keyword below). This declares an (anonymous) lambda expression. Functions can have one of more arguments and they can be typed. 

Take for example, a simple function that sums two integers:
```
let sumints = fun(n: Int, m: Int) -> Int do
    print(n + m)
    return(n + m)
end
```
The body of the function is enclosed within a `do ... end` bracket containing one or more expressions. If an explicit `return(value)` is not provided, a function will simply return the last evaluated expression. This means that we can also implement the function as follows:
```
let sumints = fun(n: Int, m: Int) -> Int do
    print(n + m)
    n + m
end
```
And, given that `print` returns its argument, we can further simplify this as:
```
let sumints = fun(n: Int, m: Int) -> Int do
    n + m
end
```

### Currying

Let's have a look at the type of this function:
```
let sumints = fun(n: Int, m: Int) -> Int do
    n + m
end
```
In typical programming languages it would be something like `(Int, Int) -> Int`. MindScript is different. As in functional programming languages like Haskell, MindScript considers an argument list as a sequence of single-argument functions. This is called *currying*. Hence the type of the function is `Int -> Int -> Int`, and it is valid to invoke `sumints` with a single parameter:
```
> let add3 = sumints(3)
m: Int -> Int

> add3(4)
7
```
The first evaluation `sumints(3)` returns a new function of type `Int -> Int` taking a single integer argument, where `n` is bound to `3`, i.e. `add3` adds 3 to its argument. 

If the function is declared without an argument, then it is interpreted as `Null`. If no return type is declared, it is interpreted as type `Any`. Hence,
```
let sayHello = fun() do
    print("Hello!")
end
```
is a function of type `Null -> Any`. Let's play with this:
```
> sayHello()
Hello!

> typeOf(sayHello)
Null -> Any

> let greeting = sayHello()
Hello!

> typeOf(greeting)
Str

> print(greeting)
Hello!
```
As expected, the function `sayHello` does not only print "Hello!" but also returns the corresponding string. How would this work without the `print` statement? Try it out in the [playground](https://www.daios.ai/playground)!

## Oracles

Like functions, oracles produce outputs from inputs, but they do so using an LLM (or, technically, using the induction afforded by the LLM). As shown in the beginning, oracles are declared using the `oracle` keyword and specified by all the annotations provided. Importantly, annotations that might be present in the types the oracle takes as arguments are also relevant. Take for instance the following annotated type:

```
let Person = type {
    # The name of the person.
    name!: Str,

    # Date of birth in DD/MM/YYYY format.
    dob!: Str
}
```
which is then used by the following oracle:

```
# Return the age of the person.
let get_age = oracle(person: Person) -> {age: Int}

```

Thus, the oracle uses the annotation `# Date of birth in DD/MM/YYYY format` associated to the formal expression `dob!: Str` to come up with an answer:

```
> get_age({name: "Daniel", dob: "01/03/1990"})

{"age": 33}
```


### Building Oracles using Examples

To help with the induction process one can also build the oracle
with examples. These are given using the `from` keyword plus an
array containing the examples.

```
let examples = [
    [0, "zero"], [1, "one"], [2, "two"], 
    [3, "three"], [4, "four"], [5, "five"]
]

let number2lang = oracle(number: Int) -> Str from examples
```
Note that this time we did not provide a description of the task. Rather, we are asking the oracle to induce the outputs following the pattern contained in the examples. 

Then we can ask the oracle to evaluate new inputs.
```
> number2lang(42)

"forty-two"

> number2lang(1024)

"one thousand twenty-four"
```
Obviously, since oracles compute by guessing plausible outputs (i.e. performing inductive inference), these are not guaranteed to be correct as in the previous example.

Each example must have the format 
```
   [arg_1, arg_2, ..., arg_n, output]
```
For instance, `[3, 2, "five"]` is a valid example for a function of type `Int -> Int -> Str`.


## Control structures

There are only three control structures in MindScript:

- logical expressions
- conditional expressions
- for-loop expressions (there are no while loops)

In particular, unlike Python, Javascript, and most C-like languages, conditionals and loops are expressions that return a value.

### Logical expressions

These expressions are built with the usual operators:

```
== != < <= > >= not and or
```
!!! important
    Logical expressions are short-circuited, which means that as soon as their truth value is known, the remaining subexpressions are not evaluated. 
For instance:
```
(2/1 == 1) or (2/2 == 1) or (2/3 == 2)
```
will only evaluate up to `(2/2 == 1)`, omitting the evaluation of `(2/3 == 2)`.

### Conditional expressions 

These expressions consist of a simple `if ... then ... else ... end` block structure with the
familiar semantics:
```
if n == 1 then
    print("The value is 1.")
elif n == 2 then
    print("The value is 2.")
else
    print("The value is unknown.")
end
```
These evaluate to the condition which is fulfilled, or to `null` otherwise.

### For-loops 

For-loops iterate over the outputs of an *iterator* `it`, which is a "function" of type `Null -> Any` that generates a sequence of values. The for loop will repeatedly call `it()` until the latter returns a `null` value. Take for example: 

```
for v in range(1, 4) do
    print(v)
end
```
This uses the [built-in iterator](builtin.md#iterators) `range`, which in this case generates 
```
1, 2, 3, null, null, null, ...
```
Other built-in iterators are `natural` (= 1, 2, 3, ...) and `natural0` (= 0, 1, 2, ...). 

Iterators can also be created from arrays and dictionaries using the
`iter(value: Any) -> Null -> Any` built-in function. Take for example:
```
for word in iter(['All', 'work', 'and', 'no', 'play', '...']) do
    println(word)
end
```
which will iterate through the elements of the given array and print them.

### Pseudo while-loops
An equivalent to a while loop can be constructed using an infinite iterator and a breaking condition. The following example uses the [built-in iterator](builtin.md#iterators) `natural`, which iterates over all natural numbers (i.e., up to infinity):
```
# Return the Fibonacci series up to the Nth term
let fib_series = fun(N:Int) -> [Int] do
    # First two Fibonacci values.
    let output = [0, 1]

    # First N Fibonacci values.
    for k in natural() do
        if k < N - 1 then
            push(output, output[k - 1] + output[k])   
        else
            break(null)
        end
    end

    return(output)
end
```
Note that the last `return(output)` expression is redundant and can be ommitted, because the entire for-loop evaluates to the `output` array.

Note that the execution flow of a for-loop can be modified through:

- `continue( expr )`, which evaluates to `expr` and initiates the next iteration, or
- `break( expr )`, which evaluates to `expr` and exits the entire for-loop.

See more examples of pseudo while-loops implemented using the functions [filter](builtin.md#filter) and [map](builtin.md#map).
## Destructuring

Destructuring assignment is a syntax that permits unpacking the members of
an array or the properties of an object into distinct values.

```
[let x, let y] = [2, -3, 1]
```
After this assignment, `x == 2` and `y == -3`. The third element `1` gets ignored.

```
{name: let n, email: let e} = {id: 1234, email: "albert@einstein.org", name: "Albert"}
```
After this assignment, `n == "Albert"` and `e == "albert@einstein.org"`. The
property `id` gets ignored.

These can be arbitrarily nested.



## Standard Library

MindScript fires up with a set of [built-in functions](manual/stdlib.md) to operate on [arrays](manual/stdlib.md#arrays), [iterators](manual/stdlib.md#iterators), [objects](manual/stdlib.md#objects), [strings](manual/stdlib.md#string-functions), as well as a standard set of [mathematical functions](manual/stdlib.md#mathematical-functions). 

To obtain an object that shows all the variables defined, use `getEnv`:
```
> getEnv()

{
    "dirFun": obj:{} -> [Str],
    "mute": _:Any -> Null,
    "dir": obj:{} -> [Str],
    "netImport": url:Str -> {},
    "www": url:Str -> Str?,
    "natural0": _:Null -> (Null -> Int?),
    "natural": _:Null -> (Null -> Int?),
    ...
```

### Importing modules

Modules are text files with MindScript code. It is customary to use the extension `.ms` for MindScript code.

You can import a module using the `import` function if the code is on your local filesystem, or using `netImport` for remote modules. 

For instance, try importing the **language module** `lang.ms` provided with the standard library.
```
> let lang = import("ms/lib/lang.ms")

{
    "write": instruction:Str -> {result: Str}?,
    "similarity": text1:Str -> text2:Str -> Similarity?,
    "similarityExamples": [
    ...
```
Modules are loaded as objects and need to be assigned to a variable in order to use it. So, for instance, the functions above become available through the `lang` object.

We can list the properties of a module (or any object for that matter) using `dir`:
```
> dir(lang)

[
    "write",
    "similarity",
    "similarityExamples",
    "Similarity",
    "keywordsExamples",
    "keywords",
    "coref",
    ...
```
This lists all the defined symbols. As a demonstration, let's try the keyword extractor:
```
> lang.keywords("JavaScript is a high-level, often just-in-time compiled language that conforms
  to the ECMAScript standard.")

{
    "keywords": [
        "JavaScript",
        "high-level",
        "just-in-time compiled",
        "language",
        "ECMAScript standard"
    ]
}

```

To explore the standard library, just type the name of an object&mdash;the informal type annotation will provide information about what it does.
```
> unshift

Pops the first value from the array.
array:[Any] -> Any

> http

Makes an HTTP request.
params:HTTPParams? -> method:Str? -> url:Str -> {}
```



