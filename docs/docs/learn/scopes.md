
# Scopes & Control

In this chapter, we’ll explore how MindScript handles variable scope, block structure, and the three core control constructs: logical expressions, conditional expressions, and loops. 


## Lexical Scoping and Blocks

MindScript uses *lexical (static) scoping*, meaning each variable is bound to the nearest enclosing block at *write* time. A block is any section of code surrounded by `do ... end` (and other delimiters in conditional blocks which we'll see later). The only exception to this is the *global scope* which has no delimiters. 

Variables declared inside an inner block *shadow* variables with the same name declared in an outer block.

```mindscript
let a = "global a"
let b = "global b"
let c = "global c"

# A block creates a new scope
do
    let a = "inner a"     # shadows the outer 'a'
    b = "inner b"         # overwrites the outer 'b'
    println(a)            # inner a
    println(b)            # inner b
    println(c)            # global c
end

println(a)  # global a
println(b)  # inner b
println(c)  # global c
```

In this code, `a` declared inside the block shadows `a` declared in the global scope and thus `let a = "inner a"` does not affect `global a`. Also, variables declared with `let` inside a block vanish when the block ends.

The value of a block is the last evaluated expression. Therefore, in the case above, the value of the block is whatever the last expression inside it evaluates to. In fact, one could even assign the result of a block to a variable.

```mindscript
let name = "Andreas"

let result =
    do
        let prefix = "Hello, "
        let greeting = prefix + name
        greeting
    end

println(result)   # prints "Hello, Andreas"
```

Use block scoping to limit variable lifetimes, avoid name collisions, and make your code easier to reason about.

## Logical Expressions (Short-Circuit)

MindScript has three control flow constructs: logical expressions, conditionals, and loops.

The logical operators `and` and `or` *short-circuit*: the evaluation stops as soon as the result is determined.

* In an `and` expression, if the left side is `false`, MindScript does not evaluate the right side.
* In an `or` expression, if the left side is `true`, MindScript does not evaluate the right side.

Both operators work on booleans: the left-hand side must evaluate to a `Bool`. For example:

```mindscript
let isValid = (userInput != "") and (len(userInput) < 100)

let quickTest = (1 == 2) or expensiveCheck()
```

In the first line, if `userInput == ""`, then the left side is `false` and the second term is not evaluated. In the second line, if `(1 == 2)` were `true`, then `expensiveCheck()` would not be evaluated. Here it is `false`, so `expensiveCheck()` is evaluated.

## Conditional Expressions

MindScript’s `if ... then ... else` is an **expression**—it returns a value. Use `elif` for multiple branches.

```mindscript
let age = 27

let describeAge =
    if age < 13 then
        "Child"
    elif age < 20 then
        "Teenager"
    elif age < 65 then
        "Adult"
    else
        "Senior"
    end

println(describeAge)  # Adult
```

Here, the individual branches are blocks: they have their own scope and return the last evaluated expression. If the `else` branch is omitted, it is implied to evaluate to `null`.


## Loop Expressions

MindScript has `for` loops and `while` loops, and both are **expressions**: they typically evaluate to the last value produced by the loop body.

### The `for` loop form

The loop constructor takes the form 
``` mindscript
for V in IT do BLOCK end
```
where `V` is an assignment target, `BLOCK` is an expression block, and `IT` is:

* an array,
* an object,
* or an iterator function.

For instance, the following loop computes and prints square numbers:

```mindscript
let nums = [1, 2, 3, 4]

for n in nums do
    println(n * n)
end
```

This prints `1`, `4`, `9`, `16`, one per line. 

Since loops are expressions, the value of the entire loop is the last computed value—in this case, `16`. You can influence the execution of a loop using `break` and `continue`. Both carry a value:

* `break x` exits the loop and the loop expression evaluates to `x`;
* `continue x` skips to the next iteration after evaluating `x`.

If the value is omitted, then it is assumed to be equal to `null`. 

Similarly, iterating over a map works as expected:

```mindscript
let obj = {first: "Ada", last: "Lovelace", age: null, job: "programmer"}

for [key, value] in obj do
    println(key + ": " + value)
end
```

that is, the loop iterates over the key-value pairs (in an unspecified order).


An **iterator function** is a function of type `Null -> Any` that either generates the next item or `null` to signal "no more items". A `for` loop terminates as soon as the iterator the completion by generating  a stop signal. 

The standard library offers builtin iterators such as `range`, `natural` and `natural0`, and functions building iterators from others. You can also write your own iterators, using [closures](/learn/functions).

### The `while` loop form

A while loop has the form

```mindscript
while COND do BLOCK end
```

were `COND` is a boolean expression and `BLOCK` is an expression block. They execute the block as long as the condition is `true` and return the last evaluated expression.

For example, the following while loop prints the square numbers from 1 to 4 and returns `5`:

```mindscript
let i = 1
while i < 5 do
    println(i)
    i = i + 1
end
```

## Best Practices

1. **Keep scopes small**: declare variables as late as possible inside the smallest `do ... end` block.
2. **Use descriptive names**: avoid shadowing outer variables unless intentional.
3. **Leverage expression-based control**: capture `if` and loop results directly in variables.
4. **Handle iterators cleanly**: design iterators to return `null` when done, enabling idiomatic loops.

