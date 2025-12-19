
# Scopes & Control

In this chapter, we’ll explore how MindScript handles variable scope, block structure, and the three core control constructs: logical expressions, conditional expressions, and for-loops. 


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

## Control Structures

MindScript has three control flow constructs: logical expressions, conditionals, and loops.

## Logical Expressions (Short-Circuit)

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

Here, the individual branches are blocks: they have their own scope and return the last evaluated expression.

## Loop Expressions

MindScript has `for` loops and `while` loops, and both are **expressions**: they evaluate to a value (typically the last value produced by the loop body).

This section focuses on `for`, because it is the most common loop when processing arrays and objects.

### Iterators

`for` loops operate over *iterators*. An iterator is a function of type `Null -> Any?` that returns:

* the next item, or
* `null` to signal “no more items”.

One way of building iterators from arrays or objects is using the `iter` function from the standard library:

```mindscript
let arr = [1, 2, 3, 4]
let obj = {a: 1, b: 2, c: 3}

let arrIt = iter(arr)
let objIt = iter(obj)
```

Then, repeatedly invoking `arrIt()` and `objIt()` will return the sequences:

```
1, 2, 3, 4, null, ...

["a", 1], ["b", 2], ["c", 3], null, ...
```

For objects, the iterator yields key–value pairs as two-element arrays. Objects also preserve insertion order when iterating.

### The `for` loop form

The loop constructor takes the form `for [v] in [it] do [block] end`, where `[v]` is an assignment target, `[it]` is:

* an array,
* an object, or
* an iterator function (`Null -> Any?`),

and `[block]` is a block of expressions.

Here is code to compute and print squares:

```mindscript
let nums = [1, 2, 3, 4]

for n in nums do
    println(n * n)
end
```

This prints `1, 4, 9, 16`. Loops stop as soon as the iterator signals completion by returning `null`. Since loops are expressions, the value of the entire loop is the last computed value—in this case, `16`.

You can influence the execution of a loop using `break` and `continue`. Both may carry a value, but they do not have to:

* `break(x)` exits the loop and the loop expression evaluates to `x`.
* `break` exits the loop and the loop expression evaluates to `null`.
* `continue(x)` skips to the next iteration after evaluating `x`.
* `continue` skips to the next iteration and yields `null` for that step.

We can use this to mimic the behavior of a while loop. The code below computes the sum of positive numbers until a negative number is encountered:

```mindscript
let it = iter([2, 3, -1, 7])

let total = 0
for v in it do
    if v < 0 then
        break(total)
    end
    total = total + v
end
```

This loop adds `2` and `3` and then breaks with a value equal to `5`.

## Best Practices

1. **Keep scopes small**: declare variables as late as possible inside the smallest `do ... end` block.
2. **Use descriptive names**: avoid shadowing outer variables unless intentional.
3. **Leverage expression-based control**: capture `if` and loop results directly in variables.
4. **Handle iterators cleanly**: design iterators to return `null` when done, enabling idiomatic loops.

