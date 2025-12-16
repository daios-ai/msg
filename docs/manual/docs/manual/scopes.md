# Scopes & Control

In this chapter, we’ll explore how MindScript handles variable scope, block structure, and the three core control constructs: logical expressions, conditional expressions, and for-loops. 


### Lexical Scoping and Blocks

MindScript uses *lexical (static) scoping*, meaning each variable is bound to the nearest enclosing block at *write* time. A block is any section of code surrounded by `do ... end` (and other delimiters in conditional blocks which we'll see later). The only exception to this is the *global scope* which has no delimiters. 

Variables declared inside an inner block *shadow* variables with the same name declared in an outer block.

```mindscript
let a = "global a"
let b = "global b"
let c = "global c"

## A block creates a new scope
do
    let a = "inner a"     ## shadows the outer 'a'
    b = "inner b"         ## overwrites the outer 'b'
    println(a)            ## inner a
    println(b)            ## inner b
    println(c)            ## global c
end

println(a)  ## global a
println(b)  ## inner b
println(c)  ## global b
```
In this code, `a` declared inside the block shadows `a` declared in the global scope and thus `let a = "inner a"` does not affect `global a`. Also, variables declared with `let` inside a block vanish when the block ends.

The value of a block is the last evaluated expression. Therefore, in the case above, the value of the block is `"global c"`. In fact, one could even assign the result of a block to a variable.
```
let name = "Andreas"

let result = 
    do
        let prefix = "Hello, "
        let greeting = prefix + name
    end

println(result)   ## prints "Hello, Andreas"
```

Use block scoping to limit variable lifetimes, avoid name collisions, and make your code easier to reason about.


### Control Structures

MindScript has three control flow constructs: logical expressions, conditionals, and loops.

#### Logical Expressions (Short-Circuit)

The logical operators `and` and `or` *short-circuit*: the evaluation stops as soon as the result is determined. In a sequence of `and` operations, the evaluation continues until any subexpression is `false`. Similarly, a sequence of `or` operations stops its evaluation as soon as it encounters a subexpression that is `false`. For example:

```mindscript
let isValid = (userInput != "") and (length(userInput) < 100)

let quickTest = (1 == 2) or expensiveCheck()
```
In the first line, if `userInput` is equal to the empty string `""`, then the evaluation stops immediately and the second term is not evaluated. Likewise, in the second line, `expensiveCheck()` will always be evaluated, because `(1 == 2)` is false.


#### Conditional Expressions

MindScript’s `if ... then ... else` is an **expression**&mdash;it returns a value. Use `elif` for multiple branches.

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

println(describeAge)  ## Adult
```
Here, the individual branches between the conditions are blocks, that is, they have their own scope and return the last evaluated expression.


#### Loop Expressions

MindScript’s `for` loops operate over *iterators*. This is similar to Python's `for ... in ...` construction. Technically, iterators are [closures](manual/functions.md), a topic we'll discuss later. For our purposes here, iterators are functions that generate sequences of values.

One way of building iterators from arrays or objects is using the `iter` function.
```mindscript
let arr = [1, 2, 3, 4]
let obj = {a: 1, b: 2, c: 3}

let arrIt = iter(arr)
let objIt = iter(obj)
```
Then, repeatedly invoking `arrIt()` and `objIt()` will return the sequences
```
1, 2, 3, 4, null, null, ...

[a, 1], [b, 2], [c, 3], null, null, ...
```
respectively. After the last element in the container was produced these iterators return `null` values forever. For objects, two additional useful functions are `keys(obj)` and `values(obj)`, which produce a sequence of `obj`'s keys and values respectively.

The loop constructor takes the form `for [v] in [it] do [block] end`, where `[v]` is a variable, `[it]` an iterator, and `[block]` a block of expressions. Let's have a look at this code to compute squared numbers:
```mindscript
let nums = [1, 2, 3, 4]
let it = iter(nums)

for n in it do
    println(x * x)
end
```
This will print the sequence `1, 4, 9, 16`. Note that loops stop as soon as the iterator returns a `null` value. Since loops are expressions, the value of the entire loop is the last computed value&mdash;in this case, `16`. 

You can influence the execution of the loop using `break(expr)` and `continue(expr)`, where

- `break(expr)` exits the loops and returns `expr`,
- and `continue(expr)` skips to the next iteration after evaluating `expr`. 

The returning values are mandatory because loops are expressions, unlike the loop constructs in other programming languages.

We can use this to mimic the behavior of a while loop. The code below computes the sum of positive numbers until a negative number is encountered:
```mindscript
let mixed = iter([2, 3, -1, 7])

var total = 0
for v in it do
    if v < 0 do
        break(total)
    end
    total = total + v
end
```
This loop adds `2` and `3` and then breaks with a value equal to `5`.


### Best Practices

1. **Keep scopes small**: declare variables as late as possible inside the smallest `do ... end` block.
2. **Use descriptive names**: avoid shadowing outer variables unless intentional.
3. **Leverage expression-based control**: capture `if` and `for` results directly in variables.
4. **Handle iterators cleanly**: always design iterators to return `null` when done, enabling idiomatic loops.


