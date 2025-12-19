# Introduction

MindScript is a programming language that seamlessly combines the features of a traditional programming language and the power of large language models (LLMs). The question was: "What if, rather than bolting LLM-powered functions onto a Python framework, we built a Turing-complete language from the ground up with LLM capabilities as first-class citizens?"

The motivation behind MindScript is to provide programmers with a minimalistic language using both programmatic and inductive constructs which allow for semantic processing in a way that wasn't possible before the advent of LLMs.

This manual assumes you have `msg` installed and working. `msg` is the state of the art MindScript runtime. To set up your environment, follow the instructions in the [Installation](/installation).


## Your first MindScript program

Let's go over the basics of writing a MindScript program. First, we create a file with the following content
```mindscript
# Say hello in a funny way!
let sayHello = oracle() -> Str

println(sayHello())
```
and save it as `first.ms`. In order to execute it, run
```bash
msg run first.ms
```

Running the above should print something like
```bash
msg run first.ms

Greetings, Earthling! Ready for some rib-tickling fun
and intergalactic high fives? Let's make today out of
this world! 🚀😄
```
The greeting will depend on what your LLM came up with by following the instruction `Say hello in a funny way!`.

Let's go over the program, line by line. The very first line is a comment. Comments start with a hash `#` and are followed by a string.
``` hl_lines="1"
# Say hello in a funny way!
let sayHello = oracle() -> Str

println(sayHello())
```
*Unlike* other programming languages, a `#` line is an annotation which attaches descriptive text to the value that follows it. It serves as a hint for both for programmers and for the LLM.

In this case, it is attached to the value produced by evaluating the next expression
``` hl_lines="2"
# Say hello in a funny way!
let sayHello = oracle() -> Str

println(sayHello())
```
which assigns a new oracle to the newly created variable `sayHello`.

In MindScript there's only one construct to declare a new variable. This is done using the `let` keyword and providing a variable name. Variable names must always start with a alphabetic character or an underscore followed by zero or more alphanumeric characters or an underscore. The following are all valid variable names:
```
x
_secret
AlphaBeta
coordinate1
the_speed_of_light
```

The expression `oracle() -> Str` declares an **oracle**. Oracles are like functions, expect that their implementation is a black box and their behavior is guided by hints. Oracles are the main feature of MindScript.

Unlike most programming languages, function and oracles are always anonymous values that are often immediately assigned to a variable. In addition, they are always typed, and input and output values are checked at runtime, yielding an error if they don't conform. 

In MindScript everything is an expression. Hence, declarations and assignments themselves return a value. In this case, the entire line evaluates to an oracle object.

Because it evaluates to an oracle object, the annotation in the preceeding line gets attached to it. Attaching a hint to an oracle guides its evaluation.

In the final line we evaluate the oracle and print the result followed by a newline
``` hl_lines="4"
# Say hello in a funny way!
let sayHello = oracle() -> Str

println(sayHello())
```
Because the oracle is a black box, it will return an arbitrary string guided by the hint. Multiple executions of this program should print different greetings.
