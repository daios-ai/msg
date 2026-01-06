# Metaprogramming and Introspection


Metaprogramming is writing programs that work with **programs**.

Instead of treating code as something you only execute, you treat it as **data** you can build, inspect, transform, and evaluate. Typical metaprogramming tasks include:

* generating code from a higher-level description (a small “code generator”),
* rewriting code (a formatter or refactor tool),
* capturing values as constructor code (snapshots / reproducible fixtures),
* building small evaluators or templating engines (trusted inputs).

MindScript supports this by giving every program a second, machine-friendly form: a *JSON-shaped S-expression tree* called **runtime-S**. You can parse source into runtime-S, transform it, format it back to canonical source, or evaluate it directly. It is also easy to store, transmit, and even generate programmatically or using oracles.

---

## Programs have two faces

A program expressed as runtime-S is a syntax tree, where each node is an array of the form:

```mindscript
[TAG, child1, child2, ...]
```

where `TAG` indicates the node kind. You can obtain the runtime-S of code using `astParse(src)`:

```mindscript-repl
==> astParse("1 + 2")
["block", ["binop", "+", ["int", 1], ["int", 2]]]

==> astParse("let x = if true then 1 else 0 end")
[
	"block",
	[
		"assign",
		["let", ["id", "x"]],
		[
			"if",
			["pair", ["bool", true], ["block", ["int", 1]]],
			[
				"block",
				["int", 0]
			]
		]
	]
]
```

Because runtime-S is JSON-shaped data, you can stringify it, store it, send it over HTTP, or feed it into other tools.


### Building and executing runtime-S

You can build runtime-S directly by writing the expression. The function `astFormat(expr)` transforms it into a MindScript code string, e.g. for inspection. Then, run it with `astEval(expr)`. For instance

```mindscript-repl
==> let expr = ["binop", "*", ["int", 6], ["int", 7]]
["binop", "*", ["int", 6], ["int", 7]]

==> astFormat(expr)
"6 * 7"

==> astEval(expr)
42
```

We can write functions that operate on programs. For instance, the function `setBinopOp` below takes a runtime-S with a `"binop"` root node and replaces the operation with new one. 

```mindscript
let setBinopOp = fun(ast: [Any], op: Str) -> Any? do
    if len(ast) != 4 or ast[0] != "binop" then
        return null
    end
    ["binop", op, ast[2], ast[3]]
end
```

We can use this function to swap the multiplication in `expr` for a sum:

```mindscript-repl
==> setBinopOp(expr, "+")
["binop", "+", ["int", 6], ["int", 7]]

==> let expr2 = replaceBinop(expr, "+")
["binop", "+", ["int", 6], ["int", 7]]

==> astFormat(expr2)
"6 + 7"

==> astEval(expr2)
13
```

If you are familiar with metaprogramming in Lisp-like languages, you should be familiar with this pattern.

### Shape validation

Before you format or execute a runtime-S tree, especially if you built it programmatically or got it from an oracle, validate its *shape*. `astValidate(expr)` checks that tags exist and that each node has the right kind of payload, returning a list of issues.


For instance, below we'll check one well-formed tree and two malformed expressions.

```mindscript-repl
==> let good = ["binop", "+", ["int", 1], ["int", 3]]
["binop", "+", ["int", 1], ["int", 3]]

==> astValidate(good)
[]

==> let bad1 = ["bniop", "+", ["int", 1], ["int", 2]]
["bniop", "+", ["int", 1], ["int", 2]]

==> astValidate(bad1)
[
	{
		path: "/",
		code: "E_UNKNOWN_TAG",
		message: "unknown tag",
		got: "bniop",
		expect: "known tag"
	}
]

==> let bad2 = ["id", ["int", 7]]
["id", ["int", 7]]

==> astValidate(bad2)
[
	{
		path: "/",
		code: "E_PAYLOAD_KIND",
		message: "invalid payload kind for id",
		got: "2/[]interface {}",
		expect: "id payload"
	}
]
```

An empty list means "structurally valid." A non-empty list gives you a precise location (`path`) and a machine-friendly error code you can report or act on. 

---

## Reflection and reification

In MindScript, you can also turn runtime values into **constructor code** and later rebuild them. The function `reflect(v)` produces runtime-S that reconstructs `v`; and `reify(ast)` evaluates that constructor code persistently.

A minimal "round-trip a value through code" looks like this:

```mindscript-repl
==> let val = {a: 1, b: [2]}
{a: 1, b: [2]}

==> let rt = reflect(val)
["map", ["pair", ["str", "a"], ["int", 1]], ["pair", ["str", "b"], ["array", ["int", 2]]]]

==> astFormat(rt)
"{a: 1, b: [2]}\n"

==> let newval = reify(rt)
{a: 1, b: [2]}

==> val == newval
true
```

Basically, reflection plus reification allows you to obtain a runtime-S representation for a value which you can store, ship over the network, and instantiate again.

There is one exception however: handles. Handles cannot be reflected because they are opaque wrappers around resources. This means that any data structure that contains handles cannot be reflected either.

```mindscript-repl
==> let chan = chanOpen()
<handle: chan>

==> reflect(chan)
null # cannot reflect value to constructor code

==> reflect({ channel: chan })
null # cannot reflect value to constructor code
```

---

## Oracle-assisted metaprogramming

!!! warning
    This section is under construction.

Oracles can generate data and validate its shape. Runtime-S is data. That means you can ask an oracle for code **in the structured form**, validate it, and only then run it.

A simple pattern is: oracle proposes runtime-S for an expression; you validate; you evaluate.

```mindscript
# Produce runtime-S for a MindScript expression that evaluates to 42.
let genExpr = oracle() -> [Any]
```

Then:

```mindscript-repl
==> let ast = genExpr()
["binop", "*", ["int", 6], ["int", 7]]

==> astValidate(ast)
[]

==> astEval(ast)
42
```

This is the key advantage of "code as JSON-shaped data": you don’t need to trust free-form text output. You can insist on a strict tree shape, reject malformed structures, and keep the whole pipeline inspectable. 


