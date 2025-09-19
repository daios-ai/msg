# ------ JSON-MindScript Manual -------------------------

let _manual = fun() do
	let ROOT = osEnv("MSGPATH")
	if ROOT == null then
		return noteSet("<environment variable MSGPATH is undefined>", null)
	end
	let man = readFile(pathJoin([ROOT, "data", "msjson.md"]))
	if man == null then
		return noteSet("<JSON-MindScript manual not found (MSGPATH/data/msjson.md)>", null)
	end
	man
end()

# ------ Natural language -> Spec -----------------------

# FEW-SHOTS: each output is a JSON-MS AST that EVALUATES to:
# { doc: Str, jsonSpec: { name: Str, type: Type } }

let _specExamples = [
	[
		"Add two real numbers and return the sum.",
		[
			"map",
			[
				"pair",
				["str", "doc"],
				[
					"str",
					"Add two numbers.\n\nArgs: a:Num, b:Num\nReturn: Num"
				]
			],
			[
				"pair",
				["str", "jsonSpec"],
				[
					"map",
					["pair", ["str", "name"], ["str", "add"]],
					[
						"pair",
						["str", "type"],
						[
							"type",
							[
								"binop",
								"->",
								["id", "Num"],
								[
									"binop",
									"->",
									["id", "Num"],
									["id", "Num"]
								]
							]
						]
					]
				]
			]
		]
	],
	[
		"Factorial of n (n >= 0). Return null when n < 0.",
		[
			"map",
			[
				"pair",
				["str", "doc"],
				[
					"str",
					"Factorial with input check.\n\nArgs: n:Int\nReturn: Int?"
				]
			],
			[
				"pair",
				["str", "jsonSpec"],
				[
					"map",
					["pair", ["str", "name"], ["str", "factorial"]],
					[
						"pair",
						["str", "type"],
						[
							"type",
							[
								"binop",
								"->",
								["id", "Int"],
								["unop", "?", ["id", "Int"]]
							]
						]
					]
				]
			]
		]
	],
	[
		"Sort an array of strings ascending (stable).",
		[
			"map",
			[
				"pair",
				["str", "doc"],
				[
					"str",
					"Sort strings ascending (stable).\n\nArgs: xs:[Str]\nReturn: [Str]"
				]
			],
			[
				"pair",
				["str", "jsonSpec"],
				[
					"map",
					["pair", ["str", "name"], ["str", "sortStrings"]],
					[
						"pair",
						["str", "type"],
						[
							"type",
							[
								"binop",
								"->",
								["array", ["id", "Str"]],
								[
									"array",
									["id", "Str"]
								]
							]
						]
					]
				]
			]
		]
	],
	[
		"Return the host part of a URL string; null if invalid.",
		[
			"map",
			[
				"pair",
				["str", "doc"],
				[
					"str",
					"Extract host from URL.\n\nArgs: url:Str\nReturn: Str?"
				]
			],
			[
				"pair",
				["str", "jsonSpec"],
				[
					"map",
					["pair", ["str", "name"], ["str", "host"]],
					[
						"pair",
						["str", "type"],
						[
							"type",
							[
								"binop",
								"->",
								["id", "Str"],
								["unop", "?", ["id", "Str"]]
							]
						]
					]
				]
			]
		]
	],
	[
		"Summarize a diff into a concise git commit subject (<= 72 chars). Return null if not possible.",
		[
			"map",
			[
				"pair",
				["str", "doc"],
				[
					"str",
					"Generate a short git commit subject.\n\nArgs: diff:Str\nReturn: Str?"
				]
			],
			[
				"pair",
				["str", "jsonSpec"],
				[
					"map",
					["pair", ["str", "name"], ["str", "commitSubject"]],
					[
						"pair",
						["str", "type"],
						[
							"type",
							[
								"binop",
								"->",
								["id", "Str"],
								["unop", "?", ["id", "Str"]]
							]
						]
					]
				]
			]
		]
	],
	[
		"Compute HMAC-SHA256 hex digest of msg with key.",
		[
			"map",
			[
				"pair",
				["str", "doc"],
				[
					"str",
					"Compute HMAC-SHA256.\n\nArgs: key:Str, msg:Str\nReturn: Str"
				]
			],
			[
				"pair",
				["str", "jsonSpec"],
				[
					"map",
					["pair", ["str", "name"], ["str", "hmac256"]],
					[
						"pair",
						["str", "type"],
						[
							"type",
							[
								"binop",
								"->",
								["id", "Str"],
								[
									"binop",
									"->",
									["id", "Str"],
									["id", "Str"]
								]
							]
						]
					]
				]
			]
		]
	]
]

# Oracle: minimal task, few-shots only. Assumes _manual: Str is already defined.
let _specOracle = oracle(desc: Str) -> [Any]? from _specExamples
_specOracle = noteSet("Return ONLY a JSON-MindScript AST for a map: { doc: Str, jsonSpec: { name: Str, type: Type } }.\n" + "Wrap the type with [\"type\", <TypeExpr>].\n\n" + _manual, _specOracle)

# Public: natural language -> { doc, jsonSpec } (validate then eval)
let spec = fun(desc: Str) -> {}? do
	let ast = _specOracle(desc)
	if ast == null then
		return ast
	end
	let diags = astValidate(ast)
	if diags != [] then
		return noteSet(formatValue(diags[0]), null)
	end
	astEval(ast)
end

# ------ Coding Oracle ----------------------------------

# Render a stabilized spec into a deterministic prompt for the coding oracle.
# Expects: { doc!: Str, jsonSpec!: { name!: Str, type!: Type } }
let _genPrompt = fun(s: {}) -> Str do
	let name = s.jsonSpec.name
	let tstr = formatValue(s.jsonSpec.type) # e.g., "<type: [Str] -> [Str]>"

	let doc = s.doc
	join([
		"Implement a JSON-MindScript anonymous function or oracle matching this spec.",
		"",
		"Name: " + name,
		"Type: " + tstr,
		"",
		"Doc:",
		doc
	], "\n")
end

# Few-shots now use the SAME prompt shape produced by _renderSpecPrompt.
# Each example’s first element is a synthetic prompt string with Name/Type/Doc.
let _genExamples = [
	[
		join([
			"Implement a JSON-MindScript anonymous function or oracle matching this spec.",
			"",
			"Name: add",
			"Type: <type: Num -> Num -> Num>",
			"",
			"Doc:",
			"Add two numbers.\n\nArgs: a:Num, b:Num\nReturn: Num"
		], "\n"),
		[
			"annot",
			[
				"str",
				"Add two numbers.\n\nArgs: a:Num, b:Num\nReturn: Num"
			],
			[
				"fun",
				[
					"array",
					["pair", ["id", "a"], ["id", "Num"]],
					[
						"pair",
						["id", "b"],
						["id", "Num"]
					]
				],
				["id", "Num"],
				[
					"block",
					["binop", "+", ["id", "a"], ["id", "b"]]
				]
			]
		]
	],
	[
		join([
			"Implement a JSON-MindScript anonymous function or oracle matching this spec.",
			"",
			"Name: factorial",
			"Type: <type: Int -> Int?>",
			"",
			"Doc:",
			"Factorial with input check.\n\nArgs: n:Int\nReturn: Int?"
		], "\n"),
		[
			"annot",
			[
				"str",
				"Factorial with input check.\n\nArgs: n:Int\nReturn: Int?"
			],
			[
				"fun",
				["array", ["pair", ["id", "n"], ["id", "Int"]]],
				[
					"unop",
					"?",
					["id", "Int"]
				],
				[
					"block",
					[
						"if",
						[
							"pair",
							["binop", "<", ["id", "n"], ["int", 0]],
							[
								"block",
								[
									"return",
									[
										"annot",
										["str", "<negative input>"],
										["null"]
									]
								]
							]
						],
						[
							"block",
							["assign", ["decl", "res"], ["int", 1]],
							[
								"assign",
								["decl", "i"],
								["int", 2]
							],
							[
								"while",
								[
									"binop",
									"<=",
									["id", "i"],
									["id", "n"]
								],
								[
									"block",
									[
										"assign",
										["id", "res"],
										[
											"binop",
											"*",
											["id", "res"],
											["id", "i"]
										]
									],
									[
										"assign",
										["id", "i"],
										[
											"binop",
											"+",
											["id", "i"],
											["int", 1]
										]
									]
								]
							],
							["id", "res"]
						]
					]
				]
			]
		]
	],
	[
		join([
			"Implement a JSON-MindScript anonymous function or oracle matching this spec.",
			"",
			"Name: chooseColor",
			"Type: <type: () -> Enum[\"red\",\"green\",\"blue\"]>",
			"",
			"Doc:",
			"Pick a primary color.\n\nArgs: none\nReturn: Enum(red|green|blue)"
		], "\n"),
		[
			"annot",
			[
				"str",
				"Pick a primary color.\n\nArgs: none\nReturn: Enum(red|green|blue)"
			],
			[
				"oracle",
				["array"], # no params
				[
					"type",
					[
						"enum",
						["str", "red"],
						["str", "green"],
						["str", "blue"]
					]
				],
				[
					"map",
					[
						"pair",
						["str", "doc"],
						[
							"str",
							"Pick one of the allowed colors."
						]
					],
					[
						"pair",
						["str", "examples"],
						[
							"array",
							["array", ["array"], ["str", "red"]],
							[
								"array",
								["array"],
								["str", "green"]
							],
							["array", ["array"], ["str", "blue"]]
						]
					]
				]
			]
		]
	],
	[
		join([
			"Implement a JSON-MindScript anonymous function or oracle matching this spec.",
			"",
			"Name: commitSubject",
			"Type: <type: Str -> Str>",
			"",
			"Doc:",
			"Generate a short, imperative git commit message.\n\nArgs: diff:Str\nReturn: Str"
		], "\n"),
		[
			"annot",
			[
				"str",
				"Generate a short, imperative git commit message.\n\nArgs: diff:Str\nReturn: Str"
			],
			[
				"oracle",
				["array", ["pair", ["id", "diff"], ["id", "Str"]]],
				["id", "Str"],
				[
					"map",
					[
						"pair",
						["str", "doc"],
						[
							"str",
							"Summarize the diff into a concise commit subject (<= 72 chars)."
						]
					],
					[
						"pair",
						["str", "examples"],
						[
							"array",
							[
								"array",
								[
									"array",
									[
										"str",
										"+ add input validation to /login"
									]
								],
								["str", "add input validation for /login"]
							],
							[
								"array",
								[
									"array",
									[
										"str",
										"- remove dead code in UserService"
									]
								],
								["str", "remove dead code in UserService"]
							]
						]
					]
				]
			]
		]
	],
	[
		join([
			"Implement a JSON-MindScript anonymous function or oracle matching this spec.",
			"",
			"Name: host",
			"Type: <type: Str -> Str?>",
			"",
			"Doc:",
			"Extract host from URL.\n\nArgs: url:Str\nReturn: Str?"
		], "\n"),
		[
			"annot",
			[
				"str",
				"Extract host from URL.\n\nArgs: url:Str\nReturn: Str?"
			],
			[
				"fun",
				["array", ["pair", ["id", "url"], ["id", "Str"]]],
				[
					"unop",
					"?",
					["id", "Str"]
				],
				[
					"block",
					[
						"assign",
						["decl", "u"],
						[
							"call",
							["id", "urlParse"],
							["id", "url"]
						]
					],
					[
						"if",
						[
							"pair",
							["binop", "==", ["id", "u"], ["null"]],
							[
								"block",
								[
									"return",
									[
										"annot",
										["str", "<invalid url>"],
										["null"]
									]
								]
							]
						],
						["block", ["idx", ["id", "u"], ["str", "host"]]]
					]
				]
			]
		]
	]
]

# The coding oracle still takes a Str prompt (rendered from the spec object).
let _genOracle = oracle(spec: Str) -> [Any]? from _genExamples

let _genComment = "Return ONLY a JSON-MindScript AST for an anonymous function OR oracle " + "that conforms to the provided Name/Type and reflects the Doc.\n\n" + _manual
_genOracle = noteSet(_genComment, _genOracle)

# ------ Public ----------------------------------

# Generate a function/oracle from a stabilized spec object.
# Expects: { doc!: Str, jsonSpec!: { name!: Str, type!: Type } }
let gen = fun(spec: {}) do
	let prompt = _genPrompt(spec)

	let g = _genOracle(prompt)
	if g == null then
		return g
	end

	let validation = astValidate(g)
	if validation != [] then
		return noteSet(validation[0].message, null)
	end

	astEval(g)
end

# ------ Test Ideas Oracle ------------------------------

# Few-shots mirror the same prompt shape as _genPrompt(Name/Type/Doc).
# Output is a JSON-MS AST that EVALUATES to: [Str]
let _ideasExamples = [
	[
		join([
			"Implement a JSON-MindScript anonymous function or oracle matching this spec.",
			"",
			"Name: add",
			"Type: <type: Num -> Num -> Num>",
			"",
			"Doc:",
			"Add two numbers.\n\nArgs: a:Num, b:Num\nReturn: Num",
			"",
			"Now list tiny, orthogonal test ideas as an array of strings."
		], "\n"),
		[
			"array",
			["str", "simple positives (2.0 + 3.0 → 5.0)"],
			[
				"str",
				"mixed int/num (2 + 3.5 → 5.5)"
			],
			["str", "negative numbers (-1.0 + -2.0 → -3.0)"],
			[
				"str",
				"addition with zero (x + 0 → x)"
			],
			["str", "commutativity (a+b == b+a)"]
		]
	],
	[
		join([
			"Implement a JSON-MindScript anonymous function or oracle matching this spec.",
			"",
			"Name: factorial",
			"Type: <type: Int -> Int?>",
			"",
			"Doc:",
			"Factorial with input check.\n\nArgs: n:Int\nReturn: Int?",
			"",
			"Now list tiny, orthogonal test ideas as an array of strings."
		], "\n"),
		[
			"array",
			["str", "base case n=0 → 1"],
			["str", "small n=1 → 1"],
			["str", "typical n=5 → 120"],
			[
				"str",
				"error path n<0 → null with <negative input>"
			],
			["str", "growth check n=10 (no overflow in int64 range)"]
		]
	],
	[
		join([
			"Implement a JSON-MindScript anonymous function or oracle matching this spec.",
			"",
			"Name: sortStrings",
			"Type: <type: [Str] -> [Str]>",
			"",
			"Doc:",
			"Sort strings ascending (stable).\n\nArgs: xs:[Str]\nReturn: [Str]",
			"",
			"Now list tiny, orthogonal test ideas as an array of strings."
		], "\n"),
		[
			"array",
			["str", "empty array → empty array"],
			[
				"str",
				"already sorted remains unchanged"
			],
			["str", "reverse order sorted ascending"],
			[
				"str",
				"stability: equal keys preserve order"
			],
			["str", "mixed case: verify lexicographic (not locale)"]
		]
	],
	[
		join([
			"Implement a JSON-MindScript anonymous function or oracle matching this spec.",
			"",
			"Name: host",
			"Type: <type: Str -> Str?>",
			"",
			"Doc:",
			"Extract host from URL.\n\nArgs: url:Str\nReturn: Str?",
			"",
			"Now list tiny, orthogonal test ideas as an array of strings."
		], "\n"),
		[
			"array",
			["str", "valid http url → host (http://a.com → a.com)"],
			[
				"str",
				"https with port → host (https://a.com:443 → a.com)"
			],
			["str", "invalid url string → null with <invalid url>"],
			[
				"str",
				"url with path/query → host only"
			],
			["str", "ipv4 host → returns ip"]
		]
	],
	[
		join([
			"Implement a JSON-MindScript anonymous function or oracle matching this spec.",
			"",
			"Name: hmac256",
			"Type: <type: Str -> Str -> Str>",
			"",
			"Doc:",
			"Compute HMAC-SHA256.\n\nArgs: key:Str, msg:Str\nReturn: Str",
			"",
			"Now list tiny, orthogonal test ideas as an array of strings."
		], "\n"),
		[
			"array",
			["str", "known test vector (RFC)"],
			[
				"str",
				"empty key, non-empty msg"
			],
			["str", "non-empty key, empty msg"],
			[
				"str",
				"repeatability with same inputs"
			],
			["str", "different keys produce different digests"]
		]
	]
]

let _ideasOracle = oracle(prompt: Str) -> [Any]? from _ideasExamples
_ideasOracle = noteSet("Return ONLY a JSON-MindScript AST for an array of strings.\n" + "Each string is a tiny, orthogonal test idea (one thing to check). Prefer boundaries, null/error path, typical case, and one property if relevant. Keep it 3–7 items.\n\n" + _manual, _ideasOracle)

# Public: testIdeas(spec:{doc!:Str, jsonSpec!:{name!:Str, type!:Type}}) -> [Str]?
let testIdeas = fun(spec: {}) -> [Str]? do
	# Reuse the same prompt shape and just append a single directive.
	let prompt = _genPrompt(spec) + "\n\nNow list tiny, orthogonal test ideas as an array of strings."
	let ast = _ideasOracle(prompt)
	if ast == null then
		return ast
	end
	let diags = astValidate(ast)
	if diags != [] then
		return noteSet(formatValue(diags[0]), null)
	end
	astEval(ast)
end

# ------ Test Ideas → Tests --------------------------------

# Helper: reuse the same spec prompt and specialize with a single idea.
let _testPrompt = fun(s: {}, idea: Str) -> Str do
	_genPrompt(s) + "\n\n" + "Write ONE tiny test as a JSON-MindScript AST for a function **value** with shape:\n" + "(impl: Any) -> { pass!: Bool, trace: {} }\n\n" + "Use this idea (verbatim) to pick concrete inputs/expectations:\n" + "«" + idea + "»\n" + "Keep it minimal: call impl, compare to expected, return { pass, trace:{ idea, input, got, want } }."
end

# FEW-SHOTS: same Name/Type/Doc prompt shape, each yields a single test fn.
# Each example EVALUATES to: (impl:Any) -> { pass!: Bool, trace:{} }
let _testExamples = [
	[
		join([
			"Implement a JSON-MindScript anonymous function or oracle matching this spec.",
			"",
			"Name: add",
			"Type: <type: Num -> Num -> Num>",
			"",
			"Doc:",
			"Add two numbers.\n\nArgs: a:Num, b:Num\nReturn: Num",
			"",
			"Write ONE tiny test as a JSON-MindScript AST for a function **value** with shape:",
			"(impl: Any) -> { pass!: Bool, trace: {} }",
			"",
			"Use this idea (verbatim) to pick concrete inputs/expectations:",
			"«simple positives (2.0 + 3.0 → 5.0)»",
			"Keep it minimal: call impl, compare to expected, return { pass, trace:{ idea, input, got, want } }."
		], "\n"),
		[
			"fun",
			["array", ["pair", ["id", "impl"], ["id", "Any"]]],
			["id", "Any"],
			[
				"block",
				[
					"assign",
					["decl", "args"],
					["array", ["num", 2.0], ["num", 3.0]]
				],
				[
					"assign",
					["decl", "got"],
					[
						"call",
						[
							"call",
							["id", "impl"],
							["idx", ["id", "args"], ["int", 0]]
						],
						["idx", ["id", "args"], ["int", 1]]
					]
				],
				["assign", ["decl", "want"], ["num", 5.0]],
				[
					"map",
					[
						"pair",
						["str", "pass"],
						[
							"binop",
							"==",
							["id", "got"],
							["id", "want"]
						]
					],
					[
						"pair",
						["str", "trace"],
						[
							"map",
							[
								"pair",
								["str", "idea"],
								[
									"str",
									"simple positives (2.0 + 3.0 → 5.0)"
								]
							],
							["pair", ["str", "input"], ["id", "args"]],
							[
								"pair",
								["str", "got"],
								["id", "got"]
							],
							["pair", ["str", "want"], ["id", "want"]]
						]
					]
				]
			]
		]
	],
	[
		join([
			"Implement a JSON-MindScript anonymous function or oracle matching this spec.",
			"",
			"Name: factorial",
			"Type: <type: Int -> Int?>",
			"",
			"Doc:",
			"Factorial with input check.\n\nArgs: n:Int\nReturn: Int?",
			"",
			"Write ONE tiny test as a JSON-MindScript AST for a function **value** with shape:",
			"(impl: Any) -> { pass!: Bool, trace: {} }",
			"",
			"Use this idea (verbatim) to pick concrete inputs/expectations:",
			"«error path n<0 → null with <negative input>»",
			"Keep it minimal: call impl, compare to expected, return { pass, trace:{ idea, input, got, want } }."
		], "\n"),
		[
			"fun",
			["array", ["pair", ["id", "impl"], ["id", "Any"]]],
			["id", "Any"],
			[
				"block",
				["assign", ["decl", "n"], ["int", -1]],
				[
					"assign",
					["decl", "got"],
					["call", ["id", "impl"], ["id", "n"]]
				],
				[
					"map",
					[
						"pair",
						["str", "pass"],
						["binop", "==", ["id", "got"], ["null"]]
					],
					[
						"pair",
						["str", "trace"],
						[
							"map",
							[
								"pair",
								["str", "idea"],
								[
									"str",
									"error path n<0 → null with <negative input>"
								]
							],
							["pair", ["str", "input"], ["id", "n"]],
							[
								"pair",
								["str", "got"],
								["id", "got"]
							],
							["pair", ["str", "want"], ["null"]]
						]
					]
				]
			]
		]
	],
	[
		join([
			"Implement a JSON-MindScript anonymous function or oracle matching this spec.",
			"",
			"Name: host",
			"Type: <type: Str -> Str?>",
			"",
			"Doc:",
			"Extract host from URL.\n\nArgs: url:Str\nReturn: Str?",
			"",
			"Write ONE tiny test as a JSON-MindScript AST for a function **value** with shape:",
			"(impl: Any) -> { pass!: Bool, trace: {} }",
			"",
			"Use this idea (verbatim) to pick concrete inputs/expectations:",
			"«valid http url → host (http://a.com → a.com)»",
			"Keep it minimal: call impl, compare to expected, return { pass, trace:{ idea, input, got, want } }."
		], "\n"),
		[
			"fun",
			["array", ["pair", ["id", "impl"], ["id", "Any"]]],
			["id", "Any"],
			[
				"block",
				["assign", ["decl", "url"], ["str", "http://a.com"]],
				[
					"assign",
					["decl", "got"],
					["call", ["id", "impl"], ["id", "url"]]
				],
				["assign", ["decl", "want"], ["str", "a.com"]],
				[
					"map",
					[
						"pair",
						["str", "pass"],
						[
							"binop",
							"==",
							["id", "got"],
							["id", "want"]
						]
					],
					[
						"pair",
						["str", "trace"],
						[
							"map",
							[
								"pair",
								["str", "idea"],
								[
									"str",
									"valid http url → host (http://a.com → a.com)"
								]
							],
							["pair", ["str", "input"], ["id", "url"]],
							[
								"pair",
								["str", "got"],
								["id", "got"]
							],
							["pair", ["str", "want"], ["id", "want"]]
						]
					]
				]
			]
		]
	]
]

let _testOracle = oracle(prompt: Str) -> [Any]? from _testExamples
_testOracle = noteSet("Return ONLY a JSON-MindScript AST for a SINGLE tiny test function value.\n" + "Shape: (impl: Any) -> { pass!: Bool, trace: {} }\n" + "Call impl with concrete inputs implied by the idea; compare to expected; return pass + trace.\n\n" + _manual, _testOracle)

# Helper: build one test from a single idea, then annotate it.
let _buildTest = fun(spec: {}, idea: Str) -> Any? do
	let ast = _testOracle(_testPrompt(spec, idea))
	if ast == null then
		println(idea + ": 3")
		return ast
	end
	let diags = astValidate(ast)
	if diags != [] then
		return
	end
	let t = astEval(ast)
	noteSet("test idea: " + idea, t)
end

# Public: parallel test generation + per-test annotation
let tests = fun(spec: {}) -> [Any]? do
	let ideas = testIdeas(spec)
	if ideas == null then
		return
	end

	# Spawn one worker per idea (parallelizable)
	let ps = []
	for idea in ideas do
		push(ps, procSpawn(fun() do
			_buildTest(spec, idea)
		end))
	end

	# Collect results, drop nulls
	let out = []
	for r in procJoinAll(ps) do
		if r != null then
			push(out, r)
		end
	end
	out
end