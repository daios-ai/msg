
# ------ Coding Oracle ----------------------------------

let _genManual = fun() do
	let ROOT = osEnv("MSGPATH")
	if ROOT == null then
		# environment variable MSGPATH is undefined
		return null
	end
	let man = readFile(pathJoin([ROOT, "data", "msjson.md"]))
	if man == null then
		# JSON-MindScript manual not found ("MSGPATH/data/msjson.md")
		return null
	end
	man
end()

let _genExamples = [
    ["Add two numbers (Int/Num) and return their sum as a Num.",
    [
	"annot",
	["str","Add two numbers.\n\nArgs: a:Num, b:Num\nReturn: Num"],
	[
		"fun",
		[
			"array",
			["pair", ["id","a"], ["id","Num"]],
			["pair", ["id","b"], ["id","Num"]]
		],
		["id","Num"],
		[
			"block",
			["binop","+", ["id","a"], ["id","b"]]
		]
	]
]
    ],
    [
        "Compute factorial(n) for n ≥ 0 using a while loop; return soft-null if n < 0.",
        [
	"annot",
	["str","Factorial with input check.\n\nArgs: n:Int\nReturn: Int? (annot-null if n < 0)"],
	[
		"fun",
		[
			"array",
			["pair", ["id","n"], ["id","Int"]]
		],
		["unop","?", ["id","Int"]],
		[
			"block",
			["if",
				[
					"pair",
					["binop","<", ["id","n"], ["int",0]],
					[
						"block",
						["return", ["annot", ["str","<negative input>"], ["null"]]]
					]
				],
				["block",
					["assign", ["decl","res"], ["int",1]],
					["assign", ["decl","i"], ["int",2]],
					["while",
						["binop","<=", ["id","i"], ["id","n"]],
						[
							"block",
							["assign", ["id","res"], ["binop","*", ["id","res"], ["id","i"]]],
							["assign", ["id","i"], ["binop","+", ["id","i"], ["int",1]]]
						]
					],
					["id","res"]
				]
			]
		]
	]
]

    ],
    [
        "Pick a primary color.",
[
	"annot",
	["str","Pick a primary color.\n\nArgs: none\nReturn: Enum(red|green|blue)? (soft-null if unavailable)"],
	[
		"oracle",
		["array"],
		["enum", ["str","red"], ["str","green"], ["str","blue"]],
		[
			"map",
			["pair", ["str","doc"], ["str","Pick one of the allowed colors."]],
			["pair", ["str","examples"],
				[
					"array",
					["array", ["array"], ["str","red"]],
					["array", ["array"], ["str","green"]],
					["array", ["array"], ["str","blue"]]
				]
			]
		]
	]
]

    ],
    [
        "Suggest a concise git commit message from a diff.",
        [
	"annot",
	["str","Generate a short, imperative git commit message.\n\nArgs: diff:Str\nReturn: Str? (annot-null if cannot summarize)"],
	[
		"oracle",
		[
			"array",
			["pair", ["id","diff"], ["id","Str"]]
		],
		["id","Str"],
		[
			"map",
			["pair", ["str","doc"], ["str","Summarize the diff into a concise commit subject (<= 72 chars)."]],
			["pair", ["str","examples"],
				[
					"array",
					[
						"array",
						["array", ["str","+ add input validation to /login"]],
						["str","add input validation for /login"]
					],
					[
						"array",
						["array", ["str","- remove dead code in UserService"]],
						["str","remove dead code in UserService"]
					]
				]
			]
		]
	]
]

    ],
    [
        "Parse a URL string and return its host; soft-null if invalid URL.",
        [
	"annot",
	["str","Extract host from URL.\n\nArgs: url:Str\nReturn: Str? (annot-null if invalid)"],
	[
		"fun",
		[
			"array",
			["pair", ["id","url"], ["id","Str"]]
		],
		["unop","?", ["id","Str"]],
		[
			"block",
			["assign", ["decl","u"], ["call", ["id","urlParse"], ["id","url"]]],
			["if",
				[
					"pair",
					["binop","==", ["id","u"], ["null"]],
					[
						"block",
						["return", ["annot", ["str","<invalid url>"], ["null"]]]
					]
				],
				[
					"block",
					["idx", ["id","u"], ["str","host"]]
				]
			]
		]
	]
]

    ],
]

let _genOracle = oracle(spec: Str) -> []? from _genExamples
let _genComment = (
	"Write a JSON-MindScript anonymous function or oracle for the requested behavior."
	+ "\n\n"
	+ _genManual
)
_genOracle = noteSet(_genComment, _genOracle)

# ------ Public ----------------------------------

# Generate a function/oracle given a specification.
#
# Args: 
# - spec:Str A description of the desired behavior. 
#
# Return:
# - A function or oracle.
let gen = fun(spec: Str) -> Any do
    let g = _genOracle(spec)
	if g == null then
		return g
	end
    let validation = astValidate(g)
    if validation != [] then
        return noteSet(validation[0].message, null)  
    end
    return astEval(g)
end

