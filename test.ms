let guess = astEval(
["fun",
  ["array"],
  ["id","Bool"],
  ["block",
    ["assign", ["decl","secret"], ["call", ["id","randInt"], ["int", 10]]],
    ["call", ["id","write"], ["id","STDOUT"], ["str","Guess a number 0-9: "]],
    ["call", ["id","flush"], ["id","STDOUT"]],
    ["assign", ["decl","line"], ["call", ["id","readLine"], ["id","STDIN"]]],
    ["assign", ["decl","guess"], ["call", ["id","int"], ["id","line"]]],
    ["assign", ["decl","ok"], ["call", ["call", ["id","isType"], ["id","guess"]], ["type", ["id","Int"]]]],
    ["if",
      ["pair",
        ["id","ok"],
        ["block",
          ["assign", ["decl","g"], ["id","guess"]],
          ["if",
            ["pair",
              ["binop","==", ["id","g"], ["id","secret"]],
              ["block",
                ["call", ["id","write"], ["id","STDOUT"], ["str","correct!\n"]],
                ["call", ["id","flush"], ["id","STDOUT"]],
                ["return", ["bool", true]]
              ]
            ],
            ["block",
              ["call", ["id","write"], ["id","STDOUT"],
                ["call", ["call", ["id","sprintf"], ["str","nope, it was %d\n"]],
                  ["array", ["id","secret"]]]],
              ["call", ["id","flush"], ["id","STDOUT"]],
              ["return", ["bool", false]]
            ]
          ]
        ]
      ],
      ["block",
        ["call", ["id","write"], ["id","STDOUT"], ["str","not a number.\n"]],
        ["call", ["id","flush"], ["id","STDOUT"]],
        ["return", ["bool", false]]
      ]
    ]
  ]
]
)

guess()
