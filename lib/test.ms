let Test = type {name: Str, age: Int}

let print = fun(t) do
	printf("[print] " + formatValue(t) + "\n", [])
end