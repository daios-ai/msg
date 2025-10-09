let Test = type {age: Int, name: Str}

let print = fun(t) do
	printf("[print] " + formatValue(t) + "\n", [])
end