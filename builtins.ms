# Write builtins (and other visible bindings) to builtins.txt,
# skipping names that start with "_".
# Format: <name>: <Type> — <first line of note>
do
	let env = snapshot(null)

	# Collect names (skip those starting with "_")
	let names = []
	for [k, _] in env do
		if len(k) > 0 and substr(k, 0, 1) == "_" then
			continue
		end
		names = names + [k]
	end

	# Sort for stable output (insertion sort)
	let n = len(names)
	let i = 0
	while i < n do
		let j = i
		while j > 0 and names[j - 1] > names[j] do
			let tmp = names[j - 1]
			names[j - 1] = names[j]
			names[j] = tmp
			j = j - 1
		end
		i = i + 1
	end

	# Build lines
	let lines = []
	for name in names do
		let v = env[name]
		let T = typeOf(v)

		let tStr0 = str(T)
		let tStr =
			if tStr0 != null then tStr0 else formatValue(T) end

		let note = noteGet(v)
		let summary =
			if note == null then
				""
			else
				split(note, "\n")[0]
			end

		let line = sprintf("%s: %s — %s", [name, tStr, summary])
		lines = lines + [line]
	end

	writeFile("builtins.txt", join(lines, "\n"))
end
