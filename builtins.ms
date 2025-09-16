# Write builtins (and other visible bindings) to builtins.txt,
# skipping names that start with "_".
# Format: <name>: <Type|fun line from formatValue> — <first line of note>
do
	let env = snapshot(null)

	# Collect names (skip leading "_")
	let names = []
	for [k, _] in env do
		if len(k) > 0 and substr(k, 0, 1) == "_" then
			continue
		end
		names = names + [k]
	end

	# Sort (insertion sort)
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

	# Helpers
	let _trim = fun(s: Str) -> Str do
		strip(s)
	end
	let _rstrip = fun(s: Str) -> Str do
		rstrip(s)
	end
	let _startsWith = fun(s: Str, p: Str) -> Bool do
		len(s) >= len(p) and substr(s, 0, len(p)) == p
	end

	let lines = []
	for name in names do
		let v = env[name]
		let fv = formatValue(v)

		# Doc = first line of explicit note, else first '#' line from formatValue
		let doc = ""
		let note = noteGet(v)
		if note != null then
			doc = _rstrip(split(note, "\n")[0])
		elif fv != null then
			for raw in split(fv, "\n") do
				let line = _trim(raw)
				if _startsWith(line, "#") then
					doc = if _startsWith(line, "# ") then
						_rstrip(substr(line, 2, len(line) - 2))
					else
						_rstrip(substr(line, 1, len(line) - 1))
					end
					break
				end
			end
		end

		# Signature = LAST non-empty line of formatValue(v), verbatim
		let sig = ""
		if fv != null then
			let parts = split(fv, "\n")
			let k = len(parts) - 1
			while k >= 0 do
				let L = _trim(parts[k])
				if len(L) > 0 then
					sig = L
					break
				end
				k = k - 1
			end
		end

		# Fallback if formatValue returned null/empty (rare)
		if sig == "" then
			let t = str(typeOf(v))
			sig = if t != null then
				t
			else
				"<type: Any>"
			end
		end

		let line = sprintf("%s: %s — %s", [name, sig, doc])
		lines = lines + [line]
	end

	writeFile("builtins.txt", join(lines, "\n"))
end