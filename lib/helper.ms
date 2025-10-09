# mschat.ms — patch: decode escapes before printing

let bot = import("bot")

# Decode JSON-style escapes in a string.
# Strategy: wrap as a JSON string literal, escaping only the quotes,
# then parse; on failure, return the original input.
let _unescape = fun(s: Str) -> Str do
	# Escape only double quotes; keep backslashes as-is
	let esc = replace("\"", "\\\"", s)
	let json = "\"" + esc + "\""
	let r = try(fun() do
		jsonParse(json)
	end)
	if r.ok and isType(r.value, type Str) then
		r.value
	else
		s
	end
end

# unchanged
let _readManual = fun() -> Str? do
	let base = osEnv("MSGPATH")
	if base == null then
		return
	end
	let p = pathJoin([base, "data", "msplain.md"])
	let fh = open(p, "r")
	if fh == null then
		return
	end
	let s = readAll(fh)
	close(fh)
	s
end

# --- MOD: decode before println ---
let _printer = fun(ask: Str -> Str) -> Str -> Bool do
	fun(q: Str) -> Bool do
		let r = try(fun() do
			ask(q)
		end)
		if r.ok and r.value != null then
			println(_unescape(r.value))
			true
		else
			println("Sorry—I'm having trouble right now.")
			false
		end
	end
end

# constructors (unchanged)
let msBot = fun(cfg: {maxTurns!: Int, system: Str?}?) -> Str -> Bool do
	let man = _readManual()
	let ask = if man == null then
		bot.bot(cfg)
	else
		bot.botWithSystem(man, cfg)
	end
	_printer(ask)
end

let msBotWithSystem = fun(system: Str, cfg: {maxTurns!: Int, system: Str?}?) -> Str -> Bool do
	_printer(bot.botWithSystem(system, cfg))
end

let ask = msBot(null)