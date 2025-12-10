# bot.ms — ultra-minimal chat loop (now with optional system/manual)

# Some quick tests!

# an object
let obj = {
	name: "John", # the name
	age: 17 # the age
}

# an array
let arr = [
	"John", # the name
	17 # the age
]

# a function
let f = fun(name: # the name
Str, age: # the age
Int) do
	name
	age
	return
end

obj.name
obj.name
obj["name"]
arr[0]
arr[1]

# -----------------------------
# Public data types
# -----------------------------

let Role = type Enum["user", "assistant"]
let Message = type {role!: Role, text!: Str}
let ChatState = type {summary: Str?, turns!: [Message]}

# Add optional system background (e.g., a manual) to config.
let ChatConfig = type {maxTurns!: Int, system: Str?}

# Summarize the text.
let summarize = oracle(past: [Str], present: Str) -> Str

let _trimIfNeeded = fun(state: ChatState, cfg: ChatConfig) -> ChatState do
	if len(state.turns) <= cfg.maxTurns then
		state
	else
		let cut = len(state.turns) - cfg.maxTurns
		let older = slice(state.turns, 0, cut)
		let keep = slice(state.turns, cut, len(state.turns))
		let s = summarize(older, state.summary)
		let newSum = if s == null then
			state.summary
		else
			s.gist
		end
		{summary: newSum, turns: keep}
	end
end

# -----------------------------
# Oracles
# -----------------------------

# Core responder.
# Be helpful and concise.
# Respect: system (background/manual) + summary + turns.
# No external facts.
let respond = oracle(summary: Str?, context: [Message], user: Str, system: Str?) -> {
	reply!: Str
}

# -----------------------------
# Single-turn interaction
# -----------------------------

let step = fun(state: ChatState, cfg: ChatConfig, userMsg: Str) -> {
	reply!: Str,
	state!: ChatState
} do
	let s0 = _trimIfNeeded(state, cfg)
	let ctx = push(s0.turns, userMsg)

	let r = respond(s0.summary, ctx, userMsg, cfg.system)
	if r == null then
		let fb = "Sorry—I'm having trouble right now. Could you rephrase?"
		let turns2 = push(ctx, fb)
		return {state: {summary: s0.summary, turns: turns2}, reply: fb}
	end

	let reply = r.reply
	let turns2 = push(ctx, reply)
	{
		state: {summary: s0.summary, turns: turns2},
		reply: reply
	}
end

# -----------------------------
# Defaults
# -----------------------------

let initState = {summary: null, turns: []}
let defaultConfig = {maxTurns: 10, system: null}

# -----------------------------
# Curried bot constructors
# -----------------------------

# bot(cfg?) -> (userMsg: Str) -> Str
# Create a stateful function with its own state and config.
let bot = fun(cfg: ChatConfig?) -> Str -> Str do
	let st = initState
	let cf = if cfg == null then
		defaultConfig
	else
		{maxTurns: cfg.maxTurns, system: cfg.system}
	end
	fun(msg: Str) -> Str do
		let res = step(st, cf, msg)
		st = res.state
		res.reply
	end
end

# Convenience: supply a manual/background string while keeping other defaults.
let botWithSystem = fun(system: Str, cfg: ChatConfig?) -> Str -> Str do
	let base = if cfg == null then
		defaultConfig
	else
		{
			maxTurns: cfg.maxTurns,
			system: if cfg.system == null then
				system
			else
				cfg.system
			end
		}
	end
	bot({maxTurns: base.maxTurns, system: system})
end

let ask = bot() # Ask the bot.