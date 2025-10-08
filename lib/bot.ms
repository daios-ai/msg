# bot.ms — ultra-minimal chat loop (no RAG)
# Tiny types, two oracles, small helpers, curried bot constructor.

# -----------------------------
# Public data types
# -----------------------------

let Role = type Enum["user", "assistant"]

let Message = type { role!: Role, text!: Str }

let ChatState = type {
	summary: Str?,
	turns!: [Message]
}

let ChatConfig = type { maxTurns!: Int }


# -----------------------------
# Oracles
# -----------------------------

# Core responder.
# Be helpful and concise. Respect summary + turns. No external facts.
let respond = oracle(
	summary: Str?,
	context: [Message],
	user: Str
) -> { reply!: Str }

# Summarize overflowed history (≤ 120 words; preserve intent/decisions).
let summarize = oracle(
	older: [Message],
	prior: Str?
) -> { gist!: Str }


# -----------------------------
# Helpers
# -----------------------------

let _push = fun(ts: [Message], r: Role, t: Str) -> [Message] do
	ts + [{ role: r, text: t }]
end

let _trimIfNeeded = fun(state: ChatState, cfg: ChatConfig) -> ChatState do
	if len(state.turns) <= cfg.maxTurns then
		state
	else
		let cut = len(state.turns) - cfg.maxTurns
		let older = slice(state.turns, 0, cut)
		let keep = slice(state.turns, cut, len(state.turns))
		let s = summarize(older, state.summary)
		let newSum = if s == null then state.summary else s.gist end
		{ summary: newSum, turns: keep }
	end
end


# -----------------------------
# Single-turn interaction
# -----------------------------

let step = fun(state: ChatState, cfg: ChatConfig, userMsg: Str) -> { state!: ChatState, reply!: Str } do
	let s0 = _trimIfNeeded(state, cfg)
	let ctx = _push(s0.turns, "user", userMsg)

	let r = respond(s0.summary, ctx, userMsg)
	if r == null then
		let fb = "Sorry—I'm having trouble right now. Could you rephrase?"
		let turns2 = _push(ctx, "assistant", fb)
		return { state: { summary: s0.summary, turns: turns2 }, reply: fb }
	end

	let reply = r.reply
	let turns2 = _push(ctx, "assistant", reply)
	{ state: { summary: s0.summary, turns: turns2 }, reply: reply }
end


# -----------------------------
# Defaults
# -----------------------------

let initState = { summary: null, turns: [] }
let defaultConfig = { maxTurns: 10 }


# -----------------------------
# Curried bot constructor
# -----------------------------

# bot(cfg?) -> (userMsg: Str) -> Str
# Create a stateful function that maintains its own state and config.
let bot = fun(cfg: ChatConfig?) -> Str -> Str do
	let st = initState
	let cf = if cfg == null then defaultConfig else cfg end
	fun(msg: Str) -> Str do
		let res = step(st, cf, msg)
		st = res.state
		res.reply
	end
end

# Ask the bot.
let ask = bot()