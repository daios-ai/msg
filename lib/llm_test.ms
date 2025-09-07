# Tests for llm.ms — minimal LLM backend manager.

let testing = import("testing")
let llm = import("./llm.ms")

# Small helper: assert a condition or fail with a message.
#
# Args:
# 	ok:   Bool — condition to verify
# 	msg:  Str  — failure message
#
# Returns:
# 	Bool
let _assert = fun(ok: Bool, msg: Str) -> Bool do
	if not ok then
		fail(msg)
	end
	true
end

# Convert a Str? soft error to its annotation (or "").
#
# Args:
# 	x: Str? — maybe-null string
#
# Returns:
# 	Str
let _ann = fun(x: Str?) -> Str do
	if x == null then
		let a = noteGet(x)   # annotated null → message (Str?)
		if a == null then "" else a end
	else
		""
	end
end

# Reset llm into a deterministic state (no secrets, no model).
#
# Returns:
# 	Bool
let _reset = fun(_: Null) -> Bool do
	# Choose a known backend and clear model/options.
	let _ = llm.useBackend("ollama")
	let _ = llm.setOptions({model: null, options: {}})
	# Also clear OpenAI auth to make 'authed' predictable when we switch to it.
	let _ = llm.useBackend("openai")
	let _ = llm.auth({"apiKey": null})
	let _ = llm.setOptions({model: null, options: {}})
	# Default back to ollama for most tests.
	let _ = llm.useBackend("ollama")
	true
end

# backends() exposes the built-ins.
testing.test("llm/backends lists core names", fun(_: Null) -> Bool do
	let _ = _reset(null)
	let xs = llm.backends(null)
	let s = "," + join(xs, ",") + ","
	_assert(match(",ollama,", s) != [], "ollama not listed")
	_assert(match(",openai,", s) != [], "openai not listed")
end)

# status() shape and defaults for Ollama (authed=true, model=null).
testing.test("llm/status default (ollama)", fun(_: Null) -> Bool do
	let _ = _reset(null)
	let st = llm.status(null)
	_assert(st.backend == "ollama", "backend should be ollama")
	_assert(st.model == null, "model should be null by default")
	_assert(st.authed == true, "ollama should report authed=true")
	_assert(typeOf(st.options) == type {Str: Any}, "options should be a map")
end)

# useBackend: unknown returns annotated null.
testing.test("llm/useBackend unknown → soft error", fun(_: Null) -> Bool do
	let _ = _reset(null)
	let r = llm.useBackend("does-not-exist")
	_assert(r == null, "expected annotated null on unknown backend")
	let msg = noteGet(r)
	_assert(bool(msg), "expected error annotation on null")
end)

# getOptions / setOptions round-trip merge.
testing.test("llm/options round-trip + merge", fun(_: Null) -> Bool do
	let _ = _reset(null)
	let before = llm.getOptions(null)
	_assert(before.backend == "ollama", "getOptions.backend mismatch")
	_assert(mapHas(before, "options"), "getOptions.options missing")

	let _ = llm.setOptions({model: "tiny-llm", options: {"temperature": 0.7}})
	let after = llm.getOptions(null)
	_assert(after.model == "tiny-llm", "setOptions did not set model")
	_assert(after.options.temperature == 0.7, "temperature merge failed")

	# Merge again (right wins).
	let _ = llm.setOptions({options: {"temperature": 0.1, "top_p": 0.9}})
	let again = llm.getOptions(null)
	_assert(again.options.temperature == 0.1, "temperature second merge failed")
	_assert(again.options.top_p == 0.9, "top_p not applied")
end)

# auth() toggles authed for OpenAI; status never exposes secrets.
testing.test("llm/auth openai toggles authed; secrets hidden", fun(_: Null) -> Bool do
	let _ = _reset(null)
	let _ = llm.useBackend("openai")
	let st0 = llm.status(null)
	_assert(st0.authed == false, "openai should start authed=false without key")

	let _ = llm.auth({"apiKey": "sk-test-123"})
	let st1 = llm.status(null)
	_assert(st1.authed == true, "openai authed should be true after auth()")

	# Ensure status does not leak apiKey.
	_assert(not mapHas(st1, "apiKey"), "status must not expose secrets")
end)

# useModel sets model; reflected in status().
testing.test("llm/useModel updates status", fun(_: Null) -> Bool do
	let _ = _reset(null)
	let _ = llm.useBackend("ollama")
	let _ = llm.useModel("phi3")
	let st = llm.status(null)
	_assert(st.model == "phi3", "useModel did not update model")
end)

# exec() can be stubbed; we avoid real network by replacing backend.exec.
testing.test("llm/exec stubbed returns boxed JSON", fun(_: Null) -> Bool do
	let _ = _reset(null)
	let _ = llm.useBackend("ollama")
	# Make sure a model is set so the stub mirrors normal preconditions.
	let _ = llm.setOptions({model: "stub-model"})

	# Replace exec with a stub that returns boxed JSON.
	let b = llm._state.backends.ollama
	b.exec = fun(self: {}, prompt: Str) -> Str? do
		jsonStringify({"output": "ok:" + prompt})
	end

	let out = llm.exec("PING")
	_assert(out != null, "exec returned null")
	let j = jsonParse(out)
	_assert(j.output == "ok:PING", "stubbed exec did not return boxed output")
end)

# models(): stub listModels to avoid network and exercise happy path.
testing.test("llm/models stubbed happy path", fun(_: Null) -> Bool do
	let _ = _reset(null)
	let _ = llm.useBackend("ollama")
	let b = llm._state.backends.ollama
	b.listModels = fun(self: {}) -> [Str]? do
		["alpha", "beta"]
	end

	let xs = llm.models(null)
	_assert(xs != null, "models returned null")
	_assert(len(xs) == 2, "expected two models")
	_assert(xs[0] == "alpha" and xs[1] == "beta", "models content mismatch")
end)
