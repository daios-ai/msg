# llm_test.ms — tiny, orthogonal tests for llm.ms (current contract: raw-string, no boxing)
# 
# Usage:
# let T = import("testing")
# T.run({ root: ".", verbose: true, parallel: false, timeoutMs: 0 })

let T = import("testing")
let L = import("llm")

# ---------- Model constants (fill these in) ----------
let OPENAI_MODEL = "gpt-5-mini"
let OPENAI_RESPONSES_MODEL = "gpt-5-mini"
let ANTHROPIC_MODEL = "claude-sonnet-4-20250514"
let COHERE_MODEL = "command-a-03-2025"
let OLLAMA_MODEL = "Phi3:latest"

# Tight network/config knobs
let _tight = {
	timeoutMs: 5000,
	options: {temperature: 0.0, top_p: 1.0, max_tokens: 16}
}

# --------
# Local helpers
# --------

# Array contains (strings)
let _containsStr = fun(xs: [Str], s: Str) -> Bool do
	for x in xs do
		if x == s then
			return true
		end
	end
	false
end

# --------
# Module surface
# --------

T.test("llm/backends_lists_registered", fun(_: Null) do
	let xs = L.backends(null)
	T.assertType(xs, type [Str])
	T.assert(_containsStr(xs, "ollama"), "ollama missing")
	T.assert(_containsStr(xs, "openai"), "openai missing")
	T.assert(_containsStr(xs, "openai-responses"), "openai-responses missing")
	T.assert(_containsStr(xs, "anthropic"), "anthropic missing")
	T.assert(_containsStr(xs, "cohere"), "cohere missing")
end)

T.test("llm/init_returns_names_and_current", fun(_: Null) do
	let r = L.init(null)
	T.assertType(r.backends, type [Str])
	T.assertType(r.current, type Str)
	T.assert(len(r.backends) >= 5, "expected at least 5 backends")
	T.assert(_containsStr(r.backends, r.current), "current not in backends")
end)

T.test("llm/status_shape_without_assuming_default", fun(_: Null) do
	let s = L.status(null)
	let xs = L.backends(null)
	T.assert(_containsStr(xs, s.backend), "status.backend not a known backend")
	T.assertType(s.options, type {})
	T.assertType(s.authed, type Bool)
	# model may be null or Str
	if s.model != null then
		T.assertType(s.model, type Str)
	end
end)

T.test("llm/useBackend_unknown_softnull", fun(_: Null) do
	let before = L.status(null).backend
	let res = L.useBackend("nope")
	T.assertEq(res, null) # soft-null

	T.assertEq(L.status(null).backend, before)
end)

T.test("llm/useBackend_switch_roundtrip", fun(_: Null) do
	let orig = L.status(null).backend
	let _ = L.useBackend("openai")
	T.assertEq(L.status(null).backend, "openai")
	let _2 = L.useBackend(orig)
	T.assertEq(L.status(null).backend, orig)
end)

# -------------
# Config & auth
# -------------

T.test("llm/getConfig_shape_types", fun(_: Null) do
	let cfg = L.getConfig(null)
	T.assertType(cfg.backend, type Str)
	if cfg.baseUrl != null then
		T.assertType(cfg.baseUrl, type Str)
	end
	if cfg.model != null then
		T.assertType(cfg.model, type Str)
	end
	T.assertType(cfg.options, type {})
	if mapHas(cfg, "timeoutMs") and cfg.timeoutMs != null then
		T.assertType(cfg.timeoutMs, type Int)
	end
end)

T.test("llm/setConfig_replaces_options_shallow", fun(_: Null) do
	let _ = L.useBackend("ollama")
	# Give options with three keys.
	let _1 = L.setConfig({options: {temperature: 0.1, top_p: 0.5, top_k: 20}})
	# Replace with a smaller set; missing keys should disappear.
	let _2 = L.setConfig({options: {temperature: 0.25, top_p: 0.9}})
	let after = L.getConfig(null).options
	T.assertEq(after.temperature, 0.25)
	T.assertEq(after.top_p, 0.9)
	T.assert(not mapHas(after, "top_k"), "expected options replacement, but top_k persisted")
end)

T.test("llm/setConfig_ignores_unknown_top_level", fun(_: Null) do
	let snap = L.status(null)
	let _ = L.setConfig({foo: 1})
	let now = L.status(null)
	T.assertEq(snap.backend, now.backend)
	T.assertEq(snap.model, now.model)
	T.assertEq(snap.authed, now.authed)
end)

T.test("llm/setConfig_allows_clearing_model", fun(_: Null) do
	let _ = L.useBackend("ollama")
	let _ = L.useModel("phi")
	T.assertEq(L.status(null).model, "phi")
	let _ = L.setConfig({model: null})
	T.assert(L.status(null).model == null, "model should be cleared to null")
end)

T.test("llm/setConfig_updates_baseUrl", fun(_: Null) do
	let _ = L.useBackend("ollama")
	let _ = L.setConfig({baseUrl: "http://localhost:9999"})
	T.assertEq(L.getConfig(null).baseUrl, "http://localhost:9999")
end)

T.test("llm/setConfig_timeoutMs_roundtrip", fun(_: Null) do
	let _ = L.useBackend("openai")
	let _ = L.setConfig({timeoutMs: 1234})
	let cfg = L.getConfig(null)
	T.assertType(cfg.timeoutMs, type Int)
	T.assertEq(cfg.timeoutMs, 1234)
end)

T.test("llm/auth_sets_authed_for_openai", fun(_: Null) do
	let _ = L.useBackend("openai")
	let _ = L.auth({apiKey: null}) # ensure cleared

	let s0 = L.status(null)
	T.assertEq(s0.authed, false)
	let _2 = L.auth({apiKey: "XYZ"})
	let s1 = L.status(null)
	T.assertEq(s1.authed, true)
end)

T.test("llm/auth_shallow_merge_nonkey_fields", fun(_: Null) do
	let _ = L.useBackend("openai")
	let _ = L.auth({apiKey: null}) # ensure unauth

	let _2 = L.auth({organization: "org-123"}) # no apiKey provided

	let s = L.status(null)
	T.assertEq(s.authed, false) # still unauthenticated

end)

T.test("llm/auth_clears_key_when_null", fun(_: Null) do
	let _ = L.useBackend("openai")
	let _ = L.auth({apiKey: "XYZ"})
	T.assertEq(L.status(null).authed, true)
	let _2 = L.auth({apiKey: null})
	T.assertEq(L.status(null).authed, false)
end)

T.test("llm/status_authed_true_for_local_backends", fun(_: Null) do
	let _ = L.useBackend("ollama")
	T.assertEq(L.status(null).authed, true)
end)

T.test("llm/auth_is_per_backend", fun(_: Null) do
	# Start from a clean slate on both backends.
	let _ = L.useBackend("openai")
	let _ = L.auth({apiKey: null})
	let _ = L.useBackend("cohere")
	let _ = L.auth({apiKey: null})

	# Auth only OpenAI.
	let _ = L.useBackend("openai")
	let _ = L.auth({apiKey: "XYZ"})
	T.assertEq(L.status(null).authed, true)

	# Switching to Cohere should not inherit OpenAI auth.
	let _2 = L.useBackend("cohere")
	T.assertEq(L.status(null).authed, false)

	# Switching back to OpenAI preserves its own auth.
	let _3 = L.useBackend("openai")
	T.assertEq(L.status(null).authed, true)
end)

# ----------------------
# Exec negative (no HTTP)
# ----------------------

T.test("llm/ollama_exec_requires_model", fun(_: Null) do
	let _ = L.useBackend("ollama")
	let _ = L.setConfig({model: null}) # ensure no model

	let out = L.exec("hello")
	T.assertEq(null, out) # expect soft-null

end)

T.test("llm/openai_exec_requires_key", fun(_: Null) do
	let _ = L.useBackend("openai")
	let _ = L.auth({apiKey: null}) # ensure no key

	let out = L.exec("hi")
	T.assertEq(out, null) # soft-null due to missing API key

end)

T.test("llm/openai_exec_requires_model_even_when_authed", fun(_: Null) do
	let _ = L.useBackend("openai")
	let _ = L.auth({apiKey: "XYZ"}) # dummy value for shape only

	let _ = L.setConfig({model: null}) # ensure no model

	let out = L.exec("hi")
	T.assertEq(null, out) # soft-null due to missing model

end)

T.test("llm/cohere_exec_requires_model", fun(_: Null) do
	let _ = L.useBackend("cohere")
	let _ = L.auth({apiKey: "XYZ"})
	let out = L.exec("ping")
	T.assertEq(out, null) # soft-null due to missing model

end)

T.test("llm/exec_ignores_unknown_options", fun(_: Null) do
	let _ = L.useBackend("openai")
	let _ = L.auth({apiKey: "XYZ"})
	let _2 = L.setConfig({options: {temperature: 0.1, weird_unused_flag: 1}})
	# Still soft-null due to missing model, but must not hard-fail.
	let out = L.exec("hi")
	T.assertEq(out, null)
end)

# -----------
# Models
# -----------

T.test("llm/models_softnull_when_missing_key", fun(_: Null) do
	let _ = L.useBackend("openai")
	let _ = L.auth({apiKey: null})
	let m = L.models(null)
	T.assertEq(m, null) # soft-null due to missing key

end)

T.test("llm/models_supported_returns_array_or_null", fun(_: Null) do
	let _ = L.useBackend("ollama")
	let m = L.models(null)
	# Either returns [Str] if daemon is up, or soft-null (request failed).
	T.assert(m == null or typeOf(m) == type [Str], "expected [Str] or null")
end)

# --------------
# State integrity
# --------------

T.test("llm/status_reflects_useModel", fun(_: Null) do
	let _ = L.useBackend("ollama")
	let _ = L.useModel("phi")
	T.assertEq(L.status(null).model, "phi")
	T.assertEq(L.getConfig(null).model, "phi")
end)

T.test("llm/useBackend_switch_is_localized", fun(_: Null) do
	let _ = L.useBackend("openai")
	let _ = L.useModel("gpt-X")
	let _2 = L.useBackend("anthropic")
	T.assertEq(L.status(null).model, null) # anthropic model untouched

	let _3 = L.useBackend("openai")
	T.assertEq(L.status(null).model, "gpt-X") # original model preserved

end)

# =========================
# OpenAI (Chat Completions)
# =========================

T.test("llm/live/openai/models_lists_nonempty_if_authed", fun(_: Null) do
	L.useBackend("openai")
	L.setConfig({timeoutMs: _tight.timeoutMs})
	if not L.status(null).authed then
		println("SKIP: openai not authed")
		return
	end

	let ms = L.models(null)
	if ms == null then
		println("SKIP: OpenAI /models soft-null (transport or rate limit)")
		return
	end
	T.assert(typeOf(ms) == type [Str], "models() should return [Str]")
	T.assert(len(ms) >= 1, "expected at least 1 model")
end)

T.test("llm/live/openai/exec_returns_text_if_model", fun(_: Null) do
	L.useBackend("openai")
	L.setConfig(_tight)
	if not L.status(null).authed then
		println("SKIP: openai not authed")
		return
	end
	if OPENAI_MODEL == null then
		println("SKIP: set OPENAI_MODEL constant")
		return
	end
	L.useModel(OPENAI_MODEL)

	let out = L.exec("Return a short word.")
	if out == null then
		println("SKIP: OpenAI exec soft-null (transport/rate limit/model issue)")
		return
	end
	T.assertType(out, type Str)
	T.assert(len(out) > 0, "expected non-empty response text")
end)

# =========================
# OpenAI (Responses API)
# =========================

T.test("llm/live/openai-responses/models_lists_nonempty_if_authed", fun(_: Null) do
	L.useBackend("openai-responses")
	L.setConfig({timeoutMs: _tight.timeoutMs})
	if not L.status(null).authed then
		println("SKIP: openai-responses not authed")
		return
	end

	let ms = L.models(null)
	if ms == null then
		println("SKIP: OpenAI Responses /models soft-null")
		return
	end
	T.assert(typeOf(ms) == type [Str], "models() should return [Str]")
	T.assert(len(ms) >= 1, "expected at least 1 model")
end)

T.test("llm/live/openai-responses/exec_returns_text_if_model", fun(_: Null) do
	L.useBackend("openai-responses")
	L.setConfig(_tight)
	if not L.status(null).authed then
		println("SKIP: openai-responses not authed")
		return
	end
	if OPENAI_RESPONSES_MODEL == null then
		println("SKIP: set OPENAI_RESPONSES_MODEL constant")
		return
	end
	L.useModel(OPENAI_RESPONSES_MODEL)

	let out = L.exec("Return a short JSON object with a greeting field.")
	if out == null then
		println("SKIP: OpenAI Responses exec soft-null")
		return
	end
	T.assertType(out, type Str)
	T.assert(len(out) > 0, "expected non-empty response text")
end)

# ==========
# Anthropic
# ==========

T.test("llm/live/anthropic/models_lists_nonempty_if_authed", fun(_: Null) do
	L.useBackend("anthropic")
	L.setConfig({timeoutMs: _tight.timeoutMs})
	if not L.status(null).authed then
		println("SKIP: anthropic not authed")
		return
	end

	let ms = L.models(null)
	if ms == null then
		println("SKIP: Anthropic /models soft-null")
		return
	end
	T.assert(typeOf(ms) == type [Str], "models() should return [Str]")
	T.assert(len(ms) >= 1, "expected at least 1 model")
end)

T.test("llm/live/anthropic/exec_returns_text_if_model", fun(_: Null) do
	L.useBackend("anthropic")
	L.setConfig(_tight)
	if not L.status(null).authed then
		println("SKIP: anthropic not authed")
		return
	end
	if ANTHROPIC_MODEL == null then
		println("SKIP: set ANTHROPIC_MODEL constant")
		return
	end
	L.useModel(ANTHROPIC_MODEL)

	let out = L.exec("Return a short word.")
	if out == null then
		println("SKIP: Anthropic exec soft-null")
		return
	end
	T.assertType(out, type Str)
	T.assert(len(out) > 0, "expected non-empty response text")
end)

# ========
# Cohere
# ========

T.test("llm/live/cohere/models_lists_nonempty_if_authed", fun(_: Null) do
	L.useBackend("cohere")
	L.setConfig({timeoutMs: _tight.timeoutMs})
	if not L.status(null).authed then
		println("SKIP: cohere not authed")
		return
	end

	let ms = L.models(null)
	if ms == null then
		println("SKIP: Cohere /models soft-null")
		return
	end
	T.assert(typeOf(ms) == type [Str], "models() should return [Str]")
	T.assert(len(ms) >= 1, "expected at least 1 model")
end)

T.test("llm/live/cohere/exec_returns_text_if_model", fun(_: Null) do
	L.useBackend("cohere")
	L.setConfig(_tight)
	if not L.status(null).authed then
		println("SKIP: cohere not authed")
		return
	end
	if COHERE_MODEL == null then
		println("SKIP: set COHERE_MODEL constant")
		return
	end
	L.useModel(COHERE_MODEL)

	let out = L.exec("Return a short word.")
	if out == null then
		println("SKIP: Cohere exec soft-null")
		return
	end
	T.assertType(out, type Str)
	T.assert(len(out) > 0, "expected non-empty response text")
end)

# =======
# Ollama
# =======

T.test("llm/live/ollama/models_array_or_softnull", fun(_: Null) do
	L.useBackend("ollama")
	L.setConfig({timeoutMs: _tight.timeoutMs})
	let ms = L.models(null)
	T.assert(ms == null or typeOf(ms) == type [Str], "expected [Str] or soft-null")
end)

T.test("llm/live/ollama/exec_returns_text_if_model", fun(_: Null) do
	L.useBackend("ollama")
	L.setConfig(_tight)
	if OLLAMA_MODEL == null then
		println("SKIP: set OLLAMA_MODEL constant")
		return
	end
	L.useModel(OLLAMA_MODEL)

	let out = L.exec("Return a short word.")
	if out == null then
		println("SKIP: Ollama exec soft-null (daemon down or model missing)")
		return
	end
	T.assertType(out, type Str)
	T.assert(len(out) > 0, "expected non-empty response text")
end)