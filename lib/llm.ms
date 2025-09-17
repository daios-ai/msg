# # llm.ms — minimal LLM backend manager (uniform contract + startup probe)
# #
# # Public surface:
# #   init()                 -> { backends!: [Str], current!: Str }          # probe all, pick a usable default, print diagnostics
# #   backends()             -> [Str]                                        # available backend names
# #   status()               -> { backend!: Str, model: Str?, authed!: Bool, options!: {} }
# #   useBackend(Str)        -> {}?                                          # switch current; returns status or soft-null
# #   models()               -> [Str]?                                       # list models for current backend (if supported)
# #   useModel(Str)          -> {}?                                          # set model for current backend; returns status
# #   getConfig()            -> {}                                           # editable snapshot {backend, baseUrl, model, options, [timeoutMs]}
# #   setConfig({})          -> {}?                                          # shallow-merge config; returns status
# #   auth({})               -> {}?                                          # store credentials (e.g., {"apiKey":"…"}); returns status
# #   exec(Str)              -> Str?                                         # run and return the provider's RAW STRING (or soft-null on error)
# #
# # Notes:
# # - Backends expose { name, state, listModels?, probe, exec }.
# # - init() prints diagnostics and auto-selects the first OK backend if current is not OK.
# # - Only API keys are read from env; baseUrl/model are configured via setConfig/useModel.
# # - Options live in per-backend state.options. Common knobs:
# #     temperature, top_p, top_k, max_tokens, presence_penalty, frequency_penalty
# #   Backends ignore unknown/unsupported options gracefully.
# # - HTTP failures and provider error payloads are surfaced as soft errors (annotated null) with clear messages.
# # - Oracles are responsible for validating/boxing as {"output": x}. Backends DO NOT normalize/box.

# =============================
# Internal state and minimal helpers
# =============================

let _state = {
	current: "ollama",
	backends: {} # name -> backend record
}

# Soft error → annotated null.
let _err = fun(msg: Str) -> Null do
	error("llm: " + msg)
end

# Render a short backend status line.
let _diagLine = fun(name: Str, ok: Bool, authed: Bool, model: Str?, baseUrl: Str?, reason: Str?) -> Str do
	let mark = if ok then
		"✓"
	else
		"✗"
	end
	let m = if model == null then
		""
	else
		" model=" + model
	end
	let u = if baseUrl == null then
		""
	else
		" @ " + baseUrl
	end
	let a = if ok then
		""
	else
		if reason == null then
			""
		else
			" — " + reason
		end
	end
	sprintf("llm: %s %s%s%s%s", [name, mark, m, u, a])
end

# =============================
# Backends (raw-string exec, no boxing)
# =============================

# Backend: Ollama
let _ollama = {
	name: "ollama",
	state: {
		baseUrl: "http://localhost:11434",
		model: null,
		options: {temperature: 0.5}
	},
	listModels: fun(self: {}) -> [Str]? do
		let url = self.state.baseUrl + "/api/tags"
		let r = http({url: url, method: "GET"})
		if r == null then
			return _err("ollama listModels: request failed")
		end
		if r.status != 200 then
			return _err("ollama listModels: HTTP " + str(r.status))
		end
		let body = jsonParse(r.body)
		if body == null then
			return _err("ollama listModels: bad JSON")
		end
		let out = []
		if mapHas(body, "models") then
			for m in iter(body.models) do
				out = out + [m.name]
			end
		end
		out
	end,
	probe: fun(self: {}) -> {} do
		let r = http({url: self.state.baseUrl + "/api/tags", method: "GET"})
		if r == null then
			return {ok: false, authed: true, reason: "request failed"}
		end
		if r.status != 200 then
			return {ok: false, authed: true, reason: "HTTP " + str(r.status)}
		end
		{ok: true, authed: true}
	end,
	exec: fun(self: {}, prompt: Str) -> Str? do
		if self.state.model == null then
			return _err("ollama: model not set (use llm.useModel)")
		end
		let eff = self.state.options
		let reqBody = {
			model: self.state.model,
			prompt: prompt,
			stream: false,
			options: {}
		}
		if mapHas(eff, "temperature") and num(eff.temperature) != null then
			reqBody.options.temperature = num(eff.temperature)
		end
		if mapHas(eff, "top_p") and num(eff.top_p) != null then
			reqBody.options.top_p = num(eff.top_p)
		end
		if mapHas(eff, "top_k") and num(eff.top_k) != null then
			reqBody.options.top_k = int(num(eff.top_k))
		end

		let r = http({
			url: self.state.baseUrl + "/api/generate",
			method: "POST",
			headers: {"Content-Type": "application/json"},
			body: jsonStringify(reqBody)
		})
		if r == null then
			return _err("ollama: request failed")
		end
		if r.status != 200 then
			let b = jsonParse(r.body)
			if b != null and mapHas(b, "error") then
				return _err("ollama: " + str(b.error))
			end
			return _err("ollama: HTTP " + str(r.status))
		end

		let j = jsonParse(r.body)
		if j == null then
			return _err("ollama: bad JSON from server")
		end
		if not mapHas(j, "response") or j.response == null then
			return _err("ollama: missing response")
		end
		j.response
	end
}

# Backend: OpenAI (Chat Completions)
let _openai = {
	name: "openai",
	state: {
		baseUrl: "https://api.openai.com/v1",
		model: null,
		apiKey: osEnv("OPENAI_API_KEY"),
		options: {temperature: 0.5}
	},
	listModels: fun(self: {}) -> [Str]? do
		if self.state.apiKey == null then
			return _err("openai listModels: missing API key")
		end
		let r = http({
			url: self.state.baseUrl + "/models",
			method: "GET",
			headers: {Authorization: "Bearer " + self.state.apiKey}
		})
		if r == null then
			return _err("openai listModels: request failed")
		end
		if r.status != 200 then
			return _err("openai listModels: HTTP " + str(r.status))
		end
		let body = jsonParse(r.body)
		if body == null then
			return _err("openai listModels: bad JSON")
		end
		let out = []
		for m in iter(body.data) do
			out = out + [m.id]
		end
		out
	end,
	probe: fun(self: {}) -> {} do
		let authed = self.state.apiKey != null
		if not authed then
			return {ok: false, authed: false, reason: "missing API key"}
		end
		let r = http({
			url: self.state.baseUrl + "/models",
			method: "GET",
			headers: {Authorization: "Bearer " + self.state.apiKey}
		})
		if r == null then
			return {ok: false, authed: authed, reason: "request failed"}
		end
		if r.status != 200 then
			return {ok: false, authed: authed, reason: "HTTP " + str(r.status)}
		end
		{ok: true, authed: authed}
	end,
	exec: fun(self: {}, prompt: Str) -> Str? do
		if self.state.apiKey == null then
			return _err("openai: missing API key (llm.auth)")
		end
		if self.state.model == null then
			return _err("openai: model not set")
		end

		let eff = self.state.options
		let body = {
			model: self.state.model,
			messages: [
				{
					role: "system",
					content: "You are a careful assistant that outputs only JSON."
				},
				{role: "user", content: prompt}
			]
		}
		if mapHas(eff, "temperature") and num(eff.temperature) != null then
			body.temperature = num(eff.temperature)
		end
		if mapHas(eff, "top_p") and num(eff.top_p) != null then
			body.top_p = num(eff.top_p)
		end
		if mapHas(eff, "presence_penalty") and num(eff.presence_penalty) != null then
			body.presence_penalty = num(eff.presence_penalty)
		end
		if mapHas(eff, "frequency_penalty") and num(eff.frequency_penalty) != null then
			body.frequency_penalty = num(eff.frequency_penalty)
		end
		if mapHas(eff, "max_tokens") and num(eff.max_tokens) != null then
			body.max_tokens = int(num(eff.max_tokens))
		end

		let r = http({
			url: self.state.baseUrl + "/chat/completions",
			method: "POST",
			headers: {
				"Content-Type": "application/json",
				Authorization: "Bearer " + self.state.apiKey
			},
			body: jsonStringify(body)
		})
		if r == null then
			return _err("openai: request failed")
		end
		if r.status != 200 then
			let b = jsonParse(r.body)
			if b != null and mapHas(b, "error") and mapHas(b.error, "message") then
				return _err("openai: " + b.error.message)
			end
			return _err("openai: HTTP " + str(r.status))
		end

		let j = jsonParse(r.body)
		if j == null then
			return _err("openai: bad JSON from server")
		end
		if not mapHas(j, "choices") or len(j.choices) == 0 or not mapHas(j.choices[0], "message") or not mapHas(j.choices[0].message, "content") or j.choices[0].message.content == null then
			return _err("openai: missing content")
		end
		j.choices[0].message.content
	end
}

# Backend: OpenAI (Responses API)
let _openai_responses = {
	name: "openai-responses",
	state: {
		baseUrl: "https://api.openai.com/v1",
		model: null,
		apiKey: osEnv("OPENAI_API_KEY"),
		options: {temperature: 0.5}
	},
	listModels: fun(self: {}) -> [Str]? do
		if self.state.apiKey == null then
			return _err("openai-responses listModels: missing API key")
		end
		let r = http({
			url: self.state.baseUrl + "/models",
			method: "GET",
			headers: {Authorization: "Bearer " + self.state.apiKey}
		})
		if r == null then
			return _err("openai-responses listModels: request failed")
		end
		if r.status != 200 then
			return _err("openai-responses listModels: HTTP " + str(r.status))
		end
		let body = jsonParse(r.body)
		if body == null then
			return _err("openai-responses listModels: bad JSON")
		end
		let out = []
		for m in iter(body.data) do
			out = out + [m.id]
		end
		out
	end,
	probe: fun(self: {}) -> {} do
		let authed = self.state.apiKey != null
		if not authed then
			return {ok: false, authed: false, reason: "missing API key"}
		end
		let r = http({
			url: self.state.baseUrl + "/models",
			method: "GET",
			headers: {Authorization: "Bearer " + self.state.apiKey}
		})
		if r == null then
			return {ok: false, authed: authed, reason: "request failed"}
		end
		if r.status != 200 then
			return {ok: false, authed: authed, reason: "HTTP " + str(r.status)}
		end
		{ok: true, authed: authed}
	end,
	exec: fun(self: {}, prompt: Str) -> Str? do
		if self.state.apiKey == null then
			return _err("openai-responses: missing API key (llm.auth)")
		end
		if self.state.model == null then
			return _err("openai-responses: model not set")
		end

		let eff = self.state.options
		let body = {
			model: self.state.model,
			input: prompt,
			text: {format: {type: "json_object"}}
		}
		if mapHas(eff, "temperature") and num(eff.temperature) != null then
			body.temperature = num(eff.temperature)
		end
		if mapHas(eff, "top_p") and num(eff.top_p) != null then
			body.top_p = num(eff.top_p)
		end
		if mapHas(eff, "presence_penalty") and num(eff.presence_penalty) != null then
			body.presence_penalty = num(eff.presence_penalty)
		end
		if mapHas(eff, "frequency_penalty") and num(eff.frequency_penalty) != null then
			body.frequency_penalty = num(eff.frequency_penalty)
		end
		if mapHas(eff, "max_tokens") and num(eff.max_tokens) != null then
			body.max_output_tokens = int(num(eff.max_tokens))
		end

		let r = http({
			url: self.state.baseUrl + "/responses",
			method: "POST",
			headers: {
				"Content-Type": "application/json",
				Authorization: "Bearer " + self.state.apiKey
			},
			body: jsonStringify(body)
		})
		if r == null then
			return _err("openai-responses: request failed")
		end
		if r.status != 200 then
			let b = jsonParse(r.body)
			if b != null and mapHas(b, "error") and mapHas(b.error, "message") then
				return _err("openai-responses: " + b.error.message)
			end
			return _err("openai-responses: HTTP " + str(r.status))
		end

		let j = jsonParse(r.body)
		if j == null then
			return _err("openai-responses: bad JSON from server")
		end
		if not mapHas(j, "output") or len(j.output) == 0 then
			return _err("openai-responses: missing content")
		end

		let text = null
		let msg = j.output[0]
		if mapHas(msg, "content") and len(msg.content) > 0 then
			for part in msg.content do
				if mapHas(part, "type") and part.type == "output_text" and mapHas(part, "text") and part.text != null then
					text = part.text
					break
				end
			end
		end
		if text == null then
			return _err("openai-responses: missing content")
		end
		text
	end
}

# Backend: Anthropic (Messages API)
let _anthropic = {
	name: "anthropic",
	state: {
		baseUrl: "https://api.anthropic.com/v1",
		model: null,
		apiKey: osEnv("ANTHROPIC_API_KEY"),
		version: "2023-06-01",
		options: {temperature: 0.5, max_tokens: 1024}
	},
	listModels: fun(self: {}) -> [Str]? do
		if self.state.apiKey == null then
			return _err("anthropic listModels: missing API key")
		end
		let r = http({
			url: self.state.baseUrl + "/models",
			method: "GET",
			headers: {
				Authorization: "Bearer " + self.state.apiKey,
				"anthropic-version": self.state.version
			}
		})
		if r == null then
			return _err("anthropic listModels: request failed")
		end
		if r.status != 200 then
			return _err("anthropic listModels: HTTP " + str(r.status))
		end
		let body = jsonParse(r.body)
		if body == null then
			return _err("anthropic listModels: bad JSON")
		end
		let out = []
		if mapHas(body, "data") then
			for m in iter(body.data) do
				out = out + [m.id]
			end
		end
		out
	end,
	probe: fun(self: {}) -> {} do
		let authed = self.state.apiKey != null
		if not authed then
			return {ok: false, authed: false, reason: "missing API key"}
		end
		let r = http({
			url: self.state.baseUrl + "/models",
			method: "GET",
			headers: {
				Authorization: "Bearer " + self.state.apiKey,
				"anthropic-version": self.state.version
			}
		})
		if r == null then
			return {ok: false, authed: authed, reason: "request failed"}
		end
		if r.status != 200 then
			return {ok: false, authed: authed, reason: "HTTP " + str(r.status)}
		end
		{ok: true, authed: authed}
	end,
	exec: fun(self: {}, prompt: Str) -> Str? do
		if self.state.apiKey == null then
			return _err("anthropic: missing API key (llm.auth)")
		end
		if self.state.model == null then
			return _err("anthropic: model not set")
		end

		let eff = self.state.options
		let body = {
			model: self.state.model,
			messages: [{role: "user", content: prompt}]
		}
		if mapHas(eff, "temperature") and num(eff.temperature) != null then
			body.temperature = num(eff.temperature)
		end
		if mapHas(eff, "max_tokens") and num(eff.max_tokens) != null then
			body.max_tokens = int(num(eff.max_tokens))
		end
		if mapHas(eff, "top_p") and num(eff.top_p) != null then
			body.top_p = num(eff.top_p)
		end
		if mapHas(eff, "top_k") and num(eff.top_k) != null then
			body.top_k = int(num(eff.top_k))
		end

		let r = http({
			url: self.state.baseUrl + "/messages",
			method: "POST",
			headers: {
				"Content-Type": "application/json",
				Authorization: "Bearer " + self.state.apiKey,
				"anthropic-version": self.state.version
			},
			body: jsonStringify(body)
		})
		if r == null then
			return _err("anthropic: request failed")
		end
		if r.status != 200 then
			let b = jsonParse(r.body)
			if b != null and mapHas(b, "error") and mapHas(b.error, "message") then
				return _err("anthropic: " + b.error.message)
			end
			return _err("anthropic: HTTP " + str(r.status))
		end

		let j = jsonParse(r.body)
		if j == null then
			return _err("anthropic: bad JSON from server")
		end
		if not mapHas(j, "content") or len(j.content) == 0 or not mapHas(j.content[0], "text") or j.content[0].text == null then
			return _err("anthropic: missing content")
		end
		j.content[0].text
	end
}

# Backend: Cohere (Chat API)
let _cohere = {
	name: "cohere",
	state: {
		baseUrl: "https://api.cohere.ai/v1",
		model: null,
		apiKey: osEnv("COHERE_API_KEY"),
		options: {temperature: 0.5}
	},
	listModels: fun(self: {}) -> [Str]? do
		if self.state.apiKey == null then
			return _err("cohere listModels: missing API key")
		end
		let r = http({
			url: self.state.baseUrl + "/models",
			method: "GET",
			headers: {Authorization: "Bearer " + self.state.apiKey}
		})
		if r == null then
			return _err("cohere listModels: request failed")
		end
		if r.status != 200 then
			return _err("cohere listModels: HTTP " + str(r.status))
		end
		let body = jsonParse(r.body)
		if body == null then
			return _err("cohere listModels: bad JSON")
		end
		let out = []
		if mapHas(body, "models") then
			for m in iter(body.models) do
				out = out + [m.name]
			end
		elif mapHas(body, "data") then
			for m in iter(body.data) do
				out = out + [m.id]
			end
		end
		out
	end,
	probe: fun(self: {}) -> {} do
		let authed = self.state.apiKey != null
		if not authed then
			return {ok: false, authed: false, reason: "missing API key"}
		end
		let r = http({
			url: self.state.baseUrl + "/models",
			method: "GET",
			headers: {Authorization: "Bearer " + self.state.apiKey}
		})
		if r == null then
			return {ok: false, authed: authed, reason: "request failed"}
		end
		if r.status != 200 then
			return {ok: false, authed: authed, reason: "HTTP " + str(r.status)}
		end
		{ok: true, authed: authed}
	end,
	exec: fun(self: {}, prompt: Str) -> Str? do
		if self.state.apiKey == null then
			return _err("cohere: missing API key (llm.auth)")
		end
		if self.state.model == null then
			return _err("cohere: model not set")
		end

		let eff = self.state.options
		let body = {model: self.state.model, message: prompt}
		if mapHas(eff, "temperature") and num(eff.temperature) != null then
			body.temperature = num(eff.temperature)
		end
		if mapHas(eff, "top_p") and num(eff.top_p) != null then
			body.p = num(eff.top_p)
		end
		if mapHas(eff, "top_k") and num(eff.top_k) != null then
			body.k = int(num(eff.top_k))
		end
		if mapHas(eff, "max_tokens") and num(eff.max_tokens) != null then
			body.max_tokens = int(num(eff.max_tokens))
		end

		let r = http({
			url: self.state.baseUrl + "/chat",
			method: "POST",
			headers: {
				"Content-Type": "application/json",
				Authorization: "Bearer " + self.state.apiKey
			},
			body: jsonStringify(body)
		})
		if r == null then
			return _err("cohere: request failed")
		end
		if r.status != 200 then
			let b = jsonParse(r.body)
			if b != null and mapHas(b, "message") then
				return _err("cohere: " + b.message)
			end
			return _err("cohere: HTTP " + str(r.status))
		end

		let j = jsonParse(r.body)
		if j == null then
			return _err("cohere: bad JSON from server")
		end
		if mapHas(j, "text") and j.text != null then
			return j.text
		end
		if mapHas(j, "message") and mapHas(j.message, "content") and j.message.content != null then
			return j.message.content
		end
		_err("cohere: missing content")
	end
}

# =============================
# Registry & environment sync
# =============================

_state.backends["ollama"] = _ollama
_state.backends["openai"] = _openai
_state.backends["openai-responses"] = _openai_responses
_state.backends["anthropic"] = _anthropic
_state.backends["cohere"] = _cohere

# If OPENAI_API_KEY is present, mirror it into both OpenAI backends at load.
if _openai.state.apiKey != null then
	_state.backends["openai"].state.apiKey = _openai.state.apiKey
	_state.backends["openai-responses"].state.apiKey = _openai.state.apiKey
end

# =============================
# Public API
# =============================

# Probe all backends, print diagnostics, and ensure a usable current backend.
# Args: _: Null (ignored)
# Return: { backends!: [Str], current!: Str }
let init = fun(_: Null) -> {} do
	let names = []
	let firstOK = null
	for [k, _] in _state.backends do
		names = names + [k]
	end

	let currentOK = false
	for name in names do
		let b = _state.backends[name]
		let pr = b.probe(b)
		let line = _diagLine(name, pr.ok, pr.authed, if mapHas(b.state, "model") then
			b.state.model
		else
			null
		end, if mapHas(b.state, "baseUrl") then
			b.state.baseUrl
		else
			null
		end, if mapHas(pr, "reason") then
			pr.reason
		else
			null
		end)
		println(line)
		if pr.ok and firstOK == null then
			firstOK = name
		end
		if name == _state.current and pr.ok then
			currentOK = true
		end
	end

	if not currentOK and firstOK != null then
		_state.current = firstOK
		println(sprintf("llm: switched current backend to \"%s\"", [
			_state.current
		]))
	end

	{backends: names, current: _state.current}
end

# List registered backend names.
let backends = fun(_: Null) -> [Str] do
	let out = []
	for [k, _] in _state.backends do
		out = out + [k]
	end
	out
end

# Report current backend status without network calls.
let status = fun(_: Null) -> {} do
	let b = _state.backends[_state.current]
	let requiresKey = b.name == "openai" or b.name == "openai-responses" or b.name == "anthropic" or b.name == "cohere"
	let authed = if requiresKey then
		mapHas(b.state, "apiKey") and b.state.apiKey != null
	else
		true
	end
	{
		backend: b.name,
		model: if mapHas(b.state, "model") then
			b.state.model
		else
			null
		end,
		authed: authed,
		options: if mapHas(b.state, "options") then
			b.state.options
		else
			{}
		end
	}
end

# Switch the current backend by name.
let useBackend = fun(name: Str) -> {}? do
	if not mapHas(_state.backends, name) then
		return _err("unknown backend: " + name)
	end
	_state.current = name
	status(null)
end

# List models for the current backend (if supported).
let models = fun(_: Null) -> [Str]? do
	let b = _state.backends[_state.current]
	if not mapHas(b, "listModels") then
		return _err(b.name + " does not support model listing")
	end
	b.listModels(b)
end

# Set the active model for the current backend.
let useModel = fun(name: Str) -> {}? do
	let b = _state.backends[_state.current]
	b.state.model = name
	status(null)
end

# Get an editable config snapshot for the current backend.
let getConfig = fun(_: Null) -> {} do
	let b = _state.backends[_state.current]
	let cfg = {
		backend: b.name,
		baseUrl: if mapHas(b.state, "baseUrl") then
			b.state.baseUrl
		else
			null
		end,
		model: if mapHas(b.state, "model") then
			b.state.model
		else
			null
		end,
		options: if mapHas(b.state, "options") then
			b.state.options
		else
			{}
		end
	}
	if mapHas(b.state, "timeoutMs") then
		cfg.timeoutMs = b.state.timeoutMs
	end
	cfg
end

# Merge a config object into the current backend's state.
let setConfig = fun(cfg: {}) -> {}? do
	let b = _state.backends[_state.current]
	if mapHas(cfg, "baseUrl") then
		b.state.baseUrl = cfg.baseUrl
	end
	if mapHas(cfg, "model") then
		b.state.model = cfg.model
	end
	if mapHas(cfg, "options") then
		b.state.options = cfg.options
	end
	if mapHas(cfg, "timeoutMs") then
		b.state.timeoutMs = cfg.timeoutMs
	end
	status(null)
end

# Store credentials for the current backend (never printed).
let auth = fun(creds: {}) -> {}? do
	let b = _state.backends[_state.current]
	# Ensure apiKey is set/cleared where status() looks.
	if mapHas(creds, "apiKey") then
		b.state.apiKey = creds.apiKey
	end
	# Shallow-merge any other credential keys.
	b.state = b.state + creds
	status(null)
end

# Execute the current backend with a prompt.
# Returns the provider's RAW STRING (or soft-null on error).
let exec = fun(prompt: Str) -> Str? do
	let b = _state.backends[_state.current]
	b.exec(b, prompt)
end