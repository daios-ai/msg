## llm.ms — minimal LLM backend manager (uniform contract + startup probe)
##
## Public surface:
##   init()                 -> { backends!: [Str], current!: Str }          # probe all, pick a usable default, print diagnostics
##   backends()             -> [Str]                                        # available backend names
##   status()               -> { backend!: Str, model: Str?, authed!: Bool, options!: {} }
##   useBackend(Str)        -> {}?                                          # switch current; returns status or soft-null
##   models()               -> [Str]?                                       # list models for current backend (if supported)
##   useModel(Str)          -> {}?                                          # set model for current backend; returns status
##   getOptions()           -> {}                                           # editable config snapshot {backend, baseUrl, model, options, [timeoutMs]}
##   setOptions({})         -> {}?                                          # shallow-merge config; returns status
##   set({})                -> {}?                                          # alias of setOptions
##   auth({})               -> {}?                                          # store credentials (e.g., {"apiKey":"…"}); returns status
##   exec(Str)              -> Str?                                         # run and ALWAYS return a JSON string (see normalization notes below)
##
## Notes:
## - Simple, uniform backend contract (see `_mkBackend` usage).
## - `init()` prints one-line diagnostics for each backend and auto-selects the first usable one if current is not OK.
## - Options are kept in per-backend `state.options` and surfaced via `getOptions`/`setOptions`.
##   Common numeric options supported across backends when available:
##     temperature, top_p, top_k, max_tokens, presence_penalty, frequency_penalty
##   Backends will gracefully ignore options they do not support.
## - Soft errors use `error("llm: ...")` and return `null` (annotated).
## - Normalization: providers that return:
##     * JSON with {"output": …}  → passed through as JSON string
##     * JSON without "output"    → boxed to {"output": …}
##     * Plain text                → boxed to {"output": "<text>"}

# =============================
# Internal state and helpers
# =============================

let _state = {
	current: "ollama",
	backends: {}   # name -> backend record
}

# Shallow merge (right wins).
let _merge = fun(a: {}, b: {}) -> {} do
	let o = {}
	for let [k, v] in a do o[k] = v end
	for let [k, v] in b do o[k] = v end
	o
end

# Soft error → annotated null.
let _err = fun(msg: Str) -> Null do
	error("llm: " + msg)
end

# Read env with a default.
let _env = fun(key: Str, def: Str) -> Str do
	let v = osEnv(key)
	if v == null then def else v end
end

# Box any value under {"output": …} and stringify.
let _boxJSON = fun(v: Any) -> Str do
	jsonStringify({ "output": v })
end

# Normalize arbitrary provider text/JSON into a JSON string (guaranteed).
let _normalizeToJSON = fun(textOrJSON: Any) -> Str do
	# If it’s already a map, ensure boxing.
	let isMap = try(fun() do isType(textOrJSON, type {}) end)
	if isMap.ok and isMap.value then
		let hasOut = try(fun() do mapHas(textOrJSON, "output") end)
		if hasOut.ok then
			return jsonStringify(textOrJSON)
		else
			return _boxJSON(textOrJSON)
		end
	end

	# If it’s a string, try to parse JSON, then box if needed.
	let s = if str(textOrJSON) != null then str(textOrJSON) else formatValue(textOrJSON) end
	let p = try(fun() do jsonParse(s) end)
	if p.ok and p.value != null then
		let v = p.value
		let hasOut = try(fun() do mapHas(v, "output") end)
		if hasOut.ok then jsonStringify(v) else _boxJSON(v) end
	else
		_boxJSON(s)
	end
end

# Coerce numeric option if present; else default.
let _getNumOpt = fun(opts: {}, key: Str, def: Num) -> Num do
	if not mapHas(opts, key) or opts[key] == null then
		return def
	end
	let n = num(opts[key])
	if n == null then def else n end
end

# Extract a numeric option by key; returns {present!:Bool, value:Num}
let _maybeNumOpt = fun(opts: {}, key: Str) -> {} do
	if not mapHas(opts, key) or opts[key] == null then
		return { present: false, value: 0.0 }
	end
	let n = num(opts[key])
	if n == null then
		{ present: false, value: 0.0 }
	else
		{ present: true, value: n }
	end
end

# Render a short backend status line.
let _diagLine = fun(name: Str, ok: Bool, authed: Bool, model: Str?, baseUrl: Str?, reason: Str?) -> Str do
	let mark = if ok then "✓" else "✗" end
	let m = if model == null then "" else (" model=" + model) end
	let u = if baseUrl == null then "" else (" @ " + baseUrl) end
	let a = if ok then "" else (if reason == null then "" else (" — " + reason) end) end
	sprintf("llm: %s %s%s%s%s", [name, mark, m, u, a])
end

# Limit a map to a whitelist of keys (pass-through of present keys only).
let _pick = fun(src: {}, keys: [Str]) -> {} do
	let out = {}
	for k in keys do
		if mapHas(src, k) and src[k] != null then out[k] = src[k] end
	end
	out
end

# =============================
# Uniform backend factory
# =============================

# Each backend record MUST provide:
#   name: Str
#   state: { baseUrl?: Str, model?: Str, apiKey?: Str, options: {}, [timeoutMs?: Int] }
#   listModels(self:{}) -> [Str]?           (optional; soft-null on failure/unsupported)
#   probe(self:{}) -> { ok!:Bool, authed!:Bool, reason:Str? }
#   exec(self:{}, prompt:Str) -> Str?       (returns JSON string; soft-null on failure)
#
# NOTE: Backends should read supported numeric options from state.options:
#   temperature, top_p, top_k, max_tokens, presence_penalty, frequency_penalty
# and map/ignore as appropriate for the provider.
let _mkBackend = fun(rec: {}) -> {} do rec end

# =============================
# Backend: Ollama
# =============================

let _ollama = do
	let baseUrl = _env("OLLAMA_BASE_URL", "http://localhost:11434")
	let model   = osEnv("OLLAMA_MODEL")
	_mkBackend({
		name: "ollama",
		state: {
			baseUrl: baseUrl,
			model: model,
			options: { "temperature": 0.5 }
		},

		listModels: fun(self: {}) -> [Str]? do
			let r = try(fun() do http({ "url": self.state.baseUrl + "/api/tags", "method": "GET" }) end)
			if not r.ok then return _err("ollama listModels: " + str(r.error)) end
			if r.value.status != 200 then return _err("ollama listModels: HTTP " + str(r.value.status)) end
			let body = jsonParse(r.value.body)
			if body == null then return _err("ollama listModels: bad JSON") end
			let out = []
			if mapHas(body, "models") then
				for m in iter(body.models) do out = out + [m.name] end
			end
			out
		end,

		probe: fun(self: {}) -> {} do
			let r = try(fun() do http({ "url": self.state.baseUrl + "/api/tags", "method": "GET" }) end)
			if not r.ok then return { ok: false, authed: true, reason: str(r.error) } end
			if r.value.status != 200 then return { ok: false, authed: true, reason: "HTTP " + str(r.value.status) } end
			{ ok: true, authed: true }
		end,

		exec: fun(self: {}, prompt: Str) -> Str? do
			if self.state.model == null then return _err("ollama: model not set (use llm.useModel or set OLLAMA_MODEL)") end
			let eff = self.state.options
			let reqBody = {
				"model": self.state.model,
				"prompt": prompt,
				"stream": false,
				"options": _merge(eff, {
					"temperature": _getNumOpt(eff, "temperature", 0.5),
					"top_p": _getNumOpt(eff, "top_p", 1.0),
					"top_k": int(_getNumOpt(eff, "top_k", 0.0))
				})
			}
			let r = try(fun() do http({
				"url": self.state.baseUrl + "/api/generate",
				"method": "POST",
				"headers": { "Content-Type": "application/json" },
				"body": jsonStringify(reqBody)
			}) end)
			if not r.ok then return _err("ollama: " + str(r.error)) end
			if r.value.status != 200 then return _err("ollama: HTTP " + str(r.value.status)) end

			let j = jsonParse(r.value.body)
			if j == null then return _err("ollama: bad JSON from server") end
			let text = j.response
			if text == null then return _err("ollama: missing response") end
			_normalizeToJSON(text)
		end
	})
end

# =============================
# Backend: OpenAI (Chat Completions)
# =============================

let _openai = do
	let baseUrl = _env("OPENAI_BASE_URL", "https://api.openai.com/v1")
	let model   = osEnv("OPENAI_MODEL")
	let apiKey  = osEnv("OPENAI_API_KEY")
	_mkBackend({
		name: "openai",
		state: {
			baseUrl: baseUrl,
			model: model,
			apiKey: apiKey,
			options: { "temperature": 0.5 }
		},

		listModels: fun(self: {}) -> [Str]? do
			if self.state.apiKey == null then return _err("openai listModels: missing API key") end
			let r = try(fun() do http({
				"url": self.state.baseUrl + "/models",
				"method": "GET",
				"headers": { "Authorization": "Bearer " + self.state.apiKey }
			}) end)
			if not r.ok then return _err("openai listModels: " + str(r.error)) end
			if r.value.status != 200 then return _err("openai listModels: HTTP " + str(r.value.status)) end
			let body = jsonParse(r.value.body)
			if body == null then return _err("openai listModels: bad JSON") end
			let out = []
			for m in iter(body.data) do out = out + [m.id] end
			out
		end,

		probe: fun(self: {}) -> {} do
			let authed = self.state.apiKey != null
			if not authed then return { ok: false, authed: false, reason: "missing API key" } end
			let r = try(fun() do http({
				"url": self.state.baseUrl + "/models",
				"method": "GET",
				"headers": { "Authorization": "Bearer " + self.state.apiKey }
			}) end)
			if not r.ok then return { ok: false, authed: authed, reason: str(r.error) } end
			if r.value.status != 200 then return { ok: false, authed: authed, reason: "HTTP " + str(r.value.status) } end
			{ ok: true, authed: authed }
		end,

		exec: fun(self: {}, prompt: Str) -> Str? do
			if self.state.apiKey == null then return _err("openai: missing API key (llm.auth)") end
			if self.state.model == null then return _err("openai: model not set") end

			let eff = self.state.options
			let body = {
				"model": self.state.model,
				"messages": [
					{ "role": "system", "content": "You are a careful assistant that outputs only JSON." },
					{ "role": "user",   "content": prompt }
				],
				"response_format": { "type": "json_object" },
				"temperature": _getNumOpt(eff, "temperature", 0.5)
			}

			let r = try(fun() do http({
				"url": self.state.baseUrl + "/chat/completions",
				"method": "POST",
				"headers": {
					"Content-Type": "application/json",
					"Authorization": "Bearer " + self.state.apiKey
				},
				"body": jsonStringify(body)
			}) end)
			if not r.ok then return _err("openai: " + str(r.error)) end
			if r.value.status != 200 then return _err("openai: HTTP " + str(r.value.status)) end

			let j = jsonParse(r.value.body)
			if j == null then return _err("openai: bad JSON from server") end
			let text = j.choices[0].message.content
			if text == null then return _err("openai: missing content") end
			_normalizeToJSON(text)
		end

	})
end

# =============================
# Backend: OpenAI (Responses API)
# =============================

let _openai_responses = do
	let baseUrl = _env("OPENAI_BASE_URL", "https://api.openai.com/v1")
	let model   = osEnv("OPENAI_MODEL")
	let apiKey  = osEnv("OPENAI_API_KEY")
	_mkBackend({
		name: "openai-responses",
		state: {
			baseUrl: baseUrl,
			model: model,
			apiKey: apiKey,
			options: { "temperature": 0.5 }
		},

		listModels: fun(self: {}) -> [Str]? do
			if self.state.apiKey == null then return _err("openai-responses listModels: missing API key") end
			let r = try(fun() do http({
				"url": self.state.baseUrl + "/models",
				"method": "GET",
				"headers": { "Authorization": "Bearer " + self.state.apiKey }
			}) end)
			if not r.ok then return _err("openai-responses listModels: " + str(r.error)) end
			if r.value.status != 200 then return _err("openai-responses listModels: HTTP " + str(r.value.status)) end
			let body = jsonParse(r.value.body)
			if body == null then return _err("openai-responses listModels: bad JSON") end
			let out = []
			for m in iter(body.data) do out = out + [m.id] end
			out
		end,

		probe: fun(self: {}) -> {} do
			let authed = self.state.apiKey != null
			if not authed then return { ok: false, authed: false, reason: "missing API key" } end
			let r = try(fun() do http({
				"url": self.state.baseUrl + "/models",
				"method": "GET",
				"headers": { "Authorization": "Bearer " + self.state.apiKey }
			}) end)
			if not r.ok then return { ok: false, authed: authed, reason: str(r.error) } end
			if r.value.status != 200 then return { ok: false, authed: authed, reason: "HTTP " + str(r.value.status) } end
			{ ok: true, authed: authed }
		end,

		exec: fun(self: {}, prompt: Str) -> Str? do
			if self.state.apiKey == null then return _err("openai-responses: missing API key (llm.auth)") end
			if self.state.model == null then return _err("openai-responses: model not set") end

			let eff = self.state.options
			let body = {
				"model": self.state.model,
				"input": prompt,
				"response_format": { "type": "json_object" },
				"temperature": _getNumOpt(eff, "temperature", 0.5)
			}

			let r = try(fun() do http({
				"url": self.state.baseUrl + "/responses",
				"method": "POST",
				"headers": {
					"Content-Type": "application/json",
					"Authorization": "Bearer " + self.state.apiKey
				},
				"body": jsonStringify(body)
			}) end)
			if not r.ok then return _err("openai-responses: " + str(r.error)) end
			if r.value.status != 200 then return _err("openai-responses: HTTP " + str(r.value.status)) end

			let j = jsonParse(r.value.body)
			if j == null then return _err("openai-responses: bad JSON from server") end

			let text =
				if mapHas(j, "output_text") and j.output_text != null then j.output_text
				elif mapHas(j, "content") and len(j.content) > 0 and mapHas(j.content[0], "text") then j.content[0].text
				else null end

			if text != null then
				_normalizeToJSON(text)
			else
				_normalizeToJSON(j)
			end
		end
	})
end

# =============================
# Backend: Anthropic (Messages API)
# =============================

let _anthropic = do
	let baseUrl = _env("ANTHROPIC_BASE_URL", "https://api.anthropic.com/v1")
	let model   = osEnv("ANTHROPIC_MODEL")
	let apiKey  = osEnv("ANTHROPIC_API_KEY")
	let version = _env("ANTHROPIC_VERSION", "2023-06-01")
	_mkBackend({
		name: "anthropic",
		state: {
			baseUrl: baseUrl,
			model: model,
			apiKey: apiKey,
			version: version,
			options: { "temperature": 0.5, "max_tokens": 1024 }
		},

		listModels: fun(self: {}) -> [Str]? do
			if self.state.apiKey == null then return _err("anthropic listModels: missing API key") end
			let r = try(fun() do http({
				"url": self.state.baseUrl + "/models",
				"method": "GET",
				"headers": {
					"Authorization": "Bearer " + self.state.apiKey,
					"anthropic-version": self.state.version
				}
			}) end)
			if not r.ok then return _err("anthropic listModels: " + str(r.error)) end
			if r.value.status != 200 then return _err("anthropic listModels: HTTP " + str(r.value.status)) end
			let body = jsonParse(r.value.body)
			if body == null then return _err("anthropic listModels: bad JSON") end
			let out = []
			if mapHas(body, "data") then
				for m in iter(body.data) do out = out + [m.id] end
			end
			out
		end,

		probe: fun(self: {}) -> {} do
			let authed = self.state.apiKey != null
			if not authed then return { ok: false, authed: false, reason: "missing API key" } end
			let r = try(fun() do http({
				"url": self.state.baseUrl + "/models",
				"method": "GET",
				"headers": {
					"Authorization": "Bearer " + self.state.apiKey,
					"anthropic-version": self.state.version
				}
			}) end)
			if not r.ok then return { ok: false, authed: authed, reason: str(r.error) } end
			if r.value.status != 200 then return { ok: false, authed: authed, reason: "HTTP " + str(r.value.status) } end
			{ ok: true, authed: authed }
		end,

		exec: fun(self: {}, prompt: Str) -> Str? do
			if self.state.apiKey == null then return _err("anthropic: missing API key (llm.auth)") end
			if self.state.model == null then return _err("anthropic: model not set") end

			let eff = self.state.options
			let mx  = int(_getNumOpt(eff, "max_tokens", 1024.0))
			let body = {
				"model": self.state.model,
				"messages": [ { "role": "user", "content": prompt } ],
				"temperature": _getNumOpt(eff, "temperature", 0.5),
				"max_tokens": mx
			}
			# Optional: top_p/top_k if provided
			let tp = _maybeNumOpt(eff, "top_p")
			if tp.present then body.top_p = tp.value end
			let tk = _maybeNumOpt(eff, "top_k")
			if tk.present then body.top_k = int(tk.value) end

			let r = try(fun() do http({
				"url": self.state.baseUrl + "/messages",
				"method": "POST",
				"headers": {
					"Content-Type": "application/json",
					"Authorization": "Bearer " + self.state.apiKey,
					"anthropic-version": self.state.version
				},
				"body": jsonStringify(body)
			}) end)
			if not r.ok then return _err("anthropic: " + str(r.error)) end
			if r.value.status != 200 then return _err("anthropic: HTTP " + str(r.value.status)) end

			let j = jsonParse(r.value.body)
			if j == null then return _err("anthropic: bad JSON from server") end

			let text =
				if mapHas(j, "content") and len(j.content) > 0 and mapHas(j.content[0], "text") then j.content[0].text
				else null end

			if text == null then
				_normalizeToJSON(j)
			else
				_normalizeToJSON(text)
			end
		end
	})
end

# =============================
# Backend: Cohere (Chat API)
# =============================

let _cohere = do
	let baseUrl = _env("COHERE_BASE_URL", "https://api.cohere.ai/v1")
	let model   = osEnv("COHERE_MODEL")
	let apiKey  = osEnv("COHERE_API_KEY")
	_mkBackend({
		name: "cohere",
		state: {
			baseUrl: baseUrl,
			model: model,
			apiKey: apiKey,
			options: { "temperature": 0.5 }
		},

		listModels: fun(self: {}) -> [Str]? do
			if self.state.apiKey == null then return _err("cohere listModels: missing API key") end
			let r = try(fun() do http({
				"url": self.state.baseUrl + "/models",
				"method": "GET",
				"headers": { "Authorization": "Bearer " + self.state.apiKey }
			}) end)
			if not r.ok then return _err("cohere listModels: " + str(r.error)) end
			if r.value.status != 200 then return _err("cohere listModels: HTTP " + str(r.value.status)) end
			let body = jsonParse(r.value.body)
			if body == null then return _err("cohere listModels: bad JSON") end
			let out = []
			if mapHas(body, "models") then
				for m in iter(body.models) do out = out + [m.name] end
			elif mapHas(body, "data") then
				for m in iter(body.data) do out = out + [m.id] end
			end
			out
		end,

		probe: fun(self: {}) -> {} do
			let authed = self.state.apiKey != null
			if not authed then return { ok: false, authed: false, reason: "missing API key" } end
			let r = try(fun() do http({
				"url": self.state.baseUrl + "/models",
				"method": "GET",
				"headers": { "Authorization": "Bearer " + self.state.apiKey }
			}) end)
			if not r.ok then return { ok: false, authed: authed, reason: str(r.error) } end
			if r.value.status != 200 then return { ok: false, authed: authed, reason: "HTTP " + str(r.value.status) } end
			{ ok: true, authed: authed }
		end,

		exec: fun(self: {}, prompt: Str) -> Str? do
			if self.state.apiKey == null then return _err("cohere: missing API key (llm.auth)") end
			if self.state.model == null then return _err("cohere: model not set") end

			let eff = self.state.options
			let body = {
				"model": self.state.model,
				"message": prompt,
				"temperature": _getNumOpt(eff, "temperature", 0.5)
			}
			# Map common options if present
			let tp = _maybeNumOpt(eff, "top_p")
			if tp.present then body.p = tp.value end
			let tk = _maybeNumOpt(eff, "top_k")
			if tk.present then body.k = int(tk.value) end
			let mx = _maybeNumOpt(eff, "max_tokens")
			if mx.present then body.max_tokens = int(mx.value) end

			let r = try(fun() do http({
				"url": self.state.baseUrl + "/chat",
				"method": "POST",
				"headers": {
					"Content-Type": "application/json",
					"Authorization": "Bearer " + self.state.apiKey
				},
				"body": jsonStringify(body)
			}) end)
			if not r.ok then return _err("cohere: " + str(r.error)) end
			if r.value.status != 200 then return _err("cohere: HTTP " + str(r.value.status)) end

			let j = jsonParse(r.value.body)
			if j == null then return _err("cohere: bad JSON from server") end

			let text =
				if mapHas(j, "text") and j.text != null then j.text
				elif mapHas(j, "message") and mapHas(j.message, "content") then j.message.content
				else null end

			if text == null then
				_normalizeToJSON(j)
			else
				_normalizeToJSON(text)
			end
		end
	})
end

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
# Notes:
#	- Prints one diagnostic line per backend: "llm: <name> ✓|✗ [model=…] [@ baseUrl] [— reason]".
#	- If the current backend is not OK but another is, auto-switches to the first OK backend.
let init = fun(_: Null) -> {} do
	let names = []
	let firstOK = null
	for let [k, _] in _state.backends do names = names + [k] end
	# Sort for stable output
	let _sort = fun(xs: [Str]) -> [Str] do
		let n = len(xs)
		let i = 0
		while i < n do
			let j = i
			while j > 0 and xs[j - 1] > xs[j] do
				let t = xs[j - 1]
				xs[j - 1] = xs[j]
				xs[j] = t
				j = j - 1
			end
			i = i + 1
		end
		xs
	end
	names = _sort(names)

	let currentOK = false
	for name in names do
		let b = _state.backends[name]
		let pr = b.probe(b)
		let line = _diagLine(
			name,
			pr.ok,
			pr.authed,
			b.state.model,
			(if mapHas(b.state, "baseUrl") then b.state.baseUrl else null end),
			pr.reason)
		println(line)
		if pr.ok and firstOK == null then firstOK = name end
		if name == _state.current and pr.ok then currentOK = true end
	end

	if not currentOK and firstOK != null then
		_state.current = firstOK
		println(sprintf('llm: switched current backend to "%s"', [_state.current]))
	end

	{ backends: names, current: _state.current }
end

# List registered backend names.
# Args: _: Null (ignored)
# Return: [Str]
let backends = fun(_: Null) -> [Str] do
	let out = []
	for let [k, _] in _state.backends do out = out + [k] end
	out
end

# Report current backend status without network calls.
# Args: _: Null (ignored)
# Return: { backend!: Str, model: Str?, authed!: Bool, options!: {} }
# Notes:
#	- `authed` is computed from local state (presence of API key when required).
let status = fun(_: Null) -> {} do
	let b = _state.backends[_state.current]
	let requiresKey = (b.name == "openai" or b.name == "openai-responses" or b.name == "anthropic" or b.name == "cohere")
	let authed = if requiresKey then (mapHas(b.state, "apiKey") and b.state.apiKey != null) else true end
	{
		backend: b.name,
		model: (if mapHas(b.state,"model") then b.state.model else null end),
		authed: authed,
		options: (if mapHas(b.state,"options") then b.state.options else {} end)
	}
end

# Switch the current backend by name.
# Args: name: Str
# Return: { backend!: Str, model: Str?, authed!: Bool, options!: {} }?  # soft-null on unknown backend
let useBackend = fun(name: Str) -> {}? do
	if not mapHas(_state.backends, name) then
		return _err("unknown backend: " + name)
	end
	_state.current = name
	status(null)
end

# List models for the current backend (if supported).
# Args: _: Null (ignored)
# Return: [Str]?  # soft-null if unsupported or on error
let models = fun(_: Null) -> [Str]? do
	let b = _state.backends[_state.current]
	if not mapHas(b, "listModels") then return _err(b.name + " does not support model listing") end
	b.listModels(b)
end

# Set the active model for the current backend.
# Args: name: Str
# Return: { backend!: Str, model: Str?, authed!: Bool, options!: {} }?
# Notes:
#	- Does not validate the model against provider; use `models()` to discover.
let useModel = fun(name: Str) -> {}? do
	let b = _state.backends[_state.current]
	b.state.model = name
	status(null)
end

# Get an editable config snapshot for the current backend.
# Args: _: Null (ignored)
# Return: { backend!: Str, baseUrl: Str?, model: Str?, options!: {}, timeoutMs: Int? }
# Notes:
#	- Only includes keys present/supported by the backend.
let getOptions = fun(_: Null) -> {} do
	let b = _state.backends[_state.current]
	let cfg = {
		backend: b.name,
		baseUrl: (if mapHas(b.state,"baseUrl") then b.state.baseUrl else null end),
		model:   (if mapHas(b.state,"model")   then b.state.model   else null end),
		options: (if mapHas(b.state,"options") then b.state.options else {} end)
	}
	if mapHas(b.state, "timeoutMs") then cfg.timeoutMs = b.state.timeoutMs end
	cfg
end

# Merge a config object into the current backend's state.
# Args: cfg: {}
# Return: { backend!: Str, model: Str?, authed!: Bool, options!: {} }?
# Notes:
#	- Unknown top-level keys in `cfg` are ignored.
#	- `options` is shallow-merged; right-hand keys take precedence.
#	- Common numeric options are surfaced but not validated; providers may ignore unsupported ones.
let setOptions = fun(cfg: {}) -> {}? do
	let b = _state.backends[_state.current]
	if mapHas(cfg, "baseUrl")   then b.state.baseUrl = cfg.baseUrl end
	if mapHas(cfg, "model")     then b.state.model   = cfg.model end
	if mapHas(cfg, "options")   then b.state.options = _merge(b.state.options, cfg.options) end
	if mapHas(cfg, "timeoutMs") then b.state.timeoutMs = cfg.timeoutMs end
	status(null)
end

# Ergonomic alias for setOptions.
# Args: cfg: {}
# Return: { backend!: Str, model: Str?, authed!: Bool, options!: {} }?
let set = fun(cfg: {}) -> {}? do
	setOptions(cfg)
end

# Store credentials for the current backend (never printed).
# Args: creds: {}   # e.g., {"apiKey": "..."}
# Return: { backend!: Str, model: Str?, authed!: Bool, options!: {} }?
# Notes:
#	- Shallow-merged into backend state; secrets are not exposed by printers.
let auth = fun(creds: {}) -> {}? do
	let b = _state.backends[_state.current]
	b.state = _merge(b.state, creds)
	status(null)
end

# Execute the current backend with a prompt.
# Args: prompt: Str
# Return: Str?  # JSON string; soft-null on error
# Notes:
#	- Returns a JSON string. Providers that return plain text are boxed as {"output": "<text>"}.
#	- Uses backend state/options as configured via `setOptions`.
let exec = fun(prompt: Str) -> Str? do
	let b = _state.backends[_state.current]
	b.exec(b, prompt)
end
