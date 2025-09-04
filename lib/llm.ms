## llm.ms — minimal LLM backend manager (Ollama + OpenAI)
## Public surface (all visible; private names start with `_`):
##   backends()        -> [Str]
##   status()          -> {backend: Str, model: Str?, authed: Bool, options: {}}
##   useBackend(Str)   -> Null?
##   models()          -> [Str]?
##   useModel(Str)     -> Null?
##   set({})           -> Null?       # merge options/baseUrl/etc.
##   auth({})          -> Null?       # store credentials (never printed)
##   exec(Str) -> Str?                 # wired into __oracle_execute by prelude

# =============================
# Internal state and helpers
# =============================

# Internal registry/state of backends.
let _state = {
	current: "ollama",
	backends: {}  # name -> backend record
}

# Build a boxed JSON Schema for oracle outputs.
#
# {"type":"object","properties":{"output": <schema(T?)>}, "required":["output"]}
let _boxedSchema = fun(t: Type) -> {} do
	{
		"type": "object",
		"properties": { "output": typeToJSONSchema(t) },
		"required": ["output"]
	}
end

# Merge shallow object properties (right wins).
let _merge = fun(base: {}, patch: {}) -> {} do
	let out = {}
	for let [k, v] in base do out[k] = v end
	for let [k, v] in patch do out[k] = v end
	out
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

# =============================
# Backend: Ollama (default)
# =============================

let _ollama = do
	let baseUrl = _env("OLLAMA_BASE_URL", "http://localhost:11434")
	let model   = osEnv("OLLAMA_MODEL")  # may be null; user can choose later
	{
		name: "ollama",
		supportsSchema: true,
		state: {
			baseUrl: baseUrl,
			model: model,
			options: {"temperature": 0.5}
		},

		# List models from /api/tags (best-effort).
		listModels: fun(self: {}) -> [Str]? do
			let url = self.state.baseUrl + "/api/tags"
			let r = try(fun() do http({"url": url, "method":"GET"}) end)
			if not r.ok then return _err("ollama listModels: " + str(r.error)) end
			if r.value.status != 200 then return _err("ollama listModels: HTTP " + str(r.value.status)) end
			let body = jsonParse(r.value.body)
			if body == null then return _err("ollama listModels: bad JSON") end
			let out = []
			for item in iter(body.models) do
				out = out + [item.name]
			end
			out
		end,

		# Execute a prompt via /api/generate.
		exec: fun(self: {}, prompt: Str) -> Str? do
			if self.state.model == null then
				return _err("ollama: model not set (use llm.useModel or set OLLAMA_MODEL)")
			end
			let reqBody = {
				"model": self.state.model,
				"prompt": prompt,
				# No typed schema anymore; rely on model prompt adherence.
				"stream": false,
				"options": self.state.options
			}
			let req = {
				"url": self.state.baseUrl + "/api/generate",
				"method": "POST",
				"headers": {"Content-Type": "application/json"},
				"body": jsonStringify(reqBody)
			}
			let r = try(fun() do http(req) end)
			if not r.ok then return _err("ollama: " + str(r.error)) end
			if r.value.status != 200 then return _err("ollama: HTTP " + str(r.value.status)) end

			let env = jsonParse(r.value.body)
			if env == null then return _err("ollama: bad JSON from server") end
			let text = env.response
			if text == null then return _err("ollama: missing response") end

			# Normalize: if content parses to JSON and lacks "output", box it.
			let p = try(fun() do jsonParse(text) end)
			if p.ok then
				let v = p.value
				let hasOut = try(fun() do mapHas(v, "output") end)
				if hasOut.ok then
					jsonStringify(v)
				else
					jsonStringify({"output": v})
				end
			else
				text
			end
		end
	}
end

# =============================
# Backend: OpenAI
# =============================

let _openai = do
	let baseUrl = _env("OPENAI_BASE_URL", "https://api.openai.com/v1")
	let model   = osEnv("OPENAI_MODEL")     # may be null
	let apiKey  = osEnv("OPENAI_API_KEY")   # may be null
	{
		name: "openai",
		supportsSchema: true,
		state: {
			baseUrl: baseUrl,
			model: model,
			apiKey: apiKey,
			options: {"temperature": 0}
		},

		# List models (best-effort). Requires API key.
		listModels: fun(self: {}) -> [Str]? do
			if self.state.apiKey == null then return _err("openai listModels: missing API key") end
			let r = try(fun() do http({
				"url": self.state.baseUrl + "/models",
				"method": "GET",
				"headers": {"Authorization": "Bearer " + self.state.apiKey}
			}) end)
			if not r.ok then return _err("openai listModels: " + str(r.error)) end
			if r.value.status != 200 then return _err("openai listModels: HTTP " + str(r.value.status)) end
			let body = jsonParse(r.value.body)
			if body == null then return _err("openai listModels: bad JSON") end
			let out = []
			for m in iter(body.data) do
				out = out + [m.id]
			end
			out
		end,

		# Execute via Chat Completions (JSON object format; no schema now).
		exec: fun(self: {}, prompt: Str) -> Str? do
			if self.state.apiKey == null then return _err("openai: missing API key (set with llm.auth)") end
			if self.state.model == null then return _err("openai: model not set (use llm.useModel or set OPENAI_MODEL)") end

			let headers = {
				"Content-Type": "application/json",
				"Authorization": "Bearer " + self.state.apiKey
			}
			let body = {
				"model": self.state.model,
				"messages": [
					{"role":"system","content":"You are a careful assistant that outputs only JSON."},
					{"role":"user","content": prompt}
				],
				"response_format": {"type":"json_object"},
				"temperature": self.state.options.temperature
			}
			let req = {
				"url": self.state.baseUrl + "/chat/completions",
				"method":"POST",
				"headers": headers,
				"body": jsonStringify(body)
			}
			let r = try(fun() do http(req) end)
			if not r.ok then return _err("openai: " + str(r.error)) end
			if r.value.status != 200 then return _err("openai: HTTP " + str(r.value.status)) end

			let j = jsonParse(r.value.body)
			if j == null then return _err("openai: bad JSON from server") end
			let text = j.choices[0].message.content
			if text == null then return _err("openai: missing content") end

			# Normalize: if content parses to JSON and lacks "output", box it.
			let p = try(fun() do jsonParse(text) end)
			if p.ok then
				let v = p.value
				let hasOut = try(fun() do mapHas(v, "output") end)
				if hasOut.ok then jsonStringify(v) else jsonStringify({"output": v}) end
			else
				text
			end
		end
	}
end

# =============================
# Registry & public API
# =============================

# Register built-in backends at module load.
_state.backends["ollama"] = _ollama
_state.backends["openai"] = _openai

# If OPENAI_API_KEY is present, store it (convenience).
if _openai.state.apiKey != null then
	_state.backends["openai"].state.apiKey = _openai.state.apiKey
end

# List available backend names.
#
# Returns: [Str]
let backends = fun(_: Null) -> [Str] do
	let out = []
	for let [k, _] in _state.backends do out = out + [k] end
	out
end

# Show current selection / status (secrets not printed).
#
# Returns: {backend: Str, model: Str?, authed: Bool, options: {}}
let status = fun(_: Null) -> {} do
	let b = _state.backends[_state.current]
	let authed =
		if b.name == "openai" then (b.state.apiKey != null)
		else true end
	{
		backend: b.name,
		model: b.state.model,
		authed: authed,
		options: b.state.options
	}
end

# Switch active backend.
#
# Returns: {backend, model, authed, options}?  (annotated null on failure)
let useBackend = fun(name: Str) -> {}? do
	if not mapHas(_state.backends, name) then
		return _err("unknown backend: " + name)
	end
	_state.current = name
	status(null)
end

# List models for the active backend (if supported).
#
# Returns: [Str]? (null on error or unsupported)
let models = fun(_: Null) -> [Str]? do
	let b = _state.backends[_state.current]
	if not mapHas(b, "listModels") then
		return _err(b.name + " does not support model listing")
	end
	b.listModels(b)
end

# Choose model for active backend.
#
# Returns: {backend, model, authed, options}?  (annotated null on failure)
let useModel = fun(name: Str) -> {}? do
	let b = _state.backends[_state.current]
	b.state.model = name
	status(null)
end

# -----------------------------
# Options helpers
# -----------------------------

# Get editable backend options (safe shape).
#
# Returns a config object you can tweak and pass to setOptions().
# Keys always present: backend, baseUrl (or null), model (or null), options ({}).
# Keys present only if supported by the current backend: timeoutMs.
# Args: _: Null (ignored)
# Returns: {}
let getOptions = fun(_: Null) -> {} do
	let b = _state.backends[_state.current]
	# Build minimal, always-present shape.
	let cfg = {
		backend: b.name,
		baseUrl: (if mapHas(b.state, "baseUrl") then b.state.baseUrl else null end),
		model: (if mapHas(b.state, "model") then b.state.model else null end),
		options: (if mapHas(b.state, "options") then b.state.options else {} end)
	}
	# Add optional fields only if they exist.
	if mapHas(b.state, "timeoutMs") then
		cfg.timeoutMs = b.state.timeoutMs
	end
	cfg
end

# Set backend options from a config object.
#
# Expects (a superset of) the shape returned by getOptions(). Unknown keys ignored.
# Secrets (e.g., API keys) are NOT part of this config; use llm.auth for those.
# Args: cfg: {}
# Returns: {backend, model, authed, options}?  (annotated null on failure)
let setOptions = fun(cfg: {}) -> {}? do
	let b = _state.backends[_state.current]
	if mapHas(cfg, "baseUrl")   then b.state.baseUrl   = cfg.baseUrl end
	if mapHas(cfg, "model")     then b.state.model     = cfg.model end
	if mapHas(cfg, "options")   then b.state.options   = _merge(b.state.options, cfg.options) end
	# Apply optional fields only if provided.
	if mapHas(cfg, "timeoutMs") then b.state.timeoutMs = cfg.timeoutMs end
	status(null)
end

# Store credentials (never printed).
#
# For OpenAI: {"apiKey": "..."} ; other backends may use other keys.
# Returns: {backend, model, authed, options}?  (annotated null on failure)
let auth = fun(creds: {}) -> {}? do
	let b = _state.backends[_state.current]
	b.state = _merge(b.state, creds)
	status(null)
end

# Oracle executor entry point — prelude wires this into __oracle_execute.
#
# Returns: Str?  (raw JSON or null on failure)
let exec = fun(prompt: Str) -> Str? do
	let b = _state.backends[_state.current]
	b.exec(b, prompt)
end
