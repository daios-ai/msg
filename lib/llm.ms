# llm.ms — minimal LLM backend manager (uniform contract + startup probe)
#
# Public surface:
# init()                 -> { backends!: [Str], current!: Str }          # probe all, pick a usable default, print diagnostics
# backends()             -> [Str]                                        # available backend names
# status()               -> { backend!: Str, model: Str?, authed!: Bool, options!: {} }
# useBackend(Str)        -> {}?                                          # switch current; returns status or soft-null
# models()               -> [Str]?                                       # list models for current backend (if supported)
# useModel(Str)          -> {}?                                          # set model for current backend; returns status
# getConfig()            -> {}                                           # editable snapshot {backend, baseUrl, model, options, [timeoutMs]}
# setConfig({})          -> {}?                                          # shallow-merge config; returns status
# auth({})               -> {}?                                          # store credentials (e.g., {"apiKey":"…"}); returns status
# exec({})               -> Str?                                         # oracle hook: run and return provider RAW STRING (or soft-null on error)
#
# Notes:
# - Backends expose { name, state, listModels?, probe, exec }.
# - init() prints diagnostics and auto-selects the first OK backend if current is not OK.
# - Only API keys are read from env; baseUrl/model are configured via setConfig/useModel.
# - Options live in per-backend state.options. Common knobs:
#   temperature, top_p, top_k, max_tokens, presence_penalty, frequency_penalty
# - Backends ignore unknown/unsupported options gracefully.
# - HTTP failures and provider error payloads are surfaced as soft errors (annotated null) with clear messages.
# - Oracles validate/box as {"output": x}. Backends DO NOT normalize/box.
# - Structured outputs: if req.outputSchema is present, backends that support it will request
#   schema-constrained JSON output (“best available” mode).

# =============================
# Internal state and minimal helpers
# =============================

let _state = {
	current: "ollama",
	backends: {} # name -> backend record
}

# Extract prompt from oracle request bundle.
let _reqPrompt = fun(req: {}) -> Str? do
	if req == null then
		return error("missing request")
	end
	if not mapHas(req, "prompt") or req.prompt == null then
		return error("missing prompt")
	end
	str(req.prompt)
end

# Extract output schema (if provided) from oracle request bundle.
let _reqOutputSchema = fun(req: {}) -> {}? do
	if req != null and mapHas(req, "outputSchema") and req.outputSchema != null then
		req.outputSchema
	else
		null
	end
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

# schemaCloseWorld(schema) walks a JSON Schema value
# and forces every object-shaped subschema to include:
#   additionalProperties: false
# This is needed for OpenAI strict json_schema response_format.
let _schemaCloseWorld = fun(schema: Any) -> Any do
	let TMap = type {}
	let TArr = type [Any]

	let arrHas = fun(xs: [Any], needle: Any) -> Bool do
		for v in iter(xs) do
			if v == needle then
				return true
			end
		end
		false
	end

	let isObjectSchema = fun(m: {}) -> Bool do
		# Treat as object schema if:
		# - type is "object" OR includes "object" in a union array, OR
		# - it has object-y keys like properties/required/additionalProperties.
		if mapHas(m, "type") then
			if m.type == "object" then
				return true
			end
			if m.type != null and isSubtype(typeOf(m.type), TArr) and arrHas(m.type, "object") then
				return true
			end
		end
		mapHas(m, "properties") or mapHas(m, "required") or mapHas(m, "additionalProperties")
	end

	let close = fun(x: Any) -> Any do
		if x == null then
			return null
		end

		let tx = typeOf(x)

		# Arrays: close each element.
		if isSubtype(tx, TArr) then
			let out = []
			for v in iter(x) do
				out = out + [close(v)]
			end
			return out
		end

		# Maps: close each value; then close-world the object schema itself.
		if isSubtype(tx, TMap) then
			let out = {}
			for [k, v] in x do
				out[k] = close(v)
			end
			if isObjectSchema(out) then
				out.additionalProperties = false
			end
			return out
		end

		# Primitives (Str/Num/Bool/etc): unchanged.
		x
	end

	close(schema)
end

# =============================
# Backends (raw-string exec, no boxing)
# =============================

# Backend: Ollama
let _ollama = {
	name: "ollama",
	state: {baseUrl: "http://localhost:11434", model: null, options: {}},
	listModels: fun(self: {}) -> [Str]? do
		let url = self.state.baseUrl + "/api/tags"
		let r = http({url: url, method: "GET"})
		if r == null then
			return error("ollama listModels: request failed")
		end
		if r.status != 200 then
			return error("ollama listModels: HTTP " + str(r.status))
		end
		let body = jsonParse(r.body)
		if body == null then
			return error("ollama listModels: bad JSON")
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
	exec: fun(self: {}, req: {}) -> Str? do
		let prompt = _reqPrompt(req)
		if prompt == null then
			return prompt
		end
		if self.state.model == null then
			return error("ollama: model not set (use llm.useModel)")
		end

		let eff = self.state.options
		let reqBody = {
			model: self.state.model,
			prompt: prompt,
			stream: false,
			options: {}
		}

		# Best available structured output: pass JSON Schema via "format" when provided.
		let schema = _reqOutputSchema(req)
		if schema != null then
			reqBody.format = schema
		end

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
			return error("ollama: request failed")
		end
		if r.status != 200 then
			let b = jsonParse(r.body)
			if b != null and mapHas(b, "error") then
				return error("ollama: " + str(b.error))
			end
			return error("ollama: HTTP " + str(r.status))
		end

		let j = jsonParse(r.body)
		if j == null then
			return error("ollama: bad JSON from server")
		end
		if not mapHas(j, "response") or j.response == null then
			return error("ollama: missing response")
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
		options: {}
	},
	listModels: fun(self: {}) -> [Str]? do
		if self.state.apiKey == null then
			return error("openai listModels: missing API key")
		end
		let r = http({
			url: self.state.baseUrl + "/models",
			method: "GET",
			headers: {Authorization: "Bearer " + self.state.apiKey}
		})
		if r == null then
			return error("openai listModels: request failed")
		end
		if r.status != 200 then
			return error("openai listModels: HTTP " + str(r.status))
		end
		let body = jsonParse(r.body)
		if body == null then
			return error("openai listModels: bad JSON")
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
	exec: fun(self: {}, req: {}) -> Str? do
		let prompt = _reqPrompt(req)
		if prompt == null then
			return prompt
		end
		if self.state.apiKey == null then
			return error("openai: missing API key (llm.auth)")
		end
		if self.state.model == null then
			return error("openai: model not set")
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

		# Best available structured output:
		# - if schema provided: request strict JSON Schema constrained output
		# - else: request JSON object (still not schema-constrained)
		let schema = _reqOutputSchema(req)
		if schema != null then
			schema = _schemaCloseWorld(schema)
			body.response_format = {
				type: "json_schema",
				json_schema: {name: "oracle_output", schema: schema, strict: true}
			}
		else
			body.response_format = {type: "json_object"}
		end

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
			return error("openai: request failed")
		end
		if r.status != 200 then
			let b = jsonParse(r.body)
			if b != null and mapHas(b, "error") and mapHas(b.error, "message") then
				return error("openai: " + b.error.message)
			end
			return error("openai: HTTP " + str(r.status))
		end

		let j = jsonParse(r.body)
		if j == null then
			return error("openai: bad JSON from server")
		end
		if not mapHas(j, "choices") or len(j.choices) == 0 or not mapHas(j.choices[0], "message") or not mapHas(j.choices[0].message, "content") or j.choices[0].message.content == null then
			return error("openai: missing content")
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
		options: {}
	},
	listModels: fun(self: {}) -> [Str]? do
		if self.state.apiKey == null then
			return error("openai-responses listModels: missing API key")
		end
		let r = http({
			url: self.state.baseUrl + "/models",
			method: "GET",
			headers: {Authorization: "Bearer " + self.state.apiKey}
		})
		if r == null then
			return error("openai-responses listModels: request failed")
		end
		if r.status != 200 then
			return error("openai-responses listModels: HTTP " + str(r.status))
		end
		let body = jsonParse(r.body)
		if body == null then
			return error("openai-responses listModels: bad JSON")
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
	exec: fun(self: {}, req: {}) -> Str? do
		let prompt = _reqPrompt(req)
		if prompt == null then
			return prompt
		end
		if self.state.apiKey == null then
			return error("openai-responses: missing API key (llm.auth)")
		end
		if self.state.model == null then
			return error("openai-responses: model not set")
		end

		let eff = self.state.options
		let body = {
			model: self.state.model,
			input: prompt,
			text: {format: {type: "json_object"}}
		}

		# Best available structured output: request strict JSON Schema constrained output.
		let schema = _reqOutputSchema(req)
		if schema != null then
			schema = _schemaCloseWorld(schema)
			body.text = {
				format: {
					type: "json_schema",
					name: "oracle_output",
					schema: schema,
					strict: true
				}
			}
		end

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
			return error("openai-responses: request failed")
		end
		if r.status != 200 then
			let b = jsonParse(r.body)
			if b != null and mapHas(b, "error") and mapHas(b.error, "message") then
				return error("openai-responses: " + b.error.message)
			end
			return error("openai-responses: HTTP " + str(r.status))
		end

		let j = jsonParse(r.body)
		if j == null then
			return error("openai-responses: bad JSON from server")
		end
		if not mapHas(j, "output") or len(j.output) == 0 then
			return error("openai-responses: missing content")
		end

		let text = null
		for msg in j.output do
			if mapHas(msg, "content") and len(msg.content) > 0 then
				for part in msg.content do
					if mapHas(part, "type") and part.type == "output_text" and mapHas(part, "text") and part.text != null then
						text = part.text
						break
					end
				end
				break
			end
		end
		if text == null then
			return error("openai-responses: missing content")
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
		options: {max_tokens: 1024}
	},
	listModels: fun(self: {}) -> [Str]? do
		if self.state.apiKey == null then
			return error("anthropic listModels: missing API key")
		end
		let r = http({
			url: self.state.baseUrl + "/models",
			method: "GET",
			headers: {
				"x-api-key": self.state.apiKey,
				"anthropic-version": self.state.version,
				"content-type": "application/json"
			}
		})
		if r == null then
			return error("anthropic listModels: request failed")
		end
		if r.status != 200 then
			return error("anthropic listModels: HTTP " + str(r.status))
		end
		let body = jsonParse(r.body)
		if body == null then
			return error("anthropic listModels: bad JSON")
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
				"x-api-key": self.state.apiKey,
				"anthropic-version": self.state.version,
				"content-type": "application/json"
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
	exec: fun(self: {}, req: {}) -> Str? do
		let prompt = _reqPrompt(req)
		if prompt == null then
			return prompt
		end
		if self.state.apiKey == null then
			return error("anthropic: missing API key (llm.auth)")
		end
		if self.state.model == null then
			return error("anthropic: model not set")
		end

		let eff = self.state.options
		let body = {
			model: self.state.model,
			messages: [{role: "user", content: prompt}]
		}

		# Best available structured output: request JSON Schema constrained output if provided.
		let schema = _reqOutputSchema(req)
		let headers = {
			"x-api-key": self.state.apiKey,
			"anthropic-version": self.state.version,
			"content-type": "application/json"
		}
		if schema != null then
			body.output_format = {type: "json_schema", schema: schema}
			# Beta header used by Anthropic for structured outputs (may vary by deployment/model).
			headers["anthropic-beta"] = "structured-outputs-2025-11-13"
		end

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
			headers: headers,
			body: jsonStringify(body)
		})
		if r == null then
			return error("anthropic: request failed")
		end
		if r.status != 200 then
			let b = jsonParse(r.body)
			if b != null and mapHas(b, "error") and mapHas(b.error, "message") then
				return error("anthropic: " + b.error.message)
			end
			return error("anthropic: HTTP " + str(r.status))
		end

		let j = jsonParse(r.body)
		if j == null then
			return error("anthropic: bad JSON from server")
		end
		if not mapHas(j, "content") or len(j.content) == 0 or not mapHas(j.content[0], "text") or j.content[0].text == null then
			return error("anthropic: missing content")
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
		options: {}
	},
	listModels: fun(self: {}) -> [Str]? do
		if self.state.apiKey == null then
			return error("cohere listModels: missing API key")
		end
		let r = http({
			url: self.state.baseUrl + "/models",
			method: "GET",
			headers: {Authorization: "Bearer " + self.state.apiKey}
		})
		if r == null then
			return error("cohere listModels: request failed")
		end
		if r.status != 200 then
			return error("cohere listModels: HTTP " + str(r.status))
		end
		let body = jsonParse(r.body)
		if body == null then
			return error("cohere listModels: bad JSON")
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
	exec: fun(self: {}, req: {}) -> Str? do
		let prompt = _reqPrompt(req)
		if prompt == null then
			return prompt
		end
		if self.state.apiKey == null then
			return error("cohere: missing API key (llm.auth)")
		end
		if self.state.model == null then
			return error("cohere: model not set")
		end

		let eff = self.state.options
		let body = {model: self.state.model, message: prompt}

		# Best available structured output: request JSON object, attach schema when provided (if supported).
		let schema = _reqOutputSchema(req)
		if schema != null then
			body.response_format = {type: "json_object", schema: schema}
		else
			body.response_format = {type: "json_object"}
		end

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
			return error("cohere: request failed")
		end
		if r.status != 200 then
			let b = jsonParse(r.body)
			if b != null and mapHas(b, "message") then
				return error("cohere: " + b.message)
			end
			return error("cohere: HTTP " + str(r.status))
		end

		let j = jsonParse(r.body)
		if j == null then
			return error("cohere: bad JSON from server")
		end
		if mapHas(j, "text") and j.text != null then
			return j.text
		end
		if mapHas(j, "message") and mapHas(j.message, "content") and j.message.content != null then
			return j.message.content
		end
		error("cohere: missing content")
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
		return error("unknown backend: " + name)
	end
	_state.current = name
	status(null)
end

# List models for the current backend (if supported).
let models = fun(_: Null) -> [Str]? do
	let b = _state.backends[_state.current]
	if not mapHas(b, "listModels") then
		return error(b.name + " does not support model listing")
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
let auth = fun(creds: {apiKey: Str?}) -> {}? do
	let b = _state.backends[_state.current]
	# Ensure apiKey is set/cleared where status() looks.
	if mapHas(creds, "apiKey") then
		b.state.apiKey = creds.apiKey
	end
	b.state = b.state + creds # Shallow-merge any other credential keys.
	status(null)
end

# Execute the current backend with an oracle request bundle.
# Returns the provider's RAW STRING (or soft-null on error).
let exec = fun(req: {}) -> Str? do
	let b = _state.backends[_state.current]
	b.exec(b, req)
end
