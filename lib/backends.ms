## backends.ms — provider executors
##
## Each maker returns an executor with signature:
##   exec(prompt: Str, inType: Type, outType: Type, examples: [Any], opts: { backend:Str, config:{Str:Any}, system:Str? }) -> Str | Null
##
## The llm.dispatcher adapts it to the engine-required (__oracle_execute) signature.

## --- shared helpers ----------------------------------------------------------

# Produce an annotated null (so the REPL shows the message above "null").
# msg:Str -> Null
let _failNull = fun(msg: Str) do
  # {msg}
  null
end

# JSON stringify helper.
# x:Any -> Str
let _json = fun(x: Any) -> Str do
  jsonStringify(x)
end

# Convert Types to JSON Schema objects (maps), falling back to {} on error.
# (inT:Type, outT:Type) -> {in:Any, out:Any}
let _schemas = fun(inT: Type, outT: Type) -> Any do
  {
    in:  typeToJSONSchema(inT),
    out: typeToJSONSchema(outT)
  }
end

# HTTP helper that returns the raw response map on 2xx, annotated-null otherwise.
# (url, method, headers, bodyObj) -> Any?
let _httpJSON = fun(url: Str, method: Str, headers: {Str: Str}, bodyObj: Any) -> Any do
  let req = {
    url: url,
    method: method,
    headers: headers,
    body: _json(bodyObj)
  }
  let resp = http(req)
  if resp.status < 200 or resp.status >= 300 then
    _failNull(sprintf("HTTP %d", [resp.status]))
  else
    resp
  end
end

## Heuristic content extractors (provider response → raw text)

# OpenAI responses API / chat
# obj:{} -> Str?
let _extractOpenAI = fun(obj: Any) -> Str? do
  # Try new "responses" API: { output_text: "..." }
  if mapHas(obj, "output_text") then
    obj.output_text
  else
    # Try choices[0].message.content (may be Str or array of {type:"text", text:"..."})
    if mapHas(obj, "choices") and size(obj.choices) > 0 then
      let m = obj.choices[0].message
      if m == null then
        null
      else
        let c = m.content
        if c == null then null
        elif __is_fun(c) then null 
        elif size(c) >= 1 and mapHas(c[0], "text") then c[0].text 
        else c 
        end
      end
    else
      null
    end
  end
end

# Anthropic messages
# obj:{} -> Str?
let _extractAnthropic = fun(obj: Any) -> Str? do
  # { content: [{type:"text", text:"..."}] }
  if mapHas(obj, "content") and size(obj.content) >= 1 then
    let it = obj.content[0]
    if mapHas(it, "text") then it.text else null end
  else
    null
  end
end

# Ollama /generate
# obj:{} -> Str?
let _extractOllama = fun(obj: Any) -> Str? do
  # { response: "..." } for /generate
  if mapHas(obj, "response") then obj.response else null end
end

## --- OpenAI ------------------------------------------------------------------

# Create an OpenAI executor (Responses API if possible).
# _:Null -> (prompt,inT,outT,examples,opts) -> Str?
let makeOpenAI = fun() -> Any do
  fun(prompt: Str, inT: Type, outT: Type, examples: [Any], opts: Any) -> Str? do
    let cfg = opts.config
    if cfg == null or cfg.apiKey == null then
      _failNull("OpenAI: missing apiKey (llm.configure)")
    else
      let base = cfg.baseURL or "https://api.openai.com/v1"
      let model = cfg.model or "gpt-4o-mini"
      let schemas = _schemas(inT, outT)
      let system = (opts.system or cfg.system) or "You are MindScript's oracle. Output only the result."
      let jsonMode = bool(cfg.jsonMode) or true

      # Prefer the Responses API with schema if available
      let url = base + "/responses"
      let headers = {
        "Authorization": "Bearer " + cfg.apiKey,
        "Content-Type": "application/json"
      }

      let response_format =
        if jsonMode then
          { "type": "json_schema", "json_schema": { "name": "mindscript_output", "schema": schemas.out } }
        else
          null
        end

      let body = {
        "model": model,
        "input": [
          { "role": "system", "content": system },
          { "role": "user", "content": prompt }
        ],
        "temperature": cfg.temperature or 0,
        "top_p": cfg.top_p or 1,
        "response_format": response_format
      }

      let resp = _httpJSON(url, "POST", headers, body)
      if resp == null then null else
        let obj = jsonParse(resp.body)
        let text = _extractOpenAI(obj) or resp.body   ## fallback to raw body
        text
      end
    end
  end
end

## --- Anthropic ---------------------------------------------------------------

# Create an Anthropic executor (messages API).
# _:Null -> (prompt,inT,outT,examples,opts) -> Str?
let makeAnthropic = fun() -> Any do
  fun(prompt: Str, inT: Type, outT: Type, examples: [Any], opts: Any) -> Str? do
    let cfg = opts.config
    if cfg == null or cfg.apiKey == null then
      _failNull("Anthropic: missing apiKey (llm.configure)")
    else
      let base = cfg.baseURL or "https://api.anthropic.com/v1"
      let model = cfg.model or "claude-3-5-sonnet-latest"
      let schemas = _schemas(inT, outT)
      let system = (opts.system or cfg.system) or "You are MindScript's oracle. Output only the result."
      let headers = {
        "x-api-key": cfg.apiKey,
        "anthropic-version": "2023-06-01",
        "content-type": "application/json"
      }
      # Anthropic doesn't take JSON Schema directly; embed schema strongly in the prompt.
      let body = {
        "model": model,
        "max_tokens": cfg.max_tokens or 2048,
        "system": system + "\n\nJSON OUTPUT CONSTRAINT (schema):\n" + jsonStringify(schemas.out),
        "messages": [
          { "role": "user", "content": prompt }
        ],
        "temperature": cfg.temperature or 0
      }
      let resp = _httpJSON(base + "/messages", "POST", headers, body)
      if resp == null then null else
        let obj = jsonParse(resp.body)
        let text = _extractAnthropic(obj) or resp.body
        text
      end
    end
  end
end

## --- Ollama ------------------------------------------------------------------

# Create an Ollama executor (local).
# _:Null -> (prompt,inT,outT,examples,opts) -> Str?
let makeOllama = fun() -> Any do
  fun(prompt: Str, inT: Type, outT: Type, examples: [Any], opts: Any) -> Str? do
    let cfg = opts.config or {}
    let base = cfg.baseURL or "http://localhost:11434"
    let model = cfg.model or "llama3.1"
    let schemas = _schemas(inT, outT)
    let system = (opts.system or cfg.system) or "You are MindScript's oracle. Output only the result."
    let wantJSON = bool(cfg.jsonMode) or true

    let headers = { "Content-Type": "application/json" }
    # Ollama can be nudged to JSON with {format:"json"} and instruction.
    let body = {
      "model": model,
      "system": system + (if wantJSON then "\n\nJSON OUTPUT CONSTRAINT (schema):\n" + jsonStringify(schemas.out) else "" end),
      "prompt": prompt,
      "options": {
        "temperature": cfg.temperature or 0,
        "top_p": cfg.top_p or 1
      },
      "format": if wantJSON then "json" else null end,
      "stream": false
    }

    let resp = _httpJSON(base + "/api/generate", "POST", headers, body)
    if resp == null then null else
      let obj = jsonParse(resp.body)
      let text = _extractOllama(obj) or resp.body
      text
    end
  end
end

## --- Prompt debugger (no network) -------------------------------------------

# A lightweight backend to **inspect the exact prompt** the engine built,
# without calling any external LLM.
#
# Behavior:
# • If the oracle's success type is Str (or nullable Str), returns the prompt string.
# • Otherwise, returns an **annotated null** whose annotation contains the prompt
#   (so the REPL prints the prompt above "null").
#
# _:Null -> (prompt,inT,outT,examples,opts) -> Str?
let makePrompt = fun() -> Any do
  fun(prompt: Str, _inT: Type, outT: Type, _examples: [Any], _opts: Any) -> Str? do
    # Try to return raw prompt only when the schema expects Str (or Str?).
    if isSubtype(type Str, outT) or isSubtype(type Str, __resolve_type(outT)) then
      prompt
    else
      _failNull("DEBUG PROMPT:\n" + prompt)
    end
  end
end
