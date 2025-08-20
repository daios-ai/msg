## llm.ms — Oracle backend using Ollama Phi4 (Design A)

# Convenience: annotated-null
let fail = fun(msg: Str) -> Any do __annotate(msg, null) end

# Helpers
let hasKey = fun(m: {Str: Any}, k: Str) -> Bool do mapHas(m, k) end

let firstChar = fun(s: Str) -> Str do
  let t = lstrip(s)
  if t == "" then "" else substr(t, 0, 1) end
end

let looksLikeJSON = fun(s: Str) -> Bool do
  let c = firstChar(s)
  (c == "{") or (c == "[") or (c == "\"")
end

# Required: executor hook (prompt already contains the concrete call input per Design A)
let __oracle_execute =
  fun(prompt: Str, inType: Type, outType: Type, examples: [Any]) -> Any do
    # POST to Ollama
    let reqBody = { model: "phi3", prompt: prompt, stream: false }
    let resp = http({
      url: "http://localhost:11434/api/generate",
      method: "POST",
      headers: {"Content-Type": "application/json"},
      body: jsonStringify(reqBody)
    })

    if resp.status < 200 or resp.status >= 300 then
      return(fail(sprintf("ollama HTTP %d", [resp.status])))
    end

    # Parse Ollama response and extract text
    let gen = jsonParse(resp.body)
    let txt =
      if hasKey(gen, "response") then
        gen.response
      elif hasKey(gen, "output") then
        gen.output
      elif hasKey(gen, "message") and hasKey(gen.message, "content") then
        gen.message.content
      else
        ""
      end

    if txt == "" then
      return(fail("ollama: empty model response"))
    end

    # Try to parse model text as JSON
    let parsedTry = try(fun() do jsonParse(txt) end)
    let candidate =
      if parsedTry.ok then
        parsedTry.value
      elif looksLikeJSON(txt) then
        return(fail("model output looked like JSON but could not be parsed"))
      else
        txt
      end

    ## Enforce declared return type
    if isType(candidate, outType) then
      candidate
    else
      return(fail("oracle output did not match the declared return type"))
    end
end
