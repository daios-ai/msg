## stdlib.ms
## Minimal standard library implemented in MindScript on top of builtins.
## Notes:
## • Relies on: printf/sprintf, error, nowMillis, dateNow, clone, exit,
##   jsonStringify/jsonParse, randFloat, typeOf/isType/isSubtype, funInfo/funType,
##   mapHas/mapDelete, http, import/importCode, substr/split/join/strip/len, try.
## • We avoid shadowing native helpers (e.g., len).

# Print a value and return it.
# Useful for debugging or piping: insert `| print` in a chain.
# value:Any -> Any
let print = fun(x: Any) -> Any do
  printf("%s", [str(x)])
end

# Print a value and a newline and return it.
# Useful for debugging or piping: insert `| print` in a chain.
# value:Any -> Any
let println = fun(x: Any) -> Any do
  printf("%s\n", [str(x)])
end

# Return an error (a null value annotated with an error message)
let error = fun(msg: Str) -> Null do
  __annotate(msg, null)
end

# Assert that a condition is true.
# Returns true if it holds, otherwise throws an error.
# condition:Bool -> Bool
let assert = fun(cond: Bool) -> Bool do
  if cond then
    true
  else
    fail("assertion failed")
  end
end

# Current timestamp in milliseconds since the Unix epoch.
# _:Null -> Int
let tsNow = fun(_: Null) -> Int do nowMillis() end

# Random float uniformly in [0, 1).
# _:Null -> Num
let random = fun(_: Null) -> Num do randFloat() end

## -----------------------------
## Tiny internal helpers
## -----------------------------

# Build a JSON array incrementally (string-backed buffer).
# _:Null -> {push:(Any -> Null), build:(Null -> [Any])}
let _arrayBuilder = fun() -> {push:(Any -> Null), build:(Null -> [Any])} do
  let buf = "["
  let first = true
  let push = fun(x: Any) -> Null do
    if first then
      buf = buf + jsonStringify(x)
      first = false
      null
    else
      buf = buf + "," + jsonStringify(x)
      null
    end
  end
  let build = fun(_: Null) -> [Any] do
    jsonParse(buf + "]")
  end
  {push: push, build: build}
end

## -----------------------------
## Iterators
## -----------------------------

# Infinite iterator: 0, 1, 2, 3, ...
# _:Null -> (Null -> Int)
let natural0 = fun(_: Null) -> (Null -> Int) do
  let i = 0
  fun(_: Null) -> Int do
    let v = i
    i = i + 1
    v
  end
end

# Infinite iterator: 1, 2, 3, 4, ...
# _:Null -> (Null -> Int)
let natural = fun(_: Null) -> (Null -> Int) do
  let i = 1
  fun(_: Null) -> Int do
    let v = i
    i = i + 1
    v
  end
end

# Integer range iterator (stop exclusive; infinite if stop is null).
# start:Int -> stop:Int? -> (Null -> Int?)
let range = fun(start: Int, stop: Int?) -> (Null -> Int?) do
  let i = start
  fun(_: Null) -> Int? do
    if stop == null then
      let v = i
      i = i + 1
      v
    elif i >= stop then
      null
    else
      let v = i
      i = i + 1
      v
    end
  end
end

# Turn arrays into (Null -> Any?) iterators; pass iterators through unchanged.
# value:Any -> (Null -> Any?)
let iter = fun(v: Any) -> (Null -> Any?) do
  if isType(v, type [Any]) then
    let xs = v
    let i = 0
    fun(_: Null) -> Any? do
      if i < len(xs) then
        let y = xs[i]
        i = i + 1
        y
      else
        null
      end
    end
  else
    v  ## assume it’s already an iterator (Null -> Any?)
  end
end

# Collect an iterator into an array.
# iterator:(Null -> Any?) -> [Any]
let list = fun(it: (Null -> Any?)) -> [Any] do
  let b = _arrayBuilder()
  for _ in natural0(null) do
    let x = it(null)
    if x == null then break(null) end
    b.push(x)
  end
  b.build(null)
end

# Map over an iterator (lazy transform).
# f:(Any -> Any) -> it:(Null -> Any?) -> (Null -> Any?)
let map = fun(f: (Any -> Any), it: (Null -> Any?)) -> (Null -> Any?) do
  fun(_: Null) -> Any? do
    let x = it(null)
    if x == null then null else f(x) end
  end
end

# Filter an iterator (lazy predicate).
# cond:(Any -> Bool) -> it:(Null -> Any?) -> (Null -> Any?)
let filter = fun(cond: (Any -> Bool), it: (Null -> Any?)) -> (Null -> Any?) do
  fun(_: Null) -> Any? do
    for _ in natural0(null) do
      let x = it(null)
      if x == null then break(null) end
      if cond(x) then return(x) end
    end
    null
  end
end

# Fold an iterator with a binary function.
# f:(Any -> Any -> Any) -> it:(Null -> Any?) -> Any
let reduce = fun(f: (Any -> Any -> Any), it: (Null -> Any?)) -> Any do
  let first = it(null)
  if first == null then
    null
  else
    let acc = first
    for _ in natural0(null) do
      let x = it(null)
      if x == null then break(null) end
      acc = f(acc, x)
    end
    acc
  end
end

## -----------------------------
## Arrays
## -----------------------------

# Slice a subarray [s, e) (end exclusive).
# array:[Any] -> s:Int -> e:Int -> [Any]
let slice = fun(xs: [Any], s: Int, e: Int) -> [Any] do
  let b = _arrayBuilder()
  for i in range(s, e) do
    b.push(xs[i])
  end
  b.build(null)
end

# Return a new array with value appended (non-mutating).
# array:[Any] -> value:Any -> [Any]
let push = fun(xs: [Any], v: Any) -> [Any] do
  let b = _arrayBuilder()
  for x in iter(xs) do b.push(x) end
  b.push(v)
  b.build(null)
end

# Return the last element, or null if empty.
# array:[Any] -> Any
let pop = fun(xs: [Any]) -> Any do
  let n = len(xs)
  if n == 0 then null else xs[n - 1] end
end

# Return a new array with value inserted at the front (non-mutating).
# array:[Any] -> value:Any -> [Any]
let shift = fun(xs: [Any], v: Any) -> [Any] do
  let b = _arrayBuilder()
  b.push(v)
  for x in iter(xs) do b.push(x) end
  b.build(null)
end

# Return the first element, or null if empty.
# array:[Any] -> Any
let unshift = fun(xs: [Any]) -> Any do
  if len(xs) == 0 then null else xs[0] end
end

## -----------------------------
## Objects (maps)
## -----------------------------

# Delete a property (in place) and return the same object.
# obj:{} -> prop:Str -> {}
let delete = fun(obj: {}, prop: Str) -> {} do
  mapDelete(obj, prop)
end

# Check whether a key exists in an object.
# obj:{} -> key:Str -> Bool
let exists = fun(obj: {}, key: Str) -> Bool do
  mapHas(obj, key)
end

# Dynamic property get.
# obj:{} -> key:Str -> Any
let get = fun(obj: {}, key: Str) -> Any do
  obj[key]
end

# Dynamic property set (returns the assigned value).
# obj:{} -> key:Str -> value:Any -> Any
let set = fun(obj: {}, key: Str, value: Any) -> Any do
  obj[key] = value
  value
end

# Iterator over object keys (in insertion order).
# obj:{} -> (Null -> Str?)
let keys = fun(obj: {}) -> (Null -> Str?) do
  let it = iter(obj)
  fun(_: Null) -> Str? do
    let pair = it(null)
    if pair == null then
      null
    else
      pair[0]
    end
  end
end

# Iterator over object values (in insertion order).
# obj:{} -> (Null -> Any?)
let values = fun(obj: {}) -> (Null -> Any?) do
  let it = iter(obj)
  fun(_: Null) -> Any? do
    let pair = it(null)
    if pair == null then
      null
    else
      pair[1]
    end
  end
end

# List public attributes of an object (and modules, if supported).
# Filters out names that start with "_" (treated as private).
# obj:{}|Module -> [Str]
let dir = fun(x: Any) -> [Str] do
  ## Map/object path
  if isType(x, type {}) then
    let out = []
    for let [k, _] in x do
      if not (substr(k, 0, 1) == "_") then
        out = out + [k]
      end
    end
    out
  else
    ## Optional module support if a native `moduleKeys(mod) -> [Str]` exists.
    let probe = try(fun() do moduleKeys(x) end)
    if probe.ok then
      let out = []
      for name in iter(probe.value) do
        if not (substr(name, 0, 1) == "_") then
          out = out + [name]
        end
      end
      out
    else
      []
    end
  end
end

# Placeholder for method discovery on objects.
# Not implemented in pure MindScript; needs a native predicate for functions.
# obj:{} -> [Str]
let dirFun = fun(_obj: {}) -> [Str] do
  error("dirFun is not implemented in stdlib; requires native function detection")
end

## -----------------------------
## Importing / Modules
## -----------------------------

# Curried wrapper over importCode(name, src) for convenience.
# code:Str -> name:Str -> {}
let codeImport = fun(code: Str) -> (Str -> {}) do
  fun(name: Str) -> {} do
    importCode(name, code)
  end
end

# Import a module from a URL or path (delegates to native import()).
# url:Str -> {}
let netImport = fun(url: Str) -> {} do
  import(url)
end

## -----------------------------
## Networking
## -----------------------------

# GET a URL; return body on HTTP 200, otherwise null.
# url:Str -> Str?
let www = fun(url: Str) -> Str? do
  let r = http({ url: url, method: "GET" })
  if r.status == 200 then r.body else null end
end

## -----------------------------
## Schemas / Grammars (placeholders)
## -----------------------------

# Render a JSON Schema from a Type (not implemented here).
# t:Type -> Str
let schema = fun(_t: Type) -> Str do
  error("schema(Type) not implemented in stdlib; provide a native bridge")
end

# Render a BNF-like grammar from a Type (not implemented here).
# t:Type -> Str
let bnf = fun(_t: Type) -> Str do
  error("bnf(Type) not implemented in stdlib; provide a native bridge")
end

## -----------------------------
## Annotations
## -----------------------------

# Get a value’s full docstring (if any).
# x:Any -> Str?
let getNote = fun(x: Any) -> Str? do
  help(x)
end

# Attach/replace a value’s docstring (not implemented here).
# x:Any -> note:Str? -> Any
let setNote = fun(_x: Any, _ann: Str?) -> Any do
  error("setNote requires native support to attach annotations at runtime")
end

## -----------------------------
## Misc
## -----------------------------

# Bind a value as the implicit first argument of a function.
# value:Any -> func:(Any -> Any) -> (Any -> Any)
let bindMethod = fun(value: Any, f: (Any -> Any)) -> (Any -> Any) do
  fun(x: Any) -> Any do f(value, x) end
end

# Best-effort stable hash of a value’s printable form (may collide).
# value:Any -> Int
let uid = fun(value: Any) -> Int do
  let s = str(value)
  let h = 5381
  for ch in split(s, "") do
    ## crude: use string length as a surrogate for codepoint weight (no ord())
    h = ((h * 33) + len(ch))
  end
  h
end

# Swallow any value and return null.
# _:Any -> Null
let mute = fun(_: Any) -> Null do null end


## -----------------------------
## Oracle bootstrap (default backend: Ollama)
## -----------------------------

# Minimal + robust Ollama executor (auto-boxes, uses boxed schema if supported)

let __oracle_execute = fun(prompt: Str, _inT: Type, _outT: Type, _examples: Any) -> Str do
  let base = osEnv("OLLAMA_BASE_URL")
  if base == null then base = "http://localhost:11434" end

  let model = osEnv("OLLAMA_MODEL")
  if model == null then model = "Phi3" end

  # Build a boxed JSON Schema: {"output": schema(outT)}
  let boxed = {
    "type": "object",
    "properties": { "output": typeToJSONSchema(_outT) },
    "required": ["output"]
  }

  let req = {
    "url": base + "/api/generate",
    "method": "POST",
    "headers": {"Content-Type": "application/json"},
    "body": jsonStringify({
      "model": model,
      "prompt": prompt,
      # If your Ollama supports schema-in-format, this aligns with the prompt’s schema.
      # If not, Ollama will ignore and still try to emit JSON due to the prompt text.
      "format": boxed,
      "stream": false,
      "options": {"temperature": 0.5}
    })
  }

  let r = try(fun() do http(req) end)
  if r.ok != true then
    let msg = r.error
    if msg == null then msg = "request failed" end
    return __annotate("ollama: " + msg)(null)
  end

  let resp = r.value
  if resp.status != 200 then
    return __annotate("ollama: HTTP " + str(resp.status))(null)
  end

  let env = jsonParse(resp.body)
  if env == null then return __annotate("ollama: bad JSON from server")(null) end

  let text = env.response
  if text == null then return __annotate("ollama: missing response")(null) end

  # Normalize:
  # - If text is JSON of an object with "output": return canonical JSON
  # - If text is JSON of something else (number/array/string/map w/o "output"), box it
  # - If text isn't JSON at all, return as-is (execOracle will annotate accordingly)
  let p1 = try(fun() do jsonParse(text) end)
  if p1.ok == true then
    let v1 = p1.value

    # object with "output"
    let hasOut = try(fun() do mapHas(v1, "output") end)
    if hasOut.ok == true then
      return jsonStringify(v1)
    end

    # anything else → box
    return jsonStringify({"output": v1})
  end

  # Fallback: return raw text (should already be {"output": ...})
  text
end

let examples = [[1, "one"], [12, "twelve"], [7, "seven"]]

# Say the number
let number = oracle(n: Int) -> Str from examples
