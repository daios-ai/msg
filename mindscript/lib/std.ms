# stdlib.ms
# Minimal standard library implemented in MindScript on top of your builtins.
# Notes:
# • Relies on: printf/sprintf, error, nowMillis, dateNow, clone, exit,
#   jsonStringify/jsonParse, randFloat, typeOf/isType/isSubtype, funInfo/funType,
#   mapHas/mapDelete, http (native), import/importCode, substr/split/join/strip/...
# • `size(x)` is expected to be a native (arrays/maps/strings). We don’t shadow it.

# -----------------------------
# General
# -----------------------------

# Print value (newline) and return it (convenient for piping).
# value:Any -> Any
let print = fun(x: Any) -> Any do
  printf("%v\n", x)
end

# Assert or throw.
# condition:Bool -> Bool
let assert = fun(cond: Bool) -> Bool do
  if cond then
    true
  else
    error("assertion failed")
  end
end

# tsNow: current timestamp in ms
# _:Null -> Int
let tsNow = fun(_: Null) -> Int do nowMillis() end

# random: uniform [0,1)
# _:Null -> Num
let random = fun(_: Null) -> Num do randFloat() end

# -----------------------------
# Tiny internal helpers
# -----------------------------

# Build a JSON array using strings (handy when we need to accumulate without a native append).
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

# -----------------------------
# Iterators
# -----------------------------

# natural0: 0,1,2,3,...
# _:Null -> (Null -> Int)
let natural0 = fun(_: Null) -> (Null -> Int) do
  let i = 0
  fun(_: Null) -> Int do
    let v = i
    i = i + 1
    v
  end
end

# natural: 1,2,3,4,...
# _:Null -> (Null -> Int)
let natural = fun(_: Null) -> (Null -> Int) do
  let i = 1
  fun(_: Null) -> Int do
    let v = i
    i = i + 1
    v
  end
end

# range(start, stop?) -> iterator (stop exclusive; infinite when stop==null)
# start:Int -> stop:Int? -> (Null -> Int?)
let range = fun(start: Int, stop: Int?) -> (Null -> Int?) do
  let i = start
  fun(_: Null) -> Int? do
    if stop == null then
      let v = i
      i = i + 1
      v
    else
      if i >= stop then
        null
      else
        let v = i
        i = i + 1
        v
      end
    end
  end
end

# iter: turn arrays into (Null->Any?) iterators; pass iterators through.
# value:Any -> (Null -> Any?)
let iter = fun(v: Any) -> (Null -> Any?) do
  if isType(v, type [Any]) then
    let xs = v
    let i = 0
    fun(_: Null) -> Any? do
      if i < size(xs) then
        let y = xs[i]
        i = i + 1
        y
      else
        null
      end
    end
  else
    # assume it’s already an iterator (Null -> Any?)
    v
  end
end

# list: collect an iterator into an array (via JSON builder).
# iterator:(Null -> Any?) -> [Any]
let list = fun(it: (Null -> Any?)) -> [Any] do
  let b = _arrayBuilder()
  for _ in natural0(null) do
    let x = it(null)
    if x == null then
      break(null)
    end
    b.push(x)
  end
  b.build(null)
end

# map: transform elements of an iterator.
# f:(Any -> Any) -> it:(Null -> Any?) -> (Null -> Any?)
let map = fun(f: (Any -> Any), it: (Null -> Any?)) -> (Null -> Any?) do
  fun(_: Null) -> Any? do
    let x = it(null)
    if x == null then null else f(x) end
  end
end

# filter: pass only elements that satisfy cond.
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

# reduce: fold an iterator with a binary function.
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

# -----------------------------
# Arrays
# -----------------------------

# slice: array -> s -> e -> subarray (e exclusive)
# array:[Any] -> s:Int -> e:Int -> [Any]
let slice = fun(xs: [Any]) -> Int -> Int -> [Any] do
  let b = _arrayBuilder()
  for i in range(s, e) do
    b.push(xs[i])
  end
  b.build(null)
end

# push: return new array with value appended (non-mutating).
# array:[Any] -> value:Any -> [Any]
let push = fun(xs: [Any], v: Any) -> [Any] do
  let b = _arrayBuilder()
  for x in iter(xs) do b.push(x) end
  b.push(v)
  b.build(null)
end

# pop: return last value (or null if empty).
# array:[Any] -> Any
let pop = fun(xs: [Any]) -> Any do
  let n = size(xs)
  if n == 0 then null else xs[n - 1] end
end

# shift: insert value at front (returns a new array).
# array:[Any] -> value:Any -> [Any]
let shift = fun(xs: [Any], v: Any) -> [Any] do
  let b = _arrayBuilder()
  b.push(v)
  for x in iter(xs) do b.push(x) end
  b.build(null)
end

# unshift: pop first value (or null if empty).
# array:[Any] -> Any
let unshift = fun(xs: [Any]) -> Any do
  if size(xs) == 0 then null else xs[0] end
end

# -----------------------------
# Objects (maps)
# -----------------------------

# delete a property (in place) and return the object.
# obj:{} -> prop:Str -> {}
let delete = fun(obj: {}, prop: Str) -> {} do
  mapDelete(obj, prop)
end

# exists: check if key exists.
# obj:{} -> key:Str -> Bool
let exists = fun(obj: {}, key: Str) -> Bool do
  mapHas(obj, key)
end

# get: dynamic property get.
# obj:{} -> key:Str -> Any
let get = fun(obj: {}, key: Str) -> Any do
  obj[key]
end

# set: dynamic property set (returns the value).
# obj:{} -> key:Str -> value:Any -> Any
let set = fun(obj: {}, key: Str, value: Any) -> Any do
  obj[key] = value
  value
end

# keys: iterator over object keys (no extra storage).
# obj:{} -> (Null -> Str?)
let keys = fun(obj: {}) -> (Null -> Str?) do
  let i = 0
  fun(_: Null) -> Str? do
    let j = 0
    for k in obj do
      if j == i then
        i = i + 1
        return(k)
      end
      j = j + 1
    end
    null
  end
end

# values: iterator over object values.
# obj:{} -> (Null -> Any?)
let values = fun(obj: {}) -> (Null -> Any?) do
  let ki = keys(obj)
  fun(_: Null) -> Any? do
    let k = ki(null)
    if k == null then null else obj[k] end
  end
end

# dir: list of keys (in insertion order).
# obj:{} -> [Str]
let dir = fun(obj: {}) -> [Str] do
  list(keys(obj))
end

# dirFun: method names (requires real function detection; stub for now).
# obj:{} -> [Str]
let dirFun = fun(obj: {}) -> [Str] do
  error("dirFun requires native function detection (funInfo try/catch or a predicate)")
end

# -----------------------------
# Strings: regex wrappers
# (Assuming native match/replace exist; don’t redefine them here.)
# -----------------------------

# If you *don’t* have native regex yet,
# uncomment these to fail fast with a helpful message:
# let match   = fun(_p: Str, _s: Str) -> [Str] do error("regex 'match' requires native support") end
# let replace = fun(_p: Str, _r: Str, _s: Str) -> Str do error("regex 'replace' requires native support") end

# -----------------------------
# Importing / Modules
# -----------------------------

# codeImport: curried wrapper over importCode(name, src)
# code:Str -> name:Str -> {}
let codeImport = fun(code: Str) -> Str -> {} do
  importCode(name, code)
end

# netImport: just use import() with a URL.
# url:Str -> {}
let netImport = fun(url: Str) -> {} do
  import(url)
end

# -----------------------------
# Networking
# -----------------------------

# www: GET a page, return body on 200 else null
# url:Str -> Str?
let www = fun(url: Str) -> Str? do
  # native http(params?, method?, url) -> {status:Int, headers:{}, body:Str}
  let r = http(null, "GET", url)
  if r.status == 200 then r.body else null end
end

# If you want a friendlier wrapper when params omitted:
# let httpGet = fun(url:Str) -> {} do http(null, "GET", url) end

# -----------------------------
# Casting & type checking
# (Builtins already: str, bool, int, num, typeOf, isType, isSubtype)
# -----------------------------

# -----------------------------
# Schemas / Grammars (placeholders)
# -----------------------------

let schema = fun(_t: Type) -> Str do
  error("schema(Type) not implemented in stdlib; provide a tiny native to render JSON Schema")
end

let bnf = fun(_t: Type) -> Str do
  error("bnf(Type) not implemented in stdlib; provide a tiny native to render BNF")
end

# -----------------------------
# Annotations
# -----------------------------

# getNote: use full docstring (help) — returns Str?
let getNote = fun(x: Any) -> Str? do
  help(x)
end

# setNote: needs a native to attach runtime annotations to values.
let setNote = fun(_x: Any, _ann: Str?) -> Any do
  error("setNote requires native support to attach annotations at runtime")
end

# -----------------------------
# Misc
# -----------------------------

# bindMethod: bind value as implicit first argument.
# value:Any -> func:(Any -> Any) -> Any -> Any
let bindMethod = fun(value: Any, f: (Any -> Any)) -> (Any -> Any) do
  fun(x: Any) -> Any do f(value, x) end
end

# uid: best-effort stable hash of a value’s printable form.
# (Pure MindScript fallback; collisions possible.)
# value:Any -> Int
let uid = fun(value: Any) -> Int do
  let s = str(value)
  let h = 5381
  for ch in split(s, "") do
    # crude: use string length as a surrogate for codepoint weight (no ord())
    h = ((h * 33) + size(ch))
  end
  h
end

# mute: swallow any value and return null.
# _:Any -> Null
let mute = fun(_: Any) -> Null do null end

# dump: pretty-print current env (returns null)
# _:Null -> Null
let dump = fun(_: Null) -> Null do
  print(getEnv())
  null
end

# Re-export dateNow directly so it’s visible from stdlib.
let dateNow = dateNow
