## llm.ms — LLM orchestration & oracle ergonomics
##
## Main wiring:
##   let llm = import("llm.ms")
##   let st = llm.new()
##   __oracle_execute = llm.dispatcher(st)   ## set in MAIN
##   let be = import("backends.ms")
##   llm.register(st, "ollama", be.makeOllama())
##   llm.use(st, "ollama")
##   llm.configure(st, "ollama", { baseURL: "http://localhost:11434", model: "llama3.1" })

let _failNull = fun(msg: Str) do
  # {msg}
  null
end

# --- state -------------------------------------------------------------------

# Create an orchestration state that tracks backends, their config, a selected
# backend, and an optional global system hint.
# _:Null -> {backends:{}, cfg:{}, selected:Str?, system:Str?}
let new = fun() do
  {
    backends: {},        # name -> execFun(prompt,inT,outT,ex,opts) -> Str|Null
    cfg: {},             # name -> {Str:Any}
    selected: null,      # active backend name
    system: null         # optional global system hint
  } 
end

# Returns a function with the exact engine-required signature:
#   __oracle_execute(prompt: Str, inType: Type, outType: Type, examples: [Any]) -> Str | Null
# state:{...} -> (Str -> Type -> Type -> [Any] -> Str?)
let dispatcher = fun(state: Any) -> (Str -> Type -> Type -> [Any] -> Str?) do
  fun(prompt: Str, inT: Type, outT: Type, examples: [Any]) -> Str? do
    let sel = state.selected
    if sel == null then
      _failNull("no LLM backend selected (use llm.use)")
    elif not mapHas(state.backends, sel) then
      _failNull(sprintf("unknown backend: %s", [sel]))
    else
      let exec = state.backends[sel]
      # Executors accept an extra options map (may ignore it).
      let opts = { backend: sel, config: state.cfg[sel], system: state.system }
      exec(prompt, inT, outT, examples, opts)
    end
  end
end

# --- registry / selection / config ------------------------------------------

# Register a backend executor under a name.
# (state:{...}, name:Str, exec:(Str,Type,Type,[Any],{...})->Str?) -> Bool
let register = fun(state: Any, name: Str, exec: Any) -> Bool do
  if not __is_fun(exec) then
    _failNull("register expects a function")
  else
    state.backends[name] = exec
    if state.selected == null then state.selected = name end
    true
  end
end

# List all registered backend names.
# state:{...} -> [Str]
let list = fun(state: Any) -> [Str] do
  let keys = []
  for item in state.backends do keys = keys + [item.0] end
  keys
end

# Select a backend by name.
# (state:{...}, name:Str) -> Str
let use = fun(state: Any, name: Str) -> Str do
  if not mapHas(state.backends, name) then
    _failNull(sprintf("unknown backend: %s", [name]))
  else
    state.selected = name
    name
  end
end

# Get the current backend name.
# state:{...} -> Str?
let current = fun(state: Any) -> Str? do state.selected end

# Merge/extend backend config (API keys, baseURL, model, etc.)
# Uses native map merge `a + b` (shallow).
# (state:{...}, name:Str, cfg:{Str:Any}) -> {Str:Any}
let configure = fun(state: Any, name: Str, cfg: {Str: Any}) -> {Str: Any} do
  let cur = state.cfg[name]
  state.cfg[name] =
    if cur == null then cfg else cur + cfg end
  if mapHas(cfg, "system") then state.system = cfg.system end
  state.cfg[name]
end

# --- oracle ergonomics -------------------------------------------------------

# info(f) -> { isOracle: Bool, params:[{name, type}], return:Type, doc:Str? }
let info = fun(f: Any) -> Any do
  let meta = funInfo(f)
  let doc = help(f)
  {
    isOracle: bool(__is_fun(f)),   # best-effort; engine flag isn’t exposed
    params: meta.params,
    return: meta.return,
    doc: doc
  }
end

# Construct a new oracle (doc/examples optional).
# params: [{name:Str, type:Type}]
# (params:Any, ret:Type, doc:Str?, examples:Any) -> Any
let makeOracle = fun(params: Any, ret: Type, doc: Str?, examples: Any) -> Any do
  let names = []
  let types = []
  for let p in params do
    names = names + [p.name]
    types = types + [p.type]
  end
  let funv = __make_fun(names, types, ret, type Any, true, examples)
  if doc == null then funv else __annotate(doc, funv) end
end

# Replace examples on an oracle/function.
# (f:Any, ex:Any) -> Any
let setExamples = fun(f: Any, ex: Any) -> Any do
  let meta = funInfo(f)
  let doc = help(f)
  makeOracle(meta.params, meta.return, doc, ex)
end

# Add a single [input, output] example (simple replacement behavior).
# (f:Any, input:Any, output:Any) -> Any
let addExample = fun(f: Any, input: Any, output: Any) -> Any do
  let meta = funInfo(f)
  let doc = help(f)
  let ex = [[input, output]]
  makeOracle(meta.params, meta.return, doc, ex)
end

# Set/replace the docstring on an oracle/function.
# (f:Any, doc:Str) -> Any
let setDoc = fun(f: Any, doc: Str) -> Any do
  let meta = funInfo(f)
  makeOracle(meta.params, meta.return, doc, [])
end

# Optional bridge to an engine-native "last prompt" (if exposed).
# _:Null -> Str?
let lastPrompt = fun() -> Str? do
  # Optional bridge to engine if exposed; null otherwise.
  null
end
