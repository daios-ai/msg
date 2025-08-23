let _failNull = fun(msg: Str) do
	# {msg}
	null
end
# --- state -------------------------------------------------------------------
# Create an orchestration state that tracks backends, their config, a selected
# backend, and an optional global system hint.
# _:Null -> {backends:{}, cfg:{}, selected:Str?, system:Str?}
let new = fun() do
	{backends: {}, cfg: {}, selected: null, system: null}
end
# Returns a function with the exact engine-required signature:
# __oracle_execute(prompt: Str, inType: Type, outType: Type, examples: [Any]) -> Str | Null
# state:{...} -> (Str -> Type -> Type -> [Any] -> Str?)
let dispatcher = fun(state) -> Str -> Type -> Type -> [Any] -> Str? do
	fun(prompt: Str, inT: Type, outT: Type, examples: [Any]) -> Str? do
		let sel = state.selected
		if sel == null then
			_failNull("no LLM backend selected (use llm.use)")
		elif not mapHas(state.backends, sel) then
			_failNull(sprintf("unknown backend: %s", [sel]))
		else
			let exec = state.backends[sel]
			# Executors accept an extra options map (may ignore it).
			let opts = {backend: sel, config: state.cfg[sel], system: state.system}
			exec(prompt, inT, outT, examples, opts)
		end
	end
end
# --- registry / selection / config ------------------------------------------
# Register a backend executor under a name.
# (state:{...}, name:Str, exec:(Str,Type,Type,[Any],{...})->Str?) -> Bool
let register = fun(state, name: Str, exec) -> Bool do
	if not __is_fun(exec) then
		_failNull("register expects a function")
	else
		state.backends[name] = exec
		if state.selected == null then
			state.selected = name
		end
		true
	end
end
# List all registered backend names.
# state:{...} -> [Str]
let list = fun(state) -> [Str] do
	let keys = []
	for let item in state.backends do
		keys = keys + [item[0]]
	end
	keys
end
# Select a backend by name.
# (state:{...}, name:Str) -> Str
let use = fun(state, name: Str) -> Str do
	if not mapHas(state.backends, name) then
		_failNull(sprintf("unknown backend: %s", [name]))
	else
		state.selected = name
		name
	end
end
# Get the current backend name.
# state:{...} -> Str?
let current = fun(state) -> Str? do
	state.selected
end
# Merge/extend backend config (API keys, baseURL, model, etc.)
# Uses native map merge `a + b` (shallow).
# (state:{...}, name:Str, cfg:{Str:Any}) -> {Str:Any}
let configure = fun(state, name: Str, cfg: {Str: Any}) -> {Str: Any} do
	let cur = state.cfg[name]
	state.cfg[name] = 	if cur == null then
		cfg
	else
		cur + cfg
	end
	if mapHas(cfg, "system") then
		state.system = cfg.system
	end
	state.cfg[name]
end
# --- oracle ergonomics -------------------------------------------------------
# info(f) -> { isOracle: Bool, params:[{name, type}], return:Type, doc:Str? }
let info = fun(f) do
	let meta = funInfo(f)
	let doc = help(f)
	{isOracle: bool(__is_fun(f)), 
				# best-effort; engine flag isn’t exposed
	params: meta.params, , return: meta.return, doc: doc}
end
# Construct a new oracle (doc/examples optional).
# params: [{name:Str, type:Type}]
# (params:Any, ret:Type, doc:Str?, examples:Any) -> Any
let makeOracle = fun(params, ret: Type, doc: Str?, examples) do
	let names = []
	let types = []
	for let p in params do
		names = names + [p.name]
		types = types + [p.type]
	end
	let funv = __make_fun(names, types, ret, 	type Any, true, examples)
	if doc == null then
		funv
	else
		__annotate(doc, funv)
	end
end
# Replace examples on an oracle/function.
# (f:Any, ex:Any) -> Any
let setExamples = fun(f, ex) do
	let meta = funInfo(f)
	let doc = help(f)
	makeOracle(meta.params, meta.return, doc, ex)
end
# Add a single [input, output] example (simple replacement behavior).
# (f:Any, input:Any, output:Any) -> Any
let addExample = fun(f, input, output) do
	let meta = funInfo(f)
	let doc = help(f)
	let ex = [[input, output]]
	makeOracle(meta.params, meta.return, doc, ex)
end
# Set/replace the docstring on an oracle/function.
# (f:Any, doc:Str) -> Any
let setDoc = fun(f, doc: Str) do
	let meta = funInfo(f)
	makeOracle(meta.params, meta.return, doc, [])
end
# Optional bridge to an engine-native "last prompt" (if exposed).
# _:Null -> Str?
let lastPrompt = fun() -> Str? do
	# Optional bridge to engine if exposed; null otherwise.
	null
end
