
# `llm` — configuring the oracle backend

The `llm` module is a small manager for language-model backends. It gives you one place to choose which provider you use, which model to talk to, and which sampling options to apply.

MindScript oracles do not talk to providers directly. When you call an `oracle(...)` value, the runtime builds a structured prompt using its types and annotations, then sends that prompt through a single global hook:

```mindscript
__oracle_execute(prompt: Str) -> Str?
```

At startup the prelude imports `llm` and installs `llm.exec` into that hook. In practice this means two things:

* every `oracle(...)` call in your program ends up calling `llm.exec` under the hood, and
* whatever backend and model you configure through `llm` controls how both oracles and direct `llm.exec` calls behave.

The rest of this manual shows how to work with that.

---

## 1. What you get (and what you don’t)

The `llm` module gives you a single, pluggable executor for all oracles, with a small set of backends and a predictable soft-error model.

You get a registry of backends such as `ollama`, `openai`, `openai-responses`, `anthropic`, and `cohere`, each with its own `baseUrl`, `model`, and options. You get per-backend authentication, a way to list models when supported, and a single entry point `exec(prompt: Str) -> Str?` that the oracle runtime uses.

You do not get anything that changes provider semantics. `llm` does not retry, rate-limit, or rebox responses. It does not inspect or normalize provider payloads beyond basic error handling. It also does not support per-oracle backend overrides; everything shares the same executor unless you install a custom one.

---

## 2. First contact: a minimal setup

You never need to import `llm` yourself; it is loaded by the prelude. A common pattern is to run its startup probe early in your program to see what is available:

```mindscript
let info = llm.init()

println(info.backends)
println("current: " + info.current)
```

This prints one diagnostic line per backend (for example, `llm: ollama ✓ model=null @ http://localhost:11434` or `llm: openai ✗ model=null @ https://api.openai.com/v1 — missing API key`), then returns a small summary with the list of backend names and the current choice. If the current backend is not healthy but another one is, `init` switches to the first healthy backend and prints a note.

For a quick local test with Ollama you might write:

```mindscript
let _ = llm.useBackend("ollama")
let _ = llm.useModel("Phi3:latest")

let out = llm.exec("Say hello in one short sentence.")

if out == null then
	println("llm error: " + noteGet(out))
else
	println(out)
end
```

Here `exec` returns either a string from the provider or `null` with an annotation explaining what went wrong, such as `llm: ollama: model not set` or an HTTP error.

Hosted providers also need API keys. If you set them in the environment, `llm` picks them up at startup:

* `OPENAI_API_KEY` for the `openai` and `openai-responses` backends
* `ANTHROPIC_API_KEY` for the `anthropic` backend
* `COHERE_API_KEY` for the `cohere` backend

You can also set or override them at runtime with `auth`:

```mindscript
let _ = llm.useBackend("openai")
let _ = llm.auth({apiKey: "sk-..."})
let _ = llm.useModel("gpt-4.1-mini")

let out = llm.exec("Return {\"ok\": true} as compact JSON.")

if out == null then
	println("OpenAI call failed: " + noteGet(out))
else
	println(out)
end
```

Once this works, your oracles are also using the same backend and model, because `__oracle_execute` still points at `llm.exec`.

---

## 3. How `llm` and oracles fit together

Consider an oracle:

```mindscript
# Return a short greeting for a given name.
# name: person’s name
let greet = oracle(name: Str) -> Str
```

and a call:

```mindscript
let s = greet("Ada")
```

When you write this, the runtime:

1. builds a prompt from the oracle’s types, annotations, and arguments,
2. calls `__oracle_execute(prompt)`, and
3. expects a JSON string encoding some `{output: x}` that it can turn back into a MindScript value.

Because the prelude has installed `llm.exec` into `__oracle_execute`, step 2 is effectively:

```mindscript
let out = llm.exec(prompt)
```

So oracles use whatever backend and model you have configured in `llm`. Changing the backend or model moves all oracles in one place:

```mindscript
llm.useBackend("ollama")
llm.useModel("Phi3:latest")
```

After this, both:

```mindscript
let raw = llm.exec("Explain quines in one sentence.")
let answer = greet("Ada")
```

go through Ollama instead of OpenAI, with the same timeout and options.

For a quick sanity check at the oracle layer, you can use the standard helpers:

```mindscript
println(oracleStatus())

let h = oracleHealth()
if h == null then
	println("health check failed: " + noteGet(h))
else
	printf("ok, latency ~%dms\n", [h.ms])
end
```

These functions sit above whatever executor is installed (normally `llm.exec`) and help you confirm that the full oracle path is working, not just the backend HTTP calls.

---

## 4. Backends, status, and switching

The module keeps a registry of named backends. Today these include:

* `ollama` for a local Ollama HTTP server
* `openai` for the Chat Completions API
* `openai-responses` for the Responses API
* `anthropic` for Anthropic’s Messages API
* `cohere` for Cohere’s Chat API

You can inspect the registry with:

```mindscript
let xs = llm.backends()
println(xs)
```

Each backend has its own internal state. The current choice is exposed by `status`:

```mindscript
let s = llm.status()

println("backend = " + s.backend)
println("model   = " + str(s.model))
println("authed  = " + str(s.authed))
println("options = " + str(s.options))
```

The `authed` flag tells you whether this backend has the credentials it needs. For local backends like `ollama` it is always true. For hosted ones it is true if and only if an API key is present in the backend’s state.

To switch backends you use `useBackend`:

```mindscript
let orig = llm.status().backend

let _ = llm.useBackend("openai")
println(llm.status().backend)  # "openai"

let _2 = llm.useBackend(orig)
println(llm.status().backend)  # back to original
```

If you pass an unknown name, `useBackend` returns `null` and leaves the current backend unchanged, so you can probe and fall back without changing anything:

```mindscript
let before = llm.status().backend
let r = llm.useBackend("does-not-exist")

if r == null then
	println("no such backend, staying on " + before)
end
```

Models are handled similarly. Once you have selected a backend, you can ask it for available models with `models` and pick one with `useModel`:

```mindscript
let _ = llm.useBackend("ollama")

let ms = llm.models()
if ms != null then
	println("available models:")
	for m in ms do
		println("  " + m)
	end
end

let _2 = llm.useModel("Phi3:latest")
println(llm.status().model)  # "Phi3:latest"
```

Not all backends can list models. When listing is not supported or fails (for example because a daemon is down or an API key is missing), `models` returns `null` with an annotation explaining the reason.

The two OpenAI backends exist so you can choose between APIs:

* `openai` speaks the Chat Completions endpoint and is a good fit when you want a general-purpose assistant style interface.
* `openai-responses` uses the Responses API with `text.format.type = "json_object"`, which pushes the provider harder towards valid JSON and can make oracles more robust.

You select between them like any other backend.

---

## 5. Configuration and options

Each backend maintains a configuration record with:

* a `baseUrl` such as `http://localhost:11434` or `https://api.openai.com/v1`,
* the active `model`, and
* an `options` map with generation settings such as `temperature`, `top_p`, `top_k`, `max_tokens`, and similar knobs (the exact set varies by backend).

Some backends also track a `timeoutMs` for HTTP calls.

You can inspect this record for the current backend using `getConfig`:

```mindscript
let cfg = llm.getConfig()

println("backend = " + cfg.backend)
println("baseUrl = " + str(cfg.baseUrl))
println("model   = " + str(cfg.model))
println("options = " + str(cfg.options))
if mapHas(cfg, "timeoutMs") and cfg.timeoutMs != null then
	println("timeout = " + str(cfg.timeoutMs))
end
```

To change settings, call `setConfig` with a map. Recognized top-level keys are `baseUrl`, `model`, `options`, and `timeoutMs`. Any other keys are ignored:

```mindscript
let _ = llm.useBackend("openai")

let _2 = llm.setConfig({
	model: "gpt-4.1-mini",
	options: {temperature: 0.0, max_tokens: 128},
	timeoutMs: 5000
})

let after = llm.getConfig()
println(after.model)                 # "gpt-4.1-mini"
println(after.options.temperature)   # 0.0
println(after.options.max_tokens)    # 128
println(after.timeoutMs)             # 5000
```

The `options` map is replaced wholesale when you set it. Keys that are not present in the new map disappear instead of lingering. This makes it easy to switch to a clean configuration instead of accumulating stale options from earlier experiments.

You can clear the model by setting it to `null`:

```mindscript
let _ = llm.setConfig({model: null})
println(llm.status().model)  # null
```

All these settings affect both direct `llm.exec` calls and all oracle calls, because they share the same executor.

---

## 6. Authentication and per-backend state

Hosted backends need credentials. The `auth` function updates the current backend’s credentials and related fields. For example, to configure OpenAI:

```mindscript
let _ = llm.useBackend("openai")

let s0 = llm.status()
println(s0.authed)  # likely false if no key

let _2 = llm.auth({apiKey: "sk-..."})
let s1 = llm.status()
println(s1.authed)  # now true
```

Setting `apiKey` to `null` clears it and makes `authed` false again:

```mindscript
let _ = llm.auth({apiKey: null})
println(llm.status().authed)  # false
```

Credentials are stored per backend. If you authenticate one backend and then switch to another, the second one starts unauthenticated until you configure it:

```mindscript
let _ = llm.useBackend("openai")
let _2 = llm.auth({apiKey: "sk-openai"})
println(llm.status().authed)  # true

let _3 = llm.useBackend("cohere")
println(llm.status().authed)  # false

let _4 = llm.auth({apiKey: "sk-cohere"})
println(llm.status().authed)  # true for Cohere

let _5 = llm.useBackend("openai")
println(llm.status().authed)  # still true; OpenAI’s key was preserved
```

Any extra fields you pass to `auth` are shallow-merged into the backend’s state. For example, you can store an OpenAI organization id:

```mindscript
let _ = llm.auth({organization: "org-123"})
```

This does not change `authed` on its own; only the presence of `apiKey` controls that flag.

Because oracles rely on `llm.exec`, authentication problems show up as oracle failures too. If an oracle starts returning `null` with a message such as `llm: openai: missing API key (llm.auth)`, it is usually enough to fix the key with `llm.auth` or the appropriate environment variable.

---

## 7. Error handling and expectations

The key operations that can fail in normal use are `llm.exec(prompt)` and `llm.models()`. They follow the usual MindScript soft-error pattern:

* On success, they return a regular value, a `Str` for `exec` and `[Str]` for `models`.
* On failure, they return `null` with an annotation explaining what went wrong.

Typical failure reasons include missing API keys, missing models, network timeouts, non-200 HTTP status codes, and unexpected provider response shapes. Error messages include a backend prefix, which makes it easier to see where a problem comes from.

You should handle these results the same way you handle other nullable values:

```mindscript
let _ = llm.useBackend("openai")
let _2 = llm.auth({apiKey: null})  # force a bad configuration

let out = llm.exec("test")
if out == null then
	println("exec failed: " + noteGet(out))
end
```

Because the prelude has installed `llm.exec` as the oracle executor, you see similar messages when you call oracles. A failing oracle is often a signal that something is wrong with the backend configuration rather than with the oracle definition itself. A small `llm.exec("ping")` call and `oracleHealth()` are useful first checks.

---

## 8. Common pitfalls and how to avoid them

A few problems come up often when first working with `llm`:

If `llm.exec` and all oracles return `null` with a message about missing API keys, you either forgot to set the environment variables or you cleared the key with `auth`. Fix this with `llm.auth({apiKey: "..."})` on the appropriate backend or by exporting the right `*_API_KEY` variable before you start the program.

If you see messages like `llm: ollama: model not set (use llm.useModel)`, you have chosen a backend but never picked a model. Call `useModel` after `useBackend`, or embed the model name in a `setConfig` call.

If `llm.models()` returns `null` with an error message, the backend may not support model listing, the daemon may not be running (for Ollama), or the provider may be rejecting your request due to missing or invalid credentials.

If a configuration change makes previously working oracles fail, check `llm.status()`, `llm.getConfig()`, and `oracleStatus()`. It is easy to accidentally switch to a different backend, clear a model, or overwrite options with incompatible values.

---

## 9. Reference

This section summarizes the public surface of the `llm` module. Types use the usual MindScript notation.

```mindscript
# Probe all backends, print diagnostics, and ensure a usable current backend.
llm.init(_: Null) -> { backends!: [Str], current!: Str }

# List registered backend names.
llm.backends(_: Null) -> [Str]

# Report current backend status without network calls.
llm.status(_: Null) -> {
	backend!: Str,
	model: Str?,
	authed!: Bool,
	options!: {}
}

# Switch the current backend by name; returns new status or null on error.
llm.useBackend(name: Str) -> {}?

# List models for the current backend when supported; soft-null on error.
llm.models(_: Null) -> [Str]?

# Set the active model for the current backend; returns new status.
llm.useModel(name: Str) -> {}?

# Get an editable config snapshot for the current backend.
llm.getConfig(_: Null) -> {
	backend!: Str,
	baseUrl: Str?,
	model: Str?,
	options!: {},
	timeoutMs: Int?
}

# Shallow-merge config into the current backend’s state.
llm.setConfig(cfg: {}) -> {}?

# Store credentials for the current backend (typically {apiKey: Str?}).
llm.auth(creds: {}) -> {}?

# Execute the current backend with a prompt; returns provider raw string or null.
llm.exec(prompt: Str) -> Str?
```
