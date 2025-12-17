## Installation Guide

This page covers installing the `msg` runtime (MindScript), making sure it’s on your `PATH`, upgrading/rolling back, uninstalling, and setting up oracle backends (LLM providers).

MindScript is split into:

* **The language** (your `.ms` scripts)
* **The runtime** (**`msg`**, which runs scripts and provides built-ins + the bundled standard library)
* **Oracle backends** (optional; only needed when scripts call **oracles**)

Installing MindScript gives you the interpreter and standard library; installing a backend gives you an LLM to execute oracle calls.

---

## Requirements

* **Linux** or **macOS**
* An LLM backend is **only required** if you run scripts that call **oracles**.
* Windows builds are not provided in the current release pipeline.

---

## Install MindScript

### Install the latest release

This is the default install path. It downloads the latest release bundle and installs it locally (no admin/root required):

```bash
curl -fsSL https://raw.githubusercontent.com/DAIOS-AI/msg/main/install.sh | bash
```

What you get:

* A self-contained installation in `~/.mindscript`
* `msg` (runtime) at `~/.mindscript/bin/msg`
* standard library modules (including `std.ms`, `llm.ms`) and any bundled native libs (`~/.mindscript/lib`)
* examples, with runnable scripts you can copy/adapt (`~/.mindscript/examples`)

### Verify your install

Check that `msg` is on your `PATH`:

```bash
msg
```
This should print something like:
```
MindScript 0.1.4 (built 2025-12-16T13:12:08Z)

Usage:
  msg run <file.ms> [--] [args...]         Run a script.
  msg repl                                 Start the REPL.
  msg fmt [--check] [path ...]             Format MindScript file(s) by path prefix
  msg test [path] [-p] [-v] [-t <ms>]      Run tests (default root=".")
  msg get <module>@<version?>              Install a third-party module (not implemented)
  msg version                              Print the compiled version
```

Run a bundled example:

```bash
msg run examples/hello.ms
```

If these work, the runtime and bundle discovery are correct.

### Upgrade to the latest release

Running the installer again is the normal way to upgrade.
```bash
curl -fsSL https://raw.githubusercontent.com/DAIOS-AI/msg/main/install.sh | bash
```

### Install a specific version

Pin a version or roll back:

```bash
VERSION=vX.Y.Z bash -c "$(curl -fsSL https://raw.githubusercontent.com/DAIOS-AI/msg/main/install.sh)"
```

Use tags like `v0.4.1`.

!!! info
    Pin a version for shared environments (CI, team scripts, teaching) so everyone gets the same runtime behavior.

### Offline / air-gapped install

If you can’t download during install time, install from already-downloaded artifacts.

Put these files in the same directory:

* `install.sh`
* `mindscript-<os>-<arch>.tar.gz`
* `mindscript-<os>-<arch>.tar.gz.sha256`

Then run the installer with the `--local` flag:

```bash
./install.sh --local .
```

This allows making installs reproducible, and it's friendly to locked-down machines where only vetted artifacts can be moved in.

### Uninstall

Uninstall removes the installation directory and cleans up the `PATH` entries the installer added:

```bash
~/.mindscript/uninstall.sh
```

If you installed from a repo checkout, `./uninstall.sh` performs the same cleanup.

---

## Oracle backends

MindScript’s oracles are executed by an LLM backend. Installing MindScript does **not** install a backend. This is intentional:

* It keeps the core runtime lightweight.
* It lets you choose between local and hosted providers.
* It avoids forcing a particular vendor or model.

### Supported backends (from `lib/llm.ms`)

The backend manager currently registers these backend names:

* `ollama`
* `openai` (Chat Completions)
* `openai-responses` (Responses API)
* `anthropic`
* `cohere`

Backend selection is configured **in your script** (not via `msg` flags).

### Choosing a backend: practical guidance

Choosing a backend is mostly a trade-off between **control** (local) and **convenience/quality ceiling** (hosted). If you want the fastest “it works” path, use a hosted provider. If you care most about keeping data local or avoiding per-call billing, use a self-hosted backend like Ollama.

| What matters most      | Self-hosted (Ollama)                      | Online provider (OpenAI & others) |
| ---------------------- | ----------------------------------------- | ------------------------------------------------ |
| Setup & onboarding     | Install server + download models          | Set API key and go                               |
| Cost                   | Hardware/electricity; no per-call billing | Pay per usage (plus plan)                        |
| Reliability & latency  | Depends on your machine                   | Depends on network + provider; usually stable    |
| Privacy & data control | Data stays local (in your environment)    | Data sent to provider (policy-dependent)         |
| Model quality ceiling  | Limited by what you can run locally       | Access to higher-end models                      |

### Ollama

[Ollama](https://ollama.com) is a local model server. MindScript calls it over HTTP.

1. Install [Ollama](https://ollama.com/) (version 0.5 or higher).
2. Pull a model you want to use (example: `ollama pull llama3.2:3b`).
3. Ensure the server is running:
```bash
ollama list
```

By default, the `ollama` backend uses `http://localhost:11434`, but you can change it.

!!! tip
    If you're new: start with a smaller model first. Your goal is to confirm the full oracle pipeline works end-to-end; you can upgrade models later. If oracle calls fail with “model not set”: that means MindScript can reach Ollama, but you didn’t configure which model to use in the script. If oracle calls fail with “request failed” or HTTP errors: confirm Ollama is running and reachable at the base URL. If you changed the host/port, set `baseUrl` via `llm.setConfig(...)`.

### OpenAI/Anthropic/Cohere

The safe way is to set your API key in your environment.

1. Create an API key in your OpenAI/Anthropic/Cohere account.
2. Export it (choose the appropriate):
```bash
export OPENAI_API_KEY="YOUR_OPENAI_KEY_HERE"
export ANTHROPIC_API_KEY="YOUR_ANTHROPIC_KEY_HERE"
export COHERE_API_KEY="YOUR_COHERE_KEY_HERE"
```
3. Put it in your shell startup file if you want it to persist (e.g., `~/.zshrc`, `~/.bashrc`).

!!! warning
    Don’t hardcode keys in scripts. Environment variables are the simplest safe default. If you see a diagnostic like “missing API key”, that means the backend is selected but not authenticated. 


### Selecting a backend and model in your script

MindScript fires up with a preloaded LLM backend library, called `llm`. 

At the top of your script (before any oracle call) write:

```mindscript
let llm = import("llm")

llm.useBackend("ollama")
llm.useModel("llama3.2:3b")  # example model name
```

or any other backend or model. You can check which backends and models are available using the `llm.backends` and `llm.models` functions.
```mindscript
==> llm.backends()
["ollama", "openai", "openai-responses", "anthropic", "cohere"]

==> llm.models()
["qwen2.5-coder:1.5b", "qwen2.5:1.5b"]
```

If you forget to set a model, you will get an error when trying to execute an oracle.

### Setting base URL and generation options

You can update backend configuration without touching environment variables:

```mindscript
llm.useBackend("ollama")
llm.setConfig({
	baseUrl: "http://localhost:11434",
	options: {
		temperature: 0.2,
		top_p: 0.9
	}
})
llm.useModel("llama3.2:3b")
```
Common option keys (backend-dependent): `temperature`, `top_p`, `top_k`, `max_tokens`, `presence_penalty`, `frequency_penalty`.

!!! tip 
    Start with conservative settings (low temperature) while you're learning. It reduces variability and makes debugging type/schema issues easier.


### Probing backends and diagnosing setup

The `llm` module can probe all backends and print a status line for each:

```mindscript
llm.init()
```

This is useful when you’re not sure what’s “wrong”:

* If the server is down, you’ll see request failures.
* If a key is missing, you’ll see “missing API key”.
* If multiple backends are available, it can auto-select the first working one if the current backend isn’t OK.

To inspect the currently selected backend and its configuration:

```mindscript
==> llm.status()
{backend: "ollama", model: "qwen2.5:1.5b", authed: true, options: {}}

==> llm.getConfig()
{
	backend: "ollama",
	baseUrl: "http://localhost:11434",
	model: "qwen2.5:1.5b",
	options: {}
}
```

### Quick oracle health check

The standard library includes a minimal real oracle ping:

```mindscript
==> oracleStatus()
"oracle: installed"

==> oracleHealth()
{ok: true, ms: 12993}

```

Expected outcomes:
* Working: `{ok: true, ms: ...}`
* Not configured: `null` with an error message (missing key, model not set, request failed, etc.)

!!! tip

    If you're writing scripts that others will run, put your backend selection(and model) at the top of the script or in a small shared module. That way you can switch environments (OpenAI ↔ Ollama) without editing oracle code

