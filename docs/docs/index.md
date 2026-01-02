<img src="https://raw.githubusercontent.com/daios-ai/msg/refs/heads/main/docs/docs/assets/mindscript-logo-lambda-psi.svg" width="500px">

A programming language for **semantic computation** with **oracles** that let you call LLMs safely from real code.

&copy; 2024–2026 [Daios Technologies Ltd](https://www.daios.ai)

**Read the Docs:** [https://mindscript.daios.ai](https://mindscript.daios.ai)  
**Try in Browser:** [https://www.daios.ai/playground](https://www.daios.ai/playground)  
**Source code:** [https://github.com/daios-ai/msg](https://github.com/daios-ai/msg)

![preview code example](https://raw.githubusercontent.com/daios-ai/msg/5171a849ac91f8eac5ffe2847eb9658b5af5b780/docs/docs/assets/mindscript-demo.png)

---

## What is MindScript?

MindScript combines a small, practical scripting language with **oracles**: typed, schema-gated functions executed by an LLM.
You write normal code for deterministic logic, and use oracles when the step is inherently semantic (extracting meaning, labeling, rewriting, summarizing, matching, etc.).


## A tiny example

Define an oracle:

```mindscript
# Examples of translating a number into English.
let examples = [[12, "twelve"], [21, "twenty-one"], [8, "eight"]]

# Say the number in English.
let sayNumber = oracle(number: Int) -> Str from examples
```

Then, invoke it:

```mindscript-repl
==> sayNumber(1024)
"one thousand twenty-four"
```

Oracles are **typed**: inputs and outputs are validated at runtime, so you can safely compose semantic steps with deterministic code.

### Why you'd use it

- **Semantic transforms as a first-class tool** (not a bolted-on API call)
- **Guardrails by default**: runtime-checked types/schemas around LLM outputs
- **Practical scripting workflow**: run scripts and pipelines with the `msg` runtime
- **Works great with JSON** (for data interchange), but shines when the work is *meaning*

### Applications

* Extract structured, **typed JSON** from messy text (emails, chats, notes), and use this to classify, label, and route items using their **meaning**
* Build **semantic** pipelines: map unstructured content into entities/relations, align to vocabularies, and emit JSON/linked-data shapes
* Build **agentic applications** that can reason and have autonomy

## Installation

Download the installer and run it:
```bash
curl -fsSL https://raw.githubusercontent.com/daios-ai/msg/main/install.sh | bash
```
This will download the latest version and install it locally (in `~/.mindscript`). 

Once installed, you can execute the runtime by running the `msg` command:
```bash
$ msg
MindScript 0.1.4 (built 2025-12-16T13:12:08Z)

Usage:
  msg run <file.ms> [--] [args...]         Run a script.
  msg repl                                 Start the REPL.
  msg fmt [--check] [path ...]             Format MindScript file(s) by path prefix
  msg test [path] [-p] [-v] [-t <ms>]      Run tests (default root=".")
  msg get <module>@<version?>              Install a third-party module (not implemented)
  msg version                              Print the compiled version
```

For more installation options and configuring the LLM backend, check the **[Installation Guide](https://mindscript.daios.ai/installation/)**.

## Requirements

- **Linux**, **macOS**, or **Raspberry OS**.
- An LLM backend **only** if you run oracle calls.

## Features

You’re right. Here’s the same list with only the bracketed items filled in and minimal wording changes elsewhere.

## Features

* **LLM oracles as a language feature**: declare LLM-backed oracle functions with typed inputs and validated JSON outputs.
* **Multi-provider LLM support**: supports Ollama, OpenAI (Chat Completions), OpenAI Responses, Anthropic, and Cohere backends.
* **Comments as runtime data**: comments attach as documentation and hints to oracle execution.
* **Errors as values**: No exceptions that hide execution flow.
* **Dynamic, runtime-checked types**: Function parameter and return types are enforced at runtime.
* **First-class modules**: imports modules from the local filesystem and from HTTP(S) URLs.
* **Practical CLI tooling**: the CLI provides script execution, an interactive REPL, formatting, and test discovery and execution.
* **Batteries-included standard library**: Includes iterators and functional helpers, JSON and schema bridges, filesystem and networking utilities, crypto, and process execution.
* **Concurrency and communication primitives**: Processes run in isolated interpreter instances that are scheduled like Go goroutines rather than an async/await model, with channels, timers, tickers, and actors for coordination.
* **Metaprogramming**: programs can be parsed into a canonical JSON-compatible S-expression form, transformed at runtime, and evaluated again via reification.
* **Foreign function interface**: on supported builds, loads native libraries and exposes functions, variables, and callbacks to extend MindScript with C APIs.
* **Built-in testing framework**: discovers test files, supports assertions and snapshot testing, runs tests in parallel, and reports failures with file and line locations.
