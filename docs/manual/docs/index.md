<img src="/assets/mindscript-logo-lambda-psi.svg" width="500px">

A programming language for **semantic computation** with **oracles** that let you call LLMs safely from real code.

&copy; 2024–2026 [Daios Technologies Ltd](https://www.daios.ai)

**Read the Docs:** [https://mindscript.daios.ai](https://mindscript.daios.ai)  
**Try in Browser:** [https://www.daios.ai/playground](https://www.daios.ai/playground)  
**Source code:** [https://github.com/daios-ai/msg](https://github.com/daios-ai/msg)

![preview code example](/assets/mindscript-demo.png)

---

## What is MindScript?

MindScript combines a small, practical scripting language with **oracles**: typed, schema-gated functions executed by an LLM.
You write normal code for deterministic logic, and use oracles when the step is inherently semantic (extracting meaning, labeling, rewriting, summarizing, matching, etc.).


## A tiny example

```mindscript
==> # Examples of translating a number into English.
... let examples = [[12, "twelve"], [21, "twenty-one"], [8, "eight"]]
...
... # Say the number in English.
... let sayNumber = oracle(number: Int) -> Str from examples
...
... sayNumber(1024)

"one thousand twenty-four"
```
Oracles are **typed**: inputs and outputs are validated at runtime, so you can safely compose semantic steps with deterministic code.

### Why you'd use it

- **Semantic transforms as a first-class tool** (not a bolted-on API call)
- **Guardrails by default**: runtime-checked types/schemas around LLM outputs
- **Practical scripting workflow**: run scripts and pipelines with the `msg` runtime
- **Works great with JSON** (for data interchange), but shines when the work is *meaning*, not formatting

### Applications

* Extract structured, **typed JSON** from messy text (emails, chats, notes)
* Classify, label, and route items using semantics (“is this a bug report?”)
* Normalize and deduplicate real-world data (names, addresses, products)
* Enrich datasets with summaries, tags, and entities—then post-process deterministically
* Build **semantic web** pipelines: map unstructured content into entities/relations, align to vocabularies, and emit JSON/linked-data shapes

## Installation

Download the installer and run it:
```bash
wget https://raw.githubusercontent.com/daios-ai/msg/refs/heads/main/install.sh
chmod +x install.sh
./install.sh
```
This will download the latest version and install it locally (in `~/.mindscript`). 

You can test it by checking the version:
```bash
msg version
```

Want to use oracles? Follow the installation guide to configure an LLM backend.

Start with **Quickstart and Installation**, then the **5-minute language tour**.


## Requirements

- Linux or macOS
- An LLM backend **only** if you run oracle calls.
