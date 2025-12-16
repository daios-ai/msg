## Prerequisites

MindScript requires access to an LLM, either local or remote.
Right now it supports the following backends:

- [llama.cpp](https://github.com/ggerganov/llama.cpp/),
- [Ollama](https://ollama.com/) (version 0.5 or higher),
- and [OpenAI's API](https://platform.openai.com/docs/overview).

### With llama.cpp or Ollama

Follow the instructions to install the backend, download your favorite model,
and launch a server from either [llama.cpp](https://github.com/ggerganov/llama.cpp/)
or [Ollama](https://ollama.com/).

### With OpenAI API

Set the OpenAI API key as an environment variable:
```
export OPENAI_API_KEY=[YOUR API KEY]
```

## Installing MindScript

Install MindScript using pip:
```
pip install mindscript
```

## Running MindScript

To run the REPL, for example with Ollama using Phi4 as an LLM backend, enter
```
mindscript -b ollama -m phi4
```

<img src="/assets/manual/repl.png">

To run a program `myprogram.ms` with e.g. Llama.cpp, enter
```
mindscript myprogram.ms -b llamacpp
```

If you need help or to get a description of all the command line options, use
```
mindscript -h
```
