// cmd/lsp/main.go
//
// ROLE: Executable entrypoint and JSON-RPC dispatch loop.
//
// What lives here
//   • Process startup and server construction.
//   • Framed JSON-RPC read loop from stdin and write to stdout.
//   • Method routing: decode → switch on req.Method → delegate to server
//     handlers in features.go / core.go.
//   • Minimal lifecycle handling (initialize/shutdown/exit).
//
// What does NOT live here
//   • No language features, no text analysis, no diagnostics computation,
//     no document state. Keep this file small so it’s easy to test/replace
//     the transport without touching feature logic.
//
// Why this separation
//   • Clear boundary between transport concerns and language intelligence.
//   • Enables reuse of the server with different frontends/transports.

package main

import (
	"bufio"
	"encoding/json"
	"fmt"
	"io"
	"os"
)

func main() {
	s := newServer()
	in := bufio.NewReader(os.Stdin)

	for {
		msgBytes, err := readMsg(in)
		if err != nil {
			if err != io.EOF {
				// best-effort log to stderr
				fmt.Fprintln(os.Stderr, "read error:", err)
			}
			return
		}

		var req Request
		if err := json.Unmarshal(msgBytes, &req); err != nil {
			// Malformed JSON—ignore silently to be robust
			continue
		}

		switch req.Method {
		// LSP lifecycle
		case "initialize":
			s.onInitialize(req.ID, req.Params)
		case "initialized":
			// no-op
		case "shutdown":
			s.sendResponse(req.ID, nil, nil)
		case "exit":
			return

		// Text sync
		case "textDocument/didOpen":
			s.onDidOpen(req.Params)
		case "textDocument/didChange":
			s.onDidChange(req.Params)

		// Language features
		case "textDocument/hover":
			s.onHover(req.ID, req.Params)
		case "textDocument/definition":
			s.onDefinition(req.ID, req.Params)
		case "textDocument/completion":
			s.onCompletion(req.ID, req.Params)
		case "textDocument/documentSymbol":
			s.onDocumentSymbols(req.ID, req.Params)
		case "textDocument/references":
			s.onReferences(req.ID, req.Params)
		case "textDocument/signatureHelp":
			s.onSignatureHelp(req.ID, req.Params)
		case "textDocument/foldingRange":
			s.onFoldingRange(req.ID, req.Params)

		// Semantic tokens
		case "textDocument/semanticTokens/full":
			s.onSemanticTokensFull(req.ID, req.Params)
		case "textDocument/semanticTokens/range":
			s.onSemanticTokensRange(req.ID, req.Params)

		default:
			// For requests (have an id), reply with MethodNotFound; notifications are ignored.
			if len(req.ID) > 0 {
				s.sendResponse(req.ID, nil, &ResponseError{Code: -32601, Message: "method not found"})
			}
		}
	}
}
