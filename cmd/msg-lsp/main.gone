// main.go
//
// ROLE: Executable entrypoint and JSON-RPC dispatch loop, plus transport
//       helpers (framed stdio) and small server send/notify wrappers.
//
// What lives here
//   • Process startup and server construction.
//   • Framed JSON-RPC read loop from stdin and write to stdout.
//   • Method routing: decode → switch on req.Method → delegate to server
//     handlers in features.go / analysis.go.
//   • Minimal lifecycle handling (initialize/shutdown/exit).
//   • Transport helpers: Content-Length framing, sendResponse/notify.
//
// What does NOT live here
//   • No language features (hover/definition/etc.) — see features.go.
//   • No analysis or text math — see analysis.go.
//   • No server/doc structs — see state.go.

package main

import (
	"bufio"
	"bytes"
	"encoding/json"
	"fmt"
	"io"
	"os"
	"strings"
)

////////////////////////////////////////////////////////////////////////////////
// Transport (stdio framing) + send/notify
////////////////////////////////////////////////////////////////////////////////

var stdoutSink io.Writer = os.Stdout

func init() {
	// Silence unsolicited output during `go test` unless opted in.
	if strings.HasSuffix(os.Args[0], ".test") && os.Getenv("LSP_STDOUT") == "" {
		stdoutSink = io.Discard
	}
}

// readMsg reads a single JSON-RPC message body using LSP's Content-Length framing.
func readMsg(r *bufio.Reader) ([]byte, error) {
	var contentLen int
	for {
		line, err := r.ReadString('\n')
		if err != nil {
			return nil, err
		}
		line = strings.TrimRight(line, "\r\n")
		if line == "" {
			break
		}
		if i := strings.IndexByte(line, ':'); i >= 0 {
			key := strings.ToLower(strings.TrimSpace(line[:i]))
			val := strings.TrimSpace(line[i+1:])
			if key == "content-length" {
				_, _ = fmt.Sscanf(val, "%d", &contentLen)
			}
		}
	}
	if contentLen <= 0 {
		return nil, io.EOF
	}
	buf := make([]byte, contentLen)
	_, err := io.ReadFull(r, buf)
	return buf, err
}

// writeMsg writes a JSON-RPC payload with Content-Length framing.
func writeMsg(w io.Writer, v any) error {
	body, err := json.Marshal(v)
	if err != nil {
		return err
	}
	var b bytes.Buffer
	fmt.Fprintf(&b, "Content-Length: %d\r\n\r\n", len(body))
	b.Write(body)
	_, err = w.Write(b.Bytes())
	return err
}

// sendResponse sends a JSON-RPC response (result or error).
func (s *server) sendResponse(id json.RawMessage, result any, respErr *ResponseError) {
	// Always serialize result as raw JSON so decoders can test for literal null, etc.
	var rm json.RawMessage
	if respErr == nil && result == nil {
		rm = json.RawMessage([]byte("null"))
	} else if result != nil {
		if b, err := json.Marshal(result); err == nil {
			rm = json.RawMessage(b)
		}
	}
	_ = writeMsg(stdoutSink, Response{JSONRPC: "2.0", ID: id, Result: rm, Error: respErr})
}

// notify sends a JSON-RPC notification (no id).
func (s *server) notify(method string, params any) {
	_ = writeMsg(stdoutSink, map[string]any{
		"jsonrpc": "2.0",
		"method":  method,
		"params":  params,
	})
}

////////////////////////////////////////////////////////////////////////////////
// Entry point & dispatcher
////////////////////////////////////////////////////////////////////////////////

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
		case "textDocument/typeDefinition":
			s.onTypeDefinition(req.ID, req.Params)
		case "textDocument/formatting":
			s.onDocumentFormatting(req.ID, req.Params)
		case "textDocument/prepareRename":
			s.onPrepareRename(req.ID, req.Params)
		case "textDocument/rename":
			s.onRename(req.ID, req.Params)

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
