// cmd/lsp/protocol.go
//
// ROLE: Pure wire schema for JSON-RPC 2.0 and the Language Server Protocol (LSP).
//
// What lives here
//   • Go structs that mirror the on-the-wire request/response envelopes and
//     LSP payload types (positions, ranges, diagnostics, hovers, completion,
//     semantic tokens, folding, signature help, etc.).
//
// What does NOT live here
//   • No business logic, no transport framing, no feature handlers, no
//     server state. This file must remain DTOs only so the rest of the
//     codebase can import it without pulling any behavior.
//
// Why this separation
//   • Keeps (de)serialization types stable and dependency-free.
//   • Makes it easy to audit protocol changes and keep handlers decoupled.
//
// Dependencies: none (stdlib only).

package main

import "encoding/json"

////////////////////////////////////////////////////////////////////////////////
// LSP protocol types (wire structs)
////////////////////////////////////////////////////////////////////////////////

// ----- JSON-RPC envelope -----

type Request struct {
	JSONRPC string          `json:"jsonrpc"`
	ID      json.RawMessage `json:"id,omitempty"`
	Method  string          `json:"method"`
	Params  json.RawMessage `json:"params,omitempty"`
}

type Response struct {
	JSONRPC string          `json:"jsonrpc"`
	ID      json.RawMessage `json:"id,omitempty"`
	Result  any             `json:"result,omitempty"`
	Error   *ResponseError  `json:"error,omitempty"`
}

type ResponseError struct {
	Code    int    `json:"code"`
	Message string `json:"message"`
}

// ----- LSP core value types -----

type Position struct {
	Line      int `json:"line"`
	Character int `json:"character"` // UTF-16 code units
}

type Range struct {
	Start Position `json:"start"`
	End   Position `json:"end"`
}

type Location struct {
	URI   string `json:"uri"`
	Range Range  `json:"range"`
}

// ----- Text document -----

type TextDocumentIdentifier struct {
	URI string `json:"uri"`
}

type TextDocumentItem struct {
	URI        string `json:"uri"`
	LanguageID string `json:"languageId"`
	Version    int    `json:"version"`
	Text       string `json:"text"`
}

type TextDocumentContentChangeEvent struct {
	Range       *Range `json:"range,omitempty"`
	RangeLength int    `json:"rangeLength,omitempty"`
	Text        string `json:"text"`
}

// ----- Initialize / capabilities -----

type InitializeParams struct {
	Capabilities any    `json:"capabilities"`
	RootURI      string `json:"rootUri,omitempty"`
}

type TextDocumentSyncOptions struct {
	OpenClose bool `json:"openClose"`
	// 1 = Full, 2 = Incremental
	Change            int  `json:"change"`
	WillSave          bool `json:"willSave"`
	WillSaveWaitUntil bool `json:"willSaveWaitUntil"`
	Save              *struct {
		IncludeText bool `json:"includeText"`
	} `json:"save,omitempty"`
}

type ServerCapabilities struct {
	TextDocumentSync   TextDocumentSyncOptions `json:"textDocumentSync"`
	HoverProvider      bool                    `json:"hoverProvider"`
	DefinitionProvider bool                    `json:"definitionProvider"`
	CompletionProvider *struct {
		TriggerCharacters []string `json:"triggerCharacters"`
	} `json:"completionProvider,omitempty"`
	DocumentSymbolProvider          bool `json:"documentSymbolProvider"`
	ReferencesProvider              bool `json:"referencesProvider"`
	WorkspaceSymbolProvider         bool `json:"workspaceSymbolProvider"`
	DocumentFormattingProvider      bool `json:"documentFormattingProvider"`
	DocumentRangeFormattingProvider bool `json:"documentRangeFormattingProvider"`
	SignatureHelpProvider           *struct {
		TriggerCharacters   []string `json:"triggerCharacters"`
		RetriggerCharacters []string `json:"retriggerCharacters"`
	} `json:"signatureHelpProvider,omitempty"`
	SemanticTokensProvider *struct {
		Legend struct {
			TokenTypes     []string `json:"tokenTypes"`
			TokenModifiers []string `json:"tokenModifiers"`
		} `json:"legend"`
		Full  bool `json:"full"`
		Range bool `json:"range"`
	} `json:"semanticTokensProvider,omitempty"`
	FoldingRangeProvider bool `json:"foldingRangeProvider"`
}

type InitializeResult struct {
	Capabilities ServerCapabilities `json:"capabilities"`
	ServerInfo   map[string]string  `json:"serverInfo,omitempty"`
}

// ----- Diagnostics -----

type Diagnostic struct {
	Range    Range  `json:"range"`
	Severity int    `json:"severity,omitempty"` // 1 = Error
	Code     string `json:"code,omitempty"`
	Source   string `json:"source,omitempty"`
	Message  string `json:"message"`
}

type PublishDiagnosticsParams struct {
	URI         string       `json:"uri"`
	Diagnostics []Diagnostic `json:"diagnostics"`
}

// ----- Hover -----

type Hover struct {
	Contents MarkupContent `json:"contents"`
	Range    *Range        `json:"range,omitempty"`
}

type MarkupContent struct {
	Kind  string `json:"kind"`  // "plaintext" or "markdown"
	Value string `json:"value"` // content
}

// ----- Completion -----

type CompletionItem struct {
	Label            string `json:"label"`
	Kind             int    `json:"kind,omitempty"`
	Detail           string `json:"detail,omitempty"`
	InsertText       string `json:"insertText,omitempty"`
	InsertTextFormat int    `json:"insertTextFormat,omitempty"`
}

// ----- Document symbols -----

type DocumentSymbol struct {
	Name           string           `json:"name"`
	Detail         string           `json:"detail,omitempty"`
	Kind           int              `json:"kind"`
	Range          Range            `json:"range"`
	SelectionRange Range            `json:"selectionRange"`
	Children       []DocumentSymbol `json:"children,omitempty"`
}

// ----- Semantic tokens -----

type SemanticTokensParams struct {
	TextDocument TextDocumentIdentifier `json:"textDocument"`
}

type SemanticTokensRangeParams struct {
	TextDocument TextDocumentIdentifier `json:"textDocument"`
	Range        Range                  `json:"range"`
}

type SemanticTokens struct {
	Data []uint32 `json:"data"`
}

// ----- Folding ranges -----

type FoldingRange struct {
	StartLine      int     `json:"startLine"`
	StartCharacter *int    `json:"startCharacter,omitempty"`
	EndLine        int     `json:"endLine"`
	EndCharacter   *int    `json:"endCharacter,omitempty"`
	Kind           *string `json:"kind,omitempty"` // "region", "comment"
}

// ----- Signature help -----

type SignatureHelpParams struct {
	TextDocument TextDocumentIdentifier `json:"textDocument"`
	Position     Position               `json:"position"`
}

type SignatureHelp struct {
	Signatures      []SignatureInformation `json:"signatures"`
	ActiveSignature int                    `json:"activeSignature"`
	ActiveParameter int                    `json:"activeParameter"`
}

type SignatureInformation struct {
	Label         string                 `json:"label"`
	Documentation *MarkupContent         `json:"documentation,omitempty"`
	Parameters    []ParameterInformation `json:"parameters,omitempty"`
}

type ParameterInformation struct {
	Label         string         `json:"label"`
	Documentation *MarkupContent `json:"documentation,omitempty"`
}
