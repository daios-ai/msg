#!/usr/bin/env bash
#set -euo pipefail

echo "1) Tidying"
go mod tidy

echo "2) Library: testing and building"
go test -v ./internal/mindscript
go build ./internal/mindscript

echo "3) CLI: building"
go build -trimpath -ldflags="-s -w" -o ./msg ./cmd/msg

echo "LSP: testing and building"
go test -v ./cmd/msg-lsp
go build -trimpath -ldflags="-s -w" -o ./cmd/msg-lsp/vscode/server/msg-lsp ./cmd/lsp

echo "Done."