echo "Testing library"
go test -v
go build

echo "Building the CLI"
go build -o ./mindscript ./cmd/cli/main.go

echo "Building the LSP"
go test ./cmd/lsp -v
go build -o ./cmd/lsp/vscode/server/mindscript-lsp ./cmd/lsp
