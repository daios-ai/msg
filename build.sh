echo "Testing library"
go test -v
go build

echo "Building the CLI"
go build -o mindscript cmd/cli/main.go

echo "Building the LSP"
go build -o mindscript-lsp cmd/lsp/main.go