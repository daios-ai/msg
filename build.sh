go test -v
go build

go build -o mindscript cmd/ms/main.go
go build -o mindscript-lsp cmd/lsp/main.go