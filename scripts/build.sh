#!/usr/bin/env bash
#set -euo pipefail



# For tests, we need to override the MindScript root directory
# inferred from the path of the binary.
export MSGPATH="$(pwd)"

echo "1) Tidying"
go mod tidy

echo "2a) MindScript Library: testing"
go test -v ./mindscript

echo "2b) MindScript Library: building"
go build ./mindscript

echo "3a) MindScript CLI: building and testing"
go build -trimpath -ldflags="-s -w" -o ./bin/msg ./cmd/msg
./bin/msg test lib -v -p

printf "Done.\n\n"
