#!/usr/bin/env bash
#set -euo pipefail

# For tests, we need to override the MindScript root directory
# inferred from the path of the binary.
export MSGPATH="$(pwd)"

echo "1) Tidying"
go mod tidy

echo "2) Library: testing and building"
go test -v ./internal/mindscript
go build ./internal/mindscript
printf "Done.\n\n"

echo "3) CLI: building and testing"
go build -trimpath -ldflags="-s -w" -o ./bin/msg ./cmd/msg
./bin/msg test lib -v -p
printf "Done.\n\n"
