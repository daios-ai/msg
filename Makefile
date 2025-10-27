SHELL := bash
REPO  := mindscript-lang/mindscript        # <-- change if needed
VERSION := $(shell cat VERSION)

# Binaries to build
CMDS := cmd/msg cmd/msg-lsp

# Output dirs
DIST := dist
STAGE := $(DIST)/stage

# CGO is required due to ffiOpen
export CGO_ENABLED := 1

# Common Go flags
GOFLAGS :=
LDFLAGS := -s -w
BUILD_FLAGS := -trimpath -ldflags="$(LDFLAGS)"

PLATFORMS := \
  linux/amd64 \
  linux/arm64 \
  darwin/amd64 \
  darwin/arm64

all: clean package

clean:
	@rm -rf $(DIST)

# Build per-platform (expects native toolchains/runners in CI)
define build_one
mkdir -p $(STAGE)/$(1)/bin
OS=$(word 1,$(subst /, ,$(1))); ARCH=$(word 2,$(subst /, ,$(1))); \
for CMD in $(CMDS); do \
  OUT=$$(basename $$CMD); \
  GOOS=$$OS GOARCH=$$ARCH go build $(BUILD_FLAGS) -o $(STAGE)/$(1)/bin/$$OUT ./$$CMD; \
done
# stage data
mkdir -p $(STAGE)/$(1)/{lib,examples,docs,data}
cp -a lib/*       $(STAGE)/$(1)/lib/        2>/dev/null || true
cp -a examples/*  $(STAGE)/$(1)/examples/   2>/dev/null || true
cp -a docs/*      $(STAGE)/$(1)/docs/       2>/dev/null || true
cp -a data/*      $(STAGE)/$(1)/data/       2>/dev/null || true
cp -a LICENSE README.md VERSION $(STAGE)/$(1)/
# archive
mkdir -p $(DIST)
tar -C $(STAGE)/$(1) -czf $(DIST)/mindscript-$(1)-$(VERSION).tar.gz .
endef

package:
	@$(foreach P,$(PLATFORMS),$(call build_one,$(P)))
	@cd $(DIST) && sha256sum *.tar.gz > checksums.txt || shasum -a 256 *.tar.gz > checksums.txt
	@echo "Artifacts in $(DIST)/"

.PHONY: all clean package
