# Minimal MindScript Makefile (auto-detects host OS/arch and builds that one)
# Targets:
#   make            → build + package for the current machine
#   make build      → just build binaries into dist/mindscript/bin
#   make package    → tar.gz + checksum
#   make clean      → remove dist/

# --- autodetect host ---
UNAME_S := $(shell uname -s)
UNAME_M := $(shell uname -m)

# Map to Go’s GOOS/GOARCH and to our tarball naming
ifeq ($(UNAME_S),Darwin)
  GOOS := darwin
  ifeq ($(UNAME_M),arm64)
    GOARCH := arm64
    TARNAME := mindscript-macos-arm64
  else
    $(error macOS x86_64 is not a supported release target)
  endif
else ifeq ($(UNAME_S),Linux)
  GOOS := linux
  ifeq ($(UNAME_M),x86_64)
    GOARCH := amd64
    TARNAME := mindscript-linux-x86_64
  else ifeq ($(UNAME_M),aarch64)
    GOARCH := arm64
    TARNAME := mindscript-linux-arm64
  else ifeq ($(UNAME_M),arm64)
    GOARCH := arm64
    TARNAME := mindscript-linux-arm64
  else
    $(error Unsupported Linux arch: $(UNAME_M). Expected x86_64 or (aarch64/arm64))
  endif
else
  $(error Unsupported OS: $(UNAME_S))
endif

# --- version & paths ---
VERSION ?= $(shell git describe --tags --always 2>/dev/null || echo 0.0.0)
DIST_DIR := dist
ROOT     := $(DIST_DIR)/mindscript
BIN_DIR  := $(ROOT)/bin
LIB_DIR  := $(ROOT)/lib

# --- CGO + libffi ---
export CGO_ENABLED := 1
# For macOS Homebrew libffi (pkg-config lives off the default path)
ifeq ($(UNAME_S),Darwin)
  export PKG_CONFIG_PATH ?= $(shell brew --prefix 2>/dev/null)/opt/libffi/lib/pkgconfig
endif

# RPATH so binaries search ../lib next to themselves
# NOTE: backslash escapes '$' for the shell so the linker sees literal $ORIGIN
RPATH_LINUX := -Wl,-rpath,\$$ORIGIN/../lib
RPATH_MACOS := -Wl,-rpath,@loader_path/../lib

# --- checksum tool ---
ifeq ($(UNAME_S),Darwin)
  SHASUM := shasum -a 256
else
  SHASUM := sha256sum
endif

# --- default ---
.PHONY: all build package clean print
all: print check-deps build package

print:
	@echo "Building for: GOOS=$(GOOS) GOARCH=$(GOARCH)  VERSION=$(VERSION)"

check-deps:
	@command -v pkg-config >/dev/null 2>&1 || { echo "Error: pkg-config not found."; exit 1; }
	@pkg-config --exists libffi || { \
		echo "Error: libffi dev files not found (pkg-config name: libffi)."; \
		echo "  Ubuntu:  sudo apt-get install -y libffi-dev pkg-config"; \
		echo "  macOS:   brew install libffi"; \
		exit 1; }

build:
	@rm -rf $(ROOT)
	@mkdir -p $(BIN_DIR) $(LIB_DIR)
ifeq ($(UNAME_S),Linux)
	GOOS=$(GOOS) GOARCH=$(GOARCH) go build -trimpath \
	 -ldflags='-linkmode external -extldflags "$(RPATH_LINUX)" -s -w' \
	 -o $(BIN_DIR)/msg     ./cmd/msg
	GOOS=$(GOOS) GOARCH=$(GOARCH) go build -trimpath \
	 -ldflags='-linkmode external -extldflags "$(RPATH_LINUX)" -s -w' \
	 -o $(BIN_DIR)/msg-lsp ./cmd/msg-lsp
	@set -e; \
	 CAND=$$(ldd $(BIN_DIR)/msg | awk '/libffi\.so/{print $$3; exit}'); \
	 test -n "$$CAND"; \
	 REAL=$$(readlink -f "$$CAND"); \
	 BASENAME=$$(basename "$$REAL"); \
	 cp -v "$$REAL" "$(LIB_DIR)/$$BASENAME"; \
	 SONAME=$$(readelf -d "$$REAL" | awk '/SONAME/{gsub(/[\[\]]/,"");print $$5; exit}'); \
	 [ -n "$$SONAME" ] && ln -sfn "$$BASENAME" "$(LIB_DIR)/$$SONAME" || true
else ifeq ($(UNAME_S),Darwin)
	GOOS=$(GOOS) GOARCH=$(GOARCH) go build -trimpath \
	 -ldflags='-linkmode external -extldflags "$(RPATH_MACOS)" -s -w' \
	 -o $(BIN_DIR)/msg     ./cmd/msg
	GOOS=$(GOOS) GOARCH=$(GOARCH) go build -trimpath \
	 -ldflags='-linkmode external -extldflags "$(RPATH_MACOS)" -s -w' \
	 -o $(BIN_DIR)/msg-lsp ./cmd/msg-lsp
	@set -e; \
	 LIB=$$(otool -L $(BIN_DIR)/msg | awk '/libffi.*dylib/{print $$1}' | grep -v '^/usr/lib/' | head -n1); \
	 [ -n "$$LIB" ] || LIB=$$(otool -L $(BIN_DIR)/msg | awk '/libffi.*dylib/{print $$1; exit}'); \
	 BASE=$$(basename "$$LIB"); cp -av "$$LIB" "$(LIB_DIR)/"; \
	 install_name_tool -id "@loader_path/../lib/$$BASE" "$(LIB_DIR)/$$BASE"; \
	 for B in $(BIN_DIR)/msg $(BIN_DIR)/msg-lsp; do \
	   install_name_tool -change "$$LIB" "@loader_path/../lib/$$BASE" $$B; done
endif
	@test -d lib      && cp -R lib      $(ROOT)/ || true
	@test -d examples && cp -R examples $(ROOT)/ || true
	@test -d docs     && cp -R docs     $(ROOT)/ || true
	@test -d data     && cp -R data     $(ROOT)/ || true
	@test -f LICENSE  && cp LICENSE     $(ROOT)/ || true
	@test -f README.md && cp README.md  $(ROOT)/ || true
	@echo "$(VERSION)" > $(ROOT)/VERSION

package:
	@mkdir -p $(DIST_DIR)
	tar -C $(DIST_DIR) -czf $(TARNAME).tar.gz mindscript
	@$(SHASUM) $(TARNAME).tar.gz > $(TARNAME).tar.gz.sha256
	@echo "Created: $(TARNAME).tar.gz (+ .sha256)"

clean:
	@rm -rf $(DIST_DIR)
