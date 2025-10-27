# ---- config ----
PROJECT      := mindscript
BIN1         := msg
BIN2         := msg-lsp
VERSION      := $(shell cat VERSION)
PKGROOT      := $(CURDIR)/dist
OUTROOT      := $(PKGROOT)/$(VERSION)
LDFLAGS      := -s -w
GOFLAGS      := -trimpath -ldflags "$(LDFLAGS)"

# source dirs to copy into packages
COPY_DIRS    := lib examples docs data
COPY_FILES   := VERSION LICENSE README.md

# ---- helpers ----
define package_layout
	mkdir -p $(1)/$(PROJECT)/bin
	cp $(COPY_FILES) $(1)/$(PROJECT) 2>/dev/null || true
	for d in $(COPY_DIRS); do [ -d $$d ] && cp -R $$d $(1)/$(PROJECT)/ ; done
endef

# ---- builds (CGO ON) ----
# Linux amd64 (native on Ubuntu)
build-linux-amd64:
	@echo ">> build linux/amd64 (CGO ON)"
	CGO_ENABLED=1 GOOS=linux GOARCH=amd64 \
	go build $(GOFLAGS) -o $(PKGROOT)/$(BIN1) ./cmd/$(BIN1)
	CGO_ENABLED=1 GOOS=linux GOARCH=amd64 \
	go build $(GOFLAGS) -o $(PKGROOT)/$(BIN2) ./cmd/$(BIN2)

# Linux arm64 (Raspberry Pi 4+) using cross compiler
build-linux-arm64:
	@echo ">> build linux/arm64 (CGO ON via aarch64-linux-gnu-gcc)"
	CGO_ENABLED=1 CC=aarch64-linux-gnu-gcc GOOS=linux GOARCH=arm64 \
	go build $(GOFLAGS) -o $(PKGROOT)/$(BIN1) ./cmd/$(BIN1)
	CGO_ENABLED=1 CC=aarch64-linux-gnu-gcc GOOS=linux GOARCH=arm64 \
	go build $(GOFLAGS) -o $(PKGROOT)/$(BIN2) ./cmd/$(BIN2)

# macOS builds must run on macOS runners (CGO ON, Apple Clang)
# We build each arch separately on macOS (Go handles the target)
build-darwin-amd64:
	@echo ">> build darwin/amd64 (CGO ON)"
	SDKROOT=$$(xcrun --sdk macosx --show-sdk-path) \
	CGO_ENABLED=1 CC=clang GOOS=darwin GOARCH=amd64 \
	go build $(GOFLAGS) -o $(PKGROOT)/$(BIN1) ./cmd/$(BIN1)
	SDKROOT=$$(xcrun --sdk macosx --show-sdk-path) \
	CGO_ENABLED=1 CC=clang GOOS=darwin GOARCH=amd64 \
	go build $(GOFLAGS) -o $(PKGROOT)/$(BIN2) ./cmd/$(BIN2)

build-darwin-arm64:
	@echo ">> build darwin/arm64 (CGO ON)"
	SDKROOT=$$(xcrun --sdk macosx --show-sdk-path) \
	CGO_ENABLED=1 CC=clang GOOS=darwin GOARCH=arm64 \
	go build $(GOFLAGS) -o $(PKGROOT)/$(BIN1) ./cmd/$(BIN1)
	SDKROOT=$$(xcrun --sdk macosx --show-sdk-path) \
	CGO_ENABLED=1 CC=clang GOOS=darwin GOARCH=arm64 \
	go build $(GOFLAGS) -o $(PKGROOT)/$(BIN2) ./cmd/$(BIN2)

# ---- packaging ----
pack-%: build-%
	@arch=$*; \
	dest="$(OUTROOT)/$(PROJECT)-$${arch}"; rm -rf "$$dest"; \
	$(call package_layout,$(OUTROOT)); \
	mv $(PKGROOT)/$(BIN1) "$$dest/bin/$(BIN1)"; \
	mv $(PKGROOT)/$(BIN2) "$$dest/bin/$(BIN2)"; \
	( cd $(OUTROOT) && tar -czf $(PROJECT)-$${arch}.tar.gz $(PROJECT) && rm -rf $(PROJECT) ); \
	echo ">> packaged $(OUTROOT)/$(PROJECT)-$${arch}.tar.gz"

# convenience aliases
pack-linux-amd64: pack-linux-amd64
pack-linux-arm64: pack-linux-arm64
pack-darwin-amd64: pack-darwin-amd64
pack-darwin-arm64: pack-darwin-arm64

# build everything + checksums
release: clean pack-linux-amd64 pack-linux-arm64 pack-darwin-amd64 pack-darwin-arm64
	@cd $(OUTROOT) && sha256sum $(PROJECT)-*.tar.gz > SHA256SUMS

clean:
	rm -rf $(PKGROOT)
.PHONY: build-* pack-* release clean
