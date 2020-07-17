NAME := gilded-rose

# set default compiler
GO := go

.PHONY: prebuild
prebuild:

all: lint test

.PHONY: lint
lint: ## Verifies `golint` passes.
	@echo "+ $@"
	@golint ./... | grep -v '.pb.go:' | grep -v vendor | tee /dev/stderr

.PHONY: test
test: prebuild ## Runs the go tests.
	@echo "+ $@"
	@$(GO) test -v $(shell $(GO) list ./... | grep -v vendor)

