# Makefile for gilded-rose
# vim: set ft=make ts=8 noet
# Copyright Cabify.com
# Licence MIT

# Variables
# UNAME		:= $(shell uname -s)

.EXPORT_ALL_VARIABLES:

# this is godly
# https://news.ycombinator.com/item?id=11939200
.PHONY: help
help:	### this screen. Keep it first target to be default
ifeq ($(UNAME), Linux)
	@grep -P '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | \
		awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-20s\033[0m %s\n", $$1, $$2}'
else
	@# this is not tested, but prepared in advance for you, Mac drivers
	@awk -F ':.*###' '$$0 ~ FS {printf "%15s%s\n", $$1 ":", $$2}' \
		$(MAKEFILE_LIST) | grep -v '@awk' | sort
endif

# Targets
#
.PHONY: debug
debug:	### Debug Makefile itself
	@echo $(UNAME)

.PHONY: build-base
build-base:
	docker run \
		--env CGO_ENABLED=0 \
		--env GOCACHE=/.cache/go-build \
		--env GOMODCACHE=/go/mod/pkg \
		-v "$$PWD:/app:rw" -v "$$HOME/.cache/go-build:/.cache/go-build:rw" -v "$$HOME/.cache/go-mod-pkg:/go/mod/pkg:rw" \
		-w /app \
		-u "$$(id -u):$$(id -g)" \
		--entrypoint "/bin/sh" \
		--rm -it \
		golang:1.19.2-bullseye -c "$$GO_BUILD_CMD"

.PHONY: build
build:
	GO_BUILD_CMD="go install github.com/swaggo/swag/cmd/swag@v1.16.1 && swag init" make build-base
	GO_BUILD_CMD="go build -a -o target/bin/gilded-rose ./main.go" make build-base

.PHONY: build-dev
build-dev:
	GO_BUILD_CMD="go install github.com/swaggo/swag/cmd/swag@v1.16.1 && swag init" make build-base
	GO_BUILD_CMD="go build -v -o target/bin/gilded-rose ./main.go" make build-base

.PHONY: unit-test
unit-test:
	GO_BUILD_CMD="go install github.com/swaggo/swag/cmd/swag@v1.16.1 && swag init" make build-base
	GO_BUILD_CMD="go test ./... -cover -coverprofile=coverage.out" make build-base
	GO_BUILD_CMD="go tool cover -html=coverage.out -o coverage.html" make build-base

.PHONY: mod-tidy
mod-tidy:
	GO_BUILD_CMD="go mod tidy" make build-base

.PHONY: run
run: build-dev
	target/bin/gilded-rose app:serve

.PHONY: test
test: build-dev
	target/bin/gilded-rose test

.PHONY: texttest
texttest: build-dev
	docker run \
		--env XDG_CACHE_HOME=/.cache/pip \
		--env PIP_CACHE_DIR=/.cache/pip \
		--env TEXTTEST_ROOT=/app/texttests \
		--env TEXTTEST_PATH=/app \
		--env TEXTTEST_HOME=/app \
		-v "$$PWD:/app:rw" -v "$$HOME/.cache/pip:/.cache/pip:rw" \
		-w /app \
		-u "$$(id -u):$$(id -g)" \
		--entrypoint "/bin/sh" \
		--rm -it \
		python:3.9.17-slim-bookworm -c "pip install texttest && texttest -con"

.PHONY: docker
docker:
	docker build -t gilded-rose:latest .
