# Build configuration
# -------------------
APP_NAME = `grep -Eo 'name: .*' mox.cabal | cut -d ':' -f 2 | awk '{print $1}'`
GIT_BRANCH = `git rev-parse --abbrev-ref HEAD`
GIT_REVISION = `git rev-parse HEAD`

# Introspection targets
# ---------------------

.PHONY: help
help: header targets

.PHONY: header
header:
	@printf "\n\033[34mEnvironment\033[0m\n"
	@printf "\033[34m---------------------------------------------------------------\033[0m\n"
	@printf "\033[33m%-23s\033[0m" "APP_NAME"
	@printf "\033[35m%s\033[0m" $(APP_NAME)
	@echo ""
	@printf "\033[33m%-23s\033[0m" "GIT_BRANCH"
	@printf "\033[35m%s\033[0m" $(GIT_BRANCH)
	@echo ""
	@printf "\033[33m%-23s\033[0m" "GIT_REVISION"
	@printf "\033[35m%s\033[0m" $(GIT_REVISION)
	@echo ""

.PHONY: targets
targets:
	@printf "\n\033[34mTargets\033[0m\n"
	@printf "\033[34m---------------------------------------------------------------\033[0m\n"
	@perl -nle'print $& if m{^[a-zA-Z_-]+:.*?## .*$$}' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-22s\033[0m %s\n", $$1, $$2}'

# Build targets
# -------------------

.PHONY: build
build: alex ## Run GHC to produce executable
	cabal build

.PHONY: build-watch
build-watch: ## Watch files for changes and re-build
	cabal build --file-watch

.PHONY: clean
clean: ## Remove artificats
	cabal clean

.PHONY: alex
alex:
	alex src/Lexer.x

# Development targets
# -------------------

.PHONY: dev
dev: ## Run ghcid
	ghcid -c="cabal repl"

.PHONY: format
format: ## Format code
	ormolu --mode inplace **/*.hs

.PHONY: lint
lint: ## Lint code
	hlint .

.PHONY: repl
repl: ## Run repl
	cabal repl

.PHONY: test
test: ## Test code
	cabal test

.PHONY: cabal-to-nix
cabal-to-nix: ## Update nix shell configuration
	cabal2nix --shell ./. >shell.nix
	nixfmt shell.nix

.PHONY: shell
shell:
	nix-shell --argstr compiler ghc98
