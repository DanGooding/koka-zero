.DEFAULT_GOAL := all

# pass arguments as:
# $ make start -- arg1 arg2 arg3
ARGS := $(wordlist 2,$(words $(MAKECMDGOALS)),$(MAKECMDGOALS))
$(eval $(ARGS):;@:)

.PHONY: all
all:
	opam exec -- dune build --root . @install

.PHONY: deps
deps: ## Install development dependencies
	opam install -y dune-release ocamlformat utop ocaml-lsp-server merlin ocp-indent
	opam install --deps-only --with-test --with-doc -y .

.PHONY: create_switch
create_switch: ## Create an opam switch without any dependency
	opam switch create . ocaml-base-compiler.5.3.0 --no-install -y

.PHONY: lock
lock: ## Generate a lock file
	opam lock -y .

.PHONY: build
build: ## Build the project, including non installable libraries and executables
	opam exec -- dune build --root .

.PHONY: install
install: all ## Install the packages on the system
	opam exec -- dune install --root .

.PHONY: start
start: all ## Run the produced executable
	opam exec -- dune exec --root . bin/main.exe -- $(ARGS)

.PHONY: debug
debug: ## Debug the main executable
	opam exec -- dune build --root . bin/main.bc
	opam exec -- ocamldebug _build/default/bin/main.bc

.PHONY: test
test: ## Run the unit tests
	opam exec -- dune runtest --root .

.PHONY: promote
promote: ## Accept corrections to expect tests
	opam exec -- dune promote --root .

.PHONY: clean
clean: ## Clean build artifacts and other generated files
	opam exec -- dune clean --root .

.PHONY: doc
doc: ## Generate odoc documentation
	opam exec -- dune build --root . @doc

.PHONY: servedoc
servedoc: doc ## Open odoc documentation with default web browser
	xdg-open _build/default/_doc/_html/index.html

.PHONY: fmt
fmt: ## Format the codebase with ocamlformat
	opam exec -- dune build --root . --auto-promote @fmt

.PHONY: watch
watch: ## Watch for the filesystem and rebuild on every change
	opam exec -- dune build --root . --watch

.PHONY: utop
utop: ## Run a REPL and link with the project's libraries
	opam exec -- dune utop --root . lib -- -implicit-bindings

.PHONY: release
release: all ## Run the release script
	opam exec -- dune-release tag
	opam exec -- dune-release distrib
	opam exec -- dune-release publish distrib -y
	opam exec -- dune-release opam pkg
	opam exec -- dune-release opam submit --no-auto-open -y
