.DEFAULT_GOAL := build

# pass arguments as:
# $ make start -- arg1 arg2 arg3
ARGS := $(wordlist 2,$(words $(MAKECMDGOALS)),$(MAKECMDGOALS))
$(eval $(ARGS):;@:)

_opam/.opam-switch: ## Create a fresh opam installation environment
	opam switch create . ocaml-base-compiler.5.3.0 --no-install -y

.PHONY: install-deps
install-deps: _opam/.opam-switch ## first development dependencies, then project's dependencies
	opam install -y dune-release ocamlformat utop ocaml-lsp-server merlin ocp-indent
	opam install --deps-only --with-test --with-doc -y .

.PHONY: build
build: ## Build the project, including non installable libraries and executables
	opam exec -- dune build --root .

.PHONY: exec
exec:  ## Run the produced executable
	opam exec -- dune exec --root . bin/main.exe -- $(ARGS)

.PHONY: test
test: ## Run the unit tests
	opam exec -- dune runtest --root .

.PHONY: promote
promote: ## Accept corrections to expect tests
	opam exec -- dune promote --root .

.PHONY: clean
clean: ## Clean build artifacts and other generated files
	opam exec -- dune clean --root .

.PHONY: fmt
fmt: ## Format the codebase with ocamlformat
	opam exec -- dune build --root . --auto-promote @fmt

.PHONY: watch
watch: ## Watch for the filesystem and rebuild on every change
	opam exec -- dune build --root . --watch

.PHONY: utop
utop: ## Run a REPL and link with the project's libraries
	opam exec -- dune utop --root . lib -- -implicit-bindings

.PHONY: lock
lock: ## Generate a lock file
	opam lock -y .
