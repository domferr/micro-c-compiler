.DEFAULT_GOAL := all
EXE=microcc
BUILD_DIR=_build/default/
TESTDIR=test/samples
TEST_SOURCES := $(wildcard $(TESTDIR)/*.mc)

ARGS := $(wordlist 2,$(words $(MAKECMDGOALS)),$(MAKECMDGOALS))
$(eval $(ARGS):;@:)

.PHONY: all
all: 
	opam exec -- dune build --root .

.PHONY: deps
deps: ## Install development dependencies
	opam install -y dune ocamlformat utop ocaml-lsp-server
	opam install --deps-only --with-test --with-doc -y .

.PHONY: build
build: ## Build the project, including non installable libraries and executables
	opam exec -- dune build --root .

.PHONY: start
start: all ## Run the produced executable
	opam exec -- dune exec --root . bin/$(EXE).exe $(ARGS)

# It updates the file [parserMessages.messages] with new auto-generated
# comments for all error states.
.PHONY: update-parser-messages
update-parser-messages:
	cp -f lib/parserMessages.messages lib/parserMessages.messages.bak
	opam exec -- dune exec menhir -- lib/parser.mly \
	    --update-errors lib/parserMessages.messages \
	    > lib/parserMessages.updated
	mv lib/parserMessages.updated lib/parserMessages.messages
 
.PHONY: clean
clean: ## Clean build artifacts and other generated files
	opam exec -- dune clean --root .

.PHONY: doc
doc: ## Generate odoc documentation
	opam exec -- dune build --root . @doc

.PHONY: servedoc
servedoc: doc ## Open odoc documentation with default web browser
	open _build/default/_doc/_html/index.html

.PHONY: fmt
fmt: ## Format the codebase with ocamlformat
	opam exec -- dune build --root . --auto-promote @fmt

.PHONY: watch
watch: ## Watch for the filesystem and rebuild on every change
	opam exec -- dune build --root . --watch

.PHONY: utop
utop: ## Run a REPL and link with the project's libraries
	opam exec -- dune utop --root . lib -- -implicit-bindings

.PHONY: test-parser
test-parser: ## Test all the test sources
	@./tester.sh test/parser_test.exe ${TEST_SOURCES}

.PHONY: test-symbol-table
test-symbol-table: ## Test the symbol table
	opam exec -- dune exec test/symbol_table_test.exe

.PHONY: test-semantic
test-semantic: ## Test all the test sources
	@./tester.sh test/semant_test.exe ${TEST_SOURCES}
