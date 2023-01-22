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

.PHONY: merge-parser-messages
merge-parser-messages:
	opam exec -- dune exec menhir -- lib/parser.mly \
	    --list-errors \
	    > lib/parserMessages.auto.messages
	opam exec -- dune exec menhir -- lib/parser.mly \
	    --merge-errors lib/parserMessages.auto.messages \
	    --merge-errors lib/parserMessages.messages \
	    > lib/parserMessages.merged
	mv lib/parserMessages.merged lib/parserMessages.messages
	rm -f lib/parserMessages.auto.messages

.PHONY: strip-parser-messages
strip-parser-messages:
	@ sed -e "/^##/d" -i.bak parserMessages.messages

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

.PHONY: test-semantic
test-semantic: ## Test all the test sources
	@./tester.sh test/semant_test.exe ${TEST_SOURCES}

.PHONY: test-codegen
test-codegen: ## Test all the test sources
	@./tester.sh test/codegen_test.exe ${TEST_SOURCES}

.PHONY: unit-tests
unit-tests:  ## Run all the unit tests
	opam exec -- dune exec test/unit_tests/unit_tests.exe

.PHONY: clang
clang:
	@for f in $(ARGS:.mc=); do cp -- "$$f".mc "$$f".c; done
	(clang $(ARGS:.mc=.c) -o a.out && rm a.out) || true
	@rm $(ARGS:.mc=.c) || true