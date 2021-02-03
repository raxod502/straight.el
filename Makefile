VERSION ?=
CMD ?=

EMACS ?= emacs

SHELL := bash

#Point to path of your local emacs-buttercup install
BUTTERCUP ?= ../emacs-buttercup/

# The order is important for compilation.
for_compile := straight.el bootstrap.el install.el straight-x.el	\
	straight-watcher.el benchmark/straight-bench.el
for_checkdoc := straight.el
for_longlines := $(wildcard *.el *.md *.yml benchmark/*.el	\
	scripts/*.bash) Makefile
for_checkindent := $(wildcard *.el benchmark/*.el)

# excludes benchmarking, smoke and unit tests
.PHONY: all
all: clean lint

.PHONY: help
help: ## Show this message
	@echo "usage:" >&2
	@grep -h "[#]# " $(MAKEFILE_LIST)	| \
		sed 's/^/  make /'		| \
		sed 's/:[^#]*[#]# /|/'		| \
		sed 's/%/LANG/'			| \
		column -t -s'|' >&2

.PHONY: lint
lint: compile checkdoc longlines checkindent toc ## Run all the linters

.PHONY: compile
compile: ## Byte-compile
	@for file in $(for_compile); do \
	    echo "[compile] $$file" ;\
	    $(EMACS) -Q --batch -L . -f batch-byte-compile $$file 2>&1 \
	        | grep -v "^Wrote" \
	        | grep . && exit 1 || true ;\
	done

.PHONY: checkdoc
checkdoc: ## Check docstring style
	@for file in $(for_checkdoc); do \
	    echo "[checkdoc] $$file" ;\
	    $(EMACS) -Q --batch \
	        --eval "(or (fboundp 'checkdoc-file) (kill-emacs))" \
	        --eval "(setq sentence-end-double-space nil)" \
	        --eval "(checkdoc-file \"$$file\")" 2>&1 \
	        | grep . && exit 1 || true ;\
	done

.PHONY: longlines
longlines: ## Check for long lines
	@scripts/check-line-length.bash

.PHONY: checkindent
checkindent: ## Ensure that indentation is correct
	@tmpdir="$$(mktemp -d)"; for file in $(for_checkindent); do \
	    tmpfile="$$(basename $$file)"; \
	    echo "[checkindent] $$file"; \
	    $(EMACS) -Q --batch \
	        --eval "(setq inhibit-message t)" \
	        --eval "(load (expand-file-name \"indent.el\"  ) nil t)" \
	        --eval "(load (expand-file-name \"straight.el\") nil t)" \
	        --eval "(find-file \"$$file\")" \
	        --eval "(indent-region (point-min) (point-max))" \
	        --eval "(write-file \"$$tmpdir/$$tmpfile\")"; \
	    (diff <(cat          "$$file" | nl -v1 -ba | \
                           sed "s/\t/: /" | sed "s/^ */$$tmpfile:/") \
	          <(cat "$$tmpdir/$$tmpfile" | nl -v1 -ba | \
                           sed "s/\t/: /" | sed "s/^ */$$tmpfile:/") ) \
	        | grep -F ">" | grep -o "[a-z].*" | grep . && exit 1 || true; \
	done

.PHONY: toc
toc: README.md ## Update table of contents in README
	@echo "[toc] $^"
	@if command -v markdown-toc >/dev/null; then \
	    markdown-toc -i $^ ; \
	else \
	    echo "  --> markdown-toc missing, skipping" ; \
	fi

.PHONY: clean
clean: ## Remove build artifacts
	@echo "[clean]" *.elc
	@rm -f *.elc

.PHONY: smoke
smoke: ## Run smoke test (for CI use only)
	@scripts/smoke-test.bash

.PHONY: bench
bench: ## Run benchmarking script against package.el
	@$(EMACS) -Q --batch -l benchmark/straight-bench.el \
		-f straight-bench-batch

.PHONY: docker
docker: ## Start a Docker shell; e.g. make docker VERSION=25.3
	@scripts/docker.bash "$(VERSION)" "$(CMD)"

.PHONY: test
test: straight.elc
	$(EMACS) -Q --batch -L . -L $(BUTTERCUP) \
		-l buttercup -f buttercup-run-discover
