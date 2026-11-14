# Rhizome — conventional front door.
#
# The real machinery lives elsewhere: ASDF/Quicklisp builds,
# session/install.sh installs, typst renders docs. This Makefile
# gives them the entry points users (and distro packagers) reach
# for by reflex.
#
#   make build    — compile-check the whole system (ASDF/Quicklisp)
#   make test     — run every test suite, aggregate, fail on any
#   make install  — session wiring into $HOME (idempotent)
#   make docs     — rebuild docs/testing-day.pdf
#   make clean    — purge this project's compiled fasl cache

SBCL      ?= sbcl
TYPST     ?= typst
HOME_DIR  ?= $(HOME)
SUITES    := animations decorations surface window-rules floating frame-pruning \
             workspace-modes theming bar-guard session-icons quickwins \
             groups interactivity workspace-restore workspace-switch-away \
             takeover tray-menus display-brightness prompt-free-text palette-message pull-hidden server-side-decorations client-fullscreen

.PHONY: all build test install docs clean help

all: build

help:
	@echo "Rhizome targets:"
	@echo "  make build    compile-check the system (ASDF/Quicklisp)"
	@echo "  make test     run all $(words $(SUITES)) test suites (aggregated)"
	@echo "  make install  session wiring into \$HOME (idempotent)"
	@echo "  make docs     rebuild docs/testing-day.pdf"
	@echo "  make clean    purge the project's fasl cache"
	@echo "Variables: SBCL, TYPST, SUITES (space-separated suite list)"

build:
	@echo ";;; Building rhizome (compile check)..."
	@$(SBCL) --non-interactive \
	   --eval '(ql:quickload :rhizome)' \
	   --eval '(format t ";;; build OK~%")' \
	   --eval '(quit)'

# Each suite prints an "=== N passed, M failed ===" summary and
# exits non-zero on failure. A crashed suite prints no summary,
# which is itself the failure signal. Every suite runs; the target
# fails at the end if any suite failed or crashed. Run a subset with:
#   make test SUITES="surface decorations"
test:
	@echo ";;; Running $(words $(SUITES)) test suites: $(SUITES)"
	@failed=""; \
	for suite in $(SUITES); do \
	  out=$$($(SBCL) --non-interactive --load tests/$$suite.lisp 2>&1 \
	         | grep -o '[0-9]* passed, [0-9]* failed' | tail -1); \
	  if [ -z "$$out" ]; then \
	    echo ";;; $$suite CRASHED; re-running to show the tail:"; \
	    $(SBCL) --non-interactive --load tests/$$suite.lisp 2>&1 | tail -15; \
	    failed="$$failed $$suite"; \
	    continue; \
	  fi; \
	  printf ';;; %-22s %s\n' "$$suite" "$$out"; \
	  case "$$out" in \
	    *", 0 failed") ;; \
	    *) failed="$$failed $$suite" ;; \
	  esac; \
	done; \
	if [ -n "$$failed" ]; then \
	  echo ";;; SUITES FAILED:$$failed"; exit 1; \
	fi; \
	echo ";;; ALL SUITES GREEN"

install: build
	@bash session/install.sh

docs:
	@$(TYPST) compile docs/testing-day.typ docs/testing-day.pdf
	@echo ";;; docs/testing-day.pdf rebuilt"

clean:
	@rm -rf $(HOME)/.cache/common-lisp/*$(CURDIR) \
	       $(HOME)/.cache/common-lisp/*/*$(CURDIR)
	@echo ";;; fasl cache purged"
