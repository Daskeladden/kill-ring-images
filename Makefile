# kill-ring-images -- images in the Emacs kill ring.

EMACS   ?= emacs
SOURCES  = $(wildcard kill-ring-images*.el)
TESTS    = $(wildcard tests/kill-ring-images*-tests.el)
X_RESULT = $(CURDIR)/tests/x-result.txt

.PHONY: test compile test-x clean

test:
	$(EMACS) -Q --batch -L . -L tests \
	  $(foreach t,$(TESTS),-l $(t)) \
	  --eval '(ert-run-tests-batch-and-exit)'

# Every file byte-compiles with no warning.  The .elc files go again at
# once, so that the tests never load old compiled code.
compile:
	$(EMACS) -Q --batch -L . \
	  --eval '(setq byte-compile-error-on-warn t)' \
	  -f batch-byte-compile $(SOURCES); \
	status=$$?; rm -f *.elc; exit $$status

# A graphical Emacs on a display of its own, with xclip as the other
# program.  It never touches the display that you work on.
test-x:
	@if command -v xvfb-run >/dev/null 2>&1; then \
	  rm -f $(X_RESULT); \
	  xvfb-run -a $(EMACS) -Q -L . -L tests -l tests/kill-ring-images-x-test.el \
	    --eval '(kill-ring-images-x-test-run "$(X_RESULT)")'; \
	  cat $(X_RESULT); grep -q '^PASS' $(X_RESULT); \
	else \
	  echo "make test-x: skipped, xvfb-run is missing (sudo apt install xvfb)"; \
	fi

clean:
	rm -f *.elc tests/*.elc $(X_RESULT)
