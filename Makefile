EMACS ?= emacs
EMACSFLAGS := -Q --batch --eval "(setq user-emacs-directory default-directory)"
CHECKFLAGS ?=
EL_FILES := early-init.el init.el $(wildcard lisp/*.el)

.PHONY: help compile validate check clean

help:
	@printf '%s\n' \
	  'Targets:' \
	  '  make compile   Byte-compile init files and local elisp' \
	  '  make validate  Load early-init.el and init.el in batch mode' \
	  '  make check     Run compile and validate' \
	  '  make clean     Remove compiled .elc files'

compile:
	$(EMACS) $(EMACSFLAGS) \
	  $(CHECKFLAGS) \
	  --eval "(require 'package)" \
	  --eval "(package-initialize)" \
	  -L lisp \
	  -f batch-byte-compile $(EL_FILES)

validate:
	$(EMACS) $(EMACSFLAGS) $(CHECKFLAGS) -l early-init.el -l init.el

check: compile validate

clean:
	rm -f *.elc lisp/*.elc
