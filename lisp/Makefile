# -*- coding: utf-8; -*-
# Emacs Makefile for tramp-rpc
#
# This Makefile provides targets for:
# - Generating autoloads
# - Byte-compiling elisp files
# - Testing
#
# Dependencies:
# - msgpack.el: Required for the protocol. Install via package.el or MELPA.
#
# For compilation with dependencies, set EMACS_LOAD_PATH or use:
#   make compile EMACS="emacs --eval \"(package-initialize)\""

EMACS		?= emacs
RM		= rm -f

# List elisp files in dependency order
# tramp-rpc.el depends on the others
LISP_FILES	= tramp-rpc-protocol.el tramp-rpc-deploy.el tramp-rpc.el
ELC_FILES	= $(LISP_FILES:.el=.elc)

# Emacs batch command
EM		= $(EMACS) -Q -batch -L .

.PHONY: all autoloads compile clean test-autoloads

all: autoloads

# Generate autoloads file using standard ;;;###autoload cookie
autoloads: tramp-rpc-autoloads.el
tramp-rpc-autoloads.el: $(LISP_FILES)
	$(EM) -l autoload \
	  --eval "(setq generated-autoload-file \
	            (expand-file-name \"tramp-rpc-autoloads.el\"))" \
	  --eval "(setq make-backup-files nil)" \
	  -f batch-update-autoloads .

# Byte compile with dependencies available
# Note: Requires msgpack.el to be installed
compile: $(ELC_FILES)

%.elc: %.el
	$(EMACS) -Q -batch --eval "(package-initialize)" -L . \
	  -l tramp -l bytecomp -f batch-byte-compile $<

clean:
	$(RM) $(ELC_FILES) tramp-rpc-autoloads.el

# Test that autoloads file can be loaded and registers the method with tramp
test-autoloads: autoloads
	$(EM) -l tramp \
	  -l tramp-rpc-autoloads.el \
	  --eval "(if (assoc \"rpc\" tramp-methods) \
	            (message \"SUCCESS: rpc method registered in tramp-methods\") \
	            (error \"FAILED: rpc method not found in tramp-methods\"))"

# For development: test that everything loads correctly (requires msgpack)
test-load:
	$(EMACS) -Q -batch --eval "(package-initialize)" -L . \
	  -l tramp -l tramp-rpc.el \
	  --eval "(message \"tramp-rpc loaded successfully\")"
