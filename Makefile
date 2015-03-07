-include default.mk

EMACS ?= emacs
EFLAGS = -Q -batch --eval '(add-to-list '"'"'load-path ".")'

ELS := benchmark-init.el benchmark-init-modes.el
ELC := $(addsuffix c, $(ELS))

all: $(ELC) benchmark-init-loaddefs.el
clean: rm-files := $(ELC) benchmark-init-loaddefs.el

benchmark-init-loaddefs.el: $(ELS)
	-$(call cmd,loaddefs)

%.elc: %.el
	-$(call cmd,elc_single)

clean:
	-$(call cmd,rmfiles)

.PHONY: all clean
