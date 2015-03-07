squote  := '
# Fool fontify: '

ifeq ($(V), 1)
	quiet =
	Q =
else
	quiet = quiet_
	Q = @
endif

escsq = $(subst $(squote),'\$(squote)',$1)
echo-cmd = $(if $($(quiet)cmd_$(1)),\
	echo '  $(call escsq,$($(quiet)cmd_$(1)))';)
cmd = @$(echo-cmd) $(cmd_$(1))

quiet_cmd_elc_single = ELC     $(<)
      cmd_elc_single = $(EMACS) $(EFLAGS) --eval '(batch-byte-compile)' $(<)

quiet_cmd_rmfiles = $(if $(wildcard $(rm-files)),CLEAN   $(wildcard $(rm-files)))
      cmd_rmfiles = rm -f $(rm-files)

quiet_cmd_loaddefs = GEN     $(@)
      cmd_loaddefs = $(EMACS) $(EFLAGS) --eval \
	"(progn \
	  (setq vc-handled-backends nil) \
	  (defvar generated-autoload-file nil) \
	  (let ((generated-autoload-file \"$(shell pwd)/$(@)\") \
	        (make-backup-files nil)) \
	    (update-directory-autoloads \".\")))"
