GUILE:=guile
GUILE_FLAGS:=--no-auto-compile -L guile -C guile

include .config.make

PHONY_TARGETS:= all check clean default help
.PHONY: $(PHONY_TARGETS)

$(PHONY_TARGETS):
	$(GUILE) $(GUILE_FLAGS) -s make.scm $@
