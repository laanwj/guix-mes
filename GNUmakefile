GUILE:=guile
GUILE_FLAGS:=--no-auto-compile -L . -L guile -C . -C guile

include .config.make

PHONY_TARGETS:= all all-go check clean clean-go default help list
.PHONY: $(PHONY_TARGETS)

$(PHONY_TARGETS):
	$(GUILE) $(GUILE_FLAGS) -s make.scm $@

%:
	$(GUILE) $(GUILE_FLAGS) -s make.scm $@

.config.make: ./configure
