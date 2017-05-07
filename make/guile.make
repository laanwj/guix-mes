GO_FILES:=$(SCM_FILES:%.scm=%.go)
CLEAN+=$(GO_FILES)

clean-go: GO_FILES:=$(GO_FILES)
clean-go:
	@$(QUIET)rm -f $(GO_FILES)

INSTALL_SCM_FILES+=$(SCM_FILES)
INSTALL_GO_FILES+=$(GO_FILES)
INSTALL_MES_FILES+=$(MES_FILES)

GUILE_FLAGS:=\
  --no-auto-compile\
  -L guile\
  -C guile\
#

all-go: DIR:=$(DIR)
all-go: SCM_FILES:=$(SCM_FILES)
all-go: GUILE_FLAGS:=$(GUILE_FLAGS)
all-go: $(SCM_FILES)
	$(QUIET)rm -f $@
	$(QUIET)cd guile && srcdir=$(srcdir) host=$(host) $(GUILE) $(GUILE_FLAGS:guile=.) -s ../build-aux/compile-all.scm $(SCM_FILES:$(DIR)/%=%)

$(GO_FILES): all-go

# these .scm files include its .mes counterpart; must add dependency to be be remade
SCM_BASES:=$(SCM_FILES:%.scm=%)
SCM_MES_FILES:=$(filter $(SCM_BASES:%=%.mes),$(MES_FILES))
$(foreach scm_mes,$(SCM_MES_FILES),$(eval $(scm_mes:%.mes=%.go): $(scm_mes)))

CHECK := $(CHECK) $(TEST)
include make/reset.make
