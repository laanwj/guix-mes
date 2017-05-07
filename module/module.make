CLEAN+=module/mes/read-0.mo
module/mes/read-0.mo: module/mes/read-0.mes $(OUT)/mes
	@rm -f $@
	@echo " DUMP	$(notdir $^) -> $(notdir $@)"
	$(QUIET)$(OUT)/mes --dump < $< > $@

CLEAN+=module/mes/read-0-32.mo
CROSS:=$(CC32:%gcc=%)
module/mes/read-0-32.mo: CROSS:=$(CROSS)
module/mes/read-0-32.mo: module/mes/read-0.mes
module/mes/read-0-32.mo: $(OUT)/$(CROSS)mes
	@rm -f $@
	@echo " DUMP	$(notdir $^) -> $(notdir $@)"
	$(QUIET)MES_MINI=1 $(OUT)/$(CROSS)mes --dump < $< > $@

CLEAN+=module/mes/tiny-0-32.mo
module/mes/tiny-0-32.mo: CROSS:=$(CROSS)
module/mes/tiny-0-32.mo: $(OUT)/$(CROSS)mes
	@rm -f $@
	@echo " DUMP	$(notdir $^) -> $(notdir $@)"
	$(QUIET) MES_TINY=1 $(OUT)/$(CROSS)mes --dump --tiny < $< > $@

MO_FILES:=\
 module/mes/read-0.mo\
 module/mes/read-0-32.mo\
 module/mes/tiny-0-32.mo\
#
all-mo: $(MO_FILES)
clean-mo: MO_FILES:=$(MO_FILES)
clean-mo:
	@$(QUIET)rm -f $(MO_FILES)

MES_FILES:=$(shell $(GIT_LS_FILES) module/*.mes)
SCM_FILES:=$(shell $(GIT_LS_FILES) module/language/ module/nyacc/ module/mes/)
SCM_FILES:=$(filter %.scm, $(SCM_FILES))
SCM_FILES:=$(filter-out %match.scm, $(SCM_FILES))
SCM_FILES:=$(filter-out %mes/lalr.scm, $(SCM_FILES))
SCM_FILES:=$(filter-out %optargs.scm, $(SCM_FILES))
SCM_FILES:=$(filter-out %pretty-print.scm, $(SCM_FILES))
SCM_FILES:=$(filter-out %syntax.scm, $(SCM_FILES))
SCM_FILES:=$(filter-out module/mes/peg/%.scm, $(SCM_FILES))
include make/guile.make

# FIXME: https://gitlab.com/janneke/guile/commits/1.8
# Include patches here
GUILE_GIT:=../guile-1.8
GUILE_COMMIT:=ba8a7097699f69b206c9f28c546fa6da88b8656f
psyntax-import: module/mes/psyntax.ss module/mes/psyntax.pp

module/mes/psyntax.%: $(GUILE_GIT)/ice-9/psyntax.%
	git --git-dir=$(GUILE_GIT)/.git --work-tree=$(GUILE_GIT) show $(GUILE_COMMIT):ice-9/$(@F > $@

MAINTAINER-CLEAN+=module/mes/psyntax.pp
