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
