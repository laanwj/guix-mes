TESTS:=\
 tests/read.test\
 tests/base.test\
 tests/closure.test\
 tests/quasiquote.test\
 tests/let.test\
 tests/scm.test\
 tests/display.test\
 tests/cwv.test\
 tests/math.test\
 tests/vector.test\
 tests/srfi-1.test\
 tests/srfi-13.test\
 tests/srfi-14.test\
 tests/optargs.test\
 tests/fluids.test\
 tests/catch.test\
 tests/psyntax.test\
 tests/pmatch.test\
 tests/let-syntax.test\
 tests/guile.test\
 tests/record.test\
 tests/match.test\
 tests/peg.test\
#

MES-0:=guile/mes-0.scm
TEST:=guile-check
$(TEST):
	set -e; for i in $(TESTS); do\
		$(GUILE) -s <(cat $(MES-0) module/mes/test.mes $$i);\
	done
include make/check.make

TEST:=mes-check
$(TEST): $(OUT)/mes
	set -e; for i in $(TESTS); do MES_MAX_ARENA=20000000 ./$$i; done
include make/check.make
