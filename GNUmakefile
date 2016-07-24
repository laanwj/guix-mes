.PHONY: all check default 
CFLAGS=-std=c99 -O3 -finline-functions
#CFLAGS=-std=c99 -g

default: all

all: mes

mes: mes.c mes.h

mes.h: mes.c GNUmakefile
	( echo '#if MES_C'; echo '#if MES_FULL' 1>&2;\
	grep -E '^(scm [*])*[a-z0-9_]+ \(.*\)( {|$$)' $< | grep -Ev '\(.*(char |bool |int )' | sed -e 's,^scm [*],,' | sort |\
		while read f; do\
			fun=$$(echo $$f | sed -e 's,^scm [*],,' -e 's,{.*,,');\
			name=$$(echo $$fun | sed -e 's,^scm [\*],,' | grep -o '^[^ ]*');\
			scm_name=$$(echo $$name | sed -e 's,_to_,->,' -e 's,_p$$,?,' -e 's,_x$$,!,' -e 's,^builtin_,,' -re 's,(.*)_$$,c:\1,' | sed \
				-e 's,^divide$$,/,'\
				-e 's,^is?$$,=,'\
				-e 's,^greater?$$,>,'\
				-e 's,^less?$$,<,'\
				-e 's,^minus$$,-,'\
				-e 's,^multiply$$,*,'\
				-e 's,^plus$$,+,'\
				-e 's,_,-,g');\
			args=$$(echo $$fun | grep -o 'scm [\*]' | wc -l);\
			[ "$$(echo $$fun | fgrep -o ... )" = "..." ] && args=n;\
			echo "scm *$$fun;";\
			echo "scm scm_$$name = {FUNCTION$$args, .name=\"$$scm_name\", .function$$args=&$$name};";\
			echo "a = add_environment (a, \"$$scm_name\", &scm_$$name);" 1>&2;\
	done; echo '#endif'; echo '#endif' 1>&2) > $@ 2>environment.i

check: all guile-check
	./mes.test
	./mes.test ./mes
	cat scm.mes lib/srfi/srfi-0.scm test.mes | ./mes

guile-check:
	guile -s test.mes

run: all
	cat scm.mes test.mes | ./mes

psyntax: all
	cat scm.mes psyntax.mes psyntax.pp psyntax2.mes | ./mes

syntax: all
	cat scm.mes syntax.mes syntax-test.mes | ./mes

syntax.test: syntax.mes syntax-test.mes
	cat $^ > $@

guile-syntax: syntax.test
	guile -s $^

syntax-case: all
	cat scm.mes syntax.mes syntax-case-lib.mes syntax-case.mes syntax-case-after.mes syntax-case-test.mes | ./mes

syntax-case.test: syntax.mes syntax-case-lib.mes syntax-case.mes syntax-case-after.mes syntax-case-test.mes
	cat $^ > $@

guile-syntax-case: syntax-case.test
	guile -s $^

macro: all
	cat scm.mes macro.mes | ./mes

peg: all
	cat scm.mes syntax.mes syntax-case-lib.mes syntax-case.mes syntax-case-after.mes peg.mes peg/codegen.scm peg/string-peg.scm peg/simplify-tree.scm peg/using-parsers.scm peg/cache.scm peg-test.mes | ./mes

peg.test: peg/pmatch.scm peg.mes peg/codegen.scm peg/string-peg.scm peg/simplify-tree.scm peg/using-parsers.scm peg/cache.scm peg-test.mes
	cat $^ | sed 's,\(;; Packages the results of a parser\),(when (guile?) (set! compile-peg-pattern (@@ (ice-9 peg codegen) compile-peg-pattern)))\n\1,' > $@

guile-peg: peg.test
#	guile -s peg-test.mes
#	@echo "======================================="
	guile -s $^

clean:
	rm -f mes environment.i mes.h peg.test syntax.test

record: all
	cat scm.mes syntax.mes lib/record.mes lib/record.scm lib/srfi/srfi-9.scm record.mes |./mes


paren: all
	cat scm.mes syntax.mes lib/srfi/srfi-0.scm lib/record.mes lib/record.scm lib/srfi/srfi-9.scm lib/lalr.mes lib/lalr.scm paren.scm | ./mes
#	#echo '___P((()))'

paren.test: lib/lalr.scm paren.scm
	cat $^ > $@

guile-paren: paren.test
	echo '___P((()))' | guile -s $^ 
