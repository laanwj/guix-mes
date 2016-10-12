.PHONY: all check clean default 
#CFLAGS:=-std=c99 -O0
CFLAGS:=-std=c99 -O3 -finline-functions
#CFLAGS:=-pg -std=c99 -O0
#CFLAGS:=-std=c99 -O0 -g

default: all

all: mes

mes: mes.c mes.h

clean:
	rm -f mes environment.i symbol.i mes.h *.cat a.out

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
	grep -oE '^scm ([a-z_]+) = {SYMBOL,' mes.c | cut -d' ' -f 2 |\
		while read f; do\
			echo "symbols = cons (&$$f, symbols);";\
		done > symbols.i

check: all guile-check mes-check

TESTS:=\
 tests/base.test\
 tests/closure.test\
 tests/quasiquote.test\
 tests/let.test\
 tests/scm.test\
 tests/record.test\
 tests/let-syntax.test\
 tests/match.test\
#

BASE-0:=module/mes/base-0.mes
MES-0:=guile/mes-0.scm
MES:=./mes

mes-check: all
	for i in $(TESTS); do\
		cat $(BASE-0) $$(scripts/include.mes $$i) $$i | $(MES);\
	done

guile-check:
	for i in $(TESTS); do\
		guile -s <(cat $(MES-0) $$(scripts/include.mes $$i | grep -Ev 'let.mes|quasiquote.mes|srfi-0') $$i);\
	done
	for i in $(TESTS); do\
		guile -s <(cat $(MES-0) module/mes/test.mes $$i);\
	done

mescc: all
	scripts/mescc.mes
	./a.out

mescc.cat: $(MES-0) module/mes/lalr.mes module/mes/elf.mes module/mes/libc-i386.mes $(shell scripts/include.mes scripts/mescc.mes | grep -Ev '/mes/|/srfi/')
	echo '(compile)' | cat $^ - > $@

guile-mescc: mescc.cat
	cat doc/examples/main.c | guile -s $^ > a.out
	chmod +x a.out
	./a.out
