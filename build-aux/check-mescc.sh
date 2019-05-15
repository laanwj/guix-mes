#! /bin/sh

# GNU Mes --- Maxwell Equations of Software
# Copyright © 2017,2018 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
#
# This file is part of GNU Mes.
#
# GNU Mes is free software; you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3 of the License, or (at
# your option) any later version.
#
# GNU Mes is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with GNU Mes.  If not, see <http://www.gnu.org/licenses/>.

set -e
. ./config.status
. ${srcdest}build-aux/config.sh
. ${srcdest}build-aux/trace.sh

test_sh=${test_sh-${srcdest}build-aux/test.sh}

tests="
scaffold/tests/t
scaffold/tests/00-exit-0
scaffold/tests/01-return-0
scaffold/tests/02-return-1
scaffold/tests/03-call
scaffold/tests/04-call-0
scaffold/tests/05-call-1
scaffold/tests/06-call-not-1
scaffold/tests/06-not-call-1
scaffold/tests/06-call-2
scaffold/tests/06-call-string
scaffold/tests/06-call-variable
scaffold/tests/06-return-void
scaffold/tests/07-include
scaffold/tests/08-assign
scaffold/tests/08-assign-negative
scaffold/tests/08-assign-global
scaffold/tests/10-if-0
scaffold/tests/11-if-1
scaffold/tests/12-if-eq
scaffold/tests/13-if-neq
scaffold/tests/14-if-goto
scaffold/tests/15-if-not-f
scaffold/tests/16-if-t
scaffold/tests/17-compare-char
scaffold/tests/17-compare-ge
scaffold/tests/17-compare-gt
scaffold/tests/17-compare-le
scaffold/tests/17-compare-lt
scaffold/tests/17-compare-unsigned-ge
scaffold/tests/17-compare-unsigned-gt
scaffold/tests/17-compare-unsigned-le
scaffold/tests/17-compare-unsigned-lt
scaffold/tests/17-compare-unsigned-char-le
scaffold/tests/17-compare-unsigned-short-le
scaffold/tests/17-compare-unsigned-long-le
scaffold/tests/17-compare-and
scaffold/tests/17-compare-or
scaffold/tests/17-compare-and-or
scaffold/tests/17-compare-assign
scaffold/tests/17-compare-call
scaffold/tests/18-assign-shadow
scaffold/tests/20-while
scaffold/tests/21-char-array-simple
scaffold/tests/21-char-array
scaffold/tests/22-while-char-array
scaffold/tests/23-global-pointer-init-null
scaffold/tests/23-global-pointer-init
scaffold/tests/23-global-pointer-ref
scaffold/tests/23-global-pointer-pointer-ref
scaffold/tests/23-pointer-sub
scaffold/tests/23-pointer
lib/tests/mes/30-oputs
lib/tests/string/30-strlen
scaffold/tests/32-call-wrap
scaffold/tests/32-compare
scaffold/tests/33-and-or
scaffold/tests/34-pre-post
scaffold/tests/35-compare-char
scaffold/tests/36-compare-arithmetic
scaffold/tests/37-compare-assign
scaffold/tests/38-compare-call-2
scaffold/tests/38-compare-call-3
scaffold/tests/38-compare-call
scaffold/tests/40-if-else
scaffold/tests/41-ternary
scaffold/tests/42-goto-label
scaffold/tests/43-for-do-while
scaffold/tests/44-switch
scaffold/tests/44-switch-fallthrough
scaffold/tests/44-switch-body-fallthrough
scaffold/tests/45-void-call
scaffold/tests/46-function-static
scaffold/tests/47-function-expression
scaffold/tests/48-global-static
lib/tests/assert/50-assert
lib/tests/mes/50-itoa
lib/tests/posix/50-getenv
lib/tests/string/50-strcmp
lib/tests/string/50-strcpy
lib/tests/string/50-strncmp
scaffold/tests/51-pointer-sub
scaffold/tests/54-argc
scaffold/tests/54-argv
scaffold/tests/55-char-array
scaffold/tests/60-math
scaffold/tests/61-array
scaffold/tests/62-array
scaffold/tests/63-struct
scaffold/tests/63-struct-pointer
scaffold/tests/63-struct-local
scaffold/tests/63-struct-function
scaffold/tests/63-struct-assign
scaffold/tests/63-struct-array
scaffold/tests/63-struct-array-assign
scaffold/tests/63-struct-array-compare
scaffold/tests/63-struct-cell
scaffold/tests/64-make-cell
scaffold/tests/65-read
scaffold/tests/66-local-char-array
scaffold/tests/70-stdarg
lib/tests/stdio/70-printf-hello
lib/tests/stdio/70-printf-simple
lib/tests/stdio/70-printf
lib/tests/stdlib/70-strtoull
lib/tests/string/70-strchr
scaffold/tests/71-struct-array
scaffold/tests/72-typedef-struct-def
scaffold/tests/73-union-hello
scaffold/tests/73-union
scaffold/tests/74-multi-line-string
scaffold/tests/75-struct-union
scaffold/tests/76-pointer-arithmetic-pp
scaffold/tests/76-pointer-arithmetic
scaffold/tests/77-pointer-assign
scaffold/tests/78-union-struct
scaffold/tests/79-int-array-simple
scaffold/tests/79-int-array
scaffold/tests/7a-struct-char-array
scaffold/tests/7b-struct-int-array-hello
scaffold/tests/7b-struct-int-array-pointer
scaffold/tests/7b-struct-int-array
scaffold/tests/7c-dynarray
scaffold/tests/7d-cast-char
scaffold/tests/7e-struct-array-access
scaffold/tests/7f-struct-pointer-arithmetic
scaffold/tests/7g-struct-byte-word-field
scaffold/tests/7h-struct-assign
scaffold/tests/7i-struct-struct-simple
scaffold/tests/7i-struct-struct
scaffold/tests/7k-empty-for
scaffold/tests/7k-for-each-elem-simple
scaffold/tests/7k-for-each-elem
scaffold/tests/7l-struct-any-size-array-simple
scaffold/tests/7l-struct-any-size-array
scaffold/tests/7m-struct-char-array-assign
scaffold/tests/7n-struct-struct-array
scaffold/tests/7o-struct-pre-post-simple
scaffold/tests/7o-struct-pre-post
scaffold/tests/7p-struct-cast
scaffold/tests/7q-bit-field-simple
scaffold/tests/7q-bit-field
scaffold/tests/7r-sign-extend
scaffold/tests/7s-struct-short
scaffold/tests/7s-unsigned-compare
scaffold/tests/7t-function-destruct
scaffold/tests/7u-double
scaffold/tests/7u-long-long
scaffold/tests/7u-ternary-expression
scaffold/tests/7u-call-ternary
scaffold/tests/7u-inc-byte-word
scaffold/tests/7u-struct-func
scaffold/tests/7u-struct-size10
scaffold/tests/7u-vstack
lib/tests/setjmp/80-setjmp
lib/tests/stdio/80-sscanf
lib/tests/stdlib/80-qsort
lib/tests/stdlib/80-qsort-dupes
lib/tests/string/80-strncpy
lib/tests/string/80-strrchr
scaffold/tests/82-define
scaffold/tests/83-heterogenoous-init
scaffold/tests/84-struct-field-list
scaffold/tests/85-sizeof
lib/tests/dirent/90-readdir
lib/tests/io/90-stat
lib/tests/posix/90-unsetenv
lib/tests/signal/90-signal
lib/tests/stdio/90-fopen
lib/tests/stdio/90-fopen-append
lib/tests/stdio/90-fread-fwrite
lib/tests/stdio/90-fseek
lib/tests/stdlib/90-strtol
lib/tests/string/90-snprintf
lib/tests/string/90-strpbrk
lib/tests/string/90-strspn
scaffold/tests/a0-call-trunc-char
scaffold/tests/a0-call-trunc-short
scaffold/tests/a0-call-trunc-int
scaffold/tests/a0-math-divide-signed-negative
scaffold/tests/a1-global-no-align
scaffold/tests/a1-global-no-clobber
"

broken="$broken
lib/tests/posix/50-getenv
scaffold/tests/17-compare-unsigned-char-le
scaffold/tests/17-compare-unsigned-short-le
scaffold/tests/66-local-char-array
scaffold/tests/a0-call-trunc-int
scaffold/tests/a0-math-divide-signed-negative
"

if [ "$mes_arch" = "x86_64-gcc" ]; then
    broken="$broken
scaffold/tests/21-char-array
scaffold/tests/41-ternary
scaffold/tests/stdio/70-printf-stdarg
scaffold/tests/stdio/70-printf-simple
scaffold/tests/stdio/70-printf
lib/tests/setjmp/80-setjmp
scaffold/tests/a1-global-no-align
"
    # Debian: debugme itoa is broken
    broken="$broken
scaffold/tests/47-function-expression
scaffold/tests/62-array
scaffold/tests/65-read
scaffold/tests/85-sizeof
lib/tests/mes/90-abtod
lib/tests/mes/90-dtoab
lib/tests/posix/90-execlp
"
fi

set +e
expect=$(echo $broken | wc -w)
pass=0
fail=0
total=0
for t in $tests; do
    b=$(basename "$t")
    if [ -z "${b/[012][0-9]-*/}" ]; then
        libc=
    elif [ -z "${b/[34][0-9]-*/}" ]; then
        libc='-l c-mini'
    elif [ -z "${b/[78][0-9a-z]-*/}" ]; then
        libc='-l c+tcc'
    elif [ -z "${b/9[0-9a-z]-*/}" ]; then
        libc='-l c+gnu'
    else
        libc='-l c'
    fi
    sh $test_sh "$t" > "$t".log 2>&1
    r=$?
    total=$(expr $total + 1)
    if [ $r = 0 ]; then
        echo $t: [OK]
        pass=$(expr $pass + 1)
    else
        echo $t: [FAIL]
        fail=$(expr $fail + 1)
    fi
done

[ $expect != 0 ] && echo "expect: $expect"
[ $fail != 0 ] && echo "failed: $fail"
[ $fail -lt $expect ] && echo "solved: $(expr $expect - $fail)"
echo "passed: $pass"
echo "total:  $total"
if [ $fail != 0 -a $fail -gt $expect ]; then
    echo FAILED: $fail/$total
    exit 1
elif [ $fail != 0 ]; then
    echo PASS: $pass/$total
else
    echo PASS: $total
fi
