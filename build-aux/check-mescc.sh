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
lib/tests/scaffold/t
lib/tests/scaffold/00-exit-0
lib/tests/scaffold/01-return-0
lib/tests/scaffold/02-return-1
lib/tests/scaffold/03-call
lib/tests/scaffold/04-call-0
lib/tests/scaffold/05-call-1
lib/tests/scaffold/06-call-not-1
lib/tests/scaffold/06-not-call-1
lib/tests/scaffold/06-call-2
lib/tests/scaffold/06-call-string
lib/tests/scaffold/06-call-variable
lib/tests/scaffold/06-return-void
lib/tests/scaffold/07-include
lib/tests/scaffold/08-assign
lib/tests/scaffold/08-assign-negative
lib/tests/scaffold/08-assign-global
lib/tests/scaffold/10-if-0
lib/tests/scaffold/11-if-1
lib/tests/scaffold/12-if-eq
lib/tests/scaffold/13-if-neq
lib/tests/scaffold/14-if-goto
lib/tests/scaffold/15-if-not-f
lib/tests/scaffold/16-if-t
lib/tests/scaffold/17-compare-char
lib/tests/scaffold/17-compare-ge
lib/tests/scaffold/17-compare-gt
lib/tests/scaffold/17-compare-le
lib/tests/scaffold/17-compare-lt
lib/tests/scaffold/17-compare-unsigned-ge
lib/tests/scaffold/17-compare-unsigned-gt
lib/tests/scaffold/17-compare-unsigned-le
lib/tests/scaffold/17-compare-unsigned-lt
lib/tests/scaffold/17-compare-unsigned-char-le
lib/tests/scaffold/17-compare-unsigned-short-le
lib/tests/scaffold/17-compare-unsigned-long-le
lib/tests/scaffold/17-compare-and
lib/tests/scaffold/17-compare-or
lib/tests/scaffold/17-compare-and-or
lib/tests/scaffold/17-compare-assign
lib/tests/scaffold/17-compare-call
lib/tests/scaffold/18-assign-shadow
lib/tests/scaffold/20-while
lib/tests/scaffold/21-char-array-simple
lib/tests/scaffold/21-char-array
lib/tests/scaffold/22-while-char-array
lib/tests/scaffold/23-global-pointer-init-null
lib/tests/scaffold/23-global-pointer-init
lib/tests/scaffold/23-global-pointer-ref
lib/tests/scaffold/23-global-pointer-pointer-ref
lib/tests/scaffold/23-pointer-sub
lib/tests/scaffold/23-pointer
lib/tests/mes/30-oputs
lib/tests/string/30-strlen
lib/tests/scaffold/32-call-wrap
lib/tests/scaffold/32-compare
lib/tests/scaffold/33-and-or
lib/tests/scaffold/34-pre-post
lib/tests/scaffold/35-compare-char
lib/tests/scaffold/36-compare-arithmetic
lib/tests/scaffold/37-compare-assign
lib/tests/scaffold/38-compare-call-2
lib/tests/scaffold/38-compare-call-3
lib/tests/scaffold/38-compare-call
lib/tests/scaffold/40-if-else
lib/tests/scaffold/41-ternary
lib/tests/scaffold/42-goto-label
lib/tests/scaffold/43-for-do-while
lib/tests/scaffold/44-switch
lib/tests/scaffold/44-switch-fallthrough
lib/tests/scaffold/44-switch-body-fallthrough
lib/tests/scaffold/45-void-call
lib/tests/scaffold/46-function-static
lib/tests/scaffold/47-function-expression
lib/tests/scaffold/48-global-static
lib/tests/assert/50-assert
lib/tests/mes/50-itoa
lib/tests/posix/50-getenv
lib/tests/string/50-strcmp
lib/tests/string/50-strcpy
lib/tests/string/50-strncmp
lib/tests/scaffold/51-pointer-sub
lib/tests/scaffold/54-argc
lib/tests/scaffold/54-argv
lib/tests/scaffold/55-char-array
lib/tests/scaffold/60-math
lib/tests/scaffold/61-array
lib/tests/scaffold/62-array
lib/tests/scaffold/63-struct
lib/tests/scaffold/63-struct-pointer
lib/tests/scaffold/63-struct-local
lib/tests/scaffold/63-struct-function
lib/tests/scaffold/63-struct-assign
lib/tests/scaffold/63-struct-array
lib/tests/scaffold/63-struct-array-assign
lib/tests/scaffold/63-struct-array-compare
lib/tests/scaffold/63-struct-cell
lib/tests/scaffold/64-make-cell
lib/tests/scaffold/65-read
lib/tests/scaffold/66-local-char-array
lib/tests/scaffold/70-stdarg
lib/tests/stdio/70-printf-hello
lib/tests/stdio/70-printf-simple
lib/tests/stdio/70-printf
lib/tests/stdlib/70-strtoull
lib/tests/string/70-strchr
lib/tests/scaffold/71-struct-array
lib/tests/scaffold/72-typedef-struct-def
lib/tests/scaffold/73-union-hello
lib/tests/scaffold/73-union
lib/tests/scaffold/74-multi-line-string
lib/tests/scaffold/75-struct-union
lib/tests/scaffold/76-pointer-arithmetic-pp
lib/tests/scaffold/76-pointer-arithmetic
lib/tests/scaffold/77-pointer-assign
lib/tests/scaffold/78-union-struct
lib/tests/scaffold/79-int-array-simple
lib/tests/scaffold/79-int-array
lib/tests/scaffold/7a-struct-char-array
lib/tests/scaffold/7b-struct-int-array-hello
lib/tests/scaffold/7b-struct-int-array-pointer
lib/tests/scaffold/7b-struct-int-array
lib/tests/scaffold/7c-dynarray
lib/tests/scaffold/7d-cast-char
lib/tests/scaffold/7e-struct-array-access
lib/tests/scaffold/7f-struct-pointer-arithmetic
lib/tests/scaffold/7g-struct-byte-word-field
lib/tests/scaffold/7h-struct-assign
lib/tests/scaffold/7i-struct-struct-simple
lib/tests/scaffold/7i-struct-struct
lib/tests/scaffold/7k-empty-for
lib/tests/scaffold/7k-for-each-elem-simple
lib/tests/scaffold/7k-for-each-elem
lib/tests/scaffold/7l-struct-any-size-array-simple
lib/tests/scaffold/7l-struct-any-size-array
lib/tests/scaffold/7m-struct-char-array-assign
lib/tests/scaffold/7n-struct-struct-array
lib/tests/scaffold/7o-struct-pre-post-simple
lib/tests/scaffold/7o-struct-pre-post
lib/tests/scaffold/7p-struct-cast
lib/tests/scaffold/7q-bit-field-simple
lib/tests/scaffold/7q-bit-field
lib/tests/scaffold/7r-sign-extend
lib/tests/scaffold/7s-struct-short
lib/tests/scaffold/7s-unsigned-compare
lib/tests/scaffold/7t-function-destruct
lib/tests/scaffold/7u-double
lib/tests/scaffold/7u-long-long
lib/tests/scaffold/7u-ternary-expression
lib/tests/scaffold/7u-call-ternary
lib/tests/scaffold/7u-inc-byte-word
lib/tests/scaffold/7u-struct-func
lib/tests/scaffold/7u-struct-size10
lib/tests/scaffold/7u-vstack
lib/tests/setjmp/80-setjmp
lib/tests/stdio/80-sscanf
lib/tests/stdlib/80-qsort
lib/tests/stdlib/80-qsort-dupes
lib/tests/string/80-strncpy
lib/tests/string/80-strrchr
lib/tests/scaffold/82-define
lib/tests/scaffold/83-heterogenoous-init
lib/tests/scaffold/84-struct-field-list
lib/tests/scaffold/85-sizeof
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
lib/tests/scaffold/a0-call-trunc-char
lib/tests/scaffold/a0-call-trunc-short
lib/tests/scaffold/a0-call-trunc-int
lib/tests/scaffold/a0-math-divide-signed-negative
lib/tests/scaffold/a1-global-no-align
lib/tests/scaffold/a1-global-no-clobber
"

broken="$broken
lib/tests/scaffold/17-compare-unsigned-char-le
lib/tests/scaffold/17-compare-unsigned-short-le
lib/tests/scaffold/66-local-char-array
lib/tests/scaffold/a0-call-trunc-int
lib/tests/scaffold/a0-math-divide-signed-negative
lib/tests/posix/50-getenv
"

if [ "$mes_arch" = "x86_64-gcc" ]; then
    broken="$broken
lib/tests/scaffold/21-char-array
lib/tests/scaffold/41-ternary
lib/tests/scaffold/stdio/70-printf-stdarg
lib/tests/scaffold/stdio/70-printf-simple
lib/tests/scaffold/stdio/70-printf
lib/tests/setjmp/80-setjmp
lib/tests/scaffold/a1-global-no-align
"
    # Debian: debugme itoa is broken
    broken="$broken
lib/tests/scaffold/47-function-expression
lib/tests/scaffold/62-array
lib/tests/scaffold/65-read
lib/tests/scaffold/85-sizeof
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
