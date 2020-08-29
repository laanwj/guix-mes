#! /bin/sh

# GNU Mes --- Maxwell Equations of Software
# Copyright © 2018,2019 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
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

if test -z "$config_sh"; then
    . ./config.sh
fi

if [ "$V" = 2 ]; then
    set -x
fi

t=${1-lib/tests/scaffold/t.c}
b=$(dirname "$t")/$(basename "$t" .c)
o="$b"
o=lib/tests/${b#*lib/tests/}
if [ "$o" = "$b" ]; then
    o=./$(basename "$t" .c)
fi

rm -f "$o"
CC=${CC-gcc}

i=$(basename "$t" .c)

if [ -z "${MES_CHECKING_BUILTIN_LIBS}" ]
then
    MES_CHECKING_BUILTIN_LIBS="`${CC} --print-libgcc-file-name`"
fi

if [ -z "${i/[012][0-9]-*/}" ]; then
    LIBS="${MES_CHECKING_BUILTIN_LIBS}"
elif [ -z "${i/[34][0-9]-*/}" ]; then
    LIBS="-l c-mini ${MES_CHECKING_BUILTIN_LIBS} -l c-mini"
elif [ -z "${i/[78][0-9a-z]-*/}" ]; then
    LIBS="-l c+tcc ${MES_CHECKING_BUILTIN_LIBS} -l c+tcc"
elif [ -z "${i/9[0-9a-z]-*/}" ]; then
    LIBS="-l c+gnu ${MES_CHECKING_BUILTIN_LIBS} -l c+gnu"
else
    # Make it possible to resolve raise(), required by libgcc.a, provided
    # in libc.a.  The final command line has to have "-lc -lgcc -lc".
    # See <https://www.openwall.com/lists/musl/2018/05/09/1>.
    LIBS="-l c ${MES_CHECKING_BUILTIN_LIBS} -l c"
fi

if test $mes_kernel = gnu\
        && test -z "$LIBS"; then
    LIBS="-l c-mini -l mescc"
fi

if test $mes_libc = system; then
    crt1=
    LIBS='-l mes -l mescc'
else
    crt1=crt1.o
fi

$CC -g -c $AM_CPPFLAGS $CPPFLAGS $AM_CFLAGS $CFLAGS -o "$o".o "$t"
$CC -g $AM_CFLAGS $CFLAGS $AM_LDFLAGS $LDFLAGS -L . -o "$o" $crt1 "$o".o $LIBS

set +e
timeout 20 "$o" -s --long file0 file1 > "$o".1 2> "$o".2
r=$?
set -e
if [ -f "$b".exit ]; then
    e=$(cat "$b".exit)
else
    e=0
fi
[ $r = $e ] || exit 1
if [ -f "$b".stdout ]; then
    $DIFF -ub "$b".stdout "$o".1
fi
if [ -f "$b".stderr ]; then
    $DIFF -ub "$b".stderr "$o".2
fi
