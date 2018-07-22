#! /bin/sh

# GNU Mes --- Maxwell Equations of Software
# Copyright © 2018 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
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

. build-aux/config.sh
. build-aux/trace.sh

a=mes-gcc
if [ "$CC32" = "$TCC" ]; then
    a=mes-tcc
    LIBC=c+tcc # tcc bug with undefined symbols
fi
arch=x86-$a

if [ -n "$LIBC" ]; then
    CC32LIBS="${top_builddest}lib/$arch/lib$LIBC.o"
fi

c=$1

if [ -z "$ARCHDIR" ]; then
    o="${top_builddest}$c"
    d=${top_builddest}${c%%/*}
    p="$a-"
else
    b=${c##*/}
    d=${top_builddest}${c%%/*}/$arch
    o="$d/$b"
fi
mkdir -p $d

trace "CC32 $c.c" $CC32\
    -c\
    $CC32_CPPFLAGS\
    $CC32_CFLAGS\
    -o "$o".${p}o\
    "$c".c

if [ -z "$NOLINK" ]; then
    trace "CCLD32 $c.c" $CC32\
        $CC32_CPPFLAGS\
        $CC32_CFLAGS\
        -o "$o".${p}out\
        ${top_builddest}lib/$arch/crt1.o\
        "$o".${p}o\
        $CC32LIBS
fi
