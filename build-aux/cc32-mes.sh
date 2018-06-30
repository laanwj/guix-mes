#! /bin/sh

# Mes --- Maxwell Equations of Software
# Copyright © 2018 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
#
# This file is part of Mes.
#
# Mes is free software; you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3 of the License, or (at
# your option) any later version.
#
# Mes is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with Mes.  If not, see <http://www.gnu.org/licenses/>.

set -e

if [ -n "$BUILD_DEBUG" ]; then
    set -x
fi

CC32_CPPFLAGS=${CC32_CPPFLAGS-"
-D VERSION=\"$VERSION\"
-D MODULEDIR=\"$moduledir\"
-D PREFIX=\"$prefix\"
-I src
-I lib
-I include
"}

CC32_CFLAGS=${CC32_CFLAGS-"
-std=gnu99
-O0
-fno-builtin
-fno-stack-protector
-g
-m32
-nostdinc
-nostdlib
"}
LIBC=${LIBC-c}

a=mes-gcc
if [ "$CC32" = "$TCC" ]; then
    a=mes-tcc
    LIBC=c+tcc # tcc bug with undefined symbols
fi
arch=x86-$a

if [ -n "$LIBC" ]; then
    CC32LIBS="lib/$arch/lib$LIBC.o"
fi

c=$1

if [ -z "$ARCHDIR" ]; then
    o="$c"
    p="$a-"
else
    b=${c##*/}
    d=${c%%/*}
    o="$d/$arch/$b"
    mkdir -p $d/$arch
fi

$CC32\
    -c\
    $CC32_CPPFLAGS\
    $CC32_CFLAGS\
    -o "$o".${p}o\
    "$c".c

if [ -z "$NOLINK" ]; then
    $CC32\
        $CC32_CPPFLAGS\
        $CC32_CFLAGS\
        -o "$o".${p}out\
        lib/$arch/crt1.o\
        "$o".${p}o\
        $CC32LIBS
fi
