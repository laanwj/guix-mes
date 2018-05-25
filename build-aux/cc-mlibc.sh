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

CPPFLAGS=${CPPFLAGS-"
-D VERSION=\"$VERSION\"
-D MODULEDIR=\"$MODULEDIR\"
-D PREFIX=\"$PREFIX\"
-I src
-I lib
-I include
"}

C32FLAGS=${C32FLAGS-"
--std=gnu99
-O0
-fno-builtin
-fno-stack-protector
-g
-m32
-nostdinc
-nostdlib
"}
LIBC=${LIBC-lib/libc}

c=$1

$CC32\
    -c\
    $CPPFLAGS\
    $C32FLAGS\
    -o "$c".mlibc-o\
    "$c".c

if [ -z "$NOLINK" ]; then
    $CC32\
        $C32FLAGS\
        -o "$c".mlibc-out\
        lib/crt1.mlibc-o\
        "$c".mlibc-o\
        $LIBC-gcc.mlibc-o\
        $CC32LIBS
fi
