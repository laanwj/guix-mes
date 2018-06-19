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

MODULEDIR=${MODULEDIR-${DATADIR}${DATADIR:+/}module}
export CC32 CPPFLAGS C32FLAGS

CC32=${CC32-$(command -v i686-unknown-linux-gnu-gcc)}
build-aux/mes-snarf.scm --mes src/gc.c
build-aux/mes-snarf.scm --mes src/lib.c
build-aux/mes-snarf.scm --mes src/math.c
build-aux/mes-snarf.scm --mes src/mes.c
build-aux/mes-snarf.scm --mes src/posix.c
build-aux/mes-snarf.scm --mes src/reader.c
build-aux/mes-snarf.scm --mes src/vector.c

build-aux/mes-snarf.scm src/gc.c
build-aux/mes-snarf.scm src/lib.c
build-aux/mes-snarf.scm src/math.c
build-aux/mes-snarf.scm src/mes.c
build-aux/mes-snarf.scm src/posix.c
build-aux/mes-snarf.scm src/reader.c
build-aux/mes-snarf.scm src/vector.c

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
-fno-stack-protector
-g
-m32
-nostdinc
-nostdlib
"}

ARCHDIR=1 NOLINK=1 sh build-aux/cc-mes-gcc.sh lib/crt0
ARCHDIR=1 NOLINK=1 sh build-aux/cc-mes-gcc.sh lib/crt1
ARCHDIR=1 NOLINK=1 sh build-aux/cc-mes-gcc.sh lib/crti
ARCHDIR=1 NOLINK=1 sh build-aux/cc-mes-gcc.sh lib/crtn
ARCHDIR=1 NOLINK=1 sh build-aux/cc-mes-gcc.sh lib/libc-mini
ARCHDIR=1 NOLINK=1 sh build-aux/cc-mes-gcc.sh lib/libc
ARCHDIR=1 NOLINK=1 sh build-aux/cc-mes-gcc.sh lib/libgetopt
ARCHDIR=1 NOLINK=1 sh build-aux/cc-mes-gcc.sh lib/libc+tcc
ARCHDIR=1 NOLINK=1 sh build-aux/cc-mes-gcc.sh lib/libtcc1
ARCHDIR=1 NOLINK=1 sh build-aux/cc-mes-gcc.sh lib/libc+gnu
ARCHDIR=1 NOLINK=1 sh build-aux/cc-mes-gcc.sh lib/libg

sh build-aux/cc-mes-gcc.sh scaffold/main
sh build-aux/cc-mes-gcc.sh scaffold/hello
sh build-aux/cc-mes-gcc.sh scaffold/argv
sh build-aux/cc-mes-gcc.sh scaffold/malloc
sh build-aux/cc-mes-gcc.sh scaffold/micro-mes
sh build-aux/cc-mes-gcc.sh scaffold/tiny-mes
sh build-aux/cc-mes-gcc.sh scaffold/mini-mes

sh build-aux/cc-mes-gcc.sh src/mes
