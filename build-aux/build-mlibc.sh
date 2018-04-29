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

set -ex

export CC32=${CC32-$(type -p i686-unknown-linux-gnu-gcc)}
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

export CPPFLAGS=${CPPFLAGS-"
-D VERSION=\"$VERSION\"
-D MODULEDIR=\"$MODULEDIR\"
-D PREFIX=\"$PREFIX\"
-I src
-I lib
-I include
"}

export C32FLAGS=${C32FLAGS-"
--std=gnu99
-O0
-fno-stack-protector
-g
-m32
-nostdinc
-nostdlib
"}

NOLINK=1 sh build-aux/cc-mlibc.sh lib/crt1
NOLINK=1 sh build-aux/cc-mlibc.sh lib/libc-gcc
NOLINK=1 sh build-aux/cc-mlibc.sh lib/libc+tcc-gcc

sh build-aux/cc-mlibc.sh scaffold/main
sh build-aux/cc-mlibc.sh scaffold/hello
sh build-aux/cc-mlibc.sh scaffold/argv
sh build-aux/cc-mlibc.sh scaffold/malloc
sh build-aux/cc-mlibc.sh scaffold/micro-mes
sh build-aux/cc-mlibc.sh scaffold/tiny-mes
sh build-aux/cc-mlibc.sh scaffold/mini-mes

sh build-aux/cc-mlibc.sh src/mes
