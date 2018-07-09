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

export CC CFLAGS CPPFLAGS

CC=${CC-gcc}
CFLAGS=${CFLAGS-"
--std=gnu99
-O0
-g
"}

CPPFLAGS=${CPPFLAGS-"
-D VERSION=\"$VERSION\"
-D MODULEDIR=\"$moduledir\"
-D PREFIX=\"$prefix\"
-I src
-I lib
-I include
"}

build-aux/mes-snarf.scm src/gc.c
build-aux/mes-snarf.scm src/lib.c
build-aux/mes-snarf.scm src/math.c
build-aux/mes-snarf.scm src/mes.c
build-aux/mes-snarf.scm src/posix.c
build-aux/mes-snarf.scm src/reader.c
build-aux/mes-snarf.scm src/vector.c

ARCHDIR=1 NOLINK=1 sh build-aux/cc.sh lib/libmes
sh build-aux/cc.sh src/mes
