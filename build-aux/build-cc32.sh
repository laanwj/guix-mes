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

. build-aux/config.sh
. build-aux/trace.sh

LIBC=${LIBC-c}

##moduledir=${moduledir-${datadir}${datadir:+/}module}

# trace "SNARF gc.c"     build-aux/mes-snarf.scm src/gc.c
# trace "SNARF lib.c"    build-aux/mes-snarf.scm src/lib.c
# trace "SNARF math.c"   build-aux/mes-snarf.scm src/math.c
# trace "SNARF mes.c"    build-aux/mes-snarf.scm src/mes.c
# trace "SNARF posix.c"  build-aux/mes-snarf.scm src/posix.c
# trace "SNARF reader.c" build-aux/mes-snarf.scm src/reader.c
# trace "SNARF vector.c" build-aux/mes-snarf.scm src/vector.c

trace "MSNARF gc.c"     build-aux/mes-snarf.scm --mes src/gc.c
trace "MSNARF lib.c"    build-aux/mes-snarf.scm --mes src/lib.c
trace "MSNARF math.c"   build-aux/mes-snarf.scm --mes src/math.c
trace "MSNARF mes.c"    build-aux/mes-snarf.scm --mes src/mes.c
trace "MSNARF posix.c"  build-aux/mes-snarf.scm --mes src/posix.c
trace "MSNARF reader.c" build-aux/mes-snarf.scm --mes src/reader.c
trace "MSNARF vector.c" build-aux/mes-snarf.scm --mes src/vector.c

ARCHDIR=1 NOLINK=1 sh build-aux/cc32-mes.sh lib/linux/crt0
ARCHDIR=1 NOLINK=1 sh build-aux/cc32-mes.sh lib/linux/crt1
ARCHDIR=1 NOLINK=1 sh build-aux/cc32-mes.sh lib/linux/crti
ARCHDIR=1 NOLINK=1 sh build-aux/cc32-mes.sh lib/linux/crtn
ARCHDIR=1 NOLINK=1 sh build-aux/cc32-mes.sh lib/libc-mini
ARCHDIR=1 NOLINK=1 sh build-aux/cc32-mes.sh lib/libc
ARCHDIR=1 NOLINK=1 sh build-aux/cc32-mes.sh lib/libgetopt
ARCHDIR=1 NOLINK=1 sh build-aux/cc32-mes.sh lib/libc+tcc
ARCHDIR=1 NOLINK=1 sh build-aux/cc32-mes.sh lib/libtcc1
ARCHDIR=1 NOLINK=1 sh build-aux/cc32-mes.sh lib/libc+gnu
ARCHDIR=1 NOLINK=1 sh build-aux/cc32-mes.sh lib/libg

# sh build-aux/cc32-mes.sh scaffold/main
# sh build-aux/cc32-mes.sh scaffold/hello
# sh build-aux/cc32-mes.sh scaffold/argv
# sh build-aux/cc32-mes.sh scaffold/malloc
# sh build-aux/cc32-mes.sh scaffold/micro-mes
# sh build-aux/cc32-mes.sh scaffold/tiny-mes
# sh build-aux/cc32-mes.sh scaffold/mini-mes

sh build-aux/cc32-mes.sh src/mes
