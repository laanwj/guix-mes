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

. ${srcdest}build-aux/config.sh
. ${srcdest}build-aux/trace.sh

LIBC=${LIBC-c}

##moduledir=${moduledir-${datadir}${datadir:+/}module}

# native
# trace "SNARF gc.c"     ${srcdest}build-aux/mes-snarf.scm src/gc.c
# trace "SNARF lib.c"    ${srcdest}build-aux/mes-snarf.scm src/lib.c
# trace "SNARF math.c"   ${srcdest}build-aux/mes-snarf.scm src/math.c
# trace "SNARF mes.c"    ${srcdest}build-aux/mes-snarf.scm src/mes.c
# trace "SNARF posix.c"  ${srcdest}build-aux/mes-snarf.scm src/posix.c
# trace "SNARF reader.c" ${srcdest}build-aux/mes-snarf.scm src/reader.c
# trace "SNARF vector.c" ${srcdest}build-aux/mes-snarf.scm src/vector.c

# cc32-mes
trace "MSNARF gc.c"     ${srcdest}build-aux/mes-snarf.scm --mes src/gc.c
trace "MSNARF lib.c"    ${srcdest}build-aux/mes-snarf.scm --mes src/lib.c
trace "MSNARF math.c"   ${srcdest}build-aux/mes-snarf.scm --mes src/math.c
trace "MSNARF mes.c"    ${srcdest}build-aux/mes-snarf.scm --mes src/mes.c
trace "MSNARF posix.c"  ${srcdest}build-aux/mes-snarf.scm --mes src/posix.c
trace "MSNARF reader.c" ${srcdest}build-aux/mes-snarf.scm --mes src/reader.c
trace "MSNARF vector.c" ${srcdest}build-aux/mes-snarf.scm --mes src/vector.c

ARCHDIR=1 NOLINK=1 sh ${srcdest}build-aux/cc32-mes.sh lib/linux/x86-mes-gcc/crt0
ARCHDIR=1 NOLINK=1 sh ${srcdest}build-aux/cc32-mes.sh lib/linux/x86-mes-gcc/crt1
ARCHDIR=1 NOLINK=1 sh ${srcdest}build-aux/cc32-mes.sh lib/linux/x86-mes-gcc/crti
ARCHDIR=1 NOLINK=1 sh ${srcdest}build-aux/cc32-mes.sh lib/linux/x86-mes-gcc/crtn
ARCHDIR=1 NOLINK=1 sh ${srcdest}build-aux/cc32-mes.sh lib/libc-mini
ARCHDIR=1 NOLINK=1 sh ${srcdest}build-aux/cc32-mes.sh lib/libc
ARCHDIR=1 NOLINK=1 sh ${srcdest}build-aux/cc32-mes.sh lib/libgetopt
ARCHDIR=1 NOLINK=1 sh ${srcdest}build-aux/cc32-mes.sh lib/libc+tcc
ARCHDIR=1 NOLINK=1 sh ${srcdest}build-aux/cc32-mes.sh lib/libtcc1
ARCHDIR=1 NOLINK=1 sh ${srcdest}build-aux/cc32-mes.sh lib/libc+gnu
ARCHDIR=1 NOLINK=1 sh ${srcdest}build-aux/cc32-mes.sh lib/libg

sh ${srcdest}build-aux/cc32-mes.sh scaffold/main
sh ${srcdest}build-aux/cc32-mes.sh scaffold/hello
sh ${srcdest}build-aux/cc32-mes.sh scaffold/argv
sh ${srcdest}build-aux/cc32-mes.sh scaffold/malloc
sh ${srcdest}build-aux/cc32-mes.sh scaffold/micro-mes
sh ${srcdest}build-aux/cc32-mes.sh scaffold/tiny-mes
sh ${srcdest}build-aux/cc32-mes.sh scaffold/mini-mes

sh ${srcdest}build-aux/cc32-mes.sh src/mes

if [ "$CC32" = "$TCC" ]; then
    cp src/mes.mes-tcc-out src/mes
else
    cp src/mes.mes-gcc-out src/mes
fi
