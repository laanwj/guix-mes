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

. ${srcdest}build-aux/config.sh
. ${srcdest}build-aux/trace.sh

GUILE=${GUILE-guile}
if [ -z "$GUILE" -o "$GUILE" = "true" ] || ! command -v $GUILE > /dev/null; then
    GUILE=src/mes
fi

[ -z "$MESCC" ] && MESCC=scripts/mescc
MES=${MES-$(command -v mes)}
[ -z "$MES" ] && MES=src/mes

set -e

trace "HEX2       0exit-42" $HEX2\
    --LittleEndian\
    --Architecture 1\
    --BaseAddress 0x1000000\
    -f ${srcdest}lib/x86-mes/elf32-0header.hex2\
    -f ${srcdest}lib/x86-mes/elf32-body-exit-42.hex2\
    -f ${srcdest}lib/x86-mes/elf-0footer.hex2\
    --exec_enable\
    -o lib/x86-mes/0exit-42.x86-out

trace "TEST       lib/x86-mes/0exit-42.x86-out" echo lib/x86-mes/0exit-42.x86-out
{ set +e; lib/x86-mes/0exit-42.x86-out; r=$?; set -e; }
[ $r != 42 ] && echo "  => $r" && exit 1

trace "HEX2       0exit-42" $HEX2\
    --LittleEndian\
    --Architecture 1\
    --BaseAddress 0x1000000\
    -f ${srcdest}lib/x86-mes/elf32-header.hex2\
    -f ${srcdest}lib/x86-mes/elf32-body-exit-42.hex2\
    -f ${srcdest}lib/x86-mes/elf32-footer-single-main.hex2\
    --exec_enable\
    -o lib/x86-mes/exit-42.x86-out

trace "TEST       lib/x86-mes/exit-42.x86-out" echo lib/x86-mes/exit-42.x86-out
{ set +e; lib/x86-mes/exit-42.x86-out; r=$?; set -e; }
[ $r != 42 ] && echo "  => $r" && exit 1

if [ -d "$MES_SEED" ]; then
    mkdir -p lib/x86-mes
    trace "M1         crt1.S" $M1\
        $M1FLAGS\
        -f ${srcdest}lib/x86-mes/x86.M1\
        -f $MES_SEED/x86-mes/crt1.S\
        -o lib/x86-mes/crt1.o
    trace "M1         libc.S" $M1\
        $M1FLAGS\
        -f ${srcdest}lib/x86-mes/x86.M1\
        -f $MES_SEED/x86-mes/libc.S\
        -o lib/x86-mes/libc.o
    trace "M1         mes.S" $M1\
        --LittleEndian\
        --Architecture 1\
        -f ${srcdest}lib/x86-mes/x86.M1\
        -f $MES_SEED/x86-mes/mes.S\
        -o src/mes.o
    trace "BLOOD_ELF  mes.S" $BLOOD_ELF\
        -f ${srcdest}lib/x86-mes/x86.M1\
        -f $MES_SEED/x86-mes/mes.S\
        -f $MES_SEED/x86-mes/libc.S\
        -o src/mes.S.blood-elf
    trace "M1         mes.blood-elf" $M1\
        --LittleEndian\
        --Architecture 1\
        -f src/mes.S.blood-elf\
        -o src/mes.o.blood-elf
    trace "HEX2       mes.o" $HEX2\
        $HEX2FLAGS\
        -f ${srcdest}lib/x86-mes/elf32-header.hex2\
        -f lib/x86-mes/crt1.o\
        -f lib/x86-mes/libc.o\
        -f src/mes.o\
        -f src/mes.o.blood-elf\
        --exec_enable\
        -o src/mes.seed-out
    cp src/mes.seed-out src/mes
    trace "M1         libc+tcc.S" $M1\
        $M1FLAGS\
        -f ${srcdest}lib/x86-mes/x86.M1\
        -f $MES_SEED/x86-mes/libc+tcc.S\
        -o lib/x86-mes/libc+tcc.o
fi

PREPROCESS=1
if [ ! -d "$MES_SEED" ] \
       && [ "$ARCH" = "i386" \
            -o "$ARCH" = "i586" \
            -o "$ARCH" = "i686" ]; then
    MES_ARENA=100000000
fi

MES_ARENA=100000000
ARCHDIR=1 NOLINK=1 bash ${srcdest}build-aux/cc-mes.sh lib/linux/x86-mes/crt1
ARCHDIR=1 NOLINK=1 bash ${srcdest}build-aux/cc-mes.sh lib/libc-mini

PREPROCESS= bash ${srcdest}build-aux/cc-mes.sh lib/x86-mes/exit-42

trace "TEST       lib/x86-mes/exit-42.mes-out" echo lib/x86-mes/exit-42.mes-out
{ set +e; lib/x86-mes/exit-42.mes-out; r=$?; set -e; }
[ $r != 42 ] && echo "  => $r" && exit 1

ARCHDIR=1 NOLINK=1 bash ${srcdest}build-aux/cc-mes.sh lib/linux/x86-mes/crt0
ARCHDIR=1 NOLINK=1 bash ${srcdest}build-aux/cc-mes.sh lib/linux/x86-mes/crti
ARCHDIR=1 NOLINK=1 bash ${srcdest}build-aux/cc-mes.sh lib/linux/x86-mes/crtn

ARCHDIR=1 NOLINK=1 bash ${srcdest}build-aux/cc-mes.sh lib/libc
ARCHDIR=1 NOLINK=1 bash ${srcdest}build-aux/cc-mes.sh lib/libgetopt
ARCHDIR=1 NOLINK=1 bash ${srcdest}build-aux/cc-mes.sh lib/libc+tcc
ARCHDIR=1 NOLINK=1 bash ${srcdest}build-aux/cc-mes.sh lib/libc+gnu


[ -n "$SEED" ] && exit 0

MES_ARENA=${MES_ARENA-100000000}
trace "SNARF.mes  gc.c"     ./pre-inst-env bash ${srcdest}build-aux/mes-snarf.scm --mes src/gc.c
trace "SNARF.mes  lib.c"    ./pre-inst-env bash ${srcdest}build-aux/mes-snarf.scm --mes src/lib.c
trace "SNARF.mes  math.c"   ./pre-inst-env bash ${srcdest}build-aux/mes-snarf.scm --mes src/math.c
trace "SNARF.mes  mes.c"    ./pre-inst-env bash ${srcdest}build-aux/mes-snarf.scm --mes src/mes.c
trace "SNARF.mes  posix.c"  ./pre-inst-env bash ${srcdest}build-aux/mes-snarf.scm --mes src/posix.c
trace "SNARF.mes  reader.c" ./pre-inst-env bash ${srcdest}build-aux/mes-snarf.scm --mes src/reader.c
trace "SNARF.mes  vector.c" ./pre-inst-env bash ${srcdest}build-aux/mes-snarf.scm --mes src/vector.c

echo MES_ARENA=$MES_ARENA
bash ${srcdest}build-aux/cc-mes.sh scaffold/main

bash ${srcdest}build-aux/cc-mes.sh scaffold/main
bash ${srcdest}build-aux/cc-mes.sh scaffold/hello
bash ${srcdest}build-aux/cc-mes.sh scaffold/argv
bash ${srcdest}build-aux/cc-mes.sh scaffold/malloc
##sh ${srcdest}build-aux/cc-mes.sh scaffold/micro-mes
##sh ${srcdest}build-aux/cc-mes.sh scaffold/tiny-mes
# bash ${srcdest}build-aux/cc-mes.sh scaffold/mini-mes
bash ${srcdest}build-aux/cc-mes.sh src/mes
cp src/mes.mes-out src/mes
