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

. build-aux/config.sh
. build-aux/trace.sh

GUILE=${GUILE-guile}
if [ -z "$GUILE" -o "$GUILE" = "true" ] || ! command -v $GUILE > /dev/null; then
    GUILE=src/mes
fi

[ -z "$MESCC" ] && MESCC=${top_builddest}scripts/mescc
MES=${MES-$(command -v mes)}
[ -z "$MES" ] && MES=${top_builddest}src/mes

set -e

if [ -d "$MES_SEED" ]; then
    trace "M1 crt1.S" $M1\
        $M1FLAGS\
        -f lib/x86-mes/x86.M1\
        -f $MES_SEED/x86-mes/crt1.S\
        -o ${top_builddest}lib/x86-mes/crt1.o
    trace "M1 libc.S" $M1\
        $M1FLAGS\
        -f lib/x86-mes/x86.M1\
        -f $MES_SEED/x86-mes/libc.S\
        -o ${top_builddest}lib/x86-mes/libc.o
    trace "M1 mes.S" $M1\
        --LittleEndian\
        --Architecture 1\
        -f lib/x86-mes/x86.M1\
        -f $MES_SEED/x86-mes/mes.S\
        -o ${top_builddest}src/mes.o
    trace "BLOOD_ELF mes.S" $BLOOD_ELF\
        -f lib/x86-mes/x86.M1\
        -f $MES_SEED/x86-mes/mes.S\
        -f $MES_SEED/x86-mes/libc.S\
        -o ${top_builddest}src/mes.S.blood-elf
    trace "M1 mes.blood-elf" $M1\
        --LittleEndian\
        --Architecture 1\
        -f ${top_builddest}src/mes.S.blood-elf\
        -o ${top_builddest}src/mes.o.blood-elf
    trace "HEX2 mes.o" $HEX2\
        $HEX2FLAGS\
        -f lib/x86-mes/elf32-header.hex2\
        -f ${top_builddest}lib/x86-mes/crt1.o\
        -f ${top_builddest}lib/x86-mes/libc.o\
        -f ${top_builddest}src/mes.o\
        -f ${top_builddest}src/mes.o.blood-elf\
        --exec_enable\
        -o ${top_builddest}src/mes.seed-out
    cp ${top_builddest}src/mes.seed-out ${top_builddest}src/mes
    trace "M1 libc+tcc.S" $M1\
        $M1FLAGS\
        -f lib/x86-mes/x86.M1\
        -f $MES_SEED/x86-mes/libc+tcc.S\
        -o ${top_builddest}lib/x86-mes/libc+tcc.o
fi

PREPROCESS=1
if [ ! -d "$MES_SEED" ] \
       && [ "$ARCH" = "i386" \
            -o "$ARCH" = "i586" \
            -o "$ARCH" = "i686" ]; then
    MES_ARENA=100000000
fi

MES_ARENA=100000000
ARCHDIR=1 NOLINK=1 bash build-aux/cc-mes.sh lib/linux/crt0
ARCHDIR=1 NOLINK=1 bash build-aux/cc-mes.sh lib/linux/crt1
ARCHDIR=1 NOLINK=1 bash build-aux/cc-mes.sh lib/linux/crti
ARCHDIR=1 NOLINK=1 bash build-aux/cc-mes.sh lib/linux/crtn
ARCHDIR=1 NOLINK=1 bash build-aux/cc-mes.sh lib/libc-mini
ARCHDIR=1 NOLINK=1 bash build-aux/cc-mes.sh lib/libc
ARCHDIR=1 NOLINK=1 bash build-aux/cc-mes.sh lib/libgetopt
ARCHDIR=1 NOLINK=1 bash build-aux/cc-mes.sh lib/libc+tcc
ARCHDIR=1 NOLINK=1 bash build-aux/cc-mes.sh lib/libc+gnu

[ -n "$SEED" ] && exit 0

MES_ARENA=${MES_ARENA-100000000}
trace "MSNARF gc.c"     ${top_builddir}/pre-inst-env bash build-aux/mes-snarf.scm --mes src/gc.c
trace "MSNARF lib.c"    ${top_builddir}/pre-inst-env bash build-aux/mes-snarf.scm --mes src/lib.c
trace "MSNARF math.c"   ${top_builddir}/pre-inst-env bash build-aux/mes-snarf.scm --mes src/math.c
trace "MSNARF mes.c"    ${top_builddir}/pre-inst-env bash build-aux/mes-snarf.scm --mes src/mes.c
trace "MSNARF posix.c"  ${top_builddir}/pre-inst-env bash build-aux/mes-snarf.scm --mes src/posix.c
trace "MSNARF reader.c" ${top_builddir}/pre-inst-env bash build-aux/mes-snarf.scm --mes src/reader.c
trace "MSNARF vector.c" ${top_builddir}/pre-inst-env bash build-aux/mes-snarf.scm --mes src/vector.c

echo MES_ARENA=$MES_ARENA
bash build-aux/cc-mes.sh scaffold/main

bash build-aux/cc-mes.sh scaffold/main
bash build-aux/cc-mes.sh scaffold/hello
bash build-aux/cc-mes.sh scaffold/argv
bash build-aux/cc-mes.sh scaffold/malloc
##sh build-aux/cc-mes.sh scaffold/micro-mes
##sh build-aux/cc-mes.sh scaffold/tiny-mes
# bash build-aux/cc-mes.sh scaffold/mini-mes
bash build-aux/cc-mes.sh src/mes
cp ${top_builddest}src/mes.mes-out ${top_builddest}src/mes
