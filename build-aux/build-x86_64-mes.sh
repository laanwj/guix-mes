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
    --Architecture 2\
    --BaseAddress 0x1000000\
    -f ${srcdest}lib/x86_64-mes/elf64-0header.hex2\
    -f ${srcdest}lib/x86_64-mes/elf64-body-exit-42.hex2\
    -f ${srcdest}lib/x86_64-mes/elf-0footer.hex2\
    --exec_enable\
    -o lib/x86_64-mes/0exit-42.x86_64-out

trace "TEST       lib/x86_64-mes/0exit-42.x86_64-out" echo lib/x86_64-mes/0exit-42.x86_64-out
{ set +e; lib/x86_64-mes/0exit-42.x86_64-out; r=$?; set -e; }
[ $r != 42 ] && echo "  => $r" && exit 1

trace "HEX2       0exit-42" $HEX2\
    --LittleEndian\
    --Architecture 2\
    --BaseAddress 0x1000000\
    -f ${srcdest}lib/x86_64-mes/elf64-header.hex2\
    -f ${srcdest}lib/x86_64-mes/elf64-body-exit-42.hex2\
    -f ${srcdest}lib/x86_64-mes/elf64-footer-single-main.hex2\
    --exec_enable\
    -o lib/x86_64-mes/exit-42.x86_64-out

trace "TEST       lib/x86_64-mes/exit-42.x86_64-out" echo lib/x86_64-mes/exit-42.x86_64-out
{ set +e; lib/x86_64-mes/exit-42.x86_64-out; r=$?; set -e; }
[ $r != 42 ] && echo "  => $r" && exit 1

if [ -d "$MES_SEED" ]; then
    mkdir -p lib/x86_64-mes
    trace "M1         crt1.S" $M1\
        $M1FLAGS\
        -f ${srcdest}lib/x86_64-mes/x86_64.M1\
        -f $MES_SEED/x86_64-mes/crt1.S\
        -o lib/x86_64-mes/crt1.o
    trace "M1         libc.S" $M1\
        $M1FLAGS\
        -f ${srcdest}lib/x86_64-mes/x86_64.M1\
        -f $MES_SEED/x86_64-mes/libc.S\
        -o lib/x86_64-mes/libc.o
    trace "M1         mes.S" $M1\
        --LittleEndian\
        --Architecture 2\
        -f ${srcdest}lib/x86_64-mes/x86_64.M1\
        -f $MES_SEED/x86_64-mes/mes.S\
        -o src/mes.o
    trace "BLOOD_ELF  mes.S" $BLOOD_ELF\
        -f ${srcdest}lib/x86_64-mes/x86_64.M1\
        -f $MES_SEED/x86_64-mes/mes.S\
        -f $MES_SEED/x86_64-mes/libc.S\
        -o src/mes.S.blood-elf
    trace "M1         mes.blood-elf" $M1\
        --LittleEndian\
        --Architecture 2\
        -f src/mes.S.blood-elf\
        -o src/mes.o.blood-elf
    trace "HEX2       mes.o" $HEX2\
        $HEX2FLAGS\
        -f ${srcdest}lib/x86_64-mes/elf64-header.hex2\
        -f lib/x86_64-mes/crt1.o\
        -f lib/x86_64-mes/libc.o\
        -f src/mes.o\
        -f src/mes.o.blood-elf\
        --exec_enable\
        -o src/mes.seed-out
    cp src/mes.seed-out src/mes
    trace "M1         libc+tcc.S" $M1\
        $M1FLAGS\
        -f ${srcdest}lib/x86_64-mes/x86_64.M1\
        -f $MES_SEED/x86_64-mes/libc+tcc.S\
        -o lib/x86_64-mes/libc+tcc.o
fi


PREPROCESS=1
MES_ARENA=100000000

ARCHDIR=1 NOLINK=1 bash ${srcdest}build-aux/cc-x86_64-mes.sh lib/linux/x86_64-mes/crt1
ARCHDIR=1 NOLINK=1 bash ${srcdest}build-aux/cc-x86_64-mes.sh lib/libc-mini
MES_LIBS='-l c-mini' PREPROCESS= bash ${srcdest}build-aux/cc-x86_64-mes.sh lib/x86_64-mes/exit-42

trace "TEST       lib/x86_64-mes/exit-42.x86_64-mes-out" echo lib/x86_64-mes/exit-42.x86_64-mes-out
{ set +e; lib/x86_64-mes/exit-42.x86_64-mes-out; r=$?; set -e; }
[ $r != 42 ] && echo "  => $r" && exit 1

ARCHDIR=1 NOLINK=1 bash ${srcdest}build-aux/cc-x86_64-mes.sh lib/libc
ARCHDIR=1 NOLINK=1 bash ${srcdest}build-aux/cc-x86_64-mes.sh lib/libc+tcc
ARCHDIR=1 NOLINK=1 bash ${srcdest}build-aux/cc-x86_64-mes.sh lib/libc+gnu
ARCHDIR=1 NOLINK=1 bash ${srcdest}build-aux/cc-x86_64-mes.sh lib/libgetopt

MES_ARENA=${MES_ARENA-100000000}
sh ${srcdest}build-aux/snarf.sh --mes

if [ -n "$SEED" ]; then
    bash ${srcdest}build-aux/cc-mes.sh src/mes
    exit 0
fi

MES_LIBS='-l none' bash ${srcdest}build-aux/cc-x86_64-mes.sh scaffold/main

trace "TEST       scaffold/main.x86_64-mes-out" echo scaffold/main.x86_64-mes-out
{ set +e; scaffold/main.x86_64-mes-out; r=$?; set -e; }
[ $r != 42 ] && echo "  => $r" && exit 1

MES_LIBS='-l c-mini' bash ${srcdest}build-aux/cc-x86_64-mes.sh scaffold/hello
MES_LIBS='-l c-mini' bash ${srcdest}build-aux/cc-x86_64-mes.sh scaffold/argv
bash ${srcdest}build-aux/cc-x86_64-mes.sh scaffold/malloc
sh ${srcdest}build-aux/cc-x86_64-mes.sh scaffold/micro-mes
sh ${srcdest}build-aux/cc-x86_64-mes.sh scaffold/tiny-mes
bash ${srcdest}build-aux/cc-x86_64-mes.sh scaffold/mini-mes
bash ${srcdest}build-aux/cc-x86_64-mes.sh src/mes
# not yet, broken
# cp src/mes.x86_64-mes-out src/mes
