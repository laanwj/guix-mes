#! /bin/sh

# Mes --- Maxwell Equations of Software
# Copyright © 2017,2018 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
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

if [ -n "$BUILD_DEBUG" ]; then
    set -x
fi

export BLOOD_ELF GUILE HEX2 M1 MES MESCC
export M1FLAGS HEX2FLAGS PREPROCESS
export MES_SEED MES_ARENA

GUILE=${GUILE-guile}
if [ -z "$GUILE" -o "$GUILE" = "true" ] || ! command -v $GUILE > /dev/null; then
    GUILE=src/mes
fi

HEX2=${HEX2-hex2}
M1=${M1-M1}
BLOOD_ELF=${BLOOD_ELF-blood-elf}
MES_SEED=${MES_SEED-../mes-seed}
MESCC=${MESCC-$(command -v mescc)}
[ -z "$MESCC" ] && MESCC=scripts/mescc
MES=${MES-$(command -v mes)}
[ -z "$MES" ] && MES=src/mes

set -e

M1FLAGS=${M1FLAGS-"
--LittleEndian
--Architecture 1
"}
HEX2FLAGS=${HEX2FLAGS-"
--LittleEndian
--Architecture 1
--BaseAddress 0x1000000
"}

if [ -d "$MES_SEED" ]; then
    $M1\
        $M1FLAGS\
        -f lib/x86-mes/x86.M1\
        -f $MES_SEED/x86-mes/crt1.S\
        -o lib/x86-mes/crt1.o
    $M1\
        $M1FLAGS\
        -f lib/x86-mes/x86.M1\
        -f $MES_SEED/x86-mes/libc.S\
        -o lib/x86-mes/libc.o
    $M1\
        --LittleEndian\
        --Architecture 1\
        -f lib/x86-mes/x86.M1\
        -f $MES_SEED/x86-mes/mes.S\
        -o src/mes.o
    $BLOOD_ELF\
        -f lib/x86-mes/x86.M1\
        -f $MES_SEED/x86-mes/mes.S\
        -f $MES_SEED/x86-mes/libc.S\
        -o src/mes.S.blood-elf
    $M1\
        --LittleEndian\
        --Architecture 1\
        -f src/mes.S.blood-elf\
        -o src/mes.o.blood-elf
    $HEX2\
        $HEX2FLAGS\
        -f lib/x86-mes/elf32-header.hex2\
        -f lib/x86-mes/crt1.o\
        -f lib/x86-mes/libc.o\
        -f src/mes.o\
        -f src/mes.o.blood-elf\
        --exec_enable\
        -o src/mes.seed-out
    cp src/mes.seed-out src/mes
    $M1\
        $M1FLAGS\
        -f lib/x86-mes/x86.M1\
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
ARCHDIR=1 NOLINK=1 bash build-aux/cc-mes.sh lib/crt0
ARCHDIR=1 NOLINK=1 bash build-aux/cc-mes.sh lib/crt1
ARCHDIR=1 NOLINK=1 bash build-aux/cc-mes.sh lib/crti
ARCHDIR=1 NOLINK=1 bash build-aux/cc-mes.sh lib/crtn
ARCHDIR=1 NOLINK=1 bash build-aux/cc-mes.sh lib/libc-mini
ARCHDIR=1 NOLINK=1 bash build-aux/cc-mes.sh lib/libc
ARCHDIR=1 NOLINK=1 bash build-aux/cc-mes.sh lib/libgetopt
ARCHDIR=1 NOLINK=1 bash build-aux/cc-mes.sh lib/libc+tcc
ARCHDIR=1 NOLINK=1 bash build-aux/cc-mes.sh lib/libc+gnu

[ -n "$SEED" ] && exit 0

MES_ARENA=${MES_ARENA-100000000}
bash build-aux/mes-snarf.scm --mes src/gc.c
bash build-aux/mes-snarf.scm --mes src/lib.c
bash build-aux/mes-snarf.scm --mes src/math.c
bash build-aux/mes-snarf.scm --mes src/mes.c
bash build-aux/mes-snarf.scm --mes src/posix.c
bash build-aux/mes-snarf.scm --mes src/reader.c
bash build-aux/mes-snarf.scm --mes src/vector.c

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
cp src/mes.mes-out src/mes
