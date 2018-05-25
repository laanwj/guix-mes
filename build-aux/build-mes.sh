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
--Architecture=1
"}
HEX2FLAGS=${HEX2FLAGS-"
--LittleEndian
--Architecture=1
--BaseAddress=0x1000000
"}

if [ -d "$MES_SEED" ]; then
    $M1\
        $M1FLAGS\
        -f stage0/x86.M1\
        -f $MES_SEED/crt1.M1\
        -o lib/crt1.o
    $M1\
        $M1FLAGS\
        -f stage0/x86.M1\
        -f $MES_SEED/libc-mes.M1\
        -o lib/libc-mes.o
    $M1\
        --LittleEndian\
        --Architecture=1\
        -f stage0/x86.M1\
        -f $MES_SEED/mes.M1\
        -o src/mes.o
    $BLOOD_ELF\
        -f stage0/x86.M1\
        -f $MES_SEED/mes.M1\
        -f $MES_SEED/libc-mes.M1\
        -o src/mes.S.blood-elf
    $M1\
        --LittleEndian\
        --Architecture=1\
        -f src/mes.S.blood-elf\
        -o src/mes.o.blood-elf
    $HEX2\
        $HEX2FLAGS\
        -f stage0/elf32-header.hex2\
        -f lib/crt1.o\
        -f lib/libc-mes.o\
        -f src/mes.o\
        -f src/mes.o.blood-elf\
        --exec_enable\
        -o src/mes.seed-out
    cp src/mes.seed-out src/mes
    $M1\
        $M1FLAGS\
        -f stage0/x86.M1\
        -f $MES_SEED/libc+tcc-mes.M1\
        -o lib/libc+tcc-mes.o
fi

PREPROCESS=1
NOLINK=1 sh build-aux/cc-mes.sh lib/crt1
NOLINK=1 sh build-aux/cc-mes.sh lib/libc-mini-mes
NOLINK=1 sh build-aux/cc-mes.sh lib/libc-mes
NOLINK=1 sh build-aux/cc-mes.sh lib/libc+tcc-mes

cp lib/crt1.mes-o lib/crt1.o
cp lib/libc-mini-mes.mes-o lib/libc-mini-mes.o
cp lib/libc-mes.mes-o lib/libc-mes.o
cp lib/libc+tcc-mes.mes-o lib/libc+tcc-mes.o

[ -n "$SEED" ] && exit 0

MES_ARENA=${MES_ARENA-30000000}
sh build-aux/mes-snarf.scm --mes src/gc.c
sh build-aux/mes-snarf.scm --mes src/lib.c
sh build-aux/mes-snarf.scm --mes src/math.c
sh build-aux/mes-snarf.scm --mes src/mes.c
sh build-aux/mes-snarf.scm --mes src/posix.c
sh build-aux/mes-snarf.scm --mes src/reader.c
sh build-aux/mes-snarf.scm --mes src/vector.c

sh build-aux/cc-mes.sh scaffold/main
sh build-aux/cc-mes.sh scaffold/hello
sh build-aux/cc-mes.sh scaffold/argv
sh build-aux/cc-mes.sh scaffold/malloc
##sh build-aux/cc-mes.sh scaffold/micro-mes
##sh build-aux/cc-mes.sh scaffold/tiny-mes
# sh build-aux/cc-mes.sh scaffold/mini-mes

sh build-aux/cc-mes.sh src/mes
cp src/mes.mes-out src/mes
