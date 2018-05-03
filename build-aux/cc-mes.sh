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

set -x

export BLOOD_ELF GUILE HEX2 M1 MES MESCC
export M1FLAGS HEX2FLAGS PREPROCESS
export MES_SEED MES_ARENA

HEX2=${HEX2-hex2}
M1=${M1-M1}
BLOOD_ELF=${BLOOD_ELF-blood-elf}
MES_SEED=${MES_SEED-../mes-seed}
MESCC=${MESCC-$(command -v mescc)}
[ -z "$MESCC" ] && MESCC=scripts/mescc
MES=${MES-$(command -v mes)}
[ -z "$MES" ] && MES=src/mes

CPPFLAGS=${CPPFLAGS-"
-D VERSION=\"$VERSION\"
-D MODULEDIR=\"$MODULEDIR\"
-D PREFIX=\"$PREFIX\"
-I src
-I lib
-I include
"}

MESCCLAGS=${MESCCFLAGS-"
"}
LIBC=${LIBC-lib/libc}
M1FLAGS=${M1FLAGS-"
--LittleEndian
--Architecture=1
"}
HEX2FLAGS=${HEX2FLAGS-"
--LittleEndian
--Architecture=1
--BaseAddress=0x1000000
"}

c=$1

set -e

if [ -n "$PREPROCESS" ]; then
    sh -x $MESCC\
       -E\
       $CPPFLAGS\
       $MESCCFLAGS\
       -o "$c".E\
       "$c".c
    sh -x $MESCC\
       -c\
       -o "$c".M1\
       "$c".E
else
    sh -x $MESCC\
       -c\
       $CPPFLAGS\
       $MESCCFLAGS\
       -o "$c".M1\
       "$c".c
fi

$M1\
    $M1FLAGS\
    -f stage0/x86.M1\
    -f "$c".M1\
    -o "$c".hex2

if [ -z "$NOLINK" ]; then
    $BLOOD_ELF\
        -f stage0/x86.M1\
        -f "$c".M1\
        -f $LIBC-mes.M1\
        -o "$c".blood-elf-M1
    $M1\
        $M1FLAGS\
        -f "$c".blood-elf-M1\
        -o "$c".blood-elf-hex2
    $HEX2\
        $HEX2FLAGS\
        -f stage0/elf32-header.hex2\
        -f lib/crt1.hex2\
        -f $LIBC-mes.hex2\
        -f "$c".hex2\
        -f "$c".blood-elf-hex2\
        --exec_enable\
        -o "$c".mes-out
fi
