#! /bin/sh

# Mes --- Maxwell Equations of Software
# Copyright © 2017,2018 Jan Nieuwenhuizen <janneke@gnu.org>
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

HEX2=${HEX2-hex2}
M1=${M1-M1}
BLOOD_ELF=${BLOOD_ELF-blood-elf}
MES_SEED=${MES_SEED-../mes-seed}

$M1 --LittleEndian --Architecture=1\
    -f stage0/x86.M1\
    -f $MES_SEED/crt1.M1\
    -o crt1.hex2
$M1 --LittleEndian --Architecture=1\
    -f stage0/x86.M1\
    -f $MES_SEED/libc-mes.M1\
    -o libc-mes.hex2
$M1 --LittleEndian --Architecture=1\
    -f stage0/x86.M1\
    -f $MES_SEED/mes.M1\
    -o mes.hex2
$BLOOD_ELF\
    -f stage0/x86.M1\
    -f $MES_SEED/mes.M1\
    -f $MES_SEED/libc-mes.M1\
    -o mes-blood-elf-footer.M1
$M1 --LittleEndian --Architecture=1\
    -f mes-blood-elf-footer.M1\
    -o mes-blood-elf-footer.hex2
$HEX2 --LittleEndian --Architecture=1 --BaseAddress=0x1000000\
      -f stage0/elf32-header.hex2\
      -f crt1.hex2\
      -f libc-mes.hex2\
      -f mes.hex2\
      -f mes-blood-elf-footer.hex2\
      --exec_enable\
      -o src/mes

$M1 --LittleEndian --Architecture=1 -f\
    stage0/x86.M1\
    -f $MES_SEED/libc+tcc-mes.M1\
    -o libc+tcc-mes.hex2

cp crt1.hex2 lib
cp libc-mes.hex2 lib
cp libc+tcc-mes.hex2 lib

# TODO: after building from seed, build from src/mes.c
# build-aux/mes-snarf.scm --mes src/gc.c
# build-aux/mes-snarf.scm --mes src/lib.c
# build-aux/mes-snarf.scm --mes src/math.c
# build-aux/mes-snarf.scm --mes src/mes.c
# build-aux/mes-snarf.scm --mes src/posix.c
# build-aux/mes-snarf.scm --mes src/reader.c
# build-aux/mes-snarf.scm --mes src/vector.c
