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

. ./config.status
. ${srcdest}build-aux/config.sh
. ${srcdest}build-aux/trace.sh

set -e

# FIXME?
#mes_program_prefix=boot-$arch-
mes_program_prefix=$program_prefix
trace "HEX2       0exit-42" $HEX2\
      $HEX2FLAGS\
      -f ${srcdest}lib/$mes_arch/elf$bits-0header.hex2\
      -f ${srcdest}lib/$mes_arch/elf$bits-body-exit-42.hex2\
      -f ${srcdest}lib/$mes_arch/elf-0footer.hex2\
      --exec_enable\
      -o lib/$mes_arch/${mes_program_prefix}0exit-42

trace "TEST       lib/$mes_arch/${mes_program_prefix}0exit-42" echo lib/$mes_arch/${mes_program_prefix}0exit-42
{ set +e; lib/$mes_arch/${mes_program_prefix}0exit-42; r=$?; set -e; }
[ $r != 42 ] && echo "  => $r" && exit 1

trace "HEX2       exit-42" $HEX2\
      $HEX2FLAGS\
      -f ${srcdest}lib/$mes_arch/elf$bits-header.hex2\
      -f ${srcdest}lib/$mes_arch/elf$bits-body-exit-42.hex2\
      -f ${srcdest}lib/$mes_arch/elf$bits-footer-single-main.hex2\
      --exec_enable\
      -o lib/$mes_arch/${mes_program_prefix}exit-42

trace "TEST       lib/$mes_arch/${mes_program_prefix}exit-42" echo lib/$mes_arch/${mes_program_prefix}exit-42
{ set +e; lib/$mes_arch/${mes_program_prefix}exit-42; r=$?; set -e; }
[ $r != 42 ] && echo "  => $r" && exit 1


mkdir -p lib/$mes_arch
trace "M1         crt1.S" $M1\
      $M1FLAGS\
      -f ${srcdest}lib/$mes_arch/$arch.M1\
      -f $MES_SEED/$mes_arch/crt1.S\
      -o lib/$mes_arch/crt1.o
trace "M1         libc.S" $M1\
      $M1FLAGS\
      -f ${srcdest}lib/$mes_arch/$arch.M1\
      -f $MES_SEED/$mes_arch/libc.S\
      -o lib/$mes_arch/libc.o
trace "M1         mes.S" $M1\
      --LittleEndian\
      --Architecture 1\
      -f ${srcdest}lib/$mes_arch/$arch.M1\
      -f $MES_SEED/$mes_arch/mes.S\
      -o src/mes.o
trace "BLOOD_ELF  mes.S" $BLOOD_ELF\
      -f ${srcdest}lib/$mes_arch/$arch.M1\
      -f $MES_SEED/$mes_arch/mes.S\
      -f $MES_SEED/$mes_arch/libc.S\
        -o src/mes.S.blood-elf
trace "M1         mes.blood-elf" $M1\
      --LittleEndian\
      --Architecture 1\
      -f src/mes.S.blood-elf\
      -o src/mes.o.blood-elf
trace "HEX2       mes.o" $HEX2\
      $HEX2FLAGS\
      -f ${srcdest}lib/$mes_arch/elf$bits-header.hex2\
      -f lib/$mes_arch/crt1.o\
      -f lib/$mes_arch/libc.o\
      -f src/mes.o\
      -f src/mes.o.blood-elf\
      --exec_enable\
      -o src/${mes_program_prefix}mes
#cp src/${mes_program_prefix}mes src/${program_prefix}mes
trace "M1         libc+tcc.S" $M1\
      $M1FLAGS\
      -f ${srcdest}lib/$mes_arch/$arch.M1\
      -f $MES_SEED/$mes_arch/libc+tcc.S\
      -o lib/$mes_arch/libc+tcc.o
trace "M1         libc+gnu.S" $M1\
      $M1FLAGS\
      -f ${srcdest}lib/$mes_arch/$arch.M1\
      -f $MES_SEED/$mes_arch/libc+gnu.S\
      -o lib/$mes_arch/libc+gnu.o
