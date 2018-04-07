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

t=${1-t}
rm -f "$t".i686-unknown-linux-gnu-out
rm -f "$t".mes-out

M1=${M1-M1}
HEX2=${HEX2-hex2}
MESCC=${MESCC-guile/mescc.scm}

sh $MESCC -E -o scaffold/tests/$t.E scaffold/tests/$t.c
sh $MESCC -c -o scaffold/tests/$t.M1 scaffold/tests/$t.E
$M1 --LittleEndian --Architecture=1\
    -f stage0/x86.M1\
    -f scaffold/tests/$t.M1\
    -o scaffold/tests/$t.hex2

# $MESCC -E -o lib/crt1.E lib/crt1.c
# $MESCC -c -o lib/crt1.M1 lib/crt1.E
# $M1 --LittleEndian --Architecture=1 \
#     -f stage0/x86.M1\
#     -f lib/crt1.M1\
#     -o lib/crt1.hex2
# $MESCC -E -o lib/libc-mes.E lib/libc-mes.c
# $MESCC -c -o lib/libc-mes.M1 lib/libc-mes.E
# $M1 --LittleEndian --Architecture=1\
#     -f stage0/x86.M1\
#     -f lib/libc-mes.M1\
#     -o lib/libc-mes.hex2

$HEX2 --LittleEndian --Architecture=1 --BaseAddress=0x1000000\
      -f stage0/elf32-header.hex2\
      -f lib/crt1.hex2\
      -f lib/libc-mes.hex2\
      -f scaffold/tests/$t.hex2\
      -f stage0/elf32-footer-single-main.hex2\
      -o scaffold/tests/$t.mes-out
chmod +x scaffold/tests/$t.mes-out

r=0
set +e
scaffold/tests/$t.mes-out
m=$?

[ $m = $r ]
