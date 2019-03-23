#! /bin/sh

# GNU Mes --- Maxwell Equations of Software
# Copyright © 2019 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
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

set -ex

# We need mes/module/mes/boot-0.scm created
# Create traditional GNU config.h with the needed defines?
./configure

rm -f src/strings.h

################################################################################
# GLIBC build
rm -rf out-glibc
mkdir out-glibc
gcc -c -D WITH_GLIBC=1 -I include -I lib -o out-glibc/libmes.o lib/libmes.c
gcc -c -D WITH_GLIBC=1 -D VERSION=\"0.19\" -D MODULEDIR=\"module\" -D PREFIX=\"/usr/local\" -I include -o out-glibc/mes.o src/mes.c
gcc out-glibc/mes.o out-glibc/libmes.o -o out-glibc/mes

MES_PREFIX=mes out-glibc/mes --help

################################################################################
# Mes C lib build

# To get a i686-unknown-linux-gnu-gcc, you may do:
#    GUIX_PACKAGE_PATH=guix guix environment -l .guix.scm
# or
#    guix environment --ad-hoc -e '(begin (use-modules (gnu packages cross-base)) (list (cross-binutils "i686-unknown-linux-gnu") (cross-gcc "i686-unknown-linux-gnu")))'

rm -rf out-mes
mkdir out-mes
i686-unknown-linux-gnu-gcc -c -g -O0 -nostdinc -fno-builtin -fno-stack-protector -I include -o out-mes/crt1.o lib/linux/x86-mes-gcc/crt1.c
i686-unknown-linux-gnu-gcc -c -g -O0 -nostdinc -fno-builtin -fno-stack-protector -I include -I lib -o out-mes/libc.o lib/libc.c
i686-unknown-linux-gnu-gcc -c -g -O0 -nostdinc -fno-builtin -fno-stack-protector -I include -D VERSION=\"0.19\" -D MODULEDIR=\"module\" -D PREFIX=\"/usr/local\" -o out-mes/mes.o src/mes.c
i686-unknown-linux-gnu-gcc -g -nostdlib -o out-mes/mes out-mes/crt1.o out-mes/mes.o out-mes/libc.o

# Test it, use ./pre-inst-env out-mes/mes --help or simply
MES_PREFIX=mes out-mes/mes --help

################################################################################
# To silence warnings, add
# -Wno-discarded-qualifiers -Wno-int-to-pointer-cast -Wno-pointer-to-int-cast -Wno-pointer-sign -Wno-int-conversion -Wno-incompatible-pointer-types
