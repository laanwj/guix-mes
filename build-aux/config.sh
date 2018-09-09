# GNU Mes --- Maxwell Equations of Software
# Copyright © 2018 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
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

srcdir=${srcdir-.}
top_builddir=${top_builddir-.}
if [ "$V" = 2 ]; then
    echo $0
    echo srcdest=${srcdest}
    echo top_builddir=${top_builddir}
fi

export CC
export CC CFLAGS
export CC32
export CC32_CPPFLAGS
export CC64
export CC64_CPPFLAGS
export CC_CFLAGS
export CC_CPPFLAGS
export CFLAGS
export CPPFLAGS
export GUILE
export GUILE_LOAD_COMPILED_PATH
export GUILE_LOAD_PATH
export HEX2
export HEX2FLAGS
export LIBC
export M1
export M1FLAGS
export MES
export MES_CFLAGS
export MES_CPPFLAGS
export MES_LIBS
export TCC

export MES_DEBUG
export MES_SEED
export MES_ARENA
export COMPILE
export PREPROCESS
export TINYCC_PREFIX
export V

export abs_top_builddir
export abs_top_srcdir
export datadir
export moduledir
export prefix
export srcdest
export srcdir
export top_builddir

MESCC=${MESCC-mescc}
BLOOD_ELF=${BLOOD_ELF-blood-elf}
HEX2=${HEX2-hex2}
M1=${M1-M1}

CC_CPPFLAGS=${CC_CPPFLAGS-"
-D 'VERSION=\"$VERSION\"'
-D 'MODULEDIR=\"$moduledir\"'
-D 'PREFIX=\"$prefix\"'
-I src
-I ${srcdest}src
-I ${srcdest}lib
-I ${srcdest}include
"}

CC_CFLAGS=${CC_CFLAGS-"
--std=gnu99
-O0
-g
"}

CC64_CPPFLAGS=${CC64_CPPFLAGS-"
-D 'VERSION=\"$VERSION\"'
-D 'MODULEDIR=\"$moduledir\"'
-D 'PREFIX=\"$prefix\"'
-I src
-I ${srcdest}src
-I ${srcdest}lib
-I ${srcdest}include
"}

CC64_CFLAGS=${CC64_CFLAGS-"
-std=gnu99
-O0
-fno-builtin
-fno-stack-protector
-g
-m64
-nostdinc
-nostdlib
-Wno-discarded-qualifiers
-Wno-int-to-pointer-cast
-Wno-pointer-to-int-cast
-Wno-pointer-sign
-Wno-int-conversion
-Wno-incompatible-pointer-types
"}

CC32_CPPFLAGS=${CC32_CPPFLAGS-"
-D 'VERSION=\"$VERSION\"'
-D 'MODULEDIR=\"$moduledir\"'
-D 'PREFIX=\"$prefix\"'
-I src
-I ${srcdest}src
-I ${srcdest}lib
-I ${srcdest}include
"}

CC32_CFLAGS=${CC32_CFLAGS-"
-std=gnu99
-O0
-fno-builtin
-fno-stack-protector
-g
-m32
-nostdinc
-nostdlib
-Wno-discarded-qualifiers
-Wno-int-to-pointer-cast
-Wno-pointer-to-int-cast
-Wno-pointer-sign
-Wno-int-conversion
-Wno-incompatible-pointer-types
"}

MES_CPPFLAGS=${MES_CPPFLAGS-"
-D 'VERSION=\"$VERSION\"'
-D 'MODULEDIR=\"$moduledir\"'
-D 'PREFIX=\"$prefix\"'
-I src
-I ${srcdest}src
-I ${srcdest}lib
-I ${srcdest}include
"}

MES_CFLAGS=${MES_CFLAGS-"
"}

MES_CFLAGS=${MES_CFLAGS-"
"}

M1FLAGS=${M1FLAGS-"
--LittleEndian
--Architecture 1
"}

HEX2FLAGS=${HEX2FLAGS-"
--LittleEndian
--Architecture 1
--BaseAddress 0x1000000
"}
