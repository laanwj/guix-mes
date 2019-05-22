#! /bin/sh

# GNU Mes --- Maxwell Equations of Software
# Copyright © 2017,2018,2019 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
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

set -e
set -u

V=${V-1}

if [ "$V" = 2 ]; then
    set -x
fi

. ./config.sh
. ${srcdest}build-aux/trace.sh

if $courageous; then
    echo "Applying courage"
    set +e
fi

case "$mes_cpu" in
    arm)
        stage0_cpu=armv7l;;
    x86_64)
        stage0_cpu=amd64;;
    x86)
        stage0_cpu=x86;;
    *)
        stage0_cpu=$mes_cpu;;
esac

trace "HEX2       0exit-42.hex2" $HEX2\
    --LittleEndian\
    --architecture $stage0_cpu\
    --BaseAddress 0x1000000\
    -f ${srcdest}lib/$mes_cpu-mes/elf$mes_bits-0header.hex2\
    -f ${srcdest}lib/$mes_cpu-mes/elf$mes_bits-body-exit-42.hex2\
    -f ${srcdest}lib/$mes_cpu-mes/elf-0footer.hex2\
    --exec_enable\
    -o 0exit-42

trace "TEST       0exit-42"
{ set +e; ./0exit-42; r=$?; set -e; }
[ $r != 42 ] && echo "  => $r"
[ $r == 42 ]

trace "HEX2       exit-42.hex2" $HEX2\
    --LittleEndian\
    --architecture $stage0_cpu\
    --BaseAddress 0x1000000\
    -f ${srcdest}lib/$mes_cpu-mes/elf$mes_bits-header.hex2\
    -f ${srcdest}lib/$mes_cpu-mes/elf$mes_bits-body-exit-42.hex2\
    -f ${srcdest}lib/$mes_cpu-mes/elf$mes_bits-footer-single-main.hex2\
    --exec_enable\
    -o exit-42

trace "TEST       exit-42"
{ set +e; ./exit-42; r=$?; set -e; }
[ $r != 42 ] && echo "  => $r"
[ $r == 42 ]
