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

set -e
. ./config.status
. ${srcdest}build-aux/config.sh

sh ${srcdest}build-aux/test-cc.sh $1

if [ ! "$mesc_p" ]; then
    #FIXME: c&p
    unset CFLAGS CPPFLAGS LDFLAGS gcc_p tcc_p with_glibc_p
    MES=${MES-guile}
    mesc_p=1
    mes_p=1
    mes_arch=x86-mes
    program_prefix=$mes_arch-
    CC="./pre-inst-env mescc"
    sh ${srcdest}build-aux/test-cc.sh $1
fi
