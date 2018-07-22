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

. build-aux/config.sh
. build-aux/trace.sh

GUILE=${GUILE-guile}
MES=${MES-src/mes}
MES_ARENA=${MES_ARENA-100000000}

if ! command -v $GUILE > /dev/null; then
    GUILE=true
fi

set -e

[ "$GUILE" != true ] && MES=guile bash build-aux/check-mes.sh
[ "$MES" != guile ] && bash build-aux/check-mes.sh
bash build-aux/check-boot.sh
bash build-aux/check-mescc.sh
