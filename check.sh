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

export CC32
export GUILE MES MES_ARENA
export BUILD_DEBUG

GUILE=${GUILE-guile}
MES=${MES-src/mes}
MES_ARENA=${MES_ARENA-100000000}

if ! command -v $GUILE > /dev/null; then
    GUILE=true
fi

set -e

[ "$GUILE" != true ] && MES=guile bash build-aux/check-mes.sh
bash build-aux/check-mes.sh
bash build-aux/check-boot.sh
bash build-aux/check-mescc.sh
