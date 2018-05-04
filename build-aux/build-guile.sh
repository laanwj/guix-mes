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

set -ex

export GUILE
GUILE=${GUILE-$(command -v guile)}

SCM_FILES="
language/c99/compiler.scm
language/c99/info.scm
mes/as-i386.scm
mes/as.scm
mes/bytevectors.scm
mes/elf.scm
mes/guile.scm
mes/test.scm
mes/M1.scm"

export srcdir=.
export host=$($GUILE -c "(display %host-type)")
cd guile
$GUILE --no-auto-compile -L . -C . -s ../build-aux/compile-all.scm $SCM_FILES
