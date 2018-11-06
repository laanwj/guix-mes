#! /bin/bash

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

set -e

. ./config.status
. ${srcdest}build-aux/config.sh
. ${srcdest}build-aux/trace.sh

tests="

00-zero.scm
01-true.scm
02-symbol.scm
03-string.scm
04-quote.scm
05-list.scm
06-tick.scm
07-if.scm
08-if-if.scm

10-cons.scm
11-list.scm
11-vector.scm
12-car.scm
13-cdr.scm
14-exit.scm
15-display.scm

16-if-eq-quote.scm

20-define.scm
20-define-quoted.scm
20-define-quote.scm

21-define-procedure.scm
22-define-procedure-2.scm
23-begin.scm
24-begin-define.scm
25-begin-define-2.scm
26-begin-define-later.scm
27-lambda-define.scm
28-define-define.scm
29-lambda-define.scm
2a-lambda-lambda.scm
2b-define-lambda.scm
2c-define-lambda-recurse.scm
2d-define-lambda-set.scm
2d-compose.scm
2e-define-first.scm
2f-define-second.scm
2f-define-second-lambda.scm
2g-vector.scm

30-capture.scm
31-capture-define.scm
32-capture-modify-close.scm
32-capture-modify-close.scm
33-procedure-override-close.scm
34-cdr-override-close.scm
35-closure-modify.scm
36-closure-override.scm
37-closure-lambda.scm
38-simple-format.scm
39-global-define-override.scm
3a-global-define-lambda-override.scm

40-define-macro.scm
41-when.scm
42-if-when.scm
43-or.scm
44-or-if.scm
45-pass-if.scm
46-report.scm
47-pass-if-eq.scm
48-let.scm
49-macro-override.scm
4a-define-macro-define-macro.scm
4b-define-macro-define.scm
4c-quasiquote.scm
4d-let-map.scm
4e-let-global.scm
4f-string-split.scm

50-primitive-load.scm
51-module.scm
52-define-module.scm
53-closure-display.scm

60-let-syntax.scm
"

for i in $tests; do
    echo -n $i
    if [ ! -f scaffold/boot/$i ]; then
        echo ' [SKIP]'
        continue;
    fi
    x=$(
        if [ "$MES" = guile -o "$(basename $MES)" = guile ]; then
            trace "TEST       $i.guile" $GUILE -L ${srcdest}module -C module -L . <(echo '(use-modules (mes guile))'; cat scaffold/boot/$i)
        elif [ -z "${i/5[0-9]-*/}" ]; then
            cat scaffold/boot/$i | MES_BOOT=${srcdest}boot-00.scm trace "TEST       $i" $MES 2>&1;
        elif [ -z "${i/6[0-9]-*/}" ]; then
            cat scaffold/boot/$i | MES_BOOT=${srcdest}boot-01.scm trace "TEST       $i" $MES 2>&1;
        else
            MES_BOOT=${srcdest}scaffold/boot/$i trace "TEST       $i" $MES 2>&1;
        fi
     ) \
        && echo ' [OK]' \
        || (r=$?; echo ' [FAIL]'; echo -e "$x"; echo scaffold/boot/$i; exit $r)
done
