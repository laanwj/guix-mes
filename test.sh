#! /bin/sh

set -ex

t=${1-t}
rm -f "$t".i686-unknown-linux-gnu-out
rm -f "$t".mes-out

M1=${M1-M1}
HEX2=${HEX2-hex2}
MESCC=${MESCC-sh guile/mescc.scm}

$MESCC -E -o scaffold/tests/$t.E scaffold/tests/$t.c
$MESCC -c -o scaffold/tests/$t.M1 scaffold/tests/$t.E
$M1 --LittleEndian --Architecture=1\
    -f stage0/x86.M1\
    -f scaffold/tests/$t.M1\
    >  scaffold/tests/$t.hex2

$MESCC -E -o lib/crt1.E lib/crt1.c
$MESCC -c -o lib/crt1.M1 lib/crt1.E
$M1 --LittleEndian --Architecture=1 \
    -f stage0/x86.M1\
    -f lib/crt1.M1\
    > lib/crt1.hex2
$MESCC -E -o lib/libc-mes.E lib/libc-mes.c
$MESCC -c -o lib/libc-mes.M1 lib/libc-mes.E
$M1 --LittleEndian --Architecture=1\
    -f stage0/x86.M1\
    -f lib/libc-mes.M1\
    > lib/libc-mes.hex2

$HEX2 --LittleEndian --Architecture=1 --BaseAddress=0x1000000\
      -f stage0/elf32-header.hex2\
      -f lib/crt1.hex2\
      -f lib/libc-mes.hex2\
      -f scaffold/tests/$t.hex2\
      -f stage0/elf32-footer-single-main.hex2\
      > scaffold/tests/$t.mes-out
chmod +x scaffold/tests/$t.mes-out

r=0
set +e
scaffold/tests/$t.mes-out
m=$?

[ $m = $r ]
