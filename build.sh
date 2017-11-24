#! /bin/sh

set -ex

HEX2=${HEX2-hex2}
M1=${M1-M1}
MES_SEED=${MES_SEED-../mes-seed}

$M1 --LittleEndian --Architecture=1\
    -f stage0/x86.M1\
    -f $MES_SEED/crt1.M1\
    > crt1.hex2
$M1 --LittleEndian --Architecture=1\
    -f stage0/x86.M1\
    -f $MES_SEED/libc-mes.M1\
    > libc-mes.hex2
$M1 --LittleEndian --Architecture=1\
    -f stage0/x86.M1\
    -f $MES_SEED/mes.M1\
    > mes.hex2
$HEX2 --LittleEndian --Architecture=1 --BaseAddress=0x1000000\
      -f stage0/elf32-header.hex2\
      -f crt1.hex2\
      -f libc-mes.hex2\
      -f mes.hex2\
      -f stage0/elf32-footer-single-main.hex2\
      > mes
chmod +x mes

#TODO: after building from seed, build from src/mes.c

$M1 --LittleEndian --Architecture=1 -f\
    stage0/x86.M1\
    -f $MES_SEED/libc-mes+tcc.M1\
    > libc-mes+tcc.hex2
