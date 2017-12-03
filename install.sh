#! /bin/sh

set -e

PREFIX=${PREFIX-usr}
MES_PREFIX=${MES_PREFIX-$PREFIX/share/mes}
MES_SEED=${MES_SEED-../mes-seed}
TINYCC_SEED=${TINYCC_SEED-../tinycc-seed}

mkdir -p $PREFIX/bin
cp mes $PREFIX/bin/mes

mkdir -p $PREFIX/lib
mkdir -p $MES_PREFIX/lib
cp $MES_SEED/crt1.M1 $MES_PREFIX/lib/crt1.M1
cp $MES_SEED/libc-mes.M1 $MES_PREFIX/lib/libc-mes.M1
cp $MES_SEED/libc+tcc-mes.M1 $MES_PREFIX/lib/libc+tcc-mes.M1

cp crt1.hex2 $MES_PREFIX/lib/crt1.hex2
cp libc-mes.hex2 $MES_PREFIX/lib/libc-mes.hex2
cp libc+tcc-mes.hex2 $MES_PREFIX/lib/libc+tcc-mes.hex2

cp scripts/mescc.mes $PREFIX/bin/mescc.mes
cp guile/mescc.scm $PREFIX/bin/mescc.scm

mkdir -p $MES_PREFIX
tar -cf- doc guile include lib module scaffold stage0 | tar -xf- -C $MES_PREFIX
