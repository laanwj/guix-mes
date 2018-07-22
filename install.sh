#! /bin/sh

set -e

. build-aux/trace.sh

# use bash or lose if pipes fail
[ -n "$BASH" ] && set -o pipefail

SHELL=${SHELL-$(command -v sh)}
prefix=${prefix-/usr/local}
top_builddest=${top_builddest-}
top_builddir=${top_buildir-.}
abs_top_srcdir=${abs_top_srcdir-$PWD}
abs_top_builddir=${abs_top_builddir-$PWD}

MES_PREFIX=${MES_PREFIX-$prefix/share/mes}
MES_SEED=${MES_SEED-../MES-SEED}
TINYCC_SEED=${TINYCC_SEED-../TINYCC-SEED}

GUILE=${GUILE-$(command -v guile)} || true
if [ -z "$GUILE" -o "$GUILE" = true ]; then
    GUILE_EFFECTIVE_VERSION=${GUILE_EFFECTIVE_VERSION-2.2}
else
    GUILE_EFFECTIVE_VERSION=${GUILE_EFFECTIVE_VERSION-$(guile -c '(display (effective-version))')}
fi
bindir=${bindir-$prefix/bin}
datadir=${datadir-$prefix/share}
docdir=${docdir-$datadir/doc/mes-$VERSION}
infodir=${infodir-$datadir/info}
mandir=${mandir-$datadir/man}
moduledir=${moduledir-$datadir/mes/module}
guile_site_dir=${guile_site_dir-$prefix/share/guile/site/$GUILE_EFFECTIVE_VERSION}
guile_site_ccache_dir=${guile_site_ccache_dir-$prefix/lib/guile/$GUILE_EFFECTIVE_VERSION/site-ccache}

mkdir -p $DESTDIR$bindir
cp ${top_builddest}src/mes $DESTDIR$bindir/mes
cp ${top_builddest}scripts/mescc $DESTDIR$bindir/mescc

sed \
    -e "s,^#! /bin/sh,#! $SHELL," \
    scripts/diff.scm > $DESTDIR$bindir/diff.scm
chmod -w+x $DESTDIR$bindir/diff.scm

mkdir -p $docdir

if [ -n "$PERL" -a -n "$GIT" ]\
       && $PERL -v > /dev/null\
       && $GIT --status > /dev/null; then
    $PERL build-aux/gitlog-to-changelog --srcdir=. > ${top_builddest}ChangeLog
fi

cp\
    AUTHORS\
    BOOTSTRAP\
    COPYING\
    HACKING\
    INSTALL\
    NEWS\
    README\
    $DESTDIR$docdir

if [ -f ${top_builddest}ChangeLog ]; then
    cp ${top_builddest}ChangeLog $DESTDIR$docdir
else
    cp ChangeLog $DESTDIR$docdir
fi

if [ -f ${top_builddest}ChangeLog ]; then
    cp ${top_builddest}ChangeLog $docdir
else
    cp ChangeLog $docdir
fi

mkdir -p $DESTDIR$MES_PREFIX
tar -cf- include lib | tar -xf- -C $DESTDIR$MES_PREFIX
tar -cf- scaffold --exclude='*.gcc*' --exclude='*.mes*' | tar -xf- -C $DESTDIR$MES_PREFIX
tar -cf- ${top_builddest}lib | tar -xf- -C $DESTDIR$MES_PREFIX
tar -cf- --exclude='*.go' module | tar -xf- -C $DESTDIR$MES_PREFIX
tar -cf- -C mes module | tar -xf- -C $DESTDIR$MES_PREFIX

mkdir -p $DESTDIR$guile_site_dir
mkdir -p $DESTDIR$guile_site_ccache_dir
tar -cf- -C module --exclude='*.go' . | tar -xf- -C $DESTDIR$guile_site_dir
tar -cf- -C ${top_builddest}module --exclude='*.scm' . | tar -xf- -C $DESTDIR$guile_site_ccache_dir

if [ -f ${top_builddest}doc/mes.info ]; then
    mkdir -p $DESTDIR$infodir
    tar -cf- ${top_builddest}doc/mes.info* doc/images | tar -xf- --strip-components=1 -C $DESTDIR$infodir
    install-info --info-dir=$DESTDIR$infodir ${top_builddest}doc/mes.info
fi

if [ -f ${top_builddest}doc/mes.1 ]; then
    mkdir -p $DESTDIR$mandir/man1
    cp ${top_builddest}doc/mes.1 $DESTDIR$mandir/man1/
fi

if [ -f ${top_builddest}doc/mescc.1 ]; then
    mkdir -p $DESTDIR$mandir/man1
    cp ${top_builddest}doc/mescc.1 $DESTDIR$mandir/man1/
fi
