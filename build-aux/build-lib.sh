#! /bin/sh

# GNU Mes --- Maxwell Equations of Software
# Copyright © 2019 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
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
. ${srcdest}build-aux/cc.sh

trap 'test -f .log && cat .log' EXIT

srcdest=${srcdest-}

libc_mini_SOURCES="
lib/mes/eputs.c
lib/mes/oputs.c
"

if test $mes_libc = mes; then
    libc_mini_SOURCES="$libc_mini_SOURCES
lib/posix/write.c
lib/string/strlen.c
lib/stdlib/puts.c
lib/stdlib/exit.c
lib/$mes_kernel/$mes_cpu-mes-$compiler/mini.c
"
fi

libmes_SOURCES="
$libc_mini_SOURCES
lib/ctype/isnumber.c
lib/mes/abtol.c
lib/mes/itoa.c
lib/mes/ltoa.c
lib/mes/ltoab.c
lib/mes/ultoa.c
lib/mes/utoa.c
lib/mes/eputc.c
lib/mes/fdgetc.c
lib/mes/fdputc.c
lib/mes/fdputs.c
lib/mes/fdungetc.c
lib/mes/mes_open.c
lib/mes/ntoab.c
lib/mes/oputc.c
"

if test $mes_libc = mes; then
    libmes_SOURCES="$libmes_SOURCES
lib/stdlib/atoi.c
lib/ctype/isdigit.c
lib/ctype/isspace.c
lib/ctype/isxdigit.c
"
else
    libmes_SOURCES="$libmes_SOURCES
"
fi

libc_SOURCES="
$libmes_SOURCES
lib/mes/__assert_fail.c
lib/mes/__mes_debug.c
lib/posix/execv.c
lib/posix/getenv.c
lib/posix/isatty.c
lib/posix/read.c
lib/posix/setenv.c
lib/posix/wait.c
lib/stdio/fgetc.c
lib/stdio/fputc.c
lib/stdio/fputs.c
lib/stdio/getc.c
lib/stdio/getchar.c
lib/stdio/putc.c
lib/stdio/putchar.c
lib/stdio/ungetc.c
lib/stdlib/free.c
lib/stdlib/malloc.c
lib/stdlib/realloc.c
lib/string/memchr.c
lib/string/memcmp.c
lib/string/memcpy.c
lib/string/memset.c
lib/string/strcmp.c
lib/string/strcpy.c
lib/string/strncmp.c
"

if test $mes_kernel = linux; then
    libc_SOURCES="$libc_SOURCES
lib/linux/access.c
lib/linux/brk.c
lib/linux/chmod.c
lib/linux/clock_gettime.c
lib/linux/dup.c
lib/linux/dup2.c
lib/linux/execve.c
lib/linux/fork.c
lib/linux/fsync.c
lib/linux/getcwd.c
lib/linux/gettimeofday.c
lib/linux/ioctl.c
lib/linux/open.c
lib/linux/_read.c
lib/linux/time.c
lib/linux/unlink.c
lib/linux/waitpid.c
lib/linux/$mes_cpu-mes-$compiler/syscall.c
"
fi

libtcc1_SOURCES="
lib/libtcc1.c
"

libc_tcc_SOURCES="
$libc_SOURCES
lib/ctype/islower.c
lib/ctype/isupper.c
lib/ctype/tolower.c
lib/ctype/toupper.c
lib/mes/search-path.c
lib/posix/execvp.c
lib/stdio/fclose.c
lib/stdio/fdopen.c
lib/stdio/ferror.c
lib/stdio/fflush.c
lib/stdio/fopen.c
lib/stdio/fprintf.c
lib/stdio/fread.c
lib/stdio/fseek.c
lib/stdio/ftell.c
lib/stdio/fwrite.c
lib/stdio/printf.c
lib/stdio/remove.c
lib/stdio/snprintf.c
lib/stdio/sprintf.c
lib/stdio/sscanf.c
lib/stdio/vfprintf.c
lib/stdio/vprintf.c
lib/stdio/vsnprintf.c
lib/stdio/vsprintf.c
lib/stdio/vsscanf.c
lib/stdlib/calloc.c
lib/stdlib/qsort.c
lib/stdlib/strtof.c
lib/stdlib/strtol.c
lib/stdlib/strtold.c
lib/stdlib/strtoll.c
lib/stdlib/strtoul.c
lib/stdlib/strtoull.c
lib/string/memmem.c
lib/string/memmove.c
lib/string/strcat.c
lib/string/strchr.c
lib/string/strlwr.c
lib/string/strncpy.c
lib/string/strrchr.c
lib/string/strstr.c
lib/string/strupr.c
lib/stub/sigaction.c
lib/stub/ldexp.c
lib/stub/mprotect.c
lib/stub/localtime.c
lib/stub/sigemptyset.c
lib/stub/strtod.c
lib/$mes_cpu-mes-$compiler/setjmp.c
"

if test $mes_kernel = linux; then
    libc_tcc_SOURCES="$libc_tcc_SOURCES
lib/linux/close.c
lib/linux/lseek.c
lib/linux/rmdir.c
lib/linux/stat.c
"
fi

libc_gnu_SOURCES="
$libc_tcc_SOURCES
lib/ctype/isalnum.c
lib/ctype/isalpha.c
lib/ctype/isascii.c
lib/ctype/iscntrl.c
lib/ctype/isprint.c
lib/ctype/ispunct.c
lib/dirent/__getdirentries.c
lib/dirent/closedir.c
lib/dirent/opendir.c
lib/dirent/readdir.c
lib/math/fabs.c
lib/mes/fdgets.c
lib/posix/alarm.c
lib/posix/execl.c
lib/posix/mktemp.c
lib/posix/raise.c
lib/posix/sbrk.c
lib/posix/sleep.c
lib/posix/unsetenv.c
lib/stdio/clearerr.c
lib/stdio/feof.c
lib/stdio/fgets.c
lib/stdio/fileno.c
lib/stdio/freopen.c
lib/stdio/perror.c
lib/stdlib/__exit.c
lib/stdlib/abort.c
lib/stdlib/abs.c
lib/stdlib/alloca.c
lib/stdlib/atexit.c
lib/stdlib/atol.c
lib/stdlib/mbstowcs.c
lib/string/bcmp.c
lib/string/bcopy.c
lib/string/bzero.c
lib/string/index.c
lib/string/rindex.c
lib/string/strcspn.c
lib/string/strdup.c
lib/string/strerror.c
lib/string/strncat.c
lib/string/strpbrk.c
lib/string/strspn.c
lib/stub/__cleanup.c
lib/stub/bsearch.c
lib/stub/chown.c
lib/stub/ctime.c
lib/stub/fpurge.c
lib/stub/freadahead.c
lib/stub/frexp.c
lib/stub/getlogin.c
lib/stub/getpwnam.c
lib/stub/getpwuid.c
lib/stub/gmtime.c
lib/stub/pclose.c
lib/stub/popen.c
lib/stub/rand.c
lib/stub/setbuf.c
lib/stub/setlocale.c
lib/stub/setvbuf.c
lib/stub/sigaddset.c
lib/stub/sigblock.c
lib/stub/sigdelset.c
lib/stub/sigsetmask.c
lib/stub/strftime.c
lib/stub/sys_siglist.c
lib/stub/system.c
lib/stub/times.c
lib/stub/ttyname.c
lib/stub/umask.c
lib/stub/utime.c
"

if test $mes_kernel = linux; then
    libc_gnu_SOURCES="$libc_gnu_SOURCES
lib/linux/chdir.c
lib/linux/fcntl.c
lib/linux/fstat.c
lib/linux/getdents.c
lib/linux/getegid.c
lib/linux/geteuid.c
lib/linux/getgid.c
lib/linux/getpid.c
lib/linux/getppid.c
lib/linux/getrusage.c
lib/linux/getuid.c
lib/linux/kill.c
lib/linux/link.c
lib/linux/lstat.c
lib/linux/mkdir.c
lib/linux/nanosleep.c
lib/linux/pipe.c
lib/linux/rename.c
lib/linux/setgid.c
lib/linux/settimer.c
lib/linux/setuid.c
lib/linux/signal.c
lib/linux/sigprogmask.c
"
fi

mkdir -p $mes_cpu-mes
compile lib/$mes_kernel/$mes_cpu-mes-$compiler/crt1.c
cp crt1.o $mes_cpu-mes
if test -e crt1.s; then
    cp crt1.s $mes_cpu-mes
fi

archive libc-mini.a $libc_mini_SOURCES
cp libc-mini.a $mes_cpu-mes
if test -e libc-mini.s; then
    cp libc-mini.s $mes_cpu-mes
fi

archive libmes.a $libmes_SOURCES
cp libmes.a $mes_cpu-mes
if test -e libmes.s; then
    cp libmes.s $mes_cpu-mes
fi

if test $mes_libc = mes; then
    archive libc.a $libc_SOURCES
    cp libc.a $mes_cpu-mes
    if test -e libc.s; then
        cp libc.s $mes_cpu-mes
    fi
fi

archive libc+tcc.a $libc_tcc_SOURCES
cp libc+tcc.a $mes_cpu-mes
if test -e libc+tcc.s; then
    cp libc+tcc.s $mes_cpu-mes
fi

if $courageous; then
    exit 0
fi

archive libc+gnu.a $libc_gnu_SOURCES
cp libc+gnu.a $mes_cpu-mes
if test -e libc+gnu.s; then
    cp libc+gnu.s $mes_cpu-mes
fi

archive libtcc1.a $libtcc1_SOURCES
cp libtcc1.a $mes_cpu-mes
if test -e libtcc1.s; then
    cp libtcc1.s $mes_cpu-mes
fi

archive libgetopt.a lib/posix/getopt.c
cp libgetopt.a $mes_cpu-mes
if test -e libgetopt.s; then
    cp libgetopt.s $mes_cpu-mes
fi
