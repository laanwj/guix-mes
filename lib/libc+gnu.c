/* -*-comment-start: "//";comment-end:""-*-
 * GNU Mes --- Maxwell Equations of Software
 * Copyright © 2018 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
 *
 * This file is part of GNU Mes.
 *
 * GNU Mes is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or (at
 * your option) any later version.
 *
 * GNU Mes is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with GNU Mes.  If not, see <http://www.gnu.org/licenses/>.
 */

#include <libc+tcc.c>

#if __GNU__
#include <linux/gnu.c>
#elif __linux__
#include <linux/gnu.c>
#else
#error both __GNU__ and _linux__ are undefined, choose one
#endif

// m4
#include <stdlib/abort.c>
#include <stdlib/atol.c>
#include <stub/atof.c>
#include <string/bcmp.c>
#include <string/bcopy.c>
#include <string/bzero.c>
#include <stdio/fileno.c>
#include <stub/fpurge.c>
#include <stub/freadahead.c>
#include <string/index.c>
#include <ctype/isalnum.c>
#include <ctype/isalpha.c>
#include <ctype/iscntrl.c>
#include <ctype/isprint.c>
#include <ctype/ispunct.c>
#include <posix/mktemp.c>
#include <stub/pclose.c>
#include <stub/popen.c>
#include <string/rindex.c>
#include <stub/rewind.c>
#include <stub/setbuf.c>
#include <stub/system.c>
#include <string/strerror.c>

// binutils
#include <stdlib/abs.c>
#include <stub/chown.c>
#include <stub/ctime.c>
#include <stdio/fdgets.c>
#include <stdio/feof.c>
#include <stdio/fgets.c>
#include <stub/frexp.c>
#include <stdio/perror.c>
#include <stub/sigsetmask.c>
#include <string/strcspn.c>
#include <string/strncat.c>
#include <string/strpbrk.c>
#include <string/strspn.c>
#include <stub/sys_siglist.c>
#include <stub/umask.c>
#include <stub/utime.c>
#include <stub/fscanf.c>
#include <ctype/isascii.c>
#include <stdlib/bsearch.c>
#include <stub/gmtime.c>
#include <posix/sbrk.c>
#include <string/strdup.c>
#include <posix/raise.c>
#include <stub/strftime.c>
#include <stdlib/mbstowcs.c>
#include <stdio/clearerr.c>
#include <math/fabs.c>

// gcc

#include <stdio/freopen.c>
#include <stub/times.c>
#include <posix/sleep.c>
#include <posix/unsetenv.c>
#include <stdlib/atexit.c>
#include <posix/alarm.c>
#include <stub/getpwnam.c>

#if !__GNU__
#include <stdlib/alloca.c>
#endif

#include <stdlib/__exit.c>
#include <stub/__cleanup.c>

#include <dirent/__getdirentries.c>
#include <dirent/closedir.c>
#include <dirent/opendir.c>
#include <dirent/readdir.c>

// diffutils
#include <posix/execl.c>

// make
#include <stub/getlogin.c>
#include <stub/setvbuf.c>
#include <stub/sigblock.c>
#include <stub/sigaddset.c>
#include <stub/setlocale.c>

// bash
#include <stub/getpwuid.c>
#include <stub/rand.c>
#include <stub/ttyname.c>
