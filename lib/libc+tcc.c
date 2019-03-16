/* -*-comment-start: "//";comment-end:""-*-
 * GNU Mes --- Maxwell Equations of Software
 * Copyright © 2017,2018 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
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

#include <errno.h>
#include <fcntl.h>
#include <limits.h>
#include <setjmp.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <signal.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <unistd.h>

#include <libc.c>

int errno;

#if __GNU__
#include <hurd/tcc.c>
#elif __linux__
#include <linux/tcc.c>
#else
#error both __GNU__ and _linux__ are undefined, choose one
#endif

#if __MESC__ && __i386__
#include <x86-mes/setjmp.c>
#elif __MESC__ && __x86_64__
#include <x86_64-mes/setjmp.c>
#elif __i386__
#include <x86-mes-gcc/setjmp.c>
#elif __x86_64__
#include <x86_64-mes-gcc/setjmp.c>
#else
#error arch not supported
#endif

#include <ctype/islower.c>
#include <ctype/isupper.c>
#include <ctype/tolower.c>
#include <ctype/toupper.c>
#include <mes/search-path.c>
#include <posix/execvp.c>
#include <stdio/fclose.c>
#include <stdio/fdopen.c>
#include <stdio/ferror.c>
#include <stdio/fflush.c>
#include <stdio/fopen.c>
#include <stdio/fprintf.c>
#include <stdio/fread.c>
#include <stdio/fseek.c>
#include <stdio/ftell.c>
#include <stdio/fwrite.c>
#include <stdio/printf.c>
#include <stdio/remove.c>
#include <stdio/snprintf.c>
#include <stdio/sprintf.c>
#include <stdio/sscanf.c>
#include <stdio/vfprintf.c>
#include <stdio/vprintf.c>
#include <stdio/vsnprintf.c>
#include <stdio/vsprintf.c>
#include <stdio/vsscanf.c>
#include <stdlib/calloc.c>
#include <stdlib/qsort.c>
#include <stdlib/strtof.c>
#include <stdlib/strtol.c>
#include <stdlib/strtold.c>
#include <stdlib/strtoll.c>
#include <stdlib/strtoul.c>
#include <stdlib/strtoull.c>
#include <string/memmem.c>
#include <string/memmove.c>
#include <string/strcat.c>
#include <string/strchr.c>
#include <string/strlwr.c>
#include <string/strncpy.c>
#include <string/strrchr.c>
#include <string/strstr.c>
#include <string/strupr.c>
#include <stub/sigaction.c>
#include <stub/ldexp.c>
#include <stub/mprotect.c>
#include <stub/localtime.c>
#include <stub/strtod.c>
#include <stub/sigemptyset.c>
