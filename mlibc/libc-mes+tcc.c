/* -*-comment-start: "//";comment-end:""-*-
 * Mes --- Maxwell Equations of Software
 * Copyright © 2017 Jan Nieuwenhuizen <janneke@gnu.org>
 *
 * This file is part of Mes.
 *
 * Mes is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or (at
 * your option) any later version.
 *
 * Mes is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with Mes.  If not, see <http://www.gnu.org/licenses/>.
 */

#include <libc-mes.c>
#include <stdio.h>
#include <string.h>
#include <time.h>

void
close ()
{
  asm ("mov____0x8(%ebp),%ebx !8");

  asm ("mov____$i32,%eax SYS_close");
  asm ("int____$0x80");
}

char *
getcwd (char *buf, size_t size)
{
  asm ("mov____0x8(%ebp),%ebx !8");
  asm ("mov____0x8(%ebp),%ecx !12");

  asm ("mov____$i32,%eax SYS_getcwd");
  asm ("int____$0x80");
}

int
fprintf (FILE *stream, char const *format, ...)
{
  return 0;
}

struct tm *
localtime (time_t const *timep)
{
  return 0;
}

void *
memcpy (void *dest, void const *src, size_t n)
{
  return 0;
}

void *
memmove (void *dest, void const *src, size_t n)
{
  return 0;
}

void *
memset (void *s, int c, size_t n)
{
  return 0;
}

int
memcmp (void const *s1, void const *s2, size_t n)
{
  return 0;
}

int
snprintf(char *str,  size_t size,  char const *format, ...)
{
  return 0;
}

char *
strchr (char const *s, int c)
{
  return 0;
}

unsigned long
strtoul (char const *nptr, char **endptr, int base)
{
  return 0;
}

time_t time (time_t *tloc)
{
  return 0;
}
