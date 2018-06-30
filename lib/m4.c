/* -*-comment-start: "//";comment-end:""-*-
 * Mes --- Maxwell Equations of Software
 * Copyright © 2018 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
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

#include <ctype.h>

#include "stdlib/abort.c"

int
atof (int x)
{
  static int stub = 0;
  if (__mes_debug () && !stub)
    eputs ("atof stub\n");
  stub = 1;
  return 0;
}

int
atol (char const *s)
{
  return atoi (s);
}

int
bcmp (void const *s1, void const *s2, size_t size)
{
  return memcmp (s1, s2, size);
}

void
bcopy (void const *src, void *dest, size_t n)
{
  memmove (dest, src, n);
}

int
bzero (void *block, size_t size)
{
  return memset (block, 0, size);
}

int
fileno (FILE *stream)
{
  return (int)stream;
}

int
fpurge (FILE *stream)
{
  static int stub = 0;
  if (__mes_debug () && !stub)
    eputs ("fpurge stub\n");
  stub = 1;
  errno = 0;
  return 0;
}

size_t
freadahead (FILE *fp)
{
  static int stub = 0;
  if (__mes_debug () && !stub)
    eputs ("freadahead stub\n");
  stub = 1;
  errno = 0;
  return 0;
}

int
index (char const *s, int c)
{
  return strchr (s, c);
}

int
isalnum (int c)
{
  return isdigit (c) || isalpha (c);
}

int
isalpha (int c)
{
  return islower (c) || isupper (c);
}

int
iscntrl (int c)
{
  return c >= 0 && c < 32;
}

int
isprint (int c)
{
  return c >= 32 && c < 127;
}

int
ispunct (int c)
{
  return isprint (c) && !isspace (c) && !isalnum (c);
}

char *
mktemp (char *template)
{
  char *p = strchr (template, '\0');
  int q = (int)template;
  *--p = ((unsigned char)(q >> 4)) % 26 + 'a';
  *--p = ((unsigned char)(q >> 8)) % 26 + 'a';
  *--p = ((unsigned char)(q >> 12)) % 26 + 'a';
  *--p = ((unsigned char)(q >> 16)) % 26 + 'a';
  *--p = ((unsigned char)(q >> 20)) % 26 + 'a';
  *--p = ((unsigned char)(q >> 24)) % 26 + 'a';
  return template;
}

int
pclose (int x)
{
  static int stub = 0;
  if (__mes_debug () && !stub)
    eputs ("pclose stub\n");
  stub = 1;
  errno = 0;
  return 0;
}

int
popen (int x)
{
  static int stub = 0;
  if (__mes_debug () && !stub)
    eputs ("popen stub\n");
  stub = 1;
  errno = 0;
  return 0;
}

int
rindex (char const *s, int c)
{
  return strrchr (s, c);
}

int
rewind (int x)
{
  static int stub = 0;
  if (__mes_debug () && !stub)
    eputs ("rewind stub\n");
  stub = 1;
  errno = 0;
  return 0;
}

int
setbuf (int x)
{
  static int stub = 0;
  if (__mes_debug () && !stub)
    eputs ("setbuf stub\n");
  stub = 1;
  errno = 0;
  return 0;
}

int
system (int x)
{
  static int stub = 0;
  if (__mes_debug () && !stub)
    eputs ("system stub\n");
  stub = 1;
  errno = 0;
  return 0;
}

char *sys_errlist[] = {
  "error 00",
  "error 01",
  "error 02",
  "error 03",
  "error 04",
  "error 05",
  "error 06",
  "error 07",
  "error 08",
  "error 09",
  "error 10",
  "error 11",
  "error 12",
  "error 13",
  "error 14",
  "error 15",
  "error 16",
  "error 17",
  "error 18",
  "error 19",
  "error 20",
  "error 21",
  "error 22",
  "error 23",
  "error 24",
  "error 25",
  "error 26",
  "error 27",
  "error 28",
  "error 29",
  "error 30",
  "error 31",
  "error 32",
  "error 33",
  "error 34",
  "error 35",
  "error 36",
  "error 37",
  "error 38",
  "error 39",
};

int sys_nerr = 39;

char *
strerror (int errnum)
{
  if (__mes_debug ())
    {
      eputs ("strerror errnum="); eputs (itoa (errnum)); eputs ("\n");
    }
  if (errnum > 0 && errnum <= sys_nerr)
    return sys_errlist[errnum];
  return "sterror: unknown error";
}
