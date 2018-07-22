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

#include <libmes.h>
#include <stdint.h>
#include <stdlib.h>

int
abs (int x)
{
  if (x < 0)
    return -x;
  return x;
}

int
chown (char const *file_name, uid_t owner, gid_t group)
{
  static int stub = 0;
  if (__mes_debug () && !stub)
    eputs ("chown stub\n");
  stub = 1;
  errno = 0;
  return 0;
}

int
ctime (int x)
{
  static int stub = 0;
  if (__mes_debug () && !stub)
    eputs ("ctime stub\n");
  stub = 1;
  errno = 0;
  return 0;
}

char *
fdgets (char *s, int count, int fd)
{
  int c = 0;
  char *p = s;
  while (--count > 0 && c != '\n')
    {
      c = fdgetc (fd);
      if (c == EOF)
        break;
      *p++ = c;
    }
  if (p == s && (c == EOF || count == -1))
    return 0;
  *p = 0;
  return s;
}

int
feof (FILE *stream)
{
  char c = fgetc (stream);
  if (c != EOF)
    ungetc (c, stream);
  return c == EOF;
}

char *
fgets (char *s, int count, FILE *stream)
{
  return fdgets (s, count, (int)stream);
}

int
frexp (int x)
{
  static int stub = 0;
  if (__mes_debug () && !stub)
    eputs ("frexp stub\n");
  stub = 1;
  return 0;
}

void
perror (char const *message)
{
  fprintf (stderr, "%s: %s\n", strerror (errno), message);
}

int
sigsetmask (int x)
{
  static int stub = 0;
  if (__mes_debug () && !stub)
    eputs ("sigsetmask stub\n");
  stub = 1;
  errno = 0;
  return 0;
}

size_t
strcspn (char const *string, char const *stopset)
{
  char *p = string;
  while (*p)
    if (strchr (stopset, *p))
      break;
    else
      p++;
  return p - string;
}

char *
strncat (char *to, char const *from, size_t size)
{
  if (size == 0)
    return to;
  char *p = strchr (to , '\0');
  while (*from && size-- > 0)
    *p++ = *from++;
  *p = 0;
  return to;
}

char *
strpbrk (char const *string, char const* stopset)
{
  char *p = string;
  while (*p)
    if (strchr (stopset, *p))
      break;
    else
      p++;
  return p;
}

size_t
strspn (char const *string, char const *skipset)
{
  char *p = string;
  while (*p)
    if (!strchr (skipset, *p))
      break;
    else
      p++;
  return p - string;
}

int
sys_siglist (int x)
{
  static int stub = 0;
  if (__mes_debug () && !stub)
    eputs ("sys_siglist stub\n");
  stub = 1;
  errno = 0;
  return 0;
}

int
umask (int x)
{
  static int stub = 0;
  if (__mes_debug () && !stub)
    eputs ("umask stub\n");
  stub = 1;
  errno = 0;
  return 0;
}

int
utime (int x)
{
  static int stub = 0;
  if (__mes_debug () && !stub)
    eputs ("utime stub\n");
  errno = 0;
  return 0;
}

// binutils-2.10.1
int
fscanf (FILE *stream, char const *template, ...)
{
  static int stub = 0;
  if (__mes_debug () && !stub)
    eputs ("fscan stub\n");
  stub = 1;
  return 0;
}

int
isascii (int c)
{
  return c >= 0 && c <= 127;
}

void *
#if __MESC__
bsearch (void const *key, void const *array, size_t count, size_t size, void (*compare) ())
#else
bsearch (void const *key, void const *array, size_t count, size_t size, comparison_fn_t compare)
#endif
{
  static int stub = 0;
  if (__mes_debug () && !stub)
    eputs ("bsearch stub\n");
  stub = 1;
  return 0;
}

struct tm *
gmtime (time_t const *time)
{
  static int stub = 0;
  if (__mes_debug () && !stub)
    eputs ("gmtime stub\n");
  stub = 1;
  errno = 0;
  return localtime (time);
}

#if __SBRK_CHAR_PTRDIFF
char *
sbrk (ptrdiff_t)
#else
void *
sbrk (intptr_t delta)
#endif
{
  if (delta >= 0)
    return malloc (delta);
  return __brk;
}

// binutils 2.30
char *
strdup (char const *s)
{
  size_t length = strlen (s) + 1;
  char *p = (char*)malloc (length);
  if (p)
    return memcpy (p, s, length);
  return 0;
}

int
raise (int signum)
{
  kill (getpid (), signum);
}

size_t
strftime (char *s, size_t size, char const *template,
          struct tm const *brokentime)
{
  static int stub = 0;
  if (__mes_debug () && !stub)
    eputs ("strftime stub\n");
  stub = 1;
  return template;
}

#if !__MESC__
typedef char wchar_t[];

size_t
mbstowcs (wchar_t *wstring, char const *string,
          size_t size)
{
  static int stub = 0;
  if (__mes_debug () && !stub)
    eputs ("mbstowcs stub\n");
  stub = 1;
  strcpy (wstring, string);
  return strlen (string);
}
#endif

void
clearerr (FILE *stream)
{
  errno = 0;
}

double
fabs (double number)
{
  if (number < 0)
    return -number;
  return number;
}
