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

#include <libmes.h>
#include <stdint.h>

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
  eputs ("chown stub\n");
  return 0;
}

int
ctime (int x)
{
  eputs ("ctime stub\n");
  return 0;
}

int
fcntl (int x)
{
  eputs ("fcntl stub\n");
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
  eputs ("frexp stub\n");
  return 0;
}

int
getgid (int x)
{
  eputs ("getgid stub\n");
  return 0;
}

int
getpid (int x)
{
  eputs ("getpid stub\n");
  return 0;
}

int
getuid (int x)
{
  eputs ("getuid stub\n");
  return 0;
}

int
perror (int x)
{
  eputs ("perror stub\n");
  return 0;
}

void*
sbrk (ptrdiff_t delta)
{
  void *p = malloc (delta);
  if (p <= 0)
    return 0;
  return p+delta;
}

int
setitimer (int which, struct itimerval const *new,
           struct itimerval *old)
{
  eputs ("setitimer stub\n");
  return 0;
}

int
sigsetmask (int x)
{
  eputs ("sigsetmask stub\n");
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
  while (*from && size-- > 1)
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
  eputs ("sys_siglist stub\n");
  return 0;
}

int
umask (int x)
{
  eputs ("umask stub\n");
  return 0;
}

int
utime (int x)
{
  eputs ("utime stub\n");
  return 0;
}
