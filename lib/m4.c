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

int
abort (int x)
{
  eputs ("abort stub\n");
  return 0;
}

int
atof (int x)
{
  eputs ("atof stub\n");
  return 0;
}

int
atol (int x)
{
  eputs ("atol stub\n");
  return 0;
}

int
bcmp (int x)
{
  eputs ("bcmp stub\n");
  return 0;
}

void
bcopy (void const *src, void *dest, size_t n)
{
  return memmove (dest, src, n);
}

int
bzero (int x)
{
  eputs ("bzero stub\n");
  return 0;
}

int
fileno (FILE *stream)
{
  return (int)stream;
}

// void
// __fpurge (FILE *stream)
// {
//   eputs ("__fpurge stub\n");
//   return 0;
// }

int
fpurge (FILE *stream)
{
  eputs ("fpurge stub\n");
  return 0;
}

// size_t
// __freadahead (FILE *fp)
// {
//   eputs ("__freadahead stub\n");
//   return 0;
// }

size_t
freadahead (FILE *fp)
{
  eputs ("freadahead stub\n");
  return 0;
}

int
getc (FILE *stream)
{
  return fgetc ((int)stream);
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
islower (int c)
{
  return c >= 'a' && c <= 'z';
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

int
isupper (int c)
{
  return c >= 'A' && c <= 'Z';
}

int
mktemp (int x)
{
  eputs ("mktemp stub\n");
  return 0;
}

int
pclose (int x)
{
  eputs ("pclose stub\n");
  return 0;
}

int
popen (int x)
{
  eputs ("popen stub\n");
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
  eputs ("rewind stub\n");
  return 0;
}

int
setbuf (int x)
{
  eputs ("setbuf stub\n");
  return 0;
}

int
signal (int x)
{
  eputs ("signal stub\n");
  return 0;
}

int
sys_errlist (int x)
{
  eputs ("sys_errlist stub\n");
  return 0;
}

int
sys_nerr (int x)
{
  eputs ("sys_nerr stub\n");
  return 0;
}

int
system (int x)
{
  eputs ("system stub\n");
  return 0;
}

int
tolower (int x)
{
  eputs ("tolower stub\n");
  return 0;
}
