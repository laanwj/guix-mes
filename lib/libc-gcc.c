/* -*-comment-start: "//";comment-end:""-*-
 * Mes --- Maxwell Equations of Software
 * Copyright © 2016,2017 Jan Nieuwenhuizen <janneke@gnu.org>
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

#include <mlibc.h>
#include <stdio.h>

#include <mlibc.h>
#include <stdarg.h>
#include <stdlib.h>
#include <unistd.h>

#include <fcntl.h>
#include <assert.h>

#include <mini-linux-gcc.c>
#include <mini-libc.c>
#include <linux-gcc.c>
#include <libc.c>

#if POSIX

int
putchar (int c)
{
  write (STDOUT, (char*)&c, 1);
  return 0;
}

int ungetc_char = -1;
char ungetc_buf[2];

int
getchar ()
{
  char c;
  int i;
  if (ungetc_char == -1)
    {
      int r = read (g_stdin, &c, 1);
      if (r < 1) return -1;
      i = c;
    }
  else
    i = ungetc_buf[ungetc_char--];

  if (i < 0) i += 256;

  return i;
}

int
fdungetc (int c, int fd)
{
  assert (ungetc_char < 2);
  ungetc_buf[++ungetc_char] = c;
  return c;
}

#endif // POSIX
