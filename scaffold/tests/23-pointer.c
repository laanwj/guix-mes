/* -*-comment-start: "//";comment-end:""-*-
 * Mes --- Maxwell Equations of Software
 * Copyright © 2017 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
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

#include "00-test.i"

char *g_hello = "hello";
char g_arena[4] = "XXX";
char *g_chars = g_arena;

struct foo {
  int fd;
};

struct foo *file;

int
test ()
{
  if (*g_hello != 'h') return 1;
  if (g_hello[0] != 'h') return 2;
  if (g_chars[0] != 'X') return 3;
  if (*g_chars != 'X') return 4;

  g_arena[0] = 'A';
  if (*g_chars != 'A') return 5;
  char *x = g_arena;
  if (*x++ != 'A') return 5;
  *x++ = 'C';
  if (g_chars[1] != 'C') return 7;
  if (g_chars[2] != 'X') return 8;
  *--x = 'X';
  if (g_chars[1] != 'X') return 9;

  char **pp = &x;
  if (**pp != 'X') return 10;

  char *p = *pp;
  if (*p != 'X') return 11;

  char ***ppp = &pp;
  if (***ppp != 'X') return 12;

  char **pp2 = *ppp;
  if (**pp2 != 'X') return 13;

  struct foo *f = 0;
  if (f) return 14;
  if (file) return 15;

  return 0;
}
