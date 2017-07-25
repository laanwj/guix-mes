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

#include "00-test.i"

char g_arena[4] = "XXX";
char *g_chars = g_arena;

struct foo {
  int fd;
};

struct foo *file;

int
test ()
{
  if (*g_chars != 'X') return 1;
  g_arena[0] = 'A';
  if (*g_chars != 'A') return 2;
  char *x = g_arena;
  if (*x++ != 'A') return 3;
  *x++ = 'C';
  if (g_chars[1] != 'C') return 4;

  char **pp = &x;
  if (**pp != 'X') return 5;

  char *p = *pp;
  if (*p != 'X') return 6;

  char ***ppp = &pp;
  //if (***ppp != 'X') return 7;

  char **pp2 = *ppp;
  if (**pp2 != 'X') return 8;

  struct foo *f = 0;
  if (f) return 9;
  if (file) return 10;

  return 0;
}
