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

#include "30-test.i"
#include <stdio.h>

char g_arena[10];
char *g_chars = g_arena;

int
test ()
{
  int i = 0;
  char c = 'C';
  char *p = "mes";
  char *x = g_arena;
  char *y = g_chars;

  puts ("\n");
  puts ("t: p[0] != 'm'\n");
  if (p[0] != 'm') return p[0];

  puts ("t: p[i] != 't'\n");
  if (p[i] != 'm') return p[i];

  puts ("t: *g_chars != 'A'\n");
  g_arena[0] = 'A';
  if (*g_chars != 'A') return 1;

  puts ("t: *x != 'A'\n");
  if (*x != 'A') return 1;

  puts ("t: *y != 'A'\n");
  if (*y != 'A') return 1;

  puts ("t: *x != 'Q'\n");
  g_chars[0] = 'Q';
  if (*x != 'Q') return 1;

  puts ("t: *x++ != 'C'\n");
  *x++ = c;
  if (*g_chars != 'C') return 1;

  puts ("t: *g_chars == 'B'\n");
  g_arena[0] = 'B';
  if (*g_chars == 'B') goto ok1;
  return 1;
  ok1:

  puts ("t: *x == 'B'\n");
  x = g_arena;
  if (*x == 'B') goto ok2;
  return 1;
 ok2:

  puts ("t: *y == 'B'\n");
  y = g_chars;
  if (*y == 'B') goto ok3;
  return 1;
 ok3:

  puts ("t: *x == 'R'\n");
  g_chars[0] = 'R';
  if (*x == 'R') goto ok4;
  return 1;
 ok4:

  puts ("t: *x++ == 'C'\n");
  *x++ = c;
  if (*g_chars == 'C') goto ok5;
  return 1;
 ok5:

  return 0;
}