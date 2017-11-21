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

#include "30-test.i"

char g_arena[4] = "XXX";
char *g_chars = g_arena;

int
test ()
{
  puts ("X\n");
  if (*g_chars != 'X') return 1;
  g_arena[0] = 'A';
  puts ("A\n");
  if (*g_chars != 'A') return 1;

  puts ("*x A\n");
  char *x = g_arena;
  if (*x != 'A') return 1;

  puts ("*x++ A\n");
  if (*x++ != 'A') return 1;

  puts ("t: *x++ != 'C'\n");
  *x++ = 'C';
  if (g_chars[1] != 'C') return 1;

  return 0;
}
