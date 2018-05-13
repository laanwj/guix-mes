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

// struct foo {int length; char* string; struct foo *next;};
// struct foo stack[] = {{20, "foo", 0}, {4, "baaz", 0}, {0, 0, 0}};

struct info {int flag;};
struct foo {int length; char* string; struct info info;};
struct foo stack[] = {{3, "foo", {11}},{4, "baar", {12}}};

int
main ()
{
  puts (stack[0].string); puts ("\n");
  puts (stack[1].string); puts ("\n");
  struct foo* top = &stack[1];
  int i;
  i = (top--)->info.flag;
  top++;
  int j = (--top)->info.flag;
  return i - j - 1;
}
