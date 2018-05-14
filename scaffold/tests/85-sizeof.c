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

struct foo
{
  int length;
  char buf[16];
};

struct bar
{
  struct
  {
    int x;
    int y;
    int z;
  };
};

int
main ()
{
  char **p;
  if (sizeof (*p) != 4)
    return 1;
  if (sizeof (**p) != 1)
    return 2;
  puts ("size: "); puts (itoa (sizeof (struct foo))); puts ("\n");
  if (sizeof (struct foo) != 20)
    return 3;
  struct foo f;
  if (sizeof f != 20)
    return 4;
  if (sizeof (struct bar) != 12)
    return 5;
  return 0;
}
