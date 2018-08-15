/* -*-comment-start: "//";comment-end:""-*-
 * GNU Mes --- Maxwell Equations of Software
 * Copyright © 2017 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
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
#include <stdio.h>

union foo
{
  int i;
};

int
main ()
{
  int f;
  int t = 1;
  int one = t;

  puts ("\n");
  puts ("t: (one == 1) ?\n");
  (one == 1) ? 1 : exit (1);

  puts ("t: (f) ?\n");
  (f) ? exit (2) : 1;

  union foo fu;
  fu.i = 1 ? 0 : 1;

  return 0;
}
