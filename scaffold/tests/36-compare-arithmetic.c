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
#include <stdio.h>

int
test ()
{
  puts ("\n");
  puts ("t: 1 + 2\n");
  if (1 + 2 != 3) return 1;

  puts ("t: 2 - 1\n");
  if (2 - 1 != 1) return 1;

  puts ("t: 1 << 3\n");
  if (1 << 3 != 8) return 1;

  puts ("t: 8 >> 3\n");
  if (8 >> 3 != 1) return 1;

  puts ("t: 8 / 4\n");
  if (8 / 4 != 2) return 1;

  return 0;
}
