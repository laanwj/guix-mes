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

int
test ()
{
  int f = 0;
  int t = 1;
  int one = t;

  puts ("\n");
  puts ("t: if (f)\n");
  if (f) return 1;

  puts ("t: if (one != 1)\n");
  if (one != 1) return 1;

  puts ("t: if (1 != one)\n");
  if (1 != one) return 1;

  puts ("t: if (one > 1)\n");
  if (one > 1) return 1;

  puts ("t: if (one < 0)\n");
  if (one < 0) return 1;

  puts ("t: if (one <= 0)\n");
  if (one <= 0) return 1;

  puts ("t: if (one >= 2)\n");
  if (one >= 2) return 1;

  puts ("t: if (!1)\n");
  if (!1) return 1;

  puts ("t: if (one == 0)\n");
  if (one == 0) return 1;

  puts ("t: if (f != 0)\n");
  if (one != 1) return 1;

  puts ("t: if (1)\n");
  if (1) goto ok0;
  return 1;
 ok0:

  puts ("t: if (0); return 1; else;\n");
  if (0) return 1; else goto ok1;
 ok1:

  puts ("t: if (t)\n");
  if (t) goto ok2;
  return 1;
 ok2:

  puts ("t: if (one > 0)\n");
  if (one > 0) goto ok3;
  return 1;
 ok3:

  puts ("t: if (one < 2)\n");
  if (one < 2) goto ok4;
  return 1;
 ok4:

  puts ("t: if (one >= 0)\n");
  if (one >= 0) goto ok5;
  return 1;
 ok5:

  puts ("t: if (one >= 1)\n");
  if (one >= 0) goto ok6;
  return 1;
 ok6:

  puts ("t: if (one <= 2)\n");
  if (one <= 2) goto ok7;
  return 1;
 ok7:

  puts ("t: if (one <= 1)\n");
  if (one <= 1) goto ok8;
  return 1;
 ok8:

  puts ("t: if (!0)\n");
  if (!0) goto ok9;
  return 1;
 ok9:

  puts ("t: if (one == 1)\n");
  if (one == 1) goto ok10;
  return 1;
 ok10:

  puts ("t: if (one != 0)\n");
  if (one != 0) goto ok11;
  return 1;
 ok11:

  return 0;
}
