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

#include <stdio.h>
#include <string.h>

#include "30-test.i"

int
add (int a, int b)
{
  return a + b;
}

int
inc (int i)
{
  return i + 1;
}

int
test ()
{
  int i;

  puts ("\n");
  puts ("t: 0 < 0\n");
  if (0 < 0) return 1;

  puts ("t: 2 < 1\n");
  if (2 < 1) return 2;

  puts ("t: -1 < -2\n");
  if (-1 < -2) return 3;

  puts ("t: 0 < -1\n");
  if (0 < -1) return 4;

  puts ("t: 0 > 0\n");
  if (0 > 0) return 5;

  puts ("t: 1 > 2\n");
  if (1 > 2) return 6;

  puts ("t: -2 > -1\n");
  if (-2 > -1) return 7;

  puts ("t: -1 > 0\n");
  if (-1 > 0) return 9;

  puts ("t: 1 == inc (0)\n");
  if (1 == inc (0)) goto ok0;
  return 10;
 ok0:

  puts ("t: 0 < inc (0)\n");
  if (0 < inc (0)) goto ok1;
  return 11;
 ok1:

  puts ("t: inc (0) + 2 != 3\n");
  if (inc (0) + inc (1) != 3) return 12;

  puts ("t: 4/2=");
  i = 4 / 2;
  if (i!=2) return 13;
  i += 48;
  putchar (i);
  puts ("\n");

  puts ("t: 3*4=\n");
  i = 3 * 4;
  if (i!=12) return 14;

  puts ("t: i /= 4\n");
  i /= 4;
  if (i!=3) return 15;

  puts ("t: i *= 4\n");
  i *= 4;
  if (i!=12) return 16;

  puts ("t: 1 << 3\n");
  if (1 << 3 != 8) return 1 << 3;

  puts ("t: 3 << 4\n");
  if (3 << 4 != 48) return 3 << 4;

  puts ("t: 48 >> 3\n");
  if (48 >> 4 != 3) return 48 >> 4;

  puts ("t: 10 >> 1\n");
  if (10 >> 1 != 5) return 10 >> 1;

  puts ("t: 1 | 4\n");
  if ((1 | 4) != 5) return 1 | 4;

  i = -3;
  puts ("t: -i\n");
  if (-i != 3) return 22;

  puts ("t: -1 + 2\n");
  if (-1 + 2 != 1) return 23;

  puts ("t: 1 & 3\n");
  if ((1 & 3) != 1) return 24;

  puts ("t: ~0\n");
  if (~0 != -1) return 25;

  puts ("t: 1 | 3\n");
  if ((1 | 2) != 3) return 26;

  puts ("t: ^ 1 \n");
  if ((1 ^ 3) != 2) return 27;

  puts ("t: 3 == 3\n");
  if ((3 == 3) !=  1) return 28;

  puts ("t: 3 != 3\n");
  if ((3 != 3) !=  0) return 29;

  puts ("t: 011 == 15\n");
  if (011 != 9) return 30;

  puts ("t: 0b11 == 3\n");
  if (0b11 != 3) return 31;

  puts ("t: 0x11 == 3\n");
  if (0x11 != 17) return 32;

  return 0;
}
