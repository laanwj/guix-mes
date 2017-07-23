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

enum type_t {TCHAR, TCLOSURE, TCONTINUATION, TFUNCTION, TKEYWORD, TMACRO, TNUMBER, TPAIR, TREF, TSPECIAL, TSTRING, TSYMBOL, TVALUES, TVECTOR, TBROKEN_HEART};

int
swits (int c)
{
  int x = -1;

  switch (c)
    {
    case TCHAR: {goto next;}
    case 1: {goto next;}
    case 2: {goto next;}
    default: {goto next;}
    }

  return 1;
 next:
  switch (c)
    {
    case 0:
      {
        x = 0;
        c = 34;
        break;
      }
    case -1:
    case 1:
      x = 1;
      break;
    default:
      x = 2;
      x = 2;
      break;
    }
  return x;
}

int
test ()
{
  puts ("\n");
  puts ("t: switch 0\n");
  if (swits (0) != 0) return swits (0);

  puts ("t: switch 1\n");
  if (swits (1) != 1) return 1;

  puts ("t: switch -1\n");
  if (swits (-1) != 1) return 1;

  puts ("t: switch -1\n");
  if (swits (-2) != 2) return 1;

  return 0;
}
