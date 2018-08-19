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

#include <libmes-mini.h>

enum type_t {TCHAR, TCLOSURE, TCONTINUATION, TFUNCTION, TKEYWORD, TMACRO, TNUMBER, TPAIR, TREF, TSPECIAL, TSTRING, TSYMBOL, TVALUES, TVECTOR, TBROKEN_HEART};

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
identity (int i)
{
  return i;
}

int
main ()
{
  int i = 0;
  int f = 0;
  int t = 1;
  int one = t;
  char *p = "mes";

  puts ("\n");
  puts ("t: if (strlen (\"\"))\n");
  if (strlen ("")) return 1;

  puts ("t: if (strlen (p) != 3)\n");
  if (strlen (p) != 3) return 1;

  puts ("t: if (!strlen (\".\"))\n");
  if (!strlen (".")) return 1;

  puts ("t: identity (p[i]) != 'm'\n");
  if (identity (p[i]) != 'm') return identity (p[i]);

  puts ("t: inc (0)\n");
  if (inc (0) != 1) return 1;

  puts ("t: inc (inc (0))\n");
  if (inc (inc (0)) != 2) return 1;

  puts ("t: inc (inc (inc (0)))\n");
  if (inc (inc (inc (0))) != 3) return 1;

  puts ("t: add (1, 2)\n");
  if (add (1, 2) != 3) return 1;

  puts ("t: add (inc (0), inc (1))\n");
  if (add (inc (0), inc (1)) != 3) return 1;

  puts ("t: add (TSTRING, 3)\n");
  if (add (TSTRING, 3) != 13) return 1;

  puts ("t: add (inc (inc (0)), inc (inc (1)))\n");
  if (add (inc (inc (0)), inc (inc (1))) != 5) return 1;

  puts ("t: if (strlen (\".\"))\n");
  if (strlen (".")) goto ok1;
  return 1;
 ok1:

  puts ("t: if (strlen (p) == 3)\n");
  if (strlen (p) == 3) goto ok2;
 ok2:

  return 0;
}
