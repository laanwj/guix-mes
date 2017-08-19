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
#include <mlibc.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

unsigned long long
strtoull (char const *p, char **end, int base)
{
  *end = p;
  return _atoi (end, base);
}

int
test ()
{
  char *p = "42foo\n";
  int n = _atoi (&p, 0);
  if (n != 42) return 1;
  eputs (p);
  if (strcmp (p, "foo\n")) return 2;

  p = "2azar\n";
  n = strtoull (p, (char**)&p, 16);  
  if (n != 42) return 3;
  eputs (p);
  if (strcmp (p, "zar\n")) return 4;
  
  return 0;
}
