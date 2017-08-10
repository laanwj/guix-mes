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

char *list[2] = {"foo\n", "bar\n"};

int
test ()
{
  char *pc = 0;
  void *pv = 0;
  int *pi = 0;
  char **ppc = 0;
  void **ppv = 0;
  int **ppi = 0;

  if (++pc != 1) return 1;
  if (++pv != 1) return 2;
  if (++pi != 4) return 3;
  if (++ppc != 4) return 4;
  if (++ppv != 4) return 5;
  if (++ppi != 4) return 6;
  if (pc + 1 != 2) return 7;
  if (pv + 1 != 2) return 8;
  if (pi + 1 != 8) return 9;
  if (ppc + 1 != 8) return 10;
  if (ppv + 1 != 8) return 11;
  if (ppi + 1 != 8) return 12;

  char **p = list;
  ++*p;
  eputs (*p);
  if (strcmp (*p, "oo\n")) return 1;
  --*p;
  eputs (*p);
  if (strcmp (*p, "foo\n")) return 2;

  return 0;
}
