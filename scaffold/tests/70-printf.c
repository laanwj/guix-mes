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
#include <string.h>

int
test ()
{
  char *s = "mes";
  char c = 'm';
  int i = 3;
  char buf[10];

  printf ("c=%c\n", c);
  sprintf (buf, "c=%c\n", c);
  if (strcmp (buf, "c=m\n")) return 1;

  printf ("i=%d\n", i);
  sprintf (buf, "i=%d\n", i);
  if (strcmp (buf, "i=3\n")) return 1;

  printf ("s=%s\n", s);
  sprintf (buf, "s=%s\n", s);
  if (strcmp (buf, "s=mes\n")) return 1;

  return 0;
}
