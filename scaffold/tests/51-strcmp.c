/* -*-comment-start: "//";comment-end:""-*-
 * Mes --- Maxwell Equations of Software
 * Copyright © 2017,2018 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
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
#include <assert.h>
#include <stdio.h>
#include <string.h>

int
test ()
{
  char *p = "mes";

  puts ("\n");
  puts ("t: if (strcmp (p, \"foo\"))\n");
  if (!strcmp (p, "foo"))
    return 1;

  puts ("t: if (strcmp (p, \"t.c\\n\"))\n");
  if (strcmp (p, "mes"))
    return 2;

  puts ("t: if (!strcmp (p, \"t.c\\n\"))\n");
  if (!strcmp (p, "mes")) goto ok1;
  return 3;
 ok1:

  puts ("t: if (strcmp (p, \"foo\"))\n");
  if (strcmp (p, "foo")) goto ok2;
  return 4;
 ok2:

  puts ("t: itoa (33) == \"33\"\n");
  if (strcmp (itoa (33), "33"))
    return 5;

  puts ("strcmp (itoa (-1), \"-1\")\n");
  if (strcmp (itoa (-1), "-1"))
    return 6;

  puts ("strcmp (itoa (0), \"0\")\n");
  if (strcmp (itoa (0), "0"))
    return 7;

  puts ("strcmp (itoa (1), \"1\")\n");
  if (strcmp (itoa (1), "1"))
    return 8;

  if (strncmp ("abc", "a", 1))
    return 9;

  if (!strncmp ("abc", "x", 1))
    return 10;

  if (!strncmp ("abc", "", 0))
    return 11;

  return 0;
}
