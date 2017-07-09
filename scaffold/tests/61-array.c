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

#include <mlibc.h>

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

char *env[] = {"foo", "bar", "baz", 0};

int
test (char **e)
{
  int i = 0;

  puts ("\n");
  puts ("a[i] = i-1\n");
  int a[3];
  for (int i=0; i < 3; i++) a[i] = i-1;
  for (int i=0; i < 3; i++) if (a[i] != i-1) return 1;

  puts ("env [");
  puts (itoa ((int)env));
  puts ("]\n");

  puts ("e [");
  puts (itoa ((int)e));
  puts ("]\n");

  puts ("env [0] == \"foo\"\n");
  if (strcmp (env[0], "foo")) return 1;

  puts ("env [1] == \"bar\"\n");
  if (strcmp (env[1], "bar")) return 1;

  puts ("t: **p in *env[]\n");

  char **pp = env;
  while (*pp)
    {
      puts ("pp [");
      puts (itoa ((int)pp));
      puts ("]: ");
      if (*pp) puts (*pp);
      puts ("\n");
      pp++;
      i++;
    }
  if (i != 3) return i;

  pp = env;
  puts ("t: *pp++ == \"foo\"\n");
  if (strcmp (*pp++, "foo")) return 1;

  puts ("t: *pp++ == \"bar\"\n");
  if (strcmp (*pp++, "bar")) return 1;

  char *buf = "hello";
  puts ("t: buf[0]\n");
  if (buf[0] != 'h') return 1;

  puts ("t: buf + 1\n");
  if (*(buf+1) != 'e') return 1;

  char **p = &buf;
  puts ("t: **p\n");
  if (**p != 'h') return 1;

  puts ("t: *(p + 1)\n");
  if (*(*p + 1) != 'e') return 1;

  puts ("t: getenv ()");
  if (!getenv ("PATH")) return 1;

  return 0;
}

int
main (int argc, char *argv[])
{
  return test (env);
}
