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
#include <string.h>

struct foo;

struct foo* krak;

typedef struct foo foo_struct;

struct foo
{
  int bar[2];
  char name[10];
};
  
struct foo g_foo;

int a, b;
int i, *j;
int *k = 0, l;

typedef struct baz
{
  int bar;
  //struct baz *f, *g;
  struct baz *f;
  struct baz *g;
} baz;

int
test ()
{
  foo_struct f;
  f.bar[0] = 0x22;
  f.bar[1] = 0x34;

  printf ("eentje: %d\n", f.bar[0]);
  printf ("tweetje: %d\n", f.bar[1]);

  int *pf = &f;
  if (*pf != 0x22) return 1;
  if (*(pf + 1) != 0x34) return 2;

  struct foo *g = &f;
  printf ("punter eentje: %d\n", g->bar[0]);
  printf ("punter tweetje: %d\n", g->bar[1]);

  char *strings[] = { "one\n", "two\n", "three\n", NULL };
  char **p = strings;
  while (*p) puts (*p++);

  strcpy (f.name, "hallo\n");
  puts (f.name);

  struct foo fu;
  strcpy (fu.name, "hello\n");
  puts (fu.name);

  strcpy (g_foo.name, "hey\n");
  puts (g_foo.name);

  char buf[10];
  struct foo* s = &buf;
  strcpy (s->name, "hi\n");
  puts (s->name);

  return 0;
}
