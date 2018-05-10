/* -*-comment-start: "//";comment-end:""-*-
 * Mes --- Maxwell Equations of Software
 * Copyright © 2018 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
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


int puts (char const*);
#include <string.h>
char global_arena[10];
int global_i = 1;
int global_unitialized;
char* global_string = "foo";
char global_array[8] = "XXX";
char *global_chars = global_array;
typedef int SCM;
enum type_t {TCHAR};
char *env[] = {"foo", "bar", "baz", 0};
char *list[2] = {"foo\n", "bar\n"};

struct foo {int length; char* string;};
struct foo f = {3, "foo"};
struct foo g_foes[2];
int g_foe;

struct anon {struct {int bar; int baz;};};

struct here {int and;} there;

int
test (struct foo* p)
{
  struct foo *g = &f;
  g[0].length = 0;
  p[0].length = 0;
}

int
main (int argc, char* argv[])
{
  int i;
  int j = 1;
  int k, l = 1;
  if (j != 1)
    return 1;
  if (l != 1)
    return 2;
  if (global_i != 1)
    return 3;
  global_arena[1] = 0;
  if (global_i != 1)
    return 4;
  if (global_unitialized != 0)
    return 5;
  if (strcmp (global_string, "foo"))
    return 6;
  char *s = "bar";
  if (strcmp (s, "bar"))
    return 7;
  if (*global_array != 'X')
    return 8;
  if (*global_chars != 'X')
    return 9;
  SCM x = 0;
  if (x != 0)
    return 9;
  if (TCHAR != 0)
    return 11;
  if (strncmp (argv[0], "scaffold/test", 5))
    return 12;
  if (strcmp (env[0], "foo"))
    return 13;
  if (strcmp (env[2], "baz"))
    return 14;
  if (env[3])
    return 15;
  if (f.length != 3)
    return 16;
  if (strcmp (f.string, "foo"))
    return 17;
  struct foo g = {4, "baar"};
  if (g.length != 4)
    return 18;
  if (strcmp (g.string, "baar"))
    return 19;
  struct foo f = {3, "foo"};
  g_foes[0] = f;
  g_foes[1] = f;
  if (g_foe)
    return 20;
  char *strings[] = { "one\n", "two\n", "three\n", 0 };
  char **p = strings;
  while (*p) puts (*p++);
  if (strcmp (strings[1], "two\n"))
    return 21;
  p = list;
  struct anon a = {3,4};
  eputs ("bar:"); eputs (itoa (a.bar)); eputs ("\n");
  eputs ("baz:"); eputs (itoa (a.baz)); eputs ("\n");
  if (a.bar != 3) return 22;
  if (a.baz != 4) return 23;

  i = 1;
  int lst[6] = {-1, 1 - 1, i, 2, 3};
  for (int i = 0; i < 4; i++)
    {
      puts ("i: "); puts (itoa (lst[i])); puts ("\n");
      if (lst[i+1] != i)
        return 30 + i;
    }

  return 0;
}
