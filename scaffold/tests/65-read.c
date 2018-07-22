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

#include "30-test.i"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

struct scm {
  int type;
  int car;
  int cdr;
};

char g_arena[84];
#if __MESC__
struct scm *g_cells = g_arena;
#else
struct scm *g_cells = (struct scm*)g_arena;
#endif
char *g_chars = g_arena;

int g = 48;
int
get ()
{
  int i = g;
  g++;
  return i;
}

int
test ()
{
  char *p = (char*)g_chars;
  int i = 0;

  puts ("\n: ");
  puts ("t: read 0123456789\nt: ");
  int c = get ();
  while (i < 10) {
    *p++ = c;
    putchar (c);
    c = get ();
    i++;
  }
  puts ("\n");
  if (strcmp (g_chars, "0123456789")) return 1;

  puts ("t: ungetc ('A') == getchar ()\n");
  ungetc ('A', STDIN);
  if (getchar () != 'A') return 1;
  ungetc (0, STDIN);
  //ungetc ('\1', STDIN);
  ungetc (1, STDIN);
  puts ("t: ungetc ();ungetc ();getchar ();getchar ()\n");
  if (getchar () != 1) return 1;
  //if (getchar () != '\0') return 1;
  if (getchar () != 0) return 1;

  puts ("t: i == 'm'\n");
  char m = 0x1122336d;
  i = m;
  if (i != 'm') return 1;

  return 0;
}
