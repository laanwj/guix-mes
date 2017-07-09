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

struct foo;

typedef struct foo foo_struct;

struct foo
{
  int bar[2];
};
  
int
test ()
{
  //struct foo f;
  foo_struct f;
  f.bar[0] = 0x22;
  f.bar[1] = 0x34;
  printf ("eentje: %d\n", f.bar[0]);
  printf ("tweetje: %d\n", f.bar[1]);

  struct foo *g = &f;
  printf ("punter eentje: %d\n", g->bar[0]);
  printf ("punter tweetje: %d\n", g->bar[1]);

  return 0;
}
