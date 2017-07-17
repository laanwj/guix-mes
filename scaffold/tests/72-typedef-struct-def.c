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

typedef struct foo
{
  int i;
} foo;

typedef struct
{
  int i;
} bar;

int
test ()
{
  foo f = {1};
  printf ("f.i=%d\n", f.i);

  bar b = {1};
  printf ("b.i=%d\n", b.i);
  bar* p = &b;
  p->i = 2;
  printf ("p->i=%d\n", b.i);

  p->i++;
  printf ("p->i=%d\n", b.i);

  p->i--;
  printf ("p->i=%d\n", b.i);


  bar** pp = &p;
  (*pp)->i = 3;
  printf ("(*pp)->i=%d\n", b.i);

  return 0;
}
