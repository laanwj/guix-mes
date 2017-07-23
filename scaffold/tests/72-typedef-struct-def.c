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
  struct foo f;
  struct foo *p;
} bar;


bar baz[2] = {1, 2, 3, 4, 5, 6};

//NYACC
//#define offsetof(type, field) ((size_t) &((type *)0)->field)
#if __MESC__
#define offsetof(type, field) (&((type *)0)->field)
#else
#define offsetof(type, field) ((size_t)&((type *)0)->field)
#endif

int
test ()
{
  foo f = {1};
  printf ("f.i=%d\n", f.i);

  bar b = {1, 2, &f};
  printf ("b.i=%d\n", b.i);

  printf ("b.f.i=%d\n", b.f.i);
  if (b.f.i != 2) return 1;

  printf ("b.p->i=%d\n", b.p->i);
  if (b.p->i != 1) return 1;

  bar* p = &b;
  p->i = 2;
  printf ("p->i=%d\n", b.i);

  p->i++;
  printf ("p->i=%d\n", b.i);

  p->i--;
  printf ("p->i=%d\n", b.i);

  printf ("p->f.i=%d\n", p->f.i);
  if (p->f.i != 2) return 1;

  printf ("p->p->i=%d\n", p->p->i);
  if (p->p->i != 1) return 1;

  bar** pp = &p;
  (*pp)->i = 3;
  printf ("(*pp)->i=%d\n", b.i);

  printf ("sizeof i:%d\n", sizeof (p->i));
  if ((sizeof p->i) != 4) return 1;

  printf ("offsetof g=%d\n", (offsetof (bar ,f)));
  if ((offsetof (bar ,f)) != 4) return 1;

  printf ("(*pp)->b.i=%d\n", (*pp)->f.i);
  if ((*pp)->f.i != 2) return 1;

  if (baz[0].i != 1) return 1;
  printf ("baz[0].f.i=%d\n", baz[0].f.i);
  if (baz[0].f.i != 2) return 1;

  printf ("baz[1].i=%d\n", baz[1].i);
  if (baz[1].i != 4) return 1;
  printf ("baz[1].f.i=%d\n", baz[1].f.i);
  if (baz[1].f.i != 5) return 1;

  bar one = {0};
  printf ("one.i\n", one.i);
  if (one.i != 0) return 1;
  printf ("one.f.i\n", one.f.i);
  if (one.f.i != 0) return 1;

  return 0;
}
