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

struct s
{
  int bar;
  int baz;
};

struct foo
{
  struct s s;
};

struct anon
{
  struct {
    int bar;
    int baz;
  };
};


int
test ()
{
  struct foo f = {1,2};
  f.s.baz = 2; // FIXME
  printf ("f.s.bar=%d\n", f.s.bar);
  if (f.s.bar != 1) return 1;
  printf ("f.s.baz=%d\n", f.s.baz);
  if (f.s.baz != 2) return 2;

  struct anon a = {3,4};
  a.baz = 4; // FIXME
  printf ("a.bar=%d\n", a.bar);
  if (a.bar != 3) return 1;
  printf ("a.baz=%d\n", a.baz);
  if (a.baz != 4) return 1;
  
  return 0;
}
