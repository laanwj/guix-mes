/* -*-comment-start: "//";comment-end:""-*-
 * Mes --- Maxwell Equations of Software
 * Copyright © 2016 Jan Nieuwenhuizen <janneke@gnu.org>
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

scm *
greater_p (scm *x) ///((name . ">") (args . n))
{
  int n = INT_MAX;
  while (x != &scm_nil)
    {
      assert (x->car->type == NUMBER);
      if (x->car->value >= n) return &scm_f;
      n = x->car->value;
      x = cdr (x);
    }
  return &scm_t;
}

scm *
less_p (scm *x) ///((name . "<") (args . n))
{
  int n = INT_MIN;
  while (x != &scm_nil)
    {
      assert (x->car->type == NUMBER);
      if (x->car->value <= n) return &scm_f;
      n = x->car->value;
      x = cdr (x);
    }
  return &scm_t;
}

scm *
is_p (scm *x) ///((name . "=") (args . n))
{
  if (x == &scm_nil) return &scm_t;
  assert (x->car->type == NUMBER);
  int n = x->car->value;
  x = cdr (x);
  while (x != &scm_nil)
    {
      if (x->car->value != n) return &scm_f;
      x = cdr (x);
    }
  return &scm_t;
}

scm *
minus (scm *x) ///((name . "-") (args . n))
{
  scm *a = car (x);
  assert (a->type == NUMBER);
  int n = a->value;
  x = cdr (x);
  if (x == &scm_nil)
    n = -n;
  while (x != &scm_nil)
    {
      assert (x->car->type == NUMBER);
      n -= x->car->value;
      x = cdr (x);
    }
  return make_number (n);
}

scm *
plus (scm *x) ///((name . "+") (args . n))
{
  int n = 0;
  while (x != &scm_nil)
    {
      assert (x->car->type == NUMBER);
      n += x->car->value;
      x = cdr (x);
    }
  return make_number (n);
}

scm *
divide (scm *x) ///((name . "/") (args . n))
{
  int n = 1;
  if (x != &scm_nil) {
    assert (x->car->type == NUMBER);
    n = x->car->value;
    x = cdr (x);
  }
  while (x != &scm_nil)
    {
      assert (x->car->type == NUMBER);
      n /= x->car->value;
      x = cdr (x);
    }
  return make_number (n);
}

scm *
modulo (scm *a, scm *b)
{
  assert (a->type == NUMBER);
  assert (b->type == NUMBER);
  return make_number (a->value % b->value);
}

scm *
multiply (scm *x) ///((name . "*") (args . n))
{
  int n = 1;
  while (x != &scm_nil)
    {
      assert (x->car->type == NUMBER);
      n *= x->car->value;
      x = cdr (x);
    }
  return make_number (n);
}

scm *
logior (scm *x) ///((args . n))
{
  int n = 0;
  while (x != &scm_nil)
    {
      assert (x->car->type == NUMBER);
      n |= x->car->value;
      x = cdr (x);
    }
  return make_number (n);
}
