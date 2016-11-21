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

SCM
greater_p (SCM x) ///((name . ">") (arity . n))
{
  int n = INT_MAX;
  while (x != cell_nil)
    {
      assert (TYPE (car (x)) == NUMBER);
      if (VALUE (car (x)) >= n) return cell_f;
      n = VALUE (car (x));
      x = cdr (x);
    }
  return cell_t;
}

SCM
less_p (SCM x) ///((name . "<") (arity . n))
{
  int n = INT_MIN;
  while (x != cell_nil)
    {
      assert (TYPE (car (x)) == NUMBER);
      if (VALUE (car (x)) <= n) return cell_f;
      n = VALUE (car (x));
      x = cdr (x);
    }
  return cell_t;
}

SCM
is_p (SCM x) ///((name . "=") (arity . n))
{
  if (x == cell_nil) return cell_t;
  assert (TYPE (car (x)) == NUMBER);
  int n = VALUE (car (x));
  x = cdr (x);
  while (x != cell_nil)
    {
      if (VALUE (car (x)) != n) return cell_f;
      x = cdr (x);
    }
  return cell_t;
}

SCM
minus (SCM x) ///((name . "-") (arity . n))
{
  SCM a = car (x);
  assert (TYPE (a) == NUMBER);
  int n = VALUE (a);
  x = cdr (x);
  if (x == cell_nil)
    n = -n;
  while (x != cell_nil)
    {
      assert (TYPE (car (x)) == NUMBER);
      n -= VALUE (car (x));
      x = cdr (x);
    }
  return make_number (n);
}

SCM
plus (SCM x) ///((name . "+") (arity . n))
{
  int n = 0;
  while (x != cell_nil)
    {
      assert (TYPE (car (x)) == NUMBER);
      n += VALUE (car (x));
      x = cdr (x);
    }
  return make_number (n);
}

SCM
divide (SCM x) ///((name . "/") (arity . n))
{
  int n = 1;
  if (x != cell_nil) {
    assert (TYPE (car (x)) == NUMBER);
    n = VALUE (car (x));
    x = cdr (x);
  }
  while (x != cell_nil)
    {
      assert (TYPE (car (x)) == NUMBER);
      n /= VALUE (car (x));
      x = cdr (x);
    }
  return make_number (n);
}

SCM
modulo (SCM a, SCM b)
{
  assert (TYPE (a) == NUMBER);
  assert (TYPE (b) == NUMBER);
  return make_number (VALUE (a) % VALUE (b));
}

SCM
multiply (SCM x) ///((name . "*") (arity . n))
{
  int n = 1;
  while (x != cell_nil)
    {
      assert (TYPE (car (x)) == NUMBER);
      n *= VALUE (car (x));
      x = cdr (x);
    }
  return make_number (n);
}

SCM
logior (SCM x) ///((arity . n))
{
  int n = 0;
  while (x != cell_nil)
    {
      assert (TYPE (car (x)) == NUMBER);
      n |= VALUE (car (x));
      x = cdr (x);
    }
  return make_number (n);
}
