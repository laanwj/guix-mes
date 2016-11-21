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

SCM caar (SCM x) {return car (car (x));}
SCM cadr (SCM x) {return car (cdr (x));}
SCM cdar (SCM x) {return cdr (car (x));}
SCM cddr (SCM x) {return cdr (cdr (x));}
SCM caaar (SCM x) {return car (car (car (x)));}
SCM caadr (SCM x) {return car (car (cdr (x)));}
SCM caddr (SCM x) {return car (cdr (cdr (x)));}
SCM cdadr (SCM x) {return cdr (car (cdr (x)));}
SCM cadar (SCM x) {return car (cdr (car (x)));}
SCM cddar (SCM x) {return cdr (cdr (car (x)));}
SCM cdddr (SCM x) {return cdr (cdr (cdr (x)));}
SCM cadddr (SCM x) {return car (cdr (cdr (cdr (x))));}

SCM
length (SCM x)
{
  int n = 0;
  while (x != cell_nil)
    {
      n++;
      x = cdr (x);
    }
  return make_number (n);
}

SCM
last_pair (SCM x)
{
  while (x != cell_nil && cdr (x) != cell_nil)
    x = cdr (x);
  return x;
}

SCM
list (SCM x) ///((arity . n))
{
  return x;
}

SCM
list_ref (SCM x, SCM k)
{
  assert (type (x) == PAIR);
  assert (type (k) == NUMBER);
  int n = value (k);
  while (n-- && g_cells[x].cdr != cell_nil) x = g_cells[x].cdr;
  return x != cell_nil ? car (x) : cell_undefined;
}

SCM
vector_to_list (SCM v)
{
  SCM x = cell_nil;
  for (int i = 0; i < LENGTH (v); i++) {
    SCM e = VECTOR (v)+i;
    if (type (e) == REF) e = g_cells[e].ref;
    x = append2 (x, cons (e, cell_nil));
  }
  return x;
}

SCM
integer_to_char (SCM x)
{
  assert (type (x) == NUMBER);
  return make_char (value (x));
}

SCM
char_to_integer (SCM x)
{
  assert (type (x) == CHAR);
  return make_number (value (x));
}

SCM
builtin_exit (SCM x)
{
  assert (type (x) == NUMBER);
  exit (value (x));
}
