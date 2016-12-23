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
string (SCM x) ///((arity . n))
{
  return make_string (x);
}

SCM
string_append (SCM x) ///((arity . n))
{
  SCM p = cell_nil;
  while (x != cell_nil)
    {
      SCM s = car (x);
      assert (TYPE (s) == STRING);
      p = append2 (p, STRING (s));
      x = cdr (x);
    }
  return make_string (p);
}

SCM
list_to_string (SCM x)
{
  return make_string (x);
}

SCM
string_length (SCM x)
{
  assert (TYPE (x) == STRING);
  return MAKE_NUMBER (VALUE (length (STRING (x))));
}

SCM
string_ref (SCM x, SCM k)
{
  assert (TYPE (x) == STRING);
  assert (TYPE (k) == NUMBER);
  VALUE (tmp_num) = VALUE (k);
  return MAKE_CHAR (VALUE (list_ref (STRING (x), tmp_num)));
}

SCM
substring (SCM x) ///((arity . n))
{
  assert (TYPE (x) == PAIR);
  assert (TYPE (car (x)) == STRING);
  SCM s = STRING (car (x));
  assert (TYPE (cadr (x)) == NUMBER);
  int start = VALUE (cadr (x));
  int end = VALUE (length (s));
  if (TYPE (cddr (x)) == PAIR) {
    assert (TYPE (caddr (x)) == NUMBER);
    assert (VALUE (caddr (x)) <= end);
    end = VALUE (caddr (x));
  }
  int n = end - start;
  while (start--) s = cdr (s);
  SCM p = cell_nil;
  while (n-- && s != cell_nil) {
    p = append2 (p, cons (MAKE_CHAR (VALUE (car (s))), cell_nil));
    s = cdr (s);
  }
  return make_string (p);
}

SCM
number_to_string (SCM x)
{
  assert (TYPE (x) == NUMBER);
  int n = VALUE (x);
  SCM p = n < 0 ? cons (MAKE_CHAR ('-'), cell_nil) : cell_nil;
  do {
    p = cons (MAKE_CHAR (n % 10 + '0'), p);
    n = n / 10;
  } while (n);
  return make_string (p);
}

SCM
string_to_symbol (SCM x)
{
  assert (TYPE (x) == STRING);
  return STRING (x) == cell_nil ? cell_nil : make_symbol (STRING (x));
}

SCM
symbol_to_string (SCM x)
{
  assert (TYPE (x) == SYMBOL);
  return make_string (STRING (x));
}

SCM
keyword_to_symbol (SCM x)
{
  assert (TYPE (x) == KEYWORD);
  return make_symbol (STRING (x));
}

SCM
symbol_to_keyword (SCM x)
{
  assert (TYPE (x) == SYMBOL);
  g_cells[tmp_num].value = KEYWORD;
  return make_cell (tmp_num, STRING (x), 0);
}
