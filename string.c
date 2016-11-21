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
      assert (g_cells[s].type == STRING);
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
  assert (g_cells[x].type == STRING);
  return make_number (value (length (STRING (x))));
}

SCM
string_ref (SCM x, SCM k)
{
  assert (g_cells[x].type == STRING);
  assert (g_cells[k].type == NUMBER);
  g_cells[tmp_num].value = value (k);
  return make_char (value (list_ref (STRING (x), tmp_num)));
}

SCM
substring (SCM x) ///((arity . n))
{
  assert (g_cells[x].type == PAIR);
  assert (g_cells[car (x)].type == STRING);
  SCM s = g_cells[car (x)].string;
  assert (g_cells[cadr (x)].type == NUMBER);
  int start = g_cells[cadr (x)].value;
  int end = g_cells[length (s)].value;
  if (g_cells[cddr (x)].type == PAIR) {
    assert (g_cells[caddr (x)].type == NUMBER);
    assert (g_cells[caddr (x)].value <= end);
    end = g_cells[caddr (x)].value;
  }
  int n = end - start;
  while (start--) s = cdr (s);
  SCM p = cell_nil;
  while (n-- && s != cell_nil) {
    p = append2 (p, cons (make_char (g_cells[car (s)].value), cell_nil));
    s = cdr (s);
  }
  return make_string (p);
}

SCM
number_to_string (SCM x)
{
  assert (g_cells[x].type == NUMBER);
  int n = value (x);
  SCM p = n < 0 ? cons (make_char ('-'), cell_nil) : cell_nil;
  do {
    p = cons (make_char (n % 10 + '0'), p);
    n = n / 10;
  } while (n);
  return make_string (p);
}

SCM
string_to_symbol (SCM x)
{
  assert (g_cells[x].type == STRING);
  return make_symbol (STRING (x));
}

SCM
symbol_to_string (SCM x)
{
  assert (g_cells[x].type == SYMBOL);
  return make_string (STRING (x));
}
