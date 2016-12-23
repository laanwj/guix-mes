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
string_to_symbol (SCM x)
{
  assert (TYPE (x) == STRING);
  return STRING (x) == cell_nil ? cell_nil : make_symbol (STRING (x));
}

SCM
symbol_to_string (SCM x)
{
  assert (TYPE (x) == SYMBOL);
  return MAKE_STRING (STRING (x));
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
