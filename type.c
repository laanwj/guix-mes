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

#if !TYPE0

SCM
char_p (SCM x)
{
  return TYPE (x) == CHAR ? cell_t : cell_f;
}

SCM
closure_p (SCM x)
{
  return (TYPE (x) == PAIR && CAR (x) == cell_closure) ? cell_t : cell_f;
}

SCM
car_ (SCM x)
{
  return CAR (x);
}

SCM
cdr_ (SCM x)
{
  return CDR (x);
}

SCM
keyword_p (SCM x)
{
  return TYPE (x) == KEYWORD ? cell_t : cell_f;
}

SCM
macro_p (SCM x)
{
  return TYPE (x) == MACRO ? cell_t : cell_f;
}

SCM
number_p (SCM x)
{
  return TYPE (x) == NUMBER ? cell_t : cell_f;
}

SCM
pair_p (SCM x)
{
  return (TYPE (x) == PAIR && CAR (x) != cell_closure) ? cell_t : cell_f;
}

SCM
ref_p (SCM x)
{
  return TYPE (x) == REF ? cell_t : cell_f;
}

SCM
string_p (SCM x)
{
  return TYPE (x) == STRING ? cell_t : cell_f;
}

SCM
symbol_p (SCM x)
{
  return TYPE (x) == SYMBOL ? cell_t : cell_f;
}

SCM
vector_p (SCM x)
{
  return TYPE (x) == VECTOR ? cell_t : cell_f;
}

SCM
builtin_p (SCM x)
{
  return TYPE (x) == FUNCTION ? cell_t : cell_f;
}

// Non-types

SCM
atom_p (SCM x)
{
  return (TYPE (x) == PAIR ? cell_f : cell_t);
}

SCM
boolean_p (SCM x)
{
  return (x == cell_t || x == cell_f) ? cell_t : cell_f;
}
#endif

SCM make_number (int);
SCM
mes_type_of (SCM x)
{
  return make_number (TYPE (x));
}
