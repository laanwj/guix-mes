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

scm *
char_p (scm *x)
{
  return x->type == CHAR ? &scm_t : &scm_f;
}

scm *
macro_p (scm *x)
{
  return x->type == MACRO ? &scm_t : &scm_f;
}

scm *
number_p (scm *x)
{
  return x->type == NUMBER ? &scm_t : &scm_f;
}

scm *
pair_p (scm *x)
{
  return x->type == PAIR ? &scm_t : &scm_f;
}

scm *
ref_p (scm *x)
{
  return x->type == REF ? &scm_t : &scm_f;
}

scm *
string_p (scm *x)
{
  return x->type == STRING ? &scm_t : &scm_f;
}

scm *
symbol_p (scm *x)
{
  return x->type == SYMBOL ? &scm_t : &scm_f;
}

scm *
vector_p (scm *x)
{
  return x->type == VECTOR ? &scm_t : &scm_f;
}

scm *
builtin_p (scm *x)
{
  return x->type == FUNCTION ? &scm_t : &scm_f;
}

// Non-types
scm *
null_p (scm *x)
{
  return x == &scm_nil ? &scm_t : &scm_f;
}

scm *
atom_p (scm *x)
{
  return (x->type == PAIR ? &scm_f : &scm_t);
}

scm *
boolean_p (scm *x)
{
  return (x == &scm_t || x == &scm_f) ? &scm_t : &scm_f;
}
#endif

scm*make_number (int);
scm *
mes_type_of (scm *x)
{
  return make_number (x->type);
}

