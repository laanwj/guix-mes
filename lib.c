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

scm *caar (scm *x) {return car (car (x));}
scm *cadr (scm *x) {return car (cdr (x));}
scm *cdar (scm *x) {return cdr (car (x));}
scm *cddr (scm *x) {return cdr (cdr (x));}
scm *caaar (scm *x) {return car (car (car (x)));}
scm *caadr (scm *x) {return car (car (cdr (x)));}
scm *caddr (scm *x) {return car (cdr (cdr (x)));}
scm *cdadr (scm *x) {return cdr (car (cdr (x)));}
scm *cadar (scm *x) {return car (cdr (car (x)));}
scm *cddar (scm *x) {return cdr (cdr (car (x)));}
scm *cdddr (scm *x) {return cdr (cdr (cdr (x)));}

scm *
length (scm *x)
{
  int n = 0;
  while (x != &scm_nil)
    {
      n++;
      x = cdr (x);
    }
  return make_number (n);
}

scm *
last_pair (scm *x)
{
  while (x != &scm_nil && cdr (x) != &scm_nil)
    x = cdr (x);
  return x;
}

scm *
list (scm *x) ///((args . n))
{
  return x;
}

scm *
list_ref (scm *x, scm *k)
{
  assert (x->type == PAIR);
  assert (k->type == NUMBER);
  int n = k->value;
  while (n-- && x->cdr != &scm_nil) x = x->cdr;
  return x != &scm_nil ? x->car : &scm_undefined;
}

scm *
vector_to_list (scm *v)
{
  scm *x = &scm_nil;
  for (int i = 0; i < v->length; i++) {
    scm *e = &v->vector[i];
    if (e->type == REF) e = e->ref;
    x = append2 (x, cons (e, &scm_nil));
  }
  return x;
}

scm *
integer_to_char (scm *x)
{
  assert (x->type == NUMBER);
  return make_char (x->value);
}

scm *
char_to_integer (scm *x)
{
  assert (x->type == CHAR);
  return make_number (x->value);
}

scm *
builtin_exit (scm *x)
{
  assert (x->type == NUMBER);
  exit (x->value);
}
