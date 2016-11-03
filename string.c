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
string (scm *x) ///((arity . n))
{
  return make_string (x);
}

scm *
string_append (scm *x) ///((arity . n))
{
  scm *p = &scm_nil;
  while (x != &scm_nil)
    {
      scm *s = car (x);
      assert (s->type == STRING);
      p = append2 (p, s->string);
      x = cdr (x);
    }
  return make_string (p);
}

scm *
list_to_string (scm *x)
{
  return make_string (x);
}

scm *
string_length (scm *x)
{
  assert (x->type == STRING);
  return make_number (length (x->string)->value);
}

scm *
string_ref (scm *x, scm *k)
{
  assert (x->type == STRING);
  assert (k->type == NUMBER);
  scm n = {NUMBER, .value=k->value};
  return make_char (list_ref (x->string, &n)->value);
}

scm *
substring (scm *x) ///((arity . n))
{
  assert (x->type == PAIR);
  assert (x->car->type == STRING);
  scm *s = x->car->string;
  assert (x->cdr->car->type == NUMBER);
  int start = x->cdr->car->value;
  int end = length (s)->value;
  if (x->cdr->cdr->type == PAIR) {
    assert (x->cdr->cdr->car->type == NUMBER);
    assert (x->cdr->cdr->car->value <= end);
    end = x->cdr->cdr->car->value;
  }
  int n = end - start;
  while (start--) s = s->cdr;
  scm *p = &scm_nil;
  while (n-- && s != &scm_nil) {
    p = append2 (p, cons (make_char (s->car->value), &scm_nil));
    s = s->cdr;
  }
  return make_string (p);
}

scm *
number_to_string (scm *x)
{
  assert (x->type == NUMBER);
  int n = x->value;
  scm *p = n < 0 ? cons (make_char ('-'), &scm_nil) : &scm_nil;
  do {
    p = cons (make_char (n % 10 + '0'), p);
    n = n / 10;
  } while (n);
  return make_string (p);
}

scm *
string_to_symbol (scm *x)
{
  assert (x->type == STRING);
  return make_symbol (x->string);
}

scm *
symbol_to_string (scm *x)
{
  assert (x->type == SYMBOL);
  return make_string (x->string);
}
