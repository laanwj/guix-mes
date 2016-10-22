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
string (scm *x) ///((args . n))
{
  char buf[STRING_MAX] = "";
  char *p = buf;
  while (x != &scm_nil)
    {
      scm *s = car (x);
      assert (s->type == CHAR);
      *p++ = s->value;
      x = cdr (x);
    }
  return make_string (buf);
}

scm *
string_append (scm *x) ///((args . n))
{
  char buf[STRING_MAX] = "";

  while (x != &scm_nil)
    {
      scm *s = car (x);
      assert (s->type == STRING);
      strcat (buf, s->name);
      x = cdr (x);
    }
  return make_string (buf);
}

scm *
list_to_string (scm *x)
{
  char buf[STRING_MAX] = "";
  char *p = buf;
  while (x != &scm_nil)
    {
      scm *s = car (x);
      assert (s->type == CHAR);
      *p++ = s->value;
      x = cdr (x);
    }
  *p = 0;
  return make_string (buf);
}

scm *
string_length (scm *x)
{
  assert (x->type == STRING);
  return make_number (strlen (x->name));
}

scm *
string_ref (scm *x, scm *k)
{
  assert (x->type == STRING);
  assert (k->type == NUMBER);
  return make_char (x->name[k->value]);
}

scm *
substring (scm *x) ///((args . n))
{
  assert (x->type == PAIR);
  assert (x->car->type == STRING);
  char const *s = x->car->name;
  assert (x->cdr->car->type == NUMBER);
  int start = x->cdr->car->value;
  int end = strlen (s);
  if (x->cdr->cdr->type == PAIR) {
    assert (x->cdr->cdr->car->type == NUMBER);
    assert (x->cdr->cdr->car->value <= end);
    end = x->cdr->cdr->car->value;
  }
  char buf[STRING_MAX];
  strncpy (buf, s+start, end - start);
  buf[end-start] = 0;
  return make_string (buf);
}

scm *
number_to_string (scm *x)
{
  assert (x->type == NUMBER);
  char buf[STRING_MAX];
  sprintf (buf,"%d", x->value);
  return make_string (buf);
}

scm *
string_to_symbol (scm *x)
{
  assert (x->type == STRING);
  return make_symbol (x->name);
}

scm *
symbol_to_string (scm *x)
{
  assert (x->type == SYMBOL);
  return make_string (x->name);
}
