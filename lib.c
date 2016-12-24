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
      if (TYPE (x) != PAIR) return MAKE_NUMBER (-1);
      x = cdr (x);
    }
  return MAKE_NUMBER (n);
}

SCM
list (SCM x) ///((arity . n))
{
  return x;
}

SCM
exit_ (SCM x) ///((name . "exit"))
{
  assert (TYPE (x) == NUMBER);
  exit (VALUE (x));
}

char const*
string_to_cstring (SCM s)
{
  static char buf[1024];
  char *p = buf;
  s = STRING (s);
  while (s != cell_nil)
    {
      *p++ = VALUE (car (s));
      s = cdr (s);
    }
  *p = 0;
  return buf;
}

int
error (char const* msg, SCM x)
{
  fprintf (stderr, msg);
  if (x) stderr_ (x);
  fprintf (stderr, "\n");
  assert(!msg);
}

SCM
assert_defined (SCM x, SCM e)
{
  if (e == cell_undefined) error ("eval: unbound variable: ", x);
  return e;
}

SCM
check_formals (SCM f, SCM formals, SCM args)
{
  int flen = (TYPE (formals) == NUMBER) ? VALUE (formals) : VALUE (length (formals));
  int alen = VALUE (length (args));
  if (alen != flen && alen != -1 && flen != -1)
    {
      char buf[1024];
      sprintf (buf, "apply: wrong number of arguments; expected: %d, got: %d: ", flen, alen);
      error (buf, f);
    }
  return cell_unspecified;
}

SCM
check_apply (SCM f, SCM e)
{
  char const* type = 0;
  if (f == cell_f || f == cell_t) type = "bool";
  if (TYPE (f) == CHAR) type = "char";
  if (TYPE (f) == NUMBER) type = "number";
  if (TYPE (f) == STRING) type = "string";
  if (f == cell_unspecified) type = "*unspecified*";
  if (f == cell_undefined) type =  "*undefined*";

  if (type)
    {
      char buf[1024];
      sprintf (buf, "cannot apply: %s:", type);
      fprintf (stderr, " [");
      stderr_ (e);
      fprintf (stderr, "]\n");
      error (buf, f);
    }
  return cell_unspecified;
}
