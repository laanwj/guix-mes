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

FILE *g_stdin;
int
dump ()
{
  r1 = g_symbols;
  SCM frame = cons (r1, cons (r2, cons (r3, cons (r0, cell_nil))));
  stack = cons (frame, stack);
  stack = gc (stack);
  gc_frame (stack);
  char *p = (char*)g_cells;
  fputc ('M', stdout);
  fputc ('E', stdout);
  fputc ('S', stdout);
  fputc (stack >> 8, stdout);
  fputc (stack % 256, stdout);
  for (int i=0; i<g_free.value * sizeof(scm); i++)
    fputc (*p++, stdout);
  return 0;
}

SCM
load_env (SCM a) ///((internal))
{
  r0 =a;
  g_stdin = fopen ("module/mes/read-0.mes", "r");
  g_stdin = g_stdin ? g_stdin : fopen (PREFIX "module/mes/read-0.mes", "r");
  if (!g_function) r0 = mes_builtins (r0);
  r3 = read_input_file_env (r0);
  g_stdin = stdin;
  return r3;
}

SCM
bload_env (SCM a) ///((internal))
{
  g_stdin = fopen ("module/mes/read-0.mo", "r");
  g_stdin = g_stdin ? g_stdin : fopen (PREFIX "module/mes/read-0.mo", "r");
  char *p = (char*)g_cells;
  assert (getchar () == 'M');
  assert (getchar () == 'E');
  assert (getchar () == 'S');
  stack = getchar () << 8;
  stack += getchar ();
  int c = getchar ();
  while (c != EOF)
    {
      *p++ = c;
      c = getchar ();
    }
  g_free.value = (p-(char*)g_cells) / sizeof (scm);
  gc_frame (stack);
  g_symbols = r1;
  g_stdin = stdin;

  r0 = mes_builtins (r0);
  return r3;
}
