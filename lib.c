/* -*-comment-start: "//";comment-end:""-*-
 * Mes --- Maxwell Equations of Software
 * Copyright © 2016,2017 Jan Nieuwenhuizen <janneke@gnu.org>
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

//MINI_MES
// SCM
// length (SCM x)
// {
//   int n = 0;
//   while (x != cell_nil)
//     {
//       n++;
//       if (TYPE (x) != TPAIR) return MAKE_NUMBER (-1);
//       x = cdr (x);
//     }
//   return MAKE_NUMBER (n);
// }

SCM fdisplay_ (SCM,FILE*);

int g_depth;

SCM
display_helper (SCM x, int cont, char* sep, FILE *fd)
{
  fputs (sep, fd);
  if (g_depth == 0) return cell_unspecified;
  g_depth = g_depth - 1;

  switch (TYPE (x))
    {
    case TCHAR:
      {
        fputs ("#\\", fd);
        putc (VALUE (x), fd);
        break;
      }
    case TFUNCTION:
      {
        fputs ("#<procedure ", fd);
        char const *p = "?";
        if (FUNCTION (x).name != 0)
          p = FUNCTION (x).name;
        fputs (p, fd);
        fputs ("[", fd);
        fputs (itoa (CDR (x)), fd);
        fputs (",", fd);
        fputs (itoa (x), fd);
        fputs ("]>", fd);
        break;
      }
    case TMACRO:
      {
        fputs ("#<macro ", fd);
        display_helper (cdr (x), cont, "", fd);
        fputs (">", fd);
        break;
      }
    case TNUMBER:
      {
        fputs (itoa (VALUE (x)), fd);
        break;
      }
    case TPAIR:
      {
        if (!cont) fputs ("(", fd);
        if (x && x != cell_nil) fdisplay_ (CAR (x), fd);
        if (CDR (x) && TYPE (CDR (x)) == TPAIR)
          display_helper (CDR (x), 1, " ", fd);
        else if (CDR (x) && CDR (x) != cell_nil)
          {
            if (TYPE (CDR (x)) != TPAIR)
              fputs (" . ", fd);
            fdisplay_ (CDR (x), fd);
          }
        if (!cont) fputs (")", fd);
        break;
      }
    case TSPECIAL:
#if __NYACC__
      // FIXME
      //{}
      {
        SCM t = CAR (x);
        while (t && t != cell_nil)
          {
            putc (VALUE (CAR (t)), fd);
            t = CDR (t);
          }
        break;
      }
#endif
    case TSTRING:
#if __NYACC__
      // FIXME
      {
        SCM t = CAR (x);
        while (t && t != cell_nil)
          {
            putc (VALUE (CAR (t)), fd);
            t = CDR (t);
          }
        break;
      }
#endif
    case TSYMBOL:
      {
        SCM t = CAR (x);
        while (t && t != cell_nil)
          {
            putc (VALUE (CAR (t)), fd);
            t = CDR (t);
          }
        break;
      }
    default:
      {
        fputs ("<", fd);
        fputs (itoa (TYPE (x)), fd);
        fputs (":", fd);
        fputs (itoa (x), fd);
        fputs (">", fd);
        break;
      }
    }
  return 0;
}

SCM
display_ (SCM x)
{
  g_depth = 5;
  return display_helper (x, 0, "", stdout);
}

SCM
display_error_ (SCM x)
{
  g_depth = 5;
  return display_helper (x, 0, "", stderr);
}

SCM
fdisplay_ (SCM x, FILE *fd) ///((internal))
{
  g_depth = 5;
  return display_helper (x, 0, "", fd);
}

SCM
exit_ (SCM x) ///((name . "exit"))
{
  assert (TYPE (x) == TNUMBER);
  exit (VALUE (x));
}

//MINI_MES
// char const*
// string_to_cstring (SCM s)
// {
//   static char buf[1024];
//   char *p = buf;
//   s = STRING (s);
//   while (s != cell_nil)
//     {
//       *p++ = VALUE (car (s));
//       s = cdr (s);
//     }
//   *p = 0;
//   return buf;
// }

// SCM
// error (SCM key, SCM x)
// {
//   SCM throw;
//   if ((throw = assq_ref_env (cell_symbol_throw, r0)) != cell_undefined)
//     return apply (throw, cons (key, cons (x, cell_nil)), r0);
//   assert (!"error");
// }

SCM
assert_defined (SCM x, SCM e) ///((internal))
{
  if (e == cell_undefined) return error (cell_symbol_unbound_variable, x);
  return e;
}

SCM
check_formals (SCM f, SCM formals, SCM args) ///((internal))
{
  int flen = (TYPE (formals) == TNUMBER) ? VALUE (formals) : VALUE (length (formals));
  int alen = VALUE (length (args));
  if (alen != flen && alen != -1 && flen != -1)
    {
      char buf[1024];
      sprintf (buf, "apply: wrong number of arguments; expected: %d, got: %d: ", flen, alen);
      SCM e = MAKE_STRING (cstring_to_list (buf));
      return error (cell_symbol_wrong_number_of_args, cons (e, f));
    }
  return cell_unspecified;
}

SCM
check_apply (SCM f, SCM e) ///((internal))
{
  char const* type = 0;
  if (f == cell_f || f == cell_t) type = "bool";
  if (f == cell_nil) type = "nil";
  if (f == cell_unspecified) type = "*unspecified*";
  if (f == cell_undefined) type = "*undefined*";
  if (TYPE (f) == TCHAR) type = "char";
  if (TYPE (f) == TNUMBER) type = "number";
  if (TYPE (f) == TSTRING) type = "string";

  if (type)
    {
      char buf[1024];
      sprintf (buf, "cannot apply: %s:", type);
      fprintf (stderr, " [");
      display_error_ (e);
      fprintf (stderr, "]\n");
      SCM e = MAKE_STRING (cstring_to_list (buf));
      return error (cell_symbol_wrong_type_arg, cons (e, f));
    }
  return cell_unspecified;
}

char const*
itoa (int x)
{
  static char buf[10];
  char *p = buf+9;
  *p-- = 0;

  int sign = x < 0;
  if (sign)
    x = -x;
  
  do
    {
      *p-- = '0' + (x % 10);
      x = x / 10;
    } while (x);

  if (sign)
    *p-- = '-';

  return p+1;
}

//FILE *g_stdin;
int
dump ()
{
  fputs ("program r2=", stderr);
  display_error_ (r2);
  fputs ("\n", stderr);

  r1 = g_symbols;
  gc_push_frame ();
  gc ();
  gc_peek_frame ();
  char *p = (char*)g_cells;
  fputc ('M', stdout);
  fputc ('E', stdout);
  fputc ('S', stdout);
  fputc (g_stack >> 8, stdout);
  fputc (g_stack % 256, stdout);
  // See HACKING, simple crafted dump for tiny-mes.c
  if (getenv ("MES_TINY"))
    {
      TYPE (9) = 0x2d2d2d2d;
      CAR (9) = 0x2d2d2d2d;
      CDR (9) = 0x3e3e3e3e;

      TYPE (10) = TPAIR;
      CAR (10) = 11;
      CDR (10) = 12;

      TYPE (11) = TCHAR;
      CAR (11) = 0x58585858;
      CDR (11) = 65;

      TYPE (12) = TPAIR;
      CAR (12) = 13;
      CDR (12) = 1;

      TYPE (13) = TCHAR;
      CAR (11) = 0x58585858;
      CDR (13) = 66;

      TYPE (14) = 0x3c3c3c3c;
      CAR (14) = 0x2d2d2d2d;
      CDR (14) = 0x2d2d2d2d;

      g_free = 15;
    }
  for (int i=0; i<g_free * sizeof(struct scm); i++)
    fputc (*p++, stdout);
  return 0;
}

SCM
load_env (SCM a) ///((internal))
{
  r0 = a;
  g_stdin = open ("module/mes/read-0.mes", O_RDONLY);
  g_stdin = g_stdin ? g_stdin : open (PREFIX "module/mes/read-0.mes", O_RDONLY);
  if (!g_function) r0 = mes_builtins (r0);
  r2 = read_input_file_env (r0);
  g_stdin = STDIN;
  return r2;
}

SCM
bload_env (SCM a) ///((internal))
{
#if MES_MINI
  g_stdin = fopen ("module/mes/read-0-32.mo", O_RDONLY);
#else
  g_stdin = open ("module/mes/read-0.mo", O_RDONLY);
  g_stdin = g_stdin ? g_stdin : open (PREFIX "module/mes/read-0.mo", O_RDONLY);
#endif

  char *p = (char*)g_cells;
  assert (getchar () == 'M');
  assert (getchar () == 'E');
  assert (getchar () == 'S');
  g_stack = getchar () << 8;
  g_stack += getchar ();
  int c = getchar ();
  while (c != EOF)
    {
      *p++ = c;
      c = getchar ();
    }
  g_free = (p-(char*)g_cells) / sizeof (struct scm);
  gc_peek_frame ();
  g_symbols = r1;
  g_stdin = STDIN;
  r0 = mes_builtins (r0);
  return r2;
}

SCM
xassq (SCM x, SCM a) ///for speed in core only
{
  while (a != cell_nil && x != CDAR (a)) a = CDR (a);
  return a != cell_nil ? CAR (a) : cell_f;
}
