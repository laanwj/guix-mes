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

#if !MINI_MES
#include <fcntl.h>

FILE *g_stdin;
int
getchar ()
{
  return getc (g_stdin);
}
#endif

int
ungetchar (int c)
{
  return ungetc (c, g_stdin);
}

int
peekchar ()
{
  int c = getchar ();
  ungetchar (c);
  return c;
}

SCM
peek_byte ()
{
  return MAKE_NUMBER (peekchar ());
}

SCM
read_byte ()
{
  return MAKE_NUMBER (getchar ());
}

SCM
unread_byte (SCM i)
{
  ungetchar (VALUE (i));
  return i;
}

SCM
write_byte (SCM x) ///((arity . n))
{
  SCM c = car (x);
  SCM p = cdr (x);
  int fd = 1;
  if (TYPE (p) == TPAIR && TYPE (car (p)) == TNUMBER) fd = VALUE (car (p));
#if !MES_MINI
  FILE *f = fd == 1 ? stdout : stderr;
  fputc (VALUE (c), f);
#else
  char cc = VALUE (c);
  write (1, (char*)&cc, fd);
#endif
#if __GNUC__
  assert (TYPE (c) == TNUMBER || TYPE (c) == TCHAR);
#endif
  return c;
}

char const*
string_to_cstring (SCM s)
{
  static char buf[1024];
  char *p = buf;
  s = STRING(s);
  while (s != cell_nil)
    {
      *p++ = VALUE (car (s));
      s = cdr (s);
    }
  *p = 0;
  return buf;
}

int g_depth;

SCM
display_helper (SCM x, int cont, char* sep)
{
  gputs (sep);
  if (g_depth == 0) return cell_unspecified;
  //FIXME:
  //g_depth--;
  g_depth = g_depth - 1;
  
  // eputs ("<display>\n");
  switch (TYPE (x))
    {
    case TCHAR:
      {
        //gputs ("<char>\n");
        gputs ("#\\");
        putchar (VALUE (x));
        break;
      }
    case TFUNCTION:
      {
        gputs ("#<procedure ");
        ///gputs (FUNCTION (x).name ? FUNCTION (x).name : "?");
        char *p = "?";
        if (FUNCTION (x).name != 0)
          p = FUNCTION (x).name;
        gputs (p);
        gputs ("[");
        gputs (itoa (CDR (x)));
        gputs (",");
        gputs (itoa (x));
        gputs ("]>");
        break;
      }
    case TMACRO:
      {
        gputs ("#<macro ");
        display_helper (cdr (x), cont, "");
        gputs (">");
        break;
      }
    case TNUMBER:
      {
        //gputs ("<number>\n");
        gputs (itoa (VALUE (x)));
        break;
      }
    case TPAIR:
      {
        if (!cont) gputs ("(");
        if (x && x != cell_nil) display_ (CAR (x));
        if (CDR (x) && TYPE (CDR (x)) == TPAIR)
          display_helper (CDR (x), 1, " ");
        else if (CDR (x) && CDR (x) != cell_nil)
          {
            if (TYPE (CDR (x)) != TPAIR)
              gputs (" . ");
            display_ (CDR (x));
          }
        if (!cont) gputs (")");
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
            putchar (VALUE (CAR (t)));
            t = CDR (t);
          }
        break;
      }
#endif
    case TSTRING:
#if __NYACC__
      // FIXME
      {}
#endif
    case TSYMBOL:
      {
        SCM t = CAR (x);
        while (t && t != cell_nil)
          {
            putchar (VALUE (CAR (t)));
            t = CDR (t);
          }
        break;
      }
    default:
      {
        //gputs ("<default>\n");
        gputs ("<");
        gputs (itoa (TYPE (x)));
        gputs (":");
        gputs (itoa (x));
        gputs (">");
        break;
      }
    }
  return 0;
}

SCM
display_ (SCM x)
{
  g_depth = 5;
  return display_helper (x, 0, "");
}

SCM
stderr_ (SCM x)
{
  SCM write;
  if (TYPE (x) == TSTRING)
    eputs (string_to_cstring (x));
#if __GNUC__
  else if ((write = assq_ref_env (cell_symbol_write, r0)) != cell_undefined)
    apply (assq_ref_env (cell_symbol_display, r0), cons (x, cons (MAKE_NUMBER (2), cell_nil)), r0);
#endif
  else if (TYPE (x) == TSPECIAL || TYPE (x) == TSTRING || TYPE (x) == TSYMBOL)
    eputs (string_to_cstring (x));
  else if (TYPE (x) == TNUMBER)
    eputs (itoa (VALUE (x)));
  else
    eputs ("core:stderr: display undefined\n");
  return cell_unspecified;
}

SCM
getenv_ (SCM s) ///((name . "getenv"))
{
  char *p = getenv (string_to_cstring (s));
  return p ? MAKE_STRING (cstring_to_list (p)) : cell_f;
}

SCM
open_input_file (SCM file_name)
{
  return MAKE_NUMBER (open (string_to_cstring (file_name), O_RDONLY));
}

SCM
current_input_port ()
{
  return MAKE_NUMBER (fileno (g_stdin));
}

SCM
set_current_input_port (SCM port)
{
  g_stdin = VALUE (port) ? fdopen (VALUE (port), "r") : stdin;
  return current_input_port ();
}

SCM
force_output (SCM p) ///((arity . n))
{
  int fd = 1;
  if (TYPE (p) == TPAIR && TYPE (car (p)) == TNUMBER) fd = VALUE (car (p));
  FILE *f = fd == 1 ? stdout : stderr;
  fflush (f);
  return cell_unspecified;
}
