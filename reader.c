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

#if _POSIX_SOURCE
#undef fputs
#undef fdputs
#undef fdputc
#endif

SCM
___end_of_mes___ ()
{
  return 0;
}

SCM
read_input_file_env_ (SCM e, SCM a)
{
  if (e == cell_nil) return e;
  return cons (e, read_input_file_env_ (read_env (a), a));
}

SCM
read_input_file_env (SCM a)
{
  r0 = a;
  if (assq_ref_env (cell_symbol_read_input_file, r0) != cell_undefined)
    return apply (cell_symbol_read_input_file, cell_nil, r0);
  return read_input_file_env_ (read_env (r0), r0);
}

int
read_line_comment (int c)
{
  if (c == '\n') return c;
  return read_line_comment (getchar ());
}

SCM
read_word (int c, SCM w, SCM a)
{
  if (c == EOF && w == cell_nil) return cell_nil;
  if (c == '\t') return read_word ('\n', w, a);
  if (c == '\f') return read_word ('\n', w, a);
  if (c == '\n' && w == cell_nil) return read_word (getchar (), w, a);
  if (c == '\n' && VALUE (car (w)) == '.' && cdr (w) == cell_nil) return cell_dot;
  if (c == EOF || c == '\n') return lookup_ (w, a);
  if (c == ' ') return read_word ('\n', w, a);
  if (c == '(' && w == cell_nil) return read_list (a);
  if (c == '(') {ungetchar (c); return lookup_ (w, a);}
  if (c == ')' && w == cell_nil) {ungetchar (c); return cell_nil;}
  if (c == ')') {ungetchar (c); return lookup_ (w, a);}
  if (c == ';') {read_line_comment (c); return read_word ('\n', w, a);}
  return read_word (getchar (), append2 (w, cons (MAKE_CHAR (c), cell_nil)), a);
}

int
eat_whitespace (int c)
{
  while (c == ' ' || c == '\t' || c == '\n' || c == '\f') c = getchar ();
  if (c == ';') return eat_whitespace (read_line_comment (c));
  return c;
}

SCM
read_list (SCM a)
{
  int c = getchar ();
  c = eat_whitespace (c);
  if (c == ')') return cell_nil;
  SCM w = read_word (c, cell_nil, a);
  if (w == cell_dot)
    return car (read_list (a));
  return cons (w, read_list (a));
}

SCM
read_env (SCM a)
{
  return read_word (getchar (), cell_nil, a);
}

SCM
lookup_ (SCM s, SCM a)
{
  if (isdigit (VALUE (car (s))) || (VALUE (car (s)) == '-' && cdr (s) != cell_nil)) {
    SCM p = s;
    int sign = 1;
    if (VALUE (car (s)) == '-') {
      sign = -1;
      p = cdr (s);
    }
    int n = 0;
    while (p != cell_nil && isdigit (VALUE (car (p)))) {
      n *= 10;
      n += VALUE (car (p)) - '0';
      p = cdr (p);
    }
    if (p == cell_nil) return MAKE_NUMBER (n * sign);
  }

  return lookup_symbol_ (s);
}

//FILE *g_stdin;
int
dump ()
{
  eputs ("program r2=");
  display_error_ (r2);
  eputs ("\n");

  r1 = g_symbols;
  gc_push_frame ();
  gc ();
  gc_peek_frame ();
  char *p = (char*)g_cells;
  putc ('M');
  putc ('E');
  putc ('S');
  putc (g_stack >> 8);
  putc (g_stack % 256);
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
    putc (*p++);
  return 0;
}
