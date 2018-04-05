/* -*-comment-start: "//";comment-end:""-*-
 * Mes --- Maxwell Equations of Software
 * Copyright © 2016,2017,2018 Jan Nieuwenhuizen <janneke@gnu.org>
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

#include <ctype.h>

SCM
read_input_file_env_ (SCM e, SCM a)
{
  if (e == cell_nil)
    return e;
  return cons (e, read_input_file_env_ (read_env (a), a));
}

SCM
read_input_file_env (SCM a)
{
  r0 = a;
#if 0
  if (assq_ref_env (cell_symbol_read_input_file, r0) != cell_undefined)
    return apply (cell_symbol_read_input_file, cell_nil, r0);
#endif
  return read_input_file_env_ (read_env (r0), r0);
}

int
reader_read_line_comment (int c)
{
  if (c == '\n') return c;
  return reader_read_line_comment (getchar ());
}

SCM reader_read_block_comment (int s, int c);
SCM read_hash (int c, SCM w, SCM a);

SCM
reader_read_word_ (int c, SCM w, SCM a)
{
  if (c == EOF && w == cell_nil) return cell_nil;
  if (c == '\t') return reader_read_word_ ('\n', w, a);
  if (c == '\f') return reader_read_word_ ('\n', w, a);
  if (c == '\n' && w == cell_nil) return reader_read_word_ (getchar (), w, a);
  if (c == '\n' && VALUE (car (w)) == '.' && cdr (w) == cell_nil) return cell_dot;
  if (c == ' ') return reader_read_word_ ('\n', w, a);
  if (c == EOF || c == '\n') return reader_lookup_ (w, a);

  if (c == '(' && w == cell_nil) return reader_read_list (a);
  if (c == '(') {ungetchar (c); return reader_lookup_ (w, a);}
  if (c == ')' && w == cell_nil) {ungetchar (c); return cell_nil;}
  if (c == ')') {ungetchar (c); return reader_lookup_ (w, a);}
  if (c == ';') {reader_read_line_comment (c); return reader_read_word_ ('\n', w, a);}

  if (c == '"' && w == cell_nil) return reader_read_string ();
  if (c == '"') {ungetchar (c); return reader_lookup_ (w, a);}
  if (c == ',' && peekchar () == '@') {getchar (); return cons (cell_symbol_unquote_splicing,
                                                                cons (reader_read_word_ (getchar (), w, a),
                                                                      cell_nil));}
  if (c == '\'') return cons (cell_symbol_quote, cons (reader_read_word_ (getchar (), w, a), cell_nil));
  if (c == '`') return cons (cell_symbol_quasiquote, cons (reader_read_word_ (getchar (), w, a), cell_nil));
  if (c == ',') return cons (cell_symbol_unquote, cons (reader_read_word_ (getchar (), w, a), cell_nil));

  if (c == '#' && peekchar () == '!') {c = getchar (); reader_read_block_comment (c, getchar ()); return reader_read_word_ (getchar (), w, a);}
  if (c == '#' && peekchar () == '|') {c = getchar (); reader_read_block_comment (c, getchar ()); return reader_read_word_ (getchar (), w, a);}
  if (c == '#' && peekchar () == 'f') return reader_read_word_ (getchar (), append2 (w, cons (MAKE_CHAR (c), cell_nil)), a);
  if (c == '#' && peekchar () == 't') return reader_read_word_ (getchar (), append2 (w, cons (MAKE_CHAR (c), cell_nil)), a);
  if (c == '#') return read_hash (getchar (), w, a);

  return reader_read_word_ (getchar (), append2 (w, cons (MAKE_CHAR (c), cell_nil)), a);
}

int
eat_whitespace (int c)
{
  while (c == ' ' || c == '\t' || c == '\n' || c == '\f') c = getchar ();
  if (c == ';') return eat_whitespace (reader_read_line_comment (c));
  if (c == '#' && (peekchar () == '!' || peekchar () == '|')) {c=getchar (); reader_read_block_comment (c, getchar ()); return eat_whitespace (getchar ());}
  return c;
}

SCM
reader_read_list (SCM a)
{
  int c = getchar ();
  c = eat_whitespace (c);
  if (c == ')') return cell_nil;
  SCM w = reader_read_word_ (c, cell_nil, a);
  if (w == cell_dot)
    return car (reader_read_list (a));
  return cons (w, reader_read_list (a));
}

SCM
read_env (SCM a)
{
  return reader_read_word_ (getchar (), cell_nil, a);
}

SCM
reader_lookup_ (SCM s, SCM a)
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

SCM
reader_read_block_comment (int s, int c)
{
  if (c == s && peekchar () == '#') return getchar ();
  return reader_read_block_comment (s, getchar ());
}

SCM
read_hash (int c, SCM w, SCM a)
{
  if (c == ',')
    {
      if (peekchar () == '@')
        {
          getchar ();
          return cons (cell_symbol_unsyntax_splicing, cons (reader_read_word_ (getchar (), w, a), cell_nil));
        }
      return cons (cell_symbol_unsyntax, cons (reader_read_word_ (getchar (), w, a), cell_nil));
    }
  if (c == '\'') return cons (cell_symbol_syntax, cons (reader_read_word_ (getchar (), w, a), cell_nil));
  if (c == '`') return cons (cell_symbol_quasisyntax, cons (reader_read_word_ (getchar (), w, a), cell_nil));
  if (c == ':') return MAKE_KEYWORD (CAR (reader_read_word_ (getchar (), cell_nil, a)));
  if (c == 'o') return reader_read_octal ();
  if (c == 'x') return reader_read_hex ();
  if (c == '\\') return reader_read_character ();
  if (c == '(') return list_to_vector (reader_read_list (a));
  if (c == ';') reader_read_word_ (getchar (), w, a); return reader_read_word_ (getchar (), w, a);
  if (c == '!') {reader_read_block_comment (c, getchar ()); return reader_read_word_ (getchar (), w, a);}
  if (c == '|') {reader_read_block_comment (c, getchar ()); return reader_read_word_ (getchar (), w, a);}
  if (c == 'f') return cell_f;
  if (c == 't') return cell_t;

  return reader_read_word_ (getchar (), append2 (w, cons (MAKE_CHAR (c), cell_nil)), a);
}

SCM
reader_read_word (SCM c, SCM w, SCM a)
{
  return reader_read_word_ (VALUE (c), w, a);
}

SCM
reader_read_character ()
{
  int c = getchar ();
  if (c >= '0' && c <= '7'
      && peekchar () >= '0' && peekchar () <= '7')
    {
      c = c - '0';
      while (peekchar () >= '0' && peekchar () <= '7')
        {
          c <<= 3;
          c += getchar () - '0';
        }
    }
  else if (((c >= 'a' && c <= 'z')
            || c == '*')
           && ((peekchar () >= 'a' && peekchar () <= 'z')
               || peekchar () == '*'))
    {
      char buf[10];
      char *p = buf;
      *p++ = c;
      while ((peekchar () >= 'a' && peekchar () <= 'z')
             || peekchar () == '*')
        {
          *p++ = getchar ();
        }
      *p = 0;
      if (!strcmp (buf, "*eof*")) c = EOF;
      else if (!strcmp (buf, "nul")) c = '\0';
      else if (!strcmp (buf, "alarm")) c = '\a';
      else if (!strcmp (buf, "backspace")) c = '\b';
      else if (!strcmp (buf, "tab")) c = '\t';
      else if (!strcmp (buf, "linefeed")) c = '\n';
      else if (!strcmp (buf, "newline")) c = '\n';
      else if (!strcmp (buf, "vtab")) c = '\v';
      else if (!strcmp (buf, "page")) c = '\f';
#if __MESC__
      //Nyacc bug
      else if (!strcmp (buf, "return")) c = 13;
      else if (!strcmp (buf, "esc")) c = 27;
#else
      else if (!strcmp (buf, "return")) c = '\r';
      //Nyacc crash else if (!strcmp (buf, "esc")) c = '\e';
#endif
      else if (!strcmp (buf, "space")) c = ' ';

#if 1 // Nyacc uses old abbrevs
      else if (!strcmp (buf, "bel")) c = '\a';
      else if (!strcmp (buf, "bs")) c = '\b';
      else if (!strcmp (buf, "ht")) c = '\t';
      else if (!strcmp (buf, "vt")) c = '\v';

#if __MESC__
      //Nyacc bug
      else if (!strcmp (buf, "cr")) c = 13;
#else
      else if (!strcmp (buf, "cr")) c = '\r';
#endif
#endif // Nyacc uses old abbrevs

      else
        {
          eputs ("char not supported: ");
          eputs (buf);
          eputs ("\n");
#if !__MESC__
          assert (!"char not supported");
#endif
        }
    }
  return MAKE_CHAR (c);
}

SCM
reader_read_octal ()
{
  int n = 0;
  int c = peekchar ();
  int s = 1;
  if (c == '-') {s = -1;getchar (); c = peekchar ();}
  while (c >= '0' && c <= '7')
    {
      n <<= 3;
      n+= c - '0';
      getchar ();
      c = peekchar ();
    }
  return MAKE_NUMBER (s*n);
}

SCM
reader_read_hex ()
{
  int n = 0;
  int c = peekchar ();
  int s = 1;
  if (c == '-') {s = -1;getchar (); c = peekchar ();}
  while ((c >= '0' && c <= '9')
         || (c >= 'A' && c <= 'F')
         || (c >= 'a' && c <= 'f'))
    {
      n <<= 4;
      if (c >= 'a') n += c - 'a' + 10;
      else if (c >= 'A') n += c - 'A' + 10;
      else n+= c - '0';
      getchar ();
      c = peekchar ();
    }
  return MAKE_NUMBER (s*n);
}

SCM
append_char (SCM x, int i)
{
  return append2 (x, cons (MAKE_CHAR (i), cell_nil));
}

SCM
reader_read_string ()
{
  SCM p = cell_nil;
  int c = getchar ();
  while (1) {
    if (c == '"') break;
    if (c == '\\' && peekchar () == '\\') p = append_char (p, getchar ());
    else if (c == '\\' && peekchar () == '"') p = append_char (p, getchar ());
    else if (c == '\\' && peekchar () == 'n') {getchar (); p = append_char (p, '\n');}
    else if (c == '\\' && peekchar () == 't') {getchar (); p = append_char (p, '\t');}
#if !__MESC__
    else if (c == EOF) assert (!"EOF in string");
#endif
    else p = append_char (p, c);
    c = getchar ();
  }
  return MAKE_STRING (p);
}

int g_tiny = 0;

int
dump ()
{
  r1 = g_symbols;
  gc_push_frame ();
  gc ();
  gc_peek_frame ();
  char *p = (char*)g_cells;
  putchar ('M');
  putchar ('E');
  putchar ('S');
  putchar (g_stack >> 8);
  putchar (g_stack % 256);
  eputs ("dumping\n");
  if (g_debug > 1)
    {
      eputs ("program r2=");
      display_error_ (r2);
      eputs ("\n");
    }

  for (int i=0; i<g_free * sizeof (struct scm); i++)
    putchar (*p++);
  return 0;
}
