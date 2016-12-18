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

SCM
peek_char ()
{
  return make_char (peekchar ());
}

SCM
read_char ()
{
  return make_char (getchar ());
}

SCM
unread_char (SCM c)
{
  return ungetchar (VALUE (c));
}

int
read_block_comment (int c)
{
  if (c == '!' && peekchar () == '#') return getchar ();
  return read_block_comment (getchar ());
}

int
read_line_comment (int c)
{
  if (c == '\n') return c;
  return read_line_comment (getchar ());
}


SCM lookup_char (int c, SCM a);

SCM
read_word (int c, SCM w, SCM a)
{
  if (c == EOF && w == cell_nil) return cell_nil;
  if (c == '\t') return read_word ('\n', w, a);
  if (c == '\f') return read_word ('\n', w, a);
  if (c == '\n' && w == cell_nil) return read_word (getchar (), w, a);
  if (c == '\n' && VALUE (car (w)) == '.' && cdr (w) == cell_nil) return cell_dot;
  if (c == EOF || c == '\n') return lookup (w, a);
  if (c == ' ') return read_word ('\n', w, a);
  if (c == '(' && w == cell_nil) return read_list (a);
  if (c == '(') {ungetchar (c); return lookup (w, a);}
  if (c == ')' && w == cell_nil) {ungetchar (c); return cell_nil;}
  if (c == ')') {ungetchar (c); return lookup (w, a);}
  if (c == ';') {read_line_comment (c); return read_word ('\n', w, a);}
#if READER
  if (c == '"' && w == cell_nil) return read_string ();
  if (c == '"') {ungetchar (c); return lookup (w, a);}
  if (c == ',' && peekchar () == '@') {getchar (); return cons (lookup (STRING (cell_symbol_unquote_splicing), a),
                                                                   cons (read_word (getchar (), w, a),
                                                                         cell_nil));}
  if ((c == '\''
       || c == '`'
       || c == ',')
      && w == cell_nil) {return cons (lookup_char (c, a),
                                     cons (read_word (getchar (), w, a),
                                           cell_nil));}
  if (c == '#' && peekchar () == ',' && w == cell_nil) {
    getchar ();
    if (peekchar () == '@'){getchar (); return cons (lookup (STRING (cell_symbol_unsyntax_splicing), a),
                                                     cons (read_word (getchar (), w, a),
                                                           cell_nil));}
    return cons (lookup (STRING (cell_symbol_unsyntax), a), cons (read_word (getchar (), w, a), cell_nil));
  }
  if (c == '#' && (peekchar () == '\'' || peekchar () == '`') && w == cell_nil) {
    c = getchar ();
    return cons (lookup (cons (make_char ('#'), cons (make_char (c), cell_nil)), a),
                 cons (read_word (getchar (), w, a), cell_nil));}
  if (c == '#' && peekchar () == 'x') {getchar (); return read_hex ();}
  if (c == '#' && peekchar () == '\\') {getchar (); return read_character ();}
  if (c == '#' && w == cell_nil && peekchar () == '(') {getchar (); return list_to_vector (read_list (a));}
  if (c == '#' && peekchar () == ';') {getchar (); read_word (getchar (), w, a); return read_word (getchar (), w, a);}
  if (c == '#' && peekchar () == '!') {getchar (); read_block_comment (getchar ()); return read_word (getchar (), w, a);}
#endif //READER
  return read_word (getchar (), append2 (w, cons (make_char (c), cell_nil)), a);
}

SCM
read_character ()
{
  int c = getchar ();
  if (c >= '0' && c <= '7'
      && peekchar () >= '0' && peekchar () <= '7') {
    c = c - '0';
    while (peekchar () >= '0' && peekchar () <= '7') {
      c <<= 3;
      c += getchar () - '0';
    }
  }
  else if (c >= 'a' && c <= 'z'
      && peekchar () >= 'a' && peekchar () <= 'z') {
    char buf[10];
    char *p = buf;
    *p++ = c;
    while (peekchar () >= 'a' && peekchar () <= 'z') {
      *p++ = getchar ();
    }
    *p = 0;
    if (!strcmp (buf, char_nul.name)) c = char_nul.value;
    else if (!strcmp (buf, char_backspace.name)) c = char_backspace.value;
    else if (!strcmp (buf, char_tab.name)) c = char_tab.value;
    else if (!strcmp (buf, char_newline.name)) c = char_newline.value;
    else if (!strcmp (buf, char_vt.name)) c = char_vt.value;
    else if (!strcmp (buf, char_page.name)) c = char_page.value;
    else if (!strcmp (buf, char_return.name)) c = char_return.value;
    else if (!strcmp (buf, char_space.name)) c = char_space.value;
    else {
      fprintf (stderr, "char not supported: %s\n", buf);
      assert (!"char not supported");
    }
  }
  return make_char (c);
}

SCM
read_hex ()
{
  int n = 0;
  int c = peekchar ();
  while ((c >= '0' && c <= '9')
         || (c >= 'A' && c <= 'F')
         || (c >= 'a' && c <= 'f')) {
    n <<= 4;
    if (c >= 'a') n += c - 'a' + 10;
    else if (c >= 'A') n += c - 'A' + 10;
    else n+= c - '0';
    getchar ();
    c = peekchar ();
  }
  return make_number (n);
}

SCM
append_char (SCM x, int i)
{
  return append2 (x, cons (make_char (i), cell_nil));
}

SCM
read_string ()
{
  SCM p = cell_nil;
  int c = getchar ();
  while (true) {
    if (c == '"') break;
    if (c == '\\' && peekchar () == '"') p = append_char (p, getchar ());
    else if (c == '\\' && peekchar () == 'n') {getchar (); p = append_char (p, '\n');}
    else if (c == EOF) assert (!"EOF in string");
    else p = append_char (p, c);
    c = getchar ();
  }
  return make_string (p);
}

int
eat_whitespace (int c)
{
  while (c == ' ' || c == '\t' || c == '\n' || c == '\f') c = getchar ();
  if (c == ';') return eat_whitespace (read_line_comment (c));
#if READER
  if (c == '#' && peekchar () == '!') {getchar (); read_block_comment (getchar ()); return eat_whitespace (getchar ());}
#endif
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
lookup (SCM s, SCM a)
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
    if (p == cell_nil) return make_number (n * sign);
  }

  if (VALUE (car (s)) == '#' && VALUE (cadr (s)) == ':') return make_keyword (cddr (s));

  SCM x = internal_lookup_symbol (s);
  if (x) return x;

  if (cdr (s) == cell_nil) {
    if (VALUE (car (s)) == '\'') return cell_symbol_quote;
    if (VALUE (car (s)) == '`') return cell_symbol_quasiquote;
    if (VALUE (car (s)) == ',') return cell_symbol_unquote;
  }
  else if (cddr (s) == cell_nil) {
    if (VALUE (car (s)) == ',' && VALUE (cadr (s)) == '@') return cell_symbol_unquote_splicing;
    if (VALUE (car (s)) == '#' && VALUE (cadr (s)) == '\'') return cell_symbol_syntax;
    if (VALUE (car (s)) == '#' && VALUE (cadr (s)) == '`') return cell_symbol_quasisyntax;
    if (VALUE (car (s)) == '#' && VALUE (cadr (s)) == ',') return cell_symbol_unsyntax;
  }
  else if (cdddr (s) == cell_nil) {
    if (VALUE (car (s)) == '#' && VALUE (cadr (s)) == ',' && VALUE (caddr (s)) == '@') return cell_symbol_unsyntax_splicing;
        if (VALUE (car (s)) == 'E' && VALUE (cadr (s)) == 'O' && VALUE (caddr (s)) == 'F') {
      fprintf (stderr, "mes: got EOF\n");
      return cell_nil; // `EOF': eval program, which may read stdin
    }
  }

  return internal_make_symbol (s);
}

SCM
lookup_char (int c, SCM a)
{
  return lookup (cons (make_char (c), cell_nil), a);
}

SCM
list_of_char_equal_p (SCM a, SCM b)
{
  while (a != cell_nil && b != cell_nil && VALUE (car (a)) == VALUE (car (b))) {
    assert (TYPE (car (a)) == CHAR);
    assert (TYPE (car (b)) == CHAR);
    a = cdr (a);
    b = cdr (b);
  }
  return (a == cell_nil && b == cell_nil) ? cell_t : cell_f;
}

SCM
internal_lookup_symbol (SCM s)
{
  SCM x = g_symbols;
  while (x) {
    // .string and .name is the same field; .name is used as a handy
    // static field initializer.  A string can only be mistaken for a
    // cell with type == PAIR for the one character long, zero-padded
    // #\etx.
    SCM p = STRING (car (x));
    if (p < 0 || p >= g_free.value || TYPE (p) != PAIR)
      STRING (car (x)) = cstring_to_list (NAME (car (x)));
    if (list_of_char_equal_p (STRING (car (x)), s) == cell_t) break;
    x = cdr (x);
  }
  if (x) x = car (x);
  return x;
}
