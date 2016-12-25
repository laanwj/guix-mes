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

#if FAT_C_READER
int fat_c_eat_whitespace (int);
SCM fat_c_read_word (int, SCM, SCM);
SCM fat_c_lookup_ (SCM, SCM);
#endif // FAT_C_READER

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
#if !FAT_C_READER
  if (assq_ref_cache (cell_symbol_read_input_file, r0) != cell_undefined)
    return apply_env (cell_symbol_read_input_file, cell_nil, r0);
#endif
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
#if FAT_C_READER
  return fat_c_read_word (c, w, a);
#endif
  return read_word (getchar (), append2 (w, cons (MAKE_CHAR (c), cell_nil)), a);
}

int
eat_whitespace (int c)
{
  while (c == ' ' || c == '\t' || c == '\n' || c == '\f') c = getchar ();
  if (c == ';') return eat_whitespace (read_line_comment (c));
#if FAT_C_READER
  return fat_c_eat_whitespace (c);
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

#if FAT_C_READER
  return fat_c_lookup_ (s, a);
#endif
  SCM x = lookup_symbol_ (s);
  return x ? x : make_symbol_ (s);
}

SCM
lookup_char (int c, SCM a)
{
  return lookup_ (cons (MAKE_CHAR (c), cell_nil), a);
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
lookup_symbol_ (SCM s)
{
  SCM x = g_symbols;
  while (x) {
    if (list_of_char_equal_p (STRING (car (x)), s) == cell_t) break;
    x = cdr (x);
  }
  if (x) x = car (x);
  return x;
}


#if FAT_C_READER
SCM lookup_char (int c, SCM a);

SCM
make_keyword (SCM s) ///((internal))
{
  SCM x = lookup_symbol_ (s);
  x = x ? x : make_symbol_ (s);
  g_cells[tmp_num].value = KEYWORD;
  return make_cell (tmp_num, STRING (x), 0);
}

int
read_block_comment (int s, int c) ///((internal))
{
  if (c == s && peekchar () == '#') return getchar ();
  return read_block_comment (s, getchar ());
}

scm char_eof = {CHAR, .name="*eof*", .value=-1};
scm char_nul = {CHAR, .name="nul", .value=0};
scm char_alarm = {CHAR, .name="alarm", .value=7};
scm char_backspace = {CHAR, .name="backspace", .value=8};
scm char_tab = {CHAR, .name="tab", .value=9};
scm char_newline = {CHAR, .name="newline", .value=10};
scm char_vtab = {CHAR, .name="vtab", .value=11};
scm char_page = {CHAR, .name="page", .value=12};
scm char_return = {CHAR, .name="return", .value=13};
scm char_space = {CHAR, .name="space", .value=32};

SCM
read_character () ///((internal))
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
    else if (!strcmp (buf, char_alarm.name)) c = char_alarm.value;
    else if (!strcmp (buf, char_backspace.name)) c = char_backspace.value;
    else if (!strcmp (buf, char_tab.name)) c = char_tab.value;
    else if (!strcmp (buf, char_newline.name)) c = char_newline.value;
    else if (!strcmp (buf, char_vtab.name)) c = char_vtab.value;
    else if (!strcmp (buf, char_page.name)) c = char_page.value;
    else if (!strcmp (buf, char_return.name)) c = char_return.value;
    else if (!strcmp (buf, char_space.name)) c = char_space.value;
    else {
      fprintf (stderr, "char not supported: %s\n", buf);
      assert (!"char not supported");
    }
  }
  return MAKE_CHAR (c);
}

SCM
read_hex () ///((internal))
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
  return MAKE_NUMBER (n);
}

SCM
append_char (SCM x, int i) ///((internal))
{
  return append2 (x, cons (MAKE_CHAR (i), cell_nil));
}

SCM
read_string () ///((internal))
{
  SCM p = cell_nil;
  int c = getchar ();
  while (true) {
    if (c == '"') break;
    if (c == '\\' && peekchar () == '\\') p = append_char (p, getchar ());
    else if (c == '\\' && peekchar () == '"') p = append_char (p, getchar ());
    else if (c == '\\' && peekchar () == 'n') {getchar (); p = append_char (p, '\n');}
    else if (c == EOF) assert (!"EOF in string");
    else p = append_char (p, c);
    c = getchar ();
  }
  return MAKE_STRING (p);
}

SCM
fat_c_lookup_ (SCM s, SCM a) ///((internal))
{
  if (VALUE (car (s)) == '#' && VALUE (cadr (s)) == ':') return make_keyword (cddr (s));
  SCM x = lookup_symbol_ (s);
  return x ? x : make_symbol_ (s);
}

int
fat_c_eat_whitespace (int c) ///((internal))
{
  if (c == '#' && (peekchar () == '!' || peekchar () == '|')) {c=getchar (); read_block_comment (c, getchar ()); return eat_whitespace (getchar ());}
  return c;
}

SCM
fat_c_read_word (int c, SCM w, SCM a) ///((internal))
{
  if (c == '"' && w == cell_nil) return read_string ();
  if (c == '"') {ungetchar (c); return lookup_ (w, a);}
  if (c == ',' && peekchar () == '@') {getchar ();
    return cons (cell_symbol_unquote_splicing,
                 cons (read_word (getchar (), w, a), cell_nil));}
  if (c == '\'' && w == cell_nil)
    return cons (cell_symbol_quote, cons (read_word (getchar (), w, a), cell_nil));
  if (c == '`' && w == cell_nil)
    return cons (cell_symbol_quasiquote, cons (read_word (getchar (), w, a), cell_nil));
  if (c == ',' && w == cell_nil)
    return cons (cell_symbol_unquote, cons (read_word (getchar (), w, a), cell_nil));
  if (c == '#' && peekchar () == ',' && w == cell_nil) {
    getchar ();
    if (peekchar () == '@'){getchar ();
      return cons (cell_symbol_unsyntax_splicing,
                   cons (read_word (getchar (), w, a), cell_nil));}
    return cons (cell_symbol_unsyntax, cons (read_word (getchar (), w, a), cell_nil));
  }
  if (c == '#' && peekchar () == '\'' && w == cell_nil) {
    c = getchar ();
    return cons (cell_symbol_syntax,
                 cons (read_word (getchar (), w, a), cell_nil));}
  if (c == '#' && peekchar () == '`' && w == cell_nil) {
    c = getchar ();
    return cons (cell_symbol_quasisyntax,
                 cons (read_word (getchar (), w, a), cell_nil));}
  if (c == '#' && peekchar () == 'x') {getchar (); return read_hex ();}
  if (c == '#' && peekchar () == '\\') {getchar (); return read_character ();}
  if (c == '#' && w == cell_nil && peekchar () == '(') {getchar (); return list_to_vector (read_list (a));}
  if (c == '#' && peekchar () == ';') {getchar (); read_word (getchar (), w, a); return read_word (getchar (), w, a);}
  if (c == '#' && (peekchar () == '!' || peekchar () == '|')) {c = getchar (); read_block_comment (c, getchar ()); return read_word (getchar (), w, a);}
  return read_word (getchar (), append2 (w, cons (MAKE_CHAR (c), cell_nil)), a);
}

#endif // FAT_C_READER
