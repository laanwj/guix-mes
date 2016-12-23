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
  return read_word (getchar (), append2 (w, cons (make_char (c), cell_nil)), a);
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
    if (p == cell_nil) return make_number (n * sign);
  }

  SCM x = lookup_symbol_ (s);
  return x ? x : make_symbol_ (s);
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
