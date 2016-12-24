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

#include <fcntl.h>

int
getchar ()
{
  return getc (g_stdin);
}

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
  if (TYPE (p) == PAIR && TYPE (car (p)) == NUMBER) fd = VALUE (car (p));
  FILE *f = fd == 1 ? stdout : stderr;
  assert (TYPE (c) == NUMBER || TYPE (c) == CHAR);
  fputc (VALUE (c), f);
  return c;
}

SCM
stderr_ (SCM x)
{
  SCM display;
  if ((display = assq_ref_cache (cell_symbol_display, r0)) != cell_undefined)
    apply_env (assq_ref_cache (cell_symbol_display, r0), cons (x, cons (MAKE_NUMBER (2), cell_nil)), r0);
  else if (TYPE (x) == SPECIAL || TYPE (x) == STRING || TYPE (x) == SYMBOL)
    fprintf (stderr, string_to_cstring (x));
  else
    fprintf (stderr, "display: undefined\n");
  return cell_unspecified;
}

SCM
force_output (SCM p) ///((arity . n))
{
  int fd = 1;
  if (TYPE (p) == PAIR && TYPE (car (p)) == NUMBER) fd = VALUE (car (p));
  FILE *f = fd == 1 ? stdout : stderr;
  fflush (f);
  return cell_unspecified;
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
