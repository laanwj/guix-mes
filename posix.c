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

int g_stdin;

#if _POSIX_SOURCE
int open (char const *s, int mode);
int read (int fd, void* buf, size_t n);
void write (int fd, char const* s, int n);


#define O_RDONLY 0
#define STDIN 0
#define STDOUT 1
#define STDERR 2

int
putchar (int c)
{
  write (STDOUT, (char*)&c, 1);
  return 0;
}

int ungetc_char = -1;
char ungetc_buf[2];

int
getchar ()
{
  char c;
  int i;
  if (ungetc_char == -1)
    {
      int r = read (g_stdin, &c, 1);
      if (r < 1) return -1;
      i = c;
    }
  else
    i = ungetc_buf[ungetc_char--];

  if (i < 0) i += 256;

  return i;
}

#define ungetc fdungetc
int
fdungetc (int c, int fd)
{
  assert (ungetc_char < 2);
  ungetc_buf[++ungetc_char] = c;
  return c;
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
#if 0
  //_POSIX_SOURCE
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

char string_to_cstring_buf[1024];
char const*
string_to_cstring (SCM s)
{
  //static char buf[1024];
  //char *p = buf;
  char *p = string_to_cstring_buf;
  s = STRING(s);
  while (s != cell_nil)
    {
      *p++ = VALUE (car (s));
      s = cdr (s);
    }
  *p = 0;
  //return buf;
  return string_to_cstring_buf;
}

SCM
getenv_ (SCM s) ///((name . "getenv"))
{
#if _POSIX_SOURCE
  char *p = getenv (string_to_cstring (s));
  return p ? MAKE_STRING (cstring_to_list (p)) : cell_f;
#else
  return cell_t;
#endif
}

SCM
open_input_file (SCM file_name)
{
  return MAKE_NUMBER (open (string_to_cstring (file_name), O_RDONLY));
}

SCM
current_input_port ()
{
  return MAKE_NUMBER (g_stdin);
}

SCM
set_current_input_port (SCM port)
{
  g_stdin = VALUE (port) ? VALUE (port) : STDIN;
  return current_input_port ();
}

SCM
force_output (SCM p) ///((arity . n))
{
#if 0
  int fd = 1;
  if (TYPE (p) == TPAIR && TYPE (car (p)) == TNUMBER) fd = VALUE (car (p));
#if _POSIX_SOURCE
  FILE *f = fd == 1 ? stdout : stderr;
  fflush (f);
#endif
#endif
  return cell_unspecified;
}
