/* -*-comment-start: "//";comment-end:""-*-
 * GNU Mes --- Maxwell Equations of Software
 * Copyright © 2016,2017,2018 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
 *
 * This file is part of GNU Mes.
 *
 * GNU Mes is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or (at
 * your option) any later version.
 *
 * GNU Mes is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with GNU Mes.  If not, see <http://www.gnu.org/licenses/>.
 */

#define MAX_STRING 4096

char const*
list_to_cstring (SCM list, size_t* size)
{
  static char buf[MAX_STRING];
  size_t i = 0;
  char *p = buf;
  while (list != cell_nil)
    {
      assert (i < MAX_STRING);
      buf[i++] = VALUE (car (list));
      list = cdr (list);
    }
  buf[i] = 0;
  *size = i;
  return buf;
}

size_t
bytes_cells (size_t length)
{
  return (1 + sizeof (long) + sizeof (long) + length + sizeof (SCM)) / sizeof (SCM);
}

SCM
make_bytes (char const* s, size_t length)
{
  size_t size = bytes_cells (length);
  SCM x = alloc (size);
  TYPE (x) = TBYTES;
  LENGTH (x) = length;
  char *p = &g_cells[x].cdr;
  if (!length)
    *(char*)p = 0;
  else
    memcpy (p, s, length + 1);
  if (g_debug > 2)
    {
      eputs ("make bytes: "); eputs (s); eputs ("\n");
      eputs ("     bytes: "); eputs (CBYTES (x)); eputs ("\n");
      eputs ("    length: "); eputs (itoa (length)); eputs ("\n");
      eputs ("        ==> "); write_error_ (x);
      eputs ("\n");
    }
  return x;
}

SCM
make_string (char const* s, size_t length)
{
  assert (length < HALFLONG_MAX);
  SCM x = make_cell__ (TSTRING, length, 0);
  SCM v = make_bytes (s, length);
  CDR (x) = v;
  return x;
}

SCM
string_equal_p (SCM a, SCM b) ///((name . "string=?"))
{
  if (! ((TYPE (a) == TSTRING && TYPE (b) == TSTRING)
         || (TYPE (a) == TKEYWORD || TYPE (b) == TKEYWORD)))
    {
      eputs ("type a: "); eputs (itoa (TYPE (a))); eputs ("\n");
      eputs ("type b: "); eputs (itoa (TYPE (b))); eputs ("\n");
      eputs ("a= "); write_error_ (a); eputs ("\n");
      eputs ("b= "); write_error_ (b); eputs ("\n");
      assert ((TYPE (a) == TSTRING && TYPE (b) == TSTRING)
              || (TYPE (a) == TKEYWORD || TYPE (b) == TKEYWORD));
    }
  if (g_debug == -1)
    {
      eputs ("string=?: "); eputs (CSTRING (a));
      eputs (" =? "); eputs (CSTRING (b));
    }
  if (a == b
      || STRING (a) == STRING (b)
      || (!LENGTH (a) && !LENGTH (b))
      || (LENGTH (a) == LENGTH (b)
          && !memcmp (CSTRING (a), CSTRING (b), LENGTH (a))))
    {
      if (g_debug == -1)
        eputs (" => #t\n");
      return cell_t;
    }
  if (g_debug == -1)
    eputs (" => #f\n");
  return cell_f;
}

SCM
symbol_to_string (SCM symbol)
{
  SCM x = make_cell__ (TSTRING, CAR (symbol), CDR (symbol));

  if (g_debug > 2)
    {
      eputs ("symbol->string: "); eputs (CSTRING (x)); eputs ("\n");
      eputs ("  was: "); write_error_ (symbol);
      eputs ("==> "); write_error_ (x);
      eputs ("\n");
    }
  return x;
}

SCM
symbol_to_keyword (SCM symbol)
{
  SCM x = make_cell__ (TKEYWORD, CAR (symbol), CDR (symbol));

  if (g_debug > 2)
    {
      eputs ("symbol->keyword: "); eputs (CSTRING (x)); eputs ("\n");
      eputs ("  was: "); write_error_ (symbol);
      eputs ("==> "); write_error_ (x);
      eputs ("\n");
    }
  return x;
}

SCM
keyword_to_string (SCM keyword)
{
  SCM x = make_cell__ (TSTRING, CAR (keyword), CDR (keyword));

  if (g_debug > 2)
    {
      eputs ("keyword->string: "); eputs (CSTRING (x)); eputs ("\n");
      eputs ("  was: "); write_error_ (keyword);
      eputs ("==> "); write_error_ (x);
      eputs ("\n");
    }
  return x;
}

SCM
string_to_symbol (SCM string)
{
  SCM x = hash_ref (g_symbols, string, cell_f);
  if (x == cell_f)
    x = make_symbol (string);
  return x;
}

SCM
make_symbol (SCM string)
{
  SCM x = make_cell__ (TSYMBOL, LENGTH (string), STRING (string));
  hash_set_x (g_symbols, string, x);

  if (g_debug > 3)
    hash_table_printer (g_symbols);

  if (g_debug > 2)
    {
      eputs ("make_symbol: "); eputs (CSTRING (string)); eputs ("\n");
      eputs ("==> "); write_error_ (x);
      eputs ("\n");
    }

  return x;
}

SCM
bytes_to_list (char const* s, size_t i)
{
  SCM p = cell_nil;
  while (i--)
    {
      int c = (0x100 + s[i]) % 0x100;
      p = cons (MAKE_CHAR (c), p);
    }
  return p;
}

SCM
cstring_to_list (char const* s)
{
  return bytes_to_list (s, strlen (s));
}

SCM
cstring_to_symbol (char const *s)
{
  SCM string = MAKE_STRING0 (s);
  return string_to_symbol (string);
}

SCM
string_to_list (SCM string)
{
  return bytes_to_list (CSTRING (string), LENGTH (string));
}

SCM
list_to_string (SCM list)
{
  size_t size;
  char const *s = list_to_cstring (list, &size);
  return make_string (s, size);
}

SCM
read_string (SCM port) ///((arity . n))
{
  int fd = g_stdin;
  if (TYPE (port) == TPAIR && TYPE (car (port)) == TNUMBER)
    g_stdin = VALUE (CAR (port));
  int c = readchar ();
  static char buf[MAX_STRING];
  size_t i = 0;
  while (c != -1)
    {
      assert (i < MAX_STRING);
      buf[i++] = c;
      c = readchar ();
    }
  buf[i] = 0;
  g_stdin = fd;
  return make_string (buf, i);
}

SCM
string_append (SCM x) ///((arity . n))
{
  static char buf[MAX_STRING];
  char const *p = buf;
  buf[0] = 0;
  size_t size = 0;
  while (x != cell_nil)
    {
      SCM string = CAR (x);
      assert (TYPE (string) == TSTRING);
      memcpy (p, CSTRING (string), LENGTH (string) + 1);
      p += LENGTH (string);
      size += LENGTH (string);
      assert (size < MAX_STRING);
      x = CDR (x);
    }
  return make_string (buf, size);
}
