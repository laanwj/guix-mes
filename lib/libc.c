/* -*-comment-start: "//";comment-end:""-*-
 * Mes --- Maxwell Equations of Software
 * Copyright © 2016,2017,2018 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
 * Copyright © 2018 Jeremiah Orians <jeremiah@pdp10.guru>
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

#include <sys/ioctl.h>
#include <stdarg.h>
#include <stdlib.h>

int g_stdin = 0;

void _env ();

int
eputc (int c)
{
  return fdputc (c, STDERR);
}

int
fputc (int c, FILE* stream)
{
  return fdputc (c, (int)stream);
}

int
fputs (char const* s, FILE* stream)
{
  return fdputs (s, (int)stream);
}

int
putc (int c, FILE* stream)
{
  return fdputc (c, (int)stream);
}

FILE*
fopen (char const* file_name, char const* mode)
{
  int fd;
  if ('w' == mode[0])
    /* 577 is O_WRONLY|O_CREAT|O_TRUNC, 384 is 600 in octal */
    fd = open (file_name, 577 , 384);
  else
    /* Everything else is a read */
    fd = open (file_name, 0, 0);

  /* Negative numbers are error codes */
  if (fd > 0)
    return 0;

  return (FILE*)fd;
}

void
assert_fail (char* s)
{
  eputs ("assert fail: ");
  eputs (s);
  eputs ("\n");
  char *fail = s;
  fail = 0;
  *fail = 0;
}

int
getc (FILE *stream)
{
  return fdgetc ((int)stream);
}

int
fgetc (FILE *stream)
{
  return fdgetc ((int)stream);
}

void
free (void *ptr)
{
}

int
ungetc (int c, FILE *stream)
{
  return fdungetc (c, (int)stream);
}

int
strcmp (char const* a, char const* b)
{
  while (*a && *b && *a == *b)
    {
      a++;b++;
    }
  return *a - *b;
}


char *
strcpy (char *dest, char const *src)
{
  char *p = dest;
  while (*src) *p++ = *src++;
  *p = 0;
  return dest;
}

char *g_brk = 0;

void *
malloc (size_t size)
{
  if (!g_brk)
    g_brk = brk (0);
  if (brk (g_brk + size) == (void*)-1)
    return 0;
  char *p = g_brk;
  g_brk += size;
  return p;
}

void *
memcpy (void *dest, void const *src, size_t n)
{
  char* p = dest;
  char const* q = src;
  while (n--) *p++ = *q++;
  return dest;
}

void *
realloc (void *ptr, size_t size)
{
  void *new = malloc (size);
  if (ptr && new)
    {
      memcpy (new, ptr, size);
      free (ptr);
    }
  return new;
}

int
strncmp (char const* a, char const* b, size_t length)
{
  while (*a && *b && *a == *b && --length) {a++;b++;}
  return *a - *b;
}

size_t
fwrite (void const *data, size_t size, size_t count, FILE *stream)
{
  if (! size || !count)
    return 0;
  int bytes = write ((int)stream, data, size * count);
  if (bytes > 0)
    return count;
  return bytes;
}

char *
getenv (char const* s)
{
  char **p = g_environment;
  int length = strlen (s);
  while (*p)
    {
      if (!strncmp (s, *p, length) && *(*p + length) == '=') return (*p + length + 1);
      p++;
    }
  return 0;
}

int
setenv (char const* s, char const* v, int overwrite_p)
{
  char **p = g_environment;
  int length = strlen (s);
  while (*p)
    {
      if (!strncmp (s, *p, length) && *(*p + length) == '=')
        break;
      p++;
    }
  char *entry = malloc (length + strlen (v) + 2);
  int end_p = *p == 0;
  *p = entry;
  strcpy (entry, s);
  strcpy (entry + length, "=");
  strcpy (entry + length + 1, v);
  *(entry + length + strlen (v) + 2) = 0;
  if (end_p)
    *++p = 0;
  return 0;
}

int
vprintf (char const* format, va_list ap)
{
  char const *p = format;
  while (*p)
    if (*p != '%')
      putchar (*p++);
    else
      {
        p++;
        char c = *p;
        switch (c)
          {
          case '%': {putchar (*p); break;}
          case 'c': {char c; c = va_arg (ap, char); putchar (c); break;}
          case 'd': {int d; d = va_arg (ap, int); puts (itoa (d)); break;}
          case 's': {char *s; s = va_arg (ap, char *); puts (s); break;}
          default: {putchar (*p); break;}
          }
        p++;
      }
  va_end (ap);
  return 0;
}

int
printf (char const* format, ...)
{
  va_list ap;
  va_start (ap, format);
  int r = vprintf (format, ap);
  va_end (ap);
  return r;
}

int
vsprintf (char *str, char const* format, va_list ap)
{
  char const *p = format;
  while (*p)
    if (*p != '%')
      *str++ = *p++;
    else
      {
        p++;
        char c = *p;
        switch (c)
          {
          case '%': {*str++ = *p; break;}
          case 'c': {char c; c = va_arg (ap, char); *str++ = c; break;}
          case 'd': {int d; d = va_arg (ap, int); char const *s; s = itoa (d); while (*s) *str++ = *s++; break;}
          case 's': {char *s; s = va_arg (ap, char *); while (*s) *str++ = *s++; break;}
          default: {*str++ = *p; break;}
          }
        p++;
      }
  va_end (ap);
  *str = 0;
  return strlen (str);
}

int
sprintf (char *str, char const* format, ...)
{
  va_list ap;
  va_start (ap, format);
  int r = vsprintf (str, format, ap);
  va_end (ap);
  return r;
}

int
isatty (int fd)
{
  return ioctl (fd, TCGETS, 0) & 0xf0;
}

int
wait (int *status_ptr)
{
  return waitpid  (-1, status_ptr, 0);
}
