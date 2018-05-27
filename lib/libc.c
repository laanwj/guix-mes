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

#if POSIX
#define _GNU_SOURCE
#include <assert.h>
#include <fcntl.h>
#include <string.h>
#include <unistd.h>
#endif // POSIX

int
isdigit (int c)
{
  return (c>='0') && (c<='9');
}

int
isxdigit (int c)
{
  return isdigit (c) || (c>='a') && (c<='f');
}

int
isspace (int c)
{
  return (c == '\t' || c == '\n' || c == '\v' || c == '\f' || c == '\r' || c == ' ');
}

int
isnumber (int c, int base)
{
  if (base == 2)
    return (c>='0') && (c<='1');
  if (base == 8)
    return (c>='0') && (c<='7');
  if (base == 10)
    return isdigit (c);
  if (base == 16)
    return isxdigit (c);
}

int
abtoi (char const **p, int base)
{
  char const *s = *p;
  int i = 0;
  int sign = 1;
  if (!base) base = 10;
  if (*s && *s == '-')
    {
      sign = -1;
      s++;
    }
  while (isnumber (*s, base))
    {
      i *= base;
      int m = *s > '9' ? 'a' - 10 : '0';
      i += *s - m;
      s++;
    }
  *p = s;
  return i * sign;
}

int
atoi (char const *s)
{
  char const *p = s;
  return abtoi (&p, 0);
}

char const*
itoa (int x)
{
  static char itoa_buf[12];
  char *p = itoa_buf + 11;
  *p-- = 0;

  int sign = 0;
  unsigned u = x;
  if (x < 0)
    {
      sign = 1;
      u = -x;
    }

  do
     {
       *p-- = '0' + (u % 10);
       u = u / 10;
     } while (u);

  if (sign && *(p + 1) != '0')
    *p-- = '-';

  return p+1;
}

char const*
itoab (int x, int base)
{
  static char itoa_buf[12];
  char *p = itoa_buf + 11;
  *p-- = 0;

  int sign = 0;
  unsigned u = x;
  if (x < 0)
    {
      sign = 1;
      u = -x;
    }

  do
     {
       int i = u % base;
       *p-- = i > 9 ? 'a' + i - 10 : '0' + i;
       x = u / base;
     } while (u);

  if (sign && *(p + 1) != '0')
    *p-- = '-';

  return p+1;
}

int
fdputc (int c, int fd)
{
  write (fd, (char*)&c, 1);
  return 0;
}

int
fdputs (char const* s, int fd)
{
  int i = strlen (s);
  write (fd, s, i);
  return 0;
}

#if !POSIX

///char **g_environment = 0; // FIXME: todo extern
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
  FILE* f;
  if ('w' == mode[0])
    /* 577 is O_WRONLY|O_CREAT|O_TRUNC, 384 is 600 in octal */
    f = open (file_name, 577 , 384);
  else
    /* Everything else is a read */
    f = open (file_name, 0, 0);

  /* Negative numbers are error codes */
  if (0 > f)
    return 0;

  return f;
}

int
putchar (int c)
{
  write (STDOUT, (char*)&c, 1);
  return 0;
}

void
assert_fail (char* s)
{
  eputs ("assert fail: ");
  eputs (s);
  eputs ("\n");
  //*((int*)0) = 0;
  char *fail = s;
  fail = 0;
  *fail = 0;
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
    {
       //FIXME
       //i = ungetc_buf[ungetc_char--];
       i = ungetc_buf[ungetc_char];
       //ungetc_char--;
       ungetc_char = ungetc_char - 1;
     }
  if (i < 0) i += 256;

  return i;
}

int
fgetc (int fd)
{
  char c;
  int i;
  int r = read (fd, &c, 1);
  if (r < 1) return -1;
  i = c;
  return i;
}

void
free (void *ptr)
{
}

//#define assert(x) ((x) ? (void)0 : assert_fail (#x))
int
ungetc (int c, int fd)
{
  //FIXME
  //assert (ungetc_char < 2);
  //assert (ungetc_char == -1 || ungetc_char < 2);
  //FIXME
  //ungetc_buf[++ungetc_char] = c;
  ungetc_char++;
  ungetc_buf[ungetc_char] = c;
  return c;
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
    //return bytes/size;
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

#endif //!POSIX
