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

#include <stdio.h>
#include <mlibc.h>

#if __GNUC__
#include <stdlib.h>
#endif

#if (__GNUC__ || __TINYC__) && !POSIX

void
exit (int code)
{
#if !__TINYC__
  asm (
       "mov    %0,%%ebx\n\t"
       "mov    $1,%%eax\n\t"
       "int    $0x80\n\t"
       : // no outputs "=" (r)
       : "" (code)
       );
#else // __TINYC__
  asm (
       "mov    %0,%%ebx\n\t"
       "mov    $1,%%eax\n\t"
       "int    $128\n\t"
       : // no outputs "=" (r)
       : "Ir" (code)
       );
#endif // __TINYC__
  // not reached
  exit (0);
}

int
read (int fd, void* buf, size_t n)
{
#if !__TINYC__
  int r;
  //syscall (SYS_write, fd, s, n));
  asm (
       "mov    %1,%%ebx\n\t"
       "mov    %2,%%ecx\n\t"
       "mov    %3,%%edx\n\t"

       "movl $0x3,%%eax\n\t"
       "int  $0x80\n\t"

       "mov    %%eax,%0\n\t"
       : "=r" (r)
       : "" (fd), "" (buf), "" (n)
       : "eax", "ebx", "ecx", "edx"
       );
  return r;
#endif
}

int
write (int fd, char const* s, int n)
{
  int r;
#if __GNUC__
  asm (
       "mov    %1,%%ebx\n\t"
       "mov    %2,%%ecx\n\t"
       "mov    %3,%%edx\n\t"

       "mov    $0x04,%%eax\n\t"
       "int    $0x80\n\t"
       "mov    %%eax,%0\n\t"
       : "=r" (r)
       : "" (fd), "" (s), "" (n)
       : "eax", "ebx", "ecx", "edx"
       );

  //syscall (SYS_write, fd, s, n));
#elif __TINYC__
  asm (
       "mov    %1,%%ebx\n\t"
       "mov    %2,%%ecx\n\t"
       "mov    %3,%%edx\n\t"

       "mov    $4, %%eax\n\t"
       "int    $128\n\t"
       "mov    %%eax,%0\n\t"
       : "=r" (r)
       : "Ir" (fd), "Ir" (s), "Ir" (n)
       : "eax", "ebx", "ecx"//, "edx"
       );
#endif
  return r;
}

int
open (char const *s, int flags, ...)
{
#if !__TINYC__
  int mode;
  asm (
       "mov    %%ebp,%%eax\n\t"
       "add    $0x10,%%eax\n\t"
       "mov    (%%eax),%%eax\n\t"
       "mov    %%eax,%0\n\t"
       : "=mode" (mode)
       : //no inputs ""
       );
  int r;
  //syscall (SYS_open, mode));
  asm (
       "mov    %1,%%ebx\n\t"
       "mov    %2,%%ecx\n\t"
       "mov    %3,%%edx\n\t"
       "mov    $0x5,%%eax\n\t"
       "int    $0x80\n\t"
       "mov    %%eax,%0\n\t"
       : "=r" (r)
       : "" (s), "" (flags), "" (mode)
       : "eax", "ebx", "ecx", "edx"
       );
  return r;
#endif
}

int
access (char const *s, int mode)
{
#if !__TINYC__
  int r;
  //syscall (SYS_access, mode));
  asm (
       "mov    %1,%%ebx\n\t"
       "mov    %2,%%ecx\n\t"
       "mov    $0x21,%%eax\n\t"
       "int    $0x80\n\t"
       "mov    %%eax,%0\n\t"
       : "=r" (r)
       : "" (s), "" (mode)
       : "eax", "ebx", "ecx"
       );
  return r;
#endif
}

void *
brk (void *p)
{
#if !__TINYC__
  void *r;
  asm (
       "mov    %1,%%ebx\n\t"

       "mov    $0x2d,%%eax\n\t"
       "int    $0x80\n\t"

       "mov    %%eax,%0\n\t"
       : "=r" (r)
       : "" (p)
       : "eax", "ebx"
       );
  return r;
#endif
}

int
fsync (int fd)
{
#if !__TINYC__
  int r;
  //syscall (SYS_fsync, fd));
  asm (
       "mov    %1,%%ebx\n\t"

       "mov    $0x76, %%eax\n\t"
       "int    $0x80\n\t"
       "mov    %%eax,%0\n\t"
       : "=r" (r)
       : "" (fd)
       : "eax", "ebx"
       );
  return r;
#endif
}

int
fputc (int c, int fd)
{
  write (fd, (char*)&c, 1);
  return 0;
}

void
free (void *ptr)
{
}

char *g_brk = 0;

void *
malloc (size_t size)
{
  if (!g_brk)
    g_brk = brk (0);
  if ((int)brk (g_brk + size) == -1)
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

int
putchar (int c)
{
  write (STDOUT, (char*)&c, 1);
  return 0;
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

size_t
strlen (char const* s)
{
  int i = 0;
  while (s[i]) i++;
  return i;
}

int
strcmp (char const* a, char const* b)
{
  while (*a && *b && *a == *b) {a++;b++;}
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

int
fputs (char const* s, int fd)
{
  int i = strlen (s);
  write (fd, s, i);
  return 0;
}

int
puts (char const* s)
{
  int i = strlen (s);
  write (STDOUT, s, i);
  return 0;
}

void
assert_fail (char* s)
{
  eputs ("assert fail: ");
  eputs (s);
  eputs ("\n");
  *((int*)0) = 0;
}

#define assert(x) ((x) ? (void)0 : assert_fail (#x))

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

int
ungetc (int c, int fd)
{
  assert (ungetc_char < 2);
  ungetc_buf[++ungetc_char] = c;
  return c;
}

char const* itoa (int);

int
strncmp (char const* a, char const* b, size_t length)
{
  while (*a && *b && *a == *b && --length) {a++;b++;}
  return *a - *b;
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
_atoi (char const **p, int base)
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
  return _atoi (&p, 0);
}

// FIXME: copied from libc-mes.c now
#include <stdarg.h>

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
          default: putchar (*p);
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
          case 'd': {int d; d = va_arg (ap, int); char const *s = itoa (d); while (*s) *str++ = *s++; break;}
          case 's': {char *s; s = va_arg (ap, char *); while (*s) *str++ = *s++; break;}
          default: *str++ = *p;
          }
        p++;
      }

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
#endif

char itoa_buf[10];

char const*
itoa (int x)
{
  //static char itoa_buf[10];
  //char *p = buf+9;
  char *p = itoa_buf;
  p += 9;
  *p-- = 0;

  //int sign = x < 0;
  int sign;
  sign = x < 0;
  if (sign)
    x = -x;

  do
    {
      *p-- = '0' + (x % 10);
      x = x / 10;
    } while (x);

  if (sign)
    *p-- = '-';

  return p+1;
}

#if POSIX
#define _GNU_SOURCE
#include <assert.h>
#include <fcntl.h>
#include <string.h>
#include <unistd.h>
#endif // POSIX

int
fdputs (char const* s, int fd)
{
  int i = strlen (s);
  write (fd, s, i);
  return 0;
}

int
eputc (int c)
{
  return fdputc (c, STDERR);
}

int
eputs (char const* s)
{
  return fdputs (s, STDERR);
}

int
fdputc (int c, int fd)
{
  write (fd, (char*)&c, 1);
  return 0;
}

#if POSIX

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

int
fdungetc (int c, int fd)
{
  assert (ungetc_char < 2);
  ungetc_buf[++ungetc_char] = c;
  return c;
}

#endif // POSIX
