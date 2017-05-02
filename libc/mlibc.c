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

char **g_environment = 0;
int g_stdin = 0;

#include <stdio.h>
#include <mlibc.h>

#if __GNUC__ && !POSIX

#include <stdlib.h>

void
exit (int code)
{
  asm (
       "movl %0,%%ebx\n\t"
       "movl $1,%%eax\n\t"
       "int  $0x80"
       : // no outputs "=" (r)
       : "" (code)
       );
  // not reached
  exit (0);
}

int
read (int fd, void* buf, size_t n)
{
  int r;
  //syscall (SYS_write, fd, s, n));
  asm (
       "movl %1,%%ebx\n\t"
       "movl %2,%%ecx\n\t"
       "movl %3,%%edx\n\t"

       "movl $0x3,%%eax\n\t"
       "int  $0x80\n\t"

       "mov %%eax,%0\n\t"
       : "=r" (r)
       : "" (fd), "" (buf), "" (n)
       : "eax", "ebx", "ecx", "edx"
       );
  return r;
}

int
write (int fd, char const* s, int n)
{
  int r;
  //syscall (SYS_write, fd, s, n));
  asm (
       "mov %1,%%ebx\n\t"
       "mov %2,%%ecx\n\t"
       "mov %3,%%edx\n\t"

       "mov $0x4, %%eax\n\t"
       "int $0x80\n\t"
       "mov %%eax,%0\n\t"
       : "=r" (r)
       : "" (fd), "" (s), "" (n)
       : "eax", "ebx", "ecx", "edx"
       );
  return r;
}

int
open (char const *s, int mode)
{
  int r;
  //syscall (SYS_open, mode));
  asm (
       "mov %1,%%ebx\n\t"
       "mov %2,%%ecx\n\t"
       "mov $0x5,%%eax\n\t"
       "int $0x80\n\t"
       "mov %%eax,%0\n\t"
       : "=r" (r)
       : "" (s), "" (mode)
       : "eax", "ebx", "ecx"
       );
  return r;
}

int
access (char const *s, int mode)
{
  int r;
  //syscall (SYS_access, mode));
  asm (
       "mov %1,%%ebx\n\t"
       "mov %2,%%ecx\n\t"
       "mov $0x21,%%eax\n\t"
       "int $0x80\n\t"
       "mov %%eax,%0\n\t"
       : "=r" (r)
       : "" (s), "" (mode)
       : "eax", "ebx", "ecx"
       );
  return r;
}

void *
brk (void *p)
{
  void *r;
  asm (
       "mov %1,%%ebx\n\t"

       "mov $0x2d,%%eax\n\t"
       "int $0x80\n\t"

       "mov %%eax,%0\n\t"
       : "=r" (r)
       : "" (p)
       : "eax", "ebx"
       );
  return r;
}

int
fsync (int fd)
{
  int r;
  //syscall (SYS_fsync, fd));
  asm (
       "mov %1,%%ebx\n\t"

       "mov $0x76, %%eax\n\t"
       "int $0x80\n\t"
       "mov %%eax,%0\n\t"
       : "=r" (r)
       : "" (fd)
       : "eax", "ebx"
       );
  return r;
}

int
fputc (int c, int fd)
{
  write (fd, (char*)&c, 1);
  return 0;
}

int
putchar (int c)
{
  write (STDOUT, (char*)&c, 1);
  return 0;
}

void *g_malloc_base = 0;

void *
malloc (size_t size)
{
  void *p = brk (0);
  if (!g_malloc_base) g_malloc_base = p;
  brk (p+size);
  return p;
}

void *
realloc (void *p, size_t size)
{
  (void)p;
  brk (g_malloc_base + size);
  return g_malloc_base;
}

void
free (void *p)
{
  int *n = (int*)p-1;
  //munmap ((void*)p, *n);
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

int
eputs (char const* s)
{
  int i = strlen (s);
  write (STDERR, s, i);
  return 0;
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

char const*
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
atoi (char const *s)
{
  int i = 0;
  int sign = 1;
  if (*s && *s == '-')
    {
      sign = -1;
      s++;
    }
  while (isdigit (*s))
    {
      i *= 10;
      i += (*s - '0');
      s++;
    }
  return i * sign;
}

int
printf (char const* format, ...)
{
  int va_arg = 0;
  int va;
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
          case 'c':
            {
            asm (
                 "mov    -0xc(%%ebp),%%eax\n\t"
                 "shl     $0x2,%%eax\n\t"
                 "add     %%ebp,%%eax\n\t"
                 "add     $0xc,%%eax\n\t"
                 "mov     (%%eax),%%eax\n\t"
                 //"mov     %%eax,%0\n\t"
                 : "=va" (va)
                 : //no inputs ""
                 );
            putchar ((char)va);
            va_arg++;
            break;
            }
          case 'd': {
            asm (
                 "mov    -0xc(%%ebp),%%eax\n\t"
                 "shl     $0x2,%%eax\n\t"
                 "add     %%ebp,%%eax\n\t"
                 "add     $0xc,%%eax\n\t"
                 "mov     (%%eax),%%eax\n\t"
                 //"mov     %%eax,%0\n\t"
                 : "=va" (va)
                 : //no inputs ""
                 );
            puts (itoa ((int)va));
            va_arg++;
            break;
          }
          case 's': {
            asm (
                 "mov    -0xc(%%ebp),%%eax\n\t"
                 "shl     $0x2,%%eax\n\t"
                 "add     %%ebp,%%eax\n\t"
                 "add     $0xc,%%eax\n\t"
                 "mov     (%%eax),%%eax\n\t"
                 //"mov     %%eax,%0\n\t"
                 : "=va" (va)
                 : //no inputs ""
                 );
            puts ((char*)va);
            va_arg++;
            break;
          }
          default: putchar (*p);
          }
        p++;
      }
  return 0;
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

#if POSIX

int
fdputc (int c, int fd)
{
  write (fd, (char*)&c, 1);
  return 0;
}

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
