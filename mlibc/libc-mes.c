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

int g_stdin = 0;
char **g_environment;
int _env ();
int exit ();
int main(int,char*[]);

int
_start ()
{
  asm ("mov____%ebp,%eax");      // mov    %ebp,%eax
  asm ("add____$i8,%eax !8");    // add    $0x8,%eax
  asm ("push___%eax");           // push   %eax

  asm ("mov____%ebp,%eax");      // mov    %ebp,%eax
  asm ("add____$i8,%eax !4");    // add    $0x4,%eax
  asm ("movzbl_(%eax),%eax");    // movzbl (%eax),%eax
  asm ("push___%eax");           // push   %eax

  asm ("mov____%ebp,%eax");      // mov    %ebp,%eax
  asm ("add____$i8,%eax !4");    // add    $0x4,%eax

  asm ("movzbl_(%eax),%eax");    // movzbl (%eax),%eax
  asm ("add____$i8,%eax !3");    // add    $0x3,%eax

  asm ("shl____$i8,%eax !0x02"); // shl    $0x2,%eax
  asm ("add____%ebp,%eax");      // add    %ebp,%eax
  asm ("push___%eax");           // push   %eax

  g_environment = _env ();
  asm ("pop____%eax");           // pop   %eax
  int r = main ();
  exit (r);
}

char **
_env (char **e)
{
  return e;
}

void
exit ()
{
  asm ("mov____0x8(%ebp),%ebx !8");               // mov    0x8(%ebp),%ebx

  asm ("mov____$i32,%eax SYS_exit");              // mov    $0x1,%eax
  asm ("int____$0x80");                           // int    $0x80
}

void
read ()
{
  asm ("mov____0x8(%ebp),%ebx !8");               // mov    0x8(%ebp),%ebx
  asm ("mov____0x8(%ebp),%ecx !12");              // mov    0x8(%ebp),%ecx
  asm ("mov____0x8(%ebp),%edx !16");              // mov    0x8(%ebp),%edx

  asm ("mov____$i32,%eax SYS_read");              // mov    $0x3,%eax
  asm ("int____$0x80");                           // int    $0x80
}

void
write ()
{
  asm ("mov____0x8(%ebp),%ebx !8");               // mov    0x8(%ebp),%ebx
  asm ("mov____0x8(%ebp),%ecx !12");              // mov    0x8(%ebp),%ecx
  asm ("mov____0x8(%ebp),%edx !16");              // mov    0x8(%ebp),%edx

  asm ("mov____$i32,%eax SYS_write");             // mov    $0x4,%eax
  asm ("int____$0x80");                           // int    $0x80
}

void
open ()
{
  asm ("mov____0x8(%ebp),%ebx !8");               // mov    0x8(%ebp),%ebx
  asm ("mov____0x8(%ebp),%ecx !12");              // mov    0x8(%ebp),%ecx
  asm ("mov____0x8(%ebp),%edx !16");              // mov    0x8(%ebp),%edx

  asm ("mov____$i32,%eax SYS_open");              // mov    $0x5,%eax
  asm ("int____$0x80");                           // int    $0x80
}

void
access ()
{
  asm ("mov____0x8(%ebp),%ebx !8");               // mov    0x8(%ebp),%ebx
  asm ("mov____0x8(%ebp),%ecx !12");              // mov    0x8(%ebp),%ecx

  asm ("mov____$i32,%eax SYS_access");            // mov    $0x21,%eax
  asm ("int____$0x80");                           // int    $0x80
}

void
brk ()
{
  asm ("mov____0x8(%ebp),%ebx !8");               // mov    0x8(%ebp),%ebx

  asm ("mov____$i32,%eax SYS_brk");               // mov    $0x2d,%eax
  asm ("int____$0x80");                           // int    $0x80
}

void
fsync ()
{
  asm ("mov____0x8(%ebp),%ebx !8");               // mov    0x8(%ebp),%ebx

  asm ("mov____$i32,%eax SYS_fsync");             // mov    $0x7c,%eax
  asm ("int____$0x80");                           // int    $0x80
}

int
strlen (char const* s)
{
  int i = 0;
  while (s[i]) i++;
  return i;
}

int
eputs (char const* s)
{
  int i = strlen (s);
  write (2, s, i);
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
  write (1, s, i);
  return 0;
}

int
putchar (int c)
{
  write (1, (char*)&c, 1);
  return 0;
}

int
fputc (int c, int fd)
{
  write (fd, (char*)&c, 1);
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

char itoa_buf[10];

char const*
itoa (int x)
{
  //static char itoa_buf[10];
  //char *p = buf+9;
  char *p = itoa_buf;
  p += 9;
  *p-- = 0;

  //int sign = x < 0; // FIXME
  int sign = 0;
  if (x < 0) sign = 1;
  if (sign)
    x = -x;

  do
     {
       *p-- = '0' + (x % 10);
       x = x / 10;
     } while (x);

  if (sign && *(p + 1) != '0')
    *p-- = '-';

  return p+1;
}

int
isdigit (char c)
{
  //return (c>='0') && (c<='9');
  if (c>='0' && c<='9') return 1;
  return 0;
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

//void *g_malloc_base = 0;
char *g_malloc_base = 0;

//void *
int *
malloc (int size)
{
  //void *p = brk (0);
  char *p = 0;
  p = brk (0);
  if (!g_malloc_base) g_malloc_base = p;
  brk (p+size);
  return p;
}

//void *
int *
//realloc (void *p, int size)
realloc (int *p, int size)
{
  brk (g_malloc_base + size);
  return g_malloc_base;
}

int
strncmp (char const* a, char const* b, int length)
{
  while (*a && *b && *a == *b && --length) {a++;b++;}
  return *a - *b;
}

char **g_environment;
char *
getenv (char const* s)
{
  char **p = g_environment;
  p = *g_environment;
  int length = strlen (s);
  while (*p)
    {
      if (!strncmp (s, *p, length) && *(*p + length) == '=') return (*p + length + 1);
      p++;
    }
  return 0;
}

#include <stdarg.h>

int
vprintf (char const* format, va_list ap)
{
  char *p = format;
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
        va_end (ap);
        p++;
      }
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
