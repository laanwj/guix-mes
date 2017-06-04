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

void
exit ()
{
  asm (".byte 0x8b 0x5d 0x08");                    // mov    0x8(%ebp),%ebx
  asm (".byte 0xb8 0x01 0x00 0x00 0x00");          // mov    $0x1,%eax
  asm (".byte 0xcd 0x80");                         // int    $0x80
}

void
read ()
{
  asm (".byte 0x8b 0x5d 0x08");                    // mov    0x8(%ebp),%ebx
  asm (".byte 0x8b 0x4d 0x0c");                    // mov    0xc(%ebp),%ecx
  asm (".byte 0x8b 0x55 0x10");                    // mov    0x10(%ebp),%edx

  asm (".byte 0xb8 0x03 0x00 0x00 0x00");          // mov    $0x3,%eax
  asm (".byte 0xcd 0x80");                         // int    $0x80
}

void
write ()
{
  asm (".byte 0x8b 0x5d 0x08");                   // mov    0x8(%ebp),%ebx
  asm (".byte 0x8b 0x4d 0x0c");                   // mov    0xc(%ebp),%ecx
  asm (".byte 0x8b 0x55 0x10");                   // mov    0x10(%ebp),%edx

  asm (".byte 0xb8 0x04 0x00 0x00 0x00");         // mov    $0x4,%eax
  asm (".byte 0xcd 0x80");                        // int    $0x80
}

void
open ()
{
  asm (".byte 0x8b 0x5d 0x08");                    // mov    0x8(%ebp),%ebx
  asm (".byte 0x8b 0x4d 0x0c");                    // mov    0xc(%ebp),%ecx
  asm (".byte 0x8b 0x55 0x10");                    // mov    0x10(%ebp),%edx

  asm (".byte 0xb8 0x05 0x00 0x00 0x00");          // mov    $0x5,%eax
  asm (".byte 0xcd 0x80");                         // int    $0x80
}

void
access ()
{
  asm (".byte 0x8b 0x5d 0x08");                    // mov    0x8(%ebp),%ebx
  asm (".byte 0x8b 0x4d 0x0c");                    // mov    0xc(%ebp),%ecx

  asm (".byte 0xb8 0x21 0x00 0x00 0x00");          // mov    $0x21,%eax
  asm (".byte 0xcd 0x80");                         // int    $0x80
}

void
brk ()
{
  asm (".byte 0x8b 0x5d 0x08");                    // mov    0x8(%ebp),%ebx
  asm (".byte 0xb8 0x2d 0x00 0x00 0x00");          // mov    $0x2d,%eax
  asm (".byte 0xcd 0x80");                         // int    $0x80
}

void
fsync ()
{
  asm (".byte 0x8b 0x5d 0x08");                    // mov    0x8(%ebp),%ebx
  asm (".byte 0xb8 0x76 0x00 0x00 0x00");          // mov    $0x76,%eax
  asm (".byte 0xcd 0x80");                         // int    $0x80
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

#if 0

// !__MESC__
// FIXME: mes+nyacc parser bug here
// works fine with Guile, but let's keep a single input source

#define pop_va_arg \
  asm (".byte 0x8b 0x45 0xfc"); /* mov   -<0x4>(%ebp),%eax :va_arg */ \
  asm (".byte 0xc1 0xe0 0x02"); /* shl   $0x2,%eax */ \
  asm (".byte 0x01 0xe8");      /* add   %ebp,%eax */ \
  asm (".byte 0x83 0xc0 0x0c"); /* add   $0xc,%eax */ \
  asm (".byte 0x8b 0x00");      /* mov   (%eax),%eax */ \
  asm (".byte 0x89 0x45 0xf8"); /* mov   %eax,-0x8(%ebp) :va */ \
  asm (".byte 0x50")            /* push   %eax */

#else // __MESC__

#define pop_va_arg asm (".byte 0x8b 0x45 0xfc 0xc1 0xe0 0x02 0x01 0xe8 0x83 0xc0 0x0c 0x8b 0x00 0x89 0x45 0xf8 0x50")

#endif

int
printf (char const* format, int va_args)
{
  int va_arg = 0;
  int va;
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
          case 'c': {pop_va_arg; putchar ((char)va); va_arg++; break;}
          case 'd': {pop_va_arg; puts (itoa (va)); va_arg++; break;}
          case 's': {pop_va_arg; puts ((char*)va); va_arg++; break;}
          default: putchar (*p);
          }
        p++;
      }
  return 0;
}

char **g_environment;
char **
_env (char **e)
{
  return e;
}

int main(int,char*[]);
int
_start ()
{
  asm (".byte 0x89 0xe8");      // mov    %ebp,%eax
  asm (".byte 0x83 0xc0 0x08"); // add    $0x8,%eax
  asm (".byte 0x50");           // push   %eax

  asm (".byte 0x89 0xe8");      // mov    %ebp,%eax
  asm (".byte 0x83 0xc0 0x04"); // add    $0x4,%eax
  asm (".byte 0x0f 0xb6 0x00"); // movzbl (%eax),%eax
  asm (".byte 0x50");           // push   %eax

  asm (".byte 0x89 0xe8");      // mov    %ebp,%eax
  asm (".byte 0x83 0xc0 0x04"); // add    $0x4,%eax
  asm (".byte 0x0f 0xb6 0x00"); // movzbl (%eax),%eax
  asm (".byte 0x83 0xc0 0x03"); // add    $0x3,%eax
  asm (".byte 0xc1 0xe0 0x02"); // shl    $0x2,%eax
  asm (".byte 0x01 0xe8");      // add    %ebp,%eax
  asm (".byte 0x50");           // push   %eax

  g_environment = _env ();
  asm (".byte 0x58");
  int r = main ();
  exit (r);
}
