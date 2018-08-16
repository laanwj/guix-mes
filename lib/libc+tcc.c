/* -*-comment-start: "//";comment-end:""-*-
 * GNU Mes --- Maxwell Equations of Software
 * Copyright © 2017,2018 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
 * Copyright © 2018 Jeremiah Orians <jeremiah@pdp10.guru>
 * Copyright (C) 2018 Han-Wen Nienhuys <hanwen@xs4all.nl>
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

#include <errno.h>
#include <fcntl.h>
#include <limits.h>
#include <setjmp.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <signal.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <unistd.h>

#include <libc.c>

#if __GNU__
#include <hurd/tcc.c>
#elif __linux__
#include <linux/tcc.c>
#else
#error both __GNU__ and _linux__ are undefined, choose one
#endif

#if __MESC__
#include <x86-mes/setjmp.c>
#elif __i386__
#include <x86-mes-gcc/setjmp.c>
#elif __x86_64__
#include <x86_64-mes-gcc/setjmp.c>
#else
#error arch not supported
#endif

char *
search_path (char const *file_name)
{
  static char buf[256];
  char *path = getenv ("PATH");
  if (__mes_debug ())
    {
      eputs ("\n search-path: "); eputs (file_name); eputs ("\n");
    }
  while (*path)
    {
      char *end = strchr (path, ':');
      if (!end)
        end = strchr (path, '\0');
      strncpy (buf, path, end - path);
      buf[end - path] = 0;
      if (__mes_debug ())
        {
          eputs (" dir: "); eputs (buf); eputs ("\n");
        }
      if (buf[end - path] != '/')
        strcat (buf, "/");
      strcat (buf, file_name);
      if (!access (buf, X_OK))
        {
          if (__mes_debug ())
            {
              eputs (" found: "); eputs (buf); eputs ("\n");
            }
          return buf;
        }
      path = end + 1;
    }
  return 0;
}

int
execvp (char const *file_name, char *const argv[])
{
  if (file_name[0] != '/')
    file_name = search_path (file_name);
  if (!file_name)
    {
      errno = ENOENT;
      return -1;
    }
  if (__mes_debug ())
    {
      eputs (" EXEC: "); eputs (file_name); eputs ("\n");
      int i = 0;
      while (argv[i])
        {
          eputs (" arg["); eputs (itoa (i)); eputs ("]: "); eputs (argv[i]); eputs ("\n");
          i++;
        }
    }
  return execve (file_name, argv, environ);
}

int
fclose (FILE *stream)
{
  int fd = (int)stream;
  return close (fd);
}

FILE *
fdopen (int fd, char const *mode)
{
  return (FILE*)fd;
}

int
ferror (FILE *stream)
{
  int fd = (int)stream;
  if (fd == -1) return -1;
  return 0;
}

int
fflush (FILE *stream)
{
  fsync ((int)stream);
}

int
fprintf (FILE *stream, char const *format, ...)
{
  va_list ap;
  va_start (ap, format);
  int r = vfprintf (stream, format, ap);
  va_end (ap);
  return r;
}

int
_fungetc_p (FILE *stream)
{
  return _fdungetc_p ((int)stream);
}

size_t
fread (void *data, size_t size, size_t count, FILE *stream)
{
  if (! size || !count)
    return 0;

  size_t todo = size * count;
  char *buf = (char*)data;

  int bytes = 0;
  while (_fungetc_p (stream) && todo-- && ++bytes)
    *buf++ = fgetc (stream);
  if (todo)
    {
      int r = read ((int)stream, buf, todo);
      if (r < 0 && !bytes)
        bytes = r;
      else
        bytes += r;
    }

  if (__mes_debug ())
    {
      eputs ("fread fd="); eputs (itoa ((int)stream));
      eputs (" bytes="); eputs (itoa (bytes)); eputs ("\n");
      static char buf[4096];
      if (bytes > 0 && bytes < sizeof (buf))
        {
          strncpy (buf, data, bytes);
          buf[bytes] = 0;
          eputs ("fread buf="); eputs (buf); eputs ("\n");
        }
    }

  if (bytes > 0)
    return bytes/size;

  return 0;
}

size_t
fwrite (void const *data, size_t size, size_t count, FILE *stream)
{
  if (__mes_debug () > 1)
    {
      eputs ("fwrite "); eputs (itoa ((int)stream));
      eputs ("  "); eputs (itoa (size)); eputs ("\n");
    }

  if (! size || !count)
    return 0;
  int bytes = write ((int)stream, data, size * count);

  if (__mes_debug () > 2)
    {
      eputs (" => "); eputs (itoa (bytes)); eputs ("\n");
    }

  if (bytes > 0)
    return bytes/size;
  return 0;
}

long
ftell (FILE *stream)
{
  return lseek ((int)stream, 0, SEEK_CUR);
}

FILE*
fopen (char const *file_name, char const *opentype)
{
  if (__mes_debug ())
    {
      eputs ("fopen "); eputs (file_name);
      eputs (" "); eputs (opentype); eputs ("\n");
    }

  int fd;
  int mode = 0600;
  if ((opentype[0] == 'a' || !strcmp (opentype, "r+"))
      && !access (file_name, O_RDONLY))
    {
      int flags = O_RDWR;
      if (opentype[0] == 'a')
        flags |= O_APPEND;
      fd = open (file_name, flags, mode);
    }
  else if (opentype[0] == 'w' || opentype[0] == 'a' || !strcmp (opentype, "r+"))
    {
      char *plus_p = strchr (opentype, '+');
      int flags = plus_p ? O_RDWR | O_CREAT : O_WRONLY | O_CREAT | O_TRUNC;
      fd = open (file_name, flags, mode);
    }
  else
    fd = open (file_name, 0, 0);

  if (__mes_debug ())
    {
      eputs (" => fd="); eputs (itoa (fd)); eputs ("\n");
    }

  if (!fd)
    {
      eputs (" ***MES LIB C*** fopen of stdin: signal me in band\n");
      exit (1);
    }
  if (fd < 0)
    fd = 0;
  return (FILE*)fd;
}

int
fseek (FILE *stream, long offset, int whence)
{
  int pos = lseek ((int)stream, offset, whence);
  if (__mes_debug ())
    {
      eputs ("fread fd="); eputs (itoa ((int)stream));
      eputs ("  =>"); eputs (itoa (pos)); eputs ("\n");
    }
  if (pos >= 0)
    return 0;
  return -1;
}

int
gettimeofday (struct timeval *tv, struct timezone *tz)
{
  static int stub = 0;
  if (__mes_debug () && !stub)
    eputs ("gettimeofday stub\n");
  stub = 1;
  errno = 0;
  return 0;
}

double
ldexp (double x, int exp)
{
  static int stub = 0;
  if (__mes_debug () && !stub)
    eputs ("ldexp stub\n");
  stub = 1;
  return 0;
}

struct tm *
localtime (time_t const *timep)
{
  static int stub = 0;
  if (__mes_debug () && !stub)
    eputs ("localtime stub\n");
  stub = 1;
  errno = 0;
  return 0;
}

void *
memmove (void *dest, void const *src, size_t n)
{
  if (dest < src)
    return memcpy (dest, src, n);
  char *p = dest + n;
  char const *q = src +n;
  while (n--)
    *--p = *--q;
  return dest;
}

void *
memset (void *s, int c, size_t n)
{
  char *p = s;
  while (n--) *p++ = c;
  return s;
}

int
memcmp (void const *s1, void const *s2, size_t size)
{
  if (!size)
    return 0;
  char *a = s1;
  char *b = s2;
  while (*a == *b && --size)
    {
      a++;
      b++;
    }
  return *a - *b;
}

int
mprotect (void *addr, size_t len, int prot)
{
  return 0;
}

void
qswap (void *a, void *b, size_t size)
{
  char *buf[8];
  memcpy (buf, a, size);
  memcpy (a, b, size);
  memcpy (b, buf, size);
}

size_t
qpart (void *base, size_t count, size_t size, int (*compare)(void const *, void const *))
{
  void* p = base + count*size;
  size_t i = 0;
  for (size_t j = 0; j < count; j++)
    {
      if (compare (base+j*size, p) < 0)
        {
          qswap (base+i*size, base+j*size, size);
          i++;
        }
    }
  if (compare (base+count*size, base+i*size) < 0)
    qswap (base+i*size, base+count*size, size);
  return i;
}

void
qsort (void *base, size_t count, size_t size, int (*compare)(void const *, void const *))
{
  if (count > 1)
    {
      int p = qpart (base, count-1, size, compare);
      qsort (base, p, size, compare);
      qsort (base+p*size, count-p, size, compare);
    }
}

int
remove (char const *file_name)
{
  struct stat buf;
  if (stat (file_name, &buf) < 0)
    return -1;
  if (S_ISDIR (buf.st_mode))
    return rmdir (file_name);
  return unlink (file_name);
}

int
sigaction (int signum, struct sigaction const *act, struct sigaction *oldact)
{
  return 0;
}

int
sigemptyset (sigset_t *set)
{
  return 0;
}

int
snprintf(char *str,  size_t size,  char const *format, ...)
{
  va_list ap;
  va_start (ap, format);
  int r = vsprintf (str, format, ap);
  va_end (ap);
  return r;
}

int
sscanf (char const *str, const char *template, ...)
{
  va_list ap;
  va_start (ap, template);
  int r = vsscanf (str, template, ap);
  va_end (ap);
  return r;
}

char *
strcat (char *to, char const *from)
{
  char *p = strchr (to, '\0');
  while (*from)
    *p++ = *from++;
  *p = 0;
  return to;
}

char *
strchr (char const *s, int c)
{
  char const *p = s;
  while (*p || !c)
    {
      if (c == *p)
        return p;
      p++;
    }
  return 0;
}

char *
strncpy (char *to, char const *from, size_t size)
{
  if (size == 0)
    return to;
  char *p = to;
  while (*from && size--)
    *p++ = *from++;
  if (*from)
    size++;
  while (size--)
    *p++ = 0;
  return to;
}

char *
strrchr (char const *s, int c)
{
  int n = strlen (s);
  if (!n)
    return 0;
  char const *p = s + n;
  if (!*p && !c)
    return p;
  p--;
  while (n-- && (*p || !c))
    {
      if (c == *p)
        return p;
      p--;
    }
  return 0;
}

/** locate a substring. #memmem# finds the first occurrence of
    #needle# in #haystack#.  This is not ANSI-C.

    The prototype is not in accordance with the Linux Programmer's
    Manual v1.15, but it is with /usr/include/string.h   */

unsigned char *
_memmem (unsigned char const *haystack, int haystack_len,
         unsigned char const *needle, int needle_len)
{
  unsigned char const *end_haystack = haystack + haystack_len - needle_len + 1;
  unsigned char const *end_needle = needle + needle_len;

  /* Ahhh ... Some minimal lowlevel stuff. This *is* nice; Varation
     is the spice of life */
  while (haystack < end_haystack)
    {
      unsigned char const *subneedle = needle;
      unsigned char const *subhaystack = haystack;
      while (subneedle < end_needle)
        if (*subneedle++ != *subhaystack++)
          goto next;

      /* Completed the needle.  Gotcha.  */
      return (unsigned char *) haystack;
    next:
      haystack++;
    }
  return 0;
}

void *
memmem (void const *haystack, int haystack_len,
        void const *needle, int needle_len)
{
  unsigned char const *haystack_byte_c = (unsigned char const *)haystack;
  unsigned char const *needle_byte_c = (unsigned char const *)needle;
  return _memmem (haystack_byte_c, haystack_len, needle_byte_c, needle_len);
}

char *
strstr (char const *haystack, char const *needle)
{
  return memmem (haystack, strlen (haystack), needle, strlen (needle));
}

double
strtod (char const *string, char **tailptr)
{
  static int stub = 0;
  if (__mes_debug () && !stub)
    eputs ("strtod stub\n");
  stub = 1;
  return 0;
}

float
strtof (char const *string, char **tailptr)
{
  return strtod (string, tailptr);
}

long double
strtold (char const *string, char **tailptr)
{
  return strtod (string, tailptr);
}

long
strtol (char const *string, char **tailptr, int base)
{
  if (!strncmp (string, "0x", 2))
    {
      string += 2;
      base = 16;
    }
  if (tailptr)
    {
      *tailptr = string;
      return abtol (tailptr, base);
    }
  char **p = &string;
  return abtol (p, base);
}

long long int
strtoll (char const *string, char **tailptr, int base)
{
  return strtol (string, tailptr, base);
}

unsigned long
strtoul (char const *string, char **tailptr, int base)
{
  return strtol (string, tailptr, base);
}

unsigned long long
strtoull (char const *string, char **tailptr, int base)
{
  return strtol (string, tailptr, base);
}

time_t
time (time_t *tloc)
{
  static int stub = 0;
  if (__mes_debug () && !stub)
    eputs ("time stub\n");
  stub = 1;
  errno = 0;
  return 0;
}

int
vsnprintf (char *str, size_t size, char const *format, va_list ap)
{
  return vsprintf (str, format, ap);
}

void *
calloc (size_t nmemb, size_t size)
{
  size_t count = nmemb * size;
  void *p = malloc (count);
  memset (p, 0, count);
  return p;
}

int
islower (int c)
{
  return c >= 'a' && c <= 'z';
}

int
isupper (int c)
{
  return c >= 'A' && c <= 'Z';
}

int
tolower (int c)
{
  if (isupper (c))
    return c + ('a' - 'A');
  return c;
}

int
toupper (int c)
{
  if (islower (c))
    return c - ('a' - 'A');
  return c;
}

char *
strlwr (char *string)
{
  char *p = string;
  while (*p)
    {
      *p = tolower (*p);
      p++;
    }
  return string;
}

char *
strupr (char *string)
{
  char *p = string;
  while (*p)
    {
      *p = toupper (*p);
      p++;
    }
  return string;
}

int
vfprintf (FILE* f, char const* format, va_list ap)
{
  int fd = (int)f;
  char const *p = format;
  int count = 0;
  while (*p)
    if (*p != '%')
      {
        count++;
        fputc (*p++, fd);
      }
    else
      {
        p++;
        char c = *p;
        int left_p = 0;
        int precision = -1;
        int width = -1;
        if (c == '-')
          {
            left_p = 1;
            c = *++p;
          }
        char pad = ' ';
        if (c == '0')
          {
            pad = c;
            c = *p++;
          }
        if (c >= '0' && c <= '9')
          {
            width = abtol (&p, 10);
            c = *p;
          }
        else if (c == '*')
          {
            width = va_arg (ap, int);
            c = *++p;
          }
        if (c == '.')
          {
            c = *++p;
            if (c >= '0' && c <= '9')
              {
                precision = abtol (&p, 10);
                c = *p;
              }
            else if (c == '*')
              {
                precision = va_arg (ap, int);
                c = *++p;
              }
          }
        if (c == 'l')
          c = *++p;
        if (c == 'l')
          {
            eputs ("vfprintf: skipping second: l\n");
            c = *++p;
          }
        switch (c)
          {
          case '%': {fputc (*p, fd); count++; break;}
          case 'c': {char c; c = va_arg (ap, int); fputc (c, fd); break;}
          case 'd':
          case 'i':
          case 'o':
          case 'u':
          case 'x':
          case 'X':
            {
              int d = va_arg (ap, int);
              int base = c == 'o' ? 8
                : c == 'x' || c == 'X' ? 16
                : 10;
              char const *s = ntoab (d, base, c != 'u' && c != 'x' && c != 'X');
              if (c == 'X')
                strupr (s);
              int length = strlen (s);
              if (precision == -1)
                precision = length;
              if (!left_p)
                {
                  while (width-- > precision)
                    {
                      fputc (pad, f);
                      count++;
                    }
                  while (precision > length)
                    {
                      fputc ('0', f);
                      precision--;
                      width--;
                      count++;
                    }
                }
              while (*s)
                {
                  if (precision-- <= 0)
                    break;
                  width--;
                  fputc (*s++, f);
                  count++;
                }
              while (width > 0)
                {
                  width--;
                  fputc (pad, f);
                  count++;
                }
              break;
            }
          case 's':
            {
              char *s = va_arg (ap, char *);
              int length = strlen (s);
              if (precision == -1)
                precision = length;
              if (!left_p)
                {
                  while (width-- > precision)
                    {
                      fputc (pad, f);
                      count++;
                    }
                  while (precision > length)
                    {
                      fputc (' ', f);
                      precision--;
                      width--;
                      count++;
                    }
                }
              while (*s)
                {
                  if (precision-- <= 0)
                    break;
                  width--;
                  fputc (*s++, f);
                  count++;
                }
              while (width > 0)
                {
                  width--;
                  fputc (pad, f);
                  count++;
                }
              break;
            }
          case 'n':
            {
              int *n = va_arg (ap, int *);
              *n = count;
              break;
            }
          default:
            {
              eputs ("vfprintf: not supported: %:");
              eputc (c);
              eputs ("\n");
              p++;
            }
          }
        p++;
      }
  va_end (ap);
  return 0;
}

int
vprintf (char const* format, va_list ap)
{
  return vfprintf (STDOUT, format, ap);
}

int
vsscanf (char const *s, char const *template, va_list ap)
{
  char const *p = s;
  char const *t = template;
  int count = 0;
  while (*t && *p)
    if (*t != '%')
      {
        t++;
        p++;
      }
    else
      {
        t++;
        char c = *t;
        if (c == 'l')
          c = *++t;
        switch (c)
          {
          case '%': {p++; break;}
          case 'c':
            {
              char *c = va_arg (ap, char*);
              *c = *p++;
              count++;
              break;
            }
          case 'd':
          case 'i':
          case 'u':
            {
              int *d = va_arg (ap, int*);
              *d = abtol (&p, 10);
              count++;
              break;
            }
          default:
            {
              eputs ("vsscanf: not supported: %:");
              eputc (c);
              eputs ("\n");
              t++;
              p++;
            }
          }
        t++;
      }
  va_end (ap);
  return count;
}

int
vsprintf (char *str, char const* format, va_list ap)
{
  char const *p = format;
  int count = 0;
  while (*p)
    if (*p != '%')
      {
        *str++ = *p++;
        count++;
      }
    else
      {
        p++;
        char c = *p;
        int left_p = 0;
        int precision = -1;
        int width = -1;
        if (c == '-')
          {
            left_p = 1;
            c = *++p;
          }
        char pad = ' ';
        if (c == '0')
          {
            pad = c;
            c = *p++;
          }
        if (c >= '0' && c <= '9')
          {
            width = abtol (&p, 10);
            c = *p;
          }
        else if (c == '*')
          {
            width = va_arg (ap, int);
            c = *++p;
          }
        if (c == '.')
          {
            c = *++p;
            if (c >= '0' && c <= '9')
              {
                precision = abtol (&p, 10);
                c = *p;
              }
            else if (c == '*')
              {
                precision = va_arg (ap, int);
                c = *++p;
              }
          }
        if (c == 'l')
          c = *++p;
        if (c == 'l')
          c = *++p;
        if (c == 'l')
          {
            eputs ("vfprintf: skipping second: l\n");
            c = *++p;
          }
        switch (c)
          {
          case '%': {*str++ = *p; count++; break;}
          case 'c': {c = va_arg (ap, int); *str++ = c; count++; break;}
          case 'd':
          case 'i':
          case 'o':
          case 'u':
          case 'x':
          case 'X':
            {
              int d = va_arg (ap, int);
              int base = c == 'o' ? 8
                : c == 'x' || c == 'X' ? 16
                : 10;
              char const *s = ntoab (d, base, c != 'u' && c != 'x' && c != 'X');
              if (c == 'X')
                strupr (s);
              int length = strlen (s);
              if (precision == -1)
                precision = length;
              if (!left_p)
                {
                  while (width-- > precision)
                    {
                      *str++ = pad;
                      count++;
                    }
                  while (precision > length)
                    {
                      *str++ = '0';
                      precision--;
                      width--;
                      count++;
                    }
                }
              while (*s)
                {
                  if (precision-- <= 0)
                    break;
                  width--;
                  *str++ = *s++;
                  count++;
                }
              while (width > 0)
                {
                  width--;
                  *str++ = pad;
                  count++;
                }
              break;
            }
          case 's':
            {
              char *s = va_arg (ap, char *);
              int length = strlen (s);
              if (precision == -1)
                precision = length;
              if (!left_p)
                {
                  while (width-- > precision)
                    {
                      *str++ = pad;
                      count++;
                    }
                  while (width > length)
                    {
                      *str++ = ' ';
                      precision--;
                      width--;
                      count++;
                    }
                }
              while (*s)
                {
                  if (precision-- <= 0)
                    break;
                  width--;
                  *str++ = *s++;
                  count++;
                }
              while (width > 0)
                {
                  width--;
                  *str++ = pad;
                  count++;
                }
              break;
            }
          case 'n':
            {
              int *n = va_arg (ap, int *);
              *n = count;
              break;
            }
          default:
            {
              eputs ("vsprintf: not supported: %:");
              eputc (c);
              eputs ("\n");
              p++;
            }
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
printf (char const* format, ...)
{
  va_list ap;
  va_start (ap, format);
  int r = vprintf (format, ap);
  va_end (ap);
  return r;
}
