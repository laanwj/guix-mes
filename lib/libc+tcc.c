/* -*-comment-start: "//";comment-end:""-*-
 * Mes --- Maxwell Equations of Software
 * Copyright © 2017 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
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

#include <setjmp.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <signal.h>
#include <sys/mman.h>
#include <sys/time.h>
#include <unistd.h>

int
dlclose (void *handle)
{
  return 0;
}

void *
dlopen (char const *filename, int flags)
{
  return 0;
}

int
execvp (char const *file, char *const argv[])
{
  eputs ("execvp stub\n");
  return 0;
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
  eputs ("fflush stub\n");
  return 0;
}

FILE *
fopen (char const *pathname, char const *mode)
{
  eputs ("fopen stub\n");
  return 0;
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

size_t
fread (void *ptr, size_t size, size_t nmemb, FILE *stream)
{
  eputs ("fread stub\n");
  return 0;
}

int
fseek (FILE *stream, long offset, int whence)
{
  eputs ("fseek stub\n");
  return 0;
}

long
ftell (FILE *stream)
{
  eputs ("ftell stub\n");
  return 0;
}

size_t
fwrite (void const *ptr, size_t size, size_t nmemb, FILE *stream)
{
  int fd = (int)stream;
  return write (fd, ptr, size * nmemb);
}

int
gettimeofday (struct timeval *tv, struct timezone *tz)
{
  return 0;
}

struct tm *
localtime (time_t const *timep)
{
  eputs ("localtime stub\n");
  return 0;
}

void *
memset (void *s, int c, size_t n)
{
  char *p = s;
  while (n--) *p++ = c;
  return s;
}

int
memcmp (void const *s1, void const *s2, size_t n)
{
  char *a = s1;
  char *b = s2;
  while (*a == *b && --n) {a++;b++;}
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
  eputs ("remove stub\n");
  return 0;
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
sscanf (char const *str, const char *format, ...)
{
  eputs ("sscanf stub\n");
  return 0;
}

char *
strcat (char *dest, char const *src)
{
  char *p = strchr (dest, '\0');
  while (*src++) *p++ = *src++;
  *p = 0;
  return dest;
}

char *
strchr (char const *s, int c)
{
  char const *p = s;
  while (*p || !c)
    {
      if (c == *p) return p;
      *p++;
    }
  return 0;
}

char *
strrchr (char const *s, int c)
{
  int n = strlen (s);
  if (!n) return 0;
  char const *p = s + n - 1;
  while (*p || !c)
    {
      if (c == *p) return p;
      *p--;
    }
  return 0;
}

char *
strstr (char const *haystack, char const *needle)
{
  eputs ("strstr stub\n");
  return 0;
}

long
strtol (char const *nptr, char **endptr, int base)
{
  if (!strncmp (nptr, "0x", 2))
    {
      char const *p = nptr + 2;
      return abtoi (&p, 16);
    }
  return abtoi (&nptr, base);
}

long long int
strtoll (char const *nptr, char **endptr, int base)
{
  eputs ("strtoll stub\n");
  return 0;
}

unsigned long
strtoul (char const *nptr, char **endptr, int base)
{
  eputs ("strtoul stub\n");
  return 0;
}

unsigned long long
strtoull (char const *p, char **endptr, int base)
{
  *endptr = p;
  return abtoi (endptr, base);
}

time_t time (time_t *tloc)
{
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
vfprintf (FILE* f, char const* format, va_list ap)
{
  int fd = (int)f;
  char const *p = format;
  while (*p)
    if (*p != '%')
      fputc (*p++, fd);
    else
      {
        p++;
        char c = *p;
        switch (c)
          {
          case '%': {fputc (*p, fd); break;}
          case 'c': {char c; c = va_arg (ap, char); fputc (c, fd); break;}
          case 'd': {int d; d = va_arg (ap, int); fputs (itoa (d), fd); break;}
          case 's': {char *s; s = va_arg (ap, char *); fputs (s, fd); break;}
          default: {fputc (*p, fd); break;}
          }
        p++;
      }
  va_end (ap);
  return 0;
}
