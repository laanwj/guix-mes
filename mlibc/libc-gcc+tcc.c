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

#define FULL_MALLOC 1
#include <libc-gcc.c>
#include <libc-mes+tcc.c>

int errno;

#include <unistd.h>
#include <stdarg.h>
#include <signal.h>
#include <sys/mman.h>
#include <sys/time.h>

#define SYS_exit   "0x01"
#define SYS_read   "0x03"
#define SYS_write  "0x04"
#define SYS_open   "0x05"
#define SYS_close  "0x06"
#define SYS_unlink "0x0a"
#define SYS_lseek  "0x13"
#define SYS_access "0x21"
#define SYS_brk    "0x2d"
#define SYS_fsync  "0x76"
#define SYS_getcwd "0xb7"

int
close (int fd)
{
  int r;
  asm (
       "mov    %0,%%ebx\n\t"
       "mov    $"SYS_close",%%eax\n\t"
       "int    $0x80"
       : "=r" (r)
       : "" (fd)
       );
  return r;
}

int
unlink (char const *file_name)
{
  int r;
  asm (
       "mov    %0,%%ebx\n\t"
       "mov    $"SYS_unlink",%%eax\n\t"
       "int    $0x80"
       : "=r" (r)
       : "" (file_name)
       );
  return r;
}

off_t
lseek (int fd, off_t offset, int whence)
{
  int r;
  asm (
       "mov    %1,%%ebx\n\t"
       "mov    %2,%%ecx\n\t"
       "mov    %3,%%edx\n\t"

       "mov    $"SYS_lseek",%%eax\n\t"
       "int  $0x80\n\t"

       "mov    %%eax,%0\n\t"
       : "=r" (r)
       : "" (fd), "" (offset), "" (whence)
       : "eax", "ebx", "ecx", "edx"
       );
  return r;
}

char *
getcwd (char *buf, size_t size)
{
  int r;
  asm (
       "mov    %1,%%ebx\n\t"
       "mov    %2,%%ecx\n\t"

       "mov    $"SYS_getcwd",%%eax\n\t"
       "int  $0x80\n\t"

       "mov    %%eax,%0\n\t"
       : "=r" (r)
       : "" (buf), "" (size)
       : "eax", "ebx", "ecx"
       );
  return r;
}

int dlclose (void *handle)
{
  return 0;
}

void *
dlopen (char const *filename, int flags)
{
  return 0;
}

int
mprotect (void *addr, size_t len, int prot)
{
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

char *
strcat (char *dest, char const *src)
{
  return 0;
}

int
vfprintf (FILE* f, char const* format, va_list ap)
{
  int fd = (int)f;
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

int
__udivdi3 (int a, int b)
{
  return a / b;
}

int
__umoddi3 (int a, int b)
{
  return a % b;
}
