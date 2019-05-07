/* -*-comment-start: "//";comment-end:""-*-
 * GNU Mes --- Maxwell Equations of Software
 * Copyright © 2017,2018 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
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

#include <mes/lib-mini.h>

#if 0
#include <linux/x86_64/syscall.h>

#define _write _xwrite
#define write xwrite
#define strlen xstrlen
#define oputs xoputs

#if __GNUC__
#define SYS_write   "0x01"
#define SYS_exit    "0x3c"

ssize_t
_write (int filedes, void const *buffer, size_t size)
{
  long r;
  asm (
       "mov     $"SYS_write",%%rax\n\t"
       "mov     %1,%%rdi\n\t"
       "mov     %2,%%rsi\n\t"
       "mov     %3,%%rdx\n\t"
       "syscall \n\t"
       "mov     %%rax,%0\n\t"
       : "=r" (r)
       : "rm" (filedes), "rm" (buffer), "rm" (size)
       : "rax", "rdi", "rsi", "rdx"
       );
  return r;
}
#else
#define SYS_write   0x01
#define SYS_exit    0x3c
void
_write (int filedes, void const *buffer, size_t size)
{
  asm ("mov____0x8(%rbp),%rdi !0x10");
  asm ("mov____0x8(%rbp),%rsi !0x18");
  asm ("mov____0x8(%rbp),%rdx !0x20");
  asm ("mov____$i32,%rax SYS_write");

  asm ("syscall");
}
#endif

ssize_t
write (int filedes, void const *buffer, size_t size)
{
  int r = _write (filedes, buffer, size);
  if (r < 0)
    {
      errno = -r;
      r = -1;
    }
  else
    errno = 0;
  return r;
}

size_t
strlen (char const* s)
{
  int i = 0;
  while (s[i])
    i++;
  return i;
}

int
oputs (char const* s)
{
  int i = strlen (s);
  write (1, s, i);
  return 0;
}
#endif

int
main ()
{
  oputs ("\n");
  oputs ("mes\n");
  return 0;
}
