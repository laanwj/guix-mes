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
#include <stdlib.h>

#if !POSIX

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
chmod (char const *s, int mode)
{
#if !__TINYC__
  int r;
  //syscall (SYS_chmod, mode));
  asm (
       "mov    %1,%%ebx\n\t"
       "mov    %2,%%ecx\n\t"
       "mov    $0x0f,%%eax\n\t"
       "int    $0x80\n\t"
       "mov    %%eax,%0\n\t"
       : "=r" (r)
       : "" (s), "" (mode)
       : "eax", "ebx", "ecx"
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

#endif //!POSIX
