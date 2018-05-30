/* -*-comment-start: "//";comment-end:""-*-
 * Mes --- Maxwell Equations of Software
 * Copyright © 2016,2017,2018 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
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
#include <libmes.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/stat.h>
#include <sys/wait.h>

#define SYS_fork    "0x02"
#define SYS_read    "0x03"
#define SYS_open    "0x05"
#define SYS_waitpid "0x07"
#define SYS_execve  "0x0b"
#define SYS_chmod   "0x0f"
#define SYS_access  "0x21"
#define SYS_brk     "0x2d"
#define SYS_ioctl   "0x36"
#define SYS_fsync   "0x76"

int
fork ()
{
#if !__TINYC__
  int r;
  asm (
       "mov    $"SYS_fork",%%eax\n\t"
       "int    $0x80\n\t"
       "mov    %%eax,%0\n\t"
       : "=r" (r)
       : //no inputs
       : "eax"
       );
  return r;
#endif
}

int
read (int fd, void* buf, size_t n)
{
#if !__TINYC__
  int r;
  asm (
       "mov    %1,%%ebx\n\t"
       "mov    %2,%%ecx\n\t"
       "mov    %3,%%edx\n\t"

       "mov    $"SYS_read",%%eax\n\t"
       "int    $0x80\n\t"

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

       "mov    $"SYS_open",%%eax\n\t"
       "int    $0x80\n\t"
       "mov    %%eax,%0\n\t"
       : "=r" (r)
       : "" (s), "" (flags), "" (mode)
       : "eax", "ebx", "ecx", "edx"
       );
  return r;
#endif
}

pid_t
waitpid (pid_t pid, int *status_ptr, int options)
{
#if !__TINYC__
  int r;
  asm (
       "mov    %1,%%ebx\n\t"
       "mov    %2,%%ecx\n\t"
       "mov    %3,%%edx\n\t"

       "mov    $"SYS_waitpid",%%eax\n\t"
       "int    $0x80\n\t"

       "mov    %%eax,%0\n\t"
       : "=r" (r)
       : "" (pid), "" (status_ptr), "" (options)
       : "eax", "ebx", "ecx", "edx"
       );
  return r;
#endif
}

int
execve (char const* file_name, char *const argv[], char *const env[])
{
#if !__TINYC__
  int r;
  asm (
       "mov    %1,%%ebx\n\t"
       "mov    %2,%%ecx\n\t"
       "mov    %3,%%edx\n\t"

       "mov    $"SYS_execve",%%eax\n\t"
       "int    $0x80\n\t"

       "mov    %%eax,%0\n\t"
       : "=r" (r)
       : "" (file_name), "" (argv), "" (env)
       : "eax", "ebx", "ecx", "edx"
       );
  return r;
#endif
}

int
chmod (char const *s, mode_t mode)
{
#if !__TINYC__
  int r;
  asm (
       "mov    %1,%%ebx\n\t"
       "mov    %2,%%ecx\n\t"

       "mov    $"SYS_chmod",%%eax\n\t"
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
  asm (
       "mov    %1,%%ebx\n\t"
       "mov    %2,%%ecx\n\t"

       "mov    $"SYS_access",%%eax\n\t"
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

       "mov    $"SYS_brk",%%eax\n\t"
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
ioctl (int fd, unsigned long request, ...)
{
#if !__TINYC__
  int p;
  asm (
       "mov    %%ebp,%%eax\n\t"
       "add    $0x10,%%eax\n\t"
       "mov    (%%eax),%%eax\n\t"
       "mov    %%eax,%0\n\t"
       : "=p" (p)
       : //no inputs ""
       );
  int r;
  //syscall (SYS_ioctl, fd));
  asm (
       "mov    %1,%%ebx\n\t"
       "mov    %2,%%ecx\n\t"
       "mov    %3,%%edx\n\t"

       "mov    $"SYS_ioctl",%%eax\n\t"
       "int    $0x80\n\t"
       "mov    %%eax,%0\n\t"
       : "=r" (r)
       : "" (fd), "" (request), "" (p)
       : "eax", "ebx", "ecx", "edx"
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

       "mov    $"SYS_fsync",%%eax\n\t"
       "int    $0x80\n\t"
       "mov    %%eax,%0\n\t"
       : "=r" (r)
       : "" (fd)
       : "eax", "ebx"
       );
  return r;
#endif
}
