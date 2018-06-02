/* -*-comment-start: "//";comment-end:""-*-
 * Mes --- Maxwell Equations of Software
 * Copyright © 2018 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
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

#define SYS_link   "0x09"
#define SYS_rename "0x26"
#define SYS_mkdir  "0x27"
#define SYS_dup    "0x29"
#define SYS_pipe   "0x2a"
#define SYS_lstat  "0x6b"
#define SYS_fstat  "0x6c"

#define SYS_kill 0x25
#define SYS_nanosleep 0xa2

int
link (char const *old_name, char const *new_name)
{
#if !__TINYC__
  int r;
  asm (
       "mov    %1,%%ebx\n\t"
       "mov    %2,%%ecx\n\t"

       "mov    $"SYS_link",%%eax\n\t"
       "int  $0x80\n\t"

       "mov    %%eax,%0\n\t"
       : "=r" (r)
       : "" (old_name), "" (new_name)
       : "eax", "ebx", "ecx"
       );
  return r;
#endif
}

int
kill (pid_t pid, int signum)
{
  return _sys_call2 (SYS_kill, pid, signum);
}

int
rename (char const *old_name, char const *new_name)
{
#if !__TINYC__
  int r;
  asm (
       "mov    %1,%%ebx\n\t"
       "mov    %2,%%ecx\n\t"

       "mov    $"SYS_rename",%%eax\n\t"
       "int  $0x80\n\t"

       "mov    %%eax,%0\n\t"
       : "=r" (r)
       : "" (old_name), "" (new_name)
       : "eax", "ebx", "ecx"
       );
  return r;
#endif
}

int
mkdir (char const *s, mode_t mode)
{
#if !__TINYC__
  int r;
  asm (
       "mov    %1,%%ebx\n\t"
       "mov    %2,%%ecx\n\t"
       "mov    $"SYS_mkdir",%%eax\n\t"
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
dup (int old)
{
#if !__TINYC__
  int r;
  asm (
       "mov    %0,%%ebx\n\t"
       "mov    $"SYS_dup",%%eax\n\t"
       "int    $0x80"
       : "=r" (r)
       : "" (old)
       );
  return r;
#endif
}

int
pipe (int filedes[2])
{
#if !__TINYC__
  int r;
  asm (
       "mov    %0,%%ebx\n\t"
       "mov    $"SYS_pipe",%%eax\n\t"
       "int    $0x80"
       : "=r" (r)
       : "" (filedes)
       );
  return r;
#endif
}

int
lstat (char const *file_name, struct stat *statbuf)
{
#if !__TINYC__
  int r;
  asm (
       "mov    %1,%%ebx\n\t"
       "mov    %2,%%ecx\n\t"

       "mov    $"SYS_lstat",%%eax\n\t"
       "int  $0x80\n\t"

       "mov    %%eax,%0\n\t"
       : "=r" (r)
       : "" (file_name), "" (statbuf)
       : "eax", "ebx", "ecx"
       );
  return r;
#endif
}

int
nanosleep (const struct timespec *requested_time,
           struct timespec *remaining)
{
  return _sys_call2 (SYS_execve, (int)requested_time, (int)remaining);
}

int
fstat (int fd, struct stat *statbuf)
{
#if !__TINYC__
  int r;
  asm (
       "mov    %1,%%ebx\n\t"
       "mov    %2,%%ecx\n\t"

       "mov    $"SYS_fstat",%%eax\n\t"
       //"mov    $"SYS_oldfstat",%%eax\n\t"
       "int  $0x80\n\t"

       "mov    %%eax,%0\n\t"
       : "=r" (r)
       : "" (fd), "" (statbuf)
       : "eax", "ebx", "ecx"
       );
  return r;
#endif
}
