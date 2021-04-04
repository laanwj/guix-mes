/* -*-comment-start: "//";comment-end:""-*-
 * GNU Mes --- Maxwell Equations of Software
 * Copyright © 2016,2017,2018,2019 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
 * Copyright © 2021 W. J. van der Laan <laanwj@protonmail.com>
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

#include <linux/riscv64/syscall.h>

// *INDENT-OFF*
static long
__sys_call_internal (long sys_call)
{
  register long __a7 asm ("a7") = sys_call;
  register long __a0 asm ("a0");
  asm volatile (
       "ecall\n\t"
       : "=r" (__a0)
       : "r" (__a7)
       );
  return __a0;
}

static long
__sys_call2_internal (long sys_call, long one, long two)
{
  register long __a7 asm ("a7") = sys_call;
  register long __a0 asm ("a0") = one;
  register long __a1 asm ("a1") = two;
  asm volatile (
       "ecall\n\t"
       : "+r" (__a0)
       : "r" (__a7), "r" (__a1)
       );
  return __a0;
}
// *INDENT-ON*

/* Returns < 0 on error (errno-like value from kernel), or 0 on success */
int
__raise (int signum)
{
  long pid = __sys_call_internal (SYS_getpid);
  if (pid < 0)
    return pid;
  else
    return __sys_call2_internal (SYS_kill, pid, signum);
}
