/* -*-comment-start: "//";comment-end:""-*-
 * GNU Mes --- Maxwell Equations of Software
 * Copyright © 2016,2017,2018,2020 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
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

#include <errno.h>
#include <linux/riscv64/syscall.h>

long
__sys_call (long sys_call)
{
  asm ("ld_____%a7,0x10(%fp)");

  asm ("ecall");

  asm ("mv_____%t0,%a0");
}

long
__sys_call1 (long sys_call, long one)
{
  asm ("ld_____%a7,0x10(%fp)");
  asm ("ld_____%a0,0x18(%fp)");

  asm ("ecall");

  asm ("mv_____%t0,%a0");
}

long
__sys_call2 (long sys_call, long one, long two)
{
  asm ("ld_____%a7,0x10(%fp)");
  asm ("ld_____%a0,0x18(%fp)");
  asm ("ld_____%a1,0x20(%fp)");

  asm ("ecall");

  asm ("mv_____%t0,%a0");
}

long
__sys_call3 (long sys_call, long one, long two, long three)
{
  asm ("ld_____%a7,0x10(%fp)");
  asm ("ld_____%a0,0x18(%fp)");
  asm ("ld_____%a1,0x20(%fp)");
  asm ("ld_____%a2,0x28(%fp)");

  asm ("ecall");

  asm ("mv_____%t0,%a0");
}

long
__sys_call4 (long sys_call, long one, long two, long three, long four)
{
  asm ("ld_____%a7,0x10(%fp)");
  asm ("ld_____%a0,0x18(%fp)");
  asm ("ld_____%a1,0x20(%fp)");
  asm ("ld_____%a2,0x28(%fp)");
  asm ("ld_____%a3,0x30(%fp)");

  asm ("ecall");

  asm ("mv_____%t0,%a0");
}

long
__sys_call5 (long sys_call, long one, long two, long three, long four, long five)
{
  asm ("ld_____%a7,0x10(%fp)");
  asm ("ld_____%a0,0x18(%fp)");
  asm ("ld_____%a1,0x20(%fp)");
  asm ("ld_____%a2,0x28(%fp)");
  asm ("ld_____%a3,0x30(%fp)");
  asm ("ld_____%a4,0x38(%fp)");

  asm ("ecall");

  asm ("mv_____%t0,%a0");
}

long
_sys_call (long sys_call)
{
  long r = __sys_call (sys_call);
  if (r < 0)
    {
      errno = -r;
      r = -1;
    }
  else
    errno = 0;
  return r;
}

long
_sys_call1 (long sys_call, long one)
{
  long r = __sys_call1 (sys_call, one);
  if (r < 0)
    {
      errno = -r;
      r = -1;
    }
  else
    errno = 0;
  return r;
}

long
_sys_call2 (long sys_call, long one, long two)
{
  long r = __sys_call2 (sys_call, one, two);
  if (r < 0)
    {
      errno = -r;
      r = -1;
    }
  else
    errno = 0;
  return r;
}

long
_sys_call3 (long sys_call, long one, long two, long three)
{
  long r = __sys_call3 (sys_call, one, two, three);
  if (r < 0)
    {
      errno = -r;
      r = -1;
    }
  else
    errno = 0;
  return r;
}

long
_sys_call4 (long sys_call, long one, long two, long three, long four)
{
  long r = __sys_call4 (sys_call, one, two, three, four);
  if (r < 0)
    {
      errno = -r;
      r = -1;
    }
  else
    errno = 0;
  return r;
}

long
_sys_call5 (long sys_call, long one, long two, long three, long four, long five)
{
  long r = __sys_call5 (sys_call, one, two, three, four, five);
  if (r < 0)
    {
      errno = -r;
      r = -1;
    }
  else
    errno = 0;
  return r;
}
