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

#include <errno.h>

int
_sys_call (int sys_call)
{
#if !__TINYC__
  int r;
  asm (
       "mov    %1,%%eax\n\t"
       "int    $0x80\n\t"
       "mov    %%eax,%0\n\t"
       : "=r" (r)
       : "" (sys_call)
       : "eax"
       );
  if (r < 0)
    {
      errno = -r;
      r = -1;
    }
  else
    errno = 0;
  return r;
#endif
}

int
_sys_call1 (int sys_call, int one)
{
#if !__TINYC__
  int r;
  asm (
       "mov    %1,%%eax\n\t"
       "mov    %2,%%ebx\n\t"
       "int    $0x80\n\t"
       "mov    %%eax,%0\n\t"
       : "=r" (r)
       : "" (sys_call), "" (one)
       : "eax", "ebx"
       );
  if (r < 0)
    {
      errno = -r;
      r = -1;
    }
  else
    errno = 0;
  return r;
#endif
}

int
_sys_call2 (int sys_call, int one, int two)
{
#if !__TINYC__
  int r;
  asm (
       "mov    %1,%%eax\n\t"
       "mov    %2,%%ebx\n\t"
       "mov    %3,%%ecx\n\t"
       "int    $0x80\n\t"
       "mov    %%eax,%0\n\t"
       : "=r" (r)
       : "" (sys_call), "" (one), "" (two)
       : "eax", "ebx", "ecx"
       );
  if (r < 0)
    {
      errno = -r;
      r = -1;
    }
  else
    errno = 0;
  return r;
#endif
}

int
_sys_call3 (int sys_call, int one, int two, int three)
{
#if !__TINYC__
  int r;
  asm (
       "mov    %2,%%ebx\n\t"
       "mov    %3,%%ecx\n\t"
       "mov    %4,%%edx\n\t"
       "mov    %1,%%eax\n\t"
       "int    $0x80\n\t"
       "mov    %%eax,%0\n\t"
       : "=r" (r)
       : "" (sys_call), "" (one), "" (two), "" (three)
       : "eax", "ebx", "ecx", "edx"
       );
  if (r < 0)
    {
      errno = -r;
      r = -1;
    }
  else
    errno = 0;
  return r;
#endif
}
