/* -*-comment-start: "//";comment-end:""-*-
 * GNU Mes --- Maxwell Equations of Software
 * Copyright © 2016,2017,2018,2019,2020 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
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
#include <linux/x86/syscall.h>

#if !__TINYC__
// *INDENT-OFF*
long
__sys_call (long sys_call)
{
  long r;
  asm (
       "mov    r7, %1\n\t"
       "swi    $0\n\t"
       "mov    %0, r0\n\t"
       : "=r" (r)
       : "r" (sys_call)
       : "r0", "r7"
       );
  return r;
}
#else //__TINYC__
long __sys_call (long sys_call);
__asm__ (".global __sys_call\n");
__asm__ ("__sys_call:\n");
__asm__ (".int 0xe92d0880\n"); //push  {r7, fp}
__asm__ (".int 0xe28db004\n"); //add   fp, sp, #4
__asm__ (".int 0xe24dd010\n"); //sub   sp, sp, #16
__asm__ (".int 0xe50b0010\n"); //str   r0, [fp, #-16]
__asm__ (".int 0xe51b3010\n"); //ldr   r3, [fp, #-16]
__asm__ (".int 0xe1a07003\n"); //mov   r7, r3
__asm__ (".int 0xef000000\n"); //svc   0x00000000
__asm__ (".int 0xe1a03000\n"); //mov   r3, r0
__asm__ (".int 0xe50b3008\n"); //str   r3, [fp, #-8]
__asm__ (".int 0xe51b3008\n"); //ldr   r3, [fp, #-8]
__asm__ (".int 0xe1a00003\n"); //mov   r0, r3
__asm__ (".int 0xe24bd004\n"); //sub   sp, fp, #4
__asm__ (".int 0xe8bd0880\n"); //pop   {r7, fp}
__asm__ (".int 0xe12fff1e\n"); //bx    lr
#endif //__TINYC__

#if !__TINYC__
long
__sys_call1 (long sys_call, long one)
{
  long r;
  asm (
       "mov    r7, %1\n\t"
       "mov    r0, %2\n\t"
       "swi    $0\n\t"
       "mov    %0, r0\n\t"
       : "=r" (r)
       : "r" (sys_call), "r" (one)
       : "r0", "r7"
       );
  return r;
}
#else //__TINYC__
long __sys_call1 (long sys_call, long one);
__asm__ (".global __sys_call1\n");
__asm__ ("__sys_call1:\n");
__asm__ (".int 0xe92d0880\n"); //push  {r7, fp}
__asm__ (".int 0xe28db004\n"); //add   fp, sp, #4
__asm__ (".int 0xe24dd010\n"); //sub   sp, sp, #16
__asm__ (".int 0xe50b0010\n"); //str   r0, [fp, #-16]
__asm__ (".int 0xe50b1014\n"); //str   r1, [fp, #-20]   ; 0xffffffec
__asm__ (".int 0xe51b3010\n"); //ldr   r3, [fp, #-16]
__asm__ (".int 0xe51b2014\n"); //ldr   r2, [fp, #-20]   ; 0xffffffec
__asm__ (".int 0xe1a07003\n"); //mov   r7, r3
__asm__ (".int 0xe1a00002\n"); //mov   r0, r2
__asm__ (".int 0xef000000\n"); //svc   0x00000000
__asm__ (".int 0xe1a03000\n"); //mov   r3, r0
__asm__ (".int 0xe50b3008\n"); //str   r3, [fp, #-8]
__asm__ (".int 0xe51b3008\n"); //ldr   r3, [fp, #-8]
__asm__ (".int 0xe1a00003\n"); //mov   r0, r3
__asm__ (".int 0xe24bd004\n"); //sub   sp, fp, #4
__asm__ (".int 0xe8bd0880\n"); //pop   {r7, fp}
__asm__ (".int 0xe12fff1e\n"); //bx   lr
#endif //__TINYC__

#if !__TINYC__
long
__sys_call2 (long sys_call, long one, long two)
{
  long r;
  asm (
       "mov    r7, %1\n\t"
       "mov    r0, %2\n\t"
       "mov    r1, %3\n\t"
       "swi    $0\n\t"
       "mov    %0, r0\n\t"
       : "=r" (r)
       : "r" (sys_call), "r" (one), "r" (two)
       : "r0", "r1", "r7"
       );
  return r;
}
#else //__TINYC__
long __sys_call2 (long sys_call, long one, long two);
__asm__ (".global __sys_call2\n");
__asm__ ("__sys_call2:\n");
__asm__ (".int 0xe92d0880\n"); //push   {r7, fp}
__asm__ (".int 0xe28db004\n"); //add   fp, sp, #4
__asm__ (".int 0xe24dd018\n"); //sub   sp, sp, #24
__asm__ (".int 0xe50b0010\n"); //str   r0, [fp, #-16]
__asm__ (".int 0xe50b1014\n"); //str   r1, [fp, #-20]   ; 0xffffffec
__asm__ (".int 0xe50b2018\n"); //str   r2, [fp, #-24]   ; 0xffffffe8
__asm__ (".int 0xe51b3010\n"); //ldr   r3, [fp, #-16]
__asm__ (".int 0xe51b2014\n"); //ldr   r2, [fp, #-20]   ; 0xffffffec
__asm__ (".int 0xe51bc018\n"); //ldr   ip, [fp, #-24]   ; 0xffffffe8
__asm__ (".int 0xe1a07003\n"); //mov   r7, r3
__asm__ (".int 0xe1a00002\n"); //mov   r0, r2
__asm__ (".int 0xe1a0100c\n"); //mov   r1, ip
__asm__ (".int 0xef000000\n"); //svc   0x00000000
__asm__ (".int 0xe1a03000\n"); //mov   r3, r0
__asm__ (".int 0xe50b3008\n"); //str   r3, [fp, #-8]
__asm__ (".int 0xe51b3008\n"); //ldr   r3, [fp, #-8]
__asm__ (".int 0xe1a00003\n"); //mov   r0, r3
__asm__ (".int 0xe24bd004\n"); //sub   sp, fp, #4
__asm__ (".int 0xe8bd0880\n"); //pop   {r7, fp}
__asm__ (".int 0xe12fff1e\n"); //bx    lr
#endif //__TINYC__

#if !__TINYC__
long
__sys_call3 (long sys_call, long one, long two, long three)
{
  long r;
  asm (
       "mov    r7, %1\n\t"
       "mov    r0, %2\n\t"
       "mov    r1, %3\n\t"
       "mov    r2, %4\n\t"
       "swi    $0\n\t"
       "mov    %0, r0\n\t"
       : "=r" (r)
       : "r" (sys_call), "r" (one), "r" (two), "r" (three)
       : "r0", "r1", "r2", "r7"
       );
  return r;
}
#else //__TINYC__
long __sys_call3 (long sys_call, long one, long two, long three);
__asm__ (".global __sys_call3\n");
__asm__ ("__sys_call3:\n");
__asm__ (".int 0xe92d4890\n"); //push  {r4, r7, fp, lr}
__asm__ (".int 0xe28db00c\n"); //add   fp, sp, #12
__asm__ (".int 0xe24dd018\n"); //sub   sp, sp, #24
__asm__ (".int 0xe50b0018\n"); //str   r0, [fp, #-24]   ; 0xffffffe8
__asm__ (".int 0xe50b101c\n"); //str   r1, [fp, #-28]   ; 0xffffffe4
__asm__ (".int 0xe50b2020\n"); //str   r2, [fp, #-32]   ; 0xffffffe0
__asm__ (".int 0xe50b3024\n"); //str   r3, [fp, #-36]   ; 0xffffffdc
__asm__ (".int 0xe51b3018\n"); //ldr   r3, [fp, #-24]   ; 0xffffffe8
__asm__ (".int 0xe51bc01c\n"); //ldr   ip, [fp, #-28]   ; 0xffffffe4
__asm__ (".int 0xe51be020\n"); //ldr   lr, [fp, #-32]   ; 0xffffffe0
__asm__ (".int 0xe51b4024\n"); //ldr   r4, [fp, #-36]   ; 0xffffffdc
__asm__ (".int 0xe1a07003\n"); //mov   r7, r3
__asm__ (".int 0xe1a0000c\n"); //mov   r0, ip
__asm__ (".int 0xe1a0100e\n"); //mov   r1, lr
__asm__ (".int 0xe1a02004\n"); //mov   r2, r4
__asm__ (".int 0xef000000\n"); //svc   0x00000000
__asm__ (".int 0xe1a03000\n"); //mov   r3, r0
__asm__ (".int 0xe50b3010\n"); //str   r3, [fp, #-16]
__asm__ (".int 0xe51b3010\n"); //ldr   r3, [fp, #-16]
__asm__ (".int 0xe1a00003\n"); //mov   r0, r3
__asm__ (".int 0xe24bd00c\n"); //sub   sp, fp, #12
__asm__ (".int 0xe8bd8890\n"); //pop   {r4, r7, fp, pc}
#endif //__TINYC__

#if !__TINYC__
long
__sys_call4 (long sys_call, long one, long two, long three, long four)
{
  long r;
  asm (
       "mov    r7, %1\n\t"
       "mov    r0, %2\n\t"
       "mov    r1, %3\n\t"
       "mov    r2, %4\n\t"
       "mov    r3, %5\n\t"
       "swi    $0\n\t"
       "mov    %0, r0\n\t"
       : "=r" (r)
       : "r" (sys_call), "r" (one), "r" (two), "r" (three), "r" (four)
       : "r0", "r1", "r2", "r3", "r7"
       );
  return r;
}
#else //__TINYC__
long __sys_call4 (long sys_call, long one, long two, long three, long four);
__asm__ (".global __sys_call4\n");
__asm__ ("__sys_call4:\n");
__asm__ (".int 0xe92d48f0\n"); //push  {r4, r5, r6, r7, fp, lr}
__asm__ (".int 0xe28db014\n"); //add   fp, sp, #20
__asm__ (".int 0xe24dd018\n"); //sub   sp, sp, #24
__asm__ (".int 0xe50b0020\n"); //str   r0, [fp, #-32]   ; 0xffffffe0
__asm__ (".int 0xe50b1024\n"); //str   r1, [fp, #-36]   ; 0xffffffdc
__asm__ (".int 0xe50b2028\n"); //str   r2, [fp, #-40]   ; 0xffffffd8
__asm__ (".int 0xe50b302c\n"); //str   r3, [fp, #-44]   ; 0xffffffd4
__asm__ (".int 0xe51bc020\n"); //ldr   ip, [fp, #-32]   ; 0xffffffe0
__asm__ (".int 0xe51be024\n"); //ldr   lr, [fp, #-36]   ; 0xffffffdc
__asm__ (".int 0xe51b4028\n"); //ldr   r4, [fp, #-40]   ; 0xffffffd8
__asm__ (".int 0xe51b502c\n"); //ldr   r5, [fp, #-44]   ; 0xffffffd4
__asm__ (".int 0xe59b6004\n"); //ldr   r6, [fp, #4]
__asm__ (".int 0xe1a0700c\n"); //mov   r7, ip
__asm__ (".int 0xe1a0000e\n"); //mov   r0, lr
__asm__ (".int 0xe1a01004\n"); //mov   r1, r4
__asm__ (".int 0xe1a02005\n"); //mov   r2, r5
__asm__ (".int 0xe1a03006\n"); //mov   r3, r6
__asm__ (".int 0xef000000\n"); //svc   0x00000000
__asm__ (".int 0xe1a0c000\n"); //mov   ip, r0
__asm__ (".int 0xe50bc018\n"); //str   ip, [fp, #-24]   ; 0xffffffe8
__asm__ (".int 0xe51b3018\n"); //ldr   r3, [fp, #-24]   ; 0xffffffe8
__asm__ (".int 0xe1a00003\n"); //mov   r0, r3
__asm__ (".int 0xe24bd014\n"); //sub   sp, fp, #20
__asm__ (".int 0xe8bd88f0\n"); //pop   {r4, r5, r6, r7, fp, pc}
#endif //__TINYC__

#if 0
long
__sys_call6 (long sys_call, long one, long two, long three, long four, long five, long six)
{
  long r;
  asm (
       "mov    r7, %1\n\t"
       "mov    r0, %2\n\t"
       "mov    r1, %3\n\t"
       "mov    r2, %4\n\t"
       "mov    r3, %5\n\t"
       "mov    r4, %6\n\t"
       "mov    r5, %7\n\t"
       "swi    $0\n\t"
       "mov    %0, r0\n\t"
       : "=r" (r)
       : "r" (sys_call), "r" (one), "r" (two), "r" (three), "r" (four), "r" (five), "r" (six)
       : "r0", "r1", "r2", "r3", "r4", "r5" //, "r7" FIXME
       );
  return r;
}
#endif

// *INDENT-ON*

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

#if 0
long
_sys_call6 (long sys_call, long one, long two, long three, long four, long five, long six)
{
  long r = __sys_call6 (sys_call, one, two, three, four, five, six);
  if (r < 0)
    {
      errno = -r;
      r = -1;
    }
  else
    errno = 0;
  return r;
}
#endif
