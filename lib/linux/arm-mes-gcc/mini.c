/* -*-comment-start: "//";comment-end:""-*-
 * GNU Mes --- Maxwell Equations of Software
 * Copyright © 2016,2017,2019,2020 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
 * Copyright © 2019,2020 Danny Milosavljevic <dannym@scratchpost.org>
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

#include "mes/lib-mini.h"

#define SYS_exit   "0x01"
#define SYS_write  "0x04"

#if !__TINYC__
// *INDENT-OFF*
void
_exit (int code)
{
  asm (
       "mov    r7, $"SYS_exit"\n\t"
       "mov    r0, %0\n\t"
       "swi    $0\n\t"
       : // no outputs "=" (r)
       : "r" (code)
       : "r0", "r7"
       );
  // not reached
  _exit (0);
}
#else //__TINYC__
__asm__ (".global _exit\n");
__asm__ ("_exit:\n");
__asm__ (".int 0xe92d4880\n"); //push  {r7, fp, lr}
__asm__ (".int 0xe28db008\n"); //add   fp, sp, #8
__asm__ (".int 0xe24dd00c\n"); //sub   sp, sp, #12
__asm__ (".int 0xe50b0010\n"); //str   r0, [fp, #-16]
__asm__ (".int 0xe51b3010\n"); //ldr   r3, [fp, #-16]
__asm__ (".int 0xe3a07001\n"); //mov   r7, #1
__asm__ (".int 0xe1a00003\n"); //mov   r0, r3
__asm__ (".int 0xef000000\n"); //svc   0x00000000
__asm__ (".int 0xe3a00000\n"); //mov   r0, #0
__asm__ (".int 0xebfffffe\n"); //bl    0 <_exit>
__asm__ (".int 0xe320f000\n"); //nop   {0}
__asm__ (".int 0xe24bd008\n"); //sub   sp, fp, #8
__asm__ (".int 0xe8bd8880\n"); //pop   {r7, fp, pc}
#endif //__TINYC__

#if !__TINYC__
ssize_t
_write (int filedes, void const *buffer, size_t size)
{
  long r;
  asm (
       "mov    r7, $"SYS_write"\n\t"
       "mov    r0, %1\n\t"
       "mov    r1, %2\n\t"
       "mov    r3, %3\n\t"
       "swi    $0\n\t"
       "mov    %0, r0\n\t"
       : "=r" (r)
       : "r" (filedes), "r" (buffer), "r" (size)
       : "r0", "r1", "r2", "r7"
       );
  return r;
}
// *INDENT-ON*
#else //__TINYC__
__asm__ (".global _write\n");
__asm__ ("_write:\n");
__asm__ (".int 0xe92d4880\n"); //push  {r7, fp, lr}
__asm__ (".int 0xe28db008\n"); //add   fp, sp, #8
__asm__ (".int 0xe24dd01c\n"); //sub   sp, sp, #28
__asm__ (".int 0xe50b0018\n"); //str   r0, [fp, #-24]   ; 0xffffffe8
__asm__ (".int 0xe50b101c\n"); //str   r1, [fp, #-28]   ; 0xffffffe4
__asm__ (".int 0xe50b2020\n"); //str   r2, [fp, #-32]   ; 0xffffffe0
__asm__ (".int 0xe51b3018\n"); //ldr   r3, [fp, #-24]   ; 0xffffffe8
__asm__ (".int 0xe51bc01c\n"); //ldr   ip, [fp, #-28]   ; 0xffffffe4
__asm__ (".int 0xe51be020\n"); //ldr   lr, [fp, #-32]   ; 0xffffffe0
__asm__ (".int 0xe3a07004\n"); //mov   r7, #4
__asm__ (".int 0xe1a00003\n"); //mov   r0, r3
__asm__ (".int 0xe1a0100c\n"); //mov   r1, ip
__asm__ (".int 0xe1a0300e\n"); //mov   r3, lr
__asm__ (".int 0xef000000\n"); //svc   0x00000000
__asm__ (".int 0xe1a03000\n"); //mov   r3, r0
__asm__ (".int 0xe50b3010\n"); //str   r3, [fp, #-16]
__asm__ (".int 0xe51b3010\n"); //ldr   r3, [fp, #-16]
__asm__ (".int 0xe1a00003\n"); //mov   r0, r3
__asm__ (".int 0xe24bd008\n"); //sub   sp, fp, #8
__asm__ (".int 0xe8bd8880\n"); //pop   {r7, fp, pc}
#endif //__TINYC__
