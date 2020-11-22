/* -*-comment-start: "//";comment-end:""-*-
 * GNU Mes --- Maxwell Equations of Software
 * Copyright © 2017,2018,2019,2020 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
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

#include <mes/lib-mini.h>
//int main (int argc, char *argv[], char *envp[]);

#if !__TINYC__
/* Note: GCC automatically emits a preable in order to set up the
frame pointer: "push {fp}" "add fp, sp, 0"
 */
// *INDENT-OFF*
void
_start ()
{
  asm (
       "mov    r0,#0\n\t"
       "mov    %0,r0\n"
       : "=r" (__stdin)
       : //no inputs ""
       );

  asm (
       "mov    r0,#1\n\t"
       "mov    %0,r0\n"
       : "=r" (__stdout)
       : //no inputs ""
       );

  asm (
       "mov    r0,#2\n\t"
       "mov    %0,r0\n"
       : "=r" (__stderr)
       : //no inputs ""
       );

  /* environ = argv + argc + 1 */
  asm (
       "ldr     r0,[fp,#4]\n\t" /* r0 = argc */
       "add     r1,fp,#8\n\t" /* r1 = &argv[0] */
       "add     r2,r0,#1\n\t" /* r2 = r0 + 1 */
       "lsl     r2,#2\n\t" /* r2 = (r0 + 1) << 2 */
       "add     r2,r2,r1\n\t" /* r2 = ((r0 + 1) << 2) + r1 */
       "push    {r2}\n\t" /* envp */
       "push    {r1}\n\t" /* argv */
       "push    {r0}\n\t" /* argc */
       "mov     %0,r2\n\t"
       : "=r" (environ)
       : //no inputs ""
       );
  asm (
       "ldr     r0,[sp]\n\t" /* argc */
       "ldr     r1,[sp, #4]\n\t" /* argv */
       "ldr     r2,[sp, #8]\n\t" /* envp */
       "bl      main\n\t"
       "mov     r7, #1\n\t"
       "swi     #0\n\t"
       "wfi     \n\t"
       );
}
#else //__TINYC__
__asm__ (".global _start\n");
__asm__ ("_start:\n");
__asm__ (".int 0xe52db004\n"); //push  {fp}      ; (str fp, [sp, #-4]!)
__asm__ (".int 0xe28db000\n"); //add   fp, sp, #0
__asm__ (".int 0xe3a00000\n"); //mov   r0, #0
__asm__ (".int 0xe1a02000\n"); //mov   r2, r0
__asm__ (".int 0xe3003000\n"); //movw  r3, #0
__asm__ (".int 0xe3403000\n"); //movt  r3, #0
__asm__ (".int 0xe5832000\n"); //str   r2, [r3]
__asm__ (".int 0xe3a00001\n"); //mov   r0, #1
__asm__ (".int 0xe1a02000\n"); //mov   r2, r0
__asm__ (".int 0xe3003000\n"); //movw  r3, #0
__asm__ (".int 0xe3403000\n"); //movt  r3, #0
__asm__ (".int 0xe5832000\n"); //str   r2, [r3]
__asm__ (".int 0xe3a00002\n"); //mov   r0, #2
__asm__ (".int 0xe1a02000\n"); //mov   r2, r0
__asm__ (".int 0xe3003000\n"); //movw  r3, #0
__asm__ (".int 0xe3403000\n"); //movt  r3, #0
__asm__ (".int 0xe5832000\n"); //str   r2, [r3]
__asm__ (".int 0xe59b0004\n"); //ldr   r0, [fp, #4]
__asm__ (".int 0xe28b1008\n"); //add   r1, fp, #8
__asm__ (".int 0xe2802001\n"); //add   r2, r0, #1
__asm__ (".int 0xe1a02102\n"); //lsl   r2, r2, #2
__asm__ (".int 0xe0822001\n"); //add   r2, r2, r1
__asm__ (".int 0xe52d2004\n"); //push  {r2}      ; (str r2, [sp, #-4]!)
__asm__ (".int 0xe52d1004\n"); //push  {r1}      ; (str r1, [sp, #-4]!)
__asm__ (".int 0xe52d0004\n"); //push  {r0}      ; (str r0, [sp, #-4]!)
__asm__ (".int 0xe1a02002\n"); //mov   r2, r2
__asm__ (".int 0xe3003000\n"); //movw  r3, #0
__asm__ (".int 0xe3403000\n"); //movt  r3, #0
__asm__ (".int 0xe5832000\n"); //str   r2, [r3]
__asm__ (".int 0xe59d0000\n"); //ldr   r0, [sp]
__asm__ (".int 0xe59d1004\n"); //ldr   r1, [sp, #4]
__asm__ (".int 0xe59d2008\n"); //ldr   r2, [sp, #8]
__asm__ (".int 0xebfffffe\n"); //bl    0 <main>
__asm__ (".int 0xe3a07001\n"); //mov   r7, #1
__asm__ (".int 0xef000000\n"); //svc   0x00000000
__asm__ (".int 0xe320f003\n"); //wfi
__asm__ (".int 0xe320f000\n"); //nop   {0}
__asm__ (".int 0xe28bd000\n"); //add   sp, fp, #0
__asm__ (".int 0xe49db004\n"); //pop   {fp}      ; (ldr fp, [sp], #4)
__asm__ (".int 0xe12fff1e\n"); //bx    lr
#endif //__TINYC__
