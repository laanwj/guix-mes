/* -*-comment-start: "//";comment-end:""-*-
 * GNU Mes --- Maxwell Equations of Software
 * Copyright © 2017,2018,2019 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
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

#include <setjmp.h>
#include <stdlib.h>

void
__attribute__ ((noinline))
longjmp (jmp_buf env, int val)
{
  // *INDENT-OFF*
  asm (
       "mov r0, %0\n\t"
       "mov r1, %1\n\t"
       "cmp r1, #0\n\t"
       "moveq r1, #1\n\t" /* returning 0 is not allowed, even when the user wanted to. */
       "ldr r13, [r0], #4\n\t" /* stack pointer (sp) */
       "ldr r14, [r0], #4\n\t" /* link register (lr) */
       "ldmia r0!, {r4, r5, r6, r7, r8, r9, r10, r11}\n\t"
       // TODO: If using VFP, vldmia r0!, {d8-d15}
       "mov r0, r1\n\t"
       :
       : "r" (env), "r" (val));
  // *INDENT-ON*
  // not reached
}

int
__attribute__ ((noinline))
setjmp (jmp_buf env)
{
  // *INDENT-OFF*
  asm (
       "mov r0, %0\n\t"
       "str r13, [r0], #4\n\t" /* stack pointer (sp) */
       "str r14, [r0], #4\n\t" /* link register (lr) */
       "stmia r0!, {r4, r5, r6, r7, r8, r9, r10, r11}\n\t"
       // TODO: If using VFP, vstmia r0!, {d8-d15}
       :
       : "r" (env)
       : "r0");
  // *INDENT-ON*
  return 0;
}
