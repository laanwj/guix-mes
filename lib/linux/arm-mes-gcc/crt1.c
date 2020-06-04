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

#include <mes/lib-mini.h>
//int main (int argc, char *argv[], char *envp[]);

/* Note: GCC automatically emits a preable in order to set up the frame pointer:
"push {fp}"
"add fp, sp, 0"
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
       "mov	r7, #1\n\t"
       "swi     #0\n\t"
       "wfi     \n\t"
       );
}
