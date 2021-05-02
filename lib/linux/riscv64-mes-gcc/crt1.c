/* -*-comment-start: "//";comment-end:""-*-
 * GNU Mes --- Maxwell Equations of Software
 * Copyright © 2017,2018,2019,2020 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
 * Copyright © 2019,2020 Danny Milosavljevic <dannym@scratchpost.org>
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

#include <mes/lib-mini.h>
//int main (int argc, char *argv[], char *envp[]);

/* gcc will generate the following preamble:

   addi    sp,sp,-16
   sd      s0,8(sp)
   addi    s0,sp,16
*/

// *INDENT-OFF*
void
_start ()
{
  asm (
       ".option push\n\t"
       ".option norelax\n\t"
       "lla   gp, __global_pointer$\n\t"
       ".option pop\n\t"
       "andi  sp, sp, ~15\n\t" // make sure sp conforms to ABI alignment
      );

  asm (
       "li    t0, 0\n\t"
       "mv    %0, t0\n"
       : "=r" (__stdin)
       : //no inputs ""
       );

  asm (
       "li    t0, 1\n\t"
       "mv    %0, t0\n"
       : "=r" (__stdout)
       : //no inputs ""
       );

  asm (
       "li    t0, 2\n\t"
       "mv    %0, t0\n"
       : "=r" (__stderr)
       : //no inputs ""
       );

  // environ is &argv[argc + 1]
  asm (
       "lw    t0, 0(s0)\n\t"
       "addi  t1, s0, 8\n\t"
       "addi  t0, t0, 1\n\t"
       "slli  t0, t0, 3\n\t"
       "add   %0, t1, t0\n\t"
       : "=r" (environ)
       : //no inputs ""
       );

  asm (
       "lw    a0, 0(s0)\n\t"  // a0 argc
       "addi  a1, s0, 8\n\t"  // a1 argv
       "mv    a2, %0\n\t"     // a2 envp
       "jal   main\n\t"

       "li    a7, 93\n\t"     // SYS_exit
       "ecall\n\t"            // exit(return value from main)

       "ebreak\n\t"
       : //no outputs ""
       : "r" (environ)
       );
}
