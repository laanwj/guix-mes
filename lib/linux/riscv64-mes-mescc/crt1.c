/* -*-comment-start: "//";comment-end:""-*-
 * GNU Mes --- Maxwell Equations of Software
 * Copyright © 2018 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
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

#include "mes/lib-mini.h"
int main (int argc, char *argv[], char *envp[]);

/* mesc will generate the following preamble:

   push    ra
   push    fp
*/
int
_start ()
{
  asm ("=I0 li_____%t0,0");
  asm ("=U__stdin lui____%t1,0");
  asm ("=I__stdin addi___%t1,%t1,0");
  asm ("sw_____%t0,0(%t1)");

  asm ("=I1 li_____%t0,0");
  asm ("=U__stdout lui____%t1,0");
  asm ("=I__stdout addi___%t1,%t1,0");
  asm ("sw_____%t0,0(%t1)");

  asm ("=I2 li_____%t0,0");
  asm ("=U__stderr lui____%t1,0");
  asm ("=I__stderr addi___%t1,%t1,0");
  asm ("sw_____%t0,0(%t1)");

  // environ is &argv[argc + 1]
  asm ("mv_____%t1,%fp");
  asm ("=I0x18 addi___%t1,%t1,0"); // 0x10 to skip over pushed fp+ra, 0x8 to skip over argc
  asm ("=I0x10 addi___%t5,%fp,0"); // 0x10 to skip over pushed fp+ra
  asm ("ld_____%t0,0(%t5)");
  asm ("=I1 addi___%t0,%t0,0");
  asm ("=I0x3 li_____%t5,0"); // skip over all arguments and the final NULL
  asm ("sll____%t0,%t0,%t5");
  asm ("add____%t0,%t0,%t1");
  asm ("push___%t0"); // envp
  asm ("push___%t1"); // argv
  asm ("=Uenviron lui____%t1,0");
  asm ("=Ienviron addi___%t1,%t1,0");
  asm ("sd_____%t0,0(%t1)");
  asm ("=I0x10 addi___%t5,%fp,0"); // 0x10 to skip over pushed fp+ra
  asm ("ld_____%t0,0(%t5)");
  asm ("push___%t0"); // argc

  main ();

  asm ("mv_____%a0,%t0");
  asm ("li_____%a7,SYS_exit");
  asm ("ecall");
  asm ("ebreak");
}
