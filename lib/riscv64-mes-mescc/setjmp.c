/* -*-comment-start: "//";comment-end:""-*-
 * GNU Mes --- Maxwell Equations of Software
 * Copyright © 2017,2018,2019 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
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

#include <setjmp.h>
#include <stdlib.h>

void
longjmp (jmp_buf env, int val)
{
  val = val == 0 ? 1 : val;
  asm ("ld_____%fp,0x10(%fp)"); // env*

  asm ("ld_____%t0,0x8(%fp)");  // env.__pc
  asm ("ld_____%sp,0x10(%fp)"); // env.__sp
  asm ("ld_____%fp,0x0(%fp)");  // env.__bp
  asm ("jr_____%t0");
  // not reached
  exit (42);
}

int
setjmp (__jmp_buf * env)
{
  long *p = (long *) &env;
  env[0].__bp = p[-2];
  env[0].__pc = p[-1];
  env[0].__sp = (long) &env;
  return 0;
}
