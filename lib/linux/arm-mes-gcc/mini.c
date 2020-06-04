/* -*-comment-start: "//";comment-end:""-*-
 * GNU Mes --- Maxwell Equations of Software
 * Copyright © 2016,2017,2019 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
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
