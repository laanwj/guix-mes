/* -*-comment-start: "//";comment-end:""-*-
 * GNU Mes --- Maxwell Equations of Software
 * Copyright © 2016,2017,2019,2020 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
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

#define SYS_write   64

// *INDENT-OFF*
ssize_t
_write (int filedes, void const *buffer, size_t size)
{
  register long __a7 asm ("a7") = (long) SYS_write;
  register long __a0 asm ("a0") = (long) filedes;
  register long __a1 asm ("a1") = (long) buffer;
  register long __a2 asm ("a2") = (long) size;
  asm volatile (
       "ecall\n\t"
       : "+r" (__a0)
       : "r" (__a7), "r" (__a1), "r" (__a2)
       );
  return (ssize_t)__a0;
}
