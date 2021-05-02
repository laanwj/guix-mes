/* -*-comment-start: "//";comment-end:""-*-
 * GNU Mes --- Maxwell Equations of Software
 * Copyright © 2016,2017,2018,2019,2021 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
 * Copyright © 2021 Danny Milosavljevic <dannym@scratchpost.org>
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

#include <mes/lib.h>
#include <string.h>
#include <stddef.h>
#include <stdint.h>

char *__brk = 0;

void *
malloc (size_t size)
{
  if (!__brk)
    __brk = (char *) brk (0);
  /* align what we give back. */
  __brk = (char*) (((uintptr_t) __brk
                    + sizeof (max_align_t) - 1) & -sizeof (max_align_t));
  if (brk (__brk + size) == -1)
    return 0;
  char *p = __brk;
  __brk += size;
  return p;
}
