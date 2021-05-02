/* -*-comment-start: "//";comment-end:""-*-
 * GNU Mes --- Maxwell Equations of Software
 * Copyright © 2018,2019 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
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

#include <linux/syscall.h>
#include <syscall.h>
#include <sys/types.h>

int
getdents (int filedes, char *buffer, size_t nbytes)
{
#if defined (SYS_getdents)
  return _sys_call3 (SYS_getdents, (int) filedes, (long) buffer, (long) nbytes);
#elif defined (SYS_getdents64)
  return _sys_call3 (SYS_getdents64, (int) filedes, (long) buffer, (long) nbytes);
#else
#error No usable getdents syscall
#endif
}
