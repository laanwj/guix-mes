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
#include <fcntl.h>
#include <sys/stat.h>

int
mkdir (char const *file_name, mode_t mode)
{
#if defined (SYS_mkdir)
  return _sys_call2 (SYS_mkdir, (long) file_name, (long) mode);
#elif defined (SYS_mkdirat)
  return _sys_call3 (SYS_mkdirat, AT_FDCWD, (long) file_name, (long) mode);
#else
#error No usable mkdir syscall
#endif
}
