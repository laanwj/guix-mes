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
#include <sys/stat.h>
#include <fcntl.h>

int
mknod (char const *file_name, mode_t mode, dev_t dev)
{
#if defined (SYS_mknod)
  return _sys_call3 (SYS_mknod, (long) file_name, (long) mode, (long) dev);
#elif defined (SYS_mknodat)
  return _sys_call4 (SYS_mknodat, AT_FDCWD, (long) file_name, (long) mode, (long) dev);
#else
#error No usable mknod syscall
#endif
}
