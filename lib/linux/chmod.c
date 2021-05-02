/* -*-comment-start: "//";comment-end:""-*-
 * GNU Mes --- Maxwell Equations of Software
 * Copyright © 2016,2017,2018,2019 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
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
chmod (char const *file_name, mode_t mask)
{
#if defined (SYS_chmod)
  return _sys_call2 (SYS_chmod, (long) file_name, (long) mask);
#elif defined (SYS_fchmodat)
  return _sys_call3 (SYS_fchmodat, AT_FDCWD, (long) file_name, (long) mask);
#else
#error No usable chmod syscall
#endif
}
