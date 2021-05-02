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

int
link (char const *old_name, char const *new_name)
{
#if defined(SYS_link)
  return _sys_call2 (SYS_link, (long) old_name, (long) new_name);
#elif defined(SYS_linkat)
  return _sys_call4 (SYS_linkat, AT_FDCWD, (long) old_name, AT_FDCWD, (long) new_name);
#else
#error No usable link syscall
#endif
}
