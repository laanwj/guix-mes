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
#include <mes/lib.h>
#include <fcntl.h>

int
_open3 (char const *file_name, int flags, int mask)
{
#if defined (SYS_open)
  int r = _sys_call3 (SYS_open, (long) file_name, (int) flags, (int) mask);
#elif defined (SYS_openat)
  int r = _sys_call4 (SYS_openat, AT_FDCWD, (long) file_name, (int) flags, (int) mask);
#else
#error No usable open syscall
#endif
  __ungetc_init ();
  if (r > 2)
    {
      __ungetc_clear (r);
      __buffered_read_clear (r);
    }
  return r;
}
