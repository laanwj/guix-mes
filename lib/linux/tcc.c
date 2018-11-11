/* -*-comment-start: "//";comment-end:""-*-
 * GNU Mes --- Maxwell Equations of Software
 * Copyright © 2016,2017,2018 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
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

#include <time.h>

int
close (int filedes)
{
  if (_ungetc_fd == filedes)
    {
      _ungetc_pos = -1;
      _ungetc_fd = -1;
    }
  return _sys_call1 (SYS_close, (int)filedes);
}

off_t
lseek (int filedes, off_t offset, int whence)
{
  return _sys_call3 (SYS_lseek, (int)filedes, (long)offset, (int)whence);
}

int
rmdir (char const *file_name)
{
  return _sys_call1 (SYS_rmdir, (long)file_name);
}

int
stat (char const *file_name, struct stat *statbuf)
{
  return _sys_call2 (SYS_stat, (long)file_name, (long)statbuf);
}
