/* -*-comment-start: "//";comment-end:""-*-
 * Mes --- Maxwell Equations of Software
 * Copyright © 2017 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
 *
 * This file is part of Mes.
 *
 * Mes is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or (at
 * your option) any later version.
 *
 * Mes is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with Mes.  If not, see <http://www.gnu.org/licenses/>.
 */
#ifndef __MES_SYS_STAT_H
#define __MES_SYS_STAT_H 1

#if __GNUC__ && POSIX
#undef __MES_SYS_STAT_H
#include_next <sys/stat.h>
#endif // (__GNUC__ && POSIX)

struct stat {
  int st_dev;
  int st_ino;
  int st_mode;
  int st_nlink;
  int st_uid;
  int st_gid;
  int st_rdev;
  int st_size;
  int st_blksize;
  int st_blocks;
  int st_atime;
  int st_mtime;
  int st_ctime;
};

#endif // __MES_SYS_STAT_H

