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
#define __MES_SYS_STAT_H 1lei

#if __GNUC__ && POSIX
#undef __MES_SYS_STAT_H
#include_next <sys/stat.h>

#else // !(__GNUC__ && POSIX)

#include <sys/types.h>

#ifndef __MES_MODE_T
#define __MES_MODE_T
typedef int mode_t;
#endif

struct stat
{
  unsigned long	st_dev;		/* Device.  */
  unsigned long	st_ino;		/* File serial number.  */
  unsigned int	st_mode;	/* File mode.  */
  unsigned int	st_nlink;	/* Link count.  */
  unsigned int	st_uid;		/* User ID of the file's owner.  */
  unsigned int	st_gid;		/* Group ID of the file's group. */
  unsigned long	st_rdev;	/* Device number, if device.  */
  unsigned long	__pad1;
  long		st_size;	/* Size of file, in bytes.  */
  int		st_blksize;	/* Optimal block size for I/O.  */
  int		__pad2;
  long		st_blocks;	/* Number 512-byte blocks allocated. */
  long		st_atime;	/* Time of last access.  */
  unsigned long	st_atime_nsec;
  long		st_mtime;	/* Time of last modification.  */
  unsigned long	st_mtime_nsec;
  long		st_ctime;	/* Time of last status change.  */
  unsigned long	st_ctime_nsec;
  unsigned int	__unused4;
  unsigned int	__unused5;
};

int chmod (char const *file_name, mode_t mode);
int mkdir (char const *file_name, mode_t mode);
int chown (char const *file_name, uid_t owner, gid_t group);
int rmdir (char const *file_name);

#define S_IFMT  0170000
#define S_IFDIR 0040000
#define S_IFREG 0100000
#define S_IFLNK 0120000

#define S_ISDIR(m) (((m) & S_IFMT) == S_IFDIR)
#define S_ISREG(m) (((m) & S_IFMT) == S_IFREG)

#define S_IRWXU 00700
#define S_IXUSR 00100
#define S_IWUSR 00200
#define S_IRUSR 00400

#endif // !(__GNUC__ && POSIX)

#endif // __MES_SYS_STAT_H
