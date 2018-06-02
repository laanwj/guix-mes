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

int errno;

int
close (int fd)
{
  asm ("mov____0x8(%ebp),%ebx !8");

  asm ("mov____$i32,%eax SYS_close");
  asm ("int____$0x80");
}

int
unlink (char const *file_name)
{
  asm ("mov____0x8(%ebp),%ebx !8");

  asm ("mov____$i32,%eax SYS_unlink");
  asm ("int____$0x80");
}

int
rmdir (char const *file_name)
{
  asm ("mov____0x8(%ebp),%ebx !8");

  asm ("mov____$i32,%eax SYS_rmdir");
  asm ("int____$0x80");
}

int
stat (char const *file_name, struct stat *statbuf)
{
  asm ("mov____0x8(%ebp),%ebx !8");
  asm ("mov____0x8(%ebp),%ecx !12");

  asm ("mov____$i32,%eax SYS_getcwd");
  asm ("int____$0x80");
}

off_t
lseek (int fd, off_t offset, int whence)
{
  asm ("mov____0x8(%ebp),%ebx !8");
  asm ("mov____0x8(%ebp),%ecx !12");
  asm ("mov____0x8(%ebp),%edx !16");

  asm ("mov____$i32,%eax SYS_lseek");
  asm ("int____$0x80");
}

char *
getcwd (char *buf, size_t size)
{
  asm ("mov____0x8(%ebp),%ebx !8");
  asm ("mov____0x8(%ebp),%ecx !12");

  asm ("mov____$i32,%eax SYS_getcwd");
  asm ("int____$0x80");
}
