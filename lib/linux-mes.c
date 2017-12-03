/* -*-comment-start: "//";comment-end:""-*-
 * Mes --- Maxwell Equations of Software
 * Copyright © 2016,2017 Jan Nieuwenhuizen <janneke@gnu.org>
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

void
read ()
{
  asm ("mov____0x8(%ebp),%ebx !8");
  asm ("mov____0x8(%ebp),%ecx !12");
  asm ("mov____0x8(%ebp),%edx !16");

  asm ("mov____$i32,%eax SYS_read");
  asm ("int____$0x80");
}

void
open ()
{
  asm ("mov____0x8(%ebp),%ebx !8");
  asm ("mov____0x8(%ebp),%ecx !12");
  asm ("mov____0x8(%ebp),%edx !16");

  asm ("mov____$i32,%eax SYS_open");
  asm ("int____$0x80");
}

void
chmod ()
{
  asm ("mov____0x8(%ebp),%ebx !8");
  asm ("mov____0x8(%ebp),%ecx !12");

  asm ("mov____$i32,%eax SYS_chmod");
  asm ("int____$0x80");
}

void
access ()
{
  asm ("mov____0x8(%ebp),%ebx !8");
  asm ("mov____0x8(%ebp),%ecx !12");

  asm ("mov____$i32,%eax SYS_access");
  asm ("int____$0x80");
}

void
brk ()
{
  asm ("mov____0x8(%ebp),%ebx !8");

  asm ("mov____$i32,%eax SYS_brk");
  asm ("int____$0x80");
}

void
fsync ()
{
  asm ("mov____0x8(%ebp),%ebx !8");

  asm ("mov____$i32,%eax SYS_fsync");
  asm ("int____$0x80");
}
