/* -*-comment-start: "//";comment-end:""-*-
 * GNU Mes --- Maxwell Equations of Software
 * Copyright © 2017,2018 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
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

int
main ()
{
#if __i386__

#if __MESC__
  asm ("mov____$i32,%ebx %0");
  asm ("mov____$i32,%eax SYS_exit");
  asm ("int____$0x80");
#elif __TINYC__
  asm ("mov    $0,%ebx");
  asm ("mov    $1,%eax");
  asm ("int    $128");
#else // !__TINYC__
  asm ("mov    $0,%ebx");
  asm ("mov    $1,%eax");
  asm ("int    $0x80");
#endif // !__TINYC__

#elif __x86_64__

#if __MESC__
  asm ("mov____$i32,%rdi %0");
  asm ("mov____$i32,%rax SYS_exit");
  asm ("syscall");
#elif __TINYC__
  asm ("mov    $0,%rdi");
  asm ("mov    $0x3c,%rax");
  asm ("syscall");
#else // !__TINYC__
  asm ("mov    $0,%rdi");
  asm ("mov    $60,%rax");
  asm ("syscall");
#endif // !__TINYC__

#endif // ! __x86_64__
}
