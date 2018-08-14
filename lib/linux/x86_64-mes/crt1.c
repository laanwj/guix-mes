/* -*-comment-start: "//";comment-end:""-*-
 * GNU Mes --- Maxwell Equations of Software
 * Copyright © 2018 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
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

char **environ = 0;
int main (int argc, char *argv[]);

int
_start ()
{
  asm ("mov____%rbp,%rax");
  asm ("add____$i8,%rax !8");

  asm ("mov____(%rax),%rax");
  asm ("add____$i8,%rax !0x03");

  asm ("shl____$i8,%rax !0x03");
  asm ("add____%rbp,%rax");

  // 40017a:	48 a3 88 77 66 55 44 	movabs %rax,0x1122334455667788
  // 48 89 05 bd 0e 20 00 	mov    %rax,0x200ebd(%rip)        # 601000 <_GLOBAL_OFFSET_TABLE_>
  // FIXME: 64-bit addresses...DUNNO!
  // asm ("mov____%rax,0x32 &environ");

  asm ("mov____%rbp,%rax");
  asm ("add____$i8,%rax !16");
  asm ("mov____%rax,%rsi");

  asm ("mov____%rbp,%rax");
  asm ("add____$i8,%rax !8");
  asm ("mov____(%rax),%rax");
  asm ("mov____%rax,%rdi");

  main ();
  // FIXME
  //asm ("call32 &main !00 !00 !00 !00");

  asm ("mov____%rax,%rdi");
  asm ("mov____$i32,%rax %0x3c");
  asm ("syscall");
  asm ("hlt");
}
