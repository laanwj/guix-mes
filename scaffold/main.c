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

// #if __i386__
// #define main _start
// #endif

int
main (int argc, char *argv[])
{
#if 0


#if __MESC__
  asm ("mov____$i32,%ebx !42");
  asm ("mov____$i32,%eax !1");
  asm ("int____$0x80");
#elif __i386__
  asm (".byte 144");
  asm (".byte 144");
  asm (".byte 144");
#if 1 //MESC_TCC
  asm (".byte 184");  //mov $1,%eax
  asm (".byte 42");
  asm (".byte 0");
  asm (".byte 0");
  asm (".byte 0");

  asm (".byte 201");  // leave
  asm (".byte 195");  // ret

#if 0

  asm (".byte 187");  // mov $42,%ebx
  asm (".byte 42");
  asm (".byte 0");
  asm (".byte 0");
  asm (".byte 0");

  asm (".byte 184");  //mov $1,%eax
  asm (".byte 1");
  asm (".byte 0");
  asm (".byte 0");
  asm (".byte 0");

  asm (".byte 201");  // leave
  asm (".byte 195");  // ret


  asm (".byte 205");  //int $0x80
  asm (".byte 128");

  asm (".byte 137"); //%eax,-0x4(%ebp)
  asm (".byte 69");
  asm (".byte 252");

  asm (".byte 244"); //hlt

  // asm (".byte 201");
  // asm (".byte 195");
#endif
  
#else
  asm ("mov    $42,%eax");
  asm ("leave");
  asm ("ret");
  
  // asm ("mov    $42,%ebx");
  // asm ("mov    $1,%eax");
  // asm ("int    $128");
#endif

  asm (".byte 144");
  asm (".byte 144");
  asm (".byte 144");

#elif __x86_64__
  asm (".byte 144");
  asm ("mov    $42,%rbx");
  asm ("mov    $1,%rax");
  asm ("int    $128");
  asm ("hlt");
#else
  #error "platform not supported"
#endif
  // not reached

#endif
  
  return 42;
}
