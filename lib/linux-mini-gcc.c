/* -*-comment-start: "//";comment-end:""-*-
 * Mes --- Maxwell Equations of Software
 * Copyright © 2016,2017 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
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

#define SYS_exit   "0x01"
#define SYS_write  "0x04"

void
exit (int code)
{
#if !__TINYC__
  asm (
       "mov    %0,%%ebx\n\t"
       "mov    $1,%%eax\n\t"
       "int    $0x80\n\t"
       : // no outputs "=" (r)
       : "" (code)
       );
#else // __TINYC__
  asm (
       "mov    %0,%%ebx\n\t"

       "mov    $"SYS_exit",%%eax\n\t"
       "int    $128\n\t"
       : // no outputs "=" (r)
       : "Ir" (code)
       );
#endif // __TINYC__
  // not reached
  exit (0);
}

int
write (int fd, char const* s, int n)
{
  int r;
#if __GNUC__
  asm (
       "mov    %1,%%ebx\n\t"
       "mov    %2,%%ecx\n\t"
       "mov    %3,%%edx\n\t"

       "mov    $0x04,%%eax\n\t"
       "int    $0x80\n\t"
       "mov    %%eax,%0\n\t"
       : "=r" (r)
       : "" (fd), "" (s), "" (n)
       : "eax", "ebx", "ecx", "edx"
       );
#elif __TINYC__
  asm (
       "mov    %1,%%ebx\n\t"
       "mov    %2,%%ecx\n\t"
       "mov    %3,%%edx\n\t"

       "mov    $"SYS_write",%%eax\n\t"
       "int    $128\n\t"
       "mov    %%eax,%0\n\t"
       : "=r" (r)
       : "Ir" (fd), "Ir" (s), "Ir" (n)
       : "eax", "ebx", "ecx"//, "edx"
       );
#endif
  return r;
}
