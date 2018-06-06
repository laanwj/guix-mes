/* -*-comment-start: "//";comment-end:""-*-
 * Mes --- Maxwell Equations of Software
 * Copyright © 2018 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
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

// gcc/xgcc wants -lg with all these
// what's the story here?

#include <sys/time.h>

#define exit __exit
#define fprintf _fprintf
#define longjmp _longjmp
#define malloc _malloc
#define printf _printf
#define putchar _putchar
#define puts _puts
#define setjmp _setjmp
#define signal _signal
#define strcmp _strcmp
#define sprintf _sprintf
#define sscanf _sscanf

#include <libc+tcc.c>
#include <linux+gnu.c>
#include <m4.c>
#include <binutils.c>
#include <gcc.c>

int
__cleanup ()
{
  eputs ("__cleanup stub\n");
  return 0;
}

int
__libc_subinit ()
{
  eputs ("__libc_subinit stub\n");
  return 0;
}

int
__syscall_error ()
{
  eputs ("__syscall_error stub\n");
  return 0;
}

int
__fpu_control ()
{
  eputs ("__fpu_control stub\n");
  return 0;
}
