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
#define setjmp _setjmp
#define signal _signal
#define strcmp _strcmp
#define sprintf _sprintf
#define sscanf _sscanf

#include <libc+tcc.c>
#include <linux+gnu-gcc.c>
#include <m4.c>
#include <binutils.c>
#include <gcc.c>

int
__cleanup ()
{
  eputs ("cleanup stub\n");
  return 0;
}

int
_dprop ()
{
  eputs ("dprop stub\n");
}

int
_edprop ()
{
  eputs ("edprop stub\n");
}

int
_eldprop ()
{
  eputs ("eldprop stub\n");
}

int
_iprop ()
{
  eputs ("iprop stub\n");
}

int
_lprop ()
{
  eputs ("lprop stub\n");
}

int
_ldprop ()
{
  eputs ("ldprop stub\n");
}

int
_uiprop ()
{
  eputs ("uiprop stub\n");
}
int
_ulprop ()
{
  eputs ("ulprop stub\n");
}
