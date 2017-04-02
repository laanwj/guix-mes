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

#if __GNUC__
#include "mlibc.c"
#endif
#define assert(x) ((x) ? (void)0 : assert_fail (#x))

int
main (int argc, char *argv[])
{
  g_stdin = open ("mesmes", 0);
  int c = getchar ();
  while (c != -1) {
    putchar (c);
    c = getchar ();
  }
  return c;
}

#if __GNUC__
#include "mstart.c"
#endif
