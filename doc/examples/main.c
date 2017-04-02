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

#define GNU 0
// #if __GNUC__
// #include "mlibc.c"
// #endif

int
//main ()
main (int argc, char *argv[])
{
  //puts ("Hi Mes!\n");
  //if (argc > 1 && !strcmp (argv[1], "--help")) return puts ("argc > 1 && --help\n");
  if (argc > 1) return argc;
  return 42;
}

// #if __GNUC__
// #include "mstart.c"
// #endif
