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

#if POSIX
#error "POSIX not supported"
#endif

#include <mlibc.h>

int
main (int argc, char *argv[])
{
  int size = 5000;
  puts ("m!\n");
  //int *p = 0;
  char *p = 0;
  p = malloc (size);
  size = 5000;
  puts ("p=");
  puts (itoa (p));
  puts ("\n");
  for (int i = 0; i < size; i++)
    {
      puts ("set ");
      puts (itoa (i));
      puts ("\n");
      p[i] = i;
    }
  for (int i = 0; i < size; i++)
    {
      puts (itoa (i));
      puts (": ");
      puts (itoa (p[i]));
      puts ("\n");
    }
  return 0;
}
