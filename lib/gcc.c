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

#include <libmes.h>
#include <stdint.h>
#include <time.h>
#include <sys/time.h>

FILE *
freopen (char const *file_name, char const *opentype, FILE *stream)
{
  fclose (stream);
  return fopen (file_name, opentype);
}

clock_t
times (struct tms *buffer)
{
  eputs ("times stub\n");
  return 0;
}

unsigned int
sleep (unsigned int seconds)
{
  struct timespec requested_time;
  struct timespec remaining;
  requested_time.tv_sec = seconds;
  requested_time.tv_nsec = 0;
  return nanosleep (&requested_time, &remaining);
}
