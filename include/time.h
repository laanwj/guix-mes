/* -*-comment-start: "//";comment-end:""-*-
 * Mes --- Maxwell Equations of Software
 * Copyright © 2017 Jan Nieuwenhuizen <janneke@gnu.org>
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
#ifndef __MES_TIME_H
#define __MES_TIME_H 1

#if __GNUC__ && POSIX
#undef __MES_TIME_H
#include_next <time.h>
#else // ! (__GNUC__ && POSIX)
typedef int time_t;

struct tm {
  int tm_sec;    /* Seconds (0-60) */
  int tm_min;    /* Minutes (0-59) */
  int tm_hour;   /* Hours (0-23) */
  int tm_mday;   /* Day of the month (1-31) */
  int tm_mon;    /* Month (0-11) */
  int tm_year;   /* Year - 1900 */
  int tm_wday;   /* Day of the week (0-6, Sunday = 0) */
  int tm_yday;   /* Day in the year (0-365, 1 Jan = 0) */
  int tm_isdst;  /* Daylight saving time */
};

struct tm *localtime (time_t const *timep);
time_t time (time_t *tloc);

#endif // ! (__GNUC__ && POSIX)

#endif // __MES_TIME_H

