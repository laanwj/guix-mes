/* -*-comment-start: "//";comment-end:""-*-
 * Mes --- Maxwell Equations of Software
 * Copyright (C) 1989, 1990, 1991, 1992 Free Software Foundation, Inc.
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
#ifndef __MES_GETOPT_H
#define __MES_GETOPT_H 1

#if __GNUC__ && POSIX
#ifndef _GNU_SOURCE
#define _GNU_SOURCE
#endif
#undef __MES_GETOPT_H
#include_next <getopt.h>

#else // ! (__GNUC__ && POSIX)
#include <endian.h>
int isdigit (int);
int isxdigit (int);
#endif // ! (__GNUC__ && POSIX)

char *optarg;
int optind;
int opterr;
struct option
{
  char const *name;
  int has_arg;
  int *flag;
  int val;
};

enum _argtype
{
  no_argument,
  required_argument,
  optional_argument
};

int getopt (int argc, char *const *argv, char const *shortopts);
int getopt_long (int argc, char *const *argv, char const *shortopts,
                 struct option const *longopts, int *longind);

#endif // __MES_GETOPT_H

