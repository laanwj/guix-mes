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

#if  __MESC__
char **g_environment;
int g_stdin = 0;
#define assert(x) ((x) ? (void)0 : assert_fail (#x))
#endif

#if !__MESC__
#include "mlibc.c"
#endif

typedef int SCM;

#if __GNUC__
int g_debug = 0;
#endif

int g_free = 0;

SCM g_symbols = 0;
SCM g_stack = 0;
SCM r0 = 0; // a/env
SCM r1 = 0; // param 1
SCM r2 = 0; // save 2+load/dump
SCM r3 = 0; // continuation

SCM
mes_environment ()
{
  return 0;
}

SCM
bload_env (SCM a) ///((internal))
{
  eputs ("bload_env\n");
  return 0;
}

int
main (int argc, char *argv[])
{
#if __GNUC__
  g_debug = (int)getenv ("MES_DEBUG");
#endif
  //if (getenv ("MES_ARENA")) ARENA_SIZE = atoi (getenv ("MES_ARENA"));

  // FIXME
  //if (argc > 1 && !strcmp (argv[1], "--help")) return eputs ("Usage: mes [--dump|--load] < FILE\n");
  //if (argc > 1 && !strcmp (argv[1], "--version")) {eputs ("Mes ");eputs (VERSION);return eputs ("\n");};

  r0 = mes_environment ();

  puts ("Hello micro-mes!\n");
  SCM program = bload_env (r0);
  int i = argc;
  return i;
}

#if !__MESC__
#include "mstart.c"
#endif
