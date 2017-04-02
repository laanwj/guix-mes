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
#define assert(x) ((x) ? (void)0 : assert_fail(#x))


#define MES_MINI 1

#if __GNUC__
#define  __NYACC__ 0
#define NYACC
#define NYACC2
#else
#define  __NYACC__ 1
#define NYACC nyacc
#define NYACC2 nyacc2
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

  if (argc > 1 && !strcmp (argv[1], "--help")) return eputs ("Usage: mes [--dump|--load] < FILE\n");
  if (argc > 1 && !strcmp (argv[1], "--version")) {eputs ("Mes ");eputs (VERSION);return eputs ("\n");};

#if __GNUC__
  g_stdin = STDIN;
  r0 = mes_environment ();
#endif

#if MES_MINI
  puts ("Hello micro-mes!\n");
  SCM program = bload_env (r0);
#else
  SCM program = (argc > 1 && !strcmp (argv[1], "--load"))
    ? bload_env (r0) : load_env (r0);
  if (argc > 1 && !strcmp (argv[1], "--dump")) return dump ();

  push_cc (r2, cell_unspecified, r0, cell_unspecified);
  r3 = cell_vm_begin;
  r1 = eval_apply ();
  stderr_ (r1);

  eputs ("\n");
  gc (g_stack);
#endif
  int i = argc;
  //int i = strcmp (argv[1], "1");
  return i;
#if __GNUC__
  if (g_debug)
    {
      eputs ("\nstats: [");
      eputs (itoa (g_free));
      eputs ("]\n");
    }
#endif
  return 0;
}

#if __GNUC__
#include "mstart.c"
#endif
