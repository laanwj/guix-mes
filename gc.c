/* -*-comment-start: "//";comment-end:""-*-
 * Mes --- Maxwell Equations of Software
 * Copyright © 2016 Jan Nieuwenhuizen <janneke@gnu.org>
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

SCM
gc_up_arena () ///((internal))
{
  ARENA_SIZE *= 2;
  void *p = realloc (g_cells-1, 2*ARENA_SIZE*sizeof(struct scm));
  if (!p) error (cell_symbol_system_error, cons (MAKE_STRING (cstring_to_list (strerror (errno))), MAKE_NUMBER (g_free)));
  g_cells = (struct scm*)p;
  g_cells++;
  gc_init_news ();
}

SCM
gc_flip () ///((internal))
{
  struct scm *cells = g_cells;
  g_cells = g_news;
  g_news = cells;
  if (g_debug) fprintf (stderr, " => jam[%d]\n", g_free);
  return g_stack;
}

SCM
gc_copy (SCM old) ///((internal))
{
  if (TYPE (old) == TBROKEN_HEART) return g_cells[old].car;
  SCM new = g_free++;
  g_news[new] = g_cells[old];
  if (NTYPE (new) == TVECTOR)
    {
      g_news[new].vector = g_free;
      for (int i=0; i<LENGTH (old); i++)
        g_news[g_free++] = g_cells[VECTOR (old)+i];
    }
  g_cells[old].type = TBROKEN_HEART;
  g_cells[old].car = new;
  return new;
}

SCM
gc_relocate_car (SCM new, SCM car) ///((internal))
{
  g_news[new].car = car;
  return cell_unspecified;
}

SCM
gc_relocate_cdr (SCM new, SCM cdr) ///((internal))
{
  g_news[new].cdr = cdr;
  return cell_unspecified;
}

SCM
gc_loop (SCM scan) ///((internal))
{
  while (scan < g_free)
    {
      if (NTYPE (scan) == TCLOSURE
          || NTYPE (scan) == TCONTINUATION
          || NTYPE (scan) == TFUNCTION
          || NTYPE (scan) == TKEYWORD
          || NTYPE (scan) == TMACRO
          || NTYPE (scan) == TPAIR
          || NTYPE (scan) == TREF
          || scan == 1 // null
          || NTYPE (scan) == TSPECIAL
          || NTYPE (scan) == TSTRING
          || NTYPE (scan) == TSYMBOL)
        {
          SCM car = gc_copy (g_news[scan].car);
          gc_relocate_car (scan, car);
        }
      if ((NTYPE (scan) == TCLOSURE
           || NTYPE (scan) == TCONTINUATION
           || NTYPE (scan) == TMACRO
           || NTYPE (scan) == TPAIR
           || NTYPE (scan) == TVALUES)
          && g_news[scan].cdr) // allow for 0 terminated list of symbols
        {
          SCM cdr = gc_copy (g_news[scan].cdr);
          gc_relocate_cdr (scan, cdr);
        }
      scan++;
    }
  return gc_flip ();
}

SCM
gc ()
{
  if (g_debug) fprintf (stderr, "***gc[%d]...", g_free);
  g_free = 1;
  if (g_cells < g_news && ARENA_SIZE < MAX_ARENA_SIZE) gc_up_arena ();
  for (int i=g_free; i<g_symbol_max; i++)
    gc_copy (i);
  make_tmps (g_news);
  g_symbols = gc_copy (g_symbols);
  SCM new = gc_copy (g_stack);
  if (g_debug) fprintf (stderr, "new=%d\n", new, g_stack);
  g_stack = new;
  return gc_loop (1);
}
