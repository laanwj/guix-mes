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

#define CACHE_SIZE 30
#define ENV_HEAD 15

#if! ENV_CACHE
SCM cache_invalidate (SCM x){}
SCM cache_invalidate_range (SCM p,SCM a){}
SCM cache_save (SCM p){}
SCM cache_lookup (SCM x){}
#else // ENV_CACHE

SCM env_cache_cars[CACHE_SIZE];
SCM env_cache_cdrs[CACHE_SIZE];
int cache_threshold = 0;
SCM
cache_save (SCM p)
{
  int n = g_cells[car (p)].hits;
  if (n < cache_threshold) return cell_unspecified;
  int j = -1;
  for (int i=0; i < CACHE_SIZE; i++) {
    if (!env_cache_cars[i]) {
      j = i;
      break;
    }
    if (env_cache_cars[i] == car (p)) return cell_unspecified;
    if (n > g_cells[env_cache_cars[i]].hits) {
      n = g_cells[env_cache_cars[i]].hits;
      j = i;
    }
  }
  if (j >= 0) {
    cache_threshold = g_cells[car (p)].hits;
    env_cache_cars[j] = car (p);
    env_cache_cdrs[j] = cdr (p);
  }
  return cell_unspecified;
}

SCM
cache_lookup (SCM x)
{
  for (int i=0; i < CACHE_SIZE; i++) {
    if (!env_cache_cars[i]) break;
    if (env_cache_cars[i] == x) return env_cache_cdrs[i];
  }
  return cell_undefined;
}

SCM
cache_invalidate (SCM x)
{
  for (int i=0; i < CACHE_SIZE; i++) {
    if (env_cache_cars[i] == x) {
      env_cache_cars[i] = 0;
      break;
    }
  }
  return cell_unspecified;
}

SCM
cache_invalidate_range (SCM p, SCM a)
{
  do {
    cache_invalidate (caar (p));
    p = cdr (p);
  } while (p != a);
  return cell_unspecified;
}

SCM
assq_ref_cache (SCM x, SCM a) ///((internal))
{
  g_cells[x].hits++;
  SCM c = cache_lookup (x);
  if (c != cell_undefined) return c;
  int i = 0;
  while (a != cell_nil && x != CAAR (a)) {i++;a = cdr (a);}
  if (a == cell_nil) return cell_undefined;
  if (i>ENV_HEAD) cache_save (car (a));
  return cdar (a);
}
#endif // ENV_CACHE
