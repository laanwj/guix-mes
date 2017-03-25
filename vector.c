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

SCM
make_vector (SCM n)
{
  int k = VALUE (n);
  VALUE (tmp_num) = TVECTOR;
  SCM v = alloc (k);
  SCM x = make_cell_ (tmp_num, k, v);
#if 0
  //__GNUC__
  for (int i=0; i<k; i++) g_cells[v+i] = g_cells[vector_entry (cell_unspecified)];
#else
  for (int i=0; i<k; i++)
    {
      SCM y = v+i;
      SCM z = vector_entry (cell_unspecified);
      //g_cells[y] = g_cells[z];
      SCM zz = TYPE (z);
      TYPE (y) = zz;
      zz = CAR (z);
      CAR (y) = zz;
      zz = CDR (z);
      CDR (y) = zz;
    }
#endif
  return x;
}

SCM
vector_length (SCM x)
{
  assert (TYPE (x) == TVECTOR);
  return MAKE_NUMBER (LENGTH (x));
}

SCM
vector_ref (SCM x, SCM i)
{
  assert (TYPE (x) == TVECTOR);
  assert (VALUE (i) < LENGTH (x));
  SCM e = VECTOR (x) + VALUE (i);
  if (TYPE (e) == TREF) e = REF (e);
  if (TYPE (e) == TCHAR) e = MAKE_CHAR (VALUE (e));
  if (TYPE (e) == TNUMBER) e = MAKE_NUMBER (VALUE (e));
  return e;
}

SCM
vector_entry (SCM x) {
  if (TYPE (x) == TPAIR || TYPE (x) == TSPECIAL || TYPE (x) == TSTRING || TYPE (x) == TSYMBOL || TYPE (x) == TVECTOR) x = MAKE_REF (x);
  return x;
}

SCM
vector_set_x (SCM x, SCM i, SCM e)
{
  assert (TYPE (x) == TVECTOR);
  assert (VALUE (i) < LENGTH (x));
#if 0
  //__GNUC__
  g_cells[VECTOR (x)+VALUE (i)] = g_cells[vector_entry (e)];
#else
  SCM y = VECTOR (x)+VALUE (i);
  SCM z = vector_entry (e);
  //g_cells[y] = g_cells[z];
  SCM zz = TYPE (z);
  TYPE (y) = zz;
  zz = CAR (z);
  CAR (y) = zz;
  zz = CDR (z);
  CDR (y) = zz;
#endif
  return cell_unspecified;
}

SCM
list_to_vector (SCM x)
{
  VALUE (tmp_num) = VALUE (length (x));
  SCM v = make_vector (tmp_num);
  SCM p = VECTOR (v);
  while (x != cell_nil)
    {
#if 0
      //__GNUC__
      g_cells[p++] = g_cells[vector_entry (car (x))];
#else
      SCM y = p;
      SCM z = vector_entry (car (x));
      //g_cells[p++] = g_cells[y];
      SCM zz = TYPE (z);
      TYPE (y) = zz;
      zz = CAR (z);
      CAR (y) = zz;
      zz = CDR (z);
      CDR (y) = zz;
      p++;
#endif
      x = cdr (x);
    }
  return v;
}

SCM
vector_to_list (SCM v)
{
  SCM x = cell_nil;
  for (int i = 0; i < LENGTH (v); i++) {
    SCM e = VECTOR (v)+i;
    if (TYPE (e) == TREF) e = REF (e);
    x = append2 (x, cons (e, cell_nil));
  }
  return x;
}
