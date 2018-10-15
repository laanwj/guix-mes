/* -*-comment-start: "//";comment-end:""-*-
 * GNU Mes --- Maxwell Equations of Software
 * Copyright © 2018 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
 *
 * This file is part of GNU Mes.
 *
 * GNU Mes is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or (at
 * your option) any later version.
 *
 * GNU Mes is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with GNU Mes.  If not, see <http://www.gnu.org/licenses/>.
 */

SCM make_vector__ (long k);
SCM vector_ref_ (SCM x, long i);
SCM vector_set_x_ (SCM x, long i, SCM e);

int
char_hash (int c)
{
  if (c >= 'a' && c <= 'z')
    return c - 'a';
  return 27;
}

int
hashq_ (SCM x, long size)
{
  int hash = char_hash (VALUE (CAR (STRING (x)))) * 27;
  if (TYPE (CDR (STRING (x))) == TPAIR)
    hash = hash + char_hash (VALUE (CADR (STRING (x))));
  else
    hash = hash + char_hash (0);
  assert (hash <= 756);
  return hash;
}

int
hashq (SCM x, SCM size)
{
  return hashq_ (x, VALUE (size));
}

SCM
hashq_ref (SCM table, SCM key, SCM dflt)
{
  unsigned hash = hashq_ (key, 0);
  SCM bucket = vector_ref_ (table, hash);
  SCM x = cell_f;
  if (TYPE (dflt) == TPAIR)
    x = CAR (dflt);
  if (TYPE (bucket) == TPAIR)
    x = assq (key, bucket);
  return x;
}

SCM
hashq_set_x (SCM table, SCM key, SCM value)
{
  unsigned hash = hashq_ (key, 0);
  SCM bucket = vector_ref_ (table, hash);
  if (TYPE (bucket) != TPAIR)
    bucket = cell_nil;
  bucket = acons (key, value, bucket);
  vector_set_x_ (table, hash, bucket);
  return value;
}

SCM
make_hash_table_ (long size)
{
  if (!size)
    size = 30 * 27;
  return make_vector__ (size);
}

SCM
make_hash_table (SCM x)
{
  long size = 0;
  if (TYPE (x) == TPAIR)
    {
      assert (TYPE (x) == TNUMBER);
      size = VALUE (x);
    }
  return make_hash_table_ (size);
}
