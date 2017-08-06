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

#include "30-test.i"
#include <string.h>

struct string {
  char *str;
  int len;
};

struct string g_t;

int
test ()
{
  struct string s = {"hallo"};
  s.len = strlen (s.str);
  struct string t;
  t = s;

  if (t.len != s.len) return 1;
  if (strcmp (t.str, s.str)) return 2;

  g_t = s;
  if (g_t.len != s.len) return 3;
  if (strcmp (g_t.str, s.str)) return 4;

  return 0;
}
