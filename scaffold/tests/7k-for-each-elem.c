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

struct section {
  unsigned char *data;
  int offset;
};

struct sym {
  char* name;
  int index;
};


struct sym tab[3] = {"foo", 0, "bar", 1, "baz", 2};

struct section section;

#define for_each_elem(sec, startoff, elem, type) \
    for (elem = (type *) sec->data + startoff; \
         elem < (type *) (sec->data + sec->offset); elem++)
#define for_each_elem2(sec, startoff, elem, type) \
  elem = sec->data + sizeof (type) * startoff; \
  for (;elem < ((type *) (sec->data + sec->offset)); elem++)

int
test ()
{
  section.data = tab;
  section.offset = 24;

  struct sym* p;
  int size = sizeof (struct sym);
  eputs ("size="); eputs (itoa (size)); eputs ("\n");
  if (size != 8) return 1;
  struct section* psection = &section;
  p = (struct sym*)psection->data + 1;
  struct sym* q = tab;
  int i = (int)p;
  i -= (int)q;
  eputs ("diff="); eputs (itoa (i)); eputs ("\n");
  if (i != 8) return 2;

  for_each_elem(psection, 1, p, struct section) {
    eputs ("i="); eputs (itoa (p->index));
    eputs (" name="); eputs (p->name); eputs ("\n");
  }

  for_each_elem2(psection, 1, p, struct section) {
    eputs ("i="); eputs (itoa (p->index));
    eputs (" name="); eputs (p->name); eputs ("\n");
  }

  return 0;
}
