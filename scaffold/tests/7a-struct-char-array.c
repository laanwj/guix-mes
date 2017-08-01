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

struct file {
  unsigned char buffer[1];
};

int fill0;
int fill1;
int fill2;
int fill3;
int fill4;

struct file file;

int fill5;
int fill6;
int fill7;
int fill8;
int fill9;

int
test ()
{
  struct file *pfile = &file;
  strcpy (file.buffer, "0123456789\n");
  eputs (file.buffer);
  char *p = pfile->buffer;
  if (p[4] != '4') return 1;
  if (file.buffer[4] != '4') return 2;
  if (pfile->buffer[4] != '4') return 3;

  memcpy (pfile->buffer + 4, " ", 1);
  eputs (file.buffer);
  if (p[4] != ' ') return 4;
  if (file.buffer[4] != ' ') return 5;
  if (pfile->buffer[4] != ' ') return 6;

  strcpy (file.buffer, "0123456789\n");
  eputs (file.buffer);
  p[4] = 'A';
  eputs (file.buffer);
  if (p[4] != 'A') return 7;
  if (file.buffer[4] != 'A') return 8;
  if (pfile->buffer[4] != 'A') return 9;

  return 0;
}
