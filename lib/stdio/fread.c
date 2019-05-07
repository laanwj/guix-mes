/* -*-comment-start: "//";comment-end:""-*-
 * GNU Mes --- Maxwell Equations of Software
 * Copyright © 2017,2018 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
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

#include <mes/lib.h>
#include <stdio.h>

int
_fungetc_p (FILE *stream)
{
  return _fdungetc_p ((int)stream);
}

size_t
fread (void *data, size_t size, size_t count, FILE *stream)
{
  if (! size || !count)
    return 0;

  size_t todo = size * count;
  char *buf = (char*)data;

  int bytes = 0;
  while (_fungetc_p (stream) && todo-- && ++bytes)
    *buf++ = fgetc (stream);
  if (todo)
    {
      int r = read ((int)stream, buf, todo);
      if (r < 0 && !bytes)
        bytes = r;
      else
        bytes += r;
    }

  if (__mes_debug ())
    {
      eputs ("fread fd="); eputs (itoa ((int)stream));
      eputs (" bytes="); eputs (itoa (bytes)); eputs ("\n");
      static char buf[4096];
      if (bytes > 0 && bytes < sizeof (buf))
        {
          strncpy (buf, data, bytes);
          buf[bytes] = 0;
          eputs ("fread buf="); eputs (buf); eputs ("\n");
        }
    }

  if (bytes > 0)
    return bytes/size;

  return 0;
}
