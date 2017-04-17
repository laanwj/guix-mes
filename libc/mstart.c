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
void
_start ()
{
  //  char **;
  asm (
       "mov     %%ebp,%%eax\n\t"
       "addl    $4,%%eax\n\t"
       "movzbl  (%%eax),%%eax\n\t"
       "addl    $3,%%eax\n\t"
       "shl     $2,%%eax\n\t"
       "add     %%ebp,%%eax\n\t"
       "movl    %%eax,%0\n\t"
       : "=g_environment" (g_environment)
       : //no inputs ""
       );
  int r;
  asm (
       "mov     %%ebp,%%eax\n\t"
       "addl    $8,%%eax\n\t"
       "push    %%eax\n\t"

       "mov     %%ebp,%%eax\n\t"
       "addl    $4,%%eax\n\t"
       "movzbl  (%%eax),%%eax\n\t"
       "push     %%eax\n\t"

       "call    main\n\t"
       "movl    %%eax,%0\n\t"
       : "=r" (r)
       : //no inputs ""
       );
  exit (r);
}
#endif
