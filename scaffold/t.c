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
exit (int code)
{
  asm (
       "movl %0,%%ebx\n\t"
       "movl $1,%%eax\n\t"
       "int  $0x80"
       : // no outputs "=" (r)
       : "" (code)
       );
  // not reached
  exit (0);
}

void
write (int fd, char const* s, int n)
{
  int r;
  //syscall (SYS_write, fd, s, n));
  asm (
       "mov %0,%%ebx\n\t"
       "mov %1,%%ecx\n\t"
       "mov %2,%%edx\n\t"

       "mov $0x4,%%eax\n\t"
       "int $0x80\n\t"
       : // no outputs "=" (r)
       : "" (fd), "" (s), "" (n)
       : "eax", "ebx", "ecx", "edx"
       );
}

#define STDOUT 1

typedef long size_t;
size_t
strlen (char const* s)
{
  int i = 0;
  while (s[i]) i++;
  return i;
}

int
puts (char const* s)
{
  //write (STDOUT, s, strlen (s));
  //int i = write (STDOUT, s, strlen (s));
  int i = strlen (s);
  write (1, s, i);
  return 0;
}

int
strcmp (char const* a, char const* b)
{
  while (*a && *b && *a == *b) {a++;b++;}
  return *a - *b;
}
int test ();
#endif

int
main (int argc, char *argv[])
{
  puts ("t.c\n");
  return test ();
}

int
test ()
{
  int f = 0;
  int t = 1;
  int one = 1;

  puts ("t: if (0)\n");
  if (0) return 1;

  puts ("t: if (f)\n");
  if (f) return 1;

  puts ("t: if (one > 1)\n");
  if (one > 1) return 1;

  puts ("t: if (one < 0)\n");
  if (one < 0) return 1;

  puts ("t: stlrlen (\"\")\n");
  if (strlen ("")) return 1;

  puts ("t: if (!1)\n");
  if (!1) return 1;

  puts ("t: if (one == 0)\n");
  if (one == 0) return 1;

  puts ("t: if (f != 0)\n");
  if (one != 1) return 1;

  puts ("t: if (1 && 0)\n");
  if (1 && 0) return 1;

  int i=0;
  puts ("t: if (i++)\n");
  if (i++) return 1;

  puts ("t: if (--i)\n");
  if (--i) return 1;

  puts ("t: if (1)\n");
  if (1) goto ok0;
  return 1;
 ok0:
  
  puts ("t: if (t)\n");
  if (t) goto ok1;
  return 1;
 ok1:

  puts ("t: if (one > 0)\n");
  if (one > 0) goto ok2;
  return 1;
 ok2:

  puts ("t: if (one < 2)\n");
  if (one < 2) goto ok3;
  return 1;
 ok3:

  puts ("t: if (strlen (\".\"))\n");
  if (strlen (".")) goto ok4;
  return 1;
 ok4:

  puts ("t: if (!0)\n");
  if (!0) goto ok5;
  return 1;
 ok5:

  puts ("t: if (one == 1)\n");
  if (one == 1) goto ok6;
  return 1;
 ok6:

  puts ("t: if (one != 0)\n");
  if (one != 0) goto ok7;
  return 1;
 ok7:

  puts ("t: if (1 && !0)\n");
  if (1 && !0) goto ok8;
  return 1;
 ok8:

  puts ("t: if (++i)\n");
  if (++i) goto ok9;
 ok9:

  puts ("t: if (i--)\n");
  if (i--) goto ok10;
 ok10:

  puts ("t: for (i=0; i<4; ++i)\n");
  for (i=0; i<4; ++i);
  if (i != 4) return i;

  return 0;
}

#if __GNUC__
void
_start ()
{
  // int r=main ();
  // exit (r);
  int r;
  asm (
       "mov %%ebp,%%eax\n\t"
       "addl $8,%%eax\n\t"
       "push %%eax\n\t"

       "mov %%ebp,%%eax\n\t"
       "addl $4,%%eax\n\t"
       "movzbl (%%eax),%%eax\n\t"
       "push %%eax\n\t"

       "call main\n\t"
       
       "movl %%eax,%0\n\t"
       : "=r" (r)
       : //no inputs "" (&main)
       );
  exit (r);
}
#endif
