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

#define MES_MINI 1

#if __GNUC__
#define  __NYACC__ 0
#define NYACC
#define NYACC2
#else
#define  __NYACC__ 1
#define NYACC nyacc
#define NYACC2 nyacc2
#endif

typedef long size_t;
void *malloc (size_t i);
int open (char const *s, int mode);
int read (int fd, int n);
void write (int fd, char const* s, int n);

#if __GNUC__
void
exit (int code)
{
  asm (
       "movl %0, %%ebx\n\t"
       "movl $1,  %%eax\n\t"
       "int  $0x80"
       : // no outputs "=" (r)
       : "" (code)
       );
  // not reached
  exit (0);
}

int
open (char const *s, int mode)
{
  //return syscall (SYS_open, s, mode);
  return 0;
}

int
read (int fd, int n)
{
  //syscall (SYS_read, 1, 1);
  return 0;
}

void
write (int fd, char const* s, int n)
{
  int r;
  //syscall (SYS_write, fd, s, n));
  asm (
       "mov %0, %%ebx\n\t"
       "mov %1, %%ecx\n\t"
       "mov %2, %%edx\n\t"

       "mov $0x4, %%eax\n\t"
       "int $0x80\n\t"
       : // no outputs "=" (r)
       : "" (fd), "" (s), "" (n)
       : "eax", "ebx", "ecx", "edx"
       );
}

void *
malloc (size_t size)
{
  int *n;
  int len = size + sizeof (size);
  //n = mmap (0, len, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANONYMOUS, 0, 0 );
  *n = len;
  return (void*)(n+1);
}

void
free (void *p)
{
  int *n = (int*)p-1;
  //munmap ((void*)p, *n);
}
#endif

#define EOF -1
#define STDIN 0
#define STDOUT 1
#define STDERR 2

//#include <stdio.h>
//#include <string.h>
//#include <stdlib.h>

int g_stdin;

#if __GNUC__
size_t
strlen (char const* s)
{
  int i = 0;
  while (s[i]) i++;
  return i;
}

int
strcmp (char const* a, char const* b)
{
  while (*a && *b && *a == *b) {a++;b++;}
  return *a - *b;
}

int
getc ()
{
  return read (g_stdin, 1);
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
eputs (char const* s)
{
  //write (STDERR, s, strlen (s));
  //int i = write (STDERR, s, strlen (s));
  int i = strlen (s);
  write (2, s, i);
  return 0;
}

int g_a;
int g_b;

#if 0
void
eputs2 (char const* s, int a)
{
  g_a = a;
  write (STDERR, s, strlen (s));
  //return 0;
}

void
eputs3 (char const* s, int a, int b)
{
  g_a = a;
  g_b = b;
  write (STDERR, s, strlen (s));
  //return 0;
}

char const*
itoa (int x)
{
  static char buf[10];
  char *p = buf+9;
  *p-- = 0;

  int sign = x < 0;
  if (sign)
    x = -x;
  
  do
    {
      *p-- = '0' + (x % 10);
      x = x / 10;
    } while (x);

  if (sign)
    *p-- = '-';

  return p+1;
}
#endif

void
assert_fail (char* s)
{
  eputs ("assert fail:");
  eputs (s);
  eputs ("\n");
  *((int*)0) = 0;
}

#endif

#define assert(x) ((x) ? (void)0 : assert_fail(#x))
#define false 0
#define true 1
typedef int bool;

int
main (int argc, char *argv[])
{
  puts ("arg0=");
  puts (argv[0]);
  puts ("\narg1=");
  puts (argv[1]);
  puts ("\n");
  eputs ("Strlen...\n");
  puts ("Bye micro\n");
  int i = argc;
  return i;
}

#if __GNUC__
// int
// test1()
// {
//   return 9;
// }

// void
// test()
// {
//   int r;
//   r=7;
//   r=test1();
// }

void
_start ()
{
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
