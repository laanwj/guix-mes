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
#define FIXME_NYACC 1
#define  __NYACC__ 0
#define NYACC_CAR
#define NYACC_CDR
#else
#define  __NYACC__ 1
#define NYACC_CAR nyacc_car
#define NYACC_CDR nyacc_cdr
#endif

char arena[200];

int g_stdin = 0;

#if __GNUC__
typedef long size_t;
void *malloc (size_t i);
int open (char const *s, int mode);
int read (int fd, void* buf, size_t n);
void write (int fd, char const* s, int n);

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

char const*
getenv (char const* p)
{
  return 0;
}

int
read (int fd, void* buf, size_t n)
{
  int r;
  //syscall (SYS_write, fd, s, n));
  asm (
       "movl %1,%%ebx\n\t"
       "movl %2,%%ecx\n\t"
       "movl %3,%%edx\n\t"
       "movl $0x3,%%eax\n\t"
       "int  $0x80\n\t"
       "mov %%eax,%0\n\t"
       : "=r" (r)
       : "" (fd), "" (buf), "" (n)
       : "eax", "ebx", "ecx", "edx"
       );
  return r;
}

int
open (char const *s, int mode)
{
  int r;
  //syscall (SYS_open, mode));
  asm (
       "mov %1,%%ebx\n\t"
       "mov %2,%%ecx\n\t"
       "mov $0x5,%%eax\n\t"
       "int $0x80\n\t"
       "mov %%eax,%0\n\t"
       : "=r" (r)
       : "" (s), "" (mode)
       : "eax", "ebx", "ecx"
       );
  return r;
}

int
getchar ()
{
  char c;
  int r = read (g_stdin, &c, 1);
  if (r < 1) return -1;
  return c;
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

       "mov $0x4, %%eax\n\t"
       "int $0x80\n\t"
       : // no outputs "=" (r)
       : "" (fd), "" (s), "" (n)
       : "eax", "ebx", "ecx", "edx"
       );
}

int
putchar (int c)
{
  //write (STDOUT, s, strlen (s));
  //int i = write (STDOUT, s, strlen (s));
  write (1, (char*)&c, 1);
  return 0;
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

#define EOF -1
#define STDIN 0
#define STDOUT 1
#define STDERR 2

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
#if __GNUC__
  eputs (s);
#endif
  eputs ("\n");
#if __GNUC__
  *((int*)0) = 0;
#endif
}

#if __GNUC__
#define assert(x) ((x) ? (void)0 : assert_fail ("boo:" #x))
#else
//#define assert(x) ((x) ? (void)0 : assert_fail ("boo:" #x))
#define assert(x) ((x) ? (void)0 : assert_fail (0))
#endif

typedef int SCM;

#if __GNUC__
int g_debug = 0;
#endif

int g_free = 0;

SCM g_symbols = 0;
SCM g_stack = 0;
SCM r0 = 0; // a/env
SCM r1 = 0; // param 1
SCM r2 = 0; // save 2+load/dump
SCM r3 = 0; // continuation

#if __NYACC__ || FIXME_NYACC
enum type_t {CHAR, CLOSURE, CONTINUATION, FUNCTION, KEYWORD, MACRO, NUMBER, PAIR, REF, SPECIAL, TSTRING, SYMBOL, VALUES, TVECTOR, BROKEN_HEART};
#else
enum type_t {CHAR, CLOSURE, CONTINUATION, FUNCTION, KEYWORD, MACRO, NUMBER, PAIR, REF, SPECIAL, STRING, SYMBOL, VALUES, VECTOR, BROKEN_HEART};
#endif

struct scm {
  enum type_t type;
  SCM car;
  SCM cdr;
};

//char arena[200];
//struct scm *g_cells = arena;
//struct scm *g_cells = (struct scm*)arena;
struct scm *g_cells = arena;

#define cell_nil 1
#define cell_f 2
#define cell_t 3

#define TYPE(x) (g_cells[x].type)

#define CAR(x) g_cells[x].car

#define CDR(x) g_cells[x].cdr
//#define VALUE(x) g_cells[x].value
#define VALUE(x) g_cells[x].cdr

SCM
car (SCM x)
{
#if MES_MINI
  //Nyacc
  //assert ("!car");
#else
  if (TYPE (x) != PAIR) error (cell_symbol_not_a_pair, cons (x, cell_symbol_car));
#endif
  return CAR (x);
}

SCM
cdr (SCM x)
{
#if MES_MINI
  //Nyacc
  //assert ("!cdr");
#else
  if (TYPE (x) != PAIR) error (cell_symbol_not_a_pair, cons (x, cell_symbol_cdr));
#endif
  return CDR(x);
}
SCM caar (SCM x) {return car (car (x));}
SCM cadr (SCM x) {return car (cdr (x));}
SCM cdar (SCM x) {return cdr (car (x));}
SCM cddr (SCM x) {return cdr (cdr (x));}

SCM
gc_peek_frame ()
{
  SCM frame = car (g_stack);
  r1 = car (frame);
  r2 = cadr (frame);
  r3 = car (cddr (frame));
  r0 = cadr (cddr (frame));
  return frame;
}

// Environment setup

SCM
mes_environment ()
{
  return 0;
}

SCM
mes_builtins (SCM a)
{
  return a;
}

SCM
fill ()
{
  TYPE (0) = 0x6c6c6168;
  CAR (0) = 0x6a746f6f;
  CDR (0) = 0x00002165;

  TYPE (1) = SYMBOL;
  CAR (1) = 0x2d2d2d2d;
  CDR (1) = 0x3e3e3e3e;

  TYPE (9) = 0x2d2d2d2d;
  CAR (9) = 0x2d2d2d2d;
  CDR (9) = 0x3e3e3e3e;

  // (A(B))
  TYPE (10) = PAIR;
  CAR (10) = 11;
  CDR (10) = 12;

  TYPE (11) = CHAR;
  CAR (11) = 0x58585858;
  CDR (11) = 89;

  TYPE (12) = PAIR;
  CAR (12) = 13;
  CDR (12) = 1;

  TYPE (13) = CHAR;
  CAR (11) = 0x58585858;
  CDR (13) = 90;

  TYPE (14) = 0x58585858;
  CAR (14) = 0x58585858;
  CDR (14) = 0x58585858;

  TYPE (14) = 0x58585858;
  CAR (14) = 0x58585858;
  CDR (14) = 0x58585858;

  TYPE (16) = 0x3c3c3c3c;
  CAR (16) = 0x2d2d2d2d;
  CDR (16) = 0x2d2d2d2d;
  return 0;
}

SCM
display_ (SCM x)
{
  //puts ("<display>\n");
  switch (TYPE (x))
    {
    case CHAR:
      {
        //puts ("<char>\n");
        puts ("#\\");
        putchar (VALUE (x));
        break;
      }
    case FUNCTION:
      {
        //puts ("<function>\n");
        if (VALUE (x) == 0)
          puts ("make-cell");
        if (VALUE (x) == 1)
          puts ("cons");
        if (VALUE (x) == 2)
          puts ("car");
        if (VALUE (x) == 3)
          puts ("cdr");
        break;
      }
    case NUMBER:
      {
        //puts ("<number>\n");
#if __GNUC__
        putchar (48 + VALUE (x));
#else
        int i;
        i = VALUE (x);
        i = i + 48;
        putchar (i);
#endif
        break;
      }
    case PAIR:
      {
        //puts ("<pair>\n");
        //if (cont != cell_f) puts "(");
        puts ("(");
        if (x && x != cell_nil) display_ (CAR (x));
        if (CDR (x) && CDR (x) != cell_nil)
          {
#if __GNUC__
            if (TYPE (CDR (x)) != PAIR)
              puts (" . ");
#else
            int c;
            c = CDR (x);
            c = TYPE (c);
            if (c != PAIR)
              puts (" . ");
#endif
            display_ (CDR (x));
          }
        //if (cont != cell_f) puts (")");
        puts (")");
        break;
      }
    default:
      {
        //puts ("<default>\n");
        puts ("_");
        break;
      }
    }
  return 0;
}

SCM
bload_env (SCM a) ///((internal))
{
  puts ("reading: ");
  char *mo = "module/mes/hack-32.mo";
  puts (mo);
  puts ("\n");
  g_stdin = open (mo, 0);
  if (g_stdin < 0) {eputs ("no such file: module/mes/read-0-32.mo\n");return 1;} 

  // BOOM
  //char *p = arena;
  char *p = (char*)g_cells;
  int c;

  c = getchar ();
  putchar (c);
  if (c != 'M') exit (10);
  c = getchar ();
  putchar (c);
  if (c != 'E') exit (11);
  c = getchar ();
  putchar (c);
  if (c != 'S') exit (12);
  puts (" *GOT MES*\n");

  // skip stack
  getchar ();
  getchar ();

  c = getchar ();
  while (c != -1)
    {
      *p++ = c;
      c = getchar ();
    }

  puts ("read done\n");
  display_ (10);

  puts ("\n");
  return r2;
}

int
main (int argc, char *argv[])
{
  fill ();
  puts (g_cells);
  puts ("\n");
  display_ (10);
  puts ("\n");
  SCM program = bload_env (r0);

  return 0;
}

#if __GNUC__
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

