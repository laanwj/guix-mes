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
  eputs (s);
  eputs ("\n");
  *((int*)0) = 0;
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

typedef int SCM;
#if __NYACC__ || FIXME_NYACC
enum type_t {CHAR, CLOSURE, CONTINUATION, FUNCTION, KEYWORD, MACRO, NUMBER, PAIR, REF, SPECIAL, TSTRING, SYMBOL, VALUES, TVECTOR, BROKEN_HEART};
#else
enum type_t {CHAR, CLOSURE, CONTINUATION, FUNCTION, KEYWORD, MACRO, NUMBER, PAIR, REF, SPECIAL, STRING, SYMBOL, VALUES, VECTOR, BROKEN_HEART};
#endif
typedef SCM (*function0_t) (void);
typedef SCM (*function1_t) (SCM);
typedef SCM (*function2_t) (SCM, SCM);
typedef SCM (*function3_t) (SCM, SCM, SCM);
typedef SCM (*functionn_t) (SCM);
typedef struct function_struct {
  union {
    function0_t function0;
    function1_t function1;
    function2_t function2;
    function3_t function3;
    functionn_t functionn;
  } data;
  int arity;
} function_t;
struct scm;

typedef struct scm_struct {
  enum type_t type;
  union {
    char const *name;
    SCM string;
    SCM car;
    SCM ref;
    int length;
  } NYACC_CAR;
  union {
    int value;
    int function;
    SCM cdr;
    SCM closure;
    SCM continuation;
    SCM macro;
    SCM vector;
    int hits;
  } NYACC_CDR;
} scm;

char arena[200000];
scm *g_cells = (scm*)arena;

#define CAR(x) g_cells[x].car

#define CDR(x) g_cells[x].cdr

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
bload_env (SCM a) ///((internal))
{
  puts ("bload_env\n");
  g_stdin = open ("module/mes/read-0.mo", 0);
  if (g_stdin < 0) {eputs ("no such file: module/mes/read-0.mo\n");return 1;} 
#if __GNUC__
  puts ("fd: ");
  puts (itoa (g_stdin));
  puts ("\n");
  //g_stdin = g_stdin ? g_stdin : fopen (PREFIX "module/mes/read-0.mo", "r");
#endif
  char *p = (char*)g_cells;

  // int x;
  // x = getchar ();
  // if (x == 'M') puts ("M");
  // x = getchar ();
  // if (x == 'E') puts ("E");
  // x = getchar ();
  // if (x == 'S') puts ("S");
  
  assert (getchar () == 'M');
  assert (getchar () == 'E');
  assert (getchar () == 'S');
  puts ("GOT MES\n");
  g_stack = getchar () << 8;
  g_stack += getchar ();
  int c = getchar ();
  while (c != -1)
    {
      *p++ = c;
      c = getchar ();
    }
  g_free = (p-(char*)g_cells) / sizeof (scm);
  gc_peek_frame ();
  g_symbols = r1;
  g_stdin = STDIN;
  r0 = mes_builtins (r0);

#if __GNUC__
  puts ("cells read: ");
  puts (itoa (g_free));
  puts ("\n");
#endif
  return r2;
}

int
main (int argc, char *argv[])
{
#if __GNUC__
  g_debug = (int)getenv ("MES_DEBUG");
#endif
  //if (getenv ("MES_ARENA")) ARENA_SIZE = atoi (getenv ("MES_ARENA"));

  if (argc > 1 && !strcmp (argv[1], "--help")) return eputs ("Usage: mes [--dump|--load] < FILE\n");
  if (argc > 1 && !strcmp (argv[1], "--version")) {eputs ("Mes ");eputs (VERSION);return eputs ("\n");};

  if (argc > 1 && !strcmp (argv[1], "--help")) return eputs ("Usage: mes [--dump|--load] < FILE\n");


#if __GNUC__
  g_stdin = STDIN;
  r0 = mes_environment ();
#endif

#if MES_MINI
  puts ("Hello tiny-mes!\n");
  SCM program = bload_env (r0);
#else
  SCM program = (argc > 1 && !strcmp (argv[1], "--load"))
    ? bload_env (r0) : load_env (r0);
  if (argc > 1 && !strcmp (argv[1], "--dump")) return dump ();

  push_cc (r2, cell_unspecified, r0, cell_unspecified);
  r3 = cell_vm_begin;
  r1 = eval_apply ();
  stderr_ (r1);

  eputs ("\n");
  gc (g_stack);
#endif
#if __GNUC__
  if (g_debug)
    {
      eputs ("\nstats: [");
      eputs (itoa (g_free));
      eputs ("]\n");
    }
#endif
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
