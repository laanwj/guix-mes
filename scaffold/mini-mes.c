/* -*-comment-start: "//";comment-end:""-*-
 * Mes --- Maxwell Equations of Software
 * Copyright © 2016 Jan Nieuwenhuizen <janneke@gnu.org>
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

#define EOF -1
#define STDIN 0
#define STDOUT 1
#define STDERR 2

//#include <stdio.h>
//#include <string.h>
//#include <stdlib.h>

int g_stdin;

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
  while (*a && *b && *a == *b) {*a++;b++;}
  return *a == *b;
}

int
getc ()
{
  return read (g_stdin, 1);
}

int
puts (char const* s)
{
  write (STDOUT, s, strlen (s));
  return 0;
}

int
eputs (char const* s)
{
  write (STDERR, s, strlen (s));
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

void
assert_fail (char* s)
{
  eputs ("assert fail:");
  eputs (s);
  eputs ("\n");
  *((int*)0) = 0;
}

#define assert(x) ((x) ? (void)0 : assert_fail(#x))
#define false 0
#define true 1
typedef int bool;

int ARENA_SIZE = 100000;

typedef int SCM;
enum type_t {CHAR, CLOSURE, CONTINUATION, FUNCTION, KEYWORD, MACRO, NUMBER, PAIR, REF, SPECIAL, STRING, SYMBOL, VALUES, VECTOR, BROKEN_HEART};
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
  } NYACC;
  union {
    int value;
    int function;
    SCM cdr;
    SCM closure;
    SCM continuation;
    SCM macro;
    SCM vector;
    int hits;
  } NYACC2;
} scm;

scm scm_nil = {SPECIAL, "()"};
scm scm_f = {SPECIAL, "#f"};
scm scm_t = {SPECIAL, "#t"};
scm scm_dot = {SPECIAL, "."};
scm scm_arrow = {SPECIAL, "=>"};
scm scm_undefined = {SPECIAL, "*undefined*"};
scm scm_unspecified = {SPECIAL, "*unspecified*"};
scm scm_closure = {SPECIAL, "*closure*"};
scm scm_circular = {SPECIAL, "*circular*"};
scm scm_begin = {SPECIAL, "*begin*"};


//#include "mes.symbols.h"
#define cell_nil 1
#define cell_f 2
#define cell_t 3
#define cell_dot 4
#define cell_arrow 5
#define cell_undefined 6
#define cell_unspecified 7
#define cell_closure 8
#define cell_circular 9
#define cell_begin 10
#define cell_symbol_dot 11
#define cell_symbol_lambda 12
#define cell_symbol_begin 13
#define cell_symbol_if 14
#define cell_symbol_quote 15
#define cell_symbol_set_x 16

#if __GNUC__
bool g_debug = false;
#endif

int g_free = 0;
scm *g_cells;
//scm *g_news = 0;
SCM tmp;
SCM tmp_num;
SCM tmp_num2;

function_t functions[200];
int g_function = 0;

SCM g_symbols = 0;
SCM g_stack = 0;
SCM r0 = 0; // a/env
SCM r1 = 0; // param 1
SCM r2 = 0; // save 2+load/dump
SCM r3 = 0; // continuation

SCM make_cell (SCM type, SCM car, SCM cdr);
function_t fun_make_cell = {&make_cell, 3};
scm scm_make_cell = {FUNCTION, "make-cell", 0};
SCM cell_make_cell;

SCM cons (SCM x, SCM y);
function_t fun_cons = {&cons, 2};
scm scm_cons = {FUNCTION, "cons", 0};
SCM cell_cons;

SCM car (SCM x);
function_t fun_car = {&car, 1};
scm scm_car = {FUNCTION, "car", 0};
SCM cell_car;

SCM cdr (SCM x);
function_t fun_cdr = {&cdr, 1};
scm scm_cdr = {FUNCTION, "cdr", 0};
SCM cell_cdr;

// SCM eq_p (SCM x, SCM y);
// function_t fun_eq_p = {&eq_p, 2};
// scm scm_eq_p = {FUNCTION, "eq?", 0};
// SCM cell_eq_p;

#define TYPE(x) g_cells[x].type

#define CAR(x) g_cells[x].car
#define LENGTH(x) g_cells[x].length
#define STRING(x) g_cells[x].string

#define CDR(x) g_cells[x].cdr
#define VALUE(x) g_cells[x].value
#define VECTOR(x) g_cells[x].vector

#define MAKE_CHAR(n) make_cell (tmp_num_ (CHAR), 0, tmp_num2_ (n))
//#define MAKE_CONTINUATION(n) make_cell (tmp_num_ (CONTINUATION), n, g_stack)
//#define MAKE_NUMBER(n) make_cell (tmp_num_ (NUMBER), 0, tmp_num2_ (n))
//#define MAKE_REF(n) make_cell (tmp_num_ (REF), n, 0)
#define MAKE_STRING(x) make_cell (tmp_num_ (STRING), x, 0)

SCM
alloc (int n)
{
  assert (g_free + n < ARENA_SIZE);
  SCM x = g_free;
  g_free += n;
  return x;
}

SCM
make_cell (SCM type, SCM car, SCM cdr)
{
  SCM x = alloc (1);
  assert (TYPE (type) == NUMBER);
  TYPE (x) = VALUE (type);
  if (VALUE (type) == CHAR || VALUE (type) == NUMBER) {
    if (car) CAR (x) = CAR (car);
    if (cdr) CDR (x) = CDR (cdr);
  } else if (VALUE (type) == FUNCTION) {
    if (car) CAR (x) = car;
    if (cdr) CDR (x) = CDR (cdr);
  } else {
    CAR (x) = car;
    CDR (x) = cdr;
  }
  return x;
}

SCM
tmp_num_ (int x)
{
  VALUE (tmp_num) = x;
  return tmp_num;
}

SCM
tmp_num2_ (int x)
{
  VALUE (tmp_num2) = x;
  return tmp_num2;
}

SCM
cons (SCM x, SCM y)
{
  VALUE (tmp_num) = PAIR;
  return make_cell (tmp_num, x, y);
}

SCM
car (SCM x)
{
#if MES_MINI
  assert("!car");
#else
  if (TYPE (x) != PAIR) error (cell_symbol_not_a_pair, cons (x, cell_symbol_car));
#endif
  return CAR (x);
}

SCM
cdr (SCM x)
{
#if MES_MINI
  assert("!car");
#else
  if (TYPE (x) != PAIR) error (cell_symbol_not_a_pair, cons (x, cell_symbol_cdr));
#endif
  return CDR (x);
}

// SCM
// eq_p (SCM x, SCM y)
// {
//   return (x == y
//           || ((TYPE (x) == KEYWORD && TYPE (y) == KEYWORD
//                && STRING (x) == STRING (y)))
//           || (TYPE (x) == CHAR && TYPE (y) == CHAR
//               && VALUE (x) == VALUE (y))
//           || (TYPE (x) == NUMBER && TYPE (y) == NUMBER
//               && VALUE (x) == VALUE (y)))
//     ? cell_t : cell_f;
// }

SCM
gc_push_frame ()
{
  SCM frame = cons (r1, cons (r2, cons (r3, cons (r0, cell_nil))));
  return g_stack = cons (frame, g_stack);
}

SCM
push_cc (SCM p1, SCM p2, SCM a, SCM c) ///((internal))
{
  SCM x = r3;
  r3 = c;
  r2 = p2;
  gc_push_frame ();
  r1 = p1;
  r0 = a;
  r3 = x;
  return cell_unspecified;
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

SCM
mes_g_stack (SCM a) ///((internal))
{
  r0 = a;
  r1 = MAKE_CHAR (0);
  r2 = MAKE_CHAR (0);
  r3 = MAKE_CHAR (0);
  g_stack = cons (cell_nil, cell_nil);
  return r0;
}

// Environment setup
SCM
make_tmps (scm* cells)
{
  tmp = g_free++;
  cells[tmp].type = CHAR;
  tmp_num = g_free++;
  cells[tmp_num].type = NUMBER;
  tmp_num2 = g_free++;
  cells[tmp_num2].type = NUMBER;
}

SCM
make_symbol_ (SCM s)
{
  VALUE (tmp_num) = SYMBOL;
  SCM x = make_cell (tmp_num, s, 0);
  g_symbols = cons (x, g_symbols);
  return x;
}

SCM
make_symbol (SCM s)
{
#if MES_MINI
  SCM x = 0;
#else
  SCM x = lookup_symbol_ (s);
#endif
  return x ? x : make_symbol_ (s);
}

SCM
cstring_to_list (char const* s)
{
  SCM p = cell_nil;
  int i = strlen (s);
  while (i--)
    p = cons (MAKE_CHAR (s[i]), p);
  return p;
}

SCM
acons (SCM key, SCM value, SCM alist)
{
  return cons (cons (key, value), alist);
}

// Jam Collector
SCM g_symbol_max;

SCM
gc_init_cells ()
{
  g_cells = (scm *)malloc (2*ARENA_SIZE*sizeof(scm));
  g_cells[0].type = VECTOR;
  LENGTH (0) = 1000;
  VECTOR (0) = 0;
  g_cells++;
  g_cells[0].type = CHAR;
  VALUE (0) = 'c';
}

// INIT NEWS

SCM
mes_symbols () ///((internal))
{
  gc_init_cells ();
  //  gc_init_news ();

#if __GNUC__ && 0
  //#include "mes.symbols.i"
#else
g_free++;
g_cells[cell_nil] = scm_nil;

g_free++;
g_cells[cell_f] = scm_f;

g_free++;
g_cells[cell_t] = scm_t;

g_free++;
g_cells[cell_dot] = scm_dot;

g_free++;
g_cells[cell_arrow] = scm_arrow;

g_free++;
g_cells[cell_undefined] = scm_undefined;

g_free++;
g_cells[cell_unspecified] = scm_unspecified;

g_free++;
g_cells[cell_closure] = scm_closure;

g_free++;
g_cells[cell_circular] = scm_circular;

g_free++;
g_cells[cell_begin] = scm_begin;

#endif

  g_symbol_max = g_free;
  make_tmps (g_cells);

  g_symbols = 0;
  for (int i=1; i<g_symbol_max; i++)
    g_symbols = cons (i, g_symbols);

  SCM a = cell_nil;

#if __GNUC__ && 0
  //#include "mes.symbol-names.i"
#else
g_cells[cell_nil].car = cstring_to_list (scm_nil.name);
g_cells[cell_f].car = cstring_to_list (scm_f.name);
g_cells[cell_t].car = cstring_to_list (scm_t.name);
g_cells[cell_dot].car = cstring_to_list (scm_dot.name);
g_cells[cell_arrow].car = cstring_to_list (scm_arrow.name);
g_cells[cell_undefined].car = cstring_to_list (scm_undefined.name);
g_cells[cell_unspecified].car = cstring_to_list (scm_unspecified.name);
g_cells[cell_closure].car = cstring_to_list (scm_closure.name);
g_cells[cell_circular].car = cstring_to_list (scm_circular.name);
g_cells[cell_begin].car = cstring_to_list (scm_begin.name);
#endif

  // a = acons (cell_symbol_mes_version, MAKE_STRING (cstring_to_list (VERSION)), a);
  // a = acons (cell_symbol_mes_prefix, MAKE_STRING (cstring_to_list (PREFIX)), a);

  a = acons (cell_symbol_dot, cell_dot, a); //
  a = acons (cell_symbol_begin, cell_begin, a);
  a = acons (cell_closure, a, a);

  // a = acons (cell_symbol_call_with_current_continuation, cell_call_with_current_continuation, a);
  // a = acons (cell_symbol_sc_expand, cell_f, a);

  return a;
}

SCM
mes_environment () ///((internal))
{
  SCM a = mes_symbols ();
  return mes_g_stack (a);
}

SCM
mes_builtins (SCM a)
{
#if __GNUC__ && 0 // FIXME: Nyacc
// #include "mes.i"

// #include "lib.i"
// #include "math.i"
// #include "posix.i"
// #include "reader.i"

// #include "lib.environment.i"
// #include "math.environment.i"
// #include "mes.environment.i"
// #include "posix.environment.i"
// #include "reader.environment.i"
#else
scm_make_cell.function = g_function;
functions[g_function++] = fun_make_cell;
cell_make_cell = g_free++;
g_cells[cell_make_cell] = scm_make_cell;

scm_cons.function = g_function;
functions[g_function++] = fun_cons;
cell_cons = g_free++;
g_cells[cell_cons] = scm_cons;

scm_car.function = g_function;
functions[g_function++] = fun_car;
cell_car = g_free++;
g_cells[cell_car] = scm_car;

scm_cdr.function = g_function;
functions[g_function++] = fun_cdr;
cell_cdr = g_free++;
g_cells[cell_cdr] = scm_cdr;

// scm_eq_p.function = g_function;
// functions[g_function++] = fun_eq_p;
// cell_eq_p = g_free++;
// g_cells[cell_eq_p] = scm_eq_p;


scm_make_cell.string = cstring_to_list (scm_make_cell.name);
g_cells[cell_make_cell].string = MAKE_STRING (scm_make_cell.string);
a = acons (make_symbol (scm_make_cell.string), cell_make_cell, a);

scm_cons.string = cstring_to_list (scm_cons.name);
g_cells[cell_cons].string = MAKE_STRING (scm_cons.string);
a = acons (make_symbol (scm_cons.string), cell_cons, a);

scm_car.string = cstring_to_list (scm_car.name);
g_cells[cell_car].string = MAKE_STRING (scm_car.string);
a = acons (make_symbol (scm_car.string), cell_car, a);

scm_cdr.string = cstring_to_list (scm_cdr.name);
g_cells[cell_cdr].string = MAKE_STRING (scm_cdr.string);
a = acons (make_symbol (scm_cdr.string), cell_cdr, a);

// scm_eq_p.string = cstring_to_list (scm_eq_p.name);
// g_cells[cell_eq_p].string = MAKE_STRING (scm_eq_p.string);
// a = acons (make_symbol (scm_eq_p.string), cell_eq_p, a);

#endif
  return a;
}

int
getchar ()
{
  return getc (g_stdin);
}

SCM
bload_env (SCM a) ///((internal))
{
  g_stdin = open ("module/mes/read-0.mo", 0);
#if __GNUC__
  //g_stdin = g_stdin ? g_stdin : fopen (PREFIX "module/mes/read-0.mo", "r");
#endif
  char *p = (char*)g_cells;
  assert (getchar () == 'M');
  assert (getchar () == 'E');
  assert (getchar () == 'S');
  g_stack = getchar () << 8;
  g_stack += getchar ();
  int c = getchar ();
  while (c != EOF)
    {
      *p++ = c;
      c = getchar ();
    }
  g_free = (p-(char*)g_cells) / sizeof (scm);
  gc_peek_frame ();
  g_symbols = r1;
  g_stdin = STDIN;
  r0 = mes_builtins (r0);
  return r2;
}

char const*
string_to_cstring (SCM s)
{
  static char buf[1024];
  char *p = buf;
  s = STRING (s);
  while (s != cell_nil)
    {
      *p++ = VALUE (car (s));
      s = cdr (s);
    }
  *p = 0;
  return buf;
}

SCM
stderr_ (SCM x)
{
  //SCM write;
  if (TYPE (x) == STRING)
    eputs (string_to_cstring (x));
  // else if ((write = assq_ref_cache (cell_symbol_write, r0)) != cell_undefined)
  //   apply (assq_ref_cache (cell_symbol_display, r0), cons (x, cons (MAKE_NUMBER (2), cell_nil)), r0);
  else if (TYPE (x) == SPECIAL || TYPE (x) == STRING || TYPE (x) == SYMBOL)
    eputs (string_to_cstring (x));
  else if (TYPE (x) == NUMBER)
    eputs (itoa (VALUE (x)));
  else
    eputs ("display: undefined\n");
  return cell_unspecified;
}

int
main (int argc, char *argv[])
{
  eputs (itoa (234));
  eputs ("\n");
  assert(!"boo");
  return 33;
  
#if __GNUC__
  //g_debug = getenv ("MES_DEBUG");
#endif
  //if (getenv ("MES_ARENA")) ARENA_SIZE = atoi (getenv ("MES_ARENA"));
  if (argc > 1 && !strcmp (argv[1], "--help")) return eputs ("Usage: mes [--dump|--load] < FILE");
  if (argc > 1 && !strcmp (argv[1], "--version")) {eputs ("Mes ");return eputs (VERSION);};
  g_stdin = STDIN;
  r0 = mes_environment ();

#if MES_MINI
  SCM program = bload_env (r0);
#else  
  SCM program = (argc > 1 && !strcmp (argv[1], "--load"))
    ? bload_env (r0) : load_env (r0);
  if (argc > 1 && !strcmp (argv[1], "--dump")) return dump ();
#endif

  push_cc (r2, cell_unspecified, r0, cell_unspecified);
  // r3 = cell_vm_begin;
  // r1 = eval_apply ();
  stderr_ (r1);

  eputs ("\n");
#if !MES_MINI
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
  puts ("Hello mini-mes!\n");
  return 0;
}

void
_start ()
{
  /* main body of program: call main(), etc */
  
  /* exit system call */
  asm (
       "movl $1,%eax;"
       "xorl %ebx,%ebx;"
       "int  $0x80"
       );
}
