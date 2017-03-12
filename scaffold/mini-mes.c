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
#define FIXED_PRIMITIVES 0

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

int ARENA_SIZE = 1200000;
char arena[1200000];

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

int puts (char const*);
char const* itoa (int);

int
getchar ()
{
  char c;
  int r = read (g_stdin, &c, 1);
  if (r < 1) return -1;
  int i = c;
  if (i < 0) i += 256;
  return i;
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
#endif

char itoa_buf[10];

char const*
itoa (int x)
{
  //static char itoa_buf[10];
  //char *p = buf+9;
  char *p = itoa_buf;
  p += 9;
  *p-- = 0;

  //int sign = x < 0;
  int sign;
  sign = x < 0;
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

SCM g_continuations = 0;
SCM g_symbols = 0;
SCM g_stack = 0;
// a/env
SCM r0 = 0;
// param 1
SCM r1 = 0;
// save 2+load/dump
SCM r2 = 0;
// continuation
SCM r3 = 0;

enum type_t {TCHAR, TCLOSURE, TCONTINUATION, TFUNCTION, TKEYWORD, TMACRO, TNUMBER, TPAIR, TREF, TSPECIAL, TSTRING, TSYMBOL, TVALUES, TVECTOR, TBROKEN_HEART};

struct scm {
  enum type_t type;
  SCM car;
  SCM cdr;
};

typedef int (*f_t) (void);
struct function {
  int (*function) (void);
  int arity;
  char *name;
};

struct scm *g_cells = arena;

//FIXME
//struct scm *g_news = 0;

struct scm scm_nil = {TSPECIAL, "()",0};
struct scm scm_f = {TSPECIAL, "#f",0};
struct scm scm_t = {TSPECIAL, "#t",0};
struct scm scm_dot = {TSPECIAL, ".",0};
struct scm scm_arrow = {TSPECIAL, "=>",0};
struct scm scm_undefined = {TSPECIAL, "*undefined*",0};
struct scm scm_unspecified = {TSPECIAL, "*unspecified*",0};
struct scm scm_closure = {TSPECIAL, "*closure*",0};
struct scm scm_circular = {TSPECIAL, "*circular*",0};
struct scm scm_begin = {TSPECIAL, "*begin*",0};

struct scm scm_symbol_dot = {TSYMBOL, "*dot*",0};
struct scm scm_symbol_lambda = {TSYMBOL, "lambda",0};
struct scm scm_symbol_begin = {TSYMBOL, "begin",0};
struct scm scm_symbol_if = {TSYMBOL, "if",0};
struct scm scm_symbol_quote = {TSYMBOL, "quote",0};
struct scm scm_symbol_set_x = {TSYMBOL, "set!",0};

struct scm scm_symbol_sc_expand = {TSYMBOL, "sc-expand",0};
struct scm scm_symbol_macro_expand = {TSYMBOL, "macro-expand",0};
struct scm scm_symbol_sc_expander_alist = {TSYMBOL, "*sc-expander-alist*",0};

struct scm scm_symbol_call_with_values = {TSYMBOL, "call-with-values",0};
struct scm scm_call_with_current_continuation = {TSPECIAL, "*call/cc*",0};
struct scm scm_symbol_call_with_current_continuation = {TSYMBOL, "call-with-current-continuation",0};
struct scm scm_symbol_current_module = {TSYMBOL, "current-module",0};
struct scm scm_symbol_primitive_load = {TSYMBOL, "primitive-load",0};
struct scm scm_symbol_read_input_file = {TSYMBOL, "read-input-file",0};
struct scm scm_symbol_write = {TSYMBOL, "write",0};
struct scm scm_symbol_display = {TSYMBOL, "display",0};

struct scm scm_symbol_throw = {TSYMBOL, "throw",0};
struct scm scm_symbol_not_a_pair = {TSYMBOL, "not-a-pair",0};
struct scm scm_symbol_system_error = {TSYMBOL, "system-error",0};
struct scm scm_symbol_wrong_number_of_args = {TSYMBOL, "wrong-number-of-args",0};
struct scm scm_symbol_wrong_type_arg = {TSYMBOL, "wrong-type-arg",0};
struct scm scm_symbol_unbound_variable = {TSYMBOL, "unbound-variable",0};

struct scm scm_symbol_argv = {TSYMBOL, "%argv",0};
struct scm scm_symbol_mes_prefix = {TSYMBOL, "%prefix",0};
struct scm scm_symbol_mes_version = {TSYMBOL, "%version",0};

struct scm scm_symbol_car = {TSYMBOL, "car",0};
struct scm scm_symbol_cdr = {TSYMBOL, "cdr",0};
struct scm scm_symbol_null_p = {TSYMBOL, "null?",0};
struct scm scm_symbol_eq_p = {TSYMBOL, "eq?",0};
struct scm scm_symbol_cons = {TSYMBOL, "cons",0};

struct scm scm_vm_evlis = {TSPECIAL, "*vm-evlis*",0};
struct scm scm_vm_evlis2 = {TSPECIAL, "*vm-evlis2*",0};
struct scm scm_vm_evlis3 = {TSPECIAL, "*vm-evlis3*",0};
struct scm scm_vm_apply = {TSPECIAL, "core:apply",0};
struct scm scm_vm_apply2 = {TSPECIAL, "*vm-apply2*",0};
struct scm scm_vm_eval = {TSPECIAL, "core:eval",0};

//FIXED_PRIMITIVES
struct scm scm_vm_eval_car = {TSPECIAL, "*vm-eval-car*",0};
struct scm scm_vm_eval_cdr = {TSPECIAL, "*vm-eval-cdr*",0};
struct scm scm_vm_eval_cons = {TSPECIAL, "*vm-eval-cons*",0};
struct scm scm_vm_eval_null_p = {TSPECIAL, "*vm-eval-null-p*",0};

struct scm scm_vm_eval_set_x = {TSPECIAL, "*vm-eval-set!*",0};
struct scm scm_vm_eval_macro = {TSPECIAL, "*vm-eval-macro*",0};
struct scm scm_vm_eval2 = {TSPECIAL, "*vm-eval2*",0};
struct scm scm_vm_macro_expand = {TSPECIAL, "core:macro-expand",0};
struct scm scm_vm_begin = {TSPECIAL, "*vm-begin*",0};
struct scm scm_vm_begin_read_input_file = {TSPECIAL, "*vm-begin-read-input-file*",0};
struct scm scm_vm_begin2 = {TSPECIAL, "*vm-begin2*",0};
struct scm scm_vm_if = {TSPECIAL, "*vm-if*",0};
struct scm scm_vm_if_expr = {TSPECIAL, "*vm-if-expr*",0};
struct scm scm_vm_call_with_values2 = {TSPECIAL, "*vm-call-with-values2*",0};
struct scm scm_vm_call_with_current_continuation2 = {TSPECIAL, "*vm-call-with-current-continuation2*",0};
struct scm scm_vm_return = {TSPECIAL, "*vm-return*",0};

struct scm scm_test = {TSYMBOL, "test",0};

#include "mini-mes.symbols.h"

SCM tmp;
SCM tmp_num;
SCM tmp_num2;

struct function g_functions[200];
int g_function = 0;

// #include "lib.h"
// #include "math.h"
#include "mini-mes.h"
// #include "posix.h"
// #include "reader.h"


#define TYPE(x) (g_cells[x].type)

#define CAR(x) g_cells[x].car
#define LENGTH(x) g_cells[x].car
#define STRING(x) g_cells[x].car

#define CDR(x) g_cells[x].cdr
#define CLOSURE(x) g_cells[x].cdr
#define CONTINUATION(x) g_cells[x].cdr
#if __GNUC__
//#define FUNCTION(x) g_functions[g_cells[x].function]
#endif

#define FUNCTION(x) g_functions[g_cells[x].cdr]
#define MACRO(x) g_cells[x].car
#define VALUE(x) g_cells[x].cdr
#define VECTOR(x) g_cells[x].cdr

#define MAKE_CHAR(n) make_cell (tmp_num_ (TCHAR), 0, tmp_num2_ (n))
#define MAKE_CONTINUATION(n) make_cell (tmp_num_ (TCONTINUATION), n, g_stack)
#define MAKE_NUMBER(n) make_cell (tmp_num_ (TNUMBER), 0, tmp_num2_ (n))
//#define MAKE_REF(n) make_cell (tmp_num_ (REF), n, 0)


#define CAAR(x) CAR (CAR (x))
#define CDAR(x) CDR (CAR (x))
#define CADAR(x) CAR (CDR (CAR (x)))
#define CADDR(x) CAR (CDR (CDR (x)))
// #define CDDDR(x) CDR (CDR (CDR (x)))
#define CDADAR(x) CAR (CDR (CAR (CDR (x))))
#define CADR(x) CAR (CDR (x))

#define MAKE_STRING(x) make_cell (tmp_num_ (TSTRING), x, 0)

SCM
alloc (int n)
{
  assert (g_free + n < ARENA_SIZE);
  SCM x = g_free;
  g_free += n;
  return x;
}

#define DEBUG 0

SCM
make_cell (SCM type, SCM car, SCM cdr)
{
  SCM x = alloc (1);
  assert (TYPE (type) == TNUMBER);
  TYPE (x) = VALUE (type);
  if (VALUE (type) == TCHAR || VALUE (type) == TNUMBER) {
    if (car) CAR (x) = CAR (car);
    if (cdr) CDR(x) = CDR(cdr);
  }
  else if (VALUE (type) == TFUNCTION) {
    if (car) CAR (x) = car;
    if (cdr) CDR(x) = CDR(cdr);
  }
  else {
    CAR (x) = car;
    CDR(x) = cdr;
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
  VALUE (tmp_num) = TPAIR;
  return make_cell (tmp_num, x, y);
}

SCM
car (SCM x)
{
#if MES_MINI
  //Nyacc
  //assert ("!car");
#else
  if (TYPE (x) != TPAIR) error (cell_symbol_not_a_pair, cons (x, cell_symbol_car));
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
  if (TYPE (x) != TPAIR) error (cell_symbol_not_a_pair, cons (x, cell_symbol_cdr));
#endif
  return CDR(x);
}

SCM
null_p (SCM x)
{
  return x == cell_nil ? cell_t : cell_f;
}

SCM
eq_p (SCM x, SCM y)
{
  return (x == y
          || ((TYPE (x) == TKEYWORD && TYPE (y) == TKEYWORD
               && STRING (x) == STRING (y)))
          || (TYPE (x) == TCHAR && TYPE (y) == TCHAR
              && VALUE (x) == VALUE (y))
          || (TYPE (x) == TNUMBER && TYPE (y) == TNUMBER
              && VALUE (x) == VALUE (y)))
    ? cell_t : cell_f;
}

SCM
type_ (SCM x)
{
  return MAKE_NUMBER (TYPE (x));
}

SCM
car_ (SCM x)
{
  return (TYPE (x) != TCONTINUATION
          && (TYPE (CAR (x)) == TPAIR // FIXME: this is weird
              || TYPE (CAR (x)) == TREF
              || TYPE (CAR (x)) == TSPECIAL
              || TYPE (CAR (x)) == TSYMBOL
              || TYPE (CAR (x)) == TSTRING)) ? CAR (x) : MAKE_NUMBER (CAR (x));
}

SCM
cdr_ (SCM x)
{
  return (TYPE (CDR (x)) == TPAIR
          || TYPE (CDR (x)) == TREF
          || TYPE (CAR (x)) == TSPECIAL
          || TYPE (CDR (x)) == TSYMBOL
          || TYPE (CDR (x)) == TSTRING) ? CDR (x) : MAKE_NUMBER (CDR (x));
}

SCM
assert_defined (SCM x, SCM e) ///((internal))
{
  if (e != cell_undefined) return e;
  // error (cell_symbol_unbound_variable, x);
  puts ("unbound variable");
  exit (33);
  return e;
}

SCM
gc_push_frame () ///((internal))
{
  SCM frame = cons (r1, cons (r2, cons (r3, cons (r0, cell_nil))));
  g_stack = cons (frame, g_stack);
  return g_stack;
}

SCM
append2 (SCM x, SCM y)
{
  if (x == cell_nil) return y;
#if __GNUC__
  //FIXME GNUC
  assert (TYPE (x) == TPAIR);
#endif
  return cons (car (x), append2 (cdr (x), y));
}

SCM
pairlis (SCM x, SCM y, SCM a)
{
  if (x == cell_nil)
    return a;
  if (TYPE (x) != TPAIR)
    return cons (cons (x, y), a);
  return cons (cons (car (x), car (y)),
               pairlis (cdr (x), cdr (y), a));
}


#if __GNUC__
SCM display_ (SCM);
#endif

SCM
call (SCM fn, SCM x)
{
  if ((FUNCTION (fn).arity > 0 || FUNCTION (fn).arity == -1)
      && x != cell_nil && TYPE (CAR (x)) == TVALUES)
    x = cons (CADAR (x), CDR (x));
  if ((FUNCTION (fn).arity > 1 || FUNCTION (fn).arity == -1)
      && x != cell_nil && TYPE (CDR (x)) == TPAIR && TYPE (CADR (x)) == TVALUES)
    x = cons (CAR (x), cons (CDADAR (x), CDR (x)));
  switch (FUNCTION (fn).arity)
    {
    // case 0: return FUNCTION (fn).function0 ();
    // case 1: return FUNCTION (fn).function1 (car (x));
    // case 2: return FUNCTION (fn).function2 (car (x), cadr (x));
    // case 3: return FUNCTION (fn).function3 (car (x), cadr (x), car (cddr (x)));
    // case -1: return FUNCTION (fn).functionn (x);
    case 0: {return (FUNCTION (fn).function) ();}
    case 1: {return ((SCM(*)(SCM))(FUNCTION (fn).function)) (car (x));}
    case 2: {return ((SCM(*)(SCM,SCM))(FUNCTION (fn).function)) (car (x), cadr (x));}
    case 3: {return ((SCM(*)(SCM,SCM,SCM))(FUNCTION (fn).function)) (car (x), cadr (x), car (cddr (x)));}
#if __GNUC__
      // FIXME GNUC
    case -1: {return ((SCM(*)(SCM))(FUNCTION (fn).function)) (x);}
#endif
    default: {return ((SCM(*)(SCM))(FUNCTION (fn).function)) (x);}
    }

  return cell_unspecified;
}

SCM
assq (SCM x, SCM a)
{
  //while (a != cell_nil && eq_p (x, CAAR (a)) == cell_f) a = CDR (a);
  //while (a != cell_nil && x != CAAR (a)) a = CDR (a);

#if BDEBUG
  puts  ("assq: ");
  display_ (x);
  puts  (" [");
  puts (itoa (x));
  puts  ("]\n");
#endif
  int i;
  while (a != cell_nil) // && x != CAR (CAR (a)))
    {
      a = CDR (a);
      // FIXME
      i = CAR (CAR (a));
#if  1
      //!__GNUC__
      // puts ("  ");
      // puts (itoa (i));
      // if (x == i) puts ("***FOUND*** ");
      if (x == i) goto found;
      // puts ("  ");
      // display_ (CAAR (a));
      // puts  ("[");
      // puts (itoa (CAAR (a)));
      // puts  ("]\n");
#endif
    }
 found:
#if BDEBUG
  //!__GNUC__
  //puts  ("assq: ");
  puts  ("  ");
  puts  (" [");
  puts (itoa (x));
  puts  ("]");
  display_ (x);
  puts  (" => ");
  if (a == cell_nil) display_ (cell_f);
  else display_ (CAR (a));
  puts  ("[");
  puts (itoa (CDR (CDR (CAR (a)))));
  puts  ("]\n");
#endif
  return a != cell_nil ? car (a) : cell_f;
}

SCM
assq_ref_env (SCM x, SCM a)
{
  x = assq (x, a);
  if (x == cell_f) return cell_undefined;
  return cdr (x);
}

SCM
set_car_x (SCM x, SCM e)
{
  assert (TYPE (x) == TPAIR);
  CAR (x) = e;
  return cell_unspecified;
}

SCM
set_cdr_x (SCM x, SCM e)
{
  //if (TYPE (x) != TPAIR) error (cell_symbol_not_a_pair, cons (x, cell_set_cdr_x));
  CDR (x) = e;
  return cell_unspecified;
}

SCM
set_env_x (SCM x, SCM e, SCM a)
{
  SCM p = assert_defined (x, assq (x, a));
  //if (TYPE (p) != TPAIR)  error (cell_symbol_not_a_pair, cons (p, x));
  return set_cdr_x (p, e);
}

SCM
call_lambda (SCM e, SCM x, SCM aa, SCM a) ///((internal))
{
  //FIXME
  //SCM cl = cons (cons (cell_closure, x), x);
  SCM cl;
  cl = cons (cons (cell_closure, x), x);
  r1 = e;
  r0 = cl;
  return cell_unspecified;
}

SCM
make_closure (SCM args, SCM body, SCM a)
{
  return make_cell (tmp_num_ (TCLOSURE), cell_f, cons (cons (cell_circular, a), cons (args, body)));
}

SCM
lookup_macro (SCM x, SCM a)
{
  if (TYPE (x) != TSYMBOL) return cell_f;
  SCM m = assq_ref_env (x, a);
  if (TYPE (m) == TMACRO) return MACRO (m);
  return cell_f;
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

#if __GNUC__
//FIXME
SCM gc_pop_frame (); //((internal))
#endif

SCM
eval_apply ()
{
 eval_apply:
  // if (g_free + GC_SAFETY > ARENA_SIZE)
  //   gc_pop_frame (gc (gc_push_frame ()));

  switch (r3)
    {
    case cell_vm_evlis: goto evlis;
    case cell_vm_evlis2: goto evlis2;
    case cell_vm_evlis3: goto evlis3;
    case cell_vm_apply: goto apply;
    case cell_vm_apply2: goto apply2;
    case cell_vm_eval: goto eval;
#if FIXED_PRIMITIVES
    case cell_vm_eval_car: goto eval_car;
    case cell_vm_eval_cdr: goto eval_cdr;
    case cell_vm_eval_cons: goto eval_cons;
    case cell_vm_eval_null_p: goto eval_null_p;
#endif
    case cell_vm_eval_set_x: goto eval_set_x;
    case cell_vm_eval_macro: goto eval_macro;
    case cell_vm_eval2: goto eval2;
    case cell_vm_macro_expand: goto macro_expand;
    case cell_vm_begin: goto begin;
      ///case cell_vm_begin_read_input_file: goto begin_read_input_file;
    case cell_vm_begin2: goto begin2;
    case cell_vm_if: goto vm_if;
    case cell_vm_if_expr: goto if_expr;
    case cell_vm_call_with_current_continuation2: goto call_with_current_continuation2;
    case cell_vm_call_with_values2: goto call_with_values2;
    case cell_vm_return: goto vm_return;
    case cell_unspecified: return r1;
    default: assert (0);
    }

  SCM x = cell_nil;
  SCM y = cell_nil;
 evlis:
  if (r1 == cell_nil) goto vm_return;
  if (TYPE (r1) != TPAIR) goto eval;
  push_cc (car (r1), r1, r0, cell_vm_evlis2);
  goto eval;
 evlis2:
  push_cc (cdr (r2), r1, r0, cell_vm_evlis3);
  goto evlis;
 evlis3:
  r1 = cons (r2, r1);
  goto vm_return;

 apply:
  switch (TYPE (car (r1)))
    {
    case TFUNCTION: {
      //check_formals (car (r1), MAKE_NUMBER (FUNCTION (car (r1)).arity), cdr (r1));
      r1 = call (car (r1), cdr (r1)); /// FIXME: move into eval_apply
      goto vm_return;
    }
    case TCLOSURE:
      {
        //FIXME
        //SCM cl = CLOSURE (car (r1));
        SCM cl;
        cl = CLOSURE (car (r1));
        SCM formals = cadr (cl);
        SCM body = cddr (cl);
        SCM aa = cdar (cl);
        aa = cdr (aa);
        //check_formals (car (r1), formals, cdr (r1));
        SCM p = pairlis (formals, cdr (r1), aa);
        call_lambda (body, p, aa, r0);
        goto begin;
      }
      case TCONTINUATION:
        {
          x = r1;
          g_stack = CONTINUATION (CAR (r1));
          gc_pop_frame ();
          r1 = cadr (x);
          goto eval_apply;
        }
    case TSPECIAL:
      {
        switch (car (r1))
          {
          case cell_vm_apply:
            {
              push_cc (cons (CADR (r1), CADDR (r1)), r1, r0, cell_vm_return);
              goto apply;
            }
          case cell_vm_eval:
            {
              push_cc (CADR (r1), r1, CADDR (r1), cell_vm_return);
              goto eval;
            }
          case cell_call_with_current_continuation:
            {
              r1 = cdr (r1);
              goto call_with_current_continuation;
            }
            //default: check_apply (cell_f, car (r1));
          }
      }
    case TSYMBOL:
      {
        if (car (r1) == cell_symbol_call_with_values)
          {
            r1 = cdr (r1);
            goto call_with_values;
          }
        if (car (r1) == cell_symbol_current_module)
          {
            r1 = r0;
            goto vm_return;
          }
        break;
      }
    case TPAIR:
      {
        switch (caar (r1))
          {
          case cell_symbol_lambda:
            {
              SCM formals = cadr (car (r1));
              SCM body = cddr (car (r1));
              SCM p = pairlis (formals, cdr (r1), r0);
              //check_formals (r1, formals, cdr (r1));
              call_lambda (body, p, p, r0);
              goto begin;
            }
          }
      }
    }
  push_cc (car (r1), r1, r0, cell_vm_apply2);
  goto eval;
 apply2:
  //check_apply (r1, car (r2));
  r1 = cons (r1, cdr (r2));
  goto apply;

 eval:
  switch (TYPE (r1))
    {
    case TPAIR:
      {
        switch (car (r1))
          {
#if FIXED_PRIMITIVES
          case cell_symbol_car:
            {
              push_cc (CADR (r1), r1, r0, cell_vm_eval_car); goto eval;
            eval_car:
              x = r1; gc_pop_frame (); r1 = car (x); goto eval_apply;
            }
          case cell_symbol_cdr:
            {
              push_cc (CADR (r1), r1, r0, cell_vm_eval_cdr); goto eval;
            eval_cdr:
              x = r1; gc_pop_frame (); r1 = cdr (x); goto eval_apply;
            }
          case cell_symbol_cons: {
            push_cc (CDR (r1), r1, r0, cell_vm_eval_cons); goto evlis;
            eval_cons:
            x = r1;
            gc_pop_frame ();
            r1 = cons (CAR (x), CADR (x));
            goto eval_apply;
          }
          case cell_symbol_null_p:
            {
              push_cc (CADR (r1), r1, r0, cell_vm_eval_null_p);
              goto eval;
            eval_null_p:
              x = r1; gc_pop_frame (); r1 = null_p (x); goto eval_apply;
            }
#endif // FIXED_PRIMITIVES
          case cell_symbol_quote:
            {
              x = r1; gc_pop_frame (); r1 = cadr (x); goto eval_apply;
            }
          case cell_symbol_begin: goto begin;
          case cell_symbol_lambda:
            {
              r1 = make_closure (cadr (r1), cddr (r1), assq (cell_closure, r0));
              goto vm_return;
            }
          case cell_symbol_if: {r1=cdr (r1); goto vm_if;}
          case cell_symbol_set_x:
            {
              push_cc (car (cddr (r1)), r1, r0, cell_vm_eval_set_x);
              goto eval;
            eval_set_x:
              x = r2;
              r1 = set_env_x (cadr (x), r1, r0);
              goto vm_return;
            }
          case cell_vm_macro_expand:
            {
              push_cc (cadr (r1), r1, r0, cell_vm_return);
              goto macro_expand;
            }
          default: {
            push_cc (r1, r1, r0, cell_vm_eval_macro);
            goto macro_expand;
            eval_macro:
            x = r2;
            if (r1 != r2)
              {
                if (TYPE (r1) == TPAIR)
                  {
                    set_cdr_x (r2, cdr (r1));
                    set_car_x (r2, car (r1));
                  }
                goto eval;
              }
            push_cc (CDR (r1), r1, r0, cell_vm_eval2); goto evlis;
            eval2:
            r1 = cons (car (r2), r1);
            goto apply;
          }
          }
      }
    case TSYMBOL:
      {
        r1 = assert_defined (r1, assq_ref_env (r1, r0));
        goto vm_return;
      }
    default: {goto vm_return;}
    }

  SCM macro;
  SCM expanders;
 macro_expand:
#if __GNUC__
  //FIXME
  if (TYPE (r1) == TPAIR
      && (macro = lookup_macro (car (r1), r0)) != cell_f) // FIXME GNUC
    {
      r1 = cons (macro, CDR (r1));
      goto apply;
    }
  else if (TYPE (r1) == TPAIR
           && TYPE (CAR (r1)) == TSYMBOL
           && ((expanders = assq_ref_env (cell_symbol_sc_expander_alist, r0)) != cell_undefined)
           && ((macro = assq (CAR (r1), expanders)) != cell_f))
    {
      SCM sc_expand = assq_ref_env (cell_symbol_macro_expand, r0);
      if (sc_expand != cell_undefined && sc_expand != cell_f)
        {
          r1 = cons (sc_expand, cons (r1, cell_nil));
          goto apply;
        }
    }
#endif
  goto vm_return;
 begin:
  x = cell_unspecified;
  while (r1 != cell_nil) {
    if (TYPE (r1) == TPAIR && TYPE (CAR (r1)) == TPAIR)
      {
        if (caar (r1) == cell_symbol_begin)
          r1 = append2 (cdar (r1), cdr (r1));
        else if (caar (r1) == cell_symbol_primitive_load)
          {
            push_cc (cons (cell_symbol_read_input_file, cell_nil), r1, r0, cell_vm_begin_read_input_file);
            goto apply;
          begin_read_input_file:
            r1 = append2 (r1, cdr (r2));
          }
      }
    if (CDR (r1) == cell_nil)
      {
        r1 = car (r1);
        goto eval;
      }
    push_cc (CAR (r1), r1, r0, cell_vm_begin2);
    goto eval;
  begin2:
    x = r1;
    r1 = CDR (r2);
  }
  r1 = x;
  goto vm_return;

 vm_if:
  push_cc (car (r1), r1, r0, cell_vm_if_expr);
  goto eval;
 if_expr:
  x = r1;
  r1 = r2;
  if (x != cell_f)
    {
      r1 = cadr (r1);
      goto eval;
    }
  if (cddr (r1) != cell_nil)
    {
      r1 = car (cddr (r1));
      goto eval;
    }
  r1 = cell_unspecified;
  goto vm_return;

 call_with_current_continuation:
  gc_push_frame ();
#if __GNUC__
  // FIXME GCC
  x = MAKE_CONTINUATION (g_continuations++);
#else
  x = MAKE_CONTINUATION (g_continuations);
  g_continuations++;
#endif
  gc_pop_frame ();
  push_cc (cons (car (r1), cons (x, cell_nil)), x, r0, cell_vm_call_with_current_continuation2);
  goto apply;
 call_with_current_continuation2:
  CONTINUATION (r2) = g_stack;
  goto vm_return;

 call_with_values:
  push_cc (cons (car (r1), cell_nil), r1, r0, cell_vm_call_with_values2);
  goto apply;
 call_with_values2:
  if (TYPE (r1) == TVALUES)
    r1 = CDR (r1);
  r1 = cons (cadr (r2), r1);
  goto apply;

 vm_return:
  x = r1;
  gc_pop_frame ();
  r1 = x;
  goto eval_apply;
}

SCM
gc_peek_frame () ///((internal))
{
  SCM frame = car (g_stack);
  r1 = car (frame);
  r2 = cadr (frame);
  r3 = car (cddr (frame));
  r0 = cadr (cddr (frame));
  return frame;
}

SCM
gc_pop_frame () ///((internal))
{
  SCM frame = gc_peek_frame (g_stack);
  g_stack = cdr (g_stack);
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
make_tmps (struct scm* cells)
{
  tmp = g_free++;
  cells[tmp].type = TCHAR;
  tmp_num = g_free++;
  cells[tmp_num].type = TNUMBER;
  tmp_num2 = g_free++;
  cells[tmp_num2].type = TNUMBER;
  return 0;
}

SCM
make_symbol_ (SCM s)
{
  VALUE (tmp_num) = TSYMBOL;
  ///FIXMESCM x = make_cell (tmp_num, s, 0);
  SCM x;
  x = make_cell (tmp_num, s, 0);
  puts ("MAKE SYMBOL: ");
  // puts ("[s=");
  // puts (itoa (s));
  // puts (",s.car=");
  // puts (itoa (CAR (s)));
  // puts (",s.car.cdr=");
  // //  puts (itoa (CDR (CAR (s))));
  // putchar (CDR (CAR (s)));
  // puts (",x=");
  // puts (itoa (x));
  // puts (",x.car=");
  // puts (itoa (CAR (x)));
  // puts ("]");


  ////TYPE (x) = TSYMBOL;
  display_ (x);
  puts ("\n");
  g_symbols = cons (x, g_symbols);
  return x;
}

SCM
list_of_char_equal_p (SCM a, SCM b)
{
  while (a != cell_nil && b != cell_nil && VALUE (car (a)) == VALUE (car (b))) {
    assert (TYPE (car (a)) == TCHAR);
    assert (TYPE (car (b)) == TCHAR);
    a = cdr (a);
    b = cdr (b);
  }
  return (a == cell_nil && b == cell_nil) ? cell_t : cell_f;
}

SCM
lookup_symbol_ (SCM s)
{
  SCM x = g_symbols;
#if !MES_MINI
  while (x) {
    if (list_of_char_equal_p (STRING (car (x)), s) == cell_t) break;
    x = cdr (x);
  }
  if (x) x = car (x);
#endif;
  return x;
}

SCM
make_symbol (SCM s)
{
#if MES_MINI
  return make_symbol_ (s);
#else
  SCM x = lookup_symbol_ (s);
  // FIXME: does not work with mescc?!
  // return x != 0 ? x : make_symbol_ (s);
  return x ? x : make_symbol_ (s);
#endif
// FIXME
// #if MES_MINI
//   SCM x = 0;
// #else
//   SCM x = lookup_symbol_ (s);
// #endif
//   //FIXME
//   //return x ? x : make_symbol_ (s);
//   return x != 0 ? x : make_symbol_ (s);
}

SCM
cstring_to_list (char const* s)
{
  char *x = s;
  SCM p = cell_nil;
  int i = strlen (s);
  puts ("cstring_to_list[");
  puts (s);
  puts ("]: ");
  while (i--)
    {
#if 0
      //FIXME
      p = cons (MAKE_CHAR (s[i]), p);
#else
      char c;
      c = *x;
      puts ("[c:");
      putchar (c);
#if __GNUC__
      p = cons (MAKE_CHAR (c), p);
#else
      SCM xx;
      xx = MAKE_CHAR (c);
      //FIXME
      TYPE (xx) = 0;
      VALUE (xx) = c;
      puts (",t=");
      puts (itoa (TYPE (xx)));
      puts (",v=");
      putchar (VALUE (xx));
      puts ("]");
      p = cons (xx, p);
#endif
      x++;
#endif
    }
  puts ("\n");
  return p;
}

SCM
acons (SCM key, SCM value, SCM alist)
{
  return cons (cons (key, value), alist);
}


// MINI_MES: temp-lib

SCM
write_byte (SCM x) ///((arity . n))
{
  SCM c = car (x);
  SCM p = cdr (x);
  int fd = 1;
  if (TYPE (p) == TPAIR && TYPE (car (p)) == TNUMBER) fd = VALUE (car (p));
  //FILE *f = fd == 1 ? stdout : stderr;
  assert (TYPE (c) == TNUMBER || TYPE (c) == TCHAR);
  //  fputc (VALUE (c), f);
  // FIXME
  //char cc = VALUE (c);
  char cc;
  cc = VALUE (c);
  write (1, (char*)&cc, fd);
  return c;
}

SCM
display_ (SCM x)
{
  // eputs ("<display>\n");
  switch (TYPE (x))
    {
    case TCHAR:
      {
        //puts ("<char>\n");
        puts ("#\\");
        putchar (VALUE (x));
        break;
      }
    case TFUNCTION:
      {
#if 1
        puts ("#<procedure ");
        ///puts (FUNCTION (x).name ? FUNCTION (x).name : "?");
        char *p = "?";
        if (FUNCTION (x).name != 0)
          p = FUNCTION (x).name;
        puts (p);
        puts ("[");
        puts (itoa (CDR (x)));
        puts ("]>");
        break;
#endif
        // //puts ("<function>\n");
        // if (VALUE (x) == 0)
        //   puts ("make-cell");
        // if (VALUE (x) == 1)
        //   puts ("cons");
        // if (VALUE (x) == 2)
        //   puts ("car");
        // if (VALUE (x) == 3)
        //   puts ("cdr");
        // break;
      }
    case TNUMBER:
      {
        //puts ("<number>\n");
#if __GNUC__
        puts (itoa (VALUE (x)));
#else
        int i;
        i = VALUE (x);
        i = i + 48;
        putchar (i);
#endif
        break;
      }
    case TPAIR:
      {
        //puts ("<pair>\n");
        //if (cont != cell_f) puts "(");
        puts ("(");
        if (x && x != cell_nil) display_ (CAR (x));
        if (CDR (x) && CDR (x) != cell_nil)
          {
#if __GNUC__
            if (TYPE (CDR (x)) != TPAIR)
              puts (" . ");
#else
            int c;
            c = CDR (x);
            c = TYPE (c);
            if (c != TPAIR)
              puts (" . ");
#endif
            display_ (CDR (x));
          }
        //if (cont != cell_f) puts (")");
        puts (")");
        break;
      }
    case TSPECIAL:
      {
        switch (x)
          {
          case 1: {puts ("()"); break;}
          case 2: {puts ("#f"); break;}
          case 3: {puts ("#t"); break;}
          default:
            {
#if __GNUC__
        puts ("<x:");
        puts (itoa (x));
        puts (">");
#else
        puts ("<x>");
#endif
            }
          }
        break;
      }
    case TSYMBOL:
      {
#if 0
        puts ("<s:");
        puts (itoa (x));
        puts (">");
#endif
        // FIXME
        ///SCM t = CAR (x);
        SCM t;
        t = CAR (x);
        while (t != cell_nil)
          {
            //FIXME
            //SCM xx = CAR (t);
            // SCM xx;
            // xx = CAR (t);
            // puts ("[c:");
            // puts (itoa (xx));
            // puts (",");
            // puts (itoa (VALUE (xx)));
            // puts ("]");
            // putchar (VALUE (xx));
            putchar (VALUE (CAR (t)));
            t = CDR (t);
          }
        break;
      }
    default:
      {
        //puts ("<default>\n");
#if 1
        puts ("<");
        puts (itoa (TYPE (x)));
        puts (":");
        puts (itoa (x));
        puts (">");
#else
        puts ("_");
#endif
        break;
      }
    }
  return 0;
}


// Jam Collector
SCM g_symbol_max;

SCM
gc_init_cells () ///((internal))
{
  return 0;
//   g_cells = (scm *)malloc (2*ARENA_SIZE*sizeof(scm));

// #if __NYACC__ || FIXME_NYACC
//   TYPE (0) = TVECTOR;
// // #else
// //   TYPE (0) = VECTOR;
// #endif
//   LENGTH (0) = 1000;
//   VECTOR (0) = 0;
//   g_cells++;
//   TYPE (0) = CHAR;
//   VALUE (0) = 'c';
}

// INIT NEWS

SCM
mes_symbols () ///((internal))
{
  gc_init_cells ();
  //  gc_init_news ();

  #include "mini-mes.symbols.i"

  g_symbol_max = g_free;
  make_tmps (g_cells);

  g_symbols = 0;
  for (int i=1; i<g_symbol_max; i++)
    g_symbols = cons (i, g_symbols);

  SCM a = cell_nil;

  #include "mini-mes.symbol-names.i"

  // a = acons (cell_symbol_mes_version, MAKE_STRING (cstring_to_list (VERSION)), a);
  // a = acons (cell_symbol_mes_prefix, MAKE_STRING (cstring_to_list (PREFIX)), a);

  a = acons (cell_symbol_dot, cell_dot, a);
  a = acons (cell_symbol_begin, cell_begin, a);
  a = acons (cell_closure, a, a);

  // a = acons (cell_symbol_call_with_current_continuation, cell_call_with_current_continuation, a);
  // a = acons (cell_symbol_sc_expand, cell_f, a);

  return a;
}

SCM
mes_environment () ///((internal))
{
  SCM a = 0;
  a = mes_symbols ();
  a = mes_g_stack (a);
  return a;
}

SCM
mes_builtins (SCM a) ///((internal))
{
  #include "mini-mes.i"

// #include "lib.i"
// #include "math.i"
// #include "posix.i"
// #include "reader.i"

// #include "lib.environment.i"
// #include "math.environment.i"
  #include "mini-mes.environment.i"
// #include "posix.environment.i"
// #include "reader.environment.i"

  return a;
}

SCM
bload_env (SCM a) ///((internal))
{
  char *mo = "mini-0-32.mo";
  //char *mo = "module/mes/read-0-32.mo";
  g_stdin = open (mo, 0);
  if (g_stdin < 0) {eputs ("no such file: ");eputs (mo);eputs ("\n");return 1;} 
  assert (getchar () == 'M');
  assert (getchar () == 'E');
  assert (getchar () == 'S');
  eputs ("*GOT MES*\n");
  g_stack = getchar () << 8;
  g_stack += getchar ();

  char *p = (char*)g_cells;
  int c = getchar ();
  while (c != -1)
    {
      *p++ = c;
      c = getchar ();
    }
  g_free = (p-(char*)g_cells) / sizeof (struct scm);
  gc_peek_frame ();
  g_symbols = r1;
  g_stdin = STDIN;
  r0 = mes_builtins (r0);
#if 1
  //__GNUC__
  puts ("symbols: ");
  SCM s = g_symbols;
  while (s && s != cell_nil) {
    display_ (CAR (s));
    puts (" ");
    s = CDR (s);
  }
  puts ("\n");
  puts ("functions: ");
  puts (itoa (g_function));
  puts ("\n");
  for (int i = 0; i < g_function; i++)
    {
      puts ("[");
      puts (itoa (i));
      puts ("]: ");
      puts (g_functions[i].name);
      puts ("\n");
    }
  display_ (r0);
  puts ("\n");
#endif
  return r2;
}

char string_to_cstring_buf[1024];
char const*
string_to_cstring (SCM s)
{
  //static char buf[1024];
  //char *p = buf;
  char *p = string_to_cstring_buf;
  s = STRING(s);
  while (s != cell_nil)
    {
      *p++ = VALUE (car (s));
      s = cdr (s);
    }
  *p = 0;
  //return buf;
  return string_to_cstring_buf;
}

SCM
stderr_ (SCM x)
{
  //SCM write;
#if __NYACC__ || FIXME_NYACC
  if (TYPE (x) == TSTRING)
// #else
//   if (TYPE (x) == STRING)
#endif
    eputs (string_to_cstring (x));
  // else if ((write = assq_ref_env (cell_symbol_write, r0)) != cell_undefined)
  //   apply (assq_ref_env (cell_symbol_display, r0), cons (x, cons (MAKE_NUMBER (2), cell_nil)), r0);
#if __NYACC__ || FIXME_NYACC
  else if (TYPE (x) == TSPECIAL || TYPE (x) == TSTRING || TYPE (x) == TSYMBOL)
// #else
//   else if (TYPE (x) == SPECIAL || TYPE (x) == STRING || TYPE (x) == SYMBOL)
#endif
    eputs (string_to_cstring (x));
  else if (TYPE (x) == TNUMBER)
    eputs (itoa (VALUE (x)));
  else
    eputs ("core:stderr: display undefined\n");
  return cell_unspecified;
}

int
main (int argc, char *argv[])
{
  eputs ("Hello mini-mes!\n");

  // make_tmps (g_cells);
  // SCM x = cstring_to_list ("bla");
  // while (x != 1)
  //   {
  //     putchar (CDR (CAR (x)));
  //     x = CDR (x);
  //   }
  // return 0;

#if __GNUC__
  //g_debug = getenv ("MES_DEBUG");
#endif
  //if (getenv ("MES_ARENA")) ARENA_SIZE = atoi (getenv ("MES_ARENA"));
  if (argc > 1 && !strcmp (argv[1], "--help")) return eputs ("Usage: mes [--dump|--load] < FILE");
#if __GNUC__
  if (argc > 1 && !strcmp (argv[1], "--version")) {eputs ("Mes ");return eputs (VERSION);};
#else
  if (argc > 1 && !strcmp (argv[1], "--version")) {eputs ("Mes ");return eputs ("0.4");};
#endif
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
  eputs ("program: ");
  display_ (r1);
  eputs ("\n");
  r3 = cell_vm_begin;
  r1 = eval_apply ();
  display_ (r1);
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
