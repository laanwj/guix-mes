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

#if  __MESC__
char **g_environment;
int g_stdin = 0;
#define assert(x) ((x) ? (void)0 : assert_fail (#x))
#endif

#if !__MESC__
#include "mlibc.c"
#endif

int ARENA_SIZE = 100000;
int MAX_ARENA_SIZE = 20000000;
//int GC_SAFETY_DIV = 400;
//int GC_SAFETY = ARENA_SIZE / 400;
int GC_SAFETY = 250;

char *g_arena = 0;
typedef int SCM;

int g_debug = 0;
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

#if !_POSIX_SOURCE
struct scm {
  enum type_t type;
  SCM car;
  SCM cdr;
};
struct function {
  int (*function) (void);
  int arity;
  char *name;
};
#else
typedef SCM (*function0_t) (void);
typedef SCM (*function1_t) (SCM);
typedef SCM (*function2_t) (SCM, SCM);
typedef SCM (*function3_t) (SCM, SCM, SCM);
typedef SCM (*functionn_t) (SCM);
struct function {
  union {
    function0_t function0;
    function1_t function1;
    function2_t function2;
    function3_t function3;
    functionn_t functionn;
  };
  int arity;
  char const *name;
};
struct scm {
  enum type_t type;
  union {
    char const* name;
    SCM string;
    SCM car;
    SCM ref;
    int length;
  };
  union {
    int value;
    int function;
    SCM cdr;
    SCM closure;
    SCM continuation;
    SCM macro;
    SCM vector;
    int hits;
  };
};
#endif

#if __MESC__
//FIXME
char *foobar = 0;
struct scm *g_cells = foobar;
struct scm *g_news = foobar;
#else
struct scm *g_cells = 0;
struct scm *g_news = 0;
#endif

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
struct scm scm_vm_eval_check_func = {TSPECIAL, "*vm-eval-check-func*",0};
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

struct scm scm_symbol_gnuc = {TSYMBOL, "%gnuc",0};
struct scm scm_symbol_mesc = {TSYMBOL, "%mesc",0};

struct scm scm_test = {TSYMBOL, "test",0};

#if !_POSIX_SOURCE
#include "mes.mes.symbols.h"
#else
#include "mes.symbols.h"
#endif

SCM tmp;
SCM tmp_num;
SCM tmp_num2;

struct function g_functions[200];
int g_function = 0;

#if !__GNUC__ || !_POSIX_SOURCE
#include "gc.mes.h"
#include "lib.mes.h"
#include "math.mes.h"
#include "mes.mes.h"
#include "posix.mes.h"
#if MES_FULL
#include "reader.mes.h"
#endif
#include "vector.mes.h"
#else
#include "gc.h"
#include "lib.h"
#include "math.h"
#include "mes.h"
#include "posix.h"
#include "reader.h"
#include "vector.h"
#endif

#define TYPE(x) g_cells[x].type
#define CAR(x) g_cells[x].car
#define CDR(x) g_cells[x].cdr

#define NTYPE(x) g_news[x].type
#define NCAR(x) g_news[x].car
#define NCDR(x) g_news[x].cdr

#if !_POSIX_SOURCE
#define LENGTH(x) g_cells[x].car
#define REF(x) g_cells[x].car
#define STRING(x) g_cells[x].car

#define CLOSURE(x) g_cells[x].cdr
#define CONTINUATION(x) g_cells[x].cdr

#define FUNCTION(x) g_functions[g_cells[x].cdr]
#define MACRO(x) g_cells[x].cdr
#define VALUE(x) g_cells[x].cdr
#define VECTOR(x) g_cells[x].cdr

#define NLENGTH(x) g_news[x].car

#define NVALUE(x) g_news[x].cdr
#define NVECTOR(x) g_news[x].cdr

#else
#define CONTINUATION(x) g_cells[x].cdr
#define HITS(x) g_cells[x].hits
#define LENGTH(x) g_cells[x].length
#define NAME(x) g_cells[x].name
#define STRING(x) g_cells[x].string
#define CLOSURE(x) g_cells[x].closure
#define MACRO(x) g_cells[x].macro
#define REF(x) g_cells[x].ref
#define VALUE(x) g_cells[x].value
#define VECTOR(x) g_cells[x].vector
#define FUNCTION(x) g_functions[g_cells[x].function]

#define NLENGTH(x) g_news[x].length

#define NVALUE(x) g_news[x].value
#define NVECTOR(x) g_news[x].vector
#endif

#define MAKE_CHAR(n) make_cell_ (tmp_num_ (TCHAR), 0, tmp_num2_ (n))
#define MAKE_CONTINUATION(n) make_cell_ (tmp_num_ (TCONTINUATION), n, g_stack)
#define MAKE_NUMBER(n) make_cell_ (tmp_num_ (TNUMBER), 0, tmp_num2_ (n))
#define MAKE_REF(n) make_cell_ (tmp_num_ (TREF), n, 0)
#define MAKE_STRING(x) make_cell_ (tmp_num_ (TSTRING), x, 0)

#define CAAR(x) CAR (CAR (x))
#define CADR(x) CAR (CDR (x))
#define CDAR(x) CDR (CAR (x))
#define CDDR(x) CDR (CDR (x))
#define CADAR(x) CAR (CDR (CAR (x)))
#define CADDR(x) CAR (CDR (CDR (x)))
#define CDADAR(x) CAR (CDR (CAR (CDR (x))))

SCM
alloc (int n)
{
  assert (g_free + n < ARENA_SIZE);
  SCM x = g_free;
  g_free += n;
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
make_cell_ (SCM type, SCM car, SCM cdr)
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
make_symbol_ (SCM s) ///((internal))
{
  VALUE (tmp_num) = TSYMBOL;
  SCM x = make_cell_ (tmp_num, s, 0);
  g_symbols = cons (x, g_symbols);
  return x;
}

SCM
list_of_char_equal_p (SCM a, SCM b) ///((internal))
{
  while (a != cell_nil && b != cell_nil && VALUE (CAR (a)) == VALUE (CAR (b))) {
    assert (TYPE (CAR (a)) == TCHAR);
    assert (TYPE (CAR (b)) == TCHAR);
    a = CDR (a);
    b = CDR (b);
  }
  return (a == cell_nil && b == cell_nil) ? cell_t : cell_f;
}

SCM
lookup_symbol_ (SCM s)
{
  SCM x = g_symbols;
  while (x) {
    if (list_of_char_equal_p (STRING (CAR (x)), s) == cell_t) break;
    x = CDR (x);
  }
  if (x) x = CAR (x);
  if (!x) x = make_symbol_ (s);
  return x;
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
arity_ (SCM x)
{
  assert (TYPE (x) == TFUNCTION);
  return MAKE_NUMBER (FUNCTION (x).arity);
}

SCM
cons (SCM x, SCM y)
{
  VALUE (tmp_num) = TPAIR;
  return make_cell_ (tmp_num, x, y);
}

SCM
car (SCM x)
{
#if !__MESC_MES__
  if (TYPE (x) != TPAIR) error (cell_symbol_not_a_pair, cons (x, cell_symbol_car));
#endif
  return CAR (x);
}

SCM
cdr (SCM x)
{
#if !__MESC_MES__
  if (TYPE (x) != TPAIR) error (cell_symbol_not_a_pair, cons (x, cell_symbol_cdr));
#endif
  return CDR (x);
}

SCM
list (SCM x) ///((arity . n))
{
  return x;
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
values (SCM x) ///((arity . n))
{
  SCM v = cons (0, x);
  TYPE (v) = TVALUES;
  return v;
}

SCM
acons (SCM key, SCM value, SCM alist)
{
  return cons (cons (key, value), alist);
}

SCM
length (SCM x)
{
  int n = 0;
  while (x != cell_nil)
    {
      n++;
      if (TYPE (x) != TPAIR) return MAKE_NUMBER (-1);
      x = CDR (x);
    }
  return MAKE_NUMBER (n);
}

SCM apply (SCM, SCM, SCM);

SCM
error (SCM key, SCM x)
{
#if !__MESC_MES__
  SCM throw;
  if ((throw = assq_ref_env (cell_symbol_throw, r0)) != cell_undefined)
    return apply (throw, cons (key, cons (x, cell_nil)), r0);
#endif
  display_error_ (key);
  eputs (": ");
  display_error_ (x);
  eputs ("\n");
  exit (1);
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

//  extra lib
SCM
assert_defined (SCM x, SCM e) ///((internal))
{
  if (e == cell_undefined) return error (cell_symbol_unbound_variable, x);
  return e;
}

SCM
check_formals (SCM f, SCM formals, SCM args) ///((internal))
{
  int flen = (TYPE (formals) == TNUMBER) ? VALUE (formals) : VALUE (length (formals));
  int alen = VALUE (length (args));
  if (alen != flen && alen != -1 && flen != -1)
    {
      char *s = "apply: wrong number of arguments; expected: ";
      eputs (s);
      eputs (itoa (flen));
      eputs (", got: ");
      eputs (itoa (alen));
      eputs ("\n");
      display_error_ (f);
      SCM e = MAKE_STRING (cstring_to_list (s));
      return error (cell_symbol_wrong_number_of_args, cons (e, f));
    }
  return cell_unspecified;
}

SCM
check_apply (SCM f, SCM e) ///((internal))
{
  char* type = 0;
  if (f == cell_f || f == cell_t) type = "bool";
  if (f == cell_nil) type = "nil";
  if (f == cell_unspecified) type = "*unspecified*";
  if (f == cell_undefined) type = "*undefined*";
  if (TYPE (f) == TCHAR) type = "char";
  if (TYPE (f) == TNUMBER) type = "number";
  if (TYPE (f) == TSTRING) type = "string";

  if (type)
    {
      char *s = "cannot apply: ";
      eputs (s);
      eputs (type);
      eputs ("[");
      display_error_ (e);
      eputs ("]\n");
      SCM e = MAKE_STRING (cstring_to_list (s));
      return error (cell_symbol_wrong_type_arg, cons (e, f));
    }
  return cell_unspecified;
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
  assert (TYPE (x) == TPAIR);
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
#if __MESC__ || !_POSIX_SOURCE
    case 0: return (FUNCTION (fn).function) ();
    case 1: return ((SCM(*)(SCM))(FUNCTION (fn).function)) (CAR (x));
    case 2: return ((SCM(*)(SCM,SCM))(FUNCTION (fn).function)) (CAR (x), CADR (x));
    case 3: return ((SCM(*)(SCM,SCM,SCM))(FUNCTION (fn).function)) (CAR (x), CADR (x), CAR (CDDR (x)));
    case -1: return ((SCM(*)(SCM))(FUNCTION (fn).function)) (x);
    default: return ((SCM(*)(SCM))(FUNCTION (fn).function)) (x);
#else
    case 0: return FUNCTION (fn).function0 ();
    case 1: return FUNCTION (fn).function1 (CAR (x));
    case 2: return FUNCTION (fn).function2 (CAR (x), CADR (x));
    case 3: return FUNCTION (fn).function3 (CAR (x), CADR (x), CAR (CDDR (x)));
    case -1: return FUNCTION (fn).functionn (x);
#endif
    }

  return cell_unspecified;
}

SCM
assq (SCM x, SCM a)
{
  //FIXME: move into fast-non eq_p-ing assq core:assq?
  //while (a != cell_nil && x != CAAR (a)) a = CDR (a);
  while (a != cell_nil && eq_p (x, CAAR (a)) == cell_f) a = CDR (a);
  return a != cell_nil ? CAR (a) : cell_f;
}

SCM
assq_ref_env (SCM x, SCM a)
{
  x = assq (x, a);
  if (x == cell_f) return cell_undefined;
  return CDR (x);
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
  if (TYPE (x) != TPAIR) error (cell_symbol_not_a_pair, cons (x, cell_set_cdr_x));
  CDR (x) = e;
  return cell_unspecified;
}

SCM
set_env_x (SCM x, SCM e, SCM a)
{
  SCM p = assert_defined (x, assq (x, a));
  if (TYPE (p) != TPAIR) error (cell_symbol_not_a_pair, cons (p, x));
  return set_cdr_x (p, e);
}

SCM
call_lambda (SCM e, SCM x, SCM aa, SCM a) ///((internal))
{
  SCM cl = cons (cons (cell_closure, x), x);
  r1 = e;
  r0 = cl;
  return cell_unspecified;
}

SCM
make_closure_ (SCM args, SCM body, SCM a) ///((internal))
{
  return make_cell_ (tmp_num_ (TCLOSURE), cell_f, cons (cons (cell_circular, a), cons (args, body)));
}

SCM
lookup_macro_ (SCM x, SCM a) ///((internal))
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

SCM
gc_peek_frame () ///((internal))
{
  SCM frame = CAR (g_stack);
  r1 = CAR (frame);
  r2 = CADR (frame);
  r3 = CAR (CDDR (frame));
  r0 = CADR (CDDR (frame));
  return frame;
}

SCM
gc_pop_frame () ///((internal))
{
  SCM frame = gc_peek_frame (g_stack);
  g_stack = CDR (g_stack);
  return frame;
}

SCM
eval_apply ()
{
 eval_apply:
  gc_check ();
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
    case cell_vm_eval_check_func: goto eval_check_func;
    case cell_vm_eval2: goto eval2;
    case cell_vm_macro_expand: goto macro_expand;
    case cell_vm_begin: goto begin;
    case cell_vm_begin_read_input_file: goto begin_read_input_file;
    case cell_vm_begin2: goto begin2;
    case cell_vm_if: goto vm_if;
    case cell_vm_if_expr: goto if_expr;
    case cell_vm_call_with_current_continuation2: goto call_with_current_continuation2;
    case cell_vm_call_with_values2: goto call_with_values2;
    case cell_vm_return: goto vm_return;
    case cell_unspecified: return r1;
    default:
      assert (0);
    }

  SCM x = cell_nil;
 evlis:
  gc_check ();
  if (r1 == cell_nil) goto vm_return;
  if (TYPE (r1) != TPAIR) goto eval;
  push_cc (CAR (r1), r1, r0, cell_vm_evlis2);
  goto eval;
 evlis2:
  push_cc (CDR (r2), r1, r0, cell_vm_evlis3);
  goto evlis;
 evlis3:
  r1 = cons (r2, r1);
  goto vm_return;

 apply:
  gc_check ();
  switch (TYPE (CAR (r1)))
    {
    case TFUNCTION: {
      check_formals (CAR (r1), MAKE_NUMBER (FUNCTION (CAR (r1)).arity), CDR (r1));
      r1 = call (CAR (r1), CDR (r1)); /// FIXME: move into eval_apply
      goto vm_return;
    }
    case TCLOSURE:
      {
        SCM cl = CLOSURE (CAR (r1));
        SCM formals = CADR (cl);
        SCM body = CDDR (cl);
        SCM aa = CDAR (cl);
        aa = CDR (aa);
        check_formals (CAR (r1), formals, CDR (r1));
        SCM p = pairlis (formals, CDR (r1), aa);
        call_lambda (body, p, aa, r0);
        goto begin;
      }
      case TCONTINUATION:
        {
          x = r1;
          g_stack = CONTINUATION (CAR (r1));
          gc_pop_frame ();
          r1 = CADR (x);
          goto eval_apply;
        }
    case TSPECIAL:
      {
        switch (CAR (r1))
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
              r1 = CDR (r1);
              goto call_with_current_continuation;
            }
          default: check_apply (cell_f, CAR (r1));
          }
      }
    case TSYMBOL:
      {
        if (CAR (r1) == cell_symbol_call_with_values)
          {
            r1 = CDR (r1);
            goto call_with_values;
          }
        if (CAR (r1) == cell_symbol_current_module)
          {
            r1 = r0;
            goto vm_return;
          }
        break;
      }
    case TPAIR:
      {
        switch (CAAR (r1))
          {
          case cell_symbol_lambda:
            {
              SCM formals = CADR (CAR (r1));
              SCM body = CDDR (CAR (r1));
              SCM p = pairlis (formals, CDR (r1), r0);
              check_formals (r1, formals, CDR (r1));
              call_lambda (body, p, p, r0);
              goto begin;
            }
          }
      }
    }
  push_cc (CAR (r1), r1, r0, cell_vm_apply2);
  goto eval;
 apply2:
  check_apply (r1, CAR (r2));
  r1 = cons (r1, CDR (r2));
  goto apply;

 eval:
  gc_check ();
  switch (TYPE (r1))
    {
    case TPAIR:
      {
        switch (CAR (r1))
          {
#if FIXED_PRIMITIVES
          case cell_symbol_car:
            {
              push_cc (CADR (r1), r1, r0, cell_vm_eval_car); goto eval;
            eval_car:
              x = r1; gc_pop_frame (); r1 = CAR (x); goto eval_apply;
            }
          case cell_symbol_cdr:
            {
              push_cc (CADR (r1), r1, r0, cell_vm_eval_cdr); goto eval;
            eval_cdr:
              x = r1; gc_pop_frame (); r1 = CDR (x); goto eval_apply;
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
              x = r1; gc_pop_frame (); r1 = CADR (x); goto eval_apply;
            }
          case cell_symbol_begin: goto begin;
          case cell_symbol_lambda:
            {
              r1 = make_closure_ (CADR (r1), CDDR (r1), assq (cell_closure, r0));
              goto vm_return;
            }
          case cell_symbol_if: {r1=CDR (r1); goto vm_if;}
          case cell_symbol_set_x:
            {
              push_cc (CAR (CDDR (r1)), r1, r0, cell_vm_eval_set_x);
              goto eval;
            eval_set_x:
              x = r2;
              r1 = set_env_x (CADR (x), r1, r0);
              goto vm_return;
            }
          case cell_vm_macro_expand:
            {
              push_cc (CADR (r1), r1, r0, cell_vm_return);
              goto macro_expand;
            }
          default: {
            push_cc (r1, r1, r0, cell_vm_eval_macro);
            goto macro_expand;
            eval_macro:
            if (r1 != r2)
              {
                if (TYPE (r1) == TPAIR)
                  {
                    set_cdr_x (r2, CDR (r1));
                    set_car_x (r2, CAR (r1));
                  }
                goto eval;
              }
            push_cc (CAR (r1), r1, r0, cell_vm_eval_check_func); goto eval;
            eval_check_func:
            push_cc (CDR (r2), r2, r0, cell_vm_eval2); goto evlis;
            eval2:
            r1 = cons (CAR (r2), r1);
            goto apply;
          }
          }
      }
    case TSYMBOL:
      {
        r1 = assert_defined (r1, assq_ref_env (r1, r0));
        goto vm_return;
      }
    default: goto vm_return;
    }

  SCM macro;
  SCM expanders;
 macro_expand:
  if (TYPE (r1) == TPAIR
      && (macro = lookup_macro_ (CAR (r1), r0)) != cell_f)
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
  goto vm_return;

 begin:
  x = cell_unspecified;
  while (r1 != cell_nil) {
    gc_check ();
    if (TYPE (r1) == TPAIR && TYPE (CAR (r1)) == TPAIR)
      {
        if (CAAR (r1) == cell_symbol_begin)
          r1 = append2 (CDAR (r1), CDR (r1));
        else if (CAAR (r1) == cell_symbol_primitive_load)
          {
            push_cc (cons (cell_symbol_read_input_file, cell_nil), r1, r0, cell_vm_begin_read_input_file);
            goto apply;
          begin_read_input_file:
            r1 = append2 (r1, CDR (r2));
          }
      }
    if (CDR (r1) == cell_nil)
      {
        r1 = CAR (r1);
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
  push_cc (CAR (r1), r1, r0, cell_vm_if_expr);
  goto eval;
 if_expr:
  x = r1;
  r1 = r2;
  if (x != cell_f)
    {
      r1 = CADR (r1);
      goto eval;
    }
  if (CDDR (r1) != cell_nil)
    {
      r1 = CAR (CDDR (r1));
      goto eval;
    }
  r1 = cell_unspecified;
  goto vm_return;

 call_with_current_continuation:
  gc_push_frame ();
  x = MAKE_CONTINUATION (g_continuations++);
  gc_pop_frame ();
  push_cc (cons (CAR (r1), cons (x, cell_nil)), x, r0, cell_vm_call_with_current_continuation2);
  goto apply;
 call_with_current_continuation2:
  CONTINUATION (r2) = g_stack;
  goto vm_return;

 call_with_values:
  push_cc (cons (CAR (r1), cell_nil), r1, r0, cell_vm_call_with_values2);
  goto apply;
 call_with_values2:
  if (TYPE (r1) == TVALUES)
    r1 = CDR (r1);
  r1 = cons (CADR (r2), r1);
  goto apply;

 vm_return:
  x = r1;
  gc_pop_frame ();
  r1 = x;
  goto eval_apply;
}

SCM
apply (SCM f, SCM x, SCM a) ///((internal))
{
  push_cc (cons (f, x), cell_unspecified, r0, cell_unspecified);
  r3 = cell_vm_apply;
  return eval_apply ();
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

#include "posix.c"
#include "math.c"
#include "lib.c"

// Jam Collector
SCM g_symbol_max;

SCM
gc_init_cells () ///((internal))
{
  g_cells = (struct scm *)malloc (2*ARENA_SIZE*sizeof (struct scm));

  TYPE (0) = TVECTOR;
  LENGTH (0) = 1000;
  VECTOR (0) = 0;
#if __MESC__
  g_cells += sizeof (struct scm);
#else
  g_cells++;
#endif
  TYPE (0) = TCHAR;
  VALUE (0) = 'c';
  return 0;
}

SCM
gc_init_news () ///((internal))
{
#if __MESC__
  char *p = g_cells;
  p -= sizeof (struct scm);
  p += ARENA_SIZE * sizeof (struct scm);
  g_news = p;
#else
  g_news = g_cells-1 + ARENA_SIZE;
#endif

  NTYPE (0) = TVECTOR;
  NLENGTH (0) = 1000;
  NVECTOR (0) = 0;
#if __MESC__
  g_news += sizeof (struct scm);
#else
  g_news++;
#endif
  NTYPE (0) = TCHAR;
  NVALUE (0) = 'n';
  return 0;
}

SCM
mes_symbols () ///((internal))
{
  gc_init_cells ();
  gc_init_news ();

#if !_POSIX_SOURCE
#include "mes.mes.symbols.i"
#else
#include "mes.symbols.i"
#endif

  g_symbol_max = g_free;
  make_tmps (g_cells);

  g_symbols = 0;
  for (int i=1; i<g_symbol_max; i++)
    g_symbols = cons (i, g_symbols);

  SCM a = cell_nil;

#if !_POSIX_SOURCE
#include "mes.mes.symbol-names.i"
#else
#include "mes.symbol-names.i"
#endif

  a = acons (cell_symbol_mes_version, MAKE_STRING (cstring_to_list (VERSION)), a);
  a = acons (cell_symbol_mes_prefix, MAKE_STRING (cstring_to_list (PREFIX)), a);

  a = acons (cell_symbol_dot, cell_dot, a);
  a = acons (cell_symbol_begin, cell_begin, a);
  a = acons (cell_symbol_call_with_values, cell_symbol_call_with_values, a);
  a = acons (cell_symbol_current_module, cell_symbol_current_module, a);
  a = acons (cell_symbol_call_with_current_continuation, cell_call_with_current_continuation, a);
  a = acons (cell_symbol_sc_expand, cell_f, a);

#if __GNUC__
  a = acons (cell_symbol_gnuc, cell_t, a);
  a = acons (cell_symbol_mesc, cell_f, a);
#else
  a = acons (cell_symbol_gnuc, cell_f, a);
  a = acons (cell_symbol_mesc, cell_t, a);
#endif

  a = acons (cell_closure, a, a);

  return a;
}

SCM
mes_environment () ///((internal))
{
  SCM a = mes_symbols ();
  return mes_g_stack (a);
}

SCM
mes_builtins (SCM a) ///((internal))
{
#if !__GNUC__ || !_POSIX_SOURCE
#include "mes.mes.i"

// Do not sort: Order of these includes define builtins
#include "posix.mes.i"
#include "math.mes.i"
#include "lib.mes.i"
#include "vector.mes.i"
#include "gc.mes.i"
#if MES_FULL
#include "reader.mes.i"
#endif

#include "gc.mes.environment.i"
#include "lib.mes.environment.i"
#include "math.mes.environment.i"
#include "mes.mes.environment.i"
#include "posix.mes.environment.i"
#if MES_FULL
#include "reader.mes.environment.i"
#endif
#include "vector.mes.environment.i"
#else
#include "mes.i"

// Do not sort: Order of these includes define builtins
#include "posix.i"
#include "math.i"
#include "lib.i"
#include "vector.i"
#include "gc.i"
#include "reader.i"

#include "gc.environment.i"
#include "lib.environment.i"
#include "math.environment.i"
#include "mes.environment.i"
#include "posix.environment.i"
#include "reader.environment.i"
#include "vector.environment.i"
#endif

  if (g_debug)
    {
      fputs ("functions: ", STDERR);
      fputs (itoa (g_function), STDERR);
      fputs ("\n", STDERR);
      for (int i = 0; i < g_function; i++)
        {
          fputs ("[", STDERR);
          fputs (itoa (i), STDERR);
          fputs ("]: ", STDERR);
          fputs (g_functions[i].name, STDERR);
          fputs ("\n", STDERR);
        }
      fputs ("\n", STDERR);
    }

  return a;
}

SCM
load_env (SCM a) ///((internal))
{
  r0 = a;
  g_stdin = open ("module/mes/read-0.mes", O_RDONLY);
  g_stdin = g_stdin >= 0 ? g_stdin : open (MODULEDIR "mes/read-0.mes", O_RDONLY);
  if (!g_function) r0 = mes_builtins (r0);
  r2 = read_input_file_env (r0);
  g_stdin = STDIN;
  return r2;
}

SCM
bload_env (SCM a) ///((internal))
{
#if __MESC__
  char *mo = "mes/read-0-32.mo";
  g_stdin = open ("module/mes/read-0-32.mo", O_RDONLY);
  g_stdin = g_stdin >= 0 ? g_stdin : open (MODULEDIR "mes/read-0-32.mo", O_RDONLY);
#else
  char *mo ="mes/read-0.mo";
  g_stdin = open ("module/mes/read-0.mo", O_RDONLY);
  g_stdin = g_stdin >= 0 ? g_stdin : open (MODULEDIR "mes/read-0.mo", O_RDONLY);
#endif

  if (g_stdin < 0) {eputs ("no such file: ");eputs (mo);eputs ("\n");return 1;} 
  assert (getchar () == 'M');
  assert (getchar () == 'E');
  assert (getchar () == 'S');
  eputs ("*GOT MES*\n");
  g_stack = getchar () << 8;
  g_stack += getchar ();

  char *p = (char*)g_cells;
  int c = getchar ();
  while (c != EOF)
    {
      *p++ = c;
      c = getchar ();
    }
  g_free = (p-(char*)g_cells) / sizeof (struct scm);
  gc_peek_frame ();
  g_symbols = r1;
  g_stdin = STDIN;
  r0 = mes_builtins (r0);

#if __GNUC__
  set_env_x (cell_symbol_gnuc, cell_t, r0);
  set_env_x (cell_symbol_mesc, cell_f, r0);
#else
  set_env_x (cell_symbol_gnuc, cell_f, r0);
  set_env_x (cell_symbol_mesc, cell_t, r0);
#endif

  if (g_debug)
    {
      eputs ("symbols: ");
      SCM s = g_symbols;
      while (s && s != cell_nil) {
        display_error_ (CAR (s));
        eputs (" ");
        s = CDR (s);
      }
      eputs ("\n");
      eputs ("functions: ");
      eputs (itoa (g_function));
      eputs ("\n");
      for (int i = 0; i < g_function; i++)
        {
          eputs ("[");
          eputs (itoa (i));
          eputs ("]: ");
          eputs (g_functions[i].name);
          eputs ("\n");
        }
      //display_error_ (r0);
      //puts ("\n");
    }
  return r2;
}

#include "vector.c"
#include "gc.c"
#if _POSIX_SOURCE || MES_FULL
#include "reader.c"
#endif

int
main (int argc, char *argv[])
{
#if __GNUC__
  g_debug = getenv ("MES_DEBUG") != 0;
  if (g_debug) {eputs ("MODULEDIR=");eputs (MODULEDIR);eputs ("\n");}
#endif
#if _POSIX_SOURCE
  if (getenv ("MES_MAX_ARENA")) MAX_ARENA_SIZE = atoi (getenv ("MES_MAX_ARENA"));
  if (getenv ("MES_ARENA")) ARENA_SIZE = atoi (getenv ("MES_ARENA"));
#endif
  if (argc > 1 && !strcmp (argv[1], "--help")) return puts ("Usage: mes [--dump|--load] < FILE");
  if (argc > 1 && !strcmp (argv[1], "--version")) {puts ("Mes ");puts (VERSION);return 0;};
  g_stdin = STDIN;
  r0 = mes_environment ();

#if __MESC__
  SCM program = bload_env (r0);
  g_debug = 1;
#else
  SCM program = (argc > 1 && !strcmp (argv[1], "--load"))
    ? bload_env (r0) : load_env (r0);
  g_tiny = argc > 2 && !strcmp (argv[2], "--tiny");
  if (argc > 1 && !strcmp (argv[1], "--dump")) return dump ();
#endif

  SCM lst = cell_nil;
#if !__MESC__
  for (int i=argc-1; i>=0; i--) lst = cons (MAKE_STRING (cstring_to_list (argv[i])), lst);
#endif
  r0 = acons (cell_symbol_argv, lst, r0);
  push_cc (r2, cell_unspecified, r0, cell_unspecified);
  if (g_debug)
    {
      eputs ("program: ");
      display_error_ (r1);
      eputs ("\n");
    }
  r3 = cell_vm_begin;
  r1 = eval_apply ();
  display_error_ (r1);
  eputs ("\n");
  gc (g_stack);
  if (g_debug)
    {
      eputs ("\nstats: [");
      eputs (itoa (g_free));
      eputs ("]\n");
    }
  return 0;
}

#if !_POSIX_SOURCE && !__MESC__
#include "mstart.c"
#endif
