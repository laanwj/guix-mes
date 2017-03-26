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

#define _GNU_SOURCE
#if __GNUC__
#define  __NYACC__ 0
#define NYACC
#define NYACC2
#include <assert.h>
#include <ctype.h>
#include <errno.h>
#include <limits.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <stdbool.h>
#else
typedef int bool;
#define  __NYACC__ 1
#define NYACC nyacc
#define NYACC2 nyacc2
#endif

#define DEBUG 0
#define FIXED_PRIMITIVES 1

int ARENA_SIZE = 100000;
int MAX_ARENA_SIZE = 20000000;
int GC_SAFETY = 100;

typedef int SCM;
enum type_t {TCHAR, TCLOSURE, TCONTINUATION, TFUNCTION, TKEYWORD, TMACRO, TNUMBER, TPAIR, TREF, TSPECIAL, TSTRING, TSYMBOL, TVALUES, TVECTOR, TBROKEN_HEART};
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
  } NYACC;
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
};

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

struct scm scm_symbol_gnuc = {TSYMBOL, "%gnuc",0};
struct scm scm_symbol_mesc = {TSYMBOL, "%mesc",0};

struct scm scm_test = {TSYMBOL, "test",0};


int g_free = 0;
struct scm *g_cells;
struct scm *g_news = 0;
SCM g_symbol_max;
bool g_debug = false;

#include "mes.symbols.h"

SCM tmp;
SCM tmp_num;
SCM tmp_num2;

struct function g_functions[200];
int g_function = 0;

SCM g_continuations = 0;
SCM g_symbols = 0;
SCM g_stack = 0;
SCM r0 = 0; // a/env
SCM r1 = 0; // param 1
SCM r2 = 0; // save 2+load/dump
SCM r3 = 0; // continuation

#include "gc.h"
#include "lib.h"
#include "math.h"
#include "mes.h"
#include "posix.h"
#include "reader.h"
#include "vector.h"

#define CAR(x) g_cells[x].car
#define CDR(x) g_cells[x].cdr
#define CONTINUATION(x) g_cells[x].cdr
#define HITS(x) g_cells[x].hits
#define LENGTH(x) g_cells[x].length
#define NAME(x) g_cells[x].name
#define STRING(x) g_cells[x].string
#define TYPE(x) g_cells[x].type
#define CLOSURE(x) g_cells[x].closure
#define MACRO(x) g_cells[x].macro
#define REF(x) g_cells[x].ref
#define VALUE(x) g_cells[x].value
#define VECTOR(x) g_cells[x].vector
#define FUNCTION(x) g_functions[g_cells[x].function]
#define NCAR(x) g_news[x].car
#define NTYPE(x) g_news[x].type

#define CAAR(x) CAR (CAR (x))
#define CDAR(x) CDR (CAR (x))
#define CAAR(x) CAR (CAR (x))
#define CADAR(x) CAR (CDR (CAR (x)))
#define CADDR(x) CAR (CDR (CDR (x)))
#define CDDDR(x) CDR (CDR (CDR (x)))
#define CDADAR(x) CAR (CDR (CAR (CDR (x))))
#define CADR(x) CAR (CDR (x))

#define MAKE_CHAR(n) make_cell (tmp_num_ (TCHAR), 0, tmp_num2_ (n))
#define MAKE_CONTINUATION(n) make_cell (tmp_num_ (TCONTINUATION), n, g_stack)
#define MAKE_NUMBER(n) make_cell (tmp_num_ (TNUMBER), 0, tmp_num2_ (n))
#define MAKE_REF(n) make_cell (tmp_num_ (TREF), n, 0)
#define MAKE_STRING(x) make_cell (tmp_num_ (TSTRING), x, 0)

SCM vm_call (function0_t f, SCM p1, SCM a);
char const* itoa(int);

#define eputs(s) fputs(s, stderr)

SCM
tmp_num_ (int x)
{
  g_cells[tmp_num].value = x;
  return tmp_num;
}

SCM
tmp_num2_ (int x)
{
  g_cells[tmp_num2].value = x;
  return tmp_num2;
}

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
  assert (TYPE (type) == TNUMBER);
  TYPE (x) = VALUE (type);
  if (VALUE (type) == TCHAR || VALUE (type) == TNUMBER) {
    if (car) CAR (x) = CAR (car);
    if (cdr) CDR (x) = CDR (cdr);
  } else if (VALUE (type) == TFUNCTION) {
    if (car) CAR (x) = car;
    if (cdr) CDR (x) = CDR (cdr);
  } else {
    CAR (x) = car;
    CDR (x) = cdr;
  }
  return x;
}

SCM
cons (SCM x, SCM y)
{
  g_cells[tmp_num].value = TPAIR;
  return make_cell (tmp_num, x, y);
}

SCM
car (SCM x)
{
  if (TYPE (x) != TPAIR) error (cell_symbol_not_a_pair, cons (x, cell_symbol_car));
  return CAR (x);
}

SCM
cdr (SCM x)
{
  if (TYPE (x) != TPAIR) error (cell_symbol_not_a_pair, cons (x, cell_symbol_cdr));
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

// MIMI_MES lib.c?
SCM
length (SCM x)
{
  int n = 0;
  while (x != cell_nil)
    {
      n++;
      if (TYPE (x) != TPAIR) return MAKE_NUMBER (-1);
      x = cdr (x);
    }
  return MAKE_NUMBER (n);
}

SCM
error (SCM key, SCM x)
{
  SCM throw;
  if ((throw = assq_ref_env (cell_symbol_throw, r0)) != cell_undefined)
    return apply (throw, cons (key, cons (x, cell_nil)), r0);
  assert (!"error");
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
#if 0
  eputs ("call: ");
  if (FUNCTION (fn).name) eputs (FUNCTION (fn).name);
  else eputs (itoa (CDR (fn)));
  eputs ("\n");
#endif
  switch (FUNCTION (fn).arity)
    {
    case 0: return FUNCTION (fn).function0 ();
    case 1: return FUNCTION (fn).function1 (car (x));
    case 2: return FUNCTION (fn).function2 (car (x), cadr (x));
    case 3: return FUNCTION (fn).function3 (car (x), cadr (x), car (cddr (x)));
    case -1: return FUNCTION (fn).functionn (x);
    }

  return cell_unspecified;
}

SCM
assq (SCM x, SCM a)
{
  while (a != cell_nil && eq_p (x, CAAR (a)) == cell_f) a = CDR (a);
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
  if (TYPE (x) != TPAIR) error (cell_symbol_not_a_pair, cons (x, cell_set_cdr_x));
  CDR (x) = e;
  return cell_unspecified;
}

SCM
set_env_x (SCM x, SCM e, SCM a)
{
  SCM p = assert_defined (x, assq (x, a));
  if (TYPE (p) != TPAIR)  error (cell_symbol_not_a_pair, cons (p, x));
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
make_closure (SCM args, SCM body, SCM a)
{
  return make_cell (tmp_num_ (TCLOSURE), cell_f, cons (cons (cell_circular, a), cons (args, body)));
}

SCM
lookup_macro (SCM x, SCM a)
{
  if (TYPE (x) != TSYMBOL) return cell_f;
  SCM m = assq_ref_env (x, a);
#if 0
  if (TYPE (m) == TMACRO)
    {
      fputs ("XXmacro: ", stdout);
      fputs ("[", stdout);
      fputs (itoa (m), stdout);
      fputs ("]: ", stdout);
      display_ (m);
      fputs ("\n", stdout);

    }
#endif
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

SCM
eval_apply ()
{
 eval_apply:
  if (g_free + GC_SAFETY > ARENA_SIZE)
    gc_pop_frame (gc (gc_push_frame ()));

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
      check_formals (car (r1), MAKE_NUMBER (FUNCTION (car (r1)).arity), cdr (r1));
      r1 = call (car (r1), cdr (r1)); /// FIXME: move into eval_apply
      goto vm_return;
    }
    case TCLOSURE:
      {
        SCM cl = CLOSURE (car (r1));
        SCM formals = cadr (cl);
        SCM body = cddr (cl);
        SCM aa = cdar (cl);
        aa = cdr (aa);
        check_formals (car (r1), formals, cdr (r1));
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
          default: check_apply (cell_f, car (r1));
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
              check_formals (r1, formals, cdr (r1));
              call_lambda (body, p, p, r0);
              goto begin;
            }
          }
      }
    }
  push_cc (car (r1), r1, r0, cell_vm_apply2);
  goto eval;
 apply2:
  check_apply (r1, car (r2));
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
    default: goto vm_return;
    }

  SCM macro;
  SCM expanders;
 macro_expand:
  if (TYPE (r1) == TPAIR
      && (macro = lookup_macro (car (r1), r0)) != cell_f)
    {
      r1 = cons (macro, CDR (r1));
#if 0
      fputs ("macro: ", stdout);
      display_ (macro);
      fputs ("\n", stdout);
      fputs ("r1: ", stdout);
      display_ (r1);
      fputs ("\n", stdout);
#endif
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
#if 0
        fputs ("begin: ", stdout);
        display_ (r1);
        fputs ("\n", stdout);
#endif
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
  x = MAKE_CONTINUATION (g_continuations++);
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
gc_push_frame () ///((internal))
{
  SCM frame = cons (r1, cons (r2, cons (r3, cons (r0, cell_nil))));
  return g_stack = cons (frame, g_stack);
}

SCM
apply (SCM f, SCM x, SCM a) ///((internal))
{
  push_cc (cons (f, x), cell_unspecified, r0, cell_unspecified);
  r3 = cell_vm_apply;
  return eval_apply ();
}

SCM
make_symbol_ (SCM s)
{
  g_cells[tmp_num].value = TSYMBOL;
  SCM x = make_cell (tmp_num, s, 0);
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
  while (x) {
    if (list_of_char_equal_p (STRING (car (x)), s) == cell_t) break;
    x = cdr (x);
  }
  if (x) x = car (x);
  return x;
}

SCM
make_symbol (SCM s)
{
  SCM x = lookup_symbol_ (s);
  return x ? x : make_symbol_ (s);
}

//MINI_MES reader.c
SCM
lookup_ (SCM s, SCM a)
{
  if (isdigit (VALUE (car (s))) || (VALUE (car (s)) == '-' && cdr (s) != cell_nil)) {
    SCM p = s;
    int sign = 1;
    if (VALUE (car (s)) == '-') {
      sign = -1;
      p = cdr (s);
    }
    int n = 0;
    while (p != cell_nil && isdigit (VALUE (car (p)))) {
      n *= 10;
      n += VALUE (car (p)) - '0';
      p = cdr (p);
    }
    if (p == cell_nil) return MAKE_NUMBER (n * sign);
  }

  SCM x = lookup_symbol_ (s);
  return x ? x : make_symbol_ (s);
}

SCM
acons (SCM key, SCM value, SCM alist)
{
  return cons (cons (key, value), alist);
}

void
make_tmps (struct scm* cells)
{
  tmp = g_free++;
  cells[tmp].type = TCHAR;
  tmp_num = g_free++;
  cells[tmp_num].type = TNUMBER;
  tmp_num2 = g_free++;
  cells[tmp_num2].type = TNUMBER;
}

// Environment setup
SCM
gc_init_cells () ///((internal))
{
  g_cells = (struct scm *)malloc (2*ARENA_SIZE*sizeof(struct scm));
  g_cells[0].type = TVECTOR;
  g_cells[0].length = 1000;
  g_cells[0].vector = 0;
  g_cells++;
  g_cells[0].type = TCHAR;
  g_cells[0].value = 'c';
}

SCM
gc_init_news () ///((internal))
{
  g_news = g_cells-1 + ARENA_SIZE;
  g_news[0].type = TVECTOR;
  g_news[0].length = 1000;
  g_news[0].vector = 0;
  g_news++;
  g_news[0].type = TCHAR;
  g_news[0].value = 'n';
}

SCM
mes_symbols () ///((internal))
{
  gc_init_cells ();
  gc_init_news ();

#include "mes.symbols.i"

  g_symbol_max = g_free;
  make_tmps (g_cells);

  g_symbols = 0;
  for (int i=1; i<g_symbol_max; i++)
    g_symbols = cons (i, g_symbols);

  SCM a = cell_nil;

#include "mes.symbol-names.i"

  a = acons (cell_symbol_mes_version, MAKE_STRING (cstring_to_list (VERSION)), a);
  a = acons (cell_symbol_mes_prefix, MAKE_STRING (cstring_to_list (PREFIX)), a);

#if BOOT
  a = acons (cell_symbol_label, cell_t, a);
#endif
  a = acons (cell_symbol_dot, cell_dot, a);
  a = acons (cell_symbol_begin, cell_begin, a);
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

#define gputs(x) fputs(x,stdout);

SCM
mes_builtins (SCM a) ///((internal))
{
#include "mes.i"

#include "posix.i"
#include "math.i"
#include "lib.i"
#include "reader.i"
#include "vector.i"
#include "gc.i"

#include "gc.environment.i"
#include "lib.environment.i"
#include "math.environment.i"
#include "mes.environment.i"
#include "posix.environment.i"
#include "reader.environment.i"
#include "vector.environment.i"

  if (g_debug)
    {
      gputs ("functions: ");
      gputs (itoa (g_function));
      gputs ("\n");
      for (int i = 0; i < g_function; i++)
        {
          gputs ("[");
          gputs (itoa (i));
          gputs ("]: ");
          gputs (g_functions[i].name);
          gputs ("\n");
        }
      gputs ("\n");
    }

  return a;
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

SCM
mes_environment () ///((internal))
{
  SCM a = mes_symbols ();
  return mes_g_stack (a);
}

FILE *g_stdin;
#include "math.c"
#include "posix.c"
#include "lib.c"
#include "reader.c"
#include "gc.c"
#include "vector.c"

int
main (int argc, char *argv[])
{
#if __GNUC__
  g_debug = getenv ("MES_DEBUG");
#endif
  if (getenv ("MES_ARENA")) ARENA_SIZE = atoi (getenv ("MES_ARENA"));
  if (argc > 1 && !strcmp (argv[1], "--help")) return puts ("Usage: mes [--dump|--load] < FILE");
  if (argc > 1 && !strcmp (argv[1], "--version")) {puts ("Mes ");puts (VERSION);return 0;};
  g_stdin = stdin;
  r0 = mes_environment ();

  SCM program = (argc > 1 && !strcmp (argv[1], "--load"))
    ? bload_env (r0) : load_env (r0);
  if (argc > 1 && !strcmp (argv[1], "--dump")) return dump ();

  SCM lst = cell_nil;
  for (int i=argc; i; i--) lst = cons (MAKE_STRING (cstring_to_list (argv[i-1])), lst);
  r0 = acons (cell_symbol_argv, lst, r0);

  push_cc (r2, cell_unspecified, r0, cell_unspecified);
  r3 = cell_vm_begin;
  r1 = eval_apply ();
  ///stderr_ (r1);
  display_ (r1);
  fputs ("", stdout);
  gc (g_stack);
#if __GNUC__
  if (g_debug) fprintf (stderr, "\nstats: [%d]\n", g_free);
#else
#endif
  return 0;
}
