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

#define _GNU_SOURCE
#include <assert.h>
#include <ctype.h>
#include <errno.h>
#include <limits.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <stdbool.h>

#define DEBUG 0
#define FIXED_PRIMITIVES 1

int ARENA_SIZE = 100000;
int MAX_ARENA_SIZE = 20000000;
int GC_SAFETY = 100;

typedef int SCM;
enum type_t {CHAR, CLOSURE, FUNCTION, KEYWORD, MACRO, NUMBER, PAIR, REF, SPECIAL, STRING, SYMBOL, VALUES, VECTOR, BROKEN_HEART};
typedef SCM (*function0_t) (void);
typedef SCM (*function1_t) (SCM);
typedef SCM (*function2_t) (SCM, SCM);
typedef SCM (*function3_t) (SCM, SCM, SCM);
typedef SCM (*functionn_t) (SCM);
typedef struct function_t {
  union {
    function0_t function0;
    function1_t function1;
    function2_t function2;
    function3_t function3;
    functionn_t functionn;
  };
  int arity;
} function;
struct scm_t;
typedef struct scm_t {
  enum type_t type;
  union {
    char const *name;
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
    SCM macro;
    SCM vector;
    int hits;
  };
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

scm scm_symbol_dot = {SYMBOL, "*dot*"};
scm scm_symbol_lambda = {SYMBOL, "lambda"};
scm scm_symbol_begin = {SYMBOL, "begin"};
scm scm_symbol_if = {SYMBOL, "if"};
scm scm_symbol_set_x = {SYMBOL, "set!"};

scm scm_symbol_quote = {SYMBOL, "quote"};

scm scm_symbol_sc_expand = {SYMBOL, "sc-expand"};
scm scm_symbol_macro_expand = {SYMBOL, "macro-expand"};
scm scm_symbol_sc_expander_alist = {SYMBOL, "*sc-expander-alist*"};

scm scm_symbol_call_with_values = {SYMBOL, "call-with-values"};
scm scm_symbol_current_module = {SYMBOL, "current-module"};
scm scm_symbol_primitive_load = {SYMBOL, "primitive-load"};
scm scm_symbol_read_input_file = {SYMBOL, "read-input-file"};
scm scm_symbol_write = {SYMBOL, "write"};
scm scm_symbol_display = {SYMBOL, "display"};

scm scm_symbol_mes_version = {SYMBOL, "%version"};
scm scm_symbol_mes_prefix = {SYMBOL, "%prefix"};

scm scm_symbol_car = {SYMBOL, "car"};
scm scm_symbol_cdr = {SYMBOL, "cdr"};
scm scm_symbol_null_p = {SYMBOL, "null?"};
scm scm_symbol_eq_p = {SYMBOL, "eq?"};
scm scm_symbol_cons = {SYMBOL, "cons"};

scm g_free = {NUMBER, .value=0};
scm *g_cells;
scm *g_news = 0;

#include "mes.symbols.h"

SCM tmp;
SCM tmp_num;
SCM tmp_num2;
SCM tmp_num3;
SCM tmp_num4;

function functions[200];
int g_function = 0;

SCM g_symbols = 0;
SCM g_stack = 0;
SCM r0 = 0; // a/env
SCM r1 = 0; // param 1
SCM r2 = 0; // save 2+load/dump

#include "lib.h"
#include "math.h"
#include "mes.h"
#include "posix.h"
#include "reader.h"

#define CAR(x) g_cells[x].car
#define CDR(x) g_cells[x].cdr
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
#define FUNCTION(x) functions[g_cells[x].function]
#define NCAR(x) g_news[x].car
#define NTYPE(x) g_news[x].type

#define CAAR(x) CAR (CAR (x))
#define CDAR(x) CDR (CAR (x))
#define CAAR(x) CAR (CAR (x))
#define CADAR(x) CAR (CDR (CAR (x)))
#define CADDR(x) CAR (CDR (CDR (x)))
#define CDADAR(x) CAR (CDR (CAR (CDR (x))))
#define CADR(x) CAR (CDR (x))

#define MAKE_CHAR(n) make_cell (tmp_num_ (CHAR), 0, tmp_num2_ (n))
#define MAKE_NUMBER(n) make_cell (tmp_num_ (NUMBER), 0, tmp_num2_ (n))
#define MAKE_REF(n) make_cell (tmp_num_ (REF), n, 0)
#define MAKE_STRING(x) make_cell (tmp_num_ (STRING), x, 0)

int error (char const* msg, SCM x);
SCM vm_call (function0_t f, SCM p1, SCM a);

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
  assert (g_free.value + n < ARENA_SIZE);
  SCM x = g_free.value;
  g_free.value += n;
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
cons (SCM x, SCM y)
{
  g_cells[tmp_num].value = PAIR;
  return make_cell (tmp_num, x, y);
}

SCM
car (SCM x)
{
  if (TYPE (x) != PAIR) error ("car: not pair: ", x);
  return CAR (x);
}

SCM
cdr (SCM x)
{
  if (TYPE (x) != PAIR) error ("cdr: not pair: ", x);
  return CDR (x);
}

SCM
type_ (SCM x)
{
  return MAKE_NUMBER (TYPE (x));
}

SCM
car_ (SCM x)
{
  return (TYPE (CAR (x)) == PAIR
          || TYPE (CAR (x)) == REF
          || TYPE (CAR (x)) == SYMBOL
          || TYPE (CAR (x)) == STRING) ? CAR (x) : MAKE_NUMBER (CAR (x));
}

SCM
cdr_ (SCM x)
{
  return (TYPE (CDR (x)) == PAIR
          || TYPE (CDR (x)) == REF
          || TYPE (CDR (x)) == SYMBOL
          || TYPE (CDR (x)) == STRING) ? CDR (x) : MAKE_NUMBER (CDR (x));
}

SCM
eq_p (SCM x, SCM y)
{
  return (x == y
          || ((TYPE (x) == KEYWORD && TYPE (y) == KEYWORD
               && STRING (x) == STRING (y)))
          || (TYPE (x) == CHAR && TYPE (y) == CHAR
              && VALUE (x) == VALUE (y))
          || (TYPE (x) == NUMBER && TYPE (y) == NUMBER
              && VALUE (x) == VALUE (y)))
    ? cell_t : cell_f;
}

SCM
set_car_x (SCM x, SCM e)
{
  assert (TYPE (x) == PAIR);
  CAR (x) = e;
  return cell_unspecified;
}

SCM
set_cdr_x (SCM x, SCM e)
{
  if (TYPE (x) != PAIR) error ("set-cdr!: not pair: ", x);
  CDR (x) = e;
  return cell_unspecified;
}

SCM
set_env_x (SCM x, SCM e, SCM a)
{
  SCM p = assert_defined (x, assq (x, a));
  if (TYPE (p) != PAIR) error ("set-env!: not pair: ", x);
  return set_cdr_x (p, e);
}

SCM
pairlis (SCM x, SCM y, SCM a)
{
  if (x == cell_nil)
    return a;
  if (TYPE (x) != PAIR)
    return cons (cons (x, y), a);
  return cons (cons (car (x), car (y)),
               pairlis (cdr (x), cdr (y), a));
}

SCM
assq (SCM x, SCM a)
{
  while (a != cell_nil && eq_p (x, CAAR (a)) == cell_f) a = CDR (a);
  return a != cell_nil ? car (a) : cell_f;
}

SCM
assq_ref_cache (SCM x, SCM a)
{
  x = assq (x, a);
  if (x == cell_f) return cell_undefined;
  return cdr (x);
}

enum eval_apply_t {EVLIS, APPLY, EVAL, MACRO_EXPAND, BEGIN, IF, CALL_WITH_VALUES};
enum eval_apply_t g_target;

SCM
call_lambda (SCM e, SCM x, SCM aa, SCM a) ///((internal))
{
  SCM cl = cons (cons (cell_closure, x), x);
  r1 = e;
  r0 = cl;
  return cell_unspecified;
}

SCM
eval_apply ()
{
  switch (g_target)
    {
    case EVLIS: goto evlis;
    case APPLY: goto apply;
    case EVAL: goto eval;
    case MACRO_EXPAND: goto macro_expand;
    case BEGIN: goto begin;
    case IF: goto label_if;
    case CALL_WITH_VALUES: goto call_with_values;
    }

 evlis:
  if (r1 == cell_nil) return cell_nil;
  if (TYPE (r1) != PAIR) goto eval;
  r2 = eval_env (car (r1), r0);
  r1 = evlis_env (cdr (r1), r0);
  return cons (r2, r1);

 apply:
  switch (TYPE (car (r1)))
    {
    case FUNCTION: {
      check_formals (car (r1), MAKE_NUMBER (FUNCTION (car (r1)).arity), cdr (r1));
      return call (car (r1), cdr (r1));
    }
    case CLOSURE:
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
    case SYMBOL:
      {
        if (car (r1) == cell_symbol_call_with_values)
          {
            r1 = cdr (r1);
            goto call_with_values;
          }
        if (car (r1) == cell_symbol_current_module) return r0;
        break;
      }
    case PAIR:
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
  SCM e = eval_env (car (r1), r0);
  check_apply (e, car (r1));
  r1 = cons (e, cdr (r1));
  goto apply;

 eval:
  switch (TYPE (r1))
    {
    case PAIR:
      {
        switch (car (r1))
          {
#if FIXED_PRIMITIVES
          case cell_symbol_car: return car (eval_env (CADR (r1), r0));
          case cell_symbol_cdr: return cdr (eval_env (CADR (r1), r0));
          case cell_symbol_cons: {SCM m = evlis_env (CDR (r1), r0);
              return cons (CAR (m), CADR (m));}
          case cell_symbol_null_p: return null_p (eval_env (CADR (r1), r0));
#endif // FIXED_PRIMITIVES
          case cell_symbol_quote: return cadr (r1);
          case cell_symbol_begin: goto begin;
          case cell_symbol_lambda:
            return make_closure (cadr (r1), cddr (r1), assq (cell_closure, r0));
          case cell_symbol_if: {r1=cdr (r1); goto label_if;}
          case cell_symbol_set_x: {
            SCM x = eval_env (car (cddr (r1)), r0); return set_env_x (cadr (r1), x, r0);
          }
          default: {
            SCM x = macro_expand_env (r1, r0);
            if (x != r1)
              {
                if (TYPE (x) == PAIR)
                  {
                    set_cdr_x (r1, cdr (x));
                    set_car_x (r1, car (x));
                  }
                r1 = x;
                goto eval;
              }
            SCM m = evlis_env (CDR (r1), r0);
            r1 = cons (car (r1), m);
            goto apply;
          }
          }
      }
    case SYMBOL: return assert_defined (r1, assq_ref_cache (r1, r0));
    default: return r1;
    }

  SCM macro;
  SCM expanders;
 macro_expand:
  if (TYPE (r1) == PAIR
      && (macro = lookup_macro (car (r1), r0)) != cell_f)
    {
      r1 = cons (macro, CDR (r1));
      goto apply;
    }
  else if (TYPE (r1) == PAIR
           && TYPE (CAR (r1)) == SYMBOL
           && ((expanders = assq_ref_cache (cell_symbol_sc_expander_alist, r0)) != cell_undefined)
           && ((macro = assq (CAR (r1), expanders)) != cell_f))
    {
      SCM sc_expand = assq_ref_cache (cell_symbol_macro_expand, r0);
      if (sc_expand != cell_undefined && sc_expand != cell_f)
        {
          r1 = cons (sc_expand, cons (r1, cell_nil));
          goto apply;
        }
    }
  return r1;

  SCM r;
 begin:
  r = cell_unspecified;
  while (r1 != cell_nil) {
    if (TYPE (r1) == PAIR && TYPE (CAR (r1)) == PAIR)
      {
        if (caar (r1) == cell_symbol_begin)
          r1 = append2 (cdar (r1), cdr (r1));
        else if (caar (r1) == cell_symbol_primitive_load)
          {
            SCM f = read_input_file_env (r0);
            r1 = append2 (f, cdr (r1));
          }
      }
    if (CDR (r1) == cell_nil)
      {
        r1 = car (r1);
        goto eval;
      }
    r = eval_env (car (r1), r0);
    r1 = CDR (r1);
  }
  return r;

  SCM x;
 label_if:
  x = eval_env (car (r1), r0);
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
  return cell_unspecified;

  SCM v;
 call_with_values:
  v = apply_env (car (r1), cell_nil, r0);
  if (TYPE (v) == VALUES)
    v = CDR (v);
  r1 = cons (cadr (r1), v);
  goto apply;
}

SCM
call (SCM fn, SCM x)
{
  if ((FUNCTION (fn).arity > 0 || FUNCTION (fn).arity == -1)
      && x != cell_nil && TYPE (CAR (x)) == VALUES)
    x = cons (CADAR (x), CDR (x));
  if ((FUNCTION (fn).arity > 1 || FUNCTION (fn).arity == -1)
      && x != cell_nil && TYPE (CDR (x)) == PAIR && TYPE (CADR (x)) == VALUES)
    x = cons (CAR (x), cons (CDADAR (x), CDR (x)));
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
gc_peek_frame ()
{
  SCM frame = car (g_stack);
  r1 = car (frame);
  r2 = cadr (frame);
  r0 = cadr (cddr (frame));
  return frame;
}

SCM
gc_pop_frame ()
{
  SCM frame = gc_peek_frame (g_stack);
  g_stack = cdr (g_stack);
  return frame;
}

SCM
gc_push_frame ()
{
  SCM frame = cons (r1, cons (r2, cons (r0, cell_nil)));
  return g_stack = cons (frame, g_stack);
}

SCM
vm_call (function0_t f, SCM p1, SCM a)
{
  gc_push_frame ();
  r1 = p1;
  r0 = a;
  if (g_free.value + GC_SAFETY > ARENA_SIZE)
    gc_pop_frame (gc (gc_push_frame ()));

  SCM r = f ();
  gc_pop_frame ();
  return r;
}

SCM
evlis_env (SCM m, SCM a)
{
  g_target = EVLIS;
  return vm_call (eval_apply, m, a);
}

SCM
apply_env (SCM fn, SCM x, SCM a)
{
  g_target = APPLY;
  return vm_call (eval_apply, cons (fn, x), a);
}

SCM
eval_env (SCM e, SCM a)
{
  g_target = EVAL;
  return vm_call (eval_apply, e, a);
}

SCM
macro_expand_env (SCM e, SCM a)
{
  g_target = MACRO_EXPAND;
  return vm_call (eval_apply, e, a);
}

SCM
begin_env (SCM e, SCM a)
{
  g_target = BEGIN;
  return vm_call (eval_apply, e, a);
}

SCM
if_env (SCM e, SCM a)
{
  g_target = IF;
  return vm_call (eval_apply, e, a);
}

SCM
call_with_values_env (SCM producer, SCM consumer, SCM a)
{
  g_target = CALL_WITH_VALUES;
  return vm_call (eval_apply, cons (producer, cons (consumer, cell_nil)), a);
}

SCM
append2 (SCM x, SCM y)
{
  if (x == cell_nil) return y;
  assert (TYPE (x) == PAIR);
  return cons (car (x), append2 (cdr (x), y));
}

SCM
append (SCM x) ///((arity . n))
 {
  if (x == cell_nil) return cell_nil;
  if (cdr (x) == cell_nil) return car (x);
  return append2 (car (x), append (cdr (x)));
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
null_p (SCM x)
{
  return x == cell_nil ? cell_t : cell_f;
}

SCM
make_symbol_ (SCM s)
{
  g_cells[tmp_num].value = SYMBOL;
  SCM x = make_cell (tmp_num, s, 0);
  g_symbols = cons (x, g_symbols);
  return x;
}

SCM
make_symbol (SCM s)
{
  SCM x = lookup_symbol_ (s);
  return x ? x : make_symbol_ (s);
}

SCM
make_vector (SCM n)
{
  int k = VALUE (n);
  g_cells[tmp_num].value = VECTOR;
  SCM v = alloc (k);
  SCM x = make_cell (tmp_num, k, v);
  for (int i=0; i<k; i++) g_cells[v+i] = g_cells[vector_entry (cell_unspecified)];
  return x;
}

SCM
arity_ (SCM x)
{
  assert (TYPE (x) == FUNCTION);
  return MAKE_NUMBER (FUNCTION (x).arity);
}

SCM
values (SCM x) ///((arity . n))
{
  SCM v = cons (0, x);
  TYPE (v) = VALUES;
  return v;
}

SCM
vector_length (SCM x)
{
  assert (TYPE (x) == VECTOR);
  return MAKE_NUMBER (LENGTH (x));
}

SCM
vector_ref (SCM x, SCM i)
{
  assert (TYPE (x) == VECTOR);
  assert (VALUE (i) < LENGTH (x));
  SCM e = VECTOR (x) + VALUE (i);
  if (TYPE (e) == REF) e = g_cells[e].ref;
  if (TYPE (e) == CHAR) e = MAKE_CHAR (VALUE (e));
  if (TYPE (e) == NUMBER) e = MAKE_NUMBER (VALUE (e));
  return e;
}

SCM
vector_entry (SCM x) {
  if (TYPE (x) == PAIR || TYPE (x) == SPECIAL || TYPE (x) == STRING || TYPE (x) == SYMBOL || TYPE (x) == VECTOR) x = MAKE_REF (x);
  return x;
}

SCM
vector_set_x (SCM x, SCM i, SCM e)
{
  assert (TYPE (x) == VECTOR);
  assert (VALUE (i) < LENGTH (x));
  g_cells[VECTOR (x)+g_cells[i].value] = g_cells[vector_entry (e)];
  return cell_unspecified;
}

SCM
list_to_vector (SCM x)
{
  VALUE (tmp_num) = VALUE (length (x));
  SCM v = make_vector (tmp_num);
  SCM p = VECTOR (v);
  while (x != cell_nil)
    {
      g_cells[p++] = g_cells[vector_entry (car (x))];
      x = cdr (x);
    }
  return v;
}

SCM
vector_to_list (SCM v)
{
  SCM x = cell_nil;
  for (int i = 0; i < LENGTH (v); i++) {
    SCM e = VECTOR (v)+i;
    if (TYPE (e) == REF) e = g_cells[e].ref;
    x = append2 (x, cons (e, cell_nil));
  }
  return x;
}

void
make_tmps (scm* cells)
{
  tmp = g_free.value++;
  cells[tmp].type = CHAR;
  tmp_num = g_free.value++;
  cells[tmp_num].type = NUMBER;
  tmp_num2 = g_free.value++;
  cells[tmp_num2].type = NUMBER;
  tmp_num3 = g_free.value++;
  cells[tmp_num3].type = NUMBER;
  tmp_num4 = g_free.value++;
  cells[tmp_num4].type = NUMBER;
}

// Jam Collector
SCM g_symbol_max;
bool g_debug = false;

SCM
gc_up_arena ()
{
  ARENA_SIZE *= 2;
  void *p = realloc (g_cells-1, 2*ARENA_SIZE*sizeof(scm));
  if (!p) error (strerror (errno), MAKE_NUMBER (g_free.value));
  g_cells = (scm*)p;
  g_cells++;
  gc_init_news ();
}

SCM
gc ()
{
  if (g_debug) fprintf (stderr, "***gc[%d]...", g_free.value);
  g_free.value = 1;
  if (g_cells < g_news && ARENA_SIZE < MAX_ARENA_SIZE) gc_up_arena ();
  for (int i=g_free.value; i<g_symbol_max; i++)
    gc_copy (i);
  make_tmps (g_news);
  g_symbols = gc_copy (g_symbols);
  SCM new = gc_copy (g_stack);
  if (g_debug) fprintf (stderr, "new=%d\n", new, g_stack);
  g_stack = new;
  return gc_loop (1);
}

SCM
gc_loop (SCM scan)
{
  while (scan < g_free.value)
    {
      if (NTYPE (scan) == CLOSURE
          || NTYPE (scan) == FUNCTION
          || NTYPE (scan) == KEYWORD
          || NTYPE (scan) == MACRO
          || NTYPE (scan) == PAIR
          || NTYPE (scan) == REF
          || scan == 1 // null
          || NTYPE (scan) == SPECIAL
          || NTYPE (scan) == STRING
          || NTYPE (scan) == SYMBOL)
        {
          SCM car = gc_copy (g_news[scan].car);
          gc_relocate_car (scan, car);
        }
      if ((NTYPE (scan) == CLOSURE
           || NTYPE (scan) == MACRO
           || NTYPE (scan) == PAIR
           || NTYPE (scan) == VALUES)
          && g_news[scan].cdr) // allow for 0 terminated list of symbols
        {
          SCM cdr = gc_copy (g_news[scan].cdr);
          gc_relocate_cdr (scan, cdr);
        }
      scan++;
    }
  return gc_flip ();
}

SCM
gc_copy (SCM old)
{
  if (TYPE (old) == BROKEN_HEART) return g_cells[old].car;
  SCM new = g_free.value++;
  g_news[new] = g_cells[old];
  if (NTYPE (new) == VECTOR)
    {
      g_news[new].vector = g_free.value;
      for (int i=0; i<LENGTH (old); i++)
        g_news[g_free.value++] = g_cells[VECTOR (old)+i];
    }
  g_cells[old].type = BROKEN_HEART;
  g_cells[old].car = new;
  return new;
}

SCM
gc_relocate_car (SCM new, SCM car)
{
  g_news[new].car = car;
  return cell_unspecified;
}

SCM
gc_relocate_cdr (SCM new, SCM cdr)
{
  g_news[new].cdr = cdr;
  return cell_unspecified;
}

SCM
gc_flip ()
{
  scm *cells = g_cells;
  g_cells = g_news;
  g_news = cells;
  if (g_debug) fprintf (stderr, " => jam[%d]\n", g_free.value);
  return g_stack;
}

// Environment setup
SCM
acons (SCM key, SCM value, SCM alist)
{
  return cons (cons (key, value), alist);
}

SCM
gc_init_cells ()
{
  g_cells = (scm *)malloc (2*ARENA_SIZE*sizeof(scm));
  g_cells[0].type = VECTOR;
  g_cells[0].length = 1000;
  g_cells[0].vector = 0;
  g_cells++;
  g_cells[0].type = CHAR;
  g_cells[0].value = 'c';
}

SCM
gc_init_news ()
{
  g_news = g_cells-1 + ARENA_SIZE;
  g_news[0].type = VECTOR;
  g_news[0].length = 1000;
  g_news[0].vector = 0;
  g_news++;
  g_news[0].type = CHAR;
  g_news[0].value = 'n';
}

SCM
mes_symbols () ///((internal))
{
  gc_init_cells ();
  gc_init_news ();

#include "mes.symbols.i"

  g_symbol_max = g_free.value;
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
  a = acons (cell_symbol_sc_expand, cell_f, a);
  a = acons (cell_closure, a, a);

  return a;
}

SCM
mes_builtins (SCM a)
{
#include "mes.i"

#include "lib.i"
#include "math.i"
#include "posix.i"
#include "reader.i"

#include "lib.environment.i"
#include "math.environment.i"
#include "mes.environment.i"
#include "posix.environment.i"
#include "reader.environment.i"

  return a;
}

SCM
mes_g_stack (SCM a) ///((internal))
{
  r0 = a;
  r1 = MAKE_CHAR (0);
  r2 = MAKE_CHAR (0);
  g_stack = cons (cell_nil, cell_nil);
  return r0;
}

SCM
mes_environment () ///((internal))
{
  SCM a = mes_symbols ();
  return mes_g_stack (a);
}

SCM
make_closure (SCM args, SCM body, SCM a)
{
  return make_cell (tmp_num_ (CLOSURE), cell_f, cons (cons (cell_circular, a), cons (args, body)));
}

SCM
lookup_macro (SCM x, SCM a)
{
  if (TYPE (x) != SYMBOL) return cell_f;
  SCM m = assq_ref_cache (x, a);
  if (TYPE (m) == MACRO) return MACRO (m);
  return cell_f;
}

FILE *g_stdin;
#include "lib.c"
#include "math.c"
#include "posix.c"
#include "reader.c"

int
main (int argc, char *argv[])
{
  g_debug = getenv ("MES_DEBUG");
  if (getenv ("MES_ARENA")) ARENA_SIZE = atoi (getenv ("MES_ARENA"));
  if (argc > 1 && !strcmp (argv[1], "--help")) return puts ("Usage: mes [--dump|--load] < FILE");
  if (argc > 1 && !strcmp (argv[1], "--version")) return puts ("Mes " VERSION);
  g_stdin = stdin;
  r0 = mes_environment ();
  SCM program = (argc > 1 && !strcmp (argv[1], "--load"))
    ? bload_env (r0) : load_env (r0);
  if (argc > 1 && !strcmp (argv[1], "--dump")) return dump ();
  stderr_ (begin_env (program, r0));
  fputs ("", stderr);
  gc (g_stack);
  if (g_debug) fprintf (stderr, "\nstats: [%d]\n", g_free.value);
  return 0;
}
