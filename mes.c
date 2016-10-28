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
#include <limits.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <stdbool.h>

#define DEBUG 0
#define QUASIQUOTE 1
//#define QUASISYNTAX 0

#define GC 1
#define MES_FULL 1
#define MES_MINI 0 // 1 for gc-2a.test, gc-3.test

#if MES_FULL
int ARENA_SIZE = 300000000; // need this much for tests/match.scm
//int ARENA_SIZE = 30000000; // need this much for tests/record.scm
//int ARENA_SIZE = 500000; // enough for tests/scm.test
//int ARENA_SIZE = 60000; // enough for tests/base.test
int GC_SAFETY = 10000;
int GC_FREE = 20000;
#else
// just enough for empty environment and tests/gc-2.test.
//int ARENA_SIZE = 7500; // gc-3.test, gc-2a.test
//int ARENA_SIZE = 10000; // gc-2a.test
int ARENA_SIZE = 18000; // gc-2.test -->KRAK
//int ARENA_SIZE = 23000; // gc-2.test OK
int GC_SAFETY = 1000;
int GC_FREE = 1000;
#endif

enum type {CHAR, FUNCTION, MACRO, NUMBER, PAIR, SCM, STRING, SYMBOL, REF, VALUES, VECTOR, BROKEN_HEART};
typedef struct scm_t* (*function0_t) (void);
typedef struct scm_t* (*function1_t) (struct scm_t*);
typedef struct scm_t* (*function2_t) (struct scm_t*, struct scm_t*);
typedef struct scm_t* (*function3_t) (struct scm_t*, struct scm_t*, struct scm_t*);
typedef struct scm_t* (*functionn_t) (struct scm_t*);
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
  enum type type;
  union {
    char const *name;
    struct scm_t* string;
    struct scm_t* car;
    struct scm_t* ref;
    int length;
  };
  union {
    int value;
    function* function;
    struct scm_t* cdr;
    struct scm_t* macro;
    struct scm_t* vector;
    int hits;
  };
} scm;

#include "define.environment.h"
#include "lib.environment.h"
#include "math.environment.h"
#include "mes.environment.h"
#include "posix.environment.h"
#include "quasiquote.environment.h"
#include "string.environment.h"
#include "type.environment.h"

scm *display_ (FILE* f, scm *x);
scm *display_helper (FILE*, scm*, bool, char const*, bool);

scm *symbols = 0;
scm *stack = 0;
scm *r0 = 0; // a/env
scm *r1 = 0; // param 1
scm *r2 = 0; // param 2
scm *r3 = 0; // param 3

scm scm_nil = {SCM, "()"};
scm scm_dot = {SCM, "."};
scm scm_f = {SCM, "#f"};
scm scm_t = {SCM, "#t"};
scm scm_undefined = {SCM, "*undefined*"};
scm scm_unspecified = {SCM, "*unspecified*"};
scm scm_closure = {SCM, "*closure*"};
scm scm_circular = {SCM, "*circular*"};
#if BOOT
scm scm_label = {
  SCM, "label"};
#endif
scm scm_begin = {SCM, "*begin*"};

scm symbol_lambda = {SYMBOL, "lambda"};
scm symbol_begin = {SYMBOL, "begin"};
scm symbol_if = {SYMBOL, "if"};
scm symbol_define = {SYMBOL, "define"};
scm symbol_define_macro = {SCM, "define-macro"};
scm symbol_set_x = {SYMBOL, "set!"};

scm symbol_quote = {SYMBOL, "quote"};
scm symbol_quasiquote = {SYMBOL, "quasiquote"};
scm symbol_unquote = {SYMBOL, "unquote"};
scm symbol_unquote_splicing = {SYMBOL, "unquote-splicing"};

scm symbol_sc_expand = {SYMBOL, "sc-expand"};
scm symbol_expand_macro = {SYMBOL, "expand-macro"};
scm symbol_sc_expander_alist = {SYMBOL, "*sc-expander-alist*"};
scm symbol_noexpand = {SYMBOL, "noexpand"};
scm symbol_syntax = {SYMBOL, "syntax"};
scm symbol_quasisyntax = {SYMBOL, "quasisyntax"};
scm symbol_unsyntax = {SYMBOL, "unsyntax"};
scm symbol_unsyntax_splicing = {SYMBOL, "unsyntax-splicing"};

scm symbol_call_with_values = {SYMBOL, "call-with-values"};
scm symbol_current_module = {SYMBOL, "current-module"};
scm symbol_primitive_load = {SYMBOL, "primitive-load"};

scm char_nul = {CHAR, .name="nul", .value=0};
scm char_backspace = {CHAR, .name="backspace", .value=8};
scm char_tab = {CHAR, .name="tab", .value=9};
scm char_newline = {CHAR, .name="newline", .value=10};
scm char_vt = {CHAR, .name="vt", .value=11};
scm char_page = {CHAR, .name="page", .value=12};
scm char_return = {CHAR, .name="return", .value=13};
scm char_space = {CHAR, .name="space", .value=32};

// PRIMITIVES

scm *
car (scm *x)
{
  assert (x->type == PAIR);
  return x->car;
}

scm *
cdr (scm *x)
{
  assert (x->type == PAIR);
  return x->cdr;
}

scm g_free = {NUMBER, .value=0};
scm *g_cells;
scm *g_news;

scm *
alloc (int n)
{
#if GC
  assert (g_free.value + n < ARENA_SIZE);
  scm* x = &g_cells[g_free.value];
  g_free.value += n;
  return x;
#else
  return (scm*)malloc(n*sizeof (scm));
#endif
}

scm *
gc_alloc (int n)
{
  assert (g_free.value + n < ARENA_SIZE);
  scm* x = &g_cells[g_free.value];
  g_free.value += n;
  return x;
}

scm *
gc (scm *a)
{
  fprintf (stderr, "***gc[%d]...", g_free.value);
  g_free.value = 0;
  scm *new = gc_copy (stack);
  gc_copy (symbols);
  return gc_loop (new);
}

scm *
gc_loop (scm *scan)
{
  while (scan - g_news < g_free.value)
    {
      if (scan->type == MACRO
          || scan->type == PAIR
          || scan->type == REF
          || (scan->type == SCM && scan->car->type == PAIR)
          || (scan->type == STRING && scan->car->type == PAIR)
          || (scan->type == SYMBOL && scan->car->type == PAIR))
        {
          scm *car = gc_copy (scan->car);
          gc_relocate_car (scan, car);
        }
      if ((scan->type == MACRO
           || scan->type == PAIR)
          && scan->cdr) // allow for 0 terminated list of symbols
        {
          scm *cdr = gc_copy (scan->cdr);
          gc_relocate_cdr (scan, cdr);
        }
      scan++;
    }
  return gc_flip ();
}

scm *
gc_copy (scm *old)
{
  if (old->type == BROKEN_HEART) return old->car;
  if (old->type == FUNCTION) return old;
  if (old->type == SCM) return old;
  if (old < g_cells && old < g_news) return old;
  scm *new = &g_news[g_free.value++];
  *new = *old;
  if (new->type == VECTOR)
    for (int i=0; i<old->length; i++)
      *(new+i+1) = old->vector[i];
  old->type = BROKEN_HEART;
  old->car = new;
  return new;
}

scm *
gc_relocate_car (scm *new, scm *car)
{
  new->car = car;
  return &scm_unspecified;
}

scm *
gc_relocate_cdr (scm *new, scm *cdr)
{
  new->cdr = cdr;
  return &scm_unspecified;
}

scm *
gc_flip ()
{
  scm *cells = g_cells;
  g_cells = g_news;
  g_news = cells;
  (g_cells-1)->vector = g_news;
  (g_news-1)->vector = g_cells;

  fprintf (stderr, " => jam[%d]\n", g_free.value);
  // Reduce arena size to quickly get multiple GC's.
  // Startup memory footprint is relatively high because of builtin
  // function names
  //ARENA_SIZE = g_free.value + GC_FREE + GC_SAFETY;
  // fprintf (stderr, "ARENA SIZE => %d\n", ARENA_SIZE - GC_SAFETY);
  symbols = &g_cells[1];
  return &g_cells[0];
}

scm *
gc_bump ()
{
  g_cells += g_free.value;
  g_news += g_free.value;
  ARENA_SIZE -= g_free.value;
  g_free.value = 0;
  return &scm_unspecified;
}

scm *
gc_show ()
{
  fprintf (stderr, "cells: ");
  display_ (stderr, g_cells-1);
  fprintf (stderr, "\n");
  fprintf (stderr, "news: ");
  display_ (stderr, g_news-1);
  fprintf (stderr, "\n");
  return &scm_unspecified;
}

scm *
gc_make_cell (scm *type, scm *car, scm *cdr)
{
  scm *x = gc_alloc (1);
  assert (type->type == NUMBER);
  x->type = type->value;
  if (type->value == CHAR || type->value == NUMBER) {
    if (car) x->car = car->car;
    if (cdr) x->cdr = cdr->cdr;
  } else {
    x->car = car;
    x->cdr = cdr;
  }
  return x;
}

scm *
gc_make_vector (scm *n)
{
  scm t = {NUMBER, .value=VECTOR};
  scm *v = gc_alloc (n->value);
  scm *x = gc_make_cell (&t, (scm*)(long)n->value, v);
  for (int i=0; i<n->value; i++) x->vector[i] = *vector_entry (&scm_unspecified);
  return x;
}

scm *
make_cell (scm *type, scm *car, scm *cdr)
{
  scm *x = alloc (1);
  assert (type->type == NUMBER);
  x->type = type->value;
  if (type->value == CHAR || type->value == NUMBER) {
    if (car) x->car = car->car;
    if (cdr) x->cdr = cdr->cdr;
  } else {
    x->car = car;
    x->cdr = cdr;
  }
  return x;
}

scm *
cons (scm *x, scm *y)
{
  scm t = {NUMBER, .value=PAIR};
  return make_cell (&t, x, y);
}

scm *
eq_p (scm *x, scm *y)
{
  return (x == y
          || (x->type == CHAR && y->type == CHAR
              && x->value == y->value)
          || (x->type == NUMBER && y->type == NUMBER
              && x->value == y->value))
    ? &scm_t : &scm_f;
}

scm *
set_car_x (scm *x, scm *e)
{
  assert (x->type == PAIR);
  x->car = e;
  return &scm_unspecified;
}

scm *
set_cdr_x (scm *x, scm *e)
{
  assert (x->type == PAIR);
  cache_invalidate (x->cdr);
  x->cdr = e;
  return &scm_unspecified;
}

scm *
set_env_x (scm *x, scm *e, scm *a)
{
  cache_invalidate (x);
  scm *p = assert_defined (x, assq (x, a));
  return set_cdr_x (p, e);
}

scm *
quote (scm *x)
{
  return cons (&symbol_quote, x);
}

scm *
quasiquote (scm *x)
{
  return cons (&symbol_quasiquote, x);
}

scm *
quasisyntax (scm *x)
{
  return cons (&symbol_quasisyntax, x);
}

scm *
pairlis (scm *x, scm *y, scm *a)
{
  if (x == &scm_nil)
    return a;
  if (pair_p (x) == &scm_f)
    return cons (cons (x, y), a);
  return cons (cons (car (x), car (y)),
               pairlis (cdr (x), cdr (y), a));
}

scm *
assq (scm *x, scm *a)
{
  while (a != &scm_nil && eq_p (x, a->car->car) == &scm_f)
    {
      if (a->type == BROKEN_HEART || a->car->type == BROKEN_HEART)
        fprintf (stderr, "oops, broken heart\n");
      a = a->cdr;
    }
  return a != &scm_nil ? a->car : &scm_f;
}

#define ENV_CACHE 1
#define CACHE_SIZE 30
#define ENV_HEAD 15

#if !ENV_CACHE
scm *
assq_ref_cache (scm *x, scm *a)
{
  x = assq (x, a);
  if (x == &scm_f) return &scm_undefined;
  return x->cdr;
}
scm*cache_invalidate (scm*x){}
scm*cache_invalidate_range (scm*p,scm*a){}
scm*cache_save (scm*p){}
scm*cache_lookup (scm*x){}

#else // ENV_CACHE

scm *env_cache_cars[CACHE_SIZE];
scm *env_cache_cdrs[CACHE_SIZE];
int cache_threshold = 0;
scm *
cache_save (scm *p)
{
  int n = p->car->hits;
  if (n < cache_threshold) return &scm_unspecified;
  int j = -1;
  for (int i=0; i < CACHE_SIZE; i++) {
    if (!env_cache_cars[i]) {
      j = i;
      break;
    }
    if (env_cache_cars[i] == p->car) return &scm_unspecified;
    if (n > env_cache_cars[i]->hits) {
      n = env_cache_cars[i]->hits;
      j = i;
    }
  }
  if (j >= 0) {
    cache_threshold = p->car->hits;
    env_cache_cars[j] = p->car;
    env_cache_cdrs[j] = p->cdr;
  }
  return &scm_unspecified;
}

scm *
cache_lookup (scm *x)
{
  for (int i=0; i < CACHE_SIZE; i++) {
    if (!env_cache_cars[i]) break;
    if (env_cache_cars[i] == x) return env_cache_cdrs[i];
  }
  return &scm_undefined;
}

scm *
cache_invalidate (scm *x)
{
  for (int i=0; i < CACHE_SIZE; i++) {
    if (env_cache_cars[i] == x) {
      env_cache_cars[i] = 0;
      break;
    }
  }
  return &scm_unspecified;
}

scm *
cache_invalidate_range (scm *p, scm *a)
{
  do {
    cache_invalidate (p->car->car);
    p = p->cdr;
  } while (p != a);
  return &scm_unspecified;
}

scm *
assq_ref_cache (scm *x, scm *a)
{
  x->hits++;
  scm *c = cache_lookup (x);
  if (c != &scm_undefined) return c;
  int i = 0;
  while (a != &scm_nil && x != a->car->car) {i++;a = a->cdr;}
  if (a == &scm_nil) return &scm_undefined;
  if (i>ENV_HEAD) cache_save (a->car);
  return a->car->cdr;
}
#endif // ENV_CACHE

scm *
assert_defined (scm *x, scm *e)
{
  if (e == &scm_undefined)
    {
      fprintf (stderr, "eval: unbound variable:");
      display_ (stderr, x);
      fprintf (stderr, "\n");
      assert (!"unbound variable");
    }
  return e;
}

scm *
vm_call (function0_t f, scm *p1, scm *p2, scm *a)
{
  scm *frame = cons (r1, cons (r2, cons (r3, cons (r0, &scm_nil))));
  stack = cons (frame, stack);
  r1 = p1;
  r2 = p2;
  r0 = a;
  //if (f == vm_expand_macro_env && g_free.value + GC_SAFETY > ARENA_SIZE)
  if (g_free.value + GC_SAFETY > ARENA_SIZE)
    {
      frame = cons (r1, cons (r2, cons (r3, cons (r0, &scm_nil))));
      stack = cons (frame, stack);
      scm *x = gc (stack);
      *stack = *x;
      frame = car (stack);      
      stack = cdr (stack);
      r1 = car (frame);
      r2 = cadr (frame);
      r3 = caddr (frame);
      r0 = cadddr (frame);
    }

  scm *r = f ();
  frame = car (stack);
  stack = cdr (stack);
  r1 = car (frame);
  r2 = cadr (frame);
  r3 = caddr (frame);
  r0 = cadddr (frame);
  return r; 
}

scm *
evlis_env (scm *m, scm *a)
{
  return vm_call (vm_evlis_env, m, &scm_undefined, a);
}

scm *
apply_env (scm *fn, scm *x, scm *a)
{
  return vm_call (vm_apply_env, fn, x, a);
}

scm *
eval_env (scm *e, scm *a)
{
  return vm_call (vm_eval_env, e, &scm_undefined, a);
}

scm *
expand_macro_env (scm *e, scm *a)
{
  return vm_call (vm_expand_macro_env, e, &scm_undefined, a);
}

scm *
begin_env (scm *e, scm *a)
{
  return vm_call (vm_begin_env, e, &scm_undefined, a);
}

scm *
if_env (scm *e, scm *a)
{
  return vm_call (vm_if_env, e, &scm_undefined, a);
}

scm *
call_lambda (scm *e, scm *x, scm* aa, scm *a) ///((internal))
{
  scm *cl = cons (cons (&scm_closure, x), x);
  r1 = e;
  r0 = cl;
  r2 = a;
  r3 = aa;
  cache_invalidate_range (r0, r3->cdr);
  scm *r = vm_call_lambda ();
  cache_invalidate_range (r0, r3->cdr);
  return r;
}

scm *
vm_evlis_env ()
{
  if (r1 == &scm_nil) return &scm_nil;
  if (r1->type != PAIR) return eval_env (r1, r0);
  r2 = eval_env (car (r1), r0);
  r1 = evlis_env (cdr (r1), r0);
  return cons (r2, r1);
}

scm *
vm_call_lambda ()
{
  return vm_call (vm_begin_env, r1, &scm_undefined, r0);
}

scm *
vm_apply_env ()
{
  if (r1->type != PAIR)
    {
      if (r1->type == FUNCTION) return call (r1, r2);
      if (r1 == &symbol_call_with_values)
        return call (&scm_call_with_values_env, append2 (r2, cons (r0, &scm_nil)));
      if (r1 == &symbol_current_module) return r0;
    }
  else if (r1->car == &symbol_lambda) {
    scm *args = cadr (r1);
    scm *body = cddr (r1);
    scm *p = pairlis (args, r2, r0);
    return call_lambda (body, p, p, r0);
    // r2 = p;
    // cache_invalidate_range (r2, r0->cdr);
    // scm *r = begin_env (cddr (r1), cons (cons (&scm_closure, p), p));
    // cache_invalidate_range (r2, r0->cdr);
    // return r;
  }
  else if (r1->car == &scm_closure) {
    scm *args = caddr (r1);
    scm *body = cdddr (r1);
    scm *aa = cdadr (r1);
    aa = cdr (aa);
    scm *p = pairlis (args, r2, aa);
    return call_lambda (body, p, aa, r0);
    // r2 = p;
    // r3 = aa;
    // cache_invalidate_range (r2, r3->cdr);
    // scm *r = begin_env (body, cons (cons (&scm_closure, p), p));
    // cache_invalidate_range (r2, r3->cdr);
    // return r;
  }
#if BOOT
  else if (r1->car == &scm_label)
    return apply_env (caddr (r1), r2, cons (cons (cadr (r1), caddr (r1)), r0));
#endif
  scm *e = eval_env (r1, r0);
  char const* type = 0;
  if (e == &scm_f || e == &scm_t) type = "bool";
  if (e->type == CHAR) type = "char";
  if (e->type == NUMBER) type = "number";
  if (e->type == STRING) type = "string";
  if (e == &scm_unspecified) type = "*unspecified*";
  if (e == &scm_undefined) type =  "*undefined*";
  if (type)
    {
      fprintf (stderr, "cannot apply: %s: ", type);
      display_ (stderr, e);
      fprintf (stderr, " [");
      display_ (stderr, r1);
      fprintf (stderr, "]\n");
      assert (!"cannot apply");
    }
  return apply_env (e, r2, r0);
}

scm*cstring_to_list (char const* s);

scm *
vm_eval_env ()
{
  switch (r1->type)
    {
    case PAIR:
      {
        if (r1->car == &symbol_quote)
          return cadr (r1);
#if QUASISYNTAX
        if (r1->car == &symbol_syntax)
          return r1;
#endif
        if (r1->car == &symbol_begin)
          return begin_env (r1, r0);
        if (r1->car == &symbol_lambda)
          return make_closure (cadr (r1), cddr (r1), assq (&scm_closure, r0));
        if (r1->car == &scm_closure)
          return r1;
        if (r1->car == &symbol_if)
          return if_env (cdr (r1), r0);
#if !BOOT
        if (r1->car == &symbol_define)
          return define_env (r1, r0);
        if (r1->car == &symbol_define_macro)
          return define_env (r1, r0);
        if (r1->car == &symbol_primitive_load)
          return load_env (r0);
#else
        if (r1->car == &symbol_define) {
        fprintf (stderr, "C DEFINE: ");
        display_ (stderr,
                  r1->cdr->car->type == SYMBOL
                  ? r1->cdr->car->string
                  : r1->cdr->car->car->string);
        fprintf (stderr, "\n");
      }
      assert (r1->car != &symbol_define);
      assert (r1->car != &symbol_define_macro);
#endif
#if 1 //!BOOT
      if (r1->car == &symbol_set_x)
        return set_env_x (cadr (r1), eval_env (caddr (r1), r0), r0);
#else
      assert (r1->car != &symbol_set_x);
#endif
#if QUASIQUOTE
      if (r1->car == &symbol_unquote)
        return eval_env (cadr (r1), r0);
      if (r1->car == &symbol_quasiquote)
        return eval_quasiquote (cadr (r1), add_unquoters (r0));
#endif //QUASIQUOTE
#if QUASISYNTAX
      if (r1->car == &symbol_unsyntax)
        return eval_env (cadr (r1), r0);
      if (r1->car == &symbol_quasisyntax)
        return eval_quasisyntax (cadr (r1), add_unsyntaxers (r0));
#endif //QUASISYNTAX
      scm *x = expand_macro_env (r1, r0);
      if (x != r1)
          return eval_env (x, r0);
      scm *m = evlis_env (r1->cdr, r0);
      return apply_env (r1->car, m, r0);
      }
    case SYMBOL: return assert_defined (r1, assq_ref_cache (r1, r0));
    default: return r1;
    }
}

scm *
vm_expand_macro_env ()
{
  if (car (r1)->type == STRING && string_to_symbol (car (r1)) == &symbol_noexpand)
    return cadr (r1);

  scm *macro;
  scm *expanders;
  if (r1->type == PAIR
      && (macro = lookup_macro (r1->car, r0)) != &scm_f)
    return apply_env (macro, r1->cdr, r0);
  else if (r1->type == PAIR
    && car (r1)->type == SYMBOL
    && ((expanders = assq_ref_cache (&symbol_sc_expander_alist, r0)) != &scm_undefined)
    && ((macro = assq (car (r1), expanders)) != &scm_f))
    {
      scm *sc_expand = assq_ref_cache (&symbol_expand_macro, r0);
      if (sc_expand != &scm_undefined && sc_expand != &scm_f)
        r1 = apply_env (sc_expand, cons (r1, &scm_nil), r0);
    }
  return r1;
}

scm *
vm_begin_env ()
{
  scm *r = &scm_unspecified;
  while (r1 != &scm_nil) {
    if (car (r1)->type == PAIR && caar (r1) == &symbol_begin)
      r1 = append2 (cdar (r1), cdr (r1));
    r = eval_env (r1->car, r0);
    r1 = r1->cdr;
  }
  return r;
}

scm *
vm_if_env ()
{
  scm *x = eval_env (car (r1), r0);
  if (x != &scm_f)
    return eval_env (cadr (r1), r0);
  if (cddr (r1) != &scm_nil)
    return eval_env (caddr (r1), r0);
  return &scm_unspecified;
}

//Helpers

scm *
display (scm *x) ///((arity . n))
{
  scm *e = car (x);
  scm *p = cdr (x);
  int fd = 1;
  if (p->type == PAIR && p->car->type == NUMBER) fd = p->car->hits;
  FILE *f = fd == 1 ? stdout : stderr;
  return display_helper (f, e, false, "", false);
}

scm *
display_ (FILE* f, scm *x)
{
  return display_helper (f, x, false, "", false);
}

scm *
call (scm *fn, scm *x)
{
  if ((fn->function->arity > 0 || fn->function->arity == -1)
      && x != &scm_nil && car (x)->type == VALUES)
    x = cons (x->car->cdr->car, x->cdr);
  if ((fn->function->arity > 1 || fn->function->arity == -1)
      && x != &scm_nil && x->cdr->car->type == VALUES)
    x = cons (x->car, cons (x->cdr->car->cdr->car, x->cdr));
  switch (fn->function->arity)
    {
    case 0: return fn->function->function0 ();
    case 1: return fn->function->function1 (car (x)); 
    case 2: return fn->function->function2 (car (x), cadr (x));
    case 3: return fn->function->function3 (car (x), cadr (x), caddr (x)); 
    case -1: return fn->function->functionn (x);
    }
  return &scm_unspecified;
}

scm *
append2 (scm *x, scm *y)
{
  if (x == &scm_nil) return y;
  assert (x->type == PAIR);
  return cons (car (x), append2 (cdr (x), y));
}

scm *
append (scm *x) ///((arity . n))
 {
  if (x == &scm_nil) return &scm_nil;
  return append2 (car (x), append (cdr (x)));
 }

scm *
make_char (int x)
{
  scm t = {NUMBER, .value=CHAR};
  scm n = {NUMBER, .value=x};  
  return make_cell (&t, &n, &n);
}

scm *
make_macro (scm *name, scm *x)
{
  scm t = {NUMBER, .value=MACRO};
  return make_cell (&t, name->string, x);
}

scm *
make_number (int x)
{
  scm t = {NUMBER, .value=NUMBER};
  scm n = {NUMBER, .value=x};  
  return make_cell (&t, &n, &n);
}

scm *
make_ref (scm *x)
{
  scm t = {NUMBER, .value=REF};
  return make_cell (&t, x, x);
}

scm *
make_string (scm *x)
{
  scm t = {NUMBER, .value=STRING};
  return make_cell (&t, x, 0);
}

scm *
cstring_to_list (char const* s)
{
  scm *p = &scm_nil;
  while (s && *s)
    p = append2 (p, cons (make_char (*s++), &scm_nil));
  return p;
}

scm *
list_of_char_equal_p (scm *a, scm *b)
{
  while (a != &scm_nil && b != &scm_nil && a->car->value == b->car->value) {
    assert (a->car->type == CHAR);
    assert (b->car->type == CHAR);
    a = a->cdr;
    b = b->cdr;
  }
  return (a == &scm_nil && b == &scm_nil) ? &scm_t : &scm_f;
}

scm *
internal_lookup_symbol (scm *s)
{
  scm *x = symbols;
  while (x) {
    // .string and .name is the same field; .name is used as a handy
    // static field initializer.  A string can only be mistaken for a
    // cell with type == PAIR for the one character long, zero-padded
    // #\etx.
    if (x->car->string->type != PAIR)
      x->car->string = cstring_to_list (x->car->name);
    if (list_of_char_equal_p (x->car->string, s) == &scm_t) break;
    x = x->cdr;
  }
  if (x) x = x->car;
  return x;
}

scm *
internal_make_symbol (scm *s)
{
  scm t = {NUMBER, .value=SYMBOL};
  scm *x = make_cell (&t, s, 0);
  symbols = cons (x, symbols);
  return x;
}

scm *
make_symbol (scm *s)
{
  scm *x = internal_lookup_symbol (s);
  return x ? x : internal_make_symbol (s);
}

scm *
make_vector (scm *n)
{
  scm t = {NUMBER, .value=VECTOR};
  scm *v = alloc (n->value);
  scm *x = make_cell (&t, (scm*)(long)n->value, v);
  for (int i=0; i<n->value; i++) x->vector[i] = *vector_entry (&scm_unspecified);
  return x;
}

scm *
values (scm *x) ///((arity . n))
{
  scm *v = cons (0, x);
  v->type = VALUES;
  return v;
}

scm *
call_with_values_env (scm *producer, scm *consumer, scm *a)
{
  scm *v = apply_env (producer, &scm_nil, a);
  if (v->type == VALUES)
    v = v->cdr;
  return apply_env (consumer, v, a);
}

scm *
vector_length (scm *x)
{
  assert (x->type == VECTOR);
  return make_number (x->length);
}

scm *
vector_ref (scm *x, scm *i)
{
  assert (x->type == VECTOR);
  assert (i->value < x->length);
  scm *e = &x->vector[i->value];
  if (e->type == REF) e = e->ref;
  if (e->type == CHAR) e = make_char (e->value);
  if (e->type == NUMBER) e = make_number (e->value);
  return e;
}

scm *
vector_entry (scm *x) {
  if (x->type == PAIR || x->type == SCM || x->type == STRING || x->type == SYMBOL || x->type == VECTOR) x = make_ref (x);
  return x;
}

scm *
vector_set_x (scm *x, scm *i, scm *e)
{
  assert (x->type == VECTOR);
  assert (i->value < x->length);
  x->vector[i->value] = *vector_entry (e);
  return &scm_unspecified;
}

scm *
lookup (scm *s, scm *a)
{
  if (isdigit (s->car->value) || (s->car->value == '-' && s->cdr != &scm_nil)) {
    scm *p = s;
    int sign = 1;
    if (s->car->value == '-') {
      sign = -1;
      p = s->cdr;
    }
    int n = 0;
    while (p != &scm_nil && isdigit (p->car->value)) {
      n *= 10;
      n += p->car->value - '0';
      p = p->cdr;
    }
    if (p == &scm_nil) return make_number (n * sign);
  }
  
  scm *x = internal_lookup_symbol (s);
  if (x) return x;

  if (s->cdr == &scm_nil) {
    if (s->car->value == '\'') return &symbol_quote;
    if (s->car->value == '`') return &symbol_quasiquote;
    if (s->car->value == ',') return &symbol_unquote;
  }
  else if (s->cdr->cdr == &scm_nil) {
    if (s->car->value == ',' && s->cdr->car->value == '@') return &symbol_unquote_splicing;
    if (s->car->value == '#' && s->cdr->car->value == '\'') return &symbol_syntax;
    if (s->car->value == '#' && s->cdr->car->value == '`') return &symbol_quasisyntax;
    if (s->car->value == '#' && s->cdr->car->value == ',') return &symbol_unsyntax;
  }
  else if (s->cdr->cdr->cdr == &scm_nil) {
    if (s->car->value == '#' && s->cdr->car->value == ',' && s->cdr->cdr->car->value == '@') return &symbol_unsyntax_splicing;
    if (s->car->value == 'E' && s->cdr->car->value == 'O' && s->cdr->cdr->car->value == 'F') {
      fprintf (stderr, "mes: got EOF\n");
      return &scm_nil; // `EOF': eval program, which may read stdin
    }
  }

  return internal_make_symbol (s);
}

scm *
lookup_char (int c, scm *a)
{
  return lookup (cons (make_char (c), &scm_nil), a);
}

scm *
list_to_vector (scm *x)
{
  scm n = {NUMBER, .value=length (x)->value};
  scm *v = make_vector (&n);
  scm *p = v->vector;
  while (x != &scm_nil)
    {
      *p++ = *vector_entry (car (x));
      x = cdr (x);
    }
  return v;
}

scm *
newline (scm *p) ///((arity . n))
{
  int fd = 1;
  if (p->type == PAIR && p->car->type == NUMBER) fd = p->car->value;
  FILE *f = fd == 1 ? stdout : stderr;
  fputs ("\n", f);
  return &scm_unspecified;
}

scm *
force_output (scm *p) ///((arity . n))
{
  int fd = 1;
  if (p->type == PAIR && p->car->type == NUMBER) fd = p->car->value;
  FILE *f = fd == 1 ? stdout : stderr;
  fflush (f);
}

scm *
display_helper (FILE* f, scm *x, bool cont, char const *sep, bool quote)
{
  scm *r;
  fprintf (f, "%s", sep);
  switch (x->type)
    {
    case CHAR:
      {
        char const *name = 0;
        if (x->value == char_nul.value) name = char_nul.name;
        else if (x->value == char_backspace.value) name = char_backspace.name;
        else if (x->value == char_tab.value) name = char_tab.name;
        else if (x->value == char_newline.value) name = char_newline.name;
        else if (x->value == char_vt.value) name = char_vt.name;
        else if (x->value == char_page.value) name = char_page.name;
        else if (x->value == char_return.value) name = char_return.name;
        else if (x->value == char_space.value) name = char_space.name;
        if (name) fprintf (f, "#\\%s", name);
        else fprintf (f, "#\\%c", x->value);
        break;
      }
    case MACRO:
      fprintf (f, "(*macro* ");
      display_helper (f, x->macro, cont, sep, quote);
      fprintf (f, ")");
      break;
    case NUMBER: fprintf (f, "%d", x->value); break;
    case PAIR:
      {
        if (car (x) == &scm_circular) {
          fprintf (f, "(*circ* . #-1#)");
          return &scm_unspecified;
        }
        if (car (x) == &scm_closure) {
          fprintf (f, "(*closure* . #-1#)");
          return &scm_unspecified;
        }
        if (car (x) == &scm_quote) {
          fprintf (f, "'");
          return display_helper (f, car (cdr (x)), cont, "", true);
        }
        if (!cont) fprintf (f, "(");
        display_ (f, car (x));
        if (cdr (x) && cdr (x)->type == PAIR)
          display_helper (f, cdr (x), true, " ", false);
        else if (cdr (x) != &scm_nil) {
          fprintf (f, " . ");
          display_ (f, cdr (x));
        }
        if (!cont) fprintf (f, ")");
        break;
      }
    case VECTOR:
      {
        fprintf (f, "#(", x->length);
        for (int i = 0; i < x->length; i++) {
          if (x->vector[i].type == VECTOR
              || (x->vector[i].type == REF
                  && x->vector[i].ref->type == VECTOR))
            fprintf (f, "%s#(...)", i ? " " : "");
          else
            display_helper (f, &x->vector[i], false, i ? " " : "", false);
        }
        fprintf (f, ")");
        break;
      }
    case REF: display_helper (f, x->ref, cont, "", true); break;
    case FUNCTION: fprintf (f, "#<procedure %s>", x->name); ;break;
    case BROKEN_HEART: fprintf (f, "<3"); break;
    default:
      if (x->string)
        {
          scm *p = x->string;
          assert (p);
          while (p != &scm_nil) {
            assert (p->car->type == CHAR);
            fputc (p->car->value, f);
            p = p->cdr;
          }
        }
      else if (x->type != PAIR && x->name) fprintf (f, "%s", x->name);
    }
  return &scm_unspecified;
}

// READ

FILE *g_stdin;
int
getchar ()
{
  return getc (g_stdin);
}

int
ungetchar (int c)
{
  return ungetc (c, g_stdin);
}

int
peekchar ()
{
  int c = getchar ();
  ungetchar (c);
  return c;
}

scm *
peek_char ()
{
  return make_char (peekchar ());
}

scm *
read_char ()
{
  return make_char (getchar ());
}

scm *
write_char (scm *x) ///((arity . n))
{
  scm *c = car (x);
  scm *p = cdr (x);
  int fd = 1;
  if (p->type == PAIR && p->car->type == NUMBER) fd = p->car->value;
  FILE *f = fd == 1 ? stdout : stderr;
  assert (c->type == NUMBER || c->type == CHAR);
  fputc (c->value, f);
  return c;
}

scm *
unget_char (scm *c)
{
  assert (c->type == NUMBER || c->type == CHAR);
  ungetchar (c->value);
  return c;
}

int
readcomment (int c)
{
  if (c == '\n') return c;
  return readcomment (getchar ());
}

int
readblock (int c)
{
  if (c == '!' && peekchar () == '#') return getchar ();
  return readblock (getchar ());
}

scm *
readword (int c, scm *w, scm *a)
{
  if (c == EOF && w == &scm_nil) return &scm_nil;
  if (c == '\n' && w == &scm_nil) return readword (getchar (), w, a);
  if (c == '\n' && w->car->value == '.' && w->cdr == &scm_nil) return &scm_dot;
  if (c == EOF || c == '\n') return lookup (w, a);
  if (c == ' ') return readword ('\n', w, a);
  if (c == '"' && w == &scm_nil) return readstring ();
  if (c == '"') {ungetchar (c); return lookup (w, a);}
  if (c == '(' && w == &scm_nil) return readlist (a);
  if (c == '(') {ungetchar (c); return lookup (w, a);}
  if (c == ')' && w == &scm_nil) {ungetchar (c); return &scm_nil;}
  if (c == ')') {ungetchar (c); return lookup (w, a);}
  if (c == ',' && peekchar () == '@') {getchar (); return cons (lookup (symbol_unquote_splicing.string, a),
                                                                   cons (readword (getchar (), w, a),
                                                                         &scm_nil));}
  if ((c == '\''
       || c == '`'
       || c == ',')
      && w == &scm_nil) {return cons (lookup_char (c, a),
                                     cons (readword (getchar (), w, a),
                                           &scm_nil));}
  if (c == '#' && peekchar () == ',' && w == &scm_nil) {
    getchar ();
    if (peekchar () == '@'){getchar (); return cons (lookup (symbol_unsyntax_splicing.string, a),
                                                     cons (readword (getchar (), w, a),
                                                           &scm_nil));}
    return cons (lookup (symbol_unsyntax.string, a), cons (readword (getchar (), w, a), &scm_nil));
  }
  if (c == '#' && (peekchar () == '\'' || peekchar () == '`') && w == &scm_nil) {
    c = getchar ();
    return cons (lookup (cons (make_char ('#'), cons (make_char (c), &scm_nil)), a),
                 cons (readword (getchar (), w, a), &scm_nil));}
  if (c == ';') {readcomment (c); return readword ('\n', w, a);}
  if (c == '#' && peekchar () == 'x') {getchar (); return read_hex ();}
  if (c == '#' && peekchar () == '\\') {getchar (); return read_character ();}
  if (c == '#' && w == &scm_nil && peekchar () == '(') {getchar (); return list_to_vector (readlist (a));}
  if (c == '#' && peekchar () == '(') {ungetchar (c); return lookup (w, a);}
  if (c == '#' && peekchar () == '!') {getchar (); readblock (getchar ()); return readword (getchar (), w, a);}
  return readword (getchar (), append2 (w, cons (make_char (c), &scm_nil)), a);
}

scm *
read_hex ()
{
  int n = 0;
  int c = peekchar ();
  while ((c >= '0' && c <= '9')
         || (c >= 'A' && c <= 'F')
         || (c >= 'a' && c <= 'f')) {
    n <<= 4;
    if (c >= 'a') n += c - 'a' + 10;
    else if (c >= 'A') n += c - 'A' + 10;
    else n+= c - '0';
    getchar ();
    c = peekchar ();
  }
  return make_number (n);
}

scm *
read_character ()
{
  int c = getchar ();
  if (c >= '0' && c <= '7'
      && peekchar () >= '0' && peekchar () <= '7') {
    c = c - '0';
    while (peekchar () >= '0' && peekchar () <= '7') {
      c <<= 3;
      c += getchar () - '0';
    }
  }
  else if (c >= 'a' && c <= 'z'
      && peekchar () >= 'a' && peekchar () <= 'z') {
    char buf[10];
    char *p = buf;
    *p++ = c;
    while (peekchar () >= 'a' && peekchar () <= 'z') {
      *p++ = getchar ();
    }
    *p = 0;
    if (!strcmp (buf, char_nul.name)) c = char_nul.value;
    else if (!strcmp (buf, char_backspace.name)) c = char_backspace.value;
    else if (!strcmp (buf, char_tab.name)) c = char_tab.value;
    else if (!strcmp (buf, char_newline.name)) c = char_newline.value;
    else if (!strcmp (buf, char_vt.name)) c = char_vt.value;
    else if (!strcmp (buf, char_page.name)) c = char_page.value;
    else if (!strcmp (buf, char_return.name)) c = char_return.value;
    else if (!strcmp (buf, char_space.name)) c = char_space.value;
    else {
      fprintf (stderr, "char not supported: %s\n", buf);
      assert (!"char not supported");
    }
  }
  return make_char (c);
}

scm *
append_char (scm *x, int i)
{
  return append2 (x, cons (make_char (i), &scm_nil));
}

scm *
readstring ()
{
  scm *p = &scm_nil;
  int c = getchar ();
  while (true) {
    if (c == '"') break;
    if (c == '\\' && peekchar () == '"') p = append_char (p, getchar ());
    else if (c == '\\' && peekchar () == 'n') {getchar (); p = append_char (p, '\n');}
    else if (c == EOF) assert (!"EOF in string");
    else p = append_char (p, c);
    c = getchar ();
  }
  return make_string (p);
}

int
eat_whitespace (int c)
{
  while (c == ' ' || c == '\t' || c == '\n') c = getchar ();
  if (c == ';') return eat_whitespace (readcomment (c));
  if (c == '#' && peekchar () == '!') {getchar (); readblock (getchar ()); return eat_whitespace (getchar ());}
  return c;
}

scm *
readlist (scm *a)
{
  int c = getchar ();
  c = eat_whitespace (c);
  if (c == ')') return &scm_nil;
  scm *w = readword (c, &scm_nil, a);
  if (w == &scm_dot)
    return car (readlist (a));
  return cons (w, readlist (a));
}

scm *
read_env (scm *a)
{
  return readword (getchar (), &scm_nil, a);
}

scm *
acons (scm *key, scm *value, scm *alist)
{
  return cons (cons (key, value), alist);
}

scm *
add_environment (scm *a, char const *name, scm *x)
{
  return acons (make_symbol (cstring_to_list (name)), x, a);
}

scm *
mes_environment () ///((internal))
{
  scm *a = &scm_nil;

  // setup GC
  g_cells = (scm*)malloc (ARENA_SIZE*sizeof(scm));
  g_news = (scm*)malloc (ARENA_SIZE*sizeof(scm));
  g_cells[0].type = VECTOR;
  g_cells[0].length = ARENA_SIZE - 1;
  g_cells[0].vector = &g_cells[1];
  g_news[0].type = VECTOR;
  g_news[0].length = ARENA_SIZE - 1;
  g_news[0].vector = &g_news[1];

  g_cells++;
  g_news++;
  // a = add_environment (a, "%free", &g_free); hihi, gets <3 moved
  // a = add_environment (a, "%the-cells", g_cells);
  // a = add_environment (a, "%new-cells", g_news);

  #include "mes.symbols.i"

#if BOOT
  symbols = cons (&scm_label, symbols);
  a = cons (cons (&scm_label, &scm_t), a);
#endif
  a = cons (cons (&symbol_begin, &scm_begin), a);

#if MES_FULL
#include "posix.environment.i"
#include "string.environment.i"
#include "math.environment.i"
#include "lib.environment.i"
#include "mes.environment.i"
//#include "quasiquote.environment.i"
#include "define.environment.i"
#include "type.environment.i"
#else
   a = add_environment (a, "cons", &scm_cons);
   a = add_environment (a, "eq?", &scm_eq_p);
   a = add_environment (a, "display", &scm_display);
   a = add_environment (a, "newline", &scm_newline);

#if !MES_MINI
   a = add_environment (a, "*", &scm_multiply);
   a = add_environment (a, "list", &scm_list);
   //
   a = add_environment (a, "car", &scm_car);
   a = add_environment (a, "cdr", &scm_cdr);
   a = add_environment (a, "+", &scm_plus);
   a = add_environment (a, "quote", &scm_quote);
   a = add_environment (a, "null?", &scm_null_p);
   a = add_environment (a, "=", &scm_is_p);

   // a = add_environment (a, "gc", &scm_gc);
   // a = add_environment (a, "apply-env", &scm_apply_env);
   // a = add_environment (a, "eval-env", &scm_eval_env);
   // a = add_environment (a, "cadr", &scm_cadr);
#endif
#endif

  a = add_environment (a, "sc-expand", &scm_f);

  a = cons (cons (&scm_closure, a), a);

  internal_lookup_symbol (&scm_nil);

  gc_bump (); // secure the .string of builtins, scm and symbols
  r0 = a;
  r1 = make_char (0);
  r2 = make_char (0);
  r3 = make_char (0);
  stack = cons (&scm_nil, &scm_nil);

  return a;
}

scm *
make_lambda (scm *args, scm *body)
{
  return cons (&symbol_lambda, cons (args, body));
}

scm *
make_closure (scm *args, scm *body, scm *a)
{
  return cons (&scm_closure, cons (cons (&scm_circular, a), cons (args, body)));
}

scm *
lookup_macro (scm *x, scm *a)
{
  if (x->type != SYMBOL) return &scm_f;
  scm *m = assq_ref_cache (x, a);
  if (macro_p (m) == &scm_t) return m->macro;
  return &scm_f;
}

scm *
read_input_file_env (scm *e, scm *a)
{
  if (e == &scm_nil) return e;
  return cons (e, read_input_file_env (read_env (a), a));
}

scm *
load_env (scm *a)
{
  return begin_env (read_input_file_env (read_env (a), a), a);
}

#include "type.c"
#include "define.c"
#include "lib.c"
#include "math.c"
#include "posix.c"
#include "quasiquote.c"
#include "string.c"

int
main (int argc, char *argv[])
{
  if (argc > 1 && !strcmp (argv[1], "--help")) return puts ("Usage: mes < FILE\n");
  if (argc > 1 && !strcmp (argv[1], "--version")) return puts ("Mes 0.2\n");
  g_stdin = stdin;
  scm *a = mes_environment ();
  display_ (stderr, load_env (a));
  fputs ("", stderr);
  fprintf (stderr, "\nstats: [%d]\n", g_free.value);
  return 0;
}
