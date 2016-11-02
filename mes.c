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

enum type {CHAR, MACRO, NUMBER, PAIR, SCM, STRING, SYMBOL, REF, VALUES, VECTOR,
           FUNCTION0, FUNCTION1, FUNCTION2, FUNCTION3, FUNCTIONn};
struct scm_t;
typedef struct scm_t* (*function0_t) (void);
typedef struct scm_t* (*function1_t) (struct scm_t*);
typedef struct scm_t* (*function2_t) (struct scm_t*, struct scm_t*);
typedef struct scm_t* (*function3_t) (struct scm_t*, struct scm_t*, struct scm_t*);
typedef struct scm_t* (*functionn_t) (struct scm_t*);

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
    function0_t function0;
    function1_t function1;
    function2_t function2;
    function3_t function3;
    functionn_t functionn;
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
#include "quasiquote.environment.h"
#include "string.environment.h"
#include "type.environment.h"

scm *display_ (FILE* f, scm *x);
scm *display_helper (FILE*, scm*, bool, char const*, bool);

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
scm scm_begin = {SCM, "begin"};

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

scm *
alloc (int n)
{
  return (scm*)malloc (n * sizeof (scm));
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
  scm *p = assq (x, a);
  if (p->type != PAIR)
    {
      fprintf (stderr, "set!: unbound variable:");
      display_ (stderr, x);
      fprintf (stderr, "\n");
      assert (!"unbound variable");
    }
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
  while (a != &scm_nil && eq_p (x, a->car->car) == &scm_f) a = a->cdr;
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
evlis_env (scm *m, scm *a)
{
  if (m == &scm_nil) return &scm_nil;
  if (m->type != PAIR) return builtin_eval (m, a);
  scm *e = builtin_eval (car (m), a);
  return cons (e, evlis_env (cdr (m), a));
}

scm *
apply_env (scm *fn, scm *x, scm *a)
{
  if (fn->type != PAIR)
    {
      if (fn == &scm_car) return x->car->car;
      if (fn == &scm_cdr) return x->car->cdr;
      if (builtin_p (fn) == &scm_t)
        return call (fn, x);
      if (eq_p (fn, &symbol_call_with_values) == &scm_t)
        return call (&scm_call_with_values_env, append2 (x, cons (a, &scm_nil)));
      if (fn == &symbol_current_module) return a;
    }
  else if (fn->car == &symbol_lambda) {
    scm *p = pairlis (cadr (fn), x, a);
    cache_invalidate_range (p, a->cdr);
    scm *r = begin_env (cddr (fn), cons (cons (&scm_closure, p), p));
    cache_invalidate_range (p, a->cdr);
    return r;
  }
  else if (fn->car == &scm_closure) {
    scm *args = caddr (fn);
    scm *body = cdddr (fn);
    a = cdadr (fn);
    a = cdr (a);
    scm *p = pairlis (args, x, a);
    cache_invalidate_range (p, a->cdr);
    scm *r = begin_env (body, cons (cons (&scm_closure, p), p));
    cache_invalidate_range (p, a->cdr);
    return r;
  }
#if BOOT
  else if (fn->car == &scm_label)
    return apply_env (caddr (fn), x, cons (cons (cadr (fn), caddr (fn)), a));
#endif
  scm *efn = builtin_eval (fn, a);
  if (efn == &scm_f || efn == &scm_t) assert (!"apply bool");
  if (efn->type == NUMBER) assert (!"apply number");
  if (efn->type == STRING) assert (!"apply string");
  if (efn == &scm_unspecified) assert (!"apply *unspecified*");
  return apply_env (efn, x, a);
}

scm *
builtin_eval (scm *e, scm *a)
{
  if (builtin_p (e) == &scm_t) return e;
  if (e->type == SCM) return e;

  e = expand_macro_env (e, a);

  if (e->type == SYMBOL) {
    scm *y = assq_ref_cache (e, a);
    if (y == &scm_undefined) {
      fprintf (stderr, "eval: unbound variable:");
      display_ (stderr, e);
      fprintf (stderr, "\n");
      assert (!"unbound variable");
    }
    return y;
  }
  else if (e->type != PAIR)
    return e;
  else if (e->car->type != PAIR)
    {
      if (e->car->type == STRING && string_to_symbol (e->car) == &symbol_noexpand)
        e = cadr (e);
      else
        e = sc_expand_env (e, a);
      if (e->car == &symbol_quote)
        return cadr (e);
#if QUASISYNTAX
      if (e->car == &symbol_syntax)
        return e;
#endif
      if (e->car == &symbol_begin)
        return begin_env (e, a);
      if (e->car == &symbol_lambda)
        return make_closure (cadr (e), cddr (e), assq (&scm_closure, a));
      if (e->car == &scm_closure)
        return e;
      if (e->car == &symbol_if)
        return builtin_if (cdr (e), a);
#if !BOOT
      if (e->car == &symbol_define)
        return define_env (e, a);
      if (e->car == &symbol_define_macro)
        return define_env (e, a);
#else
      if (e->car == &symbol_define) {
        fprintf (stderr, "C DEFINE: ");
        display_ (stderr,
                  e->cdr->car->type == SYMBOL
                  ? e->cdr->car->string
                  : e->cdr->car->car->string);
        fprintf (stderr, "\n");
      }
      assert (e->car != &symbol_define);
      assert (e->car != &symbol_define_macro);
#endif
      if (e->car == &symbol_set_x)
        return set_env_x (cadr (e), builtin_eval (caddr (e), a), a);
#if QUASIQUOTE
      if (e->car == &symbol_unquote)
        return builtin_eval (cadr (e), a);
      if (e->car == &symbol_quasiquote)
        return eval_quasiquote (cadr (e), add_unquoters (a));
#endif //QUASIQUOTE
#if QUASISYNTAX
      if (e->car == &symbol_unsyntax)
        return builtin_eval (cadr (e), a);
      if (e->car == &symbol_quasisyntax)
        return eval_quasisyntax (cadr (e), add_unsyntaxers (a));
#endif //QUASISYNTAX
    }
  return apply_env (e->car, evlis_env (e->cdr, a), a);
}

scm *
expand_macro_env (scm *e, scm *a)
{
  scm *macro;
  if (e->type == PAIR
      && (macro = lookup_macro (e->car, a)) != &scm_f)
    return expand_macro_env (apply_env (macro, e->cdr, a), a);
  return e;
}

scm *
sc_expand_env (scm *e, scm *a)
{
  scm *expanders;
  scm *macro;
  if (e->type == PAIR
    && car (e)->type == SYMBOL

    && car (e) != &symbol_lambda
    && car (e) != &symbol_set_x
    && car (e) != &symbol_if
    && car (e) != &symbol_begin
    && car (e) != &symbol_define

    && car (e) != &symbol_quasiquote
    && car (e) != &symbol_quote
    && car (e) != &symbol_unquote
    && car (e) != &symbol_unquote_splicing
    && ((expanders = assq_ref_cache (&symbol_sc_expander_alist, a)) != &scm_undefined)
    && ((macro = assq (car (e), expanders)) != &scm_f))
    {
      scm *sc_expand = assq_ref_cache (&symbol_expand_macro, a);
      if (sc_expand != &scm_undefined && sc_expand != &scm_f)
        {
          e = apply_env (sc_expand, cons (e, &scm_nil), a);
          return expand_macro_env (e, a);
        }
    }
  return e;
}

scm *
begin_env (scm *e, scm *a)
{
  scm *r = &scm_unspecified;
  while (e != &scm_nil) {
    r = builtin_eval (e->car, a);
    e = e->cdr;
  }
  return r;
}

scm *
builtin_if (scm *e, scm *a)
{
  if (builtin_eval (car (e), a) != &scm_f)
    return builtin_eval (cadr (e), a);
  if (cddr (e) != &scm_nil)
    return builtin_eval (caddr (e), a);
  return &scm_unspecified;
}

//Helpers

scm *
display (scm *x) ///((args . n))
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
  if (fn->type == FUNCTION0)
    return fn->function0 ();
  if (x != &scm_nil && x->car->type == VALUES)
    x = cons (x->car->cdr->car, x->cdr);
  if (fn->type == FUNCTION1)
    return fn->function1 (car (x));
  if (x != &scm_nil && x->cdr->car->type == VALUES)
    x = cons (x->car, cons (x->cdr->car->cdr->car, x->cdr));
  if (fn->type == FUNCTION2)
    return fn->function2 (car (x), cadr (x));
  if (fn->type == FUNCTION3)
    return fn->function3 (car (x), cadr (x), caddr (x));
  if (fn->type == FUNCTIONn)
    return fn->functionn (x);
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
append (scm *x) ///((args . n))
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

scm *symbols = 0;

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
values (scm *x) ///((args . n))
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
newline (scm *p) ///((args . n))
{
  int fd = 1;
  if (p->type == PAIR && p->car->type == NUMBER) fd = p->car->value;
  FILE *f = fd == 1 ? stdout : stderr;
  fputs ("\n", f);
  return &scm_unspecified;
}

scm *
force_output (scm *p) ///((args . n))
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
  if (x->type == CHAR && x->value == char_nul.value) fprintf (f, "#\\%s", char_nul.name);
  else if (x->type == CHAR && x->value == char_backspace.value) fprintf (f, "#\\%s", char_backspace.name);
  else if (x->type == CHAR && x->value == char_tab.value) fprintf (f, "#\\%s", char_tab.name);
  else if (x->type == CHAR && x->value == char_newline.value) fprintf (f, "#\\%s", char_newline.name);
  else if (x->type == CHAR && x->value == char_vt.value) fprintf (f, "#\\%s", char_vt.name);
  else if (x->type == CHAR && x->value == char_page.value) fprintf (f, "#\\%s", char_page.name);
  else if (x->type == CHAR && x->value == char_return.value) fprintf (f, "#\\%s", char_return.name);
  else if (x->type == CHAR && x->value == char_space.value) fprintf (f, "#\\%s", char_space.name);
  else if (x->type == CHAR) fprintf (f, "#\\%c", x->value);
  else if (x->type == MACRO) {
    fprintf (f, "(*macro* ");
    display_helper (f, x->macro, cont, sep, quote);
    fprintf (f, ")");
  }
  else if (x->type == NUMBER) fprintf (f, "%d", x->value);
  else if (x->type == PAIR) {
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
    if (cdr (x)->type == PAIR)
      display_helper (f, cdr (x), true, " ", false);
    else if (cdr (x) != &scm_nil) {
      fprintf (f, " . ");
      display_ (f, cdr (x));
    }
    if (!cont) fprintf (f, ")");
  }
  else if (x->type == VECTOR) {
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
  }
  else if (x->type == REF) display_helper (f, x->ref, cont, "", true);
  else if (builtin_p (x) == &scm_t) fprintf (f, "#<procedure %s>", x->name);
  else if (x->type != PAIR && x->string) {
    scm *p = x->string;
    assert (p);
    while (p != &scm_nil) {
      assert (p->car->type == CHAR);
      fputc (p->car->value, f);
      p = p->cdr;
    }
  }
  else if (x->type != PAIR && x->name) fprintf (f, "%s", x->name);

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
write_char (scm *x) ///((args . n))
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
add_environment (scm *a, char const *name, scm *x)
{
  return cons (cons (make_symbol (cstring_to_list (name)), x), a);
}

scm *
mes_environment () ///((internal))
{
  scm *a = &scm_nil;

  #include "mes.symbols.i"

#if BOOT
  symbols = cons (&scm_label, symbols);
  a = cons (cons (&scm_label, &scm_t), a);
#endif
  a = cons (cons (&symbol_begin, &scm_begin), a);

#include "string.environment.i"
#include "math.environment.i"
#include "lib.environment.i"
#include "mes.environment.i"
#include "define.environment.i"
#include "type.environment.i"

  a = add_environment (a, "sc-expand", &scm_f);

  a = cons (cons (&scm_closure, a), a);
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
read_file_env (scm *e, scm *a)
{
  if (e == &scm_nil) return e;
  return cons (e, read_file_env (read_env (a), a));
}

scm *
load_file_env (scm *a)
{
  return begin_env (read_file_env (read_env (a), a), a);
}

#include "type.c"
#include "define.c"
#include "lib.c"
#include "math.c"
#include "quasiquote.c"
#include "string.c"

int
main (int argc, char *argv[])
{
  if (argc > 1 && !strcmp (argv[1], "--help")) return puts ("Usage: mes < FILE\n");
  if (argc > 1 && !strcmp (argv[1], "--version")) return puts ("Mes 0.1\n");
  g_stdin = stdin;
  scm *a = mes_environment ();
  display_ (stderr, load_file_env (a));
  fputs ("", stderr);
  return 0;
}
