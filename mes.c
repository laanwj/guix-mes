/*
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

// (setq comment-start "//")
// (setq comment-end "")
/*
 * The Maxwell Equations of Software -- John McCarthy page 13
 * http://www.softwarepreservation.org/projects/LISP/book/LISP%201.5%20Programmers%20Manual.pdf
 */

#define _GNU_SOURCE
#include <assert.h>
#include <ctype.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <stdbool.h>

#define DEBUG 0
#define MES_FULL 1

enum type {CHAR, MACRO, NUMBER, PAIR, STRING, SYMBOL, VALUES, VECTOR,
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
    char *name;
    struct scm_t* car;
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
    struct scm_t** vector;
  };
} scm;

#define MES_C 1
#include "mes.h"

scm *display_helper (scm*, bool, char*, bool);
bool
symbol_eq (scm *x, char *s)
{
  return x->type == SYMBOL && !strcmp (x->name, s);
}

scm scm_nil = {SYMBOL, "()"};
scm scm_dot = {SYMBOL, "."};
scm scm_t = {SYMBOL, "#t"};
scm scm_f = {SYMBOL, "#f"};
scm scm_unspecified = {SYMBOL, "*unspecified*"};

scm symbol_closure = {SYMBOL, "*lambda*"};
scm symbol_circ = {SYMBOL, "*circ*"};
scm symbol_lambda = {SYMBOL, "lambda"};
scm symbol_begin = {SYMBOL, "begin"};
scm symbol_list = {SYMBOL, "list"};
scm symbol_cond = {SYMBOL, "cond"};
scm symbol_quote = {SYMBOL, "quote"};
scm symbol_quasiquote = {SYMBOL, "quasiquote"};
scm symbol_unquote = {SYMBOL, "unquote"};
scm symbol_unquote_splicing = {SYMBOL, "unquote-splicing"};

scm symbol_call_with_values = {SYMBOL, "call-with-values"};
scm symbol_current_module = {SYMBOL, "current-module"};
scm symbol_define = {SYMBOL, "define"};
scm symbol_define_macro = {SYMBOL, "define-macro"};
scm symbol_set_x = {SYMBOL, "set!"};

// PRIMITIVES

scm *
atom_p (scm *x)
{
  return x->type == PAIR ? &scm_f : &scm_t;
}

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
cons (scm *x, scm *y)
{
  scm *p = malloc (sizeof (scm));
  p->type = PAIR;
  p->car = x;
  p->cdr = y;
  return p;
}

scm *
eq_p (scm *x, scm *y)
{
  return (x == y
          || (x->type == CHAR && y->type == CHAR
              && x->value == y->value)
          || (x->type == NUMBER && y->type == NUMBER
              && x->value == y->value)
          // FIXME: alist lookup symbols
          || (atom_p (x) == &scm_t
              && atom_p (y) == &scm_t
              && x->type != CHAR
              && y->type != CHAR
              && x->type != NUMBER
              && y->type != NUMBER
              && x->type != STRING
              && y->type != STRING
              && x->type != VECTOR
              && y->type != VECTOR
              && !strcmp (x->name, y->name)))
    ? &scm_t : &scm_f;
}

scm *
macro_p (scm *x)
{
  return x->type == MACRO ? &scm_t : &scm_f;
}

scm *
null_p (scm *x)
{
  return eq_p (x, &scm_nil);
}

scm *
pair_p (scm *x)
{
  return x->type == PAIR ? &scm_t : &scm_f;
}

scm *
set_cdr_x (scm *x, scm *e)
{
  assert (x->type == PAIR);
  x->cdr = e;
  return &scm_unspecified;
}

scm *
set_x (scm *x, scm *e, scm *a)
{
  return set_cdr_x (assq (x, a), e);
}

scm *
set_env_x (scm *x, scm *e, scm *a)
{
  return set_cdr_x (assq (x, a), e);
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
unquote (scm *x) //int must not add to environment
{
  return cons (&symbol_unquote, x);
}
scm *unquote (scm *x);
scm scm_unquote = {FUNCTION1, .name="unquote", .function1=&unquote};

scm *
unquote_splicing (scm *x) //int must not add to environment
{
  return cons (&symbol_unquote_splicing, x);
}
scm *unquote_splicing (scm *x);
scm scm_unquote_splicing = {FUNCTION1, .name="unquote-splicing", .function1=&unquote_splicing};

//Library functions

// Derived, non-primitives
scm *caar (scm *x) {return car (car (x));}
scm *cadr (scm *x) {return car (cdr (x));}
scm *cdar (scm *x) {return cdr (car (x));}
scm *cddr (scm *x) {return cdr (cdr (x));}
scm *caadr (scm *x) {return car (car (cdr (x)));}
scm *caddr (scm *x) {return car (cdr (cdr (x)));}
scm *cdadr (scm *x) {return cdr (car (cdr (x)));}
scm *cadar (scm *x) {return car (cdr (car (x)));}
scm *cddar (scm *x) {return cdr (cdr (car (x)));}
scm *cdddr (scm *x) {return cdr (cdr (cdr (x)));}

scm *
pairlis (scm *x, scm *y, scm *a)
{
  if (x == &scm_nil)
    return a;
  if (atom_p (x) == &scm_t)
    return cons (cons (x, y), a);
  return cons (cons (car (x), car (y)),
               pairlis (cdr (x), cdr (y), a));
}

scm *
assq (scm *x, scm *a)
{
  if (a == &scm_nil) {
#if DEBUG
    printf ("alist miss: %s\n", x->name);
#endif
    return &scm_f;
  }
  if (eq_p (caar (a), x) == &scm_t)
    return car (a);
  return assq (x, cdr (a));
}

scm *
apply_env (scm *fn, scm *x, scm *a)
{
#if DEBUG
  printf ("\napply_env fn=");
  display (fn);
  printf (" x=");
  display (x);
  puts ("");
#endif
  scm *macro;
  if (atom_p (fn) != &scm_f)
    {
      if (fn == &symbol_current_module) return a;
      if (eq_p (fn, &symbol_call_with_values) == &scm_t)
        return call (&scm_call_with_values_env, append2 (x, cons (a, &scm_nil)));
      if (builtin_p (fn) == &scm_t)
        return call (fn, x);
    }
  else if (car (fn) == &symbol_lambda)
    return eval (cons (&symbol_begin, cddr (fn)), pairlis (cadr (fn), x, a));
  else if (car (fn) == &symbol_closure) {
    scm *args = caddr (fn);
    scm *body = cdddr (fn);
    a = cdadr (fn);
    return eval (cons (&symbol_begin, body), pairlis (args, x, a));
  }
  else if ((macro = lookup_macro (car (fn), a)) != &scm_f) {
    scm *r = apply_env (eval (macro, a), cdr (fn), a);
    scm *e = eval (r, a);
    return apply_env (e, x, a);
  }
  scm *efn = eval (fn,  a);
  if (efn->type == NUMBER || efn == &scm_f || efn == &scm_t) assert (!"apply bool");
  return apply_env (efn, x, a);
}

scm *make_symbol (char const *s);

scm *
eval (scm *e, scm *a)
{
#if DEBUG
  printf ("\neval e=");
  display (e);
  puts ("");
#endif
  scm *macro;
  if (e->type == SYMBOL) {
    scm *y = assq (e, a);
    if (y == &scm_f) {
      //return e;
      printf ("eval: no such symbol: %s\n", e->name);
      assert (!"unknown symbol");
    }
    return cdr (y);
  }
  else if (pair_p (e) == &scm_f)
    return e;
  else if (atom_p (car (e)) == &scm_t)
    {
      if (car (e) == &symbol_quote)
        return cadr (e);
      if (car (e) == &symbol_begin)
        {
          scm *body = cdr (e);
          scm *defines = &scm_nil;
          while (body != &scm_nil) {
            e = car (body);
            body = cdr (body);
            if (e->type == PAIR
                && (eq_p (car (e), &symbol_define) == &scm_t
                    || eq_p (car (e), &symbol_define_macro) == &scm_t)) {
              defines = append2 (defines, cons (def (e), &scm_nil));
              e = &scm_unspecified;
            }
            else break;
          }
          a = append2 (defines, a);
          while (defines != &scm_nil) {
            scm *name = caar (defines);
            scm *entry = assq (name, a);
            scm *x = cdar (defines);
            set_cdr_x (entry, cdr (define (x, a)));
            defines = cdr (defines);
          }
          scm *fubar = cons (&scm_dot, &scm_dot);
          scm *r = eval (e, cons (fubar, a));
          if (r->type == PAIR && macro_p (cdr (r)))
            a = cons (r, a); // macros defining macros...
          if (body == &scm_nil) return r;
          return eval (cons (&symbol_begin, body), a);
        }
      if (car (e) == &symbol_lambda)
        return make_closure (cadr (e), cddr (e), a);
      if (car (e) == &symbol_closure)
        return e;
      if (car (e) == &symbol_unquote)
        return eval (cadr (e), a);
      if (car (e) == &symbol_quasiquote)
        return eval_quasiquote (cadr (e), add_unquoters (a));
      if (car (e) == &symbol_cond)
        return evcon (cdr (e), a);
      if (eq_p (car (e), &symbol_define_macro) == &scm_t)
        return define (e, a);
      if ((macro = lookup_macro (car (e), a)) != &scm_f)
        return eval (apply_env (macro, cdr (e), a), a);
      if (car (e) == &symbol_set_x)
        return set_env_x (cadr (e), eval (caddr (e), a), a);
    }
  return apply_env (car (e), evlis (cdr (e), a), a);
}

scm *
evcon (scm *c, scm *a)
{
  if (c == &scm_nil) return &scm_unspecified;
  scm *clause = car (c);
  scm *expr = eval (car (clause), a);
  if (expr != &scm_f) {
    if (cdr (clause) == &scm_nil)
      return expr;
    if (cddr (clause) == &scm_nil)
      return eval (cadr (clause), a);
    eval (cadr (clause), a);
    return evcon (cons (cons (&scm_t, cddr (clause)), &scm_nil), a);
  }
  return evcon (cdr (c), a);
}

scm *
evlis (scm *m, scm *a)
{
  if (m == &scm_nil) return &scm_nil;
  if (m->type != PAIR) return eval (m, a);
  scm *e = eval (car (m), a);
  return cons (e, evlis (cdr (m), a));
}

scm *
eval_quasiquote (scm *e, scm *a)
{
  if (e == &scm_nil) return e;
  else if (atom_p (e) == &scm_t) return e;
  else if (eq_p (car (e), &symbol_unquote) == &scm_t)
    return eval (cadr (e), a);
  else if (e->type == PAIR && e->car->type == PAIR
           && eq_p (caar (e), &symbol_unquote_splicing) == &scm_t)
      return append2 (eval (cadar (e), a), eval_quasiquote (cdr (e), a));
  return cons (eval_quasiquote (car (e), a), eval_quasiquote (cdr (e), a));
}

//Helpers

scm *
builtin_p (scm *x)
{
  return (x->type == FUNCTION0
          || x->type == FUNCTION1
          || x->type == FUNCTION2
          || x->type == FUNCTION3
          || x->type == FUNCTIONn)
    ? &scm_t : &scm_f;
}

scm *
boolean_p (scm *x)
{
  return (x == &scm_t || x == &scm_f) ? &scm_t : &scm_f;
}

scm *
char_p (scm *x)
{
  return x->type == CHAR ? &scm_t : &scm_f;
}

scm *
number_p (scm *x)
{
  return x->type == NUMBER ? &scm_t : &scm_f;
}

scm *
string_p (scm *x)
{
  return x->type == STRING ? &scm_t : &scm_f;
}

scm *
symbol_p (scm *x)
{
  return (x->type == SYMBOL
          && x != &scm_nil
          && x != &scm_f
          && x != &scm_t) ? &scm_t : &scm_f;
}

scm *
vector_p (scm *x)
{
  return x->type == VECTOR ? &scm_t : &scm_f;
}

scm *
display (scm *x)
{
  return display_helper (x, false, "", false);
}

scm *
call (scm *fn, scm *x)
{
  if (fn->type == FUNCTION0)
    return fn->function0 ();
  if (x->car->type == VALUES)
    x = cons (x->car->cdr->car, &scm_nil);
  if (fn->type == FUNCTION1)
    return fn->function1 (car (x));
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
append (scm *x/*...*/)
 {
  if (x == &scm_nil) return &scm_nil;
  return append2 (car (x), append (cdr (x)));
 }

scm *
make_char (int x)
{
  scm *p = malloc (sizeof (scm));
  p->type = CHAR;
  p->value = x;
  return p;
}

scm *
make_macro (scm *x) //int
{
  scm *p = malloc (sizeof (scm));
  p->type = MACRO;
  p->macro = x;
  return p;
}

scm *
make_number (int x)
{
  scm *p = malloc (sizeof (scm));
  p->type = NUMBER;
  p->value = x;
  return p;
}

scm *
make_string (char const *s)
{
  scm *p = malloc (sizeof (scm));
  p->type = STRING;
  p->name = strdup (s);
  return p;
}

scm *
make_symbol (char const *s)
{
  // TODO: alist lookup symbols
  scm *p = malloc (sizeof (scm));
  p->type = SYMBOL;
  p->name = strdup (s);
  return p;
}

scm *
make_vector (int n)
{
  scm *p = malloc (sizeof (scm));
  p->type = VECTOR;
  p->length = n;
  p->vector = malloc (n * sizeof (scm*));
  return p;
}

scm *
string (scm *x/*...*/)
{
  char buf[256] = "";
  char *p = buf;
  while (x != &scm_nil)
    {
      scm *s = car (x);
      assert (s->type == CHAR);
      *p++ = s->value;
      x = cdr (x);
    }
  return make_string (buf);
}

scm *
string_append (scm *x/*...*/)
{
  char buf[256] = "";

  while (x != &scm_nil)
    {
      scm *s = car (x);
      assert (s->type == STRING);
      strcat (buf, s->name);
      x = cdr (x);
    }
  return make_string (buf);
}

scm *
string_length (scm *x)
{
  assert (x->type == STRING);
  return make_number (strlen (x->name));
}

scm *
length (scm *x)
{
  int n = 0;
  while (x != &scm_nil)
    {
      n++;
      x = cdr (x);
    }
  return make_number (n);
}

scm *
builtin_list (scm *x/*...*/)
{
  return x;
}

scm *
values (scm *x/*...*/)
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
  return x->vector[i->value];
}

scm *
vector_set_x (scm *x, scm *i, scm *e)
{
  assert (x->type == VECTOR);
  assert (i->value < x->length);
  x->vector[i->value] = e;
  return &scm_unspecified;
}

scm *
lookup (char *x, scm *a)
{
  if (isdigit (*x) || (*x == '-' && isdigit (*(x+1))))
    return make_number (atoi (x));

  if (!strcmp (x, scm_dot.name)) return &scm_dot;
  if (!strcmp (x, scm_f.name)) return &scm_f;
  if (!strcmp (x, scm_nil.name)) return &scm_nil;
  if (!strcmp (x, scm_t.name)) return &scm_t;
  if (!strcmp (x, scm_unspecified.name)) return &scm_unspecified;

  if (!strcmp (x, symbol_begin.name)) return &symbol_begin;
  if (!strcmp (x, symbol_closure.name)) return &symbol_closure;
  if (!strcmp (x, symbol_cond.name)) return &symbol_cond;
  if (!strcmp (x, symbol_current_module.name)) return &symbol_current_module;
  if (!strcmp (x, symbol_lambda.name)) return &symbol_lambda;
  if (!strcmp (x, symbol_quasiquote.name)) return &symbol_quasiquote;
  if (!strcmp (x, symbol_quote.name)) return &symbol_quote;
  if (!strcmp (x, symbol_set_x.name)) return &symbol_set_x;
  if (!strcmp (x, symbol_unquote.name)) return &symbol_unquote;
  if (!strcmp (x, symbol_unquote_splicing.name)) return &symbol_unquote_splicing;

  if (!strcmp (x, scm_car.name)) return &scm_car;
  if (!strcmp (x, scm_cdr.name)) return &scm_cdr;
  if (!strcmp (x, scm_display.name)) return &scm_display;
  if (!strcmp (x, scm_builtin_list.name)) return &scm_builtin_list;

  if (*x == '\'') return &symbol_quote;
  if (*x == '`') return &symbol_quasiquote;
  if (*x == ',' && *(x+1) == '@') return &symbol_unquote_splicing;
  if (*x == ',') return &symbol_unquote;

  return make_symbol (x);
}

scm *
lookup_char (int c, scm *a)
{
  char buf[2];
  buf[0] = c;
  buf[1] = 0;
  return lookup (buf, a);
}

char *
list2str (scm *l) // char*
{
  static char buf[256];
  char *p = buf;
  while (l != &scm_nil) {
    scm *c = car (l);
    assert (c->type == NUMBER);
    *p++ = c->value;
    l = cdr (l);
  }
  *p = 0;
  return buf;
}

scm*
list_to_vector (scm *x)
{
  int n = length (x)->value;
  scm *v = make_vector (n);
  scm **p = v->vector;
  while (x != &scm_nil)
    {
      *p++ = car (x);
      x = cdr (x);
    }
  return v;
}

scm*
integer_to_char (scm *x)
{
  assert (x->type == NUMBER);
  return make_char (x->value);
}

scm*
char_to_integer (scm *x)
{
  assert (x->type == CHAR);
  return make_number (x->value);
}

scm*
number_to_string (scm *x)
{
  assert (x->type == NUMBER);
  char buf[256];
  sprintf (buf,"%d", x->value);
  return make_string (buf);
}

scm*
builtin_exit (scm *x)
{
  assert (x->type == NUMBER);
  exit (x->value);
}

scm*
string_to_symbol (scm *x)
{
  assert (x->type == STRING);
  return make_symbol (x->name);
}

scm*
symbol_to_string (scm *x)
{
  assert (x->type == SYMBOL);
  return make_string (x->name);
}

scm*
vector_to_list (scm *v)
{
  scm *x = &scm_nil;
  for (int i = 0; i < v->length; i++)
    x = append2 (x, cons (v->vector[i], &scm_nil));
  return x;
}

scm *
newline ()
{
  puts ("");
  return &scm_unspecified;
}

scm *
display_helper (scm *x, bool cont, char *sep, bool quote)
{
  scm *r;
  printf ("%s", sep);
  if (x->type == CHAR && x->value == 10) printf ("#\\%s", "newline");
  else if (x->type == CHAR && x->value == 32) printf ("#\\%s", "space");
  else if (x->type == CHAR) printf ("#\\%c", x->value);
  else if (x->type == MACRO) {
    printf ("(*macro* ");
    display_helper (x->macro, cont, sep, quote);
    printf (")");
  }
  else if (x->type == NUMBER) printf ("%d", x->value);
  else if (x->type == PAIR) {
    if (car (x) == &symbol_circ) {
      printf ("(*circ* . #-1#)");
      return &scm_unspecified;
    }
    if (car (x) == &scm_quote) {
      printf ("'");
      return display_helper (car (cdr (x)), cont, "", true);
    }
    if (car (x) == &scm_quasiquote) {
      printf ("`");
      return display_helper (car (cdr (x)), cont, "", true);
    }
    if (car (x) == &scm_unquote) {
      printf (",");
      return display_helper (car (cdr (x)), cont, "", true);
    }
    if (car (x) == &scm_unquote_splicing) {
      printf (",@");
      return display_helper (car (cdr (x)), cont, "", true);
    }
    if (!cont) printf ("(");
    display (car (x));
    if (cdr (x)->type == PAIR)
      display_helper (cdr (x), true, " ", false);
    else if (cdr (x) != &scm_nil) {
      printf (" . ");
      display (cdr (x));
    }
    if (!cont) printf (")");
  }
  else if (x->type == VECTOR) {
    printf ("#(");
    for (int i = 0; i < x->length; i++)
      display_helper (x->vector[i], true, i ? " " : "", false);
    printf (")");
  }
  else if (atom_p (x) == &scm_t) printf ("%s", x->name);

  return &scm_unspecified;
}

// READ

int
ungetchar (int c) //int
{
  return ungetc (c, stdin);
}

int
peekchar () //int
{
  int c = getchar ();
  ungetchar (c);
  return c;
}

scm*
builtin_getchar ()
{
  return make_number (getchar ());
}

scm*
builtin_peekchar ()
{
  return make_number (peekchar ());
}

scm*
builtin_ungetchar (scm *c)
{
  assert (c->type == NUMBER);
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
readword (int c, char* w, scm *a)
{
  if (c == EOF && !w) return &scm_nil;
  if (c == '\n' && !w) return readword (getchar (), w, a);
  if (c == '\n' && *w == '.' && w[1] == 0) return &scm_dot;
  if (c == EOF || c == '\n') return lookup (w, a);
  if (c == ' ') return readword ('\n', w, a);
  if (c == '"' && !w) return readstring ();
  if (c == '"') {ungetchar (c); return lookup (w, a);}
  if (c == '(' && !w) return readlist (a);
  if (c == '(') {ungetchar (c); return lookup (w, a);}
  if (c == ')' && !w) {ungetchar (c); return &scm_nil;}
  if (c == ')') {ungetchar (c); return lookup (w, a);}
  if (c == ',' && peekchar () == '@') {getchar (); return cons (lookup (",@", a),
                                                                cons (readword (getchar (), w, a),
                                                                      &scm_nil));}
  if ((c == '\''
       || c == '`'
       || c == ',')
      && !w) {return cons (lookup_char (c, a),
                                     cons (readword (getchar (), w, a),
                                           &scm_nil));}
  if (c == ';') {readcomment (c); return readword ('\n', w, a);}
  if (c == '#' && peekchar () == '\\') {getchar (); return readchar ();}
  if (c == '#' && !w && peekchar () == '(') {getchar (); return list_to_vector (readlist (a));}
  if (c == '#' && peekchar () == '(') {ungetchar (c); return lookup (w, a);}
  if (c == '#' && peekchar () == '!') {getchar (); readblock (getchar ()); return readword (getchar (), w, a);}
  char buf[256] = {0};
  char ch = c;
  return readword (getchar (), strncat (w ? w : buf, &ch, 1), a);
}

scm *
readchar ()
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
    char buf[256];
    char *p = buf;
    *p++ = c;
    while (peekchar () >= 'a' && peekchar () <= 'z') {
      *p++ = getchar ();
    }
    *p = 0;
    if (!strcmp (buf, "newline")) c = 10;
    else if (!strcmp (buf, "space")) c = 32;
    else {
      printf ("char not supported: %s", buf);
      assert (!"char not supported");
    }
  }
  return make_char (c);
}

scm *
readstring ()
{
  char buf[256];
  char *p = buf;
  int c = getchar ();
  while (true) {
    if (c == '"') break;
    *p++ = c;
    if (c == '\\' && peekchar () == '"') *p++ = getchar ();
    if (c == EOF) assert (!"EOF in string");
    c = getchar ();
  }
  *p = 0;
  return make_string (buf);
}

int
eat_whitespace (int c)
{
  while (c == ' ' || c == '\n') c = getchar ();
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
  scm *w = readword (c, 0, a);
  if (w == &scm_dot)
    return car (readlist (a));
  return cons (w, readlist (a));
}

scm *
readenv (scm *a)
{
  return readword (getchar (), 0, a);
}

scm *
greater_p (scm *a, scm *b)
{
  assert (a->type == NUMBER);
  assert (b->type == NUMBER);
  return a->value > b->value ? &scm_t : &scm_f;
}

scm *
less_p (scm *a, scm *b)
{
  assert (a->type == NUMBER);
  assert (b->type == NUMBER);
  return a->value < b->value ? &scm_t : &scm_f;
}

scm *
minus (scm *x/*...*/)
{
  scm *a = car (x);
  assert (a->type == NUMBER);
  int n = a->value;
  x = cdr (x);
  if (x == &scm_nil)
    n = -n;
  while (x != &scm_nil)
    {
      assert (x->car->type == NUMBER);
      n -= x->car->value;
      x = cdr (x);
    }
  return make_number (n);
}

scm *
plus (scm *x/*...*/)
{
  int n = 0;
  while (x != &scm_nil)
    {
      assert (x->car->type == NUMBER);
      n += x->car->value;
      x = cdr (x);
    }
  return make_number (n);
}

scm *
divide (scm *x/*...*/)
{
  int n = 1;
  if (x != &scm_nil) {
    assert (x->car->type == NUMBER);
    n = x->car->value;
    x = cdr (x);
  }
  while (x != &scm_nil)
    {
      assert (x->car->type == NUMBER);
      n /= x->car->value;
      x = cdr (x);
    }
  return make_number (n);
}

scm *
multiply (scm *x/*...*/)
{
  int n = 1;
  while (x != &scm_nil)
    {
      assert (x->car->type == NUMBER);
      n *= x->car->value;
      x = cdr (x);
    }
  return make_number (n);
}

scm *
is_p (scm *a, scm *b)
{
  assert (a->type == NUMBER);
  assert (b->type == NUMBER);
  return a->value == b->value ? &scm_t : &scm_f;
}

scm *add_environment (scm *a, char *name, scm *x);

scm *
add_unquoters (scm *a)
{
  a = add_environment (a, "unquote", &scm_unquote);
  a = add_environment (a, "unquote-splicing", &scm_unquote_splicing);
  return a;
}

scm *
add_environment (scm *a, char *name, scm *x)
{
  return cons (cons (make_symbol (name), x), a);
}

scm *
mes_environment ()
{
  scm *a = &scm_nil;

  a = cons (cons (&scm_f, &scm_f), a);
  a = cons (cons (&scm_nil, &scm_nil), a);
  a = cons (cons (&scm_t, &scm_t), a);
  a = cons (cons (&scm_unspecified, &scm_unspecified), a);
  
#include "environment.i"
  
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
  return cons (&symbol_closure, cons (cons (&symbol_circ, cdr (a)), cons (args, body)));
}

scm *
def (scm *x)
{
  if (atom_p (cadr (x)) != &scm_f)
    return cons (cadr (x), x);
  return cons (caadr (x), x);
}

scm *
define (scm *x, scm *a)
{
  scm *e;
  scm *name = cadr (x);
  if (name->type != PAIR)
    e = eval (caddr (x), cons (cons (cadr (x), cadr (x)), a));
  else {
    name = car (name);
    scm *p = pairlis (cadr (x), cadr (x), a);
    e = eval (make_lambda (cdadr (x), cddr (x)), p);
  }
  if (eq_p (car (x), &symbol_define_macro) == &scm_t)
    e = make_macro (e);
  return cons (name, e);
}
 
scm *
lookup_macro (scm *x, scm *a)
{
  scm *m = assq (x, a);
  if (m != &scm_f && macro_p (cdr (m)) != &scm_f)
    return cdr (m)->macro;
  return &scm_f;
}

scm *
read_file (scm *e, scm *a)
{
  if (e == &scm_nil) return e;
  return cons (e, read_file (readenv (a), a));
}

int
main (int argc, char *argv[])
{
  scm *a = mes_environment ();
  display (eval (cons (&symbol_begin, read_file (readenv (a), a)), a));
  newline ();
  return 0;
}
