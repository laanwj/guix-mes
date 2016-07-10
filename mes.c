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

#define MACROS 1
#define QUASIQUOTE 1

#ifndef QUOTE_SUGAR
#define QUOTE_SUGAR 1
#endif

enum type {ATOM, NUMBER, PAIR, FUNCTION0, FUNCTION1, FUNCTION2, FUNCTION3};
struct scm_t;
typedef struct scm_t* (*function0_t) (void);
typedef struct scm_t* (*function1_t) (struct scm_t*);
typedef struct scm_t* (*function2_t) (struct scm_t*, struct scm_t*);
typedef struct scm_t* (*function3_t) (struct scm_t*, struct scm_t*, struct scm_t*);

typedef struct scm_t {
  enum type type;
  union {
    char *name;
    struct scm_t* car;
  };
  union {
    int value;
    function0_t function0;
    function1_t function1;
    function2_t function2;
    function3_t function3;
    struct scm_t* cdr;
  };
} scm;

#define MES 1
#include "mes.h"

scm *display_helper (scm*, bool, char*, bool);
bool
symbol_eq (scm *x, char *s)
{
  return x->type == ATOM && !strcmp (x->name, s);
}

scm scm_nil = {ATOM, "()"};
scm scm_dot = {ATOM, "."};
scm scm_t = {ATOM, "#t"};
scm scm_f = {ATOM, "#f"};
scm scm_lambda = {ATOM, "lambda"};
scm scm_label = {ATOM, "label"};
scm scm_unspecified = {ATOM, "*unspecified*"};
scm scm_symbol_cond = {ATOM, "cond"};
scm scm_symbol_quote = {ATOM, "quote"};
#if QUASIQUOTE
scm scm_symbol_quasiquote = {ATOM, "quasiquote"};
scm scm_symbol_unquote = {ATOM, "unquote"};
#endif
#if MACROS
scm scm_macro = {ATOM, "*macro*"};
#endif

scm scm_symbol_EOF = {ATOM, "EOF"};
scm scm_symbol_EOF2 = {ATOM, "EOF2"};
scm scm_symbol_current_module = {ATOM, "current-module"};
scm scm_symbol_define = {ATOM, "define"};
scm scm_symbol_define_macro = {ATOM, "define-macro"};
scm scm_symbol_eval = {ATOM, "eval"};
scm scm_symbol_loop2 = {ATOM, "loop2"};
scm scm_symbol_set_x = {ATOM, "set!"};

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
          || (x->type == NUMBER && y->type == NUMBER
              && x->value == y->value)
          // FIXME: alist lookup symbols
          || (atom_p (x) == &scm_t
              && x->type != NUMBER
              && y->type != NUMBER
              && atom_p (y) == &scm_t
              && !strcmp (x->name, y->name)))
    ? &scm_t : &scm_f;
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
}

scm *
set_env_x (scm *x, scm *e, scm *a)
{
  return set_cdr_x (assoc (x, a), e);
}

scm *
quote (scm *x)
{
  return cons (&scm_symbol_quote, x);
}

#if QUASIQUOTE
scm *
unquote (scm *x)
{
  return cons (&scm_symbol_unquote, x);
}

scm *
quasiquote (scm *x)
{
  return cons (&scm_symbol_quasiquote, x);
}

scm *eval_quasiquote (scm *, scm *);

#endif

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

scm* make_atom (char const *);

scm *
pairlis (scm *x, scm *y, scm *a)
{
#if DEBUG
  printf ("pairlis x=");
  display (x);
  printf (" y=");
  display (y);
  puts ("");
#endif
  if (x == &scm_nil)
    return a;
  if (atom_p (x) == &scm_t)
    return cons (cons (x, y), a);
  return cons (cons (car (x), car (y)),
               pairlis (cdr (x), cdr (y), a));
}

scm *
assoc (scm *x, scm *a)
{
  if (a == &scm_nil) {
#if DEBUG
    printf ("alist miss: %s\n", x->name);
#endif
    return &scm_f;
  }
  if (eq_p (caar (a), x) == &scm_t)
    return car (a);
  return assoc (x, cdr (a));
}

scm *apply (scm*, scm*, scm*);
scm *eval_ (scm*, scm*);
scm *apply_ (scm*, scm*, scm*);

scm *
eval_quote (scm *fn, scm *x)
{
  return apply (fn, x, &scm_nil);
}

scm *builtin_p (scm*);
scm *call (scm *, scm*);
scm *display (scm*);
scm *newline ();

scm *
apply_ (scm *fn, scm *x, scm *a)
{
#if DEBUG
  printf ("apply fn=");
  display (fn);
  printf (" x=");
  display (x);
  puts ("");
#endif
  if (atom_p (fn) != &scm_f)
    {
      if (fn == &scm_symbol_current_module) // FIXME
        return a;
      if (builtin_p (fn) == &scm_t)
        return call (fn, x);
      return apply (eval (fn,  a), x, a);
    }
  else if (car (fn) == &scm_lambda) {
    scm *body = cddr (fn);
    scm *ca = cadr (fn);
    scm *ax = pairlis (cadr (fn), x, a);
    scm *result = eval (car (body), ax);
    if (cdr (body) == &scm_nil)
      return result;
    return apply (cons (car (fn), cons (cadr (fn), cdddr (fn))), x, ax);
  }
  else if (car (fn) == &scm_label)
    return apply (caddr (fn), x, cons (cons (cadr (fn), caddr (fn)), a));
  return &scm_unspecified;
}

scm *evcon (scm*, scm*);
scm *evlis (scm*, scm*);

scm *
eval_ (scm *e, scm *a)
{
#if DEBUG
  printf ("eval e=");
  display (e);
  puts ("");
#endif
  if (e->type == NUMBER)
    return e;
  else if (atom_p (e) == &scm_t) {
    scm *y = assoc (e, a);
    if (y == &scm_f) {
      printf ("eval: no such symbol: %s\n", e->name);
      exit (1);
    }
    return cdr (y);
  }
  if (builtin_p (e) == &scm_t)
    return e;
  else if (atom_p (car (e)) == &scm_t)
    {
#if MACROS
      scm *macro;
#endif // MACROS
      if (car (e) == &scm_symbol_quote)
        return cadr (e);
      if (car (e) == &scm_lambda)
        return e;
      if (car (e) == &scm_symbol_set_x)
        return set_env_x (cadr (e), eval (caddr (e), a), a);
#if QUASIQUOTE
      else if (car (e) == &scm_symbol_unquote)
        return eval (cadr (e), a);
      else if (car (e) == &scm_symbol_quasiquote) {
#if DEBUG
        printf ("cadr e:");
        display (cadr (e));
        puts ("");
        printf ("qq:");
        display (eval_quasiquote (cadr (e), a));
        puts ("");
#endif // DEBUG
        return eval_quasiquote (cadr (e), a);
      }
#endif // QUASIQUOTE
      else if (car (e) == &scm_symbol_cond)
        return evcon (cdr (e), a);
#if MACROS
      else if ((macro = assoc (car (e), cdr (assoc (&scm_macro, a)))) != &scm_f)
        return eval (apply_ (cdr (macro), cdr (e), a), a);
#endif // MACROS
      return apply (car (e), evlis (cdr (e), a), a);
    }
  return apply (car (e), evlis (cdr (e), a), a);
}

scm *
evcon_ (scm *c, scm *a)
{
#if DEBUG
  printf ("evcon_ clause=");
  display (car (c));
  puts ("");
#endif
  if (eval (caar (c), a) != &scm_f) {
#if DEBUG
    //if (fn != &scm_display && fn != &scm_call)
    //if (fn != &scm_call)
    printf ("#t clause=");
    display (car (c));
    printf (" cddar=");
    display (cddar (c));
    printf (" nil=%d", cddar (c) == &scm_nil);
    puts ("");
#endif
    if (cddar (c) == &scm_nil)
      return eval (cadar (c), a);
    eval (cadar (c), a);
    return evcon_ (cons (cons (&scm_t, cddar (c)), &scm_nil), a);
  }
  return evcon_ (cdr (c), a);
}

scm *
evcon (scm *c, scm *a)
{
#if DEBUG
  printf ("\n****evcon=");
  display (c);
  puts ("");
#endif
  return evcon_ (c, a);
}

scm *
evlis (scm *m, scm *a)
{
#if DEBUG
  printf ("evlis m=");
  display (m);
  puts ("");
#endif
  if (m == &scm_nil)
    return &scm_nil;
  scm *e = eval (car (m), a);
  return cons (e, evlis (cdr (m), a));
}

//Helpers

scm *
builtin_p (scm *x)
{
  return (x->type == FUNCTION0
          || x->type == FUNCTION1
          || x->type == FUNCTION2
          || x->type == FUNCTION3)
    ? &scm_t : &scm_f;
}

scm *
number_p (scm *x)
{
  return x->type == NUMBER ? &scm_t : &scm_f;
}

scm *
display (scm *x)
{
  return display_helper (x, false, "", false);
}

scm *
call (scm *fn, scm *x)
{
#if DEBUG
  //if (fn != &scm_display && fn != &scm_call)
  //if (fn != &scm_call)
  {
    printf ("\ncall fn=");
    display (fn);
    printf (" x=");
    display (x);
    puts ("");
  }
#endif
  if (fn->type == FUNCTION0)
    return fn->function0 ();
  if (fn->type == FUNCTION1)
    return fn->function1 (car (x));
  if (fn->type == FUNCTION2)
    return fn->function2 (car (x), cadr (x));
  if (fn->type == FUNCTION3)
    return fn->function3 (car (x), cadr (x), caddr (x));
  return &scm_unspecified;
}

scm *
append (scm *x, scm *y)
{
  if (x == &scm_nil) return y;
  assert (x->type == PAIR);
   return cons (car (x), append (cdr (x), y));
}

scm *
make_atom (char const *s)
{
  // TODO: alist lookup symbols
  scm *p = malloc (sizeof (scm));
  p->type = ATOM;
  p->name = strdup (s);
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
lookup (char *x, scm *a)
{
  if (isdigit (*x) || (*x == '-' && isdigit (*(x+1))))
    return make_number (atoi (x));
  if (*x == '\'') return &scm_symbol_quote;

  if (!strcmp (x, scm_symbol_cond.name)) return &scm_symbol_cond;
  if (!strcmp (x, scm_symbol_quote.name)) return &scm_symbol_quote;
  if (!strcmp (x, scm_lambda.name)) return &scm_lambda;
  if (!strcmp (x, scm_label.name)) return &scm_label;
  if (!strcmp (x, scm_nil.name)) return &scm_nil;

#if QUASIQUOTE
  if (*x == '`') return &scm_symbol_quasiquote;
  if (*x == ',') return &scm_symbol_unquote;  
  if (!strcmp (x, scm_symbol_unquote.name)) return &scm_symbol_unquote;
  if (!strcmp (x, scm_symbol_quasiquote.name)) return &scm_symbol_quasiquote;
#endif

  return make_atom (x);
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
list2str (scm *l)
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

scm *
builtin_lookup (scm *l, scm *a)
{
  return lookup (list2str (l), a);
}

scm *
cossa (scm *x, scm *a)
{
  if (a == &scm_nil) return &scm_f;
  if (eq_p (cdar (a), x) == &scm_t)
    return car (a);
  return cossa (x, cdr (a));
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
  if (x->type == NUMBER) printf ("%d", x->value);
  else if (x->type == PAIR) {
#if QUOTE_SUGAR
    if (car (x) == &scm_quote) {
      printf ("'");
      return display_helper (car (cdr (x)), cont, "", true);
    }
#if QUASIQUOTE
    if (car (x) == &scm_symbol_quasiquote
        || car (x) == &scm_quasiquote) {
      printf ("`");
      return display_helper (car (cdr (x)), cont, "", true);
    }
    if (car (x) == &scm_symbol_unquote
        || car (x) == &scm_unquote) {
      printf (",");
      return display_helper (car (cdr (x)), cont, "", true);
    }
#endif
#endif
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

scm *readlis (scm *a);

scm *
readword (int c, char* w, scm *a)
{
  if (c == EOF && !w) return &scm_nil;
  if (c == '\n' && !w) return readword (getchar (), w, a);
  if (c == '\n' && *w == '.' && w[1] == 0) return &scm_dot;
  if (c == EOF || c == '\n') return lookup (w, a);
  if (c == ' ') return readword ('\n', w, a);
  if (c == '(' && !w) return readlis (a);
  if (c == '(') {ungetchar (c); return lookup (w, a);}
  if (c == ')' && !w) {ungetchar (c); return &scm_nil;}
  if (c == ')') {ungetchar (c); return lookup (w, a);}
  if ((c == '\''
#if QUASIQUOTE
       || c == '`'
       || c == ','
#endif
       )
      && !w) {return cons (lookup_char (c, a),
                                     cons (readword (getchar (), w, a),
                                           &scm_nil));}
  if (c == ';') {readcomment (c); return readword ('\n', w, a);}
  if (c == '#' && peekchar () == '!') {getchar (); readblock (getchar ()); return readword (getchar (), w, a);}
  char buf[256] = {0};
  char ch = c;
  return readword (getchar (), strncat (w ? w : buf, &ch, 1), a);
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
readlis (scm *a)
{
  int c = getchar ();
  c = eat_whitespace (c);
  if (c == ')') return &scm_nil;
  scm *w = readword (c, 0, a);
  if (w == &scm_dot)
    return car (readlis (a));
  return cons (w, readlis (a));
}

scm *
readenv (scm *a)
{
  return readword (getchar (), 0, a);
}

// Extras to make interesting program

scm *
hello_world ()
{
  puts ("c: hello world");
  return &scm_unspecified;
}

scm *
less_p (scm *a, scm *b)
{
  assert (a->type == NUMBER);
  assert (b->type == NUMBER);
  return a->value < b->value ? &scm_t : &scm_f;
}

scm *
minus (scm *a, scm *b)
{
  assert (a->type == NUMBER);
  assert (b->type == NUMBER);
  return make_number (a->value - b->value);
}

scm *
plus (scm *a, scm *b)
{
  assert (a->type == NUMBER);
  assert (b->type == NUMBER);
  return make_number (a->value + b->value);
}

scm *
divide (scm *a, scm *b)
{
  assert (a->type == NUMBER);
  assert (b->type == NUMBER);
  return make_number (a->value / b->value);
}

scm *
multiply (scm *a, scm *b)
{
  assert (a->type == NUMBER);
  assert (b->type == NUMBER);
  return make_number (a->value * b->value);
}

scm *
is_p (scm *a, scm *b)
{
  assert (a->type == NUMBER);
  assert (b->type == NUMBER);
  return a->value == b->value ? &scm_t : &scm_f;
}

#if QUASIQUOTE
scm *
eval_quasiquote (scm *e, scm *a)
{
#if DEBUG
  printf ("\nc:eval_quasiquote e=");
  display (e);
  if (pair_p (e) == &scm_t) {
    printf ("\ncar (e)=");
    display (car (e));
    printf (" atom=");
    display (atom_p (car (e)));
  }
  puts ("");
#endif
  if (e == &scm_nil) return e;
  else if (atom_p (e) == &scm_t) return e;
  else if (atom_p (car (e)) == &scm_t)
    return cons (car (e), eval_quasiquote (cdr (e), a));
  else if (eq_p (caar (e), &scm_symbol_unquote) == &scm_t)
    return cons (eval (cadar (e), a), &scm_nil);
  else if (eq_p (caar (e), &scm_symbol_quote) == &scm_t)
    return cons (cadar (e), &scm_nil);
  else if (eq_p (caar (e), &scm_symbol_quasiquote) == &scm_t)
    return cdar (e);
  return cons (car (e), eval_quasiquote (cdr (e), a));
}
#endif

scm *
add_environment (scm *a, char *name, scm *x)
{
  return cons (cons (make_atom (name), x), a);
}

scm *
mes_environment ()
{
  scm *a = &scm_nil;

  a = add_environment (a, "()", &scm_nil);
  a = add_environment (a, "#t", &scm_t);
  a = add_environment (a, "#f", &scm_f);
  a = add_environment (a, "*unspecified*", &scm_unspecified);
  a = add_environment (a, "label", &scm_label);
  a = add_environment (a, "lambda", &scm_lambda);
  a = add_environment (a, "*macro*", &scm_nil);
  a = add_environment (a, "*dot*", &scm_dot);
  a = add_environment (a, "current-module", &scm_symbol_current_module);

  a = add_environment (a, "'", &scm_quote);
#if QUASIQUOTE
  a = add_environment (a, ",", &scm_unquote);
  a = add_environment (a, "`", &scm_quasiquote);
#endif

#include "environment.i"
  
  return a;
}

scm *
define_lambda (scm *x)
{
  return cons (caadr (x), cons (&scm_lambda, cons (cdadr (x), cddr (x))));
}

scm *
define (scm *x, scm *a)
{
  if (atom_p (cadr (x)) != &scm_f)
    return cons (cadr (x), eval (caddr (x), a));
  return define_lambda (x);
}

scm *
define_macro (scm *x, scm *a)
{
#if DEBUG
  printf ("\nc:define_macro a=");
  scm *aa =cons (&scm_macro,
               cons (define_lambda (x),
                     cdr (assoc (&scm_macro, a))));
  display (aa);
  puts ("");
#endif
  return cons (&scm_macro,
               cons (define_lambda (x),
                     cdr (assoc (&scm_macro, a))));
}

scm *
loop (scm *r, scm *e, scm *a)
{
#if 0//DEBUG
  printf ("\nc:loop e=");
  display (e);
  puts ("");
#endif
  if (e == &scm_nil)
    return r;
  else if (eq_p (e, &scm_symbol_EOF) == &scm_t)
    return apply (cdr (assoc (&scm_symbol_loop2, a)),
                  cons (&scm_unspecified, cons (&scm_t, cons (a, &scm_nil))), a);
  else if (eq_p (e, &scm_symbol_EOF2) == &scm_t)
    return r;
  else if (atom_p (e) == &scm_t)
    return loop (eval (e, a), readenv (a), a);
  else if (eq_p (car (e), &scm_symbol_define) == &scm_t)
    return loop (&scm_unspecified,
                 readenv (a),
                 cons (define (e, a), a));
  else if (eq_p (car (e), &scm_symbol_define_macro) == &scm_t)
    return loop (&scm_unspecified,
                 readenv (a),
                 cons (define_macro (e, a), a));
  else if (eq_p (car (e), &scm_symbol_set_x) == &scm_t)
    return loop (set_env_x (cadr (e), eval (caddr (e), a), a), readenv (a), a);
  return loop (eval (e, a), readenv (a), a);
}

int
main (int argc, char *argv[])
{
  scm *a = mes_environment ();
  display (loop (&scm_unspecified, readenv (a), a));
  newline ();
  return 0;
}

scm *
apply (scm *fn, scm *x, scm *a)
{
#if DEBUG
  printf ("\nc:apply fn=");
  display (fn);
  printf (" x=");
  display (x);
  puts ("");
#endif
  if (fn == &scm_apply_)
    return eval_ (x, a);
  return apply_ (fn, x, a);
}

bool evalling_p = false;

scm *
eval (scm *e, scm *a)
{
#if DEBUG
  printf ("\nc:eval e=");
  display (e);
  puts ("");
#endif

  scm *eval__ = assoc (&scm_symbol_eval, a);
  assert (eval__ != &scm_f);
  eval__ = cdr (eval__);
  if (builtin_p (eval__) == &scm_t
      || evalling_p)
    return eval_ (e, a);
  evalling_p = true;
  scm *r = apply (eval__, cons (e, cons (a, &scm_nil)), a);
  evalling_p = false;
  return r;
}
