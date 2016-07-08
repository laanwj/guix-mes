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
#include <stdarg.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <stdbool.h>

#define DEBUG 0

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

scm scm_nil = {ATOM, "()"};
scm scm_t = {ATOM, "#t"};
scm scm_f = {ATOM, "#f"};
scm scm_lambda = {ATOM, "lambda"};
scm scm_label = {ATOM, "label"};
scm scm_unspecified = {ATOM, "*unspecified*"};
scm scm_symbol_cond = {ATOM, "cond"};
scm scm_symbol_quote = {ATOM, "quote"};

// PRIMITIVES

scm *
atom_p (scm *x)
{
  return x->type == PAIR ? &scm_f : &scm_t;
}
scm scm_atom = {FUNCTION1, .name="atom", .function1 = &atom_p};

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

scm *eval (scm*, scm*);

scm *display (scm*);

scm scm_quote;
scm *
quote (scm *x)
{
  return cons (&scm_quote, x);
}

//Library functions
scm scm_read;


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
scm scm_caar  = {FUNCTION1, .name="caar ", .function1 = &caar };
scm scm_cadr  = {FUNCTION1, .name="cadr ", .function1 = &cadr };
scm scm_cdar  = {FUNCTION1, .name="cdar ", .function1 = &cdar };
scm scm_cddr  = {FUNCTION1, .name="cddr ", .function1 = &cddr };
scm scm_caadr = {FUNCTION1, .name="caadr", .function1 = &caadr};
scm scm_caddr = {FUNCTION1, .name="caddr", .function1 = &caddr};
scm scm_cdadr = {FUNCTION1, .name="cdadr", .function1 = &cdadr};
scm scm_cadar = {FUNCTION1, .name="cadar", .function1 = &cadar};
scm scm_cddar = {FUNCTION1, .name="cddar", .function1 = &cddar};
scm scm_cdddr = {FUNCTION1, .name="cdddr", .function1 = &cdddr};

scm *
list (scm *x, ...)
{
  va_list args;
  scm *lst = &scm_nil;

  va_start (args, x);
  while (x != &scm_unspecified)
    {
      lst = cons (x, lst);
      x = va_arg (args, scm*);
    }
  va_end (args);
  return lst;
}

scm *
pairlis (scm *x, scm *y, scm *a)
{
#if 0 //DEBUG
  printf ("pairlis x=");
  display (x);
  printf (" y=");
  display (y);
  puts ("");
#endif
  if (x == &scm_nil)
    return a;
  return cons (cons (car (x), car (y)),
               pairlis (cdr (x), cdr (y), a));
}
scm scm_pairlis = {FUNCTION3, .name="pairlis", .function3 = &pairlis};

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
scm scm_assoc = {FUNCTION2, .name="assoc", .function2 = &assoc};

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
  //printf (" x=");
  //display (x);
  puts ("");
#endif
  if (atom_p (fn) != &scm_f)
    {
      if (builtin_p (fn) == &scm_t)
        return call (fn, x);
      return apply (eval (fn,  a), x, a);
    }
  else if (car (fn) == &scm_lambda) {
    scm *body = cddr (fn);
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
      if (car (e) == &scm_symbol_quote)
        return cadr (e);
      else if (car (e) == &scm_symbol_cond)
        return evcon (cdr (e), a);
      else
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

scm scm_evcon = {FUNCTION2, .name="evcon", .function2 = &evcon};

scm *
evlis (scm *m, scm *a)
{
  if (m == &scm_nil)
    return &scm_nil;
  return cons (eval (car (m), a), evlis (cdr (m), a));
}
scm scm_evlis = {FUNCTION2, .name="evlis", .function2 = &evlis};


//Primitives
scm scm_car = {FUNCTION1, "car", .function1 = &car};
scm scm_cdr = {FUNCTION1, "cdr", .function1 = &cdr};
scm scm_cons = {FUNCTION2, "cons", .function2 = &cons};
scm scm_cond = {FUNCTION2, "cond", .function2 = &evcon};
scm scm_eq_p = {FUNCTION2, "eq", .function2 = &eq_p};
scm scm_null_p = {FUNCTION1, "null", .function1 = &null_p};
scm scm_pair_p = {FUNCTION1, "pair", .function1 = &pair_p};
scm scm_quote = {FUNCTION1, "quote", .function1 = &quote};

scm scm_eval = {FUNCTION2, .name="eval", .function2 = &eval};
scm scm_apply = {FUNCTION3, .name="apply", .function3 = &apply};

scm scm_apply_ = {FUNCTION3, .name="c:apply", .function3 = &apply_};
scm scm_eval_ = {FUNCTION2, .name="c:eval", .function2 = &eval_};

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
scm scm_builtin_p = {FUNCTION1, .name="builtin", .function1 = &builtin_p};

scm *
number_p (scm *x)
{
  return x->type == NUMBER ? &scm_t : &scm_f;
}
scm scm_number_p = {FUNCTION1, .name="number", .function1 = &number_p};

scm *display_helper (scm*, bool, char*, bool);

scm *
display (scm *x)
{
  return display_helper (x, false, "", false);
}
scm scm_display = {FUNCTION1, .name="display", .function1 = &display};

scm *call (scm*, scm*);
scm scm_call = {FUNCTION2, .name="call", .function2 = &call};

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
scm scm_append = {FUNCTION2, .name="append", .function2 = &append};


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

  return make_atom (x);
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
scm scm_lookup = {FUNCTION2, .name="lookup", .function2 = &builtin_lookup};

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
scm scm_newline = {FUNCTION0, .name="newline", .function0 = &newline};

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
ungetchar (int c)
{
  return ungetc (c, stdin);
}

int
peekchar ()
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
scm scm_getchar = {FUNCTION0, .name="getchar", .function0 = &builtin_getchar};

scm*
builtin_peekchar ()
{
  return make_number (peekchar ());
}
scm scm_peekchar = {FUNCTION0, .name="peekchar", .function0 = &builtin_peekchar};

scm*
builtin_ungetchar (scm* c)
{
  assert (c->type == NUMBER);
  ungetchar (c->value);
  return c;
}
scm scm_ungetchar = {FUNCTION1, .name="ungetchar", .function1 = &builtin_ungetchar};

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
  if (c == EOF || c == '\n') return lookup (w, a);
  if (c == ' ') return readword ('\n', w, a);
  if (c == '(' && !w) return readlis (a);
  if (c == '(') {ungetchar (c); return lookup (w, a);}
  if (c == ')' && !w) {ungetchar (c); return &scm_nil;}
  if (c == ')') {ungetchar (c); return lookup (w, a);}
  if (c == '\'' && !w) {return cons (lookup ("'", a),
                                     cons (readword (getchar (), w, a),
                                           &scm_nil));}
  if (c == ';') {readcomment (c); return readword ('\n', w, a);}
  if (c == '#' && peekchar () == '!') {getchar (); readblock (getchar ()); return readword (getchar (), w, a);}
  char buf[256] = {0};
  char ch = c;
  return readword (getchar (), strncat (w ? w : buf, &ch, 1), a);
}

scm *
readlis (scm *a)
{
  int c = getchar ();
  if (c == ')') return &scm_nil;
  scm *w = readword (c, 0, a);
  return cons (w, readlis (a));
}

scm *
readenv (scm *a)
{
  return readword (getchar (), 0, a);
}
scm scm_readenv = {FUNCTION1, .name="readenv", .function1 = &readenv};

// Extras to make interesting program

scm *
hello_world ()
{
  puts ("c: hello world");
  return &scm_unspecified;
}
scm scm_hello_world = {FUNCTION0, .name="hello-world", .function0 = &hello_world};


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
#if DEBUG
  printf ("\nminus a=");
  display (a);
  printf (" b=");
  display (b);
  puts ("");
#endif
  assert (a->type == NUMBER);
  assert (b->type == NUMBER);
  //return make_number (a->value - b->value);
  scm *r = make_number (a->value - b->value);
#if DEBUG
  printf ("  ==> ");
  display (r);
  puts ("");
#endif
  return r;
}

scm scm_less_p = {FUNCTION2, .name="<", .function2 = &less_p};
scm scm_minus = {FUNCTION2, .name="-", .function2 = &minus};


scm *
add_environment (scm *a, char *name, scm* x)
{
  return cons (cons (make_atom (name), x), a);
}

scm *
initial_environment ()
{
  scm *a = &scm_nil;

  a = add_environment (a, "()", &scm_nil);
  a = add_environment (a, "#t", &scm_t);
  a = add_environment (a, "#f", &scm_f);
  a = add_environment (a, "*unspecified*", &scm_unspecified);

  a = add_environment (a, "label", &scm_label);
  a = add_environment (a, "lambda", &scm_lambda);

  a = add_environment (a, "atom", &scm_atom);
  a = add_environment (a, "car", &scm_car);
  a = add_environment (a, "cdr", &scm_cdr);
  a = add_environment (a, "cons", &scm_cons);
  a = add_environment (a, "cond", &scm_cond);
  a = add_environment (a, "eq", &scm_eq_p);

  a = add_environment (a, "null", &scm_null_p);
  a = add_environment (a, "pair", &scm_pair_p);
  a = add_environment (a, "quote", &scm_quote);
  a = add_environment (a, "'", &scm_quote);

  a = add_environment (a, "evlis", &scm_evlis);
  a = add_environment (a, "evcon", &scm_evcon);
  a = add_environment (a, "pairlis", &scm_pairlis);
  a = add_environment (a, "assoc", &scm_assoc);

  a = add_environment (a, "c:eval", &scm_eval_);
  a = add_environment (a, "c:apply", &scm_apply_);
  a = add_environment (a, "eval", &scm_eval);
  a = add_environment (a, "apply", &scm_apply);

  a = add_environment (a, "getchar", &scm_getchar);
  a = add_environment (a, "peekchar", &scm_peekchar);
  a = add_environment (a, "ungetchar", &scm_ungetchar);
  a = add_environment (a, "lookup", &scm_lookup);

  a = add_environment (a, "readenv", &scm_readenv);
  a = add_environment (a, "display", &scm_display);
  a = add_environment (a, "newline", &scm_newline);

  a = add_environment (a, "builtin", &scm_builtin_p);
  a = add_environment (a, "number", &scm_number_p);
  a = add_environment (a, "call", &scm_call);


  a = add_environment (a, "hello-world", &scm_hello_world);
  a = add_environment (a, "<", &scm_less_p);
  a = add_environment (a, "-", &scm_minus);

  // DERIVED
  a = add_environment (a, "caar", &scm_caar);
  a = add_environment (a, "cadr", &scm_cadr);
  a = add_environment (a, "cdar", &scm_cdar);
  a = add_environment (a, "cddr", &scm_cddr);
  a = add_environment (a, "caadr", &scm_caadr);
  a = add_environment (a, "caddr", &scm_caddr);
  a = add_environment (a, "cdadr", &scm_cdadr);
  a = add_environment (a, "cadar", &scm_cadar);
  a = add_environment (a, "cddar", &scm_cddar);
  a = add_environment (a, "cdddr", &scm_cdddr);

  a = add_environment (a, "append", &scm_append);

  //
  a = add_environment (a, "*macro*", &scm_nil);

  return a;
}

scm *
define_lambda (scm *x, scm *a)
{
  return cons (caadr (x), cons (&scm_lambda, cons (cdadr (x), cddr (x))));
}

scm *
define (scm *x, scm *a)
{
  if (atom_p (cadr (x)) != &scm_f)
    return cons (cadr (x), eval (caddr (x), a));
  return define_lambda (x, a);
}

scm *
loop (scm *r, scm *e, scm *a)
{
  if (e == &scm_nil)
    return r;
  else if (eq_p (e, make_atom ("EOF")) == &scm_t)
    return apply (cdr (assoc (make_atom ("loop2"), a)),
                  cons (&scm_unspecified, cons (&scm_t, cons (a, &scm_nil))), a);
  else if (eq_p (e, make_atom ("EOF2")) == &scm_t)
    return r;
  else if (atom_p (e) == &scm_t)
    return loop (eval (e, a), readenv (a), a);
  else if (eq_p (car (e), make_atom ("define")) == &scm_t)
    return loop (&scm_unspecified,
                 readenv (a),
                 cons (define (e, a), a));
  return loop (eval (e, a), readenv (a), a);
}

int
main (int argc, char *argv[])
{
  scm *a = initial_environment ();
  display (loop (&scm_unspecified, readenv (a), a));
  newline ();
  return 0;
}

scm *
apply (scm* fn, scm *x, scm *a)
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

  scm *eval__ = assoc (make_atom ("eval"), a);
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
