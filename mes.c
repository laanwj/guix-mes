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

#ifndef QUOTE_SUGAR
#define QUOTE_SUGAR 1
#endif

enum type {NIL, F, T, ATOM, NUMBER, PAIR, UNSPECIFIED, FUNCTION0, FUNCTION1, FUNCTION2, FUNCTION3, LAMBDA, LABEL};

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

scm scm_nil = {NIL, "()"};
scm scm_t = {T, "#t"};
scm scm_f = {F, "#f"};
scm scm_lambda = {LAMBDA, "lambda"};
scm scm_label = {LABEL, "label"};
scm scm_unspecified = {UNSPECIFIED, "#<unspecified>"};

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
          // FIXME: alist lookup symbols
          || (x->type == ATOM && y->type == ATOM
              && !strcmp (x->name, y->name))
          || (x->type == NUMBER && y->type == NUMBER
              && x->value == y->value))
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

scm *
cond (scm *x, scm *a)
{
  if (x == &scm_nil) return &scm_unspecified;
  assert (x->type == PAIR);
  scm *clause = car (x);
  assert (clause->type == PAIR);
  scm *expr = eval (car (clause), a);
  if (expr != &scm_f) {
    if (clause->type != PAIR)
      return expr;
    return eval (car (cdr (clause)), a);
  }
  return cond (cdr (x), a);
}

scm scm_quote;
scm *
quote (scm *x)
{
  return cons (&scm_quote, x);
}

//PRIMITIVES
scm scm_car = {FUNCTION1, .function1 = &car};
scm scm_cdr = {FUNCTION1, .function1 = &cdr};
scm scm_cons = {FUNCTION2, .function2 = &cons};
scm scm_cond = {FUNCTION2, .function2 = &cond};
scm scm_eq_p = {FUNCTION2, .function2 = &eq_p};
scm scm_null_p = {FUNCTION1, .function1 = &null_p};
scm scm_pair_p = {FUNCTION1, .function1 = &pair_p};
scm scm_quote = {FUNCTION1, .function1 = &quote};

//LIBRARY FUNCTIONS
scm scm_read;


// NEXT
scm *caar (scm *x) {return (car (car (x)));}
scm *cadr (scm *x) {return (car (cdr (x)));}
scm *cdar (scm *x) {return (cdr (car (x)));}
scm *caddr (scm *x) {return car (cdr (cdr (x)));}
scm *cadar (scm *x) {return car (cdr (car (x)));}

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
atom (scm *x)
{
#if EVAL_COND
  return cond
    (list (cons (pair_p (x), &scm_f),
           cons (null_p (x), &scm_f),
           cons (&scm_t, x),
           &scm_unspecified),
     &scm_nil);
#else
  if (pair_p (x) == &scm_t)
    return &scm_f;
  else if (null_p (x) == &scm_t)
    return &scm_f;
  return &scm_t;
#endif
}

// Page 12
scm *
pairlis (scm *x, scm *y, scm *a)
{
#if EVAL_COND
  return cond
    (list (cons (null_p (x), a),
           cons (&scm_t, cons (cons (car (x), car (y)),
                               pairlis (cdr (x), cdr (y), a))),
           &scm_unspecified),
     a);
#else
  if (x == &scm_nil)
    return a;
  return cons (cons (car (x), car (y)),
               pairlis (cdr (x), cdr (y), a));

#endif
}

scm *
assoc (scm *x, scm *a)
{
#if EVAL_COND
  return cond
    (list (cons (eq_p (caar (a), x), car (a)),
           cons (&scm_t, assoc (x, cdr (a))),
           &scm_unspecified),
     a);
#else
  // not Page 12:
  if (a == &scm_nil) return &scm_f;
  // 
  if (eq_p (caar (a), x) == &scm_t)
    return car (a);
  return assoc (x, cdr (a));
#endif
}

// Page 13
scm *apply (scm*, scm*, scm*);

scm *
eval_quote (scm *fn, scm *x)
{
  return apply (fn, x, &scm_nil);
}

scm *procedure_p (scm*);
scm *call (scm *, scm*);
scm *display (scm*);

// .. continued Page 13
scm *
apply (scm *fn, scm *x, scm *a)
{
#if EVAL_COND
  return cond
    (list (cons (atom (fn),
                 cond (list (
                             &scm_unspecified),
                       a)),
           cons (eq_p (car (fn), &scm_lambda),
                 eval (caddr (fn), pairlis (cadr (fn), x, a))),
           &scm_unspecified), a);
#else
#if 0
  printf ("apply fn=");
  display (fn);
  printf (" x=");
  display (x);
  puts ("");
#endif
  if (atom (fn) != &scm_f)
    {
      if (fn == &scm_car)
        return caar (x);
      else if (fn == &scm_cdr)
        return cdar (x);
      else if (fn == &scm_cdr)
        return cdar (x);
      else if (fn == &scm_cons)
        return cons (car (x), cadr (x));
      else if (fn == &scm_eq_p)
        return eq_p (car (x), cadr (x));
      else if (procedure_p (fn) != &scm_f)
        return call (fn, x);
      else
        return apply (eval (fn,  a), x, a);
    }
  else if (car (fn) == &scm_lambda)
    return eval (caddr (fn), pairlis (cadr (fn), x, a));
  else if (car (fn) == &scm_label)
    return apply (caddr (fn), x, cons (cons (cadr (fn),
                                             caddr (fn)),
                                       a));
  return &scm_unspecified;
#endif
}

scm *evcon (scm*, scm*);
scm *evlis (scm*, scm*);

scm *
eval (scm *e, scm *a)
{
#if EVAL_COND
#error no eval cond here
#else
  // not Page 12
  if (e->type == NUMBER
      || e == &scm_t
      || e== &scm_f)
    return e;
  //
  else if (atom (e) == &scm_t)
    return cdr (assoc (e, a));
  else if (atom (car (e)) == &scm_t)
    {
      if (car (e) == &scm_quote)
        return cadr (e);
      else if (car (e) == &scm_cond)
        return evcon (cdr (e), a);
      else
        return apply (car (e), evlis (cdr (e), a), a);
    }
  return apply (car (e), evlis (cdr (e), a), a);
#endif
}

scm *
evcon (scm *c, scm *a)
{
  if (eval (caar (c), a) != &scm_f)
    return eval (cadar (c), a);
  return evcon (cdr (c), a);
}

scm *
evlis (scm *m, scm *a)
{
  if (m == &scm_nil)
    return &scm_nil;
  return cons (eval (car (m), a), evlis (cdr (m), a));
}

// EXTRAS
scm scm_eval = {FUNCTION2, .function2 = &eval};
scm scm_apply = {FUNCTION3, .function3 = &apply};

scm *
procedure_p (scm *x)
{
  return (x->type == FUNCTION0
          || x->type == FUNCTION1
          || x->type == FUNCTION2
          || x->type == FUNCTION3)
    ? &scm_t : &scm_f;
}

scm *
call (scm *fn, scm *x)
{
  if (fn->type == FUNCTION0)
    return fn->function0 ();
  else if (fn->type == FUNCTION1)
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

scm *environment = &scm_nil;

scm *
lookup (char *x)
{
  if (!strcmp (x, " ()")) return &scm_nil;
  if (!strcmp (x, "#t")) return &scm_t;
  if (!strcmp (x, "#f")) return &scm_f;
  if (!strcmp (x, "'")) return &scm_quote; // assert !quote?
  if (isdigit (*x) || (*x == '-' && isdigit (*(x+1))))
    return make_number (atoi (x));

  // TODO: alist lookup symbols
  if (!strcmp (x, "label")) return &scm_label;
  if (!strcmp (x, "lambda")) return &scm_lambda;  

  if (!strcmp (x, "car")) return &scm_car;
  if (!strcmp (x, "cdr")) return &scm_cdr;
  if (!strcmp (x, "cons")) return &scm_cons;
  if (!strcmp (x, "eq")) return &scm_eq_p;
  if (!strcmp (x, "quote")) return &scm_quote;
  if (!strcmp (x, "cond")) return &scm_cond;

  if (x) {
    scm *y = make_atom (x);
    scm *r = assoc (y, environment);
    if (r != &scm_f) return cdr (r);
    return y;
  }

  return &scm_unspecified;
}

scm *
cossa (scm *x, scm *a)
{
  if (a == &scm_nil) return &scm_f;
  if (eq_p (cdar (a), x) == &scm_t)
    return car (a);
  return cossa (x, cdr (a));
}

scm *display_helper (scm*, bool, char*);

scm *
display (scm *x)
{
  return display_helper (x, false, "");
}

scm *
display_helper (scm *x, bool cont, char *sep)
{
  scm *r;
  printf (sep);
  if (x == &scm_nil) printf ("()");
  else if (x == &scm_t) printf ("#t");
  else if (x == &scm_f) printf ("#f");
  else if (x == &scm_unspecified) printf ("#<unspecified>");
  else if (x == &scm_quote) printf ("quote");

  else if (x == &scm_label) printf ("label");
  else if (x == &scm_lambda) printf ("lambda");

  else if (x == &scm_car) printf ("car");
  else if (x == &scm_cdr) printf ("cdr");
  else if (x == &scm_cons) printf ("cons");
  else if (x == &scm_cond) printf ("cond");
  else if (x == &scm_eq_p) printf ("eq");
  else if (x == &scm_null_p) printf ("null");
  else if (x == &scm_pair_p) printf ("pair");
  else if (x == &scm_quote) printf ("quote");

  else if (x->type == NUMBER) printf ("%d", x->value);
  else if (x->type == NUMBER) printf ("0");
  else if (x->type == ATOM) printf (x->name);
  else if (x->type == PAIR) {
#if QUOTE_SUGAR
    if (car (x) == &scm_quote) {
      printf ("'");
      return display_helper (car (cdr (x)), cont, "");
    }
#endif
    if (!cont) printf ("(");
    display (car (x));
    if (cdr (x)->type == PAIR)
      display_helper (cdr (x), true, " ");
    else if (cdr (x) != &scm_nil) {
      printf (" . ");
      display (cdr (x));
    }
    if (!cont) printf (")");
  }
  else if ((r = cossa (x, environment)) != &scm_f)
    printf (car (r)->name);

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
  if (c == EOF || c == '\n') return lookup (w);
  if (c == ' ') return readword ('\n', w, a);
  if (c == '(' && !w) return readlis (a);
  if (c == '(') {ungetchar (c); return lookup (w);}
  if (c == ')' && !w) {ungetchar (c); return &scm_nil;}
  if (c == ')') {ungetchar (c); return lookup (w);}
  if (c == '\'' && !w) {return cons (lookup ("'"),
                                     cons (readword (getchar (), w, a),
                                           &scm_nil));}
  if (c == ';') {readcomment (c); return readword ('\n', w, a);}
  if (c == '#' && peekchar () == '!') {getchar (); readblock (getchar ()); return readword (getchar (), w, a);}
  char s[2];
  s[0] = c;
  s[1] = 0;
  char buf[256] = "";
  return readword (getchar (), strcat (w ? w : buf, s), a);
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
read ()
{
  return readword (getchar (), 0, environment);
}
scm scm_read = {FUNCTION0, .function0 = &read};

scm *
add_environment (scm *a, char *name, scm* x)
{
  return cons (cons (make_atom (name), x), a);
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

scm scm_less_p = {FUNCTION2, .function2 = &less_p};
scm scm_minus = {FUNCTION2, .function2 = &minus};

scm *
fill_environment ()
{
  scm *a = &scm_nil;
  a = add_environment (a, "<", &scm_less_p);
  a = add_environment (a, "-", &scm_minus);
  return a;
}

int
main (int argc, char *argv[])
{
  environment = fill_environment ();

  scm *program = read ();
#if DEBUG
  puts ("");
  display (program);
  puts ("\n  =>");
#endif
  scm *result;
  result = eval (program, environment);
  display (result);
  puts ("");
  exit (0);
}
