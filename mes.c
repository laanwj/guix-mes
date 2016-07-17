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

#define BOOT 1
#define MACROS 1
#define QUASIQUOTE 1
#define QUOTE_SUGAR 1

enum type {CHAR, NUMBER, PAIR, STRING, SYMBOL, VALUES, VECTOR,
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
    struct scm_t** vector;
  };
} scm;

#define MES 1
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
scm scm_lambda = {SYMBOL, "lambda"};
scm scm_label = {SYMBOL, "label"};
scm scm_unspecified = {SYMBOL, "*unspecified*"};
scm scm_symbol_cond = {SYMBOL, "cond"};
scm scm_symbol_quote = {SYMBOL, "quote"};
#if QUASIQUOTE
scm scm_symbol_quasiquote = {SYMBOL, "quasiquote"};
scm scm_symbol_unquote = {SYMBOL, "unquote"};
scm scm_symbol_unquote_splicing = {SYMBOL, "unquote-splicing"};
#endif
#if MACROS
scm scm_macro = {SYMBOL, "*macro*"};
#endif

scm scm_symbol_EOF = {SYMBOL, "EOF"};
scm scm_symbol_EOF2 = {SYMBOL, "EOF2"};
scm scm_symbol_call_with_values = {SYMBOL, "call-with-values"};
scm scm_symbol_current_module = {SYMBOL, "current-module"};
scm scm_symbol_define = {SYMBOL, "define"};
scm scm_symbol_define_macro = {SYMBOL, "define-macro"};
scm scm_symbol_eval = {SYMBOL, "eval"};
scm scm_symbol_loop2 = {SYMBOL, "loop2"};
scm scm_symbol_set_x = {SYMBOL, "set!"};
scm scm_symbol_values = {SYMBOL, "values"};

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
              && x->type != CHAR
              && y->type != CHAR
              && x->type != NUMBER
              && y->type != NUMBER
              && x->type != VECTOR
              && y->type != VECTOR
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
  return cons (&scm_symbol_quote, x);
}

#if QUASIQUOTE
scm *
quasiquote (scm *x)
{
  return cons (&scm_symbol_quasiquote, x);
}

scm *
unquote (scm *x)
{
  return cons (&scm_symbol_unquote, x);
}

scm *
unquote_splicing (scm *x)
{
  return cons (&scm_symbol_unquote_splicing, x);
}
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
apply_env_ (scm *fn, scm *x, scm *a)
{
#if DEBUG
  printf ("apply_env fn=");
  display (fn);
  printf (" x=");
  display (x);
  puts ("");
#endif
#if MACROS
  scm *macro;
#endif
  if (atom_p (fn) != &scm_f)
    {
      if (fn == &scm_symbol_current_module) // FIXME
        return a;
      if (eq_p (fn, &scm_symbol_call_with_values) == &scm_t)
        return call (&scm_call_with_values_env, append2 (x, cons (a, &scm_nil)));
      if (builtin_p (fn) == &scm_t)
        return call (fn, x);
      return apply_env (eval (fn,  a), x, a);
    }
  else if (car (fn) == &scm_lambda)
    return begin_env (cddr (fn), pairlis (cadr (fn), x, a));
  else if (car (fn) == &scm_label)
    return apply_env (caddr (fn), x, cons (cons (cadr (fn), caddr (fn)), a));
#if MACROS
  else if ((macro = assq (car (fn), cdr (assq (&scm_macro, a)))) != &scm_f) {
#if DEBUG
    printf ("APPLY GOTTA MACRO! name=");
    display (car (fn));
    printf (" body=");
    display (cdr (macro));
    printf (" args=");
    display (cdr (fn));
    puts ("");
#endif
    scm *r = apply_env (cdr (macro), cdr (fn), a);
#if DEBUG
    printf ("APPLY MACRO GOT: ==> ");
    display (r);
    puts ("");
#endif
    return apply_env (r, x, a);
    //return eval_ (cons (r, x), a);
    //return apply_env_ (eval (cdr (macro), a), x, a);
    //return eval (apply_env_ (cdr (macro), x, a), a);
    //return eval (apply_env_ (eval (cdr (macro), a), x, a), a);
  }
#endif // MACROS
  return &scm_unspecified;
}

scm *
eval_ (scm *e, scm *a)
{
#if DEBUG
  printf ("eval e=");
  display (e);
  puts ("");
#endif
  if (e->type == CHAR)
    return e;
  else if (e->type == NUMBER)
    return e;
  else if (e->type == STRING)
    return e;
  else if (e->type == VECTOR)
    return e;
  else if (atom_p (e) == &scm_t) {
    scm *y = assq (e, a);
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
      else if (eq_p (car (e), &scm_symbol_define_macro) == &scm_t)
        return define_macro (e, a);
      else if ((macro = assq (car (e), cdr (assq (&scm_macro, a)))) != &scm_f) {
#if DEBUG
        printf ("GOTTA MACRO! name=");
        display (car (e));
        printf (" body=");
        display (cdr (macro));
        printf (" args=");
        display (cdr (e));
        puts ("");
#endif
        return eval (apply_env_ (cdr (macro), cdr (e), a), a);
      }
#endif // MACROS
      return apply_env (car (e), evlis (cdr (e), a), a);
    }
  return apply_env (car (e), evlis (cdr (e), a), a);
}

scm *
evcon_ (scm *c, scm *a)
{
#if DEBUG
  printf ("evcon_ clause=");
  display (car (c));
  puts ("");
#endif
  if (c == &scm_nil) return &scm_unspecified;
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
          || x->type == FUNCTION3
          || x->type == FUNCTIONn)
    ? &scm_t : &scm_f;
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
  //TODO: #f,#t,nil also `symbols' atm
  return x->type == SYMBOL ? &scm_t : &scm_f;
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

#if 0
scm *
builtin_list (scm *x/*...*/) // int
{
  return x;
}

scm *
vector (scm *x/*...*/) // int
{
  return list_to_vector (x);
}
#endif

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
  scm *v = apply_env_ (producer, &scm_nil, a);
  if (v->type == VALUES)
    v = v->cdr;
  return apply_env_ (consumer, v, a);
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
  if (*x == '\'') return &scm_symbol_quote;

  if (!strcmp (x, scm_unspecified.name)) return &scm_unspecified;
  if (!strcmp (x, scm_symbol_cond.name)) return &scm_symbol_cond;
  if (!strcmp (x, scm_symbol_quote.name)) return &scm_symbol_quote;
  if (!strcmp (x, scm_lambda.name)) return &scm_lambda;
  if (!strcmp (x, scm_label.name)) return &scm_label;
  if (!strcmp (x, scm_nil.name)) return &scm_nil;
  if (!strcmp (x, scm_symbol_set_x.name)) return &scm_symbol_set_x;

#if QUASIQUOTE
  if (*x == '`') return &scm_symbol_quasiquote;
  if (*x == ',' && *(x+1) == '@') return &scm_symbol_unquote_splicing;
  if (*x == ',') return &scm_symbol_unquote;
  if (!strcmp (x, scm_symbol_quasiquote.name)) return &scm_symbol_quasiquote;
  if (!strcmp (x, scm_symbol_unquote.name)) return &scm_symbol_unquote;
  if (!strcmp (x, scm_symbol_unquote_splicing.name)) return &scm_symbol_unquote_splicing;
#endif

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
number_to_string (scm *x)
{
  assert (x->type == NUMBER);
  char buf[256];
  sprintf (buf,"%d", x->value);
  return make_string (buf);
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
  if (x->type == CHAR && x->value == 10) printf ("#\\%s", "newline");
  else if (x->type == CHAR && x->value == 32) printf ("#\\%s", "space");
  else if (x->type == CHAR) printf ("#\\%c", x->value);
  else if (x->type == NUMBER) printf ("%d", x->value);
  else if (x->type == PAIR) {
#if QUOTE_SUGAR
    if (car (x) == &scm_quote) {
      printf ("'");
      return display_helper (car (cdr (x)), cont, "", true);
    }
#if QUASIQUOTE
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
  else if (x->type == VECTOR) {
    printf ("#(");
    for (int i = 0; i < x->length; i++)
      display_helper (x->vector[i], true, i ? " " : "", false);
    printf (")");
  }
  else if (atom_p (x) == &scm_t) printf ("%s", x->name);

  //return &scm_unspecified;
  return x; // FIXME: eval helper for macros
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
#if QUASIQUOTE
       || c == '`'
       || c == ','
#endif
       )
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
#if DEBUG
  scm *e = readword (getchar (), 0, a);
  printf ("readenv: ");
  display (e);
  puts ("");
  return e;
#else
  return readword (getchar (), 0, a);
#endif
}

// Extras to make interesting program

scm *
hello_world ()
{
  puts ("c: hello world");
  return &scm_unspecified;
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
  // else if (eq_p (car (e), &scm_symbol_quote) == &scm_t)
  //   return cons (car (e), eval_quasiquote (cdr (e), a));
  // else if (eq_p (car (e), &scm_symbol_quasiquote) == &scm_t)
  //   return cons (e, eval_quasiquote (cdr (e), a));
  else if (eq_p (car (e), &scm_symbol_unquote) == &scm_t)
    return eval (cadr (e), a);
  // else if (atom_p (car (e)) == &scm_t)
  //   return cons (car (e), eval_quasiquote (cdr (e), a));
  else if (e->type == PAIR && e->car->type == PAIR
           && eq_p (caar (e), &scm_symbol_unquote_splicing) == &scm_t)
      return append2 (eval_ (cadar (e), a), eval_quasiquote (cdr (e), a));
  return cons (eval_quasiquote (car (e), a), eval_quasiquote (cdr (e), a));
}
#endif

scm *
add_environment (scm *a, char *name, scm *x)
{
  return cons (cons (make_symbol (name), x), a);
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
make_lambda (scm *args, scm *body)
{
  return cons (&scm_lambda, cons (args, body));
}

scm *
define (scm *x, scm *a)
{
  if (atom_p (cadr (x)) != &scm_f)
    return cons (cadr (x), eval (caddr (x), a));
  return cons (caadr (x), make_lambda (cdadr (x), cddr (x)));
}

scm *
define_macro (scm *x, scm *a)
{
#if DEBUG
  scm *name = caadr (x);
  scm *args = cdadr (x);
  scm *body = cddr (x);
  printf ("\nc:define_macro name=");
  display (name);
  printf (" args=");
  display (args);
  printf (" body=");
  display (body);
  printf ("\nmacro=");
  scm *aa =cons (&scm_macro,
                 cons (cons (name, make_lambda (args, body)),
                       cdr (assq (&scm_macro, a))));
  display (aa);
  puts ("");
#endif
  scm *macros = assq (&scm_macro, a);
  scm *macro;
  if (atom_p (cadr (x)) != &scm_f)
    macro = cons (cadr (x), eval (caddr (x), a));
  else
    macro = cons (caadr(x), make_lambda (cdadr (x), cddr (x)));
  set_cdr_x (macros, cons (macro, cdr (macros)));
  return a;
}

scm *
begin_env (scm *body, scm *a)
{
  if (body == &scm_nil) return &scm_unspecified;
  scm *e = car (body);
#if DEBUG
  printf ("\nc:begin_env e=");
  display (e);
  puts ("");
#endif
  if (e->type == PAIR) {
    if (eq_p (car (e), &scm_symbol_define) == &scm_t)
      return begin_env (cdr (body), cons (define (e, a), a));
    else if (eq_p (car (e), &scm_symbol_define_macro) == &scm_t)
      return begin_env (cdr (body), cons (define_macro (e, a), a));
    else if (eq_p (car (e), &scm_symbol_set_x) == &scm_t) {
      set_env_x (cadr (e), eval (caddr (e), a), a);
      return begin_env (cdr (body), a);
    }
#if BOOT
    else if (eq_p (e, &scm_symbol_EOF) == &scm_t)
      return apply_env (cdr (assq (&scm_symbol_loop2, a)),
                        cons (&scm_unspecified, cons (&scm_t, cons (a, &scm_nil))), a);
    else if (eq_p (e, &scm_symbol_EOF2) == &scm_t)
      return make_symbol ("exit boot");
#endif
  }
  scm *result = eval (e, a);
  if (cdr (body) == &scm_nil)
    return result;
  return begin_env (cdr (body), a);
}

scm *
read_file (scm *e, scm *a)
{
  if (e == &scm_nil) return e;
  return cons (e, read_file (readenv (a), a));
}

scm *
apply_env (scm *fn, scm *x, scm *a)
{
#if DEBUG
  printf ("\nc:apply_env fn=");
  display (fn);
  printf (" x=");
  display (x);
  puts ("");
#endif
  if (fn == &scm_apply_env_)
    return eval_ (x, a);
  return apply_env_ (fn, x, a);
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

  scm *eval__ = assq (&scm_symbol_eval, a);
  assert (eval__ != &scm_f);
  eval__ = cdr (eval__);
  if (builtin_p (eval__) == &scm_t
      || evalling_p)
    return eval_ (e, a);
  evalling_p = true;
  scm *r = apply_env (eval__, cons (e, cons (a, &scm_nil)), a);
  evalling_p = false;
  return r;
}

int
main (int argc, char *argv[])
{
  scm *a = mes_environment ();
  display (begin_env (read_file (readenv (a), a), a));
  newline ();
  return 0;
}
