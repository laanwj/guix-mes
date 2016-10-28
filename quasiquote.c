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

#if QUASIQUOTE
scm *add_environment (scm *a, char const *name, scm *x);

scm *
unquote (scm *x) ///((no-environment))
{
  return cons (&symbol_unquote, x);
}

scm *
unquote_splicing (scm *x) ///((no-environment))
{
  return cons (&symbol_unquote_splicing, x);
}

scm *
eval_quasiquote (scm *e, scm *a)
{
  return vm_call (vm_eval_quasiquote, e, &scm_undefined, a);
}

scm *
vm_eval_quasiquote ()
{
  if (r1 == &scm_nil) return r1;
  else if (atom_p (r1) == &scm_t) return r1;
  else if (eq_p (car (r1), &symbol_unquote) == &scm_t)
    return eval_env (cadr (r1), r0);
  else if (r1->type == PAIR && r1->car->type == PAIR
           && eq_p (caar (r1), &symbol_unquote_splicing) == &scm_t)
    {
      r2 = eval_env (cadar (r1), r0);
      return append2 (r2, eval_quasiquote (cdr (r1), r0));
    }
  r2 = eval_quasiquote (car (r1), r0);
  return cons (r2, eval_quasiquote (cdr (r1), r0));
}

scm *
the_unquoters = &scm_nil;

scm *
add_unquoters (scm *a)
{
  if (the_unquoters == &scm_nil)
    the_unquoters = cons (cons (&symbol_unquote, &scm_unquote),
                          cons (cons (&symbol_unquote_splicing, &scm_unquote_splicing),
                                &scm_nil));
  return append2 (the_unquoters, a);
}
#else // !QUASIQUOTE

scm*add_unquoters (scm *a){}
scm*eval_quasiquote (scm *e, scm *a){}

#endif // QUASIQUOTE

#if QUASISYNTAX
scm *
syntax (scm *x)
{
  return cons (&symbol_syntax, x);
}

scm *
unsyntax (scm *x) ///((no-environment))
{
  return cons (&symbol_unsyntax, x);
}

scm *
unsyntax_splicing (scm *x) ///((no-environment))
{
  return cons (&symbol_unsyntax_splicing, x);
}

scm *
eval_quasisyntax (scm *e, scm *a)
{
  if (e == &scm_nil) return e;
  else if (atom_p (e) == &scm_t) return e;
  else if (eq_p (car (e), &symbol_unsyntax) == &scm_t)
    return eval_env (cadr (e), a);
  else if (e->type == PAIR && e->car->type == PAIR
           && eq_p (caar (e), &symbol_unsyntax_splicing) == &scm_t)
      return append2 (eval_env (cadar (e), a), eval_quasisyntax (cdr (e), a));
  return cons (eval_quasisyntax (car (e), a), eval_quasisyntax (cdr (e), a));
}

scm *
add_unsyntaxers (scm *a)
{
  a = cons (cons (&symbol_unsyntax, &scm_unsyntax), a);
  a = cons (cons (&symbol_unsyntax_splicing, &scm_unsyntax_splicing), a);
  return a;
}

#else // !QUASISYNTAX
scm*syntax (scm *x){}
scm*unsyntax (scm *x){}
scm*unsyntax_splicing (scm *x){}
scm*add_unsyntaxers (scm *a){}
scm*eval_unsyntax (scm *e, scm *a){}
scm*eval_quasisyntax (scm *e, scm *a){}

#endif // !QUASISYNTAX
