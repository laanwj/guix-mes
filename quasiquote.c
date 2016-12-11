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
SCM
unquote (SCM x) ///((no-environment))
{
  return cons (cell_symbol_unquote, x);
}

SCM
unquote_splicing (SCM x) ///((no-environment))
{
  return cons (cell_symbol_unquote_splicing, x);
}

SCM
eval_quasiquote (SCM e, SCM a)
{
  return vm_call (vm_eval_quasiquote, e, cell_undefined, a);
}

SCM
vm_eval_quasiquote ()
{
  if (r1 == cell_nil) return r1;
  else if (atom_p (r1) == cell_t) return r1;
  else if (eq_p (car (r1), cell_symbol_unquote) == cell_t)
    return eval_env (cadr (r1), r0);
  else if (TYPE (r1) == PAIR && TYPE (car (r1)) == PAIR
           && eq_p (caar (r1), cell_symbol_unquote_splicing) == cell_t)
    {
      r2 = eval_env (cadar (r1), r0);
      return append2 (r2, eval_quasiquote (cdr (r1), r0));
    }
  r2 = eval_quasiquote (car (r1), r0);
  return cons (r2, eval_quasiquote (cdr (r1), r0));
}

SCM
add_unquoters (SCM a)
{
  SCM q = assq_ref_cache (cell_symbol_the_unquoters, a);
  return append2 (q, a);
}
#else // !QUASIQUOTE

SCM add_unquoters (SCM a){}
SCM eval_quasiquote (SCM e, SCM a){}

SCM unquote (SCM x){}
SCM unquote_splicing (SCM x){}
SCM vm_eval_quasiquote () {}

#endif // QUASIQUOTE

#if QUASISYNTAX
SCM
syntax (SCM x)
{
  return cons (cell_symbol_syntax, x);
}

SCM
unsyntax (SCM x) ///((no-environment))
{
  return cons (cell_symbol_unsyntax, x);
}

SCM
unsyntax_splicing (SCM x) ///((no-environment))
{
  return cons (cell_symbol_unsyntax_splicing, x);
}

SCM
eval_quasisyntax (SCM e, SCM a)
{
  if (e == cell_nil) return e;
  else if (atom_p (e) == cell_t) return e;
  else if (eq_p (car (e), cell_symbol_unsyntax) == cell_t)
    return eval_env (cadr (e), a);
  else if (TYPE (e) == PAIR && TYPE (car (e)) == PAIR
           && eq_p (caar (e), cell_symbol_unsyntax_splicing) == cell_t)
      return append2 (eval_env (cadar (e), a), eval_quasisyntax (cdr (e), a));
  return cons (eval_quasisyntax (car (e), a), eval_quasisyntax (cdr (e), a));
}

SCM
add_unsyntaxers (SCM a)
{
  a = cons (cons (cell_symbol_unsyntax, cell_unsyntax), a);
  a = cons (cons (cell_symbol_unsyntax_splicing, cell_unsyntax_splicing), a);
  return a;
}

#else // !QUASISYNTAX
SCM syntax (SCM x){}
SCM unsyntax (SCM x){}
SCM unsyntax_splicing (SCM x){}
SCM add_unsyntaxers (SCM a){}
SCM eval_quasisyntax (SCM e, SCM a){}

#endif // !QUASISYNTAX
