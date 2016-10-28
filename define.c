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

#if !BOOT
scm *
define_env (scm *e, scm *a)
{
  return vm_call (vm_define_env, e, &scm_undefined, a);
}

scm *
vm_define_env ()
{
  scm *x;
  scm *name = cadr (r1);
  if (name->type != PAIR)
    x = eval_env (caddr (r1), cons (cons (cadr (r1), cadr (r1)), r0));
  else {
    name = car (name);
    scm *p = pairlis (cadr (r1), cadr (r1), r0);
    cache_invalidate_range (p, r0);
    x = eval_env (make_lambda (cdadr (r1), cddr (r1)), p);
  }
  if (eq_p (car (r1), &symbol_define_macro) == &scm_t)
    x = make_macro (name, x);
  
  scm *entry = cons (name, x);
  scm *aa = cons (entry, &scm_nil);
  set_cdr_x (aa, cdr (r0));
  set_cdr_x (r0, aa);
  scm *cl = assq (&scm_closure, r0);
  set_cdr_x (cl, aa);
  return entry;
}
#else // BOOT
scm*define_env (scm *r1, scm *a){}
scm*vm_define_env (scm *r1, scm *a){}
#endif

scm *
define_macro (scm *r1, scm *a)
{
}
