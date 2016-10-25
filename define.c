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
define (scm *x, scm *a)
{
  scm *e;
  scm *name = cadr (x);
  if (name->type != PAIR)
    e = builtin_eval (caddr (x), cons (cons (cadr (x), cadr (x)), a));
  else {
    name = car (name);
    scm *p = pairlis (cadr (x), cadr (x), a);
    cache_invalidate_range (p, a);
    e = builtin_eval (make_lambda (cdadr (x), cddr (x)), p);
  }
  if (eq_p (car (x), &symbol_define_macro) == &scm_t)
    e = make_macro (name, e);
  scm *entry = cons (name, e);
  scm *aa = cons (entry, &scm_nil);
  set_cdr_x (aa, cdr (a));
  set_cdr_x (a, aa);
  scm *cl = assq (&scm_closure, a);
  set_cdr_x (cl, aa);
  return entry;
}
#else // BOOT
scm*define (scm *x, scm *a){}
#endif

scm *
define_macro (scm *x, scm *a)
{
}
