;;; Mes --- Maxwell Equations of Software
;;; Copyright © 2018 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
;;;
;;; This file is part of Mes.
;;;
;;; Mes is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; Mes is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with Mes.  If not, see <http://www.gnu.org/licenses/>.

(define (map1 f lst)
  (if (null? lst) (list)
      (cons (f (car lst)) (map1 f (cdr lst)))))

(define (cadr x) (car (cdr x)))

(define-macro (let bindings . rest)
  (cons (cons 'lambda (cons (map1 car bindings) rest))
        (map1 cadr bindings)))

(let ((x 0)) x)
(let ((y 0)) y)
(exit (let ((xx 0)) xx))
(exit 1)
