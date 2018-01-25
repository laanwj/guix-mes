;;; Mes --- Maxwell Equations of Software
;;; Copyright © 2018 Jan Nieuwenhuizen <janneke@gnu.org>
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

(define map 'boo)
(define (map f lst)
  (if (null? lst) (list)
      (cons (f (car lst)) (map f (cdr lst)))))

(define (cadr x) (car (cdr x)))

(define-macro (let bindings . rest)
  (cons (cons 'lambda (cons (map car bindings) rest))
        (map cadr bindings)))

(let ((a 0)
      (b 1)
      (c 2)
      (d 3)
      (e 4)
      (f 5)
      (g 6)
      (h 7)
      (i 8))
  (+ a b))
