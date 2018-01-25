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

(define-macro (or . x)
  (if (null? x) #f
      (if (null? (cdr x)) (car x)
          (list (list (quote lambda) (list (quote r))
                      (list (quote if) (quote r) (quote r)
                            (cons (quote or) (cdr x))))
                (car x)))))

(define (f a)
  (or #t a))

(define-macro (foo bar)
  (list f bar))

(foo 3)

(if #t (foo 3))
