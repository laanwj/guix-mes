#! /bin/sh
# -*-scheme-*-
export GUILE_AUTO_COMPILE=0
exec ${GUILE-guile} -L $(pwd)/guile -e '(nyacc)' -s "$0" "$@"
!#

;;; Mes --- The Maxwell Equations of Software
;;; Copyright © 2016 Jan Nieuwenhuizen <janneke@gnu.org>
;;;
;;; This file is part of GNU Guix.
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

;; The Maxwell Equations of Software -- John McCarthy page 13
;; http://www.softwarepreservation.org/projects/LISP/book/LISP%201.5%20Programmers%20Manual.pdf

#!
Run with Guile-1.8:
GUILE='~/src/guile-1.8/build/pre-inst-guile --debug -q' guile/nyacc.scm
!#

(cond-expand
 (guile-2)
 (guile
  (use-modules (ice-9 syncase))
  (use-modules (ice-9 optargs))))

(define-module (nyacc)
  #:use-module (nyacc lang c99 parser)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 pretty-print)
  #:export (main))

(define (main arguments)
  (let* ((file (if (> (length arguments) 1) (cadr arguments)
                   "doc/examples/main.c"))
         (ast (with-input-from-file file
                (lambda () (parse-c99 #:inc-dirs '())))))
    (pretty-print ast)))
