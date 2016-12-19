#! /bin/sh
# -*-scheme-*-
exec ${GUILE-guile} -L $(pwd)/module -e '(nyacc)' -s "$0" "$@"
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

;; Tcalc.scm - calculator
;;
;; Copyright (C) 2015 Matthew R. Wette
;; 
;; Copying and distribution of this file, with or without modification,
;; are permitted in any medium without royalty provided the copyright
;; notice and this notice are preserved.  This file is offered as-is,
;; without any warranty.

(define-module (nyacc)
  #:use-module (ice-9 syncase) ;; guile-1.8
  #:use-module (ice-9 optargs) ;; guile-1.8
  #:use-module (nyacc lalr)
  #:use-module (nyacc lex)
  #:use-module (nyacc parse)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 pretty-print)
  #:export (main))

(define simple-spec
  (lalr-spec
   (prec< (left "+" "-") (left "*" "/"))
   (start expr)
   (grammar
    (expr
     (expr "+" expr ($$ (+ $1 $3)))
     (expr "-" expr ($$ (- $1 $3)))
     (expr "*" expr ($$ (* $1 $3)))
     (expr "/" expr ($$ (/ $1 $3)))
     ("*" $error)
     ($fixed ($$ (string->number $1)))
     ($float ($$ (string->number $1)))
     ("(" expr ")" ($$ $2))))))

(define simple-mach (make-lalr-machine simple-spec))
;; OR
;; (use-modules (nyacc bison))
;; (define simple-mach (make-lalr-machine/bison simple-spec))

(define match-table (assq-ref simple-mach 'mtab))

(define gen-lexer (make-lexer-generator match-table))

(define parse (make-lalr-parser simple-mach))

(define demo-string "2 + 2")

(define (main arguments)
  (display demo-string)
  (display " => ")
  (display (with-input-from-string demo-string
             (lambda () (parse (gen-lexer)))))
  (newline))
