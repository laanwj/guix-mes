#! /bin/sh
# -*-scheme-*-
exec guile -L $(pwd) -e '(scm)' -s "$0" "$@"
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

(define-module (scm)
  #:export (main))

(set-current-module
 (make-module 10 `(,(resolve-interface
                     '(guile)
                     #:select '(
                                ;; Debugging
                                apply
                                cons*
                                current-output-port
                                current-error-port
                                display
                                force-output
                                format
                                newline

                                ;; Guile admin
                                module-define!
                                resolve-interface

                                ;; PRIMITIVES
                                car
                                cdr
                                cons
                                eq?
                                null?
                                pair?
                                )
                     #:renamer (symbol-prefix-proc 'guile:)))))

(define (logf port string . rest)
  (guile:apply guile:format (guile:cons* port string rest))
  (guile:force-output port)
  #t)

(define (stderr string . rest)
  (guile:apply logf (guile:cons* (guile:current-error-port) string rest)))

(define (stdout string . rest)
  (guile:apply logf (guile:cons* (guile:current-output-port) string rest)))

(define (debug . x) #t)
;;(define debug stderr)

;; TODO
(define (atom x)
  (cond
   ((guile:pair? x) #f)
   ((guile:null? x) #f)
   (#t x)))

;; PRIMITIVES
(define car guile:car)
(define cdr guile:cdr)
(define cons guile:cons)
(define eq guile:eq?)
(define null guile:null?)


(define ATOM 'atom)
(define CAR 'car)
(define CDR 'cdr)
(define COND 'cond)
(define CONS 'cons)
(define EQ 'eq)
(define LABEL 'label)
(define LAMBDA 'lambda)
(define NIL '())
(define QUOTE 'quote)

(define (caar x) (guile:car (guile:car x)))
(define (cadr x) (guile:car (guile:cdr x)))
(define (cdar x) (guile:car (guile:cdr (guile:car x))))
(define (caddr x) (guile:car (guile:cdr (guile:cdr x))))
(define (cadar x) (guile:car (guile:cdr (guile:car x))))

;; Page 12
(define (pairlis x y a)
  (debug "pairlis x=~a y=~a a=~a\n" x y a)
  (cond
   ((null x) a)
   (#t (cons (cons (car x) (car y))
             (pairlis (cdr x) (cdr y) a)))))

(define (assoc x a)
  (debug "assoc x=~a a=~a\n" x a)
  (cond
   ((eq (caar a) x) (car a))
   (#t (assoc x (cdr a)))))

;; Page 13
(define (eval-quote fn x)
  (debug "eval-quote fn=~a x=~a" fn x)
  (apply fn x NIL))

(define (apply fn x a)
  (debug "apply fn=~a x=~a a=~a\n" fn x a)
  (cond
   ((atom fn)
    (debug "(atom fn)=~a\n" (atom fn))
    (cond
     ((eq fn CAR) (caar x))
     ((eq fn CDR) (cdar x))
     ((eq fn CONS) (cons (car x) (cadr x)))
     ((eq fn ATOM) (atom (car x)))
     ((eq fn EQ) (eq (car x) (cadr x)))
     (#t (apply (eval fn a) x a))))
   ((eq (car fn) LAMBDA) (eval (caddr fn) (pairlis (cadr fn) x a)))
   ((eq (car fn) LABEL) (apply (caddr fn) x (cons (cons (cadr fn)
                                                        (caddr fn)) a)))))

(define (eval e a)
  (debug "eval e=~a a=~a\n" e a)
  (debug "eval (atom ~a)=~a\n" e (atom e))
  (cond
   ((atom e) (cdr (assoc e a)))
   ((atom (car e))
    (cond
     ((eq (car e) QUOTE) (cadr e))
     ((eq (car e) COND) (evcon (cdr e) a))
     (#t (apply (car e) (evlis (cdr e) a) a))))
   (#t (apply (car e) (evlis (cdr e) a) a))))

(define (evcon c a)
  (debug "evcon c=~a a=~a\n" c a)
  (cond
   ((eval (caar c) a) (eval (cadar c) a))
   (#t (evcon (cdr c) a))))

(define (evlis m a)
  (debug "evlis m=~a a=~a\n" m a)
  (cond
   ((null m) NIL)
   (#t (cons (eval (car m) a) (evlis (cdr m) a)))))

(define (main arguments)
  (stdout "Hello scm\n")
  (guile:display (eval 0 '((0 . 0) (1 . 1))))
  (guile:newline)
  (guile:display (eval 1 '((0 . 0) (1 . 1))))
  (guile:newline)
  (guile:display (eval '(car '(0 1)) '((0 . 0) (1 . 1))))
  (guile:newline)
  (guile:display (eval '(cdr '(0 1)) '((0 . 0) (1 . 1))))
  (guile:newline)
  (guile:display (apply 'cons '(0 1) '((0 . 0) (1 . 1))))
  (guile:newline)
  (guile:display (eval '(cons 0 1) '((0 . 0) (1 . 1))))
  (guile:newline)
  (guile:display (apply '(lambda (x y) (cons x y)) '(0 1) '((0 . 0) (1 . 1))))
  (guile:newline)
  (guile:display (eval '((label fun (lambda (x) x)) 2 2) '((2 . 2))))
  (guile:newline))

(guile:module-define! (guile:resolve-interface '(scm)) 'main main)
