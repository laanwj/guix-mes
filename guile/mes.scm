#! /bin/sh
# -*-scheme-*-
exec guile -L $(pwd) -e '(mes)' -s "$0" "$@"
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

(define-module (mes)
  #:export (main))

(set-current-module
 (make-module 10 `(,(resolve-interface
                     '(guile)
                     #:select '(
                                ;; Debugging
                                apply
                                cons*
                                current-error-port
                                current-output-port
                                display
                                eof-object?
                                exit
                                force-output
                                format
                                newline
                                read
                                with-input-from-string

                                ;; Guile admin
                                module-define!
                                resolve-interface

                                ;; PRIMITIVE BUILTINS
                                car
                                cdr
                                cons
                                eq?
                                null?
                                pair?

                                ;; READER
                                char->integer
                                integer->char
                                read-char
                                unread-char
                                
                                ;; non-primitive BUILTINS
                                char?
                                number?
                                procedure?
                                string?
                                <
                                -
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
(define (atom? x)
  (cond
   ((guile:pair? x) #f)
   ((guile:null? x) #f)
   (#t x)))

;; PRIMITIVES
(define car guile:car)
(define cdr guile:cdr)
(define cons guile:cons)
(define eq? guile:eq?)
(define null? guile:null?)
(define pair? guile:pair?)
(define builtin? guile:procedure?)
(define char? guile:char?)
(define number? guile:number?)
(define string? guile:number?)
(define call guile:apply)
(define (peek-byte)
  (unread-byte (read-byte)))
(define (read-byte)
  (guile:char->integer (guile:read-char)))
(define (unread-byte x)
  (guile:unread-char (guile:integer->char x))
  x)
(define (lookup x a)
  ;; TODO
  (stderr "lookup x=~a\n" x)
  x)

(include "mes.mes")

(define (append2 x y)
  (cond ((null? x) y)
        (#t (cons (car x) (append2 (cdr x) y)))))

(define (eval-environment e a)
  (eval e (append2 a environment)))

(define (apply-environment fn e a)
  (apply-env fn e (append2 a environment)))

;; READER: TODO lookup
(define (readenv a)
  (let ((x (guile:read)))
    (if (guile:eof-object? x) '()
        x)))

(define environment
  `(
    (() . ())
    (#t . #t)
    (#f . #f)
    
    (*unspecified* . ,*unspecified*)

    (atom? . ,atom?)
    (car . ,car)
    (cdr . ,cdr)
    (cons . ,cons)
    (cond . ,evcon)
    (eq? . ,eq?)

    (null? . ,null?)
    (pair? . ,guile:pair?)
    ;;(quote . ,quote)

    (evlis . ,evlis)
    (evcon . ,evcon)
    (pairlis . ,pairlis)
    (assq . ,assq)

    (eval . ,eval-environment)
    (apply-env . ,apply-environment)

    (readenv . ,readenv)
    (display . ,guile:display)
    (newline . ,guile:newline)

    (builtin? . ,builtin?)
    (number? . ,number?)
    (call . ,call)

    (< . ,guile:<)
    (- . ,guile:-)

    ;; DERIVED
    (caar . ,caar)
    (cadr . ,cadr)
    (cdar . ,cdar)
    (cddr . ,cddr)
    (caadr . ,caadr)
    (caddr . ,caddr)
    (cdadr . ,cdadr)
    (cadar . ,cadar)
    (cddar . ,cddar)
    (cdddr . ,cdddr)

    (append2 . ,append2)
    (exit . ,guile:exit)

    (*macro* . ())

    ;;
    (stderr . ,stderr)))

(define (mes-define-lambda x a)
  (cons (caadr x) (cons 'lambda (cons (cdadr x) (cddr x)))))

(define (mes-define x a)
  (if (atom? (cadr x))
      (cons (cadr x) (eval (caddr x) a))
      (mes-define-lambda x a)))

(define (mes-define-macro x a)
  (cons '*macro*
        (cons (mes-define-lambda x a)
              (cdr (assq '*macro* a)))))

(define (loop r e a)
  (cond ((null? e) r)
        ((eq? e 'exit)
         (apply-env (cdr (assq 'loop a))
                    (cons *unspecified* (cons #t (cons a '())))
                    a))
        ((atom? e) (loop (eval e a) (readenv a) a))
        ((eq? (car e) 'define)
         (loop *unspecified* (readenv a) (cons (mes-define e a) a)))
        ((eq? (car e) 'define-macro)
         (loop *unspecified* (readenv a) (cons (mes-define-macro e a) a)))
        (#t (loop (eval e a) (readenv a) a))))

(define (main arguments)
  (let ((a (append2 environment `((*a* . ,environment)))))
    ;;(guile:display (eval (readenv a) a))
    (guile:display (loop *unspecified* (readenv a) a))
    )
  (guile:newline))

(guile:module-define! (guile:resolve-interface '(mes)) 'main main)
