#! /bin/sh
# -*-scheme-*-
exec guile -L $(pwd) -e '(mes)' -s "$0" "$@"
!#

;;; Mes --- The Maxwell Equations of Software
;;; Copyright © 2016,2018 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
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

(let ((guile (resolve-interface
              '(guile)
              #:select `(
                         ;; Debugging
                         apply
                         cons*
                         current-module
                         display
                         eof-object?
                         eval
                         exit
                         force-output
                         format
                         list
                         map
                         newline
                         read
                         
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
                         *unspecified*
                         
                         ;; READER
                         char->integer
                         integer->char
                         
                         ;; non-primitive BUILTINS
                         char?
                         number?
                         procedure?
                         string?
                         <
                         -
                         )
              #:renamer (symbol-prefix-proc 'guile:)))
      (guile-2.0 (resolve-interface '(guile) #:select '(define)))
      (guile-2.2 (resolve-interface '(guile) #:select '(quasiquote unquote)))
      (ports (resolve-interface
              (if (equal? (effective-version) "2.0")'(guile) '(ice-9 ports))
                  #:select '(
                             ;; Debugging
                             current-error-port
                             current-output-port
                             
                             ;; READER
                             ;;peek-char
                             read-char
                             unread-char)
                  #:renamer (symbol-prefix-proc 'guile:))))
  (set-current-module
   (make-module 10 `(,guile ,guile-2.0 ,guile-2.2 ,ports))))

(define (logf port string . rest)
  (guile:apply guile:format (guile:cons* port string rest))
  (guile:force-output port)
  #t)

(define (stderr string . rest)
  (guile:apply logf (guile:cons* (guile:current-error-port) string rest)))

(define (stdout string . rest)
  (guile:apply logf (guile:cons* (guile:current-output-port) string rest)))

(define (debug . x) #t)
(define debug stderr)

;; TODO
(define (atom? x)
  (cond
   ((guile:pair? x) #f)
   ((guile:null? x) #f)
   (#t #t)))

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
;;(define peek-byte guile:peek-char)
(define (read-byte)
  (char->integer (guile:read-char)))
(define (unread-byte x)
  (guile:unread-char (guile:integer->char x))
  x)
(define (lookup x a)
  ;; TODO
  (stderr "lookup x=~a\n" x)
  x)

(define (char->integer c)
  (if (guile:eof-object? c) -1 (guile:char->integer c)))

(include "mes.mes")
;; guile-2.2 only, guile-2.0 has no include?
(include "reader.mes")

(define (append2 x y)
  (cond ((null? x) y)
        (#t (cons (car x) (append2 (cdr x) y)))))

;; READER: TODO lookup
(define (read)
  (let ((x (guile:read)))
    (if (guile:eof-object? x) '()
        x)))

(define (lookup-macro e a)
  #f)

(define guile:dot '#{.}#)

(define environment
  (guile:map
   (lambda (x) (cons (car x) (guile:eval (cdr x) (guile:current-module))))
   '(
     (*closure* . #t)
     ((guile:list) . (guile:list))
     (#t . #t)
     (#f . #f)
    
     (*unspecified* . guile:*unspecified*)

     (atom? . atom?)
     (car . car)
     (cdr . cdr)
     (cons . cons)
     ;; (cond . evcon)
     (eq? . eq?)

     (null? . null?)
     (pair? . guile:pair?)
     ;; (quote . quote)

     (evlis-env . evlis-env)
     (evcon . evcon)
     (pairlis . pairlis)
     (assq . assq)
     (assq-ref-env . assq-ref-env)

     (eval-env . eval-env)
     (apply-env . apply-env)

     (read . read)
     (display . guile:display)
     (newline . guile:newline)

     (builtin? . builtin?)
     (number? . number?)
     (call . call)

     (< . guile:<)
     (- . guile:-)

     ;; DERIVED
     (caar . caar)
     (cadr . cadr)
     (cdar . cdar)
     (cddr . cddr)
     (caadr . caadr)
     (caddr . caddr)
     (cdadr . cdadr)
     (cadar . cadar)
     (cddar . cddar)
     (cdddr . cdddr)

     (append2 . append2)
     (exit . guile:exit)

     (*macro* . (guile:list))
     (*dot* . guile:dot)

     ;;
     (stderr . stderr))))

(define (main arguments)
  (let ((program (cons 'begin (read-input-file))))
    (stderr "program:~a\n" program)
    (stderr "=> ~s\n" (eval-env program environment)))
  (guile:newline))

(guile:module-define! (guile:resolve-interface '(mes)) 'main main)
