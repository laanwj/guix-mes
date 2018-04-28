;;; -*-scheme-*-

;;; Mes --- Maxwell Equations of Software
;;; Copyright © 2017,2018 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
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

;;; Commentary:

;;; Code:

(define-module (mes guile)
  #:export (
            <cell:char>
            <cell:keyword>
            <cell:number>
            <cell:pair>
            <cell:string>
            <cell:symbol>
            <cell:vector>

            append2
            core:apply
            core:display
            core:display-error
            core:display-port
            core:exit
            core:macro-expand
            core:write
            core:write-error
            core:write-port
            core:type)
  ;;#:re-export (open-input-file open-input-string with-input-from-string)
  )

(cond-expand
 (guile
  (define core:exit exit)
  (define core:display display)
  (define core:display-port display)
  (define (core:display-error o) (display o (current-error-port)))
  (define core:write write)
  (define (core:write-error o) (write o (current-error-port)))
  (define core:write-port write)
  (define core:macro-expand identity)
  (define (core:apply f a . m) (apply f a))
  (define append2 append)

  (define guile:keyword? keyword?)
  (define guile:number? number?)
  (define guile:pair? pair?)
  (define guile:string? string?)
  (define guile:symbol? symbol?)

  (define <cell:char> 0)
  (define <cell:keyword> 4)
  (define <cell:number> 6)
  (define <cell:pair> 7)
  (define <cell:string> 10)
  (define <cell:symbol> 11)
  (define <cell:vector> 15)

  (define (core:type x)
    (cond ((guile:keyword? x) <cell:keyword>)
          ((guile:number? x) <cell:number>)
          ((guile:pair? x) <cell:pair>)
          ((guile:string? x) <cell:string>)
          ((guile:symbol? x) <cell:symbol>))))

 (mes))

(cond-expand
 (guile-2.2)
 (guile-2
  (eval-when (compile load eval)
    (define-syntax include-from-path
      (lambda (x)
        (syntax-case x ()
          ((k filename)
           (let ((fn (syntax->datum #'filename)))
             (with-syntax ((fn (datum->syntax
                                #'filename
                                (canonicalize-path
                                 (or (%search-load-path fn)
                                     (syntax-violation 'include-from-path
                                                       "file not found in path"
                                                       x #'filename))))))
               #'(include fn))))))))
  (export include-from-path))
 (guile
  (use-modules (ice-9 syncase))
  (define (compose proc . rest)
  (if (null? rest) proc
      (lambda args
        (proc (apply (apply compose rest) args)))))
  (export compose))
 (mes))
