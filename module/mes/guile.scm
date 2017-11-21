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
            core:type
            pmatch-car
            pmatch-cdr
            )
  ;;#:re-export (open-input-file open-input-string with-input-from-string)
  )

(cond-expand
 (guile
  (define pmatch-car car)
  (define pmatch-cdr cdr)
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
  (define (core:type x)
    (define <cell:keyword> 4)
    (define <cell:number> 6)
    (define <cell:pair> 7)
    (define <cell:string> 10)
    (define <cell:symbol> 11)
    (cond ((guile:keyword? x) <cell:keyword>)
          ((guile:number? x) <cell:number>)
          ((guile:pair? x) <cell:pair>)
          ((guile:string? x) <cell:string>)
          ((guile:symbol? x) <cell:symbol>)))

;;   (define core:open-input-file open-input-file)
;;   (define (open-input-file file)
;;     (let ((port (core:open-input-file file)))
;;       (when (getenv "MES_DEBUG")
;;         (core:display-error (string-append "open-input-file: `" file " port="))
;;         (core:display-error port)
;;         (core:display-error "\n"))
;;       port))

;;   (define core:open-input-string open-input-string)
;;   (define (open-input-string string)
;;     (let ((port (core:open-input-string string)))
;;       (when (getenv "MES_DEBUG")
;;         (core:display-error (string-append "open-input-string: `" string " port="))
;;         (core:display-error port)
;;         (core:display-error "\n"))
;;       port))

;;   (define core:with-input-from-string with-input-from-string)
;;   (define (with-input-from-string string thunk)
;;     (if (getenv "MES_DEBUG")
;;         (core:display-error (string-append "with-input-from-string: `" string "'\n")))
;;     (core:with-input-from-string string thunk))
  )
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
