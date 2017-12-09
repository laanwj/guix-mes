;;; -*-scheme-*-

;;; Mes --- Maxwell Equations of Software
;;; Copyright © 2017 Jan Nieuwenhuizen <janneke@gnu.org>
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
  #:export (core:display core:display-error))

(cond-expand
 (guile
  (define core:display display)
  (define (core:display-error o) (display o (current-error-port))))
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
  (use-modules (ice-9 syncase)))
 (mes))
