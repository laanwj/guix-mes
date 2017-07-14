;;; -*-scheme-*-

;;; Mes --- Maxwell Equations of Software
;;; Copyright © 2016,2017 Jan Nieuwenhuizen <janneke@gnu.org>
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

;;; info.scm defines [Guile] record data types for compiler.mes

;;; Code:

(define-module (language c99 info)
  #:use-module (ice-9 optargs)
  #:use-module (system base pmatch)
  #:export (<info>
            ;; <types>
            ;; <constants>
            ;; <functions>
            ;; <globals>
            ;; <locals>
            ;; <function>
            ;; <text>
            ;; <break>
            ;; <continue>

            make
            info?

            .info
            .types
            .constants
            .functions
            .globals
            .locals
            .function
            .text
            .break
            .continue))

(cond-expand
 (guile-2)
 (guile
  (use-modules (ice-9 syncase)))
 (mes))

;;(include-from-path "language/c99/info.mes")

(define <info> '<info>)
(define <types> '<types>)
(define <constants> '<constants>)
(define <functions> '<functions>)
(define <globals> '<globals>)
(define <locals> '<locals>)
(define <function> '<function>)
(define <text> '<text>)
(define <break> '<break>)
(define <continue> '<continue>)

(define* (make o #:key (types '()) (constants '()) (functions '()) (globals '()) (locals '()) (function #f) (text '()) (break '()) (continue '()))
  (pmatch o
    (<info> (list <info>
                  (cons <types> types)
                  (cons <constants> constants)
                  (cons <functions> functions)
                  (cons <globals> globals)
                  (cons <locals> locals)
                  (cons <function> function)
                  (cons <text> text)
                  (cons <break> break)
                  (cons <continue> continue)))))

(define (.types o)
  (pmatch o
    ((<info> . ,alist) (assq-ref alist <types>))))

(define (.constants o)
  (pmatch o
    ((<info> . ,alist) (assq-ref alist <constants>))))

(define (.functions o)
  (pmatch o
    ((<info> . ,alist) (assq-ref alist <functions>))))

(define (.globals o)
  (pmatch o
    ((<info> . ,alist) (assq-ref alist <globals>))))

(define (.locals o)
  (pmatch o
    ((<info> . ,alist) (assq-ref alist <locals>))))

(define (.function o)
  (pmatch o
    ((<info> . ,alist) (assq-ref alist <function>))))

(define (.text o)
  (pmatch o
    ((<info> . ,alist) (assq-ref alist <text>))))

(define (.break o)
  (pmatch o
    ((<info> . ,alist) (assq-ref alist <break>))))

(define (.continue o)
  (pmatch o
    ((<info> . ,alist) (assq-ref alist <continue>))))

(define (info? o)
  (and (pair? o) (eq? (car o) <info>)))
