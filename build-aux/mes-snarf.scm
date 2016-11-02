#! /bin/sh
# -*- scheme -*-
exec ${GUILE-guile} --no-auto-compile -L $HOME/src/mes/build-aux -L build-aux -e '(@@ (mes-snarf) main)' -s "$0" ${1+"$@"}
!#

;;; Mes --- Maxwell Equations of Software
;;; Copyright © 2016 Jan Nieuwenhuizen <janneke@gnu.org>
;;;
;;; mes-snarf.scm: This file is part of Mes.
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

(define-module (mes-snarf)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 curried-definitions)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 regex)
  #:use-module (oop goops))

(define ((regexp-replace regexp replace) string)
  (or (and=> (string-match regexp string)
             (cut regexp-substitute #f <> 'pre replace 'post))
      string))

;; (define-record-type function (make-function name formals annotation)
;;   function?
;;   (name .name)
;;   (formals .formals)
;;   (annotation .annotation))

(define-class <file> ()
  (name #:accessor .name #:init-keyword #:name)
  (content #:accessor .content #:init-keyword #:content))

(define-class <function> ()
  (name #:accessor .name #:init-keyword #:name)
  (formals #:accessor .formals #:init-keyword #:formals)
  (annotation #:accessor .annotation #:init-keyword #:annotation))

(define (function-scm-name f)
  (or (assoc-ref (.annotation f) 'name)
      ((compose
        (regexp-replace "_" "-")
        (regexp-replace "_" "-")
        (regexp-replace "_" "-")
        (regexp-replace "_" "-")
        (regexp-replace "^builtin_" "")
        (regexp-replace "_to_" "->")
        (regexp-replace "_x$" "!")
        (regexp-replace "_p$" "?"))
       (.name f))))

(define (function-builtin-name f)
  (string-append %builtin-prefix% (.name f)))

(define (function->source f)
  (format #f "a = add_environment (a, ~S, &~a);\n" (function-scm-name f) (function-builtin-name f)))

(define (symbol->source s)
  (format #f "symbols = cons (&~a, symbols);\n" s))

(define %builtin-prefix% "scm_")
(define (function->header f)
  (let* ((n (or (assoc-ref (.annotation f) 'args)
                (if (string-null? (.formals f)) 0
                    (length (string-split (.formals f) #\,))))))
    (string-append (format #f "scm *~a (~a);\n" (.name f) (.formals f))
                   (format #f "scm ~a = {FUNCTION~a, .name=~S, .function~a=&~a};\n" (function-builtin-name f) n (function-scm-name f) n (.name f)))))

(define (snarf-symbols string)
  (let* ((matches (append (list-matches "\nscm ([a-z_0-9]+) = [{](SCM)," string)
                          (list-matches "\nscm ([a-z_0-9]+) = [{](SYMBOL)," string))))
    (map (cut match:substring <> 1) matches)))

(define (snarf-functions string)
  (let* ((matches (list-matches
                   "\nscm [*]\n?([a-z0-9_]+) [(]((scm *[^,)]+|, )*)[)][^\n(]*([^\n]*)"
                   string)))
    (map (lambda (m)
           (make <function>
             #:name (match:substring m 1)
             #:formals (match:substring m 2)
             #:annotation (with-input-from-string (match:substring m 4) read)))
         matches)))

(define (content? f)
  ((compose not string-null? .content) f))

(define (internal? f)
  ((compose (cut assoc-ref <> 'internal) .annotation) f))

(define (no-environment? f)
  ((compose (cut assoc-ref <> 'no-environment) .annotation) f))

(define (generate-includes file-name)
  (let* ((string (with-input-from-file file-name read-string))
         (functions (snarf-functions string))
         (functions (delete-duplicates functions (lambda (a b) (equal? (.name a) (.name b)))))
         (functions (sort functions (lambda (a b) (string< (.name a) (.name b)))))
         (functions (filter (negate internal?) functions))
         (symbols (snarf-symbols string))
         (base-name (basename file-name ".c"))
         (header (make <file>
                   #:name (string-append base-name ".environment.h")
                   #:content (string-join (map function->header functions))))
         (environment (make <file>
                        #:name (string-append base-name ".environment.i")
                        #:content (string-join (map function->source (filter (negate no-environment?) functions)))))
         (symbols (make <file>
                    #:name (string-append base-name ".symbols.i")
                    #:content (string-join (map symbol->source symbols)))))
    (list header environment symbols)))

(define (file-write file)
  (with-output-to-file (.name file) (lambda () (display (.content file)))))

(define (main args)
  (let* ((files (cdr args)))
    (map file-write (filter content? (append-map generate-includes files)))))

;;(define string (with-input-from-file "../mes.c" read-string))

