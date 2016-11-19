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

(define %builtin-prefix% "scm_")
(define (function-builtin-name f)
  (string-append %builtin-prefix% (.name f)))

(define %cell-prefix% "cell_")
(define (function-cell-name f)
  (string-append %cell-prefix% (.name f)))

(define %start 1)
(define (symbol->header s i)
  (format #f "SCM cell_~a;\n" s))

(define (symbol->source s i)
  (string-append
   (format #f "cell_~a = g_free.value++;\n" s)
   (format #f "g_cells[cell_~a] = scm_~a;\n\n" s s)))

(define (function->header f i)
  (let* ((arity (or (assoc-ref (.annotation f) 'arity)
                    (if (string-null? (.formals f)) 0
                        (length (string-split (.formals f) #\,)))))
         (n (if (eq? arity 'n) -1 arity)))
    (string-append
     (format #f "SCM ~a (~a);\n" (.name f) (.formals f))
     (format #f "function fun_~a = {.function~a=&~a, .arity=~a};\n" (.name f) arity (.name f) n)
     (format #f "scm ~a = {FUNCTION, .name=~S, .function=0};\n" (function-builtin-name f) (function-scm-name f))
     (format #f "SCM cell_~a;\n\n" (.name f)))))

(define (function->source f i)
  (string-append
   (format #f "~a.function = g_function;\n" (function-builtin-name f))
   (format #f "functions[g_function++] = fun_~a;\n" (.name f))
   (format #f "cell_~a = g_free.value++;\n" (.name f))
   (format #f "g_cells[cell_~a] = ~a;\n\n" (.name f) (function-builtin-name f))))

(define (function->environment f i)
  (string-append
   (format #f "a = add_environment (a, ~S, ~a);\n" (function-scm-name f) (function-cell-name f))))

(define (snarf-symbols string)
  (let* ((matches (append (list-matches "\nscm scm_([a-z_0-9]+) = [{](SPECIAL)," string)
                          (list-matches "\nscm scm_([a-z_0-9]+) = [{](SYMBOL)," string))))
    (map (cut match:substring <> 1) matches)))

(define (snarf-functions string)
  (let* ((matches (list-matches
                   "\nSCM[ \n]?([a-z0-9_]+) [(]((SCM ?[^,)]+|, )*)[)][^\n(]*([^\n]*)"
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
         (functions (filter (negate internal?) functions))
         (symbols (snarf-symbols string))
         (base-name (basename file-name ".c"))
         (header (make <file>
                   #:name (string-append base-name ".h")
                   #:content (string-join (map function->header functions (iota (length functions) (+ %start (length symbols)))) "")))
         (source (make <file>
                        #:name (string-append base-name ".i")
                        #:content (string-join (map function->source (filter (negate no-environment?) functions) (iota (length functions) (+ (length symbols) %start))) ""))) 
         (environment (make <file>
                        #:name (string-append base-name ".environment.i")
                        #:content (string-join (map function->environment (filter (negate no-environment?) functions) (iota (length functions) (+ (length symbols) %start))) "")))
         (symbols.h (make <file>
                      #:name (string-append base-name ".symbols.h")
                      #:content (string-join (map symbol->header symbols (iota (length symbols) %start)) "")))
         (symbols.i (make <file>
                      #:name (string-append base-name ".symbols.i")
                      #:content (string-join (map symbol->source symbols (iota (length symbols))) ""))))
    (list header source environment symbols.h symbols.i)))

(define (file-write file)
  (with-output-to-file (.name file) (lambda () (display (.content file)))))

(define (main args)
  (let* ((files (cdr args)))
    (map file-write (filter content? (append-map generate-includes files)))))

;;(define string (with-input-from-file "../mes.c" read-string))
