;;; -*-scheme-*-

;;; Mes --- Maxwell Equations of Software
;;; Copyright © 2016,2017,2018 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
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

;;; read-0.mes - bootstrap reader.  This file is read by a minimal
;;; core reader.  It only supports s-exps and line-comments; quotes,
;;; character literals, string literals cannot be used here.

;;; Code:

;; boot-00.scm
(define mes %version)

(define (defined? x)
  (assq x (current-module)))

(define (cond-expand-expander clauses)
  (if (defined? (car (car clauses)))
      (cdr (car clauses))
      (cond-expand-expander (cdr clauses))))

(define-macro (cond-expand . clauses)
  (cons 'begin (cond-expand-expander clauses)))
;; end boot-00.scm

;; boot-01.scm
(define <cell:character> 0)
(define <cell:pair> 7)
(define <cell:string> 10)

(define (pair? x) (eq? (core:type x) <cell:pair>))
(define (not x) (if x #f #t))

(define (display x . rest)
  (if (null? rest) (core:display x)
      (core:display-port x (car rest))))

(define (write x . rest)
  (if (null? rest) (core:write x)
      (core:write-port x (car rest))))

(define (list->string lst)
  (core:make-cell <cell:string> lst 0))

(define (integer->char x)
  (core:make-cell <cell:character> 0 x))

(define (newline . rest)
  (core:display (list->string (list (integer->char 10)))))

(define (string->list s)
  (core:car s))

(define (cadr x) (car (cdr x)))

(define (map1 f lst)
  (if (null? lst) (list)
      (cons (f (car lst)) (map1 f (cdr lst)))))

(define (map f lst)
  (if (null? lst) (list)
      (cons (f (car lst)) (map f (cdr lst)))))

(define (cons* . rest)
  (if (null? (cdr rest)) (car rest)
      (cons (car rest) (core:apply cons* (cdr rest) (current-module)))))

(define (apply f h . t)
  (if (null? t) (core:apply f h (current-module))
      (apply f (apply cons* (cons h t)))))

(define (append . rest)
  (if (null? rest) '()
      (if (null? (cdr rest)) (car rest)
          (append2 (car rest) (apply append (cdr rest))))))
;; end boot-01.scm

;; boot-02.scm
(define-macro (and . x)
  (if (null? x) #t
      (if (null? (cdr x)) (car x)
          (list (quote if) (car x) (cons (quote and) (cdr x))
                #f))))

(define-macro (or . x)
  (if (null? x) #f
      (if (null? (cdr x)) (car x)
          (list (list (quote lambda) (list (quote r))
                      (list (quote if) (quote r) (quote r)
                            (cons (quote or) (cdr x))))
                (car x)))))

(define-macro (module-define! module name value)
  ;;(list 'define name value)
  #t)

(define-macro (mes-use-module module)
  #t)
;; end boot-02.scm

;; boot-0.scm
(define (primitive-eval e) (core:eval e (current-module)))
(define eval core:eval)

(define (current-output-port) 1)
(define (current-error-port) 2)
(define (port-filename port) "<stdin>")
(define (port-line port) 0)
(define (port-column port) 0)
(define (ftell port) 0)
(define (false-if-exception x) x)

(define (cons* . rest)
  (if (null? (cdr rest)) (car rest)
      (cons (car rest) (core:apply cons* (cdr rest) (current-module)))))

(define (apply f h . t)
  (if (null? t) (core:apply f h (current-module))
      (apply f (apply cons* (cons h t)))))

(define-macro (cond . clauses)
  (list 'if (pair? clauses)
        (list (cons
               'lambda
               (cons
                '(test)
                (list (list 'if 'test
                            (if (pair? (cdr (car clauses)))
                                (if (eq? (car (cdr (car clauses))) '=>)
                                    (append2 (cdr (cdr (car clauses))) '(test))
                                    (list (cons 'lambda (cons '() (cons 'test (cdr (car clauses)))))))
                                (list (cons 'lambda (cons '() (cons 'test (cdr (car clauses)))))))
                            (if (pair? (cdr clauses))
                                (cons 'cond (cdr clauses)))))))
              (car (car clauses)))))

(define else #t)

(define-macro (load file)
  (list 'begin
        (list 'if (list getenv "MES_DEBUG")
              (list 'begin
                    (list core:display-error ";;; read ")
                    (list core:display-error file)
                    (list core:display-error "\n")))
     (list 'primitive-load file)))

(define-macro (include file) (list 'load file))

(define (append . rest)
  (if (null? rest) '()
      (if (null? (cdr rest)) (car rest)
          (append2 (car rest) (apply append (cdr rest))))))

(define (string->list s)
  (core:car s))

(define %prefix (getenv "MES_PREFIX"))
(define %moduledir
  (if (not %prefix) "module/"
      (list->string
       (append (string->list %prefix)
               (string->list "/module") ; `module/' gets replaced upon install
               (string->list "/")))))

(include (list->string
          (append2 (string->list %moduledir) (string->list "/mes/type-0.mes"))))

(define (symbol->string s)
  (apply string (symbol->list s)))

(define (string-append . rest)
  (apply string (apply append (map1 string->list rest))))

(define %version (if (eq? (car (string->list "@VERSION@")) #\@) "git"
                     "@VERSION@"))
(define (effective-version) %version)

(if (getenv "MES_DEBUG")
    (begin
      (core:display-error ";;; %moduledir=")
      (core:display-error %moduledir)
      (core:display-error "\n")))

(define-macro (include-from-path file)
  (list 'load (list string-append %moduledir file)))

(define (string-join lst infix)
  (if (null? (cdr lst)) (car lst)
      (string-append (car lst) infix (string-join (cdr lst) infix))))

(include-from-path "mes/module.mes")

(mes-use-module (mes base))
;; ;; (mes-use-module (srfi srfi-0))
(mes-use-module (mes quasiquote))
(mes-use-module (mes let))

(mes-use-module (mes scm))

(mes-use-module (srfi srfi-1)) ;; FIXME: module read order
(mes-use-module (srfi srfi-13))

(mes-use-module (mes fluids)) ;; FIXME: module read order
(mes-use-module (mes catch))

(mes-use-module (mes posix))
;; ;; end boot-0.scm

(mes-use-module (mes getopt-long))

(primitive-load 0)
(let ((tty? (isatty? 0)))
  (define (parse-opts args)
    (let* ((option-spec
            '((dump)
              (help (single-char #\h))
              (load)
              (source (single-char #\s) (value #t))
              (version (single-char #\V)))))
      (getopt-long args option-spec #:stop-at-first-non-option #t)))
  (define (source-arg? o)
    (equal? "-s" o))
  (let* ((s-index (list-index source-arg? %argv))
         (args (if s-index (list-head %argv (+ s-index 2)) %argv))
         (options (parse-opts args))
         (source (option-ref options 'source #f))
         (files (if s-index (list-tail %argv (+ s-index 1))
                    (option-ref options '() '())))
         (help? (option-ref options 'help #f))
         (usage? (and (not help?) (null? files) (not tty?)))
         (version? (option-ref options 'version #f)))
    (or
     (and version?
          (display (string-append "mes (Mes) " %version "\n"))
          (exit 0))
     (and (or help? usage?)
          (display "Usage: mes [OPTION]... [FILE]...
Evaluate code with Mes, interactively or from a script.

  [-s] FILE      load source code from FILE, and exit
  --             stop scanning arguments; run interactively

The above switches stop argument processing, and pass all
remaining arguments as the value of (command-line).

  --dump             dump binary program to stdout
  -h, --help         display this help and exit
  --load             load binary program [module/mes/boot-0.32-mo]
  -v, --version      display version information and exit
" (or (and usage? (current-error-port)) (current-output-port)))
          (exit (or (and usage? 2) 0)))
     options)
    (cond ((pair? files)
           (let* ((file (car files))
                  (port (if (equal? file "-") 0
                            (open-input-file file))))
             (set! %argv files)
             (set-current-input-port port)))
          ((and (null? files) tty?)
           
           (mes-use-module (mes repl))
           (set-current-input-port 0)
           (repl))
          (else #t))))
(primitive-load 0)
