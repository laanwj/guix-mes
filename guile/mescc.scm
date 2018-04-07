#! /bin/sh
# -*-scheme-*-
GODIR=${GODIR-@GODIR@}
GUILEDIR=${GUILEDIR-@GUILEDIR@}
[ "$GODIR" = @"GODIR"@ ] && GODIR=$(dirname $0)
[ "$GUILEDIR" = @"GUILEDIR"@ ] && GUILEDIR=$(dirname $0)
export GUILE_AUTO_COMPILE=${GUILE_AUTO_COMPILE-0}
GUILE_LOAD_COMPILED_PATH=$GODIR:$GUILE_LOAD_COMPILED_PATH
exec ${GUILE-guile} -L $GUILEDIR -e '(mescc)' -s "$0" "$@"
!#

;;; Mes --- The Maxwell Equations of Software
;;; Copyright © 2016,2017,2018 Jan Nieuwenhuizen <janneke@gnu.org>
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

#!
Run with Guile-1.8:
GUILE='~/src/guile-1.8/build/pre-inst-guile --debug -q' guile/mescc.scm
!#

(define-module (mescc)
  #:use-module (language c99 info)
  #:use-module (language c99 compiler)
  #:use-module (mes elf)
  #:use-module (mes M1)
  #:use-module (ice-9 getopt-long)
  #:use-module (ice-9 pretty-print)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:export (main))

(cond-expand
 (guile-2)
 (guile
  (use-modules (ice-9 syncase))))

(define %prefix (if (string-prefix? "@PREFIX" "@PREFIX@") (or (getenv "MES_PREFIX") "") "@PREFIX@"))
(module-define! (resolve-module '(language c99 compiler)) '%prefix %prefix)

(define (parse-opts args)
  (let* ((option-spec
          '((c (single-char #\c))
            (define (single-char #\D) (value #t))
            (E (single-char #\E))
            (g (single-char #\g))
            (help (single-char #\h))
            (include (single-char #\I) (value #t))
            (o (single-char #\o) (value #t))
            (version (single-char #\V) (value #t))))
         (options (getopt-long args option-spec))
         (help? (option-ref options 'help #f))
         (files (option-ref options '() '()))
         (usage? (and (not help?) (null? files)))
         (version? (option-ref options 'version #f)))
    (or
     (and version?
          (format (current-output-port) "mescc.scm (mes) ~a\n" %version))
     (and (or help? usage?)
          (format (or (and usage? (current-error-port)) (current-output-port)) "\
Usage: mescc.scm [OPTION]... FILE...
  -c                 compile and assemble, but do not link
  -D DEFINE          define DEFINE
  -E                 preprocess only; do not compile, assemble or link
  -g                 add debug info [GDB, objdump] TODO: hex2 footer
  -h, --help         display this help and exit
  -I DIR             append DIR to include path
  -o FILE            write output to FILE
  -v, --version      display version and exit
")
          (exit (or (and usage? 2) 0)))
     options)))

(define (read-object file)
  (let ((char (with-input-from-file file read-char)))
    (if (eq? char #\#) (error "hex2 format not supported:" file)))
  (with-input-from-file file read))

(define (main:ast->info file)
  (let ((ast (with-input-from-file file read)))
    (c99-ast->info ast)))

(define (source->ast defines includes)
  (lambda (file)
    (with-input-from-file file
      (lambda ()
        (pretty-print (c99-input->ast #:defines defines #:includes includes))))))

(define (source->info defines includes)
  (lambda (file)
    (with-input-from-file file
      (lambda ()
        ((c99-input->info #:defines defines #:includes includes))))))

(define (ast? o)
  (or (string-suffix? ".E" o)
      (string-suffix? ".guile-E" o)))

(define (object? o)
  (or (string-suffix? ".o" o)
      (string-suffix? ".guile-o" o)))

(define (main args)
  (let* ((options (parse-opts args))
         (files (option-ref options '() '()))
         (file (car files))
         (preprocess? (option-ref options 'E #f))
         (compile? (option-ref options 'c #f))
         (debug-info? (option-ref options 'g #f))
         (asts (filter ast? files))
         (objects (filter object? files))
         (sources (filter (cut string-suffix? ".c" <>) files))
         (base (substring file (1+ (or (string-rindex file #\/) -1)) (- (string-length file) 2)))
         (out (option-ref options 'o (cond (compile? (string-append base ".o"))
                                           (preprocess? (string-append base ".E"))
                                           (else "a.out"))))
         (multi-opt (lambda (option) (lambda (o) (and (eq? (car o) option) (cdr o)))))
         (defines (reverse (filter-map (multi-opt 'define) options)))
         (includes (reverse (filter-map (multi-opt 'include) options))))
    (setenv "NYACC_TRACE" "yes")
    (when (getenv "MES_DEBUG") (format (current-error-port) "options=~s\n" options)
          (format (current-error-port) "output: ~a\n" out))
    (if (and (pair? sources) (pair? objects)) (error "cannot mix source and object files:" files))
    (with-output-to-file out
      (lambda ()
        (if (and (not compile?)
                 (not preprocess?)) (set-port-encoding! (current-output-port) "ISO-8859-1"))
        (cond ((pair? objects) (let ((objects (map read-object objects)))
                                 (if compile? (objects->M1 objects)
                                     (objects->elf objects))))
              ((pair? asts) (let* ((infos (map main:ast->info asts))
                                   (objects (map info->object infos)))
                              (if compile? (objects->M1 objects)
                                  (objects->elf objects))))
              ((pair? sources) (if preprocess? (map (source->ast defines includes) sources)
                                   (let* ((infos (map (source->info defines includes) sources))
                                          (objects (map info->object infos)))
                                     (if compile? (objects->M1 objects)
                                         (objects->elf objects))))))))
    (if (and (not compile?)
             (not preprocess?))
        (chmod out #o755))))
