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

(define-module (mescc mescc)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 pretty-print)
  #:use-module (ice-9 getopt-long)
  #:use-module (mes guile)
  #:use-module (mes misc)

  #:use-module (mescc preprocess)
  #:use-module (mescc compile)
  #:use-module (mescc M1)
  #:export (mescc:preprocess
            mescc:compile
            mescc:assemble
            mescc:link))

(define (mescc:preprocess options)
  (let* ((defines (reverse (filter-map (multi-opt 'define) options)))
         (includes (reverse (filter-map (multi-opt 'include) options)))
         (pretty-print/write (string->symbol (option-ref options 'write (if guile? "pretty-print" "write"))))
         (pretty-print/write (if (eq? pretty-print/write 'pretty-print) pretty-print write))
         (files (option-ref options '() '("a.c")))
         (input-file-name (car files))
         (ast-file-name (cond ((and (option-ref options 'preprocess #f)
                                    (option-ref options 'output #f)))
                              (else (replace-suffix input-file-name ".E"))))
         (prefix (option-ref options 'prefix "")))
    (with-output-to-file ast-file-name
      (lambda _ (for-each (cut c->ast prefix defines includes write <>) files)))))

(define (c->ast prefix defines includes write file-name)
  (with-input-from-file file-name
    (cut write (c99-input->ast #:prefix prefix #:defines defines #:includes includes))))

(define (mescc:compile options)
  (let* ((files (option-ref options '() '("a.c")))
         (input-file-name (car files))
         (M1-file-name (cond ((and (option-ref options 'compile #f)
                                   (option-ref options 'output #f)))
                             (else (replace-suffix input-file-name ".S"))))
         (infos (map (cut file->info options <>) files))
         (verbose? (option-ref options 'verbose #f)))
    (when verbose?
      (stderr "dumping: ~a\n" M1-file-name))
    (with-output-to-file M1-file-name
      (cut infos->M1 M1-file-name infos))
    M1-file-name))

(define (file->info options file-name)
  (cond ((.c? file-name) (c->info options file-name))
        ((.E? file-name) (E->info options file-name))))

(define (c->info options file-name)
  (let ((defines (reverse (filter-map (multi-opt 'define) options)))
        (includes (reverse (filter-map (multi-opt 'include) options)))
        (prefix (option-ref options 'prefix "")))
    (with-input-from-file file-name
      (cut c99-input->info #:prefix prefix #:defines defines #:includes includes))))

(define (E->info options file-name)
  (let ((ast (with-input-from-file file-name read)))
    (c99-ast->info ast)))

(define (mescc:assemble options)
  (let* ((files (option-ref options '() '("a.c")))
         (input-file-name (car files))
         (hex2-file-name (cond ((and (option-ref options 'assemble #f)
                                     (option-ref options 'output #f)))
                               (else (replace-suffix input-file-name ".o"))))
         (S-files (filter .S? files))
         (hex2-files  M1->hex2 ) ;; FIXME
         (source-files (filter (disjoin .c? .E?) files))
         (infos (map (cut file->info options <>) source-files)))
    (if (and (pair? S-files) (pair? infos))
        (error "mixing source and object not supported:" source-files S-files))
    (when (pair? S-files)
      (M1->hex2 options S-files))
    (when (pair? infos)
      (infos->hex2 options hex2-file-name infos))
    hex2-file-name))

(define (mescc:link options)
  (define (library->hex2 o)
    (prefix-file options (string-append "lib/lib" o "-mes.o")))
  (let* ((files (option-ref options '() '("a.c")))
         (source-files (filter (disjoin .c? .E?) files))
         (S-files (filter .S? files))
         (o-files (filter .o? files))
         (input-file-name (car files))
         (hex2-file-name (if (or (string-suffix? ".hex2" input-file-name)
                                 (string-suffix? ".o" input-file-name)) input-file-name
                                 (replace-suffix input-file-name ".o")))
         (infos (map (cut file->info options <>) source-files))
         (S-files (filter .S? files))
         (hex2-files (filter .o? files))
         (hex2-files (if (null? S-files) hex2-files
                         (append hex2-files (list (M1->hex2 options S-files)))))
         (hex2-files (if (null? infos) hex2-files
                         (append hex2-files
                                 (list (infos->hex2 options hex2-file-name infos)))))
         (libraries (filter-map (multi-opt 'library) options))
         (libraries (if (pair? libraries) libraries '("c")))
         (hex2-libraries (map library->hex2 libraries))
         (hex2-files (append hex2-files hex2-libraries))
         (S-files (append S-files (map (cut replace-suffix <> ".S") hex2-libraries)))
         (debug-info? (option-ref options 'debug-info #f))
         (S-files (cons (replace-suffix input-file-name ".S") S-files))
         (elf-footer (and debug-info?
                          (or (M1->blood-elf options S-files)
                              (exit 1)))))
    (or (hex2->elf options hex2-files #:elf-footer elf-footer)
        (exit 1))))

(define (infos->hex2 options hex2-file-name infos)
  (let* ((input-file-name (car (option-ref options '() '("a.c"))))
         (M1-file-name (replace-suffix hex2-file-name ".S"))
         (options (acons 'compile #t options)) ; ugh
         (options (acons 'output hex2-file-name options))
         (verbose? (option-ref options 'verbose #f)))
    (when verbose?
      (stderr "dumping: ~a\n" M1-file-name))
    (with-output-to-file M1-file-name
      (cut infos->M1 M1-file-name infos))
    (or (M1->hex2 options (list M1-file-name))
        (exit 1))))

(define (M1->hex2 options M1-files)
  (let* ((input-file-name (car (option-ref options '() '("a.c"))))
         (M1-file-name (car M1-files))
         (hex2-file-name (cond ((and (option-ref options 'assemble #f)
                                     (option-ref options 'output #f)))
                               ((option-ref options 'assemble #f)
                                (replace-suffix input-file-name ".o"))
                               (else (replace-suffix M1-file-name ".o"))))
         (verbose? (option-ref options 'verbose #f))
         (M1 (or (getenv "M1") "M1"))
         (command `(,M1
                    "--LittleEndian"
                    "--Architecture=1"
                    "-f" ,(prefix-file options "stage0/x86.M1")
                    ,@(append-map (cut list "-f" <>) M1-files)
                    "-o" ,hex2-file-name)))
    (when verbose?
      (stderr "~a\n" (string-join command)))
    (and (zero? (apply system* command))
         hex2-file-name)))

(define* (hex2->elf options hex2-files #:key elf-footer)
  (let* ((input-file-name (car (option-ref options '() '("a.c"))))
         (elf-file-name (cond ((option-ref options 'output #f))
                              (else (replace-suffix input-file-name ""))))
         (verbose? (option-ref options 'verbose #f))
         (elf-footer (or elf-footer (prefix-file options "stage0/elf32-footer-single-main.hex2")))
         (hex2 (or (getenv "HEX2") "hex2"))
         (command `(,hex2
                    "--LittleEndian"
                    "--Architecture=1"
                    "--BaseAddress=0x1000000"
                    "-f" ,(prefix-file options "stage0/elf32-header.hex2")
                    "-f" ,(prefix-file options "lib/crt1.o")
                    ,@(append-map (cut list "-f" <>) hex2-files)
                    "-f" ,elf-footer
                    "--exec_enable"
                    "-o" ,elf-file-name)))
    (when verbose?
      (stderr "command=~s\n" command)
      (format (current-error-port) "~a\n" (string-join command)))
    (and (zero? (apply system* command))
         elf-file-name)))

(define (M1->blood-elf options M1-files)
  (let* ((M1-file-name (car M1-files))
         (M1-blood-elf-footer (string-append M1-file-name ".blood-elf"))
         (hex2-file-name (replace-suffix M1-file-name ".o"))
         (blood-elf-footer (string-append hex2-file-name ".blood-elf"))
         (verbose? (option-ref options 'verbose #f))
         (blood-elf (or (getenv "BLOOD_ELF") "blood-elf"))
         (command `(,blood-elf
                      "-f" ,(prefix-file options "stage0/x86.M1")
                      ,@(append-map (cut list "-f" <>) M1-files)
                      "-o" ,M1-blood-elf-footer)))
    (when verbose?
        (format (current-error-port) "~a\n" (string-join command)))
    (and (zero? (apply system* command))
         (let* ((options (acons 'compile #t options)) ; ugh
                (options (acons 'output blood-elf-footer options)))
           (M1->hex2 options (list M1-blood-elf-footer))))))

(define (replace-suffix file-name suffix)
  (let* ((parts (string-split file-name #\.))
         (base (if (pair? (cdr parts)) (drop-right parts 1))))
    (string-append (string-join base ".") suffix)))

(define (prefix-file options file-name)
  (let ((prefix (option-ref options 'prefix "")))
    (define (prefix-file o)
      (if (string-null? prefix) o (string-append prefix "/" o)))
    (prefix-file file-name)))

(define (multi-opt option-name) (lambda (o) (and (eq? (car o) option-name) (cdr o))))

(define (.c? o) (or (string-suffix? ".c" o)
                    (string-suffix? ".M2" o)))
(define (.E? o) (string-suffix? ".E" o))
(define (.S? o) (or (string-suffix? ".S" o)
                    (string-suffix? ".mes-S" o)
                    (string-suffix? "S" o)
                    (string-suffix? ".M1" o)))
(define (.o? o) (or (string-suffix? ".o" o)
                    (string-suffix? ".mes-o" o)
                    (string-suffix? "o" o)
                    (string-suffix? ".hex2" o)))
