;;; GNU Mes --- Maxwell Equations of Software
;;; Copyright © 2016,2017,2018,2019,2020,2021 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
;;; Copyright © 2021 W. J. van der Laan <laanwj@protonmail.com>
;;;
;;; This file is part of GNU Mes.
;;;
;;; GNU Mes is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; GNU Mes is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Mes.  If not, see <http://www.gnu.org/licenses/>.

(define-module (mescc mescc)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 pretty-print)
  #:use-module (ice-9 getopt-long)
  #:use-module (mes misc)

  #:use-module (mescc info)
  #:use-module (mescc armv4 info)
  #:use-module (mescc i386 info)
  #:use-module (mescc x86_64 info)
  #:use-module (mescc riscv64 info)
  #:use-module (mescc preprocess)
  #:use-module (mescc compile)
  #:use-module (mescc M1)
  #:export (count-opt
            mescc:preprocess
            mescc:get-host
            mescc:compile
            mescc:assemble
            mescc:link
            multi-opt))

(define GUILE-with-output-to-file with-output-to-file)
(define (with-output-to-file file-name thunk)
  (if (equal? file-name "-") (thunk)
      (GUILE-with-output-to-file file-name thunk)))

(define (mescc:preprocess options)
  (let* ((pretty-print/write (string->symbol (option-ref options 'write (if guile? "pretty-print" "write"))))
         (pretty-print/write (if (eq? pretty-print/write 'pretty-print) pretty-print write))
         (files (option-ref options '() '("a.c")))
         (input-file-name (car files))
         (input-base (basename input-file-name))
         (ast-file-name (cond ((and (option-ref options 'preprocess #f)
                                    (option-ref options 'output #f)))
                              (else (replace-suffix input-base ".E"))))
         (dir (dirname input-file-name))
         (defines (reverse (filter-map (multi-opt 'define) options)))
         (includes (reverse (filter-map (multi-opt 'include) options)))
         (includes (cons (option-ref options 'includedir #f) includes))
         (includes (cons dir includes))
         (prefix (option-ref options 'prefix ""))
         (machine (option-ref options 'machine "32"))
         (arch (arch-get options))
         (defines (append (arch-get-defines options) defines))
         (verbose? (count-opt options 'verbose)))
    (with-output-to-file ast-file-name
      (lambda _ (for-each (cut c->ast prefix defines includes arch pretty-print/write verbose? <>) files)))))

(define (c->ast prefix defines includes arch write verbose? file-name)
  (with-input-from-file file-name
    (cut write (c99-input->ast #:prefix prefix #:defines defines #:includes includes #:arch arch #:verbose? verbose?))))

(define (mescc:compile options)
  (let* ((files (option-ref options '() '("a.c")))
         (input-file-name (car files))
         (input-base (basename input-file-name))
         (M1-file-name (cond ((and (option-ref options 'compile #f)
                                   (option-ref options 'output #f)))
                             ((string-suffix? ".S" input-file-name) input-file-name)
                             (else (replace-suffix input-base ".s"))))
         (infos (map (cut file->info options <>) files))
         (verbose? (count-opt options 'verbose))
         (numbered-arch? (option-ref options 'numbered-arch? #f))
         (align (filter-map (multi-opt 'align) options))
         (align (if (null? align) '(functions globals) (map string->symbol align)))
         (align (if (not numbered-arch?) align
                    ;; function alignment not supported by MesCC-Tools 0.5.2
                    (filter (negate (cut eq? <> 'functions)) align))))
    (when verbose?
      (stderr "dumping: ~a\n" M1-file-name))
    (with-output-to-file M1-file-name
      (cut infos->M1 M1-file-name infos #:align align #:verbose? verbose?))
    M1-file-name))

(define (file->info options file-name)
  (cond ((.c? file-name) (c->info options file-name))
        ((.E? file-name) (E->info options file-name))))

(define (c->info options file-name)
  (let* ((dir (dirname file-name))
         (defines (reverse (filter-map (multi-opt 'define) options)))
         (includes (reverse (filter-map (multi-opt 'include) options)))
         (includes (cons (option-ref options 'includedir #f) includes))
         (includes (cons dir includes))
         (prefix (option-ref options 'prefix ""))
         (defines (append (arch-get-defines options) defines))
         (arch (arch-get options))
         (verbose? (count-opt options 'verbose)))
    (with-input-from-file file-name
      (cut c99-input->info (arch-get-info options) #:prefix prefix #:defines defines #:includes includes #:arch arch #:verbose? verbose?))))

(define (E->info options file-name)
  (let ((ast (with-input-from-file file-name read))
        (verbose? (count-opt options 'verbose)))
    (c99-ast->info (arch-get-info options) ast #:verbose? verbose?)))

(define (mescc:assemble options)
  (let* ((files (option-ref options '() '("a.c")))
         (input-file-name (car files))
         (input-base (basename input-file-name))
         (hex2-file-name (cond ((and (option-ref options 'assemble #f)
                                     (option-ref options 'output #f)))
                               (else (replace-suffix input-base ".o"))))
         (s-files (filter .s? files))
         (hex2-files  M1->hex2 ) ;; FIXME
         (source-files (filter (disjoin .c? .E?) files))
         (infos (map (cut file->info options <>) source-files)))
    (if (and (pair? s-files) (pair? infos))
        (error "mixing source and object not supported:" source-files s-files))
    (when (pair? s-files)
      (M1->hex2 options s-files))
    (when (pair? infos)
      (infos->hex2 options hex2-file-name infos))
    hex2-file-name))

(define (mescc:link options)
  (let* ((files (option-ref options '() '("a.c")))
         (source-files (filter (disjoin .c? .E?) files))
         (s-files (filter .s? files))
         (o-files (filter .o? files))
         (input-file-name (car files))
         (hex2-file-name (if (or (string-suffix? ".hex2" input-file-name)
                                 (string-suffix? ".o" input-file-name)) input-file-name
                                 (replace-suffix input-file-name ".o")))
         (infos (map (cut file->info options <>) source-files))
         (s-files (filter .s? files))
         (hex2-files (filter .o? files))
         (hex2-files (if (null? s-files) hex2-files
                         (append hex2-files (list (M1->hex2 options s-files)))))
         (hex2-files (if (null? infos) hex2-files
                         (append hex2-files
                                 (list (infos->hex2 options hex2-file-name infos)))))
         (default-libraries (if (or (option-ref options 'nodefaultlibs #f)
                                    (option-ref options 'nostdlib #f))
                                '()
                                '("mescc" "c")))
         (libraries (filter-map (multi-opt 'library) options))
         (libraries (delete-duplicates (append libraries default-libraries)))
         (hex2-libraries (map (cut find-library options ".a" <>) libraries))
         (hex2-files (append hex2-files hex2-libraries))
         (s-files (append s-files (map (cut find-library options ".s" <>)  libraries)))
         (debug-info? (option-ref options 'debug-info #f))
         (s-files (if (string-suffix? ".S" input-file-name) s-files
                      (cons (replace-suffix input-file-name ".s") s-files)))
         (elf-footer (and debug-info?
                          (or (M1->blood-elf options hex2-files)
                              (exit 1)))))
    (or (hex2->elf options hex2-files #:elf-footer elf-footer)
        (exit 1))))

(define (infos->hex2 options hex2-file-name infos)
  (let* ((input-file-name (car (option-ref options '() '("a.c"))))
         (M1-file-name (replace-suffix hex2-file-name ".s"))
         (options (acons 'compile #t options)) ; ugh
         (options (acons 'output hex2-file-name options))
         (verbose? (count-opt options 'verbose))
         (numbered-arch? (option-ref options 'numbered-arch? #f))
         (align (filter-map (multi-opt 'align) options))
         (align (if (null? align) '(functions globals) (map string->symbol align)))
         (align (if (not numbered-arch?) align
                    ;; function alignment not supported by MesCC-Tools 0.5.2
                    (filter (negate (cut eq? <> 'functions)) align))))
    (when verbose?
      (stderr "dumping: ~a\n" M1-file-name))
    (with-output-to-file M1-file-name
      (cut infos->M1 M1-file-name infos #:align align))
    (or (M1->hex2 options (list M1-file-name))
        (exit 1))))

(define (M1->hex2 options M1-files)
  (let* ((input-file-name (car (option-ref options '() '("a.c"))))
         (input-base (basename input-file-name))
         (M1-file-name (car M1-files))
         (hex2-file-name (cond ((and (option-ref options 'assemble #f)
                                     (option-ref options 'output #f)))
                               ((option-ref options 'assemble #f)
                                (replace-suffix input-base ".o"))
                               (else (replace-suffix M1-file-name ".o"))))
         (verbose? (count-opt options 'verbose))
         (M1 (or (getenv "M1") "M1"))
         (command `(,M1
                    "--LittleEndian"
                    ,@(arch-get-architecture options)
                    "-f" ,(arch-find options (arch-get-m1-macros options))
                    ,@(append-map (cut list "-f" <>) M1-files)
                    "-o" ,hex2-file-name)))
    (when (and verbose? (> verbose? 1))
      (stderr "~a\n" (string-join command)))
    (and (zero? (apply assert-system* command))
         hex2-file-name)))

(define* (hex2->elf options hex2-files #:key elf-footer)
  (let* ((input-file-name (car (option-ref options '() '("a.c"))))
         (elf-file-name (cond ((option-ref options 'output #f))
                              (else "a.out")))
         (verbose? (count-opt options 'verbose))
         (hex2 (or (getenv "HEX2") "hex2"))
         (base-address (option-ref options 'base-address "0x1000000"))
         (machine (arch-get-machine options))
         (elf-footer
          (or elf-footer
              (kernel-find
               options
               (string-append "elf" machine "-footer-single-main.hex2"))))
         (start-files (if (or (option-ref options 'nostartfiles #f)
                              (option-ref options 'nostdlib #f)) '()
                              `("-f" ,(arch-find options "crt1.o"))))
         (command `(,hex2
                    "--LittleEndian"
                    ,@(arch-get-architecture options)
                    "--BaseAddress" ,base-address
                    "-f" ,(kernel-find
                           options
                           (string-append "elf" machine "-header.hex2"))
                    ,@start-files
                    ,@(append-map (cut list "-f" <>) hex2-files)
                    "-f" ,elf-footer
                    "--exec_enable"
                    "-o" ,elf-file-name)))
    (when (and verbose? (> verbose? 1))
      (stderr "~a\n" (string-join command)))
    (and (zero? (apply assert-system* command))
         elf-file-name)))

(define (M1->blood-elf options M1-files)
  (let* ((M1-file-name (car M1-files))
         (M1-blood-elf-footer (string-append M1-file-name ".blood-elf"))
         (hex2-file-name (replace-suffix M1-file-name ".o"))
         (blood-elf-footer (string-append hex2-file-name ".blood-elf"))
         (verbose? (count-opt options 'verbose))
         (blood-elf (or (getenv "BLOOD_ELF") "blood-elf"))
         (command `(,blood-elf
                      ,@(if (equal? (arch-get-machine options) "64") '("--64") '())
                      "-f" ,(arch-find options (arch-get-m1-macros options))
                      ,@(append-map (cut list "-f" <>) M1-files)
                      "-o" ,M1-blood-elf-footer)))
    (when (and verbose? (> verbose? 1))
      (format (current-error-port) "~a\n" (string-join command)))
    (and (zero? (apply assert-system* command))
         (let* ((options (acons 'compile #t options)) ; ugh
                (options (acons 'output blood-elf-footer options)))
           (M1->hex2 options (list M1-blood-elf-footer))))))

(define (replace-suffix file-name suffix)
  (let* ((parts (string-split file-name #\.))
         (base (if (pair? (cdr parts)) (drop-right parts 1) (list file-name)))
         (old-suffix (if (pair? (cdr parts)) (last parts) ""))
         (program-prefix (cond ((string-prefix? "arm-mes-" old-suffix) ".arm-mes-")
                               ((string-prefix? "x86-mes-" old-suffix) ".x86-mes-")
                               ((string-prefix? "x86_64-mes-" old-suffix) ".x86_64-mes-")
                               ((string-prefix? "riscv64-mes-" old-suffix) ".riscv64-mes-")
                               (else "."))))
    (if (string-null? suffix)
        (if (string-null? program-prefix) (string-join base ".")
            (string-append (string-drop program-prefix 1) (string-join base ".")))
        (string-append (string-join base ".") program-prefix (string-drop suffix 1)))))

(define (find-library options ext o)
  (arch-find options (string-append "lib" o ext)))

(define* (arch-find options file-name #:key kernel)
  (let* ((srcdest (or (getenv "srcdest") ""))
         (srcdir-lib (string-append srcdest "lib"))
         (srcdir-mescc-lib (string-append srcdest "mescc-lib"))
         (libdir (option-ref options 'libdir "lib"))
         (libdir-mescc (string-append
                        (dirname (option-ref options 'libdir "lib"))
                        "/mescc-lib"))
         (arch (string-append (arch-get options) "-mes"))
         (path (append (if (getenv "MES_UNINSTALLED")
                           (list srcdir-mescc-lib
                                 srcdir-lib
                                 libdir-mescc)
                           '())
                       (list libdir)
                       (or (and=> (getenv "LIBRARY_PATH")
                                  (cut string-split <> #\:)) '())
                       (filter-map (multi-opt 'library-dir) options)))
         (arch-file-name (string-append arch "/" file-name))
         (arch-file-name (if kernel (string-append kernel "/" arch-file-name)
                             arch-file-name))
         (verbose? (count-opt options 'verbose)))
    (let ((file (search-path path arch-file-name)))
      (when (and verbose? (> verbose? 1))
        (stderr "arch-find=~s\n" arch-file-name)
        (stderr "     path=~s\n" path)
        (stderr "  => ~s\n" file))
      (or file
          (error (format #f "mescc: file not found: ~s" arch-file-name))))))

(define (kernel-find options file-name)
  (let ((kernel (option-ref options 'kernel "linux")))
    (or (arch-find options file-name #:kernel kernel)
        (arch-find options file-name))))

(define (assert-system* . args)
  (let ((status (apply system* args)))
    (when (not (zero? status))
      (stderr "mescc: failed: ~a\n" (string-join args))
      (exit (status:exit-val status)))
    status))

(define (arch-get options)
  (let* ((machine (option-ref options 'machine #f))
         (arch (option-ref options 'arch #f)))
    (if machine (cond ((member arch '("x86" "x86_64")) (cond ((equal? machine "32") "x86")
                                                             ((equal? machine "64") "x86_64")))
                      ((equal? arch "arm") (cond ((equal? machine "32") "arm")
                                                 ((equal? machine "arm") "arm")))
                      ((member arch '("riscv32" "riscv64")) (cond ((equal? machine "32") "riscv32")
                                                                  ((equal? machine "64") "riscv64"))))
        arch)))

(define (mescc:get-host options)
  (let ((cpu (arch-get options))
        (kernel (option-ref options 'kernel "linux")))
    (string-join (list cpu kernel "mes") "-")))

(define (arch-get-info options)
  (let ((arch (arch-get options)))
    (cond ((equal? arch "arm") (armv4-info))
          ((equal? arch "x86") (x86-info))
          ((equal? arch "x86_64") (x86_64-info))
          ((equal? arch "riscv64") (riscv64-info)))))

(define (arch-get-defines options)
  (let* ((arch (arch-get options))
         (info (arch-get-info options))
         (types (.types info)))
    (define (sizeof type)
      (type:size (assoc-ref types type)))
    (let ((int (sizeof "int"))
          (long (sizeof "long"))
          (long-long (sizeof "long long")))
      (append (cond ((equal? arch "arm")
                   '("__arm__=1"))
                  ((equal? arch "x86")
                   '("__i386__=1"))
                  ((equal? arch "x86_64")
                   '("__x86_64__=1"))
                  ((equal? arch "riscv32")
                   '("__riscv=1" "__riscv_xlen=32"))
                  ((equal? arch "riscv64")
                   '("__riscv=1" "__riscv_xlen=64")))
            `(,(string-append "__SIZEOF_INT__=" (number->string int))
              ,(string-append "__SIZEOF_LONG__=" (number->string long))
              ,@(if (< long-long 8) '() ;C99: long long must be >= 8
                    '("__SIZEOF_LONG_LONG__=8")))))))

(define (arch-get-machine options)
  (let* ((machine (option-ref options 'machine #f))
         (arch (option-ref options 'arch #f))
         (machine (or machine arch "32")))
    (cond ((member machine '("64" "riscv64" "x86_64")) "64")
          ((member machine '("arm")) "32")
          (else "32"))))

(define (arch-get-m1-macros options)
  (let ((arch (arch-get options)))
    (cond ((equal? arch "arm") "arm.M1")
          ((equal? arch "x86") "x86.M1")
          ((equal? arch "x86_64") "x86_64.M1")
          ((equal? arch "riscv64") "riscv64.M1"))))

(define (arch-get-architecture options)
  (let* ((arch (arch-get options))
         (numbered-arch? (option-ref options 'numbered-arch? #f))
        (flag (if numbered-arch? "--Architecture" "--architecture")))
    (list flag
          (cond ((equal? arch "arm") (if numbered-arch? "40" "armv7l"))
                ((equal? arch "x86") (if numbered-arch? "1" "x86"))
                ((equal? arch "x86_64") (if numbered-arch? "2" "amd64"))
                ((equal? arch "riscv64") (if numbered-arch? "243" "riscv64"))))))

(define (multi-opt option-name) (lambda (o) (and (eq? (car o) option-name) (cdr o))))
(define (count-opt options option-name)
  (let ((lst (filter-map (multi-opt option-name) options)))
    (and (pair? lst) (length lst))))

(define (.c? o) (or (string-suffix? ".c" o)
                    (string-suffix? ".M2" o)))
(define (.E? o) (or (string-suffix? ".E" o)
                    (string-suffix? ".mes-E" o)
                    (string-suffix? ".arm-mes-E" o)
                    (string-suffix? ".x86-mes-E" o)
                    (string-suffix? ".x86_64-mes-E" o)
                    (string-suffix? ".riscv32-mes-E" o)
                    (string-suffix? ".riscv64-mes-E" o)))
(define (.s? o) (or (string-suffix? ".s" o)
                    (string-suffix? ".S" o)
                    (string-suffix? ".mes-S" o)
                    (string-suffix? ".arm-mes-S" o)
                    (string-suffix? ".x86-mes-S" o)
                    (string-suffix? ".x86_64-mes-S" o)
                    (string-suffix? ".riscv32-mes-S" o)
                    (string-suffix? ".riscv64-mes-S" o)
                    (string-suffix? ".M1" o)))
(define (.o? o) (or (string-suffix? ".o" o)
                    (string-suffix? ".mes-o" o)
                    (string-suffix? ".arm-mes-o" o)
                    (string-suffix? ".x86-mes-o" o)
                    (string-suffix? ".x86_64-mes-o" o)
                    (string-suffix? ".riscv32-mes-o" o)
                    (string-suffix? ".riscv64-mes-o" o)
                    (string-suffix? ".hex2" o)))
