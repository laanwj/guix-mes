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

;;; make

;;; Code:

(define-module (guix make)
  #:use-module (ice-9 curried-definitions)
  #:use-module (ice-9 format)
  #:use-module (ice-9 optargs)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 pretty-print)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 receive)

  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)

  #:use-module (guix records)
  #:use-module (guix shell-utils)

  #:export (build
            check
            clean
            group
            target-prefix?
            check-target?

            cpp.mescc
            compile.mescc
            ld

            bin.mescc
            bin.gcc
            snarf

            libc-mes.E
            mini-libc-mes.E
            add-target
            get-target

            system**
            target-file-name

            target
            %targets
            %status))

(define %status 0)
(define %targets '())
(define %store-dir ".store")
(mkdir-p %store-dir)
(define %command-log (open-output-file "script"))

(define (base-name file-name suffix)
  (string-drop-right file-name (string-length suffix)))

(define (system** . command)
  (format %command-log "~a\n" (string-join command " "))
  (apply system* command))

(define (gulp-pipe* . command)
  (let* ((port (apply open-pipe* (cons OPEN_READ command)))
         (foo (set-port-encoding! port "ISO-8859-1"))
         (output (read-string port))
         (status (close-pipe port)))
    (format %command-log "~a\n" (string-join command " "))
    (values output status)))

(define (assert-gulp-pipe* . command)
  (receive (output status)
      (apply gulp-pipe* command)
    (if (zero? status) (string-trim-right output #\newline)
        (error (format #f "pipe failed: ~d ~s"
                       (or (status:exit-val status)
                           (status:term-sig status)) command)))))

(define-record-type* <method>
  method make-method
  method?
  (name       method-name)
  (build      method-build (default (lambda _ #t)))
  (inputs     method-inputs (default (list))))

(define-record-type* <target>
  target make-target
  target?
  (file-name  target-file-name (default #f))       ; string
  (file-names target-file-names (default '()))     ; (string)
  (hash       target-hash (default #f))            ; string
  (method     target-method (default method-file)) ; <method>
  (inputs     target-inputs (default (list)))      ; list
                                                   ; For check targets
  (exit       target-exit (default #f))            ; number
  (signal     target-signal (default #f)))         ; number

(define method-file (method (name "FILE")))
(define method-check
  (method (name "CHECK")
          (build (lambda (o t)
                   (let* ((inputs (target-inputs t))
                          (file-name (target-file-name (build (car inputs))))
                          (run file-name)
                          (exit (target-exit t))
                          (signal (target-signal t))
                          (log (string-append file-name "-check.log")))
                     (format (current-error-port) "  CHECK\t~a" (basename file-name))
                     (receive (output result)
                         ;; FIXME: quiet MES tests are not fun
                         (if (string-prefix? "tests/" run) (values #f (system** run))
                             (gulp-pipe* run))
                       (let ((status (if (string? result) 0
                                         (or (status:term-sig result) (status:exit-val result)))))
                         (if (not (string-null? output)) (with-output-to-file log (lambda _ (display output))))
                         (store #:add-file log)
                         (format (current-error-port) "\t[~a]\n"
                                 (if (or (and signal (= status signal))
                                         (and exit (= status exit))) "OK"
                                         (begin (set! %status 1) "FAIL"))))))))))

(define (hash-target o)
  (let ((inputs (target-inputs o)))
    (if (null? inputs) (or (target-hash o) (target-hash (store #:add o)))
        (let ((input-shas (map hash-target inputs)))
          (and (every identity input-shas)
               (let ((method (target-method o)))
                 (string-hash (format #f "~s" (cons* (target-file-name o)
                                                     (method-build method)
                                                     (map target-hash (method-inputs method))
                                                     input-shas)))))))))

(define (string-hash o)
  (number->string (hash o (expt 2 63))))

(define (file-hash o)
  (string-hash (with-input-from-file o read-string)))

(define (store-file-name o)
  (string-append %store-dir "/" (if (string? o) o
                                    (target-hash o))))

(define (assert-link existing-file new-file)
  (if (not (file-exists? new-file)) (link existing-file new-file)))

(define store
  (let ((*store* '()))
    (define (prune? o)
      (let ((t (cdr o)))
        (pair? (target-inputs t))))
    (define ((file-name? file-name) o)
      (let ((t (cdr o)))
        (equal? (target-file-name t) (target-file-name file-name))))
    (lambda* (#:key add add-file delete get key print prune)
      (cond ((and add key) (let ((value (target (inherit add) (hash key))))
                             (set! *store* (assoc-set! (filter (negate (file-name? add)) *store*) key value))
                             (let ((file-name (target-file-name value)))
                               (if (and file-name (file-exists? file-name))
                                   (assert-link file-name (store-file-name value))))
                             value))
            (add (let ((key (if (null? (target-inputs add)) (file-hash (target-file-name add))
                                (hash-target add))))
                   (if (not key) (error "store: no hash for:" add))
                   (store #:add add #:key key)))
            (add-file (and (file-exists? add-file)
                           (store #:add (target (file-name add-file)))))
            ((and get key)
             (or (assoc-ref *store* key)
                 (let ((store-file (store-file-name key))
                       (file-name (target-file-name get)))
                   (and (file-exists? store-file)
                        (if (file-exists? file-name) (delete-file file-name))
                        (link store-file file-name)
                        (store #:add get #:key key)))))
            (get (assoc-ref *store* get))
            (delete (and (assoc-ref *store* delete)
                         (set! *store* (filter (lambda (e) (not (equal? (car e) delete))) *store*))))
            (print (pretty-print (map (lambda (e) (cons (target-file-name (cdr e)) (car e))) *store*)))
            ((eq? prune 'file-system)
             (set! *store* (filter prune? *store*)))
            (else (error "store: dunno"))))))

(define (build o)
  (let ((hash (hash-target o)))
    (or (and hash (store #:get o #:key hash))
        (begin
          ;;(format (current-error-port) "must rebuild hash=~s\n" hash)
          (for-each build (target-inputs o))
          (let ((method (target-method o)))
            ((method-build method) method o))
          (store #:add o #:key hash)))))

(define* (check name #:key (exit 0) (signal #f) (dependencies '()))
  (target (file-name (string-append "check-" name))
          (method method-check)
          (inputs (cons (get-target name) dependencies))
          (exit exit)
          (signal signal)))

(define* (group name #:key (dependencies '()))
  (target (file-name name)
          (inputs (map get-target dependencies))))

(define (target->input-files o)
  (let ((inputs (target-inputs o)))
    (if (null? inputs) '()
        (append (cons (target-file-name o) (target-file-names o)) (append-map target->input-files inputs)))))

(define* (clean #:optional targets)
  (for-each
   delete-file
   (filter file-exists? (delete-duplicates (append-map (cut target->input-files <>) (or targets %targets))))))

(define (tree o)
  (let ((inputs (target-inputs o)))
    (if (null? inputs) o
        (cons o (append (map tree inputs) (map tree (method-inputs (target-method o))))))))


(define (verbose fmt . o)
  ;;(apply format (cons* (current-error-port) fmt o))
  #t
  )

(define (PATH-search-path name)
  (or (search-path (string-split (getenv "PATH") #\:) name)
      (and (format (current-error-port) "warning: not found: ~a\n" name)
           name)))

(define %CC (PATH-search-path "gcc"))
(define %CC32 (PATH-search-path "i686-unknown-linux-gnu-gcc"))
(define %C-FLAGS
  '("--std=gnu99"
    "-O0"
    "-g"
    "-D"
    "POSIX=1"
    "-I" "src"
    "-I" "mlibc/include"
    "--include=mlibc/libc-gcc.c"
    ))
(define %C32-FLAGS
  '("--std=gnu99"
    "-O0"
    "-g"
    "-I" "src"
    "-I" "mlibc/include"
    "--include=mlibc/libc-gcc.c"
    ))

(define* (CC.gcc #:key (libc #t) (cc (if libc %CC %CC32)) (c-flags (if libc %C-FLAGS %C32-FLAGS)) (defines '()))
  (method (name "CC.gcc")
          (build (lambda (o t)
                   (let* ((input-files (map target-file-name (target-inputs t)))
                          (command `(,cc
                                     "-c"
                                     ,@(append-map (cut list "-D" <>) defines)
                                     ,@(if libc '() '("-nostdinc" "-fno-builtin"))
                                     ,@c-flags
                                     "-o" ,(target-file-name t)
                                     ,@input-files)))
                     (format (current-error-port) "  ~a\t ~a -> ~a\n" (method-name o) (string-join input-files) (target-file-name t))
                     (unless (zero? (apply system** command))
                       (format (current-error-port) "FAILED:~s\n" command)
                       (exit 1)))))
          (inputs (list (store #:add-file "mlibc/libc-gcc.c"))))) ;; FIXME: FLAGS

(define* (CPP.mescc #:key (cc %MESCC) (defines '()))
  (method (name "CPP.mescc")
          (build (lambda (o t)
                   (let ((input-files (map target-file-name (target-inputs t))))
                     (format #t "  ~a\t ~a -> ~a\n" (method-name o) (string-join input-files) (target-file-name t))
                     (apply system**
                            `(,cc
                              "-E"
                              ,@(append-map (cut list "-D" <>) defines)
                              "-o" ,(target-file-name t)
                              ,@input-files)))))))

(define %MESCC "guile/mescc.scm")
(define* (CC.mescc #:key (cc %MESCC))
  (method (name "CC.mescc")
          (build (lambda (o t)
                   (let ((input-files (map target-file-name (target-inputs t))))
                     (format #t "  ~a\t ~a -> ~a\n" (method-name o) (string-join input-files) (target-file-name t))
                     (apply system**
                            `("guile/mescc.scm" "-c"
                              "-o" ,(target-file-name t)
                              ,@input-files)))))
          (inputs (list (store #:add-file "guile/language/c99/compiler.go")
                        (store #:add-file "guile/mes/as.go")
                        (store #:add-file "guile/mes/as-i386.go")
                        (store #:add-file "guile/mes/M1.go")))))

(define %M1 (PATH-search-path "M1"))
(define %M1-FLAGS
  '("--LittleEndian"
    "--Architecture=1"
    ;;"--BaseAddress=0x1000000"
    ))
(define* (M1.asm #:key (m1 %M1) (m1-flags %M1-FLAGS))
  (method (name "M1")
          (build (lambda (o t)
                   (let* ((input-files (map target-file-name (target-inputs t)))
                          (input-files (filter (lambda (f) (string-suffix? "M1" f))
                                               input-files)))
                     (format #t "  ~a\t ~a -> ~a\n" (method-name o) (string-join input-files) (target-file-name t))
                     (with-output-to-file (target-file-name t)
                       (lambda _
                         (display
                          (apply assert-gulp-pipe*
                                 `(,m1
                                   "-f"
                                   "stage0/x86.M1"
                                   ,@(append-map (cut list "-f" <>) input-files)
                                   ,@m1-flags)))
                         (newline))))))
          (inputs (list (store #:add-file "stage0/x86.M1")))))

(define %HEX2-FLAGS
  '("--LittleEndian"
    "--Architecture=1"
    "--BaseAddress=0x1000000"))
(define %HEX2 (PATH-search-path "hex2"))

(define* (LINK.hex2 #:key (hex2 %HEX2) (hex2-flags %HEX2-FLAGS) debug?)
  (method (name "LINK.hex2")
          (build (lambda (o t)
                   (let* ((input-files (map target-file-name (target-inputs t)))
                          ;; FIXME: snarf inputs
                          (input-files (filter (lambda (f) (string-suffix? "hex2" f))
                                               input-files)))
                     (format #t "  ~a\t ~a -> ~a\n" (method-name o) (string-join input-files) (target-file-name t))
                     (with-output-to-file (target-file-name t)
                       (lambda _
                         (set-port-encoding! (current-output-port) "ISO-8859-1")
                         (display
                          (apply assert-gulp-pipe*
                                 `(,hex2
                                   ,@hex2-flags
                                   "-f"
                                   ,(if (not debug?) "stage0/elf32-0header.hex2"
                                        "stage0/elf32-header.hex2")
                                   ,@(append-map (cut list "-f" <>) input-files)
                                   "-f"
                                   ,(if (not debug?) "stage0/elf-0footer.hex2"
                                        "stage0/elf32-footer-single-main.hex2"))))))
                     (chmod (target-file-name t) #o755))))
          (inputs (list (store #:add-file "stage0/elf32-0header.hex2")
                        (store #:add-file "stage0/elf-0footer.hex2")))))

(define* (LINK.gcc #:key (cc %CC) (c-flags %C-FLAGS) (libc #t))
  (method (name "LINK.gcc")
          (build (lambda (o t)
                   (let* ((input-files (map target-file-name (target-inputs t)))
                          (command `(,cc
                                     ,@c-flags
                                     ,@(if libc '() '("-nostdlib"))
                                     "-o"
                                     ,(target-file-name t)
                                     ,@input-files)))
                     (format #t "  ~a\t ~a -> ~a\n" (method-name o) (string-join input-files) (target-file-name t))
                     (unless (zero? (apply system** command))
                       (format (current-error-port) "FAILED:~s\n" command)
                       (exit 1)))))))

(define SNARF "build-aux/mes-snarf.scm")
(define (SNARF.mes mes?)
  (method (name "SNARF.mes")
          (build (lambda (o t)
                   (let* ((input-files (map target-file-name (target-inputs t)))
                          (command `(,SNARF
                                     ,@(if mes? '("--mes") '())
                                     ,@input-files)))
                     (format #t "  ~a\t ~a -> ~a\n" (method-name o) (string-join input-files) (target-file-name t))
                     (unless (zero? (apply system** command))
                       (format (current-error-port) "FAILED:~s\n" command)
                       (exit 1)))))))

(define* (cpp.mescc input-file-name #:key (cc %MESCC) (defines '()))
  (let* ((c-target (target (file-name input-file-name)))
         (base-name (base-name input-file-name ".c"))
         (suffix ".E")
         (target-file-name (string-append base-name suffix)))
    (target (file-name target-file-name)
            (inputs (list c-target))
            (method (CPP.mescc #:cc cc #:defines defines)))))

(define mini-libc-mes.E (cpp.mescc "mlibc/mini-libc-mes.c"))
(define libc-mes.E (cpp.mescc "mlibc/libc-mes.c"))

(define* (compile.gcc input-file-name #:key (libc #t) (cc (if libc %CC %CC32)) (defines '()))
  (let* ((base-name (base-name input-file-name ".c"))
         (cross (if libc "" "mlibc-"))
         (suffix (string-append "." cross "o"))
         (target-file-name (string-append base-name suffix))
         (c-target (target (file-name input-file-name))))
    (target (file-name target-file-name)
            (inputs (list c-target))
            (method (CC.gcc #:cc cc #:libc libc #:defines defines)))))

(define* (compile.mescc input-file-name #:key (cc %CC) (libc libc-mes.E) (defines '()))
  (let* ((base-name (base-name input-file-name ".c"))
         ;;(foo (format (current-error-port) "COMPILE[~s .c] base=~s\n" input-file-name base-name))
         (suffix (cond ((not libc) ".0-M1")
                       ((eq? libc libc-mes.E) ".M1")
                       (else ".mini-M1")))
         (target-file-name (string-append base-name suffix))
         (E-target (cpp.mescc input-file-name #:cc cc #:defines defines)))
    (target (file-name target-file-name)
            (inputs `(,@(if libc (list libc) '()) ,E-target))
            (method (CC.mescc #:cc cc)))))

(define* (m1-asm input-file-name #:key (cc %MESCC) (m1 %M1) (libc libc-mes.E) (defines '()))
  (let* ((base-name (base-name input-file-name ".c"))
         ;;(foo (format (current-error-port) "m1-asm[~s .m1] base=~s\n" input-file-name base-name))
         (suffix (cond ((not libc) ".0-hex2")
                       ((eq? libc libc-mes.E) ".hex2")
                       (else ".mini-hex2")))
         (target-file-name (string-append base-name suffix))
         (m1-target (compile.mescc input-file-name #:cc cc #:libc libc #:defines defines))
         (libc.m1 (cond ((eq? libc libc-mes.E)
                         (compile.mescc "mlibc/libc-mes.c" #:libc #f #:defines defines))
                        ((eq? libc mini-libc-mes.E)
                         (compile.mescc "mlibc/mini-libc-mes.c" #:libc #f #:defines defines))
                        (else #f))))
    (target (file-name target-file-name)
            ;;(inputs `(,@(if libc (list libc.m1) '()) ,m1-target))
            (inputs `(,m1-target))
            (method (M1.asm #:m1 m1)))))

(define* (bin.mescc input-file-name #:key (cc %MESCC) (hex2 %HEX2) (m1 %M1) (libc libc-mes.E) (dependencies '()) (defines '()))
  (let* ((base-name (base-name input-file-name ".c"))
         ;;(foo (format (current-error-port) "bin[~s .c] base=~s\n" input-file-name base-name))
         (suffix (cond ((not libc) ".0-guile")
                       ((eq? libc libc-mes.E) ".guile")
                       (else ".mini-guile")))
         (target-file-name (string-append base-name suffix))
         (hex2-target (m1-asm input-file-name #:m1 m1 #:cc cc #:libc libc #:defines defines)))
    (target (file-name target-file-name)
            (inputs (cons hex2-target dependencies))
            (method (LINK.hex2 #:hex2 hex2 #:debug? (eq? libc libc-mes.E))))))

(define* (bin.gcc input-file-name #:key (libc #t) (cc (if libc %CC %CC32)) (dependencies '()) (defines '()))
  (let* ((base-name (base-name input-file-name ".c"))
         (suffix (if libc ".gcc" ".mlibc-gcc"))
         (target-file-name (string-append base-name suffix))
         (o-target (compile.gcc input-file-name #:cc cc #:libc libc #:defines defines)))
    (target (file-name target-file-name)
            (inputs (list o-target))
            (method (LINK.gcc #:cc cc #:libc libc)))))

(define* (snarf input-file-name #:key (dependencies '()) (mes? #t))
  (let* ((base-name (base-name input-file-name ".c"))
         (suffixes '(".h" ".i" ".environment.i" ".symbol-names.i" ".symbols.i"  ".symbols.h"))
         (suffixes (if mes? (map (cut string-append ".mes" <>) suffixes) suffixes))
         (target-file-names (map (cut string-append base-name <>) suffixes))
         (snarf-target (target (file-name input-file-name))))
    (target (file-name (car target-file-names))
            (file-names (cdr target-file-names))
            (inputs (cons snarf-target dependencies))
            ;;(inputs (list snarf-target))
            (method (SNARF.mes mes?)))))

(define ((target-prefix? prefix) o)
  (string-prefix? prefix (target-file-name o)))

(define (check-target? o)
  ((target-prefix? "check-") o))

(define (add-target o)
  (set! %targets (append %targets (list o)))
  o)
(define (get-target o)
  (if (target? o) o
      (find (lambda (t) (equal? (target-file-name t) o)) %targets)))
