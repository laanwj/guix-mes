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

  #:export (base-name
            build
            check
            clean
            group
            install
            target-prefix?
            check-target?
            install-target?

            cpp.mescc
            compile.mescc
            compile.gcc
            ld

            bin.mescc
            bin.gcc
            snarf
            m1.as

            crt1.mlibc-o
            libc-gcc.mlibc-o
            libc+tcc-gcc.mlibc-o

            add-target
            get-target

            conjoin
            system**
            target-file-name

            method
            target
            store
            target-inputs
            method-name
            assert-gulp-pipe*

            PATH-search-path

            %MESCC
            %HEX2
            %M1

            %targets
            %status

            %version
            %prefix
            %datadir
            %docdir
            %moduledir
            %guiledir
            %godir))

(define %status 0)
(define %targets '())
(define %store-dir ".store")
(mkdir-p %store-dir)
(define %command-log (open-output-file "script"))

(define (base-name file-name suffix)
  (string-drop-right file-name (string-length suffix)))

(define (conjoin . predicates)
  (lambda (. arguments)
    (every (cut apply <> arguments) predicates)))

(define (system** . command)
  (format %command-log "~a\n" (string-join command " "))
  (unless (zero? (apply system* command))
    (format (current-error-port) "FAILED:~s\n" command)
    (exit 1)))

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
  (baseline   target-baseline (default #f))        ; string: file-name
  (exit       target-exit (default #f))            ; number
  (signal     target-signal (default #f)))         ; number

(define method-file (method (name "FILE")))
(define method-check
  (method (name "CHECK")
          (build (lambda (o t)
                   (let* ((inputs (target-inputs t))
                          (file-name (target-file-name (build (car inputs))))
                          (run file-name)
                          (baseline (target-baseline t))
                          (exit (target-exit t))
                          (signal (target-signal t))
                          (log (string-append file-name "-check.log")))
                     (format (current-error-port) "  CHECK\t~a" (basename file-name))
                     (receive (output result)
                         ;; FIXME: quiet MES tests are not fun
                         (if (string-prefix? "tests/" run) (values #f (system* run "arg1" "arg2" "arg3" "arg4" "arg5"))
                             (gulp-pipe* run "arg1" "arg2" "arg3" "arg4" "arg5"))
                       (if (file-exists? log) (delete-file log))
                       (if (or baseline (and output (not (string-null? output)))) (with-output-to-file log (lambda _ (display output))))
                       (if baseline (set! result (system* "diff" "-bu" baseline log)))
                       (let ((status (if (string? result) 0
                                         (or (status:term-sig result) (status:exit-val result)))))
                         (if (file-exists? log) (store #:add-file log))
                         (format (current-error-port) "\t[~a]\n"
                                 (if (or (and signal (= status signal))
                                         (and exit (= status exit))) "OK"
                                         (begin (set! %status 1) "FAIL"))))))))))

(define %version (or (getenv "VERSION") "git"))
(define %prefix (or (getenv "PREFIX") ""))
(define %datadir "share/mes")
(define %docdir "share/doc/mes")
(define %moduledir (string-append %datadir "/module"))
(define %guiledir (string-append "share/guile/site/" (effective-version)))
(define %godir (string-append "lib/guile/" (effective-version) "/site-ccache"))

(define* (method-cp #:key substitutes)
  (method (name "INSTALL")
          (build (lambda (o t)
                   (let ((file-name (target-file-name t)))
                     (mkdir-p (dirname file-name))
                     (format (current-error-port) "  INSTALL\t~a\n" file-name)
                     (copy-file ((compose target-file-name car target-inputs) t) file-name)
                     (if substitutes
                         (begin
                           (substitute* file-name
                             (("module/") (string-append %prefix "/" %moduledir "/"))
                             (("@DATADIR@") (string-append %prefix "/" %datadir "/"))
                             (("@DOCDIR@") (string-append %prefix "/" %docdir "/"))
                             (("@GODIR@") (string-append %prefix "/" %godir "/"))
                             (("@GUILEDIR@") (string-append %prefix "/" %guiledir "/"))
                             (("@MODULEDIR@") (string-append %prefix "/" %moduledir "/"))
                             (("@PREFIX@") (string-append %prefix "/"))
                             (("@VERSION@") %version)))))))))

(define (hash-target o)
  (if (find (negate identity) (target-inputs o))
      (format (current-error-port) "invalid inputs[~s]: ~s\n" (target-file-name o) (target-inputs o)))
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
  (number->string (hash o (expt 2 31))))

(define (file-hash o)
  (string-hash (with-input-from-file o read-string)))

(define (store-file-name o)
  (string-append %store-dir "/" (if (string? o) o
                                    (target-hash o))))

(define (link-or-cp existing-file new-file)
  (catch #t
    (lambda _ (link existing-file new-file))
    (lambda _ (copy-file existing-file new-file))))

(define (assert-link existing-file new-file)
  (if (not (file-exists? new-file)) (link-or-cp existing-file new-file)))

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
            (add-file
             (or (and=> (find (lambda (t) (equal? (target-file-name t) add-file)) (map cdr *store*))
                        (compose (cut store #:get <>) target-hash))
                 (and (file-exists? add-file)
                      (store #:add (target (file-name add-file))))
                 (error (format #f "store add-file: no such file: ~s\n" add-file))))
            ((and get key)
             (or (assoc-ref *store* key)
                 (let ((store-file (store-file-name key))
                       (file-name (target-file-name get)))
                   (and (file-exists? store-file)
                        (if (file-exists? file-name) (delete-file file-name))
                        (link-or-cp store-file file-name)
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

(define* (check name #:key baseline (exit 0) (signal #f) (dependencies '()))
  (target (file-name (string-append "check-" name))
          (method method-check)
          (inputs (cons (get-target name) dependencies))
          (baseline baseline)
          (exit exit)
          (signal signal)))

(define* (install name #:key (dir (dirname name)) (installed-name (basename name)) (prefix %prefix) substitutes (dependencies '()))
  (target (file-name (string-append prefix "/" dir "/" installed-name))
          (method (method-cp #:substitutes substitutes))
          (inputs (cons (or (get-target name)
                            (store #:add-file name)) dependencies))))

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

(define* (PATH-search-path name #:key (default name))
  (or (search-path (string-split (getenv "PATH") #\:) name)
      (and (format (current-error-port) "warning: not found: ~a\n" name)
           default)))

(define %CC (or (getenv "CC") (PATH-search-path "gcc")))
(define %CC32 (or (getenv "CC32")
                  (PATH-search-path "i686-unknown-linux-gnu-gcc" #:default #f)
                  (and (format (current-error-port) "warning: CC32 not found, trying gcc -m32")
                       %CC)))

(define %C-FLAGS
  '("--std=gnu99"
    "-O0"
    "-g"
    "-D"
    "POSIX=1"
    "-I" "src"
    "-I" "lib"
    "-I" "include"
    "--include=lib/libc-gcc.c"))

(define %C32-FLAGS
  '("--std=gnu99"
    "-O0"
    "-fno-stack-protector"
    "-g"
    "-m32"
    "-I" "src"
    "-I" "lib"
    "-I" "include"))

(define* (CC.gcc #:key (libc #t) (cc (if (eq? libc #t) %CC %CC32)) (c-flags (if (eq? libc #t) %C-FLAGS %C32-FLAGS)) (defines '()) (includes '()))
  (method (name "CC.gcc")
          (build (lambda (o t)
                   (let* ((input-files (map target-file-name (target-inputs t)))
                          (command `(,cc
                                     "-c"
                                     ,@(append-map (cut list "-D" <>) defines)
                                     ,@(append-map (cut list "-I" <>) includes)
                                     ,@(if (eq? libc #t) '() '("-nostdinc" "-fno-builtin"))
                                     ,@c-flags
                                     "-o" ,(target-file-name t)
                                     ,@(filter (cut string-suffix? ".c" <>) input-files))))
                     (format (current-error-port) "  ~a\t ~a -> ~a\n" (method-name o) (string-join input-files) (target-file-name t))
                     (apply system** command))))))

(define* (CPP.mescc #:key (cc %MESCC) (defines '()) (includes '()))
  (method (name "CPP.mescc")
          (build (lambda (o t)
                   (let ((input-files (map target-file-name (target-inputs t))))
                     (format #t "  ~a\t ~a -> ~a\n" (method-name o) (string-join input-files) (target-file-name t))
                     (apply system**
                            `(,cc
                              "-E"
                              ,@(append-map (cut list "-D" <>) defines)
                              ,@(append-map (cut list "-I" <>) includes)
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
          (inputs (list (store #:add-file "guile/language/c99/info.go")
                        (store #:add-file "guile/language/c99/compiler.go")
                        (store #:add-file "guile/mes/as-i386.go")
                        (store #:add-file "guile/mes/as.go")
                        (store #:add-file "guile/mes/elf.go")
                        (store #:add-file "guile/mes/bytevectors.go")
                        (store #:add-file "guile/mes/M1.go")
                        (store #:add-file "guile/mes/guile.go")))))

(define %M1 (or (PATH-search-path "M1" #:default #f)
                (PATH-search-path "M0" #:default #f) ; M1 is in unreleased mescc-tools 0.2
                (and (format (current-error-port) "error: no macro assembler found, please install mescc-tools\n")
                     (exit 1))))
(define %M0-FLAGS
  '("--LittleEndian"))
(define %M1-FLAGS
  '("--LittleEndian"
    "--Architecture=1"))
(if (equal? (basename %M1) "M0")
    (set! %M1-FLAGS %M0-FLAGS))

(define* (M1.as #:key (m1 %M1) (m1-flags %M1-FLAGS))
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

(define* (LINK.gcc #:key (cc %CC) (libc #t) (c-flags (if (eq? libc #t) %C-FLAGS %C32-FLAGS)) (crt1 #f))
  (method (name "LINK.gcc")
          (build (lambda (o t)
                   (let* ((input-files (map target-file-name (target-inputs t)))
                          (command `(,cc
                                     ,@c-flags
                                     ,@(if (eq? libc #t) '() '("-nostdlib"))
                                     "-o"
                                     ,(target-file-name t)
                                     ,@(if crt1 (list (target-file-name crt1))'())
                                     ,@input-files
                                     ,@(cond ((eq? libc #t) '())
                                             (libc (list (target-file-name libc)))
                                             (else '())))))
                     (format #t "  ~a\t ~a -> ~a\n" (method-name o) (string-join input-files) (target-file-name t))
                     (apply system** command))))))

(define SNARF "build-aux/mes-snarf.scm")
(define (SNARF.mes mes?)
  (method (name "SNARF.mes")
          (build (lambda (o t)
                   (let* ((input-files (map target-file-name (target-inputs t)))
                          (command `(,SNARF
                                     ,@(if mes? '("--mes") '())
                                     ,@input-files)))
                     (format #t "  ~a\t ~a -> ~a\n" (method-name o) (string-join input-files) (target-file-name t))
                     (apply system** command))))))

(define* (cpp.mescc input-file-name #:key (cc %MESCC) (defines '()) (includes '()) (dependencies '()))
  (let* ((c-target (target (file-name input-file-name)))
         (base-name (base-name input-file-name ".c"))
         (suffix ".E")
         (target-file-name (string-append base-name suffix)))
    (target (file-name target-file-name)
            (inputs (cons c-target dependencies))
            (method (CPP.mescc #:cc cc #:defines defines #:includes includes)))))

(define* (compile.gcc input-file-name #:key (libc #t) (cc (if (eq? libc #t) %CC %CC32)) (defines '()) (includes '()) (dependencies '()))
  (let* ((base-name (base-name input-file-name ".c"))
         (cross (if (eq? libc #t) "" "mlibc-"))
         (suffix (string-append "." cross "o"))
         (target-file-name (string-append base-name suffix))
         (c-target (target (file-name input-file-name))))
    (target (file-name target-file-name)
            (inputs (cons c-target dependencies))
            (method (CC.gcc #:cc cc #:libc libc #:defines defines #:includes includes)))))

(define* (compile.mescc input-file-name #:key (cc %MESCC) (defines '()) (includes '()) (dependencies '()))
  (let* ((base-name (base-name input-file-name ".c"))
         (suffix ".M1")
         (target-file-name (string-append base-name suffix))
         (E-target (cpp.mescc input-file-name #:cc cc #:defines defines #:includes includes #:dependencies dependencies)))
    (target (file-name target-file-name)
            (inputs `(,E-target))
            (method (CC.mescc #:cc cc)))))

(define* (m1.as input-file-name #:key (cc %MESCC) (m1 %M1) (defines '()) (includes '()) (dependencies '()))
  (let* ((base-name (base-name input-file-name ".c"))
         ;;(foo (format (current-error-port) "m1.as[~s .m1] base=~s\n" input-file-name base-name))
         (suffix ".hex2")
         (target-file-name (string-append base-name suffix))
         (m1-target (compile.mescc input-file-name #:cc cc #:defines defines #:includes includes #:dependencies dependencies)))
    (target (file-name target-file-name)
            (inputs `(,m1-target))
            (method (M1.as #:m1 m1)))))

(define* (bin.gcc input-file-name #:key (libc #t) (crt1 (if (eq? libc #t) #f crt1.mlibc-o)) (cc (if (eq? libc #t) %CC %CC32)) (dependencies '()) (defines '()) (includes '()))
  (and cc
       (let* ((base-name (base-name input-file-name ".c"))
          (suffix (if (eq? libc #t) ".gcc" ".mlibc-gcc"))
          (target-file-name (string-append base-name suffix))
          (o-target (compile.gcc input-file-name #:cc cc #:libc libc #:defines defines #:includes includes #:dependencies dependencies)))
     (target (file-name target-file-name)
             (inputs (list o-target))
             (method (LINK.gcc #:cc cc #:libc libc #:crt1 crt1))))))

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
  (and o ((target-prefix? "check-") o)))

(define (install-target? o)
  (and o ((target-prefix? (or (getenv "PREFIX") "/")) o)))

(define (add-target o)
  (and o (set! %targets (append %targets (list o))))
  o)
(define (get-target o)
  (if (target? o) o
      (find (lambda (t) (equal? (target-file-name t) o)) %targets)))

(define crt1.mlibc-o (compile.gcc "lib/crt1.c" #:libc #f))
(define libc-gcc.mlibc-o (compile.gcc "lib/libc-gcc.c" #:libc #f))
(define libc+tcc-gcc.mlibc-o (compile.gcc "lib/libc+tcc-gcc.c" #:libc #f))
