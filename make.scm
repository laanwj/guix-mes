#! /usr/bin/env guile
!#

(set! %load-path (cons "guile" %load-path))
(set! %load-path (cons "../guix" %load-path))
(set! %load-compiled-path (cons "guile" %load-compiled-path))
(set! %load-compiled-path (cons "../guix" %load-compiled-path))

(use-modules (guix shell-utils))

;; FIXME: .go dependencies
;; workaround: always update .go before calculating hashes
;;(use-modules ((mes make) #:select (sytem**)))
(let* ((scm-files '("guix/make.scm"
                    "guix/records.scm"
                    "guix/shell-utils.scm"
                    "language/c99/compiler.scm"
                    "mes/as-i386.scm"
                    "mes/as.scm"
                    "mes/elf.scm"
                    "mes/M1.scm")))
  (setenv "srcdir" "guile")
  (setenv "host" %host-type)
  (with-directory-excursion "guile"
    (apply system* `("guile"
                     "--no-auto-compile"
                     "-L" "."
                     "-C" "."
                     "-s"
                     "../build-aux/compile-all.scm"
                     ,@scm-files))))

(use-modules (srfi srfi-1)
             (srfi srfi-26)
             (ice-9 match)
             (guix make))

(add-target (bin.mescc "stage0/exit-42.c" #:libc #f))
(add-target (check "stage0/exit-42.0-guile" #:signal 11))  ; FIXME: segfault

(add-target (bin.mescc "stage0/exit-42.c" #:libc mini-libc-mes.E))
(add-target (check "stage0/exit-42.mini-guile" #:exit 42))

(add-target (bin.mescc "stage0/exit-42.c"))
(add-target (check "stage0/exit-42.guile" #:exit 42))


(add-target (bin.gcc "scaffold/hello.c"))
(add-target (check "scaffold/hello.gcc" #:exit 42))

(add-target (bin.gcc "scaffold/hello.c" #:libc #f))
(add-target (check "scaffold/hello.mlibc-gcc" #:exit 42))

(add-target (bin.mescc "scaffold/hello.c" #:libc mini-libc-mes.E))
(add-target (check "scaffold/hello.mini-guile" #:exit 42))

(add-target (bin.mescc "scaffold/hello.c"))
(add-target (check "scaffold/hello.guile" #:exit 42))


(add-target (bin.gcc "scaffold/m.c"))
(add-target (check "scaffold/m.gcc" #:exit 255))

(add-target (bin.gcc "scaffold/m.c" #:libc #f))
(add-target (check "scaffold/m.mlibc-gcc" #:exit 255))

(add-target (bin.mescc "scaffold/m.c"))
(add-target (check "scaffold/m.guile" #:exit 255))


(add-target (bin.gcc "scaffold/t-tcc.c"))
(add-target (check "scaffold/t-tcc.gcc"))

(add-target (bin.gcc "scaffold/t-tcc.c" #:libc #f))
(add-target (check "scaffold/t-tcc.mlibc-gcc"))

(add-target (bin.mescc "scaffold/t-tcc.c"))
(add-target (check "scaffold/t-tcc.guile"))


(add-target (bin.gcc "scaffold/micro-mes.c" #:libc #f))
(add-target (check "scaffold/micro-mes.mlibc-gcc" #:exit 1))

(add-target (bin.mescc "scaffold/micro-mes.c"))
(add-target (check "scaffold/micro-mes.guile" #:exit 1))


(add-target (bin.gcc "scaffold/t.c"))
(add-target (check "scaffold/t.gcc"))

(add-target (bin.gcc "scaffold/t.c" #:libc #f))
(add-target (check "scaffold/t.mlibc-gcc"))

(add-target (bin.mescc "scaffold/t.c"))
(add-target (check "scaffold/t.guile"))

(define snarf-bases
  '("gc" "lib" "math" "mes" "posix" "reader" "vector"))

(define bla
  `(,@(map (cut string-append "src/" <> ".c") snarf-bases)
    ,@(map (cut string-append "src/" <> ".mes.h") snarf-bases)
    ,@(map (cut string-append "src/" <> ".mes.i") snarf-bases)
    ,@(map (cut string-append "src/" <> ".mes.environment.i") snarf-bases)))

(define gcc-snarf-targets
  (list
   (add-target (snarf "src/gc.c" #:mes? #f))
   (add-target (snarf "src/lib.c" #:mes? #f))
   (add-target (snarf "src/math.c" #:mes? #f))
   (add-target (snarf "src/mes.c" #:mes? #f))
   (add-target (snarf "src/posix.c" #:mes? #f))
   (add-target (snarf "src/reader.c" #:mes? #f))
   (add-target (snarf "src/vector.c" #:mes? #f))))

(define mes-snarf-targets
  (list
   (add-target (snarf "src/gc.c" #:mes? #t))
   (add-target (snarf "src/lib.c" #:mes? #t))
   (add-target (snarf "src/math.c" #:mes? #t))
   (add-target (snarf "src/mes.c" #:mes? #t))
   (add-target (snarf "src/posix.c" #:mes? #t))
   (add-target (snarf "src/reader.c" #:mes? #t))
   (add-target (snarf "src/vector.c" #:mes? #t))))

(define VERSION "0.8")
(define PREFIX (or (getenv "PREFIX") "/usr/local"))
(define DATADIR (or (getenv "DATADIR") (string-append PREFIX " /share")))
(define MODULEDIR (or (getenv "MODULEDIR") (string-append DATADIR "/module/")))

(add-target (bin.gcc "src/mes.c" #:dependencies gcc-snarf-targets
                     #:defines `("FIXED_PRIMITIVES=1"
                                 "MES_FULL=1"
                                 "POSIX=1"
                                 ,(string-append "VERSION=\"" VERSION "\"")
                                 ,(string-append "MODULEDIR=\"" MODULEDIR "\"")
                                 ,(string-append "PREFIX=\"" PREFIX "\""))))

(add-target (bin.gcc "src/mes.c" #:libc #f
                     #:dependencies mes-snarf-targets
                     #:defines `("FIXED_PRIMITIVES=1"
                                 "MES_FULL=1"
                                 ,(string-append "VERSION=\"" VERSION "\"")
                                 ,(string-append "MODULEDIR=\"" MODULEDIR "\"")
                                 ,(string-append "PREFIX=\"" PREFIX "\""))))

(add-target (bin.mescc "src/mes.c" #:dependencies mes-snarf-targets
                       #:defines `("FIXED_PRIMITIVES=1"
                                   "MES_FULL=1"
                                 ,(string-append "VERSION=\"" VERSION "\"")
                                 ,(string-append "MODULEDIR=\"" MODULEDIR "\"")
                                 ,(string-append "PREFIX=\"" PREFIX "\""))))

(define mes-tests
  '("tests/read.test"
    "tests/base.test"
    "tests/closure.test"
    "tests/quasiquote.test"
    "tests/let.test"
    "tests/scm.test"
    "tests/display.test"
    "tests/cwv.test"
    "tests/math.test"
    "tests/vector.test"
    "tests/srfi-1.test"
    "tests/srfi-13.test"
    "tests/srfi-14.test"
    "tests/optargs.test"
    "tests/fluids.test"
    "tests/catch.test"
    "tests/psyntax.test"
    "tests/pmatch.test"
    "tests/let-syntax.test"
    "tests/guile.test"
    "tests/record.test"
    ;;sloooowwww
    ;;"tests/match.test"
    ;;"tests/peg.test"
    ))

(define (add-mes.gcc-test o)
  (add-target (target (file-name o)))
  (add-target (check o #:dependencies (list (get-target "src/mes.mlibc-gcc")))))

(define (add-mes.guile-test o)
  (add-target (target (file-name o)))
  (add-target (check o #:dependencies (list (get-target "src/mes.guile")))))

;; takes long, and should always pass if...
;;(for-each add-mes.gcc-test mes-tests)

;; ...mes.guile passes :-)
(for-each add-mes.guile-test mes-tests)

;; FIXME: run tests/base.test
(setenv "MES" "src/mes.guile")

(define (check-target? o)
  (string-prefix? "check-" (target-file-name o)))

(define (main args)
  (cond ((member "clean" args) (clean))
        ((member "help" args) (display "Usage: ./make.scm [TARGET]...

Targets:
    all
    check
    clean

    stage0/exit42.mini-guile
    scaffold/hello.guile
    src/mes.guile
"))
        (else
         (let ((targets (match args
                          (() (filter (negate check-target?) %targets))
                          ((? (cut member "all" <>)) (filter (negate check-target?) %targets))
                          ((? (cut member "check" <>)) (filter check-target? %targets))
                          (_ (filter-map (cut get-target <>) args)))))
           (for-each build targets)
           ;;((@@ (mes make) store) #:print 0)
           (exit %status)))))

(main (cdr (command-line)))
