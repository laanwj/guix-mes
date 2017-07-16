#! /bin/sh
# -*- scheme -*-
exec ${GUILE-guile} --no-auto-compile -L . -L guile -C . -C guile -s "$0" ${1+"$@"}
!#

(use-modules (srfi srfi-26)
             (guix shell-utils))

;; FIXME: .go dependencies
;; workaround: always update .go before calculating hashes
;;(use-modules ((mes make) #:select (sytem**)))
(define %scm-files
  '("guix/make.scm"
    "guix/records.scm"
    "guix/shell-utils.scm"
    "language/c99/compiler.scm"
    "language/c99/info.scm"
    "mes/as-i386.scm"
    "mes/as.scm"
    "mes/bytevectors.scm"
    "mes/elf.scm"
    "mes/M1.scm"))
(define %go-files (map (compose (cut string-append <> ".go") (cut string-drop-right <> 4)) %scm-files))
(setenv "srcdir" ".")
(setenv "host" %host-type)
(with-directory-excursion "guile"
  (apply system* `("guile"
                   "--no-auto-compile"
                   "-L" "."
                   "-C" "."
                   "-s"
                   "../build-aux/compile-all.scm"
                   ,@%scm-files)))

(use-modules (srfi srfi-1)
             (ice-9 curried-definitions)
             (ice-9 match)
             (guix make))

(add-target (bin.mescc "stage0/exit-42.c" #:libc #f))
(add-target (check "stage0/exit-42.0-guile" #:signal 11))  ; FIXME: segfault

(add-target (cpp.mescc "mlibc/mini-libc-mes.c"))
(add-target (compile.mescc "mlibc/mini-libc-mes.c"))

(add-target (bin.mescc "stage0/exit-42.c" #:libc mini-libc-mes.E))
(add-target (check "stage0/exit-42.mini-guile" #:exit 42))

(add-target (cpp.mescc "mlibc/libc-mes.c"))
(add-target (compile.mescc "mlibc/libc-mes.c"))

(add-target (bin.mescc "stage0/exit-42.c"))
(add-target (check "stage0/exit-42.guile" #:exit 42))

(define* (add-scaffold-test name #:key (exit 0) (libc libc-mes.E))
  (add-target (bin.gcc (string-append "scaffold/tests/" name ".c") #:libc #f))
  (add-target (check (string-append "scaffold/tests/" name ".mlibc-gcc") #:exit exit))

  (add-target (bin.mescc (string-append "scaffold/tests/" name ".c") #:libc libc))
  (add-target (check (string-append "scaffold/tests/" name "." (cond ((not libc) "0-")
                                                                     ((eq? libc mini-libc-mes.E) "mini-")
                                                                     (else "")) "guile") #:exit exit)))


(add-scaffold-test "t" #:libc mini-libc-mes.E)

;; tests/00: exit, functions without libc
(add-scaffold-test "00-exit-0" #:libc #f)
(add-scaffold-test "01-return-0" #:libc #f)
(add-scaffold-test "02-return-1" #:libc #f #:exit 1)
(add-scaffold-test "03-call" #:libc #f)
(add-scaffold-test "04-call-0" #:libc #f)
(add-scaffold-test "05-call-1" #:libc #f #:exit 1)
(add-scaffold-test "06-call-!1" #:libc #f)

(add-target (group "check-scaffold-tests/0" #:dependencies (filter (target-prefix? "check-scaffold/tests/0") %targets)))

;; tests/10: control without libc
(for-each
 (cut add-scaffold-test <> #:libc #f)
 '("10-if-0"
   "11-if-1"
   "12-if-=="
   "13-if-!="
   "14-if-goto"
   "15-if-!f"
   "16-if-t"))

(add-target (group "check-scaffold-tests/1" #:dependencies (filter (target-prefix? "check-scaffold/tests/1") %targets)))

;; tests/20: loop without libc
(for-each
 (cut add-scaffold-test <> #:libc #f)
 '("20-while"
   "21-char[]"
   "22-while-char[]"
   "23-pointer"))

(add-target (group "check-scaffold-tests/2" #:dependencies (filter (target-prefix? "check-scaffold/tests/2") %targets)))

;; tests/30: call, compare: mini-libc-mes.c
(for-each
 (cut add-scaffold-test <> #:libc mini-libc-mes.E)
 '("30-strlen"
   "31-eputs"
   "32-compare"
   "33-and-or"
   "34-pre-post"
   "35-compare-char"
   "36-compare-arithmetic"
   "37-compare-assign"
   "38-compare-call"))

(add-target (group "check-scaffold-tests/3" #:dependencies (filter (target-prefix? "check-scaffold/tests/3") %targets)))

;; tests/40: control: mini-libc-mes.c
(for-each
 (cut add-scaffold-test <> #:libc mini-libc-mes.E)
 '("40-if-else"
   "41-?"
   "42-goto-label"
   "43-for-do-while"
   "44-switch"
   "45-void-call"))

(add-target (group "check-scaffold-tests/4" #:dependencies (filter (target-prefix? "check-scaffold/tests/4") %targets)))

;; tests/50: libc-mes.c
(for-each
 add-scaffold-test
 '("50-assert"
   "51-strcmp"
   "52-itoa"
   "54-argv"))

(add-target (group "check-scaffold-tests/5" #:dependencies (filter (target-prefix? "check-scaffold/tests/5") %targets)))

;; tests/60: building up to scaffold/m.c, scaffold/micro-mes.c
(for-each
 add-scaffold-test
 '("60-math"
   "61-array"
   "63-struct-cell"
   "64-make-cell"
   "65-read"))

(add-target (group "check-scaffold-tests/6" #:dependencies (filter (target-prefix? "check-scaffold/tests/6") %targets)))

;; tests/70: and beyond src/mes.c -- building up to 8cc.c, pcc.c, tcc.c, libguile/eval.c
(for-each
 add-scaffold-test
 '("70-printf"
   "71-struct-array"
   "72-typedef-struct-def"
   "73-union"))

(add-target (group "check-scaffold-tests/7" #:dependencies (filter (target-prefix? "check-scaffold/tests/7") %targets)))

(add-target (group "check-scaffold-tests" #:dependencies (filter (target-prefix? "check-scaffold/tests") %targets)))


(define* (add-tcc-test name)
  (add-target (bin.gcc (string-append "scaffold/tinycc/" name ".c") #:libc #f #:includes '("scaffold/tinycc")))
  (add-target (check (string-append "scaffold/tinycc/" name ".mlibc-gcc") #:baseline (string-append "scaffold/tinycc/" name ".expect")))

  (add-target (bin.mescc (string-append "scaffold/tinycc/" name ".c") #:includes '("scaffold/tinycc")))
  (add-target (check (string-append "scaffold/tinycc/" name ".guile") #:baseline (string-append "scaffold/tinycc/" name ".expect"))))
(map
 add-tcc-test
 '("00_assignment"
   "01_comment"
   "02_printf"
   "03_struct"
   "04_for"
   "05_array"
   "06_case"
   "07_function"
   "08_while"
   "09_do_while"

   "10_pointer"
   "11_precedence"
   "12_hashdefine"
   "13_integer_literals"
   "14_if"
   "15_recursion"
   "16_nesting"
   "17_enum"
   "18_include"
   "19_pointer_arithmetic"

   "20_pointer_comparison"
   "21_char_array"
   ;;"22_floating_point"       ; float
   ;;"23_type_coercion"        ; float
   ;;"24_math_library"         ; float
   "25_quicksort"
   ;;"27_sizeof"               ; float
   ;;"28_strings"              ; TODO: strncpy strchr strrchr memset memcpy memcmp
   "29_array_address"

   ;;"30_hanoi"                ; fails with GCC
   "31_args"
   ;;"32_led"                  ; unsupported: (decl (decl-spec-list (stor-spec (static)) (type-spec (fixed-type "int"))) (init-declr-list (init-declr (array-of (ident "d") (p-expr (fixed "32"))))))
   ;;"34_array_assignment"     ; fails with GCC
   "33_ternary_op"
   "35_sizeof"
   ;;"36_array_initialisers"   ; unspported: (decl (decl-spec-list (type-spec (fixed-type "int"))) (init-declr-list (init-declr (array-of (ident "Array") (p-expr (fixed "10"))) (initzer (initzer-list (initzer (p-expr (fixed "12"))) (initzer (p-expr (fixed "34"))) (initzer (p-expr (fixed "56"))) (initzer (p-expr (fixed "78"))) (initzer (p-expr (fixed "90"))) (initzer (p-expr (fixed "123"))) (initzer (p-expr (fixed "456"))) (initzer (p-expr (fixed "789"))) (initzer (p-expr (fixed "8642"))) (initzer (p-expr (fixed "9753"))))))))
   ;; "37_sprintf"             ; integer formatting unsupported
   ;;"38_multiple_array_index" ; unspported: (decl (decl-spec-list (type-spec (fixed-type "int"))) (init-declr-list (init-declr (array-of (array-of (ident "a") (p-expr (fixed "4"))) (p-expr (fixed "4"))))))
   ;;"39_typedef"              ; unsupported: (decl (decl-spec-list (stor-spec (typedef)) (type-spec (typename "MyFunStruct"))) (init-declr-list (init-declr (ptr-declr (pointer) (ident "MoreFunThanEver")))))

   ;;"40_stdio"                ; f* functions
   "41_hashif"
   ;;"42_function_pointer"     ; f* functions
   "43_void_param"
   "44_scoped_declarations"
   ;; "45_empty_for"           ; unsupported
   ;;"46_grep"                 ; f* functions
   "47_switch_return"
   "48_nested_break"
   ;;"49_bracket_evaluation"   ; float

   "50_logical_second_arg"
   ;;"51_static"               ; unsupported: (decl (decl-spec-list (stor-spec (static)) (type-spec (fixed-type "int"))) (init-declr-list (init-declr (ident "fred") (initzer (p-expr (fixed "1234"))))))
   ;;"52_unnamed_enum"         ; unsupported: (decl (decl-spec-list (stor-spec (typedef)) (type-spec (enum-def (enum-def-list (enum-defn (ident "e")) (enum-defn (ident "f")) (enum-defn (ident "g")))))) (init-declr-list (init-declr (ident "h"))))
   "54_goto"
   ;;"55_lshift_type"          ; unsigned
   ))

(add-target (group "check-scaffold-tinycc" #:dependencies (filter (target-prefix? "check-scaffold/tinycc") %targets)))

;;(add-target (group "check-scaffold" #:dependencies (filter (target-prefix? "check-scaffold") %targets)))

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

(add-target (bin.gcc "scaffold/micro-mes.c" #:libc #f))
(add-target (check "scaffold/micro-mes.mlibc-gcc" #:exit 6)) ; arg1 arg2 arg3 arg4 arg5

(add-target (bin.mescc "scaffold/micro-mes.c"))
(add-target (check "scaffold/micro-mes.guile" #:exit 6)) ; arg1 arg2 arg3 arg4 arg5

(add-target (group "check-scaffold" #:dependencies (filter (target-prefix? "check-scaffold") %targets)))

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
   (add-target (snarf "src/gc.c"))
   (add-target (snarf "src/lib.c" #:mes? #t))
   (add-target (snarf "src/math.c" #:mes? #t))
   (add-target (snarf "src/mes.c" #:mes? #t))
   (add-target (snarf "src/posix.c" #:mes? #t))
   (add-target (snarf "src/reader.c" #:mes? #t))
   (add-target (snarf "src/vector.c" #:mes? #t))))

(add-target (bin.gcc "src/mes.c" #:dependencies gcc-snarf-targets
                     #:defines `("FIXED_PRIMITIVES=1"
                                 "MES_FULL=1"
                                 "POSIX=1"
                                 ,(string-append "VERSION=\"" %version "\"")
                                 ,(string-append "MODULEDIR=\"" (string-append %moduledir "/") "\"")
                                 ,(string-append "PREFIX=\"" %prefix "\""))))

(add-target (bin.gcc "src/mes.c" #:libc #f
                     #:dependencies mes-snarf-targets
                     #:defines `("FIXED_PRIMITIVES=1"
                                 "MES_FULL=1"
                                 ,(string-append "VERSION=\"" %version "\"")
                                 ,(string-append "MODULEDIR=\"" (string-append %moduledir "/") "\"")
                                 ,(string-append "PREFIX=\"" %prefix "\""))))

(add-target (bin.mescc "src/mes.c" #:dependencies mes-snarf-targets
                       #:defines `("FIXED_PRIMITIVES=1"
                                   "MES_FULL=1"
                                 ,(string-append "VERSION=\"" %version "\"")
                                 ,(string-append "MODULEDIR=\"" (string-append %moduledir "/") "\"")
                                 ,(string-append "PREFIX=\"" %prefix "\""))))

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

(add-target (install "guile/mescc.scm" #:dir "bin" #:substitutes #t))
(add-target (install "scripts/mescc.mes" #:dir "bin" #:substitutes #t))
(add-target (install "scripts/repl.mes" #:dir "bin" #:substitutes #t))
(define bootstrap? #f)
(if bootstrap?
    (add-target (install "src/mes.mes" #:dir "bin" #:installed-name "mes"))
    (add-target (install "src/mes.guile" #:dir "bin" #:installed-name "mes")))

(define* ((install-dir #:key dir) name)
  (add-target (install name  #:dir (string-append dir "/" (dirname name)))))

(add-target (install "module/mes/base-0.mes" #:dir (string-append %moduledir "/mes") #:substitutes #t))
(add-target (install "module/language/c99/compiler.mes" #:dir (string-append %moduledir "/language/c99") #:substitutes #t))

(define %module-dir "share/mes")
(for-each
 (lambda (f)
   ((install-dir #:dir (string-append %module-dir)) f))
 '("module/language/c99/compiler.mes"
   "module/language/c99/compiler.scm"
   "module/language/c99/info.mes"
   "module/language/c99/info.scm"
   "module/language/paren.mes"
   "module/mes/M1.mes"
   "module/mes/M1.scm"
   "module/mes/as-i386.mes"
   "module/mes/as-i386.scm"
   "module/mes/as.mes"
   "module/mes/as.scm"
   ;;"module/mes/base-0.mes"
   "module/mes/base.mes"
   "module/mes/bytevectors.mes"
   "module/mes/bytevectors.scm"
   "module/mes/catch.mes"
   "module/mes/display.mes"
   "module/mes/elf.mes"
   "module/mes/elf.scm"
   "module/mes/fluids.mes"
   "module/mes/getopt-long.mes"
   "module/mes/getopt-long.scm"
   "module/mes/guile.mes"
   "module/mes/lalr.mes"
   "module/mes/lalr.scm"
   "module/mes/let.mes"
   "module/mes/match.mes"
   "module/mes/match.scm"
   "module/mes/optargs.mes"
   "module/mes/optargs.scm"
   "module/mes/peg.mes"
   "module/mes/peg/cache.scm"
   "module/mes/peg/codegen.scm"
   "module/mes/peg/simplify-tree.scm"
   "module/mes/peg/string-peg.scm"
   "module/mes/peg/using-parsers.scm"
   "module/mes/pmatch.mes"
   "module/mes/pmatch.scm"
   "module/mes/posix.mes"
   "module/mes/pretty-print.mes"
   "module/mes/pretty-print.scm"
   "module/mes/psyntax-0.mes"
   "module/mes/psyntax-1.mes"
   "module/mes/psyntax.mes"
   "module/mes/psyntax.pp"
   "module/mes/psyntax.ss"
   "module/mes/quasiquote.mes"
   "module/mes/quasisyntax.mes"
   "module/mes/quasisyntax.scm"
   "module/mes/read-0.mes"
   "module/mes/record-0.mes"
   "module/mes/record.mes"
   "module/mes/repl.mes"
   "module/mes/scm.mes"
   "module/mes/syntax.mes"
   "module/mes/syntax.scm"
   "module/mes/test.mes"
   "module/mes/tiny-0.mes"
   "module/mes/type-0.mes"
   "module/nyacc/lalr.mes"
   "module/nyacc/lang/c99/cpp.mes"
   "module/nyacc/lang/c99/parser.mes"
   "module/nyacc/lang/calc/parser.mes"
   "module/nyacc/lang/util.mes"
   "module/nyacc/lex.mes"
   "module/nyacc/parse.mes"
   "module/nyacc/util.mes"
   "module/rnrs/arithmetic/bitwise.mes"
   "module/srfi/srfi-0.mes"
   "module/srfi/srfi-1.mes"
   "module/srfi/srfi-1.scm"
   "module/srfi/srfi-13.mes"
   "module/srfi/srfi-14.mes"
   "module/srfi/srfi-16.mes"
   "module/srfi/srfi-16.scm"
   "module/srfi/srfi-26.mes"
   "module/srfi/srfi-26.scm"
   "module/srfi/srfi-43.mes"
   "module/srfi/srfi-9-psyntax.mes"
   "module/srfi/srfi-9.mes"
   "module/srfi/srfi-9.scm"
   "module/sxml/xpath.mes"
   "module/sxml/xpath.scm"))

(define* ((install-guile-dir #:key dir) name)
  (add-target (install (string-append "guile/" name) #:dir (string-append dir "/" (dirname name)))))

(for-each
 (lambda (f)
   ((install-guile-dir #:dir (string-append %guiledir)) f))
 %scm-files)

(for-each
 (lambda (f)
   ((install-guile-dir #:dir (string-append %godir)) f))
 %go-files)

(add-target (install "mlibc/libc-mes.E" #:dir "lib"))
(add-target (install "mlibc/libc-mes.M1" #:dir "lib"))
(add-target (install "mlibc/mini-libc-mes.E" #:dir "lib"))
(add-target (install "mlibc/mini-libc-mes.M1" #:dir "lib"))

(for-each
 (lambda (f)
   ((install-dir #:dir "share/") f))
 '("mlibc/include/alloca.h"
   "mlibc/include/assert.h"
   "mlibc/include/ctype.h"
   "mlibc/include/dlfcn.h"
   "mlibc/include/errno.h"
   "mlibc/include/fcntl.h"
   "mlibc/include/features.h"
   "mlibc/include/inttypes.h"
   "mlibc/include/libgen.h"
   "mlibc/include/limits.h"
   "mlibc/include/locale.h"
   "mlibc/include/math.h"
   "mlibc/include/mlibc.h"
   "mlibc/include/setjmp.h"
   "mlibc/include/signal.h"
   "mlibc/include/stdarg.h"
   "mlibc/include/stdbool.h"
   "mlibc/include/stdint.h"
   "mlibc/include/stdio.h"
   "mlibc/include/stdlib.h"
   "mlibc/include/stdnoreturn.h"
   "mlibc/include/string.h"
   "mlibc/include/strings.h"
   "mlibc/include/sys/cdefs.h"
   "mlibc/include/sys/mman.h"
   "mlibc/include/sys/stat.h"
   "mlibc/include/sys/time.h"
   "mlibc/include/sys/timeb.h"
   "mlibc/include/sys/types.h"
   "mlibc/include/sys/ucontext.h"
   "mlibc/include/sys/wait.h"
   "mlibc/include/time.h"
   "mlibc/include/unistd.h"))

(for-each
 (compose add-target (cut install <> #:dir "share/doc/mes"))
 '("AUTHORS"
   ;;"ChangeLog"
   "COPYING"
   "HACKING"
   "INSTALL"
   "NEWS"
   "README"))

(add-target (install "doc/fosdem/fosdem.pdf" #:dir "share/doc/mes"))

(define (main args)
  (cond ((member "all-go" args) #t)
        ((member "clean-go" args) (map delete-file (filter file-exists? %go-files)))
        ((member "clean" args) (clean))
        ((member "list" args) (display (string-join (map target-file-name %targets) "\n" 'suffix)))
        ((member "help" args) (format #t "Usage: ./make.scm [TARGET]...

Targets:
    all
    all-go
    check
    clean
    clean-go
    help~a
    install
    list
"
                                      (string-join (filter (negate (cut string-index <> #\/)) (map target-file-name %targets)) "\n    " 'prefix)))
        (else
         (let ((targets (match args
                          (() (filter (negate check-target?) %targets))
                          ((? (cut member "all" <>)) (filter (conjoin (negate install-target?)
                                                                      (negate check-target?))
                                                             %targets))
                          ((? (cut member "check" <>)) (filter check-target? %targets))
                          ((? (cut member "install" <>)) (filter install-target? %targets))
                          (_ (filter-map (cut get-target <>) args)))))
           (for-each build targets)
           ;;((@@ (mes make) store) #:print 0)
           (exit %status)))))

(main (cdr (command-line)))
