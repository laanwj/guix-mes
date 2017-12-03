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
    "mes/guile.scm"
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

(define crt1.hex2 (m1.as "lib/crt1.c"))
(add-target crt1.hex2)

(add-target crt1.mlibc-o)

(define %HEX2-FLAGS
  '("--LittleEndian"
    "--Architecture=1"
    "--BaseAddress=0x1000000"))
(define %HEX2 (PATH-search-path "hex2"))

(define* (LINK.hex2 #:key (hex2 %HEX2) (hex2-flags %HEX2-FLAGS) (crt1 crt1.hex2) (libc libc-mes.hex2) debug?)
  (method (name "LINK.hex2")
          (build (lambda (o t)
                   (let* ((input-files (map target-file-name (target-inputs t)))
                          ;; FIXME: snarf inputs
                          (input-files (filter (lambda (f) (and (string-suffix? "hex2" f)
                                                                (not (member f (cdr input-files)))))
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
                                   ,@(if crt1 `("-f" ,(target-file-name crt1)) '())
                                   ,@(if libc `("-f" ,(target-file-name libc)) '())
                                   ,@(append-map (cut list "-f" <>) input-files)
                                   "-f"
                                   ,(if (not debug?) "stage0/elf-0footer.hex2"
                                        "stage0/elf32-footer-single-main.hex2"))))))
                     (chmod (target-file-name t) #o755))))
          (inputs `(,(store #:add-file "stage0/elf32-0header.hex2")
                    ,@(if crt1 (target-inputs crt1) '())
                    ,@(if libc (target-inputs libc) '())
                    ,(store #:add-file "stage0/elf-0footer.hex2")))))

(define* (bin.mescc input-file-name #:key (cc %MESCC) (hex2 %HEX2) (m1 %M1) (crt1 crt1.hex2) (libc libc-mes.hex2) (dependencies '()) (defines '()) (includes '()))
  (let* ((base-name (base-name input-file-name ".c"))
         ;;(foo (format (current-error-port) "bin[~s .c] base=~s\n" input-file-name base-name))
         (suffix (cond ((not libc) ".0-guile")
                       ((eq? libc libc-mes.hex2) ".guile")
                       ((eq? libc libc+tcc-mes.hex2) ".tcc-guile")
                       (else ".mini-guile")))
         (target-file-name (string-append base-name suffix))
         (hex2-target (m1.as input-file-name #:m1 m1 #:cc cc #:defines defines #:includes includes #:dependencies dependencies)))
    (target (file-name target-file-name)
            (inputs `(,hex2-target
                      ,@(if crt1 (list crt1) '())
                      ,@(if libc (list libc) '())))
            (method (LINK.hex2 #:hex2 hex2 #:crt1 crt1 #:libc libc #:debug? (eq? libc libc-mes.hex2))))))

;;(define mini-libc-mes.E (m1.as "lib/mini-libc-mes.c"))

(define libc-mes.hex2 (m1.as "lib/libc-mes.c"))
(add-target libc-mes.hex2)

(define mini-libc-mes.hex2 (m1.as "lib/mini-libc-mes.c"))
(add-target mini-libc-mes.hex2)

(define libc+tcc-mes.hex2 (m1.as "lib/libc+tcc-mes.c"))
(add-target libc+tcc-mes.hex2)

(add-target (bin.mescc "stage0/exit-42.c" #:libc #f))
(add-target (check "stage0/exit-42.0-guile" #:exit 42))

(add-target (cpp.mescc "lib/mini-libc-mes.c"))
(add-target (compile.mescc "lib/mini-libc-mes.c"))

(add-target (bin.mescc "stage0/exit-42.c" #:libc mini-libc-mes.hex2))
(add-target (check "stage0/exit-42.mini-guile" #:exit 42))

(add-target (cpp.mescc "lib/libc-mes.c"))
(add-target (compile.mescc "lib/libc-mes.c"))

(add-target (bin.mescc "stage0/exit-42.c"))
(add-target (check "stage0/exit-42.guile" #:exit 42))

(define* (add-scaffold-test name #:key (exit 0) (libc libc-mes.hex2) (libc-gcc libc-gcc.mlibc-o) (includes '()))
  (add-target (bin.gcc (string-append "scaffold/tests/" name ".c") #:libc libc-gcc #:includes includes))
  (add-target (check (string-append "scaffold/tests/" name ".mlibc-gcc") #:exit exit))

  (add-target (bin.mescc (string-append "scaffold/tests/" name ".c") #:libc libc #:includes includes))
  (add-target (check (string-append "scaffold/tests/" name "." (cond ((not libc) "0-")
                                                                     ((eq? libc mini-libc-mes.hex2) "mini-")
                                                                     ((eq? libc libc+tcc-mes.hex2) "tcc-")
                                                                     (else "")) "guile") #:exit exit)))

(add-target (compile.gcc "lib/crt1.c" #:libc #f))
(add-target (compile.gcc "lib/libc-gcc.c" #:libc #f))
(add-target (compile.gcc "lib/libc+tcc-gcc.c" #:libc #f))
;;(add-target (compile.gcc "lib/libc+tcc-mes.c" #:libc #f))

;;(add-scaffold-test "t" #:libc mini-libc-mes.hex2)
(add-scaffold-test "t")
;;(add-scaffold-test "t" #:libc libc+tcc-mes.hex2)

;; tests/00: exit, functions without libc
(add-scaffold-test "00-exit-0" #:libc #f)
(add-scaffold-test "01-return-0" #:libc #f)
(add-scaffold-test "02-return-1" #:libc #f #:exit 1)
(add-scaffold-test "03-call" #:libc #f)
(add-scaffold-test "04-call-0" #:libc #f)
(add-scaffold-test "05-call-1" #:libc #f #:exit 1)
(add-scaffold-test "06-call-!1" #:libc #f)
(add-scaffold-test "07-include" #:libc #f #:includes '("scaffold/tests") #:exit 42)

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
 (cut add-scaffold-test <> #:libc mini-libc-mes.hex2)
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
 (cut add-scaffold-test <> #:libc mini-libc-mes.hex2)
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
   "65-read"
   "66-local-char-array"))

(add-target (group "check-scaffold-tests/6" #:dependencies (filter (target-prefix? "check-scaffold/tests/6") %targets)))

;; tests/70: and beyond src/mes.c -- building up to 8cc.c, pcc.c, tcc.c, libguile/eval.c
(for-each
 add-scaffold-test
 '("70-printf"
   "71-struct-array"
   "72-typedef-struct-def"
   "73-union"
   "74-multi-line-string"
   "75-struct-union"
   "76-pointer-arithmetic"
   "77-pointer-assign"
   "78-union-struct"
   "79-int-array"
   "7a-struct-char-array"
   "7b-struct-int-array"
   "7c-dynarray"
   "7d-cast-char"
   "7e-struct-array-access"
   "7f-struct-pointer-arithmetic"
   "7g-struct-byte-word-field"
   "7h-struct-assign"
   "7i-struct-struct"
   "7j-strtoull"
   "7k-for-each-elem"
   "7l-struct-any-size-array"
   "7m-struct-char-array-assign"
   "7n-struct-struct-array"))

(add-target (group "check-scaffold-tests/7" #:dependencies (filter (target-prefix? "check-scaffold/tests/7") %targets)))

(add-target (group "check-scaffold-tests" #:dependencies (filter (target-prefix? "check-scaffold/tests") %targets)))

;; tests/80: and beyond tinycc; building GNU GCC and dependencies
(for-each
 (cut add-scaffold-test <> #:libc libc+tcc-mes.hex2 #:libc-gcc libc+tcc-gcc.mlibc-o)
 '("80-setjmp"))

(add-target (group "check-scaffold-tests/8" #:dependencies (filter (target-prefix? "check-scaffold/tests/8") %targets)))

(add-target (group "check-scaffold-tests" #:dependencies (filter (target-prefix? "check-scaffold/tests") %targets)))

(add-target (cpp.mescc "lib/libc+tcc-mes.c"))
(add-target (compile.mescc "lib/libc+tcc-mes.c"))

(define* (add-tcc-test name)
  (add-target (bin.gcc (string-append "scaffold/tinycc/" name ".c") #:libc libc-gcc.mlibc-o #:includes '("scaffold/tinycc")))
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
   "45_empty_for"           ; unsupported
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

(add-target (bin.gcc "scaffold/main.c"))
(add-target (check "scaffold/main.gcc" #:exit 42))

(add-target (bin.gcc "scaffold/main.c" #:libc #f))
(add-target (check "scaffold/main.mlibc-gcc" #:exit 42))

(add-target (bin.mescc "scaffold/main.c" #:libc mini-libc-mes.hex2))
(add-target (check "scaffold/main.mini-guile" #:exit 42))

(add-target (bin.mescc "scaffold/main.c"))
(add-target (check "scaffold/main.guile" #:exit 42))


(add-target (bin.gcc "scaffold/hello.c"))
(add-target (check "scaffold/hello.gcc" #:exit 42))

(add-target (bin.gcc "scaffold/hello.c" #:libc libc-gcc.mlibc-o))
(add-target (check "scaffold/hello.mlibc-gcc" #:exit 42))

(add-target (bin.mescc "scaffold/hello.c" #:libc mini-libc-mes.hex2))
(add-target (check "scaffold/hello.mini-guile" #:exit 42))

(add-target (bin.mescc "scaffold/hello.c"))
(add-target (check "scaffold/hello.guile" #:exit 42))


(add-target (bin.gcc "scaffold/m.c"))
(add-target (check "scaffold/m.gcc" #:exit 255))

(add-target (bin.gcc "scaffold/m.c" #:libc libc-gcc.mlibc-o))
(add-target (check "scaffold/m.mlibc-gcc" #:exit 255))

(add-target (bin.mescc "scaffold/m.c"))
(add-target (check "scaffold/m.guile" #:exit 255))

(add-target (bin.gcc "scaffold/micro-mes.c" #:libc libc-gcc.mlibc-o))
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
                     #:defines `("MES_C_READER=1"
                                 "MES_FIXED_PRIMITIVES=1"
                                 "MES_FULL=1"
                                 "POSIX=1"
                                 ,(string-append "VERSION=\"" %version "\"")
                                 ,(string-append "MODULEDIR=\"" (string-append %prefix (if (string-null? %prefix) "" "/") %moduledir "/") "\"")
                                 ,(string-append "PREFIX=\"" %prefix "\""))
                     #:includes '("src")))

(add-target (bin.gcc "src/mes.c" #:libc libc-gcc.mlibc-o
                     #:dependencies mes-snarf-targets
                     #:defines `("MES_C_READER=1"
                                 "MES_FIXED_PRIMITIVES=1"
                                 "MES_FULL=1"
                                 ,(string-append "VERSION=\"" %version "\"")
                                 ,(string-append "MODULEDIR=\"" (string-append %prefix (if (string-null? %prefix) "" "/") "/" %moduledir "/") "\"")
                                 ,(string-append "PREFIX=\"" %prefix "\""))
                     #:includes '("src")))

(add-target (bin.mescc "src/mes.c" #:dependencies mes-snarf-targets
                       #:defines `("MES_C_READER=1"
                                   "MES_FIXED_PRIMITIVES=1"
                                   "MES_FULL=1"
                                   ,(string-append "VERSION=\"" %version "\"")
                                   ,(string-append "MODULEDIR=\"" (string-append %prefix (if (string-null? %prefix) "" "/") %moduledir "/") "\"")
                                   ,(string-append "PREFIX=\"" %prefix "\""))
                       #:includes '("src")))

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

(define (add-guile-test o)
  (add-target (target (file-name o)))
  (add-target (check o)))

(define (add-mes.gcc-test o)
  (add-target (target (file-name o)))
  (add-target (check o #:dependencies (list (get-target "src/mes.mlibc-gcc")))))

(define (add-mes.guile-test o)
  (add-target (target (file-name o)))
  (add-target (check o #:dependencies (list (get-target "src/mes.guile")))))

(for-each add-guile-test (map (cut string-append <> "-guile") mes-tests))

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
   "module/nyacc/lang/c99/pprint.mes"
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

(add-target (install "lib/crt1.hex2" #:dir "lib"))
(add-target (install "lib/libc-mes.M1" #:dir "lib"))
(add-target (install "lib/libc-mes.hex2" #:dir "lib"))
(add-target (install "lib/libc+tcc-mes.M1" #:dir "lib"))
(add-target (install "lib/libc+tcc-mes.hex2" #:dir "lib"))
(add-target (install "lib/mini-libc-mes.M1" #:dir "lib"))
(add-target (install "lib/mini-libc-mes.hex2" #:dir "lib"))

(add-target (install "lib/crt1.mlibc-o" #:dir "lib"))
(add-target (install "lib/libc-gcc.mlibc-o" #:dir "lib"))
(add-target (install "lib/libc+tcc-gcc.mlibc-o" #:dir "lib"))

(for-each
 (lambda (f)
   ((install-dir #:dir "share/") f))
 '("include/alloca.h"
   "include/assert.h"
   "include/ctype.h"
   "include/dlfcn.h"
   "include/errno.h"
   "include/fcntl.h"
   "include/features.h"
   "include/inttypes.h"
   "include/libgen.h"
   "include/limits.h"
   "include/locale.h"
   "include/math.h"
   "include/mlibc.h"
   "include/setjmp.h"
   "include/signal.h"
   "include/stdarg.h"
   "include/stdbool.h"
   "include/stdint.h"
   "include/stdio.h"
   "include/stdlib.h"
   "include/stdnoreturn.h"
   "include/string.h"
   "include/strings.h"
   "include/sys/cdefs.h"
   "include/sys/mman.h"
   "include/sys/stat.h"
   "include/sys/time.h"
   "include/sys/timeb.h"
   "include/sys/types.h"
   "include/sys/ucontext.h"
   "include/sys/wait.h"
   "include/time.h"
   "include/unistd.h"))

(for-each
 (compose add-target (cut install <> #:dir "share/doc/mes"))
 '("AUTHORS"
   ;;"ChangeLog"
   "BOOTSTRAP"
   "COPYING"
   "HACKING"
   "INSTALL"
   "NEWS"
   "README"
   "doc/ANNOUNCE-0.11"))

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
                          (() (filter (conjoin (negate install-target?)
                                               (negate check-target?))
                                      %targets))
                          ((? (cut member "all" <>)) (filter (conjoin (negate install-target?)
                                                                      (negate check-target?))
                                                             %targets))
                          ((? (cut member "check" <>)) (filter check-target? %targets))
                          ((? (cut member "install" <>)) (filter install-target? %targets))
                          (_ (filter-map (cut get-target <>) args)))))
           ;;((@@ (guix make) store) #:print 0)
           (for-each build targets)
           (exit %status)))))

(main (cdr (command-line)))
