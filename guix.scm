;;; guix.scm -- Guix package definition

;;; Mes --- Maxwell Equations of Software
;;; Copyright © 2016,2017,2018 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>

;;; Also borrowing code from:
;;; guile-sdl2 --- FFI bindings for SDL2
;;; Copyright © 2015 David Thompson <davet@gnu.org>

;;;
;;; guix.scm: This file is part of Mes.
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
;;
;; GNU Guix development package.  To build and install, run:
;;
;;   guix package -f guix.scm
;;
;; To build it, but not install it, run:
;;
;;   guix build -f guix.scm
;;
;; To use as the basis for a development environment, run:
;;
;;   guix environment -l guix.scm
;;
;;; Code:

(use-modules (srfi srfi-1)
             (srfi srfi-26)
             (ice-9 match)
             (ice-9 popen)
             (ice-9 rdelim)
             (gnu packages)
             (gnu packages base)
             (gnu packages commencement)
             (gnu packages cross-base)
             (gnu packages elf)
             (gnu packages gcc)
             (gnu packages guile)
             (gnu packages mes)
             (gnu packages package-management)
             (gnu packages perl)
             ((guix build utils) #:select (with-directory-excursion))
             (guix build-system gnu)
             (guix build-system trivial)
             (guix gexp)
             (guix download)
             (guix git-download)
             (guix licenses)
             (guix packages))

(define %source-dir (dirname (current-filename)))

(define git-file?
  (let* ((pipe (with-directory-excursion %source-dir
                 (open-pipe* OPEN_READ "git" "ls-files")))
         (files (let loop ((lines '()))
                  (match (read-line pipe)
                    ((? eof-object?)
                     (reverse lines))
                    (line
                     (loop (cons line lines))))))
         (status (close-pipe pipe)))
    (lambda (file stat)
      (match (stat:type stat)
        ('directory #t)
        ((or 'regular 'symlink)
         (any (cut string-suffix? <> file) files))
        (_ #f)))))

(define-public nyacc
  (package
    (name "nyacc")
    (version "0.82.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://savannah/nyacc/"
                                  name "-" version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0ykz64jlf1kpxz3qqr0nmci57r5yqwyd3s2g93vrmcnpy9d7y22p"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("guile" ,guile-2.2)))
    (synopsis "LALR(1) Parser Generator in Guile")
    (description
     "NYACC is an LALR(1) parser generator implemented in Guile.
The syntax and nomenclature should be considered not stable.  It comes with
extensive examples, including parsers for the Javascript and C99 languages.")
    (home-page "https://savannah.nongnu.org/projects/nyacc")
    (license (list gpl3+ lgpl3+))))

(define-public nyacc
  (package
    (name "nyacc")
    (version "0.80.42")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://gitlab.com/janneke/nyacc"
                                  "/repository/archive.tar.gz?ref=v"
                                  version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "101k3hy4jk5p109k6w4dpx3bjm0g53zwb1yxvvad8khfq00wb8hd"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("guile" ,guile-2.2)))
    (synopsis "LALR(1) Parser Generator in Guile")
    (description
     "NYACC is an LALR(1) parser generator implemented in Guile.
The syntax and nomenclature should be considered not stable.  It comes with
extensive examples, including parsers for the Javascript and C99 languages.")
    (home-page "https://savannah.nongnu.org/projects/nyacc")
    (license (list gpl3+ lgpl3+))))

(define-public mescc-tools
  (package
    (name "mescc-tools")
    (version "0.4")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/oriansj/mescc-tools/archive/Release_"
                    version
                    ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1iwc8xqwzdaqckb4jkkisljrgn8ii4bl7dzk1l2kpv98hsyq9vi1"))))
    (build-system gnu-build-system)
    (supported-systems '("i686-linux" "x86_64-linux"))
    (arguments
     `(#:make-flags (list (string-append "PREFIX=" (assoc-ref %outputs "out")))
       #:test-target "test"
       #:phases (modify-phases %standard-phases
                  (delete 'configure)
                  (add-after 'install 'install-2
                    (lambda _
                      (let ((out (assoc-ref %outputs "out")))
                       (copy-file "bin/blood-elf" (string-append out "/bin/blood-elf"))))))))
    (synopsis "Tools for the full source bootstrapping process")
    (description
     "Mescc-tools is a collection of tools for use in a full source
bootstrapping process.  Currently consists of the M1 macro assembler and the
hex2 linker.")
    (home-page "https://github.com/oriansj/mescc-tools")
    (license gpl3+)))

(define-public mes
  (let ((commit "0330aab202188306aa7676badfa3edafab707403")
        (revision "0")
        (triplet "i686-unknown-linux-gnu")
        (version "0.12"))
    (package
      (name "mes")
      (version (string-append version "-" revision "." (string-take commit 7)))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://gitlab.com/janneke/mes")
                      (commit commit)))
                (file-name (string-append name "-" version))
                (sha256
                 (base32 "1jifgmvgds8fxxv0zkna66f2p9cn46ych2qwn9p04088hdvkknwk"))))
      (build-system gnu-build-system)
      (supported-systems '("i686-linux" "x86_64-linux"))
      (propagated-inputs
       `(("mescc-tools" ,mescc-tools)
         ("nyacc" ,nyacc)))
      (native-inputs
       `(("guile" ,guile-2.2)
         ,@(if (string-prefix? "x86_64-linux" (or (%current-target-system)
                                                  (%current-system)))
               ;; Use cross-compiler rather than #:system "i686-linux" to get
               ;; MesCC 64 bit .go files installed ready for use with Guile.
               `(("i686-linux-binutils" ,(cross-binutils triplet))
                 ("patchelf" ,patchelf)
                 ("i686-linux-libc" ,(cross-libc triplet)) ;; FIXME: for debugging only
                 ("i686-linux-gcc" ,(cross-gcc triplet)))
               '())
         ("perl" ,perl)))               ;build-aux/gitlog-to-changelog
      (arguments
       `(#:phases
         (modify-phases %standard-phases
           (add-before 'build 'make-git-source-writable
           (lambda* (#:key outputs #:allow-other-keys)
             (for-each make-file-writable
                       (find-files "." ".*\\.M1"))))
           (add-before 'install 'generate-changelog
             (lambda _
               (with-output-to-file "ChangeLog"
                 (lambda ()
                   (display "Please run
    build-aux/gitlog-to-changelog --srcdir=<git-checkout> > ChangeLog\n")))
               #t))
           (delete 'strip)))) ; binutil's strip b0rkes Mescc/M1/hex2 binaries
      (synopsis "Scheme interpreter and C compiler for full source bootstrapping")
      (description
       "Mes [Maxwell Equations of Software] aims to create full source
bootstrapping for GuixSD.  It consists of a mutual self-hosting [close to
Guile-] Scheme interpreter prototype in C and a Nyacc-based C compiler in
[Guile] Scheme.")
      (home-page "https://gitlab.com/janneke/mes")
      (license gpl3+))))

(define-public mes.git
 (let ((version "0.12")
        (revision "0")
        (commit (read-string (open-pipe "git show HEAD | head -1 | cut -d ' ' -f 2" OPEN_READ))))
    (package
      (inherit mes)
      (name "mes.git")
      (version (string-append version "-" revision "." (string-take commit 7)))
      (source (local-file %source-dir #:recursive? #t #:select? git-file?)))))

;; Return it here so `guix build/environment/package' can consume it directly.
mes.git
