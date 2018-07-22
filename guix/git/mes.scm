;;; GNU Mes --- Maxwell Equations of Software
;;; Copyright © 2016,2017,2018 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
;;;
;;; This file is part of GNU Mes.
;;;
;;; Also borrowing code from:
;;; guile-sdl2 --- FFI bindings for SDL2
;;; Copyright © 2015 David Thompson <davet@gnu.org>
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

(define-module (git mes)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 match)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 rdelim)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages commencement)
  #:use-module (gnu packages cross-base)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages graphviz)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages man)
  #:use-module (gnu packages mes)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages texinfo)
  #:use-module ((guix build utils) #:select (with-directory-excursion))
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system trivial)
  #:use-module (guix gexp)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix licenses)
  #:use-module (guix packages))

(define %source-dir (getcwd))

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

(define-public nyacc-for-mes
  (package
    (inherit nyacc)
    (version "0.80.42")
      (source (origin
                (method url-fetch)
                (uri (string-append "https://gitlab.com/janneke/nyacc"
                                    "/-/archive/v" version
                                    "/nyacc-" version ".tar.gz"))
                (sha256
                 (base32
                  "0c8c8kxir0h2d4nxr131xbkfs7c80haipmkp2g6677sh14wn0b3y"))))))

(define-public mescc-tools
  (package
    (name "mescc-tools")
    (version "0.5.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/oriansj/mescc-tools/archive/Release_"
                    version
                    ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0rsxbjc3bg0jl3h7ai4hndxx2iyyk8bvwj9nd3xv2vgz3bmypnah"))))
    (build-system gnu-build-system)
    (supported-systems '("i686-linux" "x86_64-linux"))
    (arguments
     `(#:make-flags (list (string-append "PREFIX=" (assoc-ref %outputs "out")))
       #:test-target "test"
       #:phases (modify-phases %standard-phases
                  (delete 'configure))))
    (synopsis "Tools for the full source bootstrapping process")
    (description
     "Mescc-tools is a collection of tools for use in a full source
bootstrapping process.  Currently consists of the M1 macro assembler and the
hex2 linker.")
    (home-page "https://github.com/oriansj/mescc-tools")
    (license gpl3+)))

(define-public mes
  (let ((triplet "i686-unknown-linux-gnu")
        (version "0.16.1"))
    (package
      (name "mes")
      (version version)
      (source (origin
                (method url-fetch)
                (uri (string-append
                      "https://alpha.gnu.org/pub/gnu/mes/mes-" version ".tar.gz"))
                (sha256
                 (base32 #!mes!# "171bwanlnvwy406i5s0a6806iffcdz086njk8wbhgrc33n6jr8ir"))))
      (build-system gnu-build-system)
      (supported-systems '("i686-linux" "x86_64-linux"))
      (propagated-inputs
       `(("mescc-tools" ,mescc-tools)
         ("nyacc" ,nyacc-for-mes)))
      (native-inputs
       `(("git" ,git)
         ("guile" ,guile-2.2)
         ,@(if (string-prefix? "x86_64-linux" (or (%current-target-system)
                                                  (%current-system)))
               ;; Use cross-compiler rather than #:system "i686-linux" to get
               ;; MesCC 64 bit .go files installed ready for use with Guile.
               `(("i686-linux-binutils" ,(cross-binutils triplet))
                 ("i686-linux-gcc" ,(cross-gcc triplet)))
               '())
         ("graphviz" ,graphviz)
         ("help2man" ,help2man)
         ("perl" ,perl)                ; build-aux/gitlog-to-changelog
         ("texinfo" ,texinfo)))
      (arguments
       `(#:phases
         (modify-phases %standard-phases
           (add-before 'build 'make-git-source-writable
           (lambda* (#:key outputs #:allow-other-keys)
             (for-each make-file-writable
                       (find-files "." ".*\\.M1"))))
           (delete 'strip)))) ; binutil's strip b0rkes Mescc/M1/hex2 binaries
      (synopsis "Scheme interpreter and C compiler for full source bootstrapping")
      (description
       "GNU Mes [Maxwell Equations of Software] aims to create full source
bootstrapping for GuixSD.  It consists of a mutual self-hosting [close to
Guile-] Scheme interpreter prototype in C and a Nyacc-based C compiler in
[Guile] Scheme.")
      (home-page "https://www.gnu.org/software/mes")
      (license gpl3+))))

(define-public mes.git
 (let ((version "0.16.1")
        (revision "0")
        (commit (read-string (open-pipe "git show HEAD | head -1 | cut -d ' ' -f 2" OPEN_READ))))
    (package
      (inherit mes)
      (name "mes.git")
      (version (string-append version "-" revision "." (string-take commit 7)))
      (source (local-file %source-dir #:recursive? #t #:select? git-file?)))))
