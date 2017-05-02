;;; guix.scm -- Guix package definition

;;; Mes --- Maxwell Equations of Software
;;; Copyright © 2016,2017 Jan Nieuwenhuizen <janneke@gnu.org>

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
             (gnu packages gcc)
             (gnu packages guile)
             (gnu packages package-management)
             (gnu packages perl)
             (gnu packages version-control)
             ((guix build utils) #:select (with-directory-excursion))
             (guix build-system gnu)
             (guix build-system trivial)
             (guix gexp)
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

(define-public mes
  (let ((triplet "i686-unknown-linux-gnu"))
    (package
      (name "mes")
      (version "0.5.4f2ccd17")
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://gitlab.com/janneke/mes")
                      (commit "4f2ccd170df32c0a2988c0886de69a9a2b71f224")))
                (file-name (string-append name "-" version))
                (sha256
                 (base32 "01m8n7zk4f1ryd61dj589zarx09vbi7fc5f8m1x5zfk6r7l0zja2"))))
      (build-system gnu-build-system)
      (supported-systems '("x86_64-linux"))
      (native-inputs
       `(("git" ,git)
         ("guile" ,guile-2.2)
         ("gcc" ,gcc-toolchain-4.9)
         ;; Use cross-compiler rather than #:system "i686-linux" to get
         ;; MesCC 64 bit .go files installed ready for use with Guile.
         ("i686-linux-binutils" ,(cross-binutils triplet))
         ("i686-linux-gcc" ,(let ((triplet triplet)) (cross-gcc triplet)))
         ("perl" ,perl)))        ; build-aux/gitlog-to-changelog
      (synopsis "Maxwell Equations of Software")
      (description
       "Mes aims to create full source bootstrapping for GuixSD.  It
consists of a mutual self-hosting [close to Guile-] Scheme interpreter
prototype in C and a Nyacc-based C compiler in [Guile] Scheme.")
      (home-page "https://gitlab.com/janneke/mes")
      (license gpl3+))))

(define-public mes.git
  (package
    (inherit mes)
    (name "mes.git")
    (version "git")
    (source (local-file %source-dir #:recursive? #t #:select? git-file?))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'install 'generate-changelog
                     (lambda _
                       (with-output-to-file "ChangeLog"
                         (lambda ()
                           (display "Please run
    build-aux/gitlog-to-changelog --srcdir=<git-checkout> > ChangeLog\n")))
                       #t)))))))

;; Return it here so `guix build/environment/package' can consume it directly.
mes.git
