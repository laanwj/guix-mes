;;; guix.scm -- Guix package definition

;;; Mes --- Maxwell Equations of Software
;;; Copyright © 2016 Jan Nieuwenhuizen <janneke@gnu.org>
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

(use-modules (gnu packages)
             (gnu packages base)
             (gnu packages guile)
             (gnu packages package-management)
             (gnu packages perl)
             (gnu packages version-control)
             (guix git-download)
             (guix licenses)
             (guix packages)
             (guix build-system gnu))

(define-public mes
  (package
    (name "mes")
    (version "0.4.f84e97fc")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://gitlab.com/janneke/mes")
                    (commit "f84e97fc33f5e2a2ad7033795967d44c95d34b8f")))
              (file-name (string-append name "-" version))
              (sha256
               (base32 "1jpm8m8y2dqsl3sc6flf8da4rpdrqh6zgr2mghzjw0lg34v1r21j"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("git" ,git)
       ("guile" ,guile-2.0)
       ("perl" ,perl)))                ; build-aux/gitlog-to-changelog
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'install 'generate-changelog
           (lambda _
             (with-output-to-file "ChangeLog"
               (lambda ()
                 (display "Please run\n  build-aux/gitlog-to-changelog --srcdir=<git-checkout> > ChangeLog\n")))
             #t)))))
    (synopsis "Maxwell Equations of Software")
    (description
     "Mes aims to create an entirely source-based bootstrapping path.
The target is to [have GuixSD] boostrap from a minimal, easily
inspectable binary --that should be readable as source-- into
something close to R6RS Scheme.")
    (home-page "https://gitlab.com/janneke/mes")
    (license gpl3+)))

;; Return it here so 'guix build/environment/package' can consume it directly.
mes
