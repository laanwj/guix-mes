;;; -*-scheme-*-

;;; Mes --- Maxwell Equations of Software
;;; Copyright © 2016,2017 Jan Nieuwenhuizen <janneke@gnu.org>
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

;;; Code:

(define-module (mes hex2)
  #:use-module (srfi srfi-1)
  #:use-module (system base pmatch)
  #:use-module (mes elf-util)
  #:use-module (mes elf)
  #:export (object->hex2
            objects->hex2
            objects->elf))

(cond-expand
 (guile-2)
 (guile
  (use-modules (ice-9 syncase)))
 (mes))

(include-from-path "mes/hex2.mes")
