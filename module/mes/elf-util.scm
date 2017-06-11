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

(define-module (mes elf-util)
  #:use-module (srfi srfi-1)
  #:use-module (mes bytevectors)
  #:export (data-offset
            dec->hex
            function-offset
            int->bv16
            int->bv32
            label-offset
            functions->lambdas
            functions->text
            lambda/label->list
            text->list
            globals->data
            make-global
            global:type
            global:pointer
            global:value))

(cond-expand
 (guile-2)
 (guile
  (use-modules (ice-9 syncase)))
 (mes))

(include-from-path "mes/elf-util.mes")
