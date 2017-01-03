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

;;; libc-i386.mes defines C library routines

;;; Code:

(define-module (mes libc-i386)
  #:use-module (srfi srfi-1)
  #:use-module (mes elf)
  #:export (i386:accu->local
            i386:call
            i386:exit
            i386:formal
            i386:function-preamble
            i386:function-locals
            i386:eputs
            i386:jump
            i386:local-add
            i386:local-assign
            i386:local->accu
            i386:local->base
            i386:mem-byte->accu
            i386:push-accu
            i386:puts
            i386:ref-global
            i386:ref-local
            i386:ret
            i386:ret-local
            i386:test-jump
            i386:write
            ))

(cond-expand
 (guile-2)
 (guile
  (use-modules (ice-9 syncase)))
 (mes))

(include-from-path "mes/libc-i386.mes")
