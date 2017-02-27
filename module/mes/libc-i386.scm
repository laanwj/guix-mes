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
  #:export (
            i386:accu-not
            i386:accu-cmp-value
            i386:accu->base
            i386:accu->base-address
            i386:accu->base-address+n
            i386:accu->global
            i386:accu->global-address
            i386:accu->local
            i386:accu-non-zero?
            i386:accu-test
            i386:accu-zero?
            i386:accu+accu
            i386:accu+base
            i386:accu+value
            i386:accu/base
            i386:accu-base
            i386:accu-shl
            i386:base-sub
            i386:base->accu
            i386:base->accu-address
            i386:byte-accu->base-address
            i386:base->global
            i386:base->local
            i386:base-mem->accu
            i386:byte-base-sub
            i386:byte-base->accu-address
            i386:byte-base->accu-address+n
            i386:byte-base-mem->accu
            i386:local-address->accu
            i386:byte-local->accu
            i386:byte-mem->accu
            i386:base-mem+n->accu
            i386:byte-mem->base
            i386:byte-test-base
            i386:byte-sub-base
            i386:call
            i386:call-accu
            i386:formal
            i386:function-locals
            i386:function-preamble
            i386:global-add
            i386:global->accu
            i386:global->base
            i386:global-address->accu
            i386:global-address->base
            i386:jump
            i386:jump
            i386:jump-byte-nz
            i386:jump-byte-z
            i386:jump-c
            i386:jump-cz
            i386:jump-le
            i386:jump-nc
            i386:jump-ncz
            i386:jump-nz
            i386:jump-z
            i386:local->accu
            i386:local->base
            i386:local-add
            i386:local-address->accu
            i386:local-ptr->accu
            i386:local-ptr->base
            i386:local-address->base
            i386:local-test
            i386:mem->accu
            i386:mem+n->accu
            i386:pop-accu
            i386:push-accu
            i386:pop-base
            i386:push-base
            i386:push-global
            i386:push-global-address
            i386:push-local
            i386:push-local-de-ref
            i386:push-local-address
            i386:ret
            i386:ret-local
            i386:sub-base
            i386:test-base
            i386:test-jump-z
            i386:value->accu
            i386:value->accu-address
            i386:value->accu-address+n
            i386:value->global
            i386:value->local
            i386:value->base
            i386:xor-accu
            i386:xor-zf

            ;; long jump
            i386:Xjump
            i386:Xjump
            i386:Xjump-c
            i386:Xjump-nc
            i386:Xjump-nz
            i386:Xjump-z

            i386:XXjump

            ;; libc
            i386:exit
            i386:open
            i386:read
            i386:write
            ))

(cond-expand
 (guile-2)
 (guile
  (use-modules (ice-9 syncase)))
 (mes))

(include-from-path "mes/libc-i386.mes")
