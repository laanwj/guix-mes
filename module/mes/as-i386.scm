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

;;; as-i386.scm defines i386 assembly

;;; Code:

(define-module (mes as-i386)
  #:use-module (mes guile)
  #:use-module (mes as)
  #:export (
            i386:accu%base
            i386:accu*base
            i386:accu+accu
            i386:accu+base
            i386:accu+value
            i386:accu->base
            i386:accu->base-mem
            i386:byte-accu->base-mem
            i386:word-accu->base-mem
            i386:accu->base-mem+n
            i386:byte-accu->base-mem+n
            i386:word-accu->base-mem+n
            i386:accu->label
            i386:accu->local
            i386:accu-and-base
            i386:accu-base
            i386:accu-cmp-value
            i386:accu-mem-add
            i386:accu-mem->base-mem
            i386:accu-negate
            i386:accu-not
            i386:accu-or-base
            i386:accu-shl
            i386:accu-test
            i386:accu-xor-base
            i386:accu-zero?
            i386:accu/base
            i386:accu<->stack
            i386:accu<<base
            i386:accu>>base
            i386:base+value
            i386:base->accu
            i386:base->accu-mem
            i386:base->label
            i386:base->local
            i386:base-mem->accu-mem
            i386:base-mem+n->accu
            i386:base-mem->accu
            i386:base-sub
            i386:byte-accu->base-mem
            i386:byte-base->accu-mem
            i386:byte-base->accu-mem+n
            i386:byte-base-mem->accu
            i386:byte-base-sub
            i386:byte-local->accu
            i386:byte-local->base
            i386:byte-mem->accu
            i386:byte-mem->base
            i386:byte-sub-base
            i386:byte-test-base
            i386:call-accu
            i386:call-label
            i386:formal
            i386:function-locals
            i386:function-preamble
            i386:jump
            i386:jump
            i386:jump-byte-z
            i386:jump-g
            i386:jump-ge
            i386:jump-l
            i386:jump-le
            i386:jump-nz
            i386:jump-z
            i386:label->accu
            i386:label->base
            i386:label-mem->accu
            i386:label-mem->base
            i386:label-mem-add
            i386:local->accu
            i386:local->base
            i386:local-add
            i386:local-address->accu
            i386:local-address->accu
            i386:local-address->base
            i386:local-ptr->accu
            i386:local-ptr->base
            i386:local-test
            i386:mem+n->accu
            i386:byte-mem+n->accu
            i386:word-mem+n->accu
            i386:mem->accu
            i386:mem->base
            i386:nop
            i386:nz->accu
            i386:pop-accu
            i386:pop-base
            i386:push-accu
            i386:push-base
            i386:push-byte-local-de-de-ref
            i386:push-byte-local-de-ref
            i386:push-label
            i386:push-label-mem
            i386:push-local
            i386:push-local-address
            i386:push-local-de-ref
            i386:ret
            i386:ret-local
            i386:sub-base
            i386:test-base
            i386:value->accu
            i386:value->accu-mem
            i386:value->accu-mem+n
            i386:value->base
            i386:value->label
            i386:value->local
            i386:xor-accu
            i386:xor-zf
            i386:g?->accu
            i386:ge?->accu
            i386:l?->accu
            i386:le?->accu
            i386:z->accu
            ))

(include-from-path "mes/as-i386.mes")
