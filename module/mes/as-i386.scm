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
  #:use-module (mes as)
  #:export (
            i386:accu-not
            i386:accu-cmp-value
            i386:accu->base
            i386:accu->base-address
            i386:accu->base-address+n
            i386:accu->label
            i386:accu->local
            i386:accu-test
            i386:accu-zero?
            i386:accu+accu
            i386:accu+base
            i386:accu+value
            i386:accu/base
            i386:accu%base
            i386:accu*base
            i386:accu-base
            i386:accu-shl
            i386:accu-and-base
            i386:accu-or-base
            i386:accu-xor-base
            i386:accu<<base
            i386:accu>>base
            i386:base-sub
            i386:base->accu
            i386:base->accu-address
            i386:byte-accu->base-address
            i386:base->label
            i386:base->local
            i386:base-mem->accu
            i386:byte-base-sub
            i386:byte-base->accu-address
            i386:byte-base->accu-address+n
            i386:byte-base-mem->accu
            i386:local-address->accu
            i386:byte-local->accu
            i386:byte-local->base
            i386:byte-mem->accu
            i386:base-mem+n->accu
            i386:byte-mem->base
            i386:byte-test-base
            i386:byte-sub-base
            i386:call-accu
            i386:call-label
            i386:formal
            i386:function-locals
            i386:function-preamble
            i386:label-mem-add
            i386:label->accu
            i386:label->base
            i386:label-mem->accu
            i386:label-mem->base
            i386:jump
            i386:jump
            i386:jump-byte-z
            i386:jump-g
            i386:jump-ge
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
            i386:accu-mem-add
            i386:mem->accu
            i386:mem+n->accu
            i386:pop-accu
            i386:push-accu
            i386:pop-base
            i386:push-base
            i386:push-label
            i386:push-label-mem
            i386:push-local
            i386:push-byte-local-de-ref
            i386:push-byte-local-de-de-ref
            i386:push-local-de-ref
            i386:push-local-address
            i386:ret
            i386:ret-local
            i386:sub-base
            i386:test-base
            i386:value->accu
            i386:value->accu-address
            i386:value->accu-address+n
            i386:value->label
            i386:value->local
            i386:value->base
            i386:xor-accu
            i386:xor-zf
            i386:accu+n
            i386:base+n
            i386:base-address->accu-address
            i386:nz->accu
            i386:z->accu
            i386:accu<->stack
            ))

(include-from-path "mes/as-i386.mes")
