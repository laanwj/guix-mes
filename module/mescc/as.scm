;;; GNU Mes --- Maxwell Equations of Software
;;; Copyright © 2016,2017,2018 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
;;;
;;; This file is part of GNU Mes.
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

(define-module (mescc as)
  #:use-module (srfi srfi-1)
  #:use-module (mes guile)
  #:use-module (mescc bytevectors)
  #:use-module (mescc info)
  #:export (as
            dec->hex
            int->bv8
            int->bv16
            int->bv32))

(define (int->bv32 value)
  (let ((bv (make-bytevector 4)))
    (bytevector-u32-native-set! bv 0 value)
    bv))

(define (int->bv16 value)
  (let ((bv (make-bytevector 2)))
    (bytevector-u16-native-set! bv 0 value)
    bv))

(define (int->bv8 value)
  (let ((bv (make-bytevector 1)))
    (bytevector-u8-set! bv 0 value)
    bv))

(define (dec->hex o)
  (cond ((number? o) (number->string o 16))
        ((char? o) (number->string (char->integer o) 16))
        (else (format #f "~s" o))))

(define (as info instruction . rest)
  (let ((proc (assoc-ref (.instructions info) instruction)))
    (apply proc info rest)))
