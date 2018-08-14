;;; GNU Mes --- Maxwell Equations of Software
;;; Copyright © 2018 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
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

;;; Commentary:

;;; Define x86_64 M1 assembly

;;; Code:

(define-module (mescc x86_64 as)
  #:use-module (mes guile)
  #:use-module (mescc as)
  #:use-module (mescc info)
  #:export (
            x86_64:instructions
            ))

(define (x86_64:function-preamble . rest)
  '(("push___%rbp")
    ("mov____%rsp,%rbp")
    ;;("mov____%rdi,0x8(%rbp)" "!-0x08")
    ;;("mov____%rsi,0x8(%rbp)" "!-0x10")
    ;;("mov____%rdx,0x8(%rbp)" "!-0x18")
    ;;("mov____%rcx,0x8(%rbp)" "!-0x20")
    ))

(define (x86_64:function-locals . rest)
  `(
    ;; FIXME: how on x86_64?
    ("sub____$i32,%rsp" (#:immediate ,(+ (* 4 1025) (* 20 8))))
    )) ; 4*1024 buf, 20 local vars

(define (x86_64:r0->local info n)
  (or n (error "invalid value: x86_64:r0->local: " n))
  (let ((r0 (car (if (pair? (.allocated info)) (.allocated info) (.registers info))))
        (n (- 0 (* 8 n))))
    `(,(if (< (abs n) #x80) `(,(string-append "mov____%" r0 ",0x8(%rbp)") (#:immediate1 ,n))
           `(,(string-append "mov____%" r0 ",0x32(%rbp)") (#:immediate ,n))))))

(define (x86_64:value->r0 info v)
  (or v (error "invalid value: x86_64:value->r0: " v))
  (let ((r0 (car (if (pair? (.allocated info)) (.allocated info) (.registers info)))))
    `((,(string-append "mov____$i32,%" r0) (#:immediate ,v)))))

(define (x86_64:ret . rest)
  '(("mov____%rbp,%rsp")
    ("pop____%rbp")
    ("ret")))

(define (x86_64:r0-zero? info)
  (let ((r0 (car (if (pair? (.allocated info)) (.allocated info) (.registers info)))))
    `((,(string-append "test___%" r0 "," "%" r0)))))

(define (x86_64:local->r0 info n)
  (or n (error "invalid value: x86_64:local->r0: " n))
  (let ((r0 (car (if (pair? (.allocated info)) (.allocated info) (.registers info))))
        (n (- 0 (* 8 n))))
    `(,(if (< (abs n) #x80) `(,(string-append "mov____0x8(%rbp),%" r0) (#:immediate1 ,n))
           `(,(string-append "mov____0x32(%rbp),%" r0) (#:immediate ,n))))))

(define (x86_64:call-label info label n)
  `((call32 (#:offset ,label))
    ;;("add____$i8,%esp" (#:immediate1 ,(* n 4)))
    ))

(define x86_64:instructions
  `(
    (call-label . ,x86_64:call-label)
    (function-preamble . ,x86_64:function-preamble)
    (function-locals . ,x86_64:function-locals)
    (local->r0 . ,x86_64:local->r0)
    (r0->local . ,x86_64:r0->local)
    (r0-zero? . ,x86_64:r0-zero?)
    (ret . ,x86_64:ret)
    (value->r0 . ,x86_64:value->r0)
    ))
