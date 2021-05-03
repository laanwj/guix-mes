;;; GNU Mes --- Maxwell Equations of Software
;;; Copyright © 2018 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
;;; Copyright © 2021 W. J. van der Laan <laanwj@protonmail.com>
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

;;; Define riscv64 M1 assembly

;;; Code:

(define-module (mescc riscv64 as)
  #:use-module (mes guile)
  #:use-module (mescc as)
  #:use-module (mescc info)
  #:use-module (mescc riscv64 info)
  #:export (
            riscv64:instructions
            ))

;;; reserved temporary intermediate registers
; t6 is used internally by M1 sequences (and in riscv64:addi)
(define %intreg "t6")
; t4 and t5 are scratch registers for code generation here
(define %tmpreg1 "t5")
(define %tmpreg2 "t4")
; registers for condition flags emulation
(define %condregx "s10")
(define %condregy "s11")

;;; register for return values
(define %retreg "t0")

;;; internal: return instruction to load an intermediate value into a register
(define (riscv64:li r v)
  (cond
    ((and (>= v #x-800) (<= v #x7ff))
      `((#:immediate-field "I" ,v) ,(string-append "li_____%" r ",0")))
    ((and (>= v #x-80000000) (<= v #x7fffffff))
      `((#:immediate-field "U" ,v) ,(string-append "lui____%" r ",0")
        (#:immediate-field "I" ,v) ,(string-append "addi___%" r ",%" r ",0")))
    (else
      `(,(string-append "li_____%" r ",$i64") (#:immediate8 ,v)))))

;;; internal: return instruction to load value of label into a register
(define (riscv64:li-address r label)
  `((#:address-field "U" ,label) ,(string-append "lui____%" r ",0")
    (#:address-field "I" ,label) ,(string-append "addi___%" r ",%" r ",0"))) ;; FIXME 64bit

;;; internal: return instruction to add an intermediate value into a register
(define (riscv64:addi r0 r1 v)
  (cond
    ((= v 0)
      `(,(string-append "; addi___%" r0 ",%" r1 ",0"))) ; nothing to do
    ((and (>= v #x-800) (<= v #x7ff))
      `((#:immediate-field "I" ,v) ,(string-append "addi___%" r0 ",%" r1 ",0")))
    ((and (>= v #x-80000000) (<= v #x7fffffff))
      `((#:immediate-field "U" ,v) ,(string-append "lui____%" %intreg ",0")
        (#:immediate-field "I" ,v) ,(string-append "addi___%" %intreg ",%" %intreg ",0")
        ,(string-append "add____%" r0 ",%" r1 ",%" %intreg)))
    (else
      `(,(string-append "addi___%" r0 ",%" r1 ",$i64") (#:immediate8 ,v)))))

;;; internal: load b/h/w/d from fp+n
(define (riscv64:local+n->x-r x n r)
  (if (and (>= n #x-800) (<= n #x7ff))
    `(((#:immediate-field "I" ,n) ,(string-append "l" x "_____%" r ",0(%fp)")))
    `(,(riscv64:addi %tmpreg1 "fp" n)
      (,(string-append "l" x "_____%" r ",0(%" %tmpreg1 ")")))))

;;; internal: store b/h/w/d to fp+n
(define (riscv64:x-r->local+n x n r)
  (if (and (>= n #x-800) (<= n #x7ff))
    `(((#:immediate-field "S" ,n) ,(string-append "s" x "_____%" r ",0(%fp)")))
    `(,(riscv64:addi %tmpreg1 "fp" n)
      (,(string-append "s" x "_____%" r ",0(%" %tmpreg1 ")")))))

;;; the preamble of every function
(define (riscv64:function-preamble info . rest)
  `(((#:immediate-field "I" -16) "addi___%sp,%sp,0")
    ((#:immediate-field "S" 8) "sd_____%ra,0(%sp)")
    ((#:immediate-field "S" 0) "sd_____%fp,0(%sp)")
    ("mv_____%fp,%sp")))

;;; allocate function locals
(define (riscv64:function-locals . rest)
  `(
    ,(riscv64:addi "sp" "sp" (- (+ (* 4 1025) (* 20 8))))
    )) ; 4*1024 buf, 20 local vars

;;; immediate value to register
(define (riscv64:value->r info v)
  (or v (error "invalid value: riscv64:value->r: " v))
  (let ((r (get-r info)))
    `(,(riscv64:li r v))))

;;; assign immediate value to r0
(define (riscv64:value->r0 info v)
  (let ((r0 (get-r0 info)))
    `(,(riscv64:li r0 v))))

;;; function epilogue
(define (riscv64:ret . rest)
  '(("mv_____%sp,%fp")
    ((#:immediate-field "I" 0) "ld_____%fp,0(%sp)")
    ((#:immediate-field "I" 8) "ld_____%ra,0(%sp)")
    ((#:immediate-field "I" 16) "addi___%sp,%sp,0")
    ("ret")))

;;; stack local to register
(define (riscv64:local->r info n)
  (let ((r (car (if (pair? (.allocated info)) (.allocated info) (.registers info))))
        (n (- 0 (* 8 n))))
    (riscv64:local+n->x-r "d" n r)))

;;; call a function through a label
(define (riscv64:call-label info label n)
  `(((#:offset-field "J" ,label) "jal____0")
    ,(riscv64:addi "sp" "sp" (* n 8))
    ))

;;; call function pointer in register
(define (riscv64:call-r info n)
  (let ((r (get-r info)))
    `((,(string-append "jalr___%" r))
      ,(riscv64:addi "sp" "sp" (* n 8)))))

;;; register to function argument.
(define (riscv64:r->arg info i)
  (let ((r (get-r info)))
    `((,(string-append "push___%" r)))))

;;; label to function argument
(define (riscv64:label->arg info label i)
  `(,(riscv64:li-address %tmpreg1 label)
    (,(string-append "push___%" %tmpreg1))))

;;; ALU: r0 := r0 + r1
(define (riscv64:r0+r1 info)
  (let ((r1 (get-r1 info))
        (r0 (get-r0 info)))
    `((,(string-append "add____%" r0 ",%" r0 ",%" r1)))))

;;; ALU: r0 := r0 - r1
(define (riscv64:r0-r1 info)
  (let ((r0 (get-r0 info))
        (r1 (get-r1 info)))
    `((,(string-append "sub____%" r0 ",%" r0 ",%" r1)))))

;;; add immediate value to r0
(define (riscv64:r0+value info v)
  (let ((r0 (get-r0 info)))
    `(,(riscv64:addi r0 r0 v))))

;;; add immediate to contents of 8-bit word addressed by register
(define (riscv64:r-byte-mem-add info v)
  (let ((r (get-r info)))
    `((,(string-append "lb_____%" %tmpreg1 ",0(%" r ")"))
      ,(riscv64:addi %tmpreg1 %tmpreg1 v)
      (,(string-append "sb_____%" %tmpreg1 ",0(%" r ")")))))

;;; add immediate to contents of 16-bit word addressed by register
(define (riscv64:r-word-mem-add info v)
  (let ((r (get-r info)))
    `((,(string-append "lh_____%" %tmpreg1 ",0(%" r ")"))
      ,(riscv64:addi %tmpreg1 %tmpreg1 v)
      (,(string-append "sh_____%" %tmpreg1 ",0(%" r ")")))))

;;; add immediate to contents of 32-bit word addressed by register
(define (riscv64:r-long-mem-add info v)
  (let ((r (get-r info)))
    `((,(string-append "lw_____%" %tmpreg1 ",0(%" r ")"))
      ,(riscv64:addi %tmpreg1 %tmpreg1 v)
      (,(string-append "sw_____%" %tmpreg1 ",0(%" r ")")))))

;;; add immediate to contents of 64-bit word addressed by register
(define (riscv64:r-mem-add info v)
  (let ((r (get-r info)))
    `((,(string-append "ld_____%" %tmpreg1 ",0(%" r ")"))
      ,(riscv64:addi %tmpreg1 %tmpreg1 v)
      (,(string-append "sd_____%" %tmpreg1 ",0(%" r ")")))))

;;; compute address of local variable and write result into register
(define (riscv64:local-ptr->r info n)
  (let ((r (get-r info))
        (n (- 0 (* 8 n))))
      `((,(string-append "mv_____%" r ",%fp"))
        ,(riscv64:addi r r n))))

;;; label address into register
(define (riscv64:label->r info label)
  (let ((r (get-r info)))
    `(,(riscv64:li-address r label))))

;;; copy register r0 to register r1 (see also r1->r0)
(define (riscv64:r0->r1 info)
  (let ((r0 (get-r0 info))
        (r1 (get-r1 info)))
    `((,(string-append  "mv_____%" r1 ",%" r0)))))

;;; copy register r1 to register r0 (see also r0->r1)
(define (riscv64:r1->r0 info)
  (let ((r0 (get-r0 info))
        (r1 (get-r1 info)))
    `((,(string-append  "mv_____%" r0 ",%" r1)))))

;;; zero-extend 8-bit in register r
(define (riscv64:byte-r info)
  (let ((r (get-r info)))
    `((,(string-append "ext.b__%" r)))))

;;; sign-extend 8-bit in register r
(define (riscv64:byte-signed-r info)
  (let ((r (get-r info)))
    `((,(string-append "sext.b_%" r)))))

;;; zero-extend 16-bit in register r
(define (riscv64:word-r info)
  (let ((r (get-r info)))
    `((,(string-append "ext.h__%" r)))))

;;; sign-extend 16-bit in register r
(define (riscv64:word-signed-r info)
  (let ((r (get-r info)))
    `((,(string-append "sext.h_%" r)))))

;;; zero-extend 32-bit in register r
(define (riscv64:long-r info)
  (let ((r (get-r info)))
    `((,(string-append "ext.w__%" r)))))

;;; sign-extend 32-bit in register r
(define (riscv64:long-signed-r info)
  (let ((r (get-r info)))
    `((,(string-append "sext.w_%" r)))))

;;; unconditional jump to label
(define (riscv64:jump info label)
  `(((#:offset-field "J" ,label) "j______0")))

;;;; Flag setters ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; test if a register is zero, set z flag accordingly
;;; see also test-r
(define (riscv64:r-zero? info)
  (let ((r (car (if (pair? (.allocated info)) (.allocated info) (.registers info)))))
    `((,(string-append "mv_____%" %condregx ",%" r))
      ,(riscv64:li %condregy 0))))

;;; test register r against 0 and set flags
;;; this is used for jump-* and cc?->r:
;;; z (both)
;;; g ge l le (signed)
;;; a ae b be (unsigned)
(define (riscv64:test-r info)
  (let ((r (get-r info)))
    `((,(string-append "mv_____%" %condregx ",%" r))
      ,(riscv64:li %condregy 0))))

;;; negate zero flag
(define (riscv64:xor-zf info)
  '(("cond.nz")))

;;; compare register to immediate value and set flags (see test-r)
(define (riscv64:r-cmp-value info v)
  (let ((r (get-r info)))
    `((,(string-append "mv_____%" %condregx ",%" r))
      ,(riscv64:li %condregy v))))

;;; compare register to another register and set flags (see test-r)
(define (riscv64:r0-cmp-r1 info)
  (let ((r0 (get-r0 info))
        (r1 (get-r1 info)))
    `((,(string-append "mv_____%" %condregx ",%" r0))
      (,(string-append "mv_____%" %condregy ",%" r1)))))

;;;; Flag users ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; flag-based conditional jumps (equality)
; because a B field only has a limited range, and we cannot rely on the linker being
; intelligent about this, this is the long way around: jump around a j with the inverse condition.
; - nz becomes beq instead of bne
; - z becomes bne instead of beq
(define (riscv64:jump-nz info label)
  `(((#:immediate-field "B" 8) ,(string-append "beq____%" %condregx ",%" %condregy ",0"))
    ((#:offset-field "J" ,label) ,(string-append "j______0"))))

(define (riscv64:jump-z info label)
  `(((#:immediate-field "B" 8) ,(string-append "bne____%" %condregx ",%" %condregy ",0"))
    ((#:offset-field "J" ,label) ,(string-append "j______0"))))

; assuming the result was properly zero/sign-extended, this is the same as a
; normal jump-z
(define (riscv64:jump-byte-z info label)
  `(((#:immediate-field "B" 8) ,(string-append "bne____%" %condregx ",%" %condregy ",0"))
    ((#:offset-field "J" ,label) ,(string-append "j______0"))))

;;; zero flag to register
(define (riscv64:zf->r info)
  (let ((r (get-r info)))
    `((,(string-append "seq____%" r)))))

;;; boolean: r := !e
(define (riscv64:r-negate info)
  (let ((r (get-r info)))
    `((,(string-append "seq____%" r)))))

;; flag-based conditional setters (signed)
(define (riscv64:g?->r info)
  (let ((r (get-r info)))
    `((,(string-append "sgt____%" r)))))

(define (riscv64:ge?->r info)
  (let ((r (get-r info)))
    `((,(string-append "sge____%" r)))))

(define (riscv64:l?->r info)
  (let ((r (get-r info)))
    `((,(string-append "slt____%" r)))))

(define (riscv64:le?->r info)
  (let ((r (get-r info)))
    `((,(string-append "sle____%" r)))))

;; flag-based conditional setters (unsigned)
(define (riscv64:a?->r info)
  (let ((r (get-r info)))
    `((,(string-append "sgtu___%" r)))))

(define (riscv64:ae?->r info)
  (let ((r (get-r info)))
    `((,(string-append "sgeu___%" r)))))

(define (riscv64:b?->r info)
  (let ((r (get-r info)))
    `((,(string-append "sltu___%" r)))))

(define (riscv64:be?->r info)
  (let ((r (get-r info)))
    `((,(string-append "sleu___%" r)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; store lower 8-bit of r0 at address r1
(define (riscv64:byte-r0->r1-mem info)
  (let ((r0 (get-r0 info))
        (r1 (get-r1 info)))
    `((,(string-append "sb_____%" r0 ",0(%" r1 ")")))))

;;; load word at label into register r
(define (riscv64:label-mem->r info label)
  (let ((r (get-r info)))
    `(,(riscv64:li-address %tmpreg1 label)
      (,(string-append "ld_____%" r ",0(%" %tmpreg1 ")")))))

;;; read 8-bit (and zero-extend) from address in register r into register r
(define (riscv64:byte-mem->r info)
  (let ((r (get-r info)))
    `((,(string-append "lbu____%" r ",0(%" r ")")))))

;;; read 16-bit (and zero-extend) from address in register r into register r
(define (riscv64:word-mem->r info)
  (let ((r (get-r info)))
    `((,(string-append "lhu____%" r ",0(%" r ")")))))

;;; read 32-bit (and zero-extend) from address in register r into register r
(define (riscv64:long-mem->r info)
  (let ((r (get-r info)))
    `((,(string-append "lwu____%" r ",0(%" r ")")))))

;;; read 64-bit from address in register r into register r
(define (riscv64:mem->r info)
  (let ((r (get-r info)))
    `((,(string-append "ld_____%" r ",0(%" r ")")))))

(define (riscv64:local-add info n v)
  (let ((n (- 0 (* 8 n))))
    (if (and (>= n #x-800) (<= n #x7ff))
      `(((#:immediate-field "I" ,n) ,(string-append "ld_____%" %tmpreg2 ",0(%fp)"))
        ,(riscv64:addi %tmpreg2 %tmpreg2 v)
        ((#:immediate-field "S" ,n) ,(string-append "sd_____%" %tmpreg2 ",0(%fp)")))
      `(,(riscv64:li %tmpreg1 n)
        (,(string-append "add____%" %tmpreg1 ",%" %tmpreg1 ",%fp"))
        (,(string-append "ld_____%" %tmpreg2 ",0(%" %tmpreg1 ")"))
        ,(riscv64:addi %tmpreg2 %tmpreg2 v)
        (,(string-append "sd_____%" %tmpreg2 ",0(%" %tmpreg1 ")"))))))

(define (riscv64:label-mem-add info label v)
  `(,(riscv64:li-address %tmpreg1 label)
    (,(string-append "ld_____%" %tmpreg2 ",0(%" %tmpreg1 ")"))
    ,(riscv64:addi %tmpreg2 %tmpreg2 v)
    (,(string-append "sd_____%" %tmpreg2 ",0(%" %tmpreg1 ")"))))

;; no-operation
(define (riscv64:nop info)
  '(("nop")))

;; swap the contents of register r0 and r1
(define (riscv64:swap-r0-r1 info)
  (let ((r0 (get-r0 info))
        (r1 (get-r1 info)))
    `((,(string-append "mv_____%" %tmpreg1 ",%" r1))
      (,(string-append "mv_____%" r1 ",%" r0))
      (,(string-append "mv_____%" r0 ",%" %tmpreg1)))))

;;; write 8-bit from register r to memory at the label
(define (riscv64:r->byte-label info label)
  (let ((r (get-r info)))
    `(,(riscv64:li-address %tmpreg1 label)
      (,(string-append "sb_____%" r ",0(%" %tmpreg1 ")")))))

;;; write 16-bit from register r to memory at the label
(define (riscv64:r->word-label info label)
  (let ((r (get-r info)))
    `(,(riscv64:li-address %tmpreg1 label)
      (,(string-append "sh_____%" r ",0(%" %tmpreg1 ")")))))

;;; write 32-bit from register r to memory at the label
(define (riscv64:r->long-label info label)
  (let ((r (get-r info)))
    `(,(riscv64:li-address %tmpreg1 label)
      (,(string-append "sw_____%" r ",0(%" %tmpreg1 ")")))))

;;; write 64-bit from register r to memory at the label
(define (riscv64:r->label info label)
  (let ((r (get-r info)))
    `(,(riscv64:li-address %tmpreg1 label)
      (,(string-append "sd_____%" r ",0(%" %tmpreg1 ")")))))

;;; ALU r0 := r0 * r1
(define (riscv64:r0*r1 info)
  (let ((r0 (get-r0 info))
        (r1 (get-r1 info)))
    `((,(string-append "mul____%" r0 ",%" r0 ",%" r1)))))

;;; bitwise r0 := r0 << r1
(define (riscv64:r0<<r1 info)
  (let ((r0 (get-r0 info))
        (r1 (get-r1 info)))
    `((,(string-append "sll____%" r0 ",%" r0 ",%" r1)))))

;;; bitwise r0 := r0 << imm
(define (riscv64:shl-r info n)
  (let ((r (get-r info)))
    `(,(riscv64:li %tmpreg1 n)
      (,(string-append "sll____%" r ",%" r ",%" %tmpreg1)))))

;;; bitwise r0 := r0 >> r1 (logical, so shift in zero bits)
(define (riscv64:r0>>r1 info)
  (let ((r0 (get-r0 info))
        (r1 (get-r1 info)))
    `((,(string-append "srl____%" r0 ",%" r0 ",%" r1)))))

;;; bitwise r0 := r0 & r1
(define (riscv64:r0-and-r1 info)
  (let ((r0 (get-r0 info))
        (r1 (get-r1 info)))
    `((,(string-append "and____%" r0 ",%" r0 ",%" r1)))))

;;; bitwise r0 := r0 | r1
(define (riscv64:r0-or-r1 info)
  (let ((r0 (get-r0 info))
        (r1 (get-r1 info)))
    `((,(string-append "or_____%" r0 ",%" r0 ",%" r1)))))

;;; bitwise r := r & imm
(define (riscv64:r-and info n)
  (let ((r (get-r info)))
    `(,(riscv64:li %tmpreg1 n)
      (,(string-append "and____%" r ",%" r ",%" %tmpreg1)))))

;;; bitwise r0 := r0 ^ r1
(define (riscv64:r0-xor-r1 info)
  (let ((r0 (get-r0 info))
        (r1 (get-r1 info)))
    `((,(string-append "xor____%" r0 ",%" r0 ",%" r1)))))

;;; ALU r0 := r0 / r1
(define (riscv64:r0/r1 info signed?)
  (let ((r0 (get-r0 info))
        (r1 (get-r1 info)))
    `((,(string-append "div____%" r0 ",%" r0 ",%" r1)))))

;;; ALU r0 := r0 % r1
(define (riscv64:r0%r1 info signed?)
  (let ((r0 (get-r0 info))
        (r1 (get-r1 info)))
    `((,(string-append "rem____%" r0 ",%" r0 ",%" r1)))))

;;; ALU r0 := r0 + imm
(define (riscv64:r+value info v)
  (let ((r (get-r info)))
    `(,(riscv64:addi r r v))))

;;; store 8-bit r0 into address ported by r1
(define (riscv64:byte-r0->r1-mem info)
  (let ((r0 (get-r0 info))
        (r1 (get-r1 info)))
    `((,(string-append "sb_____%" r0 ",0(%" r1 ")")))))

;;; store 16-bit r0 into address ported by r1
(define (riscv64:word-r0->r1-mem info)
  (let ((r0 (get-r0 info))
         (r1 (get-r1 info)))
    `((,(string-append "sh_____%" r0 ",0(%" r1 ")")))))

;;; store 32-bit r0 into address ported by r1
(define (riscv64:long-r0->r1-mem info)
  (let ((r0 (get-r0 info))
         (r1 (get-r1 info)))
    `((,(string-append "sw_____%" r0 ",0(%" r1 ")")))))

;;; store 64-bit r0 into address ported by r1
(define (riscv64:r0->r1-mem info)
  (let ((r0 (get-r0 info))
        (r1 (get-r1 info)))
    `((,(string-append "sd_____%" r0 ",0(%" r1 ")")))))

;;; push register to stack
(define (riscv64:push-register info r)
  `((,(string-append "push___%" r))))

;;; push register r0 to stack (see also push-register)
(define (riscv64:push-r0 info)
  (let ((r0 (get-r0 info)))
    `((,(string-append "push___%" r0)))))

;;; pop register from stack
(define (riscv64:pop-register info r)
  `((,(string-append "pop____%" r))))

;;; pop register r0 from stack (see also pop-register)
(define (riscv64:pop-r0 info)
  (let ((r0 (get-r0 info)))
    `((,(string-append "pop____%" r0)))))

;;; get function return value
(define (riscv64:return->r info)
  (let ((r (car (.allocated info))))
    (if (equal? r %retreg) '()
        `((,(string-append "mv_____%" r ",%" %retreg))))))

;;; bitwise r := r + r (doubling)
(define (riscv64:r+r info)
  (let ((r (get-r info)))
    `((,(string-append "add____%" r ",%" r ",%" r)))))

;;; bitwise r := ~r
(define (riscv64:not-r info)
  (let ((r (get-r info)))
    `((,(string-append "not____%" r ",%" r)))))

;;; load 8-bit at address r0, store to address r1
(define (riscv64:byte-r0-mem->r1-mem info)
  (let* ((r0 (get-r0 info))
         (r1 (get-r1 info)))
    `((,(string-append "lb_____%" %tmpreg1 ",0(%" r0 ")"))
      (,(string-append "sb_____%" %tmpreg1 ",0(%" r1 ")")))))

;;; load 16-bit at address r0, store to address r1
(define (riscv64:word-r0-mem->r1-mem info)
  (let* ((r0 (get-r0 info))
         (r1 (get-r1 info)))
    `((,(string-append "lh_____%" %tmpreg1 ",0(%" r0 ")"))
      (,(string-append "sh_____%" %tmpreg1 ",0(%" r1 ")")))))

;;; load 32-bit at address r0, store to address r1
(define (riscv64:long-r0-mem->r1-mem info)
  (let* ((r0 (get-r0 info))
         (r1 (get-r1 info)))
    `((,(string-append "lw_____%" %tmpreg1 ",0(%" r0 ")"))
      (,(string-append "sw_____%" %tmpreg1 ",0(%" r1 ")")))))

;;; load 64-bit at address r0, store to address r1
(define (riscv64:r0-mem->r1-mem info)
  (let* ((r0 (get-r0 info))
         (r1 (get-r1 info)))
    `((,(string-append "ld_____%" %tmpreg1 ",0(%" r0 ")"))
      (,(string-append "sd_____%" %tmpreg1 ",0(%" r1 ")")))))

;;; register (8-bit) to stack local
(define (riscv64:byte-r->local+n info id n)
  (let ((n (+ (- 0 (* 8 id)) n))
         (r (get-r info)))
    (riscv64:x-r->local+n "b" n r)))

;;; register (16-bit) to stack local
(define (riscv64:word-r->local+n info id n)
  (let ((n (+ (- 0 (* 8 id)) n))
         (r (get-r info)))
    (riscv64:x-r->local+n "h" n r)))

;;; register (32-bit) to stack local
(define (riscv64:long-r->local+n info id n)
  (let ((n (+ (- 0 (* 8 id)) n))
         (r (get-r info)))
    (riscv64:x-r->local+n "w" n r)))

;;; register (64-bit) to stack local
(define (riscv64:r->local info n)
  (let ((r (get-r info))
        (n (- 0 (* 8 n))))
    (riscv64:x-r->local+n "d" n r)))

;;; register (64-bit) to stack local (how does this differ from r->local ?)
;;; n is computed differently
(define (riscv64:r->local+n info id n)
  (let ((n (+ (- 0 (* 8 id)) n))
         (r (get-r info)))
    (riscv64:x-r->local+n "d" n r)))

;;; swap value of register r with the top word of the stack
;; seems unused
(define (riscv64:swap-r-stack info)
  (let ((r (get-r info)))
    `((,(string-append "ld_____%" %tmpreg1 ",0(%sp)"))
      (,(string-append "sd_____%" r ",0(%sp)"))
      (,(string-append "mv_____%" r ",%" %tmpreg1)))))

;;; swap value of register r0 (not r1) with the top word of the stack
;; used in expr->arg
(define (riscv64:swap-r1-stack info)
  (let ((r0 (get-r0 info)))
    `((,(string-append "ld_____%" %tmpreg1 ",0(%sp)"))
      (,(string-append "sd_____%" r0 ",0(%sp)"))
      (,(string-append "mv_____%" r0 ",%" %tmpreg1)))))

;;; not entirely sure what this is supposed to do
;;; i guess the idea would be to copy register r2 to r1, but what is the pop/push about?
(define (riscv64:r2->r0 info)
  (let ((r0 (get-r0 info))
        (r1 (get-r1 info))
        (allocated (.allocated info)))
    (if (> (length allocated) 2)
        (let ((r2 (cadddr allocated)))
          `((,(string-append  "mv_____%" r1 ",%" r2))))
        `((,(string-append  "pop____%" r0))
          (,(string-append  "push___%" r0))))))

(define riscv64:instructions
  `(
    (a?->r . ,riscv64:a?->r)
    (ae?->r . ,riscv64:ae?->r)
    (b?->r . ,riscv64:b?->r)
    (be?->r . ,riscv64:be?->r)
    (byte-mem->r . ,riscv64:byte-mem->r)
    (byte-r . ,riscv64:byte-r)
    (byte-r->local+n . ,riscv64:byte-r->local+n)
    (byte-r0->r1-mem . ,riscv64:byte-r0->r1-mem)
    (byte-r0-mem->r1-mem . ,riscv64:byte-r0-mem->r1-mem)
    (byte-signed-r . ,riscv64:byte-signed-r)
    (call-label . ,riscv64:call-label)
    (call-r . ,riscv64:call-r)
    (function-locals . ,riscv64:function-locals)
    (function-preamble . ,riscv64:function-preamble)
    (g?->r . ,riscv64:g?->r)
    (ge?->r . ,riscv64:ge?->r)
    (jump . ,riscv64:jump)
;   (jump-a . ,riscv64:jump-a)
;   (jump-ae . ,riscv64:jump-ae)
;   (jump-b . ,riscv64:jump-b)
;   (jump-be . ,riscv64:jump-be)
    (jump-byte-z . ,riscv64:jump-byte-z)
;   (jump-g . , riscv64:jump-g)
;   (jump-ge . , riscv64:jump-ge)
;   (jump-l . ,riscv64:jump-l)
;   (jump-le . ,riscv64:jump-le)
    (jump-nz . ,riscv64:jump-nz)
    (jump-z . ,riscv64:jump-z)
    (l?->r . ,riscv64:l?->r)
    (label->arg . ,riscv64:label->arg)
    (label->r . ,riscv64:label->r)
    (label-mem->r . ,riscv64:label-mem->r)
    (label-mem-add . ,riscv64:label-mem-add)
    (le?->r . ,riscv64:le?->r)
    (local->r . ,riscv64:local->r)
    (local-add . ,riscv64:local-add)
    (local-ptr->r . ,riscv64:local-ptr->r)
    (long-mem->r . ,riscv64:long-mem->r)
    (long-r . ,riscv64:long-r)
    (long-r->local+n . ,riscv64:long-r->local+n)
    (long-r0->r1-mem . ,riscv64:long-r0->r1-mem)
    (long-r0-mem->r1-mem . ,riscv64:long-r0-mem->r1-mem)
    (long-signed-r . ,riscv64:long-signed-r)
    (mem->r . ,riscv64:mem->r)
    (nop . ,riscv64:nop)
    (not-r . ,riscv64:not-r)
    (pop-r0 . ,riscv64:pop-r0)
    (pop-register . ,riscv64:pop-register)
    (push-r0 . ,riscv64:push-r0)
    (push-register . ,riscv64:push-register)
    (quad-r0->r1-mem . ,riscv64:r0->r1-mem)
    (r+r . ,riscv64:r+r)
    (r+value . ,riscv64:r+value)
    (r->arg . ,riscv64:r->arg)
    (r->byte-label . ,riscv64:r->byte-label)
    (r->label . ,riscv64:r->label)
    (r->local . ,riscv64:r->local)
    (r->local+n . ,riscv64:r->local+n)
    (r->long-label . ,riscv64:r->long-label)
    (r->word-label . ,riscv64:r->word-label)
    (r-and . ,riscv64:r-and)
    (r-byte-mem-add . ,riscv64:r-byte-mem-add)
    (r-cmp-value . ,riscv64:r-cmp-value)
    (r-long-mem-add . ,riscv64:r-long-mem-add)
    (r-mem-add . ,riscv64:r-mem-add)
    (r-negate . ,riscv64:r-negate)
    (r-word-mem-add . ,riscv64:r-word-mem-add)
    (r-zero? . ,riscv64:r-zero?)
    (r0%r1 . ,riscv64:r0%r1)
    (r0*r1 . ,riscv64:r0*r1)
    (r0+r1 . ,riscv64:r0+r1)
    (r0+value . ,riscv64:r0+value)
    (r0->r1 . ,riscv64:r0->r1)
    (r0->r1-mem . ,riscv64:r0->r1-mem)
    (r0-and-r1 . ,riscv64:r0-and-r1)
    (r0-cmp-r1 . ,riscv64:r0-cmp-r1)
    (r0-mem->r1-mem . ,riscv64:r0-mem->r1-mem)
    (r0-or-r1 . ,riscv64:r0-or-r1)
    (r0-r1 . ,riscv64:r0-r1)
    (r0-xor-r1 . ,riscv64:r0-xor-r1)
    (r0/r1 . ,riscv64:r0/r1)
    (r0<<r1 . ,riscv64:r0<<r1)
    (r0>>r1 . ,riscv64:r0>>r1)
    (r1->r0 . ,riscv64:r1->r0)
    (r2->r0 . ,riscv64:r2->r0)
    (ret . ,riscv64:ret)
    (return->r . ,riscv64:return->r)
    (shl-r . ,riscv64:shl-r)
    (swap-r-stack . ,riscv64:swap-r-stack)
    (swap-r0-r1 . ,riscv64:swap-r0-r1)
    (swap-r1-stack . ,riscv64:swap-r1-stack)
    (test-r . ,riscv64:test-r)
    (value->r . ,riscv64:value->r)
    (value->r0 . ,riscv64:value->r0)
    (word-mem->r . ,riscv64:word-mem->r)
    (word-r . ,riscv64:word-r)
    (word-r->local+n . ,riscv64:word-r->local+n)
    (word-r0->r1-mem . ,riscv64:word-r0->r1-mem)
    (word-r0-mem->r1-mem . ,riscv64:word-r0-mem->r1-mem)
    (word-signed-r . ,riscv64:word-signed-r)
    (xor-zf . ,riscv64:xor-zf)
    (zf->r . ,riscv64:zf->r)
    ))
