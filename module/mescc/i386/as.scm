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

;;; Commentary:

;;; define i386 assembly

;;; Code:

(define-module (mescc i386 as)
  #:use-module (mes guile)
  #:use-module (mescc as)
  #:use-module (mescc info)
  #:export (
            i386:accu%base
            i386:accu*base
            i386:accu*n->label
            i386:accu*n->local
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
            i386:accu->local+n
            i386:accu->local+n
            i386:accu-and
            i386:accu-and-base
            i386:accu-and-base-mem
            i386:accu-base
            i386:accu-cmp-value
            i386:accu-mem-add
            i386:accu-mem->base-mem
            i386:accu-negate
            i386:accu-not
            i386:accu-or-base
            i386:accu-or-base-mem
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
            i386:base-mem->accu-mem
            i386:base-mem+n->accu
            i386:base-mem->accu
            i386:base-sub
            i386:byte-accu->base-mem
            i386:word-accu->base-mem
            i386:byte-base->accu-mem
            i386:byte-base->accu-mem+n
            i386:byte-base-mem->accu
            i386:byte-base-sub
            i386:byte-local->base
            i386:byte-mem->accu
            i386:word-mem->accu
            i386:byte-mem->base
            i386:byte-sub-base
            i386:byte-test-base
            i386:call-accu
            i386:call-label
            i386:formal
            i386:jump
            i386:jump
            i386:jump-a
            i386:jump-ae
            i386:jump-b
            i386:jump-be
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
            i386:push-word-local-de-ref
            i386:push-label
            i386:push-label-mem
            i386:push-local
            i386:push-local-address
            i386:push-local-de-ref
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
            i386:a?->accu
            i386:ae?->accu
            i386:b?->accu
            i386:be?->accu
            i386:z->accu
            i386:byte-accu
            i386:signed-byte-accu
            i386:word-accu
            i386:signed-word-accu

            i386:instructions
            ))

(define (i386:nop)
  '(("nop")))

(define (i386:function-preamble . rest)
  '(("push___%ebp")
    ("mov____%esp,%ebp")))

(define (i386:function-locals . rest)
  `(("sub____$i32,%esp" (#:immediate ,(+ (* 4 1025) (* 20 4)))))) ; 4*1024 buf, 20 local vars

(define (i386:push-label label)
  `(("push___$i32" (#:address ,label)))) ; push  $0x<label>

(define (i386:push-label-mem label)
  `(("mov____0x32,%eax" (#:address ,label)) ; mov    0x804a000,%eax
    ("push___%eax")))                       ; push  %eax


;;;  locals

(define (i386:push-local n)
  (or n (error "invalid value: push-local: " n))
  (let ((n (- 0 (* 4 n))))
    `(,(if (< (abs n) #x80) `("push___0x8(%ebp)" (#:immediate1 ,n))
           `("push___0x32(%ebp)" (#:immediate ,n))))))

(define (i386:push-local-address n)
  (or n (error "invalid value: push-local-address: " n))
  (let ((n (- 0 (* 4 n))))
    `(,(if (< (abs n) #x80) `("lea____0x8(%ebp),%eax" (#:immediate1 ,n))
           `("lea____0x32(%ebp),%eax" (#:immediate ,n)))
      ("push___%eax"))))

(define (i386:push-byte-local-de-ref n)
  (or n (error "invalid value: push-byte-local-de-ref: " n))
  (let ((n (- 0 (* 4 n))))
    `(,(if (< (abs n) #x80) `("mov____0x8(%ebp),%eax" (#:immediate1 ,n))
           `("mov____0x32(%ebp),%eax" (#:immediate ,n)))
      ("movzbl_(%eax),%eax")
      ("push___%eax"))))

(define (i386:push-word-local-de-ref n)
  (or n (error "invalid value: push-word-local-de-ref: " n))
  (let ((n (- 0 (* 4 n))))
    `(,(if (< (abs n) #x80) `("mov____0x8(%ebp),%eax" (#:immediate1 ,n))
           `("mov____0x32(%ebp),%eax" (#:immediate ,n)))
      ("movzwl_(%eax),%eax")
      ("push___%eax"))))

(define (i386:push-byte-local-de-de-ref n)
  (or n (error "invalid value: push-byte-local-de-de-ref: " n))
  (let ((n (- 0 (* 4 n))))
    `(,(if (< (abs n) #x80) `("mov____0x8(%ebp),%eax" (#:immediate1 ,n))
           `("mov____0x32(%ebp),%eax" (#:immediate ,n)))
      ("mov____(%eax),%eax")
      ("movzbl_(%eax),%eax")
      ("push___%eax"))))

(define (i386:push-local-de-ref n)
  (or n (error "invalid value: push-byte-local-de-ref: " n))
  (let ((n (- 0 (* 4 n))))
    `(,(if (< (abs n) #x80) `("mov____0x8(%ebp),%eax" (#:immediate1 ,n))
           `("mov____0x32(%ebp),%eax" (#:immediate ,n)))
      ("mov____(%eax),%eax")
      ("push___%eax"))))

(define (i386:local-add n v)
  (or n (error "invalid value: i386:local-add: " n))
  (let ((n (- 0 (* 4 n))))
    `(,(if (and (< (abs n) #x80)
                (< (abs v) #x80)) `("add____$i8,0x8(%ebp)" (#:immediate1 ,n) (#:immediate1 ,v))
                `("add____$i32,0x32(%ebp)" (#:immediate ,n) (#:immediate ,v))))))

(define (i386:accu->local n)
  (or n (error "invalid value: accu->local: " n))
  (let ((n (- 0 (* 4 n))))
    `(,(if (< (abs n) #x80) `("mov____%eax,0x8(%ebp)" (#:immediate1 ,n))
           `("mov____%eax,0x32(%ebp)" (#:immediate ,n))))))

(define (i386:accu->local+n id n)
  (let ((n (+ (- 0 (* 4 id)) n)))
    `(,(if (< (abs n) #x80) `("mov____%eax,0x8(%ebp)" (#:immediate1 ,n))
           `("mov____%eax,0x32(%ebp)" (#:immediate ,n))))))

(define (i386:accu*n->local i n)
  (or n (error "invalid value: accu->local: " n))
  (let ((o (- 0 (* 4 i))))
    (let loop ((i 0))
      (if (>= i n) '()  ;; FIXME: byte, word-sized
          (let ((o (+ o i)))
            (append
             (if (< (abs o) #x80) `(("mov____0x8(%eax),%ebx" (#:immediate1 ,i))
                                    ("mov____%ebx,0x8(%ebp)" (#:immediate1 ,o)))
                 `(("mov____0x8(%eax),%ebx" (#:immediate1 ,i))
                   ("mov____%ebx,0x32(%ebp)" (#:immediate ,o))))
             (loop (+ i 4))))))))

(define (i386:local->accu n)
  (or n (error "invalid value: local->accu: " n))
  (let ((n (- 0 (* 4 n))))
    `(,(if (< (abs n) #x80) `("mov____0x8(%ebp),%eax" (#:immediate1 ,n))
           `("mov____0x32(%ebp),%eax" (#:immediate ,n))))))

(define (i386:local-address->accu n)
  (or n (error "invalid value: ladd: " n))
  (let ((n (- 0 (* 4 n))))
    `(,(if (< (abs n) #x80) `("lea____0x8(%ebp),%eax" (#:immediate1 ,n))
           `("lea____0x32(%ebp),%eax" (#:immediate ,n))))))

(define (i386:local-ptr->accu n)
  (or n (error "invalid value: local-ptr->accu: " n))
  (let ((n (- 0 (* 4 n))))
  `(("mov____%ebp,%eax")                ; mov    %ebp,%eax
    ,(if (< (abs n) #x80) `("add____$i8,%eax" (#:immediate1 ,n))
         `("add____$i32,%eax" (#:immediate ,n))))))

(define (i386:byte-local->base n)
  (or n (error "invalid value: byte-local->base: " n))
  (let ((n (- 0 (* 4 n))))
    `(,(if (< (abs n) #x80) `("movzbl_0x8(%ebp),%edx" (#:immediate1 ,n))
           `,@(("mov_0x32(%ebp),%edx" (#:immediate ,n))
               ("movzbl_%dl,%edx"))))))

(define (i386:local->base n)
  (or n (error "invalid value: local->base: " n))
  (let ((n (- 0 (* 4 n))))
    `(,(if (< (abs n) #x80) `("mov____0x8(%ebp),%edx" (#:immediate1 ,n))
           `("mov____0x32(%ebp),%edx" (#:immediate ,n))))))

(define (i386:local-address->base n) ;; DE-REF
  (or n (error "invalid value: local-address->base: " n))
  (let ((n (- 0 (* 4 n))))
    `(,(if (< (abs n) #x80) `("lea____0x8(%ebp),%edx" (#:immediate1 ,n))
           `("lea____0x32(%ebp),%edx" (#:immediate ,n))))))

(define (i386:local-ptr->base n)
  (or n (error "invalid value: local-ptr->base: " n))
  (let ((n (- 0 (* 4 n))))
    `(("mov____%ebp,%edx")                ; mov    %ebp,%edx
      ,(if (< (abs n) #x80) `("add____$i8,%edx" (#:immediate1 ,n))
           `("add____$i32,%edx" (#:immediate ,n))))))

(define (i386:value->local n v)
  (or n (error "invalid value: value->local: " n))
  (let ((n (- 0 (* 4 n))))
    `(,(if (< (abs n) #x80) `("mov____$i32,0x8(%ebp)" (#:immediate1 ,n) (#:immediate ,v))
           `("mov____$i32,0x32(%ebp)" (#:immediate ,n) (#:immediate ,v))))))

(define (i386:local-test n v)
  (or n (error "invalid value: local-test: " n))
  (let ((n (- 0 (* 4 n))))
    `(,(cond ((and (< (abs n) #x80)
                   (< (abs v) #x80)) `("cmp____$i8,0x8(%ebp)" (#:immediate1 ,n) (#:immediate1 ,v)))
             ((< (abs n) #x80) `("cmp____$i32,0x8(%ebp)" (#:immediate1 ,n) (#:immediate ,v)))
             ((< (abs v) #x80) `("cmp____$i8,0x32(%ebp)" (#:immediate ,n) (#:immediate1 ,v)))
             (else `("cmp____$i32,0x32(%ebp)" (#:immediate ,n) (#:immediate ,v)))))))

(define (i386:pop-accu)
  '(("pop____%eax")))                   ; pop %eax

(define (i386:push-accu)
  '(("push___%eax")))                   ; push %eax

(define (i386:pop-base)
  '(("pop____%edx")))                   ; pop %edx

(define (i386:push-base)
  '(("push___%edx")))                   ; push %edx

(define (i386:ret . rest)
  '(("leave")
    ("ret")))

(define (i386:accu->base)
  '(("mov____%eax,%edx")))              ; mov    %eax,%edx

(define (i386:accu->base-mem)
  '(("mov____%eax,(%edx)")))            ; mov    %eax,(%edx)

(define (i386:byte-accu->base-mem)
  '(("mov____%al,(%edx)")))             ; mov    %al,(%edx)

(define (i386:word-accu->base-mem)
  '(("mov____%ax,(%edx)")))             ; mov    %ax,(%edx)

(define (i386:accu->base-mem+n n)
  (or n (error "invalid value: accu->base-mem+n: " n))
  `(,(if (< (abs n) #x80) `("mov____%eax,0x8(%edx)" (#:immediate1 ,n))
         `("mov____%eax,0x32(%edx)" (#:immediate ,n)))))

(define (i386:byte-accu->base-mem+n n)
  (or n (error "invalid value: accu->base-mem+n: " n))
  `(,(if (< (abs n) #x80) `("mov____%al,0x8(%edx)" (#:immediate1 ,n))
         `("mov____%al,0x32(%edx)" (#:immediate ,n)))))

(define (i386:word-accu->base-mem+n n)
  (or n (error "invalid value: accu->base-mem+n: " n))
  `(,(if (< (abs n) #x80) `("mov____%ax,0x8(%edx)" (#:immediate1 ,n))
         `("mov____%ax,0x32(%edx)" (#:immediate ,n)))))

(define (i386:accu->label label)
  `(("mov____%eax,0x32" (#:address ,label)))) ; mov    %eax,0x<label>

(define (i386:accu*n->label label n)
  (append
   '(("push___%edx"))
   (let loop ((i 0))
     (if (>= i n) '() ;; FIXME: byte, word-sized
         (append
          `(("mov____$i32,%edx" (#:address ,label))
            ("mov____0x8(%eax),%ebx" (#:immediate1 ,i))
            ("mov____%ebx,0x8(%edx)" (#:immediate1 ,i)))
          (loop (+ i 4)))))
   '(("pop____%edx"))))

(define (i386:accu-shl n)
  (or n (error "invalid value: accu:shl n: " n))
  `(("shl____$i8,%eax" (#:immediate1 ,n)))) ; shl    $0x8,%eax

(define (i386:accu<<base)
  '(("xor____%ecx,%ecx")                ; xor    %ecx,%ecx
    ("mov____%edx,%ecx")                ; mov    %edx,%ecx
    ("shl____%cl,%eax")))               ; shl    %cl,%eax

(define (i386:accu>>base)
  '(("xor____%ecx,%ecx")                ; xor    %ecx,%ecx
    ("mov____%edx,%ecx")                ; mov    %edx,%ecx
    ("shr____%cl,%eax")))               ; shr    %cl,%eax

(define (i386:accu-and-base)
  '(("and____%edx,%eax")))

(define (i386:accu-and v)
  `(("and____$i32,%eax" (#:immediate ,v))))

(define (i386:accu-and-base-mem)
  '(("and____(%edx),%eax")))

(define (i386:accu-or-base-mem)
  '(("or_____(%edx),%eax")))

(define (i386:accu-not)
  '(("not____%eax")))                   ; not %eax

(define (i386:accu-or-base)
  '(("or_____%edx,%eax")))              ; or    %edx,%eax

(define (i386:accu-xor-base)
  '(("xor____%edx,%eax")))              ; xor    %edx,%eax

(define (i386:accu+accu)
  '(("add____%eax,%eax")))              ; add    %eax,%eax

(define (i386:accu+base)
  `(("add____%edx,%eax")))              ; add    %edx,%eax

(define (i386:accu+value v)
  `(,(if (< (abs v) #x80) `("add____$i8,%eax" (#:immediate1 ,v))
         `("add____$i32,%eax" (#:immediate ,v)))))

(define (i386:base+value v)
  `(,(if (< (abs v) #x80) `("add____$i8,%edx" (#:immediate1 ,v))
         `("add____$i32,%edx" (#:immediate ,v)))))

(define (i386:accu-base)
  `(("sub____%edx,%eax")))              ; sub    %edx,%eax

(define (i386:accu*base)
  `(("mul____%edx")))                   ; mul    %edx

(define (i386:accu/base)
  '(("mov____%edx,%ebx")                ; mov    %edx,%ebx
    ("xor____%edx,%edx")                ; xor    %edx,%edx
    ("idiv___%ebx")))                   ; div    %ebx

(define (i386:accu%base)
  '(("mov____%edx,%ebx")                ; mov    %edx,%ebx
    ("xor____%edx,%edx")                ; xor    %edx,%edx
    ("idiv___%ebx")                     ; div    %ebx
    ("mov____%edx,%eax")))              ; mov    %edx,%eax

(define (i386:base->accu)
  '(("mov____%edx,%eax")))              ; mov    %edx,%eax

(define (i386:label->accu label)
  `(("mov____$i32,%eax" (#:address ,label)))) ; mov    $<n>,%eax

(define (i386:label->base label)
  `(("mov____$i32,%edx" (#:address ,label)))) ; mov   $<n>,%edx

(define (i386:label-mem->accu label)
  `(("mov____0x32,%eax" (#:address ,label)))) ; mov    0x<n>,%eax

(define (i386:label-mem->base label)
  `(("mov____0x32,%edx" (#:address ,label)))) ; mov    0x<n>,%edx

(define (i386:label-mem-add label v)
  `(,(if (< (abs v) #x80) `("add____$i8,0x32" (#:address ,label) (#:immediate1 ,v))
         `("add____$i32,0x32" (#:address ,label) (#:immediate ,v)))))

(define (i386:byte-base-mem->accu)
  '(("add____%edx,%eax")                ; add    %edx,%eax
    ("movzbl_(%eax),%eax")))            ; movzbl (%eax),%eax

(define (i386:byte-mem->accu)
  '(("movzbl_(%eax),%eax")))            ; movzbl (%eax),%eax

(define (i386:word-mem->accu)
  '(("movzwl_(%eax),%eax")))

(define (i386:byte-mem->base)
  '(("movzbl_(%edx),%edx")))            ; movzbl (%edx),%edx

(define (i386:base-mem->accu)
  '(("mov____(%edx),%eax")))

(define (i386:mem->accu)
  '(("mov____(%eax),%eax")))

(define (i386:mem->base)
  '(("mov____(%edx),%edx")))

(define (i386:mem+n->accu n)
  `(,(if (< (abs n) #x80) `("mov____0x8(%eax),%eax" (#:immediate1 ,n))
         `("mov____0x32(%eax),%eax" (#:immediate ,n)))))

(define (i386:byte-mem+n->accu n)
  `(,(if (< (abs n) #x80) `("movzbl_0x8(%eax),%eax" (#:immediate1 ,n))
         `("movzbl_0x32(%eax),%eax" (#:immediate ,n)))))

(define (i386:word-mem+n->accu n)
  `(,(if (< (abs n) #x80) `("movzwl_0x8(%eax),%eax" (#:immediate1 ,n))
         `("movzwl_xb0x32(%eax),%eax" (#:immediate ,n)))))

(define (i386:base-mem+n->accu v)
  (or v (error "invalid value: base-mem+n->accu: " v))
  `(("add___%edx,%eax")
    ,(if (< (abs v) #x80) `("mov____0x8(%eax),%eax" (#:immediate1 ,v))
         `("mov____0x32(%eax),%eax" (#:immediate ,v)))))

(define (i386:value->accu v)
  (or v (error "invalid value: i386:value->accu: " v))
  `(("mov____$i32,%eax" (#:immediate ,v))))

(define (i386:value->accu-mem v)
  `(("mov____$i32,(%eax)" (#:immediate ,v)))) ; movl   $0x<v>,(%eax)

(define (i386:value->accu-mem+n n v)
  (or v (error "invalid value: i386:value->accu-mem+n: " v))
  `(,(if (< (abs v) #x80) `("mov____$i32,0x8(%eax)" (#:immediate1 ,n) (#:immediate ,v))
         `("mov____$i32,0x32(%eax)" (#:immediate ,n) (#:immediate ,v)))))

(define (i386:base->accu-mem)
  '(("mov____%edx,(%eax)")))            ; mov    %edx,(%eax)

(define (i386:accu-mem->base-mem)
  '(("mov____(%eax),%ecx")
    ("mov____%ecx,(%edx)")))

(define (i386:base-mem->accu-mem)
  '(("mov____(%edx),%ecx")              ; mov    (%edx),%ecx
    ("mov____%ecx,(%eax)")))            ; mov    %ecx,(%eax)

(define (i386:byte-base->accu-mem)
  '(("mov____%dl,(%eax)")))             ; mov    %dl,(%eax)

(define (i386:byte-base->accu-mem+n n)
  (or n (error "invalid value: byte-base->accu-mem+n: " n))
  `(,(if (< (abs n) #x80) `("mov____%dl,0x8(%eax)" (#:immediate1 ,n))
         `("mov____%dl,0x32(%eax)" (#:immediate ,n)))))

(define (i386:value->base v)
  (or v (error "invalid value: i386:value->base: " v))
  `(("mov____$i32,%edx" (#:immediate ,v)))) ; mov    $<v>,%edx

(define (i386:accu-mem-add v)
  `(,(if (< (abs v) #x80) `("add____$i8,(%eax)" (#:immediate1 ,v))
         `("add____$i32,(%eax)" (#:immediate ,v)))))

(define (i386:value->label label v)
  (or v (error "invalid value: value->label: " v))
  `(("mov____$i32,0x32" (#:address ,label)
     (#:immediate ,v))))

(define (i386:call-label info label n)
  `((call32 (#:offset ,label))
    ("add____$i8,%esp" (#:immediate1 ,(* n 4)))))

(define (i386:call-accu n)
  `(,@(i386:push-accu)
    ,@(i386:pop-accu)
    ("call___*%eax")                    ; call   *%eax
    ("add____$i8,%esp" (#:immediate1  ,(* n 4))))) ; add    $00,%esp

(define (i386:accu-zero?)
  '(("test___%eax,%eax")))

(define (i386:accu-negate)
  '(("sete___%al")                      ; sete %al
    ("movzbl_%al,%eax")))               ; movzbl %al,%eax

(define (i386:xor-accu v)
  (or v (error "invalid value: i386:xor-accu: n: " v))
  `(("xor___$i32,%eax" (#:immediate ,v)))) ;xor    $0xff,%eax

(define (i386:xor-zf)
  '(("lahf")                               ; lahf
    ("xor____$i8,%ah" (#:immediate1 #x40)) ; xor    $0x40,%ah
    ("sahf")))                             ; sahf

(define (i386:accu-cmp-value v)
  `(,(if (< (abs v) #x80) `("cmp____$i8,%eax" (#:immediate1 ,v))
         `("cmp____$i32,%eax" (#:immediate ,v)))))

(define (i386:accu-test)
  '(("test___%eax,%eax")))              ; test   %eax,%eax

(define (i386:jump label)
  `(("jmp32 " (#:offset ,label))))

(define (i386:jump-z label)
  `(("je32  " (#:offset ,label))))        ; jz . + <n>

(define (i386:jump-byte-z label)
  `(("test___%al,%al")                  ; test   %al,%al
    ("je32  " (#:offset ,label))))      ; je <n>

;; signed
(define (i386:jump-g label)
  `(("jg32  " (#:offset ,label))))

(define (i386:jump-ge label)
  `(("jge32 " (#:offset ,label))))

(define (i386:jump-l label)
  `(("jl32  " (#:offset ,label))))

(define (i386:jump-le label)
  `(("jle32 " (#:offset ,label))))

(define (i386:g?->accu)
  '(("setg___%al")
    ("movzbl_%al,%eax")))

(define (i386:ge?->accu)
  '(("setge__%al")
    ("movzbl_%al,%eax")))

(define (i386:l?->accu)
  '(("setl___%al")
    ("movzbl_%al,%eax")))

(define (i386:le?->accu)
  '(("setle__%al")
    ("movzbl_%al,%eax")))

;; unsigned
(define (i386:jump-a label)
  `(("ja32  " (#:offset ,label))))

(define (i386:jump-ae label)
  `(("jae32 " (#:offset ,label))))

(define (i386:jump-b label)
  `(("jb32  " (#:offset ,label))))

(define (i386:jump-be label)
  `(("jbe32 " (#:offset ,label))))

(define (i386:a?->accu)
  '(("seta___%al")
    ("movzbl_%al,%eax")))

(define (i386:ae?->accu)
  '(("setae__%al")
    ("movzbl_%al,%eax")))

(define (i386:b?->accu)
  '(("setb___%al")
    ("movzbl_%al,%eax")))

(define (i386:be?->accu)
  '(("setbe__%al")
    ("movzbl_%al,%eax")))

(define (i386:jump-nz label)
  `(("jne32 " (#:offset ,label))))       ; jnz . + <n>

(define (i386:byte-test-base)
  '(("cmp____%al,%dl")))                ; cmp    %al,%dl

(define (i386:test-base)
  (("cmp____%edx,%eax")))               ; cmp    %edx,%eax

(define (i386:byte-sub-base)
  '(("sub____%dl,%al")))                ; sub    %dl,%al

(define (i386:byte-base-sub)
  `(("sub____%al,%dl")))                ; sub    %al,%dl

(define (i386:sub-base)
  `(("sub____%edx,%eax")))              ; sub    %edx,%eax

(define (i386:base-sub)
  `(("sub____%eax,%edx")))              ; sub    %eax,%edx

(define (i386:nz->accu)
  '(("setne__%al")                      ; setne   %al
    ("movzbl_%al,%eax")))               ; movzbl %al,%eax

(define (i386:z->accu)
  '(("sete___%al")                      ; sete   %al
    ("movzbl_%al,%eax")))               ; movzbl %al,%eax

(define (i386:accu<->stack)
  '(("xchg___%eax,(%esp)")))            ; xchg   %eax,(%esp)

(define (i386:byte-accu)
  '(("movzbl_%al,%eax")))

(define (i386:signed-byte-accu)
  '(("movsbl_%al,%eax")))

(define (i386:word-accu)
  '(("movzwl_%ax,%eax")))

(define (i386:signed-word-accu)
  '(("movswl_%ax,%eax")))



;;;;;;;;;;;;
(define (i386:r0->local info n)
  (or n (error "invalid value: i386:r0->local: " n))
  (let ((r0 (car (if (pair? (.allocated info)) (.allocated info) (.registers info))))
        (n (- 0 (* 4 n))))
    `(,(if (< (abs n) #x80) `(,(string-append "mov____%" r0 ",0x8(%ebp)") (#:immediate1 ,n))
           `(,(string-append "mov____%" r0 ",0x32(%ebp)") (#:immediate ,n))))))

(define (i386:value->r0 info v)
  (or v (error "invalid value: i386:value->r0: " v))
  (let ((r0 (car (if (pair? (.allocated info)) (.allocated info) (.registers info)))))
    `((,(string-append "mov____$i32,%" r0) (#:immediate ,v)))))

(define (i386:r0-zero? info)
  (let ((r0 (car (if (pair? (.allocated info)) (.allocated info) (.registers info)))))
    `((,(string-append "test___%" r0 "," "%" r0)))))

(define (i386:local->r0 info n)
  (or n (error "invalid value: i386:local->r0: " n))
  (let ((r0 (car (if (pair? (.allocated info)) (.allocated info) (.registers info))))
        (n (- 0 (* 4 n))))
    `(,(if (< (abs n) #x80) `(,(string-append "mov____0x8(%ebp),%" r0) (#:immediate1 ,n))
           `(,(string-append "mov____0x32(%ebp),%" r0) (#:immediate ,n))))))

(define i386:instructions
  `(
    (call-label . ,i386:call-label)
    (function-preamble . ,i386:function-preamble)
    (function-locals . ,i386:function-locals)
    (local->r0 . ,i386:local->r0)
    (r0->local . ,i386:r0->local)
    (r0-zero? . ,i386:r0-zero?)
    (ret . ,i386:ret)
    (value->r0 . ,i386:value->r0)
    ))
