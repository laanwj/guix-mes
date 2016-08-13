;; rnrs compatibility
(define (bytevector-u32-native-set! bv index value)
  (when (not (= 0 index)) (error "bytevector-u32-native-set! index not zero: " index " value: " value))
  (let ((x (list
            (modulo value #x100)
            (quotient (modulo value #x10000) #x100)
            (quotient (modulo value #x1000000) #x10000)
            (quotient value #x1000000))))
    (set-car! bv (car x))
    (set-cdr! bv (cdr x))
    x))

(define (bytevector-u16-native-set! bv index value)
  (when (not (= 0 index)) (error "bytevector-u16-native-set! index not zero: " index " value: " value))
  (let ((x (list (modulo value #x100)
                 (quotient (modulo value #x10000) #x100))))
    (set-car! bv (car x))
    (set-cdr! bv (cdr x))
    x))

(define (make-bytevector length)
  (make-list length 0))
