
(define-module (guile gc))

(define (R) (reload-module (current-module)))

(define gc-size 10)
(define the-cars (make-vector gc-size '(* . *)))
(define the-cdrs (make-vector gc-size '(* . *)))
(define gc-free 0)
(define (show-gc)
  (display "\nfree:") (display gc-free) (newline)
  (display "cars:") (display the-cars) (newline)
  (display "cdrs:") (display the-cdrs) (newline))
(show-gc)

(define (cell-type c) (car (gc-cell c)))
(define cell-index cdr)
(define (cell-value c) (cdr (gc-cell c)))

(define (make-cell type . x)
  (cons type (if (pair? x) (car x) '*)))

(define (gc-alloc)
  ((lambda (index)
     (set! gc-free (+ gc-free 1))
     (make-cell '* index))
   gc-free))

(define (make-number x)
  ((lambda (cell)
     (vector-set! the-cars (cell-index cell) (make-cell 'n x))
     cell)
   (gc-alloc)))

(define (make-symbol x)
  ((lambda (cell)
     (vector-set! the-cars (cell-index cell) (make-cell 's x))
     cell)
   (gc-alloc)))

(define (gc-cons x y)
  ((lambda (cell)
     (vector-set! the-cars (cell-index cell) (make-cell 'p (cell-index x)))
     (vector-set! the-cdrs (cell-index cell) y)
     cell)
   (gc-alloc)))

(define (gc-car c)
  (vector-ref the-cars (cell-index c)))

(define (gc-cdr c)
  (vector-ref the-cdrs (cell-index c)))

(define gc-cell gc-car)
(define (gc-pair? c)
  (eq? (cell-type c) 'p))

(define (gc-set-car! c x)
  (if (gc-pair? c) (vector-set! the-cars (cell-index c) x)))

(define (gc-set-cdr! c x)
  (if (gc-pair? c) (vector-set! the-cdrs (cell-index c) x)))

(display "number 7=") (display (make-number 7)) (newline)
(define first (make-number 8)) (newline)
(show-gc)
(define second (make-number 9)) (newline)
(show-gc)
(define pair (gc-cons first second))
(show-gc)
(display "pair:") (display pair) (newline)

(display "car:") (display (gc-car pair)) (newline)
(display "cdr:") (display (gc-cdr pair)) (newline)

(define (gc-null? x) (eq? (car x) 'e))

(define gc-nil (make-cell 'e 0))
(display "nil: ") (display gc-nil) (newline)

(define (gc-list . rest)
  (if (null? rest) gc-nil
      (gc-cons (car rest) (apply gc-list (cdr rest)))))

(define lst (gc-list (make-symbol 'a) (make-symbol 'b) (make-symbol 'c)))
(display "lst:") (display lst) (newline)
(show-gc)

(define (gc-display x . cont?)
  (if (gc-pair? x) (begin (if (null? cont?) (display "("))
                          (gc-display (gc-car x))
                          (if (gc-pair? (gc-cdr x)) (display " "))
                          (gc-display (gc-cdr x) #t)
                          (if (null? cont?) (display ")")))
      (if (gc-null? x) (if (not cont?) (display "()"))
          (display (cell-value x)))))
(display "gc-display lst=") (gc-display lst) (newline)
(show-gc)
