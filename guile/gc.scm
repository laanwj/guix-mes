
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

(define cell-type car)
(define cell-index cdr)

(define (make-cell type . x)
  (cons type (if (pair? x) (cell-type x) '*)))

(define (gc-alloc)
  ((lambda (index)
     (set! gc-free (+ gc-free 1))
     (make-cell '* index))
   gc-free))

(define (gc-make-number x)
  ((lambda (cell)
     (vector-set! the-cars (cell-index cell) (make-cell 'n x))
     cell)
   (gc-alloc)))

(define (gc-cons x y)
  ((lambda (cell)
     ((lambda (pair)
      (vector-set! the-cars (cell-index cell) pair)
      (vector-set! the-cars (cell-index cell) (make-cell 'p (cell-index x)))
      (vector-set! the-cdrs (cell-index cell) (make-cell 'p (cell-index y)))
      pair)
      (make-cell 'p (cell-index cell))))
   (gc-alloc)))

(define (gc-car c)
  (if (eq? (cell-type c) 'p) (vector-ref the-cars
                                         (cell-index
                                          (vector-ref the-cars (cell-index c))))))

(define (gc-cdr c)
  (if (eq? (cell-type c) 'p) (vector-ref the-cars
                                         (cell-index
                                          (vector-ref the-cdrs (cell-index c))))))

(display (gc-make-number 7)) (newline)
(define first (gc-make-number 8)) (newline)
(show-gc)
(define second (gc-make-number 9)) (newline)
(show-gc)
(define pair (gc-cons first second))
(show-gc)
(display "pair:") (display pair) (newline)

(display "car:") (display (gc-car pair)) (newline)
(display "cdr:") (display (gc-cdr pair)) (newline)
