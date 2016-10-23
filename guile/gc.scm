
(define-module (guile gc))

(define (R) (reload-module (current-module)))

(define gc-size 20)
(define the-cars (make-vector gc-size))
(define the-cdrs (make-vector gc-size))
(define gc-free 0)
(define (show-gc)
  (display "free:") (display gc-free) (newline)
  (display "cars:") (display the-cars) (newline))
(show-gc)

(define (make-cell type . x)
  (cons type (if (pair? x) (car x))))

(define (gc-alloc)
  ((lambda (index)
     (set! gc-free (+ gc-free 1))
     ;;(cons 'cell index)
     (make-cell *unspecified* index)
     )
   gc-free))

(define (gc-make-number x)
  ((lambda (cell)
     (vector-set! the-cars (cdr cell) (make-cell 'number x))
     cell)
   (gc-alloc)))

(display (gc-make-number 3)) (newline)
(show-gc)
