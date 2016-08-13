(define (find pred lst)
  (let loop ((lst lst))
    (if (null? lst) #f
        (if (pred (car lst)) (car lst)
            (loop (cdr lst))))))

(define (filter pred lst)
  (let loop ((lst lst))
    (if (null? lst) '()
        (if (pred (car lst))
            (cons (car lst) (loop (cdr lst)))
            (loop (cdr lst))))))

(define (append-map f lst)
  (apply append (map f lst)))
