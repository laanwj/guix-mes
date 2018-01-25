(define (read) 1)
(exit
 ((lambda ()
    (define read (lambda () 0))
    (read))))
