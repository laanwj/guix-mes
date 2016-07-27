(define mes '(0 1))

(define-macro (defined? x)
  `(assq ,x (cddr (current-module))))

(define (cond-expand-expander clauses)
  (let loop ((clauses clauses))
    (if (defined? (caar clauses))
        (eval (cons 'begin (cdar clauses)) (current-module))
        (loop (cdr clauses)))))

(define-macro (cond-expand . clauses)
  `(cond-expand-expander (quote ,clauses)))
