
(define (cons* . rest)
  (if (null? (cdr rest)) (car rest)
      (cons (car rest) (core:apply cons* (cdr rest) (current-module)))))

(define (caar x) (car (car x)))
(define (cadr x) (car (cdr x)))
(define (cdar x) (cdr (car x)))
(define (cddr x) (cdr (cdr x)))

(define <cell:symbol> 11)
(define (symbol? x)
  (eq? (core:type x) <cell:symbol>))

(define (map f lst)
  (if (null? lst) (list)
      (cons (f (car lst)) (map f (cdr lst)))))

(define-macro (simple-let bindings . rest)
  (cons (cons 'lambda (cons (map car bindings) rest))
        (map cadr bindings)))

;; (define-macro (xsimple-let bindings rest)
;;   `(,`(lambda ,(map car bindings) ,@rest)
;;     ,@(map cadr bindings)))

(define-macro (xsimple-let bindings rest)
  (cons* (cons* (quote lambda)
                (map car bindings) (append2 rest (quote ())))
         (append2 (map cadr bindings) (quote ()))))

;; (define-macro (xnamed-let name bindings rest)
;;   `(simple-let ((,name *unspecified*))
;;      (set! ,name (lambda ,(map car bindings) ,@rest))
;;      (,name ,@(map cadr bindings))))

(define-macro  (xnamed-let name bindings rest)
  (list (quote simple-let)
        (list (cons* name (quote (*unspecified*))))
        (list (quote set!)
              name
              (cons* (quote lambda)
                     (map car bindings)
                     (append2 rest (quote ()))))
        (cons* name (append2 (map cadr bindings) (quote ())))))

;; (define-macro (let bindings-or-name . rest)
;;   (if (symbol? bindings-or-name)
;;       `(xnamed-let ,bindings-or-name ,(car rest) ,(cdr rest))
;;       `(xsimple-let ,bindings-or-name ,rest)))

(define-macro (let bindings-or-name . rest)
  (if (symbol? bindings-or-name) (list (quote xnamed-let) bindings-or-name (car rest) (cdr rest))
      (list (quote xsimple-let) bindings-or-name rest)))

(define ss-memq-inner #f)
(define (ss-memq x lst)
  (if (null? lst) #f ;; IF
      (if (eq? x (car lst)) lst
          (ss-memq-inner x (cdr lst)))))

(define (ss-memq-inner x lst)
  (if (null? lst) #f ;; IF
      (if (eq? x (car lst)) lst
          (ss-memq-inner x (cdr lst)))))

(define (ss-list-head x n)
  (if (= 0 n) '()
      (cons (car x) (ss-list-head (cdr x) (- n 1)))))

;; (define (foo x y)
;;   (cons x y))

;; (define (ss-list-head x n)
;;   (if (= 0 n) '()
;;       (foo (car x) (ss-list-head (cdr x) (- n 1)))))

(define (string->list s)
  (core:car s))

(define <cell:string> 10)

(define (list->string lst)
  (core:make-cell <cell:string> lst 0))

(define (not x) (if x #f #t))

(define (string-split s c)
  (let loop ((lst (string->list s)) (result '()))
    (let ((rest (ss-memq c lst)))
      (if (not rest) (append2 result (list (list->string lst)))
          (loop (cdr rest)
                (append2 result
                         (list (list->string (ss-list-head lst (- (length lst) (length rest)))))))))))

(core:display-error "*START*\n")
(string-split "foo bar" #\space)
(string-split "baz bla" #\space)
