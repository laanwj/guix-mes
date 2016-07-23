;; Copyright (c) 1993 by Richard Kelsey and Jonathan Rees.  See file COPYING.

;; There's no implicit name concatenation, so it can be defined
;; entirely using syntax-rules.  Example:
;;  (define-record-type foo
;;    (make-foo x y)
;;    foo?              - predicate name is optional
;;    (x foo-x)
;;    (y foo-y)
;;    (z foo-z set-foo-z!))

;; Copyright (c) 1993-2004 by Richard Kelsey and Jonathan Rees. See file COPYING.

(define-syntax define-record-type
  (syntax-rules ()
    ((define-record-type type
       (constructor arg ...)
       (field . field-stuff)
       ...)
     (begin (define type (make-record-type 'type '(field ...)))
            (define constructor (record-constructor type '(arg ...)))
            (define-accessors type (field . field-stuff) ...)))
    ((define-record-type type
       (constructor arg ...)
       pred
       more ...)
     (begin (define-record-type type
              (constructor arg ...)
              more ...)
            (define pred (record-predicate type))))))

;; Straightforward version
(define-syntax define-accessors
  (syntax-rules ()
    ((define-accessors type field-spec ...)
     (begin (define-accessor type . field-spec) ...))))

(define-syntax define-accessor
  (syntax-rules ()
    ((define-accessor type field accessor)
     (define accessor (record-accessor type 'field)))
    ((define-accessor type field accessor modifier)
     (begin (define accessor (record-accessor type 'field))
            (define modifier (record-modifier type 'field))))))
