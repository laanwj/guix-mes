;;; nyacc/lalr.scm
;;;
;;; Copyright (C) 2014-2016 Matthew R. Wette
;;;
;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Lesser General Public
;;; License as published by the Free Software Foundation; either
;;; version 3 of the License, or (at your option) any later version.
;;;
;;; This library is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Lesser General Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public
;;; License along with this library; if not, write to the Free Software
;;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA

;; I need to find way to preserve srconf, rrconf after hashify.
;; compact needs to deal with it ...

(define-module (nyacc lalr)
  #:export-syntax (lalr-spec)
  #:export (*nyacc-version*
	    make-lalr-machine compact-machine hashify-machine 
	    lalr-match-table
	    restart-spec add-recovery-logic!
	    pp-lalr-notice pp-lalr-grammar pp-lalr-machine
	    write-lalr-actions write-lalr-tables
	    pp-rule find-terminal gen-match-table ; used by (nyacc bison)

	    ;; for debugging:
	    with-spec make-LR0-machine
	    its-member its-trans
	    looking-at first-item
	    terminal? non-terminal?
	    range-next
            process-spec
            reserved?
	    )
  #:use-module (ice-9 optargs)
  #:use-module (ice-9 syncase)
  #:use-module ((srfi srfi-1) #:select (fold fold-right remove lset-union
					     lset-intersection lset-difference))
  #:use-module ((srfi srfi-9) #:select (define-record-type))
  #:use-module ((srfi srfi-43) #:select (vector-map vector-for-each vector-any))
  #:use-module (nyacc util)
  )

(define *nyacc-version* "0.72.0")


;; @deffn proxy-? sym rhs
;; @example
;; (LHS (($? RHS))
;; ($P (($$ #f))
;;     ($P RHS ($$ (set-cdr! (last-pair $1) (list $2)) $1)))
;; @end example
(define (proxy-? sym rhs)
  (list sym
	(list '(action #f #f (list)))
	rhs))

;; @deffn proxy-+ sym rhs
;; @example
;; (LHS (($* RHS))
;; ($P (($$ '()))
;;     ($P RHS ($$ (set-cdr! (last-pair $1) (list $2)) $1)))
;; @end example
(define (proxy-* sym rhs)
  (if (pair? (filter (lambda (elt) (eqv? 'action (car elt))) rhs))
      (error "no RHS action allowed")) ;; rhs
  (list
   sym
   (list '(action #f #f (list)))
   (append (cons (cons 'non-terminal sym) rhs)
	   (list '(action #f #f
			  (set-cdr! (last-pair $1) (list $2))
			  $1)))))

;; @deffn proxy-+ sym rhs
;; @example
;; (LHS (($+ RHS))
;; ($P (RHS ($$ (list $1)))
;;     ($P RHS ($$ (set-cdr! (last-pair $1) (list $2)) $1)))
;; @end example
(define (proxy-+ sym rhs)
  (if (pair? (filter (lambda (elt) (eq? 'action (car elt))) rhs))
      (error "no RHS action allowed")) ;; rhs
  (list
   sym
   (append rhs (list '(action #f #f (list $1))))
   (append (cons (cons 'non-terminal sym) rhs)
	   (list '(action #f #f
			  (set-cdr! (last-pair $1) (list $2))
			  $1)))))

;; @deffn reserved? grammar-symbol
;; Determine whether the syntax argument is a reserved symbol, that is.
;; So instead of writing @code{'$fixed} for syntax one can write
;; @code{$fixed}.  We may want to change this to
;; @example
;; (reserved-terminal? grammar-symbol)
;; (reserved-non-term? grammar-symbol)
;; @end example
(define (reserved? grammar-symbol)
  ;; If the first character `$' then it's reserved.
  (eqv? #\$ (string-ref (symbol->string (syntax->datum grammar-symbol)) 0)))
  
;; @deffn lalr-spec grammar => spec
;; This routine reads a grammar in a scheme-like syntax and returns an a-list.
;; This spec' can be an input for @item{make-parser-generator} or 
;; @item{pp-spec}.
;;.This will return the specification.  Notably the grammar will have rhs
;; arguments decorated with type (e.g., @code{(terminal . #\,)}).
;; Each production rule in the grammar will be of the form
;; @code{(lhs rhs1 rhs2 ...)} where each element of the RHS is one of
;; @itemize
;; @item @code{('terminal . atom)}
;; @item @code{('non-terminal . symbol)}
;; @item @code{('action . (ref narg guts)}
;; @item @code{('proxy . production-rule)}
;; @end itemize
;; Currently, the number of arguments for items is computed in the routine
;; @code{process-grammar}.
(define-syntax lalr-spec
  (syntax-rules +++ () 
    ((_ <expr> +++)
     (let* ()
       (letrec-syntax
	   ((with-attr-list
	     (syntax-rules ($prune)
	       ((_ ($prune <symb>) <ex> ...)
		(cons '(prune . <symb>) (with-attr-list <ex> ...)))
	       ((_) '())))
	    (parse-rhs
	     (lambda (x)
	       ;; The following is syntax-case because we use a fender.
	       (syntax-case x (quote $$ $$/ref $$-ref $prec $with $empty
				     $? $* $+)
		 ;; action specifications
		 ((_ ($$ <guts> ...) <e2> ...)
		  #'(cons '(action #f #f <guts> ...) (parse-rhs <e2> ...)))
		 ((_ ($$-ref <ref>) <e2> ...)
		  ;;#'(cons '(action #f <ref> #f) (parse-rhs <e2> ...)))
		  #'(cons `(action #f ,<ref> . #f) (parse-rhs <e2> ...)))
		 ((_ ($$/ref <ref> <guts> ...) <e2> ...)
		  #'(cons `(action #f ,<ref> <guts> ...) (parse-rhs <e2> ...)))

		 ;; other internal $-syntax
		 ((_ ($prec <tok>) <e2> ...)
		  #'(cons (cons 'prec (tokenize <tok>)) (parse-rhs <e2> ...)))
		 ((_ ($with <lhs-ref> <ex> ...) <e2> ...)
		  #'(cons `(with <lhs-ref> ,@(with-attr-list <ex> ...))
			  (parse-rhs <e2> ...)))
		 ((_ $empty <e2> ...)	; TODO: propagate to processor
		  #'(parse-rhs <e2> ...))
		 
		 ;; (experimental) proxies
		 ((_ ($? <s1> <s2> ...) <e2> ...)
		  #'(cons (cons* 'proxy proxy-? (parse-rhs <s1> <s2> ...))
			  (parse-rhs <e2> ...)))
		 ((_ ($+ <s1> <s2> ...) <e2> ...)
		  #'(cons (cons* 'proxy proxy-+ (parse-rhs <s1> <s2> ...))
			  (parse-rhs <e2> ...)))
		 ((_ ($* <s1> <s2> ...) <e2> ...)
		  #'(cons (cons* 'proxy proxy-* (parse-rhs <s1> <s2> ...))
			  (parse-rhs <e2> ...)))
		 
		 ;; terminals and non-terminals
		 ((_ (quote <e1>) <e2> ...)
		  #'(cons '(terminal . <e1>) (parse-rhs <e2> ...)))
		 ((_ (<f> ...) <e2> ...)
		  #'(cons (<f> ...) (parse-rhs <e2> ...)))
		 ((_ <e1> <e2> ...)
		  (identifier? (syntax <e1>)) ; fender to trap non-term's
		  (if (reserved? (syntax <e1>))
		      #'(cons '(terminal . <e1>) (parse-rhs <e2> ...))
		      #'(cons '(non-terminal . <e1>) (parse-rhs <e2> ...))))
		 ((_ <e1> <e2> ...)
		  #'(cons '(terminal . <e1>) (parse-rhs <e2> ...)))
		 ((_) #'(list)))))
	    (parse-rhs-list
	     (syntax-rules ()
	       ((_ (<ex> ...) <rhs> ...)
		(cons (parse-rhs <ex> ...)
		      (parse-rhs-list <rhs> ...)))
	       ((_) '())))
	    (parse-grammar
	     (syntax-rules ()
	       ((_ (<lhs> <rhs> ...) <prod> ...)
		(cons (cons '<lhs> (parse-rhs-list <rhs> ...))
		      (parse-grammar <prod> ...)))
	       ((_) '())))
	    (tokenize
	     (lambda (x)
	       (syntax-case x ()
		 ((_ <tk>) (identifier? (syntax <tk>)) #'(quote <tk>))
		 ((_ <tk>) #'<tk>))))
	    (tokenize-list
	     (syntax-rules ()
	       ((_ <tk1> <tk2> ...)
		(cons (tokenize <tk1>) (tokenize-list <tk2> ...)))
	       ((_) '())))
	    (parse-precedence
	     (syntax-rules (left right nonassoc)
	       ((_ (left <tk> ...) <ex> ...)
		(cons (cons 'left (tokenize-list <tk> ...))
		      (parse-precedence <ex> ...)))
	       ((_ (right <tk> ...) <ex> ...)
		(cons (cons 'right (tokenize-list <tk> ...))
		      (parse-precedence <ex> ...)))
	       ((_ (nonassoc <tk> ...) <ex> ...)
		(cons (cons 'nonassoc (tokenize-list <tk> ...))
		      (parse-precedence <ex> ...)))
	       ((_ <tk> <ex> ...)
		(cons (list 'undecl (tokenize <tk>))
		      (parse-precedence <ex> ...)))
	       ((_) '())))
	    (lalr-spec-1
	     (syntax-rules (start expect notice prec< prec> grammar)
	       ((_ (start <symb>) <e> ...)
		(cons (cons 'start '<symb>) (lalr-spec-1 <e> ...)))
	       ((_ (expect <n>) <e> ...)
		(cons (cons 'expect <n>) (lalr-spec-1 <e> ...)))
	       ((_ (notice <str>) <e> ...)
		(cons (cons 'notice <str>) (lalr-spec-1 <e> ...)))
	       ((_ (prec< <ex> ...) <e> ...)
		(cons (cons 'precedence (parse-precedence <ex> ...))
		      (lalr-spec-1 <e> ...)))
	       ((_ (prec> <ex> ...) <e> ...)
		(cons (cons 'precedence (reverse (parse-precedence <ex> ...)))
		      (lalr-spec-1 <e> ...)))
	       ((_ (grammar <prod> ...) <e> ...)
		(cons (cons 'grammar (parse-grammar <prod> ...))
		      (lalr-spec-1 <e> ...))) 
	       ((_) '()))))
	 (process-spec (lalr-spec-1 <expr> +++)))))))

;; @deffn atomize terminal => object
;; Generate an atomic object for a terminal.   Expected terminals are strings,
;; characters and symbols.  This will convert the strings @code{s} to symbols
;; of the form @code{'$:s}.
(define (atomize terminal)
  (if (string? terminal)
      (string->symbol (string-append "$:" terminal))
      terminal))

;; @deffn normize terminal => char|symbol
;; Normalize a token. This routine will normalize tokens in order to check
;; for similarities. For example, @code{"+"} and @code{#\+} are similar,
;; @code{'foo} and @code{"foo"} are similar.
(define (normize terminal)
  (if (not (string? terminal)) terminal
      (if (= 1 (string-length terminal))
	  (string-ref terminal 0)
	  (string->symbol terminal))))

;; @deffn eqv-terminal? a b
;; This is a predicate to determine if the terminals @code{a} and @code{b}
;; are equivalent.
(define (eqv-terminal? a b)
  (eqv? (atomize a) (atomize b)))

;; @deffn find-terminal symb term-l => term-symb
;; Find the terminal in @code{term-l} that is equivalent to @code{symb}.
(define (find-terminal symb term-l)
  (let iter ((tl term-l))
    (if (null? tl) #f
	(if (eqv-terminal? symb (car tl)) (car tl)
	    (iter (cdr tl))))))
  
;; @deffn process-spec tree => specification (as a-list)
;; Here we sweep through the production rules. We flatten and order the rules
;; and place all p-rules with like LHSs together.  There is a non-trivial
;; amount of extra code to deal with mid-rule actions (MRAs).
(define (process-spec tree)

  ;; Make a new symbol. This is a helper for proxies and mid-rule-actions.
  ;; The counter here is the only @code{set!} in @code{process-spec}.
  ;; Otherwise, I believe @code{process-spec} is referentially transparent.
  (define maksy
    (let ((cntr 1))
      (lambda ()
	(let ((c cntr))
	  (set! cntr (1+ cntr))
	  (string->symbol (string-append "$P" (number->string c)))))))

  ;; Canonicalize precedence and associativity. Precedence will appear
  ;; as sets of equivalent items in increasing order of precedence
  ;; (e.g., @code{((+ -) (* /)}).  The input tree has nodes that look like
  ;; @example
  ;; '(precedence (left "+" "-") (left "*" "/"))
  ;; '(precedence ('then "else")
  ;; @end example
  ;; @noindent
  ;; =>
  ;; @example
  ;; (prec ((+ -) (* /)) ((then) (else)))
  ;; @end example
  (define (prec-n-assc tree)
    ;; prec-l; lt-assc-l rt-assc-l non-assc-l pspec
    (let iter ((pll '()) (pl '()) (la '()) (ra '()) (na '())
	       (spec '()) (tree tree))
      (cond
       ((pair? spec)
	;; item ~ ('left "+" "-") => a ~ 'left, tl ~ (#\+ #\-)
	(let* ((item (car spec)) (as (car item)) (tl (map atomize (cdr item))))
	  (case as
	    ((left)
	     (iter pll (cons tl pl) (append tl la) ra na (cdr spec) tree))
	    ((right)
	     (iter pll (cons tl pl) la (append tl ra) na (cdr spec) tree))
	    ((nonassoc)
	     (iter pll (cons tl pl) la ra (append tl na) (cdr spec) tree))
	    ((undecl)
	     (iter pll (cons tl pl) la ra na (cdr spec) tree)))))
       ((pair? pl)
	(iter (cons (reverse pl) pll) '() la ra na spec tree))
       ((pair? tree)
	(iter pll pl la ra na
	      (if (eqv? 'precedence (caar tree)) (cdar tree) '()) (cdr tree)))
       (else
	(list
	 `(prec . ,(reverse pll))
	 `(assc (left ,@la) (right ,@ra) (nonassoc ,@na)))))))

  ;;.@deffn make-mra-proxy sy pel act => ???
  ;; Generate a mid-rule-action proxy.
  (define (make-mra-proxy sy pel act)
    (list sy (list (cons* 'action (length pel) (cdr act)))))

  ;; @deffn gram-check-2 tl nl err-l
  ;; Check for fatal: symbol used as terminal and non-terminal.
  (define (gram-check-2 tl nl err-l)
    (let ((cf (lset-intersection eqv? (map atomize tl) nl)))
      (if (pair? cf)
	  (cons (fmtstr "*** symbol is terminal and non-terminal: ~S" cf)
		err-l) err-l)))
	       
  ;; @deffn gram-check-3 ll nl err-l
  ;; Check for fatal: non-terminal's w/o production rule.
  (define (gram-check-3 ll nl err-l)
    (fold
     (lambda (n l)
       (if (not (memq n ll))
	   (cons (fmtstr "*** non-terminal with no production rule: ~A" n) l)
	   l))
     err-l nl))

  ;; @deffn gram-check-4 ll nl err-l
  ;; Check for warning: unused LHS.
  ;; TODO: which don't appear in OTHER RHS, e.g., (foo (foo))
  (define (gram-check-4 ll nl err-l)
    (fold
     (lambda (s l) (cons (fmtstr "+++ LHS not used in any RHS: ~A" s) l))
     err-l
     (let iter ((ull '()) (all ll)) ; unused LHSs, all LHS's
       (if (null? all) ull
	   (iter (if (or (memq (car all) nl)
			 (memq (car all) ull)
			 (eq? (car all) '$start))
		     ull (cons (car all) ull))
		 (cdr all))))))

  ;; TODO: check for repeated tokens in precedence spec's: prec<, prec>
	       
  (let* ((gram (assq-ref tree 'grammar))
	 (start-symbol (and=> (assq-ref tree 'start) atomize))
	 (start-rule (lambda () (list start-symbol)))
	 (add-el (lambda (e l) (if (memq e l) l (cons e l))))
	 (pna (prec-n-assc tree)))
    ;; We sweep through the grammar to generate a canonical specification.
    ;; Note: the local rhs is used to hold RHS terms, but a
    ;; value of @code{'()} is used to signal "add rule", and a value of
    ;; @code{#f} is used to signal ``done, proceed to next rule.''
    ;; We use @code{tail} below to go through all remaining rules so that any
    ;; like LHS get absorbed before proceeding: This keeps LHS in sequence.
    ;; Note: code-comm and lone-comm are added to terminals so that they end
    ;; up in the match-table.  The parser will skip these if the automoton has
    ;; no associated transitions for these.  This allows users to parse for
    ;; comments in some rules but skip the rest.
    (let iter ((ll '($start))		; LHS list
	       (@l (list		; attributes per prod' rule
		    `((rhs . ,(vector start-symbol))
		      (ref . all) (act 1 $1))))
	       (tl '($code-comm $lone-comm $error $end)) ; set of terminals
	       (nl (list start-symbol))	; set of non-terminals
	       ;;
	       (head gram)	       ; head of unprocessed productions
	       (prox '())	       ; proxy productions for MRA
	       (lhs #f)		       ; current LHS (symbol)
	       (tail '())	       ; tail of grammar productions
	       (rhs-l '())	       ; list of RHSs being processed
	       (attr '())	       ; per-rule attributes (action, prec)
	       (pel '())	       ; processed RHS terms: '$:if ...
	       (rhs #f))	       ; elts to process: (terminal . '$:if) ...
      (cond
       ((pair? rhs)
	;; Capture info on RHS term.
	(case (caar rhs)
	  ((terminal)
	   (iter ll @l (add-el (cdar rhs) tl) nl head prox lhs tail
		 rhs-l attr (cons (atomize (cdar rhs)) pel) (cdr rhs)))
	  ((non-terminal)
	   (iter ll @l tl (add-el (cdar rhs) nl) head prox lhs tail
		 rhs-l attr (cons (cdar rhs) pel) (cdr rhs)))
	  ((action)
	   (if (pair? (cdr rhs))
	       ;; mid-rule action: generate a proxy (car act is # args)
	       (let* ((sy (maksy))
		      (pr (make-mra-proxy sy pel (cdar rhs))))
		 (iter ll @l tl (cons sy nl) head (cons pr prox)
		       lhs tail rhs-l attr (cons sy pel) (cdr rhs)))
	       ;; end-rule action
	       (iter ll @l tl nl head prox lhs tail
		     rhs-l (acons 'action (cdar rhs) attr) pel (cdr rhs))))
	  ((proxy)
	   (let* ((sy (maksy))
		  (pf (cadar rhs))	; proxy function
		  (p1 (pf sy (cddar rhs))))
	     (iter ll @l tl (cons sy nl) head (cons p1 prox) lhs
		   tail rhs-l attr (cons sy pel) (cdr rhs))))
	  ((prec)
	   (iter ll @l (add-el (cdar rhs) tl) nl head prox lhs tail rhs-l
		 (acons 'prec (atomize (cdar rhs)) attr) pel (cdr rhs)))
	  ((with)
	   (let* ((psy (maksy))		      ; proxy symbol
		  (rhsx (cadar rhs))	      ; symbol to expand
		  (p-l (map cdr (cddar rhs))) ; prune list
		  (p1 (list psy `((non-terminal . ,rhsx)
				  (action #f #f $1)))))
	     (iter ll @l tl (cons psy nl) head (cons p1 prox) lhs tail rhs-l
		   (acons 'with (cons psy p-l) attr) (cons psy pel) (cdr rhs))))
	  (else
	   (error (fmtstr "bug=~S" (caar rhs))))))

       ((null? rhs)
	;; End of RHS items for current rule.
	;; Add the p-rules items to the lists ll, rl, xl, and @@l.
	;; @code{act} is now:
	;; @itemize
	;; @item for mid-rule-action: (narg ref code)
	;; @item for end-rule-action: (#f ref code)
	;; @end itemize
	(let* ((ln (length pel))
	       (action (assq-ref attr 'action))
	       (with (assq-ref attr 'with))
	       (nrg (if action (or (car action) ln) ln))  ; number of args
	       (ref (if action (cadr action) #f))
	       (act (cond
		     ((and action (cddr action)) (cddr action))
		     ;; if error rule then default action is print err msg:
		     ((memq '$error pel) '((display "syntax error\n")))
		     ((zero? nrg) '((list)))
		     (else '($1)))))
	  (if with (simple-format #t "WITH WHAT?\n"))
	  (iter (cons lhs ll)
		(cons
		 (cons* (cons 'rhs (list->vector (reverse pel)))
			(cons* 'act nrg act) (cons 'ref ref) attr)
		 @l)
		tl nl head prox lhs tail rhs-l attr pel #f)))

       ((pair? rhs-l)
	;; Work through next RHS.
	(iter ll @l tl nl head prox lhs tail
	      (cdr rhs-l) '() '() (car rhs-l)))

       ((pair? tail)
	;; Check the next CAR of the tail.  If it matches
	;; the current LHS process it, else skip it.
	(iter ll @l tl nl head prox lhs (cdr tail) 
	      (if (eqv? (caar tail) lhs) (cdar tail) '())
	      attr pel #f))

       ((pair? prox)
	;; If a proxy then we have ((lhs RHS) (lhs RHS))
	(iter ll @l tl nl (cons (car prox) head) (cdr prox)
	      lhs tail rhs-l attr pel rhs))

       ((pair? head)
	;; Check the next rule-set.  If the lhs has aready
	;; been processed, then skip.  Otherwise, copy copy
	;; to tail and process.
	(let ((lhs (caar head)) (rhs-l (cdar head))
	      (rest (cdr head)))
	  (if (memq lhs ll)
	      (iter ll @l tl nl rest prox #f '() '() attr pel #f)
	      (iter ll @l tl nl rest prox lhs rest rhs-l attr pel rhs))))

       (else
	(let* ((al (reverse @l))	; attribute list
	       (err-1 '()) ;; not used
	       ;; symbol used as terminal and non-terminal:
	       (err-2 (gram-check-2 tl nl err-1))
	       ;; non-terminal's w/o production rule:
	       (err-3 (gram-check-3 ll nl err-2))
	       ;; TODO: which don't appear in OTHER RHS, e.g., (foo (foo))
	       (err-4 (gram-check-4 ll nl err-3))
	       ;; todo: Check that with withs are not mixed
	       (err-l err-4))
	  (for-each (lambda (e) (fmterr "~A\n" e)) err-l)
	  (if (pair? (filter (lambda (s) (char=? #\* (string-ref s 0))) err-l))
	      #f
	      (list
	       ;; most referenced
	       (cons 'non-terms nl)
	       (cons 'lhs-v (list->vector (reverse ll)))
	       (cons 'rhs-v (map-attr->vector al 'rhs))
	       (cons 'terminals tl)
	       (cons 'start start-symbol)
	       (cons 'attr (list
			    (cons 'expect (or (assq-ref tree 'expect) 0))
			    (cons 'notice (assq-ref tree 'notice))))
	       (cons 'prec (assq-ref pna 'prec)) ; lowest-to-highest
	       (cons 'assc (assq-ref pna 'assc))
	       (cons 'prp-v (map-attr->vector al 'prec)) ; per-rule precedence
	       (cons 'act-v (map-attr->vector al 'act))
	       (cons 'ref-v (map-attr->vector al 'ref))
	       (cons 'err-l err-l)))))))))
  
;;; === Code for processing the specification. ================================

;; @subsubheading Note
;; The fluid @code{*lalr-core*} is used during the machine generation
;; cycles to access core parameters of the specification.  This includes
;; the list of non-terminals, the vector of left-hand side symbols and the
;; vector of vector of right-hand side symbols.
(define *lalr-core* (make-fluid #f))

;; This record holds the minimum data from the grammar needed to build the
;; machine from the grammar specification.
(define-record-type lalr-core-type
  (make-lalr-core non-terms terminals start lhs-v rhs-v eps-l)
  lalr-core-type?
  (non-terms core-non-terms)	      ; list of non-terminals
  (terminals core-terminals)	      ; list of non-terminals
  (start core-start)		      ; start non-terminal
  (lhs-v core-lhs-v)		      ; vec of left hand sides
  (rhs-v core-rhs-v)		      ; vec of right hand sides
  (eps-l core-eps-l))		      ; non-terms w/ eps prod's

;; @deffn make-core spec => lalr-core-type
(define (make-core spec)
  (make-lalr-core (assq-ref spec 'non-terms)
		  (assq-ref spec 'terminals)
		  (assq-ref spec 'start)
		  (assq-ref spec 'lhs-v)
		  (assq-ref spec 'rhs-v)
		  '()))

;; @deffn make-core/extras spec => lalr-core-type
;; Add list of symbols with epsilon productions.
(define (make-core/extras spec)
  (let ((non-terms (assq-ref spec 'non-terms))
	(terminals (assq-ref spec 'terminals))
	(start (assq-ref spec 'start))
	(lhs-v (assq-ref spec 'lhs-v))
	(rhs-v (assq-ref spec 'rhs-v)))
    (make-lalr-core non-terms terminals start lhs-v rhs-v
		    (find-eps non-terms lhs-v rhs-v))))


;; @section Routines

;; @deffn <? a b po => #t | #f
;; Given tokens @code{a} and @code{b} and partial ordering @code{po} report
;; if precedence of @code{b} is greater than @code{a}?
(define (<? a b po)
  (if (member (cons a b) po) #t
      (let iter ((po po))
	(if (null? po) #f
	    (if (and (eqv? (caar po) a)
		     (<? (cdar po) b po))
		#t
		(iter (cdr po)))))))

;; @deffn prece a b po
;; Return precedence for @code{a,b} given the partial order @code{po} as
;; @code{#\<}, @code{#\>}, @code{#\=} or @code{#f}.
;; This is not a true partial order as we can have a<b and b<a => a=b.
;; @example
;; @code{(prece a a po)} => @code{#\=}.
;; @end example
(define (prece a b po)
  (cond
   ((eqv? a b) #\=)
   ((eqv? a '$error) #\<)
   ((eqv? b '$error) #\>)
   ((<? a b po)  (if (<? b a po) #\= #\<))
   (else (if (<? b a po) #\> #f))))
  
;; @deffn non-terminal? symb
(define (non-terminal? symb)
  (cond
   ((eqv? symb '$epsilon) #t)
   ((eqv? symb '$end) #f)
   ((eqv? symb '$@) #f)
   ((string? symb) #f)
   (else
    (memq symb (core-non-terms (fluid-ref *lalr-core*))))))

;; @deffn terminal? symb
(define (terminal? symb)
  (not (non-terminal? symb)))

;; @deffn prule-range lhs => (start-ix . (1+ end-ix))
;; Find the range of productiion rules for the lhs.
;; If not found raise error.
(define (prule-range lhs)
  ;; If this needs to be really fast then we move to where lhs is an integer
  ;; and that used to index into a table that provides the ranges.
  (let* ((core (fluid-ref *lalr-core*))
	 (lhs-v (core-lhs-v core))
	 (n (vector-length lhs-v))
	 (match? (lambda (ix symb) (eqv? (vector-ref lhs-v ix) symb))))
    (cond
     ((terminal? lhs) '())
     ((eq? lhs '$epsilon) '())
     (else
      (let iter-st ((st 0))
	;; Iterate to find the start index.
	(if (= st n) '()		; not found
	    (if (match? st lhs)
		;; Start found, now iteratate to find end index.
		(let iter-nd ((nd st))
		  (if (= nd n) (cons st nd)
		      (if (not (match? nd lhs)) (cons st nd)
			  (iter-nd (1+ nd)))))
		(iter-st (1+ st)))))))))

;; @deffn range-next rng -> rng
;; Given a range in the form of @code{(cons start (1+ end))} return the next
;; value or '() if at end.  That is @code{(3 . 4)} => @code{'()}.
(define (range-next rng)
  (if (null? rng) '()
      (let ((nxt (cons (1+ (car rng)) (cdr rng))))
	(if (= (car nxt) (cdr nxt)) '() nxt))))

;; @deffn range-last? rng
;; Predicate to indicate last p-rule in range.
;; If off end (i.e., null rng) then #f.
(define (range-last? rng)
  (and (pair? rng) (= (1+ (car rng)) (cdr rng))))

;; @deffn lhs-symb prod-ix
;; Return the LHS symbol for the production at index @code{prod-id}.
(define (lhs-symb gx)
  (vector-ref (core-lhs-v (fluid-ref *lalr-core*)) gx))

;; @deffn looking-at (p-rule-ix . rhs-ix)
;; Return symbol we are looking at for this item state.
;; If at the end (position = -1) (or rule is zero-length) then return
;; @code{'$epsilon}.
(define (looking-at item)
  (let* ((core (fluid-ref *lalr-core*))
	 (rhs-v (core-rhs-v core))
	 (rule (vector-ref rhs-v (car item))))
    (if (last-item? item)
	'$epsilon
	(vector-ref rule (cdr item)))))

;; @deffn first-item gx
;; Given grammar rule index return the first item.
;; This will return @code{(gx . 0)}, or @code{(gx . -1)} if the rule has
;; no RHS elements.
(define (first-item gx)
  (let* ((core (fluid-ref *lalr-core*))
	 (rlen (vector-length (vector-ref (core-rhs-v core) gx))))
    (cons gx (if (zero? rlen) -1 0))))

;; @deffn last-item? item
;; Predictate to indicate last item in (or end of) production rule.
(define (last-item? item)
  (negative? (cdr item)))

;; @deffn next-item item
;; Return the next item in the production rule.
;; A position of @code{-1} means the end.  If at end, then @code{'()}
(define (next-item item)
  (let* ((core (fluid-ref *lalr-core*))
	 (gx (car item)) (rx (cdr item)) (rxp1 (1+ rx))
	 (rlen (vector-length (vector-ref (core-rhs-v core) gx))))
    (cond
     ((negative? rx) '())
     ((eqv? rxp1 rlen) (cons gx -1))
     (else (cons gx rxp1)))))

;; @deffn prev-item item
;; Return the previous item in the grammar.
;; prev (0 . 0) is currently (0 . 0)
(define (prev-item item)
  (let* ((core (fluid-ref *lalr-core*))
	 (rhs-v (core-rhs-v core))
	 (p-ix (car item))
	 (p-ixm1 (1- p-ix))
	 (r-ix (cdr item))
	 (r-ixm1 (if (negative? r-ix)
		     (1- (vector-length (vector-ref rhs-v p-ix)))
		     (1- r-ix))))
    (if (zero? r-ix)
	(if (zero? p-ix) item		; start, i.e., (0 . 0)
	    (cons p-ixm1 -1))		; prev p-rule
	(cons p-ix r-ixm1))))

;; @deffn error-rule? gx => #t|#f
;; Predicate to indicate if gx rule has @code{$error} as rhs member.
(define (error-rule? gx)
  (let* ((core (fluid-ref *lalr-core*))
	 (rhs-v (core-rhs-v core)))
    (vector-any (lambda (e) (eqv? e '$error)) (vector-ref rhs-v gx))))
     
;; @deffn non-kernels symb => list of prule indices
;; Compute the set of non-kernel rules for symbol @code{symb}.  If grammar
;; looks like
;; @example
;; 1: A => Bcd
;; ...
;; 5: B => Cde
;; ...
;; 7: B => Abe
;; @end example
;; @noindent
;; then @code{non-kernels 'A} results in @code{(1 5 7)}.
;; Note: To support pruning this routine will need to be rewritten.
(define (non-kernels symb)
  (let* ((core (fluid-ref *lalr-core*))
	 (lhs-v (core-lhs-v core))
	 (rhs-v (core-rhs-v core))
	 (glen (vector-length lhs-v))
	 (lhs-symb (lambda (gx) (vector-ref lhs-v gx))))
    (let iter ((rslt '())		; result is set of p-rule indices
	       (done '())		; symbols completed or queued
	       (next '())		; next round of symbols to process
	       (curr (list symb))	; this round of symbols to process
	       (gx 0))			; p-rule index
      (cond
       ((< gx glen)
	(cond
	 ((memq (lhs-symb gx) curr)
	  ;; Add rhs to next and rslt if not already done.
	  (let* ((rhs1 (looking-at (first-item gx))) ; 1st-RHS-sym|$eps
		 (rslt1 (if (memq gx rslt) rslt (cons gx rslt)))
		 (done1 (if (memq rhs1 done) done (cons rhs1 done)))
		 (next1 (cond ((memq rhs1 done) next)
			      ((terminal? rhs1) next)
			      (else (cons rhs1 next)))))
	    (iter rslt1 done1 next1 curr (1+ gx))))
	 (else
	  ;; Nothing to check; process next rule.
	  (iter rslt done next curr (1+ gx)))))
       ((pair? next)
	;; Start another sweep throught the grammar.
	(iter rslt done '() next 0))
       (else
	;; Done, so return.
	(reverse rslt))))))

;; @deffn expand-k-item => item-set
;; Expand a kernel-item into a list with the non-kernels.
(define (expand-k-item k-item)
  (reverse
   (fold (lambda (gx items) (cons (first-item gx) items))
	      (list k-item)
	      (non-kernels (looking-at k-item)))))

;; @deffn its-equal?
;; Helper for step1
(define (its-equal? its-1 its-2)
  (let iter ((its1 its-1) (its2 its-2)) ; cdr to strip off the ind
    (if (and (null? its1) (null? its2)) #t ; completed run through => #f
	(if (or (null? its1) (null? its2)) #f ; lists not equal length => #f
	    (if (not (member (car its1) its-2)) #f ; mismatch => #f
		(iter (cdr its1) (cdr its2)))))))

;; @deffn its-member its its-l
;; Helper for step1
;; If itemset @code{its} is a member of itemset list @code{its-l} return the
;; index, else return #f.
(define (its-member its its-l)
  (let iter ((itsl its-l))
    (if (null? itsl) #f
	(if (its-equal? its (cdar itsl)) (caar itsl)
	    (iter (cdr itsl))))))
  
;; @deffn its-trans itemset => alist of (symb . itemset)
;; Compute transitions from an itemset.   Thatis, map a list of kernel
;; items to a list of (symbol post-shift items).
;; @example
;; ((0 . 1) (2 . 3) => ((A (0 . 2) (2 . 4)) (B (2 . 4) ...))
;; @end example
(define (its-trans items)
  (let iter ((rslt '())			; result
	     (k-items items)		; items
	     (itl '()))			; one k-item w/ added non-kernels
    (cond
     ((pair? itl)
      (let* ((it (car itl))		; item
	     (sy (looking-at it))	; symbol
	     (nx (next-item it))
	     (sq (assq sy rslt)))	; if we have seen it
	(cond
	 ((eq? sy '$epsilon)
	  ;; don't transition end-of-rule items
	  (iter rslt k-items (cdr itl)))
	 ((not sq)
	  ;; haven't seen this symbol yet
	  (iter (acons sy (list nx) rslt) k-items (cdr itl)))
	 ((member nx (cdr sq))
	  ;; repeat
	  (iter rslt k-items (cdr itl)))
	 (else
	  ;; SY is in RSLT and item not yet in: add it.
	  (set-cdr! sq (cons nx (cdr sq)))
	  (iter rslt k-items (cdr itl))))))
     ((pair? k-items)
      (iter rslt (cdr k-items) (expand-k-item (car k-items))))
     (else
      rslt))))

;; @deffn step1 [input-a-list] => p-mach-1
;; Compute the sets of LR(0) kernel items and the transitions associated with
;; spec.  These are returned as vectors in the alist with keys @code{'kis-v}
;; and @code{'kix-v}, repspectively.   Each entry in @code{kis-v} is a list of
;; items in the form @code{(px . rx)} where @code{px} is the production rule
;; index and @code{rx} is the index of the RHS symbol.  Each entry in the
;; vector @code{kix-v} is an a-list with entries @code{(sy . kx)} where
;; @code{sy} is a (terminal or non-terminal) symbol and @code{kx} is the
;; index of the kernel itemset.  The basic algorithm is discussed on
;; pp. 228-229 of the DB except that we compute non-kernel items on the fly
;; using @code{expand-k-item}.  See Example 4.46 on p. 241 of the DB.
(define (step1 . rest)
  (let* ((al-in (if (pair? rest) (car rest) '()))
	 (add-kset (lambda (upd kstz)	; give upd a ks-ix and add to kstz
		     (acons (1+ (caar kstz)) upd kstz)))
	 (init '(0 (0 . 0))))
    (let iter ((ksets (list init))	; w/ index
	       (ktrnz '())		; ((symb src dst) (symb src dst) ...)
	       (next '())		; w/ index
	       (todo (list init))	; w/ index
	       (curr #f)		; current state ix
	       (trans '()))		; ((symb it1 it2 ...) (symb ...))
      (cond
       ((pair? trans)
	;; Check next symbol for transitions (symb . (item1 item2 ...)).
	(let* ((dst (cdar trans))	       ; destination item
	       (dst-ix (its-member dst ksets)) ; return ix else #f
	       (upd (if dst-ix '() (cons (1+ (caar ksets)) dst)))
	       (ksets1 (if dst-ix ksets (cons upd ksets)))
	       (next1 (if dst-ix next (cons upd next)))
	       (dsx (if dst-ix dst-ix (car upd))) ; dest state index
	       (ktrnz1 (cons (list (caar trans) curr dsx) ktrnz)))
	  (iter ksets1 ktrnz1 next1 todo curr (cdr trans))))
       ((pair? todo)
	;; Process the next state (aka itemset).
	(iter ksets ktrnz next (cdr todo) (caar todo) (its-trans (cdar todo))))
       ((pair? next)
	;; Sweep throught the grammar again.
	(iter ksets ktrnz '() next curr '()))
       (else
	(let* ((nkis (length ksets))	; also (caar ksets)
	       (kisv (make-vector nkis #f))
	       (kitv (make-vector nkis '())))
	  ;; Vectorize kernel sets
	  (for-each
	   (lambda (kis) (vector-set! kisv (car kis) (cdr kis)))
	   ksets)
	  ;; Vectorize transitions (by src kx).
	  (for-each
	   (lambda (kit)
	     (vector-set! kitv (cadr kit)
			  (acons (car kit) (caddr kit)
				 (vector-ref kitv (cadr kit)))))
	   ktrnz)
	  ;; Return kis-v, kernel itemsets, and kix-v transitions.
	  (cons* (cons 'kis-v kisv) (cons 'kix-v kitv) al-in)))))))

;; @deffn find-eps non-terms lhs-v rhs-v => eps-l
;; Generate a list of non-terminals which have epsilon productions.
(define (find-eps nterms lhs-v rhs-v)
  (let* ((nprod (vector-length lhs-v))
	 (find-new
	  (lambda (e l)
	    (let iter ((ll l) (gx 0) (lhs #f) (rhs #()) (rx 0))
	      (cond
	       ((< rx (vector-length rhs))
		(if (and (memq (vector-ref rhs rx) nterms) ; non-term
			 (memq (vector-ref rhs rx) ll))	   ; w/ eps prod
		    (iter ll gx lhs rhs (1+ rx)) ; yes: check next
		    (iter ll (1+ gx) #f #() 0))) ; no: next p-rule
	       ((and lhs (= rx (vector-length rhs))) ; we have eps-prod
		(iter (if (memq lhs ll) ll (cons lhs ll)) (1+ gx) #f #() 0))
	       ((< gx nprod)		; check next p-rule if not on list
		(if (memq (vector-ref lhs-v gx) ll)
		    (iter ll (1+ gx) #f #() 0)
		    (iter ll gx (vector-ref lhs-v gx) (vector-ref rhs-v gx) 0)))
	       (else ll))))))
    (fixed-point find-new (find-new #f '()))))

;; @deffn merge1 v l
;; add v to l if not in l
(define	(merge1 v l)
  (if (memq v l) l (cons v l)))

;; @deffn merge2 v l al
;; add v to l if not in l or al
(define (merge2 v l al)	
  (if (memq v l) l (if (memq v al) l (cons v l))))

;; @deffn first symbol-list end-token-list
;; Return list of terminals starting the string @code{symbol-list}
;; (see DB, p. 188).  If the symbol-list can generate epsilon then the
;; result will include @code{end-token-list}.
(define (first symbol-list end-token-list)
  (let* ((core (fluid-ref *lalr-core*))
	 (eps-l (core-eps-l core)))
    ;; This loop strips off the leading symbol from stng and then adds to
    ;; todo list, which results in range of p-rules getting checked for
    ;; terminals.
    (let iter ((rslt '())		; terminals collected
	       (stng symbol-list)	; what's left of input string
	       (hzeps #t)		; if eps-prod so far
	       (done '())		; non-terminals checked
	       (todo '())		; non-terminals to assess
	       (p-range '())		; range of p-rules to check
	       (item '()))		; item in production
      (cond
       ((pair? item)
	(let ((sym (looking-at item)))
	  (cond
	   ((eq? sym '$epsilon)		; at end of rule, go next
	    (iter rslt stng hzeps done todo p-range '()))
	   ((terminal? sym)		; terminal, log it
	    (iter (merge1 sym rslt) stng hzeps done todo p-range '()))
	   ((memq sym eps-l)		; symbol has eps prod
	    (iter rslt stng hzeps (merge1 sym done) (merge2 sym todo done)
		  p-range (next-item item)))
	   (else ;; non-terminal, add to todo/done, goto next
	    (iter rslt stng hzeps
		  (merge1 sym done) (merge2 sym todo done) p-range '())))))
       
       ((pair? p-range)			; next one to do
	;; run through next rule
	(iter rslt stng hzeps done todo
	      (range-next p-range) (first-item (car p-range))))

       ((pair? todo)
	(iter rslt stng hzeps done (cdr todo) (prule-range (car todo)) '()))

       ((and hzeps (pair? stng))
	;; Last pass saw an $epsilon so check the next input symbol,
	;; with saweps reset to #f.
	(let* ((symb (car stng)) (stng1 (cdr stng)) (symbl (list symb)))
	  (if (terminal? symb)
	      (iter (cons symb rslt) stng1
		    (and hzeps (memq symb eps-l))
		    done todo p-range '())
	      (iter rslt stng1
		    (or (eq? symb '$epsilon) (memq symb eps-l))
		    symbl symbl '() '()))))
       (hzeps
	;; $epsilon passes all the way through.
	;; If end-token-list provided use that.
	(if (pair? end-token-list)
	    (lset-union eqv? rslt end-token-list)
	    (cons '$epsilon rslt)))
       (else
	rslt)))))

;; @deffn item->stng item => list-of-symbols
;; Convert item (e.g., @code{(1 . 2)}) to list of symbols to the end of the
;; production(?). If item is at the end of the rule then return
;; @code{'$epsilon}.  The term "stng" is used to avoid confusion about the
;; term string.
(define (item->stng item)
  (if (eqv? (cdr item) -1)
      (list '$epsilon)
      (let* ((core (fluid-ref *lalr-core*))
	     (rhs-v (core-rhs-v core))
	     (rhs (vector-ref rhs-v (car item))))
	(let iter ((res '()) (ix (1- (vector-length rhs))))
	  (if (< ix (cdr item)) res
	      (iter (cons (vector-ref rhs ix) res) (1- ix)))))))

;; add (item . toks) to (front of) la-item-l
;; i.e., la-item-l is unmodified
(define (merge-la-item la-item-l item toks)
  (let* ((pair (assoc item la-item-l))
	 (tokl (if (pair? pair) (cdr pair) '()))
	 (allt ;; union of toks and la-item-l toks
	  (let iter ((tl tokl) (ts toks))
	    (if (null? ts) tl
		(iter (if (memq (car ts) tl) tl (cons (car ts) tl))
		      (cdr ts))))))
    (if (not pair) (acons item allt la-item-l)
	(if (eqv? tokl allt) la-item-l
	    (acons item allt la-item-l)))))

;; @deffn first-following item toks => token-list
;; For la-item A => x.By,z (where  @code{item}, @code{toks}), this
;; procedure computes @code{FIRST(yz)}.
(define (first-following item toks)
  (first (item->stng (next-item item)) toks))

;; @deffn closure la-item-l => la-item-l
;; Compute the closure of a list of la-items.
;; Ref: DB, Fig 4.38, Sec. 4.7, p. 232
(define (closure la-item-l)
  ;; Compute the fixed point of I, aka @code{la-item-l}, with procedure
  ;;    for each item [A => x.By, a] in I
  ;;      each production B => z in G
  ;;      and each terminal b in FIRST(ya)
  ;;      such that [B => .z, b] is not in I do
  ;;        add [B => .z, b] to I
  ;; The routine @code{fixed-point} operates on one element of the input set.
  (prune-assoc
   (fixed-point
    (lambda (la-item seed)
      (let* ((item (car la-item)) (toks (cdr la-item)) (symb (looking-at item)))
	(cond
	 ((last-item? (car la-item)) seed)
	 ((terminal? (looking-at (car la-item))) seed)
	 (else
	  (let iter ((seed seed) (pr (prule-range symb)))
	    (cond
	     ((null? pr) seed)
	     (else
	      (iter (merge-la-item seed (first-item (car pr))
				   (first-following item toks))
		    (range-next pr)))))))))
    la-item-l)))

;; @deffn kit-add kit-v tokens sx item
;; Add @code{tokens} to the list of lookaheads for the (kernel) @code{item}
;; in state @code{sx}.   This is a helper for @code{step2}.
(define (kit-add kit-v tokens kx item)
  (let* ((al (vector-ref kit-v kx))	; a-list for k-set kx
	 (ar (assoc item al))		; tokens for item
	 (sd (if (pair? ar)		; set difference
		 (lset-difference eqv? tokens (cdr ar))
		 tokens)))
    (cond ; no entry, update entry, no update
     ((null? tokens) #f)
     ((not ar) (vector-set! kit-v kx (acons item tokens al)) #t)
     ((pair? sd) (set-cdr! ar (append sd (cdr ar))) #t)
     (else #f))))

;; @deffn kip-add kip-v sx0 it0 sx1 it1
;; This is a helper for step2.  It updates kip-v with a propagation from
;; state @code{sx0}, item @code{it0} to state @code{sx1}, item @code{it1}.
;; [kip-v sx0] -> (it0 . ((sx1 . it1)
(define (kip-add kip-v sx0 it0 sx1 it1)
  (let* ((al (vector-ref kip-v sx0)) (ar (assoc it0 al)))
    (cond
     ((not ar)
      (vector-set! kip-v sx0 (acons it0 (list (cons sx1 it1)) al)) #t)
     ((member it1 (cdr ar)) #f)
     (else
      (set-cdr! ar (acons sx1 it1 (cdr ar))) #t))))

;; @deffn step2 p-mach-1 => p-mach-2
;; This implements steps 2 and 3 of Algorithm 4.13 on p. 242 of the DB.
;; The a-list @code{p-mach-1} includes the kernel itemsets and transitions
;; from @code{step1}.   This routine adds two entries to the a-list:
;; the initial set of lookahead tokens in a vector associated with key
;; @code{'kit-v} and a vector of spontaneous propagations associated with
;; key @code{'kip-v}.
;; @example
;; for-each item I in some itemset
;;   for-each la-item J in closure(I,#)
;;     for-each token T in lookaheads(J)
;;       if LA is #, then add to J propagate-to list
;;       otherwise add T to spontaneously-generated list
;; @end example
(define (step2 p-mach)
  (let* ((kis-v (assq-ref p-mach 'kis-v))
	 (kix-v (assq-ref p-mach 'kix-v)) ; transitions?
	 (nkset (vector-length kis-v))	; number of k-item-sets
	 ;; kernel-itemset tokens
	 (kit-v (make-vector nkset '())) ; sx => alist: (item latoks)
	 ;; kernel-itemset propagations
	 (kip-v (make-vector nkset '()))) ; sx0 => ((ita (sx1a . it1a) (sx2a
    (vector-set! kit-v 0 (closure (list (list '(0 . 0) '$end))))
    (let iter ((kx -1) (kset '()))
      (cond
       ((pair? kset)
	(for-each
	 (lambda (la-item)
	   (let* ((item (car la-item))	   ; closure item
		  (tokl (cdr la-item))	   ; tokens
		  (sym (looking-at item))  ; transition symbol
		  (item1 (next-item item)) ; next item after sym
		  (sx1 (assq-ref (vector-ref kix-v kx) sym)) ; goto(I,sym)
		  (item0 (car kset)))	   ; kernel item
	     (kit-add kit-v (delq '$@ tokl) sx1 item1) ; spontaneous
	     (if (memq '$@ tokl)	; propagates
		 (kip-add kip-v kx item0 sx1 item1))))
	 (remove ;; todo: check this remove
	  (lambda (li) (last-item? (car li)))
	  (closure (list (cons (car kset) '($@))))))
	(iter kx (cdr kset)))

       ((< (1+ kx) nkset)
	(iter (1+ kx)
	      ;; End-items don't shift, so don't propagate.
	      (remove last-item? (vector-ref kis-v (1+ kx)))))))
    ;;(when #f (pp-kip-v kip-v) (pp-kit-v kit-v)) ; for debugging
    (cons* (cons 'kit-v kit-v) (cons 'kip-v kip-v) p-mach)))

;; debug for step2
(define (pp-kit ix kset)
  (fmtout "~S:\n" ix)
  (for-each
   (lambda (item) (fmtout "    ~A, ~S\n" (pp-item (car item)) (cdr item)))
   kset))
(define (pp-kit-v kit-v)
  (fmtout "spontaneous:\n")
  (vector-for-each pp-kit kit-v))
(define (pp-kip ix kset)
  (for-each
   (lambda (x)
     (fmtout "~S: ~A\n" ix (pp-item (car x)))
     (for-each
      (lambda (y) (fmtout "   => ~S: ~A\n" (car y) (pp-item (cdr y))))
      (cdr x)))
   kset))
(define (pp-kip-v kip-v)
  (fmtout "propagate:\n")
  (vector-for-each pp-kip kip-v))

;; @deffn step3 p-mach-2 => p-mach-3
;; Execute nyacc step 3, where p-mach means ``partial machine''.
;; This implements step 4 of Algorithm 4.13 from the DB.
(define (step3 p-mach)
  (let* ((kit-v (assq-ref p-mach 'kit-v))
	 (kip-v (assq-ref p-mach 'kip-v))
	 (nkset (vector-length kit-v)))
    (let iter ((upd #t)			; token propagated?
	       (kx -1)			; current index
	       (ktal '())		; (item . LA) list for kx
	       (toks '())		; LA tokens being propagated
	       (item '())		; from item 
	       (prop '()))		; to items
      (cond
       ((pair? prop)
	;; Propagate lookaheads.
	(let* ((sx1 (caar prop)) (it1 (cdar prop)))
	  (iter (or (kit-add kit-v toks sx1 it1) upd)
		kx ktal toks item (cdr prop))))

       ((pair? ktal)
	;; Process the next (item . tokl) in the alist ktal.
	(iter upd kx (cdr ktal) (cdar ktal) (caar ktal)
	      (assoc-ref (vector-ref kip-v kx) (caar ktal))))

       ((< (1+ kx) nkset)
	;; Process the next itemset.
	(iter upd (1+ kx) (vector-ref kit-v (1+ kx)) '() '() '()))

       (upd
	;; Have updates, rerun.
	(iter #f 0 '() '() '() '()))))
    p-mach))

;; @deffn reductions kit-v sx => ((tokA gxA1 ...) (tokB gxB1 ...) ...)
;; This is a helper for @code{step4}.
;; Return an a-list of reductions for state @code{sx}.
;; The a-list pairs are make of a token and a list of prule indicies.
;; CHECK the following.  We are brute-force using @code{closure} here.
;; It works, but there should be a better algorithm.
;; Note on reductions: We reduce if the kernel-item is an end-item or a 
;; non-kernel item with an epsilon-production.  That is, if we have a
;; kernel item of the form
;; @example
;; A => abc.
;; @end example
;; or if we have the non-kernel item of the form
;; @example
;; B => .de
;; @end example
;; where FIRST(de,#) includes #.  See the second paragraph under ``Efficient
;; Construction of LALR Parsing Tables'' in DB Sec 4.7.
(define (old-reductions kit-v sx)
  (let iter ((ral '())			  ; result: reduction a-list
	     (lais (vector-ref kit-v sx)) ; la-item list
	     (toks '())			  ; kernel la-item LA tokens
	     (itms '())			  ; all items
	     (gx #f)			  ; rule reduced by tl
	     (tl '()))			  ; LA-token list
    (cond
     ((pair? tl) ;; add (token . p-rule) to reduction list
      (let* ((tk (car tl)) (rp (assq tk ral)))
	(cond
	 ;; already have this, skip to next token
	 ((and rp (memq gx (cdr rp)))
	  (iter ral lais toks itms gx (cdr tl)))
	 (rp
	  ;; have token, add prule
	  (set-cdr! rp (cons gx (cdr rp)))
	  (iter ral lais toks itms gx (cdr tl)))
	 (else
	  ;; add token w/ prule
	  (iter (cons (list tk gx) ral) lais toks itms gx (cdr tl))))))

     ((pair? itms)
      (if (last-item? (car itms))
	  ;; last item, add it 
	  (iter ral lais toks (cdr itms) (caar itms) toks)
	  ;; skip to next
	  (iter ral lais toks (cdr itms) 0 '())))

     ((pair? lais) ;; process next la-item
      (iter ral (cdr lais) (cdar lais) (expand-k-item (caar lais)) 0 '()))

     (else ral))))
;; I think the above is broken because I'm not including the proper tail
;; string.  The following just uses closure to do the job.  It works but
;; may not be very efficient: seems a bit brute force.
(define (new-reductions kit-v sx)
  (let iter ((ral '())			  ; result: reduction a-list
	     (klais (vector-ref kit-v sx)) ; kernel la-item list
	     (laits '())		   ; all la-items
	     (gx #f)			   ; rule reduced by tl
	     (tl '()))			  ; LA-token list
    (cond
     ((pair? tl) ;; add (token . p-rule) to reduction list
      (let* ((tk (car tl)) (rp (assq tk ral)))
	(cond
	 ((and rp (memq gx (cdr rp)))
	  ;; already have this, skip to next token
	  (iter ral klais laits gx (cdr tl)))
	 (rp
	  ;; have token, add prule
	  (set-cdr! rp (cons gx (cdr rp)))
	  (iter ral klais laits gx (cdr tl)))
	 (else
	  ;; add token w/ prule
	  (iter (cons (list tk gx) ral) klais laits gx (cdr tl))))))

     ((pair? laits) ;; process a la-itemset
      (if (last-item? (caar laits))
	  ;; last item, add it 
	  (iter ral klais (cdr laits) (caaar laits) (cdar laits))
	  ;; else skip to next
	  (iter ral klais (cdr laits) 0 '())))

     ((pair? klais) ;; expand next kernel la-item
      ;; There is a cheaper way than closure to do this but for now ...
      (iter ral (cdr klais) (closure (list (car klais))) 0 '()))

     (else
      ral))))
(define reductions new-reductions)

;; Generate parse-action-table from the shift a-list and reduce a-list.
;; This is a helper for @code{step4}.  It converts a list of state transitions
;; and a list of reductions into a parse-action table of shift, reduce,
;; accept, shift-reduce conflict or reduce-reduce conflict.
;; The actions take the form:
;; @example
;; (shift . <dst-state>)
;; (reduce . <rule-index>)
;; (accept . 0)
;; (srconf . (<dst-state> . <p-rule>))
;; (rrconf . <list of p-rules indices>)
;; @end example
;; If a shift has multiple reduce conflicts we report only one reduction.
(define (gen-pat sft-al red-al)
  (let iter ((res '()) (sal sft-al) (ral red-al))
    (cond
     ((pair? sal)
      (let* ((term (caar sal))		 ; terminal 
	     (goto (cdar sal))		 ; target state
	     (redp (assq term ral))	 ; a-list entry, may be removed
	     ;;(redl (if redp (cdr redp) #f))) ; reductions on terminal
	     (redl (and=> redp cdr)))	; reductions on terminal
	(cond
	 ((and redl (pair? (cdr redl)))
	  ;; This means we have a shift-reduce and reduce-reduce conflicts.
	  ;; We record only one shift-reduce and keep the reduce-reduce.
	  (iter (cons (cons* term 'srconf goto (car redl)) res)
		(cdr sal) ral))
	 (redl
	  ;; The terminal (aka token) signals a single reduction.  This means
	  ;; we have one shift-reduce conflict.  We have a chance to repair
	  ;; the parser using precedence/associativity rules so we remove the
	  ;; reduction from the reduction-list.
	  (iter (cons (cons* term 'srconf goto (car redl)) res)
		(cdr sal) (delete redp ral)))
	 (else
	  ;; The terminal (aka token) signals a shift only.
	  (iter (cons (cons* term 'shift goto) res)
		(cdr sal) ral)))))
     ((pair? ral)
      (let ((term (caar ral)) (rest (cdar ral)))
	;; We keep 'accept as explict action.  Another option is to reduce and
	;; have 0-th p-rule action generate return from parser (via prompt?).
	(iter
	 (cons (cons term
		     (cond ;; => action and arg(s)
		      ((zero? (car rest)) (cons 'accept 0))
		      ((zero? (car rest)) (cons 'reduce (car rest)))
		      ((> (length rest) 1) (cons 'rrconf rest))
		      (else (cons 'reduce (car rest)))))
	       res) sal (cdr ral))))
     (else res))))


;; @deffn step4 p-mach-0 => p-mach-1
;; This generates the parse action table from the itemsets and then applies
;; precedence and associativity rules to eliminate shift-reduce conflicts
;; where possible.  The output includes the parse action table (entry
;; @code{'pat-v} and TBD (list of errors in @code{'err-l}).
;;.per-state: alist by symbol:
;;   (symb <id>) if <id> > 0 SHIFT to <id>, else REDUCE by <id> else
;; so ('$end . 0) means ACCEPT!
;; but 0 for SHIFT and REDUCE, but reduce 0 is really ACCEPT
;; if reduce by zero we are done. so never hit state zero accept on ACCEPT?
;; For each state, the element of pat-v looks like
;; ((tokenA . (reduce . 79)) (tokenB . (reduce . 91)) ... )
(define (step4 p-mach)

  (define (setup-assc assc)
    (fold (lambda (al seed)
	    (append (x-flip al) seed)) '() assc))

  (define (setup-prec prec)
    (let iter ((res '()) (rl '()) (hd '()) (pl '()) (pll prec))
      (cond
       ((pair? pl)
	(let* ((p (car pl)) (hdp (x-comb hd p))
	       (pp (remove (lambda (p) (eqv? (car p) (cdr p))) (x-comb p p))))
	  (iter res (append rl hdp pp) (car pl) (cdr pl) pll)))
       ((pair? rl) (iter (append res rl) '() hd pl pll))
       ((pair? pll) (iter res rl '() (car pll) (cdr pll)))
       (else res))))

  (define (prev-sym act its)
    (let* ((a act)
	   (tok (car a)) (sft (caddr a)) (red (cdddr a))
	   ;; @code{pit} is the end-item in the p-rule to be reduced.
	   (pit (prev-item (prev-item (cons red -1))))
	   ;; @code{psy} is the last symbol in the p-rule to be reduced.
	   (psy (looking-at pit)))
      psy))

  (let* ((kis-v (assq-ref p-mach 'kis-v)) ; states
	 (kit-v (assq-ref p-mach 'kit-v)) ; la-toks
	 (kix-v (assq-ref p-mach 'kix-v)) ; transitions
	 (assc (assq-ref p-mach 'assc))	  ; associativity rules
	 (assc (setup-assc assc))	  ; trying it
	 (prec (assq-ref p-mach 'prec))	  ; precedence rules
	 (prec (setup-prec prec))	  ; trying it
	 (nst (vector-length kis-v))	  ; number of states
	 (pat-v (make-vector nst '()))	  ; parse-act tab /state
	 (rat-v (make-vector nst '()))	  ; removed-act tab /state
	 (gen-pat-ix (lambda (ix)	  ; pat from shifts & reduc's
		       (gen-pat (vector-ref kix-v ix) (reductions kit-v ix))))
	 (prp-v (assq-ref p-mach 'prp-v))  ; per-rule precedence
	 (tl (assq-ref p-mach 'terminals)) ; for error msgs
	 )
    ;; We run through each itemset.
    ;; @enumerate
    ;; @item We have a-list of symbols to shift state (i.e., @code{kix-v}).
    ;; @item We generate a list of tokens to reduction from @code{kit-v}.
    ;; @end enumerate
    ;; Q: should '$end be edited out of shifts?
    ;; kit-v is vec of a-lists of form ((item tok1 tok2 ...) ...)
    ;; turn to (tok1 item1 item2 ...)
    (let iter ((ix 0)		; state index
	       (pat '())	; parse-action table
	       (rat '())	; removed-action table
	       (wrn '())	; warnings on unsolicited removals
	       (ftl '())	; fatal conflicts
	       (actl (gen-pat-ix 0))) ; action list
      (cond
       ((pair? actl)
	(case (cadar actl)
	  ((shift reduce accept)
	   (iter ix (cons (car actl) pat) rat wrn ftl (cdr actl)))
	  ((srconf)
	   (let* ((act (car actl))
		  (tok (car act)) (sft (caddr act)) (red (cdddr act))
		  (prp (vector-ref prp-v red))
		  (psy (prev-sym act (vector-ref kis-v ix)))
		  (preced (or (and prp (prece prp tok prec)) ; rule-based
			      (prece psy tok prec))) ; oper-based
		  (sft-a (cons* tok 'shift sft))
		  (red-a (cons* tok 'reduce red)))
	     (call-with-values
		 (lambda ()
		   ;; Use precedence or, if =, associativity.
		   (case preced
		     ((#\>)
		      (values red-a (cons sft-a 'pre) #f #f))
		     ((#\<)
		      (values sft-a (cons red-a 'pre) #f #f))
		     ((#\=) ;; Now use associativity
		      (case (assq-ref assc tok)
			((left)
			 (values red-a (cons sft-a 'ass) #f #f))
			((right)
			 (values sft-a (cons red-a 'ass) #f #f))
			((nonassoc)
			 (values (cons* tok 'error red) #f #f (cons ix act)))
			(else
			 (values sft-a (cons red-a 'def) (cons ix act) #f))))
		     (else ;; Or default, which is shift.
		      (values sft-a (cons red-a 'def) (cons ix act) #f))))
	       (lambda (a r w f)
		 (iter ix
		       (if a (cons a pat) pat)
		       (if r (cons r rat) rat)
		       (if w (cons w wrn) wrn)
		       (if f (cons f ftl) ftl)
		       (cdr actl))))))
	  ((rrconf)
	   #;(fmterr "*** reduce-reduce conflict: in state ~A on ~A: ~A\n"
		   ix (obj->str (find-terminal (caar actl) tl)) (cddar actl))
	   (iter ix (cons (car actl) pat) rat wrn
		 (cons (cons ix (car actl)) ftl) (cdr actl)))
	  (else
	   (error "PROBLEM"))))
       ((null? actl)
	(vector-set! pat-v ix pat)
	(vector-set! rat-v ix rat)
	(iter ix pat rat wrn ftl #f))
       ((< (1+ ix) nst)
	(iter (1+ ix) '() '() wrn ftl (gen-pat-ix (1+ ix))))
       (else
	(let* ((attr (assq-ref p-mach 'attr))
	       (expect (assq-ref attr 'expect))) ; expected # srconf
	  (if (not (= (length wrn) expect))
	      (for-each (lambda (m) (fmterr "+++ warning: ~A\n" (conf->str m)))
			(reverse wrn)))
	  (for-each
	   (lambda (m) (fmterr "*** fatal: ~A\n" (conf->str m)))
	   (reverse ftl))
	  ))))
    ;; Return mach with parse-action and removed-action tables.
    (cons* (cons 'pat-v pat-v) (cons 'rat-v rat-v) p-mach)))

;; @deffn conf->str cfl => string
;; map conflict (e.g., @code{('rrconf 1 . 2}) to string.
(define (conf->str cfl)
  (let* ((st (list-ref cfl 0)) (tok (list-ref cfl 1)) (typ (list-ref cfl 2))
	 (core (fluid-ref *lalr-core*)) (terms (core-terminals core)))
    (fmtstr "in state ~A, ~A conflict on ~A"
	    st
	    (case typ
	      ((srconf) "shift-reduce")
	      ((rrconf) "reduce-reduce")
	      (else "unknown"))
	    (obj->str (find-terminal tok terms)))))
		     
;; @deffn gen-match-table mach => mach
;; Generate the match-table for a machine.  The match table is a list of
;; pairs: the car is the token used in the grammar specification, the cdr
;; is the symbol that should be returned by the lexical analyzer.
;;
;; The match-table may be passed to
;; the lexical analyzer builder to identify strings or string-types as tokens.
;; The associated key in the machine is @code{mtab}. 
;; @enumerate
;; @item
;; @sc{nyacc}-reserved symbols are provided as symbols
;; @example
;; $ident -> ($ident . $ident)
;; @end example
;; @item
;; Terminals used as symbols (@code{'comment} versus @code{"comment"}) are
;; provided as symbols.  The spec parser will provide a warning if symbols
;; are used in both ways.
;; @item
;; Others are provided as strings.
;; @end enumerate
;; The procedure @code{hashify-machine} will convert the cdrs to integers.
;; Test: "$abc" => ("$abc" '$abc) '$abc => ('$abc . '$abc)
(define (gen-match-table mach)
  (cons
   (cons 'mtab (map (lambda (term) (cons term (atomize term)))
		    (assq-ref mach 'terminals)))
   mach))


;; @deffn add-recovery-logic! mach => mach
;; Target of transition from @code{'$error} should have a default rule that
;; loops back.
(define (add-recovery-logic-1 mach)
  (let* ((kis-v (assq-ref mach 'kis-v))
	 (rhs-v (assq-ref mach 'rhs-v))
	 (pat-v (assq-ref mach 'pat-v))
	 (n (vector-length pat-v))
	 )
    (vector-for-each
     (lambda (kx kis)
       ;;(fmtout "kis=~S\n " kis)
       (for-each
	(lambda (ki)
	  (let* ((pi (prev-item ki))
		 (rhs (vector-ref rhs-v (car pi))))
	    (when (and (not (negative? (cdr pi)))
		       (eqv? '$error (looking-at pi)))
	      (vector-set! pat-v kx
			   (append
			    (vector-ref pat-v kx)
			    `(($default shift . ,kx))))
	      #;(fmtout " => ~S\n" (vector-ref pat-v kx)))))
	kis)
       ;;(fmtout "\n")
       #f)
     kis-v)
    mach))

(define (add-recovery-logic! mach)
  (let ((prev-core (fluid-ref *lalr-core*)))
    (dynamic-wind
	(lambda () (fluid-set! *lalr-core* (make-core/extras mach)))
	(lambda () (add-recovery-logic-1 mach))
	(lambda () (fluid-set! *lalr-core* prev-core)))))

;; to build parser, need:
;;   pat-v - parse action table
;;   ref-v - references
;;   len-v - rule lengths
;;   rto-v - hashed lhs symbols
;; to print itemsets need:
;;   kis-v - itemsets
;;   lhs-v - left hand sides
;;   rhs-v - right hand sides
;;   pat-v - action table

;; @deffn restart-spec spec start => spec
;; This generates a new spec with a different start.
;; @example
;; (restart-spec clang-spec 'expression) => cexpr-spec
;; @end example
(define (restart-spec spec start)
  (let* ((rhs-v (vector-copy (assq-ref spec 'rhs-v))))
    (vector-set! rhs-v 0 (vector start))
    (cons* (cons 'start start)
	   (cons 'rhs-v rhs-v)
	   spec)))

;; @deffn make-lalr-machine spec => pgen
;; Generate a-list of items used for building/debugging parsers.
;; It might be useful to add hashify and compact with keyword arguments.
(define (make-lalr-machine spec)
  (if (not spec) (error "make-lalr-machine: expecting valid specification"))
  (let ((prev-core (fluid-ref *lalr-core*)))
    (dynamic-wind
	(lambda () (fluid-set! *lalr-core* (make-core/extras spec)))
	(lambda ()
	  (let* ((sm1 (step1 spec))
		 (sm2 (step2 sm1))
		 (sm3 (step3 sm2))
		 (sm4 (step4 sm3))
		 (sm5 (gen-match-table sm4)))
	    (cons*
	     (cons 'len-v (vector-map (lambda (i v) (vector-length v))
				      (assq-ref sm5 'rhs-v)))
	     (cons 'rto-v (vector-copy (assq-ref sm5 'lhs-v)))
	     sm5)))
	(lambda () (fluid-set! *lalr-core* prev-core)))))

;; for debugging
(define (make-LR0-machine spec)
  (if (not spec) (error "make-LR0-machine: expecting valid specification"))
  (let ((prev-core (fluid-ref *lalr-core*)))
    (dynamic-wind
	(lambda () (fluid-set! *lalr-core* (make-core/extras spec)))
	(lambda () (step1 spec))
	(lambda () (fluid-set! *lalr-core* prev-core)))))

;; @deffn with-spec spec proc arg ...
;; Execute with spec or mach.
(define (with-spec spec proc . args)
  (if (not spec) (error "with-spec: expecting valid specification"))
  (let ((prev-core (fluid-ref *lalr-core*)))
    (dynamic-wind
	(lambda () (fluid-set! *lalr-core* (make-core/extras spec)))
	(lambda () (apply proc args))
	(lambda () (fluid-set! *lalr-core* prev-core)))))

;; @deffn lalr-match-table mach => match-table
;; Get the match-table
(define (lalr-match-table mach)
  (assq-ref mach 'mtab))

;; @deffn machine-compacted? mach => #t|#f
;; Indicate if the machine has been compacted.
;; TODO: needs update to deal with error recovery hooks.
(define (machine-compacted? mach)
  ;; Works by searching for $default phony-token.
  (call-with-prompt 'got-it
    ;; Search for '$default.  If not found return #f.
    (lambda ()
      (vector-for-each
       (lambda (ix pat)
	 (for-each
	  (lambda (a) (if (or (eqv? (car a) '$default) (eqv? (car a) -1))
			  (abort-to-prompt 'got-it)))
	  pat))
       (assq-ref mach 'pat-v))
      #f)
    ;; otherwise, return #t.
    (lambda () #t)))

;; @deffn compact-machine mach [#:keep 3] => mach
;; A "filter" to compact the parse table.  For each state this will replace
;; the most populus set of reductions of the same production rule with a
;; default production.  However, reductions triggered by keepers like
;; @code{'$error}, @code{'$lone-comm} or @code{'$lone-comm} are not counted.
;; The parser will want to treat errors and comments separately so that they
;; can be trapped (e.g., unaccounted comments are skipped).
(define* (compact-machine mach #:key (keep 3))
  (let* ((pat-v (assq-ref mach 'pat-v))
	 (nst (vector-length pat-v))
	 (hashed (number? (caar (vector-ref pat-v 0)))) ; been hashified?
	 (reduce? (if hashed
		      (lambda (a) (and (number? a) (negative? a)))
		      (lambda (a) (eq? 'reduce (car a)))))
	 (reduce-pr (if hashed abs cdr))
	 (reduce-to? (if hashed
			 (lambda (a r) (eqv? (- r) a))
			 (lambda (a r) (and (eq? 'reduce (car a))
					    (eqv? r (cdr a))))))
	 (mk-default (if hashed
			 (lambda (r) (cons -1 (- r)))
			 (lambda (r) `($default reduce . ,r))))
	 (mtab (assq-ref mach 'mtab))
	 (keepers (list (assq-ref mtab '$lone-comm)
			(assq-ref mtab '$code-comm)
			(assq-ref mtab '$error))))

    ;; Keep an a-list mapping reduction prod-rule => count.
    (let iter ((sx nst) (trn-l #f) (cnt-al '()) (p-max '(0 . 0)))
      (cond
       ((pair? trn-l)
	(cond
	((not (reduce? (cdar trn-l)))
	 ;; A shift, so not a candidate for default reduction.
	 (iter sx (cdr trn-l) cnt-al p-max))
	((memq (caar trn-l) keepers)
	 ;; Don't consider keepers because these will not be included.
	 (iter sx (cdr trn-l) cnt-al p-max))
	(else
	 ;; A reduction, so update the count for reducing this prod-rule.
	 (let* ((ix (reduce-pr (cdar trn-l)))
		(cnt (1+ (or (assq-ref cnt-al ix) 0)))
		(cnt-p (cons ix cnt)))
	   (iter sx (cdr trn-l) (cons cnt-p cnt-al)
		 (if (> cnt (cdr p-max)) cnt-p p-max))))))
	      
       ((null? trn-l)
	;; We have processed all transitions. If more than @code{keep} common
	;; reductions then generate default rule to replace those.
	(if (> (cdr p-max) keep)
	    (vector-set!
	     pat-v sx
	     (fold-right
	      (lambda (trn pat) ;; transition action
		;; If not a comment and reduces to the most-popular prod-rule
		;; then transfer to the default transition.
		(if (and (not (memq (car trn) keepers))
			 (reduce-to? (cdr trn) (car p-max)))
		    pat
		    (cons trn pat)))
	      (list (mk-default (car p-max))) ;; default is last
	      (vector-ref pat-v sx))))
	(iter sx #f #f #f))
       ((positive? sx) ;; next state
	(iter (1- sx) (vector-ref pat-v (1- sx)) '() '(0 . 0)))))
    mach))

;;.@section Using hash tables
;; The lexical analyzer will generate tokens.  The parser generates state
;; transitions based on these tokens.  When we build a lexical analyzer
;; (via @code{make-lexer}) we provide a list of strings to detect along with
;; associated tokens to return to the parser.  By default the tokens returned
;; are symbols or characters.  But these could as well be integers.  Also,
;; the parser uses symbols to represent non-terminals, which are also used
;; to trigger state transitions.  We could use integers instead of symbols
;; and characters by mapping via a hash table.   We will bla bla bla.
;; There are also standard tokens we need to worry about.  These are
;; @enumerate
;; @item the @code{$end} marker
;; @item identifiers (using the symbolic token @code{$ident}
;; @item non-negative integers (using the symbolic token @code{$fixed})
;; @item non-negative floats (using the symbolic token @code{$float})
;; @item @code{$default} => 0
;; @end enumerate
;; And action
;; @enumerate
;; @item positive => shift
;; @item negative => reduce
;; @item zero => accept
;; @end enumerate
;; However, if these are used they should appear in the spec's terminal list.
;; For the hash table we use positive integers for terminals and negative
;; integers for non-terminals.  To apply such a hash table we need to:
;; @enumerate
;; @item from the spec's list of terminals (aka tokens), generate a list of
;; terminal to integer pairs (and vice versa)
;; @item from the spec's list of non-terminals generate a list of symbols
;; to integers and vice versa.
;; @item Go through the parser-action table and convert symbols and characters
;; to integers
;; @item Go through the XXX list passed to the lexical analyizer and replace
;; symbols and characters with integers.
;; @end enumerate
;; One issue we need to deal with is separating out the identifier-like
;; terminals (aka keywords) from those that are not identifier-like.  I guess
;; this should be done as part of @code{make-lexer}, by filtering the token
;; list through the ident-reader.
;; NOTE: The parser is hardcoded to assume that the phony token for the
;; default (reduce) action is @code{'$default} for unhashed machine or
;; @code{-1} for a hashed machine.

;; NEW: need to add reduction of ERROR

;; @deffn machine-hashed? mach => #t|#f
;; Indicate if the machine has been hashed.
(define (machine-hashed? mach)
  ;; If hashed, the parse action for rule 0 will always be a number.
  (number? (caar (vector-ref (assq-ref mach 'pat-v) 0))))

;; @deffn hashify-machine mach => mach
(define (hashify-machine mach)
  (if (machine-hashed? mach) mach
      (let* ((terminals (assq-ref mach 'terminals))
	     (non-terms (assq-ref mach 'non-terms))
	     (lhs-v (assq-ref mach 'lhs-v))
	     (sm ;; = (cons sym->int int->sym)
	      (let iter ((si (list (cons '$default -1)))
			 (is (list (cons -1 '$default)))
			 (ix 1) (tl terminals) (nl non-terms))
		(if (null? nl) (cons (reverse si) (reverse is))
		    (let* ((s (atomize (if (pair? tl) (car tl) (car nl))))
			   (tl1 (if (pair? tl) (cdr tl) tl))
			   (nl1 (if (pair? tl) nl (cdr nl))))
		      (iter (acons s ix si) (acons ix s is) (1+ ix) tl1 nl1)))))
	     (sym->int (lambda (s) (assq-ref (car sm) s)))
	     ;;
	     (pat-v0 (assq-ref mach 'pat-v))
	     (npat (vector-length pat-v0))
	     (pat-v1 (make-vector npat '())))
	;; replace symbol/chars with integers
	(let iter1 ((ix 0))
	  (unless (= ix npat)
	    (let iter2 ((al1 '()) (al0 (vector-ref pat-v0 ix)))
	      (if (null? al0) (vector-set! pat-v1 ix (reverse al1))
		  (let* ((a0 (car al0))
			 ;; tk: token; ac: action; ds: destination
			 (tk (car a0)) (ac (cadr a0)) (ds (cddr a0))
			 ;; t: encoded token; d: encoded destination
			 (t (sym->int tk))
			 (d (case ac
			      ((shift) ds) ((reduce) (- ds))
			      ((accept) 0) (else #f))))
		    (unless t
		      (fmterr "~S ~S ~S\n" tk ac ds)
		      (error "expect something"))
		    (iter2 (acons t d al1) (cdr al0)))))
	    (iter1 (1+ ix))))
	;;
	(cons*
	 (cons 'pat-v pat-v1)
	 (cons 'siis sm) ;; sm = (cons sym->int int->sym)
	 (cons 'mtab
	       (let iter ((mt1 '()) (mt0 (assq-ref mach 'mtab)))
		 (if (null? mt0) (reverse mt1)
		     (iter (cons (cons (caar mt0) (sym->int (cdar mt0))) mt1)
			   (cdr mt0)))))
	 ;; reduction symbols = lhs:
	 (cons 'rto-v (vector-map (lambda (i v) (sym->int v)) lhs-v))
	 mach))))

;; === grammar/machine printing ======

;; @deffn elt->str elt terms => string
(define (elt->str elt terms)
  (or (and=> (find-terminal elt terms) obj->str)
      (symbol->string elt)))

;; @deffn pp-rule indent gx [port]
;; Pretty-print a production rule.
(define (pp-rule il gx . rest)
  (let* ((port (if (pair? rest) (car rest) (current-output-port)))
	 (core (fluid-ref *lalr-core*))
	 (lhs (vector-ref (core-lhs-v core) gx))
	 (rhs (vector-ref (core-rhs-v core) gx))
	 (tl (core-terminals core)))
    (display (substring "                     " 0 (min il 20)) port)
    (fmt port "~A =>" lhs)
    (vector-for-each (lambda (ix e) (fmt port " ~A" (elt->str e tl))) rhs)
    (newline port)))
	 
;; @deffn pp-item item => string
;; This could be called item->string.
;; This needs terminals to work correctly, like pp-lalr-grammar.
(define (pp-item item) 
  (let* ((core (fluid-ref *lalr-core*))
	 (tl (core-terminals core))
	 (gx (car item))
	 (lhs (vector-ref (core-lhs-v core) gx))
	 (rhs (vector-ref (core-rhs-v core) gx))
	 (rhs-len (vector-length rhs)))
    (apply
     string-append
     (let iter ((rx 0) (sl (list (fmtstr "~S =>" lhs))))
       (if (= rx rhs-len)
	   (append sl (if (= -1 (cdr item)) '(" .") '()))
	   (iter (1+ rx)
		 (append
		  sl (if (= rx (cdr item)) '(" .") '())
		  (let ((e (vector-ref rhs rx)))
		    (list (string-append " " (elt->str e tl)))))))))))

;; @deffn pp-lalr-notice spec [port]
(define (pp-lalr-notice spec . rest)
  (let* ((port (if (pair? rest) (car rest) (current-output-port)))
	 (notice (assq-ref (assq-ref spec 'attr) 'notice))
	 (lines (if notice (string-split notice #\newline) '())))
    (for-each (lambda (l) (simple-format port "  ~A\n" l)) lines)
    (newline)))

;; @deffn pp-lalr-grammar spec [port]
;; Pretty-print the grammar to the specified port, or current output.
(define (pp-lalr-grammar spec . rest)
  (let* ((port (if (pair? rest) (car rest) (current-output-port)))
	 (lhs-v (assq-ref spec 'lhs-v))
	 (rhs-v (assq-ref spec 'rhs-v))
	 (nrule (vector-length lhs-v))
	 (act-v (assq-ref spec 'act-v))
	 ;;(prp-v (assq-ref mach 'prp-v)) ; per-rule precedence
	 (terms (assq-ref spec 'terminals))
	 (prev-core (fluid-ref *lalr-core*)))
    (fluid-set! *lalr-core* (make-core spec)) ; OR dynamic-wind ???
    ;; Print out the grammar.
    (do ((i 0 (1+ i))) ((= i nrule))
      (let* ((lhs (vector-ref lhs-v i)) (rhs (vector-ref rhs-v i)))
	(if #f
	    (pp-rule 0 i)
	    (begin
	      (fmt port "~A ~A =>" i lhs)
	      (vector-for-each
	       (lambda (ix e) (fmt port " ~A" (elt->str e terms)))
	       rhs)
	      ;;(fmt port "\t~S" (vector-ref act-v i))
	      (newline port)))))
    (newline port)
    (fluid-set! *lalr-core* prev-core)))

;; @deffn pp-lalr-machine mach [port]
;; Print the states of the parser with items and shift/reduce actions.
(define (pp-lalr-machine mach . rest)
  (let* ((port (if (pair? rest) (car rest) (current-output-port)))
	 (lhs-v (assq-ref mach 'lhs-v))
	 (rhs-v (assq-ref mach 'rhs-v))
	 (nrule (vector-length lhs-v))
	 (pat-v (assq-ref mach 'pat-v))
	 (rat-v (assq-ref mach 'rat-v))
	 (kis-v (assq-ref mach 'kis-v))
	 (kit-v (assq-ref mach 'kit-v))
	 (nst (vector-length kis-v))	; number of states
	 (i->s (or (and=> (assq-ref mach 'siis) cdr) '()))
	 (terms (assq-ref mach 'terminals))
	 (prev-core (fluid-ref *lalr-core*)))
    (fluid-set! *lalr-core* (make-core mach))
    ;; Print out the itemsets and shift reduce actions.
    (do ((i 0 (1+ i))) ((= i nst))
      (let* ((state (vector-ref kis-v i))
	     (pat (vector-ref pat-v i))
	     (rat (if rat-v (vector-ref rat-v i) '())))
	(fmt port "~A:" i)	     ; itemset index (aka state index)
	(for-each
	 (lambda (k-item)
	   (for-each			; item, print it
	    (lambda (item)
	      (fmt port "\t~A" (pp-item item))
	      ;; show lookaheads:
	      (if (and #f (negative? (cdr item)) kit-v (equal? item k-item))
		  (fmt port " ~A"
		       (map (lambda (tok) (elt->str tok terms))
			    (assoc-ref (vector-ref kit-v i) k-item))))
	      (fmt port "\n"))
	    (expand-k-item k-item)))
	 state)
	(for-each			; action, print it
	 (lambda (act)
	   (if (pair? (cdr act))
	       (let ((sy (car act)) (pa (cadr act)) (gt (cddr act)))
		 (case pa
		   ((srconf)
		    (fmt port "\t\t~A => CONFLICT: shift ~A, reduce ~A\n"
			 (elt->str sy terms) (car gt) (cdr gt)))
		   ((rrconf)
		    (fmt port "\t\t~A => CONFLICT: reduce ~A\n"
			 (elt->str sy terms)
			 (string-join (map number->string gt) ", reduce ")))
		   (else
		    (fmt port "\t\t~A => ~A ~A\n" (elt->str sy terms) pa gt))))
	       (let* ((sy (car act)) (p (cdr act))
		      (pa (cond ((eq? #f p) 'CONFLICT)
				((positive? p) 'shift)
				((negative? p) 'reduce)
				(else 'accept)))
		      (gt (and=> p abs)))
		 (fmt port "\t\t~A => ~A ~A\n"
		      (elt->str (assq-ref i->s sy) terms)
		      pa gt))))
	 pat)
	(for-each			; action, print it
	 (lambda (ra)
	   ;; FIX: should indicate if precedence removed user rule or default
	   (fmt port "\t\t[~A => ~A ~A] REMOVED by ~A\n"
		(elt->str (caar ra) terms) (cadar ra) (cddar ra)
		(case (cdr ra)
		  ((pre) "precedence")
		  ((ass) "associativity")
		  ((def) "default shift")
		  (else (cdr ra)))))
	 rat)
	(newline)))
    (fluid-set! *lalr-core* prev-core)
    (values)))

;; === output routines ===============

(use-modules (ice-9 pretty-print))
(use-modules (ice-9 regex))

(define (write-notice mach port)
  (let* ((comm-leader ";; ")
	 (notice (assq-ref (assq-ref mach 'attr) 'notice))
	 (lines (if notice (string-split notice #\newline) '())))
    (for-each
     (lambda (l) (fmt port "~A~A\n" comm-leader l))
     lines)
    (if (pair? lines) (newline port))))

(define (string-sub str pat repl)
  (let ((m (string-match pat str)))
    (if m
	(regexp-substitute #f m 'pre repl 'post)
	str)))

;; @deffn write-lalr-tables mach filename [#:lang output-lang]
;; For example,
;; @example
;; write-lalr-tables mach "tables.scm"
;; write-lalr-tables mach "tables.tcl" #:lang 'tcl
;; @end example
(define* (write-lalr-tables mach filename #:key (lang 'scheme))

  (define (write-table mach name port)
    (fmt port "(define ~A\n  " name)
    (ugly-print (assq-ref mach name) port)
    (fmt port ")\n\n"))

  (call-with-output-file filename
    (lambda (port)
      (fmt port ";; ~A\n\n" (string-sub filename ".new$" ""))
      (write-notice mach port)
      (write-table mach 'len-v port)
      (write-table mach 'pat-v port)
      (write-table mach 'rto-v port)
      (write-table mach 'mtab port)
      (display ";;; end tables" port)
      (newline port))))


;; @deffn write-lalr-actions mach filename [#:lang output-lang]
;; For example,
;; @example
;; write-lalr-actions mach "actions.scm"
;; write-lalr-actions mach "actions.tcl" #:lang 'tcl
;; @end example
(define* (write-lalr-actions mach filename #:key (lang 'scheme))

  (define (pp-rule/ts gx)
    (let* ((core (fluid-ref *lalr-core*))
	   (lhs (vector-ref (core-lhs-v core) gx))
	   (rhs (vector-ref (core-rhs-v core) gx))
	   (tl (core-terminals core))
	   (line (string-append
		  (symbol->string lhs) " => "
		  (string-join 
		   (map (lambda (elt) (elt->str elt tl))
			(vector->list rhs))
		   " "))))
      (if (> (string-length line) 72)
	  (string-append (substring/shared line 0 69) "...")
	  line)))
  (define (NEW-pp-rule/ts gx)
    ;; TBD: use start for zeroth rule
    (let* ((core (fluid-ref *lalr-core*))
	   (lhs (vector-ref (core-lhs-v core) gx))
	   (rhs (vector-ref (core-rhs-v core) gx))
	   (tl (core-terminals core))
	   (line (string-append
		  (symbol->string lhs) " => "
		  (string-join 
		   (map (lambda (elt) (elt->str elt tl))
			(vector->list rhs))
		   " "))))
      (if (> (string-length line) 72)
	  (string-append (substring/shared line 0 69) "...")
	  line)))
    
  (define (write-actions mach port)
    (with-fluid*
     *lalr-core* (make-core mach)
     (lambda ()
       (fmt port "(define act-v\n  (vector\n")
       (vector-for-each
	(lambda (gx actn)
	  (fmt port "   ;; ~A\n" (pp-rule/ts gx))
	  (pretty-print (wrap-action actn) port #:per-line-prefix "   "))
	(assq-ref mach 'act-v))
       (fmt port "   ))\n\n"))))

  (call-with-output-file filename
    (lambda (port)
      (fmt port ";; ~A\n\n" (string-sub filename ".new$" ""))
      (write-notice mach port)
      (write-actions mach port)
      (display ";;; end tables" port)
      (newline port))))

;; @end itemize
;;; --- last line ---