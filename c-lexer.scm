;;; ECMAScript for Guile

;; Copyright (C) 2009, 2010, 2011 Free Software Foundation, Inc.

;;;; This library is free software; you can redistribute it and/or
;;;; modify it under the terms of the GNU Lesser General Public
;;;; License as published by the Free Software Foundation; either
;;;; version 3 of the License, or (at your option) any later version.
;;;; 
;;;; This library is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;;; Lesser General Public License for more details.
;;;; 
;;;; You should have received a copy of the GNU Lesser General Public
;;;; License along with this library; if not, write to the Free Software
;;;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA

;;; Code:

;; (define-module (language ecmascript tokenize)
;;   #:use-module (ice-9 rdelim)
;;   #:use-module ((srfi srfi-1) #:select (unfold-right))
;;   #:use-module (system base lalr)
;;   #:export (next-token make-tokenizer make-tokenizer/1 tokenize tokenize/1))

(cond-expand
  (guile
   (use-modules ((ice-9 rdelim)))

   (define (syntax-error what loc form . args)
     (throw 'syntax-error #f what
            ;;(and=> loc source-location->source-properties)
            loc
            form #f args))
   
   )
  (mes

   )
  )


(define (port-source-location port)
  (make-source-location (port-filename port)
                        (port-line port)
                        (port-column port)
                        (false-if-exception (ftell port))
                        #f))

;; taken from SSAX, sorta
(define (read-until delims loc)
  (if (eof-object? (peek-char))
      (syntax-error "EOF while reading a token" loc #f)
      (let ((token (read-delimited delims (current-input-port) 'peek)))
        (if (eof-object? (peek-char))
            (syntax-error "EOF while reading a token" loc token)
            token))))

(define (char-hex? c)
  (and (not (eof-object? c))
       (or (char-numeric? c)
           (memv c '(#\a #\b #\c #\d #\e #\f))
           (memv c '(#\A #\B #\C #\D #\E #\F)))))

(define (digit->number c)
  (- (char->integer c) (char->integer #\0)))

(define (hex->number c)
  (if (char-numeric? c)
      (digit->number c)
      (+ 10 (- (char->integer (char-downcase c)) (char->integer #\a)))))

(define (read-slash loc div?)
  (let ((c1 (begin
              (read-char)
              (peek-char))))
    (cond
     ((eof-object? c1)
      ;; hmm. error if we're not looking for a div? ?
      (make-lexical-token '/ loc #f))
     ((char=? c1 #\/)
      (read-line)
      (next-token div?))
     ((char=? c1 #\*)
      (read-char)
      (let lp ((c (read-char)))
        (cond
         ((eof-object? c)
          (syntax-error "EOF while in multi-line comment" loc #f))
         ((char=? c #\*)
          (if (eqv? (peek-char) #\/)
              (begin
                (read-char)
                (next-token div?))
              (lp (read-char))))
         (else
          (lp (read-char))))))
     (div?
      (case c1
        ((#\=) (read-char) (make-lexical-token '/= loc #f))
        (else (make-lexical-token '/ loc #f))))
     (else
      ;;;(read-regexp loc)
      (make-lexical-token '/ loc #f)))))

(define (read-string loc)
  (let ((c (read-char)))
    (let ((terms (string c #\\ #\nl #\cr)))
      (define (read-escape)
        (let ((c (read-char)))
          (case c
            ((#\' #\" #\\) c)
            ((#\b) #\bs)
            ((#\f) #\np)
            ((#\n) #\nl)
            ((#\r) #\cr)
            ((#\t) #\tab)
            ((#\v) #\vt)
            ((#\0)
             (let ((next (peek-char)))
               (cond
                ((eof-object? next) #\nul)
                ((char-numeric? next)
                 (syntax-error "octal escape sequences are not supported"
                               loc #f))
                (else #\nul))))
            ((#\x)
             (let* ((a (read-char))
                    (b (read-char)))
               (cond
                ((and (char-hex? a) (char-hex? b))
                 (integer->char (+ (* 16 (hex->number a)) (hex->number b))))
                (else
                 (syntax-error "bad hex character escape" loc (string a b))))))
            ((#\u)
             (let* ((a (read-char))
                    (b (read-char))
                    (c (read-char))
                    (d (read-char)))
               (integer->char (string->number (string a b c d) 16))))
            (else
             c))))
      (let lp ((str (read-until terms loc)))
        (let ((terminator (peek-char)))
          (cond
           ((char=? terminator c)
            (read-char)
            (make-lexical-token 'StringLiteral loc str))
           ((char=? terminator #\\)
            (read-char)
            (let ((echar (read-escape)))
              (lp (string-append str (string echar)
                                 (read-until terms loc)))))
           (else
            (syntax-error "string literals may not contain newlines"
                          loc str))))))))

(define *keywords*
  '(("break" . break)
    ("case" . case)
    ("continue" . continue)
    ("else" . else)
    ("goto" . goto)

    ("char" . char)
    ("double" . double)
    ("float" . float)
    ("int" . int)
    ("long" . long)
    ("short" . short)
    ("unsigned" . unsigned)
    
    ("return" . return)
    ("void" . void)
    ("for" . for)
    ("switch" . switch)
    ("while" . while)
    ("continue" . continue)
    ("default" . default)
    ("if" . if)
    ("do" . do)

    ;; these aren't exactly keywords, but hey
    ("true" . true)
    ("false" . false)))

(define (read-identifier loc)
  (let lp ((c (peek-char)) (chars '()))
    (if (or (eof-object? c)
            (not (or (char-alphabetic? c)
                     (char-numeric? c)
                     (char=? c #\$)
                     (char=? c #\_))))
        (let ((word (list->string (reverse chars))))
          (cond ((assoc-ref *keywords* word)
                 (make-lexical-token (assoc-ref *keywords* word) loc #f))
                (else (make-lexical-token 'Identifier loc
                                          (string->symbol word)))))
        (begin (read-char)
               (lp (peek-char) (cons c chars))))))

(define (read-numeric loc)
  (let* ((c0 (if (char=? (peek-char) #\.)
                 #\0
                 (read-char)))
         (c1 (peek-char)))
    (cond
     ((eof-object? c1) (digit->number c0))
     ((and (char=? c0 #\0) (or (char=? c1 #\x) (char=? c1 #\X)))
      (read-char)
      (let ((c (peek-char)))
        (if (not (char-hex? c))
            (syntax-error "bad digit reading hexadecimal number"
                          loc c))
        (let lp ((c c) (acc 0))
          (cond ((char-hex? c)
                 (read-char)
                 (lp (peek-char)
                     (+ (* 16 acc) (hex->number c))))
                (else
                 acc)))))
     ((and (char=? c0 #\0) (char-numeric? c1))
      (let lp ((c c1) (acc 0))
        (cond ((eof-object? c) acc)
              ((char-numeric? c)
               (if (or (char=? c #\8) (char=? c #\9))
                   (syntax-error "invalid digit in octal sequence"
                                 loc c))
               (read-char)
               (lp (peek-char)
                   (+ (* 8 acc) (digit->number c))))
              (else
               acc))))
     (else
      (let lp ((c1 c1) (acc (digit->number c0)))
        (cond
         ((eof-object? c1) acc)
         ((char-numeric? c1)
          (read-char)
          (lp (peek-char)
              (+ (* 10 acc) (digit->number c1))))
         ((or (char=? c1 #\e) (char=? c1 #\E))
          (read-char)
          (let ((add (let ((c (peek-char)))
                       (cond ((eof-object? c)
                              (syntax-error "error reading exponent: EOF"
                                            loc #f))
                             ((char=? c #\+) (read-char) +)
                             ((char=? c #\-) (read-char) -)
                             ((char-numeric? c) +)
                             (else
                              (syntax-error "error reading exponent: non-digit"
                                            loc c))))))
            (let lp ((c (peek-char)) (e 0))
              (cond ((and (not (eof-object? c)) (char-numeric? c))
                     (read-char)
                     (lp (peek-char) (add (* 10 e) (digit->number c))))
                    (else
                     (* (if (negative? e) (* acc 1.0) acc) (expt 10 e)))))))
         ((char=? c1 #\.)
          (read-char)
          (let lp2 ((c (peek-char)) (dec 0.0) (n -1))
            (cond ((and (not (eof-object? c)) (char-numeric? c))
                   (read-char)
                   (lp2 (peek-char)
                        (+ dec (* (digit->number c) (expt 10 n)))
                        (1- n)))
                  (else
                   ;; loop back to catch an exponential part
                   (lp c (+ acc dec))))))
         (else
          acc)))))))
           
(define *punctuation*
  '(("{" . lbrace)
    ("}" . rbrace)
    ("(" . lparen)
    (")" . rparen)
    ("[" . lbracket)
    ("]" . rbracket)
    ("." . dot)
    (";" . semicolon)
    ("," . comma)
    ("<" . <)
    (">" . >)
    ("<=" . <=)
    (">=" . >=)
    ("==" . ==)
    ("!=" . !=)
    ("===" . ===)
    ("!==" . !==)
    ("+" . +)
    ("-" . -)
    ("*" . *)
    ("%" . %)
    ("++" . ++)
    ("--" . --)
    ("<<" . <<)
    (">>" . >>)
    (">>>" . >>>)
    ("&" . &)
    ("|" . bor)
    ("^" . ^)
    ("!" . !)
    ("~" . ~)
    ("&&" . &&)
    ("||" . or)
    ("?" . ?)
    (":" . colon)
    ("=" . =)
    ("+=" . +=)
    ("-=" . -=)
    ("*=" . *=)
    ("%=" . %=)
    ("<<=" . <<=)
    (">>=" . >>=)
    (">>>=" . >>>=)
    ("&=" . &=)
    ("|=" . bor=)
    ("^=" . ^=)))

(define *div-punctuation*
  '(("/" . /)
    ("/=" . /=)))

;; node ::= (char (symbol | #f) node*)
(define read-punctuation
  (let ((punc-tree (let lp ((nodes '()) (puncs *punctuation*))
                     (cond ((null? puncs)
                            nodes)
                           ((assv-ref nodes (string-ref (caar puncs) 0))
                            (let ((node-tail (assv-ref nodes (string-ref (caar puncs) 0))))
                              (if (= (string-length (caar puncs)) 1)
                                  (set-car! node-tail (cdar puncs))
                                  (set-cdr! node-tail
                                            (lp (cdr node-tail)
                                                `((,(substring (caar puncs) 1)
                                                   . ,(cdar puncs))))))
                              (lp nodes (cdr puncs))))
                           (else
                            (lp (cons (list (string-ref (caar puncs) 0) #f) nodes)
                                puncs))))))
    (lambda (loc)
      (let lp ((c (peek-char)) (tree punc-tree) (candidate #f))
        (display "read-punctuation c=") (display c) (newline)
        (cond
         ((assv-ref tree c)
          (let ((node-tail (assv-ref tree c)))
            (read-char)
            (lp (peek-char) (cdr node-tail) (car node-tail))))
         (candidate
          (make-lexical-token candidate loc #f))
         (else
          (syntax-error "bad syntax: character not allowed" loc c)))))))

(define (next-token div?)
  (let ((c   (peek-char))
        (loc (port-source-location (current-input-port))))
    (display "next-token c=") (display c) (newline)

    (case c
      ((#\ht #\vt #\np #\space #\x00A0) ; whitespace
       (read-char)
       (next-token div?))
      ((#\newline #\cr)                 ; line break
       (read-char)
       (next-token div?))
      ((#\/)
       ;; division, single comment, double comment, or regexp
       (read-slash loc div?))
      ((#\" #\')                        ; string literal
       (read-string loc))
      (else
       (cond
        ((eof-object? c)
         '*eoi*)
        ((or (char-alphabetic? c)
             (char=? c #\$)
             (char=? c #\_))
         ;; reserved word or identifier
         (read-identifier loc))
        ((char-numeric? c)
         ;; numeric -- also accept . FIXME, requires lookahead
         (make-lexical-token 'NumericLiteral loc (read-numeric loc)))
        (else
         ;; punctuation
         (read-punctuation loc)))))))

(define (c-lexer errorp)
  (let ((div? #f))
    (lambda ()
      (let ((tok (next-token div?)))
        (set! div? (and (lexical-token? tok)
                        (let ((cat (lexical-token-category tok)))
                          (or (eq? cat 'Identifier)
                              (eq? cat 'NumericLiteral)
                              (eq? cat 'StringLiteral)))))
        tok))))
