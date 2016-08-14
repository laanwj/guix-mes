(cond-expand
  (guile
   ;;(use-modules ((system base lalr)))
   )
  (mes
   ))

(define c-parser
  (lalr-parser

   (lbrace rbrace lparen rparen lbracket rbracket semicolon colon dot comma
           =
           Identifier NumericLiteral StringLiteral
           break case continue goto label
           return switch
           for
           if else
           (left: or && ! * / + -)
           (left: bool double float enum void int struct)
           (left: < > <= >=)
           (left: ++ --)
           (nonassoc: == !=)
           )
   
   (program
    (translation-unit *eoi*) : `(root ,@$1))

   (translation-unit
    (external-declaration) : `(,$1)
    (translation-unit external-declaration) : `(,@$1 ,@$2))

   (external-declaration
    (function-definition) : $1
    (declaration) : $1
    (error semicolon) : (begin (syntax-error "external declaration" @1 $1) '()))

   (function-definition
    (declarator compound-statement) : `(function ,$1 (signature int (formals)) ,$2)
    (declaration-specifiers declarator compound-statement) : `(function ,$2 (signature ,$1 (formals)) ,$3)
    (declaration-specifiers declarator declaration-list compound-statement) : `(function ,$2 (signature ,$1 ,$3) ,$4))

   (declaration
    (declaration-specifiers semicolon) : `(,$1)
    (declaration-specifiers init-declarator-list semicolon): `((,@$1 ,@$2))
    )

   (declaration-list
    (declaration) : `(formals ,@$1)
    (declaration-list declaration) : `(,@$1 ,@(cdr $2)))

   (declaration-specifiers
    ;;(storage-class-specifier) : `(,$1)
    (type-specifier) : `(,$1)
    ;;(type-qualifier) : `($1)
    ;;(storage-class-specifier declaration-specifiers) : (cons $1 $2)
    (type-specifier declaration-specifiers) : `(,$1 ,$2)
    ;;(type-qualifier declaration-specifiers) : (cons $1 $2)
    )

   ;; (storage_class_specifier
   ;;  (auto)
   ;;  (extern)
   ;;  (register)
   ;;  (static)
   ;;  (typedef))
   
   (type-specifier
    ;; (char) : $1
    ;; (double) : $1
    ;; (void) : $1
    ;; (float)
    (int) : 'int
    ;; (long)
    ;; (short)
    ;; (unsigned)
    ;; (struct-or-enum-specifier)
    ;; (enum-specifier)
    ;; (type-name)
    )

   ;; (type-qualifier
   ;;  (const)
   ;;  (volatile))

   ;; struct_or_union_specifier:
   ;; 		   struct_or_union_ident lbrace struct_declaration_list rbrace
   ;; 		|  struct_or_union_ident
   ;; 		;

   ;; struct_or_union_ident: struct_or_union
   ;; 		| struct_or_union Identifier
   ;; 		;

   ;; struct_or_union:   STRUCT				{ ; }
   ;; 		|  UNION				{ ; }
   ;; 		;
   
   ;; struct_declaration_list: struct_declaration
   ;; 		|  struct_declaration_list struct_declaration
   ;; 		;

   (init-declarator-list
    ;; (init-declarator %prec comma) : `(,$1) HUH?
    (init-declarator) : `(,$1)
    (init-declarator-list comma init-declarator) : `(,$1)
    )
   ;; init_declarator_list:     init_declarator %prec comma
   ;; 		|  init_declarator_list comma init_declarator
   ;; 		;

   (init-declarator
    (declarator) : $1
    (declarator = initializer) : `(= ,$1 ,$3)
    ;; 		| error { yyerror("init declarator error"); }
    )

   ;; struct_declaration: specifier_qualifier_list struct_declarator_list semicolon
   ;; 		;

   ;; specifier_qualifier_list: type_specifier
   ;; 		|  type_qualifier
   ;; 		|  type_specifier specifier_qualifier_list
   ;; 		| type_qualifier specifier_qualifier_list
   ;; 		;

   ;; struct_declarator_list: struct_declarator
   ;; 		|  struct_declarator_list comma struct_declarator
   ;; 		;

   ;; struct_declarator: declarator
   ;; 		|  COLON constant_expression		{ ; }
   ;; 		|  declarator COLON constant_expression
   ;; 		;

   ;; enum_specifier:	   ENUM Identifier lbrace enumerator_list rbrace	{ ; }
   ;; 		|  ENUM lbrace enumerator_list rbrace		{ ; }
   ;; 		|  ENUM Identifier				{ ; }
   ;; 		;

   ;; enumerator_list:   enumerator
   ;; 		|  enumerator_list comma enumerator
   ;; 		;

   ;; enumerator:		   Identifier
   ;; 		|  Identifier EQ constant_expression
   ;; 		;

   (declarator
    (direct-declarator) : $1
    ;;(pointer direct-declarator)
    )

   (direct-declarator
    (Identifier) : $1
    ;; (lparen declarator rparen)
    ;; (direct-declarator lbracket rbracket)
    ;; (direct-declarator lbracket constant-expression rbracket)
    ;; (lbracket constant-expression rbracket)
    ;; (direct-declarator lparen parameter-type-list rparen)
    (direct-declarator lparen rparen) : $1
    ;; (direct-declarator lparen identifier-list rparen)
    )

   ;; pointer:	   STAR					{ ; }
   ;; 		|  STAR pointer				{ ; }
   ;; 		|  STAR type_qualifier_list		{ ; }
   ;; 		|  STAR type_qualifier_list pointer	{ ; }
   ;; 		;

   ;; type_qualifier_list: type_qualifier
   ;; 		|  type_qualifier_list type_qualifier
   ;; 		;

   ;; parameter_type_list: parameter_list
   ;; 		| parameter_list comma ELLIPSIS
   ;; 		;

   ;; parameter_list:	   parameter_declaration
   ;; 		|  parameter_list comma parameter_declaration
   ;; 		;

   ;; parameter_declaration:
   ;; 		   declaration_specifiers declarator
   ;; 		|  declaration_specifiers
   ;; 		|  declaration_specifiers abstract_declarator
   ;; 		;

   ;; identifier_list:        Identifier
   ;; 		|  identifier_list comma Identifier
   ;; 		|  error { yyerror("identifier list error"); }
   ;; 		;

   (initializer
    ;;(assignment-expression %prec comma) HUH?
    (assignment-expression) : $1
    ;; initializer:       assignment_expression %prec comma
    ;; 		|  lbrace initializer_list rbrace		{ ; }
    ;; 		|  lbrace initializer_list comma rbrace		{ ; }
    ;; 		;
    )

   ;; initializer_list:         initializer %prec comma
   ;; 		|  initializer_list comma initializer
   ;; 		;

   ;; type_name:	   specifier_qualifier_list
   ;; 		|  specifier_qualifier_list abstract_declarator
   ;; 		;

   ;; abstract_declarator:	pointer
   ;; 		|  direct_abstract_declarator
   ;; 		|  pointer direct_abstract_declarator
   ;; 		;

   ;; direct_abstract_declarator:
   ;; 		   lparen abstract_declarator rparen		{ ; }
   ;; 		|  lbrace rbrace				{ ; }
   ;; 		|  direct_abstract_declarator lbrace rbrace
   ;; 		|  lbrace constant_expression rbrace		{ ; }
   ;; 		|  direct_abstract_declarator lbrace constant_expression rbrace
   ;; 		|  lparen rparen				{ ; }
   ;; 		|  direct_abstract_declarator lparen rparen
   ;; 		|  lparen parameter_list rparen			{ ; }
   ;; 		|  direct_abstract_declarator lparen parameter_list rparen
   ;; 		;

   
   (statement
    ;;(labeled-statement) 
    (expression-statement) : $1
    (compound-statement) : $1
    ;;(selection-statement)
    (iteration-statement) : $1
    (jump-statement) : $1
    (semicolon) : '()
    (error semicolon) : (begin (syntax-error "statement error" @1 $1) '())
    (error rbrace) : (begin (syntax-error "statement error" @1 $1) '()))
   		

   ;; labeled_statement:
   ;; 		   Identifier COLON statement
   ;; 		|  CASE x COLON statement		{ ; }
   ;; 		|  DEFAULT COLON statement		{ ; }
   ;; 		;

   (expression-statement
    (x semicolon) : $1)

   (compound-statement
    (lbrace rbrace) : '(compound)
    (lbrace declaration-list rbrace) : `(compound ,$2)
    (lbrace statement-list rbrace) :  `(compound ,@$2)
    (lbrace declaration-list statement-list rbrace) : `(compound ,$2 ,@$3))

   (statement-list
    (statement) : `(,$1)
    (statement-list statement) : `(,@$1 ,$2))
   
   ;; selection_statement:
   ;; 		   IF lparen x rparen statement			{ ; }
   ;; 		|  IF lparen x rparen statement ELSE statement	{ ; }
   ;; 		|  SWITCH lparen x rparen statement		{ ; }
   ;; 		;

   (iteration-statement
    ;; iteration_statement:
    ;; 		   WHILE lparen x rparen statement		{ ; }
    ;; 		|  DO statement WHILE lparen x rparen semicolon	{ ; }
    (for lparen forcntrl rparen statement) : `(for ,@$3 ,$5))
   
   (forcntrl
    ;; 		| semicolon semicolon x				{ ; }
    ;; 		| semicolon x semicolon				{ ; }
    ;; 		| semicolon x semicolon x				{ ; }
    ;; 		| x semicolon semicolon
    ;; 		| x semicolon semicolon x
    ;; 		| x semicolon x semicolon
    (x semicolon x semicolon x) : `((start ,$1) (test ,$3) (step ,$5)))

   (jump-statement
    (goto Identifier semicolon) : `(goto ,$2)
    (continue semicolon) : '(continue)
    (break semicolon) : '(break)
    (return semicolon) : '(return)
    (return x semicolon) : `(return ,$2))

   (x
    (assignment-expression) : $1
    (x comma assignment-expression) : `(,$1 ,@$3))
   		
   (assignment-expression
    (equality-expression) : $1 ;; skip some
    ;;(conditional-expression) : $1
    (unary-expression assignment-operator assignment-expression) : `(,$2 ,$1 ,$3))

   (assignment-operator
    (=) : '=)
   ;; 		|  PLUSEQ				{ ; }
   ;; 		|  MINUSEQ				{ ; }
   ;; 		|  MUEQ					{ ; }
   ;; 		|  DIVEQ				{ ; }
   ;; 		|  MODEQ				{ ; }
   ;; 		|  SLEQ				{ ; }
   ;; 		|  SREQ				{ ; }
   ;; 		|  ANEQ				{ ; }
   ;; 		|  OREQ				{ ; }
   ;; 		|  XOREQ				{ ; }
   ;; 		;

   ;; conditional_expression: logical_or_expression
   ;; 		|  logical_or_expression IF_THEN x COLON conditional_expression
   ;; 		;

   ;; constant_expression: conditional_expression
   ;; 		;

   ;; logical_or_expression: logical_and_expression
   ;; 		|  logical_or_expression OROR logical_and_expression
   ;; 		;

   ;; logical_and_expression: inclusive_or_expression
   ;; 		|  logical_and_expression ANDAND inclusive_or_expression
   ;; 		;

   ;; inclusive_or_expression: exclusive_or_expression
   ;; 		|  inclusive_or_expression OR exclusive_or_expression
   ;; 		;

   ;; exclusive_or_expression: and_expression
   ;; 		|  exclusive_or_expression XOR and_expression
   ;; 		;

   ;; and_expression: equality_expression
   ;; 		|  and_expression AND equality_expression
   ;; 		;

   (equality-expression
    (relational-expression) : $1
    (equality-expression == relational-expression) : `(== ,$1 ,$3)
    (equality-expression != relational-expression) : `(!= ,$1 ,$3))

   (relational-expression
    (shift-expression) : $1
    (relational-expression < shift-expression) : `(< ,$1 ,$3)
    (relational-expression <= shift-expression) : `(<= ,$1 ,$3)
    (relational-expression > shift-expression) : `(> ,$1 ,$3)
    (relational-expression >= shift-expression) : `(>= ,$1 ,$3))

   (shift-expression
    (unary-expression) : $1 ;; skip some
    ;; shift_expression: additive_expression
    ;; 		|  shift_expression LTLT additive_expression
    ;; 		|  shift_expression GTGT additive_expression
    ;; 		;
    )
   ;; additive_expression: multiplicative_expression
   ;; 		|  additive_expression PLUS multiplicative_expression
   ;; 		|  additive_expression MINUS multiplicative_expression
   ;; 		;

   ;; multiplicative_expression: cast_expression
   ;; 		|  multiplicative_expression STAR cast_expression
   ;; 		|  multiplicative_expression DIV cast_expression
   ;; 		|  multiplicative_expression MOD cast_expression
   ;; 		;

   ;; cast_expression:   unary_expression
   ;; 		|  lparen type_name rparen cast_expression	{ ; }
   ;; 		;

   (unary-expression
    (postfix-expression) : $1
    (++ unary-expression) : `(++x ,$2)
    (-- unary-expression) : `(--x ,$2)
   ;; 		|  SIZEOF unary_expression		{ ; }
   ;; 		|  SIZEOF lparen type_name rparen %prec SIZEOF	{ ; }
   ;; 		|  STAR cast_expression			{ ; }
   ;; 		|  AND cast_expression			{ ; }
   ;; 		|  MINUS cast_expression		{ ; }
   ;; 		|  PLUS cast_expression			{ ; }
   ;; 		|  NEG cast_expression			{ ; }
   ;; 		|  NOT cast_expression			{ ; }
   ;; 		;
    )

   (postfix-expression
    (primary-expression) : $1
    ;; 		|  postfix_expression lbracket x rbracket
    (postfix-expression lparen rparen) : `(call ,$1 (arguments))
    (postfix-expression lparen argument-expression-list rparen) : `(call ,$1 ,$3)
    ;; 		|  postfix_expression FOLLOW Identifier
    ;; 		|  postfix_expression DOT Identifier
    (postfix-expression ++) : `(x++ ,$1)
    (postfix-expression --) : `(x-- ,$1)
    )

   (primary-expression
    (Identifier): $1
    (NumericLiteral) : $1
    ;; INT_LITERAL
    ;; CHAR_LITERAL
    ;; FLOAT_LITERAL
    ;; STRING_LITERAL
    (StringLiteral) : $1
    ;; lparen x rparen
    )
   ;; 		

   (argument-expression-list
    (assignment-expression) : `(arguments ,$1)
    (argument-expression-list comma assignment-expression): `(,@$1 ,@(cdr $3)))))

(define (i386:exit code)
  `(
    #xbb ,@(int->bv32 code)        ;; mov    $code,%ebx
         #xb8 #x01 #x00 #x00 #x00       ;; mov    $0x1,%eax
         #xcd #x80                      ;; int    $0x80
         ))

(define (i386:puts data length)
  `(
     #xba ,@(int->bv32 length)           ;; mov    $length,%edx
          #xb9 ,@(int->bv32 data)        ;; mov    $data,%ecx
          #xbb #x01 #x00 #x00 #x00       ;; mov    $0x1,%ebx
          #xb8 #x04 #x00 #x00 #x00       ;; mov    $0x4,%eax
          #xcd #x80                      ;; int    $0x80
          ))

(define (i386:for start test step statement)
`(

  ;;   b:
  #x89 #xe5                	;; mov    %esp,%ebp
       ;;21:
       #xc7 #x45 #xf4 ,@(int->bv32 start) ;;	movl   $start,-0xc(%ebp)
       ;;28:
       #xeb ,(+ (length statement) 9) ;;x14	jmp    3e <main+0x3e>
       ;;2a:
       ;;#x83 #xec #x0c             ;;	sub    $0xc,%esp
       
       ;;   9:
       #x55   ;;                	push   %ebp
       
       ,@statement
       #x5d   ;;                	pop   %ebp
       ;;2d:
 ;;;;;;#x68 #x09 #x00 #x00 #x00       ;;	push   $0x9
       ;;32:
 ;;;;;;#xe8 #xfc #xff #xff #xff       ;;	call   33 <main+0x33>
       ;;37:
 ;;;;;;#x83 #xc4 #x10             ;;	add    $0x10,%esp
       ;;3a:
       ;;;;#x83 #x45 #xf4 ,step          ;;	addl   $step,-0xc(%ebp)
       ;;3e:
       ;;;;#x83 #x7d #xf4 ,test          ;;	cmpl   $test,-0xc(%ebp)
       #x81 #x45 #xf4 ,@(int->bv32 step) 	;;addl   $step,-0xc(%ebp)
       #x81 #x7d #xf4 ,@(int->bv32 test) ;;cmpl   $0x7cff,-0xc(%ebp)
       ;;42:
       ;;;;#x7e ,(- 0 (length statement) 18) ;;#xe6 ;;	jle    2a <main+0x2a>
       #x75 ,(- 0 (length statement) 18) ;;#xe6 ;;	jne    2a <main+0x2a>
))


(define mescc
  (let ((errorp
         (lambda args
           (for-each display args)
             (newline))))
    (lambda ()
      (c-parser (c-lexer errorp) errorp))))

(define (write-any x) (write-char (if (char? x) x (integer->char (if (>= x 0) x (+ x 256))))))

(define (ast:function? o)
  (and (pair? o) (eq? (car o) 'function)))

(define (.name o)
  (cadr o))

;; (define (.statement o)
;;   (match o
;;     (('function name signature statement) statement)
;;     (_ #f)))

;; (define (statement->data o)
;;   (match o
;;     (('call 'puts ('arguments string)) (string->list string))
;;     (_ '())))

;; (define (statement->text o)
;;   (match o
;;     (('call 'puts ('arguments string)) (list (lambda (data) (i386:puts data (string-length string)))))
;;     (('return code) (list (lambda (data) (i386:exit code))))
;;     (_ '())))

(define (.statement o)
  (and (pair? o)
       (eq? (car o) 'function)
       (cadddr o)))

(define (statement->data o)
  (or (and (pair? o)
           (eq? (car o) 'call)
           (string->list (cadr (caddr o))))
      (and (pair? o) (eq? (car o) 'for)
           (let ((statement (cadr (cdddr o))))
             (statement->data statement)))
      '()))

(define (statement->text data o)
  (cond
   ((and (pair? o) (eq? (car o) 'call))
    (let ((string (cadr (caddr o)))
          (offset (length data)))
      (list (lambda (data) (i386:puts (+ data offset) (string-length string))))))
   ((and (pair? o) (eq? (car o) 'for))
    (let ((start (cadr o))
          (test (caddr o))
          (step (cadddr o))
          (statement (cadr (cdddr o))))
      (display "start:" (current-error-port))
      (display start (current-error-port))
      (newline (current-error-port))

      (display "test:" (current-error-port))
      (display test (current-error-port))
      (newline (current-error-port))

      (display "step:" (current-error-port))
      (display step (current-error-port))
      (newline (current-error-port))

      (display "for-statement:" (current-error-port))
      (display statement (current-error-port))
      (newline (current-error-port))

      (let ((start (cadr (cdadr start)))
            (test (cadr (cdadr test)))
            ;;(step (cadr (cdadr step)))
            (step 1)
            (statement (car (statement->text data statement)))
            )
        (display "2start:" (current-error-port))
        (display start (current-error-port))
        (newline (current-error-port))

      (display "2for-statement:" (current-error-port))
      (display statement (current-error-port))
      (newline (current-error-port))

        (list (lambda (d) (i386:for start test step (statement d)))))))
   ((and (pair? o) (eq? (car o) 'return))
    (list (lambda (data) (i386:exit (cadr o)))))
   (else '())))

(let* ((ast (mescc))
       (functions (filter ast:function? (cdr ast)))
       (main (find (lambda (x) (eq? (.name x) 'main)) functions))
       (statements (cdr (.statement main))))
  (display "program: " (current-error-port))
  (display ast (current-error-port))
  (newline (current-error-port))
  (let loop ((statements statements) (text '()) (data '()))
    (display "text:" (current-error-port))
    (display text (current-error-port))
    (newline (current-error-port))
    (if (null? statements)
        (map write-any (make-elf (lambda (data)
                                   (append-map (lambda (f) (f data)) text)) data))
        (let* ((statement (car statements)))
          (display "statement:" (current-error-port))
          (display statement (current-error-port))
          (newline (current-error-port))
          (loop (cdr statements)
                (append text (statement->text data statement))
                (append data (statement->data statement)))))))
