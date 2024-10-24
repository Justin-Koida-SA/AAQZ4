#lang typed/racket
(require typed/rackunit)

(define-type ExprC (U numC stringC idC appC ifC lamC))
(define-type Value (U numV boolV closV primV))

(struct numV[(n : Real)] #:transparent)
(struct boolV[(bool : Boolean)] #:transparent)
(struct closV [(arg : (Listof Symbol)) (body : ExprC) (env : Enviroment)])
(struct primV [(arg : Symbol)])

(struct lamC [(args : (Listof Symbol)) (body : ExprC)] #:transparent)
(struct appC [(func : ExprC) (args : (Listof ExprC))] #:transparent)
(struct idC [(name : Symbol)] #:transparent)
(struct numC[(n : Real)] #:transparent)
(struct stringC[(str : String)] #:transparent)
(struct ifC [(test : ExprC) (then : ExprC) (else : ExprC)] #:transparent)

;;(struct Enviroment [(bindings : (Listof (Pairof Symbol Value)))] #:transparent)
(define-type Environment (Listof binding))
(struct binding [(bound : Symbol) (val : Value)] #:transparent)


;;defining hash table for invalid identifier.
;;These symbols cannot be made into functions because we use these in our language
(define invalid-table
  (hash
   'if 0
   '=> 0
   '= 0))


(define top-level-env : Environment
  (list
   (cons 'true (boolV #t))
   (cons 'false (boolV #f))
   (cons '+ (primV '+))
   (cons '- (primV '-))
   (cons '/ (primV '/))
   (cons '* (primV '*))))


(define (lookup [for : Symbol] [env : Environment]) : Value
  (match env
    ['() (error 'lookup "name not found in env AAQZ4")]
    [(cons (cons (idC name) val) rest)
     (if (symbol=? for name)
         val
         (lookup for (env rest)))]))

(define (extend-env [env : Environment] [news : Environment]) : Environment
  (match news
    ['() env]
    [(cons f r) (cons f (extend-env env r))])
  )

(define (interp [expr : ExprC] [env : Enviroment]) : Value
  (match expr
    [(numC n) (numV n)]
    [(idC n) (lookup n env)]
    [(lamC args body) (closV args body env)]
    [(appC f a) (app-intrp-helper (interp f env) a)]
    [(ifC test then else)
     (if (interp test)
         (interp then)
         (interp else))]
    
    ))

(define (app-intrp-helper [closer : closV] [args : (Listof Value)]) : Value
  (match closer
    [(closV syms body env) (interp body (extend-env env (zip syms args)))])
  )

(define (zip [l1 : (Listof Symbol)] [l2 : (Listof Value)]) : Environment
  (match (list l1 l2)
    [(list '() '()) '()]
    [(list (cons f1 r1) (cons f2 r2)) (cons (cons f1 f2) (zip r1 r2))]
    [other (error 'zip "Number of variables and arguments do not match AAQZ4: ~a" other)]))




;;takes in an S-expression and parses it into our AAQZ3 language in the form of an ExprC.
;;Checks for invalid syntaxes and invalid identifiers.
(define (parse [prog : Sexp]) : ExprC 
  (match prog
    [(? real? n) (numC n)]
    [(list 'if test then else) (ifC (parse test) (parse then) (parse else))]
    #;[(list (? symbol? op) l r)
     (if (hash-has-key? op-table op)
         (binopC op (parse l) (parse r))
         (if (hash-has-key? invalid-table op) ;;make sure op is valid id 
             (error 'parse "Invalid identifier: ~a in AAQZ3" prog)
             (appC (idC op) (list (parse l) (parse r)))))] 
    [(list s args ...) (appC (parse s) (map parse args))]
    [(? symbol? s)
     (if (hash-has-key? invalid-table s)
         (error 'parse "Invalid identifier: ~a in AAQZ3" prog)
         (idC s))]
    [(list (list args ...) '=> body)
     (cond
       [(not (andmap symbol? args)) (error 'parse "AAQZ Expected a list of symbols for arguments got ~a" args)]
       [else (lamC (check-duplicate-arg  args) (parse body))]) ;using '() as place holder top environment replace later
     ]
    [other (error 'parse "syntax error in AAQZ4, got ~e" other)]))

#;(define (serialize [expr : ExprC]) : String
  1)


;;takes in a list of idC representing arguments and checks if there are any duplicate names for
;;arguments in the given list using check-duplicate-arg-helper. Returns the list of arguments.
(define (check-duplicate-arg [args : (Listof Symbol)]) : (Listof Symbol)
  (match args
    ['() '()]
    [(cons first rest) (cons (check-duplicate-arg-helper first rest) (check-duplicate-arg rest))]))

;;Takes in an idC called 'new' and a list of idC and checks whether 'new' is in the list of idC. Throws
;;an error if new is found in the list of idC.
(define (check-duplicate-arg-helper [new : Symbol] [existing : (Listof Symbol)]) : Symbol
  (match existing
    ['() new]
    [(cons arg rest)
     (if (equal? new arg)
         (error "AAQZ4 found a syntax error repeated argument name\n")
         (check-duplicate-arg-helper new rest))]))



