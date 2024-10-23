#lang typed/racket
(require typed/rackunit)

(define-type ExprC (U numC stringC idC appC ifC lamC))
(define-type Value (U numV boolV closV primV))

(struct numV[(n : Real)] #:transparent)
(struct boolV[(bool : Boolean)] #:transparent)
(struct closV [(arg : (Listof idC)) (body : ExprC) (env : Enviroment)])
(struct primV [(arg : Symbol) (body : ExprC) (env : Enviroment)])

(struct lamC [(args : (Listof Symbol)) (body : ExprC)] #:transparent)
(struct appC [(func : ExprC) (args : (Listof ExprC))] #:transparent)
(struct idC [(name : Symbol)] #:transparent)
(struct numC[(n : Real)] #:transparent)
(struct stringC[(str : String)] #:transparent)
(struct ifC [(test : ExprC) (then : ExprC) (else : ExprC)] #:transparent)

(struct Enviroment [(bindings : (Listof (Pairof idC Value)))] #:transparent)



;;defining hash table for invalid identifier.
;;These symbols cannot be made into functions because we use these in our language
(define invalid-table
  (hash
   'if 0
   '=> 0))


(define top-level-env
  (list
   (cons (idC 'true) #t)
   (cons (idC 'false) #f)))


(define (lookup [for : Symbol] [env : Enviroment]) : Value
  (match env
    ['() (error 'lookup "name not found in env AAQZ4")]
    [(cons (cons (idC name) val) rest)
     (if (symbol=? for name)
         val
         (lookup for (Enviroment rest)))]))

(define (extend-env [env : Enviroment] [new : (Pairof idC Value)]) : Enviroment
  (Enviroment (cons new (Enviroment-bindings env))))

(define (interp [expr : ExprC] [env : Enviroment]) : Value
  (match expr
    [(numC n) (numV n)]
    [(idC n) (lookup n env)]
    [(lamC )]
    [(appC f a) (interp f) ]))





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
       [else (lamC (check-duplicate-arg (map idC args)) (parse body))]) ;using '() as place holder top environment replace later
     ]
    [other (error 'parse "syntax error in AAQZ3, got ~e" other)]))

#;(define (serialize [expr : ExprC]) : String
  1)


;;takes in a list of idC representing arguments and checks if there are any duplicate names for
;;arguments in the given list using check-duplicate-arg-helper. Returns the list of arguments.
(define (check-duplicate-arg [args : (Listof idC)]) : (Listof idC)
  (match args
    ['() '()]
    [(cons first rest) (cons (check-duplicate-arg-helper first rest) (check-duplicate-arg rest))]))

;;Takes in an idC called 'new' and a list of idC and checks whether 'new' is in the list of idC. Throws
;;an error if new is found in the list of idC.
(define (check-duplicate-arg-helper [new : idC] [existing : (Listof idC)]) : idC
  (match existing
    ['() new]
    [(cons arg rest)
     (if (equal? new arg)
         (error "AAQZ3 found a syntax error repeated argument name\n")
         (check-duplicate-arg-helper new rest))]))



