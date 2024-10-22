#lang typed/racket
(require typed/rackunit)

(define-type ExprC (U numC binopC FundefC idC appC ifleq0?))
(define-type Value (U numV boolV closV primV))

(struct numV[(n : Real)] #:transparent)
(struct boolV[(bool : Boolean)] #:transparent)
(struct closV [(arg : symbol) (body : ExprC) (env : EnvC)])
(struct primV [(arg : symbol) (body : ExprC) (env : EnvC)])

(struct lamC [(args : (Listof Symbol)) (body : ExprC)] #:transparent)
(struct appC [(func : exprC) (args : (Listof ExprC))] #:transparent)
(struct idC [(name : Symbol)] #:transparent)
(struct numC[(n : Real)] #:transparent)
(struct stringC[(str : String)] #:transparent)
(struct ifC [(test : ExprC) (then : ExprC) (else : ExprC)] #:transparent)

(struct Enviroment [(bindings : (Listof (Pairof idC ExprC)))] #:transparent)




(define top-level-env
  (list
   (cons (idC 'true) #t)
   (cons (idC 'false) #f)))


(define (lookup [for : symbol] [env : Enviroment]) : Real
  (match env
    [(empty? env) (error 'lookup "name not found in env AAQZ4")]
    [(cons (cons (idC name) val) rest)
     (if (symbol=? for name)
         val
         (lookup for (Enviroment rest)))]))

(define (extend-env [env : Enviroment] [new : bindC]) : envC
  (envC (cons new (Enviroment-bindings env))))

(define (interp [expr : ExprC] [env : Enviroment]) : Value
  (match expr
    [(numC n) n]
    [(idC n) (lookup n env)]
    [(appC f a) (... add to empty env...)]))


;;takes in an S-expression and parses it into our AAQZ3 language in the form of an ExprC.
;;Checks for invalid syntaxes and invalid identifiers.
(define (parse [prog : Sexp]) : ExprC 
  (match prog
    [(? real? n) (numC n)]
    [(list 'ifleq0? test then else) (ifleq0? (parse test) (parse then) (parse else))]
    [(list (? symbol? op) l r)
     (if (hash-has-key? op-table op)
         (binopC op (parse l) (parse r))
         (if (hash-has-key? invalid-table op) ;;make sure op is valid id 
             (error 'parse "Invalid identifier: ~a in AAQZ3" prog)
             (appC (idC op) (list (parse l) (parse r)))))] 
    [(list (? symbol? s) args ...)
     (if (hash-has-key? invalid-table s)
         (error 'parse "Invalid identifier: ~a in AAQZ3" prog)
         (appC (idC s) (map parse args)))]
    [(? symbol? s)
     (if (hash-has-key? invalid-table s)
         (error 'parse "Invalid identifier: ~a in AAQZ3" prog)
         (idC s))]
    [other (error 'parse "syntax error in AAQZ3, got ~e" other)]))

(define (serialize [expr : ExprC]) : String
  1)




