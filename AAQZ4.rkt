#lang typed/racket
(require typed/rackunit)

(define-type ExprC (U numC stringC idC appC ifC lamC))
(define-type Value (U numV boolV closV primV stringV))

(struct numV[(n : Real)] #:transparent)
(struct boolV[(bool : Boolean)] #:transparent)
(struct closV [(arg : (Listof Symbol)) (body : ExprC) (env : Environment)])
(struct primV [(arg : Symbol)])
(struct stringV [(str : String)])

(struct lamC [(args : (Listof Symbol)) (body : ExprC)] #:transparent)
(struct appC [(func : ExprC) (args : (Listof ExprC))] #:transparent)
(struct idC [(name : Symbol)] #:transparent)
(struct numC[(n : Real)] #:transparent)
(struct stringC[(str : String)] #:transparent)
(struct ifC [(test : ExprC) (then : ExprC) (else : ExprC)] #:transparent)

;;(struct Enviroment [(bindings : (Listof (Pairof Symbol Value)))] #:transparent)
(define-type Environment (Listof binding))
(struct binding [(bound : Symbol) (val : Value)] #:transparent)

(struct bindpair [(funName : (Listof Symbol)) (fun : (Listof ExprC))])

;;defining hash table for invalid identifier.
;;These symbols cannot be made into functions because we use these in our language
(define invalid-table
  (hash
   'if 0
   '=> 0
   '= 0))


(define top-level-env : Environment
  (list
   (binding 'true (boolV #t))
   (binding 'false (boolV #f))
   (binding '+ (primV '+))
   (binding '- (primV '-))
   (binding '/ (primV '/))
   (binding '* (primV '*))
   (binding '<= (primV '<=))))


(define (lookup [for : Symbol] [env : Environment]) : Value
  (match env
    ['() (error 'lookup "name not found in env AAQZ4")]
    [(cons (binding name val) rest)
     (if (symbol=? for name)
         val
         (lookup for rest))]))

(define (extend-env [env : Environment] [news : Environment]) : Environment
  (match news
    ['() env]
    [(cons f r) (cons f (extend-env env r))])
  )
 
(define (top-interp [prog : Sexp]) : String
  (serialize (interp (parse prog) top-level-env)))

(define (interp [expr : ExprC] [env : Environment]) : Value
  (match expr
    [(numC n) (numV n)]
    [(stringC str) (stringV str)]
    [(idC n) (lookup n env)]
    [(lamC args body) (closV args body env)]
    [(appC f a)
     (define resolved-f (interp f env))
     (match resolved-f
       [(primV _) (do-math resolved-f a env)]
       [(closV args body env) (app-intrp-helper resolved-f (interp-args a env))]
       [other other])]
    [(ifC test then else)
     (if (interp test env)
         (interp then env)
         (interp else env))]))

(define (do-math [op : primV] [arg : (Listof ExprC)] [env : Environment]) : Value
  (match  arg
    [(list l r)
     (define interp-l (interp l env))
     (define interp-r (interp r env))
     (match op
       [(primV '+) 
        (if (and (numV? interp-l) (numV? interp-r))
            (numV (+ (numV-n interp-l) (numV-n interp-r)))
            (error  "AAQZ4 need an integer with ~a operator" '+))]
       [(primV '-)
        (if (and (numV? interp-l) (numV? interp-r))
            (numV (- (numV-n interp-l) (numV-n interp-r)))
            (error  "AAQZ4 need an integer with ~a operator" '-))]
       [(primV '*)
        (if (and (numV? interp-l) (numV? interp-r))
            (numV (* (numV-n interp-l) (numV-n interp-r)))
            (error  "AAQZ4 need an integer with ~a operator" '*))]
       [(primV '/)
        (if (and (numV? interp-l) (numV? interp-r))
            (if (not (= (numV-n interp-r) 0))
                (numV (/ (numV-n interp-l) (numV-n interp-r)))
                (error "AAQZ4 cant divide by zero :("))
            (error  "AAQZ4 need an integer with ~a operator" '/))]
       [(primV '<=)
        (if (and (numV? interp-l) (numV? interp-r) )
            (boolV (<= (numV-n interp-l) (numV-n interp-r)))
            (error  "AAQZ4 need an integer with ~a operator" '<=))]
       [(primV 'equal?)
        (cond
          [(and (numV? interp-l) (numV? interp-r)) (boolV (= (numV-n interp-l) (numV-n interp-r)))]
          [(and (stringV? interp-l) (stringV? interp-r)) (boolV (string=? (stringV-str interp-l) (stringV-str interp-r)))]
          [(and (boolV? interp-l) (boolV? interp-r)) (boolV (eq? (boolV-bool interp-l) (boolV-bool interp-r)))]
          [else (error  "AAQZ4 can only check equal for num string and bool" )])]
          
       )]
    [other (error  "wrong number of variable for primV AAQZ4: ~a" other)]))



(define (app-intrp-helper [closer : closV] [args : (Listof Value)]) : Value
  (match closer
    [(closV syms body env) (interp body (extend-env env (bind syms args)))])
  )

 
(define (bind [l1 : (Listof Symbol)] [l2 : (Listof Value)]) : Environment
  (match (list l1 l2)
    [(list '() '()) '()]
    [(list (cons f1 r1) (cons f2 r2)) (cons (binding f1 f2) (bind r1 r2))]
    [other (error 'bind "Number of variables and arguments do not match AAQZ4: ~a" other)]))

(define (interp-args [args : (Listof ExprC)] [env : Environment]) : (Listof Value)
  (match args
    ['() '()]
    [(cons other r) (cons (interp other env) (interp-args r env))]))

(define (serialize [v : Value]) : String
  (match v
    [(numV n) (number->string n)] 
    [(stringC str) (stringV str)]
    [(boolV bool) (if bool "true" "false")]
    [(closV _ _ _) "#<procedure>"]
    [(primV _) "#<primop>"]))


;;takes in an S-expression and parses it into our AAQZ3 language in the form of an ExprC.
;;Checks for invalid syntaxes and invalid identifiers.
(define (parse [prog : Sexp]) : ExprC 
  (match prog
    [(list 'bind clauses ... expr)
     (define pClause (parse-binds (cast clauses (Listof Sexp))))
     (appC (lamC (check-duplicate-arg (bindpair-funName pClause)) (parse expr)) (bindpair-fun pClause))]
    [(list (list args ...) '=> body)
     (cond
       [(not (andmap symbol? args)) (error 'parse "AAQZ Expected a list of symbols for arguments got ~a" args)]
       [else (lamC (check-duplicate-arg  args) (parse body))])]
    [(? real? n) (numC n)]
    [(? string? str) (stringC str)]
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
    [other (error 'parse "syntax error in AAQZ4, got ~e" other)]))

(define (parse-binds [clauses : Sexp]) : bindpair
  (match clauses
    ['() (bindpair '() '())]
    [(cons (list (? symbol? id) '= expr) r)
     (let* ([parsed-rest (parse-binds r)] 
            [ids (cons id (bindpair-funName parsed-rest))]
            [exprs (cons (parse expr) (bindpair-fun parsed-rest))]) 
       (bindpair ids exprs))]))


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

;;test
(check-equal? (parse
               '{bind [x = 5]
                      [y = 7]
                      {+ x y}})
              (appC (lamC '(x y)
                          (appC (idC '+)
                                (list (idC 'x)
                                      (idC 'y))))
                    (list (numC 5) (numC 7))))


(check-equal? (parse
               '{bind [x = 5]
                      [y = 7]
                      {+ x y}})
              (appC (lamC '(x y)
                          (appC (idC '+)
                                (list (idC 'x)
                                      (idC 'y))))
                    (list (numC 5) (numC 7))))

(check-equal? (parse
               '{{(add1) => {add1 42}}
                {(x) => {+ x 1}}})
              (appC (lamC '(add1) (appC (idC 'add1)
                                        (list (numC 42))))
                    (list (lamC '(x) (appC (idC '+)
                                           (list (idC 'x)
                                                 (numC 1)))))))
(check-equal? (parse
               '{{(x y) => {+ x y}}
                5 7}) (appC (lamC '(x y)
                                  (appC (idC '+)
                                        (list (idC 'x)
                                              (idC 'y))))
                            (list (numC 5) (numC 7)))) 


(check-equal? (top-interp
               '{{(add1) => {add1 42}}
                {(x) => {+ x 1}}}) "43")

(check-equal? (top-interp
               '{{(min1) => {min1 42}}
                {(x) => {- x 1}}}) "41")

(check-equal? (top-interp
               '{{(mult2) => {mult2 42}}
                {(x) => {* x 2}}}) "84")

(check-equal? (top-interp
               '{{(div3) => {div3 9}}
                {(x) => {/ x 3}}}) "3")

(check-equal? (top-interp
               '{{(noArg) => {noArg}}
                {() => {3}}}) "3")

(check-equal? (top-interp
               '{{(noArg) => {noArg}}
                {() => {3}}}) "3")

(check-equal? (top-interp '(+ 2 3)) "5")
(check-equal? (top-interp '{if true 34 39}) "34")
(check-equal? (top-interp '{{(x y) => {+ x y}} 4 3}) "7")

(check-exn #rx"AAQZ4 found a syntax error repeated argument name\n"
           (lambda ()
             (top-interp
               '{{(add1) => {add1 42}}
                {(x x) => {+ x 1}}})))

(check-equal?
             (top-interp
               '{bind [x = 5]
                      [y = 7]
                      {+ x y}}) "12")
(check-equal?
             (top-interp
               '{bind  {12}}) "12")


(check-exn #rx"Number of variables and arguments do not match AAQZ4"
           (lambda ()
             (top-interp
               '{{(div3) => {div3 9 5}}
                {(x) => {/ x 3}}})))
 
(check-exn #rx"AAQZ4 need an integer"
           (lambda ()
             (top-interp
               '{{(prim) => {prim "42"}}
                {(x) => {+ x 1}}})))

(check-exn #rx"AAQZ4 need an integer"
           (lambda ()
             (top-interp
               '{{(prim) => {prim "42"}}
                {(x) => {* x 1}}})))

(check-exn #rx"AAQZ4 need an integer"
           (lambda ()
             (top-interp
               '{{(prim) => {prim "42"}}
                {(x) => {- x 1}}})))

(check-exn #rx"AAQZ4 need an integer"
           (lambda ()
             (top-interp
               '{{(prim) => {prim "42"}}
                {(x) => {/ x 1}}})))

(check-exn #rx"AAQZ4 cant divide by zero"
           (lambda ()
             (top-interp
               '{{(prim) => {prim 42}}
                {(x) => {/ x 0}}})))