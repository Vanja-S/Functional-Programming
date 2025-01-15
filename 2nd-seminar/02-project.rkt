#lang racket

;;; (provide false true int .. empty exception
;;;          trigger triggered handle
;;;          if-then-else
;;;          ?int ?bool ?.. ?seq ?empty ?exception
;;;          add mul ?leq ?= head tail ~ ?all ?any
;;;          vars valof fun proc closure call
;;;          greater rev binary filtering folding mapping
;;;          fri)

(provide (all-defined-out))

;;; Data Types ;;;

;;; Integers
(struct int (value) #:transparent)

;;; Booleans
(struct true () #:transparent)
(struct false () #:transparent)

;;; Sequence
(struct empty () #:transparent)
(struct .. (e1 e2) #:transparent)

;;; Exceptions
(struct exception (exn) #:transparent)


;;; Stream Handlers ;;;

; Define the trigger structs for internal exception mechanism
(struct triggered (e) #:transparent)

(struct trigger (e) #:transparent)

(struct handle (e1 e2 e3) #:transparent)

; Flow handling
(struct if-then-else (condition e1 e2) #:transparent)

; Type checking
(struct ?int (e1) #:transparent)

(struct ?bool (e1) #:transparent)

(struct ?.. (e1) #:transparent)

(struct ?seq (e1) #:transparent)

(struct ?empty (e1) #:transparent)

(struct ?exception (e1) #:transparent)

; Arithmetic and logical operations
(struct add (e1 e2) #:transparent)

(struct mul (e1 e2) #:transparent)

(struct ?leq (e1 e2) #:transparent)

(struct ?= (e1 e2) #:transparent)

(struct ~ (e1) #:transparent)

; Sequence operations
(struct head (e) #:transparent)

(struct tail (e) #:transparent)

; all and any
(struct ?all (e))

(struct ?any (e))

;;; Variables

(struct vars (s e1 e2) #:transparent)

(struct valof (s) #:transparent)

;;; Functions and closures

(struct fun (name fargs body) #:transparent)

(struct proc (name body) #:transparent)

(struct closure (env f) #:transparent)

(struct call (e args) #:transparent)

;;; Helper functions

(define (len seq)
  (define (len-helper s acc)
    (cond
      [(empty? s) acc]
      [(..? s) (len-helper (..-e2 s) (+ 1 acc))]))
  
  (len-helper seq 0))

(define (has-duplicates? lst)
  (not (= (length lst) 
          (length (remove-duplicates lst)))))

(define (zip lst1 lst2)
  (if (or (null? lst1) (null? lst2))
      '()
      (cons (cons (car lst1) (car lst2))
            (zip (cdr lst1) (cdr lst2)))))

(define (collect-valof-vars expr)
  (match expr
    ; Base cases - no valof references
    [(or (int _) (true) (false) (empty) (exception _)) '()]
    
    ; Recursive cases for different expression types
    [(valof s) (list s)]  ; Found a valof reference
    
    ; Binary operations - check both operands
    [(or (add e1 e2) (mul e1 e2) (?leq e1 e2) (?= e1 e2))
     (append (collect-valof-vars e1) (collect-valof-vars e2))]
    
    ; Unary operations
    [(or (~ e) (head e) (tail e) (?int e) (?bool e) (?.. e) (?seq e) (?empty e) (?exception e))
     (collect-valof-vars e)]
    
    ; Sequence
    [(.. e1 e2)
     (append (collect-valof-vars e1) (collect-valof-vars e2))]
    
    ; Control structures
    [(if-then-else cond e1 e2)
     (append (collect-valof-vars cond) 
             (collect-valof-vars e1)
             (collect-valof-vars e2))]
    
    ; Exception handling
    [(trigger e) (collect-valof-vars e)]

    [(handle e1 e2 e3)
     (append (collect-valof-vars e1)
             (collect-valof-vars e2)
             (collect-valof-vars e3))]
    
    ; Nested function definitions - exclude their body from analysis
    [(fun _ _ _) '()]
    
    ; Function calls - check the function expression and arguments
    [(call e args)
     (append (collect-valof-vars e)
             (apply append (map collect-valof-vars args)))]
    
    ; Variable bindings - only check e1, not e2 since it creates new scope
    [(vars s e1 e2)
     (collect-valof-vars e1)]
    
    ; Default case for any unmatched patterns
    [_ '()]))

;;; Main ;;;

; Add handling triggered exceptions to all stream handlers

(define (fri expression environment)
  (match expression
    ; Basic data types - return as is
    [(or (int _) (true) (false) (empty) (triggered _) (exception _) ) expression]
    
    ; Sequence type - return executed
    [(.. e1 e2) 
      (let ([v1 (fri e1 environment)]
            [v2 (fri e2 environment)])
        (.. v1 v2))]

    ; Trigger expression
    [(trigger e)
      (let ([v (fri e environment)])
        (cond
          [(triggered? v) v]
          [(exception? v) (triggered v)]
          [else (triggered (exception "trigger: wrong argument type"))]))]

    ; Handle expression
    [(handle e1 e2 e3)
    (let ([v1 (fri e1 environment)])
      (cond
        [(triggered? v1) v1]  ; If e1 evaluates to triggered exception, return it
        [(not (exception? v1)) 
        (triggered (exception "handle: wrong argument type"))]  ; If e1 not exception, error
        [else
        (let ([v2 (fri e2 environment)])
          (if (and (triggered? v2) (equal? (triggered-e v2) v1))
              (fri e3 environment)  ; If e2 throws exception, evaluate e3
              v2))]))]  ; Otherwise return result of e2
    
    ; If-then-else
    [(if-then-else condition e1 e2)
    (let ([cond-result (fri condition environment)])
      (if (false? cond-result)
          (fri e2 environment)
          (fri e1 environment)))]
    
    ; Type checking
    [(?int e)
    (let ([v (fri e environment)])
      (cond
        ; First check for triggered exceptions
        [(triggered? v) v]
        [else (if (int? v) (true) (false))]))]
    
    [(?bool e)
    (let ([v (fri e environment)])
      (cond 
        ; First check for triggered exceptions
        [(triggered? v) v]
        [else (if (or (true? v) (false? v)) (true) (false))]))]
    
    [(?.. e)
    (let ([v (fri e environment)])
      (cond 
        ; First check for triggered exceptions
        [(triggered? v) v]

        [else (if (..? v) 
                (let ([head (fri (..-e1 e) environment)]
                      [tail (fri (..-e2 e) environment)])
                  (cond
                    [(triggered? head) head]
                    [(triggered? tail) tail]
                    [else (true)]
                  )) 
                (false))]))]
    
    [(?empty e)
    (let ([v (fri e environment)])
      (cond
        ; First check for triggered exceptions
        [(triggered? v) v]
        [else (if (empty? v) (true) (false))]))]
    
    [(?exception e)
    (let ([v (fri e environment)])
      (cond
        ; First check for triggered exceptions
        [(triggered? v) v]
        [else (if (exception? v) (true) (false))]))]
    
    [(?seq e)
    (let ([v (fri e environment)])
      (cond 
        ; First check for triggered exceptions
        [(triggered? v) v]
        [else 
          (if (empty? v)
            (true)
            (if (..? v)
            (let ([next (..-e2 v)])
              (if (empty? next)
                  (true)
                  (fri (?seq next) environment)))
            (false)))]))]

    ; Addition
    [(add e1 e2)
    (let ([v1 (fri e1 environment)]
          [v2 (fri e2 environment)])
      (cond
        ; First check for triggered exceptions
        [(triggered? v1) v1]
        [(triggered? v2) v2]

        ; Boolean disjunction
        [(and (or (true? v1) (false? v1))
              (or (true? v2) (false? v2)))
          (if (or (true? v1) (true? v2))
              (true)
              (false))]
        
        ; Integer addition
        [(and (int? v1) (int? v2))
          (int (+ (int-value v1) (int-value v2)))]
        
        ; Sequence concatenation
        [(and (true? (fri (?seq v1) environment))
           (true? (fri (?seq v2) environment)))
        (cond
          [(empty? v1) v2]
          [(empty? v2) v1]
          [else (.. (..-e1 v1) 
                  (fri (add (..-e2 v1) v2) environment))])] 
        
        ; Wrong types
        [else (triggered (exception "add: wrong argument type"))]))]
      
    ; Multiplication
    [(mul e1 e2)
    (let* ([v1 (fri e1 environment)]
          [v2 (fri e2 environment)])
      (cond
        ; First check for triggered exceptions
        [(triggered? v1) v1]
        [(triggered? v2) v2]

        ; Boolean conjunction
        [(and (or (true? v1) (false? v1))
              (or (true? v2) (false? v2)))
          (if (and (true? v1) (true? v2))
              (true)
              (false))]
        
        ; Integer multiplication
        [(and (int? v1) (int? v2))
          (int (* (int-value v1) (int-value v2)))]
        
        ; Wrong types
        [else (triggered (exception "mul: wrong argument type"))]))]
      
    [(?leq e1 e2)
    (let ([v1 (fri e1 environment)]
          [v2 (fri e2 environment)])
      (cond
        ; First check for triggered exceptions
        [(triggered? v1) v1]
        [(triggered? v2) v2]

        ; Boolean implication
        [(and (or (true? v1) (false? v1))
              (or (true? v2) (false? v2)))
          (if (fri (add (~ v1) v2) environment) (true) (false))]

        ; Integer leq
        [(and (int? v1) (int? v2)) (if (<= (int-value v1) (int-value v2)) (true) (false))]

        ; Sequences
        [(and (or (..? v1) (empty? v1)) 
              (or (..? v2) (empty? v2)))     
          (if (<= (len v1) (len v2)) (true) (false))]

        [else (triggered (exception "?leq: wrong argument type"))]
      ))]

    [(?= e1 e2)
    (let ([v1 (fri e1 environment)]
          [v2 (fri e2 environment)])
      (cond 
        ; First check for triggered exceptions
        [(triggered? v1) v1]
        [(triggered? v2) v2]

        ; Boolean equality
        [(and (or (true? v1) (false? v1))
            (or (true? v2) (false? v2)))
          (if (equal? v1 v2) (true) (false))]

        ; Integer equality
        [(and (int? v1) (int? v2))
          (if (= (int-value v1) (int-value v2)) (true) (false))]          

        [(and (or (..? v1) (empty? v1)) 
              (or (..? v2) (empty? v2)))     
          (if (equal? v1 v2) (true) (false))]

        [else (false)]
      ))]

    [(head e)
    (let ([v (fri e environment)])
      (cond
        ; First check for triggered exceptions
        [(triggered? v) v]

        [(..? v) (..-e1 v)]

        [(empty? v) (triggered (exception "head: empty sequence"))]

        [else (triggered (exception "head: wrong argument type"))]
      ))]
    
    [(tail e)
    (let ([v (fri e environment)])
      (cond
        ; First check for triggered exceptions
        [(triggered? v) v]

        [(..? v) (..-e2 v)]

        [(empty? v) (triggered (exception "tail: empty sequence"))]

        [else (triggered (exception "tail: wrong argument type"))]
      ))]

    [(~ e)
    (let ([v (fri e environment)])
      (cond
        ; First check for triggered exceptions
        [(triggered? v) v]

        ; Boolean negation
        [(false? v) (true)]
        [(true? v) (false)]

        ; Integer negative value
        [(int? v) (int (- (int-value v)))]

        [else (triggered (exception "~: wrong argument type"))]
      ))]

    [(?all e)
    (let* ([v (fri e environment)]
          [seq (fri (?seq v) environment)])
      (cond
        ; First check for triggered exceptions
        [(triggered? v) v]
        
        [(true? seq) 
          (define (all-helper s)
            (cond
              [(empty? s) (true)]
              [(..? s) 
                (let ([first-val (fri (..-e1 s) environment)])
                  (if (false? first-val)
                      (false)
                      (all-helper (fri (..-e2 s) environment))))]))
          (all-helper v)]

        [(false? seq) (triggered (exception "?all: wrong argument type"))]
      ))]
    
    [(?any e)
    (let* ([v (fri e environment)]
           [seq (fri (?seq v) environment)])
      (cond 
        ; First check for triggered exceptions
        [(triggered? v) v]

        [(true? seq) 
          (define (any-helper s)
            (cond
              [(empty? s) (false)]
              [(..? s) 
                (let ([first-val (fri (..-e1 s) environment)])
                  (if (false? first-val)
                      (any-helper (fri (..-e2 s) environment))
                      (true)))]))
          (any-helper v)]

        [(false? seq) (triggered (exception "?any: wrong argument type"))]
      ))]

    [(vars s e1 e2)
     (cond
       ; Case for single variable binding
       [(string? s)
        (let ([v1 (fri e1 environment)])
          (if (triggered? v1) 
              v1  ; Return triggered exception if e1 evaluates to one
              (fri e2 (cons (cons s v1) environment))))]  ; Otherwise extend environment and evaluate e2
       
       ; Case for list of variables (like Racket's let)
       [(and (list? s) (list? e1))
        (if (has-duplicates? s) 
          (triggered (exception "vars: duplicate identifier"))
          (if (= (length s) (length e1))
            (let ([evaluated-vals (map (λ (expr) (fri expr environment)) e1)])
              ; Check if any values evaluated to triggered exceptions
              (let ([triggered-val (findf triggered? evaluated-vals)])
                (if triggered-val
                    triggered-val  ; Return first triggered exception
                    ; Otherwise extend environment with all bindings
                    (fri e2 (append (map cons s evaluated-vals) environment)))))
            (triggered (exception "vars: lists must have equal length"))))]
       
       [else (triggered (exception "vars: wrong argument type"))])]

    [(valof s)
      (if (string? s)
          (let* ([binding (assoc s environment)])
            (cond
              ; First check for triggered exceptions
              [(triggered? binding) binding]

              [(not (equal? binding #f)) 
                (cond
                  [(valof? (cdr binding)) (fri (cdr binding) environment)]
                  [else (cdr binding)])]

              [else (triggered (exception "valof: undefined variable"))]))
          (triggered (exception "valof: wrong argument type")))]

    [(fun name fargs body)
      (let* ([duplicate_args (has-duplicates? fargs)]
            [valof-vars (collect-valof-vars body)]
            [defined-vars (append 
                          fargs  ; Function arguments
                          (map car environment)  ; Variables in current environment
                          (if (equal? name "") '() (list name)))]  ; Function name for recursion
            [undefined-vars (filter (λ (v) (not (member v defined-vars))) valof-vars)])
        (cond
          ; Check for duplicate args
          [(equal? duplicate_args #t) (triggered (exception "fun: duplicate argument identifier"))]

          ; Check for undefined variables
          [(not (equal? (length undefined-vars) 0))
            (triggered (exception "closure: undefined variable"))]

          ; Check for a lambda function - only return the closure immediately??
          [(equal? name "") (closure environment (fun "" fargs body))]

          ; else return a closure and add the function name to the environment
          [else (closure environment (fun name fargs body))]
        ))]

    [(proc name body)
      (proc name body)]

    [(call e args)
      (let* ([v (fri e environment)]
             [evaluated-args (map (λ (expr) (fri expr environment)) args)])
        (cond
          ; First check for triggered exceptions
          [(triggered? v) v]

          ; Handle function closure
          [(closure? v) 
            (cond
              ; Handle arity mismatch exception
              [(not (= (length (fun-fargs (closure-f v))) (length args))) (triggered (exception "call: arity mismatch"))]

              [else (let* ([env_with_function (list (cons (fun-name (closure-f v)) v))]
                           [env_with_args (zip (fun-fargs (closure-f v)) evaluated-args)]
                           [extended_env (append env_with_args env_with_function)])
                      (fri (fun-body (closure-f v)) (append extended_env (closure-env v))))])]

          ; Handle procedure
          [(proc? v)
            (cond
              ; Handle arity mismatch exception
              [(not (= (length args) 0)) (triggered (exception "call: arity mismatch"))]
              
              [else (let * ([env_with_proc (list (cons (proc-name v) v))])
                      (fri (proc-body v) (append env_with_proc environment)))])]

          [else (triggered (exception "call: wrong argument type"))]
        )  
      )]
))

;;; Macro system

(define (greater e1 e2) (~ (?leq e1 e2)))

(define (rev e)
  (vars "seq" e
      (vars "helper" 
        (fun "rev-helper" '("s" "acc")
          (if-then-else (?empty (valof "s"))
            (valof "acc")
            (call (valof "rev-helper")
              (list (tail (valof "s")) 
                    (.. (head (valof "s")) (valof "acc"))))))
        (call (valof "helper") 
          (list (valof "seq") (empty))))))

(define (binary e)
  (vars "num" e
    (vars "helper" 
      (fun "binary-helper" '("n" "acc")
        (if-then-else (?= (valof "n") (int 0))
          (if-then-else (?empty (valof "acc"))
            (.. (int 0) (empty))  ; Special case for 0
            (valof "acc"))
          (call (valof "binary-helper")
            (list 
              (div (valof "n") (int 2))
              (.. (mod (valof "n") (int 2)) (valof "acc"))))))
      (call (valof "helper") 
        (list (valof "num") (empty))))))

; Helper macros for division and modulo
(define (div e1 e2)
  (vars (list "x" "y") (list e1 e2)
    (vars "helper"
      (fun "div-helper" '("n" "d" "q")
        (if-then-else (?leq (valof "d") (valof "n"))
          (call (valof "div-helper")
            (list 
              (add (valof "n") (~ (valof "d")))
              (valof "d")
              (add (valof "q") (int 1))))
          (valof "q")))
      (call (valof "helper")
        (list (valof "x") (valof "y") (int 0))))))

(define (mod e1 e2)
  (vars (list "x" "y") (list e1 e2)
    (add (valof "x")
           (~ (mul (div (valof "x") (valof "y"))
                      (valof "y"))))))

(define (mapping f seq)
  (vars (list "fn" "sequence") (list f seq)
    (vars "helper"
      (fun "map-helper" '("s" "acc")
        (if-then-else (?empty (valof "s"))
          (valof "acc")
          (call (valof "map-helper")
            (list 
              (tail (valof "s"))
              (.. (call (valof "fn") 
                          (list (head (valof "s"))))
                    (valof "acc"))))))
      (call (valof "helper")
        (list (valof "sequence") (empty))))))

(define (filtering f seq)
  (vars (list "fn" "sequence") (list f seq)
    (vars "helper"
      (fun "filter-helper" '("s" "acc")
        (if-then-else (?empty (valof "s"))
          (valof "acc")
          (vars "test" 
            (call (valof "fn") (list (head (valof "s"))))
            (call (valof "filter-helper")
              (list 
                (tail (valof "s"))
                (if-then-else (valof "test")
                  (.. (head (valof "s")) (valof "acc"))
                  (valof "acc")))))))
      (call (valof "helper")
        (list (valof "sequence") (empty))))))

; Could be broken
(define (folding f init seq)
  (vars (list "fn" "initial" "sequence") (list f init seq)
    (vars "helper"
      (fun "fold-helper" '("s" "acc")
        (if-then-else (?empty (valof "s"))
          (valof "acc")
          (call (valof "fold-helper")
            (list 
              (tail (valof "s"))
              (call (valof "fn") 
                    (list (valof "acc") (head (valof "s"))))))))
      (call (valof "helper")
        (list (valof "sequence") (valof "initial"))))))