#lang racket

;;; Datatypes ;;;

;;; Integers
(struct int (value) #:transparent)

;;; Booleans
(struct true () #:transparent)
(struct false () #:transparent)

;;; Execution handlers ;;;

;;; Logic-Arithmetic
(struct add (e1 e2) #:transparent)

(struct mul (e1 e2) #:transparent)

(struct ?leq (e1 e2) #:transparent)

(struct ~ (e1) #:transparent)

(struct ?int (e1) #:transparent)

(struct if-then-else (condition e1 e2) #:transparent)

;;; Main ;;;

(define (fri e)
    (match e
      [(or (true) (false) (int _)) e]
      [(add e1 e2)
        (match* ((fri e1) (fri e2))
          [((int i1) (int i2)) (int (+ i1 i2))]
          [((true) _) (true)]
          [((false) e) e])]
      [(mul e1 e2)
        (match* ((fri e1) (fri e2))
          [((int i1) (int i2)) (int (* i1 i2))]
          [((true) e) e]
          [((false) _) (false)])]
      [(?leq e1 e2)
        (match* ((fri e1) (fri e2))
          [((int i1) (int i2)) (if (<= i1 i2) (true) (false))]
          [((true) (true)) (true)]
          [((true) (false)) (false)]
          [((false) _) (true)])]
      [(~ e1)
        (match (fri e1)
          [(int i1) (int (- i1))]
          [(true) (false)]
          [(false) (true)])]
      [(if-then-else condition e1 e2)
        (if (true? (fri condition)) (fri e1) (fri e2))]
      [(?int e1)
        (cond 
          [(int? (fri e1)) (true)]
          [else (false)])]
    )
)

(define (?geq e1 e2) (fri (?leq (fri e2) (fri e1))))

(define (conditional . args)
  (cond
    [(null? (cdr args)) (car args)]
    [else
     (if-then-else 
      (car args)
      (cadr args)
      (apply conditional (cddr args)))]))

(provide (all-defined-out))