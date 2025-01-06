#lang racket

(require rackunit rackunit/text-ui "08.rkt")

(define tests
    (test-suite 
        "Function tests"

        (test-case "datatypes"
            (check-equal? (fri (int 5)) (int 5))
            (check-equal? (fri (int 0)) (int 0))

            (check-equal? (fri (false)) (false))
            (check-equal? (fri (true)) (true)))

        (test-case "arithmetic-logic operations"
            (check-equal? (fri (add (int 3) (int 2))) (int 5))
            (check-equal? (fri (add (false) (true))) (true))

            (check-equal? (fri (mul (int 3) (int 2))) (int 6))
            (check-equal? (fri (mul (false) (true))) (false))
    
            (check-equal? (fri (?leq (int 3) (int 2))) (false))
            (check-equal? (fri (?leq (false) (true))) (true))
        
            (check-equal? (fri (~ (int 3))) (int -3))
            (check-equal? (fri (~ (false))) (true))

            (check-equal? (fri (?int (int 5))) (true))
            (check-equal? (fri (?int (false))) (false))
            (check-equal? (fri (?int (?leq (false) (true)))) (false))

            (check-equal? (fri (if-then-else (true) (int 5) (add (int 2) (int "a")))) (int 5))
            
            (check-equal? (?geq (add (int 1) (int 1)) (int 4)) (false))

            (check-equal? (conditional (true) (int -100) (mul (true) (false)) (add (int 1) (int 1)) (int 9000)) (if-then-else (true) (int -100) (if-then-else (mul (true) (false)) (add (int 1) (int 1)) (int 9000))))
            (check-equal? (conditional (true) (int -100) (mul (true) (false))) (if-then-else (true) (int -100) (mul (true) (false))))
            (check-equal? (conditional 1 2 3 4 5) (if-then-else 1 2 (if-then-else 3 4 5)))
        )
))

; Run the tests
(run-tests tests)