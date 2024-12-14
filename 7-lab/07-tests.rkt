#lang racket

(require rackunit rackunit/text-ui "07.rkt")

(define tests
    (test-suite 
        "Function tests"

        (test-case "ones"
            (check-equal? (car ones) 1)
            (check-equal? (car ((cdr ((cdr ones))))) 1))

        (test-case "naturals"
            (check-equal? (car naturals) 1)
            (check-equal? (car ((cdr ((cdr naturals))))) 3))

        (test-case "fibs"
            (check-equal? (car fibs) 1)
            (check-equal? (car ((cdr fibs))) 1)
            (check-equal? (car ((cdr ((cdr fibs))))) 2))

        (test-case "first"
            (check-equal? (first 1 fibs) '(1))
            (check-equal? (first 2 fibs) '(1 1))
            (check-equal? (first 5 fibs) '(1 1 2 3 5)))

        (test-case "squares"
            (check-equal? (car (squares fibs)) 1)
            (check-equal? (car ((cdr ((cdr (squares fibs)))))) 4)
            (check-equal? (first 5 (squares fibs)) '(1 1 4 9 25)))

        (test-case "sml"
            (check-equal? (sml nil) '())
            (check-equal? (sml null '()) #t)
            (check-equal? (sml null '(1)) #f)
            (check-equal? (sml hd (list 1 2 3)) 1)
            (check-equal? (sml tl (list 1 2 3)) '(2 3))
            (check-equal? (sml 5 :: null) '(5))
            (check-equal? (sml null (sml nil)) #t)
            (check-equal? (sml tl (sml 5 :: (sml 4 :: (sml nil)))) '(4)))

        (test-case "partitions"
            (check-equal? (partitions 3 7) 4))
))

; Run the tests
(run-tests tests)