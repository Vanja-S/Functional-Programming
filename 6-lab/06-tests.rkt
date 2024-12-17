#lang racket

(require rackunit rackunit/text-ui "06.rkt")

(define tests
  (test-suite
   "Function Tests"
   
   ; Power tests
   (test-case "Power function tests"
    (check-equal? (power 3 0) 1)
    (check-equal? (power 1 100) 1)
    (check-equal? (power 0 0) 1)
    (check-equal? (power 5 2) 25))
   
   ; GCD tests
   (test-case "GCD function tests"
    (check-equal? (gcd 60 48) 12)
    (check-equal? (gcd 56 98) 14)
    (check-equal? (gcd 48 18) 6)
    (check-equal? (gcd 101 10) 1)
    (check-equal? (gcd 100 100) 100)
    (check-equal? (gcd 0 5) 5)
    (check-equal? (gcd 5 0) 5)
    (check-equal? (gcd 0 0) 0))
   
   ; Fibonacci tests
   (test-case "Fibonacci function tests"
    (check-equal? (fib 3) 2))
   
   ; Reverse tests
   (test-case "Reverse function tests"
    (check-equal? (reverse (list 1 2 3)) '(3 2 1)))
   
   ; Remove tests
   (test-case "Remove function tests"
    (check-equal? (remove 3 (list 1 2 3 4 5 4 3 2 1)) '(1 2 4 5 4 2 1)))
   
   ; Map tests
   (test-case "Map function tests"
    (check-equal? (map (lambda (a) (* a 2)) (list 1 2 3)) '(2 4 6)))
   
   ; Palindrome tests
   (test-case "Palindrome function tests"
    (check-equal? (is-palindrome (list 2 3 5 1 6 1 5 3 2)) #t))))

; Run the tests
(run-tests tests)