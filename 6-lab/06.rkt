#lang racket

(define (power x n) 
    (if (equal? n 0) 
        1
        (* x (power x (- n 1)))
))

(define (gcd a b) 
    (if (equal? b 0) 
        a
        (gcd b (modulo a b))
    )
)

(define (fib n)
    (cond
    [(= n 0) 0]
    [(= n 1) 1]
    [else (+ (fib (- n 1)) (fib (- n 2)))]
))

(define (reverse sez)
  (if (null? sez)
      '()
      (append (reverse (cdr sez)) (list (car sez)))
))

(define (remove x sez)
  (cond
    [(null? sez) '()]
    [(equal? (car sez) x) (remove x (cdr sez))]
    [else (cons (car sez) (remove x (cdr sez)))]
))

(define (map f fn)
  (if (null? fn)
      '()
      (cons (f (car fn)) (map f (cdr fn)))
))

(define (filter fn ls) 
    (cond 
        [(null? ls) '()]
        [(fn (car ls)) (append (list (car ls)) (filter fn (cdr ls)))]
        [else (filter fn (cdr ls))]
))

(define (zip a b)
  (if (or (null? a) (null? b))
      '()
      (cons (cons (car a) (car b)) (zip (cdr a) (cdr b)))
))

(define (range start end step)
  (if (> start end)
      '()
      (cons start (range (+ start step) end step))
))

(define (is-palindrome fn)
  (equal? fn (reverse fn)))

(provide (all-defined-out))