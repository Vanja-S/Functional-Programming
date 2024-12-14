#lang racket

(define ones
  (cons 1 (thunk ones)))

(define naturals
  (let loop ((n 1))
    (cons n (thunk (loop (+ n 1))))))

(define fibs 
  (let loop ((a 1)
             (b 1))
    (cons a (thunk (loop b (+ a b))))))

(define (first n f_stream) 
  (if (= n 0) 
    '()
    (append (list (car f_stream)) (first (- n 1) ((cdr f_stream))))))

(define (square x)
  (* x x))

(define (squares f_stream) 
  (cons (square (car f_stream)) (thunk (squares ((cdr f_stream))))))

(define-syntax sml 
  (syntax-rules (nil null hd tl ::)
    [(sml nil)
     '()]
    [(sml null ls)
     (null? ls)]
    [(sml hd ls)
      (car ls)]
    [(sml tl ls)
      (cdr ls)]
    [(sml l :: ls)
      (append (list l) ls)]))


(define (my-delay thunk)
  (mcons 0 (mcons #f thunk)))

(define (my-force prom)
  (let ([call-count (mcar prom)]
          [result-cell (mcdr prom)])
    (if (and (not (= call-count 0)) (not (= call-count 5))) 
        (begin
          (set-mcar! prom (+ call-count 1))
          (mcar result-cell))
        (begin 
          (set-mcar! result-cell ((mcdr result-cell)))
          (set-mcar! prom 1)
          (mcar result-cell)))))

(define f 
  (my-delay 
    (lambda () (begin (write "bla") 123))))


(define (partitions k n)
    (cond 
      [(> k n) 0]
      [(= k n) 1]
      [(= k 1) 1]
      [else (+ (partitions (- k 1) (- n 1)) (partitions k (- n k)))]))



(provide (all-defined-out))