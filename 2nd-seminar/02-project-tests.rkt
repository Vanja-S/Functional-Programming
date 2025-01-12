#lang racket

(require rackunit rackunit/text-ui "02-project.rkt")

(define tests
    (test-suite 
        "Tests"

        (test-case "datatypes"
            (check-equal? (fri (int 5) null) (int 5))
            (check-equal? (fri (int 0) null) (int 0))

            (check-equal? (fri (false) null) (false))
            (check-equal? (fri (true) null) (true))

            (check-equal? (fri (false) null) (false))
            (check-equal? (fri (true) null) (true))

            (check-equal? (fri (empty) null) (empty))
            (check-equal? (fri (.. (int 1) (empty)) null) (.. (int 1) (empty)))
            (check-equal? (fri (.. (int 1) (.. (int 2) (empty))) null) (.. (int 1) (.. (int 2) (empty))))
        )

        (test-case "add"
            (check-equal?
                (add (mul (true) (true)) (false))
                (add (mul (true) (true)) (false))
            )
            (check-equal? (fri (add (int 3) (int 2)) null) (int 5))
            (check-equal? (fri (add (false) (true)) null) (true))
            (check-equal? (fri (add (false) (int 2)) null) (triggered (exception "add: wrong argument type")))
            (check-equal? (fri (add (empty) (true)) null) (triggered (exception "add: wrong argument type")))
            (check-equal? (fri (add (empty) (empty)) null) (empty))
            (check-equal? (fri (add (empty) (.. (int 1) (empty))) null) (.. (int 1) (empty)))
            (check-equal? (fri (add (.. (int 2) (empty)) (.. (int 1) (empty))) null) (.. (int 2) (.. (int 1) (empty))))
            (check-equal? (fri (add (.. (int 2) (empty)) (.. (int 1) (.. (int 0) (empty)))) null) (.. (int 2) (.. (int 1) (.. (int 0) (empty)))))
        )

        (test-case "handle"
            (check-equal?
                (fri (handle (exception "add: wrong argument type")
                    (add (add (int 9) (int 9)) (true))
                        (false)) null)
                (false)
            )
            (check-equal?
                (fri (handle (exception "fatal error")
                    (add (add (int 9) (int 9)) (true))
                        (false)) null)
                (triggered (exception "add: wrong argument type"))
            )
            (check-equal?
                (fri (handle (exception "fatal error")
                    (add (add (int 9) (int 9)) (int -1))
                    (false)) null)
                (int 17)
            )
            (check-equal? 
                (fri (handle (int 1337)
                    (add (add (int 9) (int 9)) (int -1))
                    (false)) null)
                (triggered (exception "handle: wrong argument type"))
            )
            (check-equal? 
                (fri (handle (trigger (exception "fatal error"))
                    (add (add (int 9) (int 9)) (int -1))
                    (false)) null)
                (triggered (exception "fatal error"))
            )
            (check-equal?
                (fri (handle (head (.. (exception "error") (int 10)))
                    (exception "error") (int 2)) null)
                (exception "error")
            )
            (check-equal?
                (fri (handle
                    (exception "error")
                    (trigger (exception "error"))
                    (int 2)) null)
                (int 2)
            )
            (check-equal?
                (fri (handle
                    (exception "error2")
                    (handle (exception "error1")
                            (trigger (exception "error2"))
                            (int 2))
                    (int 1)) null)
                (int 1)
            )
            (check-equal?
                (fri (handle (trigger (exception "error")) (int 1) (int 2)) null) 
                (triggered (exception "error"))
            )
            (check-equal?
                (fri (handle (exception "error") (int 1) (int 2)) null)
                (int 1)
            )
            (check-equal?
                (fri (handle (exception "error") (exception "error2") (int 2)) null)
                (exception "error2")
            )
            (check-equal?
                (fri (handle (exception "error") (exception "error") (int 2)) null)
                (exception "error")
            )
            (check-equal?
                (fri (handle (exception "error") (trigger (exception "error")) (int 2)) null)
                (int 2)
            )
            (check-equal? 
                (fri (handle (exception "error2")
                        (trigger (exception "error")) (int 2)) null)
                (triggered (exception "error"))
            )
            (check-equal?
                (trigger (exception "test"))
                (trigger (exception "test"))
            )
            (check-equal?
                (fri (trigger (exception "test")) null)
                (triggered (exception "test"))
            )
            (check-equal? 
                (fri (add (int 1) (trigger (exception "fatal error"))) null)
                (triggered (exception "fatal error"))
            )
            (check-equal? 
                (fri (trigger (exception "fatal error")) null)
                (triggered (exception "fatal error"))
            )
            (check-equal?
                (fri (trigger (trigger (exception "tra-la-la hop-sa-sa"))) null)
                (triggered (exception "tra-la-la hop-sa-sa"))
            )
        )

        (test-case "?seq"
            (check-equal?
                (?seq (.. (int 1) (.. (int 2) (empty))))
                (?seq (.. (int 1) (.. (int 2) (empty)))))
            (check-equal? (fri (?seq (empty)) null) (true))
            (check-equal? (fri (?seq (.. (int 1) (empty))) null) (true))
            (check-equal? (fri (?seq (.. (int 1) (int 2))) null) (false))
            (check-equal? (fri (?seq (.. (int 1) (.. (int 2) (empty)))) null) (true))
            (check-equal?
                (fri (.. (?seq (.. (int 1) (.. (int 2) (empty))))
                        (?seq (.. (int 1) (.. (int 2) (int 3))))) null)
                (.. (true) (false))
            )
            (check-equal?
               (?seq (empty)) (?seq (empty))
            )
            (check-equal?
                (fri (?seq (empty)) null)
                (true)
            )
        )

        (test-case "add seq"
            (check-equal?
                (fri (add (.. (false) (empty))
                        (.. (int 3) (empty))) null)
                (.. (false) (.. (int 3) (empty))))
        )

        (test-case "?.."
            (check-equal? (fri (?.. (empty)) null)
                        (false)
            )
        )
    
        (test-case "add empty" (check-equal?
            (fri (add (empty) (empty)) null)
                (empty)
            )
        )
    
        (test-case "join sequences"
            (check-equal?
                (fri (add
                    (add
                        (.. (int 1) (.. (int 2) (empty)))
                        (.. (int -1) (.. (int -2) (empty))))
                    (add
                        (.. (int 11) (.. (int 21) (empty)))
                        (.. (int -11) (.. (int -21) (empty)))))
                    null)
                (.. (int 1) (.. (int 2)
                (.. (int -1) (.. (int -2)
                    (..
                    (int 11)
                    (..
                    (int 21)
                    (.. (int -11) (.. (int -21) (empty)))))))))
            )
        )

        (test-case "join sequences exception"
            (check-equal?
                (fri (add
                    (.. (int 1) (int 2))
                    (.. (int 3) (empty)))
                    null)
                (triggered (exception "add: wrong argument type"))
            )
        )

        (test-case "?leq"
            (check-equal?
                (fri (?leq (.. (int 1) (int 3)) (int 1)) null)
                (triggered (exception "?leq: wrong argument type"))
            )
            (check-equal?
                (fri (?leq (false) (true)) null)
                (true)
            )
            (check-equal?
                (fri (?leq (false) (true)) null)
                (true)
            )
            (check-equal?
                (fri (?leq (empty) (empty)) null)
                (true)
            )
            (check-equal?
                (fri (?leq (.. (int 10) (empty)) (empty)) null)
                (false)
            )
        )

        (test-case "?="
            (check-equal? 
                (fri (?= (false) (false)) null) 
                (true)
            )
            (check-equal? 
                (fri (?= (true) (true)) null) 
                (true)
            )
            (check-equal? 
                (fri (?= (false) (true)) null) 
                (false)
            )
            (check-equal?
                (fri (?= (.. (int 1) (empty)) (empty)) null)
                (false)
            )
            (check-equal?
                (fri (?= (empty) (empty)) null)
                (true)
            )
            (check-equal?
                (fri (?= (.. (int 1) (empty)) (.. (int 1) (empty))) null)
                (true)
            )
        )    

        (test-case "head"
            (check-equal?
                (fri (head (.. (add (.. (empty) (.. (empty) (empty))) (empty)) (empty))) null)
                (.. (empty) (.. (empty) (empty)))
            )
            (check-equal?
                (fri (head (head (.. (int 1) (false)))) null)
                (triggered (exception "head: wrong argument type"))
            )
        )

        (test-case "tail"
            (check-equal?
                (fri (tail (.. (int 4) (int 9))) null)
                (int 9)
            )
            (check-equal?
                (fri (tail (.. (int 4) (empty))) null)
                (empty)
            )
            (check-equal? 
                (fri (tail (empty)) null)
                (triggered (exception "tail: empty sequence"))
            )
            (check-equal? 
                (fri (tail (int 1)) null)
                (triggered (exception "tail: wrong argument type"))
            )
        )

        (test-case "~"
            (check-equal? 
                (fri (~ (true)) null)
                (false)
            )
            (check-equal? 
                (fri (~ (false)) null)
                (true)
            )
            (check-equal?
                (fri (~ (int 3)) null)
                (int -3)
            )
            (check-equal?
                (fri (~ (int -3)) null)
                (int 3)
            )
        )
        (test-case "?all"
            (check-equal?
                (fri (?all (empty)) null)
                (true)
            )
            (check-equal?
                (fri (?all (.. (true) (false))) null)
                (triggered (exception "?all: wrong argument type"))
            )
            (check-equal?
                (fri (?all (.. (false) (.. (false) (int 1)))) null)
                (triggered (exception "?all: wrong argument type"))
            )
            (check-equal?
                (fri (?all
                    (.. (true)
                        (.. (?leq (false) (true))
                            (..
                                (?= (.. (int -19) (int 0))
                                    (.. (head
                                        (tail
                                        (tail (add (.. (int 1) (empty))
                                                    (.. (int 5) (.. (int -19) (empty)))))))
                                        (int 0)))
                                (empty)))))
                    null)
                (true)
            )
        )

        (test-case "?any"
            (check-equal?
                (fri (?any (empty)) null)
                (false)
            )
            (check-equal?
                (fri (?any (.. (false) (.. (false) (int 1)))) null)
                (triggered (exception "?any: wrong argument type"))    
            )
        )

        (test-case "vars + valof"
            (check-equal?
                (fri (vars "a" (add (mul (int 1) (int 2)) (mul (int -3) (int 4)))
                    (mul (valof "a") (valof "a"))) null)
                (int 100)
            )
            (check-equal?
                (fri (vars (list "a" "b")
                            (list (mul (mul (int 1) (int 2)) (mul (int -3) (int 4)))
                                (~ (add (mul (int 1) (int 2)) (mul (int -3) (int 4)))))
                            (add (valof "a") (valof "b"))) null)
                (int -14)
            )
            (check-equal?
                (fri (vars (list "s1" "s2" "s3")
                            (list (.. (false) (true))
                                (.. (int 1) (int 2))
                                (.. (int 4) (int 4)))
                            (mul (valof "s1") (mul (valof "s2") (valof "s3")))) null)
                (triggered (exception "mul: wrong argument type"))
            )
            (check-equal?
                (fri (vars "a" (trigger (exception "t"))
                            (trigger (exception "f"))) null)
                (triggered (exception "t"))
            )
            (check-equal?
                (fri (vars (list "a" "a" "a")
                            (list (int 4) (int 5) (int 88))
                            (valof "a")) null)
                (triggered (exception "vars: duplicate identifier"))
            )
            (check-equal?
                (fri (vars (list "a" "b")
                            (list (int 2) (mul (valof "a") (int 3)))
                            (.. (valof "a") (valof "b"))) null)
                (triggered (exception "valof: undefined variable"))
            )
        )

        (test-case "fun"
            (check-equal?
                (fri (fun "test" (list "a" "b" "c" "b") (int 1)) null)
                (triggered (exception "fun: duplicate argument identifier"))
            )
            
        )

        (test-case "vars + fun"
            (check-equal?
                (fri (vars (list "a" "b" "c")
                            (list (int 1) (int 2) (int 3))
                            (fun "linear" (list "x1" "x2" "x3")
                                (add (mul (valof "a") (valof "x1"))
                                    (add (mul (valof "b") (valof "x2"))
                                        (mul (valof "c") (valof "x3")))))) null)
                (closure (list (cons "a" (int 1))(cons "b" (int 2)) (cons "c" (int 3)))
                        (fun "linear" '("x1" "x2" "x3")
                            (add (mul (valof "a") (valof "x1"))
                                    (add (mul (valof "b") (valof "x2"))
                                        (mul (valof "c") (valof "x3"))))))
            )
        )

        (test-case "vars + proc"
            (check-equal?
                (fri (vars (list "a" "b" "c")
                        (list (int 5) (int 2)
                                (proc "" (valof "a")))
                        (call (valof "c") (list))) null)
                (int 5)
            )
            (check-equal?
                (fri (vars "a" (proc "" (int 1))
                            (call (valof "a") (list (false)))) null)
                (triggered (exception "call: arity mismatch"))
            )
        )
        
        (test-case "closure"
            (check-equal?
                (fri (call (fun "test" (list "t") (valof "test")) (list (int 1))) null)
                (closure '() (fun "test" '("t") (valof "test")))
            )
            (check-equal?
                (fri (vars (list "a" "b" "c")
                            (list (int 5) (int 2)
                                (fun "" (list) (valof "a")))
                            (call (valof "c") (list))) null)
                (triggered (exception "closure: undefined variable"))
            )
            (check-equal?
                (fri (vars (list "a" "b" "c")
                            (list (int 1) (int 2) (int 3))
                            (fun "linear" (list "x1" "x2" "x3")
                                (add (mul (valof "a") (valof "manjka"))
                                    (add (mul (valof "b") (valof "x2"))
                                        (mul (valof "c") (valof "x3")))))) null)
                (triggered (exception "closure: undefined variable"))
            )
            ;;; (check-equal?
            ;;;     (fri
            ;;;         (vars "?" (int 1001)
            ;;;             (vars "f"
            ;;;                 (fun "" (list "x")
            ;;;                     (mul (valof "x") (valof "?")))
            ;;;                     (vars "?" (int -5)
            ;;;                         (call (valof "f") (list (valof "?"))))))
            ;;;     null)
            ;;;     (int -5005)
            ;;; )
        )

        (test-case "call"
            (check-equal?
                (fri (vars "a" (proc "" (int 1))
                            (call (valof "a") (list (false)))) null)
                (triggered (exception "call: arity mismatch"))
            )
            (check-equal?
                (fri (call (fun "test" (list "test") (valof "test")) (list (int 1))) null)
                (int 1)
            )
            (check-equal?
                (fri (vars (list "a" "b" "c")
                            (list (int 5) (int 2)
                                (fun "" (list) (exception "a")))
                            (call (valof "c") (list))) null)
                (exception "a")
            )
            (check-equal?
                (fri (vars (list "a" "b" "c")
                            (list (int 5) (int 2)
                                (fun "" (list) (exception "a")))
                            (call (valof "d") (list))) null)
                (triggered (exception "valof: undefined variable"))
            )
            ;;; (check-equal?
            ;;;     (fri (call
            ;;;         (fun "fib" (list "n")
            ;;;                 (if-then-else
            ;;;                 (?leq (valof "n") (int 2))
            ;;;                 (int 1)
            ;;;                 (add (call (valof "fib")
            ;;;                             (list (add (valof "n") (int -1))))
            ;;;                     (call (valof "fib")
            ;;;                             (list (add (valof "n") (int -2)))))))
            ;;;         (list (int 10))) null)
            ;;;     (int 55)
            ;;; )
        )


))

; Run the tests
(run-tests tests)