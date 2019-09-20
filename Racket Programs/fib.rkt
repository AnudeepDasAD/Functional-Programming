;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname fib) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;;************************************
;;   Anudeep Das (20782887)
;;   CS 135 Fall 2018
;;   Assignment 09, Problem 2                                    
;;*************************************

(define base-case-zero 0)
(define base-case-one 1)
(define initial-skip 2)

;;(fast-fib num) quickly calculates the num-th term in the fibonacci sequence

;;fast-fib: Nat -> Nat


(check-expect (fast-fib 2) 1)
(check-expect (fast-fib 3) 2)
(check-expect (fast-fib 6) 8)
(check-expect (fast-fib 1) 1)
(check-expect (fast-fib 5) 5)


(define (fast-fib num)
  (cond [(= num base-case-zero) base-case-zero]
        [(= num base-case-one) base-case-one]
        [else
         (local [(define (previous-term-addition back-2 back-1 num)
                   (cond ;[(and (= back-2 (- num 2)) (= back-1 (sub1 num))) (+ back-2 back-1)]
                     [(= num 0) (+ back-2 back-1)]
                     [else
                      (previous-term-addition back-1 (+ back-2 back-1) (sub1 num))]))]
           (previous-term-addition base-case-zero base-case-one (- num initial-skip)))]))


(check-expect (fast-fib 0) 0)
(check-expect (fast-fib 4) 3)
(check-expect (fast-fib 6) 8)
(check-expect (fast-fib 7) 13)
(check-expect (fast-fib 42) 267914296)
(check-expect (fast-fib 101) 573147844013817084101)

;;cannot effectively test for massive numbers
;; (will find an answer but unsure if it is the correct answer)

;;the following are hypothetical tests that would work, but would be too slow

;;(check-expect (fast-fib 1000) (fib 1000))
;;(check-expect (fast-fib 5000) (fib 5000))
;;(check-expect (fast-fib 50000000) (fib 50000000))





;(fast-fib 100000)                  
                  


