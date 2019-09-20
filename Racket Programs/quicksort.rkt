;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname quicksort) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;pick a pivot and sort around it, the larger parts

;taking overall list and chopping into two parts and doing the smae for the rest of the parts

;with every pivot, the number of pivot is reduced by 1

(define (sort/quick lst)
  (cond
    [(empty? lst) empty]
    [else
    (local
       [(define pivotValue (first lst))
        (define smaller (filter (lambda (n) (< n pivotValue)) (rest lst)))
        (define larger (filter (lambda (n) (> n pivotValue)) (rest lst)))]
       (append (sort/quick smaller) (list pivotValue) (sort/quick larger)))]))

;quicksort is built-in

(define f (lambda (x) (+ x x)))

(f 4)