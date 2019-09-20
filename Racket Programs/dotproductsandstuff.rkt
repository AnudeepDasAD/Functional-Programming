;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname dotproductsandstuff) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;(1 3 4) . (2 4 6) = (1*2) + (2*4) +(3*5) = 20

(define (dot-product op1 op2)
  (cond
    [(empty? op1)...]
    [else (+ (* (first op1) (first op2)) (doct-product (rest op1) (rest op2)))]))