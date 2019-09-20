;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname partition) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;;************************************
;;   Anudeep Das (20782887)
;;   CS 135 Fall 2018
;;   Assignment 08, Problem 3                                    
;;*************************************


;; (partition pred? mylist) consumes a predicate and a list. It produces a two element list,
;;    (list X Y), where X is a list of those items in the consumed list that satisfy
;;     the predicate and Y is a list of those items that don’t satisfy the predicate.
;;     The order of items in each list must be the same as the original list.

;;partition: Bool (listof (anyof X Y)) -> (listof (listof X) (listof Y))
;;requires: (pred? X) must be true and (pred? Y) must be false


(check-expect (partition even? '(1 2 3 4 5)) '((2 4) (1 3 5)))
(check-expect (partition empty? empty) '(() ()))



(define (partition pred? mylist)
  (list (filter (lambda (element) (pred? element)) mylist)
        (filter (lambda (element) (not(pred? element))) mylist)))




(check-expect (partition (lambda (value) (>= (first value) (second value)))
                         '((1 2) (2 1) (33 75) (44 44) (98 -6) (0 -1)))
              '(((2 1) (44 44) (98 -6) (0 -1)) ((1 2) (33 75))))

(check-expect (partition cons? '(11 (cons 1 empty) empty "some string" '(still-some-string yea)))
              '(((cons 1 empty) '(still-some-string yea)) (11 empty "some string")))

(check-expect (partition empty? '(1 2 3 not-empty 4 5 6)) '(() (1 2 3 not-empty 4 5 6)))

(check-expect (partition number? '(1 -2 33/21 4)) '(((1 -2 33/21 4) ())))


