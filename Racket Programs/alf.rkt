;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname alf) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;;************************************
;;   Anudeep Das (20782887)
;;   CS 135 Fall 2018
;;   Assignment 08, Problem 2                                    
;;*************************************

(require "provide.rkt")
(provide subsequence)

;;PART A

;;(occurrences num-list num-to-find)  consumes a list of numbers and a number, in that order,
;;   and produces the number of times that the given number occurs in the list of numbers.

;;occurrences: (listof Num) Num -> Nat

(check-expect (occurrences '(1 1 23 2 3 3 1) -3) 0)
(check-expect (occurrences '(5 5 5 5 5 5 5) 0) 0)


(define (occurrences num-list num-to-find)
  (length(filter (lambda (number-in-list) (= number-in-list num-to-find)) num-list)))

(check-expect (occurrences '(5 5 5 5 5 5 5) 5) 7)
(check-expect (occurrences '(1 2 2 22 3 5 -5) 2) 2)
(check-expect (occurrences '(1 2 2 22 3 5 -5) 1) 1)
(check-expect (occurrences '() 1)0)
(check-expect (occurrences '(1 2 2 22 3 -5/30 -5/3) -5/3) 1)











;;PART B

(define base-case 0)

;; (absolutely-odd listof-int) consumes a list of integers and produces the sum of the absolute
;;      values of the odd integers in the list.

;;absolutely-add: listof Int -> Nat

(check-expect(absolutely-odd '(-1 1 -2 -3 3)) 10)
(check-expect(absolutely-odd '(0 0 0 0 0)) 0)


(define (absolutely-odd listof-int)
  (foldr + base-case (map abs listof-int)))


(check-expect(absolutely-odd '(-1 -1 -2 -3 -3)) 10)
(check-expect(absolutely-odd '(4 5 6 7 8)) 30)



;;PART C

;; (zip list1 list2) consumes two lists of equal length, and produces a list of pairs
;;    (two element lists) where the ith pair contains the ith element of the first list
;;    followed by the ith element of the second list.


;;zip: (listof X) (listof Y) -> (listof X and Y)
;;requires: list1 and list2 are of the same length


(check-expect (zip '(1 2 3 4) '(-1 -2 -3 -4)) '((1 -1) (2 -2) (3 -3) (4 -4)))
(check-expect (zip '(1 symbol "string" (list empty)) '(3.14159 -9 0 (list(list "string"))))
              '((1 3.14159) (symbol -9) ("string" 0) ((list empty) (list(list "string")))))


(define (zip list1 list2)
  (map (lambda (element1 element2) (list element1 element2)) list1 list2))

(check-expect (zip '() '()) empty)
(check-expect (zip '(3) '(1)) '((3 1)))






;;PART D

;;(unzip listof-pairs) consumes a list of pairs, and produces a list of two lists.
;;   The first list contains the first element from each pair, and the second list contains the
;;   second element from each pair, in the original order.
;;   Unzipping an empty list produces ’(() ()).


;;unzip: (listof (listof (anyof X Y)) -> (listof (listof X) (listof Y))

(check-expect(unzip (list (list 1 -1) (list 2 -2) (list 3 -3) (list 4 -4)))
             (list (list 1 2 3 4) (list -1 -2 -3 -4)))

(check-expect (unzip empty)(list empty empty))

             
(define (unzip listof-pairs)
 (list (map (lambda (x) (first x)) listof-pairs)
       (map (lambda (x) (second x)) listof-pairs)))


(check-expect (unzip '((1 3.14159) (symbol -9) ("string" 0) ((list empty) (list(list "string")))))
               '((1 symbol "string" (list empty)) (3.14159 -9 0 (list(list "string")))))

(check-expect (unzip '((1 3.14159) (symbol -9) ("string" 0) (empty (list(list "string")))))
               '((1 symbol "string" empty) (3.14159 -9 0 (list(list "string")))))

(check-expect (unzip '( (1 empty) ("string" no-string) (empty empty)))
              '((1 "string" empty) (empty no-string empty)))






;;PART E

;;(dedup listof-num) consumes a list of numbers and produces a new list with only
;;   the first occurrence of each element of the original list.

;;de-dup: (listof Num) -> (listof Num)

(check-expect(dedup '(1 2 3 3 3 22 2 1 1 0))(list 3 22 2 1 0))
(check-expect(dedup '(1 1 1 1 1))(list 1))

(define (dedup listof-num)
  (foldr (lambda (element1 acc) (cond
                                  ;empty means no duplicates
                                  [(empty? (filter (lambda (checking) (= checking element1)) acc))
                                   (cons element1 acc)]
                                  [else acc]))
         ;;empty base because no acc yet
         empty listof-num))
  
(check-expect (dedup '(0)) '(0))
(check-expect (dedup '(0 0.0001 -35 6 22/5 -35 22/5 0 0 22/5 6.0)) '(0.0001 -35 0 22/5 6))  




   



;;PART E

;; (subsequence mylist from to) consumes a list and two natural numbers. It produces
;;   the subsequence from lst that begins at index from and ends just before index to.
;;   Indexing starts at 0.


;;subsequence: (listof X) Nat Nat -> (listof X)
;;requires: to <= (length mylist)    KEEP OR NO?

(check-expect(subsequence '(1 2 3 4 5) 1 3)(list 2 3))
(check-expect(subsequence '(1 2 3 4 5) 0 2)(list 1 2))
(check-expect(subsequence '(1 2 3 4 5) 0 5)(list 1 2 3 4 5))
(check-expect(subsequence '(1 2 3 4 5) 4 5)(list 5))

(define (subsequence mylist from to)
  (map (lambda (element) (second element))
       (filter (lambda (index) (and (>= (first index) from) (< (first index) to)))
               (zip (build-list (length mylist) (lambda (element) (identity element))) mylist)))) 
  

(check-expect(subsequence '("string" lolz 22/6 -99) 0 1)(list "string"))
(check-expect(subsequence '("string" lolz 22/6 -99) 0 6)'("string" lolz 22/6 -99))
(check-expect(subsequence '("string" lolz 22/6 -99 500 66 klp #\c) 2 7)'(22/6 -99 500 66 klp))
(check-expect(subsequence '(empty) 1 3)
             empty)



