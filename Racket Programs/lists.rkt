;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname lists) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;;************************************
;;   Anudeep Das (20782887)
;;   CS 135 Fall 2018
;;   Assignment 04, Problem 3                                    
;;*************************************


;;PART A

;;(absolutely-odd list-of-integers)consumes a list of integers and produces the
;;    sum of the absolute values of the odd integers in the list, or 0 if there
;;    are none.

;;absolutely-odd: (listof Int) -> Nat
;; requires: (list-of-integers) must be a list of integers 

(check-expect (absolutely-odd (cons -1 (cons 2 (cons 3 empty)))) 4)
(check-expect (absolutely-odd (cons 10 (cons 9 (cons 8 empty)))) 9)

(define (absolutely-odd list-of-integers)
  (cond [(empty? list-of-integers) 0]
         ; if the first element is even, the rest of the list is analysed
        [(even? (first list-of-integers))
         (absolutely-odd (rest list-of-integers))]

        ;else it is odd and it is added
        [else (+ (abs (first list-of-integers))
                 (absolutely-odd (rest list-of-integers)))]
        )
  )

(check-expect (absolutely-odd (cons 2 (cons 4 (cons 6 empty)))) 0)
(check-expect (absolutely-odd (cons 0 empty)) 0)
(check-expect (absolutely-odd (cons -7 empty)) 7)
(check-expect (absolutely-odd empty) 0)
(check-expect (absolutely-odd (cons 5 empty)) 5)
(check-expect (absolutely-odd (cons -5 empty)) 5)
(check-expect (absolutely-odd (cons 8 empty)) 0)
(check-expect (absolutely-odd (cons -10 (cons -9 (cons -8 empty)))) 9)
(check-expect (absolutely-odd (cons 0 (cons 0 (cons 0 empty)))) 0)
(check-expect (absolutely-odd (cons 1 (cons 3 (cons -5 empty)))) 9)
(check-expect (absolutely-odd (cons 1 (cons 3 (cons 5 empty)))) 9)
(check-expect (absolutely-odd (cons -1 (cons -3 (cons -17 empty)))) 21)




              



;;PART B

(define integer-has-no-sign 0)
;;(spiraling? list-of-integers) determines if a list of integers is a
;;    spiraling list. A list is a spiraling list if the list alternates between
;;    positive and negative integers and the absolute value of the integers
;;    strictly increases.

;;spiraling?: (listof Int) -> Bool
;;requires: (list-of-integers) must be a list of integers,
;;          or just one integer or empty

(check-expect(spiraling? (cons -1 (cons 2 (cons -3 (cons 4 empty)))))true)
(check-expect(spiraling? (cons 99 (cons -100 (cons 100 empty))))false)

(define (spiraling? list-of-integers)
  (cond
    ;if it wasn't spiraling, false would have already occured
    [(empty? list-of-integers) true]
    [(and (not (list? list-of-integers)) (integer? list-of-integers)) true] 
    [(empty? (rest list-of-integers)) true]
    [(or (and (positive? (first list-of-integers))
                  (positive? (first (rest list-of-integers))))
             (and (negative? (first list-of-integers))
                  (negative? (first (rest list-of-integers)))))
         false]
       ;if the number is 0, and the rest of the list isn't empty, it is false
       [(and (= (first list-of-integers) integer-has-no-sign)
             (not (empty? (rest list-of-integers)))) false]
       [(>= (abs (first list-of-integers)) (abs(first (rest list-of-integers))))
         false] ;deal with false cases during recursions because cond exits true
        [else (spiraling? (rest list-of-integers))]))

(check-expect(spiraling? empty) true)
(check-expect(spiraling? 5) true)
(check-expect(spiraling? (cons 1 (cons -10 (cons 100 empty))))true)
(check-expect(spiraling? (cons 0 (cons -10 (cons 100 empty))))false)
(check-expect(spiraling? (cons -99 (cons -100 (cons 200 empty))))false)
(check-expect(spiraling? (cons 99 (cons 100 (cons 200 empty))))false)
(check-expect(spiraling? (cons 0 empty))true)
(check-expect(spiraling? (cons -1 empty))true)
(check-expect(spiraling? (cons 1 empty))true)
(check-expect(spiraling? (cons 0 (cons 0 empty)))false)
(check-expect(spiraling? (cons 0 (cons -10 empty)))false)
(check-expect(spiraling? (cons 10 (cons 0 empty)))false)
(check-expect(spiraling? (cons -100 (cons 200 empty)))true)





;;PART C

;;The following is a helper function
;;(number-multiplier list-of-num) multiplies all the numbers in a list
;;     of numbers together.
;; number-multiplier: (listof Num)-> Num
;; requires: All elements in list-of-num must be positive numbers greater
;;           than 0.

(check-expect (number-multiplier (cons 9 (cons 0.5 (cons 6 empty)))) 27)

(define (number-multiplier list-of-num)
  (cond [(empty? list-of-num) 1]
        [else (* (first list-of-num)(number-multiplier (rest list-of-num)))]
  )
)





;;The following is a helper function
;;(length-finder list-of-num) finds the length of a list.

;; length-finder: (listof Any)-> Nat
;;   For the purposes of this assignment, it will only take
;;   listof Num as input.

(check-expect (length-finder (cons 9 (cons 0.5 (cons 6 empty)))) 3)

(define (length-finder list-of-num)
  (cond [(empty? list-of-num) 0]
        [else (+ 1 (length-finder (rest list-of-num)))]
        )
  )



;; The following is the main function

;; (geometric-mean list-of-num) calculates the geometric mean of a list of
;;       numbers
;; geometric-mean: (listof Num)-> Num
;; requires: All elements in list-of-num must be positive numbers greater
;;           than 0.


(check-within(geometric-mean (cons 9 (cons 0.5 (cons 6 empty))))3 0.0001)
(check-within(geometric-mean (cons 1 (cons 2 (cons 3 (cons 4 empty)))))
             2.2133 0.0001)

(define (geometric-mean list-of-num)
  (expt (number-multiplier list-of-num) (/ 1 (length-finder list-of-num))))


(check-expect(geometric-mean (cons 1 (cons 1 (cons 1 (cons 1 empty)))))1)
(check-within(geometric-mean (cons 5 (cons 5 (cons 5 (cons 5 empty)))))5 0.001)
(check-within(geometric-mean (cons 4.7 (cons 5.1 (cons 6.66 (cons 1.1
                             (cons 6.1 empty))))))4.0362 0.0001)


