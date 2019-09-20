;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname abstraction) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
(define (my-filter-odd list)
  (cond [(empty? list) empty]
        [(odd? (first list)) (cond (first list) (my-filter-odd (rest list)))]
        [else (my-filter-odd (rest list))]))

;;general filter function

;any predicate

;don't necesssarily need a question mark,just used for form

(define (my-filter pred? list)
  (cond [(empty? list) empty]
        [(pred? (first list)) (cons (first list) (my-filter pred? (rest list)))]
        [else (my-filter pred? (rest list))]))

;function in parameter list, and used that function later in body of function

;(my-filter odd? '(1 2 3 4 5 6 7 8))
;(my-filter even? '(1 2 3 4 5 6 7 8))
                              

(define (filter-const my-filter) my-filter)


;;its returns a predicate!
(define (symbol2func symb)
  (cond [(symbol=? symb 'o) odd?]
        [(symbol=? symb 'e) even?]
        [else false]))

;(my-filter (symbol2func 'o) '(1 2 3 4 5 6 7 8))

(define (foo f x y) (f x y))

;(foo make-posn 2 3)

;;returns a function called func
(define (make-adder n)
  (local
    [(define (func m) (+ n m))]
    func)) ;returns the function that was defined

;(make-adder 3) has lambda error (no idea what that is yet)

(define add5 (make-adder 5))

;;these are the same
(add5 6)

;;called inner function to 6)
;; make-adder makes a function and passes 6 in as a parameter

;((make-adder 5)6) ;4 is the m

;;n comes from the call of the inner function
(define (make-divisibleX? x)
  (local
    [(define (divisible? n) (zero? (remainder n x)))]))
  
                       
       