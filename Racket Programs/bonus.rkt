;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname bonus) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;;************************************
;;   Anudeep Das (20782887)
;;   CS 135 Fall 2018
;;   Assignment 09, Problem 1                                    
;;*************************************



;;PART A


(define (solution? function list)
  (equal? (build-list (length list) function) list))





;;PART B

(define list-length-2 2)
(define list-length-1 1)
(define mult-by-2 2)
(define div-by--2 -2)
(define zero-coefficient 0)
(define empty-case 0)



(define (guess-quadratic loi)
  (cond [(empty? loi) (lambda (num) empty-case)]
        [(=(length loi) list-length-2)
         (lambda (num) (+ (* zero-coefficient (sqr num)) (* (- (second loi) (first loi)) num)
                                            (first loi)))]
        [(= (length loi) list-length-1)
         (lambda (num) (+ (* zero-coefficient (sqr num)) (* zero-coefficient num) (first loi)))]
        
        [else
         (local [(define a (/(- (* mult-by-2 (- (second loi) (first loi)))
                                (- (third loi) (first loi))) div-by--2))
          (define b (-(- (second loi) (first loi)) a))
          (define c (first loi))]
           
           (lambda (num) (+ (* a (sqr num)) (* b num) c)))]))







;;PART C

(define (try-quadratic listof-int)
  (local [(define quadratic-equation (guess-quadratic listof-int))]
  (cond [(solution? quadratic-equation listof-int)quadratic-equation]
        [else empty])))





;;PART D

(define minimum-list-length 3)
(define base-case-zero 0)
(define base-case-one 1)
(define value-is-zero 0)
(define list-length-below-1 1)



(define (guess-recursive listof-int)
  (cond [(< (length listof-int) list-length-below-1) (lambda (x) value-is-zero)]
        [(= (length listof-int) list-length-below-1) (lambda (x) (first listof-int))]
        [else 
         (local [(define b
                   (cond [(< (length listof-int) minimum-list-length) value-is-zero]

                     [(= (length listof-int) minimum-list-length)
                          (cond [(not(= (first listof-int) value-is-zero))
                                 (/ (third listof-int) (first listof-int))]
                                [else value-is-zero])]
                         
                         
                         [(= (* (first listof-int) (third listof-int))
                                (sqr (second listof-int))) value-is-zero]
                         
                         [else (/ (- (sqr (third listof-int))
                                     (* (fourth listof-int) (second listof-int)))
                              (- (* (first listof-int)
                                    (third listof-int)) (sqr (second listof-int))))]))

                 
                 (define a
                   (cond [(< (length listof-int) minimum-list-length) value-is-zero]

                     [(= (length listof-int) minimum-list-length)
                          (cond [(and(= b value-is-zero)
                                      (not (= (second listof-int) value-is-zero)))
                                 (/ (third listof-int) (second listof-int))]
                                [else value-is-zero])]
                         

                     [(not (= (second listof-int) value-is-zero))
                          (/ (- (third listof-int) (* b (first listof-int))) (second listof-int))]
                         
                         
                         [else value-is-zero]))]  
           (lambda (x)
                    (cond [(= x base-case-zero) (first listof-int)]
                          [(= x base-case-one) (second listof-int)]
                          [else
                           (local [(define (previous-term-addition back-2 back-1 num)
                                     (cond
                                       [(= num value-is-zero) (+ (* a back-1) (* b back-2))]
                                       [else
                                        (previous-term-addition back-1
                                                                (+ (* a back-1) (* b back-2))
                                                                (sub1 num))]))]
                             ;back-2 is 2 behind the term being looked at (n i-2)
                             ;back-1 is right behind the term being looked at (n i-1)
                                    (previous-term-addition (first listof-int) (second listof-int)
                                                     (- x 2)))])))]))




(define (try-recursive listof-int)
  (cond [(solution? (guess-recursive listof-int) listof-int) (guess-recursive listof-int)]
        [else empty]))





;;PART D


(define (solve listof-int)
  (local [(define quadratic-test (try-quadratic listof-int))
          (define recursive-test (try-recursive listof-int))]
  (cond [(not(empty? quadratic-test)) quadratic-test]
        [else
         (cond [(not (empty? recursive-test)) recursive-test]
               [else empty])])))

 

(define (guess-exponential listof-int)
  (lambda (x) (expt (second listof-int) x)))

(define (try-exponential listof-int)
  (cond [(solution? (guess-exponential listof-int) listof-int) (guess-exponential listof-int)]
        [else empty]))

(define (guess-cospi-n listof-int)
  (filter (lambda (x) 

(define (supersolve listof-int)
  (local [(define quadratic-or-recursive (solve listof-int))]
    (cond [(empty? quadratic-or-recursive) 0]
          [else quadratic-or-recursive]))

(check-expect ((supersolve '(0 2 4 8 16))4) 16)

         