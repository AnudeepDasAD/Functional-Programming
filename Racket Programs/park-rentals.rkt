;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname park-rentals) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;;************************************
;;   Anudeep Das (20782887)
;;   CS 135 Fall 2018
;;   Assignment 04, Problem 2                                    
;;*************************************


(define-struct bicycle (make model serial-number))
;; A Bicycle is a (make-bicycle Str Str Nat)
(define-struct boat (type serial-number))
;; A Boat is a (make-boat Sym Nat)
;; requires: type is 'paddle-boat or 'canoe
(define-struct horse (name serial-number capacity stamina))
;; A Horse is a (make-horse Str Nat Nat Num)
;; requires: stamina > 0, capacity > 0
;; A Rental is (anyof Bicycle Boat Horse)


(define-struct Rental (bicycle boat horse))


;;PART A
;; Rental-template: Rental -> Any
;; requires: A Rental is (anyof Bicycle Boat Horse)

;(define (Rental-template info)
;  (cond [(bicycle? info)
;         (...(bicycle-make)
;          ...(bicycle-model)...
;          ...(bicycle-serial-number)...)]
;        [(boat? info)
;         (...(boat-type)...
;          ...(boat- serial-number)...)]
;        [(horse? info)
;         (... (horse-name)...
;          ... (horse-serial-number)...
;          ... (horse-capacity)...
;          ... (horse-stamina)...)]))
        


;;PART B 
;; (rental-id myrental) determines the serial number for my given rental
;; rental-id: Rental -> Nat
;; requires: A Rental is (anyof Bicycle Boat Horse)
;;           serial number of a Rental must be a Nat


(check-expect(rental-id (make-bicycle "Cannondale" "Jekyll" 125)) 125)
(check-expect(rental-id (make-horse "Stomper" 79 4 1.3)) 79)

(define (rental-id myrental)
  (cond [(bicycle? myrental)(bicycle-serial-number myrental)]
        [(boat? myrental)(boat-serial-number  myrental)]
        [(horse? myrental)(horse-serial-number myrental)]
        )
  )

(check-expect(rental-id (make-bicycle "Cannondale" "Jekyll" 321)) 321)
(check-expect(rental-id (make-horse "Stomper" 82 4 1.3)) 82)
(check-expect(rental-id (make-boat 'canoe 763)) 763)





;;PART C

(define no-renters 0)
(define no-duration 0)
(define renter-limit-bike 1)
(define renter-limit-canoe 2)
(define renter-limit-paddleboat 3)

;; (rental-ok? myrental num-of-renters rental-duration-hours) determines whether
;;       a rental is legal based on the number of renters and the duration of
;;       the rental period in hours, with each type of rental having different
;;       requirements for the number of renters and duration

;; rental-ok?: Rental Nat Num -> Bool
;; requires:   rental-duration-hours >= 0

(check-expect(rental-ok? (make-bicycle "Cannondale" "Jekyll" 321) 1 2.3)true)
(check-expect(rental-ok? (make-bicycle "Cannondale" "Jekyll" 321) 4 2.3)false)



(define (rental-ok? myrental num-of-renters rental-duration-hours)
  (cond
        [(or (<= num-of-renters no-renters)
             (<= rental-duration-hours no-duration))
         false]
        
        [(and (bicycle? myrental) (= num-of-renters renter-limit-bike)) true]
        
        [(and (boat? myrental)
              (or (and (symbol=? (boat-type myrental) 'canoe)
                             (<= num-of-renters renter-limit-canoe))
                  (and (symbol=? (boat-type myrental) 'paddle-boat)
                             (<= num-of-renters renter-limit-paddleboat))))true]
        
        [(and (horse? myrental)(<= rental-duration-hours
                                   (horse-stamina myrental))) true]
        [else false]
        
        )
  )

(check-expect(rental-ok? (make-boat 'canoe 321) -1 2.3)false)
(check-expect(rental-ok? (make-boat 'canoe 321) 1 -2.3)false)
(check-expect(rental-ok? (make-horse "Stomper" 82 4 1.3) 2 1)true)
(check-expect(rental-ok? (make-horse "Stomper" 82 4 1.3) 2 1.3)true)
(check-expect(rental-ok? (make-horse "Stomper" 82 4 1.3) 2 1.5)false)
(check-expect(rental-ok? (make-boat 'paddle-boat 321) 3 0)false)
(check-expect(rental-ok? (make-boat 'paddle-boat 321) 3 1)true)
(check-expect(rental-ok? (make-boat 'paddle-boat 321) 2 1)true)
(check-expect(rental-ok? (make-boat 'paddle-boat 321) 4 1)false)
(check-expect(rental-ok? (make-boat 'canoe 321) 2 2)true)
(check-expect(rental-ok? (make-boat 'canoe 321) 1 2)true)
(check-expect(rental-ok? (make-boat 'canoe 321) 3 2)false)





;;PART D


(define multiply-by-this-number-to-convert-hours-to-half-hour-period 2)
(define upper-bound-decimal-for-adding-1-more-half-hour-period 0.5)

(define lowerbound-for-duration-time 0)
(define add-1-half-hour-period 1)
(define add-2-half-hour-periods 2)

(define no-decimal-hours 0)
(define no-more-half-hour-periods-added 0)

;; The following is a helper function
;;(hour-to-half-hour-converter rental-duration-hours) converts
;;  hours into half hour periods, rounding up fraction hours to the
;;  next half hour. 

;; hour-to-half-hour-converter: Num -> Nat
;; requires: rental-duration-hours>0

(check-expect (hour-to-half-hour-converter 2.5) 5)

(define (hour-to-half-hour-converter rental-duration-hours)
  (+ (* (floor rental-duration-hours)
        multiply-by-this-number-to-convert-hours-to-half-hour-period)

     ; if the decimal is less than or equals 0.5 (upper bound decimal),
     ;    need to add 1 more half-hour period
     (cond [(and (<=(- rental-duration-hours (floor rental-duration-hours))
                    upper-bound-decimal-for-adding-1-more-half-hour-period)
                 (>(- rental-duration-hours (floor rental-duration-hours))
                   lowerbound-for-duration-time))
            add-1-half-hour-period]

           ;if there is no decimal, no more half hours need to be added
           [(= (- rental-duration-hours (floor rental-duration-hours))
               no-decimal-hours) no-more-half-hour-periods-added]

           ;if the decimal is greater than 0.5, need to add 2 more
           ;   half hour periods
           [else add-2-half-hour-periods]))
  )






;;This is another helper function

(define num-renters-with-no-extra-charge 3)
(define num-renters-must-be-greater-than-this-number 0)
(define horse-half-hourly-cost-up-to-3-people 30)
(define extra-horse-half-hourly-cost-more-than-3-people 10)

;;(renting-horse-cost num-of-renters rental-duration-hours) determines the cost 
;;  of renting horeses based on the number of renters and the duration for which
;;  they are used.

;;renting-horse-cost: Nat Num -> Nat
;; requires: rental-duration-hours>0
;;           num-of-renters>0

(check-expect(renting-horse-cost 1 1.75)120)
(check-expect(renting-horse-cost 5 1.75)200)


(define (renting-horse-cost num-of-renters rental-duration-hours)
  (cond
    [(and (<= num-of-renters num-renters-with-no-extra-charge)
          (> num-of-renters num-renters-must-be-greater-than-this-number))

     (* horse-half-hourly-cost-up-to-3-people
            (hour-to-half-hour-converter rental-duration-hours))]

        ;more than 3 renters 
        [else (+ (* horse-half-hourly-cost-up-to-3-people
                 (hour-to-half-hour-converter rental-duration-hours))
                 
                 (* extra-horse-half-hourly-cost-more-than-3-people
                    (- num-of-renters num-renters-with-no-extra-charge)
            (hour-to-half-hour-converter rental-duration-hours)))]
        )
  )





;;The following is the main function

(define cost-per-hour-bike 20) ;in dollars
(define cost-per-hour-boat 30)

;;(rental-price myrental num-of-renters rental-duration-hours) determines the
;;    cost of a rental given the type of rental, the number of renters, and
;;    the rental duration in hours

;; rental-price: Rental Nat Num -> Nat
;;requires: (rental-ok? myrental num-of-renters rental-duration-hours)
;;          must be true

(check-expect(rental-price (make-horse "Stomper" 82 4 1.3) 2 1)60)
(check-expect(rental-price (make-bicycle "Cannondale" "Jekyll" 321) 1 2.3)60)
(check-expect(rental-price (make-boat 'canoe 321) 1 2.3)90)



(define (rental-price myrental num-of-renters rental-duration-hours)
  (cond [(bicycle? myrental) 
         (* (ceiling rental-duration-hours) cost-per-hour-bike)]
        [(boat? myrental)    
         (*(ceiling rental-duration-hours) cost-per-hour-boat)]
        [else  (renting-horse-cost num-of-renters rental-duration-hours)]))
                                    


(check-expect(rental-price (make-bicycle "Cannondale" "Jekyll" 321) 1 2.5)60)
(check-expect(rental-price (make-bicycle "Cannondale" "Jekyll" 321) 1 2.51)60)
(check-expect(rental-price (make-bicycle "Cannondale" "Jekyll" 321) 1 2)40)

(check-expect(rental-price (make-boat 'canoe 321) 1 2.3)90)
(check-expect(rental-price (make-boat 'canoe 321) 1 3.5)120)
(check-expect(rental-price (make-boat 'paddle-boat 321) 1 5)150)
(check-expect(rental-price (make-boat 'paddle-boat 321) 1 2.6)90)

(check-expect(rental-price (make-horse "Stomper" 82 4 1.3) 2 1)60)
(check-expect(rental-price (make-horse "Stomper" 82 4 1.5) 3 1)60)
(check-expect(rental-price (make-horse "Stomper" 82 4 1.5) 3 1.1)90)
(check-expect(rental-price (make-horse "Stomper" 82 4 1.5) 3 1.25)90)
(check-expect(rental-price (make-horse "Stomper" 82 4 1.5) 3 1.5)90)
(check-expect(rental-price (make-horse "Stomper" 82 4 1.5) 3 1.6)120)
(check-expect(rental-price (make-horse "Stomper" 82 4 1.5) 3 2)120)

(check-expect(rental-price (make-horse "Stomper" 82 4 1.3) 4 1)80)
(check-expect(rental-price (make-horse "Stomper" 82 4 1.3) 5 1)100)
(check-expect(rental-price (make-horse "Stomper" 82 4 1.3) 5 1.1)150)
(check-expect(rental-price (make-horse "Stomper" 82 4 1.3) 5 1.25)150)
(check-expect(rental-price (make-horse "Stomper" 82 4 1.3) 5 1.5)150)
(check-expect(rental-price (make-horse "Stomper" 82 4 1.3) 5 1.6)200)
(check-expect(rental-price (make-horse "Stomper" 82 4 1.3) 5 2)200)










