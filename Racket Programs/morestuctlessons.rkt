;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname morestuctlessons) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
(define-struct unit-grunt (hp))

;;TEMPLATE

(define (unit-grunt-template unit)
  (
   ... (unit-grunt-hp unit)..
  )
  )

(define-struct unit-mage (hp heal))



(define (unit-mage-template unit)
  (
   ... (unit-mage-hp unit)..
       (unit-mage-heal)
  )
  )

;;units with more health points defeats the other one

;;contract
;;unit-grunt (anyof unit-grunt unit-mage) -> Bool

(define (will-defeat? my-unit other-unit)
  (cond [(and (unit-grunt? other-unit) (>= (unit-grunt-hp my-unit) (unit-grunt-hp other-unit))) true]
        [(and (unit-grunt? other-unit) (< (unit-grunt-hp my-unit) (unit-grunt-hp other-unit))) false]
        [(and (unit-magegrunt? other-unit) (>= (unit-grunt-hp my-unit) (+ (unit-mage other-unit) (unit-mage-hp other-unit)))) true]
        [(and (unit-magegrunt? other-unit) (< (unit-grunt-hp my-unit) (+ (unit-mage other-unit) (unit-mage-hp other-unit)))) false]
        
  

       ; [(>= (unit-mage-hp my-unit) (unit-mage-hp other-unit))]
        [else false]
  )
  )

(define other (make-unit-mage 10 50))
(define mine (make-unit-grunt 10 50))

(unit-grunt? mine) ;;returns true
(unit-mage? mine)  ;;returns false
 

(will-defeat? (make-unit-grunt 50) (make-unit 10 50)) ; this won't work because only comparing grunt with grunt

