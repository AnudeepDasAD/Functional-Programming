;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname conversion) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;Question 3a::::  miles-per-gallon (mpg) to litres-per-100km (lp100km)

;; (miles/gallon * 1.609344km/mile * 1 gallon/ 3.785411784 L)^ -1 (for inversion) (converting miles per gallon to km per litre) 

;; parameters are gallons and miles, and they should each be converted into km and litres
;; litres/ km *100 is the final calculation

(define (mpg->lp100km mpg)    
  (* (expt(/ (* mpg 1609.344) (* 1000 3.785411784)) -1) 100))


;;Question 3b

;; 1 mile = 1760 yards, 5.5 yards = 1 rod, 4 rods = 1 chain

;;gallon--> litres --> ml--> thimble (2.1 mL)


(define (mpg->cpt mpg)
  (/ (* mpg 1760 2.1) 5.5 4 3.785411784 1000)) 


                                 