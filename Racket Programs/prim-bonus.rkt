;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname prim-bonus) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;;************************************
;;   Anudeep Das (20782887)
;;   CS 135 Fall 2018
;;   Assignment 07, Problem 2 Bonus                                    
;;*************************************



(require "a07drawinglib.rkt")



(define canvas-width 300)
(define canvas-height 300)

(define prim-picture
  (list
   ;tear system
   (make-prim-triangle (make-posn 165 150) (make-posn 175 150)
                       (make-posn 170 165) "Cyan")
   (make-prim-triangle (make-posn 125 150) (make-posn 135 150)
                       (make-posn 130 165) "Cyan")

   ;;nose system
   (make-prim-triangle (make-posn 145 160) (make-posn 155 160)
                       (make-posn 150 175) "Green")
   (make-prim-triangle (make-posn 145 160) (make-posn 155 160)
                       (make-posn 150 145) "Green")

   ;teeth system
   (make-prim-circle (make-posn 150 175) 25 "Blue")
   (make-prim-triangle (make-posn 130 190) (make-posn 140 190)
                       (make-posn 135 215) "Red")
   (make-prim-triangle (make-posn 160 190) (make-posn 170 190)
                       (make-posn 165 215) "Red")

   ;smile
   ;(make-prim-circle (make-posn 150 175) 25 "Blue")
   (make-prim-circle (make-posn 150 180) 25 "White")
   ;eyes
   (make-prim-circle (make-posn 130 140) 10 "White")
   (make-prim-circle (make-posn 170 140) 10 "White")
   ;face
   (make-prim-circle (make-posn 150 150) 60 "Blue")
   ;ears
   (make-prim-circle (make-posn 130 110) 27 "Red")
   (make-prim-circle (make-posn 180 110) 20 "Red")
   

   )
  )

(render-image (make-posn canvas-width canvas-height) prim-picture)
;(save-image (render-image (make-posn canvas-width canvas-height) prim-pictures)
;"prim-bonus.png")