;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname final-cs135-grade) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;Question 4a Calculates final grade
(define (final_grade_finder m1 m2 final assi)
  (+ (* 0.1 m1) (* 0.2 m2)(* 0.45 final) (* 0.2 assi) (* 100 0.05)));what about participation marks?

(final_grade_finder 87 91 91 87)



