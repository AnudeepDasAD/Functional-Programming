;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname morerecursionpractice) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;purpose: creates a list

(define (countdown n)
  (cond
    [(zero? n) (cons 0 empty)]
    [else (cons n (countdown (sub1 n)))] ;sub1 subtracts 1 from a number
    )
  )
;(countdown 3)

(define (countdown2 n stop)
  (cond
    [(= n stop) (cons n empty)]
    [else (cons n (countdown2 (sub1 n) stop))] ;stop is the place where we stop counting to
    )
  ) ;parameter n must be manipulated in order to avoid infinity loop

(countdown2 10 5)