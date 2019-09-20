;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname charlistpradctice) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
(define (count-l/char char-list)
  (cond [(empty? char-list) 0]
        [(char=? (first char-list #\l)) (+ 1 (count-l/char (rest char-list)))]
        [else (count-l/char (rest char-list))]
        
        )
  )

(define (count-l/str str)
  (count-l/char (string->list str))  ;converts the string to a list of chars
  )


(count-l/str "Hello")