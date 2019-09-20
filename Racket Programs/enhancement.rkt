;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname enhancement) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))



;;(my-check-expect expct-value actual-value) checks if the tested value is the
;;        same as the expected value

;; my-check-expect: Any Any -> Sym

(check-expect (my-check-expect true false) 'Failed)
(check-expect (my-check-expect 9 9) 'Passed)


(define (my-check-expect expect-value actual-value)
  (cond [(equal? expect-value actual-value) 'Passed] ;;equal works on all types
        [else 'Failed]
  )
)

(check-expect (my-check-expect 'yes 'yes) 'Passed)
(check-expect (my-check-expect -1 0) 'Failed)
(check-expect (my-check-expect "hello there" "hello there") 'Passed)


;;The following is the my-check-within function
;; (my-check-within expected-value actual-value tolerance)
;;    checks whether an output value falls within the acceptable tolerance of an
;;    expected value for the output

;;my-check-within: Num Num Num -> Sym

(check-expect (my-check-within 12.37 12 0.4) 'Passed)
(check-expect (my-check-within e 3 0.5) 'Passed)

(define (my-check-within expected-value actual-value tolerance)
  (cond [(and (>= expected-value (- actual-value tolerance))
             (<= expected-value (+ actual-value tolerance)))
             'Passed]
        [else 'Failed]
  )
)

(check-expect (my-check-within pi 3.1 0.0005) 'Failed)


