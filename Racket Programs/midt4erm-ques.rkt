;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname midt4erm-ques) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
(define a 12)
(define b (/ a 2))
(define x1 (+ a 1))
(define y1 a)
(define x2 (/ a 3))
(define y2 6)

(define (mdist x1 y1 x2 y2)
(+ (abs (- x1 x2))
   (abs (- y1 y2))))

;(mdist 3 7 4 6)

;(mdist y2 x1 x2 y1)

;(or (< 13 5) (> 2 3))    Invalid

(cond
[(symbol=? 'papaya 'bumblebee) 83]
[(>= x1 (+ x2 x2)) x2];
[else y1])

;(cond
;[(< 180 180) 'yellow]
;[(and (not false) (= (max 4 -6) (sqr 2))) 'red]
;[(= (/ 3 (sqrt 9)) 1) 'yellow]
;[else 'green])

(define (pay-duty? duration goods alcohol? profession)
  (cond [(symbol=? profession 'ambassador) false]
        [(symbol=? profession 'diplomat) false]
        [(< duration 24) true]
        [(< duration 48)
                      (cond [(> goods 200) true]
                            [alcohol? true]
                            [else false])]  ;ALL CONDS NEED AN ELSE
        [(> goods 800) true]
        [else false]))

(check-expect (pay-duty? 12 100 false 'ambassador) false)
(check-expect (pay-duty? 12 100 false 'diplomat) false)
(check-expect (pay-duty? 12 100 false 'clerk) true)
(check-expect (pay-duty? 36 100 true 'emptty) true)
(check-expect (pay-duty? 36 300 false 'ssador) true)
(check-expect (pay-duty? 49 300 false 'ssador) false)
(check-expect (pay-duty? 50 801 true 'ssador) true)
(check-expect (pay-duty? 36 100 false 'ssador) false)









(define (pay-duty?/bool duration goods alcohol? profession)
  (and
   (or (< duration 24)
       (and (< duration 48) (or (> goods 200) alcohol?))
       (and (>= duration 48) (> goods 800))
   )

   (not (or (symbol=? profession 'ambassador)
            (symbol=? profession 'diplomat)
         )
        )
   )
  )


(check-expect (pay-duty?/bool 12 100 false 'ambassador) false)
(check-expect (pay-duty?/bool 12 100 false 'diplomat) false)
(check-expect (pay-duty?/bool 12 100 false 'clerk) true)
(check-expect (pay-duty?/bool 36 100 true 'emptty) true)
(check-expect (pay-duty?/bool 36 300 false 'ssador) true)
(check-expect (pay-duty?/bool 49 300 false 'ssador) false)
(check-expect (pay-duty?/bool 50 801 true 'ssador) true)
(check-expect (pay-duty?/bool 36 100 false 'ssador) false)