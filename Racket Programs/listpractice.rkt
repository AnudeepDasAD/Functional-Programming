;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname listpractice) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
(define-struct person (name height))

(define adrian (make-person "Adrian" 1.85))
(define adrians-mom (make-person "Adrian's Mom" 1.67))
(define adrians-brother (make-person "Adrian's brother" 1.8499))

(define adrians-family (cons adrians-mom
                             (cons adrians-brother (cons adrian empty))))

(person-height(first(rest adrians-family))) ;first of the rest is brother, finds his height

(define (tallest family)
  (cond ;[(empty? family) stop] pseudocode
    [(empty? family) 0] ;empty has no height
    ;gives a person, the first person
         [else (max (person-height(first family))
                    (tallest(rest family))) ;recursion, finds tallest in the rest of the family
            
          ]
         )
  )

;does (max height1 (max height2 (max height3 0)))

;(tallest adrians-family)

(define (factorial n)
  (cond [(= n 1) 1]
        [else (* n (factorial(- n 1)))]
        )
  )

(define number-list (cons 4 (cons 3 (cons 2 (cons 1 empty)))))

(define (factorial-lists l)
(cons [(empty? l) 1]
      [else (*(first l) (factorial-lists (rest l)))]
      )
  )

(factor-lists number-list)