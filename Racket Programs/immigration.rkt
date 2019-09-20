;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname immigration) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;;************************************
;;   Anudeep Das (20782887)
;;   CS 135 Fall 2018
;;   Assignment 02, Problem 2
;;*************************************

;; PART A

;;This will require some (4) helper functions, they follow below

(define points-if-age-between-17-and-49 0)
(define points-if-age-between-20-and-29 100)

;;(age-points age) determines the number of CEC points for each age
;;age-points: Nat -> Nat

(check-expect(age-points 18) 90)
(check-expect(age-points 30) 95)
(check-expect(age-points 29) 100)

(define (age-points age)
  (cond [ (or(<= age 17)(>= age 49)) points-if-age-between-17-and-49]
        [    (< age 20) (- 100 (* 5 (- 20 age)) )] ;100 - (20-age)*5
        [(and(>= age 20) (<= age 29)) points-if-age-between-20-and-29]
        [else(- 100 (* 5 (- age 29)))]
  )
 )




;;Another helper function follows

(define undergrad-pts 112)
(define grad-pts 126)
(define highschool-pts 28)

;; (education-points education-level)determines the number of CEC
;;     points gained due to education level
;; edu-points: Sym -> Nat
;;             Sym must be either 'undergraduate, 'graduate, or 'highschool

(check-expect (education-points 'undergraduate) 112)

(define(education-points education-level)
  (cond [(symbol=? education-level 'graduate) grad-pts]
        [(symbol=? education-level 'undergraduate) undergrad-pts]
        [(symbol=? education-level 'highschool) highschool-pts]
  )
)




;;Another helper function follows

(define points-if-lang-more-than-9 116)
(define points-if-lang-is-8 88)
(define points-if-lang-is-7 64)
(define max-amount-of-proficiency-possible 10)
(define lower-lang-points 0)

;; The following helper function (lang-points lang-proficiency) calculates CEC
;;     point gained based on language proficiency
;; language-points: Nat -> Nat
;;                  Nat(input) must be between 0 and 10 inclusive 

(check-expect(language-points 9) 116)

(define (language-points lang-proficiency)
  (cond [(and (>= lang-proficiency 9)
              (<= lang-proficiency max-amount-of-proficiency-possible))
         points-if-lang-more-than-9]
        [(= lang-proficiency 8) points-if-lang-is-8]
        [(= lang-proficiency 7) points-if-lang-is-7]
        [else lower-lang-points]
   )
 )






;;Another helper function follows

(define points-if-1-yr-work 35)
(define points-if-between-1-and-2-yrs-work 56)
(define points-if-4-or-more-yrs-work 70)
(define points-if-no-work 0)

;; The next helper function (work-points yrs-work) calculatues the points
;;      obtained depending on years of work experience
;; work-points: Nat -> Nat

(check-expect(work-points 2) 56)

(define (work-points yrs-work)
  (cond [(= yrs-work 1) points-if-1-yr-work]
        [(and (>= yrs-work 2)(<= yrs-work 3))points-if-between-1-and-2-yrs-work]
        [(>= yrs-work 4) points-if-4-or-more-yrs-work]
        [else points-if-no-work]
  )
)




;;The main function follow

(define job-offer-points 200)

;; (pr-cec-score (age, edu-level, lang-proficiency, yrs-work, job-offer)
;;    Calculates the number of points they would acheive in the
;;    Canadian Experience Class (CEC) based on their age, education level,
;;    language proficiency, years of worok experience, and whether or not they
;;    already have a job offer

;;pr-cec-score: Nat Sym Nat Nat Bool -> Nat

(check-expect (pr-cec-score 22 'undergraduate 5 1 false) 247)

(define (pr-cec-score age education-level lang-proficiency yrs-work job-offer)

  (cond [job-offer (+ (age-points age)(education-points education-level)
                   (language-points  lang-proficiency)
                   (work-points yrs-work) job-offer-points)]
        [else      (+ (age-points age)(education-points education-level)
                      (language-points lang-proficiency)
                      (work-points yrs-work))]
  )
)

(check-expect (pr-cec-score 22 'undergraduate 8 1 true) 535)
(check-expect (pr-cec-score 22 'undergraduate 5 1 true) 447)
(check-expect (pr-cec-score 17 'highschool 7 4 false) 162)
(check-expect (pr-cec-score 17 'highschool 7 0 false) 92)






;;PART B of the question follows

(define cec-cutoff-score 350)

;;(pr-cec-eligible? age, edu-level, lang-proficiency, yrs-work, job-offer)
;;    uses the per-cec-score function as a helper function and returns true if
;;    the number of points is greater than or equal to 350

;;pr-cec-eligible?: Nat Sym Nat Nat Bool -> Bool

(check-expect(pr-cec-eligible? 28 'graduate 9 2 false)true)
(check-expect(pr-cec-eligible? 80 'highschool 1 2 false)false)


(define(pr-cec-eligible? age education-level lang-proficiency yrs-work
        job-offer)
  (cond
  [(>=(pr-cec-score age education-level lang-proficiency yrs-work job-offer)
       cec-cutoff-score)
             true]
       [else false]
  )
)

(check-expect (pr-cec-eligible? 17 'highschool 7 4 false) false)
(check-expect (pr-cec-eligible? 17 'highschool 7 4 true) true)
(check-expect (pr-cec-eligible? 22 'undergraduate 5 1 false) false)
(check-expect (pr-cec-eligible? 22 'undergraduate 5 1 true) true)




