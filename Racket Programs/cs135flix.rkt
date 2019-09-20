;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname cs135flix) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;;************************************
;;   Anudeep Das (20782887)
;;   CS 135 Fall 2018
;;   Assignment 05, Problem 3                                    
;;*************************************



;;PART A

;;The following is a helper fucntion
;;(add-list movie-fv1 movie-fv2) adds the elements of two lists together

;;add-list: Movie-fv Movie-fv -> (list Int Int Int Int Int Int Int Int)
;;         A Movie-fv is a (list Nat Nat Nat Nat Nat Nat Nat Nat)
;; requires: movie-fv1 and movie-fv2 are the same length


(check-expect (add-list (list 1 1 0 0 0 0 0 0)(list 1 1 1 0 0 0 0 0))
              (list 2 2 1 0 0 0 0 0))

(define (add-list movie-fv1 movie-fv2)
  ;since both lists are same length, if one empties, they are both empty
  (cond [(empty? movie-fv1) empty]
        [(empty? movie-fv2) movie-fv1] ;if only one list given, return it
        [else (cons (+ (first movie-fv1) (first movie-fv2))
                    (add-list (rest movie-fv1) (rest movie-fv2)))]))




;;The following is another helper function

(define negation-factor -1)

;;(list-negator movie-fv) negates the values in a movie-fv, that is
;;  it multiplies all of it's elements by -1 and returns the negated
;;  list

;;list-negator: Movie-fv -> (list Int Int Int Int Int Int Int Int)

(check-expect (list-negator (list 1 1 1 0 0 0 0 0)) (list -1 -1 -1 0 0 0 0 0))

(define (list-negator movie-fv)
  (cond [(empty? movie-fv) empty]
        [else (cons (* negation-factor(first movie-fv))
                    (list-negator (rest movie-fv)))]))
                      



(define good-rating 1)
(define bad-rating -1)


;; (find-preference rating-list movie-fv-list) consumes a (listof Rating) and a
;;    (listof Movie-fv) for a particular client.
;;    The two lists are the same length and the ith element of the first list
;;    corresponds to the rating of the movie whose Movie-fv is the ith element
;;    of the second list. The function find-preference should produce a Pref-v
;;    (i.e. a preference vector), which will be an eight-element list that
;;    consists of the sum of the scores for each of the eight genres.

;; find-preference (listof Rating) (listof Movie-fv) -> Pref-v
;; requires: both lists are the same size
;;           both lists are non-empty.


;; A Rating is an Int
;; requires: Int is either -1 or 1

;; A Movie-fv is a (list Nat Nat Nat Nat Nat Nat Nat Nat)
;; A Pref-v is a (list Int Int Int Int Int Int Int Int)
  
(check-expect (find-preference (list 1 1 -1)(list (list 1 1 0 0 0 0 0 0)
                                                  (list 1 1 1 0 0 0 0 0)
                                                  (list 0 1 1 1 0 0 0 0)))
              (list 2 1 0 -1 0 0 0 0))

(define (find-preference rating-list movie-fv-list)
  (cond [(empty? rating-list) empty]
        ;if the rating is a bad rating, the negated list will be analysed
        [(=(first rating-list) bad-rating)
         (add-list (list-negator (first movie-fv-list))
            (find-preference (rest rating-list)(rest movie-fv-list)))]

        ;otherwise the regular lists will be added
        [else (add-list (first movie-fv-list)
                 (find-preference (rest rating-list)(rest movie-fv-list)))]))


(check-expect (find-preference (list 1)(list (list 1 1 1 0 0 0 0 0)))
              (list 1 1 1 0 0 0 0 0))
(check-expect (find-preference (list -1)(list (list 1 1 1 0 0 0 0 0)))
               (list -1 -1 -1 0 0 0 0 0))
(check-expect (find-preference (list 1 1 1)(list (list 1 1 0 0 0 0 0 0)
                                                 (list 1 1 1 0 0 0 0 0)
                                                 (list 0 1 1 1 0 0 0 0)))
              (list 2 3 2 1 0 0 0 0))

(check-expect (find-preference (list -1 1 1)(list (list 1 1 0 0 0 0 0 0)
                                                  (list 1 1 1 0 0 0 0 0)
                                                  (list 0 1 1 1 0 0 0 0)))
              (list 0 1 2 1 0 0 0 0))

(check-expect (find-preference (list 1 -1 1)(list (list 1 1 0 0 0 0 0 0)
                                                  (list 1 1 1 0 0 0 0 0)
                                                  (list 0 1 1 1 0 0 0 0)))
              (list 0 1 0 1 0 0 0 0))


;; The first list addition that will occur will be the third movie and the empty
;;    list. The sum of those two will be added to the second movie,
;;    and the sum will then be added to the first movie.

;;If any movie has a bad rating, it's list will be negated beforehand









;;PART B

(define add-nothing 0)

(define-struct movie (title genres))
;; A Movie is a (make-movie Str Movie-fv)


;;The following is a helper function

;; (dot-product-calculator pref-v movie-genres) calculates the dot product of
;;  a pref-v and a movie-genres list, however it can calculate the dot product
;;  of any two lists.

;; dot-product-calculator: (listof Int) (listof Nat) -> Int
;; requires: none of the lists should be empty

(check-expect (dot-product-calculator (list 2 1 0 -1 0 0 0 0)
                                      (list 1 0 0  0 1 0 1 0))2)
                                 
(define (dot-product-calculator pref-v movie-genres)
  (cond [(empty? pref-v) add-nothing]
        [else (+ (*(first pref-v) (first movie-genres))
                 (dot-product-calculator (rest pref-v) (rest movie-genres)))]))



;;The following is another helper function
;; (dot-product-list-creator pref-v movie-list) creates a list of dot-product
;;    values for different movies using the above function (thus would require
;;    a preference vector for the movies, and the genres for each movie.

;; dot-product-list-creator: Pref-v (listof Movie) -> (listof Int)
;; requires: none of the lists should be empty
;;           (movie-genres (first movie-list)) must have
;;           the same length as pref-v for each movie in movie-list

(check-expect (dot-product-list-creator (list 2 1 0 -1 0 0 0 0)
                         (list (make-movie "The Meg" (list 1 0 0 0 1 0 1 0))
                          (make-movie "Smallfoot" (list 0 1 1 0 0 0 0 0))
                          (make-movie "A Star is Born" (list 0 0 0 1 0 1 0 0))))
              (list 2 1 -1))

(define (dot-product-list-creator pref-v movie-list)
  (cond [(empty? movie-list) empty]
        [else (cons (dot-product-calculator pref-v
                                            (movie-genres (first movie-list)))
                    (dot-product-list-creator pref-v (rest movie-list)))]))



;;The following is another helper function

;;(greatest-score-finder first-movie-value first-dot-product-value
;;                       dot-product-list-except-first movie-list-except-first)
;;   returns the  movie in the list of movies that has the greatest score,
;;   using the list of dot-products (which are the scores for each movie).

;; greatest-score-finder: Movie Int (listof Int) (listof Movie) -> Movie
;; requires: None of the lists should be empty
;;           dot-product-list-except-first and movie-list-except-first
;;           must have the same length.

;;           first-dot-product value must be the score of first-movie-value
;;          (dot product of it's genres and the pref-v).

;;          (movie-genres first-movie-value) and
;;          (movie-genres (first movie-list-except-first))
;;          cannot be empty for any movie in the movie list.

;;          Each dot-product in the dot-product-list-except-first and
;;          first-dot-product-value must be unique, none of these numbers should
;;          be equal.

(check-expect (greatest-score-finder
               (make-movie "Smallfoot" (list 0 1 1 0 0 0 0 0)) 1 (list 2 -1)
               (list (make-movie "The Meg" (list 1 0 0 0 1 0 1 0))
                     (make-movie "A Star is Born" (list 0 0 0 1 0 1 0 0))))
              (make-movie "The Meg" (list 1 0 0 0 1 0 1 0))) 
              
(define (greatest-score-finder first-movie-value first-dot-product-value
                                     dot-product-list-except-first
                                     movie-list-except-first)
  (cond
        ;if the end of the dot product list has been reached, the
        ;  first movie value will be the highest scoring movie
        [(empty? dot-product-list-except-first) first-movie-value]
        
        [(> (first dot-product-list-except-first) first-dot-product-value)
         (greatest-score-finder (first movie-list-except-first)
                                (first dot-product-list-except-first)
                                (rest dot-product-list-except-first)
                                (rest movie-list-except-first))]

        ;otherwise the next movie and dot-product values are analysed
        [else
         (greatest-score-finder first-movie-value first-dot-product-value
                                    (rest dot-product-list-except-first)
                                    (rest movie-list-except-first))]
        )
  )

;; It compares the first movie's score (first-dot-product-value)
;;        to the second movie's score(which is first on the dot-product-list)
;;        and if it is greater,
;;        the first-movie-value will be replaced, with the greater value,
;;        there will be recursion with the new values





;; The main function follows


;; (suggestions pref-v movie-list) takes the dot product
;;      of the Pref-v and the Movie-fv, for each Movie in the (listof Movie),
;;      to get a score.
;;      The title of the movie with the highest score is reported.

;; suggestions: Pref-v (listof Movie) -> Str
;; requires: (listof Movie) is non-empty
;;           There must not be a tie between any of the movies 

(check-expect (suggestions (list 2 1 0 -1 0 0 0 0)
                            (list (make-movie "The Meg" (list 1 0 0 0 1 0 1 0))
                                 (make-movie "Smallfoot" (list 0 1 1 0 0 0 0 0))
                          (make-movie "A Star is Born" (list 0 0 0 1 0 1 0 0))))
               "The Meg")



(define (suggestions pref-v movie-list)
  (movie-title (greatest-score-finder (first movie-list)
                        (first(dot-product-list-creator pref-v movie-list))
                        (rest (dot-product-list-creator pref-v movie-list))
                        (rest movie-list))))


(check-expect (suggestions (list 2 1 0 -1 0 0 0 0)
                           (list(make-movie "Smallfoot" (list 0 1 1 0 0 0 0 0))
                          (make-movie "A Star is Born" (list 0 0 0 1 0 1 0 0))
                          (make-movie "The Meg" (list 1 0 0 0 1 0 1 0))))
               "The Meg")

(check-expect (suggestions (list 2 1 0 -1 0 0 0 0)
                           (list(make-movie "The Meg" (list 0 1 1 0 0 0 0 0))
                          (make-movie "A Star is Born" (list 0 0 0 1 0 1 0 0))
                          (make-movie "Smallfoot" (list 1 0 0 0 1 0 1 0))))
               "Smallfoot")

(check-expect (suggestions (list 3 1 0 -1 0 1 0 1)
                           (list(make-movie "The Meg" (list 0 1 1 0 0 0 0 1))
                          (make-movie "A Star is Born" (list 1 1 0 1 0 1 0 0))
                          (make-movie "Smallfoot" (list 1 0 0 0 1 0 1 0))))
               "A Star is Born")

  

